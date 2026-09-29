# jobs.R ----------------------------------------------------------------------
# A job is one unit of work inside a run that a machine can claim: the SC fit
# (one per run), or one outcome of the post-estimation stage. Jobs are claimed
# with the lock protocol in locks.R, so any number of VMs can start worker()
# on the same shared filesystem and divide the work without a scheduler.
#
# Phase 0 supplies the mechanics only. The functions that actually fit models
# (`fit_sc()`, `fit_post_one()`) arrive in Phase 1 and are plugged in through
# the `fun` argument, which is also how the tests exercise the protocol with a
# dummy job.

.job_dir <- function(run, stage) {
  d <- switch(stage,
              sc   = run$dir,
              post = file.path(run$dir, "post"),
              stop("Unknown stage '", stage, "'.", call. = FALSE))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

.job_names <- function(run, stage) {
  switch(stage,
         sc   = "sc_fit",
         post = run$spec$post$outcomes)
}

.job_result_file <- function(run, stage, job) {
  switch(stage,
         sc   = file.path(run$dir, "sc_fit.rds"),
         post = file.path(run$dir, "post", paste0(job, ".rds")))
}

.job_failed_file <- function(run, stage, job) {
  switch(stage,
         sc   = file.path(run$dir, "sc_fit.failed"),
         post = file.path(run$dir, "post", paste0(job, ".failed")))
}

#' Table of jobs in a run and their states
#'
#' States: `done` (result file exists), `running` (a fresh lock is held),
#' `stale` (lock older than `stale_after`), `failed` (last attempt errored;
#' see the `.failed` marker and `run.log`), `pending` (nothing yet).
#' @param run A `bsc_run`.
#' @param stage `"sc"` or `"post"`.
#' @param stale_after Seconds; see [lock_is_stale()].
#' @return data.table with `stage`, `job`, `state`, `owner`, `age_secs`.
#' @export
job_table <- function(run, stage = "post", stale_after = 3 * 3600) {
  jd <- .job_dir(run, stage)
  jobs <- .job_names(run, stage)
  rows <- lapply(jobs, function(j) {
    done <- file.exists(.job_result_file(run, stage, j))
    failed <- file.exists(.job_failed_file(run, stage, j))
    info <- lock_info(jd, j)
    state <- if (done) "done" else if (!is.null(info)) {
      if (info$age_secs > stale_after) "stale" else "running"
    } else if (failed) "failed" else "pending"
    data.table::data.table(stage = stage, job = j, state = state,
                           owner = if (is.null(info)) NA_character_ else info$owner,
                           age_secs = if (is.null(info)) NA_real_ else info$age_secs)
  })
  data.table::rbindlist(rows)
}

#' Claim a job
#'
#' Returns a lock if the job is pending (or stale and reclaimable), otherwise
#' `NULL`. A job whose result file already exists is never claimed.
#' @inheritParams job_table
#' @param job Job name (an outcome for `stage = "post"`).
#' @return A `bsc_lock` or `NULL`.
#' @export
job_claim <- function(run, stage, job, stale_after = 3 * 3600) {
  if (file.exists(.job_result_file(run, stage, job))) return(NULL)
  lock_acquire(.job_dir(run, stage), job, stale_after = stale_after,
               note = paste0(run$id, "/", stage, "/", job))
}

#' Run one claimed job with heartbeat, logging and cleanup
#'
#' `fun(run, job)` must write its own result file (see
#' the result-file conventions in `jobs.R`); this wrapper only manages the lock,
#' periodic heartbeats and the log. Heartbeats are sent from a background
#' timer only if the `later` package is available; otherwise `fun` should
#' call [lock_heartbeat()] itself for long jobs (the sampler stage will).
#' @param run A `bsc_run`.
#' @param stage `"sc"` or `"post"`.
#' @param job Job name.
#' @param fun Function `(run, job, lock)` doing the work.
#' @param lock The `bsc_lock` returned by [job_claim()].
#' @return `TRUE` on success, `FALSE` on error (the error is logged).
#' @export
job_run <- function(run, stage, job, fun, lock) {
  stopifnot(inherits(lock, "bsc_lock"))
  on.exit(lock_release(lock), add = TRUE)
  bsc_log(run, "start ", stage, "/", job)
  t0 <- Sys.time()
  ok <- tryCatch({
    fun(run, job, lock)
    if (!file.exists(.job_result_file(run, stage, job)))
      stop("job function returned without writing ", .job_result_file(run, stage, job))
    TRUE
  }, error = function(e) {
    bsc_log(run, "ERROR ", stage, "/", job, ": ", conditionMessage(e))
    writeLines(c(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), host_id(), conditionMessage(e)),
               .job_failed_file(run, stage, job))
    FALSE
  })
  if (ok && file.exists(.job_failed_file(run, stage, job))) unlink(.job_failed_file(run, stage, job))
  mins <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)
  bsc_log(run, if (ok) "done " else "failed ", stage, "/", job, " (", mins, " min)")
  ok
}

#' Worker loop: claim and run pending jobs across registered runs
#'
#' Start this on every machine. It scans the registry, claims jobs in
#' `stages` order, runs them through `fun`, and either exits when nothing is
#' left (`once = TRUE`) or sleeps and rescans.
#' @param fun Function `(run, job, lock)` for the job; Phase 1 supplies the
#'   real stage functions, e.g. `function(run, job, lock) fit_post_one(run, job, lock)`.
#' @param root Project root.
#' @param stages Stages to serve, in priority order. Default `"post"`.
#' @param runs Optional character vector of run ids or names to restrict to.
#' @param max_jobs Stop after this many jobs. Default `Inf`.
#' @param once Logical; exit after one scan finds nothing. Default TRUE.
#' @param sleep Seconds between scans when `once = FALSE`. Default 60.
#' @param stale_after Seconds; see [lock_is_stale()].
#' @param retry_failed Logical; also claim jobs whose last attempt failed
#'   (`.failed` marker). Default FALSE, so a broken job does not loop forever.
#' @return Invisibly, a data.table of jobs attempted with `ok` flags.
#' @export
worker <- function(fun, root = NULL, stages = "post", runs = NULL,
                   max_jobs = Inf, once = TRUE, sleep = 60, stale_after = 3 * 3600,
                   retry_failed = FALSE) {
  states <- c("pending", "stale", if (retry_failed) "failed")
  tried <- character(0)
  done <- list()
  n <- 0L
  repeat {
    reg <- registry_read(root)
    if (!is.null(runs)) reg <- reg[run_id %in% runs | name %in% runs]
    claimed_any <- FALSE
    for (i in seq_len(nrow(reg))) {
      run <- run_load(reg$run_id[i], root)
      for (stage in stages) {
        jt <- job_table(run, stage, stale_after)
        for (j in jt[state %in% states, job]) {
          if (n >= max_jobs) break
          key <- paste(run$id, stage, j)
          if (key %in% tried) next             # never retry within one worker session
          tried <- c(tried, key)
          lk <- job_claim(run, stage, j, stale_after)
          if (is.null(lk)) next               # someone else got it between scan and claim
          claimed_any <- TRUE
          ok <- job_run(run, stage, j, fun, lk)
          n <- n + 1L
          done[[length(done) + 1L]] <- data.table::data.table(run_id = run$id, stage = stage, job = j, ok = ok)
        }
      }
    }
    if (n >= max_jobs || (once && !claimed_any)) break
    if (!claimed_any) Sys.sleep(sleep)
  }
  invisible(if (length(done)) data.table::rbindlist(done) else
              data.table::data.table(run_id = character(0), stage = character(0),
                                     job = character(0), ok = logical(0)))
}
