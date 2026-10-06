# run.R -----------------------------------------------------------------------
# Stage runners: each reads what the previous stage wrote into the run
# directory and leaves a marker. run_pipeline() chains them on one machine.

#' Build the panel and sample for a run
#'
#' load_panel -> derive_outcomes -> derive_moderators -> define_sample; writes
#' `panel.rds` (the sample's rows, with derived columns) and `sample.rds`, and
#' `sample_funnel.csv` under `tables/`.
#' @param run A `bsc_run`.
#' @param root Project root.
#' @return The `bsc_sample`, invisibly.
#' @export
run_sample <- function(run, root = NULL) {
  spec <- run$spec
  if (!is.null(spec$sc$from)) return(invisible(run_reuse_sc(run)))      # sc.from: sample, panel and SC fit come from the source run
  p <- load_panel(spec, root)
  derive_outcomes(p, focal = spec$derive$focal_spend, winsor_probs = spec$derive$winsor_probs,
                  roll_n = spec$derive$roll_n, cv_basis = spec$derive$cv_basis)
  if (!is.null(spec$derive$distress)) {         # pre-adoption distress type (checklist section 3)
    # budgetdummy is created later by define_sample(); give flag_distress() a
    # temporary indicator on the containing-week rule so onsets are identified.
    p[, .bd_tmp := as.integer(!is.na(minBudgetDate) & wID >= week_of(as.Date(minBudgetDate)))]
    args <- spec$derive$distress; args$bdata <- p; args$verbose <- FALSE; args$tr_col <- ".bd_tmp"
    args$overwrite <- TRUE
    do.call(flag_distress, args)
    p[, .bd_tmp := NULL]
  }
  smp0 <- define_sample(p, spec)
  derive_moderators(smp0$panel, launch_wID = smp0$window$launch_wID)
  saveRDS(smp0$panel, file.path(run$dir, "panel.rds"))
  smp <- smp0; smp$panel <- NULL
  smp$panel_md5 <- attr(p, "panel_md5")
  saveRDS(smp, file.path(run$dir, "sample.rds"))
  dir.create(file.path(run$dir, "tables"), showWarnings = FALSE)
  data.table::fwrite(smp0$funnel, file.path(run$dir, "tables", "sample_funnel.csv"))
  mark_done(run, "sample")
  bsc_log(run, "sample: ", sum(smp$ids$role == "treated"), " treated / ", sum(smp$ids$role == "donor"), " donors")
  invisible(smp0)
}

#' Run the SC stage (with its own lock) and diagnostics
#' @inheritParams run_sample
#' @export
run_sc <- function(run) {
  if (!is.null(run$spec$sc$from)) { run_reuse_sc(run); return(invisible(NULL)) }
  lk <- job_claim(run, "sc", "sc_fit")
  if (is.null(lk)) { message("sc_fit already done or claimed elsewhere"); return(invisible(NULL)) }
  job_run(run, "sc", "sc_fit", function(run, job, lock) { fit_sc(run); sc_diagnostics(run) }, lk)
  mark_done(run, "sc")
}

#' @rdname fit_post
#' @export
run_post <- function(run, outcomes = NULL, backend = NULL) fit_post(run, outcomes, backend)

#' Run every stage of a spec on this machine
#' @param spec A `bsc_spec`.
#' @param root Project root.
#' @return The `bsc_run`.
#' @export
run_pipeline <- function(spec, root = NULL) {
  run <- run_register(spec, root)
  if (!stage_done(run, "sample")) run_sample(run, root)
  if (!stage_done(run, "sc")) run_sc(run)
  if (!stage_done(run, "post")) run_post(run)
  if (!stage_done(run, "summary")) summarise_run(run)
  run
}

utils::globalVariables(c(".bd_tmp"))


#' Reuse another run's synthetic-control fit (`sc.from`)
#'
#' A run with `sc.from = <run id>` does not refit: it copies the source run's
#' `panel.rds`, `sample.rds`, `sc_fit.rds`, `sc_diag/` and the sample funnel, so
#' post-estimation variants (a different backend, moderator formula or chain)
#' cost minutes, not an SC fit. The two runs must agree on everything that
#' determines the SC fit: the `data`, `derive`, `sample` and `sc` sections of the
#' spec (apart from `sc.from` itself); otherwise the function stops and names
#' the sections that differ.
#' @param run A `bsc_run` whose spec has `sc.from`.
#' @return The run, invisibly.
#' @export
run_reuse_sc <- function(run) {
  from <- run$spec$sc$from
  if (is.null(from)) stop("this run has no sc.from.", call. = FALSE)
  src <- run_load(from, root = dirname(dirname(run$dir)))
  keys <- c("data", "derive", "sample", "sc")
  a <- .canonicalise(unclass(run$spec)[keys]); b <- .canonicalise(unclass(src$spec)[keys])
  a$sc$from <- NULL; b$sc$from <- NULL
  differ <- keys[!vapply(keys, function(k) identical(a[[k]], b[[k]]), logical(1))]
  if (length(differ))
    stop("sc.from run ", src$id, " differs from this run in: ", paste(differ, collapse = ", "),
         ". The SC fit can only be reused when those sections are identical.", call. = FALSE)
  need <- c("panel.rds", "sample.rds", "sc_fit.rds")
  miss <- need[!file.exists(file.path(src$dir, need))]
  if (length(miss)) stop("source run ", src$id, " is missing: ", paste(miss, collapse = ", "), " (its SC stage is not finished).", call. = FALSE)
  for (f in need) file.copy(file.path(src$dir, f), file.path(run$dir, f), overwrite = TRUE)
  if (dir.exists(file.path(src$dir, "sc_diag"))) {
    dir.create(file.path(run$dir, "sc_diag"), showWarnings = FALSE)
    file.copy(list.files(file.path(src$dir, "sc_diag"), full.names = TRUE), file.path(run$dir, "sc_diag"), overwrite = TRUE)
  }
  dir.create(file.path(run$dir, "tables"), showWarnings = FALSE)
  for (f in c("sample_funnel.csv", "sc_units_without_donors.csv")) {
    sf <- file.path(src$dir, "tables", f); if (file.exists(sf)) file.copy(sf, file.path(run$dir, "tables", f), overwrite = TRUE)
  }
  for (st in c("sample", "sc", "sc_diag")) mark_done(run, st)
  bsc_log(run, "reused the SC fit of run ", src$id, " (", src$name, ")")
  invisible(run)
}
