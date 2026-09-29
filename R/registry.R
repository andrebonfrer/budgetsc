# registry.R ------------------------------------------------------------------
# Where runs live and how they are found.
#
#   <root>/Runs/registry.csv        one row per run (append-only, rewritten only
#                                   under its own lock)
#   <root>/Runs/<run_id>/           everything the run produces
#
# A `bsc_run` object is a light handle: id, name, dir, spec. Stage functions
# receive a run and write into run$dir. Stage completion is a marker file
# <stage>.done so that status can be read without loading any result object.

#' Project root and runs directory
#'
#' Root resolution: `root` argument, option `budgetsc.root`, environment
#' variable `BUDGETSC_ROOT`, then the working directory.
#' @param root Optional path.
#' @return Normalised path.
#' @export
bsc_root <- function(root = NULL) {
  r <- root %||% getOption("budgetsc.root") %||% Sys.getenv("BUDGETSC_ROOT", unset = "") 
  if (!nzchar(r)) r <- getwd()
  normalizePath(r, mustWork = FALSE)
}

#' @rdname bsc_root
#' @export
runs_root <- function(root = NULL) file.path(bsc_root(root), "Runs")

.registry_file <- function(root = NULL) file.path(runs_root(root), "registry.csv")

.registry_cols <- c("run_id", "name", "created", "host", "spec_hash", "design", "cohort", "status")

#' Read the run registry
#' @param root Project root; see [bsc_root()].
#' @return data.table with one row per registered run (empty if none).
#' @export
registry_read <- function(root = NULL) {
  f <- .registry_file(root)
  if (!file.exists(f)) {
    return(data.table::as.data.table(setNames(replicate(length(.registry_cols), character(0), simplify = FALSE),
                                              .registry_cols)))
  }
  data.table::fread(f, colClasses = "character")
}

.registry_append <- function(row, root = NULL) {
  rr <- runs_root(root)
  dir.create(rr, recursive = TRUE, showWarnings = FALSE)
  with_lock(rr, "registry", {
    f <- .registry_file(root)
    data.table::fwrite(row, f, append = file.exists(f))
  }, wait = TRUE, timeout = 120, must = TRUE, note = "registry append")
  invisible(row)
}

#' Register a run from a specification
#'
#' Creates `Runs/<run_id>/`, writes `spec.yml`/`spec.rds` and `provenance.rds`,
#' and appends a registry row. Registering an identical spec again returns the
#' existing run (same id) without duplicating anything.
#'
#' @param spec A `bsc_spec`.
#' @param root Project root; see [bsc_root()].
#' @param package_sha Optional git SHA of the package build, recorded in
#'   provenance.
#' @return A `bsc_run` object: `id`, `name`, `dir`, `spec`.
#' @export
run_register <- function(spec, root = NULL, package_sha = NULL) {
  spec <- spec_validate(spec)
  id <- spec_id(spec)
  dir <- file.path(runs_root(root), id)
  existing <- dir.exists(dir) && file.exists(file.path(dir, "spec.rds"))

  if (!existing) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    spec_write(spec, file.path(dir, "spec.yml"))
    saveRDS(spec, file.path(dir, "spec.rds"))
    prov <- list(
      created      = Sys.time(),
      host         = host_id(),
      r_version    = R.version.string,
      packages     = .package_versions(c("budgetsc", "augMultiSynth", "scmBayesPost",
                                         "data.table", "fixest")),
      package_sha  = package_sha %||% NA_character_,
      spec_hash    = spec_id(spec, full = TRUE),
      timeline     = tryCatch(timeline_table(), error = function(e) NULL)
    )
    saveRDS(prov, file.path(dir, "provenance.rds"))
    .registry_append(data.table::data.table(
      run_id = id, name = spec$name, created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      host = host_id(), spec_hash = spec_id(spec, full = TRUE),
      design = spec$sample$design, cohort = spec$sample$cohort, status = "registered"
    ), root = root)
    bsc_log(dir, "registered run '", spec$name, "' (", id, ")")
  }
  structure(list(id = id, name = spec$name, dir = dir, spec = spec), class = "bsc_run")
}

#' Load a registered run by id or name
#' @param x Run id (10-character) or run name.
#' @param root Project root.
#' @return A `bsc_run`.
#' @export
run_load <- function(x, root = NULL) {
  reg <- registry_read(root)
  hit <- reg[run_id == x | name == x]
  if (!nrow(hit)) stop("No registered run matching '", x, "'.", call. = FALSE)
  if (nrow(hit) > 1L) {
    hit <- hit[nrow(hit)]
    message("Several runs named '", x, "'; using the most recent (", hit$run_id, ").")
  }
  dir <- file.path(runs_root(root), hit$run_id)
  spec <- readRDS(file.path(dir, "spec.rds"))
  structure(list(id = hit$run_id, name = hit$name, dir = dir, spec = spec), class = "bsc_run")
}

#' Stage completion markers
#'
#' Stages: `sample`, `sc`, `sc_diag`, `post`, `summary`. `post` is complete
#' when every outcome in the spec has a result file.
#' @param run A `bsc_run`.
#' @param stage Stage name.
#' @return `stage_done()` returns logical; `mark_done()` writes the marker and
#'   returns the run invisibly.
#' @export
stage_done <- function(run, stage) {
  if (stage == "post") {
    files <- file.path(run$dir, "post", paste0(run$spec$post$outcomes, ".rds"))
    return(length(files) > 0L && all(file.exists(files)))
  }
  file.exists(file.path(run$dir, paste0(stage, ".done")))
}

#' @rdname stage_done
#' @export
mark_done <- function(run, stage) {
  writeLines(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), file.path(run$dir, paste0(stage, ".done")))
  bsc_log(run, "stage '", stage, "' done")
  invisible(run)
}

#' Status of all registered runs
#'
#' Combines the registry with stage markers and lock scans, so it reflects
#' what is on disk right now, whichever machine wrote it.
#' @param root Project root.
#' @param stale_after Seconds; locks older than this are reported as stale.
#' @return data.table: one row per run with stage flags, and counts of post
#'   outcomes done / running / stale / pending.
#' @export
run_status <- function(root = NULL, stale_after = 3 * 3600) {
  reg <- registry_read(root)
  if (!nrow(reg)) return(reg)
  rows <- lapply(seq_len(nrow(reg)), function(i) {
    id <- reg$run_id[i]
    dir <- file.path(runs_root(root), id)
    spec <- tryCatch(readRDS(file.path(dir, "spec.rds")), error = function(e) NULL)
    run <- structure(list(id = id, name = reg$name[i], dir = dir, spec = spec), class = "bsc_run")
    jobs <- if (!is.null(spec)) job_table(run, "post", stale_after = stale_after) else NULL
    data.table::data.table(
      run_id = id, name = reg$name[i], design = reg$design[i], cohort = reg$cohort[i],
      sample = stage_done(run, "sample"), sc = stage_done(run, "sc"),
      sc_diag = stage_done(run, "sc_diag"),
      post_done    = if (is.null(jobs)) NA_integer_ else sum(jobs$state == "done"),
      post_running = if (is.null(jobs)) NA_integer_ else sum(jobs$state == "running"),
      post_stale   = if (is.null(jobs)) NA_integer_ else sum(jobs$state == "stale"),
      post_pending = if (is.null(jobs)) NA_integer_ else sum(jobs$state == "pending"),
      summary = stage_done(run, "summary")
    )
  })
  data.table::rbindlist(rows)
}

#' Remove bulky intermediates from a run
#'
#' Deletes `panel.rds` and `sc_fit.rds` (the large files) and, optionally, the
#' raw post-estimation draws, keeping tables and figures. Never touches the
#' registry.
#' @param run A `bsc_run`.
#' @param keep Character; any of `"panel"`, `"sc_fit"`, `"post"`. Default
#'   keeps `"post"`.
#' @return Invisibly, the files removed.
#' @export
run_clean <- function(run, keep = "post") {
  removed <- character(0)
  if (!"panel" %in% keep) removed <- c(removed, file.path(run$dir, "panel.rds"))
  if (!"sc_fit" %in% keep) removed <- c(removed, file.path(run$dir, "sc_fit.rds"))
  if (!"post" %in% keep) removed <- c(removed, list.files(file.path(run$dir, "post"),
                                                         pattern = "\\.rds$", full.names = TRUE))
  removed <- removed[file.exists(removed)]
  unlink(removed)
  if (length(removed)) bsc_log(run, "cleaned: ", paste(basename(removed), collapse = ", "))
  invisible(removed)
}

.package_versions <- function(pkgs) {
  v <- vapply(pkgs, function(p) {
    tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_)
  }, character(1))
  v
}

#' @export
print.bsc_run <- function(x, ...) {
  cat("bsc_run '", x$name, "' (", x$id, ")\n  dir: ", x$dir, "\n", sep = "")
  for (st in c("sample", "sc", "sc_diag", "post", "summary"))
    cat(sprintf("  %-8s %s\n", st, if (stage_done(x, st)) "done" else "-"))
  invisible(x)
}
