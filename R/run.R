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
