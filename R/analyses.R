# analyses.R ------------------------------------------------------------------
# Revision-specific analyses (checklist items) built on completed runs.
# Each returns tidy data and writes a CSV under the run's tables/ directory.

#' Held-out validation of the synthetic controls (checklist section 2)
#'
#' For a run fitted with `sc.match_end > 0`, tests whether the treated-minus-
#' synthetic gap in the held-out pre-treatment weeks is zero, outcome by
#' outcome, using the per-unit mean gaps from `sc_diag/unit_gaps.csv`
#' (one-sample t-test across treated units) and the ratio of held-out to
#' matched-window RMSPE. Holdout (placebo) outcomes are included with
#' `matched = FALSE`.
#' @param run A `bsc_run` with `sc_diag` done.
#' @return data.table, also written to `tables/heldout_validation.csv`.
#' @export
an_heldout_validation <- function(run) {
  me <- as.integer(run$spec$sc$match_end)
  if (me == 0L) stop("run was fitted with match_end = 0; nothing is held out.", call. = FALSE)
  ug <- data.table::fread(file.path(run$dir, "sc_diag", "unit_gaps.csv"))
  fs <- data.table::fread(file.path(run$dir, "sc_diag", "fit_summary.csv"))
  out <- ug[, {
    x <- gap_heldout_pre[is.finite(gap_heldout_pre)]
    tt <- if (length(x) > 2) stats::t.test(x) else list(statistic = NA_real_, p.value = NA_real_)
    .(n_treated = length(x), mean_heldout_gap = mean(x), se = stats::sd(x) / sqrt(length(x)),
      t = as.numeric(tt$statistic), p = tt$p.value,
      mean_post_gap = mean(gap_post, na.rm = TRUE),
      gap_ratio = mean(x) / mean(gap_post, na.rm = TRUE))
  }, by = outcome]
  rm_ <- data.table::dcast(fs[, .(outcome, window, rmspe)], outcome ~ window, value.var = "rmspe")
  out <- merge(out, rm_[, .(outcome, rmspe_matched = matched_pre, rmspe_heldout = heldout_pre, rmspe_post = post)], by = "outcome")
  out[, rmspe_ratio_heldout := rmspe_heldout / rmspe_matched]
  out[, matched := outcome %in% run$spec$sc$match_outcomes]
  out[, held_out_weeks := me]
  dir.create(file.path(run$dir, "tables"), showWarnings = FALSE)
  data.table::fwrite(out, file.path(run$dir, "tables", "heldout_validation.csv"))
  out
}

#' Correlations among outcomes in the pre-treatment period (checklist section 5)
#'
#' Pearson correlations of the FWB outcomes among treated units, both at the
#' customer-week level and between customer-level pre-treatment means.
#' @param run A `bsc_run` with `sample` done.
#' @param outcomes Character; default the spec's `tables.families$fwb`.
#' @return list(weekly, customer) correlation matrices; long form written to
#'   `tables/dv_correlations.csv`.
#' @export
an_dv_correlations <- function(run, outcomes = NULL) {
  outcomes <- outcomes %||% run$spec$tables$families$fwb
  p <- readRDS(file.path(run$dir, "panel.rds")); data.table::setDT(p)
  smp <- readRDS(file.path(run$dir, "sample.rds"))
  tr <- smp$ids[role == "treated"]
  pre <- p[tr, on = "customer_id"][wID < i.treat_wID, c("customer_id", outcomes), with = FALSE]
  weekly <- stats::cor(as.matrix(pre[, outcomes, with = FALSE]), use = "pairwise.complete.obs")
  cm <- pre[, lapply(.SD, mean, na.rm = TRUE), by = customer_id, .SDcols = outcomes]
  customer <- stats::cor(as.matrix(cm[, outcomes, with = FALSE]), use = "pairwise.complete.obs")
  long <- data.table::rbindlist(list(
    data.table::as.data.table(as.table(weekly))[, level := "customer_week"],
    data.table::as.data.table(as.table(customer))[, level := "customer_mean"]))
  data.table::setnames(long, c("V1", "V2", "N"), c("outcome_a", "outcome_b", "r"))
  dir.create(file.path(run$dir, "tables"), showWarnings = FALSE)
  data.table::fwrite(long, file.path(run$dir, "tables", "dv_correlations.csv"))
  list(weekly = weekly, customer = customer)
}

#' Placebo summary across runs
#'
#' Stacks outcome tables of placebo runs next to the real run, adding the
#' placebo type and the unit-level significance split, which is the relevant
#' evidence (the ATE credible interval conditions on the weights and the fixed
#' set of units).
#' @param real Run id/name of the real run.
#' @param placebos Character vector of placebo run ids/names.
#' @param root Project root.
#' @return data.table written nowhere (combine into the appendix as needed).
#' @export
an_placebo_table <- function(real, placebos, root = NULL) {
  ct <- compare_runs(c(real, placebos), root)
  ct[, placebo := name != run_load(real, root)$name]
  ct[, sig_split := pct_sig_pos - pct_sig_neg]
  ct[order(outcome, placebo)]
}

utils::globalVariables(c("gap_heldout_pre", "gap_post", "rmspe_heldout", "rmspe_matched",
  "rmspe_ratio_heldout", "held_out_weeks", "heldout_pre", "matched_pre", "post", "i.treat_wID",
  "level", "placebo", "pct_sig_pos", "pct_sig_neg", "sig_split"))
