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

# Per-unit table behind gap_ate(): for each fitted treated unit, its adoption week, the effect (mean post gap minus
# mean pre gap over the window T - L .. T + H) and the pre-adoption fit (RMSPE of the gap around its pre-window mean).
.gap_unit_table <- function(run, outcome) {
  p  <- data.table::as.data.table(readRDS(file.path(run$dir, "panel.rds"))); sc <- readRDS(file.path(run$dir, "sc_fit.rds"))
  Wm <- sc$fit$weights_mat; Y <- build_Ylist(p, outcome)$Y_list[[1]]
  L  <- run$spec$sample$n_lags; H <- run$spec$sample$n_leads
  t0 <- sc$treat_time[match(rownames(Wm), as.character(sc$unit_vals))]
  synth <- Wm %*% Y
  rows <- lapply(seq_len(nrow(Wm)), function(j) {
    idx <- (t0[j] - L):(t0[j] + H); idx <- idx[idx >= 1 & idx <= ncol(Y)]
    y <- Y[rownames(Wm)[j], idx]; gap <- y - synth[j, idx]; pre <- idx < t0[j]
    gp <- gap[pre]
    c(effect = mean(gap[!pre], na.rm = TRUE) - mean(gp, na.rm = TRUE),
      pre_rmspe = sqrt(mean((gp - mean(gp, na.rm = TRUE))^2, na.rm = TRUE)), pre_level = mean(y[pre], na.rm = TRUE)) })
  data.table::data.table(customer_id = rownames(Wm), onset = sc$time_vals[t0], do.call(rbind, rows))
}

#' Corrected effect estimate for one outcome of a finished run
#'
#' For every adopter, the gap against its synthetic twin in each week of the
#' window `T - L .. T + H`; the unit effect is the mean post gap minus the mean
#' pre gap, and the estimate is the mean over units. Drift that moves adopters
#' and donors alike, and level differences between them, cancel. Uses only the
#' saved panel and weights, so it also covers outcomes that were not in the
#' matching set (for example `liquidity_deficit_rate`, `income_share`).
#' `se_across_units` is the standard deviation of unit effects over the square
#' root of their number. Units share donors, so it is a lower bound; judge an
#' effect against the placebo distribution ([an_placebo_distribution()]).
#' @param run A `bsc_run` with `panel.rds` and `sc_fit.rds`.
#' @param outcome Outcome column in the panel.
#' @param onset Optional length-2 vector `c(first, last)`: keep only units that
#'   adopted in these weeks (inclusive). Use it to compare runs with different
#'   horizons on the same adopters, since the horizon changes who is treated.
#' @return data.table: outcome, n_units, ate, se_across_units, median_unit,
#'   pct_pos, pct_neg.
#' @export
gap_ate <- function(run, outcome, onset = NULL) {
  u <- .gap_unit_table(run, outcome)
  if (!is.null(onset)) { rng <- range(onset); u <- u[u$onset >= rng[1] & u$onset <= rng[2]] }   # the argument shares its name with the column
  eff <- u$effect
  data.table::data.table(outcome = outcome, n_units = sum(is.finite(eff)), ate = mean(eff, na.rm = TRUE),
                         se_across_units = stats::sd(eff, na.rm = TRUE) / sqrt(sum(is.finite(eff))),
                         median_unit = stats::median(eff, na.rm = TRUE),
                         pct_pos = 100 * mean(eff > 0, na.rm = TRUE), pct_neg = 100 * mean(eff < 0, na.rm = TRUE))
}

#' Unit-level corrected effects
#'
#' One row per fitted treated unit: its adoption week (`onset`), the effect used by
#' [gap_ate()] and the pre-adoption fit (`pre_rmspe`, with the mean pre-adoption
#' `pre_level` for scale). Use it to compare horizons or specifications on
#' identical units, or to inspect effects by adoption week.
#' @inheritParams gap_ate
#' @return data.table: customer_id, onset, effect, pre_rmspe, pre_level.
#' @export
gap_unit_effects <- function(run, outcome) .gap_unit_table(run, outcome)[]

#' Pre-adoption fit by adoption group
#'
#' How well the synthetic control tracks the adopters before adoption, by tertile of
#' adoption week: the mean root-mean-square gap around the pre-window mean and its
#' size relative to the outcome's level. Late adopters have fewer eligible donors
#' under per-unit eligibility, so a clearly worse fit in the last group is the price
#' of that design.
#' @param run A `bsc_run` with `panel.rds` and `sc_fit.rds`.
#' @param outcomes Outcomes to report; default the matched outcomes among
#'   `numarrears`, `total_spend`.
#' @param breaks Optional breaks for adoption-week bins; default tertiles.
#' @return data.table: outcome, adoption_week, units, mean_adoption_week,
#'   mean_pre_rmspe, mean_pre_level, rmspe_over_level.
#' @export
an_prefit <- function(run, outcomes = NULL, breaks = NULL) {
  sc <- readRDS(file.path(run$dir, "sc_fit.rds"))
  outcomes <- outcomes %||% intersect(c("numarrears", "total_spend"), sc$outcomes)
  data.table::rbindlist(lapply(outcomes, function(o) {
    u <- .gap_unit_table(run, o)
    br <- breaks %||% unique(stats::quantile(u$onset, c(0, 1/3, 2/3, 1), names = FALSE))
    u[, list(outcome = o, units = .N, mean_adoption_week = round(mean(onset), 1), mean_pre_rmspe = mean(pre_rmspe, na.rm = TRUE),
             mean_pre_level = mean(pre_level, na.rm = TRUE), rmspe_over_level = mean(pre_rmspe, na.rm = TRUE) / mean(pre_level, na.rm = TRUE)),
      by = list(adoption_week = cut(onset, br, include.lowest = TRUE))][order(adoption_week)]
  }))
}

#' Pre-adoption trend check of the synthetic-control gap
#'
#' Reads `sc_diag/pre_slope.csv`: per outcome, the mean per-unit slope of the
#' gap over the matched pre-window (per week), its t-statistic, and
#' `trend_implied_ate`, the post-minus-pre difference that continuing that trend
#' would produce. If the estimated effect is close to `trend_implied_ate`, it is
#' as consistent with trend continuation as with a step at adoption.
#' @param run A `bsc_run` with `sc_diag` done.
#' @param with_effect Logical; add the corrected estimate from [gap_ate()] for each outcome.
#' @return data.table.
#' @export
an_pretrend <- function(run, with_effect = TRUE) {
  f <- file.path(run$dir, "sc_diag", "pre_slope.csv")
  if (!file.exists(f)) stop("no sc_diag/pre_slope.csv; run sc_diagnostics(run) (budgetsc >= 0.5.0) first.", call. = FALSE)
  d <- data.table::fread(f)
  if (with_effect) {
    est <- data.table::rbindlist(lapply(d$outcome, function(o) tryCatch(gap_ate(run, o)[, .(outcome, ate)], error = function(e) NULL)))
    d <- merge(d, est, by = "outcome", all.x = TRUE)
    d[, effect_minus_trend := ate - trend_implied_ate]
  }
  d[]
}

#' How much synthetic-control weight sits on donors that adopt during a treated unit's post window?
#'
#' In the original design later adopters are donors for every treated unit, so a
#' donor that starts budgeting inside a treated unit's post window carries a
#' treatment effect into that unit's counterfactual and biases the estimate toward
#' zero. This function measures it for a finished run: for each treated unit, the
#' total weight on donors whose REAL adoption week falls in `(T_j, T_j + H]`,
#' summarised by tertiles of adoption week. It reads the real adoption dates from
#' the original panel file named in the spec (the run's own `panel.rds` has them
#' removed for later adopters). With `sc.donor_eligibility = "per_unit"` the
#' answer is zero by construction; for runs registered before 0.5.0 it is not.
#' @param run A `bsc_run` with `sc_fit.rds`.
#' @param breaks Optional breaks for adoption-week bins; default tertiles.
#' @param root Project root.
#' @return data.table: adoption_week bin, units, mean_adoption_week,
#'   mean_weight_on_contaminated_donors, share_units_any_contamination.
#' @export
an_donor_contamination <- function(run, breaks = NULL, root = NULL) {
  sc <- readRDS(file.path(run$dir, "sc_fit.rds")); Wm <- sc$fit$weights_mat
  f <- run$spec$data$panel_file; if (!grepl("^(/|[A-Za-z]:)", f)) f <- file.path(bsc_root(root), f)
  pa <- data.table::as.data.table(readRDS(f))
  on <- unique(pa[!is.na(minBudgetDate), list(customer_id, onset = week_of(as.Date(minBudgetDate)))])
  t0 <- sc$treat_time[match(rownames(Wm), as.character(sc$unit_vals))]
  don <- on$onset[match(colnames(Wm), as.character(on$customer_id))]
  H <- run$spec$sample$n_leads
  contam <- vapply(seq_len(nrow(Wm)), function(j) sum(Wm[j, ] * (!is.na(don) & don > t0[j] & don <= t0[j] + H)), numeric(1))
  br <- breaks %||% unique(stats::quantile(t0, c(0, 1/3, 2/3, 1), names = FALSE))
  data.table::data.table(onset = t0, contam = contam)[, list(units = .N, mean_adoption_week = round(mean(onset), 1),
      mean_weight_on_contaminated_donors = round(mean(contam), 3), share_units_any_contamination = round(mean(contam > 1e-8), 3)),
    by = list(adoption_week = cut(onset, br, include.lowest = TRUE))][order(adoption_week)]
}

#' Register a set of never-onboarder placebo runs
#'
#' Registers one [spec_placebo_never()] run per seed on top of `base`. Each draws
#' a different set of pseudo-treated never-onboarders (with onsets drawn from the
#' real adopters' onset dates), so the set gives the distribution of the estimate
#' when nobody is treated. The runs have no post-estimation outcomes: they need
#' only the SC stage, and [an_placebo_distribution()] reads their gaps.
#' @param base A `bsc_spec`; it must point at a panel that contains
#'   never-onboarders (donor code 2), for example
#'   `spec_main("data.panel_file" = "Processed/analysis_panel_all.rds")`.
#' @param seeds Integer seeds, one run each.
#' @param root Project root.
#' @return Character vector of run ids.
#' @export
placebo_set <- function(base = spec_main(), seeds = 1:30, root = NULL) {
  vapply(seeds, function(sd) {
    sp <- spec_placebo_never(base, seed = as.integer(sd))
    sp$name <- paste0(base$name, "_placebo_s", sd)
    sp$post$outcomes <- character(0)
    run_register(sp, root)$id
  }, character(1))
}

#' Where the real estimate sits in the placebo distribution
#'
#' For each outcome: the real corrected estimate, the mean and spread of the
#' placebo estimates, the placebo-adjusted estimate (real minus placebo mean), a
#' z-score against the placebo spread, and an empirical two-sided p-value,
#' `(1 + #{|placebo - mean| >= |real - mean|}) / (1 + n)`.
#' @param real Run id (or name) of the real run.
#' @param placebo Run ids from [placebo_set()] (only those with an SC fit are used).
#' @param outcomes Outcomes to report; default the real run's post outcomes.
#' @param root Project root.
#' @return data.table, one row per outcome.
#' @export
an_placebo_distribution <- function(real, placebo, outcomes = NULL, root = NULL) {
  rr <- run_load(real, root); outcomes <- outcomes %||% rr$spec$post$outcomes
  est <- data.table::rbindlist(lapply(outcomes, function(o) gap_ate(rr, o)[, .(outcome, ate, se_across_units)]))
  pl <- data.table::rbindlist(lapply(placebo, function(id) {
    r <- run_load(id, root)
    if (!file.exists(file.path(r$dir, "sc_fit.rds"))) return(NULL)
    data.table::rbindlist(lapply(outcomes, function(o) tryCatch(cbind(placebo_run = id, gap_ate(r, o)[, .(outcome, ate)]), error = function(e) NULL)))
  }))
  if (!nrow(pl)) stop("none of the placebo runs has an SC fit yet.", call. = FALSE)
  sm <- pl[, .(n_placebos = .N, placebo_mean = mean(ate), placebo_sd = stats::sd(ate),
               placebo_q025 = stats::quantile(ate, 0.025), placebo_q975 = stats::quantile(ate, 0.975)), by = outcome]
  out <- merge(est, sm, by = "outcome")
  out[, adjusted := ate - placebo_mean][, z := adjusted / placebo_sd]
  out[, p_empirical := vapply(seq_len(.N), function(i)
    (1 + sum(abs(pl[outcome == out$outcome[i], ate] - out$placebo_mean[i]) >= abs(out$ate[i] - out$placebo_mean[i]))) / (1 + out$n_placebos[i]), numeric(1))]
  out[match(outcomes, outcome)][]
}

utils::globalVariables(c("pre_rmspe", "pre_level", "onset", "contam", "adoption_week", "minBudgetDate", "ate", "placebo_mean", "placebo_sd", "adjusted", "z", "n_placebos", "trend_implied_ate", "effect_minus_trend", "gap_heldout_pre", "gap_post", "rmspe_heldout", "rmspe_matched",
  "rmspe_ratio_heldout", "held_out_weeks", "heldout_pre", "matched_pre", "post", "i.treat_wID",
  "level", "placebo", "pct_sig_pos", "pct_sig_neg", "sig_split"))
