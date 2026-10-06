# estimator_grid.R: do the two fixes work, alone and together?
#
# For each simulated dataset (truth known) the synthetic control is fitted twice:
#   global   : later adopters are donors for every treated unit (the original design)
#   perunit  : a donor must adopt more than H weeks after the treated unit (the rule the paper states;
#              treated units with too few eligible donors are set aside)
# and the average effect is estimated three ways on each fit:
#   stage1   : mean over adopters of (post gap - pre gap) against the synthetic twin
#   original : the original Stage 2 (raw outcomes on a stacked unit + donors panel)
#   gap      : Stage 2 run on the gaps (the corrected estimator)
# giving six estimators: stage1_global, original_global, gap_global, stage1_perunit, original_perunit, gap_perunit.
# The truth is computed on the units each design actually fits, so the comparison is like for like.
#
# Needs budgetsc >= 0.5.0, scmBayesPost >= 0.4.7, augMultiSynth (library(Matrix) is attached as a workaround
# for the fit_weights_fw issue until augMultiSynth 0.3.6).
#
#   source("estimator_grid.R")
#   estimator_grid(seeds = 1, drifts = 0.5, effects = c(FALSE, TRUE))        # smoke test
#   res <- estimator_grid(seeds = 1:20, cores = 4)                           # 3 drifts x 2 effects x 20 seeds
#   res <- estimator_grid(seeds = 1:10, sim_args = list(covid = FALSE))      # probe the spending drift question
#   res <- estimator_grid(seeds = 1:10, sim_args = list(n_weeks = 200))      # a longer panel: more eligible donors
suppressPackageStartupMessages({ library(budgetsc); library(data.table) })
stopifnot(packageVersion("budgetsc") >= "0.5.0")
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
if (requireNamespace("Matrix", quietly = TRUE)) suppressPackageStartupMessages(library(Matrix))

run_dataset <- function(drift, effect, seed, n_treated, n_later, sim_args, outcomes = c("numarrears", "total_spend")) {
  root <- tempfile(); dir.create(file.path(root, "Processed"), recursive = TRUE)
  old <- options(budgetsc.root = root); on.exit({ options(old); unlink(root, recursive = TRUE) }, add = TRUE)
  eff <- if (effect) list(spend = -0.15, arr = -0.5) else list(spend = 0, arr = 0)
  sim <- do.call(sim_panel, c(list(n_treated = n_treated, n_later = n_later, n_never = 0, seed = seed,
                                   effects = list(spend_log = eff$spend, arrears_log = eff$arr, arrears_length = 0,
                                                  share_FastFood = 0, share_Alcohol = 0, share_Gambling = 0)), sim_args))
  p <- data.table::copy(sim$panel)
  if (drift > 0) { set.seed(1000L + seed); keep <- 1 - drift * p$wID / max(p$wID); p[, numarrears := rbinom(.N, numarrears, keep)] }
  saveRDS(p, file.path(root, "Processed", "analysis_panel.rds"))
  base <- spec_main("sample.n_lags" = 23L, "sc.parallel" = FALSE, "post.gibbs.n_iter" = 200L, "post.gibbs.burn_in" = 60L,
                    "post.outcomes" = outcomes, "post.f_Z" = "budgetdummy ~ 1")
  data.table::rbindlist(lapply(c(global = "global", perunit = "per_unit"), function(el) {
    design <- if (el == "global") "global" else "perunit"
    spec <- spec_modify(base, "sc.donor_eligibility" = el); spec$name <- paste0("grid_", design)
    run <- run_register(spec); run_sample(run); fit_sc(run); d <- sc_diagnostics(run)
    pn <- data.table::as.data.table(readRDS(file.path(run$dir, "panel.rds"))); scfit <- readRDS(file.path(run$dir, "sc_fit.rds"))
    fitted <- scfit$fit$treated_unit_ids
    t0 <- data.table::data.table(customer_id = fitted, t0 = scfit$treat_time[scfit$treated_idx])
    post <- merge(pn, t0, by = "customer_id")[wID >= t0 & wID <= t0 + spec$sample$n_leads]
    data.table::rbindlist(lapply(outcomes, function(o) {
      k <- if (o == "numarrears") eff$arr else eff$spend
      truth <- mean(post[[o]]) * (1 - exp(-k))                 # what adoption removed, on the units this design fits
      s1 <- gap_ate(run, o)
      q  <- function(r) { a <- rowMeans(extract_tau_draws(r$post, r$gdata_light)); c(mean(a), stats::quantile(a, c(0.025, 0.975))) }
      orig <- q(budgetsc:::post_backend_scmbayes(data.table::copy(pn), scfit, o, spec))
      gp   <- q(post_backend_gap(data.table::copy(pn), scfit, o, spec))
      sl <- d$pre_slope[outcome == o]
      data.table::data.table(drift = drift, effect = effect, seed = seed, outcome = o, design = design, truth = truth,
        n_fitted = length(fitted), pre_slope_t = sl$t, trend_implied_ate = sl$trend_implied_ate,
        estimator = paste0(c("stage1", "original", "gap"), "_", design),
        estimate = c(s1$ate, orig[1], gp[1]),
        lo = c(s1$ate - 1.96 * s1$se_across_units, orig[2], gp[2]), hi = c(s1$ate + 1.96 * s1$se_across_units, orig[3], gp[3]))
    }))
  }))
}

estimator_grid <- function(drifts = c(0, 0.25, 0.5), effects = c(FALSE, TRUE), seeds = 1:5,
                           n_treated = 100, n_later = 220, sim_args = list(), cores = 1,
                           out = "estimator_grid_results.csv") {
  cells <- expand.grid(drift = drifts, effect = effects, seed = seeds); n <- nrow(cells)
  message(n, " simulated datasets (two SC fits and twelve Gibbs fits each)")
  f <- function(i) { t0 <- Sys.time()
    r <- tryCatch(run_dataset(cells$drift[i], cells$effect[i], cells$seed[i], n_treated, n_later, sim_args),
                  error = function(e) { message("dataset ", i, " failed: ", conditionMessage(e)); NULL })
    message(sprintf("[%d/%d] drift %.2f, effect %s, seed %d (%.0f s)", i, n, cells$drift[i], cells$effect[i], cells$seed[i],
                    as.numeric(difftime(Sys.time(), t0, units = "secs")))); r }
  res <- data.table::rbindlist(if (cores > 1) parallel::mclapply(seq_len(n), f, mc.cores = cores) else lapply(seq_len(n), f))
  data.table::fwrite(res, out); message("raw results written to ", out)
  summ <- res[, .(bias = mean(estimate - truth), rmse = sqrt(mean((estimate - truth)^2)),
                  relative_bias = if (effect[1]) mean(estimate - truth) / abs(mean(truth)) else NA_real_,
                  interval_excludes_zero = mean(lo > 0 | hi < 0), interval_covers_truth = mean(lo <= truth & hi >= truth),
                  mean_treated_fitted = mean(n_fitted), datasets = .N),
              by = .(drift, effect, outcome, estimator)][order(outcome, effect, drift, estimator)]
  print(summ[, lapply(.SD, function(x) if (is.numeric(x)) signif(x, 3) else x)], row.names = FALSE)
  if (res[, uniqueN(seed)] >= 5) {
    cat("\nDoes the pre-adoption trend diagnostic predict the error? (per-unit design, Stage 1)\n")
    print(res[estimator == "stage1_perunit", .(cor_trend_implied_vs_error = signif(cor(trend_implied_ate, estimate - truth), 3),
                                              mean_abs_pre_slope_t = signif(mean(abs(pre_slope_t)), 3)), by = .(effect, outcome)], row.names = FALSE)
  }
  invisible(list(raw = res, summary = summ))
}
