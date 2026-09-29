# End-to-end on the synthetic panel with the stub backends.
setup_root <- function(seed = 7, ...) {
  root <- tempfile(); dir.create(file.path(root, "Processed"), recursive = TRUE)
  options(budgetsc.root = root)
  sim <- sim_panel(seed = seed, ...)
  saveRDS(sim$panel, file.path(root, "Processed", "analysis_panel.rds"))
  list(root = root, sim = sim)
}
stub_spec <- function(...) spec_main("sc.backend" = "stub", "post.backend" = "stub",
                                     "post.gibbs.n_iter" = 120L, "post.gibbs.burn_in" = 20L, ...)

test_that("define_sample: later_adopters public cohort matches the script logic", {
  s <- setup_root(n_treated = 60, n_later = 80, n_pilot = 15, n_partial = 20, n_never = 20)
  old <- options(); on.exit(options(old), add = TRUE)
  spec <- stub_spec("sample.n_lags" = 23L)
  p <- load_panel(spec); derive_outcomes(p); derive_moderators(p)
  smp <- define_sample(p, spec)
  cu <- s$sim$customers
  expect_s3_class(smp, "bsc_sample")
  # pilot adopters excluded entirely; partial/never (donor > 0) excluded; later adopters are donors
  expect_false(any(cu[role == "pilot", customer_id] %in% smp$ids$customer_id))
  expect_false(any(cu[role %in% c("partial", "never"), customer_id] %in% smp$ids$customer_id))
  expect_true(all(smp$ids[role == "donor", customer_id] %in% cu[role == "later", customer_id]))
  expect_true(all(smp$ids[role == "treated", customer_id] %in% cu[role == "treated", customer_id]))
  expect_equal(smp$window$launch_wID, event_week("public_launch"))
  expect_equal(smp$window$truncate_wID, event_week("public_launch") + 40L)
  # budgetdummy switches on at minBudgetDate for treated, never for donors
  pp <- smp$panel
  expect_true(all(pp[customer_id %in% smp$ids[role == "donor", customer_id], budgetdummy] == 0L))
  tr1 <- smp$ids[role == "treated"][1]
  expect_equal(pp[customer_id == tr1$customer_id & wID == tr1$treat_wID, budgetdummy], 1L)
  expect_equal(pp[customer_id == tr1$customer_id & wID == tr1$treat_wID - 1L, budgetdummy], 0L)
  expect_true(all(c("panel", "budget setters only") %in% smp$funnel$step))
  expect_true(all(diff(smp$funnel$n_customers) <= 0))
})

test_that("define_sample: onboarder pools with stratified sampling", {
  s <- setup_root(n_treated = 60, n_later = 30, n_pilot = 0, n_partial = 150, n_never = 150, seed = 3)
  old <- options(); on.exit(options(old), add = TRUE)
  spec <- stub_spec("sample.design" = "partial_onboarders", "sample.n_lags" = 23L,
                    "sample.donor_sampling" = list(method = "stratified", target = 60L, seed = 1L))
  p <- load_panel(spec); derive_outcomes(p); derive_moderators(p)
  smp <- define_sample(p, spec)
  cu <- s$sim$customers
  expect_true(all(smp$ids[role == "donor", customer_id] %in% cu[role == "partial", customer_id]))
  expect_lte(sum(smp$ids$role == "donor"), 60L)
  spec2 <- spec_modify(spec, "sample.design" = "non_onboarders")
  smp2 <- define_sample(p, spec2)
  expect_true(all(smp2$ids[role == "donor", customer_id] %in% cu[role == "never", customer_id]))
})

test_that("full pipeline with stub backends recovers effect signs and writes tables", {
  s <- setup_root(n_treated = 60, n_later = 80, n_never = 0, seed = 5,
                  effects = list(spend_log = -0.4, arrears_log = -1.5, arrears_length = -8,
                                 share_FastFood = 0.1, share_Alcohol = 0, share_Gambling = 0))
  old <- options(); on.exit(options(old), add = TRUE)
  spec <- stub_spec("sample.n_lags" = 23L, "post.outcomes" = c("numarrears", "lengtharrears", "total_spend", "weekly_income"))
  run <- run_pipeline(spec)
  expect_true(stage_done(run, "sample")); expect_true(stage_done(run, "sc"))
  expect_true(stage_done(run, "sc_diag")); expect_true(stage_done(run, "post")); expect_true(stage_done(run, "summary"))
  scfit <- readRDS(file.path(run$dir, "sc_fit.rds"))
  expect_equal(dim(scfit$fit$tau)[2], length(spec$sc$match_outcomes))
  expect_equal(dim(scfit$fit$tau)[3], 41L)
  expect_equal(unname(rowSums(scfit$fit$weights_mat)), rep(1, nrow(scfit$fit$weights_mat)), tolerance = 1e-8)
  ot <- data.table::fread(file.path(run$dir, "tables", "outcome_table.csv"))
  expect_setequal(ot$outcome, spec$post$outcomes)
  expect_lt(ot[outcome == "numarrears", ate_mean], 0)
  expect_lt(ot[outcome == "total_spend", ate_mean], 0)
  expect_lt(abs(ot[outcome == "weekly_income", ate_mean]) / mean(s$sim$customers$income_mean), 0.03)  # placebo ~ 0
  expect_true(file.exists(file.path(run$dir, "tables", "outcome_table.tex")))
  expect_true(file.exists(file.path(run$dir, "tables", "gamma_table.csv")))
  expect_true(file.exists(file.path(run$dir, "tables", "unit_effects.csv")))
  expect_true(file.exists(file.path(run$dir, "sc_diag", "pretrend.csv")))
  diag <- data.table::fread(file.path(run$dir, "sc_diag", "fit_summary.csv"))
  expect_true(all(c("matched_pre", "post") %in% diag$window))
  expect_true("weekly_income" %in% diag$outcome)                    # holdout outcome diagnosed, not matched
  expect_false(diag[outcome == "weekly_income", matched][1])
  cmp <- compare_runs(run$id)
  expect_equal(nrow(cmp), length(spec$post$outcomes))
  expect_equal(run_status()$post_done, length(spec$post$outcomes))
})

test_that("match_end holds out pre-weeks: fit shortens L and diagnostics label the window", {
  s <- setup_root(n_treated = 40, n_later = 60, n_never = 0, seed = 9)
  old <- options(); on.exit(options(old), add = TRUE)
  spec <- stub_spec("sample.n_lags" = 23L, "sc.match_end" = 4L, "post.outcomes" = "numarrears")
  run <- run_register(spec); run_sample(run); fit_sc(run); d <- sc_diagnostics(run)
  scfit <- readRDS(file.path(run$dir, "sc_fit.rds"))
  expect_equal(scfit$fit$L, 19L); expect_equal(scfit$fit$K, 44L)
  expect_equal(d$pretrend[outcome == "numarrears" & tau_k %in% -4:-1, unique(window)], "heldout_pre")
  et <- sc_event_time_ate(scfit)
  expect_equal(min(et$tau_k), -4L)
})

test_that("worker serves sc and post jobs with the default job function", {
  s <- setup_root(n_treated = 40, n_later = 60, n_never = 0, seed = 12)
  old <- options(); on.exit(options(old), add = TRUE)
  run <- run_register(stub_spec("sample.n_lags" = 23L, "post.outcomes" = c("numarrears", "income_share")))
  out <- worker(job_fun_default(), stages = c("sc", "post"), once = TRUE)
  expect_true(all(out$ok)); expect_equal(nrow(out), 3L)
  expect_true(stage_done(run, "post"))
})
