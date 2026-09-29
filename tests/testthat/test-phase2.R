setup_root2 <- function(seed = 7, ...) {
  root <- tempfile(); dir.create(file.path(root, "Processed"), recursive = TRUE)
  options(budgetsc.root = root)
  sim <- sim_panel(seed = seed, ...)
  saveRDS(sim$panel, file.path(root, "Processed", "analysis_panel.rds"))
  list(root = root, sim = sim)
}
stub2 <- function(...) spec_main("sc.backend" = "stub", "post.backend" = "stub",
                                 "post.gibbs.n_iter" = 100L, "post.gibbs.burn_in" = 20L, ...)

test_that("placebo shift moves onsets back and keeps the post window before real adoption", {
  s <- setup_root2(n_treated = 60, n_later = 80, n_never = 0, seed = 21, n_weeks = 200)
  old <- options(); on.exit(options(old), add = TRUE)
  base <- stub2("sample.n_lags" = 23L)
  spec <- spec_placebo_shift(base)                         # shift = H + 1 = 41
  expect_equal(spec$sample$design, "placebo_dates"); expect_equal(spec$sample$placebo$shift, 41L)
  p <- load_panel(spec); derive_outcomes(p); derive_moderators(p)
  smp <- define_sample(p, spec)
  cu <- s$sim$customers
  m <- merge(smp$ids[role == "treated"], cu[, .(customer_id, adopt_wID)], by = "customer_id")
  expect_true(all(m$treat_wID == m$adopt_wID - 41L))
  expect_true(all(m$treat_wID + 40L < m$adopt_wID))
  expect_error(spec_placebo_shift(base, shift = 10L) |> (\(x) define_sample(p, x))(), "exceed")
  # runs through the pipeline: an effect-free window should give a near-zero ATE on spend
  run <- run_pipeline(spec_modify(spec, "post.outcomes" = "total_spend"))
  ot <- data.table::fread(file.path(run$dir, "tables", "outcome_table.csv"))
  expect_lt(abs(ot$ate_mean) / 800, 0.1)
})

test_that("placebo on never-onboarders assigns pseudo onsets from real adopters", {
  s <- setup_root2(n_treated = 40, n_later = 60, n_never = 80, seed = 22)
  old <- options(); on.exit(options(old), add = TRUE)
  spec <- spec_placebo_never(stub2("sample.n_lags" = 23L), n = 30L, seed = 3L)
  p <- load_panel(spec); derive_outcomes(p); derive_moderators(p)
  smp <- define_sample(p, spec)
  cu <- s$sim$customers
  expect_true(all(smp$ids[role == "treated", customer_id] %in% cu[role == "never", customer_id]))
  expect_false(any(cu[role == "treated", customer_id] %in% smp$ids$customer_id))
  expect_true(all(smp$ids[role == "treated", treat_wID] %in% cu[role == "treated", adopt_wID]))
  expect_lte(sum(smp$ids$role == "treated"), 30L)
  expect_equal(smp$placebo$units, "never_onboarders")
})

test_that("distress hook in run_sample and subset selection work", {
  s <- setup_root2(n_treated = 60, n_later = 80, n_never = 0, seed = 23, phi = 0.9, type_selection = 2)
  old <- options(); on.exit(options(old), add = TRUE)
  spec <- stub2("sample.n_lags" = 23L,
                "derive.distress" = list(shock_vars = "numarrears", TS = 3, rule = "any", out_col = "distress"),
                "sample.subset" = list(var = "distress", value = 1L), "post.outcomes" = "numarrears")
  run <- run_register(spec); smp <- run_sample(run)
  expect_true("distress" %in% names(smp$panel))
  tr <- smp$panel[customer_id %in% smp$ids[role == "treated", customer_id], unique(distress)]
  expect_equal(tr, 1L)
  expect_true(any(grepl("treated subset distress", smp$funnel$step)))
})

test_that("held-out validation and DV correlations produce tables", {
  s <- setup_root2(n_treated = 40, n_later = 60, n_never = 0, seed = 24)
  old <- options(); on.exit(options(old), add = TRUE)
  spec <- stub2("sample.n_lags" = 23L, "sc.match_end" = 4L, "post.outcomes" = "numarrears")
  run <- run_register(spec); run_sample(run); fit_sc(run); sc_diagnostics(run)
  expect_true(file.exists(file.path(run$dir, "sc_diag", "unit_gaps.csv")))
  hv <- an_heldout_validation(run)
  expect_true(all(c("mean_heldout_gap", "p", "rmspe_ratio_heldout", "matched") %in% names(hv)))
  expect_true("weekly_income" %in% hv$outcome)
  expect_false(hv[outcome == "weekly_income", matched])
  cr <- an_dv_correlations(run)
  expect_equal(dim(cr$customer), c(4, 4))
  expect_true(file.exists(file.path(run$dir, "tables", "dv_correlations.csv")))
  expect_error(an_heldout_validation(run_register(stub2("sample.n_lags" = 23L))), "match_end")
})

test_that("placebo table stacks real and placebo runs", {
  s <- setup_root2(n_treated = 40, n_later = 60, n_never = 0, seed = 25, n_weeks = 200)
  old <- options(); on.exit(options(old), add = TRUE)
  real <- run_pipeline(stub2("sample.n_lags" = 23L, "post.outcomes" = "numarrears"))
  pl <- run_pipeline(spec_placebo_shift(stub2("sample.n_lags" = 23L, "post.outcomes" = "numarrears")))
  pt <- an_placebo_table(real$id, pl$id)
  expect_equal(nrow(pt), 2L)
  expect_equal(sort(pt$placebo), c(FALSE, TRUE))
})
