# Runs only where augMultiSynth and scmBayesPost are installed (VM, or a dev
# machine with both packages). Small sizes: ~1 minute.
test_that("real backends recover effect signs on the synthetic panel", {
  skip_if_not_installed("augMultiSynth"); skip_if_not_installed("scmBayesPost")
  skip_on_cran()
  root <- tempfile(); dir.create(file.path(root, "Processed"), recursive = TRUE)
  old <- options(budgetsc.root = root); on.exit(options(old), add = TRUE)
  sim <- sim_panel(n_treated = 30, n_later = 60, n_never = 0, seed = 5,
                   effects = list(spend_log = -0.4, arrears_log = -1.5, arrears_length = -8,
                                  share_FastFood = 0.1, share_Alcohol = 0, share_Gambling = 0))
  saveRDS(sim$panel, file.path(root, "Processed", "analysis_panel.rds"))
  spec <- spec_main("sample.n_lags" = 23L, "post.gibbs.n_iter" = 120L, "post.gibbs.burn_in" = 40L,
                    "post.outcomes" = c("numarrears", "total_spend", "weekly_income"), "sc.parallel" = FALSE)
  run <- run_pipeline(spec)
  sc <- readRDS(file.path(run$dir, "sc_fit.rds"))$fit
  expect_true(all(c("treated_unit_ids", "donor_ids", "weights", "weights_mat", "tau") %in% names(sc)))
  expect_equal(unname(rowSums(sc$weights_mat)), rep(1, nrow(sc$weights_mat)), tolerance = 1e-6)
  ot <- data.table::fread(file.path(run$dir, "tables", "outcome_table.csv"))
  expect_lt(ot[outcome == "numarrears", ate_mean], 0)
  expect_lt(ot[outcome == "total_spend", ate_mean], 0)
  expect_gt(ot[outcome == "numarrears", pct_sig_neg], 60)
  # placebo: unit-level significance split roughly evenly rather than one-sided
  pl <- ot[outcome == "weekly_income"]
  expect_lt(abs(pl$pct_sig_pos - pl$pct_sig_neg), 40)
  gt <- data.table::fread(file.path(run$dir, "tables", "gamma_table.csv"))
  expect_true(all(c("q_bh", "family") %in% names(gt)))
})
