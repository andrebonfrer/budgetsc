test_that("build_analysis_panel aligns income to the containing week and can reproduce the legacy shift", {
  root <- tempfile(); dir.create(file.path(root, "Processed"), recursive = TRUE); dir.create(file.path(root, "data"))
  old <- options(budgetsc.root = root); on.exit(options(old), add = TRUE)
  sim <- sim_panel(n_treated = 20, n_later = 20, n_never = 5, seed = 31)
  base <- sim$panel[, !c("weekly_income", "weekly_balance")]          # the processed panel lacks both
  saveRDS(base, file.path(root, "Processed", "BudgetPanelDataWeekly_with_donor1.rds"))
  # salary transactions: one payment per week, dated on a random weekday of that week
  set.seed(1)
  inc <- sim$panel[weekly_income > 0, .(customer_id, payt_d = week_start + sample(0:6, .N, TRUE), payt_a = weekly_income)]
  saveRDS(inc, file.path(root, "data", "commincome_fncl_tran_slry.rds"))
  saveRDS(sim$panel[, .(customer_id, wID, weekly_balance)], file.path(root, "Processed", "weekly_balances_without_homeloans.rds"))

  p <- build_analysis_panel("Processed/BudgetPanelDataWeekly_with_donor1.rds")
  expect_true(file.exists(file.path(root, "Processed", "analysis_panel.rds")))
  m <- merge(p[, .(customer_id, wID, weekly_income)], sim$panel[, .(customer_id, wID, truth = weekly_income)], by = c("customer_id", "wID"))
  expect_equal(m$weekly_income, m$truth)                                    # containing week: exact
  expect_equal(p$weekly_balance, sim$panel$weekly_balance)
  expect_true(!is.null(attr(p, "build_info")))

  p2 <- build_analysis_panel("Processed/BudgetPanelDataWeekly_with_donor1.rds", week_alignment = "legacy_shift",
                             out_file = "Processed/analysis_panel_legacy.rds")
  m2 <- merge(p2[, .(customer_id, wID, weekly_income)], sim$panel[, .(customer_id, wID, truth = weekly_income)], by = c("customer_id", "wID"))
  expect_false(isTRUE(all.equal(m2$weekly_income, m2$truth)))              # shifted by a week
  # legacy: income of week w appears in row w-1
  m3 <- merge(p2[, .(customer_id, wID, weekly_income)], sim$panel[, .(customer_id, wID = wID - 1L, truth = weekly_income)], by = c("customer_id", "wID"))
  expect_equal(m3$weekly_income, m3$truth)
})
