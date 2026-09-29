sim_root <- function() {
  root <- tempfile(); dir.create(file.path(root, "Processed"), recursive = TRUE)
  root
}

test_that("load_panel builds wID from the timeline and asserts the origin", {
  root <- sim_root(); old <- options(budgetsc.root = root); on.exit(options(old), add = TRUE)
  sim <- sim_panel(n_treated = 30, n_later = 40, n_never = 10, seed = 2)
  saveRDS(sim$panel[, !"wID"], file.path(root, "Processed", "analysis_panel.rds"))
  p <- load_panel(spec_main())
  expect_equal(p[, unique(week_of(week_start) == wID)], TRUE)
  expect_equal(min(p$wID), 1L)
  expect_true(is.numeric(p$customer_tenure))
  expect_true(is.factor(p$state))
  expect_true(all(p$age %in% 1:7))
  expect_equal(nchar(attr(p, "panel_md5")), 32L)
  # a panel whose first week is not the origin is rejected
  bad <- sim$panel[wID > 3]
  saveRDS(bad, file.path(root, "Processed", "analysis_panel.rds"))
  expect_error(load_panel(spec_main()), "origin")
})

test_that("derive_outcomes produces the documented columns with sane values", {
  sim <- sim_panel(n_treated = 30, n_later = 40, n_never = 10, seed = 4)
  p <- data.table::copy(sim$panel)
  derive_outcomes(p)
  expect_true(all(match_outcomes_wide() %in% names(p)))
  expect_true(all(c("net_cashflow", "cf_vol", "income_share", "income_share_vol",
                    "liquidity_deficit", "liquidity_deficit_rate", "liquidity_buffer",
                    "liquidity_buffer_w", "cf_vol_w", "income_share_vol_w", "income_share_w",
                    "cv_2020_spend") %in% names(p)))
  expect_false(any(grepl("^spend_", names(p))))                       # renamed to Spend*
  expect_true(all(grepl("^irs_spend_", grep("irs_spend", names(p), value = TRUE))))
  # total_spend is the sum of the eleven focal (winsorised) categories
  focal <- paste0("Spend", FOCAL_SPEND_CATEGORIES)
  expect_equal(p$total_spend, rowSums(p[, ..focal]))
  expect_false("SpendCash" %in% focal)
  # shares in [0,1] where finite, zero where the parent is zero
  expect_true(all(p$walletshare_FastFood >= 0 & p$walletshare_FastFood <= 1))
  expect_true(all(p[SpendEating.out == 0, walletshare_FastFood] == 0))
  expect_true(all(p$liquidity_deficit %in% 0:1))
  expect_true(all(p$liquidity_deficit_rate >= 0 & p$liquidity_deficit_rate <= 1))
  expect_equal(p[wID < 8, unique(cf_vol)], 0)                           # rolling window not yet full
  expect_true(all(!is.na(p$cv_2020_spend)))
  # liquidity deficit compares weekly spend with TYPICAL weekly income
  expect_equal(p$liquidity_deficit, as.integer(p$total_spend > p$income_mean))
})

test_that("cv_basis changes cv_2020_spend as documented", {
  sim <- sim_panel(n_treated = 20, n_later = 20, n_never = 0, seed = 6)
  a <- derive_outcomes(data.table::copy(sim$panel), cv_basis = "all")
  b <- derive_outcomes(data.table::copy(sim$panel), cv_basis = "focal")
  expect_false(isTRUE(all.equal(a$cv_2020_spend, b$cv_2020_spend)))
})

test_that("derive_moderators adds splines, homeloan_mean and budgetever dummies", {
  sim <- sim_panel(n_treated = 30, n_later = 40, n_never = 10, seed = 8)
  p <- data.table::copy(sim$panel)
  derive_outcomes(p); derive_moderators(p)
  bs <- p[!is.na(budget_to_spend_ratio_mean)]
  expect_equal(bs$br_under + bs$br_target + bs$br_over, bs$budget_to_spend_ratio_mean)
  expect_true(all(bs$br_under <= 0.95 & bs$br_target <= 0.1 + 1e-12 & bs$br_over >= 0))
  expect_true("budgetever_Groceries" %in% names(p))
  expect_true(all(p$budgetever_Groceries %in% 0:1))
  expect_equal(p[, uniqueN(homeloan_mean), by = customer_id][, max(V1)], 1L)
  expect_true(all(p[total_transactions == 0, avg_transaction_size] == 0))
  expect_true(nrow(data_dictionary()) > 10)
})

test_that("spec: holdout outcomes cannot be matched; lean spec differs from wide", {
  expect_error(spec_main("sc.holdout_outcomes" = c("weekly_income", "total_spend")), "cannot serve as a placebo")
  expect_error(spec_main("sc.match_outcomes" = c("numarrears", "weekly_income")), "cannot serve as a placebo")
  expect_error(spec_main("sc.screen_outcome" = "weekly_income"), "screen_outcome")
  expect_setequal(spec_main()$sc$match_outcomes, match_outcomes_wide())
  expect_false(spec_id(spec_lean()) == spec_id(spec_main()))
  expect_true(all(match_outcomes_lean() %in% c("numarrears", "lengtharrears",
                                               "liquidity_deficit_rate", "income_share", "total_spend")))
  expect_equal(length(match_outcomes_wide()), 27L)
})
