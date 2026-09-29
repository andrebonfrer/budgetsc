test_that("sim_panel has the expected structure and respects role windows", {
  sim <- sim_panel(n_treated = 40, n_later = 60, n_pilot = 10, n_partial = 10, n_never = 20,
                   n_weeks = 156, seed = 3)
  p <- sim$panel; cu <- sim$customers
  expect_equal(nrow(p), 140 * 156)
  need <- c("customer_id", "week_start", "wID", "spend_Groceries", "spend_Eating.out",
            "irs_spend_FastFood", "irs_spend_CasinoGambling", "numarrears", "lengtharrears",
            "weekly_signins", "total_transactions", "weekly_income", "weekly_balance",
            "minBudgetDate", "onboarddate", "donor", "budgetcategoriesN", "frequency",
            "budget_to_spend_ratio_mean", "income_mean", "income_cv", "age_band", "gender",
            "state", "customer_tenure", "monthly_loan_payment_w")
  expect_true(all(need %in% names(p)))
  w_public <- event_week("public_launch"); w_trunc <- w_public + bsc_timeline()$horizon$n_leads
  expect_true(all(cu[role == "treated", adopt_wID >= w_public & adopt_wID <= w_trunc]))
  expect_true(all(cu[role == "later", adopt_wID > w_trunc]))
  expect_true(all(cu[role == "pilot", adopt_wID >= event_week("pilot_start") & adopt_wID < w_public]))
  expect_true(all(is.na(cu[role %in% c("partial", "never"), adopt_wID])))
  # minBudgetDate falls inside the adoption week
  expect_true(all(cu[!is.na(adopt_wID), week_of(minBudgetDate) == adopt_wID]))
  expect_equal(cu[role == "partial", donor][1], 1L)
  expect_equal(cu[role == "never", donor][1], 2L)
  expect_true(all(p[!is.na(minBudgetDate) & week_start >= minBudgetDate & week_start >= week_start(w_public), TRUE]))
})

test_that("dip_selection makes adoption follow a spending spike", {
  # with dip selection, the transitory shock u in the weeks before adoption is
  # positive on average; without it, it is centred on zero
  pre_u <- function(dip) {
    sim <- sim_panel(n_treated = 300, n_later = 50, n_never = 0, n_weeks = 130, phi = 0.9,
                     dip_selection = dip, type_selection = 0, keep_latent = TRUE, seed = 11)
    p <- sim$panel[!is.na(minBudgetDate)]
    p[, adopt := week_of(minBudgetDate)]
    p[wID >= adopt - 4 & wID < adopt, mean(u)]
  }
  expect_gt(pre_u(3), 0.3)
  expect_lt(abs(pre_u(0)), 0.1)
})

test_that("true effects show up in the treated post-period", {
  sim <- sim_panel(n_treated = 300, n_later = 100, n_never = 0, seed = 5,
                   effects = list(spend_log = -0.5, arrears_log = -1.5, arrears_length = -5,
                                  share_FastFood = 0.15, share_Alcohol = 0, share_Gambling = 0))
  p <- sim$panel[!is.na(minBudgetDate)]
  p[, post := week_start >= minBudgetDate]
  p[, spend := rowSums(.SD), .SDcols = patterns("^spend_")]
  # treated units: post-period spend and arrears lower than their own pre-period
  tr <- p[customer_id %in% sim$customers[role == "treated", customer_id]]
  expect_lt(tr[post == TRUE, mean(numarrears)], tr[post == FALSE, mean(numarrears)])
  ff <- tr[spend_Eating.out > 0, .(share = irs_spend_FastFood / spend_Eating.out, post)]
  expect_gt(ff[post == TRUE, mean(share)], ff[post == FALSE, mean(share)])
  # income is a placebo: no post/pre difference beyond noise
  d <- tr[, mean(weekly_income), by = post]
  expect_lt(abs(diff(d$V1)) / mean(d$V1), 0.05)
})
