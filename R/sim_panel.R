# sim_panel.R -----------------------------------------------------------------
# Synthetic weekly panel with the column names and structure of the real
# processed panel (BudgetPanelDataWeekly_with_donor*.rds plus the weekly income
# and balance columns the build step will merge in). Used for tests, for
# demonstrations, and as the data-generating process of the Ashenfelter-dip
# study.
#
# Data-generating process (per customer i, week t):
#   log total spend  m_it = a_i + lambda_i' f_t + covid_t + u_it + beta_spend * D_it
#       f_t     common factors, AR(1) over weeks (what synthetic controls match)
#       u_it    idiosyncratic transitory shock, AR(1) with persistence `phi`
#               (the "dip" lives here)
#       D_it    treatment indicator, 1 from the adoption week on
#   category spend   = total spend x customer-specific shares x noise
#   arrears          Poisson with log-rate  c0 + type_sel*S_i + 0.8*u_it + beta_arr*D_it
#       S_i     persistent financial-stress type (selection on type)
#   adoption week    drawn within the role's allowed window with probability
#                    proportional to exp(type_selection*S_i + dip_selection*u_{i,t-1})
#                    so `dip_selection > 0` makes adoption follow a spending spike
# Roles: pilot / treated (public launch) / later (donor: adopt after the
# treated window + H) / partial (onboarded, never set a budget) / never.

#' Simulate a weekly customer panel
#'
#' @param n_treated,n_later,n_pilot,n_partial,n_never Customers per role.
#' @param n_weeks Weeks in the panel (wID 1..n_weeks). Default 156.
#' @param n_factors Common factors. Default 3.
#' @param phi AR(1) persistence of the transitory shock `u`. 0 = white noise.
#' @param sigma_u SD of the innovation in `u`. Default 0.25.
#' @param type_selection Adoption-hazard loading on the persistent stress type.
#' @param dip_selection Adoption-hazard loading on last week's transitory shock.
#' @param effects Named list of true treatment effects: `spend_log` (log points
#'   on total spend), `arrears_log` (log points on the arrears rate),
#'   `arrears_length` (weeks, additive), `share_FastFood`, `share_Alcohol`,
#'   `share_Gambling` (additive on the within-parent share).
#' @param covid Logical; add a common negative spend shock in weeks 12–20 and
#'   a milder one in weeks 28–43 (Victoria-like). Default TRUE.
#' @param keep_latent Logical; keep the latent columns `u` (transitory shock),
#'   `lat` (log spend index) and `D` (treatment indicator) in the panel for
#'   diagnostics and the dip study. Default FALSE (real-data column set only).
#' @param seed Integer seed.
#' @return A list: `panel` (data.table, one row per customer-week), `customers`
#'   (one row per customer with role, latent type and adoption week), and
#'   `truth` (the parameters, for checking estimators).
#' @export
sim_panel <- function(n_treated = 300L, n_later = 400L, n_pilot = 0L,
                      n_partial = 0L, n_never = 200L, n_weeks = 156L,
                      n_factors = 3L, phi = 0.5, sigma_u = 0.25,
                      type_selection = 1, dip_selection = 0,
                      effects = list(spend_log = -0.05, arrears_log = -0.30,
                                     arrears_length = -5, share_FastFood = 0.02,
                                     share_Alcohol = -0.02, share_Gambling = -0.01),
                      covid = TRUE, keep_latent = FALSE, seed = 1L) {

  set.seed(seed)
  tl <- bsc_timeline()
  w_pilot  <- event_week("pilot_start", tl = tl)
  w_public <- event_week("public_launch", tl = tl)
  H        <- tl$horizon$n_leads
  w_trunc  <- w_public + H
  if (n_weeks <= w_trunc + 1L) stop("n_weeks must exceed public_launch week + n_leads + 1.", call. = FALSE)

  # ---- customers ------------------------------------------------------------
  roles <- c(rep("pilot", n_pilot), rep("treated", n_treated), rep("later", n_later),
             rep("partial", n_partial), rep("never", n_never))
  N <- length(roles)
  cust <- data.table::data.table(
    customer_id = seq_len(N) + 100000L,
    role        = roles,
    S           = stats::rnorm(N),                       # stress type
    O           = stats::rnorm(N),                       # planner type
    a           = stats::rnorm(N, log(800), 0.4),        # baseline log weekly spend
    income_mean = exp(stats::rnorm(N, log(1200), 0.35)),
    income_cv   = stats::runif(N, 0.05, 0.5),
    has_loan    = stats::rbinom(N, 1, 0.4),
    gender      = sample(c("M", "F"), N, TRUE),
    state       = sample(c("NSW", "VIC", "QLD", "WA", "SA", "TAS", "ACT", "NT"), N, TRUE,
                         prob = c(.32, .26, .2, .1, .07, .02, .02, .01)),
    country     = "AU",
    age_k       = sample(1:7, N, TRUE, prob = c(.05, .2, .25, .2, .15, .1, .05))
  )
  cust[, age_band := paste0("Age Band ", age_k, " : ", 15 + 10 * age_k, "-", 24 + 10 * age_k)]
  cust[, customer_tenure := as.character(pmax(0L, round(stats::rnorm(N, 8, 5))))]
  cust[sample(N, ceiling(0.05 * N)), customer_tenure := "N/A"]
  cust[, income_sd := income_mean * income_cv]
  cust[, monthly_loan_payment_w := has_loan * exp(stats::rnorm(N, log(1500), 0.4))]
  cust[, donor := data.table::fcase(role %in% c("pilot", "treated", "later"), 0L,
                                    role == "partial", 1L, default = 2L)]
  lam <- matrix(stats::rnorm(N * n_factors, 0, 0.3), N, n_factors)

  # ---- common time components ------------------------------------------------
  f <- matrix(0, n_weeks, n_factors)
  for (t in 2:n_weeks) f[t, ] <- 0.8 * f[t - 1, ] + stats::rnorm(n_factors, 0, 0.15)
  covid_t <- numeric(n_weeks)
  if (covid) { covid_t[12:20] <- -0.30; covid_t[28:43] <- -0.15 }

  # ---- idiosyncratic transitory shock u (N x T, AR(1)) ---------------------------
  u <- matrix(0, N, n_weeks)
  u[, 1] <- stats::rnorm(N, 0, sigma_u / sqrt(max(1 - phi^2, 0.05)))
  for (t in 2:n_weeks) u[, t] <- phi * u[, t - 1] + stats::rnorm(N, 0, sigma_u)

  # ---- adoption weeks ------------------------------------------------------------
  window <- list(pilot   = c(w_pilot, w_public - 1L),
                 treated = c(w_public, w_trunc),
                 later   = c(w_trunc + 1L, n_weeks - 1L))
  cust[, adopt_wID := NA_integer_]
  for (r in names(window)) {
    idx <- which(cust$role == r)
    if (!length(idx)) next
    wk <- seq(window[[r]][1], window[[r]][2])
    # hazard proportional to exp(type_selection*S + dip_selection*u_{t-1})
    lin <- type_selection * cust$S[idx] %o% rep(1, length(wk)) +
           dip_selection * u[idx, wk - 1L, drop = FALSE]
    p <- exp(lin); p <- p / rowSums(p)
    cust$adopt_wID[idx] <- vapply(seq_along(idx), function(k) sample(wk, 1, prob = p[k, ]), integer(1))
  }
  cust[, minBudgetDate := as.Date(NA)]
  cust[!is.na(adopt_wID), minBudgetDate := week_start(adopt_wID, tl$origin) + sample(0:6, .N, TRUE)]
  cust[, onboarddate := as.Date(NA)]
  cust[!is.na(adopt_wID), onboarddate := minBudgetDate - sample(0:14, .N, TRUE)]
  cust[role == "partial", onboarddate := week_start(sample(seq(w_public, n_weeks - 1L), .N, TRUE), tl$origin)]
  cust[, official_launch := as.integer(!is.na(minBudgetDate) & minBudgetDate >= tl$events$public_launch)]

  # ---- budget configuration (budget setters only) ---------------------------------
  cust[, `:=`(budgetcategoriesN = NA_integer_, numCats = NA_integer_, frequency = NA_real_,
              budget_to_spend_ratio_mean = NA_real_, numgoalcats = 0L, totgoal = 0, avggoal = 0,
              totWeeklyBudgetAmount = NA_real_, budgetN = NA_real_, numBudgetDates = NA_integer_)]
  bs <- which(!is.na(cust$adopt_wID))
  if (length(bs)) {
    cust[bs, budgetcategoriesN := 1L + stats::rpois(.N, 2)]
    cust[bs, numCats := budgetcategoriesN]
    cust[bs, frequency := sample(c(7, 14, 31), .N, TRUE, prob = c(.3, .3, .4))]
    cust[bs, budget_to_spend_ratio_mean := exp(stats::rnorm(.N, -0.1 * S, 0.25))]  # stressed set tighter budgets
    cust[bs, numgoalcats := stats::rpois(.N, 0.5)]
    cust[bs, totgoal := numgoalcats * exp(stats::rnorm(.N, log(2000), 0.5))]
    cust[bs, avggoal := ifelse(numgoalcats > 0, totgoal / numgoalcats, 0)]
    cust[bs, totWeeklyBudgetAmount := budget_to_spend_ratio_mean * exp(a)]
    cust[bs, budgetN := 1 + stats::rpois(.N, 1)]
    cust[bs, numBudgetDates := as.integer(budgetN)]
  }
  cats <- c("Cash", "Donations", "Eating.out", "Education", "Entertainment", "Groceries",
            "Health", "Home", "Shopping", "Transport", "Travel", "Utilities")
  for (cc in c("Groceries", "Eating.out", "Shopping", "Entertainment")) {
    col <- paste0("minBudgetDate_", cc)
    cust[, (col) := as.Date(NA)]
    cust[bs[stats::rbinom(length(bs), 1, 0.6) == 1], (col) := minBudgetDate]
  }

  # ---- weekly panel ------------------------------------------------------------------
  panel <- data.table::CJ(customer_id = cust$customer_id, wID = seq_len(n_weeks))
  panel[, week_start := week_start(wID, tl$origin)]
  ci <- match(panel$customer_id, cust$customer_id)
  panel[, D := as.integer(!is.na(cust$adopt_wID[ci]) & wID >= cust$adopt_wID[ci])]
  panel[, u := u[cbind(ci, wID)]]
  panel[, lat := cust$a[ci] + rowSums(lam[ci, , drop = FALSE] * f[wID, , drop = FALSE]) +
                 covid_t[wID] + u + effects$spend_log * D]
  panel[, total_spend_true := exp(lat)]

  # category shares per customer (Dirichlet via gamma), weekly noise, zero inflation
  alpha <- c(Cash = 1, Donations = 0.2, Eating.out = 2, Education = 0.3, Entertainment = 1.2,
             Groceries = 4, Health = 0.8, Home = 2.5, Shopping = 2.5, Transport = 1.5,
             Travel = 0.4, Utilities = 1.5)
  g <- matrix(stats::rgamma(N * length(cats), shape = rep(alpha, each = N)), N, length(cats))
  shares <- g / rowSums(g)
  colnames(shares) <- cats
  zero_p <- c(Cash = .3, Donations = .85, Eating.out = .05, Education = .8, Entertainment = .2,
              Groceries = .02, Health = .5, Home = .1, Shopping = .05, Transport = .1,
              Travel = .85, Utilities = .4)
  for (cc in cats) {
    col <- paste0("spend_", cc)
    val <- panel$total_spend_true * shares[ci, cc] * exp(stats::rnorm(nrow(panel), 0, 0.3))
    val[stats::runif(nrow(panel)) < zero_p[[cc]]] <- 0
    panel[, (col) := round(val, 2)]
  }
  # IRS sub-categories as shares of their parent category (with treatment effects on shares)
  sub_share <- function(parent, base, sd = 0.08, eff = 0) {
    s <- pmin(pmax(stats::rnorm(nrow(panel), base, sd) + eff * panel$D, 0), 1)
    round(panel[[paste0("spend_", parent)]] * s, 2)
  }
  panel[, irs_spend_SuperMarket       := sub_share("Groceries", 0.7)]
  panel[, irs_spend_FastFood          := sub_share("Eating.out", 0.35, eff = effects$share_FastFood)]
  panel[, irs_spend_Alcohol           := sub_share("Shopping", 0.08, sd = 0.04, eff = effects$share_Alcohol)]
  panel[, irs_spend_Tobacco           := sub_share("Shopping", 0.03, sd = 0.03)]
  panel[, irs_spend_DiscountStores    := sub_share("Shopping", 0.15)]
  panel[, irs_spend_AlcoholOut        := sub_share("Entertainment", 0.2)]
  panel[, irs_spend_CasinoGambling    := sub_share("Entertainment", 0.05, sd = 0.04, eff = effects$share_Gambling)]
  panel[, irs_spend_Childcare         := sub_share("Education", 0.5, sd = 0.15)]
  panel[, irs_spend_HealthMaintenance := sub_share("Health", 0.3)]

  # activity counts
  panel[, total_transactions     := stats::rpois(.N, 12 * exp(0.5 * (lat - cust$a[ci])))]
  panel[, daily_avg_transactions := round(total_transactions / 7, 3)]
  panel[, total_irs_transactions := round(stats::rpois(.N, 4) / 7, 3)]
  panel[, total_categories       := round(rowSums(panel[, paste0("spend_", cats), with = FALSE] > 0) / 7, 3)]
  panel[, avg_transaction        := ifelse(total_transactions > 0, round(total_spend_true / total_transactions, 2), 0)]
  panel[, weekly_signins         := stats::rpois(.N, exp(1 + 0.3 * cust$O[ci] + 0.4 * D))]  # engagement rises with adoption

  # arrears (loan holders only): stress type + transitory overspending + treatment
  z <- -2.5 + type_selection * cust$S[ci] + 0.8 * panel$u + effects$arrears_log * panel$D
  panel[, numarrears := cust$has_loan[ci] * stats::rpois(.N, exp(z))]
  panel[, lengtharrears := ifelse(numarrears > 0,
                                  pmax(7, 18 + stats::rexp(.N, 1 / 30) + effects$arrears_length * D), 0)]
  panel[, lengtharrears := round(lengtharrears, 1)]

  # income (placebo outcome: unaffected by adoption) and balance
  panel[, weekly_income := round(cust$income_mean[ci] * exp(stats::rnorm(.N, 0, cust$income_cv[ci])), 2)]
  panel[, net_cf := weekly_income - total_spend_true]
  panel[, weekly_balance := round(2000 + cumsum(net_cf), 2), by = customer_id]

  # ---- attach customer-level columns (as in the real panel) -----------------------------
  keep_cust <- c("customer_id", "donor", "official_launch", "minBudgetDate", "onboarddate",
                 "budgetcategoriesN", "numCats", "frequency", "budget_to_spend_ratio_mean",
                 "numgoalcats", "totgoal", "avggoal", "totWeeklyBudgetAmount", "budgetN",
                 "numBudgetDates", "income_mean", "income_sd", "income_cv",
                 "monthly_loan_payment_w", "age_band", "gender", "state", "country",
                 "customer_tenure", grep("^minBudgetDate_", names(cust), value = TRUE))
  panel <- merge(panel, cust[, keep_cust, with = FALSE], by = "customer_id", all.x = TRUE)
  drop <- c("total_spend_true", "net_cf", if (!keep_latent) c("D", "u", "lat"))
  panel[, (drop) := NULL]
  data.table::setorder(panel, customer_id, wID)

  truth <- list(effects = effects, phi = phi, sigma_u = sigma_u,
                type_selection = type_selection, dip_selection = dip_selection,
                windows = window, n_weeks = n_weeks, seed = seed,
                covid = covid, n_factors = n_factors)
  list(panel = panel, customers = cust, truth = truth)
}
