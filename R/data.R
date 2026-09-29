# data.R ----------------------------------------------------------------------
# Tier B of the data layer: everything computed FROM the processed panel, per
# run, in memory. Nothing here reads data/*.rds. Definitions are copied from
# runSynth_augMultiSynth_budgetsetters.R (wallet shares, focal spend,
# total_spend, cv_2020_spend) and post_reg_augMultisynth_budgetsetters.R
# (cash-flow family, moderators), with three departures noted inline:
#   1. focal spend categories are selected BY NAME (the script used positional
#      indices into a grep, which silently changes if columns are reordered);
#   2. cv_2020_spend has an explicit `cv_basis` argument because the script
#      computed it from ALL spend categories before re-defining total_spend as
#      focal-only (default "all" reproduces the script);
#   3. every derived column is documented in data_dictionary().

#' The eleven focal spending categories
#'
#' Confirmed 30 Sep 2026. `Cash`, `Gambling`, `Money.transfers`, `Tax.paid`,
#' `Uncategorised` and `X.n` exist in the panel but are excluded from
#' `total_spend` and from matching. Selected by name, not position.
#' @export
FOCAL_SPEND_CATEGORIES <- c("Donations", "Eating.out", "Education", "Entertainment",
                            "Groceries", "Health", "Home", "Shopping", "Transport",
                            "Travel", "Utilities")

#' Wallet-share definitions: numerator (IRS sub-category) / parent category
#' @export
WALLET_SHARES <- list(
  Supermarket       = c("irs_spend_SuperMarket",       "spend_Groceries"),
  FastFood          = c("irs_spend_FastFood",          "spend_Eating.out"),
  HealthMaintenance = c("irs_spend_HealthMaintenance", "spend_Health"),
  Childcare         = c("irs_spend_Childcare",         "spend_Education"),
  CasinoGambling    = c("irs_spend_CasinoGambling",    "spend_Entertainment"),
  Alcohol           = c("irs_spend_Alcohol",           "spend_Shopping"),
  AlcoholOut        = c("irs_spend_AlcoholOut",        "spend_Entertainment"),
  Tobacco           = c("irs_spend_Tobacco",           "spend_Shopping")
)

#' Load the processed analysis panel for a run
#'
#' Reads `spec$data$panel_file` (relative paths resolve against
#' [bsc_root()]), restricts to `first_date`–`cutoff`, builds `tdate` and
#' `wID` from the timeline, and applies the standard recodes from the
#' original scripts (age from `age_band`, `customer_tenure` `"N/A"` to 0,
#' factors for `state` and `gender`).
#'
#' The week index is asserted, not inferred: the earliest `week_start` after
#' the `first_date` filter must equal the timeline origin, otherwise the panel
#' and the timeline disagree and the function stops.
#'
#' @param spec A `bsc_spec`.
#' @param root Project root; see [bsc_root()].
#' @return A data.table with one row per customer-week.
#' @export
load_panel <- function(spec, root = NULL) {
  f <- spec$data$panel_file
  if (!grepl("^(/|[A-Za-z]:)", f)) f <- file.path(bsc_root(root), f)
  if (!file.exists(f)) stop("Panel file not found: ", f, call. = FALSE)
  p <- data.table::as.data.table(readRDS(f))
  need <- c("customer_id", "week_start")
  miss <- setdiff(need, names(p))
  if (length(miss)) stop("Panel lacks column(s): ", paste(miss, collapse = ", "), call. = FALSE)

  tl <- bsc_timeline()
  p[, week_start := as.Date(week_start)]
  p <- p[week_start >= as.Date(spec$data$first_date) & week_start <= as.Date(spec$data$cutoff)]
  first <- min(p$week_start)
  if (first != tl$origin)
    stop(sprintf("Panel origin %s differs from timeline origin %s; fix timeline.yml or the panel.",
                 first, tl$origin), call. = FALSE)
  p[, tdate := week_start]
  p[, wID := week_of(week_start, tl$origin)]

  # recodes from runSynth
  if ("age_band" %in% names(p))
    p[, age := as.integer(sub("Age Band ([0-9]+) :.*", "\\1", age_band))]
  if ("customer_tenure" %in% names(p)) {
    p[customer_tenure == "N/A", customer_tenure := "0"]
    p[, customer_tenure := as.numeric(customer_tenure)]
  }
  for (v in intersect(c("state", "gender"), names(p))) p[, (v) := as.factor(get(v))]
  data.table::setorder(p, customer_id, wID)
  # setattr, not attr<-: the latter copies and breaks by-reference updates downstream
  data.table::setattr(p, "panel_file", f)
  data.table::setattr(p, "panel_md5", tools::md5sum(f)[[1]])
  p
}

# Guard for by-reference functions: a data.table read from disk (readRDS) has
# no over-allocation, so := would silently work on a copy the caller never sees.
.check_by_ref <- function(p, fn) {
  if (!data.table::is.data.table(p) || data.table::truelength(p) == 0L)
    stop(fn, ": `p` must be an over-allocated data.table (from load_panel(), or run ",
         "data.table::setalloccol(p) after readRDS).", call. = FALSE)
  invisible(TRUE)
}

#' Derive outcome variables from the panel
#'
#' Adds, in this order: the eight `walletshare_*` ratios, `spend_Alcohol`
#' (in-store + out) and `spend_Tobacco`, zero-filling of `spend_*` and
#' `walletshare_*`, `cv_2020_spend`, the `spend_` → `Spend` rename, winsorised
#' focal `Spend*` columns, `total_spend` (sum of focal categories), and the
#' cash-flow family from the post-estimation script: `net_cashflow`, `cf_vol`,
#' `income_share`, `income_share_vol`, `liquidity_deficit`,
#' `liquidity_deficit_rate`, `total_spend_roll`, `liquidity_buffer` and the
#' winsorised `*_w` variants.
#'
#' `liquidity_deficit` compares weekly focal spending with the customer's
#' *typical* weekly income (`income_mean`), not with that week's income, because
#' weekly income is zero in non-pay weeks.
#'
#' @param p Panel from [load_panel()] (modified by reference).
#' @param focal Focal spending categories. Default [FOCAL_SPEND_CATEGORIES].
#' @param winsor_probs Quantiles for the focal spend winsorisation. Default
#'   `c(0.005, 0.995)` (pooled over customers and weeks, as in the script).
#' @param roll_n Window for rolling volatility and rates. Default 8 weeks.
#' @param cv_basis `"all"` (script behaviour: coefficient of variation of the
#'   sum over every `spend_*` column in 2020) or `"focal"` (sum over focal
#'   categories only).
#' @return The panel, invisibly (modified in place).
#' @export
derive_outcomes <- function(p, focal = FOCAL_SPEND_CATEGORIES,
                            winsor_probs = c(0.005, 0.995), roll_n = 8L,
                            cv_basis = c("all", "focal")) {
  cv_basis <- match.arg(cv_basis)
  .check_by_ref(p, "derive_outcomes")
  data.table::setorder(p, customer_id, wID)

  # 1. wallet shares from raw columns
  for (nm in names(WALLET_SHARES)) {
    num <- WALLET_SHARES[[nm]][1]; den <- WALLET_SHARES[[nm]][2]
    if (!all(c(num, den) %in% names(p))) stop("derive_outcomes: missing ", num, " or ", den, call. = FALSE)
    p[, (paste0("walletshare_", nm)) := get(num) / get(den)]
  }
  # 2. derived spend categories
  p[, spend_Alcohol := irs_spend_Alcohol + irs_spend_AlcoholOut]
  p[, spend_Tobacco := irs_spend_Tobacco]
  # 3. zero-fill (NaN/Inf from zero denominators become 0, as in the script)
  wcols <- grep("^spend_|^walletshare_", names(p), value = TRUE)
  p[, (wcols) := lapply(.SD, function(x) data.table::fifelse(is.finite(x), x, 0)), .SDcols = wcols]
  # 4. cv_2020_spend
  basis_cols <- if (cv_basis == "all") grep("^spend_", names(p), value = TRUE) else paste0("spend_", focal)
  p[, .tmp_total := rowSums(.SD), .SDcols = basis_cols]
  cv <- p[format(week_start, "%Y") == "2020",
          .(cv_2020_spend = stats::sd(.tmp_total, na.rm = TRUE) / mean(.tmp_total, na.rm = TRUE)),
          by = customer_id]
  p[, .tmp_total := NULL]
  if ("cv_2020_spend" %in% names(p)) p[, cv_2020_spend := NULL]
  p[cv, on = "customer_id", cv_2020_spend := i.cv_2020_spend]
  # 5. rename spend_ -> Spend (keeps irs_spend_ untouched)
  old <- grep("^spend_", names(p), value = TRUE)
  data.table::setnames(p, old, sub("^spend_", "Spend", old))
  # 6. winsorise focal categories (pooled) and 7. total_spend
  focal_cols <- paste0("Spend", focal)
  miss <- setdiff(focal_cols, names(p))
  if (length(miss)) stop("derive_outcomes: focal columns missing: ", paste(miss, collapse = ", "), call. = FALSE)
  p[, (focal_cols) := lapply(.SD, winsorise, probs = winsor_probs), .SDcols = focal_cols]
  p[, total_spend := rowSums(.SD), .SDcols = focal_cols]
  # 8. cash-flow family (post_reg)
  for (v in c("weekly_income", "weekly_balance", "income_mean"))
    if (!v %in% names(p)) stop("derive_outcomes: panel lacks ", v, call. = FALSE)
  p[is.na(weekly_income), weekly_income := 0]
  p[is.na(weekly_balance), weekly_balance := 0]
  p[, net_cashflow := weekly_income - total_spend]
  # window passed positionally: data.table < 1.17 names it `n`, >= 1.17 names it `N`
  sd_narm <- function(v) stats::sd(v, na.rm = TRUE)
  p[, cf_vol := data.table::frollapply(net_cashflow, roll_n, sd_narm, align = "right"), by = customer_id]
  p[is.na(cf_vol), cf_vol := 0]
  p[, income_share := (weekly_income - total_spend) / income_mean]
  p[, income_share_vol := data.table::frollapply(income_share, roll_n, sd_narm, align = "right"), by = customer_id]
  p[is.na(income_share_vol), income_share_vol := 0]
  p[, liquidity_deficit := as.integer(total_spend > income_mean)]
  p[, liquidity_deficit_rate := data.table::frollmean(liquidity_deficit, roll_n), by = customer_id]
  p[is.na(liquidity_deficit_rate), liquidity_deficit_rate := 0]
  p[, total_spend_roll := data.table::frollmean(total_spend, roll_n), by = customer_id]
  p[is.na(total_spend_roll), total_spend_roll := 0]
  p[, liquidity_buffer := 0]
  p[total_spend_roll > 0, liquidity_buffer := -weekly_balance / total_spend_roll]
  p[, liquidity_buffer_w  := winsorise(liquidity_buffer)]
  p[, cf_vol_w            := winsorise(cf_vol)]
  p[, income_share_vol_w  := winsorise(income_share_vol, c(0, 0.99))]
  p[, income_share_w      := winsorise(income_share, c(0, 0.99))]
  invisible(p)
}

#' Derive customer-level moderators
#'
#' From the post-estimation script: `homeloan_mean` (mean loan payment over
#' weeks up to the launch week), `numgoalcats` zero-filled, the budget-ratio
#' splines `br_under` (ratio capped at 0.95), `br_target` (0.95–1.05 segment)
#' and `br_over` (excess above 1.05), `avg_transaction_size`, and one
#' `budgetever_<Category>` dummy per `minBudgetDate_<Category>` column.
#'
#' @param p Panel after [derive_outcomes()] (modified by reference).
#' @param launch_wID Week index of the cohort's launch; default
#'   `event_week("public_launch")`.
#' @return The panel, invisibly.
#' @export
derive_moderators <- function(p, launch_wID = NULL) {
  .check_by_ref(p, "derive_moderators")
  launch_wID <- launch_wID %||% event_week("public_launch")
  if ("monthly_loan_payment_w" %in% names(p))
    p[, homeloan_mean := mean(monthly_loan_payment_w[wID <= launch_wID], na.rm = TRUE), by = customer_id]
  if ("numgoalcats" %in% names(p)) p[is.na(numgoalcats), numgoalcats := 0]
  if ("budget_to_spend_ratio_mean" %in% names(p)) {
    p[, br_under  := pmin(budget_to_spend_ratio_mean, 0.95)]
    p[, br_target := pmax(pmin(budget_to_spend_ratio_mean, 1.05) - 0.95, 0)]
    p[, br_over   := pmax(budget_to_spend_ratio_mean - 1.05, 0)]
  }
  if (all(c("total_spend", "total_transactions") %in% names(p)))
    p[, avg_transaction_size := data.table::fifelse(total_spend == 0 | total_transactions == 0, 0,
                                                     total_spend / total_transactions)]
  for (col in grep("^minBudgetDate_", names(p), value = TRUE))
    p[, (sub("^minBudgetDate_", "budgetever_", col)) := as.integer(!is.na(get(col)))]
  invisible(p)
}

#' Data dictionary of derived variables
#' @return data.table with `variable`, `stage`, `definition`.
#' @export
data_dictionary <- function() {
  d <- list(
    c("walletshare_<X>", "derive_outcomes", "irs_spend_<X> / parent spend category (see WALLET_SHARES); 0 where the parent is 0"),
    c("spend_Alcohol", "derive_outcomes", "irs_spend_Alcohol + irs_spend_AlcoholOut (renamed SpendAlcohol)"),
    c("spend_Tobacco", "derive_outcomes", "irs_spend_Tobacco (renamed SpendTobacco)"),
    c("cv_2020_spend", "derive_outcomes", "sd/mean of weekly total spending over calendar 2020, per customer (cv_basis: all or focal categories)"),
    c("Spend<Category>", "derive_outcomes", "spend_<Category> renamed; focal categories winsorised at winsor_probs pooled over rows"),
    c("total_spend", "derive_outcomes", "sum of the eleven winsorised focal Spend* categories"),
    c("net_cashflow", "derive_outcomes", "weekly_income - total_spend"),
    c("cf_vol", "derive_outcomes", "rolling roll_n-week SD of net_cashflow (0 where undefined)"),
    c("income_share", "derive_outcomes", "(weekly_income - total_spend) / income_mean  [financial slack]"),
    c("income_share_vol", "derive_outcomes", "rolling roll_n-week SD of income_share"),
    c("liquidity_deficit", "derive_outcomes", "1 if total_spend > income_mean (typical weekly income)"),
    c("liquidity_deficit_rate", "derive_outcomes", "rolling roll_n-week mean of liquidity_deficit"),
    c("total_spend_roll", "derive_outcomes", "rolling roll_n-week mean of total_spend (script name: spend_roll)"),
    c("liquidity_buffer", "derive_outcomes", "-weekly_balance / total_spend_roll (0 where the rolling mean is 0)"),
    c("*_w", "derive_outcomes", "winsorised variants: liquidity_buffer_w, cf_vol_w (1/99), income_share_vol_w, income_share_w (0/99)"),
    c("homeloan_mean", "derive_moderators", "mean monthly_loan_payment_w over weeks <= launch week"),
    c("br_under / br_target / br_over", "derive_moderators", "linear spline of budget_to_spend_ratio_mean with knots 0.95 and 1.05"),
    c("avg_transaction_size", "derive_moderators", "total_spend / total_transactions (0 if either is 0)"),
    c("budgetever_<Category>", "derive_moderators", "1 if minBudgetDate_<Category> is not NA")
  )
  data.table::rbindlist(lapply(d, function(x) data.table::data.table(variable = x[1], stage = x[2], definition = x[3])))
}

utils::globalVariables(c("tdate", "age", "age_band", "customer_tenure", "irs_spend_Alcohol",
  "irs_spend_AlcoholOut", "irs_spend_Tobacco", "spend_Alcohol", "spend_Tobacco", ".tmp_total",
  "i.cv_2020_spend", "cv_2020_spend", "total_spend", "weekly_income", "weekly_balance",
  "income_mean", "net_cashflow", "cf_vol", "income_share", "income_share_vol",
  "liquidity_deficit", "liquidity_deficit_rate", "total_spend_roll", "liquidity_buffer",
  "liquidity_buffer_w", "cf_vol_w", "income_share_vol_w", "income_share_w",
  "monthly_loan_payment_w", "homeloan_mean", "numgoalcats", "budget_to_spend_ratio_mean",
  "br_under", "br_target", "br_over", "total_transactions", "avg_transaction_size"))
