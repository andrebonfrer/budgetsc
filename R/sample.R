# sample.R --------------------------------------------------------------------
# Tier C: who is treated, who donates, which weeks. One function, several
# designs. Logic transcribed from runSynth_augMultiSynth_budgetsetters.R
# (later_adopters) and runSynth_augMultiSynth.R (onboarder pools + stratified
# sampling). Every filter records how many customers it removed so the sample
# funnel table (checklist §15) is a by-product, not a separate analysis.

#' Define the analysis sample for a run
#'
#' @param p Panel after [derive_outcomes()] and [derive_moderators()].
#' @param spec A `bsc_spec`.
#' @return A list of class `bsc_sample`: `ids` (customer_id, role, cohort,
#'   treat_wID, minBudgetDate), `window` (L, H, match_end, launch_wID,
#'   truncate_wID, launch_date), `funnel` (data.table of filter steps and
#'   counts), and `panel` (rows for the kept customers, with `budgetdummy`
#'   and `donor` recoded for the design).
#' @export
define_sample <- function(p, spec) {
  data.table::setDT(p)
  tl <- bsc_timeline()
  s <- spec$sample
  design <- s$design; cohort <- s$cohort
  L <- as.integer(s$n_lags); H <- as.integer(s$n_leads)
  funnel <- list(); note <- function(step, ids) funnel[[length(funnel) + 1L]] <<-
    data.table::data.table(step = step, n_customers = length(unique(ids)))

  x <- data.table::copy(p)
  note("panel", x$customer_id)
  if (!"minBudgetDate" %in% names(x)) stop("panel lacks minBudgetDate", call. = FALSE)
  x[, minBudgetDate := as.Date(minBudgetDate)]
  if (!"donor" %in% names(x)) x[, donor := 0L]

  # ---- placebo designs: rewrite minBudgetDate, then proceed as the base design ------
  placebo_info <- NULL
  if (design == "placebo_dates") {
    pl <- s$placebo
    if (is.null(pl$base_design)) stop("sample.placebo$base_design is required for placebo_dates", call. = FALSE)
    x <- .apply_placebo(x, pl, tl, H)
    placebo_info <- attr(x, "placebo_info")
    note(paste0("placebo: ", pl$units %||% "treated", ", shift ", pl$shift %||% NA), x$customer_id)
    design <- pl$base_design
  }

  # ---- cohort restriction and launch date ---------------------------------------
  public_launch <- tl$events$public_launch
  if (cohort == "public") {
    pre_launch_setters <- x[minBudgetDate < public_launch, unique(customer_id)]
    x <- x[!customer_id %in% pre_launch_setters]
    launch_date <- public_launch
    note("drop pilot-period budget setters", x$customer_id)
  } else {
    public_setters <- x[minBudgetDate >= public_launch, unique(customer_id)]
    x <- x[!customer_id %in% public_setters]
    if (design %in% c("partial_onboarders", "non_onboarders") && "onboarddate" %in% names(x))
      x <- x[!customer_id %in% x[as.Date(onboarddate) >= public_launch, unique(customer_id)]]
    launch_date <- x[, min(minBudgetDate, na.rm = TRUE)]   # first observed pilot adoption
    note("drop public-launch budget setters", x$customer_id)
  }
  launch_wID   <- first_week_on_or_after(launch_date, tl$origin)
  truncate_wID <- launch_wID + H
  truncate_date <- launch_date + 7L * H

  # ---- roles by design ----------------------------------------------------------
  if (design == "later_adopters") {
    x <- x[donor == 0L]                                   # budget setters only
    note("budget setters only", x$customer_id)
    x[, donor := 0L]
    x[minBudgetDate >= truncate_date, minBudgetDate := as.Date(NA)]   # later adopters -> donors
    x[is.na(minBudgetDate), donor := 1L]
  } else if (design %in% c("partial_onboarders", "non_onboarders")) {
    want <- if (design == "partial_onboarders") 1L else 2L
    x <- x[donor %in% c(0L, want)]
    note(paste0("budget setters + donor pool ", want), x$customer_id)
    x[donor == 0L & minBudgetDate >= truncate_date, donor := 3L]   # setters after window: excluded
    x <- x[donor != 3L]
    x[donor == want, donor := 1L]
    note("drop setters adopting after launch + H", x$customer_id)
  } else {
    stop("design '", design, "' is not implemented yet (Phase 3).", call. = FALSE)
  }
  # Treatment timing. The scripts used tdate >= minBudgetDate for budgetdummy
  # (i.e. the first FULL week unless the budget was set on a Monday) but the
  # containing week for the SC treat_time — two conventions. Here one setting
  # governs both: "containing_week" (default; the SC convention, partially
  # treated adoption week counts as treated) or "next_week" (the scripts'
  # Stage-2 convention).
  ts <- s$treat_start %||% "containing_week"
  x[, treat_wID := NA_integer_]
  x[!is.na(minBudgetDate) & donor == 0L, treat_wID := week_of(minBudgetDate, tl$origin) +
      as.integer(ts == "next_week" & format(minBudgetDate, "%u") != "1")]
  x[, budgetdummy := 0L]
  x[!is.na(treat_wID), budgetdummy := as.integer(wID >= treat_wID)]

  # ---- filters (pre_summ / breadth from the scripts) -------------------------------
  f <- s$filters
  pre <- x[wID < launch_wID, .(
    pre_weeks = sum(!is.na(total_spend) & !is.na(income_mean)),
    pre_spend_sum = sum(total_spend, na.rm = TRUE),
    pre_income_sum = sum(income_mean, na.rm = TRUE),
    budgetcategoriesN = if (all(is.na(budgetcategoriesN))) NA_integer_ else as.integer(max(budgetcategoriesN, na.rm = TRUE)),
    donor = max(donor)
  ), by = customer_id]
  pre[, pre_spend_pct_income := data.table::fifelse(pre_income_sum > 0, pre_spend_sum / pre_income_sum, NA_real_)]
  keep <- pre[pre_weeks >= f$min_pre_weeks & !is.na(pre_spend_pct_income) &
                pre_spend_pct_income >= f$pct_income_thresh, customer_id]
  x <- x[customer_id %in% keep]; note(sprintf("pre_weeks >= %d & spend/income >= %s", f$min_pre_weeks, f$pct_income_thresh), x$customer_id)
  keep <- pre[donor > 0L | (!is.na(budgetcategoriesN) & budgetcategoriesN >= f$n_budgets_min), customer_id]
  x <- x[customer_id %in% keep]; note(sprintf("treated have >= %d budget categories", f$n_budgets_min), x$customer_id)
  spend_cols <- grep("^Spend", names(x), value = TRUE)
  breadth <- x[wID < launch_wID, .(n_cats = sum(vapply(.SD, function(v) any(!is.na(v) & v > 0), logical(1)))),
               by = customer_id, .SDcols = spend_cols]
  x <- x[customer_id %in% breadth[n_cats >= f$min_cats, customer_id]]
  note(sprintf("spent in >= %d categories pre-launch", f$min_cats), x$customer_id)

  # ---- optional subset of treated units (e.g. distress type) -----------------------
  if (!is.null(s$subset)) {
    v <- s$subset$var; val <- s$subset$value
    if (!v %in% names(x)) stop("subset variable '", v, "' not in panel", call. = FALSE)
    keep <- x[donor > 0L | get(v) %in% val, unique(customer_id)]
    x <- x[customer_id %in% keep]; note(sprintf("treated subset %s in {%s}", v, paste(val, collapse = ",")), x$customer_id)
  }

  # ---- donor sampling (stratified, as in the onboarder scripts) --------------------
  ds <- s$donor_sampling
  if (!is.null(ds$method) && ds$method != "none") {
    donor_ids <- .sample_donors(x, launch_wID, ds)
    x <- x[donor == 0L | customer_id %in% donor_ids]
    note(sprintf("donor sampling: %s (target %s)", ds$method, ds$target), x$customer_id)
  }

  # ---- ids and treatment weeks ------------------------------------------------------
  ids <- unique(x[, .(customer_id, donor, minBudgetDate, treat_wID)])
  ids[, role := data.table::fifelse(donor == 0L, "treated", "donor")]
  ids[, cohort := cohort]
  # treated units need L observed pre-weeks and H+1 post-weeks inside the panel
  wk_range <- range(x$wID)
  short <- ids[role == "treated" & (treat_wID - L < wk_range[1] | treat_wID + H > wk_range[2])]
  if (nrow(short)) {
    x <- x[!customer_id %in% short$customer_id]; ids <- ids[!customer_id %in% short$customer_id]
    note(sprintf("treated with L=%d pre and H=%d post weeks observed", L, H), x$customer_id)
  }
  ids[, c("donor", "minBudgetDate") := list(NULL, minBudgetDate)]

  structure(list(
    ids = ids[, .(customer_id, role, cohort, treat_wID, minBudgetDate)],
    window = list(L = L, H = H, match_end = as.integer(spec$sc$match_end),
                  launch_date = launch_date, launch_wID = launch_wID, truncate_wID = truncate_wID),
    funnel = data.table::rbindlist(funnel),
    placebo = placebo_info,
    panel = x
  ), class = "bsc_sample")
}

# Placebo timing. Two forms:
#   units = "treated": each real adopter's onset is moved `shift` weeks earlier
#     (pre-adoption placebo). shift must exceed H so the placebo post window ends
#     before the real adoption; treated units without L pre-weeks before the
#     pseudo onset are dropped later by the usual window check.
#   units = "never_onboarders": customers with donor == 2 receive pseudo onsets
#     drawn (with replacement, `seed`) from the real adopters' onset dates and
#     become the "treated" group; real adopters are removed so the base design's
#     donor rule applies to the remaining pool.
.apply_placebo <- function(x, pl, tl, H) {
  units <- pl$units %||% "treated"
  if (units == "treated") {
    shift <- as.integer(pl$shift %||% (H + 1L))
    if (shift <= H) stop("sample.placebo$shift must exceed n_leads so the placebo window precedes real adoption.", call. = FALSE)
    x[!is.na(minBudgetDate) & donor == 0L, minBudgetDate := minBudgetDate - 7L * shift]
    info <- list(units = units, shift = shift)
  } else if (units == "never_onboarders") {
    real <- unique(x[!is.na(minBudgetDate) & donor == 0L, .(customer_id, minBudgetDate)])
    if (!nrow(real)) stop("no real adopters to draw pseudo onsets from", call. = FALSE)
    never <- x[donor == 2L, unique(customer_id)]
    if (!length(never)) stop("no never-onboarders (donor == 2) in the panel", call. = FALSE)
    n_take <- min(length(never), pl$n %||% nrow(real))
    set.seed(pl$seed %||% 1L)
    pseudo <- data.table::data.table(customer_id = sample(never, n_take),
                                     pseudo = sample(real$minBudgetDate, n_take, replace = TRUE))
    x <- x[!customer_id %in% real$customer_id]                 # drop real adopters
    x[pseudo, on = "customer_id", `:=`(minBudgetDate = i.pseudo, donor = 0L)]
    x[donor == 2L, donor := 1L]                                # remaining never-onboarders are donors
    info <- list(units = units, n = n_take, seed = pl$seed %||% 1L)
  } else stop("sample.placebo$units must be 'treated' or 'never_onboarders'", call. = FALSE)
  data.table::setattr(x, "placebo_info", info)
  x
}

# Stratified donor sampling: deciles of treated spend/income and log mean spend,
# crossed with home-loan status; donors drawn per stratum in proportion to the
# treated distribution (runSynth_augMultiSynth.R, "Create matching strata").
.sample_donors <- function(x, launch_wID, ds) {
  feat <- x[wID < launch_wID, .(
    spend_mean = mean(total_spend, na.rm = TRUE),
    spend_pct_income = { s <- sum(total_spend, na.rm = TRUE); y <- sum(income_mean, na.rm = TRUE); if (y > 0) s / y else NA_real_ },
    has_homeloan = as.integer(any(!is.na(monthly_loan_payment_w) & monthly_loan_payment_w > 0)),
    donor = max(donor)), by = customer_id]
  feat <- feat[!is.na(spend_pct_income)]
  if (ds$method == "random") {
    set.seed(ds$seed %||% 1L)
    pool <- feat[donor > 0L, customer_id]
    return(sample(pool, min(length(pool), ds$target)))
  }
  feat[, log_spend_mean := log1p(spend_mean)]
  qcuts <- function(v) unique(stats::quantile(v, probs = seq(0, 1, length.out = 11), na.rm = TRUE, type = 7))
  feat[, str_sp := cut(spend_pct_income, breaks = qcuts(feat[donor == 0L, spend_pct_income]), include.lowest = TRUE)]
  feat[, str_ls := cut(log_spend_mean,   breaks = qcuts(feat[donor == 0L, log_spend_mean]),   include.lowest = TRUE)]
  feat[, strata := interaction(str_sp, str_ls, has_homeloan, drop = TRUE)]
  tc <- feat[donor == 0L, .N, by = strata][, share := N / sum(N)][, n_target := pmax(1L, as.integer(round(share * ds$target)))]
  set.seed(ds$seed %||% 1L)
  pool <- feat[donor > 0L]
  picked <- pool[tc, on = "strata", {
    n_take <- i.n_target; if (is.na(n_take) || n_take <= 0L) .SD[0] else .SD[sample.int(.N, min(.N, n_take))]
  }, by = .EACHI]
  ids <- picked$customer_id
  if (length(ids) > ds$target) ids <- ids[seq_len(ds$target)]
  ids
}

#' @export
print.bsc_sample <- function(x, ...) {
  cat("bsc_sample: ", sum(x$ids$role == "treated"), " treated, ", sum(x$ids$role == "donor"), " donors; cohort ",
      x$ids$cohort[1], "; L=", x$window$L, " H=", x$window$H, " launch wID ", x$window$launch_wID, "\n", sep = "")
  print(x$funnel)
  invisible(x)
}

utils::globalVariables(c("i.pseudo", "pseudo", "minBudgetDate", "donor", "onboarddate", "budgetdummy", "pre_weeks",
  "pre_spend_sum", "pre_income_sum", "pre_spend_pct_income", "budgetcategoriesN", "n_cats",
  "treat_wID", "spend_mean", "spend_pct_income", "has_homeloan", "log_spend_mean", "str_sp",
  "str_ls", "strata", "share", "n_target", "i.n_target", "N", "cohort"))
