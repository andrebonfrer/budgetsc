#' Flag pre-adoption "distress" customers (time-invariant indicator)
#'
#' Classifies each customer as in distress (1) or not (0) on the basis of
#' shock activity in the \code{TS} months before they set a budget, and writes
#' the indicator (plus diagnostics) back into \code{bdata} by reference.
#' Non-adopters receive \code{NA} unless \code{nonadopter_onset} is supplied.
#'
#' The shock is defined by \code{shock_vars} (NOT by \code{out_col}, which only
#' names the output column). A "shock week" is a week in which the combined
#' \code{shock_test} over \code{shock_vars} is TRUE. Customers are classified
#' from their pre-window shock weeks according to \code{rule}:
#'   "any"       at least one shock week in the pre-window
#'   "threshold" at least \code{threshold} shock weeks
#'   "median"    more shock weeks than the adopter median
#'   "relative"  pre-window shock rate exceeds the customer's own earlier
#'               baseline rate by more than \code{rel_margin} (dip-type shock)
#'
#' For continuous variables such as spending, set \code{standardise = TRUE}:
#' each shock variable is z-scored within customer using only the weeks
#' BEFORE the pre-window (own baseline), so \code{shock_test = function(x) x > 1.5}
#' flags weeks 1.5 SD above the customer's normal level.
#'
#' Composites: pass several \code{shock_vars} with a \code{combine} rule, or
#' pre-compute a composite column in \code{bdata} and pass its name.
#'
#' @param bdata data.table. Weekly panel with id, time, treatment and shock columns.
#' @param shock_vars Character vector. Variable(s) defining a shock week.
#'   Default \code{"numarrears"}.
#' @param shock_test Function. Vectorised weekly test applied to each shock
#'   variable, returning logical. Default \code{function(x) x > 0}.
#' @param combine \code{"any"} (default), \code{"all"}, or \code{"sum"}.
#' @param standardise Logical. Z-score each shock variable within customer on
#'   the baseline weeks before the pre-window. Default FALSE.
#' @param TS Numeric. Months before budget set defining the pre-window. Default 3.
#' @param weeks_per_month Numeric. Default \code{52/12}; use 4 for calendar months.
#' @param min_pre_weeks Integer. Minimum observed pre-window weeks to classify.
#'   Default \code{ceiling(ts_weeks / 2)}.
#' @param min_base_weeks Integer. Minimum baseline weeks required when
#'   \code{standardise = TRUE} or \code{rule = "relative"}. Default 8.
#' @param rule Classification rule; see Details. Default \code{"any"}.
#' @param threshold Integer. Shock weeks needed under \code{rule = "threshold"}. Default 1.
#' @param rel_margin Numeric. Margin for \code{rule = "relative"}. Default 0.
#' @param nonadopter_onset Optional integer pseudo-onset for non-adopters. Default NULL.
#' @param id_col,time_col,tr_col Column names. Defaults \code{"customer_id"},
#'   \code{"wID"}, \code{"budgetdummy"}.
#' @param out_col Character. Name of the 0/1 indicator written to \code{bdata};
#'   diagnostic columns are prefixed with it. Must not be the name of an
#'   existing data column. Default \code{"distress"}.
#' @param overwrite Logical. Allow replacing columns from a previous
#'   \code{flag_distress()} run with the same \code{out_col}. Default FALSE.
#' @param verbose Logical. Print customer counts. Default TRUE.
#'
#' @return \code{bdata}, modified in place and returned invisibly. Use
#'   \code{distress_counts()} for counts by type.
#' @export
flag_distress <- function(bdata,
                          shock_vars       = "numarrears",
                          shock_test       = function(x) x > 0,
                          combine          = c("any", "all", "sum"),
                          standardise      = FALSE,
                          TS               = 3,
                          weeks_per_month  = 52 / 12,
                          min_pre_weeks    = NULL,
                          min_base_weeks   = 8L,
                          rule             = c("any", "threshold", "median", "relative"),
                          threshold        = 1L,
                          rel_margin       = 0,
                          nonadopter_onset = NULL,
                          id_col           = "customer_id",
                          time_col         = "wID",
                          tr_col           = "budgetdummy",
                          out_col          = "distress",
                          overwrite        = FALSE,
                          verbose          = TRUE) {

  combine <- match.arg(combine)
  rule    <- match.arg(rule)
  data.table::setDT(bdata)

  ## ---- checks ----------------------------------------------------------
  need <- c(id_col, time_col, tr_col, shock_vars)
  miss <- setdiff(need, names(bdata))
  if (length(miss))
    stop("Columns not found in bdata: ", paste(miss, collapse = ", "), call. = FALSE)

  ts_weeks <- as.integer(round(TS * weeks_per_month))
  if (ts_weeks < 1L) stop("TS * weeks_per_month must be at least one week.", call. = FALSE)
  if (is.null(min_pre_weeks)) min_pre_weeks <- as.integer(ceiling(ts_weeks / 2))

  type_col <- paste0(out_col, "_type")
  diag_map <- c(pre_weeks_obs   = paste0(out_col, "_pre_weeks"),
                pre_shock_weeks = paste0(out_col, "_pre_shock_weeks"),
                pre_shock_score = paste0(out_col, "_pre_shock_score"),
                pre_shock_rate  = paste0(out_col, "_pre_rate"),
                base_weeks_obs  = paste0(out_col, "_base_weeks"),
                base_shock_rate = paste0(out_col, "_base_rate"),
                shock_rel       = paste0(out_col, "_rel"))
  own_cols <- c(out_col, type_col, unname(diag_map))

  ## collision guard: never clobber a data column
  if (out_col %in% c(shock_vars, id_col, time_col, tr_col))
    stop("out_col = '", out_col, "' is a data column. out_col only NAMES the output; ",
         "define the shock with shock_vars.", call. = FALSE)
  existing <- intersect(own_cols, names(bdata))
  if (length(existing) && !overwrite)
    stop("Columns already exist in bdata: ", paste(existing, collapse = ", "),
         ". If these are from a previous flag_distress() run, set overwrite = TRUE; ",
         "otherwise choose a different out_col.", call. = FALSE)

  ## ---- working copy with fixed names ------------------------------------
  w <- bdata[, c(id_col, time_col, tr_col, shock_vars), with = FALSE]
  data.table::setnames(w, c(id_col, time_col, tr_col), c("id", "t", "tr"))

  ## ---- onset week per adopter (needed before standardising) --------------
  onset <- w[tr == 1L, .(onset = min(t)), by = id]
  w[onset, on = "id", onset := i.onset]
  w[, adopter := !is.na(onset)]
  if (!is.null(nonadopter_onset))
    w[is.na(onset), onset := as.integer(nonadopter_onset)]
  w[, base_wk := !is.na(onset) & t < onset - ts_weeks]

  ## ---- optional within-customer standardisation on baseline weeks --------
  if (isTRUE(standardise)) {
    for (v in shock_vars) {
      w[, (v) := {
        x <- get(v); b <- base_wk
        m <- mean(x[b], na.rm = TRUE); s <- stats::sd(x[b], na.rm = TRUE)
        if (!is.finite(s) || s == 0) rep(NA_real_, .N) else (x - m) / s
      }, by = id]
    }
  }

  ## ---- weekly shock indicator -------------------------------------------
  tests <- lapply(shock_vars, function(v) {
    z <- as.logical(shock_test(w[[v]]))
    z[is.na(z)] <- FALSE
    z
  })
  w[, shock_week := switch(combine,
      any = Reduce(`|`, tests),
      all = Reduce(`&`, tests),
      sum = Reduce(`+`, lapply(tests, as.integer)))]
  w[, shock_hit := shock_week > 0]

  ## ---- pre-window and own-baseline summaries ----------------------------
  pre <- w[!is.na(onset) & t >= onset - ts_weeks & t < onset,
    .(pre_weeks_obs   = .N,
      pre_shock_weeks = sum(shock_hit),
      pre_shock_score = sum(shock_week),
      pre_shock_rate  = mean(shock_hit)),
    by = id]

  base <- w[base_wk == TRUE,
    .(base_weeks_obs  = .N,
      base_shock_rate = mean(shock_hit)),
    by = id]

  cust <- unique(w[, .(id, adopter, onset)])
  cust <- merge(cust, pre,  by = "id", all.x = TRUE)
  cust <- merge(cust, base, by = "id", all.x = TRUE)
  cust[is.na(base_weeks_obs), base_weeks_obs := 0L]
  cust[, shock_rel := pre_shock_rate - base_shock_rate]

  ## ---- classify ---------------------------------------------------------
  med <- NA_real_
  if (rule == "median") {
    med <- stats::median(cust[adopter == TRUE, pre_shock_weeks], na.rm = TRUE)
    if (isTRUE(med == 0))
      message("flag_distress: adopter median shock weeks is 0; 'median' rule is equivalent to 'any'.")
  }
  cust[, (out_col) := switch(rule,
      any       = as.integer(pre_shock_weeks > 0),
      threshold = as.integer(pre_shock_weeks >= threshold),
      median    = as.integer(pre_shock_weeks > med),
      relative  = as.integer(shock_rel > rel_margin))]
  cust[is.na(pre_weeks_obs) | pre_weeks_obs < min_pre_weeks, (out_col) := NA_integer_]
  if (isTRUE(standardise) || rule == "relative")
    cust[base_weeks_obs < min_base_weeks, (out_col) := NA_integer_]

  cust[, (type_col) := factor(data.table::fifelse(get(out_col) == 1L, "distress", "none"),
                              levels = c("none", "distress"))]

  ## ---- write back to bdata (time-invariant columns) ---------------------
  data.table::setnames(cust, c("id", "onset", names(diag_map)),
                             c(id_col, "onset_wID", unname(diag_map)))
  write_cols <- c("onset_wID", "adopter", own_cols)
  for (cc in write_cols) if (cc %in% names(bdata)) bdata[, (cc) := NULL]
  bdata[cust, on = id_col, (write_cols) := mget(paste0("i.", write_cols))]

  ## ---- report -----------------------------------------------------------
  if (verbose) {
    cat(sprintf("\nflag_distress: shock_vars = %s | standardise = %s | combine = %s | rule = %s | window = %d weeks -> %s\n",
                paste(shock_vars, collapse = " + "), standardise, combine, rule, ts_weeks, out_col))
    print(cust[, .(n_customers = .N), by = c("adopter", type_col)][order(-adopter)])
    cat(sprintf("Adopters: %d | distress: %d | none: %d | unclassified: %d\n",
                cust[adopter == TRUE, .N],
                cust[adopter == TRUE & get(type_col) == "distress", .N],
                cust[adopter == TRUE & get(type_col) == "none",     .N],
                cust[adopter == TRUE & is.na(get(type_col)),         .N]))
  }

  invisible(bdata)
}


#' Customer counts by adopter status and distress type
#'
#' @param bdata data.table processed by \code{flag_distress()}.
#' @param out_col Character. The \code{out_col} used in that call. Default "distress".
#' @param id_col Character. Default "customer_id".
#' @return data.table with one row per adopter-status x type cell.
#' @export
distress_counts <- function(bdata, out_col = "distress", id_col = "customer_id") {
  type_col <- paste0(out_col, "_type")
  need <- c(id_col, "adopter", type_col)
  miss <- setdiff(need, names(bdata))
  if (length(miss))
    stop("Run flag_distress() first; missing: ", paste(miss, collapse = ", "), call. = FALSE)
  cl <- unique(bdata[, c(id_col, "adopter", type_col), with = FALSE])
  cl[, .(n_customers = .N), by = c("adopter", type_col)][order(-adopter)]
}


## ---- usage examples --------------------------------------------------------
## Arrears-based (default): any arrears in the 13 weeks before budget set
# bdata <- flag_distress(bdata)
#
## Spending-based: weeks with total_spend > 1.5 SD above the customer's OWN
## baseline level; at least 2 such weeks in the pre-window
# bdata <- flag_distress(bdata, shock_vars = "total_spend", standardise = TRUE,
#                        shock_test = function(x) x > 1.5,
#                        rule = "threshold", threshold = 2L, out_col = "distress_spend")
#
## Dip-type arrears definition: pre-window rate worse than own baseline
# bdata <- flag_distress(bdata, rule = "relative", out_col = "distress_rel")
#
## Cross-tabulate definitions (persistent vs transitory)
# unique(bdata[adopter == TRUE, .(customer_id, distress_type, distress_spend_type)])[
#   , .N, by = .(distress_type, distress_spend_type)]
#
## Counts at any later point
# distress_counts(bdata); distress_counts(bdata, out_col = "distress_spend")


utils::globalVariables(c("tr", "t", "id", "onset", "base_wk", "shock_week", "shock_hit",
  "adopter", "pre_weeks_obs", "pre_shock_weeks", "pre_shock_score", "pre_shock_rate",
  "base_weeks_obs", "base_shock_rate", "shock_rel", "i.onset"))
