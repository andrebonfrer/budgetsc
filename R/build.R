# build.R ---------------------------------------------------------------------
# Tier A: the one function allowed to read data/. Assembles the analysis panel
# a run reads: the processed weekly panel + weekly salary income + weekly
# balances. Run rarely, by you; every run records the output file's MD5.

#' Build the analysis panel (processed panel + weekly income + balances)
#'
#' Transcribed from the merge block of `post_reg_augMultisynth_budgetsetters.R`
#' with one correction: the script assigned income to
#' `as.integer(difftime(payt_d, origin, units = "weeks"))`, i.e. without the
#' `+ 1` in the panel's `wID`, so income landed one week early. The default
#' `week_alignment = "containing"` uses [week_of()] for both sides;
#' `"legacy_shift"` reproduces the script's behaviour for comparison.
#'
#' @param panel_file Processed weekly panel(s) (`BudgetPanelDataWeekly_with_donor*.rds`).
#'   With several files, each file's non-zero `donor` flag is recoded to the
#'   matching entry of `donor_codes` (default 1, 2, ...), so the donor-1 and
#'   donor-2 files give one panel with codes 0 = budget setter, 1 = partial
#'   onboarder, 2 = never-onboarder. Customers in several files are kept once.
#' @param donor_codes Integer vector, one per `panel_file`.
#' @param income_file Salary transactions with `customer_id`, `payt_d` (date),
#'   `payt_a` (amount). Default `data/commincome_fncl_tran_slry.rds`.
#' @param balance_file Weekly balances with `customer_id`, `wID`, `weekly_balance`.
#'   Default `Processed/weekly_balances_without_homeloans.rds`. Its `wID` is
#'   taken as already aligned with the panel's; see `balance_wID_offset`.
#' @param out_file Output. Default `Processed/analysis_panel.rds`.
#' @param week_alignment `"containing"` (default) or `"legacy_shift"`.
#' @param balance_wID_offset Integer added to the balance file's `wID` before
#'   merging (0 = trust it). Default 0.
#' @param root Project root; relative paths resolve against it.
#' @return The panel, invisibly; written to `out_file` with an attached
#'   `build_info` attribute (inputs, MD5s, alignment, row counts).
#' @export
build_analysis_panel <- function(panel_file, income_file = "data/commincome_fncl_tran_slry.rds",
                                 donor_codes = seq_along(panel_file),
                                 balance_file = "Processed/weekly_balances_without_homeloans.rds",
                                 out_file = "Processed/analysis_panel.rds",
                                 week_alignment = c("containing", "legacy_shift"),
                                 balance_wID_offset = 0L, root = NULL) {
  week_alignment <- match.arg(week_alignment)
  rp <- function(f) ifelse(grepl("^(/|[A-Za-z]:)", f), f, file.path(bsc_root(root), f))   # vectorised: several panel files
  tl <- bsc_timeline()

  # one or several processed panels (e.g. the donor-1 and donor-2 files): stack
  # them, recode each file's non-zero donor flag to `donor_codes[i]`, and keep
  # the first occurrence of a customer that appears in more than one file
  # (budget setters are in every file).
  parts <- lapply(seq_along(panel_file), function(i) {
    d <- data.table::as.data.table(readRDS(rp(panel_file[i])))
    if (!"donor" %in% names(d)) d[, donor := 0L]
    d[, donor := as.integer(donor)]
    if (length(panel_file) > 1L) d[donor > 0L, donor := as.integer(donor_codes[i])]
    d
  })
  if (length(parts) > 1L) {
    seen <- integer(0)
    for (i in seq_along(parts)) { parts[[i]] <- parts[[i]][!customer_id %in% seen]; seen <- c(seen, unique(parts[[i]]$customer_id)) }
  }
  p <- data.table::rbindlist(parts, use.names = TRUE, fill = TRUE)
  p[, week_start := as.Date(week_start)]
  p <- p[week_start >= tl$events$panel_first_date & week_start <= tl$events$panel_cutoff]
  if (min(p$week_start) != tl$origin)
    stop(sprintf("panel origin %s != timeline origin %s", min(p$week_start), tl$origin), call. = FALSE)
  p[, wID := week_of(week_start, tl$origin)]
  for (v in c("weekly_income", "weekly_balance")) if (v %in% names(p)) p[, (v) := NULL]

  # ---- income: sum of salary transactions per customer-week --------------------
  inc <- data.table::as.data.table(readRDS(rp(income_file)))
  need <- c("customer_id", "payt_d", "payt_a"); miss <- setdiff(need, names(inc))
  if (length(miss)) stop("income file lacks: ", paste(miss, collapse = ", "), call. = FALSE)
  inc[, payt_d := as.Date(payt_d)]
  inc[, wID := if (week_alignment == "containing") week_of(payt_d, tl$origin)
               else as.integer(floor(as.numeric(payt_d - tl$origin) / 7))]      # legacy: no +1
  inc_w <- inc[, .(weekly_income = sum(payt_a, na.rm = TRUE)), by = .(customer_id, wID)]
  p <- merge(p, inc_w, by = c("customer_id", "wID"), all.x = TRUE)
  p[is.na(weekly_income), weekly_income := 0]

  # ---- balances --------------------------------------------------------------------
  bal <- data.table::as.data.table(readRDS(rp(balance_file)))
  need <- c("customer_id", "wID", "weekly_balance"); miss <- setdiff(need, names(bal))
  if (length(miss)) stop("balance file lacks: ", paste(miss, collapse = ", "), call. = FALSE)
  bal <- bal[, .(customer_id, wID = as.integer(wID) + as.integer(balance_wID_offset), weekly_balance)]
  p <- merge(p, bal, by = c("customer_id", "wID"), all.x = TRUE)
  n_bal_na <- sum(is.na(p$weekly_balance))
  p[is.na(weekly_balance), weekly_balance := 0]
  data.table::setorder(p, customer_id, wID)

  info <- list(built = Sys.time(), host = host_id(), week_alignment = week_alignment,
               balance_wID_offset = balance_wID_offset,
               inputs = c(panel = paste(rp(panel_file), collapse = ";"), income = rp(income_file), balance = rp(balance_file)),
               donor_codes = stats::setNames(donor_codes, basename(panel_file)),
               donor_counts = p[, .(n = data.table::uniqueN(customer_id)), by = donor][order(donor)],
               input_md5 = vapply(c(rp(panel_file), rp(income_file), rp(balance_file)),
                                  function(f) tools::md5sum(f)[[1]], character(1)),
               n_rows = nrow(p), n_customers = data.table::uniqueN(p$customer_id),
               weeks = range(p$wID), share_income_weeks = mean(p$weekly_income > 0),
               n_balance_missing = n_bal_na)
  data.table::setattr(p, "build_info", info)
  out <- rp(out_file); dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  saveRDS(p, out)
  message(sprintf("analysis panel: %d rows, %d customers (donor codes %s), weeks %d-%d, income in %.1f%% of weeks, %d balance rows filled with 0 -> %s",
                  info$n_rows, info$n_customers,
                  paste(sprintf("%d: %d", info$donor_counts$donor, info$donor_counts$n), collapse = ", "),
                  info$weeks[1], info$weeks[2], 100 * info$share_income_weeks, n_bal_na, out))
  invisible(p)
}

utils::globalVariables(c("payt_d", "payt_a"))
