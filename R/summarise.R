# summarise.R -----------------------------------------------------------------
# Tidy data first, formatted tables second. Transcribed from utils-paper.R:
# extract_tau_draws, summarise_unit_effects, summarise_ate, summarise_gamma,
# summarise_one_outcome. Every table is written as CSV; HTML (gt) and LaTeX
# twins are written when the formats are requested.

#' Treatment-effect draws per treated unit from a post result
#' @param post,gdata Elements of a saved `post/<outcome>.rds` (`gdata_light`).
#' @return Matrix draws x treated units, colnames = treated ids.
#' @export
extract_tau_draws <- function(post, gdata) {
  B <- post$beta_samples; K <- length(gdata$Xcols); J0 <- gdata$J0; k_tr <- gdata$intX
  stopifnot(ncol(B) == K * J0)
  idx <- (seq_len(J0) - 1L) * K + k_tr
  td <- B[, idx, drop = FALSE]
  colnames(td) <- as.character(gdata$treated_ids %||% paste0("tr", seq_len(J0)))
  td
}

#' Unit-level, ATE and gamma summaries
#' @param tau_draws Matrix from [extract_tau_draws()].
#' @param probs Quantiles. Default 2.5/50/97.5.
#' @return data.table.
#' @export
summarise_unit_effects <- function(tau_draws, probs = c(0.025, 0.5, 0.975)) {
  q <- t(apply(tau_draws, 2, stats::quantile, probs = probs, na.rm = TRUE))
  data.table::data.table(treated_id = colnames(tau_draws), mean = colMeans(tau_draws, na.rm = TRUE),
    sd = apply(tau_draws, 2, stats::sd, na.rm = TRUE), q025 = q[, 1], q500 = q[, 2], q975 = q[, 3])[
    , `:=`(sig_pos = q025 > 0, sig_neg = q975 < 0)][]
}

#' @rdname summarise_unit_effects
#' @export
summarise_ate <- function(tau_draws, probs = c(0.025, 0.5, 0.975)) {
  a <- rowMeans(tau_draws, na.rm = TRUE); q <- stats::quantile(a, probs, na.rm = TRUE)
  data.table::data.table(ate_mean = mean(a), ate_median = stats::median(a), ate_sd = stats::sd(a),
                         ate_q025 = q[[1]], ate_q500 = q[[2]], ate_q975 = q[[3]])
}

#' @rdname summarise_unit_effects
#' @param post Post result with `gamma_samples`.
#' @export
summarise_gamma <- function(post, probs = c(0.025, 0.5, 0.975)) {
  if (is.null(post$gamma_samples)) return(NULL)
  G <- as.matrix(post$gamma_samples); q <- t(apply(G, 2, stats::quantile, probs = probs, na.rm = TRUE))
  data.table::data.table(term = colnames(G), mean = colMeans(G), sd = apply(G, 2, stats::sd),
                         q025 = q[, 1], q500 = q[, 2], q975 = q[, 3])[, sig := q025 > 0 | q975 < 0][]
}

#' Summarise a completed run into tidy tables and formatted twins
#'
#' Writes to `tables/`: `outcome_table` (ATE mean/median/CI, dispersion,
#' median unit effect, % significantly positive/negative), `gamma_table`
#' (moderator coefficients with CIs and BH q-values within each spec family),
#' `unit_effects` (posterior mean/CI per treated unit and outcome),
#' `event_time_ate` (Stage 1 gaps by event week). Files are streamed one
#' outcome at a time so no two outcomes' draws are in memory together.
#' @param run A `bsc_run` with post results.
#' @param formats Subset of `c("csv", "html", "tex")`; default from the spec.
#' @return list of the tables, invisibly.
#' @export
summarise_run <- function(run, formats = NULL) {
  formats <- formats %||% run$spec$tables$formats
  td <- file.path(run$dir, "tables"); dir.create(td, showWarnings = FALSE)
  files <- file.path(run$dir, "post", paste0(run$spec$post$outcomes, ".rds"))
  files <- files[file.exists(files)]
  if (!length(files)) stop("no post results in ", run$dir, call. = FALSE)
  out_rows <- list(); gam_rows <- list(); unit_rows <- list()
  for (f in files) {
    o <- sub("\\.rds$", "", basename(f)); res <- readRDS(f)
    tdr <- extract_tau_draws(res$post, res$gdata_light)
    u <- summarise_unit_effects(tdr); a <- summarise_ate(tdr)
    out_rows[[o]] <- cbind(data.table::data.table(outcome = o, n_treated = ncol(tdr)), a,
      data.table::data.table(effect_dispersion = stats::sd(u$mean), median_tau_unit = stats::median(u$mean),
                             pct_sig_pos = 100 * mean(u$sig_pos), pct_sig_neg = 100 * mean(u$sig_neg)))
    g <- summarise_gamma(res$post); if (!is.null(g)) gam_rows[[o]] <- cbind(outcome = o, g)
    unit_rows[[o]] <- cbind(outcome = o, u)
    rm(res, tdr); gc(FALSE)
  }
  outcome_table <- data.table::rbindlist(out_rows)
  gamma_table <- if (length(gam_rows)) data.table::rbindlist(gam_rows) else NULL
  unit_effects <- data.table::rbindlist(unit_rows)
  fam <- run$spec$tables$families
  outcome_table[, family := .family_of(outcome, fam)]
  if (!is.null(gamma_table)) {
    gamma_table[, family := .family_of(outcome, fam)]
    gamma_table[, p_approx := 2 * stats::pnorm(-abs(mean / sd))]
    gamma_table[, q_bh := stats::p.adjust(p_approx, method = "BH"), by = family]
  }
  et <- tryCatch(sc_event_time_ate(readRDS(file.path(run$dir, "sc_fit.rds"))), error = function(e) NULL)
  tabs <- list(outcome_table = outcome_table, gamma_table = gamma_table, unit_effects = unit_effects, event_time_ate = et)
  for (nm in names(tabs)) if (!is.null(tabs[[nm]])) {
    data.table::fwrite(tabs[[nm]], file.path(td, paste0(nm, ".csv")))
    if (nm %in% c("outcome_table", "gamma_table")) write_table_formats(tabs[[nm]], file.path(td, nm), formats, run$spec$tables$digits)
  }
  mark_done(run, "summary")
  invisible(tabs)
}

.family_of <- function(x, fam) {
  out <- rep(NA_character_, length(x))
  for (nm in names(fam)) out[x %in% fam[[nm]]] <- nm
  out
}

#' Write a table as HTML (gt, if installed) and LaTeX (base R writer)
#' @param dt data.table.
#' @param stem Path without extension.
#' @param formats Character; subset of `c("html", "tex")`.
#' @param digits Rounding for numeric columns.
#' @return Invisibly, the files written.
#' @export
write_table_formats <- function(dt, stem, formats = c("html", "tex"), digits = 3L) {
  d <- data.table::copy(dt)
  num <- names(d)[vapply(d, is.numeric, logical(1))]
  d[, (num) := lapply(.SD, round, digits = digits), .SDcols = num]
  written <- character(0)
  if ("html" %in% formats && requireNamespace("gt", quietly = TRUE)) {
    gt::gtsave(gt::gt(as.data.frame(d)), paste0(stem, ".html")); written <- c(written, paste0(stem, ".html"))
  }
  if ("tex" %in% formats) {
    esc <- function(v) gsub("_", "\\\\_", as.character(v))
    hdr <- paste(esc(names(d)), collapse = " & ")
    body <- apply(d, 1, function(r) paste(esc(r), collapse = " & "))
    lines <- c(sprintf("\\begin{tabular}{l%s}", strrep("r", ncol(d) - 1)), "\\toprule", paste0(hdr, " \\\\"),
               "\\midrule", paste0(body, " \\\\"), "\\bottomrule", "\\end{tabular}")
    writeLines(lines, paste0(stem, ".tex")); written <- c(written, paste0(stem, ".tex"))
  }
  invisible(written)
}

#' Stack outcome tables across runs
#' @param runs Character vector of run ids or names.
#' @param root Project root.
#' @return data.table with `run_id`, `name`, design/cohort and the outcome table rows.
#' @export
compare_runs <- function(runs, root = NULL) {
  rows <- lapply(runs, function(r) {
    run <- run_load(r, root); f <- file.path(run$dir, "tables", "outcome_table.csv")
    if (!file.exists(f)) return(NULL)
    cbind(data.table::data.table(run_id = run$id, name = run$name, design = run$spec$sample$design,
                                 cohort = run$spec$sample$cohort, L = run$spec$sample$n_lags,
                                 match_end = run$spec$sc$match_end, n_match = length(run$spec$sc$match_outcomes)),
          data.table::fread(f))
  })
  data.table::rbindlist(rows)
}

utils::globalVariables(c("q025", "q975", "sig_pos", "sig_neg", "sig", "family", "p_approx", "q_bh"))
