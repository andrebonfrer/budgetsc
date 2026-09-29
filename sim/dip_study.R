# dip_study.R — Ashenfelter-dip simulation for the SC + post pipeline.
# Not part of the package build (excluded via .Rbuildignore); source it with
# the package installed. Bias = estimated ATE − true effect; the main grid sets
# every true effect to zero so any non-zero ATE is bias.
#
#   source("sim/dip_study.R")
#   res <- dip_study(quick = TRUE)          # small grid, minutes
#   res <- dip_study(reps = 5)              # full grid, hours; run on a VM
#   dip_study_summary(res)
suppressPackageStartupMessages({library(budgetsc); library(data.table)})

dip_grid <- function(quick = FALSE) {
  if (quick) data.table::CJ(phi = c(0, 0.9), dip_selection = c(0, 3), L = c(8L, 23L),
                            n_later = 150L, matching = c("lean", "wide"), match_end = c(0L, 4L))
  else data.table::CJ(phi = c(0, 0.5, 0.9), dip_selection = c(0, 1.5, 3), L = c(8L, 23L, 70L),
                      n_later = c(100L, 500L), matching = c("single", "lean", "wide"), match_end = c(0L, 4L))
}

#' One cell: simulate, fit SC, return bias by outcome and the held-out gap
dip_cell <- function(cell, rep, n_treated = 100L, n_weeks = 200L, root) {
  set.seed(1000L * rep + as.integer(cell$L) + 7L * cell$match_end)
  zero <- list(spend_log = 0, arrears_log = 0, arrears_length = 0, share_FastFood = 0, share_Alcohol = 0, share_Gambling = 0)
  sim <- sim_panel(n_treated = n_treated, n_later = cell$n_later, n_never = 0, n_weeks = n_weeks,
                   phi = cell$phi, dip_selection = cell$dip_selection, type_selection = 1,
                   effects = zero, seed = 1000L * rep + 1L)
  dir.create(file.path(root, "Processed"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(sim$panel, file.path(root, "Processed", "analysis_panel.rds"))
  mo <- switch(cell$matching, single = "total_spend", lean = match_outcomes_lean(), wide = match_outcomes_wide())
  spec <- spec_main("sample.n_lags" = as.integer(cell$L), "sc.match_outcomes" = mo,
                    "sc.match_end" = as.integer(cell$match_end), "sc.parallel" = FALSE,
                    "post.outcomes" = c("numarrears", "total_spend", "liquidity_deficit_rate"))
  spec$name <- sprintf("dip_phi%s_dip%s_L%d_n%d_%s_me%d_r%d", cell$phi, cell$dip_selection, cell$L,
                       cell$n_later, cell$matching, cell$match_end, rep)
  run <- run_register(spec, root)
  run_sample(run, root); fit_sc(run); d <- sc_diagnostics(run)
  et <- sc_event_time_ate(readRDS(file.path(run$dir, "sc_fit.rds")))
  ate <- et[tau_k >= 0, .(ate = mean(mean)), by = outcome]
  early <- et[tau_k >= 0 & tau_k < 8, .(ate_first8 = mean(mean)), by = outcome]
  held <- d$fit_summary[window == "heldout_pre", .(outcome, heldout_gap = mean_gap)]
  out <- merge(ate, early, by = "outcome"); out <- merge(out, held, by = "outcome", all.x = TRUE)
  cbind(as.data.table(cell), rep = rep, out)
}

dip_study <- function(quick = FALSE, reps = if (quick) 1L else 5L, root = file.path(tempdir(), "dip_study"),
                      out_file = "dip_study_results.csv") {
  grid <- dip_grid(quick)
  res <- list()
  for (i in seq_len(nrow(grid))) for (r in seq_len(reps)) {
    cell <- grid[i]
    res[[length(res) + 1L]] <- tryCatch(dip_cell(cell, r, root = root),
      error = function(e) cbind(as.data.table(cell), rep = r, outcome = NA_character_, ate = NA_real_,
                                ate_first8 = NA_real_, heldout_gap = NA_real_, error = conditionMessage(e)))
    cat(sprintf("[%d/%d rep %d] %s\n", i, nrow(grid), r, paste(names(cell), unlist(cell), sep = "=", collapse = " ")))
    unlink(list.files(file.path(root, "Runs"), full.names = TRUE), recursive = TRUE)   # housekeeping
  }
  out <- rbindlist(res, fill = TRUE)
  fwrite(out, out_file)
  out
}

#' Bias (true effect is zero) by factor; the dip signature is bias in the first
#' post weeks (ate_first8) that exceeds the full-window bias, and a non-zero
#' held-out gap when match_end > 0.
dip_study_summary <- function(res) {
  res[!is.na(ate), .(bias = mean(ate), bias_first8 = mean(ate_first8), rmse = sqrt(mean(ate^2)),
                     heldout_gap = mean(heldout_gap, na.rm = TRUE), n = .N),
      by = .(outcome, phi, dip_selection, L, matching, match_end)][order(outcome, phi, dip_selection, L)]
}
