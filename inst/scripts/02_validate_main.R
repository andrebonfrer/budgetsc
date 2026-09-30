# 02_validate_main.R — first real-data run: the main specification with a short
# chain, to compare against the current results before anything else.
suppressPackageStartupMessages({library(budgetsc); library(data.table)})
options(budgetsc.root = Sys.getenv("BUDGETSC_ROOT", unset = getwd()))
if (!requireNamespace("Matrix", quietly = TRUE)) stop("Matrix not installed")
library(Matrix)   # workaround until augMultiSynth >= 0.3.6 (fit_weights_fw crossprod bug)

spec <- spec_main("post.gibbs.n_iter" = 300L, "post.gibbs.burn_in" = 100L)
spec$name <- "validate_main_short"
run <- run_register(spec)
run_sample(run);   print(fread(file.path(run$dir, "tables", "sample_funnel.csv")))
run_sc(run)
print(fread(file.path(run$dir, "sc_diag", "fit_summary.csv"))[outcome %in% c("numarrears", "lengtharrears", "total_spend", "weekly_income")])
run_post(run)
tabs <- summarise_run(run)
print(tabs$outcome_table[, .(outcome, n_treated, ate_mean, ate_q025, ate_q975, median_tau_unit, pct_sig_pos, pct_sig_neg)])
print(run_status())
