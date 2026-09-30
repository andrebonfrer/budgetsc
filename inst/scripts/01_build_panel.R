# 01_build_panel.R — run ONCE (and again only if the processed inputs change).
# Assembles Processed/analysis_panel.rds from the processed weekly panel,
# the salary transactions and the weekly balances. The only step that reads data/.
suppressPackageStartupMessages(library(budgetsc))
options(budgetsc.root = Sys.getenv("BUDGETSC_ROOT", unset = getwd()))
print(timeline_table())

build_analysis_panel(
  panel_file   = "Processed/BudgetPanelDataWeekly_with_donor1.rds",
  income_file  = "data/commincome_fncl_tran_slry.rds",
  balance_file = "Processed/weekly_balances_without_homeloans.rds",
  out_file     = "Processed/analysis_panel.rds",
  week_alignment = "containing"          # "legacy_shift" reproduces the old one-week-early income merge
)
