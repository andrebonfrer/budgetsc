# worker.R — start on every VM that should help.
#   BUDGETSC_ROOT=/nfs/project Rscript inst/scripts/worker.R post
suppressPackageStartupMessages(library(budgetsc))
options(budgetsc.root = Sys.getenv("BUDGETSC_ROOT", unset = getwd()))
stages <- commandArgs(trailingOnly = TRUE)
if (!length(stages)) stages <- "post"

res <- worker(job_fun_default(), stages = stages, once = FALSE, sleep = 120)
print(res)
