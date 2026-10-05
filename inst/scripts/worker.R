# worker.R — start one per VM. Claims SC fits and per-outcome Gibbs jobs
# through the lock protocol and runs them until nothing is pending.
#
#   RStudio: Tools > Background Jobs > Start Background Job, script = this file
#   Terminal: BUDGETSC_ROOT=/nfs/project nohup Rscript worker.R sc post > worker.log 2>&1 &
#
# Stages: command-line arguments if given, else BUDGETSC_STAGES (comma-separated),
# else "sc,post". Project root: option budgetsc.root, else BUDGETSC_ROOT, else getwd().
# one BLAS/OpenMP thread per worker: several workers on one machine otherwise
# oversubscribe the cores (load average of ~4x the core count), and the SC
# stage parallelises by forking, which wants single-threaded BLAS anyway
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
if (requireNamespace("RhpcBLASctl", quietly = TRUE)) RhpcBLASctl::blas_set_num_threads(1)
suppressPackageStartupMessages(library(budgetsc))
if (requireNamespace("Matrix", quietly = TRUE)) library(Matrix)   # augMultiSynth < 0.3.6 workaround
root <- Sys.getenv("BUDGETSC_ROOT", unset = getwd())
options(budgetsc.root = root)
args <- commandArgs(trailingOnly = TRUE)
stages <- if (length(args)) args else {
  s <- Sys.getenv("BUDGETSC_STAGES", unset = "sc,post"); trimws(strsplit(s, ",")[[1]])
}
cat(sprintf("[%s] worker on %s | root %s | stages %s\n", format(Sys.time()), host_id(), root, paste(stages, collapse = ",")))
res <- worker(job_fun_default(), stages = stages, once = FALSE, sleep = 120)
print(res)
