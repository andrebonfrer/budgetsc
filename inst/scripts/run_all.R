# run_all.R — the one script that defines which runs exist.
# Register every specification here; workers on any VM then pick up the work.
#
#   Rscript inst/scripts/run_all.R        # registers runs (idempotent)
#   Rscript inst/scripts/worker.R         # on each VM: claims and runs jobs
#
# Phase 0: registration only (model stages arrive in Phase 1).
suppressPackageStartupMessages(library(budgetsc))
options(budgetsc.root = Sys.getenv("BUDGETSC_ROOT", unset = getwd()))

specs <- list(
  spec_main(),                                                    # later adopters, public, L=70
  spec_pilot(),                                                   # later adopters, pilot, L=23
  spec_main("sc.match_end" = 4L),                                 # held-out validation window t=-4..-1
  spec_modify(spec_main(), "sample.design" = "partial_onboarders",
              "sample.donor_sampling" = list(method = "stratified", target = 4000L, seed = 1L)),
  spec_modify(spec_main(), "sample.design" = "non_onboarders",
              "sample.donor_sampling" = list(method = "stratified", target = 4000L, seed = 1L))
)
for (i in seq_along(specs)) if (specs[[i]]$name == "unnamed" || i > 2)
  specs[[i]]$name <- paste0(specs[[i]]$sample$design, "_", specs[[i]]$sample$cohort,
                            "_L", specs[[i]]$sample$n_lags, "_me", specs[[i]]$sc$match_end)

runs <- lapply(specs, run_register)
print(run_status())
