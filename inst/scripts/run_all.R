# run_all.R — the one script that defines which runs exist. Idempotent.
#   Rscript inst/scripts/run_all.R           # registers runs
#   Rscript inst/scripts/worker.R sc post    # on each VM: claims and runs jobs
# then, per run: summarise_run(run_load("<name>")) and the an_*() analyses.
suppressPackageStartupMessages(library(budgetsc))
options(budgetsc.root = Sys.getenv("BUDGETSC_ROOT", unset = getwd()))

main   <- spec_main()                                              # later adopters, public, L=70, wide matching
main23 <- spec_main("sample.n_lags" = 23L); main23$name <- "main_L23"

specs <- list(
  main,
  main23,
  spec_lean(),                                                     # lean matching, L=70
  spec_modify(spec_main("sc.match_end" = 4L),  "name" = "main_heldout4"),          # held-out validation
  spec_modify(spec_main("sc.match_end" = 12L), "name" = "main_heldout12"),
  spec_placebo_shift(main23),                                      # onsets moved 41 weeks earlier
  spec_placebo_never(main23),                                      # never-onboarders, pseudo onsets
  spec_modify(spec_main("derive.distress" = list(shock_vars = "numarrears", TS = 3, rule = "any"),
                        "sample.subset" = list(var = "distress", value = 1L)), "name" = "main_distress"),
  spec_modify(spec_main("derive.distress" = list(shock_vars = "numarrears", TS = 3, rule = "any"),
                        "sample.subset" = list(var = "distress", value = 0L)), "name" = "main_nodistress"),
  spec_modify(spec_main("sample.design" = "partial_onboarders",
                        "sample.donor_sampling" = list(method = "stratified", target = 4000L, seed = 1L)),
              "name" = "donor_partial_onboarders"),
  spec_modify(spec_main("sample.design" = "non_onboarders",
                        "sample.donor_sampling" = list(method = "stratified", target = 4000L, seed = 1L)),
              "name" = "donor_non_onboarders"),
  spec_pilot()
)
runs <- lapply(specs, run_register)
print(run_status())
