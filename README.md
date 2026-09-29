# budgetsc

Reproducible pipeline for the digital-budgeting / financial-wellbeing study
(synthetic control via `augMultiSynth`, Bayesian post-estimation via
`scmBayesPost`). See `dev/design_sketch.md` for the architecture.

## Install on a VM (no GitHub access)

```r
# on your machine
devtools::build()                      # -> ../budgetsc_0.1.0.tar.gz
# copy the tarball to the shared filesystem, then on the VM
install.packages("budgetsc_0.1.0.tar.gz", repos = NULL, type = "source")
```

## Phase 0 quick start

```r
library(budgetsc)
options(budgetsc.root = "/nfs/project")     # holds Processed/ and Runs/
timeline_table()                            # calendar anchors and week indices
run <- run_register(spec_main())            # Runs/<id>/spec.yml, registry row
run_status()
sim <- sim_panel(n_treated = 300, n_later = 400, n_never = 200)   # synthetic data
```

Model stages (`fit_sc()`, `fit_post()`, `summarise_run()`) follow in Phase 1.
