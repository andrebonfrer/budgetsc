# demo_phase0.R — exercises everything in Phase 0 on synthetic data.
# Run from the package root after install.packages(<tarball>) or devtools::load_all().
library(budgetsc)
root <- file.path(tempdir(), "bsc_demo"); dir.create(root, showWarnings = FALSE)
options(budgetsc.root = root)

# 1. calendar anchors
print(bsc_timeline())

# 2. a few specs and their ids
s_main  <- spec_main()
s_gap   <- spec_main("sc.match_end" = 4L)            # held-out validation window
s_pilot <- spec_pilot()
print(s_main); cat("gap id:", spec_id(s_gap), " pilot id:", spec_id(s_pilot), "\n")

# 3. register runs (idempotent) and look at status
runs <- lapply(list(s_main, s_gap, s_pilot), run_register)
print(run_status())

# 4. synthetic panel with a dip mechanism
sim <- sim_panel(n_treated = 200, n_later = 300, n_never = 100, phi = 0.8, dip_selection = 2,
                 keep_latent = TRUE, seed = 7)
print(sim$customers[, .N, by = role])
print(sim$panel[, .(mean_spend = mean(spend_Groceries), arrears = mean(numarrears)), by = wID][c(1, 70, 110)])

# 5. a worker with a dummy job function (Phase 1 supplies fit_post_one)
dummy <- function(run, job, lock) {
  lock_heartbeat(lock)
  saveRDS(list(job = job, note = "placeholder"), file.path(run$dir, "post", paste0(job, ".rds")))
}
print(worker(dummy, once = TRUE))
print(run_status())
