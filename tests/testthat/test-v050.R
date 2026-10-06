setup_v050 <- function(seed = 5, ...) {
  root <- tempfile(); dir.create(file.path(root, "Processed"), recursive = TRUE)
  options(budgetsc.root = root)
  sim <- sim_panel(seed = seed, ...)
  saveRDS(sim$panel, file.path(root, "Processed", "analysis_panel.rds"))
  list(root = root, sim = sim)
}
stub50 <- function(...) spec_main("sc.backend" = "stub", "post.backend" = "stub", "sample.n_lags" = 23L,
                                  "post.gibbs.n_iter" = 100L, "post.gibbs.burn_in" = 20L, ...)
contam_weight <- function(run) {                       # weight on donors that adopt inside each treated unit's post window
  sc <- readRDS(file.path(run$dir, "sc_fit.rds")); Wm <- sc$fit$weights_mat; ids <- readRDS(file.path(run$dir, "sample.rds"))$ids
  don <- ids$real_onset_wID[match(colnames(Wm), as.character(ids$customer_id))]
  t0 <- sc$treat_time[match(rownames(Wm), as.character(sc$unit_vals))]; H <- run$spec$sample$n_leads
  vapply(seq_len(nrow(Wm)), function(j) sum(Wm[j, ] * (!is.na(don) & don > t0[j] & don <= t0[j] + H)), numeric(1))
}

test_that("defaults: per-unit eligibility, a minimum donor count, gap backend; old specs fall back to the old behaviour", {
  s <- spec_main()
  expect_equal(s$sc$donor_eligibility, "per_unit"); expect_equal(s$sc$min_eligible_donors, 20L)
  expect_equal(s$post$backend, "scmbayes_gap"); expect_null(s$sc$from)
  expect_error(spec_main("sc.donor_eligibility" = "everyone"), "donor_eligibility")
  expect_error(spec_main("sc.from" = c("a", "b")), "sc.from")
  setup_v050(n_treated = 30, n_later = 60, n_never = 0)
  sp <- stub50(); sp$sc$donor_eligibility <- NULL; sp$sc$min_eligible_donors <- NULL      # as registered before 0.5.0
  run <- run_register(sp); run_sample(run); fit_sc(run)
  expect_true(any(grepl("eligibility=global", readLines(file.path(run$dir, "run.log")))))
  expect_equal(readRDS(file.path(run$dir, "sc_fit.rds"))$donor_eligibility, "global")
})

test_that("per-unit eligibility puts no weight on donors adopting inside a treated unit's post window; global does", {
  setup_v050(n_treated = 60, n_later = 70, n_never = 0)
  per <- run_register(stub50("post.outcomes" = "numarrears")); run_sample(per); fit_sc(per)
  glo <- run_register(spec_modify(stub50("post.outcomes" = "numarrears"), "sc.donor_eligibility" = "global")); run_sample(glo); fit_sc(glo)
  expect_true(all(contam_weight(per) == 0))
  expect_true(any(contam_weight(glo) > 0))
  # treated units without enough eligible donors are set aside, listed and not fitted
  f <- file.path(per$dir, "tables", "sc_units_without_donors.csv")
  expect_true(file.exists(f))
  out <- data.table::fread(f); expect_true(all(out$eligible_donors < 20L))
  sc <- readRDS(file.path(per$dir, "sc_fit.rds")); smp <- readRDS(file.path(per$dir, "sample.rds"))
  expect_equal(length(sc$treated_idx) + nrow(out), sum(smp$ids$role == "treated"))
  expect_false(any(out$customer_id %in% sc$fit$treated_unit_ids))
})

test_that("the post stage leaves out treated units without an SC fit and says so", {
  setup_v050(n_treated = 60, n_later = 80, n_never = 0, seed = 7)
  run <- run_register(stub50("post.outcomes" = c("numarrears", "weekly_income")))
  run_sample(run); fit_sc(run); sc_diagnostics(run)
  n_set_aside <- nrow(data.table::fread(file.path(run$dir, "tables", "sc_units_without_donors.csv")))
  expect_gt(n_set_aside, 0L)
  for (o in run$spec$post$outcomes) expect_error(fit_post_one(run, o), NA)       # includes an unmatched outcome (weekly_income)
  summarise_run(run)
  ot <- data.table::fread(file.path(run$dir, "tables", "outcome_table.csv"))
  n_tr <- readRDS(file.path(run$dir, "sample.rds"))$ids[role == "treated", .N]
  expect_equal(unique(ot$n_treated), n_tr - n_set_aside)
  expect_true(any(grepl("without an SC fit left out", readLines(file.path(run$dir, "run.log")))))
})

test_that("sc.from reuses a finished SC fit and refuses a mismatched one", {
  setup_v050(n_treated = 40, n_later = 100, n_never = 0, seed = 9)
  a <- run_register(stub50("post.outcomes" = "numarrears")); run_sample(a); fit_sc(a); sc_diagnostics(a)
  sp <- spec_modify(stub50("post.outcomes" = "numarrears"), "sc.from" = a$id, "post.f_Z" = "budgetdummy ~ 1")
  b <- run_register(sp)
  expect_error(fit_sc(b), "sc.from")
  run_sample(b)                                                              # copies, does not refit
  expect_true(stage_done(b, "sample")); expect_true(stage_done(b, "sc"))
  expect_identical(readRDS(file.path(b$dir, "sc_fit.rds"))$fit$weights_mat, readRDS(file.path(a$dir, "sc_fit.rds"))$fit$weights_mat)
  expect_true(file.exists(file.path(b$dir, "sc_diag", "pre_slope.csv")))
  expect_true(any(grepl("reused the SC fit", readLines(file.path(b$dir, "run.log")))))
  fit_post_one(b, "numarrears"); expect_true(file.exists(file.path(b$dir, "post", "numarrears.rds")))
  c1 <- run_register(spec_modify(stub50("post.outcomes" = "numarrears"), "sc.from" = a$id, "sample.n_lags" = 30L))
  expect_error(run_sample(c1), "differs from this run in: sample")
  d1 <- run_register(spec_modify(stub50("post.outcomes" = "numarrears"), "sc.from" = "0000000000"))
  expect_error(run_sample(d1), "No registered run")
  # a worker serves a sc.from run through the default job function
  e <- run_register(spec_modify(stub50("post.outcomes" = "numarrears"), "sc.from" = a$id, "post.f_Z" = "budgetdummy ~ age"))
  out <- worker(job_fun_default(), stages = c("sc", "post"), runs = e$id, once = TRUE)
  expect_true(all(out$ok)); expect_true(stage_done(e, "post"))
})

test_that("gap_ate equals the Stage 1 summary; pre-trend table has the expected columns", {
  setup_v050(n_treated = 50, n_later = 120, n_never = 0, seed = 11)
  run <- run_register(stub50("post.outcomes" = "numarrears")); run_sample(run); fit_sc(run); d <- sc_diagnostics(run)
  s1 <- d$fit_summary[outcome == "numarrears" & window == "post", mean_gap]
  g <- gap_ate(run, "numarrears")
  expect_equal(g$ate, s1, tolerance = 1e-8)
  expect_true(all(c("n_units", "ate", "se_across_units", "median_unit", "pct_pos", "pct_neg") %in% names(g)))
  g2 <- gap_ate(run, "liquidity_deficit_rate")                              # not in the matching set: still computable
  expect_true(is.finite(g2$ate))
  expect_true(file.exists(file.path(run$dir, "sc_diag", "pre_slope.csv")))
  pt <- an_pretrend(run)
  expect_true(all(c("outcome", "mean_slope_per_week", "se", "t", "trend_implied_ate", "ate", "effect_minus_trend") %in% names(pt)))
  expect_true("numarrears" %in% pt$outcome)
})

test_that("placebo_set registers runs with no post outcomes and an_placebo_distribution summarises them", {
  setup_v050(n_treated = 40, n_later = 60, n_never = 220, seed = 13)
  base <- stub50("post.outcomes" = c("numarrears", "total_spend"))
  ids <- placebo_set(base, seeds = 1:3)
  expect_length(unique(ids), 3L)
  for (id in ids) { r <- run_load(id); expect_length(r$spec$post$outcomes, 0L); expect_equal(nrow(job_table(r, "post")), 0L)
                    run_sample(r); fit_sc(r) }
  expect_true(all(vapply(ids, function(i) stage_done(run_load(i), "post"), logical(1))))     # nothing to wait for
  real <- run_register(base); run_sample(real); fit_sc(real)
  pd <- an_placebo_distribution(real$id, ids)
  expect_equal(sort(pd$outcome), c("numarrears", "total_spend"))
  expect_true(all(pd$n_placebos == 3L))
  expect_true(all(c("ate", "placebo_mean", "placebo_sd", "adjusted", "z", "p_empirical") %in% names(pd)))
  expect_true(all(pd$p_empirical >= 1 / 4 & pd$p_empirical <= 1))
  expect_true(nrow(run_status()) >= 4L)                                      # status works with zero-outcome runs present
})

test_that("real backends: per-unit eligibility removes contamination (augMultiSynth with treated_units)", {
  skip_if_not_installed("augMultiSynth"); skip_if_not_installed("scmBayesPost"); skip_on_cran()
  setup_v050(n_treated = 40, n_later = 150, n_never = 0, seed = 3)
  mk <- function(el) { sp <- spec_main("sample.n_lags" = 23L, "sc.parallel" = FALSE, "post.outcomes" = "numarrears", "sc.donor_eligibility" = el)
                       r <- run_register(sp); run_sample(r); fit_sc(r); r }
  per <- mk("per_unit"); glo <- mk("global")
  expect_true(all(contam_weight(per) == 0))
  expect_true(any(contam_weight(glo) > 0.05))
  expect_gt(length(readRDS(file.path(per$dir, "sc_fit.rds"))$treated_idx), 10L)
})


test_that("an_donor_contamination: positive for global donors, zero for per-unit, by adoption week", {
  setup_v050(n_treated = 80, n_later = 150, n_never = 0, seed = 15)
  glo <- run_register(spec_modify(stub50("post.outcomes" = "numarrears"), "sc.donor_eligibility" = "global")); run_sample(glo); fit_sc(glo)
  per <- run_register(stub50("post.outcomes" = "numarrears")); run_sample(per); fit_sc(per)
  cg <- an_donor_contamination(glo); cp <- an_donor_contamination(per)
  expect_true(all(c("adoption_week", "units", "mean_weight_on_contaminated_donors", "share_units_any_contamination") %in% names(cg)))
  expect_true(all(cp$mean_weight_on_contaminated_donors == 0))
  expect_gt(max(cg$mean_weight_on_contaminated_donors), 0.1)
  expect_gt(cg$mean_weight_on_contaminated_donors[nrow(cg)], cg$mean_weight_on_contaminated_donors[1])   # grows with adoption week
  expect_equal(sum(cg$units), nrow(readRDS(file.path(glo$dir, "sc_fit.rds"))$fit$weights_mat))
})
