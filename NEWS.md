# budgetsc 0.4.3

* Post-estimation jobs are `blocked` (not claimable) until the run's SC stage
  has written `sc_fit.rds` and `panel.rds`. Previously a second machine could
  claim an outcome while the first was still fitting the synthetic control and
  fail with "cannot open connection".
* `job_reset_failed(run)` clears `.failed` markers after the cause is fixed;
  `run_status()` gains a `post_failed` column.
* `worker.R` serves `sc,post` by default (or `BUDGETSC_STAGES`), attaches
  Matrix, and prints host/root/stages on start.

# budgetsc 0.4.2

* `build_analysis_panel()`: the tier-A build step (processed panel + weekly
  salary income + weekly balances -> `Processed/analysis_panel.rds`), the only
  function that reads `data/`. Income is assigned to the containing week with
  `week_of()`; the original `post_reg` merge omitted the `+ 1` and placed each
  week's income one row early (`week_alignment = "legacy_shift"` reproduces
  it for comparison). A `build_info` attribute records inputs, MD5s and counts.

# budgetsc 0.4.1

* `derive_outcomes()`: rolling-window calls pass the window positionally, so
  data.table >= 1.17 (which renamed `frollapply(n=)` to `N=`) no longer warns
  once per customer; `sd(na.rm = TRUE)` is wrapped instead of passed via `...`.
* `test-locks.R`: the two-process contention test uses the running R's own
  `Rscript`, passes `.libPaths()` to the children, and skips with the child's
  error message if `budgetsc` is not installed for them.

# budgetsc 0.4.0 (Phase 2: identification checks)

* Placebo designs: `design = "placebo_dates"` with `sample.placebo` —
  `units = "treated"` shifts every adopter's onset `shift` weeks earlier
  (must exceed H so the placebo window ends before real adoption);
  `units = "never_onboarders"` gives never-onboarders pseudo onsets drawn from
  real adopters. Helpers `spec_placebo_shift()`, `spec_placebo_never()`;
  `an_placebo_table()` stacks real and placebo outcome tables with the
  unit-level significance split.
* `flag_distress()` / `distress_counts()` are in the package; `derive.distress`
  in the spec applies the classification in `run_sample()` (a temporary
  containing-week treatment indicator is used because `budgetdummy` is created
  later by `define_sample()`), and `sample.subset = list(var = "distress",
  value = 1)` restricts the treated group.
* `sc_diagnostics()` also writes `sc_diag/unit_gaps.csv` (per-unit mean gaps in
  the matched, held-out and post windows); `an_heldout_validation()` tests the
  held-out gap across units and reports RMSPE ratios, including for holdout
  (placebo) outcomes.
* `an_dv_correlations()`: pre-treatment correlations among the FWB outcomes at
  customer-week and customer-mean level.
* `sim/dip_study.R` (outside the build): the Ashenfelter-dip grid over dip
  persistence, selection on the dip, L, donors, matching set and held-out
  window, with all true effects zero. A quick 8-cell run with the real SC
  backend shows the signature: with persistent shocks and strong selection on
  them, the first-8-week bias and the held-out gap are large and positive while
  the 41-week bias shrinks with L.
* `sample.treat_start` defaults to `"containing_week"` (confirmed).

# budgetsc 0.3.0 (Phase 1b: model stages)

* Verified end to end against the real `augMultiSynth` (0.3.5.9000, from
  GitHub) and `scmBayesPost` (0.4.6) on the synthetic panel; the integration
  test `test-real-backends.R` runs wherever both packages are installed.
* Backends now follow augMultiSynth's own fit structure (`treated_unit_ids`,
  `donor_ids`, `weights` as a list) plus a derived `weights_mat`; the stub
  produces the same fields, so `scmBayesPost::build_W_from_augMultiSynth()`
  accepts either. `sc.lambda` (ridge penalty, default 1e-3) and `sc.verbose`
  / `post.verbose` added; the augMultiSynth call passes `parallel_backend` /
  `n_cores` only when the installed version has them.
* Found and patched a bug in `augMultiSynth::fit_weights_fw()`: `Q` was a
  `Matrix::Diagonal()` object and `crossprod(d, Q %*% d)` dispatched to
  `base::crossprod` (the namespace imports nothing from Matrix), which fails
  in any session where Matrix is not attached. See
  `augMultiSynth_fit_weights_fw.patch`.

* `define_sample()` — designs `later_adopters` (public and pilot cohorts),
  `partial_onboarders`, `non_onboarders`; the pre-launch filters; stratified
  or random donor sampling; optional treated subset; a sample funnel recorded
  step by step. New `sample.treat_start` setting makes the SC treatment week
  and `budgetdummy` follow one convention (`containing_week`, default) — the
  scripts used the containing week for the SC but the first full week for
  `budgetdummy`.
* `fit_sc()` with two backends: `augMultiSynth::multiout_synth()` (default)
  and a stub (equal donor weights) for dry runs and tests. `match_end > 0`
  holds out the last pre-treatment weeks from matching. `sc_diagnostics()`
  writes pre-treatment fit and held-out gaps for matched AND holdout (placebo)
  outcomes; `sc_event_time_ate()` gives Stage 1 gaps by event week.
* `fit_post_one()` / `fit_post()` — one job per outcome through the lock
  protocol; `scmBayesPost` backend transcribed from `post_reg_*`, plus a stub.
  Results store `gdata_light`, not the full `gdata`.
* `summarise_run()` — outcome table, gamma table with BH q-values within spec
  families, unit effects, event-time ATE; CSV always, HTML via gt when
  installed, LaTeX via a base writer. `compare_runs()` stacks outcome tables.
* `run_sample()`, `run_sc()`, `run_post()`, `run_pipeline()`; `worker()` now
  serves real jobs through `job_fun_default()`.
* `load_panel()` uses `setattr()` so downstream by-reference updates work;
  `derive_*()` refuse a non-over-allocated data.table with a clear message.

# budgetsc 0.2.0 (Phase 1a: data layer)

* `load_panel()` reads the processed panel for a spec, asserts the week origin
  against `timeline.yml` (no run-time offset inference), applies the standard
  recodes, and records the panel file's MD5 for provenance.
* `derive_outcomes()` and `derive_moderators()` are the single home for every
  derived variable (wallet shares, focal spend, `total_spend`, `cv_2020_spend`,
  the cash-flow family, splines, category-ever dummies); `data_dictionary()`
  lists them with definitions.
* Focal spending categories are named (`FOCAL_SPEND_CATEGORIES`, eleven
  categories confirmed 30 Sep 2026) rather than selected by position.
* `cv_basis` makes explicit that the scripts computed `cv_2020_spend` from all
  spend categories; `"focal"` is available.
* Spec: `derive` section; `sc.match_outcomes` defaults to the wide
  `match_outcomes_wide()` set; new `sc.holdout_outcomes`
  (`weekly_income`, `monthly_loan_payment_w`) reserved for placebo and balance
  tests; `spec_validate()` rejects any overlap and requires the screen outcome
  to be matched. `spec_lean()` and `match_outcomes_lean()` give the narrow
  variant.

# budgetsc 0.1.0 (Phase 0)

Infrastructure only; no model stages yet.

* `timeline.yml` is the single source of truth for calendar anchors; `week_of()`
  and `first_week_on_or_after()` are the two week rules (containing week vs first
  Monday on/after). Origin 2020-01-06; pilot start wID 30; public launch wID 70.
* Specs (`spec_default()`, `spec_main()`, `spec_pilot()`, `spec_modify()`,
  `spec_validate()`), deterministic run ids (`spec_id()`), YAML I/O.
* Registry and run directories (`run_register()`, `run_load()`, `run_status()`,
  `run_clean()`, stage markers) under `<root>/Runs/`.
* Directory locks for NFSv4 with heartbeat and stale takeover (`lock_*()`,
  `with_lock()`); job claiming and a `worker()` loop.
* `sim_panel()`: latent-factor synthetic panel with type selection, dip
  selection (AR(1) transitory shock), pilot/public/later/partial/never roles,
  and known treatment effects, using the real panel's column names.
* Tests for all of the above, including a two-process lock contention test.
