# `budgetsc` — design sketch for the revision analysis package

*Draft for approval, 28 September 2026. Nothing below is built yet.*

---

## 0. What I read, and what it implies

The R0 code has one pipeline repeated with small parameter changes:

| Stage | Files | What varies across copies |
|---|---|---|
| Data build | `CreateBudget.R`, `CreatePanel.R`, `CreateHomeloan.R` | nothing (one-off builds of `Processed/*.rds`) |
| Sample + SC | `runSynth_augMultiSynth*.R` (5 copies) | donor pool (partial onboarders / non-onboarders / later adopters / cohort-matched), pilot vs public, filters (`pct_income_thresh`, `N_budgets_min`, `min_cats`), donor sampling (stratified 4k / `max_donors`), budget-ratio split, cohort window (`robust1`), output file name |
| Post-SC | `post_reg_augMultisynth*.R` (9 copies) | which fit file, which outcomes, moderator formula (`fZ` vs `fZ_full` with category dummies), output prefix |
| Summaries | `WriteResults*.R` (10 copies) | file-name pattern, outcome subset, output prefix |
| Diagnostics | `CommonTrends.R`, `BalanceDiagnostics.R`, `weight_diagnostics.R`, `postfit_diagnostics*.R`, `summary_fit.R` | fit file |
| Helpers | `utils.R`, `utils-paper.R`, `scenario_gamma.R`, `descriptive_statistics.R` | — |

Three things are scattered and should be centralised:

1. **Derived variables** are created in three places: wallet shares and `total_spend` in `runSynth`, liquidity/income-share/cash-flow outcomes in `post_reg`, budget-proximity splines and `avg_transaction_size` in `post_reg` and again in `descriptive_statistics`. One function must own these so every stage sees the same definitions.
2. **Sample definition** (who is treated, who is a donor, which window) is interleaved with SC fitting. It needs to be its own object so the two-launch design, cohort matching, distress splits and placebo dates are all *sample definitions* fed to one SC function.
3. **Run identity** is encoded in file names (`fit_pilot_budgetsetters_robust1_cohort3_scmBayes_numarrears.rds`). It should be a spec object with a deterministic ID, so a run's inputs are recoverable and two VMs never collide.

The package wraps `augMultiSynth::multiout_synth()` and `scmBayesPost` (`build_W_from_augMultiSynth`, `prepare_data_general`, `gibbs_postscm`); it does not re-implement them.

---

## 1. Architecture

Seven layers, each a family of functions with one job. Data flows downward; nothing reaches back up.

```
L0  spec        analysis specification (what to run)            spec_*()
L1  data        Processed/*.rds  -> analysis panel + derived vars  build_*(), derive_*()
L2  sample      panel -> treated/donor sets + windows              define_sample()
L3  sc          sample -> Y_list -> multiout_synth fit + diagnostics  fit_sc(), sc_diagnostics()
L4  post        fit + panel -> per-outcome Gibbs results           fit_post()
L5  summarise   results -> tidy tables (CSV) -> gt/LaTeX           summarise_run(), tab_*()
L6  analyses    revision-specific analyses (hazard, placebo, ...)  an_*()
L7  jobs        registry, locking, workers, housekeeping           job_*(), run_*()
```

Plus `sim/`: a synthetic-data generator and the Ashenfelter-dip study, which exercise L2–L5 end to end.

### 1.1 Directory layout on the shared filesystem

```
<project root>/
  data-raw/            (untouched)
  Processed/           (untouched; read-only from the package's point of view)
  Runs/
    registry.csv       one row per run: run_id, name, created, spec_hash, status, host
    <run_id>/
      spec.yml         the spec that defines the run (human-readable)
      spec.rds
      sample.rds       define_sample() output (ids, windows, treat_time) — small
      panel.rds        the analysis panel for this run (the old bdt_*_temp.rds)
      sc_fit.rds       multiout_synth fit + Yprep (the old fit_*.rds)
      sc_diag/         pretrend, balance, weights: CSV + PNG
      post/
        <outcome>.rds  {post, gdata_light}
        <outcome>.lock/  mkdir-lock while a worker owns it (see §4)
        <outcome>.log
      tables/          CSV (raw) + HTML/TEX (formatted)
      figures/
      run.log
      provenance.rds   sessionInfo, package versions, package git SHA, timings
```

`Runs/<run_id>/` is the unit of housekeeping: delete the directory and the run is gone; nothing else refers to it except the registry row.

### 1.2 The spec object (L0)

A run is fully described by a nested list, stored as YAML so it is diff-able and readable by co-authors:

```yaml
name: main_budgetsetters
data:
  panel_file: Processed/BudgetPanelDataWeekly_with_donor1.rds
  first_date: 2020-01-01
  cutoff: 2022-12-31
  origin: 2020-01-06            # wID origin (confirm)
sample:
  design: later_adopters         # later_adopters | partial_onboarders | non_onboarders |
                                 # two_launch | cohort_window | placebo_dates
  cohort: public                 # public | pilot
  launch_date: 2021-05-01
  n_lags: 23                     # L (70 for public later-adopter design? confirm)
  n_leads: 40                    # H
  filters: {min_pre_weeks: 8, pct_income_thresh: 0, n_budgets_min: 1, min_cats: 10}
  donor_sampling: {method: none}   # or {method: stratified, target: 4000, seed: 1}
  subset: null                   # e.g. {var: distress, value: 1}
sc:
  match_outcomes: [numarrears, lengtharrears, Spend*, weekly_signins, ..., total_spend]
  match_end: 0                   # 0 = match through tau=-1; 4 = stop at tau=-5 (held-out t=-4..-1)
  max_donors: 1000
  screen_outcome: total_spend
  standardize_outcomes: true
  intercept: outcome
  solver: fw
  nu_scale: 0.1
post:
  outcomes: [numarrears, lengtharrears, liquidity_deficit_rate, income_share, ...]
  f_Z: "budgetdummy ~ budgetcategoriesN + factor(frequency) + age + ... + br_over"
  gibbs: {n_iter: 2000, burn_in: 1000}
  priors: {Sigma_gamma_prior: 1000, a_sigma_tau_prior: 5, b_sigma_tau_prior: 1}
  first_stage: none              # or selection_probit_bayes with instruments
tables:
  families: {fwb: [...], levels: [...], shares: [...]}   # for BH correction
```

`spec_main()`, `spec_pilot()`, `spec_two_launch()` etc. return pre-filled specs; `spec_modify(spec, sample.n_lags = 70)` makes variants. The run ID is `substr(digest(spec_without_name), 1, 10)`, so identical specs map to the same directory and a re-run resumes instead of duplicating.

This is what replaces "one script per data cut": one `run_all.R` holding a list of specs.

### 1.3 Data layer (L1)

`CreateBudget.R` / `CreatePanel.R` become `build_customer_budget()` and `build_weekly_panel()` — functions that read `data/` and write `Processed/`, run rarely, and are *not* part of a run. Their logic is kept as is (with the two known quirks flagged: `income_sd` imputed from `income_mean` in `CreateBudget`, and `numarrears`/`lengtharrears` averaged over days within a week).

Everything downstream is a run and starts from `Processed/`:

- `load_panel(spec)` — reads the panel file, restricts to `first_date`–`cutoff`, builds `tdate`/`wID`, recodes `age`, `state`, `customer_tenure`.
- `derive_outcomes(panel, spec)` — **the single home** for wallet shares, `total_spend` (winsorised focal categories), spending levels by category, `cv_2020_spend`, and the cash-flow family (`weekly_income`, `net_cashflow`, `cf_vol`, `income_share`, `liquidity_deficit`, `liquidity_deficit_rate`, `liquidity_buffer`) currently built in `post_reg`. Also the new ones the checklist needs: obligation-adjusted slack, dining-out-excluding-fast-food, spending levels for the seven focal categories.
- `derive_moderators(panel, spec)` — `homeloan_mean`, `br_under/target/over` splines, `numgoalcats`, category-ever dummies (`budgetever_*`), plus new: recent spending pressure `R_i` (3-month ÷ prior-12-month, seasonal and 6-month variants), budget ratio on 6- and 12-month baselines, spending-trajectory groups.
- `flag_distress(panel, ...)` — the function already written (arrears- and spending-based, with placebo support).

Each `derive_*` function is pure (panel in, panel out) and documented with the formula it implements; a `data_dictionary()` prints every derived column with its definition so the web appendix variable table can be generated rather than typed.

### 1.4 Sample layer (L2)

`define_sample(panel, spec)` returns a `budgetsc_sample`:

```
$ids        data.table(customer_id, role = treated|donor, cohort, treat_wID, onset_date)
$window     list(L, H, match_end, launch_wID, truncate_wID)
$filters    what was applied and how many customers each step removed (feeds Table "sample funnel")
$panel      the panel restricted to ids and window
```

Designs implemented as methods of one function:

| `design` | Treated | Donors | Source in R0 |
|---|---|---|---|
| `later_adopters` | adopters in [launch, launch + H] | adopters with onset > truncate date | `runSynth_*_budgetsetters` |
| `partial_onboarders` / `non_onboarders` | adopters | donor pool 1 / 2, stratified 4k | `runSynth_augMultiSynth` |
| `cohort_window` | adopters in a `numweeks` window `cohort` steps after launch | adopters with onset ≥ window end + H | `runSynth_*_robust1` |
| `two_launch` | pilot early adopters (relative week k) | public-launch early adopters (relative week k), pre-treatment + pilot-period data only | new (checklist §1) |
| `placebo_dates` | any design, with `treat_wID` shifted back by `shift` weeks | as design | new (checklist §4) |
| `subset` | any design restricted by a customer-level flag (`distress`, spending trajectory, budget ratio) | as design | `splitsample`, new |

`match_end` implements checklist §2: weights are fitted on lags `-L..-(match_end+1)` and lags `-match_end..-1` are held out. Both `pretrend_test()` and the post stage know about it.

### 1.5 SC layer (L3)

`fit_sc(sample, spec, out_dir)`:

1. `panel_to_Ylist()` (from `utils.R`) on `spec$sc$match_outcomes`.
2. `treat_time` from `sample$ids` (no more inferring the `wID` offset at run time — it is fixed in the spec and checked once).
3. `augMultiSynth::multiout_synth(...)` with the spec's options; `nu = nu_scale * H * J0`.
4. Save `sc_fit.rds` = `{fit, Yprep, treat_time, outcomes, sample_id}`; do **not** save the panel inside it (that is what made `fit_*.rds` large) — the panel is `panel.rds` alongside.

`sc_diagnostics(run)` wraps `pretrend_test()`, `summary_pretrend()`, `balance_table()`, `weight_diagnostics()` and writes CSVs + figures to `sc_diag/`. Two additions for the checklist: balance on outcomes *not* in `match_outcomes` (§1, §2), and the held-out window gap test when `match_end > 0`.

### 1.6 Post layer (L4)

`fit_post(run, outcomes = NULL, workers = 1)`:

- builds `W` once via `build_W_from_augMultiSynth()`;
- for each outcome: reload `panel.rds`, `prepare_data_general()` (list-based, so `X_block` is never materialised), scale non-dummy moderators, `gibbs_postscm()`, save `{post, gdata_light}` — exactly the current loop, but each outcome is a *job* claimed through the lock protocol (§4) so several VMs can share one run;
- `gdata_light` keeps only `Xcols, intX, J0, treated_ids, Z_block` (as `load_results_list_light` already assumes), which cuts the result files to a fraction of their current size;
- optional thinning of `beta_samples` at save time (`post.thin`), since 1,000 draws × 1,879 units is the bulk of each file.

The selection-equation option (`first_stage = selection_probit_bayes`, instruments from `build_iv_formula()`) is a spec switch, not a separate script.

### 1.7 Summaries (L5)

`summarise_run(run)` produces tidy data first, formatted tables second, so every table in the paper has a CSV twin:

| Output | Content | Paper location (to confirm) |
|---|---|---|
| `outcome_table.csv` | ATT mean/median/CI, dispersion, % sig ±, R² of moderators, per outcome | Table 2, Web Appendix outcome tables |
| `gamma_table.csv` / `gamma_wide.csv` | moderator coefficients with CIs, sig flags, BH q-values within family | Table 3, appendix |
| `unit_effects.csv` | posterior mean/CI per treated customer per outcome (the HTE distribution) | histograms; input to L6 analyses |
| `event_time_ate.csv` | Stage-1 τ by event time (from `fit$tau`) | pre/post trajectory plots (§3) |
| `scenario_effects.csv` | `scenario_gamma` marginal effects for named personas | text/table |
| `dv_correlations.csv` | correlation matrix of FWB outcomes (pre-treatment, treated units) | §5 |
| `sample_funnel.csv` | customers remaining after each filter, by cohort | §15 |
| `sc_diag/*.csv` | pretrend, balance, weight concentration | Web Appendix |

`tab_*()` functions turn each CSV into `gt` (HTML) and LaTeX with the paper's formatting (`fmt_ci`, significance stars, BH q-values). `compare_runs(run_ids)` stacks outcome tables across runs — this is how the robustness tables (pilot vs public, donor pools, cohorts, gap-matched, distress-excluded) are produced without another `WriteResults` copy.

### 1.8 Revision analyses (L6)

Each is a function taking a run (or the panel) and returning tidy data + a table, mapped to the checklist:

| Function | Checklist | Notes |
|---|---|---|
| `an_heldout_validation(run)` | §2 | gap in held-out lags; repeated for shorter windows / fewer match variables via spec variants |
| `an_placebo_dates(spec, shifts)` | §4 | runs `placebo_dates` samples through L2–L5; reports pseudo-ATTs |
| `an_placebo_outcomes(run)` | §4 | outcomes that should not respond (to be chosen) |
| `an_mean_reversion(run)` | §3 | effects excluding spending-spike / immediate-arrears customers; long trajectories; matched public adopters' pre-adoption spikes |
| `an_dv_correlations(run)` | §5 | |
| `an_multiple_testing(tables, families)` | §6 | BH within family; q-values added to every outcome/gamma table |
| `an_reallocation(run)` | §7 | levels and shares side by side; total vs reallocation; FWB effects by reallocation subgroup |
| `an_adoption_hazard(panel, spec)` | §8–9 | person-week risk set, `R_i`, arrears/deficit/mortgage predictors, `feglm`; predicted-probability plot |
| `an_hte_groups(run, grouping)` | §10–12 | effects by trajectory / pressure group with formal interaction tests (from posterior draws) |
| `an_configuration(panel)` | §13–14 | configuration as outcome; tightness splines with alternative baselines |
| `an_sample_selection(panel)` | §15–16 | retained vs excluded; adopters vs never-adopters; standardised differences |

### 1.9 Jobs (L7)

- `run_register(spec)` — creates `Runs/<run_id>/`, writes spec, appends registry row.
- `run_sc(run_id)`, `run_post(run_id, outcomes)`, `run_summarise(run_id)` — stage runners; each writes a `.done` marker.
- `run_pipeline(spec)` — all stages in sequence for one machine.
- `worker(queue = "post")` — loops over registered runs and unclaimed outcomes, claims one, runs it, releases; safe to start on every VM.
- `run_status()` — registry + lock scan: what is done, running (host), pending, stale.
- `run_clean(run_id, keep = c("post","tables"))` — housekeeping; removes `panel.rds`/`sc_fit.rds` when only tables are needed.

---

## 2. Memory strategy

- The panel is saved once per run and reloaded per outcome (as now), never kept alongside `gdata`.
- `sc_fit.rds` excludes the panel; `post/<outcome>.rds` stores `gdata_light`, not the full `gdata`.
- Post-SC uses the list-based samplers (no `X_block`); `fit_post` calls `gc()` between outcomes and never holds two outcomes' results at once.
- `summarise_run` streams over result files (`load_results_list_light`) and drops `beta_samples` after extracting `tau_draws`; `unit_effects.csv` is written incrementally.
- Large intermediate objects are written to `tempdir()` on the VM's local disk, not the shared filesystem, except final artefacts.
- Optional `post.thin` and `post.save_beta = FALSE` (keep only unit summaries) for exploratory runs.

---

## 3. Two things in the current code to decide, not assume

1. **Launch date.** `runSynth_*` uses 2021-05-01, `robust1` uses 2021-04-15, the paper says "April 2021" and the checklist says "weeks 23/24–70" for the pilot. The spec will carry one `launch_date` per cohort; you tell me the values.
2. **wID origin.** `runSynth` infers an offset against `2020-01-06` at run time. I will fix the origin in the spec and assert the offset once when the panel is loaded, rather than re-inferring it per run.

---

## 4. Multi-VM protocol

Locks are directories, because `mkdir` is atomic on POSIX filesystems including NFS, while `file.create` and advisory `fcntl` locks are not reliable across hosts:

```
claim:    ok <- dir.create("post/<outcome>.lock")     # TRUE = mine, FALSE = someone else's
          write host, PID, start time into <outcome>.lock/owner
run:      touch <outcome>.lock/heartbeat every N minutes
release:  write result, then unlink(<outcome>.lock, recursive = TRUE)
stale:    a lock whose heartbeat is older than `stale_after` (default 3 h) is reclaimable;
          the reclaimer records the takeover in run.log
```

The same protocol guards `sc_fit` (one lock per run) and summaries. The registry file is append-only and rewritten only by `run_status()` under its own lock. Every worker writes to `Runs/<run_id>/run.log` with host and timestamp, so a run's history is reconstructable.

Parallelism within a VM stays where it is now (`multiout_synth(parallel = TRUE)`; outcomes across cores via `workers`); parallelism across VMs comes from the lock protocol. Nothing needs a scheduler beyond starting `worker()` on each machine.

---

## 5. Testing and synthetic data

`sim_panel()` generates data with the same column names and structure as the real panel (`customer_id`, `week_start`, `wID`, `minBudgetDate`, `onboarddate`, `donor`, the `Spend*`/`irs_spend_*` columns, `numarrears`, `lengtharrears`, `weekly_income`, demographics, budget configuration fields), from a latent-factor DGP:

- customer loadings on common weekly factors (so synthetic controls have something to match),
- a persistent customer type (financial stress) affecting outcomes and adoption,
- an AR(1) transitory shock with persistence φ,
- adoption hazard = f(type, recent shock, peer rate, calendar) with a pilot/public rollout,
- known treatment effects per outcome (constant or heterogeneous in observed moderators).

Small settings (e.g. 300 treated, 600 donors, 80 weeks, 6 outcomes) run through L2–L5 in minutes and form the `testthat` integration test; `sc_diagnostics` and `summarise_run` are checked against the known truth. Unit tests cover `derive_*`, `define_sample` (counts per design), the lock protocol (two processes contending), and table formatting.

Nothing in the package reads `data-raw/` or `Processed/` during tests.

---

## 6. The Ashenfelter-dip simulation (`sim/dip_study.R`)

Purpose: show whether the SC + post-SC pipeline is biased when adoption is triggered by a transitory shock, how large the bias is, and which design choices remove it. Estimand: bias = estimated ATT − true effect, with the true effect set to zero in the main grid (pure bias) and to a known nonzero value in a second grid (attenuation/inflation).

Design grid (fully crossed, R replications each):

| Factor | Levels |
|---|---|
| Dip persistence φ | 0 (no dip), 0.5, 0.9 |
| Selection on the dip (hazard sensitivity) | none, moderate, strong |
| Pre-treatment lags L | 8, 23, 70 |
| Donors available | 100, 500, 2000 |
| Outcomes matched M | 1, 3, 10 (shared factors) |
| Match end | 0 (through τ=−1), 4 (held-out) |
| Correction | none, ridge-ASCM, lagged-dependent-variable check |

Outputs: bias and RMSE tables by factor; figures of bias vs L for each φ; event-time profile of the estimated effect (the dip signature); the held-out gap statistic's power to detect a dip; a table showing which corrections restore unbiasedness. The same script doubles as the reference for the web-appendix text on mean reversion.

---

## 7. Development plan (phased; each phase leaves the package usable)

| Phase | Deliverable | Checklist coverage | Validation |
|---|---|---|---|
| 0 | Package skeleton, spec/registry, `sim_panel()`, lock protocol, tests | — | tests pass locally |
| 1 | L1–L5 for `later_adopters` and `partial/non_onboarders`; `summarise_run`; `compare_runs` | reproduce current paper tables | numbers match existing `Results/` on the app environment |
| 2 | held-out matching, placebo dates, mean-reversion set, DV correlations, BH | §2–§6 | dip simulation |
| 3 | `two_launch` and `cohort_window` designs; ITT/IV if pilot eligibility data exist | §1 | |
| 4 | reallocation (levels + shares) | §7 | |
| 5 | adoption hazard, `R_i`, trajectory groups, HTE with interaction tests | §8–§12 | |
| 6 | configuration analyses, tightness baselines | §13–§14 | |
| 7 | sample-selection checks; web-appendix generation | §15–§16 | |

I build successively; you approve each phase before the next.

---

## 8. Questions before I write code

**Environment**
1. Package name — I have used `budgetsc`; happy to change.
2. R version on the VMs; versions of `augMultiSynth` (0.4.6?) and `scmBayesPost` installed there; is `fixest` available?
3. Shared filesystem type (NFS, EFS, SMB?) and number/size of VMs — this affects the stale-lock timeout and whether `mkdir` locks are safe (they are on NFS/EFS).
4. Should `data/*.rds` (income, balances, loans) be read only inside `build_*()`, i.e. is it acceptable that a *run* never touches `data/`? (`post_reg` currently reads income and balances at run time.)

**Design decisions**
5. Launch dates: pilot start, public launch (2021-04-15 vs 2021-05-01), and the exact week boundaries 23/24–70 — one definitive set.
6. Confirm the "final" specifications behind each paper table: main = `later_adopters`, public cohort, L = 70, H = 40, `max_donors = 1000`? Appendix = donor pools 1/2 with stratified 4k? Cohort windows = `robust1` with `numweeks = 4`, `cohort ∈ {0,1,2,3}`?
7. Table numbering: which R0 outputs correspond to Tables 1–3 and Web Appendix tables W1–Wn, so `summarise_run` reproduces them by name.
8. Is there a list of pilot-*invited* customers (not just pilot adopters)? Needed only for the ITT/IV variant; the checklist's two-launch design does not need it.
9. The parsimonious outcome set for the revision (checklist §5): arrears count, arrears length, liquidity deficit rate, financial slack / `income_share`? And the seven focal categories for levels + shares.
10. Placebo outcomes that "should not respond" (§4): candidates from the data?
11. Output formats: CSV + HTML (`gt`) + LaTeX for all tables, or CSV + one of the two?
12. Keep or drop the unused dependencies (`microsynth`, `survey`, `rmarkdown`, `glue`)? I would drop them.

---

## 9. Decisions recorded 29 September 2026 (from Andre's answers to §8)

| # | Decision |
|---|---|
| 1 | Package name `budgetsc`; private GitHub repo `andrebonfrer/budgetsc` (created 30 Sep), package at repo root. The bank VMs (SageMaker) cannot reach GitHub, so deployment is by **source tarball**: `devtools::build()` locally → copy `budgetsc_x.y.z.tar.gz` to the NFS share → `install.packages(<tarball>, repos = NULL, type = "source")` on the VM. CRAN dependencies come from the RStudio package source the VMs can see. No token is needed anywhere in this workflow. |
| 2 | `augMultiSynth`, `scmBayesPost`, `fixest` installed on the VMs. |
| 3 | Shared filesystem is NFSv4 → `mkdir` locks are atomic; heartbeat/stale detection uses file mtimes with a generous timeout because of NFS attribute caching. |
| 4 | **Runs never read `data/`.** Everything a run needs (including weekly income and balances, currently pulled in by `post_reg`) is merged into one processed analysis panel by the build step. |
| 5 | Week index: origin = `min(week_start)` = **Monday 2020-01-06**, `wID = floor((date − origin)/7) + 1`. One `timeline.yml` is the source of truth for origin, pilot and launch weeks; every function reads it. Pilot period: **August 2020 (wID 30, week beginning Monday 2020-07-27) → end April 2021 (wID 69)**; public launch **Monday 2021-05-03 (wID 70)**. |
| 6 | Working main spec: `later_adopters`, public cohort, L = 70, H = 40. Final spec to be settled later; appendix will summarise headline HTE statistics across specifications, tabulated and graphically. |
| 7 | Table numbering deferred; tables are named by content and mapped to numbers at the end. |
| 8 | No invitation list. "Partial onboarders" are known to have been invited (they started onboarding). A true ITT on invitation is therefore not feasible; the two-launch design is the main identification addition, with pilot adopters vs pilot partial onboarders as a supplementary comparison. |
| 9 | Parsimonious FWB set: `numarrears`, `lengtharrears`, `liquidity_deficit` (rate), `income_share` (= financial slack). Correlations checked and low. |
| 10 | Placebo outcomes to be proposed (see §10); placebo dates before each launch; placebo units drawn from never-onboarders. |
| 11 | Outputs: CSV + HTML (`gt`) + LaTeX for every table; figures for HTE distributions, gamma coefficients with error bounds, and cross-specification comparisons. |
| 12 | Drop `microsynth`, `survey`, `rmarkdown`, `glue`, `zoo` unless a specific function needs them. |

## 10. Timeline as it will be encoded (to confirm)

Weeks begin on Monday; `wID` from origin 2020-01-06.

| Event | Week beginning | wID | Note |
|---|---|---|---|
| Panel origin | 2020-01-06 | 1 | `min(week_start)` after `first_date` filter |
| Feature created (deck/paper "June 2020") | 2020-06-15 | 24 | not the experiment start; deck's "weeks 24–70" measures from here |
| **Pilot experiment start (August 2020)** | **2020-07-27** | **30** | resolved 30 Sep: pilot starts in wID 30; exact calendar date goes in `timeline.yml` (`robust1` code's 2020-08-03 is wID 31 — one week late) |
| Last pilot adoption week (end April 2021) | 2021-04-26 | 69 | Saturday 2021-05-01 falls in this week under the containing-week rule |
| **Public launch (Monday 3 May 2021)** | **2021-05-03** | **70** | resolved 30 Sep; cohort assignment uses the *date* `minBudgetDate >= 2021-05-03` |
| End of treated adoption window (launch + H = 40) | 2022-02-07 | 110 | `truncate.wID` |
| Donor (later adopter) onset from | 2022-02-14 | 111 | |
| Panel end (`cutoff` 2022-12-31) | 2022-12-26 | 156 | |

Two week rules, kept distinct in code: `week_of(date)` = the Monday-week *containing* a date, `floor((date − origin)/7) + 1` — this is how `minBudgetDate` becomes each customer's treatment week (as `date_to_wID_raw()` already does); and `first_week_on_or_after(date)` — what the current `launch.wID <- bdata[tdate >= launch.date, min(wID)]` does. They differ only for dates that are not Mondays (2021-05-01 → 69 under the first, 70 under the second). `timeline.yml` stores dates, never wIDs; wIDs are always derived.

Check when encoding: with L = 70, adopters in week 70 have only 69 observed pre-weeks; `define_sample()` will report how many treated units are dropped for insufficient lags rather than silently shortening L.

## 11. Placebo-outcome candidates (§4 of the checklist)

An outcome qualifies if it is measured in the panel, is *not* in the matching set, and is not plausibly changed by setting a budget:

| Candidate | Column | Why it should not respond | Caveat |
|---|---|---|---|
| Weekly salary income | `weekly_income` | budgeting does not change wages | gig/variable-income customers; test on regular-income subsample |
| Contractual loan repayment amount | from `monthly_loan_payment` / loan transactions | fixed by contract | refinancing; only the scheduled amount, not extra repayments |
| Childcare fees | `irs_spend_Childcare` | fee-based, hard to cut in the short run | attrition from childcare is possible but slow |
| Health maintenance spending | `irs_spend_HealthMaintenance` | dentist/optometrist visits are need-driven | deferrable, so weaker |
| Number of active accounts/products | if available | not a budgeting decision | may not be in the panel |

Recommendation: income as the primary placebo outcome, loan repayment amount second, childcare third; report all with the same weights and the same event-time window as the real outcomes.
