# spec.R ----------------------------------------------------------------------
# A spec is the complete description of one analysis run: data source, sample
# design, SC settings, post-SC settings, table families. Specs are plain nested
# lists so they serialise to YAML that co-authors can read and diff.
#
# The run identifier is a hash of the spec content (excluding `name` and
# `notes`), so identical analyses share a directory and a re-run resumes.

#' Default specification
#'
#' Every field the pipeline reads, with the working main analysis as default:
#' later adopters as donors, public-launch cohort, L = 70, H = 40. Model
#' stages that consume these fields arrive in later phases; the fields are
#' fixed now so specs written today remain valid.
#'
#' @return A list of class `bsc_spec`.
#' @export
spec_default <- function() {
  tl <- bsc_timeline()
  s <- list(
    name  = "unnamed",
    notes = "",
    data = list(
      panel_file   = "Processed/analysis_panel.rds",
      first_date   = format(tl$events$panel_first_date, "%Y-%m-%d"),
      cutoff       = format(tl$events$panel_cutoff, "%Y-%m-%d"),
      timeline     = basename(tl$path)
    ),
    sample = list(
      design         = "later_adopters",
      cohort         = "public",
      n_lags         = tl$horizon$n_lags_public,
      n_leads        = tl$horizon$n_leads,
      treat_start    = "containing_week",
      filters        = list(min_pre_weeks = 8L, pct_income_thresh = 0,
                            n_budgets_min = 1L, min_cats = 10L),
      donor_sampling = list(method = "none", target = NULL, seed = 1L),
      subset         = NULL,
      placebo        = NULL
    ),
    derive = list(
      focal_spend  = FOCAL_SPEND_CATEGORIES,
      winsor_probs = c(0.005, 0.995),
      roll_n       = 8L,
      cv_basis     = "all",
      distress     = NULL
    ),
    sc = list(
      match_outcomes       = match_outcomes_wide(),
      holdout_outcomes     = c("weekly_income", "monthly_loan_payment_w"),
      match_end            = 0L,
      backend              = "augmultisynth",
      max_donors           = 1000L,
      screen_outcome       = "total_spend",
      screen_method        = "mse",
      standardize_outcomes = TRUE,
      intercept            = "outcome",
      solver               = "fw",
      lambda               = 1e-3,
      nu_scale             = 0.1,
      verbose              = FALSE,
      eps_sd               = 0.01,
      parallel             = TRUE
    ),
    post = list(
      outcomes    = c("numarrears", "lengtharrears", "liquidity_deficit_rate", "income_share"),
      f_Z         = paste("budgetdummy ~ budgetcategoriesN + factor(frequency) + age + income_mean +",
                          "customer_tenure + gender + income_cv + cv_2020_spend +",
                          "I(homeloan_mean/income_mean) + numgoalcats + br_under + br_target + br_over"),
      first_stage = "none",
      backend     = "scmbayes",
      instruments = NULL,
      gibbs       = list(n_iter = 2000L, burn_in = 1000L, seed = 1L),
      priors      = list(Sigma_gamma_prior = 1000, a_sigma_tau_prior = 5, b_sigma_tau_prior = 1),
      thin        = 1L,
      save_beta   = TRUE,
      verbose     = FALSE,
      w_min       = 0
    ),
    tables = list(
      families = list(
        fwb    = c("numarrears", "lengtharrears", "liquidity_deficit_rate", "income_share"),
        levels = character(0),
        shares = character(0)
      ),
      formats = c("csv", "html", "tex"),
      digits  = 3L
    )
  )
  class(s) <- c("bsc_spec", "list")
  s
}

#' Working main specification
#' @param ... Dotted-path overrides passed to [spec_modify()].
#' @return A `bsc_spec`.
#' @export
spec_main <- function(...) {
  s <- spec_default()
  s$name <- "main_later_adopters"
  spec_modify(s, ...)
}

#' The wide (default) and lean matching sets
#'
#' `match_outcomes_wide()` is the `match.out` vector of
#' `runSynth_augMultiSynth_budgetsetters.R`: arrears, the eleven focal
#' `Spend*` categories, activity counts, seven wallet shares, `SpendAlcohol`,
#' `SpendTobacco` and `total_spend`. `match_outcomes_lean()` is the four FWB
#' outcomes plus `total_spend`. Neither contains a holdout (placebo) outcome.
#' @return Character vector.
#' @export
match_outcomes_wide <- function() {
  c("numarrears", "lengtharrears",
    paste0("Spend", FOCAL_SPEND_CATEGORIES),
    "weekly_signins", "total_transactions", "total_irs_transactions", "total_categories",
    "walletshare_Supermarket", "walletshare_FastFood", "walletshare_Alcohol",
    "walletshare_Tobacco", "walletshare_HealthMaintenance", "walletshare_Childcare",
    "walletshare_CasinoGambling",
    "SpendAlcohol", "SpendTobacco",
    "total_spend")
}

#' @rdname match_outcomes_wide
#' @export
match_outcomes_lean <- function() {
  c("numarrears", "lengtharrears", "liquidity_deficit_rate", "income_share", "total_spend")
}

#' Lean-matching variant of the main specification
#' @inheritParams spec_main
#' @return A `bsc_spec`.
#' @export
spec_lean <- function(...) {
  s <- spec_default()
  s$name <- "main_lean_matching"
  s$sc$match_outcomes <- match_outcomes_lean()
  spec_modify(s, ...)
}

#' Placebo specifications
#'
#' `spec_placebo_shift()` moves every adopter's onset `shift` weeks earlier
#' (default `n_leads + 1`, so the placebo post window ends before real
#' adoption); `spec_placebo_never()` gives never-onboarders pseudo onsets drawn
#' from the real adopters' onset dates; the remaining never-onboarders are the
#' donors and the moderator formula is reduced to customer characteristics
#' (pseudo-treated units have no budget configuration). `spec_placebo_shift()`
#' defaults to `n_lags = 23` because a placebo window needs L pre-weeks before
#' the shifted onset.
#' @param base A `bsc_spec` to derive from. Default [spec_main()].
#' @param shift Integer weeks to move onsets earlier.
#' @param n Number of pseudo-treated never-onboarders.
#' @param seed Integer seed for the pseudo-onset draw.
#' @param ... Further dotted overrides passed to [spec_modify()].
#' @return A `bsc_spec`.
#' @export
spec_placebo_shift <- function(base = spec_main(), shift = NULL, ...) {
  s <- base
  s$name <- paste0(base$name, "_placebo_shift")
  s$sample$placebo <- list(base_design = base$sample$design, units = "treated",
                           shift = as.integer(shift %||% (base$sample$n_leads + 1L)))
  s$sample$design <- "placebo_dates"
  s$sample$n_lags <- min(s$sample$n_lags, 23L)
  spec_modify(s, ...)
}

#' @rdname spec_placebo_shift
#' @export
spec_placebo_never <- function(base = spec_main(), n = NULL, seed = 1L, ...) {
  s <- base
  s$name <- paste0(base$name, "_placebo_never")
  s$sample$placebo <- list(base_design = base$sample$design, units = "never_onboarders", n = n, seed = seed)
  s$sample$design <- "placebo_dates"
  # pseudo-treated units have no budget configuration: customer-level moderators only
  s$post$f_Z <- "budgetdummy ~ age + income_mean + customer_tenure + gender + income_cv + cv_2020_spend + I(homeloan_mean/income_mean)"
  spec_modify(s, ...)
}

#' Pilot-cohort specification
#' @inheritParams spec_main
#' @return A `bsc_spec`.
#' @export
spec_pilot <- function(...) {
  tl <- bsc_timeline()
  s <- spec_default()
  s$name <- "pilot_later_adopters"
  s$sample$cohort <- "pilot"
  s$sample$n_lags <- tl$horizon$n_lags_pilot
  spec_modify(s, ...)
}

#' Modify a specification with dotted paths
#'
#' `spec_modify(spec, "sample.n_lags" = 23, "sc.match_end" = 4L)`.
#' @param spec A `bsc_spec`.
#' @param ... Named values; names are dotted paths into the spec.
#' @return The modified `bsc_spec`.
#' @export
spec_modify <- function(spec, ...) {
  mods <- list(...)
  if (!length(mods)) return(spec)
  if (is.null(names(mods)) || any(!nzchar(names(mods))))
    stop("All modifications must be named with dotted paths.", call. = FALSE)
  for (nm in names(mods)) spec <- .set_dotted(spec, nm, mods[[nm]])
  class(spec) <- c("bsc_spec", "list")
  spec_validate(spec)
}

#' Validate a specification
#'
#' Checks presence of sections, allowed values for enumerations, and basic
#' types. Stops with all problems listed at once.
#' @param spec A `bsc_spec` or plain list.
#' @return The spec, invisibly, with class `bsc_spec`.
#' @export
spec_validate <- function(spec) {
  problems <- character(0)
  need <- c("name", "data", "derive", "sample", "sc", "post", "tables")
  miss <- setdiff(need, names(spec))
  if (length(miss)) problems <- c(problems, paste0("missing section(s): ", paste(miss, collapse = ", ")))

  chk_enum <- function(path, allowed) {
    v <- .get_dotted(spec, path)
    if (!is.null(v) && !(v %in% allowed))
      problems <<- c(problems, sprintf("%s = '%s' not in {%s}", path, v, paste(allowed, collapse = ", ")))
  }
  chk_int <- function(path, min = 0L) {
    v <- .get_dotted(spec, path)
    if (!is.null(v) && (!is.numeric(v) || length(v) != 1L || v < min || v != round(v)))
      problems <<- c(problems, sprintf("%s must be a single integer >= %d", path, min))
  }

  chk_enum("sample.design", c("later_adopters", "partial_onboarders", "non_onboarders",
                              "cohort_window", "two_launch", "placebo_dates"))
  chk_enum("sample.cohort", c("public", "pilot"))
  chk_enum("sample.treat_start", c("containing_week", "next_week"))
  chk_enum("sample.donor_sampling.method", c("none", "stratified", "random"))
  chk_enum("sc.intercept", c("none", "outcome", "global"))
  chk_enum("sc.solver", c("fw", "qp"))
  chk_enum("sc.backend", c("augmultisynth", "stub"))
  chk_enum("post.backend", c("scmbayes", "stub"))
  chk_enum("post.first_stage", c("none", "selection_probit_bayes"))
  chk_int("sample.n_lags", 1L)
  chk_int("sample.n_leads", 1L)
  chk_int("sc.match_end", 0L)
  chk_int("post.gibbs.n_iter", 1L)
  chk_int("post.gibbs.burn_in", 0L)
  wm <- .get_dotted(spec, "post.w_min")
  if (!is.null(wm) && (!is.numeric(wm) || wm < 0 || wm >= 1)) problems <- c(problems, "post.w_min must be in [0, 1)")

  ni <- .get_dotted(spec, "post.gibbs.n_iter"); bi <- .get_dotted(spec, "post.gibbs.burn_in")
  if (!is.null(ni) && !is.null(bi) && bi >= ni)
    problems <- c(problems, "post.gibbs.burn_in must be smaller than post.gibbs.n_iter")

  me <- .get_dotted(spec, "sc.match_end"); nl <- .get_dotted(spec, "sample.n_lags")
  if (!is.null(me) && !is.null(nl) && me >= nl)
    problems <- c(problems, "sc.match_end must be smaller than sample.n_lags")

  mo <- .get_dotted(spec, "sc.match_outcomes"); ho <- .get_dotted(spec, "sc.holdout_outcomes")
  both <- intersect(mo, ho)
  if (length(both))
    problems <- c(problems, paste0("outcome(s) in both sc.match_outcomes and sc.holdout_outcomes: ",
                                   paste(both, collapse = ", "),
                                   " (a matched outcome cannot serve as a placebo)"))
  so <- .get_dotted(spec, "sc.screen_outcome")
  if (!is.null(so) && !is.null(mo) && !(so %in% mo))
    problems <- c(problems, "sc.screen_outcome must be one of sc.match_outcomes")
  chk_enum("derive.cv_basis", c("all", "focal"))
  if (identical(.get_dotted(spec, "sample.design"), "placebo_dates")) {
    pl <- .get_dotted(spec, "sample.placebo")
    if (is.null(pl) || is.null(pl$base_design))
      problems <- c(problems, "sample.placebo must be a list with base_design (and shift or units) for design = placebo_dates")
    else if (!pl$base_design %in% c("later_adopters", "partial_onboarders", "non_onboarders"))
      problems <- c(problems, "sample.placebo$base_design must be a non-placebo design")
  }
  wp <- .get_dotted(spec, "derive.winsor_probs")
  if (!is.null(wp) && (length(wp) != 2L || any(wp < 0 | wp > 1) || wp[1] >= wp[2]))
    problems <- c(problems, "derive.winsor_probs must be two increasing values in [0, 1]")

  fam <- .get_dotted(spec, "tables.families")
  if (!is.null(fam) && !is.list(fam))
    problems <- c(problems, "tables.families must be a named list of character vectors")

  if (length(problems))
    stop("Invalid spec:\n  - ", paste(problems, collapse = "\n  - "), call. = FALSE)

  class(spec) <- c("bsc_spec", "list")
  invisible(spec)
}

#' Deterministic run identifier for a specification
#'
#' SHA-1 of the canonicalised spec (fields sorted recursively, dates as
#' strings) excluding `name` and `notes`, truncated to 10 characters.
#' @param spec A `bsc_spec`.
#' @param full Logical; return the full 40-character hash. Default FALSE.
#' @return Character scalar.
#' @export
spec_id <- function(spec, full = FALSE) {
  s <- unclass(spec)
  s$name <- NULL
  s$notes <- NULL
  h <- digest::digest(.canonicalise(s), algo = "sha1", serialize = TRUE)
  if (full) h else substr(h, 1, 10)
}

#' Write / read a specification as YAML
#' @param spec A `bsc_spec`.
#' @param path File path.
#' @return `spec_write()` returns `path` invisibly; `spec_read()` returns a
#'   validated `bsc_spec`.
#' @export
spec_write <- function(spec, path) {
  yaml::write_yaml(unclass(spec), path)
  invisible(path)
}

#' @rdname spec_write
#' @export
spec_read <- function(path) {
  s <- yaml::read_yaml(path)
  spec_validate(s)
  class(s) <- c("bsc_spec", "list")
  s
}

#' @export
print.bsc_spec <- function(x, ...) {
  cat("budgetsc spec '", x$name, "'  [id ", spec_id(x), "]\n", sep = "")
  cat("  sample : ", x$sample$design, " / ", x$sample$cohort,
      "  L=", x$sample$n_lags, " H=", x$sample$n_leads, "\n", sep = "")
  cat("  sc     : ", length(x$sc$match_outcomes), " match outcomes; match_end=", x$sc$match_end,
      "; max_donors=", x$sc$max_donors, "\n", sep = "")
  cat("  post   : ", length(x$post$outcomes), " outcomes; first_stage=", x$post$first_stage,
      "; gibbs ", x$post$gibbs$n_iter, "/", x$post$gibbs$burn_in, "\n", sep = "")
  invisible(x)
}
