# sc.R ------------------------------------------------------------------------
# Stage: synthetic control. The panel is turned into a list of unit x week
# matrices (one per matched outcome), treatment weeks into row indices, and
# the fit delegated to a backend. The default backend is
# augMultiSynth::multiout_synth(); a stub backend (mean of donors + noise)
# lets the whole pipeline run and be tested without the model packages.
#
# The fit object contract (both backends), which is augMultiSynth's own
# structure plus one derived matrix:
#   $tau              array J0 x M x (K+1): unit-level gaps by outcome and post week
#   $treated_unit_ids ids of treated units (rows of tau)
#   $donor_ids        list (length J0) of donor ids per treated unit
#   $weights          list (length J0) of donor weights aligned to donor_ids
#   $weights_mat      J0 x N matrix built by sc_weight_matrix() (rows treated, cols all units)
#   $unit_ids, $K, $L, $outcomes
# scmBayesPost::build_W_from_augMultiSynth() reads treated_unit_ids/donor_ids/weights,
# so a stub fit is accepted by the real post backend and vice versa.

#' Build the Y_list (one unit x week matrix per outcome)
#'
#' Transcribed from `panel_to_Ylist_int_time()` in utils.R: a complete
#' unit x week grid, missing cells left `NA` (or `fill_value`).
#' @param p Panel.
#' @param outcomes Character vector of outcome columns.
#' @param fill_value Value for missing cells. Default `NA`.
#' @return list(Y_list, time_vals, unit_vals).
#' @export
build_Ylist <- function(p, outcomes, fill_value = NA_real_) {
  miss <- setdiff(outcomes, names(p))
  if (length(miss)) stop("build_Ylist: missing columns: ", paste(miss, collapse = ", "), call. = FALSE)
  d <- p[, c("customer_id", "wID", outcomes), with = FALSE]
  time_vals <- sort(unique(d$wID)); unit_vals <- sort(unique(d$customer_id))
  grid <- data.table::CJ(customer_id = unit_vals, wID = time_vals)   # unit-major order
  d <- d[grid, on = c("customer_id", "wID")]                          # rows follow grid order
  Y_list <- lapply(outcomes, function(y) {
    # grid is unit-major (all weeks of unit 1, then unit 2, ...), so fill by row
    m <- matrix(d[[y]], nrow = length(unit_vals), ncol = length(time_vals), byrow = TRUE,
                dimnames = list(as.character(unit_vals), as.character(time_vals)))
    storage.mode(m) <- "double"
    if (!is.na(fill_value)) m[is.na(m)] <- fill_value
    m
  })
  names(Y_list) <- outcomes
  list(Y_list = Y_list, time_vals = time_vals, unit_vals = unit_vals)
}

# Treatment week -> column index in the Y matrices. Treated units get their own
# index. Donors get Inf ("never treated", the old behaviour, donor_eligibility =
# "global") or, with per_unit = TRUE, their REAL adoption week, so that
# augMultiSynth's rule treat_time > T_j + K can exclude donors that adopt inside
# a given treated unit's post window.
.treat_time_index <- function(sample, unit_vals, time_vals, per_unit = FALSE) {
  tt <- sample$ids[match(unit_vals, customer_id)]
  idx <- match(tt$treat_wID, time_vals)
  is_tr <- tt$role == "treated" & !is.na(idx)
  out <- ifelse(is_tr, idx, Inf)
  if (per_unit && "real_onset_wID" %in% names(tt)) {
    d_idx <- match(tt$real_onset_wID, time_vals)
    out <- ifelse(!is_tr & tt$role == "donor" & !is.na(d_idx), d_idx, out)
  }
  list(treat_time = as.numeric(out), treated_idx = which(is_tr))
}

#' Fit the synthetic control for a run
#'
#' Reads `panel.rds` from the run directory (or takes `panel`), builds the
#' Y_list on `spec$sc$match_outcomes`, fits via the backend named in
#' `spec$sc$backend`, and saves `sc_fit.rds` = list(fit, Yprep, treat_time,
#' outcomes, sample_ids). When `spec$sc$match_end > 0`, the last `match_end`
#' pre-treatment weeks are held out of the matching window by shifting the
#' treatment index back and shortening L (the held-out weeks then appear as
#' the first post "periods" of tau and are diagnosed by [sc_diagnostics()]).
#'
#' @param run A `bsc_run` with `sample.rds` and `panel.rds` present.
#' @param backend Optional function overriding `spec$sc$backend`.
#' @return The fit list, invisibly (also written to disk).
#' @export
fit_sc <- function(run, backend = NULL) {
  spec <- run$spec
  if (!is.null(spec$sc$from))
    stop("this run reuses another run's SC fit (sc.from = '", spec$sc$from, "'); use run_reuse_sc().", call. = FALSE)
  sample <- readRDS(file.path(run$dir, "sample.rds"))
  p <- readRDS(file.path(run$dir, "panel.rds"))
  data.table::setDT(p)
  outcomes <- spec$sc$match_outcomes
  Yprep <- build_Ylist(p, outcomes)
  elig <- spec$sc$donor_eligibility %||% "global"          # runs registered before 0.5.0 have no field: old behaviour
  per_unit <- identical(elig, "per_unit")
  ti <- .treat_time_index(sample, Yprep$unit_vals, Yprep$time_vals, per_unit = per_unit)
  tt <- ti$treat_time; tr_idx <- ti$treated_idx
  me <- as.integer(spec$sc$match_end)
  L <- sample$window$L - me; H <- sample$window$H + me
  tt_fit <- tt; tt_fit[tr_idx] <- tt[tr_idx] - me           # hold out the last `me` pre weeks (treated units only)
  treated_units <- NULL
  if (per_unit) {
    # augMultiSynth's eligibility rule is treat_time > T_j + K; it stops if ANY treated unit has no eligible
    # donor, so units with fewer than min_eligible_donors are set aside here and reported.
    min_d <- as.integer(spec$sc$min_eligible_donors %||% 20L)
    n_el  <- vapply(tr_idx, function(j) sum(tt_fit > tt_fit[j] + H), integer(1))
    keep  <- n_el >= min_d
    if (!any(keep)) stop("no treated unit has >= ", min_d, " eligible donors under per-unit eligibility; ",
                         "lower sc.min_eligible_donors or sample.n_leads.", call. = FALSE)
    treated_units <- tr_idx[keep]
    if (any(!keep)) {
      dir.create(file.path(run$dir, "tables"), showWarnings = FALSE)
      data.table::fwrite(data.table::data.table(customer_id = Yprep$unit_vals[tr_idx[!keep]], eligible_donors = n_el[!keep],
                                                required = min_d), file.path(run$dir, "tables", "sc_units_without_donors.csv"))
      bsc_log(run, sum(!keep), " of ", length(tr_idx), " treated unit(s) set aside: fewer than ", min_d,
              " donors adopt more than H weeks after them (tables/sc_units_without_donors.csv)")
    }
  }
  fun <- backend %||% switch(spec$sc$backend %||% "augmultisynth",
                              augmultisynth = sc_backend_augmultisynth,
                              stub = sc_backend_stub,
                              stop("unknown sc backend"))
  args <- list(Yprep$Y_list, tt_fit, Yprep$unit_vals, L = L, K = H, spec = spec)
  if (!is.null(treated_units)) args$treated_units <- treated_units
  fit <- do.call(fun, args)
  fit$outcomes <- outcomes; fit$L <- L; fit$K <- H; fit$match_end <- me
  fit$treat_time <- tt; fit$unit_ids <- Yprep$unit_vals
  fit$weights_mat <- sc_weight_matrix(fit, Yprep$unit_vals)
  treated_idx <- match(as.character(fit$treated_unit_ids), as.character(Yprep$unit_vals))   # the units actually fitted, in fit order
  out <- list(fit = fit, time_vals = Yprep$time_vals, unit_vals = Yprep$unit_vals,
              treat_time = tt, treated_idx = treated_idx, donor_eligibility = elig,
              outcomes = outcomes, sample_ids = sample$ids)
  saveRDS(out, file.path(run$dir, "sc_fit.rds"))
  bsc_log(run, "sc fit saved: ", length(treated_idx), " treated, ",
          length(Yprep$unit_vals) - length(tr_idx), " donors, M=", length(outcomes),
          " L=", L, " H=", H, " eligibility=", elig, " backend=", spec$sc$backend %||% "augmultisynth")
  invisible(out)
}

#' augMultiSynth backend
#'
#' Calls `augMultiSynth::multiout_synth()` with the spec's `sc` options.
#' Arguments are matched against the installed function's formals, so the
#' parallel arguments (`parallel_backend`/`n_cores` in >= 0.3.5, absent in
#' 0.3.4) are passed only when they exist.
#' @param Y_list,treat_time,unit_ids As in `augMultiSynth::multiout_synth()`.
#' @param L,K Lags and leads.
#' @param spec The run spec (for `sc` options).
#' @param treated_units Optional integer indices of the treated units. When given, units with a finite `treat_time` that are not listed are donors that adopt later, and augMultiSynth's eligibility rule applies per treated unit.
#' @return Fit object satisfying the contract in `sc.R`.
#' @export
sc_backend_augmultisynth <- function(Y_list, treat_time, unit_ids, L, K, spec, treated_units = NULL) {
  if (!requireNamespace("augMultiSynth", quietly = TRUE))
    stop("augMultiSynth is not installed; use spec$sc$backend = 'stub' for a dry run.", call. = FALSE)
  sc <- spec$sc
  J0 <- length(treated_units %||% which(is.finite(treat_time)))
  args <- list(
    Y_list = Y_list, treat_time = treat_time, unit_ids = unit_ids, L = L, K = K,
    max_donors = sc$max_donors, screen_outcome = which(names(Y_list) == sc$screen_outcome),
    screen_method = sc$screen_method, lambda = sc$lambda %||% 1e-3, solver = sc$solver,
    pooled_adjustment = TRUE, nu = sc$nu_scale * K * J0, verbose = isTRUE(sc$verbose),
    standardize_outcomes = sc$standardize_outcomes, intercept = sc$intercept, eps_sd = sc$eps_sd)
  fm <- names(formals(augMultiSynth::multiout_synth))
  if (isTRUE(sc$parallel) && "parallel_backend" %in% fm) {
    args$parallel_backend <- if (.Platform$OS.type == "unix") "fork" else "psock"
    if ("n_cores" %in% fm) args$n_cores <- max(1L, parallel::detectCores() - 1L)
  }
  if (!is.null(treated_units)) {
    if (!"treated_units" %in% fm) stop("per-unit donor eligibility needs an augMultiSynth whose multiout_synth() has a treated_units argument.", call. = FALSE)
    args$treated_units <- treated_units
  }
  args <- args[names(args) %in% fm]
  do.call(augMultiSynth::multiout_synth, args)
}

#' Donor weights as a J0 x N matrix from a fit
#'
#' Uses `fit$treated_unit_ids`, `fit$donor_ids` and `fit$weights` (the
#' augMultiSynth structure, also produced by the stub backend).
#' @param fit Backend fit.
#' @param unit_ids All unit ids in Y_list order.
#' @return Matrix with treated ids as rownames and all unit ids as colnames.
#' @export
sc_weight_matrix <- function(fit, unit_ids) {
  if (is.null(fit$treated_unit_ids) || is.null(fit$donor_ids) || is.null(fit$weights))
    stop("sc_weight_matrix: fit lacks treated_unit_ids / donor_ids / weights.", call. = FALSE)
  ids <- as.character(unit_ids); tr <- as.character(fit$treated_unit_ids)
  m <- matrix(0, length(tr), length(ids), dimnames = list(tr, ids))
  for (j in seq_along(tr)) {
    d <- as.character(fit$donor_ids[[j]]); w <- as.numeric(fit$weights[[j]])
    if (length(d) != length(w)) stop("sc_weight_matrix: donor_ids/weights length mismatch for ", tr[j], call. = FALSE)
    m[j, d] <- w
  }
  m
}

#' Stub backend: equal donor weights, gaps = treated minus donor mean
#'
#' No optimisation; exists so the pipeline can run end to end without the
#' model packages. Not for inference. Honours per-unit eligibility when
#' `treated_units` is given.
#' @inheritParams sc_backend_augmultisynth
#' @export
sc_backend_stub <- function(Y_list, treat_time, unit_ids, L, K, spec, treated_units = NULL) {
  ids <- as.character(unit_ids)
  per_unit <- !is.null(treated_units)
  tr <- treated_units %||% which(is.finite(treat_time))
  others <- setdiff(seq_along(ids), tr)
  M <- length(Y_list); Tn <- ncol(Y_list[[1]])
  el <- lapply(tr, function(j) if (per_unit) others[treat_time[others] > treat_time[j] + K] else others[!is.finite(treat_time[others])])
  ok <- lengths(el) > 0L; tr <- tr[ok]; el <- el[ok]; J0 <- length(tr)
  weights <- lapply(el, function(e) rep(1 / length(e), length(e))); donor_ids <- lapply(el, function(e) unit_ids[e])
  names(weights) <- names(donor_ids) <- ids[tr]
  tau <- array(NA_real_, c(J0, M, K + 1), dimnames = list(ids[tr], names(Y_list), NULL))
  for (m in seq_len(M)) {
    Y <- Y_list[[m]]
    for (j in seq_len(J0)) {
      w <- rep(0, length(ids)); w[el[[j]]] <- 1 / length(el[[j]])
      synth <- as.numeric(w %*% Y)
      t0 <- treat_time[tr[j]]; pre <- max(1, t0 - L):(t0 - 1); post <- t0:min(Tn, t0 + K)
      shift <- mean(Y[tr[j], pre] - synth[pre], na.rm = TRUE)         # outcome-specific intercept
      tau[j, m, seq_along(post)] <- Y[tr[j], post] - synth[post] - shift
    }
  }
  list(tau = tau, treated_units = tr, treated_unit_ids = unit_ids[tr],
       donors = el, donor_ids = donor_ids, weights = weights)
}

#' Event-time average effects from the SC fit
#' @param scfit The list saved by [fit_sc()].
#' @return data.table: outcome, tau (event week 0..H), mean, se, n.
#' @export
sc_event_time_ate <- function(scfit) {
  tau <- scfit$fit$tau; M <- dim(tau)[2]; Kp1 <- dim(tau)[3]
  rows <- lapply(seq_len(M), function(m) data.table::data.table(
    outcome = scfit$outcomes[m], tau_k = seq_len(Kp1) - 1L - (scfit$fit$match_end %||% 0L),
    mean = apply(tau[, m, , drop = FALSE], 3, mean, na.rm = TRUE),
    se   = apply(tau[, m, , drop = FALSE], 3, function(x) stats::sd(x, na.rm = TRUE) / sqrt(sum(is.finite(x)))),
    n    = apply(tau[, m, , drop = FALSE], 3, function(x) sum(is.finite(x)))))
  data.table::rbindlist(rows)
}

#' Pre-treatment fit and held-out gap diagnostics
#'
#' For every treated unit and outcome (matched and held-out), computes the
#' treated-minus-synthetic gap by event week over the L pre-treatment weeks
#' and the H post weeks using the fit's weights, after removing the outcome's
#' pre-window intercept. Writes `sc_diag/pretrend.csv` (mean gap and RMSPE by
#' outcome and event week) and `sc_diag/fit_summary.csv` (RMSPE over the
#' matched window, the held-out window when `match_end > 0`, and post),
#' `sc_diag/unit_gaps.csv`, and `sc_diag/pre_slope.csv`: the mean per-unit slope
#' of the gap over the matched pre-window with its t-statistic, and
#' `trend_implied_ate`, the post-minus-pre difference that trend continuation
#' alone would produce. Compare it with the estimated effect.
#' @param run A `bsc_run` with `sc_fit.rds` present.
#' @return list(pretrend, fit_summary, unit_gaps, pre_slope), invisibly.
#' @export
sc_diagnostics <- function(run) {
  scfit <- readRDS(file.path(run$dir, "sc_fit.rds"))
  p <- readRDS(file.path(run$dir, "panel.rds")); data.table::setDT(p)
  w <- scfit$fit$weights_mat; tt <- scfit$treat_time; L <- run$spec$sample$n_lags; H <- run$spec$sample$n_leads
  me <- scfit$fit$match_end %||% 0L
  outs <- unique(c(scfit$outcomes, run$spec$sc$holdout_outcomes))
  outs <- intersect(outs, names(p))
  Yp <- build_Ylist(p, outs)
  tr <- scfit$treated_idx %||% which(is.finite(tt)); Tn <- length(Yp$time_vals)   # the units actually fitted, in weights_mat row order
  rows <- list(); unit_rows <- list(); slope_rows <- list()
  for (o in outs) {
    Y <- Yp$Y_list[[o]]; synth <- w %*% Y
    g <- matrix(NA_real_, length(tr), L + H + 1)
    for (j in seq_along(tr)) {
      t0 <- tt[tr[j]]; idx <- (t0 - L):(t0 + H); ok <- idx >= 1 & idx <= Tn
      gap <- Y[tr[j], idx[ok]] - synth[j, idx[ok]]
      pre_fit <- idx[ok] < t0 - me
      gap <- gap - mean(gap[pre_fit], na.rm = TRUE)
      g[j, which(ok)] <- gap
    }
    ev <- seq(-L, H)
    # slope of each unit's gap over the matched pre-window: a flat gap means the twin tracks the adopter;
    # a slope means the post-minus-pre "effect" can be trend continuation, not a step at adoption
    pre_cols <- which(ev < -me); post_cols <- which(ev >= 0)
    slopes <- apply(g[, pre_cols, drop = FALSE], 1, function(y) { x <- ev[pre_cols]; k <- is.finite(y)
      if (sum(k) < 3L) NA_real_ else stats::cov(x[k], y[k]) / stats::var(x[k]) })
    n_s <- sum(is.finite(slopes)); ms <- mean(slopes, na.rm = TRUE); se_s <- stats::sd(slopes, na.rm = TRUE) / sqrt(max(n_s, 1L))
    slope_rows[[o]] <- data.table::data.table(outcome = o, matched = o %in% scfit$outcomes, n_units = n_s,
      mean_slope_per_week = ms, se = se_s, t = ms / se_s,
      trend_implied_ate = ms * (mean(ev[post_cols]) - mean(ev[pre_cols])))
    win <- data.table::fcase(ev < -me, "matched_pre", ev < 0, "heldout_pre", default = "post")
    unit_rows[[o]] <- data.table::data.table(
      outcome = o, treated_id = as.character(Yp$unit_vals[tr]),
      gap_matched_pre = rowMeans(g[, win == "matched_pre", drop = FALSE], na.rm = TRUE),
      gap_heldout_pre = if (me > 0) rowMeans(g[, win == "heldout_pre", drop = FALSE], na.rm = TRUE) else NA_real_,
      gap_post = rowMeans(g[, win == "post", drop = FALSE], na.rm = TRUE))
    rows[[o]] <- data.table::data.table(outcome = o, tau_k = ev,
      matched = o %in% scfit$outcomes,
      window = data.table::fcase(ev < -me, "matched_pre", ev < 0, "heldout_pre", default = "post"),
      mean_gap = colMeans(g, na.rm = TRUE), rmspe = sqrt(colMeans(g^2, na.rm = TRUE)),
      n = colSums(is.finite(g)))
  }
  pre <- data.table::rbindlist(rows)
  fs <- pre[, .(rmspe = sqrt(mean(rmspe^2, na.rm = TRUE)), mean_gap = mean(mean_gap, na.rm = TRUE)),
            by = .(outcome, matched, window)]
  ug <- data.table::rbindlist(unit_rows)
  dd <- file.path(run$dir, "sc_diag"); dir.create(dd, showWarnings = FALSE)
  data.table::fwrite(pre, file.path(dd, "pretrend.csv")); data.table::fwrite(fs, file.path(dd, "fit_summary.csv"))
  data.table::fwrite(ug, file.path(dd, "unit_gaps.csv"))
  sl <- data.table::rbindlist(slope_rows)
  data.table::fwrite(sl, file.path(dd, "pre_slope.csv"))
  mark_done(run, "sc_diag")
  invisible(list(pretrend = pre, fit_summary = fs, unit_gaps = ug, pre_slope = sl))
}

utils::globalVariables(c("tau_k", "matched", "window", "mean_gap", "rmspe", "outcome", "customer_id"))
