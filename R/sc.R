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

# Treatment week -> column index in the Y matrices, Inf for donors.
.treat_time_index <- function(sample, unit_vals, time_vals) {
  tt <- sample$ids[match(unit_vals, customer_id)]
  idx <- match(tt$treat_wID, time_vals)
  out <- ifelse(tt$role == "treated" & !is.na(idx), idx, Inf)
  as.numeric(out)
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
  sample <- readRDS(file.path(run$dir, "sample.rds"))
  p <- readRDS(file.path(run$dir, "panel.rds"))
  data.table::setDT(p)
  outcomes <- spec$sc$match_outcomes
  Yprep <- build_Ylist(p, outcomes)
  tt <- .treat_time_index(sample, Yprep$unit_vals, Yprep$time_vals)
  me <- as.integer(spec$sc$match_end)
  L <- sample$window$L - me; H <- sample$window$H + me
  tt_fit <- ifelse(is.finite(tt), tt - me, tt)      # hold out the last `me` pre weeks
  fun <- backend %||% switch(spec$sc$backend %||% "augmultisynth",
                              augmultisynth = sc_backend_augmultisynth,
                              stub = sc_backend_stub,
                              stop("unknown sc backend"))
  fit <- fun(Yprep$Y_list, tt_fit, Yprep$unit_vals, L = L, K = H, spec = spec)
  fit$outcomes <- outcomes; fit$L <- L; fit$K <- H; fit$match_end <- me
  fit$treat_time <- tt; fit$unit_ids <- Yprep$unit_vals
  fit$weights_mat <- sc_weight_matrix(fit, Yprep$unit_vals)
  out <- list(fit = fit, time_vals = Yprep$time_vals, unit_vals = Yprep$unit_vals,
              treat_time = tt, outcomes = outcomes, sample_ids = sample$ids)
  saveRDS(out, file.path(run$dir, "sc_fit.rds"))
  bsc_log(run, "sc fit saved: ", length(fit$treated_unit_ids), " treated, ",
          length(Yprep$unit_vals) - length(fit$treated_unit_ids), " donors, M=", length(outcomes),
          " L=", L, " H=", H, " backend=", spec$sc$backend %||% "augmultisynth")
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
#' @return Fit object satisfying the contract in `sc.R`.
#' @export
sc_backend_augmultisynth <- function(Y_list, treat_time, unit_ids, L, K, spec) {
  if (!requireNamespace("augMultiSynth", quietly = TRUE))
    stop("augMultiSynth is not installed; use spec$sc$backend = 'stub' for a dry run.", call. = FALSE)
  sc <- spec$sc
  J0 <- sum(is.finite(treat_time))
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
#' model packages. Not for inference.
#' @inheritParams sc_backend_augmultisynth
#' @export
sc_backend_stub <- function(Y_list, treat_time, unit_ids, L, K, spec) {
  ids <- as.character(unit_ids)
  tr <- which(is.finite(treat_time)); dn <- which(!is.finite(treat_time))
  J0 <- length(tr); M <- length(Y_list); Tn <- ncol(Y_list[[1]])
  w_eq <- rep(1 / length(dn), length(dn))
  weights <- rep(list(w_eq), J0); donor_ids <- rep(list(unit_ids[dn]), J0)
  names(weights) <- names(donor_ids) <- ids[tr]
  synth_w <- matrix(0, J0, length(ids)); synth_w[, dn] <- 1 / length(dn)
  tau <- array(NA_real_, c(J0, M, K + 1), dimnames = list(ids[tr], names(Y_list), NULL))
  for (m in seq_len(M)) {
    Y <- Y_list[[m]]; synth <- synth_w %*% Y
    for (j in seq_len(J0)) {
      t0 <- treat_time[tr[j]]; pre <- max(1, t0 - L):(t0 - 1); post <- t0:min(Tn, t0 + K)
      shift <- mean(Y[tr[j], pre] - synth[j, pre], na.rm = TRUE)   # outcome-specific intercept
      tau[j, m, seq_along(post)] <- Y[tr[j], post] - synth[j, post] - shift
    }
  }
  list(tau = tau, treated_units = tr, treated_unit_ids = unit_ids[tr],
       donors = rep(list(dn), J0), donor_ids = donor_ids, weights = weights)
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
#' matched window, the held-out window when `match_end > 0`, and post).
#' @param run A `bsc_run` with `sc_fit.rds` present.
#' @return list(pretrend, fit_summary), invisibly.
#' @export
sc_diagnostics <- function(run) {
  scfit <- readRDS(file.path(run$dir, "sc_fit.rds"))
  p <- readRDS(file.path(run$dir, "panel.rds")); data.table::setDT(p)
  w <- scfit$fit$weights_mat; tt <- scfit$treat_time; L <- run$spec$sample$n_lags; H <- run$spec$sample$n_leads
  me <- scfit$fit$match_end %||% 0L
  outs <- unique(c(scfit$outcomes, run$spec$sc$holdout_outcomes))
  outs <- intersect(outs, names(p))
  Yp <- build_Ylist(p, outs)
  tr <- which(is.finite(tt)); Tn <- length(Yp$time_vals)
  rows <- list(); unit_rows <- list()
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
  mark_done(run, "sc_diag")
  invisible(list(pretrend = pre, fit_summary = fs, unit_gaps = ug))
}

utils::globalVariables(c("tau_k", "matched", "window", "mean_gap", "rmspe", "outcome"))
