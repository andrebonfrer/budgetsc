# post.R ----------------------------------------------------------------------
# Stage: Bayesian post-estimation, one job per outcome. Transcribed from
# post_reg_augMultisynth_budgetsetters.R: W from the SC fit, prepare_data_general
# with `y ~ 1 + budgetdummy` and the moderator formula, non-dummy moderators
# scaled, gibbs_postscm with the spec's priors, result saved as
# list(post, gdata_light). A stub backend produces draws of the same shape so
# the summariser and tables can be tested without scmBayesPost.

.gdata_light <- function(gdata) list(cov = gdata$cov, Xcols = gdata$cov$Xcols, intX = gdata$cov$intX,
                                     J0 = gdata$cov$J0, treated_ids = gdata$cov$treated_ids,
                                     Z_block = gdata$Z_block)


# Treated units whose moderator row cannot enter the second stage.
# prepare_data_general() builds the moderator matrix from each treated unit's
# row in the LAST week of the panel and model.matrix() silently drops rows with
# a missing value, so one treated unit with an NA moderator leaves Z with fewer
# rows than there are treated units and the sampler fails with "non-conformable
# arguments". The check evaluates the actual model frame (so I(a/b) terms that
# give NaN or Inf are caught too) and returns the units to leave out.
.incomplete_moderator_units <- function(p, f_Z) {
  rhs <- stats::delete.response(stats::terms(.as_formula_checked(f_Z, "post.f_Z")))
  tr <- unique(p$customer_id[p$budgetdummy == 1L])
  last_t <- max(p$wID, na.rm = TRUE)
  zl <- p[wID == last_t & customer_id %in% tr]
  zl <- zl[!duplicated(customer_id)]
  no_row <- setdiff(tr, zl$customer_id)                       # treated unit absent in the last week
  bad <- character(0)
  if (nrow(zl)) {
    mf <- stats::model.frame(rhs, data = zl, na.action = stats::na.pass)
    mm <- stats::model.matrix(rhs, mf)
    ok <- apply(mm, 1, function(r) all(is.finite(r)))
    bad <- zl$customer_id[!ok]
  }
  unique(c(bad, no_row))
}

#' Fit the post-estimation model for one outcome (a job)
#'
#' @param run A `bsc_run` with `panel.rds` and `sc_fit.rds` present.
#' @param outcome Outcome column name (must exist in the panel).
#' @param lock Optional `bsc_lock` to heartbeat during long runs.
#' @param backend Optional function overriding `spec$post$backend`.
#' @return The result list, invisibly (also written to `post/<outcome>.rds`).
#' @export
fit_post_one <- function(run, outcome, lock = NULL, backend = NULL) {
  spec <- run$spec
  p <- readRDS(file.path(run$dir, "panel.rds")); data.table::setDT(p)
  if (!outcome %in% names(p)) stop("outcome '", outcome, "' not in panel", call. = FALSE)
  scfit <- readRDS(file.path(run$dir, "sc_fit.rds"))
  # treated units the SC stage could not fit (too few eligible donors) have no weights: leave them out, say so
  tr_all <- unique(p$customer_id[p$budgetdummy == 1L]); tr_fit <- as.character(scfit$fit$treated_unit_ids)
  no_fit <- tr_all[!as.character(tr_all) %in% tr_fit]
  if (length(no_fit)) {
    p <- p[!customer_id %in% no_fit]
    bsc_log(run, "post/", outcome, ": ", length(no_fit), " treated unit(s) without an SC fit left out")
  }
  # treated units with a missing moderator cannot enter the second stage: leave them out, say so
  drop_ids <- .incomplete_moderator_units(p, spec$post$f_Z)
  if (length(drop_ids)) {
    p <- p[!customer_id %in% drop_ids]
    bsc_log(run, "post/", outcome, ": ", length(drop_ids), " treated unit(s) left out (missing or non-finite moderator in f_Z)")
    dir.create(file.path(run$dir, "tables"), showWarnings = FALSE)
    data.table::fwrite(data.table::data.table(customer_id = drop_ids, reason = "missing or non-finite moderator in f_Z"),
                       file.path(run$dir, "tables", "post_units_left_out.csv"))
  }
  fun <- backend %||% switch(spec$post$backend %||% "scmbayes",
                              scmbayes = post_backend_scmbayes, scmbayes_gap = post_backend_gap, stub = post_backend_stub,
                              stop("unknown post backend"))
  if (!is.null(lock)) lock_heartbeat(lock)
  res <- fun(p, scfit, outcome, spec, lock)
  dir.create(file.path(run$dir, "post"), showWarnings = FALSE)
  if (!is.null(spec$post$thin) && spec$post$thin > 1L)
    res$post$beta_samples <- res$post$beta_samples[seq(1, nrow(res$post$beta_samples), by = spec$post$thin), , drop = FALSE]
  saveRDS(res, file.path(run$dir, "post", paste0(outcome, ".rds")))
  invisible(res)
}

#' scmBayesPost backend
#' @param p Panel.
#' @param scfit Saved SC fit list.
#' @param outcome Outcome name.
#' @param spec Run spec.
#' @param lock Optional lock for heartbeats.
#' @return list(post, gdata_light).
#' @export
post_backend_scmbayes <- function(p, scfit, outcome, spec, lock = NULL) {
  if (!requireNamespace("scmBayesPost", quietly = TRUE))
    stop("scmBayesPost is not installed; use spec$post$backend = 'stub' for a dry run.", call. = FALSE)
  # weights over the full SC universe, then restricted to the units still in the
  # post sample (units left out for missing moderators are treated, never donors,
  # so no remaining unit loses weight)
  W <- scmBayesPost::build_W_from_augMultiSynth(scfit$fit, id_universe = as.character(scfit$unit_vals), self_weight = 1)
  in_p <- as.character(unique(p$customer_id)); tr_in_p <- as.character(unique(p$customer_id[p$budgetdummy == 1L]))
  W <- W[rownames(W) %in% in_p, colnames(W) %in% tr_in_p, drop = FALSE]
  args <- list(
    dta = p, W = W, y_name = outcome,
    f.X = stats::reformulate(c("1", "budgetdummy"), response = outcome),
    f.Z = .as_formula_checked(spec$post$f_Z, "post.f_Z"),
    id_col = "customer_id", time_col = "wID", tr_col = "budgetdummy",
    treat_type = "binary", second_stage = "moderators",
    first_stage = spec$post$first_stage, verbose = isTRUE(spec$post$verbose))
  # weight floor (post.w_min): passed only when the installed scmBayesPost supports it
  w_min <- spec$post$w_min %||% 0
  if (w_min > 0) {
    if (!"w_min" %in% names(formals(scmBayesPost::prepare_data_general)))
      stop("post.w_min > 0 needs scmBayesPost with the w_min argument (prepare_data_wmin patch).", call. = FALSE)
    args$w_min <- w_min
  }
  gdata <- do.call(scmBayesPost::prepare_data_general, args)
  Z <- gdata$Z_block
  is_dummy <- apply(Z, 2, function(x) all(stats::na.omit(unique(x)) %in% c(0, 1)))
  sc_cols <- !(is_dummy | colnames(Z) == "Intercept")
  if (any(sc_cols)) Z[, sc_cols] <- scale(Z[, sc_cols])
  gdata$Z_block <- Z
  rm(p); gc()
  set.seed(spec$post$gibbs$seed %||% 1L)
  run_gibbs <- function() scmBayesPost::gibbs_postscm(gdata, n_iter = spec$post$gibbs$n_iter,
                                                      burn_in = spec$post$gibbs$burn_in,
                                                      control = spec$post$priors)
  # gibbs_postscm() always draws a text progress bar; silence it unless verbose
  post <- if (isTRUE(spec$post$verbose)) run_gibbs() else {
    out <- NULL; utils::capture.output(out <- run_gibbs(), file = nullfile()); out
  }
  list(post = post, gdata_light = .gdata_light(gdata))
}


#' scmBayesPost backend on synthetic-control gaps
#'
#' Stage 2 on the gap series instead of the raw outcomes. For each treated unit
#' the outcome is replaced by (unit outcome - weighted donor outcome) in the same
#' calendar week, over the window `T - L .. T + H`, indexed by event time, and
#' the unit is regressed on the right-hand side of `post.f_X` (default
#' `~ 1 + budgetdummy`, the behaviour before 0.6.0). The hierarchical shrinkage
#' and the moderator regression are those of [post_backend_scmbayes()]; the
#' moderators act on the coefficient of `post.tr_col` (default `budgetdummy`).
#'
#' Event-time regressors available in `post.f_X` (all per unit; the hierarchical
#' prior shrinks every coefficient other than the treatment one toward zero):
#' * `tau`: event week (a unit-specific linear pre/post trend when included);
#' * `heldout_block`: 1 on the last `post.heldout_weeks` pre-adoption weeks
#'   (default `sc.match_end`; all zeros when that is 0), the weeks the SC fit did
#'   not use. Its coefficient is the held-out divergence, and including it keeps
#'   those weeks out of the baseline for the treatment coefficient;
#' * `win_0_7`, `win_8_20`, `win_21_40` (or the names in `post.windows`): post
#'   windows, for dynamics. To moderate one of them set `post.tr_col` to its name.
#'
#' For the `two_launch` design, treated post weeks on or after the public launch
#' are dropped (donors gain access then): `window$mask_wID` from the sample.
#' @inheritParams post_backend_scmbayes
#' @return list(post, gdata_light).
#' @export
post_backend_gap <- function(p, scfit, outcome, spec, lock = NULL) {
  if (!requireNamespace("scmBayesPost", quietly = TRUE)) stop("scmBayesPost is not installed.", call. = FALSE)
  data.table::setDT(p)
  Wm <- scfit$fit$weights_mat
  Yp <- build_Ylist(p, outcome); Y <- Yp$Y_list[[1]]; tv <- Yp$time_vals
  tr <- intersect(rownames(Wm), rownames(Y))            # p may have lost treated units (no SC fit, missing moderator): keep those still in p
  Wm <- Wm[tr, rownames(Y), drop = FALSE]               # align weights and outcomes by unit name
  synth <- Wm %*% Y
  L <- as.integer(spec$sample$n_lags); H <- as.integer(spec$sample$n_leads)
  t0 <- scfit$treat_time[match(tr, as.character(scfit$unit_vals))]
  rows <- data.table::rbindlist(lapply(seq_along(tr), function(j) {
    idx <- (t0[j] - L):(t0[j] + H); idx <- idx[idx >= 1 & idx <= ncol(Y)]
    data.table::data.table(customer_id = tr[j], wID_cal = tv[idx], tau = idx - t0[j],
                           gap = Y[tr[j], idx] - synth[j, idx])
  }))
  if (is.integer(p$customer_id)) rows[, customer_id := as.integer(customer_id)] else if (is.numeric(p$customer_id)) rows[, customer_id := as.numeric(customer_id)]
  dta <- merge(rows, p, by.x = c("customer_id", "wID_cal"), by.y = c("customer_id", "wID"))
  dta[, (outcome) := gap]
  dta[, wID := tau + L + 1L]                                   # event-time index 1..L+H+1 (last row = tau = H for everyone)
  dta[, budgetdummy := as.integer(tau >= 0L)]
  ## event-time regressors for post.f_X (unused unless the formula names them; defaults reproduce `~ 1 + budgetdummy`)
  hw <- as.integer(spec$post$heldout_weeks %||% spec$sc$match_end %||% 0L)
  dta[, heldout_block := as.integer(hw > 0L & tau >= -hw & tau < 0L)]
  win <- spec$post$windows %||% list(win_0_7 = c(0L, 7L), win_8_20 = c(8L, 20L), win_21_40 = c(21L, 40L))
  for (nm in names(win)) dta[, (nm) := as.integer(tau >= win[[nm]][1] & tau <= win[[nm]][2])]
  ## two-launch design: treated post weeks on or after the public launch are not used (the donors gain access then)
  if (identical(spec$sample$design, "two_launch")) {
    mask <- .mask_wID(spec)
    n0 <- nrow(dta); dta <- dta[!(tau >= 0L & wID_cal >= mask)]
    if (!is.null(lock)) lock_heartbeat(lock)
    message(sprintf("two_launch: %d treated post weeks on/after the public launch (wID %d) masked", n0 - nrow(dta), mask))
  }
  drop_ids <- .incomplete_moderator_units(dta, spec$post$f_Z)
  if (length(drop_ids)) dta <- dta[!customer_id %in% drop_ids]
  ids <- as.character(unique(dta$customer_id))
  W <- diag(length(ids)); dimnames(W) <- list(ids, ids)        # each unit is its own (only) pseudo-panel member
  f_X <- .post_fX(spec, outcome); tr_col <- spec$post$tr_col %||% "budgetdummy"
  gdata <- scmBayesPost::prepare_data_general(
    dta = dta, W = W, y_name = outcome, f.X = f_X,
    f.Z = .as_formula_checked(spec$post$f_Z, "post.f_Z"), id_col = "customer_id", time_col = "wID", tr_col = tr_col,
    treat_type = "binary", second_stage = "moderators", first_stage = spec$post$first_stage, verbose = FALSE)
  Z <- gdata$Z_block
  is_dummy <- apply(Z, 2, function(x) all(stats::na.omit(unique(x)) %in% c(0, 1)))
  sc_cols <- !(is_dummy | colnames(Z) == "Intercept")
  if (any(sc_cols)) Z[, sc_cols] <- scale(Z[, sc_cols])
  gdata$Z_block <- Z
  set.seed(spec$post$gibbs$seed %||% 1L)
  out <- NULL
  utils::capture.output(out <- scmBayesPost::gibbs_postscm(gdata, n_iter = spec$post$gibbs$n_iter,
                          burn_in = spec$post$gibbs$burn_in, control = spec$post$priors), file = nullfile())
  list(post = out, gdata_light = .gdata_light(gdata))
}

#' Stub backend: unit effects = mean post gap from the SC fit + noise
#' @inheritParams post_backend_scmbayes
#' @export
post_backend_stub <- function(p, scfit, outcome, spec, lock = NULL) {
  tau <- scfit$fit$tau; ids <- as.character(scfit$fit$treated_unit_ids)
  m <- if (outcome %in% scfit$outcomes) which(scfit$outcomes == outcome) else NULL
  if (is.null(m)) {                                     # unmatched outcome: recompute gap with the weights
    Yp <- build_Ylist(p, outcome); Y <- Yp$Y_list[[1]]
    Wm <- scfit$fit$weights_mat; Wm <- Wm[, rownames(Y), drop = FALSE]; synth <- Wm %*% Y
    L <- scfit$fit$L; H <- scfit$fit$K
    unit_mean <- vapply(seq_len(nrow(Wm)), function(j) {
      id <- rownames(Wm)[j]; t0 <- scfit$treat_time[match(id, as.character(scfit$unit_vals))]
      pre <- max(1, t0 - L):(t0 - 1); post <- t0:min(ncol(Y), t0 + H)
      if (!id %in% rownames(Y)) return(NA_real_)
      mean(Y[id, post] - synth[j, post], na.rm = TRUE) - mean(Y[id, pre] - synth[j, pre], na.rm = TRUE)
    }, numeric(1))
  } else unit_mean <- apply(tau[, m, , drop = FALSE], 1, mean, na.rm = TRUE)
  keep <- ids %in% as.character(unique(p$customer_id)); ids <- ids[keep]; unit_mean <- unit_mean[keep]
  J0 <- length(ids); S <- spec$post$gibbs$n_iter - spec$post$gibbs$burn_in
  sdu <- stats::sd(unit_mean, na.rm = TRUE) / 4 + 1e-8
  set.seed(spec$post$gibbs$seed %||% 1L)
  beta <- matrix(NA_real_, S, 2 * J0)
  for (j in seq_len(J0)) { beta[, 2 * j - 1] <- stats::rnorm(S, 0, 1e-3); beta[, 2 * j] <- stats::rnorm(S, unit_mean[j], sdu) }
  Zterms <- attr(stats::terms(.as_formula_checked(spec$post$f_Z, "post.f_Z")), "term.labels")
  gamma <- matrix(stats::rnorm(S * length(Zterms), 0, 0.01), S, length(Zterms), dimnames = list(NULL, Zterms))
  list(post = list(beta_samples = beta, gamma_samples = gamma, sigma2_samples = stats::rgamma(S, 2, 2),
                   tau_samples = matrix(1, S, 2)),
       gdata_light = list(cov = list(Xcols = c("(Intercept)", "budgetdummy"), intX = 2L, J0 = J0, treated_ids = ids),
                          Xcols = c("(Intercept)", "budgetdummy"), intX = 2L, J0 = J0, treated_ids = ids,
                          Z_block = matrix(0, J0, length(Zterms), dimnames = list(ids, Zterms))))
}

#' Fit the post-estimation stage for all outcomes of a run (this machine)
#'
#' Claims each outcome through the job protocol so other machines can share
#' the run. Use [worker()] with [job_fun_default()] to serve many runs.
#' @param run A `bsc_run`.
#' @param outcomes Subset; default all in the spec.
#' @param backend Optional backend override.
#' @return job table after the run.
#' @export
fit_post <- function(run, outcomes = NULL, backend = NULL) {
  outcomes <- outcomes %||% run$spec$post$outcomes
  for (o in outcomes) {
    lk <- job_claim(run, "post", o)
    if (is.null(lk)) { bsc_log(run, "skip post/", o, " (done or claimed elsewhere)"); next }
    job_run(run, "post", o, function(run, job, lock) fit_post_one(run, job, lock, backend), lk)
  }
  job_table(run, "post")
}

#' Default job function for [worker()]: SC for stage "sc", one outcome for "post"
#' @return A function `(run, job, lock)`.
#' @export
job_fun_default <- function() function(run, job, lock) {
  if (job == "sc_fit") {
    if (!is.null(run$spec$sc$from)) run_reuse_sc(run) else { run_sample(run); fit_sc(run); sc_diagnostics(run); mark_done(run, "sc") }
  } else fit_post_one(run, job, lock)
}

# Right-hand side of the per-unit Stage 2 regression as a two-sided formula for the outcome.
# `post.f_X` may be a one-sided formula or string ("~ 1 + tau + heldout_block + budgetdummy"); NULL = "~ 1 + budgetdummy".
.post_fX <- function(spec, outcome) {
  fx <- spec$post$f_X
  if (is.null(fx)) return(stats::reformulate(c("1", "budgetdummy"), response = outcome))
  f <- if (inherits(fx, "formula")) fx else stats::as.formula(as.character(fx))
  if (length(f) == 3L) f <- f[-2L]                              # drop a response if one was given
  rhs <- attr(stats::terms(f), "term.labels")
  int <- attr(stats::terms(f), "intercept") == 1L
  tr_col <- spec$post$tr_col %||% "budgetdummy"
  if (!tr_col %in% rhs) stop("post.f_X must contain post.tr_col ('", tr_col, "'); it has: ", paste(rhs, collapse = ", "), call. = FALSE)
  stats::reformulate(c(if (int) "1" else "0", rhs), response = outcome)
}

# First masked calendar week for the two-launch design: the public launch (from the timeline).
.mask_wID <- function(spec) {
  tl <- bsc_timeline()
  first_week_on_or_after(tl$events$public_launch, tl$origin)
}

utils::globalVariables(c("customer_id", "wID", "budgetdummy", "wID_cal", "heldout_block", "tau"))
