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
  rhs <- stats::delete.response(stats::terms(stats::as.formula(f_Z)))
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
                              scmbayes = post_backend_scmbayes, stub = post_backend_stub,
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
    f.Z = stats::as.formula(spec$post$f_Z),
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

#' Stub backend: unit effects = mean post gap from the SC fit + noise
#' @inheritParams post_backend_scmbayes
#' @export
post_backend_stub <- function(p, scfit, outcome, spec, lock = NULL) {
  tau <- scfit$fit$tau; ids <- as.character(scfit$fit$treated_unit_ids)
  m <- if (outcome %in% scfit$outcomes) which(scfit$outcomes == outcome) else NULL
  if (is.null(m)) {                                     # unmatched outcome: recompute gap with the weights
    Yp <- build_Ylist(p, outcome); Y <- Yp$Y_list[[1]]; synth <- scfit$fit$weights_mat %*% Y
    tt <- scfit$treat_time; tr <- which(is.finite(tt)); L <- scfit$fit$L; H <- scfit$fit$K
    unit_mean <- vapply(seq_along(tr), function(j) {
      t0 <- tt[tr[j]]; pre <- max(1, t0 - L):(t0 - 1); post <- t0:min(ncol(Y), t0 + H)
      mean(Y[tr[j], post] - synth[j, post], na.rm = TRUE) - mean(Y[tr[j], pre] - synth[j, pre], na.rm = TRUE)
    }, numeric(1))
  } else unit_mean <- apply(tau[, m, , drop = FALSE], 1, mean, na.rm = TRUE)
  keep <- ids %in% as.character(unique(p$customer_id)); ids <- ids[keep]; unit_mean <- unit_mean[keep]
  J0 <- length(ids); S <- spec$post$gibbs$n_iter - spec$post$gibbs$burn_in
  sdu <- stats::sd(unit_mean, na.rm = TRUE) / 4 + 1e-8
  set.seed(spec$post$gibbs$seed %||% 1L)
  beta <- matrix(NA_real_, S, 2 * J0)
  for (j in seq_len(J0)) { beta[, 2 * j - 1] <- stats::rnorm(S, 0, 1e-3); beta[, 2 * j] <- stats::rnorm(S, unit_mean[j], sdu) }
  Zterms <- attr(stats::terms(stats::as.formula(spec$post$f_Z)), "term.labels")
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
  if (job == "sc_fit") { run_sample(run); fit_sc(run); sc_diagnostics(run); mark_done(run, "sc") } else fit_post_one(run, job, lock)
}

utils::globalVariables(c("customer_id", "wID", "budgetdummy"))
