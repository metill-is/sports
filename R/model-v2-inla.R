# Football model v2: the R-INLA engine. It takes a spec from fv2_build() and
# returns posterior draws in the engine-neutral layout (draws in rows):
# c_home, c_away, att[, slot], def[, slot], ha_att[, team], ha_def[, team],
# optional g_goal / g_ha[, cell] and lvl_goal / lvl_ha[, week], and
# hyper[, name] on the sd scale. INLA is an optional dependency: it is only
# needed by a league whose config says `engine: inla`.

#' Names of a spec's field hyperparameters, by field.
#' @noRd
fv2_hyper_names <- function(spec) {
  cls <- setdiff(names(fv2_structure(spec)), "init")
  sd_name <- c(`in` = "sigma", brk = "kappa", mov = "kappa_mov")
  fields <- if (spec$opts$param == "ad") c("att", "def") else c("str", if (spec$opts$style == "rw") "sty")
  out <- list()
  for (f in fields) out[[f]] <- paste0(c("tau", sd_name[cls]), "_", f)
  out
}

#' Number of hyperparameters INLA estimates for a spec.
#' @noRd
fv2_n_theta <- function(spec) {
  o <- spec$opts
  length(unlist(fv2_hyper_names(spec))) + 2L + 2L * isTRUE(o$div_offsets) +
    2L * isTRUE(o$level_walk) + (o$param == "sq" && o$style == "static")
}

#' Abort unless INLA used a PC prior for every hyperparameter of the spec.
#'
#' INLA falls back to its default priors without a word when it cannot read a
#' `hyper` specification from a formula built in code. Every hyperparameter
#' here carries a PC prior, so the number INLA reports must equal the number
#' the spec defines.
#' @noRd
fv2_check_inla_priors <- function(result, spec) {
  priors <- unlist(lapply(result$all.hyper$random, function(x) {
    vapply(x$hyper, function(h) if (isTRUE(h$fixed)) NA_character_ else h$prior, character(1))
  }))
  n_pc <- sum(priors == "pc.prec", na.rm = TRUE)
  if (n_pc != fv2_n_theta(spec)) {
    stop(
      "INLA used a PC prior for ", n_pc, " hyperparameters where the spec defines ",
      fv2_n_theta(spec), ": a `hyper` specification was not read.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Fit the spec with R-INLA.
#'
#' @param int_strategy "auto" (CCD or grid over the hyperparameters), "eb"
#'   (hyperparameters fixed at their mode), "grid", "ccd".
#' @param theta_init Optional internal-scale hyperparameter start (a previous
#'   fit's `theta_mode`), which shortens the mode search.
#' @param theta_fixed Hold the hyperparameters at `theta_init` instead of
#'   re-estimating them: only the states are refitted.
#' @noRd
fv2_fit_inla <- function(spec, int_strategy = "auto", n_samp = 1000L,
                         threads = "4:1", theta_init = NULL, theta_fixed = FALSE, seed = 1L,
                         verbose = FALSE, keep_result = FALSE) {
  stopifnot(requireNamespace("INLA", quietly = TRUE))
  p <- spec$priors
  o <- spec$opts
  S <- spec$S
  K <- spec$K
  des <- fv2_design(spec, "obs")
  str <- fv2_structure(spec)
  cls <- names(str)
  prior_of <- c(init = "tau", `in` = "sigma", brk = "kappa", mov = "kappa")
  start_sd <- c(init = 0.5, `in` = 0.04, brk = 0.2, mov = 0.3)
  pc <- function(ua, sd0) list(prior = "pc.prec", param = ua, initial = -2 * log(sd0))
  field_hyper <- function() {
    h <- lapply(cls, function(cl) pc(p[[prior_of[[cl]]]], start_sd[[cl]]))
    names(h) <- paste0("theta", seq_along(cls))
    h
  }
  cmat <- lapply(str, function(m) methods::as(methods::as(m, "generalMatrix"), "TsparseMatrix"))
  names(cmat) <- NULL
  entry <- matrix(0, 1L, S)
  entry[1L, spec$first_slot] <- 1
  con <- list(A = entry, e = 0)
  hyp <- field_hyper()
  hyp_iid <- list(prec = pc(p$omega, 0.05))
  hyp_cell <- list(prec = pc(p$delta, 0.05))
  hyp_level <- list(prec = pc(p$level, 0.01))
  week_values <- spec$weeks

  y <- c(spec$obs$goals_home, spec$obs$goals_away)
  fixed <- data.frame(c_home = des$c_home, c_away = des$c_away)
  if (o$movers) {
    fixed$promoted <- des$promoted
    fixed$relegated <- des$relegated
  }
  if (o$param == "ad") {
    A <- list(des$att, des$def)
    eff <- list(f1 = seq_len(S), f2 = seq_len(S))
  } else {
    A <- list(des$att + des$def)
    eff <- list(f1 = seq_len(S))
    if (o$style == "rw") {
      A <- c(A, list(des$att - des$def))
      eff <- c(eff, list(f2 = seq_len(S)))
    } else if (o$style == "static") {
      to_team <- Matrix::sparseMatrix(i = seq_len(S), j = spec$slots$team, x = 1, dims = c(S, K))
      A <- c(A, list((des$att - des$def) %*% to_team))
      eff <- c(eff, list(sty_k = seq_len(K)))
    }
  }
  A <- c(A, list(des$ha_att, des$ha_def))
  eff <- c(eff, list(ha_att = seq_len(K), ha_def = seq_len(K)))
  if (o$div_offsets) {
    A <- c(A, list(des$g_goal, des$g_ha))
    eff <- c(eff, list(g_goal = seq_along(spec$cells), g_ha = seq_along(spec$cells)))
  }
  level <- isTRUE(o$level_walk)
  if (level) {
    A <- c(A, list(des$lvl_goal, des$lvl_ha))
    eff <- c(eff, list(lvl_goal = spec$weeks, lvl_ha = spec$weeks))
  }
  A <- c(A, list(1))
  eff <- c(eff, list(fixed))
  stk <- INLA::inla.stack(data = list(y = y), A = A, effects = eff, remove.unused = FALSE)

  terms <- c(
    "-1", names(fixed),
    "f(f1, model = 'generic3', Cmatrix = cmat, hyper = hyp, extraconstr = con)",
    if ("f2" %in% names(eff)) "f(f2, model = 'generic3', Cmatrix = cmat, hyper = hyp, extraconstr = con)",
    if ("sty_k" %in% names(eff)) "f(sty_k, model = 'iid', constr = TRUE, hyper = list(prec = pc(p$tau, 0.2)))",
    "f(ha_att, model = 'iid', hyper = hyp_iid)",
    "f(ha_def, model = 'iid', hyper = hyp_iid)",
    if (o$div_offsets) c("f(g_goal, model = 'iid', hyper = hyp_cell)", "f(g_ha, model = 'iid', hyper = hyp_cell)"),
    # WHY: rw1 on the week values themselves, so a gap of d weeks carries d
    # weeks of variance; the sum-to-zero constraint leaves the level to the
    # intercepts.
    if (level) {
      paste0(
        "f(", c("lvl_goal", "lvl_ha"),
        ", model = 'rw1', values = week_values, constr = TRUE, scale.model = FALSE, hyper = hyp_level)"
      )
    }
  )
  form <- stats::as.formula(paste("y ~", paste(terms, collapse = " + ")))
  environment(form) <- environment()

  fixed_mean <- c(list(c_home = p$c_mean, c_away = p$c_mean), list(default = 0))
  fixed_prec <- c(list(c_home = 1 / p$c_sd^2, c_away = 1 / p$c_sd^2), list(default = 1 / p$beta_sd^2))
  t0 <- Sys.time()
  r <- INLA::inla(
    form,
    family = "poisson", data = INLA::inla.stack.data(stk),
    control.predictor = list(A = INLA::inla.stack.A(stk), compute = FALSE, link = 1),
    control.fixed = list(mean = fixed_mean, prec = fixed_prec),
    control.compute = list(config = TRUE, dic = FALSE, waic = FALSE, mlik = TRUE),
    control.inla = list(int.strategy = int_strategy),
    control.mode = if (is.null(theta_init)) {
      list()
    } else {
      list(theta = theta_init, restart = !theta_fixed, fixed = theta_fixed)
    },
    num.threads = threads, verbose = verbose
  )
  t_fit <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  fv2_check_inla_priors(r, spec)

  t1 <- Sys.time()
  sel <- lapply(eff[names(eff) != ""], function(i) if (is.data.frame(i)) i else seq_along(i))
  sel <- sel[!vapply(sel, is.data.frame, logical(1))]
  for (nm in names(fixed)) sel[[nm]] <- 1L
  smp <- INLA::inla.posterior.sample(n_samp, r, selection = sel, seed = seed, num.threads = 1L)
  lat <- vapply(smp, function(s) s$latent[, 1L], numeric(nrow(smp[[1L]]$latent)))
  rn <- rownames(smp[[1L]]$latent)
  block <- function(nm) t(lat[startsWith(rn, paste0(nm, ":")), , drop = FALSE])
  # WHY: with held hyperparameters INLA reports none back, so the draws carry
  # the values that were held.
  if (theta_fixed) {
    hyp_draws <- matrix(exp(-theta_init / 2), length(smp), length(theta_init), byrow = TRUE)
  } else {
    hyp_draws <- 1 / sqrt(t(vapply(smp, function(s) s$hyperpar, numeric(length(smp[[1L]]$hyperpar)))))
    if (length(smp[[1L]]$hyperpar) == 1L) hyp_draws <- t(hyp_draws)
  }
  t_samp <- as.numeric(difftime(Sys.time(), t1, units = "secs"))

  hn <- fv2_hyper_names(spec)
  hyper_names <- c(
    hn[[1L]], if ("f2" %in% names(eff)) hn[[2L]], if ("sty_k" %in% names(eff)) "tau_sty",
    "omega_att", "omega_def", if (o$div_offsets) c("delta_goal", "delta_ha"),
    if (level) c("sigma_lvl_goal", "sigma_lvl_ha")
  )
  stopifnot(length(hyper_names) == ncol(hyp_draws))
  colnames(hyp_draws) <- hyper_names

  f1 <- block("f1")
  if (o$param == "ad") {
    att <- f1
    def <- block("f2")
  } else {
    sty <- switch(o$style,
      rw = block("f2"),
      static = block("sty_k")[, spec$slots$team, drop = FALSE],
      none = 0
    )
    att <- f1 + sty
    def <- f1 - sty
  }
  if (o$movers) {
    shift <- outer(as.numeric(block("promoted")), spec$promoted) +
      outer(as.numeric(block("relegated")), spec$relegated)
    att <- att + shift
    def <- def + shift
  }
  draws <- list(
    c_home = as.numeric(block("c_home")), c_away = as.numeric(block("c_away")),
    att = att, def = def, ha_att = block("ha_att"), ha_def = block("ha_def"),
    hyper = hyp_draws
  )
  if (o$div_offsets) {
    draws$g_goal <- block("g_goal")
    draws$g_ha <- block("g_ha")
  }
  if (level) {
    draws$lvl_goal <- block("lvl_goal")
    draws$lvl_ha <- block("lvl_ha")
  }

  # WHY: the sd summary is taken from the log-precision marginal; the
  # precision-scale marginal of a near-zero sd can carry non-positive
  # abscissae, which breaks the 1 / sqrt(x) transform.
  marg_sd <- function(m) {
    tryCatch(
      {
        ms <- INLA::inla.tmarginal(function(x) exp(-x / 2), m)
        c(
          INLA::inla.emarginal(function(x) x, ms),
          INLA::inla.qmarginal(c(0.05, 0.5, 0.95), ms)
        )
      },
      error = function(e) rep(NA_real_, 4L)
    )
  }
  if (theta_fixed) {
    hyper_summary <- cbind(hyp_draws[1L, ], NA_real_, hyp_draws[1L, ], NA_real_)
  } else {
    hyper_summary <- t(vapply(r$internal.marginals.hyperpar, marg_sd, numeric(4)))
  }
  dimnames(hyper_summary) <- list(hyper_names, c("mean", "q5", "q50", "q95"))

  out <- list(
    engine = paste0("inla-", int_strategy), draws = draws,
    hyper_summary = hyper_summary, fixed = r$summary.fixed,
    state_summary = lapply(r$summary.random, function(d) d[, c("mean", "sd")]),
    theta_mode = if (theta_fixed) theta_init else r$mode$theta,
    mode_status = if (theta_fixed) 0L else r$mode$mode.status,
    mlik = r$mlik[1L, 1L], n_config = r$misc$configs$nconfig,
    t_fit = t_fit, t_samp = t_samp, cpu = r$cpu.used
  )
  if (keep_result) out$result <- r
  out
}
