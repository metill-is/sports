# Football v2: the Stan (NUTS) reference engine. It fits the same spec as the
# package's INLA engine (R/model-v2-inla.R) and returns posterior draws in the
# same layout, for like-for-like checks. Research code: it needs cmdstanr and
# is not on the production fit path.

#' Path of the Stan program that implements a spec.
fv2_stan_file <- function(spec, dir, est_rho = FALSE) {
  file.path(dir, fv2_stan_program(spec, est_rho))
}

#' Fit the spec with Stan (NUTS). Covers attack/defence walks and strength
#' with an optional fixed style, with a plain or a reverting season break.
#'
#' @param est_rho Estimate the reversion coefficient (break clock only).
fv2_fit_stan <- function(spec, stan_file, exe_dir, output_dir,
                         chains = 4L, iter_warmup = 1000L, iter_sampling = 1000L,
                         adapt_delta = 0.95, seed = 20261115L, refresh = 200L,
                         est_rho = FALSE) {
  stopifnot(requireNamespace("cmdstanr", quietly = TRUE))
  stopifnot(basename(stan_file) == fv2_stan_program(spec, est_rho))
  mod <- cmdstanr::cmdstan_model(stan_file, dir = exe_dir, quiet = TRUE)
  t0 <- Sys.time()
  fit <- mod$sample(
    data = fv2_stan_data(spec, est_rho), chains = chains, parallel_chains = chains,
    iter_warmup = iter_warmup, iter_sampling = iter_sampling,
    adapt_delta = adapt_delta, max_treedepth = 10L, init = 0.1, seed = seed,
    refresh = refresh, output_dir = output_dir,
    show_messages = FALSE, show_exceptions = FALSE
  )
  wall <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  brk <- spec$opts$clock == "break"
  fields <- if (spec$opts$param == "sq") "str" else c("att", "def")
  style <- spec$opts$param == "sq" && spec$opts$style == "static"
  indexed <- basename(stan_file) == "fv2_rev.stan"
  # Stan variable behind each hyperparameter name the engines share.
  walk_vars <- unlist(lapply(seq_along(fields), function(i) {
    names <- paste0(c("tau", "sigma", if (brk) "kappa"), "_", fields[i])
    stats::setNames(if (indexed) paste0(c("tau", "sigma", "kappa"), "[", i, "]") else names, names)
  }))
  hyper_vars <- c(
    walk_vars, if (style) c(tau_sty = "tau_sty"),
    c(omega_att = "omega_att", omega_def = "omega_def"),
    if (spec$opts$div_offsets) c(delta_goal = "delta_goal", delta_ha = "delta_ha"),
    if (isTRUE(spec$opts$level_walk)) c(sigma_lvl_goal = "sigma_lvl_goal", sigma_lvl_ha = "sigma_lvl_ha"),
    if (est_rho) c(rho = "rho")
  )
  mat <- function(v) unclass(posterior::as_draws_matrix(fit$draws(v)))
  hyp <- mat(unname(hyper_vars))
  colnames(hyp) <- names(hyper_vars)
  draws <- list(
    c_home = as.numeric(mat("c_home")), c_away = as.numeric(mat("c_away")),
    att = mat("att"), def = mat("def"), ha_att = mat("ha_att"), ha_def = mat("ha_def"),
    hyper = hyp
  )
  if (spec$opts$div_offsets) {
    draws$g_goal <- mat("g_goal")
    draws$g_ha <- mat("g_ha")
  }
  if (isTRUE(spec$opts$level_walk)) {
    draws$lvl_goal <- mat("lvl_goal")
    draws$lvl_ha <- mat("lvl_ha")
  }
  for (nm in c("att", "def", "ha_att", "ha_def", "g_goal", "g_ha", "lvl_goal", "lvl_ha")) {
    if (!is.null(draws[[nm]])) dimnames(draws[[nm]]) <- NULL
  }
  dg <- fit$diagnostic_summary(quiet = TRUE)
  sp <- fit$sampler_diagnostics(format = "df")
  summ <- posterior::summarise_draws(
    fit$draws(c("c_home", "c_away", unname(hyper_vars), "lp__")),
    "mean", "sd", ~ stats::quantile(.x, c(0.05, 0.5, 0.95)), "rhat", "ess_bulk", "ess_tail"
  )
  state_summ <- posterior::summarise_draws(fit$draws(c("att", "def")), "rhat", "ess_bulk")
  list(
    engine = "stan-nuts", draws = draws, summary = as.data.frame(summ),
    chain_id = rep(seq_len(chains), each = iter_sampling),
    diagnostics = list(
      wall_secs = wall, time = fit$time(),
      n_divergent = sum(dg$num_divergent), n_max_treedepth = sum(dg$num_max_treedepth),
      ebfmi = dg$ebfmi, mean_leapfrog = mean(sp$n_leapfrog__),
      state_max_rhat = max(state_summ$rhat, na.rm = TRUE),
      state_min_ess = min(state_summ$ess_bulk, na.rm = TRUE)
    ),
    log_lik = mat("log_lik")
  )
}
