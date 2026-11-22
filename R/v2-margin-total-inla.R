#' @include v2-margin-total-data.R
NULL

#' Prior scales for the v2 (margin, total) model, per sport.
#'
#' PROTOTYPE (2026-11-15, handball / basketball v2): not called by the
#' production fit path.
#'
#' Every standard deviation in the model gets an exponential prior stated as
#' "the sd exceeds `u_*` with probability 1 %": R-INLA's `pc.prec(u, 0.01)`
#' and, identically, `exponential(-log(0.01) / u)` in Stan. The values are in
#' score units (goals for handball, points for basketball) and are loose by
#' design: each is several times the size the quantity is expected to take.
#'
#' * `u_sigma_m`, `u_sigma_t`: sd of one match's margin / total around its
#'   mean. A handball margin has sd about 5 to 7 goals, a basketball margin
#'   about 11 to 14 points.
#' * `u_b`: spread across clubs of the long-run level (club stature).
#' * `u_u`: spread of a club's season level around its long-run level (the
#'   roster a club happens to have that year).
#' * `u_w`: within-season drift per week (injuries, mid-season signings),
#'   for the random-walk form; `u_w_ar1`: spread of a team's within-season
#'   deviation from its season level, for the AR(1) form.
#' * `u_ha`: spread of team deviations from the league home advantage.
#' * `u_season`: season-to-season step in a division's scoring level.
#' * `hfa_sd`: sd of the normal(0, .) prior on the league home advantage.
#' * `total_mean`, `total_mean_sd`: normal prior on a division's mean total.
#' * `rho_mu`, `rho_sd`: normal prior on `log((1 + rho) / (1 - rho))`, where
#'   `rho` is the season-to-season memory of the season level. Centred on
#'   `rho = 0.7` (basketball literature about 0.62, football 0.74 to 0.80)
#'   with a 95 % range of roughly 0.13 to 0.92.
#'
#' @param sport `"handball"` or `"basketball"`.
#' @return Named list of prior scales.
#' @noRd
v2_priors <- function(sport = c("handball", "basketball")) {
  sport <- match.arg(sport)
  common <- list(rho_mu = log(1.7 / 0.3), rho_sd = 0.75)
  by_sport <- switch(sport,
    handball = list(
      u_sigma_m = 15, u_sigma_t = 20, u_b = 10, u_u = 8, u_w = 1, u_w_ar1 = 4, u_ha = 3,
      u_season = 5, hfa_sd = 5, total_mean = 55, total_mean_sd = 20
    ),
    basketball = list(
      u_sigma_m = 30, u_sigma_t = 40, u_b = 25, u_u = 20, u_w = 2.5, u_w_ar1 = 10, u_ha = 6,
      u_season = 10, hfa_sd = 10, total_mean = 165, total_mean_sd = 40
    )
  )
  c(by_sport, common)
}

#' Fit one half of the v2 model (margin or total) with R-INLA.
#'
#' PROTOTYPE (2026-11-15, handball / basketball v2): not called by the
#' production fit path. Needs the INLA package, which is not on CRAN.
#'
#' The model is linear and Gaussian throughout, so R-INLA's Laplace step is
#' exact; the only approximation is the numerical integration over the six
#' hyperparameters.
#'
#' `margin = home - away ~ normal(hfa + ha[home] + s[home] - s[away], sigma)`
#' and
#' `total = home + away ~ normal(level[division, season] + s[home] + s[away], sigma)`,
#' where a team's state on a date is
#' `s = b[team] + u[team, season] + w[team-season, week]`: `b` independent
#' across clubs, `u` an AR(1) across seasons, `w` a random walk over calendar
#' weeks that sums to zero within the season. Prediction rows enter as
#' unobserved responses, so their state is read at the fixture's own week and
#' its uncertainty already includes the drift up to that week.
#'
#' @param v Index data from [build_v2_data()].
#' @param response `"margin"` or `"total"`.
#' @param priors Prior scales, see [v2_priors()].
#' @param within How a team's strength moves inside a season: `"rw1"` (a
#'   weekly random walk that sums to zero over the season; one linear
#'   constraint per team-season, which is what makes it slow), `"ar1"` (a
#'   stationary weekly AR(1) around the season level with the weekly
#'   correlation fixed at `rho_week`; no constraints) or `"none"` (strength is
#'   constant within a season).
#' @param rho_week Weekly correlation of the within-season AR(1).
#' @param club_level Include the long-run club level `b`?
#' @param int_strategy R-INLA hyperparameter integration (`"auto"`, `"ccd"`,
#'   `"grid"` or `"eb"` for the plug-in mode).
#' @param num_threads R-INLA thread specification. Repeatable fits need
#'   `"1:1"`, the default: on more threads R-INLA did not return the same
#'   numbers in two R processes, with its online optimisation on or off (the
#'   football engine's measurement, `R/model-v2-inla.R`). Posterior draws are
#'   always made on one thread, which R-INLA's sampler requires with a seed.
#' @param n_samples If positive, draw this many joint posterior samples of
#'   the prediction rows' linear predictor and of the observation sd.
#' @param cpo Compute leave-one-out predictive densities (`cpo`) for the
#'   training rows?
#' @param theta_start Optional internal hyperparameter vector from a previous
#'   fit of the same model, to start the optimiser from.
#' @param y Optional response vector for the training rows; overrides the
#'   margin or total computed from `v$y_home` and `v$y_away` (used to train
#'   on regulation-time scores).
#' @param seed Seed for the posterior sampler: for R-INLA's sampler of the
#'   latent field and for R's generator, which picks the hyperparameter
#'   configuration of each draw. Drawing does not move the caller's random
#'   stream; R-INLA's fit itself does, with or without draws.
#' @param reproducible Ask R-INLA for this fit with its online internal
#'   optimisation off (`control.compute = list(internal.opt = FALSE)`, for
#'   this call only; R-INLA's session-wide setting is not touched). With the
#'   option on, which is R-INLA's default, the same fit did not always return
#'   the same numbers in two R processes: at each of two cutoffs 37 of 44
#'   processes agreed on the most frequent result. With it off, 75 of 75 did,
#'   and two whole walk-forwards were identical. It does not make the
#'   hyperparameter mode unique; it makes the one that is found repeatable,
#'   and only on one thread (`num_threads = "1:1"`). `FALSE` passes nothing,
#'   so R-INLA goes by its session setting.
#' @return A list: `pred` (latent mean and sd per prediction row), `sigma2`
#'   (posterior mean of the observation variance), `hyper_sd` (posterior
#'   summaries on the sd / correlation scale), `fixed`, `random` (state
#'   summaries), `fitted` (latent mean and sd per training row), `samples`
#'   (when asked for), `cpo`, `theta` (internal mode, for warm starts),
#'   `mode_status` (see [v2_inla_mode_status()]; a warning of class
#'   `v2_inla_mode_not_found` is raised unless it is 0), `mlik`, `secs` and the
#'   `fit` itself.
#' @noRd
fit_v2_inla <- function(v, response = c("margin", "total"),
                        priors, within = c("rw1", "ar1", "none"),
                        rho_week = 0.98, club_level = TRUE,
                        int_strategy = "auto", num_threads = "1:1",
                        n_samples = 0L, cpo = FALSE, theta_start = NULL,
                        y = NULL, seed = 1L, reproducible = TRUE) {
  response <- match.arg(response)
  within <- match.arg(within)
  # WHY before R-INLA is asked: with no training rows its program exits with an error
  # that does not say why.
  if (v$N == 0L) {
    cli::cli_abort("{.fn fit_v2_inla} needs training rows; {.arg v} has none ({v$N_pred} to predict).")
  }
  if (!requireNamespace("INLA", quietly = TRUE)) {
    cli::cli_abort("{.fn fit_v2_inla} needs the {.pkg INLA} package.")
  }
  is_margin <- response == "margin"
  sgn <- if (is_margin) -1 else 1
  n_train <- v$N
  n <- v$N + v$N_pred
  home <- c(v$home, v$home_pred)
  away <- c(v$away, v$away_pred)
  season <- c(v$season, v$season_pred)
  div <- c(v$div, v$div_pred)
  wk <- c(v$wk, v$wk_pred)
  ts_home <- c(v$ts_home, v$ts_home_pred)
  ts_away <- c(v$ts_away, v$ts_away_pred)
  if (is.null(y)) y <- if (is_margin) v$y_home - v$y_away else v$y_home + v$y_away
  stopifnot(length(y) == n_train)
  y_all <- c(y, rep(NA_real_, v$N_pred))

  rows <- seq_len(n)
  pair <- function(j_home, j_away, n_col) {
    Matrix::sparseMatrix(
      i = c(rows, rows), j = c(j_home, j_away),
      x = c(rep(1, n), rep(sgn, n)), dims = c(n, n_col)
    )
  }
  # WHY initial: R-INLA starts every log precision at 4, an sd of 0.14, which is far
  # below any plausible sd on a goals or points scale. The PC prior peaks at zero
  # variance, so from there the optimiser can settle in a degenerate mode with the team
  # effects switched off (seen on men's basketball totals). Start at a fifth of the
  # prior's 99 % bound instead.
  start <- function(u) -2 * log(u / 5)
  pc <- function(u) list(prec = list(prior = "pc.prec", param = c(u, 0.01), initial = start(u)))
  hyper_u <- list(
    prec = list(prior = "pc.prec", param = c(priors$u_u, 0.01), initial = start(priors$u_u)),
    rho = list(prior = "normal", param = c(priors$rho_mu, 1 / priors$rho_sd^2))
  )
  hyper_b <- pc(priors$u_b)
  hyper_w <- pc(priors$u_w)
  hyper_w_ar1 <- list(
    prec = list(prior = "pc.prec", param = c(priors$u_w_ar1, 0.01), initial = start(priors$u_w_ar1)),
    rho = list(initial = log((1 + rho_week) / (1 - rho_week)), fixed = TRUE)
  )
  hyper_ha <- pc(priors$u_ha)
  hyper_season <- pc(priors$u_season)

  a_list <- list(pair((home - 1L) * v$NS + season, (away - 1L) * v$NS + season, v$K * v$NS))
  effects <- list(list(u_s = rep(seq_len(v$NS), v$K), u_k = rep(seq_len(v$K), each = v$NS)))
  terms <- "f(u_s, model = \"ar1\", values = seq_len(v$NS), replicate = u_k, hyper = hyper_u)"
  if (club_level) {
    a_list <- c(a_list, list(pair(home, away, v$K)))
    effects <- c(effects, list(list(b = seq_len(v$K))))
    terms <- c(terms, "f(b, model = \"iid\", values = seq_len(v$K), hyper = hyper_b)")
  }
  if (within != "none") {
    a_list <- c(a_list, list(pair((ts_home - 1L) * v$W + wk, (ts_away - 1L) * v$W + wk, v$TS * v$W)))
    effects <- c(effects, list(list(w_t = rep(seq_len(v$W), v$TS), w_r = rep(seq_len(v$TS), each = v$W))))
    terms <- c(terms, if (within == "rw1") {
      paste0(
        "f(w_t, model = \"rw1\", values = seq_len(v$W), replicate = w_r, constr = TRUE, ",
        "scale.model = FALSE, hyper = hyper_w)"
      )
    } else {
      "f(w_t, model = \"ar1\", values = seq_len(v$W), replicate = w_r, hyper = hyper_w_ar1)"
    })
  }
  if (is_margin) {
    a_list <- c(a_list, list(
      Matrix::sparseMatrix(i = rows, j = home, x = 1, dims = c(n, v$K)), 1
    ))
    effects <- c(effects, list(list(ha = seq_len(v$K)), data.frame(hfa = rep(1, n))))
    terms <- c("hfa", terms, "f(ha, model = \"iid\", values = seq_len(v$K), hyper = hyper_ha)")
    fixed_names <- "hfa"
    control_fixed <- list(mean = 0, prec = 1 / priors$hfa_sd^2)
  } else {
    dummies <- as.data.frame(
      stats::setNames(lapply(seq_len(v$ND), function(d) as.numeric(div == d)), paste0("alpha", seq_len(v$ND)))
    )
    a_list <- c(a_list, list(
      Matrix::sparseMatrix(i = rows, j = (div - 1L) * v$NS + season, x = 1, dims = c(n, v$ND * v$NS)), 1
    ))
    effects <- c(effects, list(
      list(s_s = rep(seq_len(v$NS), v$ND), s_d = rep(seq_len(v$ND), each = v$NS)), dummies
    ))
    terms <- c(names(dummies), terms, paste0(
      "f(s_s, model = \"rw1\", values = seq_len(v$NS), replicate = s_d, constr = TRUE, ",
      "scale.model = FALSE, hyper = hyper_season)"
    ))
    fixed_names <- names(dummies)
    control_fixed <- list(mean = priors$total_mean, prec = 1 / priors$total_mean_sd^2)
  }

  # WHY: remove.unused = FALSE keeps every grid node. With the default, nodes no match
  # touches are dropped and R-INLA would build the AR(1) / random walk on the surviving
  # indices as if they were adjacent.
  stk <- INLA::inla.stack(
    data = list(y = y_all), A = a_list, effects = effects, tag = "obs",
    compress = FALSE, remove.unused = FALSE
  )
  form <- stats::as.formula(paste("y ~ -1 +", paste(terms, collapse = " + ")))
  u_sigma <- if (is_margin) priors$u_sigma_m else priors$u_sigma_t

  t0 <- Sys.time()
  fit <- INLA::inla(
    form,
    family = "gaussian", data = INLA::inla.stack.data(stk),
    control.predictor = list(A = INLA::inla.stack.A(stk), compute = TRUE),
    control.family = list(hyper = list(prec = list(
      prior = "pc.prec", param = c(u_sigma, 0.01), initial = start(u_sigma)
    ))),
    control.fixed = control_fixed,
    # WHY per call: R-INLA 25.6.7 reads its session-wide `internal.opt` only when the
    # call's is NULL (inla.problem.section()), so this holds for this fit alone and
    # there is nothing to put back. Not passing it leaves R-INLA's session setting.
    control.compute = c(
      list(config = n_samples > 0L, cpo = cpo, dic = FALSE, waic = FALSE),
      if (reproducible) list(internal.opt = FALSE)
    ),
    control.inla = list(int.strategy = int_strategy),
    control.mode = if (is.null(theta_start)) list() else list(theta = theta_start, restart = TRUE),
    num.threads = num_threads, verbose = FALSE
  )
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  # WHY: R-INLA returns a fit whether or not its search for the hyperparameter mode
  # succeeded, and the summaries of a failed search look like any others.
  mode_status <- v2_inla_mode_status(fit)
  if (!v2_mode_ok(mode_status)) {
    cli::cli_warn(
      c(
        "R-INLA did not report a successful mode search for the {response} model (mode.status = {mode_status}).",
        i = "Its predictions and hyperparameter summaries should not be used unchecked."
      ),
      class = "v2_inla_mode_not_found"
    )
  }

  idx <- INLA::inla.stack.index(stk, "obs")$data
  lp <- fit$summary.linear.predictor[idx, c("mean", "sd")]
  prec_name <- "Precision for the Gaussian observations"
  sigma2 <- INLA::inla.emarginal(
    function(x) exp(-x),
    fit$internal.marginals.hyperpar[["Log precision for the Gaussian observations"]]
  )

  out <- list(
    response = response,
    pred = data.frame(mean = lp$mean[-seq_len(n_train)], sd = lp$sd[-seq_len(n_train)]),
    fitted = data.frame(mean = lp$mean[seq_len(n_train)], sd = lp$sd[seq_len(n_train)]),
    sigma2 = sigma2, hyper_sd = v2_inla_hyper_sd(fit), fixed = fit$summary.fixed[fixed_names, , drop = FALSE],
    random = lapply(fit$summary.random, function(s) s[, c("ID", "mean", "sd")]),
    cpo = if (cpo) fit$cpo$cpo[idx[seq_len(n_train)]] else NULL,
    theta = fit$mode$theta, mode_status = mode_status, mlik = fit$mlik[1, 1], secs = secs, fit = fit
  )
  if (n_samples > 0L && v$N_pred > 0L) {
    pred_rows <- idx[-seq_len(n_train)]
    # WHY: R-INLA's own `seed` reaches its sampler of the latent field only.
    # Which hyperparameter configuration each draw comes from is picked with
    # R's generator, so under full integration the same fit drew differently
    # in two R processes. Seeding R's generator for the call fixes both;
    # with_seed() puts the caller's stream back afterwards.
    smp <- withr::with_seed(
      as.integer(seed),
      INLA::inla.posterior.sample(
        n_samples, fit,
        # WHY "1:1": with a seed the sampler runs on one thread whatever it is asked, and
        # warns when asked for more.
        selection = list(APredictor = pred_rows), seed = seed, num.threads = "1:1"
      )
    )
    out$samples <- list(
      loc = t(vapply(smp, function(s) as.numeric(s$latent[, 1]), numeric(length(pred_rows)))),
      sigma = vapply(smp, function(s) 1 / sqrt(s$hyperpar[[prec_name]]), numeric(1))
    )
  }
  out
}

#' Status of R-INLA's search for the hyperparameter mode.
#'
#' R-INLA writes the status of its optimiser to `fit$mode$mode.status`: 0 when
#' the search ended at a mode it accepts, another value when it did not. A
#' status that is absent or `NA` says nothing about success, so it is
#' returned as `NA` and callers must treat it as a failure.
#'
#' @param fit An `inla` fit.
#' @return One integer: 0 for a successful search, `NA` when R-INLA reported
#'   none.
#' @noRd
v2_inla_mode_status <- function(fit) {
  status <- fit$mode$mode.status
  if (length(status) != 1L || is.na(status)) {
    return(NA_integer_)
  }
  as.integer(status)
}

#' Did the search for the hyperparameter mode succeed?
#'
#' Only a status of exactly 0 counts. A missing status (`NA`) says nothing
#' about success, so it counts as a failure.
#'
#' @param status Integer status, as returned in `fit_v2_inla()$mode_status`.
#' @return Logical, same length as `status`.
#' @noRd
v2_mode_ok <- function(status) {
  !is.na(status) & status == 0L
}

#' Fit, and repeat a failed warm-started fit once from a cold start.
#'
#' The rule the walk-forward follows. A fit started from the previous cutoff's
#' mode can fail where a cold start succeeds, so a warm fit that throws, or
#' whose mode search fails, is repeated once with no starting value. A cold
#' fit is never repeated: it would be the same fit again. When the repeat
#' fails as well the fit comes back as it is, with its status, for the caller
#' to flag; it is not replaced by anything that looks successful.
#'
#' @param fit_fun Function of one argument, the starting value (`NULL` for a
#'   cold start), returning a fit as `fit_v2_inla()` does.
#' @param theta_start Starting value for the first attempt, or `NULL`.
#' @return List: `fit` (`NULL` when every attempt threw), `retried`,
#'   `attempts`.
#' @noRd
v2_fit_with_retry <- function(fit_fun, theta_start = NULL) {
  attempt <- function(theta) tryCatch(fit_fun(theta), error = function(e) NULL)
  fit <- attempt(theta_start)
  attempts <- 1L
  failed <- is.null(fit) || !v2_mode_ok(fit$mode_status)
  if (failed && !is.null(theta_start)) {
    cold <- attempt(NULL)
    attempts <- 2L
    if (!is.null(cold)) fit <- cold
  }
  list(fit = fit, retried = attempts > 1L, attempts = attempts)
}

#' Starting value a fit may hand to the next one.
#'
#' Only a fit whose mode search succeeded passes its mode on. Starting the
#' next cutoff from the end point of a failed search would carry the failure
#' forward.
#'
#' @param fit A fit as `fit_v2_inla()` returns it, or `NULL`.
#' @return The internal mode, or `NULL` for a cold start.
#' @noRd
v2_warm_start <- function(fit) {
  if (is.null(fit) || !isTRUE(v2_mode_ok(fit$mode_status))) {
    return(NULL)
  }
  fit$theta
}

#' Keep the walk-forward predictions that come from successful fits.
#'
#' A cutoff's predictions are scored only when both of its fits (margin and
#' total) ended their mode search with status 0.
#'
#' @param pred Predictions, one row per match, with a `cutoff` column.
#' @param cut One row per cutoff, with `cutoff`, `status_m` and `status_t`.
#' @return The rows of `pred` to score, with the number of rows left out as
#'   attribute `"dropped"`. A walk-forward stored without the status columns
#'   comes back whole with `dropped = NA`: nothing can be known about it.
#' @noRd
v2_successful_predictions <- function(pred, cut) {
  if (is.null(pred[["cutoff"]])) {
    cli::cli_abort("{.arg pred} needs a {.field cutoff} column to be matched with {.arg cut}.")
  }
  # WHY [[ ]]: `$` on a tibble without the column warns; a walk-forward stored without the
  # status columns is an expected input here.
  if (is.null(cut[["status_m"]]) || is.null(cut[["status_t"]])) {
    return(structure(pred, dropped = NA_integer_))
  }
  at <- match(format(pred[["cutoff"]]), format(cut[["cutoff"]]))
  ok <- v2_mode_ok(cut[["status_m"]][at]) & v2_mode_ok(cut[["status_t"]][at])
  structure(pred[ok, , drop = FALSE], dropped = sum(!ok))
}

#' Posterior summaries of R-INLA hyperparameters on the sd / correlation scale.
#'
#' R-INLA reports precisions; every other part of the comparison (Stan, the
#' priors, the report) speaks in standard deviations. The summaries are
#' computed from R-INLA's internal marginals (log precision, and
#' `log((1 + rho) / (1 - rho))` for the AR(1) memory) and mapped through the
#' monotone transform, which stays finite where the user-scale precision
#' marginal's spline can dip below zero.
#'
#' @param fit An `inla` fit.
#' @return Data frame with one row per hyperparameter: `parameter`, `mean`,
#'   `sd`, `q05`, `q50`, `q95`.
#' @noRd
v2_inla_hyper_sd <- function(fit) {
  rows <- lapply(names(fit$internal.marginals.hyperpar), function(nm) {
    m <- fit$internal.marginals.hyperpar[[nm]]
    if (startsWith(nm, "Log precision")) {
      f <- function(x) exp(-x / 2)
      label <- sub("^Log precision for ", "sd: ", nm)
    } else if (startsWith(nm, "Rho_intern")) {
      f <- function(x) tanh(x / 2)
      label <- sub("^Rho_intern for ", "rho: ", nm)
    } else {
      f <- identity
      label <- nm
    }
    q <- sort(f(INLA::inla.qmarginal(c(0.05, 0.5, 0.95), m)))
    m1 <- INLA::inla.emarginal(f, m)
    m2 <- INLA::inla.emarginal(function(x) f(x)^2, m)
    data.frame(
      parameter = label, mean = m1, sd = sqrt(max(m2 - m1^2, 0)),
      q05 = q[1], q50 = q[2], q95 = q[3]
    )
  })
  do.call(rbind, rows)
}
