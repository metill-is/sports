#' @include nt-data.R
NULL

# National-team model, second generation.
#
# Goals are independent Poisson. For the side scoring (s) against the side
# conceding (c) in time step t:
#
#   log rate = mu + venue terms + w * (theta[s, t] - theta[c, t]) + delta[s] + delta[c]
#   theta[i, t] = alpha[i] + u[i, t]
#
# theta is one overall strength per team (static level alpha plus a calendar
# random walk or AR(1) u on a quarterly grid), delta is a small pooled static
# attack/defence split (attack = theta + delta, defence = theta - delta), and
# the two venue terms are pooled, sign-free fixed effects that apply only when
# the listed home side plays in its own country. `w` is 1, or `kappa` for a
# friendly when the strength gap is damped in friendlies.

.nt_design <- function(prep, kappa = 1) {
  m <- prep$matches
  n <- nrow(m)
  k <- length(prep$teams)
  n_t <- prep$n_periods
  rows <- seq_len(2L * n)
  scorer <- c(m$home_nr, m$away_nr)
  conceder <- c(m$away_nr, m$home_nr)
  period <- rep(m$period, 2L)
  friendly <- rep(m$friendly, 2L)
  home_row <- rep(c(TRUE, FALSE), each = n)
  venue <- rep(m$venue, 2L)
  w <- ifelse(friendly, kappa, 1)
  list(
    y = c(m$home_score, m$away_score),
    a_alpha = Matrix::sparseMatrix(
      i = c(rows, rows), j = c(scorer, conceder), x = c(w, -w),
      dims = c(2L * n, k)
    ),
    a_u = Matrix::sparseMatrix(
      i = c(rows, rows),
      j = c((scorer - 1L) * n_t + period, (conceder - 1L) * n_t + period),
      x = c(w, -w), dims = c(2L * n, k * n_t)
    ),
    a_delta = Matrix::sparseMatrix(
      i = c(rows, rows), j = c(scorer, conceder), x = 1,
      dims = c(2L * n, k)
    ),
    home_for = as.numeric(venue == 1L & home_row),
    home_against = as.numeric(venue == 1L & !home_row),
    friendly = friendly,
    k = k, n_t = n_t
  )
}

#' Fit the national-team model with INLA.
#'
#' @param prep Output of [nt_prepare()].
#' @param dynamics `"rw1"` (calendar random walk) or `"ar1"` (mean-reverting).
#' @param friendly How friendlies differ: `"none"`, `"damped"` (the strength
#'   gap is multiplied by `kappa` in friendlies; `kappa` is fixed per fit, see
#'   [nt_profile_kappa()]) or `"dispersion"` (an extra Gaussian term on the log
#'   rate of friendlies).
#' @param kappa Friendly multiplier on the strength gap, used when
#'   `friendly = "damped"`.
#' @param rw_sd Optional fixed innovation sd per square-root month for
#'   `dynamics = "rw1"` (see [nt_rw_sd_for_half_life()]); `NULL` estimates it.
#' @param n_draws Posterior draws of the end-of-window state to return.
#' @param int_strategy INLA integration strategy over hyperparameters (`"eb"`
#'   conditions on their mode; `"auto"` integrates).
#' @param threads INLA `num.threads`.
#' @param seed Seed for the posterior draws.
#' @param theta_init Optional starting values for INLA's internal
#'   hyperparameters (the `theta_mode` of an earlier fit of the same model), to
#'   speed up walk-forward refits.
#' @param keep_result Keep the raw INLA result as `result` (large).
#' @return List with `draws` (`theta`, `delta`: draws x teams matrices; `mu`,
#'   `home_for`, `home_against`: vectors), `teams`, `kappa`, `hyper`, `fixed`,
#'   `mlik`, `theta_mode`, `seconds` and `settings`.
#' @export
nt_fit_inla <- function(prep,
                        dynamics = c("rw1", "ar1"),
                        friendly = c("none", "damped", "dispersion"),
                        kappa = 1,
                        rw_sd = NULL,
                        n_draws = 1000L,
                        int_strategy = "eb",
                        threads = "4:1",
                        seed = 1L,
                        theta_init = NULL,
                        keep_result = FALSE) {
  rlang::check_installed("INLA", reason = "to fit the national-team model")
  dynamics <- match.arg(dynamics)
  friendly <- match.arg(friendly)
  if (friendly != "damped") kappa <- 1
  des <- .nt_design(prep, kappa = kappa)
  k <- des$k
  n_t <- des$n_t

  noise <- if (friendly == "dispersion") {
    ifelse(des$friendly, seq_along(des$y), NA_integer_)
  } else {
    rep(NA_integer_, length(des$y))
  }
  stk <- INLA::inla.stack(
    data = list(y = des$y),
    A = list(des$a_alpha, des$a_u, des$a_delta, 1),
    effects = list(
      list(alpha = seq_len(k)),
      list(u = rep(seq_len(n_t), k), u_rep = rep(seq_len(k), each = n_t)),
      list(delta = seq_len(k)),
      data.frame(
        mu = 1, home_for = des$home_for, home_against = des$home_against,
        noise = noise
      )
    ),
    tag = "obs", compress = FALSE, remove.unused = FALSE
  )

  # WHY: INLA reads `hyper` from the unevaluated formula. A list object spliced
  # into the call is ignored without a warning and the default prior is used,
  # so every hyper spec below is built as a call.
  pc <- function(u) {
    bquote(list(prec = list(prior = "pc.prec", param = c(.(u), 0.01))))
  }
  u_hyper <- if (dynamics == "ar1") {
    quote(list(
      prec = list(prior = "pc.prec", param = c(1, 0.01)),
      rho = list(prior = "pc.cor1", param = c(0.9, 0.9))
    ))
  } else if (!is.null(rw_sd)) {
    step_sd <- rw_sd * sqrt(prep$step_months)
    bquote(list(prec = list(initial = .(log(1 / step_sd^2)), fixed = TRUE)))
  } else {
    # WHY: the bound is per time step, so a monthly grid (step_months = 1)
    # loosens it to about 0.17 per quarter; only the quarterly grid was tuned.
    pc(0.1)
  }
  u_term <- if (dynamics == "ar1") {
    bquote(f(u,
      model = "ar1", values = seq_len(.(n_t)), replicate = u_rep,
      nrep = .(k), hyper = .(u_hyper)
    ))
  } else {
    bquote(f(u,
      model = "rw1", values = seq_len(.(n_t)), replicate = u_rep,
      nrep = .(k), constr = TRUE, scale.model = FALSE, hyper = .(u_hyper)
    ))
  }
  rhs <- list(
    quote(-1), quote(mu), quote(home_for), quote(home_against),
    bquote(f(alpha, model = "iid", values = seq_len(.(k)), hyper = .(pc(2)))),
    u_term,
    bquote(f(delta, model = "iid", values = seq_len(.(k)), hyper = .(pc(0.3))))
  )
  if (friendly == "dispersion") {
    rhs <- c(rhs, list(bquote(f(noise, model = "iid", hyper = .(pc(1))))))
  }
  form <- stats::as.formula(
    call("~", quote(y), Reduce(function(a, b) call("+", a, b), rhs)),
    env = environment()
  )

  t0 <- Sys.time()
  res <- INLA::inla(
    form,
    family = "poisson",
    data = INLA::inla.stack.data(stk),
    control.predictor = list(A = INLA::inla.stack.A(stk), compute = FALSE, link = 1),
    control.compute = list(config = TRUE, dic = FALSE, waic = FALSE, mlik = TRUE),
    control.fixed = list(prec = 0.25),
    control.inla = list(int.strategy = int_strategy),
    control.mode = if (is.null(theta_init)) {
      list()
    } else {
      list(theta = theta_init, restart = TRUE)
    },
    num.threads = threads
  )
  t_fit <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  t1 <- Sys.time()
  smp <- INLA::inla.posterior.sample(
    n_draws, res,
    selection = list(
      alpha = seq_len(k), delta = seq_len(k),
      u = (seq_len(k) - 1L) * n_t + n_t,
      mu = 1, home_for = 1, home_against = 1
    ),
    seed = seed, num.threads = "1:1"
  )
  latent <- vapply(smp, function(s) s$latent[, 1], numeric(nrow(smp[[1]]$latent)))
  part <- sub(":.*$", "", rownames(smp[[1]]$latent))
  pick <- function(name) t(latent[part == name, , drop = FALSE])
  t_draw <- as.numeric(difftime(Sys.time(), t1, units = "secs"))

  theta <- pick("alpha") + pick("u")
  delta <- pick("delta")
  colnames(theta) <- colnames(delta) <- prep$teams

  list(
    draws = list(
      theta = theta, delta = delta,
      mu = as.vector(pick("mu")),
      home_for = as.vector(pick("home_for")),
      home_against = as.vector(pick("home_against"))
    ),
    teams = prep$teams,
    kappa = kappa,
    hyper = .nt_hyper_sd(res, prep$step_months),
    fixed = res$summary.fixed[, c("mean", "sd", "0.025quant", "0.975quant")],
    mlik = as.numeric(res$mlik[1, 1]),
    theta_mode = res$mode$theta,
    result = if (keep_result) res else NULL,
    seconds = c(fit = t_fit, draws = t_draw),
    settings = list(
      dynamics = dynamics, friendly = friendly, rw_sd = rw_sd,
      int_strategy = int_strategy, cutoff = prep$cutoff,
      n_matches = nrow(prep$matches), n_teams = k, n_periods = n_t,
      step_months = prep$step_months
    )
  )
}

# Hyperparameters on the sd scale (posterior mode of each precision), plus the
# AR(1) coefficient when present. The random-walk innovation sd is also given
# per square-root month so fits on different time grids compare.
.nt_hyper_sd <- function(res, step_months = 1L) {
  h <- res$summary.hyperpar
  if (is.null(h) || nrow(h) == 0L) {
    return(NULL)
  }
  is_prec <- grepl("^Precision", rownames(h))
  out <- data.frame(
    parameter = sub("^Precision for ", "sd ", rownames(h)),
    mode = ifelse(is_prec, 1 / sqrt(h$mode), h$mode),
    lower = ifelse(is_prec, 1 / sqrt(h[["0.975quant"]]), h[["0.025quant"]]),
    upper = ifelse(is_prec, 1 / sqrt(h[["0.025quant"]]), h[["0.975quant"]])
  )
  rownames(out) <- NULL
  rw <- out$parameter == "sd u" & !any(grepl("^Rho", out$parameter))
  if (any(rw)) {
    per_month <- out[rw, , drop = FALSE]
    per_month$parameter <- "sd u per sqrt(month)"
    per_month[c("mode", "lower", "upper")] <-
      per_month[c("mode", "lower", "upper")] / sqrt(step_months)
    out <- rbind(out, per_month)
  }
  out
}

#' Profile the friendly damping multiplier.
#'
#' The multiplier enters the linear predictor multiplicatively, which INLA
#' cannot estimate directly, so the model is fitted once per grid value and the
#' values are compared on the log marginal likelihood.
#'
#' @param prep Output of [nt_prepare()].
#' @param grid Candidate multipliers.
#' @param ... Passed to [nt_fit_inla()].
#' @return Data frame `kappa, mlik`, with attribute `best`.
#' @export
nt_profile_kappa <- function(prep, grid = c(0.6, 0.7, 0.8, 0.9, 1), ...) {
  mlik <- vapply(grid, function(kp) {
    nt_fit_inla(prep, friendly = "damped", kappa = kp, n_draws = 1L, ...)$mlik
  }, numeric(1))
  out <- data.frame(kappa = grid, mlik = mlik)
  attr(out, "best") <- grid[which.max(mlik)]
  out
}

#' Memory of a random-walk strength, as a half-life in years.
#'
#' For a local-level model the steady-state filter weights past matches
#' geometrically, with rate `sqrt(q * info)` per year, where `q` is the
#' innovation variance per year and `info` the Fisher information about the
#' strength arriving per year (matches per year times goals per match, because
#' the strength enters both log rates with unit slope). This maps the model's
#' innovation sd onto the time-decay half-life the literature reports.
#'
#' @param rw_sd Innovation sd per square-root month.
#' @param matches_per_year Matches a team plays per year.
#' @param goals_per_match Total goals per match.
#' @return Half-life in years.
#' @export
nt_implied_half_life <- function(rw_sd, matches_per_year = 11,
                                 goals_per_match = 2.7) {
  log(2) / sqrt(12 * rw_sd^2 * matches_per_year * goals_per_match)
}

#' Innovation sd per month that gives a target half-life.
#'
#' Inverse of [nt_implied_half_life()].
#'
#' @param half_life_years Target half-life in years.
#' @inheritParams nt_implied_half_life
#' @return Innovation sd per square-root month.
#' @export
nt_rw_sd_for_half_life <- function(half_life_years, matches_per_year = 11,
                                   goals_per_match = 2.7) {
  log(2) / (half_life_years * sqrt(12 * matches_per_year * goals_per_match))
}

# Venue flags as numbers 0 (neutral) and 1 (home side at home).
.nt_venue_values <- function(venue) {
  # WHY: as.numeric() on a factor returns its level codes (1, 2), not 0 and 1.
  venue <- if (is.factor(venue)) {
    suppressWarnings(as.numeric(as.character(venue)))
  } else {
    suppressWarnings(as.numeric(venue))
  }
  if (anyNA(venue) || !all(venue %in% c(0, 1))) {
    cli::cli_abort("{.arg venue} must be 0 (neutral) or 1 (home side at home).")
  }
  venue
}

# Scoring rates of simulated matches: `draw` (one posterior draw per row) and
# `home`, `away` (matrices or vectors of column indices into the fit's teams,
# one row per draw entry) give each match's teams; `venue` is recycled along
# the columns. Same arithmetic, in the same order, as nt_match_rates().
.nt_sim_rates <- function(fit, draw, home, away, venue) {
  d <- fit$draws
  home <- as.matrix(home)
  away <- as.matrix(away)
  n <- nrow(home)
  m <- ncol(home)
  rows <- rep(draw, m)
  get <- function(x, idx) matrix(x[cbind(rows, c(idx))], n, m)
  gap <- get(d$theta, home) - get(d$theta, away)
  base <- d$mu[draw] + (get(d$delta, home) + get(d$delta, away))
  venue <- matrix(rep(venue, each = n), n, m)
  list(
    home = exp(base + gap + d$home_for[draw] * venue),
    away = exp(base - gap + d$home_against[draw] * venue)
  )
}

#' Per-draw scoring rates for a set of fixtures.
#'
#' @param fit A fit from [nt_fit_inla()] (or any list with the same `draws`,
#'   `teams` and `kappa` elements).
#' @param home,away Team names.
#' @param venue 1 when `home` plays in its own country, 0 at a neutral venue
#'   (recycled).
#' @param friendly Whether each fixture is a friendly (recycled); only matters
#'   when the fit damps friendlies.
#' @return List `home`, `away`: draws x fixtures matrices of Poisson rates.
#' @export
nt_match_rates <- function(fit, home, away, venue = 0L, friendly = FALSE) {
  # WHY: factor team columns would index the draws by their integer codes.
  home <- as.character(home)
  away <- as.character(away)
  unknown <- setdiff(c(home, away), fit$teams)
  if (length(unknown) > 0L) {
    cli::cli_abort("Team{?s} not in the fit: {.val {unknown}}.")
  }
  d <- fit$draws
  n_fix <- length(home)
  venue <- rep_len(.nt_venue_values(venue), n_fix)
  w <- ifelse(rep_len(friendly, n_fix), fit$kappa, 1)
  gap <- d$theta[, home, drop = FALSE] - d$theta[, away, drop = FALSE]
  open <- d$delta[, home, drop = FALSE] + d$delta[, away, drop = FALSE]
  base <- d$mu + open
  gap <- sweep(gap, 2L, w, `*`)
  list(
    home = exp(base + gap + outer(d$home_for, venue)),
    away = exp(base - gap + outer(d$home_against, venue))
  )
}

#' Posterior-predictive scoreline probabilities.
#'
#' Averages the product of the two Poisson pmfs over the posterior draws, so
#' the result is the exact mixture, not a Monte Carlo tally of simulated scores.
#'
#' @param rates Output of [nt_match_rates()].
#' @param max_goals Largest goal count kept per side.
#' @return Array fixtures x (max_goals + 1) x (max_goals + 1); `[m, h + 1,
#'   a + 1]` is P(home scores h, away scores a).
#' @export
nt_scoreline_probs <- function(rates, max_goals = 12L) {
  n_fix <- ncol(rates$home)
  n_draw <- nrow(rates$home)
  g <- 0:max_goals
  out <- array(0, dim = c(n_fix, max_goals + 1L, max_goals + 1L))
  for (m in seq_len(n_fix)) {
    ph <- outer(rates$home[, m], g, function(l, x) stats::dpois(x, l))
    pa <- outer(rates$away[, m], g, function(l, x) stats::dpois(x, l))
    out[m, , ] <- crossprod(ph, pa) / n_draw
  }
  out
}

#' Outcome probabilities and expected goals from scoreline probabilities.
#'
#' @param grid Output of [nt_scoreline_probs()] (or any array of the same
#'   shape).
#' @return Tibble `p_home, p_draw, p_away, eg_home, eg_away`; the three
#'   probabilities are renormalised over the truncated grid.
#' @export
nt_outcome_probs <- function(grid) {
  g <- seq_len(dim(grid)[2]) - 1L
  lower <- outer(g, g, ">")
  res <- t(apply(grid, 1L, function(p) {
    c(
      sum(p[lower]), sum(diag(p)), sum(p[t(lower)]),
      sum(rowSums(p) * g), sum(colSums(p) * g), sum(p)
    )
  }))
  tibble::tibble(
    p_home = res[, 1] / res[, 6], p_draw = res[, 2] / res[, 6],
    p_away = res[, 3] / res[, 6],
    eg_home = res[, 4] / res[, 6], eg_away = res[, 5] / res[, 6]
  )
}
