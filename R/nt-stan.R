#' @include nt-model.R
NULL

#' Stan data for the reference national-team model.
#'
#' Builds the data list for `Stan/football_national/nt_v2.stan` from the same
#' prepared inputs [nt_fit_inla()] uses, so the two engines fit one model.
#'
#' @param prep Output of [nt_prepare()].
#' @param home_pred,away_pred Optional fixtures to predict at the forecast
#'   origin.
#' @param venue_pred 1 when the home side of a predicted fixture plays in its
#'   own country, else 0 (recycled).
#' @param damp_friendly Estimate the friendly multiplier on the strength gap.
#' @return Named list.
#' @export
nt_stan_data <- function(prep, home_pred = character(), away_pred = character(),
                         venue_pred = 0, damp_friendly = FALSE) {
  m <- prep$matches
  unknown <- setdiff(c(home_pred, away_pred), prep$teams)
  if (length(unknown) > 0L) {
    cli::cli_abort("Team{?s} not in the training data: {.val {unknown}}.")
  }
  list(
    K = length(prep$teams),
    T = prep$n_periods,
    N = nrow(m),
    home = m$home_nr,
    away = m$away_nr,
    period = m$period,
    venue = as.numeric(m$venue),
    friendly = as.numeric(m$friendly),
    home_goals = m$home_score,
    away_goals = m$away_score,
    damp_friendly = as.integer(damp_friendly),
    N_pred = length(home_pred),
    home_pred = as.array(match(home_pred, prep$teams)),
    away_pred = as.array(match(away_pred, prep$teams)),
    venue_pred = as.array(rep_len(as.numeric(venue_pred), length(home_pred))),
    u_sigma_alpha = 2,
    u_sigma_u = 0.1,
    u_sigma_delta = 0.3
  )
}

#' Convert a Stan reference fit to the draws layout of the INLA fit.
#'
#' @param stan_fit A `CmdStanMCMC` of `Stan/football_national/nt_v2.stan`.
#' @param prep The [nt_prepare()] output the fit used.
#' @return List with `draws`, `teams` and `kappa` (posterior mean of the
#'   friendly multiplier, 1 when it was not estimated), usable by
#'   [nt_match_rates()] and the simulators.
#' @export
nt_stan_draws <- function(stan_fit, prep) {
  as_mat <- function(var) {
    out <- posterior::as_draws_matrix(stan_fit$draws(var))
    unclass(out)
  }
  theta <- as_mat("theta_now")
  delta <- as_mat("delta")
  dimnames(theta) <- dimnames(delta) <- list(NULL, prep$teams)
  kappa <- tryCatch(mean(as_mat("kappa_friendly")), error = function(e) 1)
  list(
    draws = list(
      theta = theta, delta = delta,
      mu = as.vector(as_mat("mu")),
      home_for = as.vector(as_mat("home_for")),
      home_against = as.vector(as_mat("home_against"))
    ),
    teams = prep$teams,
    kappa = kappa
  )
}
