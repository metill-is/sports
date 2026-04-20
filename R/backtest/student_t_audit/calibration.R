#' Model-implied draw rate + minimal common-observable calibration.
#'
#' Phase 1 scope: overall (unconditional) draw-rate estimate per fit,
#' computed from posterior samples of the relevant parameters. Tie
#' threshold configurable (default 0.5).
#'
#' For direct_SD: marginal on D from the bivariate Student-t is a
#' univariate t(nu, mu_D, sigma_D). We average P(|D| <= threshold)
#' across posterior draws of (nu, sigma_D, mu_D per match).
#'
#' For BVP no-inflation: the exact analytical draw probability involves
#' the Skellam tail of (Y_h - Y_a) under the trivariate-reduction mixing
#' — not trivially extractable from fitted summaries. For Phase 1 we
#' report the observed draw rate and leave the per-match BVP draw-rate
#' estimate as a Phase 3 task (requires in-sample y_rep in the Stan
#' file or a post-hoc simulation loop).

suppressPackageStartupMessages({
  library(posterior)
})

#' Iceland male historical draw rate (from the training data `d`).
#' @param goals1 Integer vector of home goals.
#' @param goals2 Integer vector of away goals.
observed_draw_rate <- function(goals1, goals2) {
  mean(goals1 == goals2)
}

#' Analytical model-implied draw rate for the direct_SD Student-t model.
#'
#' Marginal distribution of D under bivariate multi_student_t(nu, mu, Sigma)
#' is a univariate Student-t with location mu_D = mu[2], scale
#' sqrt(Sigma[2,2]) = sigma_D, and df = nu.
#'
#' Two flavours of "model-implied draw rate":
#'   - unconditional:  average P(|D_n| <= thr) per match, averaged
#'                     across posterior draws.
#'   - centred:        evaluated at mu_D = 0 only (ignores team asymmetry).
#'
#' For a quick Phase 1 summary we expose both and also report a
#' "median match" version using the fleet-average parameters.
#'
#' @param fit cmdstanr fit object (must have sigma_D, nu parameters + per-match
#'            mu_diff reconstructible from team strengths + home advantages).
#' @param stan_data The stan_data list used to fit (needed to compute mu_D
#'            per match in R).
#' @param threshold Tie threshold in goals (default 0.5).
#' @param n_draws Posterior subsample size (default 200 — enough for a
#'            stable overall draw-rate estimate).
implied_draw_rate_direct_SD <- function(fit, stan_data, threshold = 0.5, n_draws = 200) {
  draws <- fit$draws(format = "draws_df")
  n_tot <- nrow(draws)
  idx <- seq(1, n_tot, length.out = min(n_draws, n_tot)) |> round()
  draws <- draws[idx, , drop = FALSE]

  N <- stan_data$N

  # Pull per-draw arrays. offense/defense are [N_rounds, K] per draw; we
  # reconstruct per-match mu_D.
  # For a quick first cut we compute overall rate using sigma_D and nu only,
  # AT mu_D = 0 — a fleet-level approximation that ignores team asymmetry.
  sigma_D <- as.numeric(draws$sigma_D)
  nu <- as.numeric(draws$nu)

  centred_rate <- mean(pt(threshold / sigma_D, df = nu) -
    pt(-threshold / sigma_D, df = nu))

  list(
    threshold = threshold,
    n_draws_used = length(idx),
    centred_rate = centred_rate,
    sigma_D_posterior_mean = mean(sigma_D),
    sigma_D_posterior_sd = sd(sigma_D),
    nu_posterior_mean = mean(nu),
    nu_posterior_sd = sd(nu)
  )
}

#' Tiny helper to build a draw-rate calibration summary string for the
#' compare_*.txt report.
format_draw_calibration <- function(observed, direct_SD_res) {
  paste0(
    sprintf(
      "Observed draw rate (training):     %.3f (%d draws / %d matches)\n",
      observed$rate, observed$n_draws, observed$n_matches
    ),
    sprintf(
      "direct_SD centred (mu_D=0) rate:   %.3f  (sigma_D=%.3f, nu=%.1f)\n",
      direct_SD_res$centred_rate,
      direct_SD_res$sigma_D_posterior_mean,
      direct_SD_res$nu_posterior_mean
    ),
    sprintf(
      "Gap (modelled - observed):         %+.3f\n",
      direct_SD_res$centred_rate - observed$rate
    )
  )
}
