#' @include simulate-league-season.R publish-iceland-2dt-helpers.R
NULL

# Season simulation for the 2DT sports (basketball, handball).
#
# The match generators below replay each Stan model's generated quantities
# from per-draw parameters, so simulate_league_season() can play out every
# remaining fixture of a season rather than the ~2 rounds inside Stan's 14-day
# prediction window (spec 2026-09-16 §1-§4). Strength is frozen at the last
# fitted round, exactly as the models themselves predict (D3).
#
# Scale: the 2DT models are additive in RAW points/goals. Home advantage is
# never exponentiated or halved here (B5, F11).

# One fixture's scores across draws: what multi_student_t_rng(nu, mu, Sigma)
# does, with Sigma = [[s_h^2, rho s_h s_a], [rho s_h s_a, s_a^2]] as a SCALE
# matrix (Stan/handball_iceland/2d_student_t.stan GQ, and the basketball model
# with s_h = s_a = sigma).
#
# ONE mixing variable per draw, shared by both scores. Two independent
# chi-square draws would give two univariate t's whose joint tails are wrong:
# a blowout in one score would no longer tend to come with an extreme in the
# other.
.draw_bivariate_t_2dt <- function(mu_h, mu_a, s_h, s_a, rho, nu) {
  n <- length(mu_h)
  z1 <- stats::rnorm(n)
  z2 <- stats::rnorm(n)
  w <- sqrt(nu / stats::rchisq(n, df = nu))
  list(
    home = mu_h + s_h * z1 * w,
    away = mu_a + s_a * (rho * z1 + sqrt(1 - rho^2) * z2) * w
  )
}

# Basketball (2d_student_t_scalarsigma.stan GQ): one scalar sigma, and a
# per-fixture correlation from the strength gap `d` and total `t`, both
# computed on home-advantage-adjusted strengths.
.match_fn_basketball <- .new_match_fn(
  function(home, away, scalars) {
    d <- abs(home$off + home$def - away$off - away$def)
    t <- abs(home$off + home$def + away$off + away$def)
    rho <- 2 * stats::plogis(
      scalars$alpha_rho + scalars$beta_rho * d +
        scalars$beta2_rho * t + scalars$beta3_rho * t * d
    ) - 1
    .draw_bivariate_t_2dt(
      mu_h = scalars$mean_goals + home$off - away$def,
      mu_a = scalars$mean_goals + away$off - home$def,
      s_h = scalars$sigma, s_a = scalars$sigma,
      rho = rho, nu = scalars$nu
    )
  },
  scalar_cols = c(
    "mean_goals", "nu", "sigma",
    "alpha_rho", "beta_rho", "beta2_rho", "beta3_rho"
  )
)

# Handball (2d_student_t.stan GQ): per-team sigma_team, one scalar rho.
.match_fn_handball <- .new_match_fn(
  function(home, away, scalars) {
    .draw_bivariate_t_2dt(
      mu_h = scalars$mean_goals + home$off - away$def,
      mu_a = scalars$mean_goals + away$off - home$def,
      s_h = home$sigma_team, s_a = away$sigma_team,
      rho = scalars$rho, nu = scalars$nu
    )
  },
  scalar_cols = c("mean_goals", "nu", "rho"),
  team_cols = "sigma_team"
)

.match_fn_2dt <- function(sport) {
  switch(sport,
    basketball = .match_fn_basketball,
    handball = .match_fn_handball,
    stop("No 2DT match model for sport '", sport, "'.", call. = FALSE)
  )
}

# The published points scheme (`.points_2dt`: 2 / 1 / 0, a tie only within
# `tie_threshold`) in the shape simulate_league_season() takes.
.points_fn_2dt <- function(has_ties, tie_threshold) {
  force(has_ties)
  force(tie_threshold)
  function(g_h, g_a) {
    list(
      home = .points_2dt(g_h, g_a, "home",
        has_ties = has_ties, tie_threshold = tie_threshold
      ),
      away = .points_2dt(g_h, g_a, "away",
        has_ties = has_ties, tie_threshold = tie_threshold
      )
    )
  }
}
