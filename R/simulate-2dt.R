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

# ---- Inputs --------------------------------------------------------------------

# Per-draw parameters for the season simulation, on the models' raw scale.
#
# Team strengths are the LAST fitted round's (`cur_*_away` = offense /
# defense[N_rounds]); home advantage is the raw additive parameter, applied to
# both the home offence and defence by the simulator. The scoring level is
# `mean_goals[n_seasons]` -- prepare_data() indexes seasons in order
# (R/model-prepare.R, `as.integer(as.factor(season))`), so the last index is
# the latest season with results. `n_seasons` must come from the same prep
# the fit was trained on.
#
# `fit$draws()` is asked for whole variables (`mean_goals`, not
# `mean_goals[3]`): cmdstanr accepts either, the test stub only the former.
.extract_sim_inputs_2dt <- function(fit, teams, sport, n_seasons) {
  stopifnot(sport %in% c("basketball", "handball"))
  team_var <- function(var, col) {
    fit$draws(var) |>
      posterior::as_draws_df() |>
      tibble::as_tibble() |>
      tidyr::pivot_longer(
        c(-".chain", -".draw", -".iteration"),
        names_to = "name", values_to = col
      ) |>
      dplyr::mutate(
        team = teams$team[as.integer(readr::parse_number(.data$name))]
      ) |>
      # A fit paired with a shorter team list would otherwise inject NA teams
      # (see .extract_sim_inputs_pfi, issue #14).
      dplyr::filter(!is.na(.data$team)) |>
      dplyr::select("team", ".draw", dplyr::all_of(col))
  }
  team_vars <- c(
    cur_offense = "cur_offense_away",
    cur_defense = "cur_defense_away",
    home_advantage_off = "home_advantage_off",
    home_advantage_def = "home_advantage_def"
  )
  if (identical(sport, "handball")) {
    team_vars <- c(team_vars, sigma_team = "sigma_team")
  }
  team <- Reduce(
    function(a, b) dplyr::inner_join(a, b, by = c("team", ".draw")),
    Map(team_var, unname(team_vars), names(team_vars))
  )

  level_col <- sprintf("mean_goals[%d]", as.integer(n_seasons))
  sport_vars <- switch(sport,
    basketball = c("sigma", "alpha_rho", "beta_rho", "beta2_rho", "beta3_rho"),
    handball = c("rho", "mean_sigma_team", "scale_sigma_team")
  )
  shared_vars <- c("delta_mean_goals", "sigma_mean_goals", "nu")
  scalar <- fit$draws(c("mean_goals", shared_vars, sport_vars)) |>
    posterior::as_draws_df() |>
    tibble::as_tibble()
  if (!level_col %in% names(scalar)) {
    stop(
      ".extract_sim_inputs_2dt: the fit has no ", level_col,
      " -- pass the N_seasons of the prep the fit was trained on.",
      call. = FALSE
    )
  }
  scalar <- scalar |>
    dplyr::select(".draw",
      mean_goals_fit = dplyr::all_of(level_col),
      dplyr::all_of(c(shared_vars, sport_vars))
    ) |>
    dplyr::arrange(.data$.draw)

  list(team = team, scalar = scalar)
}

# The scoring level of the season being projected. A season the fit has no
# results for yet is stepped forward on the model's own trend (F12):
#   mean_goals[s + 1] = mean_goals[s] + delta_mean_goals + sigma_mean_goals * z
# `k` steps add k deltas and a sqrt(k)-scaled shock. `z_level` is drawn ONCE per
# draw by the caller, so every division of a cell projects the same
# league-wide level.
.season_level_2dt <- function(scalar, seasons_ahead) {
  k <- as.integer(seasons_ahead)
  stopifnot(length(k) == 1L, !is.na(k), k >= 0L, "z_level" %in% names(scalar))
  scalar$mean_goals <- scalar$mean_goals_fit +
    k * scalar$delta_mean_goals +
    sqrt(k) * scalar$sigma_mean_goals * scalar$z_level
  scalar
}
