#' @include extract-football-iceland.R
NULL

# League season simulator
#
# Forward-simulates ALL remaining league fixtures from posterior draws, per
# draw, using the same frozen-strength bivariate-Poisson match model the Stan
# generated-quantities block uses for predictions (lines 297-307 of
# Stan/football_iceland/bivariate_poisson_no_inflation.stan) and the cup
# bracket simulator (`.simulate_cup_match_pfi`). Unlike the cup simulator,
# league matches keep their drawn scoreline (3/1/0 points + goal difference)
# rather than rejection-sampling a winner.
#
# This replaces the previous final-positions logic, which only integrated over
# the matches in the model's 14-day prediction window (~2 rounds) and therefore
# reported "position after the next ~2 rounds" mislabelled as the final table.
#
# Strength is held at the latest-round posterior (`cur_offense`/`cur_defense`)
# for every remaining match — identical to how the model predicts next_games,
# match scores, and the cup bracket. No random-walk drift is projected forward;
# that is a deliberate consistency choice (the model itself freezes strength at
# the training cutoff for all predictions).

#' Simulate a league's remaining season across posterior draws
#'
#' For each posterior draw of team-strength parameters, plays out every
#' remaining fixture with a bivariate-Poisson scoreline, adds the realised
#' (already-played) standings, ranks the final table by points -> goal
#' difference -> goals for, and aggregates to per-(team, placement) and
#' per-(team, points) probabilities.
#'
#' @param sim_inputs_team Tibble with columns `team`, `.draw`, `cur_offense`,
#'   `cur_defense`, `home_advantage_off`, `home_advantage_def` (raw log-scale,
#'   latest-round strengths). Produced by `.extract_sim_inputs_pfi()`.
#' @param sim_inputs_scalar Tibble with columns `.draw`, `mean_log_goals`,
#'   `alpha_mu3`, `beta_mu3_strength_diff`. Produced by
#'   `.extract_sim_inputs_pfi()`.
#' @param remaining_fixtures Tibble with columns `home_team`, `away_team` — the
#'   unplayed fixtures of the season for this division. May be empty (season
#'   over), in which case placements are the deterministic ranking of the
#'   realised table.
#' @param base_standings Tibble with one row per league team: `team`,
#'   `base_points`, `base_gd`, `base_gf` — the realised points / goal
#'   difference / goals for from already-played matches (0 for an unplayed
#'   season). Defines the team set whose final positions are computed.
#' @param seed Optional integer. When set, `set.seed()` is called once at the
#'   entry point so the simulation is reproducible.
#'
#' @return A list with:
#'   - `final_positions`: tibble(`team`, `placement`, `probability`) — one row
#'     per (team, placement in 1..n_teams), `probability` = share of draws.
#'   - `points_distribution`: tibble(`team`, `points`, `probability`) — one row
#'     per realised (team, total season points) with `probability` = share of
#'     draws.
#'
#' @export
simulate_league_season <- function(sim_inputs_team,
                                   sim_inputs_scalar,
                                   remaining_fixtures,
                                   base_standings,
                                   seed = NULL) {
  stopifnot(
    is.data.frame(sim_inputs_team),
    is.data.frame(sim_inputs_scalar),
    is.data.frame(remaining_fixtures),
    is.data.frame(base_standings),
    all(c(
      "team", ".draw", "cur_offense", "cur_defense",
      "home_advantage_off", "home_advantage_def"
    ) %in% names(sim_inputs_team)),
    all(c(".draw", "mean_log_goals", "alpha_mu3", "beta_mu3_strength_diff")
    %in% names(sim_inputs_scalar)),
    all(c("team", "base_points", "base_gd", "base_gf") %in% names(base_standings))
  )

  if (!is.null(seed)) {
    set.seed(seed)
  }

  teams <- as.character(base_standings$team)
  n_teams <- length(teams)
  if (n_teams == 0L) {
    return(list(
      final_positions = tibble::tibble(
        team = character(), placement = integer(), probability = numeric()
      ),
      points_distribution = tibble::tibble(
        team = character(), points = integer(), probability = numeric()
      )
    ))
  }

  # Align scalar draws into a fixed row order; every per-team matrix below is
  # built against this same draw ordering so a fixture's home/away vectors and
  # the scalars index the same posterior sample row-for-row.
  scalar <- sim_inputs_scalar[order(sim_inputs_scalar$.draw), , drop = FALSE]
  draw_order <- scalar$.draw
  nd <- length(draw_order)
  mlg <- scalar$mean_log_goals
  amu3 <- scalar$alpha_mu3
  bmu3 <- scalar$beta_mu3_strength_diff

  st <- sim_inputs_team[sim_inputs_team$team %in% teams, , drop = FALSE]
  missing_teams <- setdiff(teams, unique(st$team))
  if (length(missing_teams) > 0L) {
    stop(
      "simulate_league_season: no strength draws for league team(s): ",
      paste(missing_teams, collapse = ", "),
      ". Every league team must be in the fit registry.",
      call. = FALSE
    )
  }

  # Build a (draw x team) matrix for one strength column, rows aligned to
  # `draw_order`, columns to `teams`.
  to_matrix <- function(col) {
    w <- st |>
      dplyr::select(".draw", "team", value = dplyr::all_of(col)) |>
      tidyr::pivot_wider(names_from = "team", values_from = "value")
    w <- w[match(draw_order, w$.draw), , drop = FALSE]
    as.matrix(w[, teams, drop = FALSE])
  }
  OFF <- to_matrix("cur_offense")
  DEF <- to_matrix("cur_defense")
  HAO <- to_matrix("home_advantage_off")
  HAD <- to_matrix("home_advantage_def")

  pts <- matrix(0L, nd, n_teams, dimnames = list(NULL, teams))
  gd <- matrix(0L, nd, n_teams, dimnames = list(NULL, teams))
  gf <- matrix(0L, nd, n_teams, dimnames = list(NULL, teams))

  fx <- remaining_fixtures[
    remaining_fixtures$home_team %in% teams &
      remaining_fixtures$away_team %in% teams, ,
    drop = FALSE
  ]
  for (i in seq_len(nrow(fx))) {
    h <- as.character(fx$home_team[i])
    a <- as.character(fx$away_team[i])

    # Frozen-strength bivariate-Poisson rates (log scale), identical to the
    # Stan GQ prediction loop and `.simulate_cup_match_pfi`. Home team gets the
    # offensive + defensive home advantage.
    off_h <- OFF[, h] + HAO[, h]
    def_h <- DEF[, h] + HAD[, h]
    off_a <- OFF[, a]
    def_a <- DEF[, a]
    mu_h <- mlg + off_h - def_a
    mu_a <- mlg + off_a - def_h
    strength_diff <- abs(off_h + def_h - off_a - def_a)
    mu3 <- stats::plogis(amu3 + bmu3 * strength_diff, log.p = TRUE) +
      0.5 * (mu_h + mu_a)

    # Trivariate reduction: shared component x3 drawn once per (fixture, draw).
    x3 <- stats::rpois(nd, exp(mu3))
    g_h <- stats::rpois(nd, exp(mu_h)) + x3
    g_a <- stats::rpois(nd, exp(mu_a)) + x3

    home_pts <- ifelse(g_h > g_a, 3L, ifelse(g_h == g_a, 1L, 0L))
    away_pts <- ifelse(g_a > g_h, 3L, ifelse(g_h == g_a, 1L, 0L))

    pts[, h] <- pts[, h] + home_pts
    pts[, a] <- pts[, a] + away_pts
    gf[, h] <- gf[, h] + g_h
    gf[, a] <- gf[, a] + g_a
    gd[, h] <- gd[, h] + (g_h - g_a)
    gd[, a] <- gd[, a] + (g_a - g_h)
  }

  # Add the realised (already-played) table.
  bs <- base_standings[match(teams, base_standings$team), , drop = FALSE]
  pts <- sweep(pts, 2L, as.integer(bs$base_points), `+`)
  gd <- sweep(gd, 2L, as.integer(bs$base_gd), `+`)
  gf <- sweep(gf, 2L, as.integer(bs$base_gf), `+`)

  # Rank each draw's table by points -> goal difference -> goals for (desc).
  # Packed key is order-preserving for football magnitudes (|gd| << 1000,
  # gf << 1000); ties broken deterministically by team order via "first".
  key <- pts * 1e6 + gd * 1e3 + gf
  placement <- t(apply(key, 1L, function(r) rank(-r, ties.method = "first")))
  if (nd == 1L) {
    placement <- matrix(placement, nrow = 1L)
  }
  colnames(placement) <- teams

  final_positions <- lapply(seq_len(n_teams), function(j) {
    counts <- tabulate(placement[, j], nbins = n_teams)
    tibble::tibble(
      team = teams[j],
      placement = seq_len(n_teams),
      probability = counts / nd
    )
  }) |>
    dplyr::bind_rows() |>
    dplyr::arrange(.data$team, .data$placement)

  points_distribution <- lapply(seq_len(n_teams), function(j) {
    tbl <- table(pts[, j])
    tibble::tibble(
      team = teams[j],
      points = as.integer(names(tbl)),
      probability = as.numeric(tbl) / nd
    )
  }) |>
    dplyr::bind_rows() |>
    dplyr::arrange(.data$team, .data$points)

  list(
    final_positions = final_positions,
    points_distribution = points_distribution
  )
}
