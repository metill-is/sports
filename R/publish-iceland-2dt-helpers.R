#' @include model-prepare.R storage.R config.R
NULL

# Shared helpers for publishing 2D Student-t models (basketball + handball
# Iceland). Both sports share the same Stan parameter surface
# (cur_offense_*/cur_defense_*/cur_strength_* per team x location, plus
# home_advantage_off/def, goals1_pred/goals2_pred). Differences are confined
# to top-division code, league display name, points scheme, and league size,
# and are passed in as arguments.
#
# Football's bivariate-Poisson publisher uses parallel `_pfi` helpers because
# its goals process is discrete and needs xG aggregation. The 2D Student-t
# models produce continuous score predictions and never compute xG.

# Per-team posterior draws for a single Stan vector indexed by team.
.extract_team_draws_2dt <- function(fit, var, teams, component, location) {
  fit$draws(var) |>
    posterior::as_draws_df() |>
    tibble::as_tibble() |>
    tidyr::pivot_longer(
      c(-".chain", -".draw", -".iteration"),
      names_to = "name", values_to = "value"
    ) |>
    dplyr::mutate(
      team_idx  = as.integer(readr::parse_number(.data$name)),
      team      = teams$team[.data$team_idx],
      component = component,
      location  = location
    )
}

# Summarise per-team draws into median + (lower, upper) at multiple coverage
# levels. Output: long tibble with one row per (team, component, location,
# coverage).
.summarise_team_intervals_2dt <- function(draws,
                                          coverages = c(0.5, 0.8, 0.95)) {
  draws |>
    dplyr::reframe(
      median = stats::median(.data$value),
      coverage = coverages,
      lower = stats::quantile(.data$value, 0.5 - coverages / 2),
      upper = stats::quantile(.data$value, 0.5 + coverages / 2),
      .by = c("team", "component", "location")
    )
}

# Pivot goals1_pred/goals2_pred draws + join pred_d (continuous Student-t
# scores). Returns NULL with a warning if the fit's N_pred disagrees with
# the prepared pred_d (caller writes empty placeholder JSONs).
.compute_posterior_goals_2dt <- function(fit, pred_d) {
  raw <- fit$draws(c("goals1_pred", "goals2_pred")) |>
    posterior::as_draws_df() |>
    tibble::as_tibble() |>
    tidyr::pivot_longer(
      -c(".chain", ".iteration", ".draw"),
      names_to  = "parameter",
      values_to = "value"
    ) |>
    dplyr::mutate(
      type = dplyr::if_else(
        stringr::str_detect(.data$parameter, "^goals1_pred"),
        "home_score", "away_score"
      ),
      game_nr = as.integer(
        stringr::str_match(.data$parameter, "\\[(\\d+)\\]$")[, 2]
      )
    ) |>
    dplyr::select(".draw", "type", "game_nr", "value") |>
    tidyr::pivot_wider(names_from = "type", values_from = "value")

  if (nrow(raw) == 0L || nrow(pred_d) == 0L) {
    return(tibble::tibble(
      .draw = integer(), game_nr = integer(),
      home_score = numeric(), away_score = numeric(),
      match_date = as.Date(character()),
      home_team = character(), away_team = character(),
      division = character()
    ))
  }

  n_pred_fit <- max(raw$game_nr, na.rm = TRUE)
  n_pred_data <- nrow(pred_d)
  if (n_pred_fit != n_pred_data) {
    warning(sprintf(
      paste0(
        "publish 2dt: fit was trained with N_pred=%d but prepare_data ",
        "returned %d. Posterior-dependent JSONs will be empty."
      ),
      n_pred_fit, n_pred_data
    ))
    return(tibble::tibble(
      .draw = integer(), game_nr = integer(),
      home_score = numeric(), away_score = numeric(),
      match_date = as.Date(character()),
      home_team = character(), away_team = character(),
      division = character()
    ))
  }

  pred_cols <- c("game_nr", "match_date", "home_team", "away_team", "division")
  raw |>
    dplyr::inner_join(pred_d[, pred_cols], by = "game_nr")
}

# Pad form vector to length n with leading NA so jsonlite never collapses
# a single-element list-column entry from ["W"] to "W".
.pad_form_2dt <- function(x, n = 5L) {
  utils::tail(c(rep(NA_character_, n), x), n)
}

# Map (home_score, away_score) to per-side points using a points scheme:
#   has_ties = TRUE  -> 2 / 1 / 0 (W / T / L)
#   has_ties = FALSE -> 2 / 0     (W / L; ties classified as away wins)
# `tie_threshold` lets handball's continuous Student-t draws still produce
# the rare exact-tie outcome from played-game integer scores.
.points_2dt <- function(home_score, away_score, name,
                        has_ties = FALSE, tie_threshold = 0) {
  if (has_ties) {
    diff <- home_score - away_score
    is_tie <- abs(diff) <= tie_threshold
    result <- ifelse(is_tie, "tie",
      ifelse(diff > 0, "home", "away")
    )
    dplyr::case_when(
      result == "tie" ~ 1L,
      result == name ~ 2L,
      TRUE ~ 0L
    )
  } else {
    result <- ifelse(home_score > away_score, "home", "away")
    dplyr::case_when(
      result == name ~ 2L,
      TRUE ~ 0L
    )
  }
}

# Per-draw per-team end-of-season points combining base (played) points
# with future-fixture predictions. Returns a long tibble with `.draw`,
# `team`, `points`.
#
# The team set is `base_points$team` -- every team that has PLAYED in the
# division this season -- not merely the teams appearing in `posterior_goals`.
# posterior_goals only covers the model's 14-day prediction window, so keying
# the table off it dropped any team without a fixture in that window: a 6-team
# division published a 4-team `final_positions` whose probabilities summed to
# one over the wrong support. Those teams contribute their realised points and
# no simulated ones, which is exactly right under this path's semantics.
.compute_iter_team_points_2dt <- function(posterior_goals, top_div, base_points,
                                          has_ties = FALSE,
                                          tie_threshold = 0) {
  empty <- tibble::tibble(
    .draw = integer(), team = character(), points = numeric(),
    base_points = integer(), point_diff = numeric()
  )
  if (nrow(posterior_goals) == 0L) {
    return(empty)
  }

  # `base_diff` is a tiebreaker added after this function's callers existed, so
  # a hand-built base_points (tests, and any caller predating it) may not carry
  # it. Absent means "no realised difference to carry", i.e. 0 -- never an
  # error, and never silently dropping the column downstream.
  if (!"base_diff" %in% names(base_points)) {
    base_points$base_diff <- 0
  }

  pg_top <- posterior_goals |>
    dplyr::filter(.data$division == top_div)

  # `.draw` comes from the whole posterior, not this division's slice, so a
  # division whose every fixture has already been played still gets its
  # realised table across the full draw set.
  all_draws <- sort(unique(posterior_goals$.draw))

  played_only <- function(teams) {
    if (length(teams) == 0L || length(all_draws) == 0L) {
      return(empty)
    }
    tidyr::expand_grid(.draw = all_draws, team = teams) |>
      dplyr::left_join(base_points, by = "team") |>
      dplyr::mutate(
        base_points = dplyr::coalesce(.data$base_points, 0L),
        points = as.numeric(.data$base_points),
        point_diff = as.numeric(dplyr::coalesce(.data$base_diff, 0))
      )
  }

  if (nrow(pg_top) == 0L) {
    return(played_only(base_points$team))
  }

  simulated <- pg_top |>
    tidyr::pivot_longer(c("home_team", "away_team"), values_to = "team") |>
    dplyr::mutate(
      name = dplyr::if_else(.data$name == "home_team", "home", "away"),
      points = .points_2dt(
        .data$home_score, .data$away_score, .data$name,
        has_ties = has_ties, tie_threshold = tie_threshold
      ),
      sim_diff = dplyr::if_else(
        .data$name == "home",
        .data$home_score - .data$away_score,
        .data$away_score - .data$home_score
      )
    ) |>
    dplyr::summarise(
      points = sum(.data$points),
      sim_diff = sum(.data$sim_diff),
      .by = c(".draw", "team")
    ) |>
    dplyr::left_join(base_points, by = "team") |>
    dplyr::mutate(
      base_points = dplyr::coalesce(.data$base_points, 0L),
      points      = .data$points + .data$base_points,
      point_diff  = .data$sim_diff +
        as.numeric(dplyr::coalesce(.data$base_diff, 0))
    ) |>
    dplyr::select(-"sim_diff")

  dplyr::bind_rows(
    simulated,
    played_only(setdiff(base_points$team, simulated$team))
  )
}

# Base (played) points for current-season top-division teams.
.compute_base_points_2dt <- function(top_results, has_ties = FALSE,
                                     tie_threshold = 0) {
  if (nrow(top_results) == 0L) {
    return(tibble::tibble(team = character(), base_points = integer()))
  }
  top_results |>
    tidyr::pivot_longer(c("home_team", "away_team"), values_to = "team") |>
    dplyr::mutate(
      name = dplyr::if_else(.data$name == "home_team", "home", "away"),
      points = .points_2dt(
        .data$home_score, .data$away_score, .data$name,
        has_ties = has_ties, tie_threshold = tie_threshold
      )
    ) |>
    dplyr::summarise(
      base_points = sum(.data$points),
      # Point/goal difference over played matches. Carried purely as a
      # TIEBREAKER: without it, teams level on points were ranked by fixture
      # order, which published a confident title-race call that was an
      # artefact of the calendar. Football already ranks points -> gd -> gf.
      base_diff = sum(
        dplyr::if_else(
          .data$name == "home",
          .data$home_score - .data$away_score,
          .data$away_score - .data$home_score
        )
      ),
      .by = "team"
    )
}

# Home-advantage intervals on the natural (additive) scale -- unlike
# football's bivariate Poisson, basketball/handball home_advantage_* are
# in raw points/goals, NOT log-rates. Reported value is the raw advantage.
# `total` is computed from the per-team `home_advantage_tot` Stan output.
.compute_home_advantage_2dt <- function(fit, teams, top_teams_filter) {
  draws_long <- function(var, component) {
    fit$draws(var) |>
      posterior::as_draws_df() |>
      tibble::as_tibble() |>
      tidyr::pivot_longer(c(-".chain", -".draw", -".iteration")) |>
      dplyr::mutate(
        team_idx  = as.integer(readr::parse_number(.data$name)),
        team      = teams$team[.data$team_idx],
        component = component
      )
  }

  dplyr::bind_rows(
    draws_long("home_advantage_off", "offence"),
    draws_long("home_advantage_def", "defence"),
    draws_long("home_advantage_tot", "total")
  ) |>
    dplyr::reframe(
      median = stats::median(.data$value),
      coverage = c(0.5, 0.8, 0.95),
      lower = stats::quantile(.data$value, 0.5 - .data$coverage / 2),
      upper = stats::quantile(.data$value, 0.5 + .data$coverage / 2),
      .by = c("team", "component")
    ) |>
    dplyr::semi_join(top_teams_filter, by = "team")
}
