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

# 3-letter team code: strip whitespace + dots, uppercase, take first 3 chars.
.short_code_2dt <- function(team) {
  team |>
    stringr::str_remove_all("\\s|\\.") |>
    stringr::str_to_upper() |>
    stringr::str_sub(1L, 3L)
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

# Per-team standings for the current top-division season. xG fields are
# always shipped as NA (Student-t produces no Poisson xG aggregate); the
# platform JS handles null via showExpectedCols.
.compute_standings_rows_2dt <- function(top_results, has_ties = FALSE,
                                        tie_threshold = 0) {
  if (nrow(top_results) == 0L) {
    return(tibble::tibble(
      team = character(), short = character(),
      played = integer(), wins = integer(), draws = integer(),
      losses = integer(),
      goals_for = integer(), goals_against = integer(),
      goal_diff = integer(), points = integer(),
      xg_for = numeric(), xg_against = numeric(), xpts = numeric(),
      rank = integer(),
      form = list(), xg_trend = list()
    ))
  }

  long <- dplyr::bind_rows(
    dplyr::transmute(top_results,
      team = .data$home_team, match_date = .data$match_date,
      gf = .data$home_score, ga = .data$away_score
    ),
    dplyr::transmute(top_results,
      team = .data$away_team, match_date = .data$match_date,
      gf = .data$away_score, ga = .data$home_score
    )
  )

  if (has_ties) {
    long <- dplyr::mutate(long,
      result = dplyr::case_when(
        abs(.data$gf - .data$ga) <= tie_threshold ~ "D",
        .data$gf > .data$ga ~ "W",
        TRUE ~ "L"
      )
    )
  } else {
    long <- dplyr::mutate(long,
      result = dplyr::if_else(.data$gf > .data$ga, "W", "L")
    )
  }
  long <- dplyr::arrange(long, .data$team, .data$match_date)

  # Per-side points totals derived from the result column for consistency.
  long <- dplyr::mutate(long,
    side_points = dplyr::case_when(
      .data$result == "W" ~ 2L,
      .data$result == "D" ~ 1L,
      TRUE ~ 0L
    )
  )

  long |>
    dplyr::summarise(
      played = dplyr::n(),
      wins = sum(.data$result == "W"),
      draws = sum(.data$result == "D"),
      losses = sum(.data$result == "L"),
      goals_for = sum(.data$gf),
      goals_against = sum(.data$ga),
      goal_diff = .data$goals_for - .data$goals_against,
      points = sum(.data$side_points),
      form = list(.append_format_2dt(.data$result)),
      xg_trend = list(numeric(0)),
      .by = "team"
    ) |>
    dplyr::arrange(
      dplyr::desc(.data$points), dplyr::desc(.data$goal_diff),
      dplyr::desc(.data$goals_for)
    ) |>
    dplyr::mutate(
      rank = dplyr::row_number(),
      short = .short_code_2dt(.data$team),
      xg_for = NA_real_, xg_against = NA_real_, xpts = NA_real_
    ) |>
    dplyr::select(
      "team", "short", "played", "wins", "draws", "losses",
      "goals_for", "goals_against", "goal_diff", "points",
      "xg_for", "xg_against", "xpts",
      "rank", "form", "xg_trend"
    )
}

# Last-5 form indicator, padded to exactly five entries with leading NAs.
# Wrapped to avoid leaking utils::tail through summarise() namespace handling.
.append_format_2dt <- function(result_vec) {
  .pad_form_2dt(utils::tail(result_vec, 5L))
}

# Per-draw per-team end-of-season points combining base (played) points
# with future-fixture predictions. Returns a long tibble with `.draw`,
# `team`, `points`.
.compute_iter_team_points_2dt <- function(posterior_goals, top_div, base_points,
                                          has_ties = FALSE,
                                          tie_threshold = 0) {
  if (nrow(posterior_goals) == 0L) {
    return(tibble::tibble(
      .draw = integer(), team = character(), points = numeric(),
      base_points = integer()
    ))
  }

  pg_top <- posterior_goals |>
    dplyr::filter(.data$division == top_div)

  if (nrow(pg_top) == 0L) {
    return(tibble::tibble(
      .draw = integer(), team = character(), points = numeric(),
      base_points = integer()
    ))
  }

  pg_top |>
    tidyr::pivot_longer(c("home_team", "away_team"), values_to = "team") |>
    dplyr::mutate(
      name = dplyr::if_else(.data$name == "home_team", "home", "away"),
      points = .points_2dt(
        .data$home_score, .data$away_score, .data$name,
        has_ties = has_ties, tie_threshold = tie_threshold
      )
    ) |>
    dplyr::summarise(points = sum(.data$points), .by = c(".draw", "team")) |>
    dplyr::left_join(base_points, by = "team") |>
    dplyr::mutate(
      base_points = dplyr::coalesce(.data$base_points, 0L),
      points      = .data$points + .data$base_points
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
    dplyr::summarise(base_points = sum(.data$points), .by = "team")
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

# Round number = min completed appearances across teams in current top
# division (postponed fixtures don't overstate progress).
.compute_round_num_2dt <- function(top_results) {
  if (nrow(top_results) == 0L) {
    return(0L)
  }
  counts <- top_results |>
    tidyr::pivot_longer(c("home_team", "away_team"), values_to = "team") |>
    dplyr::count(.data$team) |>
    dplyr::pull("n")
  if (length(counts) == 0L) 0L else as.integer(min(counts))
}
