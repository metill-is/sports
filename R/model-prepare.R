#' @include storage.R
NULL

#' Filter results to matches where both teams have recently competed in
#' specified divisions.
#'
#' The bivariate-Poisson football model assigns each team a hierarchically-
#' parameterised attack/defence pair plus a random-walk innovation per round.
#' With Mjólkurbikar in the training set, ~30 of ~97 teams have ≤10 matches
#' (most cup-only amateurs scoring 0-12 against top-flight sides). Their
#' parameters are weakly identified, and the inter-tier likelihood tension
#' creates funnel-shaped curvature in the posterior — manifesting as 7 % of
#' post-warmup transitions diverging at Stan's default `adapt_delta = 0.8`.
#'
#' The fix is to align the training population with the prediction
#' population: bets are only ever placed on BD/LD1/LD2/LD3 matches and on
#' Mjólkurbikar rounds that have reached at least LD3-tier opposition. So
#' restrict training to teams that have competed in one of those divisions
#' within a recent window. Their cross-tier cup matches against other
#' qualifying teams stay in (useful for tracking promotion-track strength);
#' their cup matches against amateur sides drop out (since amateur opponents
#' fail the filter).
#'
#' Operates on rows already filtered to the (sport, country, sex) and to
#' `match_date <= end_date`. The qualifying-team set is computed within the
#' window `[end_date - lookback_days, end_date]`. The match-retention filter
#' is then applied over the *full* `results` (not the window) — so a 2023
#' BD-vs-BD match between two currently-competing teams stays in training.
#'
#' @param results Tibble with at least `home_team`, `away_team`,
#'   `match_date`, `division`.
#' @param divisions Character vector of qualifying division codes.
#' @param lookback_days Integer; window relative to `end_date`.
#' @param end_date Training cutoff date. Window is `[end_date - lookback_days,
#'   end_date]`.
#' @return Subset of `results` (same columns) keeping only matches where
#'   both teams competed in `divisions` within the window.
#' @noRd
filter_results_by_top_divisions <- function(results, divisions, lookback_days,
                                            end_date) {
  if (nrow(results) == 0L || length(divisions) == 0L) {
    return(results)
  }
  window_start <- end_date - as.integer(lookback_days)
  qualifying <- results[
    results$division %in% divisions &
      results$match_date >= window_start &
      results$match_date <= end_date, ,
    drop = FALSE
  ]
  qualifying_teams <- unique(c(qualifying$home_team, qualifying$away_team))
  results[
    results$home_team %in% qualifying_teams &
      results$away_team %in% qualifying_teams, ,
    drop = FALSE
  ]
}

#' Build stan_data + pred_d + teams from the facts store.
#'
#' Reads `data/facts/results/` and `data/facts/schedules/` for the given
#' (league, sex), assembles the canonical Stan input list, and returns it
#' along with the prediction tibble (for posterior-draw joining) and the
#' team registry. Pure function -- no file I/O beyond read_table().
#'
#' The returned stan_data is a superset covering every field consumed by
#' the three production models (basketball scalar-sigma, handball per-team
#' sigma, football BVP). Each Stan model's data{} block picks only the
#' fields it declares.
#'
#' @param league A single entry from `load_leagues()` (must have `sport` +
#'   `country` set; `stan_model` is not read here).
#' @param sex "male" or "female".
#' @param end_date Cutoff date -- matches on or before this go into training.
#' @param root Data root. Default `here::here("data")`.
#' @param from_season Optional integer. If supplied, drop matches with
#'   `season < from_season`.
#' @param schedule_horizon_days How far ahead to look for prediction targets.
#'   Matches whose match_date falls in
#'   `[end_date, end_date + schedule_horizon_days]` are kept.
#' @return `list(stan_data, pred_d, teams)`.
#' @importFrom rlang .data
#' @export
prepare_data <- function(league,
                         sex,
                         end_date = Sys.Date(),
                         root = here::here("data"),
                         from_season = NULL,
                         schedule_horizon_days = 14L) {
  stopifnot(sex %in% c("male", "female"))
  stopifnot(!is.null(league$sport), !is.null(league$country))

  # -- Results (training matches) -------------------------------------------
  results <- read_table(
    "results",
    root = root,
    filter = list(sport = league$sport, country = league$country, sex = sex)
  )

  if (!is.null(from_season)) {
    results <- results[results$season >= as.integer(from_season), , drop = FALSE]
  }
  results <- results[results$match_date <= end_date, , drop = FALSE]
  results <- results[
    !is.na(results$home_score) & !is.na(results$away_score), ,
    drop = FALSE
  ]

  tf <- league$training_filter
  if (!is.null(tf) && length(tf$divisions) > 0L &&
    !is.null(tf$lookback_days)) {
    n_before <- nrow(results)
    results <- filter_results_by_top_divisions(
      results,
      divisions     = tf$divisions,
      lookback_days = as.integer(tf$lookback_days),
      end_date      = end_date
    )
    cli::cli_alert_info(
      "training_filter: kept {nrow(results)}/{n_before} matches \\
      (divisions={.val {tf$divisions}}, lookback={tf$lookback_days}d)"
    )
  }

  results <- results[order(results$match_date), , drop = FALSE]
  results$game_nr <- seq_len(nrow(results))

  # -- Team registry ---------------------------------------------------------
  teams <- tibble::tibble(
    team = sort(unique(c(results$home_team, results$away_team)))
  )
  teams$team_nr <- seq_len(nrow(teams))

  # -- Schedules (prediction matches) ----------------------------------------
  # WHY: when end_date is in the past (historical backfill), the schedules
  # table no longer carries matches between end_date and today -- they've
  # been moved into results once they played. To produce lookahead-free
  # predictions for those rounds, we union the upcoming-schedule with
  # past-results-after-end_date (using only the (sport, country, sex,
  # season, division, match_date, home_team, away_team) columns that
  # schedule entries also carry).
  schedules <- read_table(
    "schedules",
    root = root,
    filter = list(sport = league$sport, country = league$country, sex = sex)
  )

  horizon_end <- end_date + as.integer(schedule_horizon_days)
  pred_cols <- c(
    "sport", "country", "sex", "season", "division",
    "match_date", "home_team", "away_team"
  )

  upcoming_from_schedules <- schedules[
    !is.na(schedules$match_date) &
      schedules$match_date >= end_date &
      schedules$match_date <= horizon_end, ,
    drop = FALSE
  ]
  upcoming_from_schedules <- upcoming_from_schedules[
    , intersect(pred_cols, names(upcoming_from_schedules)),
    drop = FALSE
  ]

  results_all <- read_table(
    "results",
    root = root,
    filter = list(sport = league$sport, country = league$country, sex = sex)
  )
  upcoming_from_results <- results_all[
    !is.na(results_all$match_date) &
      results_all$match_date > end_date &
      results_all$match_date <= horizon_end, ,
    drop = FALSE
  ]
  upcoming_from_results <- upcoming_from_results[
    , intersect(pred_cols, names(upcoming_from_results)),
    drop = FALSE
  ]

  next_games <- dplyr::bind_rows(
    upcoming_from_schedules, upcoming_from_results
  )
  next_games <- next_games[
    !duplicated(next_games[, c("match_date", "home_team", "away_team")]), ,
    drop = FALSE
  ]

  # Only predict matches whose teams appear in the training set
  next_games <- next_games[
    next_games$home_team %in% teams$team & next_games$away_team %in% teams$team, ,
    drop = FALSE
  ]
  next_games <- next_games[order(next_games$match_date), , drop = FALSE]
  if (nrow(next_games) > 0L) next_games$game_nr <- seq_len(nrow(next_games))

  # -- Per-team time-between-matches and round index -------------------------
  long <- dplyr::bind_rows(
    dplyr::transmute(results,
      game_nr = .data$game_nr,
      match_date = .data$match_date,
      team = .data$home_team,
      side = "home"
    ),
    dplyr::transmute(results,
      game_nr = .data$game_nr,
      match_date = .data$match_date,
      team = .data$away_team,
      side = "away"
    )
  )
  long <- long[order(long$team, long$match_date), , drop = FALSE]
  long <- long |>
    dplyr::group_by(.data$team) |>
    dplyr::mutate(
      round = dplyr::row_number(),
      time_diff = as.numeric(.data$match_date - dplyr::lag(.data$match_date)),
      time_diff = dplyr::if_else(is.na(.data$time_diff), 7, .data$time_diff),
      time_diff = pmin(.data$time_diff, 100)
    ) |>
    dplyr::ungroup()

  home_long <- long[long$side == "home", c("game_nr", "round", "time_diff"), drop = FALSE]
  names(home_long) <- c("game_nr", "home_round", "home_timediff")
  away_long <- long[long$side == "away", c("game_nr", "round", "time_diff"), drop = FALSE]
  names(away_long) <- c("game_nr", "away_round", "away_timediff")

  # Season-first flag: 1 at the first appearance of each team in each season
  season_first_long <- dplyr::bind_rows(
    dplyr::transmute(results,
      game_nr = .data$game_nr,
      season = .data$season,
      team = .data$home_team,
      side = "home"
    ),
    dplyr::transmute(results,
      game_nr = .data$game_nr,
      season = .data$season,
      team = .data$away_team,
      side = "away"
    )
  )
  season_first_long <- season_first_long |>
    dplyr::arrange(.data$game_nr) |>
    dplyr::group_by(.data$team, .data$season) |>
    dplyr::mutate(
      season_round = dplyr::row_number(),
      is_first = as.integer(.data$season_round == 1L)
    ) |>
    dplyr::ungroup()
  season_first_by_game <- season_first_long |>
    dplyr::filter(.data$side == "home") |>
    dplyr::select("game_nr", season_first = "is_first")

  model_d <- results |>
    dplyr::inner_join(home_long, by = "game_nr") |>
    dplyr::inner_join(away_long, by = "game_nr") |>
    dplyr::inner_join(season_first_by_game, by = "game_nr") |>
    dplyr::inner_join(
      dplyr::rename(teams, home_nr = "team_nr"),
      by = c("home_team" = "team")
    ) |>
    dplyr::inner_join(
      dplyr::rename(teams, away_nr = "team_nr"),
      by = c("away_team" = "team")
    ) |>
    dplyr::arrange(.data$match_date)

  # Division as integer factor (1 = first level, 2 = second, ...)
  div_levels <- sort(unique(model_d$division))
  model_d$division_int <- as.integer(factor(model_d$division, levels = div_levels))

  N_rounds <- max(c(model_d$home_round, model_d$away_round))
  tbm <- matrix(0, nrow = nrow(teams), ncol = N_rounds)
  tbm[cbind(model_d$home_nr, model_d$home_round)] <- model_d$home_timediff
  tbm[cbind(model_d$away_nr, model_d$away_round)] <- model_d$away_timediff

  # -- Prediction tibble -----------------------------------------------------
  if (nrow(next_games) > 0L) {
    latest_game_dates <- long |>
      dplyr::group_by(.data$team) |>
      dplyr::summarise(latest_date = max(.data$match_date), .groups = "drop")

    upcoming_per_team <- next_games |>
      tidyr::pivot_longer(
        c("home_team", "away_team"),
        names_to = "side",
        values_to = "team"
      ) |>
      dplyr::group_by(.data$team) |>
      dplyr::arrange(.data$match_date) |>
      dplyr::mutate(team_game = dplyr::row_number()) |>
      dplyr::ungroup()

    first_upcoming <- upcoming_per_team |>
      dplyr::filter(.data$team_game == 1L) |>
      dplyr::select("team", next_date = "match_date") |>
      dplyr::inner_join(latest_game_dates, by = "team") |>
      dplyr::mutate(
        timediff = pmin(as.numeric(.data$next_date - .data$latest_date), 100)
      )

    time_to_next <- first_upcoming$timediff
    top_teams_df <- teams[teams$team %in% first_upcoming$team, , drop = FALSE]

    pred_timediffs <- upcoming_per_team |>
      dplyr::inner_join(latest_game_dates, by = "team") |>
      dplyr::group_by(.data$team) |>
      dplyr::arrange(.data$match_date) |>
      dplyr::mutate(
        prev_date = dplyr::if_else(
          .data$team_game == 1L,
          .data$latest_date,
          dplyr::lag(.data$match_date)
        ),
        timediff = pmin(as.numeric(.data$match_date - .data$prev_date), 50)
      ) |>
      dplyr::ungroup() |>
      dplyr::select("game_nr", "side", "timediff") |>
      tidyr::pivot_wider(names_from = "side", values_from = "timediff") |>
      dplyr::rename(home_timediff = "home_team", away_timediff = "away_team")

    pred_d <- next_games |>
      dplyr::inner_join(pred_timediffs, by = "game_nr") |>
      dplyr::inner_join(
        dplyr::rename(teams, home_nr = "team_nr"),
        by = c("home_team" = "team")
      ) |>
      dplyr::inner_join(
        dplyr::rename(teams, away_nr = "team_nr"),
        by = c("away_team" = "team")
      )

    pred_d$division_int <- as.integer(
      factor(pred_d$division, levels = div_levels)
    )
  } else {
    time_to_next <- numeric(0)
    top_teams_df <- teams[0L, , drop = FALSE]
    pred_d <- tibble::tibble(
      game_nr = integer(0),
      match_date = as.Date(character()),
      home_team = character(),
      away_team = character(),
      division = character(),
      home_nr = integer(0),
      away_nr = integer(0),
      home_timediff = numeric(0),
      away_timediff = numeric(0),
      division_int = integer(0)
    )
  }

  stan_data <- list(
    K                    = nrow(teams),
    N                    = nrow(model_d),
    N_pred               = nrow(pred_d),
    N_rounds             = N_rounds,
    N_seasons            = length(unique(model_d$season)),
    season               = as.integer(as.factor(model_d$season)),
    season_first         = as.integer(model_d$season_first),
    team1                = as.integer(model_d$home_nr),
    team2                = as.integer(model_d$away_nr),
    round1               = as.integer(model_d$home_round),
    round2               = as.integer(model_d$away_round),
    time_between_matches = tbm,
    goals1               = as.integer(model_d$home_score),
    goals2               = as.integer(model_d$away_score),
    division             = as.integer(model_d$division_int),
    team1_pred           = as.integer(pred_d$home_nr),
    team2_pred           = as.integer(pred_d$away_nr),
    pred_timediff1       = as.numeric(pred_d$home_timediff),
    pred_timediff2       = as.numeric(pred_d$away_timediff),
    pred_division        = as.integer(pred_d$division_int),
    time_to_next_games   = as.numeric(time_to_next),
    top_teams            = as.integer(top_teams_df$team_nr),
    N_top_teams          = nrow(top_teams_df)
  )

  list(
    stan_data = stan_data,
    pred_d = pred_d[, c(
      "game_nr", "match_date", "home_team", "away_team",
      "division", "home_nr", "away_nr",
      "home_timediff", "away_timediff"
    ), drop = FALSE],
    teams = teams
  )
}
