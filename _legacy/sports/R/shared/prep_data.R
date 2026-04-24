#### Packages ####
box::use(
  stats[setNames],
  readr[read_csv, write_csv],
  dplyr[
    bind_rows,
    select,
    filter,
    arrange,
    mutate,
    tibble,
    row_number,
    rename,
    rename_with,
    semi_join,
    inner_join,
    pull,
    distinct,
    lag,
    join_by,
    mutate_at,
    vars,
    if_else,
    case_when,
    any_of
  ],
  tidyr[pivot_longer, pivot_wider],
  lubridate[today],
  here[here],
  ggplot2[theme_set],
  metill[theme_metill]
)

theme_set(theme_metill())
Sys.setlocale("LC_ALL", "is_IS.UTF-8")

#' Prepare data for Stan model
#'
#' @param config Config list from sport-specific config file
#' @param sex Character string, either "male" or "female"
#' @param end_date Date for filtering data
#'
#' @return List containing prepared data for Stan model
#' @export
prepare_data <- function(config, sex, end_date = Sys.Date()) {
  if (!sex %in% c("male", "female")) {
    stop("Sex must be either 'male' or 'female'")
  }

  sport_dir <- config$sport_dir
  col_data <- config$columns$data
  col_schedule <- config$columns$schedule

  results_dir <- here(sport_dir, "results", sex)
  if (!dir.exists(results_dir)) {
    dir.create(results_dir, recursive = TRUE)
    dir.create(file.path(results_dir, "figures"), recursive = TRUE)
  }

  #### Data Prep ####

  d <- read_csv(
    here(sport_dir, "data", sex, "data.csv")
  )

  # Rename columns that need renaming (only the ones specified in config)
  if (length(col_data) > 0) {
    rename_map <- col_data
    # Only rename columns that exist in the data
    existing <- rename_map[rename_map %in% names(d)]
    if (length(existing) > 0) {
      d <- rename(d, !!!setNames(existing, names(existing)))
    }
  }

  d <- d |>
    select(
      season,
      division,
      date,
      home,
      away,
      home_goals,
      away_goals
    ) |>
    arrange(date) |>
    filter(
      date <= end_date
    ) |>
    mutate(
      game_nr = row_number()
    )

  write_csv(
    d,
    here(sport_dir, "results", sex, "d.csv")
  )

  # Create team mapping
  teams <- tibble(
    team = unique(c(d$home, d$away))
  ) |>
    arrange(team) |>
    mutate(team_nr = row_number())

  write_csv(
    teams,
    here(sport_dir, "results", sex, "teams.csv")
  )

  # Read and prepare next games for prediction
  next_games <- read_csv(
    here(sport_dir, "data", sex, "schedule.csv")
  )

  # Rename schedule columns
  if (length(col_schedule) > 0) {
    rename_map <- col_schedule
    existing <- rename_map[rename_map %in% names(next_games)]
    if (length(existing) > 0) {
      next_games <- rename(next_games, !!!setNames(existing, names(existing)))
    }
  }

  # Apply schedule filter if configured
  if (!is.null(config$divisions$schedule_filter)) {
    next_games <- next_games |>
      filter(date >= end_date) |>
      config$divisions$schedule_filter(end_date)
  } else {
    next_games <- next_games |>
      filter(date >= end_date)
  }

  next_games <- next_games |>
    arrange(date) |>
    filter(
      home %in% teams$team,
      away %in% teams$team
    ) |>
    mutate(
      game_nr = row_number()
    )

  # Get current teams in the top league
  if (config$divisions$filter_top_teams) {
    cur_top_teams <- teams |>
      semi_join(
        tibble(
          team = unique(
            c(
              d |>
                filter(division == 1, season == max(season)) |>
                pull(home) |>
                unique(),
              d |>
                filter(division == 1, season == max(season)) |>
                pull(away) |>
                unique()
            )
          )
        )
      )
  } else {
    cur_top_teams <- teams |>
      semi_join(
        tibble(
          team = unique(
            c(
              d |>
                filter(season == max(season)) |>
                pull(home) |>
                unique(),
              d |>
                filter(season == max(season)) |>
                pull(away) |>
                unique()
            )
          )
        )
      )
  }

  # Calculate time differences between matches for each team
  timediffs <- d |>
    pivot_longer(c(home, away)) |>
    select(
      game_nr,
      season,
      date,
      name,
      value
    ) |>
    mutate(
      time_diff = as.numeric(date - lag(date)),
      .by = value
    ) |>
    mutate(
      time_diff = if_else(is.na(time_diff), 7, time_diff),
      time_diff = pmin(time_diff, 100)
    ) |>
    select(-value, -season) |>
    pivot_wider(names_from = name, values_from = time_diff) |>
    rename(
      home_timediff = home,
      away_timediff = away
    )

  # Calculate round numbers for each team
  rounds <- d |>
    pivot_longer(c(home, away)) |>
    select(
      game_nr,
      season,
      date,
      name,
      value
    ) |>
    mutate(
      round = row_number(),
      .by = value
    ) |>
    select(-value, -season) |>
    pivot_wider(names_from = name, values_from = round) |>
    rename(
      home_round = home,
      away_round = away
    )

  # Calculate round numbers for each team and season
  season_rounds <- d |>
    pivot_longer(c(home, away)) |>
    select(
      game_nr,
      date,
      season,
      name,
      value
    ) |>
    mutate(
      season_round = row_number(),
      first_of_season = 1 * (season_round == 1),
      .by = c(season, value)
    ) |>
    select(-value, -season_round, -season) |>
    pivot_wider(names_from = name, values_from = first_of_season) |>
    rename(
      season_first = home
    ) |>
    select(-away)

  # Prepare model data
  model_d <- d |>
    inner_join(
      timediffs
    ) |>
    inner_join(
      rounds
    ) |>
    inner_join(
      season_rounds
    ) |>
    inner_join(
      teams |> rename(home_nr = team_nr),
      by = join_by(home == team)
    ) |>
    inner_join(
      teams |> rename(away_nr = team_nr),
      by = join_by(away == team)
    )

  write_csv(
    model_d,
    here(sport_dir, "results", sex, "model_d.csv")
  )

  # Create time between matches matrix
  n_rounds <- max(c(model_d$home_round, model_d$away_round))
  time_between_matches <- matrix(
    0,
    nrow = nrow(teams),
    ncol = n_rounds
  )
  for (i in 1:nrow(model_d)) {
    time_between_matches[
      model_d$home_nr[i],
      model_d$home_round[i]
    ] <- model_d$home_timediff[i]
    time_between_matches[
      model_d$away_nr[i],
      model_d$away_round[i]
    ] <- model_d$away_timediff[i]
  }

  # Determine next game dates for top teams
  if (config$divisions$filter_next_games) {
    next_game_dates <- next_games |>
      filter(division == 1) |>
      pivot_longer(c(home, away)) |>
      mutate(
        game_nr = row_number(),
        .by = value
      ) |>
      filter(
        game_nr == 1,
        .by = value
      ) |>
      select(next_date = date, team = value)
  } else {
    next_game_dates <- next_games |>
      pivot_longer(c(home, away)) |>
      mutate(
        game_nr = row_number(),
        .by = value
      ) |>
      filter(
        game_nr == 1,
        .by = value
      ) |>
      select(next_date = date, team = value)
  }

  latest_game_dates <- model_d |>
    pivot_longer(c(home, away)) |>
    select(date, team = value) |>
    filter(
      date == max(date),
      .by = team
    ) |>
    rename(latest_date = date)

  time_to_next_games <- next_game_dates |>
    inner_join(
      latest_game_dates
    ) |>
    mutate(
      timediff = as.numeric(next_date - latest_date)
    ) |>
    pull(timediff)

  if (config$divisions$filter_next_games) {
    top_teams <- next_games |>
      filter(division == 1) |>
      pivot_longer(c(home, away)) |>
      distinct(value) |>
      rename(team = value) |>
      inner_join(teams)
  } else {
    top_teams <- next_games |>
      pivot_longer(c(home, away)) |>
      distinct(value) |>
      rename(team = value) |>
      inner_join(teams)
  }

  write_csv(
    top_teams,
    here(sport_dir, "results", sex, "top_teams.csv")
  )

  next_games <- next_games |>
    inner_join(
      next_games |>
        pivot_longer(c(home, away), values_to = "team") |>
        inner_join(
          latest_game_dates
        ) |>
        mutate(
          team_game = row_number(),
          last_date = if_else(team_game == 1, latest_date, lag(date)),
          .by = team
        ) |>
        mutate(
          timediff = as.numeric(date - last_date)
        ) |>
        select(game_nr, name, timediff) |>
        pivot_wider(values_from = timediff) |>
        rename(
          home_timediff = home,
          away_timediff = away
        )
    ) |>
    mutate_at(
      vars(home_timediff, away_timediff),
      \(x) pmin(x, 50)
    )

  write_csv(
    next_games,
    here(sport_dir, "results", sex, "next_games.csv")
  )

  # Prepare prediction data
  pred_d <- next_games |>
    inner_join(
      teams |> rename(home_nr = team_nr),
      by = join_by(home == team)
    ) |>
    inner_join(
      teams |> rename(away_nr = team_nr),
      by = join_by(away == team)
    )

  write_csv(
    pred_d,
    here(sport_dir, "results", sex, "pred_d.csv")
  )

  # Prepare Stan data - always include division/pred_division
  stan_data <- list(
    K = nrow(teams),
    N = nrow(model_d),
    N_pred = nrow(pred_d),
    N_rounds = n_rounds,
    N_seasons = length(unique(model_d$season)),
    season = as.numeric(as.factor(model_d$season)),
    team1 = model_d$home_nr,
    team2 = model_d$away_nr,
    round1 = model_d$home_round,
    round2 = model_d$away_round,
    time_between_matches = time_between_matches,
    goals1 = model_d$home_goals,
    goals2 = model_d$away_goals,
    division = model_d$division,
    team1_pred = pred_d$home_nr,
    team2_pred = pred_d$away_nr,
    pred_timediff1 = pred_d$home_timediff,
    pred_timediff2 = pred_d$away_timediff,
    pred_division = pred_d$division,
    time_to_next_games = time_to_next_games,
    top_teams = top_teams$team_nr,
    N_top_teams = nrow(top_teams)
  )

  return(stan_data)
}
