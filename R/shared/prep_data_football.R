#### Packages ####
box::use(
  readr[read_csv, write_csv],
  dplyr[select, filter, arrange, mutate, tibble, row_number, rename,
        semi_join, inner_join, pull, distinct, lag, join_by, mutate_at, vars,
        if_else],
  tidyr[pivot_longer, pivot_wider],
  lubridate[today],
  here[here],
  ggplot2[theme_set],
  metill[theme_metill]
)

theme_set(theme_metill())
Sys.setlocale("LC_ALL", "is_IS.UTF-8")

#' Prepare data for football model
#'
#' @param sex Character string, either "male" or "female"
#' @param from_season Integer, starting season for analysis (default: 2021)
#'
#' @return List containing prepared data for Stan model
#' @export
prepare_football_data <- function(sex, from_season = 2021, test_date_cutoff = NULL) {

  # Validate input
  if (!sex %in% c("male", "female")) {
    stop("Sex must be either 'male' or 'female'")
  }

#### Data Prep ####

# Read historical data
d <- read_csv(
  here("data", sex, "data.csv")
)

# Support Icelandic column names (football/iceland uses these)
if ("timabil" %in% names(d)) {
  d <- d |> rename(
    season = timabil,
    date = dags,
    home = heima,
    away = gestir,
    home_goals = stig_heima,
    away_goals = stig_gestir
  )
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
  filter(
    season >= from_season
  ) |>
  arrange(date) |>
  mutate(
    game_nr = row_number()
  )

# Backtest mode: split into train/test and keep test for predictions
test_data <- NULL
if (!is.null(test_date_cutoff)) {
  test_date_cutoff <- as.Date(test_date_cutoff)
  test_data <- d |> filter(date >= test_date_cutoff)
  d <- d |> filter(date < test_date_cutoff)
  d <- d |> mutate(game_nr = row_number())
}

write_csv(
  d,
  here("results", sex, "d.csv")
)

# Create team mapping
teams <- tibble(
  team = unique(c(d$home, d$away))
) |>
  arrange(team) |>
  mutate(team_nr = row_number())

write_csv(
  teams,
  here("results", sex, "teams.csv")
)

if (!is.null(test_data)) {
  # Backtest mode: test data becomes prediction targets
  next_games <- test_data |>
    filter(
      home %in% teams$team,
      away %in% teams$team
    ) |>
    arrange(date) |>
    mutate(
      game_nr = row_number()
    )
} else {
  # Production mode: read schedule
  next_games <- read_csv(
    here("data", sex, "schedule.csv")
  ) |>
    filter(
      date >= today(),
      (date <= today() + 14) | (division == 1)
    ) |>
    arrange(date) |>
    filter(
      home %in% teams$team,
      away %in% teams$team
    ) |>
    mutate(
      game_nr = row_number()
    )
}

# Get current teams in the top league
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
  filter(
    season >= from_season
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
  filter(
    season >= from_season
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
  filter(
    season >= from_season
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
  here("results", sex, "model_d.csv")
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


# Handle empty prediction set (e.g., off-season or in-sample-only mode)
if (nrow(next_games) == 0) {
  stan_data <- list(
    K = nrow(teams),
    N = nrow(model_d),
    N_pred = 0L,
    N_rounds = n_rounds,
    season_first = model_d$season_first,
    team1 = model_d$home_nr,
    team2 = model_d$away_nr,
    round1 = model_d$home_round,
    round2 = model_d$away_round,
    time_between_matches = time_between_matches,
    goals1 = model_d$home_goals,
    goals2 = model_d$away_goals,
    team1_pred = integer(0),
    team2_pred = integer(0),
    pred_timediff1 = numeric(0),
    pred_timediff2 = numeric(0),
    time_to_next_games = numeric(0),
    top_teams = integer(0),
    N_top_teams = 0L,
    season = as.integer(as.factor(model_d$season)),
    N_seasons = max(as.integer(as.factor(model_d$season))),
    division = as.integer(model_d$division),
    pred_division = integer(0)
  )

  if (!is.null(test_data)) {
    return(list(
      stan_data = stan_data,
      teams = teams,
      pred_d = tibble(),
      test_actuals = tibble()
    ))
  }
  return(stan_data)
}

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

top_teams <- next_games |>
  filter(division == 1) |>
  pivot_longer(c(home, away)) |>
  distinct(value) |>
  rename(team = value) |>
  inner_join(teams)

write_csv(
  top_teams,
  here("results", sex, "top_teams.csv")
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
  here("results", sex, "next_games.csv")
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
  here("results", sex, "pred_d.csv")
)

# Student-t model fields: season index and division
season_int <- as.integer(as.factor(model_d$season))
division_int <- as.integer(model_d$division)
pred_division_int <- if (nrow(pred_d) > 0) as.integer(pred_d$division) else integer(0)

# Prepare Stan data
stan_data <- list(
  K = nrow(teams),
  N = nrow(model_d),
  N_pred = nrow(pred_d),
  N_rounds = n_rounds,
  season_first = model_d$season_first,
  team1 = model_d$home_nr,
  team2 = model_d$away_nr,
  round1 = model_d$home_round,
  round2 = model_d$away_round,
  time_between_matches = time_between_matches,
  goals1 = model_d$home_goals,
  goals2 = model_d$away_goals,
  team1_pred = pred_d$home_nr,
  team2_pred = pred_d$away_nr,
  pred_timediff1 = pred_d$home_timediff,
  pred_timediff2 = pred_d$away_timediff,
  time_to_next_games = time_to_next_games,
  top_teams = top_teams$team_nr,
  N_top_teams = nrow(top_teams),
  # Student-t model fields (ignored by Poisson model)
  season = season_int,
  N_seasons = max(season_int),
  division = division_int,
  pred_division = pred_division_int
)

  if (!is.null(test_data)) {
    return(list(
      stan_data = stan_data,
      teams = teams,
      pred_d = pred_d,
      test_actuals = next_games
    ))
  }
  return(stan_data)
}
