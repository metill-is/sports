#### Packages ####
library(tidyverse)
library(scales)
library(cmdstanr)
library(posterior)
library(metill)
library(geomtextpath)
library(ggtext)
library(glue)
library(here)
theme_set(theme_metill())
Sys.setlocale("LC_ALL", "is_IS.UTF-8")

fit_historical_model <- function(start_date, end_date, sex = "male") {
  # Read data
  d <- read_csv(here("data", sex, "data.csv")) |>
    select(
      season = timabil,
      division,
      date = dags,
      home = heima,
      away = gestir,
      home_goals = stig_heima,
      away_goals = stig_gestir
    ) |>
    filter(
      date >= start_date
    ) |>
    arrange(date) |>
    mutate(
      game_nr = row_number()
    )

  # Create team mapping
  teams <- tibble(
    team = unique(c(d$home, d$away))
  ) |>
    arrange(team) |>
    mutate(team_nr = row_number())

  # Read and prepare next games for prediction
  next_games <- read_csv(
    here("data", sex, "schedule.csv")
  ) |>
    filter(
      dags >= end_date,
      (dags <= end_date + 14) | (division == 1)
    ) |>
    arrange(dags) |>
    rename(
      date = dags,
      home = heima,
      away = gestir
    ) |>
    mutate(
      game_nr = row_number()
    )

  # Filter data for training period
  d <- d |>
    filter(
      date >= start_date,
      date <= end_date
    )

  # Calculate time differences between matches
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
    inner_join(timediffs) |>
    inner_join(rounds) |>
    inner_join(season_rounds) |>
    inner_join(
      teams |> rename(home_nr = team_nr),
      by = join_by(home == team)
    ) |>
    inner_join(
      teams |> rename(away_nr = team_nr),
      by = join_by(away == team)
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

  # Get latest game dates for each team
  latest_game_dates <- model_d |>
    pivot_longer(c(home, away)) |>
    select(date, team = value) |>
    filter(
      date == max(date),
      .by = team
    ) |>
    rename(latest_date = date)

  # Calculate time to next games
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

  time_to_next_games <- next_game_dates |>
    inner_join(latest_game_dates) |>
    mutate(
      timediff = as.numeric(next_date - latest_date)
    ) |>
    pull(timediff)

  # Get top teams
  top_teams <- next_games |>
    filter(division == 1) |>
    pivot_longer(c(home, away)) |>
    distinct(value) |>
    rename(team = value) |>
    inner_join(teams)

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
    N_top_teams = nrow(top_teams)
  )

  # Compile and fit model
  model <- cmdstan_model(
    here(
      "Stan",
      "bivariate_poisson_inflated_diagonal_corrmodel.stan"
    )
  )

  # Fit model
  results <- model$sample(
    data = stan_data,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 1000,
    iter_sampling = 1000,
    init = 0
  )

  # Get predictions
  posterior_goals <- results$draws(c("goals1_pred", "goals2_pred")) |>
    as_draws_df() |>
    as_tibble() |>
    pivot_longer(
      -c(.draw, .chain, .iteration),
      names_to = "parameter",
      values_to = "value"
    ) |>
    mutate(
      type = if_else(
        str_detect(parameter, "goals1"),
        "home_goals",
        "away_goals"
      ),
      game_nr = str_match(parameter, "d\\[(.*)\\]$")[, 2] |> as.numeric()
    ) |>
    select(.draw, type, game_nr, value) |>
    pivot_wider(names_from = type, values_from = value) |>
    inner_join(next_games, by = "game_nr") |>
    select(
      iteration = .draw,
      game_nr,
      division,
      date,
      home,
      away,
      home_goals,
      away_goals
    ) |>
    count(date, division, home, away, home_goals, away_goals) |>
    mutate(
      p = n / sum(n),
      .by = c(date, division, home, away)
    )

  # Save predictions
  dir.create(
    here("results", sex, "historical", end_date),
    showWarnings = FALSE,
    recursive = TRUE
  )

  posterior_goals |>
    write_csv(
      here(
        "results",
        sex,
        "historical",
        end_date,
        "posterior_goals.csv"
      )
    )

  posterior_goals <- results$draws(c("goals1_pred", "goals2_pred")) |>
    as_draws_df() |>
    as_tibble() |>
    pivot_longer(
      -c(.draw, .chain, .iteration),
      names_to = "parameter",
      values_to = "value"
    ) |>
    mutate(
      type = if_else(
        str_detect(parameter, "goals1"),
        "home_goals",
        "away_goals"
      ),
      game_nr = str_match(parameter, "d\\[(.*)\\]$")[, 2] |> as.numeric()
    ) |>
    select(.draw, type, game_nr, value) |>
    pivot_wider(names_from = type, values_from = value) |>
    inner_join(next_games, by = "game_nr") |>
    filter(
      division == 1
    ) |>
    select(
      iteration = .draw,
      game_nr,
      division,
      date,
      home,
      away,
      home_goals,
      away_goals
    )

  base_points <- d |>
    filter(season == 2025, division == 1) |>
    mutate(
      result = case_when(
        home_goals > away_goals ~ "home",
        home_goals < away_goals ~ "away",
        TRUE ~ "tie"
      )
    ) |>
    pivot_longer(c(home, away), values_to = "team") |>
    mutate(
      points = case_when(
        result == "tie" ~ 1,
        result == name ~ 3,
        TRUE ~ 0
      )
    ) |>
    summarise(
      base_points = sum(points),
      .by = c(team)
    ) |>
    arrange(desc(base_points))

  p_top <- posterior_goals |>
    mutate(
      result = case_when(
        home_goals > away_goals ~ "home",
        home_goals < away_goals ~ "away",
        TRUE ~ "tie"
      )
    ) |>
    pivot_longer(c(home, away), values_to = "team") |>
    mutate(
      points = case_when(
        result == "tie" ~ 1,
        result == name ~ 3,
        TRUE ~ 0
      )
    ) |>
    summarise(
      points = sum(points),
      .by = c(iteration, team)
    ) |>
    left_join(
      base_points
    ) |>
    mutate(
      base_points = coalesce(base_points, 0),
      points = points + base_points
    ) |>
    arrange(desc(points)) |>
    mutate(
      position = row_number(),
      .by = iteration
    ) |>
    summarise(
      p_top = mean(position <= 6),
      .by = team
    ) |>
    arrange(desc(p_top))

  plot_dat <- posterior_goals |>
    mutate(
      result = case_when(
        home_goals > away_goals ~ "home",
        home_goals < away_goals ~ "away",
        TRUE ~ "tie"
      )
    ) |>
    pivot_longer(c(home, away), values_to = "team") |>
    mutate(
      points = case_when(
        result == "tie" ~ 1,
        result == name ~ 3,
        TRUE ~ 0
      )
    ) |>
    summarise(
      points = sum(points),
      .by = c(iteration, team)
    ) |>
    left_join(
      base_points
    ) |>
    mutate(
      base_points = coalesce(base_points, 0),
      points = points + base_points
    ) |>
    count(team, points) |>
    mutate(
      p_raw = n / sum(n),
      p = p_raw / max(p_raw),
      mean = sum(p_raw * points),
      .by = team
    ) |>
    inner_join(p_top) |>
    mutate(
      p_top = scales::percent(p_top, accuracy = 1),
      team = glue("{team} ({p_top})"),
      team = fct_reorder(team, mean),
      team_nr = as.numeric(team)
    )

  plot_dat |>
    write_csv(
      here(
        "results",
        sex,
        "historical",
        end_date,
        "league_points.csv"
      )
    )

  plot_dat <- posterior_goals |>
    mutate(
      result = case_when(
        home_goals > away_goals ~ "home",
        home_goals < away_goals ~ "away",
        TRUE ~ "tie"
      )
    ) |>
    pivot_longer(c(home, away), values_to = "team") |>
    mutate(
      points = case_when(
        result == "tie" ~ 1,
        result == name ~ 3,
        TRUE ~ 0
      )
    ) |>
    summarise(
      points = sum(points),
      .by = c(iteration, team)
    ) |>
    left_join(
      base_points
    ) |>
    mutate(
      base_points = coalesce(base_points, 0),
      points = points + base_points
    ) |>
    arrange(iteration, desc(points)) |>
    mutate(
      placement = row_number(),
      .by = iteration
    ) |>
    count(team, placement) |>
    mutate(
      p_raw = n / sum(n),
      p = p_raw / max(p_raw),
      mean = sum(p_raw * placement),
      .by = team
    ) |>
    inner_join(p_top) |>
    mutate(
      team = fct_reorder(team, mean),
      team_nr = as.numeric(team)
    )

  plot_dat |>
    write_csv(
      here(
        "results",
        sex,
        "historical",
        end_date,
        "placement.csv"
      )
    )

  plot_dat_away <- results$draws("cur_strength_away") |>
    as_draws_df() |>
    as_tibble() |>
    pivot_longer(c(-.chain, -.draw, -.iteration)) |>
    mutate(
      team = teams$team[parse_number(name)],
      type = "Samtals"
    ) |>
    bind_rows(
      results$draws("cur_offense_away") |>
        as_draws_df() |>
        as_tibble() |>
        pivot_longer(c(-.chain, -.draw, -.iteration)) |>
        mutate(
          team = teams$team[parse_number(name)],
          type = "Sókn"
        )
    ) |>
    bind_rows(
      results$draws("cur_defense_away") |>
        as_draws_df() |>
        as_tibble() |>
        pivot_longer(c(-.chain, -.draw, -.iteration)) |>
        mutate(
          team = teams$team[parse_number(name)],
          type = "Vörn"
        )
    ) |>
    reframe(
      median = median(value),
      coverage = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
      lower = quantile(value, 0.5 - coverage / 2),
      upper = quantile(value, 0.5 + coverage / 2),
      .by = c(team, type)
    ) |>
    mutate(
      type = as_factor(type) |>
        fct_relevel("Sókn", "Vörn", "Samtals"),
      team = factor(
        team,
        levels = unique(team)[order(unique(median[type == "Samtals"]))]
      )
    )

  plot_dat_home <- results$draws("cur_strength_home") |>
    as_draws_df() |>
    as_tibble() |>
    pivot_longer(c(-.chain, -.draw, -.iteration)) |>
    mutate(
      team = teams$team[parse_number(name)],
      type = "Samtals"
    ) |>
    bind_rows(
      results$draws("cur_offense_home") |>
        as_draws_df() |>
        as_tibble() |>
        pivot_longer(c(-.chain, -.draw, -.iteration)) |>
        mutate(
          team = teams$team[parse_number(name)],
          type = "Sókn"
        )
    ) |>
    bind_rows(
      results$draws("cur_defense_home") |>
        as_draws_df() |>
        as_tibble() |>
        pivot_longer(c(-.chain, -.draw, -.iteration)) |>
        mutate(
          team = teams$team[parse_number(name)],
          type = "Vörn"
        )
    ) |>
    reframe(
      median = median(value),
      coverage = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
      lower = quantile(value, 0.5 - coverage / 2),
      upper = quantile(value, 0.5 + coverage / 2),
      .by = c(team, type)
    ) |>
    mutate(
      type = as_factor(type) |>
        fct_relevel("Sókn", "Vörn", "Samtals"),
      team = factor(
        team,
        levels = unique(team)[order(unique(median[type == "Samtals"]))]
      )
    )

  plot_dat <- plot_dat_away |>
    mutate(
      loc = "Gestir"
    ) |>
    bind_rows(
      plot_dat_home |>
        mutate(
          loc = "Heima"
        )
    ) |>
    mutate(
      loc = as_factor(loc) |>
        fct_relevel("Heima")
    ) |>
    semi_join(
      d |>
        filter(season == 2025, division == 1) |>
        pivot_longer(c(home, away), values_to = "team") |>
        distinct(team)
    )

  plot_dat |>
    write_csv(
      here(
        "results",
        sex,
        "historical",
        end_date,
        "current_strength.csv"
      )
    )

  # Return results for potential further analysis
  return(NULL)
}

# Example usage:
# fit_historical_model(
#   start_date = as.Date("2023-01-01"),
#   end_date = as.Date("2023-06-30"),
#   sex = "male"
# )
