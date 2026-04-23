# Export posterior summaries as JSON for the metill-platform website.
#
# Reads the Stan fit + intermediate CSVs for football/iceland and writes JSON
# snapshots into results/{sex}/website/. The Python orchestrator in
# metill-platform/scripts/export_ithrottir.py copies these into the platform
# repo's data/ directory.
#
# Outputs (per sex):
#   meta.json                 — generation metadata
#   next_games.json           — posterior over goal diffs for upcoming matches
#   team_strengths.json       — per-team home/away × offence/defence/total CIs
#   final_positions.json      — placement probability matrix + top-six probs
#   points_distribution.json  — per-team end-of-season points distribution
#   home_advantage.json       — per-team home-advantage effects
#
# Usage:
#   cd ~/sports/Sports/football/iceland
#   Rscript R/export_website_data.R                 # both sexes
#   Rscript R/export_website_data.R --sex male      # only male
#   Rscript R/export_website_data.R --sex female    # only female

library(tidyverse)
library(posterior)
library(jsonlite)
library(here)

Sys.setlocale("LC_ALL", "is_IS.UTF-8")

division_labels <- c("BD", "LD", "ÖD", "ÞD", "FjD", "MB")

extract_team_draws <- function(fit, var, teams, component, location) {
  fit$draws(var) |>
    as_draws_df() |>
    as_tibble() |>
    pivot_longer(c(-.chain, -.draw, -.iteration)) |>
    mutate(
      team_idx = parse_number(name),
      team = teams$team[team_idx],
      component = component,
      location = location
    )
}

summarise_team_intervals <- function(draws, coverages = c(0.5, 0.8, 0.95)) {
  draws |>
    reframe(
      median = median(value),
      coverage = coverages,
      lower = quantile(value, 0.5 - coverage / 2),
      upper = quantile(value, 0.5 + coverage / 2),
      .by = c(team, component, location)
    )
}

export_for_sex <- function(sex) {
  out_dir <- here("results", sex, "website")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  fit <- read_rds(here("results", sex, "fit.rds"))
  d <- read_csv(here("results", sex, "d.csv"), show_col_types = FALSE)
  teams <- read_csv(here("results", sex, "teams.csv"), show_col_types = FALSE)
  pred_d <- read_csv(here("results", sex, "pred_d.csv"), show_col_types = FALSE)
  top_teams <- read_csv(here("results", sex, "top_teams.csv"), show_col_types = FALSE)

  current_season <- max(d$season, na.rm = TRUE)

  current_top_teams <- d |>
    filter(season == current_season, division == 1) |>
    pivot_longer(c(home, away), values_to = "team") |>
    distinct(team)

  # ---- meta ---------------------------------------------------------------

  fit_mtime <- file.info(here("results", sex, "fit.rds"))$mtime
  meta <- list(
    sex = sex,
    league = "Besta deild",
    season = current_season,
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    fit_date = format(fit_mtime, "%Y-%m-%d"),
    n_draws = posterior::ndraws(fit$draws("home_advantage_tot"))
  )
  write_json(
    meta,
    file.path(out_dir, "meta.json"),
    auto_unbox = TRUE
  )

  # ---- posterior match-level draws ----------------------------------------

  posterior_goals <- fit$draws(c("goals1_pred", "goals2_pred")) |>
    as_draws_df() |>
    as_tibble() |>
    pivot_longer(
      -c(.draw, .chain, .iteration),
      names_to = "parameter",
      values_to = "value"
    ) |>
    mutate(
      type = if_else(str_detect(parameter, "goals1"), "home_goals", "away_goals"),
      game_nr = str_match(parameter, "d\\[(.*)\\]$")[, 2] |> as.numeric()
    ) |>
    select(.draw, type, game_nr, value) |>
    pivot_wider(names_from = type, values_from = value) |>
    inner_join(pred_d, by = "game_nr")

  # ---- next_games.json ----------------------------------------------------

  next_games <- posterior_goals |>
    filter(
      date >= today(),
      date <= today() + 14
    ) |>
    mutate(goal_diff = home_goals - away_goals) |>
    summarise(
      mean_home_goals = mean(home_goals),
      mean_away_goals = mean(away_goals),
      mean_goal_diff = mean(goal_diff),
      p_home_win = mean(goal_diff > 0),
      p_draw = mean(goal_diff == 0),
      p_away_win = mean(goal_diff < 0),
      goal_diff_distribution = list(
        tibble(diff = goal_diff) |>
          count(diff) |>
          mutate(p = n / sum(n)) |>
          select(diff, p)
      ),
      .by = c(game_nr, division, date, home, away)
    ) |>
    arrange(date, game_nr) |>
    mutate(
      division_code = division_labels[division],
      date = format(date, "%Y-%m-%d")
    ) |>
    select(
      date, division, division_code, home, away,
      mean_home_goals, mean_away_goals, mean_goal_diff,
      p_home_win, p_draw, p_away_win,
      goal_diff_distribution
    )

  write_json(
    list(
      generated_at = meta$generated_at,
      matches = next_games
    ),
    file.path(out_dir, "next_games.json"),
    auto_unbox = TRUE,
    dataframe = "rows",
    digits = 5
  )

  # ---- team_strengths.json -----------------------------------------------

  team_strengths <- bind_rows(
    extract_team_draws(fit, "cur_offense_home", teams, "offence", "home"),
    extract_team_draws(fit, "cur_defense_home", teams, "defence", "home"),
    extract_team_draws(fit, "cur_strength_home", teams, "total", "home"),
    extract_team_draws(fit, "cur_offense_away", teams, "offence", "away"),
    extract_team_draws(fit, "cur_defense_away", teams, "defence", "away"),
    extract_team_draws(fit, "cur_strength_away", teams, "total", "away")
  ) |>
    summarise_team_intervals() |>
    semi_join(current_top_teams, by = "team")

  write_json(
    list(
      generated_at = meta$generated_at,
      records = team_strengths
    ),
    file.path(out_dir, "team_strengths.json"),
    auto_unbox = TRUE,
    dataframe = "rows",
    digits = 5
  )

  # ---- points + placements (top division only) ---------------------------

  base_points <- d |>
    filter(season == current_season, division == 1) |>
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
    summarise(base_points = sum(points), .by = team)

  iter_team_points <- posterior_goals |>
    filter(division == 1) |>
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
    summarise(points = sum(points), .by = c(.draw, team)) |>
    left_join(base_points, by = "team") |>
    mutate(
      base_points = coalesce(base_points, 0),
      points = points + base_points
    )

  iter_positions <- iter_team_points |>
    arrange(.draw, desc(points)) |>
    mutate(placement = row_number(), .by = .draw)

  n_teams_top_division <- iter_positions |>
    distinct(team) |>
    nrow()

  final_positions <- iter_positions |>
    count(team, placement) |>
    complete(
      team,
      placement = 1:n_teams_top_division,
      fill = list(n = 0)
    ) |>
    mutate(
      probability = n / sum(n),
      .by = team
    ) |>
    select(team, placement, probability) |>
    arrange(team, placement)

  top_six <- iter_positions |>
    summarise(
      p_top_six = mean(placement <= 6),
      p_winner = mean(placement == 1),
      p_relegation = mean(placement >= n_teams_top_division - 1),
      .by = team
    )

  write_json(
    list(
      generated_at = meta$generated_at,
      season = current_season,
      n_teams = n_teams_top_division,
      records = final_positions,
      summary = top_six
    ),
    file.path(out_dir, "final_positions.json"),
    auto_unbox = TRUE,
    dataframe = "rows",
    digits = 5
  )

  points_distribution <- iter_team_points |>
    count(team, points) |>
    mutate(
      probability = n / sum(n),
      .by = team
    ) |>
    select(team, points, probability) |>
    arrange(team, points)

  points_summary <- iter_team_points |>
    summarise(
      mean_points = mean(points),
      median_points = median(points),
      lower_80 = quantile(points, 0.1),
      upper_80 = quantile(points, 0.9),
      .by = team
    ) |>
    left_join(base_points, by = "team") |>
    mutate(base_points = coalesce(base_points, 0)) |>
    left_join(top_six, by = "team")

  write_json(
    list(
      generated_at = meta$generated_at,
      season = current_season,
      records = points_distribution,
      summary = points_summary
    ),
    file.path(out_dir, "points_distribution.json"),
    auto_unbox = TRUE,
    dataframe = "rows",
    digits = 5
  )

  # ---- home_advantage.json -----------------------------------------------

  extract_home_adv <- function(var, component, transform = identity) {
    fit$draws(var) |>
      as_draws_df() |>
      as_tibble() |>
      pivot_longer(c(-.chain, -.draw, -.iteration)) |>
      mutate(
        team_idx = parse_number(name),
        team = teams$team[team_idx],
        component = component,
        value = transform(value)
      )
  }

  home_advantage <- bind_rows(
    extract_home_adv("home_advantage_off", "offence"),
    extract_home_adv("home_advantage_def", "defence"),
    extract_home_adv("home_advantage_tot", "total", transform = function(x) x / 2)
  ) |>
    mutate(multiplier = exp(value)) |>
    reframe(
      median = median(multiplier),
      coverage = c(0.5, 0.8, 0.95),
      lower = quantile(multiplier, 0.5 - coverage / 2),
      upper = quantile(multiplier, 0.5 + coverage / 2),
      .by = c(team, component)
    ) |>
    semi_join(top_teams, by = "team")

  write_json(
    list(
      generated_at = meta$generated_at,
      records = home_advantage
    ),
    file.path(out_dir, "home_advantage.json"),
    auto_unbox = TRUE,
    dataframe = "rows",
    digits = 5
  )

  message(sprintf("Exported %s website data to %s", sex, out_dir))
}

# ---- CLI dispatch ---------------------------------------------------------

parse_sex_arg <- function(args) {
  idx <- match("--sex", args, nomatch = 0L)
  if (idx > 0L && idx < length(args)) {
    sex <- args[idx + 1L]
    if (!sex %in% c("male", "female")) {
      stop("--sex must be 'male' or 'female'")
    }
    return(sex)
  }
  NULL
}

args <- commandArgs(trailingOnly = TRUE)
sex_arg <- parse_sex_arg(args)
if (is.null(sex_arg)) {
  export_for_sex("male")
  export_for_sex("female")
} else {
  export_for_sex(sex_arg)
}
