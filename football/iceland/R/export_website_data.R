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
#   standings.json            — current league table (male only, top division)
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

# Posterior-predictive aggregator for xG / xPts over played matches.
# Self-contained: recomputes and overwrites the CSV on every export run,
# so the exporter works whether `update_model_results.R` has been run
# or not. See DESIGN-xpts-2026-04-23.md for the design.
source(here("R", "utils", "played_match_expected.R"))

division_labels <- c("BD", "LD", "ÖD", "ÞD", "FjD", "MB")

# Static team -> home stadium lookup. Used to populate the `venue` field on
# next_games.json and (eventually) standings rows. Covers the 12 Besta deild
# karla clubs for the 2026 season; teams outside this list serialise as null
# and the consumer renders date-only fixture cards.
male_top_division_venues <- tibble::tribble(
  ~team,           ~venue,
  "Breiðablik",    "Kópavogsvöllur",
  "FH",            "Kaplakrikavöllur",
  "Fram",          "Laugardalsvöllur",
  "KA",            "KA-völlurinn",
  "KR",            "KR-völlur",
  "Keflavík",      "Nettóvöllurinn",
  "Stjarnan",      "Stjörnuvöllur",
  "Valur",         "Hlíðarendi",
  "Víkingur R.",   "Víkingsvöllur",
  "ÍA",            "Norðurálsvöllurinn",
  "ÍBV",           "Hásteinsvöllur",
  "Þór",           "Þórsvöllur"
)

# Append `new_rows` to a history JSON file, dedup on `key_cols` keeping the
# row with the most recent `generated_at`. Creates the file on first call.
#
# Stored shape (mirrors the snapshot files):
#   { "schema_version": 1, "records": [ {…}, {…} ] }
#
# The file grows roughly linearly with the number of R runs. For karla
# `team_strengths_history.json` at full lattice (22 rounds × 12 teams × 3
# components × 2 locations × 3 coverage levels) = ~4.8K rows × ~130 B ≈ 600 KB
# per season. Acceptable; season-rollover cleanup is a future concern.
append_to_history <- function(path, new_rows, key_cols) {
  existing <- if (file.exists(path)) {
    tryCatch(
      {
        parsed <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
        records <- parsed$records
        if (is.data.frame(records)) as_tibble(records) else tibble()
      },
      error = function(e) tibble()
    )
  } else {
    tibble()
  }

  all_rows <- bind_rows(existing, new_rows) |>
    arrange(desc(generated_at)) |>
    distinct(across(all_of(key_cols)), .keep_all = TRUE) |>
    arrange(across(all_of(key_cols)))

  write_json(
    list(schema_version = 1L, records = all_rows),
    path,
    auto_unbox = TRUE,
    dataframe = "rows",
    digits = 5,
    na = "null"
  )
}

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

  # Compute per-team posterior-predictive xG / xPts for played current-season
  # top-division matches. Writes results/{sex}/team_expected_by_draw.csv; we
  # then summarise to posterior means for the standings row.
  aggregate_played_match_expected(sex)
  team_expected <- read_csv(
    here("results", sex, "team_expected_by_draw.csv"),
    show_col_types = FALSE
  ) |>
    summarise(
      xg_for = mean(xg_for),
      xg_against = mean(xg_against),
      xpts = mean(xpts),
      .by = team
    )

  current_season <- max(d$season, na.rm = TRUE)

  current_top_teams <- d |>
    filter(season == current_season, division == 1) |>
    pivot_longer(c(home, away), values_to = "team") |>
    distinct(team)

  # ---- meta ---------------------------------------------------------------

  fit_mtime <- file.info(here("results", sex, "fit.rds"))$mtime

  # Current completed-round number in the top division. Min(played) across
  # teams, so postponed fixtures don't overstate progress. Used by the
  # website trajectory + rank-bump charts as the natural x-axis.
  round_num <- d |>
    filter(season == current_season, division == 1, !is.na(home_goals)) |>
    pivot_longer(c(home, away), values_to = "team") |>
    count(team) |>
    pull(n) |>
    (\(x) if (length(x) == 0L) 0L else min(x))()

  meta <- list(
    sex = sex,
    league = "Besta deild",
    season = current_season,
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    fit_date = format(fit_mtime, "%Y-%m-%d"),
    round = round_num,
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
    left_join(male_top_division_venues, by = c("home" = "team")) |>
    mutate(
      division_code = division_labels[division],
      date = format(date, "%Y-%m-%d")
    ) |>
    select(
      date, venue, division, division_code, home, away,
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
    digits = 5,
    na = "null"
  )

  # ---- standings.json (top division, current season) ---------------------

  if (sex == "male") {
    bd <- d |>
      filter(season == current_season, division == 1, !is.na(home_goals))

    long_bd <- bind_rows(
      bd |> transmute(team = home, date, gf = home_goals, ga = away_goals),
      bd |> transmute(team = away, date, gf = away_goals, ga = home_goals)
    ) |>
      mutate(
        result = case_when(
          gf > ga ~ "W",
          gf < ga ~ "L",
          TRUE ~ "D"
        )
      ) |>
      arrange(team, date)

    # Pad the form vector to length 5 with leading NA so jsonlite never
    # encounters a single-element list-column entry — auto_unbox would
    # otherwise collapse it from ["W"] to "W". Newest result stays last.
    pad_form <- function(x, n = 5) {
      tail(c(rep(NA_character_, n), x), n)
    }

    short_code <- function(team) {
      team |>
        str_remove_all("\\s|\\.") |>
        str_to_upper() |>
        str_sub(1, 3)
    }

    standings_rows <- long_bd |>
      summarise(
        played = n(),
        wins = sum(result == "W"),
        draws = sum(result == "D"),
        losses = sum(result == "L"),
        goals_for = sum(gf),
        goals_against = sum(ga),
        goal_diff = goals_for - goals_against,
        points = 3L * wins + draws,
        form = list(pad_form(tail(result, 5))),
        xg_trend = list(numeric(0)),
        .by = team
      ) |>
      arrange(desc(points), desc(goal_diff), desc(goals_for)) |>
      mutate(
        rank  = row_number(),
        short = short_code(team)
      ) |>
      left_join(team_expected, by = "team") |>
      select(
        team, short, played, wins, draws, losses,
        goals_for, goals_against, goal_diff, points,
        xg_for, xg_against, xpts,
        rank,
        form, xg_trend
      )

    write_json(
      list(
        generated_at = meta$generated_at,
        season       = current_season,
        as_of        = format(max(bd$date), "%Y-%m-%d"),
        rows         = standings_rows
      ),
      file.path(out_dir, "standings.json"),
      auto_unbox = TRUE,
      dataframe = "rows",
      digits = 5,
      na = "null"
    )

    # Append to history. One row per (team × run), dedup on (as_of, team)
    # so re-running the script against the same played fixtures doesn't
    # grow the history file. `form` and `xg_trend` are dropped — they live
    # in the snapshot only; the history is for trajectory/rank charts.
    standings_history_row <- standings_rows |>
      mutate(
        as_of        = format(max(bd$date), "%Y-%m-%d"),
        generated_at = meta$generated_at,
        round        = meta$round,
        season       = current_season
      ) |>
      select(
        as_of, generated_at, round, season,
        team, short, played, wins, draws, losses,
        goals_for, goals_against, goal_diff, points,
        xg_for, xg_against, xpts,
        rank
      )

    append_to_history(
      file.path(out_dir, "standings_history.json"),
      standings_history_row,
      key_cols = c("as_of", "team")
    )
  }

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

  # Append to history. One row per (team × component × location × coverage
  # × run). Dedup on fit_date so re-running against the same fit replaces
  # the earlier row rather than duplicating it.
  team_strengths_history_row <- team_strengths |>
    mutate(
      fit_date     = meta$fit_date,
      generated_at = meta$generated_at,
      round        = meta$round,
      season       = current_season
    ) |>
    select(
      fit_date, generated_at, round, season,
      team, component, location, coverage,
      median, lower, upper
    )

  append_to_history(
    file.path(out_dir, "team_strengths_history.json"),
    team_strengths_history_row,
    key_cols = c("fit_date", "team", "component", "location", "coverage")
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
