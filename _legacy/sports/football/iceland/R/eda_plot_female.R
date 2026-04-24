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

sex <- "female"

from_season <- 2021

#### Data Prep ####
results <- read_rds(here("results", sex, "fit.rds"))

d <- read_csv(here("results", sex, "d.csv"))
teams <- read_csv(here("results", sex, "teams.csv"))
next_games <- read_csv(here("results", sex, "next_games.csv"))
top_teams <- read_csv(here("results", sex, "top_teams.csv"))
pred_d <- read_csv(here("results", sex, "pred_d.csv"))
model_d <- read_csv(here("results", sex, "model_d.csv"))

offense <- results$summary("offense")
defense <- results$summary("defense")

plot_dat <- offense |>
  mutate(
    round = str_match(variable, "\\[([0-9]+)")[, 2] |> as.numeric(),
    team_nr = str_match(variable, "([0-9]+)\\]")[, 2] |> as.numeric()
  ) |>
  select(round, team_nr, median, q5, q95) |>
  inner_join(
    teams
  ) |>
  mutate(
    variable = "Sóknarstyrkur"
  ) |>
  bind_rows(
    defense |>
      mutate(
        round = str_match(variable, "\\[([0-9]+)")[, 2] |> as.numeric(),
        team_nr = str_match(variable, "([0-9]+)\\]")[, 2] |> as.numeric()
      ) |>
      select(round, team_nr, median, q5, q95) |>
      inner_join(
        teams
      ) |>
      mutate(
        variable = "Varnarstyrkur"
      )
  )

plot_dat |>
  filter(
    team %in%
      c(
        "Fram",
        "Breiðablik",
        "Þróttur R.",
        "FHL"
      )
  ) |>
  inner_join(
    d |>
      pivot_longer(c(home, away)) |>
      select(
        season,
        game_nr,
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
      select(
        season,
        round,
        team = value,
        date
      )
  ) |>
  ggplot(aes(date, median)) +
  geom_hline(
    yintercept = 0,
    lty = 2,
    alpha = 0.3
  ) +
  geom_ribbon(
    aes(ymin = q5, ymax = q95, fill = team, group = paste(team, season)),
    alpha = 0.1
  ) +
  geom_line(
    aes(
      col = team,
      group = paste(team, season)
    ),
    linewidth = 1
  ) +
  scale_x_date(
    guide = guide_axis(cap = "both"),
    breaks = breaks_width("4 month"),
    labels = label_date_short()
  ) +
  scale_y_continuous(
    guide = guide_axis(cap = "both")
  ) +
  scale_colour_brewer(
    palette = "Set1"
  ) +
  scale_fill_brewer(
    palette = "Set1"
  ) +
  facet_wrap("variable", ncol = 1) +
  labs(
    title = "Þróun styrks nokkurra félagsliða",
    x = NULL,
    y = NULL,
    col = NULL,
    fill = NULL
  )


results$summary("sigma_off")


model_d |>
  select(
    date,
    game_nr,
    team_home = home,
    team_away = away,
    round_home = home_round,
    round_away = away_round
  ) |>
  pivot_longer(
    cols = !c(date, game_nr),
    names_to = c(".value", "type"),
    names_sep = "_"
  ) |>
  mutate(
    opponent = team[((row_number()) %% 2) + 1],
    .by = game_nr
  ) |>
  inner_join(
    plot_dat |>
      select(round, team, median, variable) |>
      pivot_wider(names_from = variable, values_from = median) |>
      rename(
        offense = Sóknarstyrkur,
        defense = Varnarstyrkur
      ) |>
      mutate(
        strength = offense + defense
      ),
    by = join_by(round, opponent == team)
  ) |>
  inner_join(
    model_d |>
      select(game_nr, division, season)
  ) |>
  filter(
    season == 2025,
    division == 1
  ) |>
  arrange(date) |>
  # filter(
  #   team %in% c(
  #     "Víkingur R.",
  #     "Vestri",
  #     "Breiðablik",
  #     "Valur",
  #     "KR"
  #   )
  # ) |>
  summarise(
    mean = mean(strength),
    .by = team
  ) |>
  mutate(
    team = fct_reorder(team, mean)
  ) |>
  ggplot(aes(mean, team)) +
  geom_segment(
    aes(xend = 0, yend = team)
  ) +
  geom_point() +
  scale_x_continuous(
    guide = guide_axis(cap = "both")
  ) +
  scale_y_discrete(
    guide = guide_axis(cap = "both")
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Meðalstyrkur mótherja hingað til á tímabilinu"
  )
