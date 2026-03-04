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

sex <- "male"

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
  # filter(
  #   team %in%
  #     c(
  #       "KR",
  #       # "Víkingur R.",
  #       "Vestri"
  #     )
  # ) |>
  inner_join(
    d |>
      # filter(division == 1) |>
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
  filter(
    season >= 2021,
    team %in% c(
      "FH", "Fram",
      "Vestri", "ÍBV",
      "KA", "KR"
      )
  ) |>
  ggplot(aes(date, median)) +
  geom_hline(
    yintercept = 0,
    lty = 2,
    alpha = 0.3
  ) +
  geom_line(
    data = ~ rename(.x, tm = team),
    aes(
      group = paste(tm)
    ),
    col = "grey50",
    alpha = 0.2,
    linewidth = 1
  ) +
  geom_line(
    aes(
      group = paste(team)
    ),
    linewidth = 1
  ) +
  scale_x_date(
    guide = guide_axis(cap = "both"),
    breaks = breaks_pretty(),
    labels = label_date_short()
  ) +
  scale_y_continuous(
    guide = guide_axis(cap = "both")
  ) +
  # scale_colour_manual(
  #   values = c(
  #     "Víkingur R." = "#b30000",
  #     "Breiðablik" = "#006d2c",
  #     "Valur" = "#ce1256",
  #     "KR" = "black",
  #     "Stjarnan" = "#08519c",
  #     "ÍA" = "#fec44f",
  #     "KA" = "#9ecae1",
  #     "ÍBV" = "black",
  #     "Fram" = "#4292c6",
  #     "FH" = "#525252",
  #     "Vestri" = "#08306b",
  #     "Afturelding" = "#e31a1c"
  #   )
  # ) +
  facet_grid(
    rows = vars(variable),
    cols = vars(team)
  ) +
  theme(
    legend.position = "none"
  ) +
  labs(
    title = "Þróun styrks nokkurra félagsliða í Bestu Deild karla",
    x = NULL,
    y = NULL,
    col = NULL,
    fill = NULL
  )

ggsave(
  filename = "results/male/figures/evolution_male.png",
  width = 8,
  height = 0.5 * 8,
  scale = 1.4
)
