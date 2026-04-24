library(tidyverse)
library(metill)
library(geomtextpath)
library(cmdstanr)
library(posterior)
library(here)
library(ggtext)
theme_set(theme_metill())
end_date <- today()
country <- "denmark"
sex <- "male"

#### Data Prep ####
results <- read_rds(here("results", "denmark", sex, "fit.rds"))

d <- read_csv(here("results", "denmark", sex, "d.csv"))
teams <- read_csv(here("results", "denmark", sex, "teams.csv"))
next_games <- read_csv(here("results", "denmark", sex, "next_games.csv"))
top_teams <- read_csv(here("results", "denmark", sex, "top_teams.csv"))
pred_d <- read_csv(here("results", "denmark", sex, "pred_d.csv"))

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
    team %in% c(
      "Fredericia",
      "Aalborg",
      "Skjern",
      "Ringsted"
    )
  ) |>
  ggplot(aes(date, median)) +
  geom_hline(
    yintercept = 0,
    lty = 2,
    alpha = 0.3
  ) +
  geom_textline(
    aes(
      group = team,
      label = team,
      col = team,
      hjust = team
    ),
    linewidth = 1,
    size = 5,
    text_smoothing = 50
  ) +
  geom_richtext(
    data = tibble(x = 1),
    inherit.aes = FALSE,
    x = clock::date_build(2016, 1, 1),
    y = -1,
    label.colour = NA,
    fill = NA,
    label = "&larr; Undir meðaltali",
    hjust = 1,
    vjust = 0,
    angle = 90,
    size = 3.5,
    colour = "grey40"
  ) +
  geom_richtext(
    data = tibble(x = 1),
    inherit.aes = FALSE,
    x = clock::date_build(2016, 1, 1),
    y = 1,
    label.colour = NA,
    fill = NA,
    label = "Yfir meðaltali &rarr;",
    hjust = 0, 
    vjust = 0,
    angle = 90,
    size = 3.5,
    colour = "grey40"
  ) +
  scale_x_date(
    guide = guide_axis(cap = "both"),
    breaks = breaks_width("1 year"),
    labels = label_date_short(),
    limits = clock::date_build(c(2021, 2025), 11, 10)
  ) +
  scale_y_continuous(
    guide = guide_axis(cap = "both"),
    limits = 6 * c(0, 1)
  ) +
  # scale_colour_manual(
  #   values = c(
  #     "Ísland" = "#02529C",
  #     "Belgía" = "#2D2926",
  #     "Pólland" = "#DC143C"
  #   )
  # ) +
  # scale_hjust_manual(
  #   values = c(
  #     "Ísland" = 0.3,
  #     "Pólland" = 0.74
  #   )
  # ) +
  facet_wrap(
    vars(variable),
    ncol = 1
  ) +
  theme(
    legend.position = "none"
  ) +
  labs(
    title = "Þróun styrks nokkurra félagsliða í dönsku úrvalsdeild karla í handbolta",
    x = NULL,
    y = NULL,
    col = NULL,
    fill = NULL
  )

ggsave(
  filename = here("results",  "evolution.png"),
  width = 8,
  height = 0.8 * 8,
  scale = 1.2
)
