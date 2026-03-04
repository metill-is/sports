library(tidyverse)
library(metill)
library(geomtextpath)
library(cmdstanr)
library(posterior)
library(here)
library(ggtext)
theme_set(theme_metill())
end_date <- today() - 1
sex <- "male"

#### Data Prep ####
results <- read_rds(here("results", sex, end_date, "fit.rds"))

d <- read_csv(here("results", sex, end_date, "d.csv"))
teams <- read_csv(here("results", sex, end_date, "teams.csv"))
next_games <- read_csv(here("results", sex, end_date, "next_games.csv"))
top_teams <- read_csv(here("results", sex, end_date, "top_teams.csv"))
pred_d <- read_csv(here("results", sex, end_date, "pred_d.csv"))

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
    team %in%
      c(
        "Iceland",
        "Denmark"
      )
  ) |>
  mutate(
    team = case_when(
      team == "Iceland" ~ "Ísland",
      team == "Belgium" ~ "Belgía",
      team == "Poland" ~ "Pólland",
      team == "Italy" ~ "Ítalía",
      team == "Hungary" ~ "Ungverjaland",
      team == "Croatia" ~ "Króatía",
      team == "Sweden" ~ "Svíþjóð",
      team == "Switzerland" ~ "Sviss",
      team == "Slovenia" ~ "Slóvenía",
      team == "Denmark" ~ "Danmörk",
      TRUE ~ team
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
    size = 5
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
    breaks = breaks_width("2 year", offset = "1 year"),
    labels = label_date_short(),
    limits = clock::date_build(c(2016, 2026), 3, 1)
  ) +
  scale_y_continuous(
    guide = guide_axis(cap = "both"),
    limits = 10 * c(-1, 1)
  ) +
  scale_colour_manual(
    values = c(
      "Belgía" = "#2D2926",
      "Danmörk" = "#C8102E",
      "Ísland" = "#02529C",
      "Ítalía" = "#008C45",
      "Króatía" = "#ff0000",
      "Pólland" = "#DC143C",
      "Slóvenía" = "#FF0000",
      "Sviss" = "#DA291C",
      "Svíþjóð" = "#fd8d3c",
      "Ungverjaland" = "#477050"
      
    )
  ) +
  scale_hjust_manual(
    values = c(
      "Belgía" = 0.5,
      "Danmörk" = 0.5,
      "Ísland" = 0.15,
      "Ítalía" = 0.5,
      "Króatía" = 0.25,
      "Pólland" = 0.74,
      "Slóvenía" = 0.2,
      "Sviss" = 0.5,
      "Svíþjóð" = 0.3,
      "Ungverjaland" = 0.15
    )
  ) +
  facet_wrap(
    vars(variable),
    ncol = 1
  ) +
  theme(
    legend.position = "none"
  ) +
  labs(
    title = "Þróun styrks landsliða Íslands og Danmerkur í Handbolta karla",
    subtitle = "Bæði liðin hafa bætt sig undanfarinn áratug",
    x = NULL,
    y = "Samanburður við lönd sem hafa spilað á EM eða HM",
    col = NULL,
    fill = NULL
  )

ggsave(
  filename = here("results", "male", "evolution_iceland_denmark.png"),
  width = 8,
  height = 0.8 * 8,
  scale = 1.2
)
