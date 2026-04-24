library(tidyverse)
library(here)

from_season <- 2021

results <- here(
  "data",
  "female",
  "div1.csv"
) |>
  read_csv() |>
  mutate(
    division = 1
  ) |>
  bind_rows(
    here(
      "data",
      "female",
      "div1_lower_playoffs.csv"
    ) |>
      read_csv() |>
      mutate(
        division = 1
      )
  ) |>
  bind_rows(
    here(
      "data",
      "female",
      "div1_upper_playoffs.csv"
    ) |>
      read_csv() |>
      mutate(
        division = 1
      )
  ) |>
  bind_rows(
    here(
      "data",
      "female",
      "div2.csv"
    ) |>
      read_csv() |>
      mutate(
        division = 2
      )
  ) |>
  bind_rows(
    here(
      "data",
      "female",
      "div3.csv"
    ) |>
      read_csv() |>
      mutate(
        division = 3
      )
  ) |>
  arrange(desc(dags)) |>
  filter(
    timabil >= from_season
  )

teams <- results |>
  pivot_longer(c(heima, gestir)) |>
  summarise(
    n = n(),
    first_game = min(dags),
    last_game = max(dags),
    .by = value
  ) |>
  filter(
    n > 10,
    year(last_game) >= 2024
  ) |>
  pull(value)

cup_games <- here(
  "data",
  "female",
  "cup.csv"
) |>
  read_csv() |>
  mutate(
    division = 4
  ) |>
  filter(
    heima %in% teams,
    gestir %in% teams,
    timabil >= from_season
  ) |>
  bind_rows(
    here(
      "data",
      "female",
      "cup2.csv"
    ) |>
      read_csv() |>
      mutate(
        division = 5
      )
  )

results |>
  bind_rows(
    cup_games
  ) |>
  mutate(
    finals = coalesce(finals, 0)
  ) |>
  arrange(desc(dags)) |>
  write_csv(
    here("data", "female", "data.csv")
  )

schedule_col_types <- cols(dags = col_date(), heima = col_character(), gestir = col_character())

schedule <- here(
  "data",
  "female",
  "schedule_div1.csv"
) |>
  read_csv(col_types = schedule_col_types) |>
  mutate(
    division = 1
  ) |>
  bind_rows(
    here(
      "data",
      "female",
      "schedule_div1_upper_playoffs.csv"
    ) |>
      read_csv(col_types = schedule_col_types) |>
      mutate(
        division = 1
      )
  ) |>
  bind_rows(
    here(
      "data",
      "female",
      "schedule_div1_lower_playoffs.csv"
    ) |>
      read_csv(col_types = schedule_col_types) |>
      mutate(
        division = 1
      )
  ) |>
  bind_rows(
    here(
      "data",
      "female",
      "schedule_div2.csv"
    ) |>
      read_csv(col_types = schedule_col_types) |>
      mutate(
        division = 2
      )
  ) |>
  bind_rows(
    here(
      "data",
      "female",
      "schedule_div3.csv"
    ) |>
      read_csv(col_types = schedule_col_types) |>
      mutate(
        division = 3
      )
  )

cup_schedule <- here(
  "data",
  "female",
  "schedule_cup.csv"
) |>
  read_csv(col_types = schedule_col_types) |>
  mutate(
    division = 4
  ) |>
  filter(
    heima %in% teams,
    gestir %in% teams
  )

schedule |>
  bind_rows(
    cup_schedule
  ) |>
  arrange(desc(dags)) |>
  write_csv(
    here("data", "female", "schedule.csv")
  )
