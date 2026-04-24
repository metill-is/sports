library(tidyverse)
library(here)

from_season <- 2021

results <- here(
  "data",
  "male",
  "div1.csv"
) |>
  read_csv(show_col_types = FALSE) |>
  mutate(
    division = 1
  ) |>
  bind_rows(
    here(
      "data",
      "male",
      "div1_lower_playoffs.csv"
    ) |>
      read_csv(show_col_types = FALSE) |>
      mutate(
        division = 1
      )
  ) |>
  bind_rows(
    here(
      "data",
      "male",
      "div1_upper_playoffs.csv"
    ) |>
      read_csv(show_col_types = FALSE) |>
      mutate(
        division = 1
      )
  ) |>
  bind_rows(
    here(
      "data",
      "male",
      "div2.csv"
    ) |>
      read_csv(show_col_types = FALSE) |>
      mutate(
        division = 2
      )
  ) |>
  bind_rows(
    here(
      "data",
      "male",
      "div3.csv"
    ) |>
      read_csv(show_col_types = FALSE) |>
      mutate(
        division = 3
      )
  ) |>
  bind_rows(
    here(
      "data",
      "male",
      "div4.csv"
    ) |>
      read_csv(show_col_types = FALSE) |>
      mutate(
        division = 4
      )
  ) |>
  bind_rows(
    here(
      "data",
      "male",
      "div5.csv"
    ) |>
      read_csv(show_col_types = FALSE) |>
      mutate(
        division = 5
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
  "male",
  "cup.csv"
) |>
  read_csv(show_col_types = FALSE) |>
  mutate(
    division = max(results$division) + 1
  ) |>
  bind_rows(
    here(
      "data",
      "male",
      "cup2.csv"
    ) |>
      read_csv(show_col_types = FALSE) |>
      mutate(
        division = max(results$division) + 2
      )
  ) |>
  filter(
    heima %in% teams,
    gestir %in% teams,
    timabil >= from_season
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
    here("data", "male", "data.csv")
  )

schedule_col_types <- cols(dags = col_date(), heima = col_character(), gestir = col_character())

schedule <- here(
  "data",
  "male",
  "schedule_div1.csv"
) |>
  read_csv(col_types = schedule_col_types) |>
  mutate(
    division = 1
  ) |>
  bind_rows(
    here(
      "data",
      "male",
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
      "male",
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
      "male",
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
      "male",
      "schedule_div3.csv"
    ) |>
      read_csv(col_types = schedule_col_types) |>
      mutate(
        division = 3
      )
  ) |>
  bind_rows(
    here(
      "data",
      "male",
      "schedule_div4.csv"
    ) |>
      read_csv(col_types = schedule_col_types) |>
      mutate(
        division = 4
      )
  ) |>
  bind_rows(
    here(
      "data",
      "male",
      "schedule_div5.csv"
    ) |>
      read_csv(col_types = schedule_col_types) |>
      mutate(
        division = 5
      )
  )

cup_schedule <- here(
  "data",
  "male",
  "schedule_cup.csv"
) |>
  read_csv(col_types = schedule_col_types) |>
  mutate(
    division = max(results$division) + 1
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
    here("data", "male", "schedule.csv")
  )
