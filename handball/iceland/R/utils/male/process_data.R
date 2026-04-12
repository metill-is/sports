library(tidyverse)
library(here)
library(purrr)

d <- here("data", "male", c("historical_div1.csv", "historical_div2.csv")) |>
  map(read_csv) |>
  list_rbind() |>
  bind_rows(
    here("data", "male", c("current_div1.csv", "current_div2.csv")) |>
      map(read_csv) |>
      list_rbind()
  ) |>
  arrange(desc(dagsetning)) |>
  mutate(
    season = coalesce(season, 2025)
  )

teams <- d |>
  pivot_longer(c(home, away)) |>
  distinct(value) |>
  pull(value)

cup_results <- here("data", "male", "current_cup.csv") |>
  read_csv() |>
  filter(
    home %in% teams,
    away %in% teams
  ) |>
  mutate(
    season = 2025
  )

playoff_results_path <- here("data", "male", "current_playoffs.csv")
playoff_results <- if (file.exists(playoff_results_path)) {
  read_csv(playoff_results_path) |>
    mutate(season = 2025)
} else {
  tibble()
}

d |>
  bind_rows(
    cup_results,
    playoff_results
  ) |>
  write_csv(
    here("data", "male", "data.csv")
  )

cup_schedule <- here("data", "male", "schedule_cup.csv") |>
  read_csv() |>
  filter(
    home %in% teams,
    away %in% teams
  )

playoff_schedule_path <- here("data", "male", "schedule_playoffs.csv")
playoff_schedule <- if (file.exists(playoff_schedule_path)) {
  read_csv(playoff_schedule_path)
} else {
  tibble()
}

schedule_col_types <- cols(dagsetning = col_date(), home = col_character(), away = col_character(), division = col_double())

here("data", "male", c("schedule_div1.csv", "schedule_div2.csv")) |>
  map(\(f) read_csv(f, col_types = schedule_col_types)) |>
  list_rbind() |>
  bind_rows(
    cup_schedule,
    playoff_schedule
  ) |>
  write_csv(
    here("data", "male", "schedule.csv")
  )
