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


d |>
  bind_rows(
    cup_results
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


here("data", "male", c("schedule_div1.csv", "schedule_div2.csv")) |>
  map(read_csv) |>
  list_rbind() |>
  bind_rows(
    cup_schedule
  ) |>
  write_csv(
    here("data", "male", "schedule.csv")
  )
