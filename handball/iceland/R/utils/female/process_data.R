library(tidyverse)
library(here)
library(purrr)

d <- here("data", "female", c("historical_div1.csv", "historical_div2.csv")) |>
  map(read_csv) |>
  list_rbind() |>
  bind_rows(
    here("data", "female", c("current_div1.csv", "current_div2.csv")) |>
      map(read_csv) |>
      list_rbind()
  ) |>
  arrange(desc(dagsetning)) |>
  mutate(
    season = coalesce(season, 2025)
  )


d |>
  write_csv(
    here("data", "female", "data.csv")
  )


here("data", "female", c("schedule_div1.csv", "schedule_div2.csv")) |>
  map(read_csv) |>
  list_rbind() |>
  write_csv(
    here("data", "female", "schedule.csv")
  )
