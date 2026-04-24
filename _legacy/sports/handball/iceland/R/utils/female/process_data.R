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


playoff_results_path <- here("data", "female", "current_playoffs.csv")
playoff_results <- if (file.exists(playoff_results_path)) {
  read_csv(playoff_results_path) |>
    mutate(season = 2025)
} else {
  tibble()
}

d |>
  bind_rows(
    playoff_results
  ) |>
  write_csv(
    here("data", "female", "data.csv")
  )

playoff_schedule_path <- here("data", "female", "schedule_playoffs.csv")
playoff_schedule <- if (file.exists(playoff_schedule_path)) {
  read_csv(playoff_schedule_path)
} else {
  tibble()
}

schedule_col_types <- cols(dagsetning = col_date(), home = col_character(), away = col_character(), division = col_double())

here("data", "female", c("schedule_div1.csv", "schedule_div2.csv")) |>
  map(\(f) read_csv(f, col_types = schedule_col_types)) |>
  list_rbind() |>
  bind_rows(
    playoff_schedule
  ) |>
  write_csv(
    here("data", "female", "schedule.csv")
  )
