library(tidyverse)
library(here)
source(here("R", "utils", "scrape_ksi.R"))

d <- scrape_ksi_results(ksi_ids$male$cup)

if (nrow(d) > 0) {
  d <- d |>
    drop_na() |>
    mutate(
      round = row_number(),
      .by = timabil
    ) |>
    mutate(
      finals = 1 * (round == max(round)),
      .by = timabil
    ) |>
    select(-round)
}

read_csv(
  here("data", "male", "cup.csv")
) |>
  filter(
    timabil < min(d$timabil)
  ) |>
  bind_rows(d) |>
  arrange(
    timabil,
    desc(dags)
  ) |>
  write_csv(
    here("data", "male", "cup.csv")
  )
