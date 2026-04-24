library(tidyverse)
library(here)
source(here("R", "utils", "scrape_ksi.R"))

d <- scrape_ksi_results(ksi_ids$male$div4)

read_csv(
  here("data", "male", "div4.csv")
) |>
  filter(
    timabil < min(d$timabil)
  ) |>
  bind_rows(
    d |> drop_na()
  ) |>
  arrange(desc(dags)) |>
  write_csv(
    here("data", "male", "div4.csv")
  )
