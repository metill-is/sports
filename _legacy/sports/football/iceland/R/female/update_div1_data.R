library(tidyverse)
library(here)
source(here("R", "utils", "scrape_ksi.R"))

d <- scrape_ksi_results(ksi_ids$female$div1)

read_csv(
  here("data", "female", "div1.csv")
) |>
  filter(
    timabil < min(d$timabil)
  ) |>
  bind_rows(
    d |> drop_na()
  ) |>
  arrange(desc(dags)) |>
  write_csv(
    here("data", "female", "div1.csv")
  )
