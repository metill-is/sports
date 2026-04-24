library(tidyverse)
library(here)
source(here("R", "utils", "scrape_ksi.R"))

scrape_ksi_schedule(ksi_ids$female$div1[["2026"]]) |>
  write_csv(
    here("data", "female", "schedule_div1.csv")
  )
