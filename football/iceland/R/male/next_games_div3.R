library(tidyverse)
library(here)
source(here("R", "utils", "scrape_ksi.R"))

scrape_ksi_schedule(ksi_ids$male$div3[["2026"]]) |>
  write_csv(
    here("data", "male", "schedule_div3.csv")
  )
