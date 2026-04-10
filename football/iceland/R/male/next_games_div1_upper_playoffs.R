library(tidyverse)
library(here)
source(here("R", "utils", "scrape_ksi.R"))

scrape_ksi_schedule(ksi_ids$male$div1_upper_playoffs[["2026"]]) |>
  write_csv(
    here("data", "male", "schedule_div1_upper_playoffs.csv")
  )
