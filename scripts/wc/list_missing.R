#!/usr/bin/env Rscript
# scripts/wc/list_missing.R --
# Print the WC fixtures that should have kicked off by today but martj42 hasn't
# scored yet, as paste-ready rows for data/wc/manual_results.csv. Read-only;
# downloads the current martj42 CSV but writes nothing to the facts store.
#
# Usage:
#   Rscript scripts/wc/list_missing.R

invisible(Sys.setlocale("LC_ALL", "en_US.UTF-8"))
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))

martj42_url <- paste0(
  "https://raw.githubusercontent.com/",
  "martj42/international_results/master/results.csv"
)
csv_path <- here::here("data", "wc", "raw", "results.csv")
dir.create(dirname(csv_path), recursive = TRUE, showWarnings = FALSE)
utils::download.file(martj42_url, csv_path, mode = "wb", quiet = TRUE)

raw <- readr::read_csv(csv_path, col_types = readr::cols(
  date = readr::col_date(),
  home_team = readr::col_character(),
  away_team = readr::col_character(),
  home_score = readr::col_integer(),
  away_score = readr::col_integer(),
  tournament = readr::col_character(),
  city = readr::col_character(),
  country = readr::col_character(),
  neutral = readr::col_logical()
))

# Same official-calendar date correction ingest applies, so the scaffold rows
# key on the dates wc_apply_manual_results() will see.
raw <- wc_correct_knockout_dates(raw)

missing <- wc_list_unscored_fixtures(raw, as_of = Sys.Date())

if (nrow(missing) == 0L) {
  cli::cli_alert_success("No unscored WC fixtures past kickoff — martj42 is current.")
} else {
  cli::cli_alert_info(
    "{nrow(missing)} WC fixture{?s} played but unscored in martj42. \\
    Paste into data/wc/manual_results.csv and fill the scores:"
  )
  out <- missing
  out$home_score <- ""
  out$away_score <- ""
  cat(readr::format_csv(out, col_names = FALSE))
}
