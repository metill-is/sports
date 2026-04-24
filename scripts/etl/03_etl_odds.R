#!/usr/bin/env Rscript
# ETL: _legacy/lengjan-odds/data/{sport_key}/*.csv -> data/facts/odds/

suppressPackageStartupMessages(devtools::load_all(here::here()))
library(dplyr)
library(tidyr)

sport_keys <- list(
  basketball_iceland = list(sport = "basketball", country = "iceland"),
  handball_iceland   = list(sport = "handball", country = "iceland"),
  football_iceland   = list(sport = "football", country = "iceland")
)

pivot_1x2 <- function(df, meta) {
  df |>
    tidyr::pivot_longer(
      cols = c("o_home", "o_draw", "o_away"),
      names_to = "outcome",
      values_to = "odds"
    ) |>
    mutate(
      sport      = meta$sport,
      country    = meta$country,
      market     = "moneyline",
      outcome    = sub("^o_", "", outcome),
      line       = NA_real_,
      scraped_at = as.POSIXct(scraped_at, tz = "UTC"),
      match_date = as.Date(date),
      home_team  = as.character(home),
      away_team  = as.character(away),
      odds       = as.numeric(odds)
    ) |>
    filter(!is.na(odds)) |>
    select(
      sport, country, scraped_at, match_date, home_team, away_team,
      market, outcome, line, odds
    )
}

#' Lengjan encodes handicaps as "H-A" score strings (e.g. "0-1" = away starts
#' +1, "2-0" = home starts +2). Convert to a signed scalar from the home side:
#' H - A.
parse_handicap <- function(x) {
  parts <- strsplit(as.character(x), "-", fixed = TRUE)
  vapply(parts, function(p) {
    if (length(p) != 2L) {
      return(NA_real_)
    }
    h <- suppressWarnings(as.numeric(p[1]))
    a <- suppressWarnings(as.numeric(p[2]))
    if (is.na(h) || is.na(a)) {
      return(NA_real_)
    }
    h - a
  }, numeric(1))
}

pivot_handicap <- function(df, meta) {
  df |>
    tidyr::pivot_longer(
      cols = c("o_home", "o_draw", "o_away"),
      names_to = "outcome",
      values_to = "odds"
    ) |>
    mutate(
      sport      = meta$sport,
      country    = meta$country,
      market     = "spread",
      outcome    = sub("^o_", "", outcome),
      line       = parse_handicap(change),
      scraped_at = as.POSIXct(scraped_at, tz = "UTC"),
      match_date = as.Date(date),
      home_team  = as.character(home),
      away_team  = as.character(away),
      odds       = as.numeric(odds)
    ) |>
    filter(!is.na(odds)) |>
    select(
      sport, country, scraped_at, match_date, home_team, away_team,
      market, outcome, line, odds
    )
}

pivot_totals <- function(df, meta) {
  df |>
    tidyr::pivot_longer(
      cols = c("o_over", "o_under"),
      names_to = "outcome",
      values_to = "odds"
    ) |>
    mutate(
      sport      = meta$sport,
      country    = meta$country,
      market     = "total",
      outcome    = sub("^o_", "", outcome),
      line       = as.numeric(limit),
      scraped_at = as.POSIXct(scraped_at, tz = "UTC"),
      match_date = as.Date(date),
      home_team  = as.character(home),
      away_team  = as.character(away),
      odds       = as.numeric(odds)
    ) |>
    filter(!is.na(odds)) |>
    select(
      sport, country, scraped_at, match_date, home_team, away_team,
      market, outcome, line, odds
    )
}

etl_sport_key <- function(sport_key, meta) {
  cli::cli_h2(sport_key)
  dir <- here::here("_legacy", "lengjan-odds", "data", sport_key)
  if (!fs::dir_exists(dir)) {
    cli::cli_alert_warning("Skip {.path {dir}} (no scraped data)")
    return(invisible())
  }

  chunks <- list()
  f1 <- fs::path(dir, "odds_1x2.csv")
  if (file.exists(f1)) {
    chunks$m <- pivot_1x2(readr::read_csv(f1, show_col_types = FALSE), meta)
  }

  f2 <- fs::path(dir, "odds_handicap.csv")
  if (file.exists(f2)) {
    chunks$s <- pivot_handicap(readr::read_csv(f2, show_col_types = FALSE), meta)
  }

  f3 <- fs::path(dir, "odds_totals.csv")
  if (file.exists(f3)) {
    chunks$t <- pivot_totals(readr::read_csv(f3, show_col_types = FALSE), meta)
  }

  all <- dplyr::bind_rows(chunks)
  if (nrow(all) == 0) {
    cli::cli_alert_warning("{sport_key}: no rows after pivot")
    return(invisible())
  }

  write_table(all, "odds")
  cli::cli_alert_success("{sport_key}: {nrow(all)} odds rows written")
}

for (k in names(sport_keys)) etl_sport_key(k, sport_keys[[k]])
cli::cli_alert_success("Done.")
