#!/usr/bin/env Rscript
# ETL: _legacy/sports/*/*/history/bets_log.csv -> data/decisions/ledger/*.parquet
# Covers all 13 historical ledgers (3 active Icelandic + 10 paused non-Icelandic).
#
# Notes on the legacy data:
#   - `date_recommended` has 67 NA values across all files (mostly btts rows in
#     football/england that were logged without a recommendation timestamp).
#   - `btts` (both-teams-to-score) appears only in football/{england,iceland}
#     (11 rows total). It is not in the canonical 3-market vocabulary, but
#     dropping it would distort the PnL total. We pass it through verbatim
#     because the ledger schema does not constrain `market` to an enum.
#   - All handicap `info` values are plain signed numbers (e.g. "-1.5", "0.25")
#     — no Lengjan "H-A" score-style strings ever reached the legacy ledger.
#   - No `sex=="all"` rows exist in the legacy CSVs (that hack was only ever
#     written to the newer Parquet ledger during settlement). The filter is
#     therefore a safety no-op.

suppressPackageStartupMessages(devtools::load_all(here::here()))
library(dplyr)

market_map <- c(
  outcome   = "moneyline",
  handicap  = "spread",
  totals    = "total",
  moneyline = "moneyline",
  spread    = "spread",
  total     = "total",
  btts      = "btts"
)

parse_line <- function(info, market) {
  # `info` is free text; handle the three known markets. moneyline and btts
  # have no line (NA). totals are plain positive numbers. spread in the legacy
  # ledger is always a plain signed number, but keep the "H-A" fallback for
  # safety in case an old row slipped through in that form.
  info <- as.character(info)
  is_ml <- market == "moneyline"
  is_btts <- market == "btts"
  is_na <- is.na(info) | info == "" | info == "NA"

  result <- rep(NA_real_, length(info))
  for (i in seq_along(info)) {
    if (is_ml[i] || is_btts[i] || is_na[i]) next
    if (market[i] == "total") {
      result[i] <- suppressWarnings(as.numeric(info[i]))
    } else if (market[i] == "spread") {
      s <- info[i]
      if (grepl("^-?\\d+(\\.\\d+)?$", s)) {
        result[i] <- as.numeric(s)
      } else if (grepl("^\\d+-\\d+$", s)) {
        parts <- strsplit(s, "-", fixed = TRUE)[[1]]
        result[i] <- as.numeric(parts[1]) - as.numeric(parts[2])
      }
      # else leave NA
    }
  }
  result
}

etl_ledger <- function(csv) {
  # Path layout: <root>/_legacy/sports/{sport}/{country}/history/bets_log.csv
  # Note: when <root> is itself a directory called "sports" (true on this
  # workstation), which() finds two matches — pick the last one so the
  # `_legacy/sports/` anchor is used.
  parts <- strsplit(csv, "/", fixed = TRUE)[[1]]
  idx <- tail(which(parts == "sports"), 1)
  sport_from_path <- parts[idx + 1]
  country_from_path <- parts[idx + 2]
  key <- paste(sport_from_path, country_from_path, sep = "/")
  cli::cli_h2(key)

  raw <- readr::read_csv(
    csv,
    show_col_types = FALSE,
    locale = readr::locale(encoding = "UTF-8")
  )

  market_canonical <- unname(market_map[as.character(raw$market)])
  if (any(is.na(market_canonical))) {
    bad <- unique(raw$market[is.na(market_canonical)])
    cli::cli_alert_warning("Unknown market value(s) dropped: {.val {bad}}")
  }

  canonical <- raw |>
    mutate(
      placed_at = as.POSIXct(
        as.character(date_recommended),
        tz = "UTC",
        tryFormats = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d")
      ),
      match_date = as.Date(date_match),
      sport = as.character(sport),
      country = as.character(country),
      sex = as.character(sex),
      home_team = as.character(home),
      away_team = as.character(away),
      market = market_canonical,
      outcome = as.character(outcome),
      line = parse_line(info, market_canonical),
      odds_placed = as.numeric(odds),
      p = as.numeric(probability),
      kelly = as.numeric(kelly_frac),
      bet_amount = as.numeric(bet_amount),
      settled = !is.na(win),
      win = suppressWarnings(as.logical(win)),
      pnl = as.numeric(pnl)
    ) |>
    filter(sex != "all") |>
    filter(!is.na(market)) |>
    select(
      placed_at, match_date, sport, country, sex,
      home_team, away_team, market, outcome, line,
      odds_placed, p, kelly, bet_amount,
      settled, win, pnl
    )

  write_table(canonical, "ledger")
  cli::cli_alert_success(
    "{key}: {nrow(canonical)} ledger rows written, pnl={round(sum(canonical$pnl, na.rm=TRUE), 2)}"
  )
  canonical
}

files <- list.files(
  here::here("_legacy", "sports"),
  pattern = "bets_log\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)
cli::cli_alert_info("Found {length(files)} ledger file{?s}")

all <- lapply(files, etl_ledger) |> dplyr::bind_rows()
cli::cli_alert_success(
  "Total ledger rows written: {nrow(all)}, pnl={round(sum(all$pnl, na.rm=TRUE), 2)}"
)
