#' Convert GSheets Bets tabs to bets_log.csv format
#'
#' Reads downloaded bets CSVs from data/gsheets/{sport}/ and converts them
#' to the standardised bets_log.csv schema used by the betting pipeline.
#'
#' Run after sync_gsheets.R:
#'   Rscript R/sync_gsheets.R
#'   Rscript R/convert_bets_history.R
#'
#' For each sport, merges historical bets with any existing bets_log.csv,
#' deduplicates, and writes to {sport_dir}/history/bets_log.csv.

Sys.setlocale("LC_ALL", "is_IS.UTF-8")

library(readr)
library(dplyr)
library(here)

# --- Column mappings ---
# GSheets Bets tab columns (Icelandic):
#   dags_bet, dags_leikur, booker, type, deild, sex, heima, gestir,
#   bet, info, amount, odds, prob, payout, ev, win, change

# Market type mapping: Icelandic → English
market_map <- c(
  "Niðurstaða"   = "outcome",
  "Niourstaoa"   = "outcome",   # ASCII fallback
  "Outcome"      = "outcome",
  "Forgjöf"      = "handicap",
  "Forgjof"      = "handicap",  # ASCII fallback
  "Handicap"     = "handicap",
  "Markafjöldi"  = "totals",
  "Markafjoeldi" = "totals",    # ASCII fallback
  "Total Goals"  = "totals",
  "Mörk"         = "totals"
)

# Bet outcome mapping: Icelandic → English
outcome_map <- c(
  "heima"     = "home",
  "gestir"    = "away",
  "jafntefli" = "draw",
  "yfir"      = "over",
  "undir"     = "under",
  # English passthrough
  "home"      = "home",
  "away"      = "away",
  "draw"      = "draw",
  "over"      = "over",
  "under"     = "under"
)

#' Convert a single bets CSV to bets_log format
convert_bets <- function(path, sport, country, source_label) {
  if (!file.exists(path)) {
    cat("  SKIP", path, "(not found)\n")
    return(NULL)
  }

  d <- read_csv(path, show_col_types = FALSE)
  if (nrow(d) == 0) return(NULL)

  # Map market type
  if ("type" %in% names(d)) {
    d$market <- market_map[d$type]
  } else if ("tegund" %in% names(d)) {
    d$market <- market_map[d$tegund]
  } else {
    d$market <- NA_character_
  }

  # Map bet outcome
  if ("bet" %in% names(d)) {
    d$outcome <- outcome_map[d$bet]
  } else {
    d$outcome <- NA_character_
  }

  # Compute P&L: win * payout - amount
  if ("win" %in% names(d) && "payout" %in% names(d) && "amount" %in% names(d)) {
    d$pnl <- ifelse(
      is.na(d$win), NA_real_,
      d$win * d$payout - d$amount
    )
    d$win_bool <- ifelse(is.na(d$win), NA, as.logical(d$win))
  } else if ("change" %in% names(d) && "amount" %in% names(d)) {
    # Fallback: change = pnl
    d$pnl <- d$change
    d$win_bool <- ifelse(is.na(d$change), NA, d$change > 0)
  } else {
    d$pnl <- NA_real_
    d$win_bool <- NA
  }

  # Build output tibble
  result <- tibble(
    date_recommended = if ("dags_bet" %in% names(d)) as.Date(d$dags_bet) else NA_Date_,
    date_match       = if ("dags_leikur" %in% names(d)) as.Date(d$dags_leikur) else NA_Date_,
    sport            = sport,
    country          = country,
    sex              = if ("sex" %in% names(d)) as.character(d$sex) else NA_character_,
    market           = d$market,
    home             = if ("heima" %in% names(d)) d$heima else NA_character_,
    away             = if ("gestir" %in% names(d)) d$gestir else NA_character_,
    outcome          = d$outcome,
    odds             = if ("odds" %in% names(d)) as.numeric(d$odds) else NA_real_,
    probability      = if ("prob" %in% names(d)) as.numeric(d$prob) else NA_real_,
    ev               = if ("ev" %in% names(d)) as.numeric(d$ev) else NA_real_,
    kelly_frac       = NA_real_,
    bet_amount       = if ("amount" %in% names(d)) as.numeric(d$amount) else NA_real_,
    info             = if ("info" %in% names(d)) as.character(d$info) else NA_character_,
    win              = d$win_bool,
    pnl              = d$pnl,
    source           = source_label
  )

  result |> filter(!is.na(date_match))
}

# --- Sport registry ---
# Each entry: gsheets_dir, sport, country, bets files, output sport_dir
sports <- list(
  list(
    gsheets_dir = "basketball",
    sport = "basketball", country = "iceland",
    bets_files = list(
      list(file = "bets.csv", label = "gsheets")
    ),
    sport_dir = here("basketball", "iceland")
  ),
  list(
    gsheets_dir = "basketball_international",
    sport = "basketball", country = "international",
    bets_files = list(
      list(file = "bets.csv", label = "gsheets")
    ),
    sport_dir = here("basketball", "international")
  ),
  list(
    gsheets_dir = "handball",
    sport = "handball", country = "iceland",
    bets_files = list(
      list(file = "bets.csv", label = "gsheets"),
      list(file = "bets_lengjan.csv", label = "gsheets_lengjan")
    ),
    sport_dir = here("handball", "iceland")
  ),
  list(
    gsheets_dir = "handball_international",
    sport = "handball", country = "international",
    bets_files = list(
      list(file = "bets.csv", label = "gsheets"),
      list(file = "bets_lengjan.csv", label = "gsheets_lengjan")
    ),
    sport_dir = here("handball", "international")
  ),
  list(
    gsheets_dir = "football",
    sport = "football", country = "iceland",
    bets_files = list(
      list(file = "bets.csv", label = "gsheets"),
      list(file = "bets_lengjan.csv", label = "gsheets_lengjan")
    ),
    sport_dir = here("football", "iceland")
  ),
  list(
    gsheets_dir = "football_england",
    sport = "football", country = "england",
    bets_files = list(
      list(file = "bets.csv", label = "gsheets"),
      list(file = "bets_lengjan.csv", label = "gsheets_lengjan")
    ),
    sport_dir = here("football", "england")
  )
)

# --- Convert and merge ---
for (sp in sports) {
  cat("\n===", sp$sport, sp$country, "===\n")
  gsheets_dir <- here("data", "gsheets", sp$gsheets_dir)

  # Convert each bets file
  converted <- lapply(sp$bets_files, function(bf) {
    convert_bets(
      path = file.path(gsheets_dir, bf$file),
      sport = sp$sport,
      country = sp$country,
      source_label = bf$label
    )
  }) |> bind_rows()

  if (is.null(converted) || nrow(converted) == 0) {
    cat("  No historical bets found.\n")
    next
  }

  cat("  Converted", nrow(converted), "historical bets\n")

  # Load existing bets_log.csv (from pipeline runs)
  history_dir <- file.path(sp$sport_dir, "history")
  log_path <- file.path(history_dir, "bets_log.csv")
  if (file.exists(log_path)) {
    existing <- read_csv(log_path, show_col_types = FALSE)
    # Add missing columns to existing
    if (!"win" %in% names(existing)) existing$win <- NA
    if (!"pnl" %in% names(existing)) existing$pnl <- NA_real_
    if (!"source" %in% names(existing)) existing$source <- "pipeline"
    if ("info" %in% names(existing)) existing$info <- as.character(existing$info)
    cat("  Found", nrow(existing), "existing pipeline bets\n")
  } else {
    existing <- NULL
  }

  # Merge and deduplicate
  merged <- bind_rows(converted, existing) |>
    distinct(date_match, sport, country, market, home, away, outcome, .keep_all = TRUE) |>
    arrange(date_match, home, away)

  # Write merged output
  if (!dir.exists(history_dir)) dir.create(history_dir, recursive = TRUE)
  write_csv(merged, log_path)
  cat("  Wrote", nrow(merged), "bets to", log_path, "\n")
}

cat("\nDone.\n")
