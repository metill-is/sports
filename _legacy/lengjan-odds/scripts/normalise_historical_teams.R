#!/usr/bin/env Rscript
#
# normalise_historical_teams.R
#
# Re-apply the current team_names_*.csv mappings across every row of every
# archived odds CSV, then re-dedup using the live keep-latest rule.
#
# WHY THIS EXISTS
# ---------------
# When a new (Lengjan name -> standardised name) mapping is added to
# config/team_names_*.csv, only rows scraped after that point benefit — the
# mapping is applied at scrape time by standardise_teams() inside
# scrape_sport(). Rows written before the mapping existed keep their raw
# Lengjan name. This creates phantom duplicates: e.g. "Þór" (old scrape)
# and "Þór Akureyri" (post-mapping) show up as two distinct matches in
# data/handball_iceland/odds_1x2.csv, poisoning every downstream join.
#
# This script walks every sport_key directory, applies the current mapping
# to the archived CSVs, then re-dedups using merge_with_latest_dedup() —
# which now collapses the ex-duplicates while preserving their original
# scraped_at timestamps.
#
# Run after editing any team_names_*.csv file.
#
# USAGE
# -----
#   Rscript scripts/normalise_historical_teams.R             # apply changes
#   Rscript scripts/normalise_historical_teams.R --dry-run   # preview only
#   Rscript scripts/normalise_historical_teams.R --sport handball_iceland
#
# SAFETY
# ------
# Idempotent: running twice is a no-op. Files are only written if content
# actually changed (byte-level compare). Existing git history is the
# ultimate safety net — review the diff and revert with `git checkout` if
# anything looks wrong.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(yaml)
  library(here)
})

here::i_am("scripts/normalise_historical_teams.R")

# Load the accumulation pipeline so we reuse merge_with_latest_dedup().
source(here("R", "pipeline.R"))

# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
dry_run <- "--dry-run" %in% args

sport_filter <- {
  i <- which(args == "--sport")
  if (length(i) > 0 && i < length(args)) args[i + 1] else NULL
}

if (dry_run) cat("=== DRY RUN — no files will be modified ===\n\n")

# ---------------------------------------------------------------------------
# Find sport_keys with data + team-name mapping
# ---------------------------------------------------------------------------

data_root <- here("data")
sport_keys <- list.dirs(data_root, full.names = FALSE, recursive = FALSE)
if (!is.null(sport_filter)) {
  sport_keys <- intersect(sport_keys, sport_filter)
  if (length(sport_keys) == 0) {
    stop("No matching sport_key for --sport=", sport_filter)
  }
}

config_path <- here("config", "competitions.yml")
competitions <- yaml.load(read_file(config_path))

markets <- c(
  odds_1x2 = "odds_1x2.csv",
  odds_handicap = "odds_handicap.csv",
  odds_totals = "odds_totals.csv"
)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

load_team_names <- function(sport_key) {
  sport_cfg <- competitions[[sport_key]]
  if (is.null(sport_cfg) || is.null(sport_cfg$team_names)) {
    return(NULL)
  }
  path <- here("config", sport_cfg$team_names)
  if (!file.exists(path)) {
    return(NULL)
  }
  read_csv(path, show_col_types = FALSE) |>
    mutate(across(c(out, `in`), str_squish))
}

apply_mapping <- function(d, team_names) {
  if (is.null(team_names) || nrow(team_names) == 0 || nrow(d) == 0) {
    return(d)
  }
  d |>
    mutate(across(c(home, away), str_squish)) |>
    left_join(team_names, by = join_by(home == `in`)) |>
    mutate(home = coalesce(out, home)) |>
    select(-out) |>
    left_join(team_names, by = join_by(away == `in`)) |>
    mutate(away = coalesce(out, away)) |>
    select(-out)
}

process_market <- function(sport_key, market, filename, team_names) {
  path <- file.path(data_root, sport_key, filename)
  if (!file.exists(path)) {
    return(invisible(NULL))
  }

  before_bytes <- file.size(path)
  original <- read_csv(path, show_col_types = FALSE)
  n_before <- nrow(original)

  normalised <- apply_mapping(original, team_names)

  # Re-dedup by pretending the normalised rows are a fresh scrape against
  # an empty baseline — merge_with_latest_dedup() preserves scraped_at on
  # the earliest appearance of each (match, odds) tuple and collapses
  # identical re-observations onto last_seen_at.
  # We do this row-by-row in scraped_at order to preserve the true
  # observation sequence, which matters for oscillation handling.
  if ("scraped_at" %in% names(normalised)) {
    normalised$scraped_at <- as.POSIXct(normalised$scraped_at)
    normalised <- normalised |> arrange(scraped_at)
  }
  if ("last_seen_at" %in% names(normalised)) {
    normalised$last_seen_at <- as.POSIXct(normalised$last_seen_at)
  }

  # Ensure the schema columns exist before feeding into the merge helper.
  if (!"booker" %in% names(normalised)) normalised$booker <- "lengjan"
  if (!"status" %in% names(normalised)) normalised$status <- "open"
  if (!"last_seen_at" %in% names(normalised) && "scraped_at" %in% names(normalised)) {
    normalised$last_seen_at <- normalised$scraped_at
  }

  # Replay the data through the dedup engine: start empty, fold each
  # unique scraped_at batch in.
  merged <- tibble()
  if ("scraped_at" %in% names(normalised)) {
    batches <- split(normalised, normalised$scraped_at)
    for (batch in batches) {
      merged <- merge_with_latest_dedup(merged, batch, market)
    }
  } else {
    merged <- normalised
  }

  # Stable ordering for diff-friendliness.
  merged <- merged |> arrange(date, league, home, away, scraped_at)

  # Did we actually change anything?
  if (identical(merged, original)) {
    message(sport_key, " / ", market, ": unchanged (", n_before, " rows)")
    return(invisible(NULL))
  }

  n_after <- nrow(merged)
  msg <- sprintf(
    "%s / %s: %d -> %d rows (%+d)",
    sport_key, market, n_before, n_after, n_after - n_before
  )
  message(msg)

  if (!dry_run) {
    write_csv(merged, path)
  }
}

# ---------------------------------------------------------------------------
# Main loop
# ---------------------------------------------------------------------------

for (sport_key in sport_keys) {
  team_names <- load_team_names(sport_key)
  if (is.null(team_names)) {
    message(sport_key, ": no team_names mapping — skipping (nothing to normalise)")
    next
  }

  cat("\n==", sport_key, "==\n")
  for (market in names(markets)) {
    process_market(sport_key, market, markets[[market]], team_names)
  }
}

cat("\n")
if (dry_run) {
  cat("(Dry run — no files modified. Remove --dry-run to apply.)\n")
} else {
  cat("Done. Review with `git diff data/` and commit if the changes look right.\n")
}
