#!/usr/bin/env Rscript
# scripts/0Nm_backfill_round.R -- one-shot migration populating the results
# `round` column, which has been NA since inception (the federation scrapers
# cannot supply it; see R/derive-round.R). Dense-ranks match_date within each
# (sport, country, sex, season, division) cell.
#
# Safe + idempotent: `round` is NOT part of the results upsert natural key
# (R/storage.R) and new-row-wins on collision, so this overwrites NA in place
# with zero row churn. Re-running yields identical ranks. Read-only on the
# ledger/money path. Scoped to RESULTS only -- the schedules table holds only
# future fixtures and would mis-rank from 1, so its round stays NA (no consumer
# reads the schema round column today).
#
# Usage: Rscript scripts/0Nm_backfill_round.R

invisible(Sys.setlocale("LC_ALL", "is_IS.UTF-8"))
options(width = 120)
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))

root <- here::here("data")
results <- read_table("results", root = root)
if (nrow(results) == 0L) {
  cli::cli_alert_warning("results store is empty; nothing to backfill.")
  quit(save = "no", status = 0L)
}

n_na_before <- sum(is.na(results$round))
cli::cli_inform(
  "results: {nrow(results)} rows, {n_na_before} with round = NA before backfill"
)

results <- derive_league_round(results)

# Tripwire (exact identity of the appearance method): per non-cup cell,
# max(round) must equal the most games any single team played, and min(round)
# must be 1. Date-ranking breaks this -- distinct dates exceed games-per-team --
# so this catches a regression to date-ranking without a magnitude heuristic
# (it is league-agnostic: basketball's 4x round-robin passes, football passes).
cell_cols <- c("sport", "country", "sex", "season", "division")
max_games <- results |>
  dplyr::filter(.data$division != "CUP") |>
  (\(d) dplyr::bind_rows(
    dplyr::transmute(d, dplyr::across(dplyr::all_of(cell_cols)), team = .data$home_team),
    dplyr::transmute(d, dplyr::across(dplyr::all_of(cell_cols)), team = .data$away_team)
  ))() |>
  dplyr::count(dplyr::across(dplyr::all_of(c(cell_cols, "team")))) |>
  dplyr::group_by(dplyr::across(dplyr::all_of(cell_cols))) |>
  dplyr::summarise(max_games = max(.data$n), .groups = "drop")

chk <- results |>
  dplyr::filter(.data$division != "CUP", !is.na(.data$round)) |>
  dplyr::group_by(dplyr::across(dplyr::all_of(cell_cols))) |>
  dplyr::summarise(
    min_round = min(.data$round), max_round = max(.data$round), .groups = "drop"
  ) |>
  dplyr::left_join(max_games, by = cell_cols)
bad <- chk[chk$min_round != 1L | chk$max_round != chk$max_games, , drop = FALSE]
if (nrow(bad) > 0L) {
  print(as.data.frame(bad))
  cli::cli_abort(
    "Round derivation failed validation in {nrow(bad)} cell(s); aborting before write."
  )
}

n_na_after <- sum(is.na(results$round))
n_cup_na <- sum(is.na(results$round) & results$division == "CUP")
cli::cli_inform(
  "after derive: {n_na_after} NA remain ({n_cup_na} are CUP rows, expected); {nrow(chk)} league cells validated"
)

upsert_table(results, "results", root = root)
cli::cli_alert_success("Backfilled round for {nrow(results)} results rows.")
