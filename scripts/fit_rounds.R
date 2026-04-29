#!/usr/bin/env Rscript
# scripts/fit_rounds.R -- per-round Stan fits for time-series of
# team strengths and predicted season totals.
#
# Usage:
#   Rscript scripts/fit_rounds.R                                 # default: football_iceland, both sexes, rounds 1:22, current year
#   Rscript scripts/fit_rounds.R --league football_iceland --sex male --rounds 1:4
#   Rscript scripts/fit_rounds.R --season 2026 --top-division BD
#
# Round N = season state when every top-division team has played N matches.
# Rounds that are not yet complete in the data are skipped with a warning.

options(width = 120)
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
get_flag <- function(name) any(args == paste0("--", name))
get_arg <- function(name, default = NULL) {
  i <- which(args == paste0("--", name))
  if (length(i) == 0L) default else args[[i + 1L]]
}

league_key <- get_arg("league", "football_iceland")
sex_arg <- get_arg("sex", "all")
season <- as.integer(get_arg("season", format(Sys.Date(), "%Y")))
rounds_str <- get_arg("rounds", "1:22")
top_division <- get_arg("top-division", "BD")
seed <- as.integer(get_arg("seed", "4242"))

rounds <- eval(parse(text = rounds_str))
stopifnot(is.numeric(rounds), length(rounds) >= 1L, all(rounds >= 1L))

if (sex_arg == "all") {
  sexes <- c("male", "female")
} else {
  stopifnot(sex_arg %in% c("male", "female"))
  sexes <- sex_arg
}

leagues <- load_leagues()
if (!league_key %in% names(leagues)) {
  stop(
    "Unknown league: ", league_key,
    " (available: ", paste(names(leagues), collapse = ", "), ")",
    call. = FALSE
  )
}
league <- leagues[[league_key]]

run_one <- function(sex, round_cutoff) {
  cli::cli_h2(
    "Fit: {league_key} {sex} season {season} round {round_cutoff}"
  )
  t0 <- Sys.time()
  out <- tryCatch(
    fit_league(
      league = league,
      sex = sex,
      season = season,
      round_cutoff = round_cutoff,
      top_division = top_division,
      seed = seed
    ),
    error = function(e) {
      cli::cli_alert_danger("Error: {conditionMessage(e)}")
      NULL
    }
  )
  dt <- difftime(Sys.time(), t0, units = "mins")
  if (is.null(out)) {
    cli::cli_alert_warning(
      paste0(
        "Skipped or failed (round=", round_cutoff, ", sex=", sex,
        ", dt=", round(as.numeric(dt), 1), " min)"
      )
    )
  } else {
    cli::cli_alert_success(
      paste0(
        "Done (round=", round_cutoff, ", sex=", sex,
        ", rows=", nrow(out),
        ", dt=", round(as.numeric(dt), 1), " min)"
      )
    )
  }
  invisible(out)
}

cli::cli_h1(
  paste0(
    "Per-round fits: league=", league_key, ", season=", season,
    ", rounds=", rounds_str, ", sexes=", paste(sexes, collapse = "+")
  )
)

for (rc in rounds) {
  for (sx in sexes) {
    run_one(sex = sx, round_cutoff = as.integer(rc))
  }
}

cli::cli_alert_success("All rounds processed.")
