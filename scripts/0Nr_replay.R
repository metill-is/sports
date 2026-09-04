#!/usr/bin/env Rscript
# scripts/0Nr_replay.R --
# Re-fit and re-publish football iceland for a historical date.
#
# Replay infrastructure already exists end-to-end: fit_league(end_date=)
# can fit "as of" any past date, extract_football_iceland() persists the
# per-fit posterior summaries, and publish_iceland_league() can render
# from any fit_date partition. This script wires those into a single
# CLI so ad-hoc backfills, what-if analyses, and cumulative-xPts
# backtests don't require editing 03b_backfill_football_iceland.R.
#
# Usage:
#   # Re-fit + re-publish in place (overwrites data/publish/):
#   Rscript scripts/0Nr_replay.R --league football_iceland --sex male --as-of 2026-05-15
#
#   # Re-fit + re-publish into an isolated tree (safe for what-if):
#   Rscript scripts/0Nr_replay.R --league football_iceland --sex male \
#       --as-of 2026-05-15 --publish-to data/publish_replay/2026-05-15/
#
#   # Backfill all pre-round dates for a season (replaces 03b's hardcoded list):
#   Rscript scripts/0Nr_replay.R --league football_iceland --sex male \
#       --season 2026 --per-round
#
#   # Re-publish from existing extracts without re-fitting (~seconds):
#   Rscript scripts/0Nr_replay.R --league football_iceland --sex male \
#       --as-of 2026-05-15 --no-fit
#
# All replay paths derive the Stan seed from `as_of`
# (`as.integer(format(as_of, "%Y%m%d"))`) so re-runs reproduce the
# posterior numerically -- mirrors R/simulate-cup-bracket.R.

invisible(Sys.setlocale("LC_ALL", "en_US.UTF-8"))
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
get_flag <- function(name, default = NULL) {
  i <- which(args == paste0("--", name))
  if (length(i) == 0L) default else args[[i + 1L]]
}
has_flag <- function(name) paste0("--", name) %in% args

league_key <- get_flag("league")
sex <- get_flag("sex")
as_of_str <- get_flag("as-of")
season_str <- get_flag("season")
per_round <- has_flag("per-round")
no_fit <- has_flag("no-fit")
publish_to <- get_flag("publish-to")

if (is.null(league_key)) {
  stop("--league required (e.g. football_iceland)", call. = FALSE)
}
if (is.null(sex)) {
  stop("--sex required (male or female)", call. = FALSE)
}
if (!identical(league_key, "football_iceland")) {
  stop(
    "Replay currently supports only football_iceland. ",
    "Basketball/handball still consume the fit RDS directly ",
    "(F6 in docs/audits/2026-05-25-pipeline-cross-project-review.html).",
    call. = FALSE
  )
}

output_root <- if (!is.null(publish_to)) {
  dir.create(publish_to, recursive = TRUE, showWarnings = FALSE)
  publish_to
} else {
  here::here("data", "publish")
}

dates <- if (isTRUE(per_round)) {
  if (is.null(season_str)) {
    stop("--per-round requires --season YYYY", call. = FALSE)
  }
  season <- as.integer(season_str)
  if (is.na(season)) {
    stop("--season must be an integer year", call. = FALSE)
  }

  leagues <- load_leagues()
  league_def <- leagues[[league_key]]
  results <- read_table(
    "results",
    filter = list(
      sport = league_def$sport,
      country = league_def$country,
      sex = sex
    )
  )

  cli::cli_h1("Computing per-round dates for {league_key}/{sex} season={season}")
  out <- list()
  for (n in seq_len(50L)) {
    d <- suppressWarnings(suppressMessages(
      compute_round_cutoff_date(
        results,
        season = season,
        round_cutoff = n
      )
    ))
    if (is.null(d)) break
    out[[length(out) + 1L]] <- d
    cli::cli_alert_info("  round {n}: {format(d)}")
  }
  if (length(out) == 0L) {
    stop("No completed rounds for season ", season, call. = FALSE)
  }
  do.call(c, out)
} else {
  if (is.null(as_of_str)) {
    stop(
      "--as-of YYYY-MM-DD required (or use --per-round --season YYYY)",
      call. = FALSE
    )
  }
  d <- as.Date(as_of_str)
  if (is.na(d)) {
    stop("--as-of: could not parse '", as_of_str, "' as YYYY-MM-DD", call. = FALSE)
  }
  d
}

cli::cli_h1(
  "Replay {league_key}/{sex}: {length(dates)} date(s){if (no_fit) ' (no-fit)' else ''}"
)
for (i in seq_along(dates)) {
  d <- dates[i]
  cli::cli_h2("[{i}/{length(dates)}] {format(d)}")
  replay_football_iceland(
    sex = sex,
    as_of = d,
    fit = !no_fit,
    output_root = output_root
  )
}
cli::cli_alert_success("Replay complete: {length(dates)} date(s)")
