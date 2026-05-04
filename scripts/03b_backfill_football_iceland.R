#!/usr/bin/env Rscript
# scripts/03b_backfill_football_iceland.R --
# Backfill historical Stan fits for football iceland to populate cumulative
# xG/xPts in standings.json (per-round lookahead-free) and the pre-season
# baseline row in team_strengths.json (forest plot red comparison).
#
# Each fit is per-sex (covers both BD and LD1 divisions); the extractor
# splits the output into `division={BD,LD1}/` partitions.
#
# Usage:
#   Rscript scripts/03b_backfill_football_iceland.R
#   Rscript scripts/03b_backfill_football_iceland.R --force      # ignore skip-if-exists
#
# Targets are derived from the gap between existing archive partitions
# and 2026 round-start dates -- see docs/superpowers/specs/ for context.

invisible(Sys.setlocale("LC_ALL", "en_US.UTF-8"))
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
force <- "--force" %in% args

# Each row is one fit. `end_date` doubles as `fit_date` so the partition
# directory aligns with the training cutoff.
fits_to_run <- tibble::tribble(
  ~sex,     ~end_date,
  # Pre-BD-r1 (also pre-LD1-r1; serves as forest-plot baseline)
  "male",   as.Date("2026-04-09"),
  "male",   as.Date("2026-04-16"), # pre-BD-r2
  "male",   as.Date("2026-04-21"), # pre-BD-r3
  # Pre-BD-r1 (also pre-LD1-r1; serves as forest-plot baseline)
  "female", as.Date("2026-04-23"),
  "female", as.Date("2026-04-24") # pre-BD-r2
)

archive_root <- here::here(
  "data", "beliefs", "archive",
  "sport=football", "country=iceland"
)

partition_complete <- function(sex, fit_date) {
  partition <- file.path(
    archive_root,
    paste0("sex=", sex),
    paste0("fit_date=", format(fit_date, "%Y-%m-%d"))
  )
  expected <- c(
    "final_positions.parquet",
    "home_advantage_quantiles.parquet",
    "points_distribution.parquet",
    "predicted_matches.parquet",
    "round_strengths_quantiles.parquet",
    "team_strengths_quantiles.parquet"
  )
  divs <- c("BD", "LD1")
  all(vapply(divs, function(d) {
    all(file.exists(file.path(partition, paste0("division=", d), expected)))
  }, logical(1)))
}

cli::cli_h1("Football iceland backfill: {nrow(fits_to_run)} fits")
done <- 0L
skipped <- 0L
for (i in seq_len(nrow(fits_to_run))) {
  row <- fits_to_run[i, ]
  sex <- row$sex
  end_date <- row$end_date

  if (!force && partition_complete(sex, end_date)) {
    cli::cli_alert_info(
      "[{i}/{nrow(fits_to_run)}] sex={sex}, fit_date={end_date}: already complete -- skipping"
    )
    skipped <- skipped + 1L
    next
  }

  cli::cli_h2("[{i}/{nrow(fits_to_run)}] sex={sex}, fit_date={end_date}")
  fit_league(
    league_key = "football_iceland",
    sex = sex,
    fit_date = end_date,
    end_date = end_date,
    # Match round_cutoff-mode horizon so final_positions/points_distribution
    # cover the full remainder of the season; the publisher only consumes
    # the next-round predictions, but a wider horizon keeps the per-fit
    # archive useful for ad-hoc inspection too.
    schedule_horizon_days = 200L
  )
  done <- done + 1L
}
cli::cli_alert_success(
  "Backfill complete: {done} fitted, {skipped} skipped"
)
