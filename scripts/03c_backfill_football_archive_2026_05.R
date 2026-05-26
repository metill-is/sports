#!/usr/bin/env Rscript
# scripts/03c_backfill_football_archive_2026_05.R --
#
# One-off backfill: fill the 2026-05-04 → 2026-05-25 archive gap for
# football iceland. Background: commit eeb0b99f (Phase 3b, 2026-05-04)
# added a `!is_football_iceland` guard around the beliefs_archive write
# in fit_league(), with the intent that extract_football_iceland()'s
# per-fit Parquets under beliefs/extracts/ would replace the legacy
# per-draw archive. The drop was design-correct for the publisher and
# for replay (`scripts/0Nr_replay.R` reads extracts/) but left a hole in
# the on-disk audit trail: `data/beliefs/archive/sport=football/...`
# has no fit_date partitions between 2026-05-04 and 2026-05-25 even
# though fits ran in that window.
#
# Strategy:
#   1. Discover dates with `beliefs/extracts/.../fit_date=D/` but no
#      `beliefs/archive/.../fit_date=D/` — those are dates we know
#      were fit live but whose per-draw record was skipped.
#   2. For each (sex, date), re-fit with `end_date = D`,
#      `force_archive_write = TRUE`, and a deterministic seed derived
#      from `D` (same convention as `replay_football_iceland()`).
#   3. Skip-if-archive-exists so the script is re-runnable.
#
# Side effects:
#   - Re-runs each fit (~30 min @ 1000/1000 iters per sex on a
#     development laptop). 7 dates × 2 sexes = 14 fits = ~7 h
#     sequential, assuming the current on-disk state.
#   - Overwrites the existing `beliefs/extracts/.../fit_date=D/`
#     partitions with fresh draws. The model + data are unchanged
#     so the new draws are statistically equivalent; the
#     deterministic seed makes subsequent re-runs idempotent.
#   - Writes `beliefs/archive/.../fit_date=D/part-0.parquet` —
#     the only new persistent artefact.
#
# Detached launch:
#   nohup Rscript scripts/03c_backfill_football_archive_2026_05.R \
#       > /tmp/backfill-football-archive-2026-05.log 2>&1 & disown
#
# Force-rerun even when archive already exists:
#   Rscript scripts/03c_backfill_football_archive_2026_05.R --force

invisible(Sys.setlocale("LC_ALL", "en_US.UTF-8"))
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
force <- "--force" %in% args

WINDOW_START <- as.Date("2026-05-04")
WINDOW_END <- as.Date("2026-05-25")

discover_targets <- function(sex) {
  extracts_root <- here::here(
    "data", "beliefs", "extracts",
    "sport=football", "country=iceland", paste0("sex=", sex)
  )
  archive_root <- here::here(
    "data", "beliefs", "archive",
    "sport=football", "country=iceland", paste0("sex=", sex)
  )
  if (!fs::dir_exists(extracts_root)) {
    return(as.Date(character()))
  }

  parse_dates <- function(root) {
    if (!fs::dir_exists(root)) {
      return(as.Date(character()))
    }
    dirs <- fs::dir_ls(root, type = "directory")
    as.Date(stringr::str_remove(fs::path_file(dirs), "^fit_date="))
  }

  have_extracts <- parse_dates(extracts_root)
  have_archive <- parse_dates(archive_root)

  candidates <- have_extracts[
    have_extracts >= WINDOW_START & have_extracts <= WINDOW_END
  ]
  if (force) {
    return(sort(candidates))
  }
  sort(setdiff(candidates, have_archive))
}

leagues <- load_leagues()
league_def <- leagues[["football_iceland"]]

backfill_targets <- list(
  male = discover_targets("male"),
  female = discover_targets("female")
)

n_total <- sum(vapply(backfill_targets, length, integer(1)))
cli::cli_h1("Football iceland archive backfill: {n_total} fit(s)")
for (sex in names(backfill_targets)) {
  dates <- backfill_targets[[sex]]
  if (length(dates) == 0L) {
    cli::cli_alert_info("{sex}: nothing to backfill")
    next
  }
  cli::cli_alert_info("{sex}: {length(dates)} date(s) -- {paste(format(dates), collapse = ', ')}")
}

if (n_total == 0L) {
  cli::cli_alert_success("Nothing to backfill; archive is in sync with extracts.")
  quit(save = "no", status = 0L)
}

done <- 0L
for (sex in names(backfill_targets)) {
  for (d in backfill_targets[[sex]]) {
    d <- as.Date(d)
    done <- done + 1L
    cli::cli_h2("[{done}/{n_total}] football_iceland / {sex} / fit_date={format(d)}")
    seed <- as.integer(format(d, "%Y%m%d"))
    tryCatch(
      fit_league(
        league = league_def,
        sex = sex,
        fit_date = d,
        end_date = d,
        seed = seed,
        force_archive_write = TRUE
      ),
      error = function(e) {
        cli::cli_alert_danger(
          "fit failed for {sex} {format(d)}: {conditionMessage(e)}"
        )
      }
    )
  }
}

cli::cli_alert_success("Backfill complete: {done} fit(s)")
