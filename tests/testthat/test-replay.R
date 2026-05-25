# Tests for replay_football_iceland(): the orchestrator behind
# scripts/0Nr_replay.R that lets us re-fit + re-publish for any
# historical date (cumulative xP backtests, what-if analyses, fixing
# stale publishes after the fact). See F1 in
# docs/audits/2026-05-25-pipeline-cross-project-review.html.

backup_fit_path_replay <- function(sex) {
  root <- Sys.getenv(
    "SPORTS_BACKUP_ROOT",
    "/Users/brynjolfurjonsson/sports-backup-20260424-163153"
  )
  file.path(root, "Sports", "football", "iceland", "results", sex, "fit.rds")
}

test_that("replay_football_iceland: --no-fit publishes from existing extracts, honest fit_date", {
  fit_path <- backup_fit_path_replay("male")
  if (!file.exists(fit_path)) testthat::skip("legacy football fit unavailable")
  if (!dir.exists(here::here("data", "facts", "results"))) {
    testthat::skip("facts/results absent")
  }

  fit <- readRDS(fit_path)
  league <- load_leagues()[["football_iceland"]]
  extracts_dir <- withr::local_tempdir()
  out_dir <- withr::local_tempdir()
  hist_dir <- withr::local_tempdir()

  suppressWarnings(suppressMessages(
    extract_football_iceland(
      fit, league,
      sex = "male",
      fit_date = as.Date("2026-04-15"),
      end_date = as.Date("2026-04-15"),
      extracts_root = extracts_dir
    )
  ))

  suppressWarnings(suppressMessages(
    replay_football_iceland(
      sex = "male",
      as_of = as.Date("2026-04-15"),
      fit = FALSE,
      output_root = out_dir,
      extracts_root = extracts_dir,
      archive_root = withr::local_tempdir(),
      round_predictions_history_root = hist_dir
    )
  ))

  meta_path <- file.path(out_dir, "football", "iceland", "karla-bd", "meta.json")
  expect_true(file.exists(meta_path))
  meta <- jsonlite::read_json(meta_path)
  expect_equal(meta[["fit_date"]], "2026-04-15")
  expect_equal(meta[["sport"]], "football")
  expect_equal(meta[["sex"]], "male")
})

test_that("replay_football_iceland: --no-fit errors when no extracts at the target date", {
  extracts_dir <- withr::local_tempdir()
  out_dir <- withr::local_tempdir()

  expect_error(
    suppressWarnings(suppressMessages(
      replay_football_iceland(
        sex = "male",
        as_of = as.Date("2026-04-15"),
        fit = FALSE,
        output_root = out_dir,
        extracts_root = extracts_dir,
        round_predictions_history_root = withr::local_tempdir()
      )
    )),
    "no extracts"
  )
})

test_that("replay_football_iceland: output_root override isolates the what-if write", {
  # When the caller passes a custom output_root (e.g. /tmp/replay/),
  # neither the live data/publish/ tree nor data/beliefs/round_predictions_history/
  # should be touched -- the whole point of --publish-to is safe what-if.
  fit_path <- backup_fit_path_replay("male")
  if (!file.exists(fit_path)) testthat::skip("legacy football fit unavailable")
  if (!dir.exists(here::here("data", "facts", "results"))) {
    testthat::skip("facts/results absent")
  }

  fit <- readRDS(fit_path)
  league <- load_leagues()[["football_iceland"]]
  extracts_dir <- withr::local_tempdir()
  isolated_out <- withr::local_tempdir()

  suppressWarnings(suppressMessages(
    extract_football_iceland(
      fit, league,
      sex = "male",
      fit_date = as.Date("2026-04-15"),
      end_date = as.Date("2026-04-15"),
      extracts_root = extracts_dir
    )
  ))

  prod_publish_marker <- here::here(
    "data", "publish", "football", "iceland", "karla-bd", "meta.json"
  )
  mtime_before <- if (file.exists(prod_publish_marker)) {
    file.info(prod_publish_marker)$mtime
  } else {
    NA
  }

  suppressWarnings(suppressMessages(
    replay_football_iceland(
      sex = "male",
      as_of = as.Date("2026-04-15"),
      fit = FALSE,
      output_root = isolated_out,
      extracts_root = extracts_dir,
      archive_root = withr::local_tempdir()
    )
  ))

  expect_true(file.exists(
    file.path(isolated_out, "football", "iceland", "karla-bd", "meta.json")
  ))

  mtime_after <- if (file.exists(prod_publish_marker)) {
    file.info(prod_publish_marker)$mtime
  } else {
    NA
  }
  expect_identical(mtime_before, mtime_after)
})
