# Locate the legacy backup fit. Defaults to the developer's local path; can
# be overridden with SPORTS_BACKUP_ROOT env var (or unset for CI to skip).
backup_fit_path <- function(sex) {
  root <- Sys.getenv(
    "SPORTS_BACKUP_ROOT",
    "/Users/brynjolfurjonsson/sports-backup-20260424-163153"
  )
  file.path(root, "Sports", "football", "iceland", "results", sex, "fit.rds")
}

test_that("publish_football_iceland: skip gracefully when backup fit absent", {
  skip_if_no_football_fit <- function(sex = "male") {
    fit_path <- backup_fit_path(sex)
    if (!file.exists(fit_path)) {
      testthat::skip(paste("legacy football fit unavailable:", fit_path))
    }
    if (!dir.exists(here::here("data", "facts", "results"))) {
      testthat::skip("facts/results absent -- cannot reconstruct prepare_data")
    }
  }

  skip_if_no_football_fit()

  fit <- readRDS(backup_fit_path("male"))
  leagues <- load_leagues()
  league <- leagues[["football_iceland"]]

  out <- withr::local_tempdir()
  # The backup fit was trained on an older data snapshot (N=3172, K=68).
  # With end_date = today, prepare_data returns more rows -- the function
  # warns about the mismatch and writes valid (possibly empty) JSONs for
  # the posterior-dependent outputs.
  suppressWarnings(
    publish_football_iceland(
      fit,
      league,
      sex = "male",
      end_date = as.Date("2026-04-25"),
      output_root = out
    )
  )

  out_dir <- file.path(out, "football", "iceland", "karla")
  expected <- c(
    "meta.json", "next_games.json", "standings.json",
    "team_strengths.json", "final_positions.json",
    "points_distribution.json", "home_advantage.json"
  )
  for (f in expected) {
    expect_true(
      file.exists(file.path(out_dir, f)),
      info = paste("missing:", f)
    )
  }

  # Spot-check: meta.json has required fields
  meta <- jsonlite::read_json(file.path(out_dir, "meta.json"))
  expect_named(
    meta,
    c("sex", "league", "season", "generated_at", "fit_date", "round", "n_draws"),
    ignore.order = TRUE
  )
  expect_equal(meta[["sex"]], "male")
  expect_type(meta[["round"]], "integer")
  expect_type(meta[["n_draws"]], "integer")

  # standings.json must be valid JSON with expected top-level keys
  standings <- jsonlite::read_json(file.path(out_dir, "standings.json"))
  expect_true(all(c("generated_at", "season", "as_of", "rows") %in% names(standings)))

  # team_strengths.json must be valid JSON
  ts <- jsonlite::read_json(file.path(out_dir, "team_strengths.json"))
  expect_true(all(c("generated_at", "records") %in% names(ts)))

  # home_advantage.json must be valid JSON
  ha <- jsonlite::read_json(file.path(out_dir, "home_advantage.json"))
  expect_true(all(c("generated_at", "records") %in% names(ha)))
})

test_that("publish_football_iceland female: produces 7 JSONs", {
  fit_path <- backup_fit_path("female")
  if (!file.exists(fit_path)) {
    testthat::skip("legacy female football fit unavailable")
  }
  if (!dir.exists(here::here("data", "facts", "results"))) {
    testthat::skip("facts/results absent -- cannot reconstruct prepare_data")
  }

  fit <- readRDS(fit_path)
  leagues <- load_leagues()
  league <- leagues[["football_iceland"]]

  out <- withr::local_tempdir()
  suppressWarnings(
    publish_football_iceland(
      fit,
      league,
      sex = "female",
      end_date = as.Date("2026-04-25"),
      output_root = out
    )
  )

  out_dir <- file.path(out, "football", "iceland", "kvenna")
  expected <- c(
    "meta.json", "next_games.json", "standings.json",
    "team_strengths.json", "final_positions.json",
    "points_distribution.json", "home_advantage.json"
  )
  for (f in expected) {
    expect_true(
      file.exists(file.path(out_dir, f)),
      info = paste("missing:", f)
    )
  }

  # meta female
  meta <- jsonlite::read_json(file.path(out_dir, "meta.json"))
  expect_equal(meta[["sex"]], "female")

  # standings for female should exist (even if empty-rows)
  standings <- jsonlite::read_json(file.path(out_dir, "standings.json"))
  expect_true(all(c("generated_at", "season", "rows") %in% names(standings)))
})

test_that("publish_football_iceland: output_root creates directory", {
  fit_path <- backup_fit_path("male")
  if (!file.exists(fit_path)) {
    testthat::skip("legacy male football fit unavailable")
  }
  if (!dir.exists(here::here("data", "facts", "results"))) {
    testthat::skip("facts/results absent")
  }

  fit <- readRDS(fit_path)
  leagues <- load_leagues()
  league <- leagues[["football_iceland"]]

  out <- file.path(withr::local_tempdir(), "nested", "output", "root")
  # Directory does not exist yet -- function should create it
  expect_false(dir.exists(out))
  suppressWarnings(
    publish_football_iceland(
      fit, league,
      sex = "male",
      end_date = as.Date("2026-04-25"),
      output_root = out
    )
  )
  expect_true(dir.exists(file.path(out, "football", "iceland", "karla")))
})
