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
  extracted <- .build_extracted_football_for_test(
    fit, league,
    sex = "male", end_date = as.Date("2026-04-25")
  )
  suppressWarnings(
    publish_football_iceland(
      extracted = extracted,
      league = league,
      sex = "male",
      end_date = as.Date("2026-04-25"),
      output_root = out
    )
  )

  out_dir <- file.path(out, "football", "iceland", "karla-bd")
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
    c("sport", "sex", "league", "season", "generated_at", "fit_date", "round", "n_draws"),
    ignore.order = TRUE
  )
  expect_equal(meta[["sport"]], "football")
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

test_that("publish_football_iceland female: writes the 7 always-on JSONs", {
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
  extracted <- .build_extracted_football_for_test(
    fit, league,
    sex = "female", end_date = as.Date("2026-04-25")
  )
  suppressWarnings(
    publish_football_iceland(
      extracted = extracted,
      league = league,
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
  extracted <- .build_extracted_football_for_test(
    fit, league,
    sex = "male", end_date = as.Date("2026-04-25")
  )
  suppressWarnings(
    publish_football_iceland(
      extracted = extracted,
      league = league,
      sex = "male",
      end_date = as.Date("2026-04-25"),
      output_root = out
    )
  )
  expect_true(dir.exists(file.path(out, "football", "iceland", "karla")))
})

# ---- History helper unit tests --------------------------------------------

test_that(".append_to_history_pfi creates a new file with schema_version=1", {
  out <- withr::local_tempdir()
  path <- file.path(out, "history.json")

  rows <- tibble::tibble(
    fit_date = "2026-04-27", generated_at = "2026-04-27T12:00:00+0000",
    team = "Valur", value = 1.5
  )
  sports:::.append_to_history_pfi(path, rows, key_cols = c("fit_date", "team"))

  expect_true(file.exists(path))
  parsed <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  expect_equal(parsed$schema_version, 1L)
  expect_equal(nrow(parsed$records), 1L)
  expect_equal(parsed$records$team, "Valur")
})

test_that(".append_to_history_pfi appends new rows on subsequent calls", {
  out <- withr::local_tempdir()
  path <- file.path(out, "history.json")

  day1 <- tibble::tibble(
    fit_date = "2026-04-26", generated_at = "2026-04-26T12:00:00+0000",
    team = "Valur", value = 1.5
  )
  sports:::.append_to_history_pfi(path, day1, key_cols = c("fit_date", "team"))

  day2 <- tibble::tibble(
    fit_date = "2026-04-27", generated_at = "2026-04-27T12:00:00+0000",
    team = "Valur", value = 1.7
  )
  sports:::.append_to_history_pfi(path, day2, key_cols = c("fit_date", "team"))

  parsed <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  expect_equal(nrow(parsed$records), 2L)
  expect_setequal(parsed$records$fit_date, c("2026-04-26", "2026-04-27"))
})

test_that(".append_to_history_pfi dedups on key_cols, keeping the newest generated_at", {
  out <- withr::local_tempdir()
  path <- file.path(out, "history.json")

  earlier <- tibble::tibble(
    fit_date = "2026-04-27", generated_at = "2026-04-27T08:00:00+0000",
    team = "Valur", value = 1.5
  )
  sports:::.append_to_history_pfi(path, earlier, key_cols = c("fit_date", "team"))

  later <- tibble::tibble(
    fit_date = "2026-04-27", generated_at = "2026-04-27T16:00:00+0000",
    team = "Valur", value = 9.9
  )
  sports:::.append_to_history_pfi(path, later, key_cols = c("fit_date", "team"))

  parsed <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  expect_equal(nrow(parsed$records), 1L)
  expect_equal(parsed$records$value, 9.9)
})

test_that(".append_to_history_pfi tolerates a missing or malformed file", {
  out <- withr::local_tempdir()
  bad <- file.path(out, "bad.json")
  writeLines("not valid json {", bad)

  rows <- tibble::tibble(
    fit_date = "2026-04-27", generated_at = "2026-04-27T12:00:00+0000",
    team = "Valur", value = 1.5
  )
  sports:::.append_to_history_pfi(bad, rows, key_cols = c("fit_date", "team"))

  parsed <- jsonlite::fromJSON(bad, simplifyDataFrame = TRUE)
  expect_equal(nrow(parsed$records), 1L)
})

# ---- History integration tests --------------------------------------------

test_that("publish_football_iceland writes team_strengths_history.json (male)", {
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

  out <- withr::local_tempdir()
  extracted <- .build_extracted_football_for_test(
    fit, league,
    sex = "male", end_date = as.Date("2026-04-25")
  )
  suppressWarnings(
    publish_football_iceland(
      extracted = extracted,
      league = league,
      sex = "male",
      end_date = as.Date("2026-04-25"),
      output_root = out
    )
  )

  hist_path <- file.path(
    out, "football", "iceland", "karla", "team_strengths_history.json"
  )
  expect_true(file.exists(hist_path))

  parsed <- jsonlite::fromJSON(hist_path, simplifyDataFrame = TRUE)
  expect_equal(parsed$schema_version, 1L)
  expect_true(nrow(parsed$records) > 0L)
  expected_cols <- c(
    "fit_date", "generated_at", "round", "season",
    "team", "component", "location", "coverage",
    "median", "lower", "upper"
  )
  expect_true(all(expected_cols %in% names(parsed$records)))
  expect_setequal(unique(parsed$records$component), c("offence", "defence", "total"))
  expect_setequal(unique(parsed$records$location), c("home", "away", "avg"))
  expect_setequal(unique(parsed$records$coverage), c(0.5, 0.8, 0.95))
})

test_that("publish_football_iceland writes team_strengths_history.json (female)", {
  fit_path <- backup_fit_path("female")
  if (!file.exists(fit_path)) {
    testthat::skip("legacy female football fit unavailable")
  }
  if (!dir.exists(here::here("data", "facts", "results"))) {
    testthat::skip("facts/results absent")
  }

  fit <- readRDS(fit_path)
  leagues <- load_leagues()
  league <- leagues[["football_iceland"]]

  out <- withr::local_tempdir()
  extracted <- .build_extracted_football_for_test(
    fit, league,
    sex = "female", end_date = as.Date("2026-04-25")
  )
  suppressWarnings(
    publish_football_iceland(
      extracted = extracted,
      league = league,
      sex = "female",
      end_date = as.Date("2026-04-25"),
      output_root = out
    )
  )

  hist_path <- file.path(
    out, "football", "iceland", "kvenna", "team_strengths_history.json"
  )
  expect_true(file.exists(hist_path))
  parsed <- jsonlite::fromJSON(hist_path, simplifyDataFrame = TRUE)
  expect_equal(parsed$schema_version, 1L)
})

test_that("publish_football_iceland writes standings_history.json when matches have been played", {
  fit_path_m <- backup_fit_path("male")
  fit_path_f <- backup_fit_path("female")
  if (!file.exists(fit_path_m) || !file.exists(fit_path_f)) {
    testthat::skip("legacy football fits unavailable for both sexes")
  }
  if (!dir.exists(here::here("data", "facts", "results"))) {
    testthat::skip("facts/results absent")
  }

  leagues <- load_leagues()
  league <- leagues[["football_iceland"]]

  out <- withr::local_tempdir()
  fit_m <- readRDS(fit_path_m)
  fit_f <- readRDS(fit_path_f)
  extracted_m <- .build_extracted_football_for_test(
    fit_m, league,
    sex = "male", end_date = as.Date("2026-04-25")
  )
  extracted_f <- .build_extracted_football_for_test(
    fit_f, league,
    sex = "female", end_date = as.Date("2026-04-25")
  )
  suppressWarnings({
    publish_football_iceland(
      extracted = extracted_m,
      league = league,
      sex = "male",
      end_date = as.Date("2026-04-25"), output_root = out
    )
    publish_football_iceland(
      extracted = extracted_f,
      league = league,
      sex = "female",
      end_date = as.Date("2026-04-25"), output_root = out
    )
  })

  # Karla had matches played by 2026-04-25, so standings_history.json must exist
  # and follow the schema. Kvenna's existence depends on whether the women's
  # season had top-flight matches by the same end_date -- if it did, the file
  # follows the same schema; if not, no file is written (no empty-snapshot rows).
  expect_true(file.exists(file.path(
    out, "football", "iceland", "karla", "standings_history.json"
  )))

  expected_cols <- c(
    "as_of", "generated_at", "round", "season",
    "team", "short", "played", "wins", "draws", "losses",
    "goals_for", "goals_against", "goal_diff", "points", "rank"
  )

  for (sex_folder in c("karla", "kvenna")) {
    hist_path <- file.path(
      out, "football", "iceland", sex_folder, "standings_history.json"
    )
    if (!file.exists(hist_path)) next
    parsed <- jsonlite::fromJSON(hist_path, simplifyDataFrame = TRUE)
    expect_equal(parsed$schema_version, 1L)
    if (nrow(parsed$records) > 0L) {
      expect_true(all(expected_cols %in% names(parsed$records)))
    }
  }
})

test_that("publish_football_iceland: re-running dedups history on (round, team, ...)", {
  # Multiple fits within the same matchweek collapse to one row per
  # (round, team, component, location, coverage); latest fit wins. This
  # keeps the strength-trajectory chart plotting one point per matchweek
  # rather than stacking within-round refits as duplicate round-X points.
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

  out <- withr::local_tempdir()
  extracted <- .build_extracted_football_for_test(
    fit, league,
    sex = "male", end_date = as.Date("2026-04-25")
  )
  suppressWarnings({
    publish_football_iceland(
      extracted = extracted,
      league = league,
      sex = "male",
      end_date = as.Date("2026-04-25"), output_root = out
    )
    publish_football_iceland(
      extracted = extracted,
      league = league,
      sex = "male",
      end_date = as.Date("2026-04-25"), output_root = out
    )
  })

  hist_path <- file.path(
    out, "football", "iceland", "karla", "team_strengths_history.json"
  )
  parsed <- jsonlite::fromJSON(hist_path, simplifyDataFrame = TRUE)

  key_cols <- c("round", "team", "component", "location", "coverage")
  n_unique <- nrow(unique(parsed$records[, key_cols]))
  expect_equal(nrow(parsed$records), n_unique)
})

test_that("publish_football_iceland: BD output unchanged after target_div refactor (regression)", {
  fit_path <- backup_fit_path("male")
  if (!file.exists(fit_path)) {
    testthat::skip("legacy football fit unavailable")
  }
  if (!dir.exists(here::here("data", "facts", "results"))) {
    testthat::skip("facts/results absent")
  }

  fit <- readRDS(fit_path)
  leagues <- load_leagues()
  league <- leagues[["football_iceland"]]

  out <- withr::local_tempdir()
  extracted <- .build_extracted_football_for_test(
    fit, league,
    sex = "male", end_date = as.Date("2026-04-25")
  )
  suppressWarnings(
    publish_football_iceland(
      extracted = extracted,
      league = league,
      sex = "male",
      end_date = as.Date("2026-04-25"),
      output_root = out
    )
  )

  # After Task 1, public output is still at karla/ (the per-division dirs
  # come in Task 2). After Task 2, this expectation moves to karla-bd/.
  out_dir <- file.path(out, "football", "iceland", "karla-bd")
  expect_true(file.exists(file.path(out_dir, "standings.json")))
  standings <- jsonlite::read_json(file.path(out_dir, "standings.json"))
  # BD is 12 teams
  expect_equal(length(standings$rows), 12)
})

test_that("publish_football_iceland: writes karla-bd/ and karla-ld/ dirs", {
  fit_path <- backup_fit_path("male")
  if (!file.exists(fit_path)) {
    testthat::skip("legacy football fit unavailable")
  }
  if (!dir.exists(here::here("data", "facts", "results"))) {
    testthat::skip("facts/results absent")
  }

  fit <- readRDS(fit_path)
  leagues <- load_leagues()
  league <- leagues[["football_iceland"]]

  out <- withr::local_tempdir()
  extracted <- .build_extracted_football_for_test(
    fit, league,
    sex = "male", end_date = as.Date("2026-04-25")
  )
  suppressWarnings(
    publish_football_iceland(
      extracted = extracted,
      league = league,
      sex = "male",
      end_date = as.Date("2026-04-25"),
      output_root = out
    )
  )

  bd_dir <- file.path(out, "football", "iceland", "karla-bd")
  ld_dir <- file.path(out, "football", "iceland", "karla-ld")
  expect_true(dir.exists(bd_dir), info = "karla-bd dir missing")
  expect_true(dir.exists(ld_dir), info = "karla-ld dir missing")

  expected <- c(
    "meta.json", "next_games.json", "standings.json",
    "team_strengths.json", "final_positions.json",
    "points_distribution.json", "home_advantage.json"
  )
  for (f in expected) {
    expect_true(
      file.exists(file.path(bd_dir, f)),
      info = paste("missing in karla-bd:", f)
    )
    expect_true(
      file.exists(file.path(ld_dir, f)),
      info = paste("missing in karla-ld:", f)
    )
  }

  # BD has 12 teams; LD has 12 teams.
  bd_standings <- jsonlite::read_json(file.path(bd_dir, "standings.json"))
  ld_standings <- jsonlite::read_json(file.path(ld_dir, "standings.json"))
  expect_equal(length(bd_standings$rows), 12)
  expect_equal(length(ld_standings$rows), 12)
})
