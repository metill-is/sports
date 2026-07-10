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
  required_meta_keys <- c(
    "sport", "sex", "league", "division", "is_cup",
    "season", "generated_at", "fit_date", "round", "n_draws"
  )
  expect_true(all(required_meta_keys %in% names(meta)))
  expect_equal(meta[["sport"]], "football")
  expect_equal(meta[["sex"]], "male")
  expect_equal(meta[["division"]], "BD")
  expect_false(meta[["is_cup"]])
  expect_type(meta[["round"]], "integer")
  expect_type(meta[["n_draws"]], "integer")
  # Split-season metadata for the BD cell (male: 6/6) -- lets the platform
  # render group boundaries + labels from data.
  expect_equal(meta[["split"]], list(upper = 6L, lower = 6L))

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

  out_dir <- file.path(out, "football", "iceland", "kvenna-bd")
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
  expect_true(dir.exists(file.path(out, "football", "iceland", "karla-bd")))
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
    out, "football", "iceland", "karla-bd", "team_strengths_history.json"
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
    out, "football", "iceland", "kvenna-bd", "team_strengths_history.json"
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
    out, "football", "iceland", "karla-bd", "standings_history.json"
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
    out, "football", "iceland", "karla-bd", "team_strengths_history.json"
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

test_that("publish_football_iceland: writes karla-bikar/ with is_cup=true and empty league-table JSONs", {
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

  cup_dir <- file.path(out, "football", "iceland", "karla-bikar")
  expect_true(dir.exists(cup_dir), info = "karla-bikar dir missing")

  meta <- jsonlite::read_json(file.path(cup_dir, "meta.json"))
  expect_true(meta[["is_cup"]])
  expect_equal(meta[["division"]], "CUP")
  expect_equal(meta[["league"]], "Mj\u00f3lkurbikar")
  expect_equal(meta[["sex"]], "male")
  # n_draws must reflect the fit's posterior sample count even for cup
  # cells where predicted_matches is empty by design. Pre-2026-05-15 the
  # cup meta.json reported n_draws=0, blinding consumers to whether the
  # cup model had actually been fit. Audit fix sources n_draws from
  # extracted$sim_inputs$scalar (always populated when the fit ran) when
  # predicted_matches is empty.
  expect_gt(meta[["n_draws"]], 0L)

  # Cup has no league table: standings, final_positions, points_distribution
  # all empty even though the JSON files exist (preserved for endpoint
  # stability — the frontend reads is_cup from meta.json and skips the
  # league-table sections).
  standings <- jsonlite::read_json(file.path(cup_dir, "standings.json"))
  expect_equal(length(standings$rows), 0L)
  final_pos <- jsonlite::read_json(file.path(cup_dir, "final_positions.json"))
  expect_equal(length(final_pos$records), 0L)
  points_dist <- jsonlite::read_json(file.path(cup_dir, "points_distribution.json"))
  expect_equal(length(points_dist$records), 0L)
})

test_that("publish_football_iceland: per-division next_games is filtered by division", {
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

  bd_ng <- jsonlite::read_json(
    file.path(out, "football", "iceland", "karla-bd", "next_games.json"),
    simplifyVector = TRUE
  )
  ld_ng <- jsonlite::read_json(
    file.path(out, "football", "iceland", "karla-ld", "next_games.json"),
    simplifyVector = TRUE
  )

  if (length(bd_ng$matches) > 0 && nrow(bd_ng$matches) > 0) {
    expect_true(all(bd_ng$matches$division_code == "BD"))
  }
  if (length(ld_ng$matches) > 0 && nrow(ld_ng$matches) > 0) {
    expect_true(all(ld_ng$matches$division_code == "LD"))
  }
})

test_that("publish_football_iceland: per-division team_strengths scoped to division teams", {
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

  bd_strengths <- jsonlite::read_json(file.path(bd_dir, "team_strengths.json"))
  ld_strengths <- jsonlite::read_json(file.path(ld_dir, "team_strengths.json"))

  bd_teams <- unique(vapply(
    bd_strengths$records, function(r) r$team, character(1)
  ))
  ld_teams <- unique(vapply(
    ld_strengths$records, function(r) r$team, character(1)
  ))

  # BD and LD must be disjoint team sets (no team plays both).
  expect_length(intersect(bd_teams, ld_teams), 0)
})

test_that("publish_football_iceland: embeds preseason field when prior fit exists", {
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

  extracts <- withr::local_tempdir()
  out <- withr::local_tempdir()
  end_date <- as.Date("2026-04-25")
  preseason_date <- as.Date("2026-04-09")

  # Build the "current" extracted bundle the publisher consumes.
  extracted <- .build_extracted_football_for_test(
    fit, league,
    sex = "male", end_date = end_date
  )

  # Synthesise a "preseason" extracts partition by copying the current
  # extracted tibbles to fit_date=preseason_date. We do this directly
  # because re-extracting the backup fit at a different end_date would
  # mismatch n_pred_fit vs n_pred_data and produce empty
  # team_strengths_quantiles. In production backfill the end_dates align,
  # but the test only exercises the publisher's lookup-and-embed path.
  preseason_dir <- file.path(
    extracts,
    "sport=football", "country=iceland", "sex=male",
    paste0("fit_date=", format(preseason_date, "%Y-%m-%d"))
  )
  dir.create(preseason_dir, recursive = TRUE)
  ts_with_div <- extracted$BD$team_strengths_quantiles
  ts_with_div$division <- "BD"
  arrow::write_parquet(
    ts_with_div,
    file.path(preseason_dir, "team_strengths_quantiles.parquet")
  )
  pm_with_div <- extracted$BD$predicted_matches
  pm_with_div$division <- "BD"
  arrow::write_parquet(
    pm_with_div,
    file.path(preseason_dir, "predicted_matches.parquet")
  )

  suppressWarnings(
    publish_football_iceland(
      extracted = extracted,
      league = league,
      sex = "male",
      end_date = end_date,
      output_root = out,
      extracts_root = extracts
    )
  )

  ts <- jsonlite::read_json(
    file.path(out, "football", "iceland", "karla-bd", "team_strengths.json")
  )
  expect_true(length(ts$records) > 0L)

  with_preseason <- vapply(
    ts$records,
    function(r) "preseason" %in% names(r) && length(r$preseason) > 0L,
    logical(1)
  )
  expect_true(any(with_preseason))

  sample <- ts$records[[which(with_preseason)[1]]]
  expect_named(sample$preseason, c("median", "lower", "upper"), ignore.order = TRUE)
  expect_type(sample$preseason$median, "double")
  expect_true(sample$preseason$lower <= sample$preseason$median)
  expect_true(sample$preseason$median <= sample$preseason$upper)
})

test_that("publish_football_iceland: meta.json::fit_date reflects extracts partition, not end_date", {
  # The publisher previously stamped meta.json::fit_date with
  # format(end_date, "%Y-%m-%d") -- which lies whenever the latest
  # available extracts are older than `end_date` (e.g. a fit failed so
  # today's publish reads yesterday's extracts). The honest answer is
  # `extracted$fit_date`, the partition the publisher actually read.
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
    sex = "male", end_date = as.Date("2026-04-09")
  )
  expect_equal(extracted$fit_date, as.Date("2026-04-09"))

  suppressWarnings(
    publish_football_iceland(
      extracted = extracted,
      league = league,
      sex = "male",
      end_date = as.Date("2026-04-25"),
      output_root = out
    )
  )

  meta <- jsonlite::read_json(
    file.path(out, "football", "iceland", "karla-bd", "meta.json")
  )
  expect_equal(meta[["fit_date"]], "2026-04-09")
})

test_that("publish_football_iceland: omits preseason when no prior fit exists", {
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

  extracts <- withr::local_tempdir()
  out <- withr::local_tempdir()
  end_date <- as.Date("2026-04-25")

  extracted <- .build_extracted_football_for_test(
    fit, league,
    sex = "male", end_date = end_date
  )

  suppressWarnings(
    publish_football_iceland(
      extracted = extracted,
      league = league,
      sex = "male",
      end_date = end_date,
      output_root = out,
      extracts_root = extracts
    )
  )

  ts <- jsonlite::read_json(
    file.path(out, "football", "iceland", "karla-bd", "team_strengths.json")
  )
  expect_true(length(ts$records) > 0L)
  with_preseason <- vapply(
    ts$records,
    function(r) "preseason" %in% names(r) && length(r$preseason) > 0L,
    logical(1)
  )
  expect_false(any(with_preseason))
})
