# Guards for the 2026-05-30 fail-silent -> fail-loud edits.

test_that("odds_scrape_empty_failure fires only on a total in-season wipe-out", {
  expect_true(odds_scrape_empty_failure(n_inseason = 3, total_rows = 0))
  expect_false(odds_scrape_empty_failure(n_inseason = 3, total_rows = 5))
  expect_false(odds_scrape_empty_failure(n_inseason = 0, total_rows = 0))
  expect_false(odds_scrape_empty_failure(n_inseason = 3, total_rows = 0, force = TRUE))
})

test_that("extract_partition_exists detects an existing fit_date partition", {
  root <- withr::local_tempdir()
  cell <- file.path(
    root, "sport=football", "country=iceland", "sex=male", "fit_date=2026-05-30"
  )
  dir.create(cell, recursive = TRUE)
  expect_true(extract_partition_exists(root, "football", "iceland", "male"))
  expect_false(extract_partition_exists(root, "football", "iceland", "female"))
})

test_that("generate_active_competitions marks degraded on schedule fail-open", {
  root <- withr::local_tempdir() # no schedules dir -> fail-open
  leagues <- list(x = list(sport = "football", country = "iceland"))
  out <- withr::local_tempfile(fileext = ".json")
  generate_active_competitions(leagues, root = root, out_path = out)
  payload <- jsonlite::read_json(out)
  expect_true(payload$degraded)
  expect_true(payload$active$x)
})

test_that("generate_active_competitions is not degraded when schedules are present", {
  root <- withr::local_tempdir()
  sched <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male", season = 2026L,
    match_date = Sys.Date() + 2L, home_team = "A", away_team = "B",
    division = "BD", round = 1L, kickoff_time = NA_character_
  )
  write_table(sched, "schedules", root = root)
  leagues <- list(x = list(sport = "football", country = "iceland"))
  out <- withr::local_tempfile(fileext = ".json")
  generate_active_competitions(leagues, root = root, out_path = out)
  payload <- jsonlite::read_json(out)
  expect_false(payload$degraded)
})
