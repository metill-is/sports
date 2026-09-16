# A cell with no standings rows is either a cup or a league before its first
# match. Only the cup may lose its history: truncating a league's history there
# would erase every earlier season on the first pre-season publish
# (spec 2026-09-16 §9.1, F13).

.seed_history <- function(dir) {
  write_json_consistent(
    list(
      schema_version = 1L,
      records = data.frame(as_of = "2100-04-01", team = "A", season = 2100L)
    ),
    file.path(dir, "standings_history.json"),
    auto_unbox = TRUE, dataframe = "rows", digits = 5, na = "null"
  )
}

test_that("an empty league table keeps the cell's standings history", {
  dir <- withr::local_tempdir()
  .seed_history(dir)
  .write_empty_standings_pfi(
    dir,
    generated_at = "2100-09-16T00:00:00+0000", season = 2101L,
    end_date = as.Date("2100-09-16"), is_cup = FALSE
  )
  kept <- jsonlite::read_json(file.path(dir, "standings_history.json"))
  expect_length(kept$records, 1L)
  st <- jsonlite::read_json(file.path(dir, "standings.json"))
  expect_identical(st$season, 2101L)
  expect_identical(st$as_of, "2100-09-16")
  expect_length(st$rows, 0L)
})

test_that("a cup still truncates its standings history", {
  dir <- withr::local_tempdir()
  .seed_history(dir)
  .write_empty_standings_pfi(
    dir,
    generated_at = "2100-09-16T00:00:00+0000", season = 2100L,
    end_date = as.Date("2100-09-16"), is_cup = TRUE
  )
  cut <- jsonlite::read_json(file.path(dir, "standings_history.json"))
  expect_length(cut$records, 0L)
})

test_that("a league with no history yet still gets an empty history file", {
  dir <- withr::local_tempdir()
  .write_empty_standings_pfi(
    dir,
    generated_at = "2100-09-16T00:00:00+0000", season = 2101L,
    end_date = as.Date("2100-09-16"), is_cup = FALSE
  )
  fresh <- jsonlite::read_json(file.path(dir, "standings_history.json"))
  expect_identical(fresh$schema_version, 1L)
  expect_length(fresh$records, 0L)
})
