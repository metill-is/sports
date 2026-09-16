# Season, format and fixtures for the 2DT sports (spec 2026-09-16 §5-§6).

.res <- function(season, division = "BD", date = "2100-03-01") {
  tibble::tibble(
    season = as.integer(season), division = division,
    match_date = as.Date(date), home_team = "A", away_team = "B",
    home_score = 80L, away_score = 70L
  )
}
.sch <- function(season, division = "BD", date) {
  tibble::tibble(
    season = as.integer(season), division = division,
    match_date = as.Date(date), home_team = "A", away_team = "B"
  )
}
.end <- as.Date("2100-09-16")

test_that("a published next season is current before any of it is played", {
  expect_identical(
    .current_season_2dt(.res(2100), .sch(2101, date = "2100-09-29"), .end, "BD"),
    2101L
  )
})

test_that("without a future fixture the last results season stands", {
  expect_identical(.current_season_2dt(.res(2100), NULL, .end, "BD"), 2100L)
  expect_identical(
    .current_season_2dt(.res(2100), .sch(2101, date = "2100-09-16"), .end, "BD"),
    2100L
  )
})

test_that("another division's schedule does not move this one", {
  expect_identical(
    .current_season_2dt(.res(2100, "1D"), .sch(2101, "BD", "2100-09-29"), .end, "1D"),
    2100L
  )
})

test_that("a held division ignores its schedule and never passes its hold", {
  expect_identical(
    .current_season_2dt(.res(2100, "1D"), .sch(2101, "1D", "2100-09-29"), .end, "1D", hold = 2100L),
    2100L
  )
  started <- dplyr::bind_rows(.res(2100, "1D"), .res(2101, "1D", "2100-09-10"))
  expect_identical(
    .current_season_2dt(started, NULL, .end, "1D", hold = 2100L),
    2100L
  )
})

test_that("results after end_date do not count and an empty cell uses the calendar year", {
  expect_identical(
    .current_season_2dt(.res(2101, date = "2100-10-01"), NULL, .end, "BD"),
    2100L
  )
})
