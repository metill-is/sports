test_that("ingest_league dispatches to the source named in data_source.results", {
  fake <- list(
    fetch_results = function(league, sex, seasons = NULL) {
      tibble::tibble(
        sport = league$sport, country = league$country, sex = sex,
        season = 2026L,
        match_date = as.Date("2026-01-01"),
        home_team = "A", away_team = "B",
        home_score = 10L, away_score = 8L,
        division = "D1", round = 1L
      )
    },
    fetch_schedule = function(league, sex) {
      tibble::tibble(
        sport = league$sport, country = league$country, sex = sex,
        season = 2026L,
        match_date = as.Date("2026-02-01"),
        home_team = "C", away_team = "D",
        division = "D1", round = 2L
      )
    }
  )

  register_ingest_source("test_stub", fake)
  on.exit(unregister_ingest_source("test_stub"), add = TRUE)

  league <- list(
    sport = "basketball", country = "iceland",
    sexes = c("male"),
    data_source = list(results = "test_stub", schedule = "test_stub")
  )

  tmp <- withr::local_tempdir()
  ingest_league(league, "male", root = tmp)

  r <- read_table("results", root = tmp, filter = list(sport = "basketball"))
  s <- read_table("schedules", root = tmp, filter = list(sport = "basketball"))
  expect_equal(nrow(r), 1L)
  expect_equal(nrow(s), 1L)
  expect_equal(r$home_team, "A")
  expect_equal(s$home_team, "C")
})

test_that("ingest_league errors clearly when the source isn't registered", {
  league <- list(
    sport = "basketball", country = "iceland",
    data_source = list(results = "does_not_exist", schedule = "does_not_exist")
  )
  expect_error(
    ingest_league(league, "male"),
    regexp = "does_not_exist"
  )
})

test_that("ingest_league skips gracefully when the source returns 0 rows", {
  fake <- list(
    fetch_results = function(...) tibble::tibble(),
    fetch_schedule = function(...) tibble::tibble()
  )
  register_ingest_source("empty_stub", fake)
  on.exit(unregister_ingest_source("empty_stub"), add = TRUE)

  league <- list(
    sport = "football", country = "iceland",
    data_source = list(results = "empty_stub", schedule = "empty_stub")
  )
  tmp <- withr::local_tempdir()
  expect_no_error(ingest_league(league, "male", root = tmp))
  expect_equal(nrow(read_table("results", root = tmp)), 0L)
})
