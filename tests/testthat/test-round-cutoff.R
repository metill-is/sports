test_that("compute_round_cutoff_date returns the latest Nth-match date", {
  results <- tibble::tibble(
    season = 2026L,
    division = "BD",
    home_team = c("A", "B", "A", "B", "C", "D"),
    away_team = c("B", "A", "C", "D", "A", "B"),
    match_date = as.Date(c(
      "2026-04-01", "2026-04-08", "2026-04-15",
      "2026-04-15", "2026-04-22", "2026-04-22"
    ))
  )

  expect_equal(
    compute_round_cutoff_date(results, season = 2026, round_cutoff = 1),
    as.Date("2026-04-15")
  )
  expect_equal(
    compute_round_cutoff_date(results, season = 2026, round_cutoff = 2),
    as.Date("2026-04-22")
  )
})

test_that("compute_round_cutoff_date returns NULL when round incomplete", {
  results <- tibble::tibble(
    season = 2026L,
    division = "BD",
    home_team = c("A", "B"),
    away_team = c("B", "C"),
    match_date = as.Date(c("2026-04-01", "2026-04-08"))
  )

  out <- suppressMessages(
    compute_round_cutoff_date(results, season = 2026, round_cutoff = 2)
  )
  expect_null(out)
})

test_that("compute_round_cutoff_date ignores non-top-division matches", {
  results <- tibble::tibble(
    season = 2026L,
    division = c("BD", "BD", "LD1", "CUP"),
    home_team = c("A", "B", "X", "Y"),
    away_team = c("B", "A", "Y", "X"),
    match_date = as.Date(c(
      "2026-04-01", "2026-04-08", "2026-03-01", "2026-02-01"
    ))
  )

  expect_equal(
    compute_round_cutoff_date(
      results,
      season = 2026, round_cutoff = 1, top_division = "BD"
    ),
    as.Date("2026-04-01")
  )
  expect_equal(
    compute_round_cutoff_date(
      results,
      season = 2026, round_cutoff = 2, top_division = "BD"
    ),
    as.Date("2026-04-08")
  )
})

test_that("compute_round_cutoff_date returns NULL when top division absent", {
  results <- tibble::tibble(
    season = 2026L,
    division = "LD1",
    home_team = "A", away_team = "B",
    match_date = as.Date("2026-04-01")
  )

  out <- suppressMessages(
    compute_round_cutoff_date(results, season = 2026, round_cutoff = 1)
  )
  expect_null(out)
})

test_that("compute_round_cutoff_date filters by season", {
  results <- tibble::tibble(
    season = c(2025L, 2025L, 2026L, 2026L),
    division = "BD",
    home_team = c("A", "B", "A", "B"),
    away_team = c("B", "A", "B", "A"),
    match_date = as.Date(c(
      "2025-04-01", "2025-04-08",
      "2026-04-01", "2026-04-08"
    ))
  )

  expect_equal(
    compute_round_cutoff_date(results, season = 2025, round_cutoff = 1),
    as.Date("2025-04-01")
  )
  expect_equal(
    compute_round_cutoff_date(results, season = 2026, round_cutoff = 1),
    as.Date("2026-04-01")
  )
})
