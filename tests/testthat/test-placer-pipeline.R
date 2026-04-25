test_that("place_bets returns 0-row tibble when no recommendations match", {
  tmp <- withr::local_tempdir()
  out <- place_bets(target_date = as.Date("2050-01-01"), root = tmp)
  expect_equal(nrow(out), 0L)
})

test_that("place_bets dry-run with no bets skips chromote entirely", {
  testthat::local_mocked_bindings(
    chromote_login = function(...) stop("Should not have logged in"),
    .package = "sports"
  )
  tmp <- withr::local_tempdir()
  out <- place_bets(
    target_date = as.Date("2050-01-01"),
    root        = tmp,
    dry_run     = TRUE
  )
  expect_equal(nrow(out), 0L)
})

test_that("place_bets respects league filter", {
  tmp <- withr::local_tempdir()
  out <- place_bets(
    leagues     = "football_iceland",
    target_date = as.Date("2050-01-01"),
    root        = tmp
  )
  expect_equal(nrow(out), 0L)
})

test_that("place_bets returns a tibble with the documented status column", {
  tmp <- withr::local_tempdir()
  out <- place_bets(target_date = as.Date("2050-01-01"), root = tmp)
  expected_cols <- c(
    "sport", "country", "sex", "match_date",
    "home_team", "away_team", "market", "outcome",
    "odds", "bet_amount", "status"
  )
  expect_true(all(expected_cols %in% names(out)))
})
