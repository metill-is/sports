test_that("write_table rejects negative result scores", {
  root <- withr::local_tempdir()
  bad <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male", season = 2026L,
    match_date = as.Date("2026-05-01"), home_team = "A", away_team = "B",
    home_score = -1L, away_score = 2L, division = "BD", round = 1L
  )
  expect_error(write_table(bad, "results", root = root), "home_score")
})

test_that("write_table rejects an implausibly high result score", {
  root <- withr::local_tempdir()
  bad <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male", season = 2026L,
    match_date = as.Date("2026-05-01"), home_team = "A", away_team = "B",
    home_score = 99L, away_score = 0L, division = "BD", round = 1L
  )
  expect_error(write_table(bad, "results", root = root), "cap")
})

test_that("write_table rejects odds <= 1 or non-finite", {
  root <- withr::local_tempdir()
  bad <- tibble::tibble(
    sport = "football", country = "iceland", scraped_at = Sys.time(),
    match_date = as.Date("2026-05-01"), home_team = "A", away_team = "B",
    market = "moneyline", outcome = "home", line = NA_real_, odds = 0.8
  )
  expect_error(write_table(bad, "odds", root = root), "odds")
})

test_that("write_table accepts valid scores, generous caps, and NA scores", {
  root <- withr::local_tempdir()
  ok <- tibble::tibble(
    sport = "basketball", country = "iceland", sex = "male", season = 2026L,
    match_date = as.Date("2026-05-01"),
    home_team = c("A", "C"), away_team = c("B", "D"),
    home_score = c(190L, NA_integer_), away_score = c(85L, NA_integer_),
    division = "BD", round = c(1L, 2L)
  )
  expect_invisible(write_table(ok, "results", root = root))
})

test_that("validate_values is a no-op for tables without value rules", {
  expect_invisible(validate_values(tibble::tibble(x = 1), "beliefs_latest"))
})

test_that("validate_values flags out-of-range recommendation p", {
  bad <- tibble::tibble(p = c(0.4, 1.4), odds = c(2, 2))
  expect_gt(length(check_recommendation_values(bad)), 0L)
  ok <- tibble::tibble(p = c(0, 1, 0.5), odds = c(2, 2, 2))
  expect_length(check_recommendation_values(ok), 0L)
})
