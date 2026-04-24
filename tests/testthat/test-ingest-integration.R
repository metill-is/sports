# Integration test — after Plan 2's federation-scraper backfill the
# facts/results store should have match data for all three Icelandic sports.
# Skips cleanly when data/facts/results/ is absent (fresh clones, CI).

skip_if_no_data <- function() {
  if (!dir.exists(here::here("data", "facts", "results"))) {
    testthat::skip("data/facts/results absent; backfill hasn't run")
  }
}

test_that("results covers all 3 Icelandic sports and both sexes", {
  skip_if_no_data()
  r <- read_table("results", filter = list(country = "iceland"))

  expect_gte(nrow(r), 5000L)
  expect_setequal(unique(r$sport), c("basketball", "football", "handball"))
  expect_setequal(unique(r$sex), c("male", "female"))
})

test_that("basketball covers 6 seasons × 2 sexes", {
  skip_if_no_data()
  r <- read_table("results", filter = list(sport = "basketball", country = "iceland"))

  counts <- dplyr::count(r, sex, season)
  # 2021-2026 × male/female = 12 combos; each has > 100 rows
  expect_equal(nrow(counts), 12L)
  expect_gt(min(counts$n), 100L)
})

test_that("football men's 2021-2025 backfill populated", {
  skip_if_no_data()
  r <- read_table("results",
    filter = list(sport = "football", country = "iceland")
  )

  # Men's 2021-2025 historical scrape should yield hundreds per season.
  male_historical <- r[r$sex == "male" & r$season %in% 2021:2025, ]
  counts <- dplyr::count(male_historical, season)
  expect_equal(nrow(counts), 5L)
  expect_gt(min(counts$n), 400L)
})

test_that("handball men's historical (2021-2025) has top-flight coverage", {
  skip_if_no_data()
  r <- read_table("results",
    filter = list(sport = "handball", country = "iceland")
  )

  male_historical <- r[r$sex == "male" & r$season %in% 2021:2025, ]
  expect_gt(nrow(male_historical), 400L)
  # All historical male rows are OD (Olísdeild top flight); div2 + female
  # historical gaps are known (see commit message).
  expect_true("OD" %in% male_historical$division)
})

test_that("match_date is Date and scores are integers", {
  skip_if_no_data()
  r <- read_table("results", filter = list(country = "iceland"))
  expect_s3_class(r$match_date, "Date")
  expect_type(r$home_score, "integer")
  expect_type(r$away_score, "integer")
  expect_true(all(!is.na(r$home_score)))
  expect_true(all(!is.na(r$away_score)))
})

test_that("no duplicate matches per (sport, sex, match_date, home_team, away_team)", {
  skip_if_no_data()
  r <- read_table("results", filter = list(country = "iceland"))
  key <- paste(r$sport, r$sex, r$match_date, r$home_team, r$away_team,
    sep = "||"
  )
  expect_equal(length(key), length(unique(key)))
})
