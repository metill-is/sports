# tests/testthat/test-model-integration.R
# Integration test — after scripts/fit_all.R runs, beliefs/latest/ and
# beliefs/archive/ should be populated for all 3 active Icelandic leagues.
# Skips cleanly when data/beliefs/latest/ is absent (fresh clones, CI).

skip_if_no_beliefs <- function() {
  if (!dir.exists(here::here("data", "beliefs", "latest"))) {
    testthat::skip("data/beliefs/latest absent; fit_all.R hasn't run")
  }
}

test_that("beliefs/latest covers all 3 Icelandic sports x both sexes", {
  skip_if_no_beliefs()
  bl <- read_table("beliefs_latest", filter = list(country = "iceland"))

  counts <- dplyr::count(bl, sport, sex)
  expect_equal(nrow(counts), 6L, label = "3 sports x 2 sexes")
  expect_gt(min(counts$n), 100L)
})

test_that("beliefs columns match schemas()$beliefs_latest exactly", {
  skip_if_no_beliefs()
  bl <- read_table("beliefs_latest", filter = list(country = "iceland"))

  expected <- names(schemas()$beliefs_latest)
  expect_setequal(names(bl), expected)
  expect_s3_class(bl$match_date, "Date")
  expect_s3_class(bl$fit_date, "Date")
  expect_type(bl$draw_id, "integer")
  expect_type(bl$home_goals, "double")
  expect_type(bl$away_goals, "double")
})

test_that("beliefs have physically plausible values per sport", {
  skip_if_no_beliefs()
  bl <- read_table("beliefs_latest", filter = list(country = "iceland"))

  by_sport <- dplyr::summarise(
    dplyr::group_by(bl, sport),
    mean_home = mean(home_goals), mean_away = mean(away_goals), .groups = "drop"
  )

  # Basketball: mean ~50-150 per team
  basket <- by_sport[by_sport$sport == "basketball", , drop = FALSE]
  expect_true(all(basket$mean_home > 50 & basket$mean_home < 150))

  # Handball: mean ~15-45 per team
  handball <- by_sport[by_sport$sport == "handball", , drop = FALSE]
  expect_true(all(handball$mean_home > 15 & handball$mean_home < 45))

  # Football: mean 0-5 per team
  football <- by_sport[by_sport$sport == "football", , drop = FALSE]
  expect_true(all(football$mean_home > 0 & football$mean_home < 5))
})

test_that("beliefs/archive has at least one fit_date partition", {
  skip_if_no_beliefs()
  ba <- read_table("beliefs_archive", filter = list(country = "iceland"))
  # fit_date round-trips as character because Arrow hive partition columns
  # deserialize as strings — typed partition reads are a Plan 4 storage-layer
  # follow-up. For now assert the value is present and parseable.
  expect_gt(length(unique(ba$fit_date)), 0L)
  expect_false(any(is.na(as.Date(ba$fit_date))))
})

test_that("per-match draw count is constant within a (sport, sex) bucket", {
  skip_if_no_beliefs()
  bl <- read_table("beliefs_latest", filter = list(country = "iceland"))

  per_bucket <- bl |>
    dplyr::group_by(sport, sex, match_date, home_team, away_team) |>
    dplyr::summarise(n_draws = dplyr::n(), .groups = "drop") |>
    dplyr::group_by(sport, sex) |>
    dplyr::summarise(n_draws_distinct = dplyr::n_distinct(n_draws), .groups = "drop")

  expect_true(all(per_bucket$n_draws_distinct == 1L))
})
