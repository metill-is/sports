suppressPackageStartupMessages({
  library(here)
  library(testthat)
  library(dplyr)
  library(tibble)
  library(arrow)
})

# The Phase 1 stub had no tests. Phase 2 makes in_sample mode real.
# These tests exercise the filter logic directly: build a small parquet
# archive in a temp dir, read it via read_predictions_archive (which
# supports an archive-root override by setting sports_dir to the temp),
# and verify the correct rows survive each mode's date guard.

source(here::here("R", "storage", "store.R"))

build_fixture_archive <- function(sports_dir) {
  # Two in_sample rows at one match, two oos rows at another.
  in_rows <- tibble(
    iteration = 1:2,
    game_nr = 1L,
    division = 1L,
    date = as.Date("2026-04-15"),
    home = "Fram",
    away = "IA",
    home_goals = c(2.0, 1.0),
    away_goals = c(1.0, 0.0),
    scope = "in_sample"
  )
  oos_rows <- tibble(
    iteration = 1:2,
    game_nr = 2L,
    division = 1L,
    date = as.Date("2026-04-25"),
    home = "KA",
    away = "IBV",
    home_goals = c(1.5, 3.0),
    away_goals = c(2.0, 0.5),
    scope = "oos"
  )

  dir_path <- file.path(
    sports_dir, "store", "predictions_archive",
    "sport=football", "country=iceland", "sex=male",
    "variant=v3_free_nu", "fit_date=2026-04-21"
  )
  dir.create(dir_path, recursive = TRUE)
  arrow::write_parquet(
    bind_rows(in_rows, oos_rows),
    file.path(dir_path, "predictions.parquet")
  )
}

test_that("load_variant_predictions in_sample returns rows where fit_date >= match_date", {
  tmp <- tempfile("sports_")
  dir.create(tmp)
  build_fixture_archive(tmp)

  source(here::here("R", "backtest", "betting_pnl", "predictions_load.R"))
  result <- load_variant_predictions(
    sports_dir = tmp,
    variants = c("v3_free_nu"),
    mode = "in_sample"
  )

  # In-sample fixture: match 2026-04-15 <= fit 2026-04-21 -> passes date guard.
  # 1x2 (3 outcomes) + totals (6 lines x 2) + handicap (4 lines x 2) = 23 rows.
  expect_equal(nrow(result), 23)
  expect_true(all(result$match_id == "2026-04-15|Fram|IA"))
  expect_setequal(unique(result$market), c("1x2", "totals", "handicap"))
})

test_that("load_variant_predictions oos returns rows where fit_date < match_date", {
  tmp <- tempfile("sports_")
  dir.create(tmp)
  build_fixture_archive(tmp)

  source(here::here("R", "backtest", "betting_pnl", "predictions_load.R"))
  result <- load_variant_predictions(
    sports_dir = tmp,
    variants = c("v3_free_nu"),
    mode = "oos"
  )

  # OOS fixture: match 2026-04-25 > fit 2026-04-21 -> passes date guard.
  expect_equal(nrow(result), 23)
  expect_true(all(result$match_id == "2026-04-25|KA|IBV"))
})

test_that("load_variant_predictions with empty archive returns empty frame", {
  tmp <- tempfile("sports_empty_")
  dir.create(tmp)
  # Don't build anything - archive root doesn't exist.

  source(here::here("R", "backtest", "betting_pnl", "predictions_load.R"))
  suppressWarnings(
    result <- load_variant_predictions(
      sports_dir = tmp,
      variants = c("v3_free_nu"),
      mode = "oos"
    )
  )
  expect_equal(nrow(result), 0)
  expect_setequal(
    names(result),
    c("variant", "match_id", "market", "outcome", "line", "p_model")
  )
})
