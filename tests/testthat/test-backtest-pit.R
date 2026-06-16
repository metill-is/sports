# tests/testthat/test-backtest-pit.R

# ---- fixtures ----------------------------------------------------------------

seed_predicted <- function(root, sex, fit_date, pm) {
  dir <- file.path(
    root, "beliefs", "extracts", "sport=football", "country=iceland",
    paste0("sex=", sex), paste0("fit_date=", fit_date)
  )
  fs::dir_create(dir, recurse = TRUE)
  arrow::write_parquet(pm, file.path(dir, "predicted_matches.parquet"))
}

# Uniform pmf over the (0..1) x (0..1) score grid: 1000 draws per cell, 4000 total.
pm_fixture <- function(match_date = "2026-05-20", home = "A", away = "B",
                       division = "BD") {
  tibble::tibble(
    home_team = home, away_team = away,
    match_date = as.Date(match_date),
    home_goals = c(0L, 0L, 1L, 1L),
    away_goals = c(0L, 1L, 0L, 1L),
    count = 1000L, division = division
  )
}

results_fixture <- function(match_date = "2026-05-20", home = "A", away = "B",
                            home_score = 1L, away_score = 1L, division = "BD",
                            sex = "male") {
  tibble::tibble(
    sport = "football", country = "iceland", sex = sex,
    match_date = as.Date(match_date), home_team = home, away_team = away,
    home_score = home_score, away_score = away_score,
    division = division, round = 1L, season = 2026L
  )
}

# ---- Task 1: deterministic PIT core ------------------------------------------

test_that("bt_marginal_value derives total, diff, home, away", {
  expect_equal(bt_marginal_value(2L, 1L, "total"), 3)
  expect_equal(bt_marginal_value(2L, 1L, "diff"), 1)
  expect_equal(bt_marginal_value(2L, 1L, "home"), 2)
  expect_equal(bt_marginal_value(2L, 1L, "away"), 1)
})

test_that("bt_pit_bounds returns the discrete CDF jump [F(y-1), F(y)]", {
  values <- c(0L, 1L, 2L, 3L)
  weights <- c(1000, 1000, 1000, 1000)
  b <- bt_pit_bounds(values, weights, y = 2L)
  expect_equal(b, c(0.5, 0.75))
})

test_that("bt_pit_bounds degenerates to a point when the observed value has no mass", {
  b <- bt_pit_bounds(c(0L, 1L, 2L), c(10, 20, 70), y = 5L)
  expect_equal(b, c(1, 1))
})

test_that("bt_rpit places u inside the [F(y-1), F(y)] band", {
  values <- c(0L, 1L, 2L, 3L)
  weights <- c(1000, 1000, 1000, 1000)
  expect_equal(bt_rpit(values, weights, y = 2L, u = 0), 0.5)
  expect_equal(bt_rpit(values, weights, y = 2L, u = 1), 0.75)
  expect_equal(bt_rpit(values, weights, y = 2L, u = 0.5), 0.625)
})

# ---- Task 2: load predicted extracts -----------------------------------------

test_that("bt_load_predicted reads every fit_date partition with sex + fit_date attached", {
  root <- withr::local_tempdir()
  seed_predicted(root, "male", "2026-05-10", pm_fixture(match_date = "2026-05-12"))
  seed_predicted(root, "male", "2026-05-17", pm_fixture(match_date = "2026-05-20"))
  seed_predicted(root, "female", "2026-05-17", pm_fixture(match_date = "2026-05-20"))

  all <- bt_load_predicted(root, sex = c("male", "female"))
  expect_setequal(unique(all$fit_date), as.Date(c("2026-05-10", "2026-05-17")))
  expect_setequal(unique(all$sex), c("male", "female"))
  expect_true(all(c("home_goals", "away_goals", "count", "division") %in% names(all)))
  expect_equal(sum(all$count), 3L * 4000L)
})

test_that("bt_load_predicted filters to a season and returns the empty schema when absent", {
  root <- withr::local_tempdir()
  seed_predicted(root, "male", "2025-09-01", pm_fixture(match_date = "2025-09-03"))
  seed_predicted(root, "male", "2026-05-17", pm_fixture(match_date = "2026-05-20"))

  out <- bt_load_predicted(root, sex = "male", season = 2026L)
  expect_equal(unique(out$fit_date), as.Date("2026-05-17"))

  empty <- bt_load_predicted(withr::local_tempdir(), sex = "male")
  expect_equal(nrow(empty), 0L)
  expect_true(all(c("sex", "fit_date", "home_goals", "count") %in% names(empty)))
})

# ---- Task 3: leak-free as-of selection ---------------------------------------

test_that("bt_pit_asof keeps only the most recent fit STRICTLY before each match", {
  predicted <- dplyr::bind_rows(
    dplyr::mutate(pm_fixture(match_date = "2026-05-20"), sex = "male", fit_date = as.Date("2026-05-10")),
    dplyr::mutate(pm_fixture(match_date = "2026-05-20"), sex = "male", fit_date = as.Date("2026-05-17")),
    dplyr::mutate(pm_fixture(match_date = "2026-05-20"), sex = "male", fit_date = as.Date("2026-05-21"))
  )
  asof <- bt_pit_asof(predicted)
  expect_equal(unique(asof$fit_date), as.Date("2026-05-17"))
  expect_equal(nrow(asof), 4L)
})

test_that("bt_pit_asof drops a match with no pre-match fit", {
  predicted <- dplyr::mutate(
    pm_fixture(match_date = "2026-05-20"),
    sex = "male", fit_date = as.Date("2026-05-25")
  )
  expect_equal(nrow(bt_pit_asof(predicted)), 0L)
})

# ---- Task 4: per-match randomised-PIT table ----------------------------------

test_that("bt_pit_values computes the as-of randomised PIT per match for a marginal", {
  predicted <- dplyr::mutate(
    pm_fixture(match_date = "2026-05-20"),
    sex = "male", fit_date = as.Date("2026-05-17")
  )
  results <- results_fixture(home_score = 1L, away_score = 1L) # total = 2 -> band [0.75, 1.0]
  pit <- bt_pit_values(predicted, results, marginal = "total", seed = 1L)
  expect_equal(nrow(pit), 1L)
  expect_true(pit$u >= 0.75 && pit$u <= 1.0)
  expect_true(all(c("sex", "division", "u") %in% names(pit)))
})

test_that("bt_pit_values carries division for stratification and returns empty on no overlap", {
  predicted <- dplyr::mutate(
    pm_fixture(match_date = "2026-05-20", division = "LD1"),
    sex = "male", fit_date = as.Date("2026-05-17")
  )
  results <- results_fixture(division = "LD1")
  pit <- bt_pit_values(predicted, results, marginal = "diff", seed = 1L)
  expect_equal(pit$division, "LD1")

  other <- results_fixture(home = "X", away = "Y")
  expect_equal(nrow(bt_pit_values(predicted, other, marginal = "total")), 0L)
})

# ---- Task 5: PIT uniformity --------------------------------------------------

test_that("bt_pit_uniformity summarises u with a KS test, grouped", {
  pit <- tibble::tibble(
    sex = rep(c("male", "female"), each = 50),
    u = c(seq(0.01, 0.99, length.out = 50), seq(0.01, 0.99, length.out = 50))
  )
  s <- bt_pit_uniformity(pit, by = "sex")
  expect_setequal(s$sex, c("male", "female"))
  expect_true(all(c("n", "ks_stat", "ks_p", "mean_u") %in% names(s)))
  expect_equal(s$n, c(50L, 50L))
  expect_true(all(s$mean_u > 0.4 & s$mean_u < 0.6))
})

test_that("bt_pit_uniformity flags a U-shaped histogram with low KS p", {
  u <- c(rep(0.02, 60), rep(0.98, 60))
  s <- bt_pit_uniformity(tibble::tibble(u = u))
  expect_true(s$ks_p < 0.05)
})

# ---- Task 6: draw rate -------------------------------------------------------

test_that("bt_draw_rate compares predicted vs observed draw rate with a gap", {
  predicted <- dplyr::bind_rows(
    dplyr::mutate(pm_fixture(match_date = "2026-05-20", home = "A", away = "B"),
      sex = "male", fit_date = as.Date("2026-05-17")
    ),
    dplyr::mutate(pm_fixture(match_date = "2026-05-20", home = "C", away = "D"),
      sex = "male", fit_date = as.Date("2026-05-17")
    )
  )
  results <- dplyr::bind_rows(
    results_fixture(home = "A", away = "B", home_score = 1L, away_score = 1L), # draw
    results_fixture(home = "C", away = "D", home_score = 2L, away_score = 0L) # not
  )
  dr <- bt_draw_rate(predicted, results, by = "sex")
  expect_equal(dr$predicted_draw_rate, 0.5)
  expect_equal(dr$observed_draw_rate, 0.5)
  expect_equal(dr$gap, 0)
  expect_equal(dr$n, 2L)
})

# ---- Task 7: scoreline residual grid -----------------------------------------

test_that("bt_scoreline_residuals returns observed-minus-predicted cell frequencies", {
  predicted <- dplyr::mutate(
    pm_fixture(match_date = "2026-05-20"),
    sex = "male", fit_date = as.Date("2026-05-17")
  )
  results <- results_fixture(home_score = 1L, away_score = 1L)
  grid <- bt_scoreline_residuals(predicted, results, by = "sex")
  cell_11 <- grid[grid$home_goals == 1 & grid$away_goals == 1, ]
  expect_equal(cell_11$predicted_freq, 0.25)
  expect_equal(cell_11$observed_freq, 1)
  expect_equal(cell_11$residual, 0.75)
  expect_equal(sum(grid$residual), 0, tolerance = 1e-8)
})
