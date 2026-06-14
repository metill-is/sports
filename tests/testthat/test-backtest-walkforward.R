# tests/testthat/test-backtest-walkforward.R

test_that("bt_oos_scores computes Brier and log-loss over (p, win)", {
  settled <- tibble::tibble(
    p   = c(0.9, 0.1, 0.5, 0.8),
    win = c(TRUE, FALSE, TRUE, FALSE)
  )
  s <- bt_oos_scores(settled)
  expect_equal(s$n, 4L)
  expect_equal(
    s$brier,
    mean((c(0.9, 0.1, 0.5, 0.8) - c(1, 0, 1, 0))^2)
  )
  expect_equal(
    s$log_loss,
    -mean(c(log(0.9), log(0.9), log(0.5), log(0.2)))
  )
})

test_that("bt_oos_scores clamps p away from 0/1 so log-loss is finite", {
  settled <- tibble::tibble(p = c(0, 1), win = c(FALSE, TRUE))
  s <- bt_oos_scores(settled)
  expect_true(is.finite(s$log_loss))
  expect_true(is.finite(s$brier))
})

test_that("bt_oos_scores returns NA-row for empty input", {
  s <- bt_oos_scores(tibble::tibble(p = numeric(), win = logical()))
  expect_equal(s$n, 0L)
  expect_true(is.na(s$brier))
  expect_true(is.na(s$log_loss))
})

test_that("bt_oos_scores drops rows with NA win (unsettled) before scoring", {
  settled <- tibble::tibble(p = c(0.7, 0.4), win = c(TRUE, NA))
  s <- bt_oos_scores(settled)
  expect_equal(s$n, 1L)
  expect_equal(s$brier, (0.7 - 1)^2)
})

test_that("bt_wf_slice_odds keeps only snapshots at or before d + 12h (ASSERT-ODDS-2)", {
  d <- as.Date("2026-05-20")
  odds <- tibble::tibble(
    sport = "football", country = "iceland",
    scraped_at = as.POSIXct(
      c("2026-05-19 14:00:00", "2026-05-20 10:00:00", "2026-05-21 09:00:00"),
      tz = "UTC"
    ),
    match_date = as.Date("2026-05-22"),
    home_team = "A", away_team = "B",
    market = "moneyline", outcome = "home",
    line = NA_real_, odds = c(2.50, 2.40, 1.90)
  )
  out <- bt_wf_slice_odds(odds, d)
  expect_true(all(out$scraped_at <= as.POSIXct("2026-05-20", tz = "UTC") + lubridate::dhours(12)))
  expect_false(any(out$odds == 1.90))
  expect_equal(nrow(out), 2L)
})

test_that("bt_wf_decide selects the pre-cutoff snapshot, never the post-result one (ASSERT-ODDS-1, the mandated regression test)", {
  d <- as.Date("2026-05-20")
  odds <- tibble::tibble(
    sport = "football", country = "iceland",
    scraped_at = as.POSIXct(
      c("2026-05-19 12:00:00", "2026-05-21 12:00:00"),
      tz = "UTC"
    ),
    match_date = as.Date("2026-05-22"),
    home_team = "A", away_team = "B",
    market = "moneyline", outcome = "home",
    line = NA_real_, odds = c(2.50, 1.90)
  )
  wf_root <- withr::local_tempdir()
  write_table(bt_wf_slice_odds(odds, d), "odds", root = wf_root)
  seen <- prepare_odds(
    list(sport = "football", country = "iceland"), "male",
    end_date = d, max_age_hours = 24 * 365 * 10, root = wf_root
  )
  expect_equal(seen$odds, 2.50)
  expect_false(any(seen$odds == 1.90))
  expect_true(all(seen$scraped_at <= as.POSIXct("2026-05-20", tz = "UTC") + lubridate::dhours(12)))
})

test_that("bt_wf_slice_odds + huge max_age_hours still returns old historical snapshots (ASSERT-ODDS-3, guards the silent-empty trap)", {
  d <- as.Date("2026-05-20")
  odds <- tibble::tibble(
    sport = "football", country = "iceland",
    scraped_at = as.POSIXct("2026-05-18 09:00:00", tz = "UTC"),
    match_date = as.Date("2026-05-22"),
    home_team = "A", away_team = "B",
    market = "moneyline", outcome = "home",
    line = NA_real_, odds = 2.50
  )
  wf_root <- withr::local_tempdir()
  write_table(bt_wf_slice_odds(odds, d), "odds", root = wf_root)
  seen <- prepare_odds(
    list(sport = "football", country = "iceland"), "male",
    end_date = d, max_age_hours = bt_wf_max_age_hours(), root = wf_root
  )
  expect_equal(nrow(seen), 1L)
  expect_equal(seen$odds, 2.50)
})
