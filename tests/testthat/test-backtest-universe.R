# tests/testthat/test-backtest-universe.R
make_cand <- function(stage, ev, market = "moneyline", outcome = "home",
                      line = NA_real_, odds = 2.0, p = 0.55,
                      kelly_raw = 0.1, run_id = "2026-05-01",
                      home = "A", away = "B",
                      sport = "football", country = "iceland", sex = "male") {
  run_ts <- as.POSIXct(run_id, tz = "UTC")
  tibble::tibble(
    run_id = run_ts, run_date = as.Date(run_id),
    sport = sport, country = country, sex = sex,
    match_date = as.Date("2026-05-02"),
    home_team = home, away_team = away,
    market = market, outcome = outcome, line = line,
    p = p, odds = odds, ev = ev, kelly_raw = kelly_raw, stage = stage
  )
}

with_universe_fixture <- function(cand, recs = NULL, code) {
  root <- withr::local_tempdir(.local_envir = parent.frame())
  write_table(cand, "candidates", root = root)
  if (!is.null(recs)) write_table(recs, "recommendations", root = root)
  code(root)
}

test_that("bt_load_universe strategy='kept' keeps only kept rows", {
  cand <- dplyr::bind_rows(
    make_cand("kept", ev = 0.30),
    make_cand("dropped_low_ev", ev = 0.01, outcome = "away"),
    make_cand("dropped_min_bet", ev = 0.05, outcome = "draw")
  )
  with_universe_fixture(cand, code = function(root) {
    u <- bt_load_universe(root = root, strategy = "kept")
    expect_equal(nrow(u), 1L)
    expect_equal(u$stage, "kept")
    expect_equal(u$strategy, "kept")
  })
})

test_that("bt_load_universe dedups a bet kept across run_dates to its earliest placement", {
  cand <- dplyr::bind_rows(
    make_cand("kept", ev = 0.30, run_id = "2026-05-01", odds = 2.0),
    make_cand("kept", ev = 0.28, run_id = "2026-05-02", odds = 2.1)
  )
  with_universe_fixture(cand, code = function(root) {
    u <- bt_load_universe(root = root, strategy = "kept")
    expect_equal(nrow(u), 1L)
    expect_equal(u$run_date, as.Date("2026-05-01"))
    expect_equal(u$odds, 2.0)
  })
})

test_that("bt_load_universe leagues filter scopes to the named sport (football only)", {
  cand <- dplyr::bind_rows(
    make_cand("kept", ev = 0.30, sport = "football", home = "A", away = "B"),
    make_cand("kept", ev = 0.30, sport = "basketball", home = "X", away = "Y")
  )
  with_universe_fixture(cand, code = function(root) {
    u <- bt_load_universe(root = root, strategy = "kept", leagues = "football")
    expect_equal(nrow(u), 1L)
    expect_equal(u$sport, "football")
  })
})

test_that("bt_load_universe strategy='positive_ev' keeps all ev>0 regardless of stage", {
  cand <- dplyr::bind_rows(
    make_cand("kept", ev = 0.30),
    make_cand("dropped_min_bet", ev = 0.05, outcome = "draw"),
    make_cand("dropped_low_ev", ev = -0.02, outcome = "away")
  )
  with_universe_fixture(cand, code = function(root) {
    u <- bt_load_universe(root = root, strategy = "positive_ev")
    expect_equal(nrow(u), 2L)
    expect_true(all(u$ev > 0))
  })
})

test_that("bt_load_universe attaches recorded kelly/bet_amount only to kept bets", {
  cand <- dplyr::bind_rows(
    make_cand("kept", ev = 0.30),
    make_cand("dropped_min_bet", ev = 0.05, outcome = "draw")
  )
  recs <- tibble::tibble(
    run_id = as.POSIXct("2026-05-01", tz = "UTC"),
    run_date = as.Date("2026-05-01"),
    sport = "football", country = "iceland", sex = "male",
    match_date = as.Date("2026-05-02"), home_team = "A", away_team = "B",
    market = "moneyline", outcome = "home", line = NA_real_,
    p = 0.55, odds = 2.0, ev = 0.30, kelly = 0.04, bet_amount = 250
  )
  with_universe_fixture(cand, recs, code = function(root) {
    u <- bt_load_universe(root = root, strategy = "positive_ev")
    kept_row <- u[u$stage == "kept", ]
    drop_row <- u[u$stage == "dropped_min_bet", ]
    expect_equal(kept_row$kelly, 0.04)
    expect_equal(kept_row$bet_amount_recorded, 250)
    expect_true(is.na(drop_row$kelly))
    expect_true(is.na(drop_row$bet_amount_recorded))
  })
})

test_that("bt_load_universe returns empty-with-columns when no candidates", {
  root <- withr::local_tempdir()
  u <- bt_load_universe(root = root)
  expect_equal(nrow(u), 0L)
  expect_true(all(c("run_date", "p", "odds", "kelly", "strategy") %in% names(u)))
})
