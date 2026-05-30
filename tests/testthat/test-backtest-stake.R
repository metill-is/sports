# tests/testthat/test-backtest-stake.R
make_settled_bet <- function(win, odds = 2.0, kelly = NA_real_,
                             kelly_raw = 0.1, run_id = "2026-05-01",
                             match_date = as.Date("2026-05-02")) {
  tibble::tibble(
    run_id = run_id, run_date = as.Date(run_id),
    match_date = match_date, odds = odds,
    kelly_raw = kelly_raw, kelly = kelly, win = win
  )
}

test_that("bt_effective_fraction uses recorded kelly for kept bets", {
  u <- make_settled_bet(win = TRUE, kelly = 0.04, kelly_raw = 0.2)
  expect_equal(bt_effective_fraction(u), 0.04)
})

test_that("bt_effective_fraction estimates counterfactual frac via per-run shrink", {
  u <- dplyr::bind_rows(
    make_settled_bet(win = TRUE, kelly = 0.05, kelly_raw = 0.10), # shrink 0.5
    make_settled_bet(win = FALSE, kelly = NA_real_, kelly_raw = 0.20)
  )
  fr <- bt_effective_fraction(u)
  expect_equal(fr[1], 0.05)
  expect_equal(fr[2], 0.20 * 0.5) # kelly_raw * per-run median(kelly/kelly_raw)
})

test_that("stake_fixed sizes off a constant pool and computes pnl from win", {
  u <- dplyr::bind_rows(
    make_settled_bet(win = TRUE, kelly = 0.04, odds = 3.0),
    make_settled_bet(win = FALSE, kelly = 0.04, odds = 2.0)
  )
  out <- stake_fixed(u, ref_pool = 10000)
  expect_equal(out$stake, c(400, 400)) # 0.04 * 10000
  expect_equal(out$pnl, c(400 * (3.0 - 1), -400)) # win: stake*(odds-1); loss: -stake
  expect_true(all(out$pool_before == 10000))
})

test_that("stake_rolling compounds the pool across runs by match settlement date", {
  # Run 1 (run_date 05-01, match 05-02): one kept bet, kelly 0.10, odds 2.0, WIN.
  #   pool_1 = 10000 -> stake 1000 -> pnl +1000 (settles 05-02).
  # Run 2 (run_date 05-03, match 05-04): one kept bet, kelly 0.10, odds 2.0, LOSS.
  #   pool_2 = 10000 + 1000 (05-02 settled before 05-03) = 11000 -> stake 1100 -> pnl -1100.
  u <- dplyr::bind_rows(
    make_settled_bet(
      win = TRUE, kelly = 0.10, odds = 2.0,
      run_id = "2026-05-01", match_date = as.Date("2026-05-02")
    ),
    make_settled_bet(
      win = FALSE, kelly = 0.10, odds = 2.0,
      run_id = "2026-05-03", match_date = as.Date("2026-05-04")
    )
  )
  out <- stake_rolling(u,
    initial_pool = 10000,
    daily_budget_frac = 1.0, daily_budget_min_isk = 0
  )
  expect_equal(out$pool_before, c(10000, 11000))
  expect_equal(out$stake, c(1000, 1100))
  expect_equal(out$pnl, c(1000, -1100))
})

test_that("stake_rolling applies the daily-budget cap to an over-budget slate", {
  # Two kept bets same run, each kelly 0.10 of 10000 = 1000, total 2000.
  # Cap = max(0.05*10000, 1000) = 1000 -> scale both by 1000/2000 = 0.5 -> 500 each.
  u <- dplyr::bind_rows(
    make_settled_bet(win = TRUE, kelly = 0.10, odds = 2.0, run_id = "2026-05-01"),
    make_settled_bet(win = TRUE, kelly = 0.10, odds = 2.0, run_id = "2026-05-01")
  )
  out <- stake_rolling(u,
    initial_pool = 10000,
    daily_budget_frac = 0.05, daily_budget_min_isk = 1000
  )
  expect_equal(out$stake, c(500, 500))
})
