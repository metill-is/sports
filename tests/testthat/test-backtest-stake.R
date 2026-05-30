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
