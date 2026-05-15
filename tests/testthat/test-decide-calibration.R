# Helper: write a tiny ledger to a temp root.
setup_ledger <- function(rows) {
  tmp <- withr::local_tempdir(.local_envir = parent.frame())
  write_table(rows, "ledger", root = tmp)
  tmp
}

ledger_row <- function(sport = "basketball", country = "iceland", sex = "male",
                       win = TRUE, p = 0.6, settled = TRUE,
                       market = "moneyline") {
  tibble::tibble(
    placed_at = Sys.time(),
    match_date = Sys.Date(),
    sport = sport, country = country, sex = sex,
    home_team = "A", away_team = "B",
    market = market, outcome = "home",
    line = NA_real_, odds_placed = 1.85,
    p = p, kelly = 0.05, bet_amount = 100,
    settled = settled, win = win,
    pnl = if (isTRUE(win)) 100 * (1.85 - 1) else -100
  )
}

test_that("compute_calibration returns prior_ratio with empty ledger", {
  tmp <- withr::local_tempdir()
  league <- list(sport = "basketball", country = "iceland")
  expect_equal(
    compute_calibration(league, sex = "male", root = tmp),
    1.0
  )
})

test_that("compute_calibration returns the closed-form multiplier", {
  # 10 settled bets, all p = 0.5, 7 wins. Beta-Binomial with prior=30, ratio=1:
  # numerator   = 30*1 + 7   = 37
  # denominator = 30 + 10*0.5 = 35
  # multiplier  = 37/35      ~= 1.057
  rows <- do.call(rbind, lapply(1:10, function(i) {
    ledger_row(win = (i <= 7), p = 0.5)
  }))
  root <- setup_ledger(rows)
  league <- list(sport = "basketball", country = "iceland")

  out <- compute_calibration(league,
    sex = "male", root = root,
    prior_weight = 30, prior_ratio = 1.0,
    floor = 0.5, ceiling = 1.5
  )
  expect_equal(out, round(37 / 35, 3))
})

test_that("compute_calibration filters by sex", {
  rows <- rbind(
    do.call(rbind, lapply(1:10, function(i) ledger_row(sex = "male", win = TRUE, p = 0.5))),
    do.call(rbind, lapply(1:10, function(i) ledger_row(sex = "female", win = FALSE, p = 0.5)))
  )
  root <- setup_ledger(rows)
  league <- list(sport = "basketball", country = "iceland")

  male_mult <- compute_calibration(league, sex = "male", root = root)
  female_mult <- compute_calibration(league, sex = "female", root = root)
  # Male: all win, p=0.5 -> high multiplier
  expect_gt(male_mult, 1.0)
  # Female: all lose, p=0.5 -> low multiplier
  expect_lt(female_mult, 1.0)
})

test_that("compute_calibration clamps to [floor, ceiling]", {
  # 1000 wins, p = 0.05 each -> multiplier ~= 1000 / 50 = 20 -> clamped to 1.5
  rows <- do.call(rbind, lapply(1:1000, function(i) ledger_row(win = TRUE, p = 0.05)))
  root <- setup_ledger(rows)
  league <- list(sport = "basketball", country = "iceland")
  expect_equal(
    compute_calibration(league, sex = "male", root = root),
    1.5
  )
})

test_that("compute_calibration ignores unsettled bets", {
  # 5 settled losses + 5 unsettled "wins" -- only the 5 settled count.
  rows <- rbind(
    do.call(rbind, lapply(1:5, function(i) ledger_row(win = FALSE, p = 0.6, settled = TRUE))),
    do.call(rbind, lapply(1:5, function(i) ledger_row(win = TRUE, p = 0.6, settled = FALSE)))
  )
  root <- setup_ledger(rows)
  league <- list(sport = "basketball", country = "iceland")
  out <- compute_calibration(league, sex = "male", root = root)
  # multiplier = (30*1 + 0) / (30 + 5*0.6) = 30 / 33 = 0.909
  expect_equal(out, round(30 / 33, 3))
})

# ── K2 market-split (audit 2026-05-15 §C) ────────────────────────────────────

test_that("compute_calibration with market= filters to that market only", {
  rows <- rbind(
    do.call(rbind, lapply(1:10, function(i) ledger_row(market = "moneyline", win = TRUE, p = 0.5))),
    do.call(rbind, lapply(1:10, function(i) ledger_row(market = "total", win = FALSE, p = 0.5)))
  )
  root <- setup_ledger(rows)
  league <- list(sport = "basketball", country = "iceland")

  mny <- compute_calibration(league, sex = "male", market = "moneyline", root = root)
  tot <- compute_calibration(league, sex = "male", market = "total", root = root)
  agg <- compute_calibration(league, sex = "male", root = root)
  # Moneyline all-win: multiplier > 1
  expect_gt(mny, 1.0)
  # Total all-loss: multiplier < 1
  expect_lt(tot, 1.0)
  # Aggregate sits between
  expect_gt(agg, tot)
  expect_lt(agg, mny)
})

test_that("compute_calibrations returns aggregate when no market crosses K2 threshold", {
  rows <- do.call(rbind, lapply(1:50, function(i) {
    ledger_row(market = "moneyline", win = (i %% 2 == 0L), p = 0.5)
  }))
  root <- setup_ledger(rows)
  league <- list(sport = "basketball", country = "iceland")
  out <- compute_calibrations(league, sex = "male", root = root)
  expect_named(out, "aggregate") # No per-market keys (< 100 settled)
})

test_that("compute_calibrations promotes a market once it crosses K2_min_n", {
  rows <- rbind(
    do.call(rbind, lapply(1:120, function(i) {
      ledger_row(market = "moneyline", win = (i <= 80), p = 0.5)
    })),
    do.call(rbind, lapply(1:30, function(i) {
      ledger_row(market = "total", win = FALSE, p = 0.5)
    }))
  )
  root <- setup_ledger(rows)
  league <- list(sport = "basketball", country = "iceland")
  out <- compute_calibrations(league, sex = "male", root = root, k2_min_n = 100L)
  expect_true("aggregate" %in% names(out))
  expect_true("moneyline" %in% names(out)) # 120 bets >= 100
  expect_false("total" %in% names(out)) # 30 bets < 100
  # Sanity: per-market moneyline should not equal aggregate (within rounding) —
  # the splits resolve different underlying populations.
  expect_true(abs(out$moneyline - out$aggregate) > 1e-6)
})

test_that("compute_calibrations k2_min_n override re-tunes the split threshold", {
  rows <- do.call(rbind, lapply(1:60, function(i) {
    ledger_row(market = "moneyline", win = (i <= 40), p = 0.5)
  }))
  root <- setup_ledger(rows)
  league <- list(sport = "basketball", country = "iceland")
  # Default 100 threshold: no split (n=60 < 100)
  out_strict <- compute_calibrations(league, sex = "male", root = root)
  expect_named(out_strict, "aggregate")
  # Relaxed to 50: split fires
  out_relaxed <- compute_calibrations(league,
    sex = "male", root = root, k2_min_n = 50L
  )
  expect_true("moneyline" %in% names(out_relaxed))
})
