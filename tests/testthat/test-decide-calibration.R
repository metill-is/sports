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
  out <- compute_calibration(league, sex = "male", root = tmp)
  expect_equal(out$multiplier, 1.0)
  # The multiplier alone cannot say "we know nothing" -- the evidence fields do.
  expect_identical(out$n, 0L)
  expect_true(is.na(out$raw_ratio))
  expect_false(out$clamped)
  expect_identical(out$basis, "no_ledger_dir")
})

test_that("compute_calibration separates a partition miss from an absent ledger", {
  # The ledger store exists, but holds nothing for this (sport, country).
  root <- setup_ledger(do.call(rbind, lapply(1:5, function(i) {
    ledger_row(sport = "handball")
  })))
  league <- list(sport = "basketball", country = "iceland")

  out <- compute_calibration(league, sex = "male", root = root)
  expect_equal(out$multiplier, 1.0)
  expect_identical(out$n, 0L)
  expect_identical(out$basis, "no_partition_rows")
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
  expect_equal(out$multiplier, round(37 / 35, 3))
  # raw_ratio is the unrounded, unclamped estimate the multiplier came from.
  expect_equal(out$raw_ratio, 37 / 35)
  expect_identical(out$n, 10L)
  expect_false(out$clamped)
  expect_identical(out$basis, "evidence")
})

test_that("compute_calibration filters by sex", {
  rows <- rbind(
    do.call(rbind, lapply(1:10, function(i) ledger_row(sex = "male", win = TRUE, p = 0.5))),
    do.call(rbind, lapply(1:10, function(i) ledger_row(sex = "female", win = FALSE, p = 0.5)))
  )
  root <- setup_ledger(rows)
  league <- list(sport = "basketball", country = "iceland")

  male <- compute_calibration(league, sex = "male", root = root)
  female <- compute_calibration(league, sex = "female", root = root)
  # Male: all win, p=0.5 -> high multiplier
  expect_gt(male$multiplier, 1.0)
  # Female: all lose, p=0.5 -> low multiplier
  expect_lt(female$multiplier, 1.0)
  # Each cell rests on its own 10 rows, not the pooled 20.
  expect_identical(male$n, 10L)
  expect_identical(female$n, 10L)
})

test_that("compute_calibration clamps to [floor, ceiling]", {
  # 1000 wins, p = 0.05 each -> raw ratio = (30 + 1000) / (30 + 50) = 12.875,
  # clamped to the 1.5 ceiling.
  rows <- do.call(rbind, lapply(1:1000, function(i) ledger_row(win = TRUE, p = 0.05)))
  root <- setup_ledger(rows)
  league <- list(sport = "basketball", country = "iceland")

  # A clamp overrules the ledger's own estimate, so it says so out loud.
  expect_message(
    out <- compute_calibration(league, sex = "male", root = root),
    "clamped"
  )
  expect_equal(out$multiplier, 1.5)
  expect_true(out$clamped)
  expect_equal(out$raw_ratio, 1030 / 80)
  expect_identical(out$n, 1000L)
  expect_identical(out$basis, "evidence")
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
  expect_equal(out$multiplier, round(30 / 33, 3))
  # n counts settled evidence only -- the 5 unsettled rows are not evidence.
  expect_identical(out$n, 5L)
  expect_identical(out$basis, "evidence")
})

test_that("no history, good calibration and a floored estimate are distinguishable", {
  # The reason compute_calibration returns evidence rather than a bare number:
  # on a real-money staking path all three of these hand the caller the SAME
  # multiplier, and only `basis` / `n` / `clamped` say which one it is.
  league <- list(sport = "basketball", country = "iceland")

  # (a) Nothing settled yet -- the ledger has rows, none of them are evidence.
  none_root <- setup_ledger(do.call(rbind, lapply(1:20, function(i) {
    ledger_row(win = TRUE, p = 0.5, settled = FALSE)
  })))
  none <- compute_calibration(league, sex = "male", root = none_root)

  # (b) 100 settled bets at p = 0.5 with 50 wins -> (30 + 50) / (30 + 50) = 1.0
  #     exactly. Genuinely well calibrated on a real sample.
  good_root <- setup_ledger(do.call(rbind, lapply(1:100, function(i) {
    ledger_row(win = (i <= 50), p = 0.5)
  })))
  good <- compute_calibration(league, sex = "male", root = good_root)

  # (c) 100 settled losses -> raw ratio 30/80 = 0.375, clamped UP to a floor of
  #     1.0. Evidence this bad staking at full size is the failure the evidence
  #     record exists to surface.
  bad_root <- setup_ledger(do.call(rbind, lapply(1:100, function(i) {
    ledger_row(win = FALSE, p = 0.5)
  })))
  suppressMessages(
    floored <- compute_calibration(league, sex = "male", root = bad_root, floor = 1.0)
  )

  # Same number out of all three ...
  expect_equal(none$multiplier, 1.0)
  expect_equal(good$multiplier, 1.0)
  expect_equal(floored$multiplier, 1.0)

  # ... and the evidence is the only thing that tells them apart.
  expect_identical(none$basis, "no_settled_rows")
  expect_identical(none$n, 0L)
  expect_true(is.na(none$raw_ratio))
  expect_false(none$clamped)

  expect_identical(good$basis, "evidence")
  expect_identical(good$n, 100L)
  expect_equal(good$raw_ratio, 1.0)
  expect_false(good$clamped)

  expect_identical(floored$basis, "evidence")
  expect_identical(floored$n, 100L)
  expect_equal(floored$raw_ratio, 30 / 80)
  expect_true(floored$clamped)
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
  expect_gt(mny$multiplier, 1.0)
  # Total all-loss: multiplier < 1
  expect_lt(tot$multiplier, 1.0)
  # Aggregate sits between
  expect_gt(agg$multiplier, tot$multiplier)
  expect_lt(agg$multiplier, mny$multiplier)
  # Each split rests on its own 10 rows; the aggregate on all 20.
  expect_identical(mny$n, 10L)
  expect_identical(tot$n, 10L)
  expect_identical(agg$n, 20L)
})

test_that("compute_calibrations returns aggregate when no market crosses K2 threshold", {
  rows <- do.call(rbind, lapply(1:50, function(i) {
    ledger_row(market = "moneyline", win = (i %% 2 == 0L), p = 0.5)
  }))
  root <- setup_ledger(rows)
  league <- list(sport = "basketball", country = "iceland")
  out <- compute_calibrations(league, sex = "male", root = root)
  expect_named(out, "aggregate") # No per-market keys (< 100 settled)
  # Values stay bare numerics for decide_league()'s vapply(..., numeric(1)).
  expect_type(out$aggregate, "double")
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

  # The evidence behind each multiplier rides along so an operator can audit
  # WHY a market was promoted and what its multiplier rests on.
  ev <- attr(out, "evidence")
  expect_setequal(names(ev), c("aggregate", "moneyline"))
  expect_identical(ev$aggregate$basis, "evidence")
  expect_identical(ev$aggregate$n, 150L)
  expect_identical(ev$moneyline$n, 120L)
  expect_equal(ev$moneyline$multiplier, out$moneyline)
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
