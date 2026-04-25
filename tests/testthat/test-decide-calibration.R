# Helper: write a tiny ledger to a temp root.
setup_ledger <- function(rows) {
  tmp <- withr::local_tempdir(.local_envir = parent.frame())
  write_table(rows, "ledger", root = tmp)
  tmp
}

ledger_row <- function(sport = "basketball", country = "iceland", sex = "male",
                       win = TRUE, p = 0.6, settled = TRUE) {
  tibble::tibble(
    placed_at = Sys.time(),
    match_date = Sys.Date(),
    sport = sport, country = country, sex = sex,
    home_team = "A", away_team = "B",
    market = "moneyline", outcome = "home",
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
