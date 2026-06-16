# tests/testthat/test-backtest-market.R
# Model-vs-market de-vig + skill scoring. The de-vig correctness traps (group by
# (match, market, line); completeness; pushes; the 3-way spread/moneyline) are
# pinned here so they can never silently corrupt the report's headline number.

ml_group <- function(p, odds, win, sex = "male", md = as.Date("2026-05-08"),
                     home = "A", away = "B") {
  tibble::tibble(
    sport = "football", country = "iceland", sex = sex,
    match_date = md, home_team = home, away_team = away,
    market = "moneyline", outcome = c("home", "draw", "away"), line = NA_real_,
    p = p, odds = odds, win = win
  )
}

test_that("bt_devig normalises 1/odds to a probability within each market group", {
  # moneyline 3-way; odds imply 0.5/0.2857/0.25 = overround ~1.0357
  b <- ml_group(p = c(0.5, 0.3, 0.2), odds = c(2.0, 3.5, 4.0), win = c(TRUE, FALSE, FALSE))
  d <- bt_devig(b)
  expect_equal(nrow(d), 3L)
  expect_equal(sum(d$q_market), 1, tolerance = 1e-9)
  expect_equal(d$q_market[1], (1 / 2.0) / (1 / 2.0 + 1 / 3.5 + 1 / 4.0), tolerance = 1e-9)
  expect_equal(unique(round(d$overround, 4)), round(1 / 2.0 + 1 / 3.5 + 1 / 4.0, 4))
  expect_equal(d$y, c(1, 0, 0))
})

test_that("bt_devig keys de-vig on (match, market, line) so different lines never pool", {
  # Two total lines for ONE match. If `line` were dropped, all 4 rows would
  # normalise together and every q_market would be ~halved.
  b <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male",
    match_date = as.Date("2026-05-08"), home_team = "A", away_team = "B",
    market = "total",
    outcome = c("over", "under", "over", "under"),
    line = c(2.5, 2.5, 3.5, 3.5),
    p = c(0.6, 0.4, 0.4, 0.6),
    odds = c(1.8, 2.0, 2.5, 1.5),
    win = c(TRUE, FALSE, FALSE, TRUE) # total = 3 goals
  )
  d <- bt_devig(b)
  expect_equal(nrow(d), 4L)
  sums <- d |>
    dplyr::group_by(line) |>
    dplyr::summarise(s = sum(q_market), .groups = "drop")
  expect_true(all(abs(sums$s - 1) < 1e-9)) # each line sums to 1 on its own
})

test_that("bt_devig keeps the 3-way SPREAD intact (home/draw/away under the handicap)", {
  b <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male",
    match_date = as.Date("2026-05-08"), home_team = "A", away_team = "B",
    market = "spread", outcome = c("home", "draw", "away"), line = -1,
    p = c(0.4, 0.2, 0.4), odds = c(2.6, 4.9, 2.4),
    win = c(TRUE, FALSE, FALSE)
  )
  d <- bt_devig(b)
  expect_equal(nrow(d), 3L)
  expect_equal(sum(d$q_market), 1, tolerance = 1e-9)
})

test_that("bt_devig drops INCOMPLETE groups (model p does not sum to 1 -> missing outcome)", {
  # moneyline with the draw outcome missing: model p sums to 0.7, so a fair
  # de-vig is impossible -> the whole group is dropped.
  b <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male",
    match_date = as.Date("2026-05-08"), home_team = "A", away_team = "B",
    market = "moneyline", outcome = c("home", "away"), line = NA_real_,
    p = c(0.5, 0.2), odds = c(2.0, 4.0), win = c(TRUE, FALSE)
  )
  expect_equal(nrow(bt_devig(b)), 0L)
})

test_that("bt_devig drops PUSH groups (integer total line landing exactly -> no winner)", {
  # total line 3, match ends 2-1 (total 3): strict over/under both FALSE -> push.
  b <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male",
    match_date = as.Date("2026-05-08"), home_team = "A", away_team = "B",
    market = "total", outcome = c("over", "under"), line = 3,
    p = c(0.55, 0.45), odds = c(1.9, 1.9), win = c(FALSE, FALSE)
  )
  expect_equal(nrow(bt_devig(b)), 0L)
})

test_that("bt_devig drops unsettled rows before grouping", {
  b <- ml_group(p = c(0.5, 0.3, 0.2), odds = c(2.0, 3.5, 4.0), win = c(TRUE, NA, FALSE))
  # one outcome unsettled -> group is no longer a valid single-winner book -> dropped
  expect_equal(nrow(bt_devig(b)), 0L)
})

test_that("bt_skill reports model + market Brier/log-loss and the Brier skill score", {
  # Two complete moneyline matches; model is a bit sharper than the (de-vigged) market.
  b <- dplyr::bind_rows(
    ml_group(p = c(0.6, 0.25, 0.15), odds = c(1.7, 4.0, 6.0), win = c(TRUE, FALSE, FALSE), home = "A", away = "B"),
    ml_group(
      p = c(0.3, 0.3, 0.4), odds = c(3.2, 3.4, 2.2), win = c(FALSE, FALSE, TRUE), home = "C", away = "D",
      md = as.Date("2026-05-09")
    )
  )
  d <- bt_devig(b)
  s <- bt_skill(d)
  expect_equal(s$n, 6L)
  expect_equal(s$n_matches, 2L)
  expect_equal(s$brier_model, mean((d$p - d$y)^2), tolerance = 1e-9)
  expect_equal(s$brier_market, mean((d$q_market - d$y)^2), tolerance = 1e-9)
  expect_equal(s$brier_skill, 1 - s$brier_model / s$brier_market, tolerance = 1e-9)
  expect_true(is.finite(s$logloss_model) && is.finite(s$logloss_market))
})

test_that("bt_skill by = 'market' returns one row per market", {
  b <- dplyr::bind_rows(
    ml_group(p = c(0.6, 0.25, 0.15), odds = c(1.7, 4.0, 6.0), win = c(TRUE, FALSE, FALSE)),
    tibble::tibble(
      sport = "football", country = "iceland", sex = "male",
      match_date = as.Date("2026-05-08"), home_team = "A", away_team = "B",
      market = "total", outcome = c("over", "under"), line = 2.5,
      p = c(0.6, 0.4), odds = c(1.8, 2.0), win = c(TRUE, FALSE)
    )
  )
  d <- bt_devig(b)
  s <- bt_skill(d, by = "market")
  expect_setequal(s$market, c("moneyline", "total"))
  expect_equal(nrow(s), 2L)
})

test_that("bt_skill_ci returns a match-clustered (lo, mid, hi) interval on the Brier skill", {
  b <- dplyr::bind_rows(lapply(1:6, function(i) {
    ml_group(
      p = c(0.55, 0.25, 0.20), odds = c(1.9, 3.8, 4.2),
      win = c(i %% 2 == 0, FALSE, i %% 2 == 1),
      home = paste0("H", i), away = paste0("A", i),
      md = as.Date("2026-05-08") + i
    )
  }))
  d <- bt_devig(b)
  ci <- bt_skill_ci(d, R = 200, seed = 1)
  expect_length(ci, 3L)
  expect_true(ci[1] <= ci[2] && ci[2] <= ci[3])
})
