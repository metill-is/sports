mini_beliefs <- function(n_draws = 1000L) {
  tibble::tibble(
    draw_id    = seq_len(n_draws),
    home_goals = rpois(n_draws, lambda = 1.5),
    away_goals = rpois(n_draws, lambda = 1.0)
  )
}

test_that("build_return_matrix produces an S x B numeric matrix", {
  set.seed(11)
  draws <- mini_beliefs(500L)
  bets <- tibble::tibble(
    market  = c("moneyline", "moneyline", "moneyline"),
    outcome = c("home", "draw", "away"),
    line    = NA_real_,
    odds    = c(2.0, 3.5, 4.2)
  )
  R <- build_return_matrix(draws, bets)
  expect_equal(dim(R), c(500L, 3L))
  # Each entry is either -1 (lose) or odds-1 (win)
  expect_true(all(R %in% c(-1, bets$odds - 1)))
})

test_that("kelly_joint returns kelly_raw weights summing within max_stake", {
  set.seed(7)
  draws <- mini_beliefs(2000L)
  # Bets where home is favourite at slight ev>0
  bets <- tibble::tibble(
    market  = c("moneyline", "moneyline", "moneyline"),
    outcome = c("home", "draw", "away"),
    line    = NA_real_,
    odds    = c(1.85, 4.0, 4.5)
  )
  out <- kelly_joint(draws, bets, max_match_stake = 1.0, ev_threshold = 0.0)
  expect_named(out, c("bets", "match_pnl", "match_kelly_sum", "diagnostics"),
    ignore.order = TRUE
  )
  expect_named(out$bets, c(
    "market", "outcome", "line", "odds",
    "p", "ev", "kelly_raw"
  ), ignore.order = TRUE)
  expect_lte(sum(pmax(out$bets$kelly_raw, 0)), 1.0 + 1e-6)
  expect_equal(length(out$match_pnl), nrow(draws))
})

test_that("kelly_joint zeroes bets below ev_threshold", {
  set.seed(7)
  draws <- mini_beliefs(2000L)
  # All bets priced rich; ev clearly negative
  bets <- tibble::tibble(
    market  = "moneyline",
    outcome = c("home", "draw", "away"),
    line    = NA_real_,
    odds    = c(1.10, 1.10, 1.10)
  )
  out <- kelly_joint(draws, bets, ev_threshold = 0.0)
  expect_true(all(out$bets$kelly_raw <= 1e-6))
})

test_that("kelly_joint handles spread bets via line argument", {
  set.seed(7)
  draws <- mini_beliefs(2000L)
  bets <- tibble::tibble(
    market  = c("spread", "spread"),
    outcome = c("home", "away"),
    line    = c(-1.5, 1.5),
    odds    = c(1.95, 1.95)
  )
  out <- kelly_joint(draws, bets, max_match_stake = 1.0)
  # Two bets are exactly mutually exclusive (no tie possible at .5 line)
  # Sum of probabilities across them should equal 1.
  expect_lte(abs(sum(out$bets$p) - 1.0), 0.01)
})

test_that("kelly_joint handles totals (over/under) via line", {
  set.seed(7)
  draws <- mini_beliefs(2000L)
  bets <- tibble::tibble(
    market  = c("total", "total"),
    outcome = c("over", "under"),
    line    = c(2.5, 2.5),
    odds    = c(2.0, 1.85)
  )
  out <- kelly_joint(draws, bets, max_match_stake = 1.0)
  expect_lte(abs(sum(out$bets$p) - 1.0), 0.01)
})

test_that("max_match_stake binds when smaller than unconstrained Kelly", {
  set.seed(42)
  draws <- mini_beliefs(2000L)
  bets <- tibble::tibble(
    market  = c("moneyline", "moneyline"),
    outcome = c("home", "away"),
    line    = NA_real_,
    odds    = c(1.80, 4.50)
  )
  unconstrained <- kelly_joint(draws, bets,
    max_match_stake = 1.0,
    ev_threshold = 0.0
  )
  raw_sum <- sum(unconstrained$bets$kelly_raw)
  expect_gt(raw_sum, 0.0)

  # Tighten the cap to half the unconstrained sum and verify it binds.
  cap <- raw_sum / 2
  bound <- kelly_joint(draws, bets, max_match_stake = cap, ev_threshold = 0.0)
  expect_lte(sum(bound$bets$kelly_raw), cap + 1e-6)
})

test_that("max_match_stake = 0.05 enforces the per-match sum cap", {
  set.seed(42)
  draws <- mini_beliefs(2000L)
  bets <- tibble::tibble(
    market  = c("moneyline", "moneyline"),
    outcome = c("home", "away"),
    line    = NA_real_,
    odds    = c(1.80, 4.50)
  )
  out <- kelly_joint(draws, bets, max_match_stake = 0.05, ev_threshold = 0.0)
  expect_lte(sum(out$bets$kelly_raw), 0.05 + 1e-6)
})
