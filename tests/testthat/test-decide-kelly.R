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

test_that("kelly_joint: spread home/away share one signed line (home handicap)", {
  # parse_match_detail writes the SAME signed line to all outcomes of a spread
  # row. line is the home team's signed handicap (positive = home gets head
  # start). The away outcome is the mirror image, NOT another bet at +line on
  # the away side.
  set.seed(7)
  draws <- mini_beliefs(2000L)
  bets <- tibble::tibble(
    market  = c("spread", "spread"),
    outcome = c("home", "away"),
    line    = c(0.5, 0.5), # half-line: no push, p_home + p_away == 1
    odds    = c(1.95, 1.95)
  )
  out <- kelly_joint(draws, bets, max_match_stake = 1.0)
  expect_lte(abs(sum(out$bets$p) - 1.0), 0.01)
})

test_that("kelly_joint: spread away with positive line means away covers -line", {
  # Posterior: home strong favourite (lambda 2.4 vs 1.1). With line=+3 (home
  # gets +3 head start), the away outcome is "away covers -3" = "away wins by
  # 4+". For this strongly home-favoured posterior that's near-impossible. The
  # bookmaker prices it as long odds; p must be near-zero, not near 0.75 as
  # the pre-fix away formula (ag + line) > hg would compute.
  set.seed(42)
  draws <- tibble::tibble(
    draw_id    = seq_len(4000L),
    home_goals = rpois(4000L, lambda = 2.4),
    away_goals = rpois(4000L, lambda = 1.1)
  )
  bets <- tibble::tibble(
    market  = c("spread", "spread"),
    outcome = c("home", "away"),
    line    = c(3, 3), # SAME line both outcomes (production convention)
    odds    = c(1.02, 15.73)
  )
  out <- kelly_joint(draws, bets, ev_threshold = 0.0)
  # Home covers +3 (loses by < 4) -- near certain for home favourite
  expect_gt(out$bets$p[[1]], 0.95)
  # Away covers -3 (wins by 4+) -- near zero
  expect_lt(out$bets$p[[2]], 0.05)
  # Therefore the away bet is -EV and gets zero stake
  expect_lt(out$bets$kelly_raw[[2]], 1e-6)
})

test_that("build_return_matrix: tie_threshold = 0 preserves integer-tie semantics", {
  # For integer-goal posterior draws (football/handball), tie_threshold = 0
  # must reproduce the legacy hg==ag draw rule exactly.
  set.seed(13)
  draws <- mini_beliefs(2000L)
  bets <- tibble::tibble(
    market  = c("moneyline", "moneyline", "moneyline"),
    outcome = c("home", "draw", "away"),
    line    = NA_real_,
    odds    = c(2.0, 3.0, 4.0)
  )
  R <- build_return_matrix(draws, bets, tie_threshold = 0)
  p_home <- mean(R[, 1] > 0)
  p_draw <- mean(R[, 2] > 0)
  p_away <- mean(R[, 3] > 0)
  # Three outcomes partition the sample space exactly
  expect_equal(p_home + p_draw + p_away, 1.0, tolerance = 1e-9)
  # And matches the explicit comparison
  expect_equal(p_draw, mean(draws$home_goals == draws$away_goals), tolerance = 1e-9)
})

test_that("build_return_matrix: tie_threshold > 0 catches a band of continuous near-ties", {
  # Handball's bivariate Student-t model produces continuous goal predictions
  # with P(hg == ag) = 0. tie_threshold = 0.5 turns the draw outcome into
  # 'within half a goal' — the moneyline draw market becomes bettable.
  # Verified-dormant bug fix from the 2026-05-15 audit.
  set.seed(17)
  S <- 4000L
  continuous_draws <- tibble::tibble(
    draw_id    = seq_len(S),
    home_goals = stats::rnorm(S, mean = 27, sd = 4),
    away_goals = stats::rnorm(S, mean = 26, sd = 4)
  )
  bets <- tibble::tibble(
    market = "moneyline", outcome = "draw", line = NA_real_, odds = 8.0
  )

  R0 <- build_return_matrix(continuous_draws, bets, tie_threshold = 0)
  p0 <- mean(R0[, 1] > 0)
  R_band <- build_return_matrix(continuous_draws, bets, tie_threshold = 0.5)
  p_band <- mean(R_band[, 1] > 0)

  # Continuous draws never produce hg == ag exactly
  expect_equal(p0, 0)
  # The half-goal band captures meaningful probability mass
  expect_gt(p_band, 0.02)
})

test_that("build_return_matrix: tie_threshold partitions moneyline outcomes correctly", {
  # home / draw / away must partition every draw under the band semantics:
  #   diff > T          ⟺ home win
  #   abs(diff) <= T    ⟺ draw
  #   -diff > T         ⟺ away win
  set.seed(19)
  S <- 3000L
  draws <- tibble::tibble(
    draw_id    = seq_len(S),
    home_goals = stats::rnorm(S, mean = 24, sd = 5),
    away_goals = stats::rnorm(S, mean = 24, sd = 5)
  )
  bets <- tibble::tibble(
    market  = rep("moneyline", 3),
    outcome = c("home", "draw", "away"),
    line    = NA_real_,
    odds    = c(2, 5, 2)
  )
  R <- build_return_matrix(draws, bets, tie_threshold = 0.5)
  outcomes <- cbind(R[, 1] > 0, R[, 2] > 0, R[, 3] > 0)
  # Exactly one of three is TRUE per draw — no draw belongs to two buckets
  expect_true(all(rowSums(outcomes) == 1L))
})

test_that("build_return_matrix: spread away mirrors home under shared line", {
  # Direct unit test on the matrix builder. For any single line value, home
  # and away cover sets must partition the no-push event into mirror halves:
  #   home wins iff (hg + line) > ag
  #   away wins iff (hg + line) < ag     (NOT (ag + line) > hg)
  set.seed(101)
  draws <- mini_beliefs(2000L)
  bets <- tibble::tibble(
    market  = c("spread", "spread"),
    outcome = c("home", "away"),
    line    = c(-1.5, -1.5), # away gets +1.5 head start
    odds    = c(2.0, 2.0)
  )
  R <- build_return_matrix(draws, bets)
  home_win <- R[, 1] > 0
  away_win <- R[, 2] > 0
  # Half-line: never push, so exactly one of home/away wins per draw
  expect_true(all(xor(home_win, away_win)))
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

test_that("validate_bet_inputs rejects non-finite or <= 1 odds and non-finite p", {
  expect_equal(
    validate_bet_inputs(
      odds = c(2.0, 1.0, 0.5, NaN, Inf, 2.0),
      p = c(0.6, 0.6, 0.6, 0.6, 0.6, NaN)
    ),
    c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
  )
})

test_that("kelly_joint does not crash on NaN / Inf / <= 1 odds", {
  draws <- mini_beliefs(500L)
  bets <- tibble::tibble(
    market = rep("moneyline", 2L),
    outcome = c("home", "away"),
    line = NA_real_,
    odds = c(NaN, 1.0)
  )
  expect_no_error(kelly_joint(draws, bets))
})

test_that("kelly_joint quarantines invalid-input bets via diagnostics$valid_input", {
  set.seed(7)
  draws <- mini_beliefs(2000L)
  bets <- tibble::tibble(
    market = rep("moneyline", 3L),
    outcome = c("home", "draw", "away"),
    line = NA_real_,
    odds = c(1.85, 1.0, 0.5)
  )
  out <- kelly_joint(draws, bets, ev_threshold = 0.0)
  expect_equal(out$diagnostics$valid_input, c(TRUE, FALSE, FALSE))
  expect_equal(out$bets$kelly_raw[2:3], c(0, 0))
})

test_that("assert_outcome_prob_coherent aborts when exclusive outcomes sum to > 1", {
  bets <- tibble::tibble(
    market = c("spread", "spread", "spread"), line = c(3, 3, 3),
    outcome = c("home", "draw", "away")
  )
  # The 2026-05-13 spread sign-flip produced sums up to 1.88 -- must abort.
  expect_error(
    assert_outcome_prob_coherent(bets, c(0.85, 0.05, 0.85)),
    "sum to > 1"
  )
  # Exhaustive set summing to 1 is fine; a partial market summing < 1 is fine.
  expect_no_error(assert_outcome_prob_coherent(bets, c(0.60, 0.10, 0.30)))
  expect_no_error(assert_outcome_prob_coherent(
    tibble::tibble(market = "total", line = 2.5, outcome = "over"), 0.55
  ))
})

test_that("build_return_matrix: away on a large positive spread gets near-zero p (sign-flip cannot recur)", {
  set.seed(1)
  beliefs <- tibble::tibble(
    draw_id = 1:600,
    home_goals = rpois(600, 3.0),
    away_goals = rpois(600, 0.4)
  )
  # Away +3 against a strong home favourite: away almost never covers.
  bets <- tibble::tibble(market = "spread", outcome = "away", line = 3, odds = 18)
  R <- build_return_matrix(beliefs, bets, tie_threshold = 0)
  p <- mean(R[, 1] > 0)
  expect_lt(p, 0.05)
  # Negative EV -> dropped at ev_threshold = 0 (the buggy era priced it +EV).
  expect_lt(p * (bets$odds - 1) - (1 - p), 0)
})
