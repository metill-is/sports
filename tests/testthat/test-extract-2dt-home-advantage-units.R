# B5 (design spec 2026-09-02 section 8): basketball + handball
# `home_advantage_*` are RAW points / goals, NOT football's log-rate
# multiplier.
#   Stan/basketball_iceland/2d_student_t_scalarsigma.stan:112,116
#     vector<lower = 0>[K] home_advantage_off / _def, prior normal(0, 10),
#     entering the mean linearly (:264).
#   :277  home_advantage_tot = home_advantage_off + home_advantage_def
# Football's bivariate Poisson parameterises home advantage on the LOG scale,
# so exp() recovers a multiplier there. Applying it to an additive sum of raw
# points is meaningless, and the halving is a per-side split of a log
# multiplier that has no analogue on a sum.
#
# Measured against the real stored basketball fit on 2026-09-02: raw
# home_advantage_tot medians span 1.50 .. 12.07 points across teams, which
# exp(x/2) publishes as 2.12 .. 420. The low end is plausible enough to pass
# review while the top of the same column is absurd -- which is why it would
# have shipped.
#
# Since WS8 task 3 the pull and the band are two functions:
# .extract_home_advantage_draws_2dt() reads the posterior,
# .compute_home_advantage_quantiles_2dt() filters and quantiles it. The
# units guarantee is the COMPOSITION, so these call sites exercise both --
# an exp() reintroduced in either half moves the numbers below.
#
# This file has no skip() by design -- see spec section 4 assertion 7.

ha_stub <- function(off, def, k = 2L, n_draws = 8L) {
  # Constant draws: every posterior quantile equals the constant, so a
  # quantile band is an exact assertion rather than a tolerance game.
  blk <- function(v, prefix) {
    m <- matrix(v, nrow = n_draws, ncol = k)
    colnames(m) <- sprintf("%s[%d]", prefix, seq_len(k))
    m
  }
  stub_fit(list(
    home_advantage_off = blk(off, "home_advantage_off"),
    home_advantage_def = blk(def, "home_advantage_def"),
    home_advantage_tot = blk(off + def, "home_advantage_tot"),
    lp__ = matrix(0, nrow = n_draws, ncol = 1L, dimnames = list(NULL, "lp__"))
  ))
}

ha_teams <- function(k = 2L) {
  tibble::tibble(team = sprintf("T%02d", seq_len(k)), team_nr = seq_len(k))
}

test_that("WS2's stub_fit() exposes the $draws(var) contract this file needs", {
  # Pin the contract before depending on it, so a WS2 shape drift fails by
  # name here rather than deep inside pivot_longer().
  fit <- ha_stub(off = 1.5, def = 2.5)
  expect_true(is.function(fit$draws))
  got <- fit$draws("home_advantage_off")
  expect_setequal(
    posterior::variables(got),
    c("home_advantage_off[1]", "home_advantage_off[2]")
  )
  expect_equal(posterior::ndraws(got), 8L)
})

test_that("2DT home advantage publishes raw points, not exp() of them", {
  teams <- ha_teams()
  out <- .compute_home_advantage_quantiles_2dt(
    .extract_home_advantage_draws_2dt(
      ha_stub(off = 1.5, def = 2.5), teams
    ),
    teams["team"]
  )

  val <- function(comp) unique(round(out$value[out$component == comp], 6))

  # offence 1.5, defence 2.5, total 1.5 + 2.5 = 4.0 -- the parameters
  # themselves. Under the bug these publish as exp(1.5) = 4.4817,
  # exp(2.5) = 12.1825 and exp(4.0 / 2) = 7.3891.
  expect_equal(val("offence"), 1.5)
  expect_equal(val("defence"), 2.5)
  expect_equal(val("total"), 4.0)
})

test_that("the total component is not halved", {
  # Guards the /2 independently of the exp(): a fix that dropped only exp()
  # would still publish 2.0 here.
  teams <- ha_teams()
  out <- .compute_home_advantage_quantiles_2dt(
    .extract_home_advantage_draws_2dt(
      ha_stub(off = 3.0, def = 5.0), teams
    ),
    teams["team"]
  )
  expect_equal(unique(round(out$value[out$component == "total"], 6)), 8.0)
})

test_that("a realistic home edge does not publish as an absurd multiplier", {
  # The empirical top of the range: 12.07 raw points published as exp(6.035).
  teams <- ha_teams()
  out <- .compute_home_advantage_quantiles_2dt(
    .extract_home_advantage_draws_2dt(
      ha_stub(off = 6.0, def = 6.07), teams
    ),
    teams["team"]
  )
  tot <- unique(round(out$value[out$component == "total"], 4))
  expect_equal(tot, 12.07)
  expect_lt(tot, 20)  # exp(12.07 / 2) = 419.9
})

# Football regression guard: deliberately NOT a source grep for "exp(".
# The plan proposed one, but this file's own header explains the bug and so
# contains that string; a text guard would be fragile in both directions.
#
# CORRECTION (2026-09-05). This block used to name
# tests/testthat/test-publish-football-golden.R as "the real net", on the
# grounds that it sha256s home_advantage.json for all nine football cells. That
# was FALSE. The golden test builds its input with
# build_football_extracts_fixture() (helper-extract-fixtures.R), which
# SYNTHESISES home_advantage_quantiles closed-form as
# `round(0.15 + 0.05 * qnorm(quantile / 100), 4)` and hardcodes
# `model_units = "log_rate"`. It holds no fit and calls no extractor: rebinding
# extract_football_iceland() to a function that stop()s leaves all 21 of its
# assertions green. Its 92 hashes pin publish_iceland_league() only, so a leak
# of this fix into football's extract layer would NOT have moved them. (The
# closure it cited as `:1638` was in fact at R/extract-football-iceland.R:1525.)
#
# The real net is now executable and lives in
# tests/testthat/test-extract-football-home-advantage-units.R, which asserts
# football's units directly on .extract_home_advantage_draws_pfi(): offence and
# defence are exp(x) and the total is exp(x / 2) -- the mirror image of the
# assertions above, so a fix propagating the wrong way is caught in both
# directions.
test_that("the two sports pull home advantage through separate functions", {
  # Cheap structural assertion that the two sports do not share this code
  # path, so fixing one cannot silently alter the other.
  fb <- readLines("../../R/publish-iceland-league.R", warn = FALSE)
  tdt <- readLines("../../R/extract-iceland-2dt-shared.R", warn = FALSE)
  expect_true(any(grepl(
    ".extract_home_advantage_draws_pfi <- function", fb,
    fixed = TRUE
  )))
  expect_false(any(grepl(
    ".extract_home_advantage_draws_pfi <- function", tdt,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    ".extract_home_advantage_draws_2dt <- function", tdt,
    fixed = TRUE
  )))
  expect_false(any(grepl(
    ".extract_home_advantage_draws_2dt <- function", fb,
    fixed = TRUE
  )))
})
