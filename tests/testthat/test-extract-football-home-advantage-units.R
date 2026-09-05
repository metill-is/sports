# Football's HALF of the B5 units contract (design spec 2026-09-02 section 8).
#
# B5 was a 2DT bug -- football's exp()/halving copied onto basketball and
# handball, whose home_advantage_* are RAW points/goals. The 2DT half is
# asserted in test-extract-2dt-home-advantage-units.R. This file asserts the
# OPPOSITE direction on football, so a "fix" that propagates the wrong way and
# strips football's transform fails too. One test alone only guards one
# direction.
#
# Football parameterises home advantage on the LOG scale (bivariate Poisson,
# Stan/football_iceland/*.stan), so:
#   offence, defence  ->  exp(x)      the multiplier itself
#   total             ->  exp(x / 2)  that multiplier split per side
#
# WHY THIS FILE HAD TO EXIST. Until 2026-09-05 the claim was that
# test-publish-football-golden.R covered this, because it sha256s
# home_advantage.json for all nine football cells. It does not. Its fixture is
# built by build_football_extracts_fixture() (helper-extract-fixtures.R), which
# SYNTHESISES home_advantage_quantiles closed-form as
# `round(0.15 + 0.05 * qnorm(quantile / 100), 4)` and hardcodes
# `model_units = "log_rate"`. No fit is held and no extractor is called.
# Verified by rebinding extract_football_iceland() to a function that
# stop()s and re-running the file: all 21 assertions still pass. The 92 golden
# hashes pin publish_iceland_league(), not the extract layer.
#
# The pull is therefore a named internal, .extract_home_advantage_draws_pfi(),
# rather than a closure inside extract_football_iceland(), for exactly the
# reason its sibling .extract_team_draws_pfi() is: a units guarantee has to be
# reachable by a test.
#
# This file has no skip() by design -- see spec section 4 assertion 7.

fb_ha_stub <- function(off, def, k = 2L, n_draws = 8L) {
  # Constant draws, so every posterior quantile equals the constant and the
  # assertions below are exact rather than a tolerance game.
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

fb_ha_teams <- function(k = 2L) {
  tibble::tibble(team = sprintf("T%02d", seq_len(k)), team_nr = seq_len(k))
}

test_that("football home advantage publishes exp() of the log-rate parameter", {
  teams <- fb_ha_teams()
  out <- .extract_home_advantage_draws_pfi(
    fb_ha_stub(off = 0.20, def = 0.10), teams
  )
  val <- function(comp) unique(out$value[out$component == comp])

  # A realistic Icelandic home edge: exp(0.20) = 1.2214 goals-rate multiplier.
  expect_equal(val("offence"), exp(0.20))
  expect_equal(val("defence"), exp(0.10))
  # The mirror image of the 2DT assertion, which is that BOTH are the identity.
  expect_false(isTRUE(all.equal(val("offence"), 0.20)))
  expect_false(isTRUE(all.equal(val("defence"), 0.10)))

  expect_setequal(names(out), c("team", "component", ".draw", "value"))
  expect_setequal(unique(out$team), teams$team)
  expect_setequal(unique(out$component), c("offence", "defence", "total"))
})

test_that("football's total home advantage is halved BEFORE the exp()", {
  # The /2 splits one log multiplier per side, so it must sit inside the exp().
  # exp((a + b) / 2) is NOT exp(a + b), and it is NOT exp(a) * exp(b) / 2
  # either -- both are what a careless simplification produces.
  teams <- fb_ha_teams()
  out <- .extract_home_advantage_draws_pfi(
    fb_ha_stub(off = 0.20, def = 0.10), teams
  )
  tot <- unique(out$value[out$component == "total"])

  expect_equal(tot, exp(0.30 / 2))
  expect_false(isTRUE(all.equal(tot, exp(0.30))))
  expect_false(isTRUE(all.equal(tot, exp(0.30) / 2)))
  expect_false(isTRUE(all.equal(tot, 0.30)))
})

test_that("the extractor calls the named pull rather than a private closure", {
  # The seam is the point: if extract_football_iceland() grows its own inline
  # copy again, the tests above stop covering what ships.
  fb <- readLines("../../R/extract-football-iceland.R", warn = FALSE)
  expect_true(any(grepl(
    "home_advantage_draws <- .extract_home_advantage_draws_pfi(fit, teams)",
    fb,
    fixed = TRUE
  )))
  expect_false(any(grepl("extract_home_adv <- function", fb, fixed = TRUE)))
})
