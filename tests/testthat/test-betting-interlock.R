# WS1 -- betting interlock (spec section 3, decision D2).
# One predicate, four enforcement points: odds ingest, decide, the placer's
# recommendation loader, and the placer's pre-flight validator.

test_that("betting_enabled() defaults to TRUE when the key is absent", {
  expect_true(betting_enabled(list(betting = list(kelly_frac = 0.1))))
})

test_that("betting_enabled() defaults to TRUE when there is no betting block", {
  expect_true(betting_enabled(list(sport = "football", country = "iceland")))
})

test_that("betting_enabled() is FALSE only for an explicit FALSE", {
  expect_false(betting_enabled(list(betting = list(enabled = FALSE))))
  expect_true(betting_enabled(list(betting = list(enabled = TRUE))))
})

test_that("betting_enabled() treats a NULL betting slice as enabled", {
  # decide_one() forwards `betting` verbatim; a NULL slice must not silently
  # disarm a league that meant to bet.
  expect_true(betting_enabled(list(betting = NULL)))
})

# --- the shipped config is disarmed (D2) --------------------------------------

test_that("basketball and handball ship betting-disabled, football does not", {
  lg <- load_leagues()
  expect_false(betting_enabled(lg$basketball_iceland))
  expect_false(betting_enabled(lg$handball_iceland))
  expect_true(betting_enabled(lg$football_iceland))
})

test_that("the disarmed leagues have no Lengjan competitions to scrape", {
  lg <- load_leagues()
  expect_length(lg$basketball_iceland$lengjan$competitions, 0L)
  expect_length(lg$handball_iceland$lengjan$competitions, 0L)
  # Football must still have its full slate.
  expect_gt(length(lg$football_iceland$lengjan$competitions), 0L)
})

test_that("disarming did not clobber the team_names maps", {
  # ID-5: the competitions edit abuts `team_names:`. An off-by-one replacement
  # yields two `team_names:` keys under `lengjan:`; yaml::yaml.load() keeps the
  # last, silently discarding the maps. Guard the content, not the line numbers.
  lg <- load_leagues()
  expect_gt(length(lg$basketball_iceland$lengjan$team_names$male), 0L)
  expect_gt(length(lg$basketball_iceland$lengjan$team_names$female), 0L)
  expect_gt(length(lg$handball_iceland$lengjan$team_names$male), 0L)
})
