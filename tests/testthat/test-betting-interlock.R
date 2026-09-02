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
