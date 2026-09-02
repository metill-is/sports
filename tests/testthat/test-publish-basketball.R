# Argument validation only.
#
# This file used to carry five more blocks that located a fit.rds under a
# machine-local absolute path (/Users/.../sports-backup-20260424-163153, or
# $SPORTS_BACKUP_ROOT) and skipped when it was absent -- which was always, on
# CI and on any other machine. Those five were deleted rather than repointed at
# the fixture harness: they exercised publish_basketball_iceland(fit, ...), the
# signature the follow-on plan replaces with an extracts-based one, so
# rewriting them here would create and destroy the same work inside one PR
# chain. Publish-path coverage comes from test-publish-football-golden.R now,
# and from the extracts-based tests in the follow-on plan.
#
# test-fixture-skip-hygiene.R fails the build if a skip gate ever reappears
# here.

test_that("publish_basketball_iceland rejects wrong sport", {
  fake_fit <- structure(list(), class = "CmdStanMCMC")
  bad_league <- list(sport = "football", country = "iceland")
  expect_error(
    publish_basketball_iceland(fake_fit, bad_league, sex = "male"),
    "basketball"
  )
})

test_that("publish_basketball_iceland rejects invalid sex", {
  fake_fit <- structure(list(), class = "CmdStanMCMC")
  league <- list(sport = "basketball", country = "iceland")
  expect_error(
    publish_basketball_iceland(fake_fit, league, sex = "other"),
    "male.*female|female.*male"
  )
})
