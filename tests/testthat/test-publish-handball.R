# Argument validation only. See the note at the top of
# test-publish-basketball.R: the three fit.rds-gated blocks that used to live
# here pointed at a machine-local backup path, never ran, and exercised the
# publish_handball_iceland(fit, ...) signature the follow-on plan replaces --
# so they were deleted rather than repointed at the fixture harness.
#
# test-fixture-skip-hygiene.R fails the build if a skip gate ever reappears
# here.

test_that("publish_handball_iceland rejects wrong sport", {
  fake_fit <- structure(list(), class = "CmdStanMCMC")
  bad_league <- list(sport = "basketball", country = "iceland")
  expect_error(
    publish_handball_iceland(fake_fit, bad_league, sex = "male"),
    "handball"
  )
})

test_that("publish_handball_iceland rejects invalid sex", {
  fake_fit <- structure(list(), class = "CmdStanMCMC")
  league <- list(sport = "handball", country = "iceland")
  expect_error(
    publish_handball_iceland(fake_fit, league, sex = "other"),
    "male.*female|female.*male"
  )
})
