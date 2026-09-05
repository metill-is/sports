# One source of truth for the goal-difference bin width.
#
# meta.units.diff_bin_width has to be the number the extractor ACTUALLY binned
# with, or the published band labels lie about the data behind them. Two
# literals in two extractor entry points cannot be kept in step with a third
# number in the profile by discipline alone, so the extractors read the profile.
#
# The profile's own values (points, units, season_scope, postseason,
# placement_basis) are asserted in test-publish-profile.R, which is their sole
# author -- this file asserts only that the extractors CONSUME them.

# Package source is reached relative to the test dir, not via here::here():
# under a git worktree here::here() resolves to the MAIN checkout and would
# grep a different copy of the file than the one under test.
.extractor_source <- function(file) {
  path <- testthat::test_path("..", "..", "R", file)
  expect_true(file.exists(path), info = file)
  readLines(path, warn = FALSE)
}

test_that("the 2DT extractors read the bin width from the profile", {
  specs <- list(
    list(file = "extract-basketball-iceland.R", sport = "basketball", width = 5L),
    list(file = "extract-handball-iceland.R", sport = "handball", width = 2L)
  )
  for (spec in specs) {
    src <- .extractor_source(spec$file)
    code <- src[!grepl("^\\s*#", src)]

    # The literal is gone ...
    expect_length(
      grep("bucket_width\\s*=\\s*[0-9]+L", code, value = TRUE), 0L
    )
    # ... and the profile lookup is there, naming this sport.
    lookup <- grep("bucket_width\\s*=", code, value = TRUE)
    expect_length(lookup, 1L)
    expect_match(lookup, "sport_publish_profile", fixed = TRUE)
    expect_match(lookup, "diff_bin_width", fixed = TRUE)

    # And the value the lookup resolves to is the width the fixture was binned
    # with.
    expect_equal(
      sport_publish_profile(spec$sport)$units$diff_bin_width, spec$width,
      info = spec$sport
    )
  }
})

test_that("the committed 2DT bins are multiples of the profile width", {
  # A contract lock, not a TDD cycle: this is expected to hold on arrival. It
  # fails the day someone changes a width in one place only.
  specs <- list(
    list(sport = "basketball", width = 5L),
    list(sport = "handball", width = 2L)
  )
  for (spec in specs) {
    predicted <- arrow::read_parquet(testthat::test_path(
      "fixtures", "extracts", paste0("sport=", spec$sport),
      "country=iceland", "sex=male", "fit_date=2100-01-01",
      "predicted_matches.parquet"
    ))
    expect_gt(nrow(predicted), 0L)
    diffs <- unlist(lapply(predicted$goal_diff_distribution, function(d) d$diff))
    expect_gt(length(diffs), 0L)
    expect_true(
      all(diffs %% sport_publish_profile(spec$sport)$units$diff_bin_width == 0L),
      info = spec$sport
    )
  }
})
