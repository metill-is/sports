test_that("wc_schedule loads 72 group fixtures with aliases applied", {
  s <- wc_schedule()
  expect_equal(nrow(s), 72L)
  expect_setequal(
    names(s), c("match_no", "kickoff", "home", "away", "pair_key")
  )
  # Aliases applied: martj42 names present, fixturedownload names absent.
  teams <- unique(c(s$home, s$away))
  expect_true(all(c(
    "Czech Republic", "South Korea", "Ivory Coast",
    "Cape Verde", "DR Congo", "Iran", "Turkey",
    "United States"
  ) %in% teams))
  expect_false(any(c("Czechia", "Korea Republic", "USA", "T\u00FCrkiye") %in% teams))
  # Types + uniqueness.
  expect_type(s$match_no, "integer")
  expect_s3_class(s$kickoff, "POSIXct")
  expect_equal(length(unique(s$pair_key)), 72L)
})

test_that("wc_schedule anchors today's kickoff order (25<26<27<28)", {
  s <- wc_schedule()
  pk <- .wc_pair_key
  want <- c(
    pk("Czech Republic", "South Africa"),
    pk("Switzerland", "Bosnia and Herzegovina"),
    pk("Canada", "Qatar"),
    pk("Mexico", "South Korea")
  )
  nos <- vapply(want, function(k) s$match_no[match(k, s$pair_key)], integer(1))
  expect_false(any(is.na(nos)))
  expect_equal(nos, sort(nos)) # strictly increasing kickoff order
  expect_true(all(diff(nos) > 0))
})
