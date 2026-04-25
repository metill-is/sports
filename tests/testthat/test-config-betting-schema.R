test_that("expanded betting block validates against schema", {
  expect_no_error(load_leagues()) # validates internally
  leagues <- load_leagues()
  expect_true(all(vapply(
    leagues, function(l) !is.null(l$betting$kelly_frac),
    logical(1)
  )))
  # Football has per-sex form
  expect_true(is.list(leagues$football_iceland$betting$kelly_frac))
  expect_named(leagues$football_iceland$betting$kelly_frac,
    c("male", "female"),
    ignore.order = TRUE
  )
  # Basketball + handball use scalar form
  expect_type(leagues$basketball_iceland$betting$kelly_frac, "double")
})

test_that("malformed betting block fails validation", {
  bad <- yaml::as.yaml(list(
    bad_league = list(
      sport = "football", country = "iceland", sexes = list("male"),
      active = TRUE,
      data_source = list(results = "x", schedule = "x", odds = "y"),
      stan_model = "x.stan",
      betting = list(kelly_frac = 2.5) # > 1, invalid
    )
  ))
  tmp <- withr::local_tempfile(fileext = ".yml")
  writeLines(bad, tmp)
  expect_error(load_leagues(path = tmp), "kelly_frac|maximum|<=")
})
