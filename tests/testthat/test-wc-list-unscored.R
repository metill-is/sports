test_that("lists only past-dated, unscored WC-finals fixtures", {
  raw <- tibble::tibble(
    date = as.Date(c("2026-06-25", "2026-06-28", "2026-06-25", "2026-06-25")),
    home_team = c("Spain", "Panama", "Brazil", "Iceland"),
    away_team = c("Brazil", "England", "Croatia", "Norway"),
    home_score = c(NA_integer_, NA_integer_, 2L, NA_integer_),
    away_score = c(NA_integer_, NA_integer_, 1L, NA_integer_),
    tournament = c("FIFA World Cup", "FIFA World Cup", "FIFA World Cup", "UEFA Euro"),
    city = "x", country = "US", neutral = TRUE
  )
  out <- wc_list_unscored_fixtures(raw, as_of = as.Date("2026-06-26"))
  # Spain-Brazil: past + NA + WC  -> kept
  # Panama-England: future (06-28) -> dropped
  # Brazil-Croatia: already scored -> dropped
  # Iceland-Norway: not WC finals  -> dropped
  expect_equal(nrow(out), 1L)
  expect_equal(out$home_team, "Spain")
  expect_true(all(is.na(out$home_score)))
  expect_setequal(
    names(out), c("date", "home_team", "away_team", "home_score", "away_score")
  )
})
