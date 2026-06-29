# Shared WC constructors (wc_structure, make_*) live in helper-wc.R.

# ---- wc_knockout_results ----------------------------------------------------

test_that("wc_knockout_results returns only played cross-group WC2026 fixtures", {
  s <- wc_structure()
  tmp <- withr::local_tempdir()
  row <- function(home, away, hs, as_, division = "FIFA World Cup",
                  season = 2026L, date = "2026-06-30") {
    tibble::tibble(
      sport = "football", country = "world", sex = "male", season = season,
      match_date = as.Date(date), home_team = home, away_team = away,
      home_score = as.integer(hs), away_score = as.integer(as_),
      division = division, round = NA_integer_
    )
  }
  res <- dplyr::bind_rows(
    row("Mexico", "South Africa", 2, 0), # within group A -> excluded
    row("Mexico", "Canada", 3, 1), # cross-group WC -> kept
    row("Brazil", "Germany", 1, 1, division = "Friendly"), # non-WC -> excluded
    row("France", "Argentina", 0, 2, season = 2025L, date = "2025-06-30")
  )
  write_table(res, "results", root = tmp)

  k <- wc_knockout_results(s, root = tmp)

  expect_equal(nrow(k), 1L)
  expect_equal(k$home_team, "Mexico")
  expect_equal(k$away_team, "Canada")
  expect_type(k$home_score, "integer")
  expect_type(k$away_score, "integer")
})
