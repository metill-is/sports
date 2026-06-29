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

# ---- wc_shootout_winners / .wc_knockout_winner_of ---------------------------

test_that("wc_shootout_winners reads the shootouts store into a pair-key map", {
  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "wc"))
  writeLines(
    c(
      "date,home_team,away_team,winner",
      "2026-07-01,Canada,South Africa,South Africa"
    ),
    file.path(tmp, "wc", "shootouts.csv")
  )
  m <- wc_shootout_winners(root = tmp)
  expect_equal(unname(m[[.wc_pair_key("Canada", "South Africa")]]), "South Africa")
  # symmetric: orientation of the lookup pair does not matter
  expect_equal(unname(m[[.wc_pair_key("South Africa", "Canada")]]), "South Africa")
})

test_that("wc_shootout_winners returns NULL for an absent or header-only store", {
  tmp <- withr::local_tempdir()
  expect_null(wc_shootout_winners(root = tmp))
  dir.create(file.path(tmp, "wc"))
  writeLines("date,home_team,away_team,winner", file.path(tmp, "wc", "shootouts.csv"))
  expect_null(wc_shootout_winners(root = tmp))
})

test_that(".wc_knockout_winner_of picks the decisive winner or the shootout winner", {
  res <- function(h, a, hs, as_) {
    tibble::tibble(home_team = h, away_team = a, home_score = hs, away_score = as_)
  }
  expect_equal(.wc_knockout_winner_of(res("Canada", "South Africa", 2L, 1L)), "Canada")
  expect_equal(.wc_knockout_winner_of(res("Canada", "South Africa", 0L, 3L)), "South Africa")

  sw <- stats::setNames("South Africa", .wc_pair_key("Canada", "South Africa"))
  expect_equal(
    .wc_knockout_winner_of(res("Canada", "South Africa", 1L, 1L), sw), "South Africa"
  )
  expect_true(is.na(.wc_knockout_winner_of(res("Canada", "South Africa", 1L, 1L))))
})
