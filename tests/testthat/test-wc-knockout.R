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

# ---- .wc_knockout_pins ------------------------------------------------------

test_that(".wc_knockout_pins pins a decided R32 match with a played record", {
  s <- wc_structure()
  teams <- unlist(s$groups, use.names = FALSE)
  occ <- make_certain_occ(s, teams[1:16], teams[17:32])
  kr <- tibble::tibble(
    match_date = as.Date("2026-06-30"),
    home_team = teams[1], away_team = teams[17], home_score = 2L, away_score = 1L
  )
  kp <- .wc_knockout_pins(s$bracket, teams, occ$occ_a, occ$occ_b, kr)

  expect_equal(kp$pins[["73"]], 1L) # match 73 = first R32 row
  expect_length(kp$played, 1L)
  p <- kp$played[[1L]]
  expect_equal(p$match_no, 73L)
  expect_equal(p$winner, 1L)
  expect_equal(p$loser, 17L)
  expect_equal(p$winner_score, 2L)
  expect_equal(p$loser_score, 1L)
  expect_false(p$shootout)
})

test_that(".wc_knockout_pins resolves a level R32 via the shootout map only", {
  s <- wc_structure()
  teams <- unlist(s$groups, use.names = FALSE)
  occ <- make_certain_occ(s, teams[1:16], teams[17:32])
  kr <- tibble::tibble(
    match_date = as.Date("2026-06-30"),
    home_team = teams[1], away_team = teams[17], home_score = 1L, away_score = 1L
  )
  sw <- stats::setNames(teams[17], .wc_pair_key(teams[1], teams[17]))

  kp <- .wc_knockout_pins(s$bracket, teams, occ$occ_a, occ$occ_b, kr, sw)
  expect_equal(kp$pins[["73"]], 17L)
  expect_true(kp$played[[1L]]$shootout)

  kp2 <- .wc_knockout_pins(s$bracket, teams, occ$occ_a, occ$occ_b, kr)
  expect_length(kp2$pins, 0L)
  expect_length(kp2$played, 0L)
})

test_that(".wc_knockout_pins chains to R16 once both feeders are decided", {
  s <- wc_structure()
  teams <- unlist(s$groups, use.names = FALSE)
  occ <- make_certain_occ(s, teams[1:16], teams[17:32])
  # match 74 (R32 #2) and 77 (R32 #5) decided; their winners feed R16 match 89.
  kr <- tibble::tibble(
    match_date = as.Date("2026-07-05"),
    home_team = c(teams[2], teams[5], teams[2]),
    away_team = c(teams[18], teams[21], teams[5]),
    home_score = c(1L, 3L, 2L), away_score = c(0L, 1L, 0L)
  )
  kp <- .wc_knockout_pins(s$bracket, teams, occ$occ_a, occ$occ_b, kr)

  expect_equal(kp$pins[["74"]], 2L)
  expect_equal(kp$pins[["77"]], 5L)
  expect_equal(kp$pins[["89"]], 2L) # R16 89 = W74 vs W77
})

test_that(".wc_knockout_pins self-gates: no R16 pin until both feeders decided", {
  s <- wc_structure()
  teams <- unlist(s$groups, use.names = FALSE)
  occ <- make_certain_occ(s, teams[1:16], teams[17:32])
  # only match 74 decided; an R16 result for 89 (W74 vs W77) must be ignored.
  kr <- tibble::tibble(
    match_date = as.Date("2026-07-05"),
    home_team = c(teams[2], teams[2]),
    away_team = c(teams[18], teams[5]),
    home_score = c(1L, 2L), away_score = c(0L, 0L)
  )
  kp <- .wc_knockout_pins(s$bracket, teams, occ$occ_a, occ$occ_b, kr)

  expect_equal(kp$pins[["74"]], 2L)
  expect_null(kp$pins[["89"]])
})
