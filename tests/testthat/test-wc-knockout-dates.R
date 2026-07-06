# Knockout-date correction against the vendored official calendar.
#
# martj42 lists upcoming knockout fixtures with unreliable dates (2026-07-06
# incident: all four remaining R16 ties carried the first day's date, so the
# published forecast — and the platform's matchday reel keyed on exact
# match_date — showed 4 matches on 6 July instead of 2). The correction maps
# each unplayed knockout fixture to its official slot by venue and replaces the
# date with the slot's stadium-local date (martj42's date convention).

mrow <- function(date, home, away, hs = NA_integer_, as_ = NA_integer_,
                 city = NA_character_, tournament = "FIFA World Cup") {
  tibble::tibble(
    date = as.Date(date), home_team = home, away_team = away,
    home_score = hs, away_score = as_, tournament = tournament,
    city = city, country = "United States", neutral = TRUE
  )
}

# ---- wc_knockout_slots ------------------------------------------------------

test_that("wc_knockout_slots loads 32 knockout slots with stadium-local dates", {
  k <- wc_knockout_slots()
  expect_equal(nrow(k), 32L)
  expect_setequal(
    names(k), c("match_no", "round", "venue", "kickoff", "local_date")
  )
  expect_false(anyNA(k$kickoff))
  expect_false(anyNA(k$local_date))
  expect_true(all(k$venue %in% names(.wc_venue_tz)))
  expect_equal(sort(k$match_no), 73:104)

  loc <- function(no) k$local_date[match(no, k$match_no)]
  # The R16 split the 2026-07-06 incident got wrong: UTC kickoffs cross
  # midnight (Seattle 07-07 00:00 UTC is 6 July local; Atlanta/Vancouver are
  # 7 July both ways), so local dates are the only correct rendering.
  expect_equal(loc(93L), as.Date("2026-07-06")) # Dallas
  expect_equal(loc(94L), as.Date("2026-07-06")) # Seattle
  expect_equal(loc(95L), as.Date("2026-07-07")) # Atlanta
  expect_equal(loc(96L), as.Date("2026-07-07")) # Vancouver
  # QF at Kansas City: 07-12 01:00 UTC -> 11 July local.
  expect_equal(loc(100L), as.Date("2026-07-11"))
  # Bronze + final.
  expect_equal(loc(103L), as.Date("2026-07-18"))
  expect_equal(loc(104L), as.Date("2026-07-19"))
})

# ---- wc_correct_knockout_dates ----------------------------------------------

test_that("the 2026-07-06 incident: second-day R16 dates are corrected", {
  raw <- dplyr::bind_rows(
    mrow("2026-07-06", "Portugal", "Spain", city = "Dallas"),
    mrow("2026-07-06", "United States", "Belgium", city = "Seattle"),
    mrow("2026-07-06", "Argentina", "Egypt", city = "Atlanta"),
    mrow("2026-07-06", "Switzerland", "Colombia", city = "Vancouver")
  )
  out <- wc_correct_knockout_dates(raw)
  expect_equal(
    out$date,
    as.Date(c("2026-07-06", "2026-07-06", "2026-07-07", "2026-07-07"))
  )
  # Everything except the date passes through untouched.
  expect_equal(out[, names(out) != "date"], raw[, names(raw) != "date"])
})

test_that("already-correct knockout dates are left alone (idempotent)", {
  raw <- dplyr::bind_rows(
    mrow("2026-07-09", "France", "Morocco", city = "Foxborough"),
    mrow("2026-07-11", "Norway", "England", city = "Miami Gardens")
  )
  out <- wc_correct_knockout_dates(raw)
  expect_equal(out$date, raw$date)
})

test_that("played, group-stage, and non-WC rows are never touched", {
  raw <- dplyr::bind_rows(
    # Played knockout (scores present), deliberately off-date.
    mrow("2026-07-02", "Australia", "Egypt", 1L, 1L, city = "Arlington"),
    # Unplayed within-group pairing (match 1: Mexico vs South Africa).
    mrow("2026-06-30", "Mexico", "South Africa", city = "Mexico City"),
    # Non-WC tournament at a WC venue.
    mrow("2026-07-06", "Brazil", "Germany", city = "Dallas", tournament = "Friendly")
  )
  out <- wc_correct_knockout_dates(raw)
  expect_equal(out$date, raw$date)
})

test_that("an unmapped city warns and leaves the row unchanged", {
  raw <- mrow("2026-07-06", "Portugal", "Spain", city = "Narnia")
  expect_warning(out <- wc_correct_knockout_dates(raw), "Narnia")
  expect_equal(out$date, as.Date("2026-07-06"))
})

test_that("played fixtures consume their slot so later ties anchor correctly", {
  # Dallas hosts R32 slot 88 (local 07-03) and R16 slot 93 (local 07-06).
  # An unplayed Dallas tie dated 07-04 sits nearer 88 (1d) than 93 (2d) — but
  # 88 is consumed by the played Australia-Egypt row, so 93 must win.
  raw <- dplyr::bind_rows(
    mrow("2026-07-03", "Australia", "Egypt", 1L, 1L, city = "Arlington"),
    mrow("2026-07-04", "Portugal", "Spain", city = "Dallas")
  )
  out <- wc_correct_knockout_dates(raw)
  expect_equal(out$date[2L], as.Date("2026-07-06"))
})

test_that("an ambiguous nearest slot warns and keeps the martj42 date", {
  # Los Angeles hosts R32 slots 73 (local 06-28) and 84 (local 07-02); a
  # fixture dated 06-30 is equidistant from both.
  raw <- mrow("2026-06-30", "Brazil", "Norway", city = "Inglewood")
  expect_warning(out <- wc_correct_knockout_dates(raw), "ambiguous|no unique")
  expect_equal(out$date, as.Date("2026-06-30"))
})

test_that("a fixture too far from any slot warns and keeps the martj42 date", {
  raw <- mrow("2026-07-25", "Portugal", "Spain", city = "Philadelphia")
  expect_warning(out <- wc_correct_knockout_dates(raw), "official")
  expect_equal(out$date, as.Date("2026-07-25"))
})
