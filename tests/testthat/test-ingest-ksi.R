fixture <- function(name) {
  testthat::test_path("fixtures", "ksi_football", name)
}

test_that("parse_ksi_results_page extracts matches into canonical columns", {
  skip_if_not(
    file.exists(fixture("male_div1_2026.html")),
    "no ksi fixture captured"
  )

  html <- rvest::read_html(
    fixture("male_div1_2026.html"),
    encoding = "UTF-8"
  )
  parsed <- parse_ksi_results_page(
    html,
    sport = "football", country = "iceland", sex = "male",
    division = "BD", season = 2026L
  )

  expect_named(
    parsed,
    c(
      "sport", "country", "sex", "season", "match_date",
      "home_team", "away_team", "home_score", "away_score",
      "division", "round"
    )
  )
  expect_s3_class(parsed$match_date, "Date")
  expect_type(parsed$season, "integer")
  # The 2026 fixture captured in April contains only scheduled (unplayed)
  # matches — the season starts in May. Parser should include them with
  # NA scores.
  expect_gt(nrow(parsed), 5)
  # Football: sensible scores 0..12 typically
  expect_true(all(is.na(parsed$home_score) | parsed$home_score >= 0))
  expect_true(all(is.na(parsed$away_score) | parsed$away_score >= 0))
  expect_true(all(parsed$division == "BD"))
  expect_true(all(parsed$season == 2026L))
  # Dates must parse for every surviving row (including Icelandic month
  # names like "maí" / "júní" that appear in the fixture).
  expect_true(all(!is.na(parsed$match_date)))
})

test_that("parse_ksi_date handles Icelandic month names", {
  # Confirms the month-name map covers at least the summer months which
  # dominate the Icelandic football season.
  dates <- c(
    "Sun 26. apr\u00edl  18:00",
    "Lau 2. ma\u00ed  16:00",
    "Lau 9. j\u00fan\u00ed  14:00",
    "Fim 4. j\u00fal\u00ed  18:00",
    "Mi\u00f0 4. \u00e1g\u00fast  19:15",
    "Lau 12. september  14:00"
  )
  Encoding(dates) <- "UTF-8"
  out <- parse_ksi_date(dates, 2026L)
  expect_s3_class(out, "Date")
  expect_true(all(!is.na(out)))
  expect_equal(lubridate::month(out), c(4L, 5L, 6L, 7L, 8L, 9L))
})

test_that("parse_ksi_results_page with played_only drops unplayed", {
  skip_if_not(
    file.exists(fixture("male_div1_2026.html")),
    "no ksi fixture captured"
  )

  html <- rvest::read_html(
    fixture("male_div1_2026.html"),
    encoding = "UTF-8"
  )
  parsed <- parse_ksi_results_page(
    html,
    sport = "football", country = "iceland", sex = "male",
    division = "BD", season = 2026L,
    played_only = TRUE
  )
  # 2026 fixture captured pre-season: no played matches.
  # All surviving rows must have both scores.
  if (nrow(parsed) > 0L) {
    expect_true(all(!is.na(parsed$home_score)))
    expect_true(all(!is.na(parsed$away_score)))
  }
})

test_that("ksi_football registers itself as a source module", {
  src <- get_ingest_source("ksi_football")
  expect_true(is.function(src$fetch_results))
  expect_true(is.function(src$fetch_schedule))
})
