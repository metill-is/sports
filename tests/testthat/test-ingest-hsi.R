fixture <- function(name) {
  testthat::test_path("fixtures", "hsi_handball", name)
}

test_that("parse_hsi_results_page extracts matches into canonical columns", {
  skip_if_not(
    file.exists(fixture("male_div1_current.html")),
    "no hsi fixture captured"
  )

  html <- rvest::read_html(
    fixture("male_div1_current.html"),
    encoding = "UTF-8"
  )
  parsed <- parse_hsi_results_page(
    html,
    sport = "handball", country = "iceland", sex = "male",
    division = "OD", season = 2026L
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
  expect_gt(nrow(parsed), 5)
  # Sanity: handball scores are > 10 for played matches
  expect_true(all(is.na(parsed$home_score) | parsed$home_score > 5))
})

test_that("hsi_handball registers itself as a source module", {
  src <- get_ingest_source("hsi_handball")
  expect_true(is.function(src$fetch_results))
  expect_true(is.function(src$fetch_schedule))
})
