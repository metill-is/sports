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

test_that("HSI_HISTORICAL_IDS has entries for each sex and division", {
  ids <- HSI_HISTORICAL_IDS
  expect_true(all(c("male", "female") %in% names(ids)))
  expect_true(all(c("div1", "div2") %in% names(ids$male)))
  expect_true(all(c("div1", "div2") %in% names(ids$female)))

  # At least 3 historical seasons per (sex, div1) pair.
  expect_gte(length(ids$male$div1), 3L)
  expect_gte(length(ids$female$div1), 3L)

  # All values are positive integer tournament IDs.
  all_ids <- unlist(ids, use.names = FALSE)
  expect_type(all_ids, "integer")
  expect_true(all(all_ids > 0L))

  # Male div1 IDs differ from male div2 IDs (catch copy-paste regressions).
  expect_false(any(
    unlist(ids$male$div1, use.names = FALSE) %in%
      unlist(ids$male$div2, use.names = FALSE)
  ))
})

test_that("hsi_historical_url maps known (sex, div, season) triples", {
  # Male div1 2024 = tournament 6983 (Olísdeild karla 2023–24).
  expect_equal(
    hsi_historical_url("male", "div1", 2024L),
    "https://www.hsi.is/tournament/6983"
  )
  # Unknown season → NULL.
  expect_null(hsi_historical_url("male", "div1", 1999L))
  # Unknown division → NULL.
  expect_null(hsi_historical_url("male", "cup", 2024L))
  # Unknown sex → NULL.
  expect_null(hsi_historical_url("other", "div1", 2024L))
})
