fixture <- function(name) {
  testthat::test_path("fixtures", "kki_basketball", name)
}

test_that("parse_baskethotel_xlsx reads a results file into canonical columns", {
  skip_if_not(
    file.exists(fixture("sample_male_div1_2026.xlsx")),
    "no kki fixture captured"
  )

  parsed <- parse_baskethotel_xlsx(
    fixture("sample_male_div1_2026.xlsx"),
    sport = "basketball",
    country = "iceland",
    sex = "male",
    division = "BD",
    season = 2026L
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
  expect_true(all(!is.na(parsed$home_score))) # results only - all scored
})

test_that("kki_basketball registers itself as a source module", {
  src <- get_ingest_source("kki_basketball")
  expect_true(is.function(src$fetch_results))
  expect_true(is.function(src$fetch_schedule))
})

test_that("KKI_SEASON_IDS covers at least the current season for each (sex, div)", {
  ids <- KKI_SEASON_IDS
  expect_true(all(c("male", "female") %in% names(ids)))
  expect_true(all(c("div1", "div2") %in% names(ids$male)))
  expect_true(all(c("div1", "div2") %in% names(ids$female)))

  # Each (sex, div) has at least one season mapped.
  for (sex in names(ids)) {
    for (div in names(ids[[sex]])) {
      expect_gte(length(ids[[sex]][[div]]), 1L)
    }
  }

  # All values are positive numeric IDs.
  all_ids <- unlist(ids, use.names = FALSE)
  expect_true(all(all_ids > 0))
})

test_that("KKI_LEAGUE_IDS covers the full (sex, div) grid as typed integers", {
  expect_setequal(names(KKI_LEAGUE_IDS), c("male", "female"))
  for (sex in names(KKI_LEAGUE_IDS)) {
    expect_setequal(names(KKI_LEAGUE_IDS[[sex]]), names(KKI_DIVISION_LABELS))
    for (div in names(KKI_LEAGUE_IDS[[sex]])) {
      expect_type(KKI_LEAGUE_IDS[[sex]][[div]], "integer")
      expect_length(KKI_LEAGUE_IDS[[sex]][[div]], 1L)
    }
  }
})

test_that("kki_league_id returns the known id and aborts on an unknown cell", {
  expect_identical(kki_league_id("male", "div1"), 190L)
  expect_error(kki_league_id("male", "nosuchdiv"), "unknown KK. division")
  expect_error(kki_league_id("nosuchsex", "div1"), "unknown KK. sex")
})

test_that("kki_league_id aborts, rather than returning NA, on an unresolved id", {
  local_mocked_bindings(
    KKI_LEAGUE_IDS = list(male = list(div1 = NA_integer_))
  )
  expect_error(kki_league_id("male", "div1"), "not been resolved")
})
