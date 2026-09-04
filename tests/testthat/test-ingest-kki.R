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

# --- WS5 Task 2: season-selector parser (spec section 6) ---------------------

kki_fixture <- function(name) {
  testthat::test_path("fixtures", "kki_basketball", name)
}

test_that("parse_kki_season_options reads the season selector", {
  got <- parse_kki_season_options(kki_fixture("motayfirlit_190_2027.html"))

  expect_named(got, c("season", "season_id", "label"))
  expect_type(got$season, "integer")
  expect_type(got$season_id, "integer")
  # Newest first.
  expect_identical(got$season, c(2027L, 2026L, 2025L))
})

test_that("the parser agrees with the hand-verified registry", {
  # This is the whole licence for trusting the resolver on a season nobody has
  # verified: on the seasons we DO know, it independently recovers the values
  # already committed in KKI_SEASON_IDS. If this ever fails, the page shape
  # changed and the resolver must not be trusted for a new season either.
  got <- parse_kki_season_options(kki_fixture("motayfirlit_190_2027.html"))
  known <- KKI_SEASON_IDS$male$div1

  expect_identical(
    got$season_id[got$season == 2026L], as.integer(known[["2026"]])
  )
  expect_identical(
    got$season_id[got$season == 2025L], as.integer(known[["2025"]])
  )
})

test_that("the parser ignores the placeholder and the other two selectors", {
  # The page carries three unnamed <select>s. Only the season one has
  # YYYY-YYYY labels; `Veldu timabil` has an empty value; the stage selector
  # (Deildarkeppni 300475 / Urslitakeppni 306658) and the matchday selector
  # must not leak in as seasons.
  got <- parse_kki_season_options(kki_fixture("motayfirlit_190_2027.html"))

  expect_false(any(is.na(got$season)))
  expect_false(any(got$season_id %in% c(300475L, 306658L)))
  expect_equal(nrow(got), 3L)
})

test_that("the parser returns zero rows, not an error, on a page with no seasons", {
  got <- parse_kki_season_options(
    charToRaw("<html><body><select><option value=''>x</option></select></body></html>") |>
      rawToChar() |> rvest::read_html()
  )
  expect_equal(nrow(got), 0L)
  expect_named(got, c("season", "season_id", "label"))
})
