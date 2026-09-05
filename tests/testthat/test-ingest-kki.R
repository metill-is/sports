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

# --- WS5: fetch_kki resolves the current season only -------------------------

test_that("fetch_kki defaults to the current season, not every registry key", {
  # It used to iterate all of 2021-2026, re-downloading five historical
  # seasons that cannot change: 2 sexes x 2 types x 2 divisions x 6 seasons =
  # 48 XLSX downloads per ingest run. Now 2 divisions x 1 season per call.
  calls <- new.env(parent = emptyenv())
  calls$ids <- integer()

  local_mocked_bindings(
    kki_current_season = function(...) 2027L,
    download_baskethotel_xlsx = function(season_id, type) {
      calls$ids <- c(calls$ids, as.integer(season_id))
      "stub.xlsx"
    },
    parse_baskethotel_xlsx = function(path, sport, country, sex, division,
                                      season) {
      tibble::tibble(
        sport = sport, country = country, sex = sex, division = division,
        season = season, match_date = as.Date("2026-10-08"),
        home_team = "A", away_team = "B",
        home_score = NA_real_, away_score = NA_real_
      )
    }
  )

  out <- fetch_kki(league = NULL, sex = "male", type = "schedule_only")

  # Exactly the two male divisions' 2027 ids -- not 12 fetches.
  expect_setequal(calls$ids, c(132568L, 132571L))
  expect_equal(nrow(out), 2L)
  expect_setequal(out$division, c("BD", "1D"))
})

test_that("an unresolved season_id warns loudly instead of silently skipping", {
  local_mocked_bindings(
    kki_season_id = function(...) NULL,
    download_baskethotel_xlsx = function(...) stop("must not be reached")
  )
  # Both divisions warn, so collect rather than letting the second leak.
  seen <- character()
  withCallingHandlers(
    fetch_kki(league = NULL, sex = "male", seasons = 2099L,
              type = "schedule_only"),
    warning = function(w) {
      seen <<- c(seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(seen, 2L)
  expect_true(all(grepl("no season_id resolved", seen)))
  expect_true(any(grepl("div1", seen)) && any(grepl("div2", seen)))
})

test_that("fetch_kki aborts when the export's dates contradict the season", {
  # The corruption this guards: an id that silently still serves last season
  # would be written into a season=2027 hive partition. `season` is a
  # partition column, so the wrong rows become indistinguishable on disk.
  local_mocked_bindings(
    kki_season_id = function(...) 999L,
    download_baskethotel_xlsx = function(...) "stub.xlsx",
    parse_baskethotel_xlsx = function(path, sport, country, sex, division,
                                      season) {
      tibble::tibble(
        sport = sport, country = country, sex = sex, division = division,
        season = season,
        match_date = as.Date("2021-11-01") + 0:9,   # nowhere near 2027
        home_team = "A", away_team = "B",
        home_score = 80, away_score = 75
      )
    }
  )
  expect_error(
    fetch_kki(league = NULL, sex = "male", seasons = 2027L,
              type = "results_only"),
    class = "sports_season_stamp_error"
  )
})
