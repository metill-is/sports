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

test_that("HSI_TOURNAMENT_IDS is one season-keyed registry with distinct ids", {
  ids <- HSI_TOURNAMENT_IDS
  expect_setequal(names(ids), c("male", "female"))
  expect_setequal(names(ids$male), c("div1", "div2", "cup", "playoffs"))
  expect_setequal(names(ids$female), c("div1", "div2", "playoffs"))

  flat <- unlist(ids, use.names = FALSE)
  expect_type(flat, "integer")
  expect_true(all(flat > 0L))
  # 19 historical + 4 x 2027 league + male cup 2026 + 2 playoffs 2026.
  expect_equal(length(flat), 26L)
  # Every id distinct. This is what catches the legacy copy-paste that put the
  # male div2 2025 id (7644) under female div2 2025.
  expect_equal(length(unique(flat)), length(flat))
})

test_that("every registry (sex, division) is reachable from hsi_divisions_for_sex", {
  for (sex in names(HSI_TOURNAMENT_IDS)) {
    expect_setequal(
      names(HSI_TOURNAMENT_IDS[[sex]]),
      intersect(names(HSI_TOURNAMENT_IDS[[sex]]), hsi_divisions_for_sex(sex))
    )
  }
  # Concretely: a female "cup" key would never be fetched, so it must not exist.
  expect_false("cup" %in% names(HSI_TOURNAMENT_IDS$female))
})

test_that("hsi_url builds /tournament/<id> for every registered triple", {
  expect_equal(hsi_url("male", "div1", 2024L), "https://www.hsi.is/tournament/6983")
  expect_equal(hsi_url("male", "div1", 2027L), "https://www.hsi.is/tournament/9142")
  expect_equal(hsi_url("male", "div2", 2027L), "https://www.hsi.is/tournament/9140")
  expect_equal(hsi_url("female", "div1", 2027L), "https://www.hsi.is/tournament/9141")
  expect_equal(hsi_url("female", "div2", 2027L), "https://www.hsi.is/tournament/9143")
  expect_equal(hsi_url("male", "playoffs", 2026L), "https://www.hsi.is/tournament/8427")
  expect_equal(hsi_url("female", "playoffs", 2026L), "https://www.hsi.is/tournament/8430")
  expect_equal(hsi_url("male", "cup", 2026L), "https://www.hsi.is/tournament/8437")
})

test_that("hsi_url returns NULL rather than erroring on anything unregistered", {
  # NULL means do-not-fetch, which is the fail-safe direction.
  expect_null(hsi_url("male", "div1", 1999L))
  expect_null(hsi_url("other", "div1", 2024L))
  expect_null(hsi_url("male", "nonesuch", 2024L))
  # The legacy hole this workstream recovers -- still open until Task 7.
  expect_null(hsi_url("female", "div2", 2025L))
})

test_that("hsi_current_season names the requested season and nothing else", {
  # Icelandic winter seasons are labelled by their closing calendar year.
  expect_equal(hsi_current_season(as.Date("2026-09-02")), 2027L)
  expect_equal(hsi_current_season(as.Date("2027-03-01")), 2027L)
  expect_equal(hsi_current_season(as.Date("2026-06-30")), 2026L)
})

test_that("hsi_tournament_id falls back to the provenance cache", {
  path <- withr::local_tempfile(fileext = ".json")
  refresh_federation_seasons(
    tibble::tibble(
      federation = "hsi", sex = "male", division = "playoffs",
      season = 2027L, id = 9999L, title = NA_character_,
      source = "live-nav", discovered_at = "2026-09-02",
      verified = TRUE, note = NA_character_
    ),
    path = path
  )
  testthat::local_mocked_bindings(
    federation_seasons_path = function() path
  )
  expect_identical(hsi_tournament_id("male", "playoffs", 2027L), 9999L)
  # Registry still wins over cache for a key both hold.
  expect_identical(hsi_tournament_id("male", "div1", 2027L), 9142L)
})
