fixture <- function(name) {
  testthat::test_path("fixtures", "hsi_handball", name)
}

# Sys.sleep cannot be mocked with local_mocked_bindings ("Can't find binding"
# for base functions), so the fetchers take a sleep_fn seam instead -- the same
# pattern poll_hsi_tables() already uses.
.no_sleep <- function(...) invisible(NULL)

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

test_that("the season-stamp guard fires on the fixture's real dates", {
  html <- rvest::read_html(fixture("male_div1_current.html"), encoding = "UTF-8")
  rows <- parse_hsi_results_page(
    html, "handball", "iceland", "male", "OD", 2026L
  )
  # The fixture is the 2025-26 season: 132 rows, 90 in 2025, 42 in 2026.
  expect_equal(nrow(rows), 132L)
  expect_setequal(unique(format(rows$match_date, "%Y")), c("2025", "2026"))
  # Requested as its own season -> passes.
  expect_no_error(.assert_season_stamp(rows, 2026L, source = "fixture"))
  # Requested as 2027 -> 68.2% of dates outside {2026, 2027} -> aborts.
  expect_error(
    .assert_season_stamp(rows, 2027L, source = "fixture"),
    class = "sports_season_stamp_error"
  )
})

test_that("hsi_fetch_and_parse re-raises a season-stamp abort, not a warning", {
  testthat::local_mocked_bindings(
    fetch_hsi_html = function(url, ...) {
      rvest::read_html(fixture("male_div1_current.html"), encoding = "UTF-8")
    }
  )
  # Fixture is season 2026; asked for as 2027 the guard must escape the
  # warn-and-return-NULL handler that wraps ordinary fetch failures.
  expect_error(
    hsi_fetch_and_parse(
      "https://www.hsi.is/tournament/9142", "male", "div1", "OD", 2027L
    ),
    class = "sports_season_stamp_error"
  )
})

test_that("hsi_fetch_and_parse still degrades an ordinary fetch failure to a warning", {
  testthat::local_mocked_bindings(
    fetch_hsi_html = function(url, ...) stop("chromote boot failed")
  )
  expect_warning(
    out <- hsi_fetch_and_parse(
      "https://www.hsi.is/tournament/9142", "male", "div1", "OD", 2026L
    ),
    "HSI results fetch failed"
  )
  expect_null(out)
})

test_that("fetch_results_hsi stamps rows with the season it asked for", {
  testthat::local_mocked_bindings(
    fetch_hsi_html = function(url, ...) {
      rvest::read_html(fixture("male_div1_current.html"), encoding = "UTF-8")
    },
    # The fixture is male div1 2026; register only that triple so the mocked
    # page is never served for a season it does not belong to.
    hsi_url = function(sex, division, season) {
      if (sex == "male" && division == "div1" && season == 2026L) {
        "https://www.hsi.is/tournament/8000"
      } else {
        NULL
      }
    }
  )
  # The other three male divisions are unresolvable here and warn; Task 5's
  # deferral test is what asserts that warning's text.
  out <- suppressWarnings(
    fetch_results_hsi(NULL, "male", seasons = 2026L, sleep_fn = .no_sleep)
  )
  expect_equal(nrow(out), 132L)
  expect_true(all(out$season == 2026L))
  expect_true(all(out$division == "OD"))
})

test_that("fetch_results_hsi aborts when a registered id serves the wrong season", {
  testthat::local_mocked_bindings(
    fetch_hsi_html = function(url, ...) {
      rvest::read_html(fixture("male_div1_current.html"), encoding = "UTF-8")
    },
    hsi_url = function(sex, division, season) {
      if (sex == "male" && division == "div1") {
        "https://www.hsi.is/tournament/8000"
      } else {
        NULL
      }
    }
  )
  # Same page, asked for as 2027. This is the RED proof of the guard: a stale
  # registry entry must stop the run, not write a fake season=2027 partition.
  expect_error(
    fetch_results_hsi(NULL, "male", seasons = 2027L, sleep_fn = .no_sleep),
    class = "sports_season_stamp_error"
  )
})

test_that("fetch_schedule_hsi guards its rows against the requested season too", {
  synthetic <- tibble::tibble(
    sport = "handball", country = "iceland", sex = "male",
    season = 2027L,
    match_date = as.Date(c("2024-10-01", "2024-11-01", "2025-01-15")),
    home_team = c("A", "B", "C"), away_team = c("D", "E", "F"),
    division = "OD", round = NA_integer_
  )
  expect_error(
    .assert_season_stamp(synthetic, 2027L, source = "hsi male/div1 schedule"),
    class = "sports_season_stamp_error"
  )
  # A zero-row schedule (the fixture's case: the season is fully played) passes.
  expect_no_error(
    .assert_season_stamp(hsi_empty_schedule(), 2027L, source = "empty")
  )
})

test_that("fetch_schedule_hsi re-raises a season-stamp abort past its warn handler", {
  # A schedule page that renders fixtures from another season must stop the
  # run, not be degraded to a warning and dropped.
  stale <- tibble::tibble(
    sport = "handball", country = "iceland", sex = "male",
    season = 2027L,
    match_date = as.Date(c("2024-10-01", "2024-11-01", "2025-01-15")),
    home_team = c("A", "B", "C"), away_team = c("D", "E", "F"),
    division = "OD", round = NA_integer_
  )
  testthat::local_mocked_bindings(
    fetch_hsi_html = function(url, ...) xml2::read_html("<html><body></body></html>"),
    parse_hsi_schedule_page = function(...) stale,
    hsi_url = function(sex, division, season) {
      if (division == "div1") "https://www.hsi.is/tournament/8000" else NULL
    }
  )
  expect_error(
    fetch_schedule_hsi(NULL, "male"),
    class = "sports_season_stamp_error"
  )
})

test_that("cup and playoffs are explicitly deferred for 2027, not silently dropped", {
  gaps <- hsi_unresolved_seasons(2027L)
  expect_s3_class(gaps, "tbl_df")
  expect_named(gaps, c("sex", "division", "season"))
  expect_setequal(
    paste(gaps$sex, gaps$division),
    c("male cup", "male playoffs", "female playoffs")
  )
  # The four league cells DO resolve for 2027 -- the deferral is scoped.
  expect_false(any(gaps$division %in% c("div1", "div2")))
})

test_that("hsi_unresolved_seasons scopes the gap to the season asked about", {
  # 2026: male cup 8437 + both playoffs 8427/8430 are registered, but the four
  # league cells are not (they were only ever reachable as dated slugs).
  gaps26 <- hsi_unresolved_seasons(2026L)
  expect_setequal(
    paste(gaps26$sex, gaps26$division),
    c("male div1", "male div2", "female div1", "female div2")
  )
  # 2024: every league cell registered, cup and playoffs never were.
  gaps24 <- hsi_unresolved_seasons(2024L)
  expect_false(any(gaps24$division %in% c("div1", "div2")))
})

test_that("an unresolved division warns and the rest of the ingest continues", {
  testthat::local_mocked_bindings(
    fetch_hsi_html = function(url, ...) {
      rvest::read_html(fixture("male_div1_current.html"), encoding = "UTF-8")
    },
    hsi_url = function(sex, division, season) {
      if (division == "div1") "https://www.hsi.is/tournament/8000" else NULL
    }
  )
  warnings <- character()
  out <- withCallingHandlers(
    fetch_results_hsi(NULL, "male", seasons = 2026L, sleep_fn = .no_sleep),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("no tournament id for male/div2", warnings, fixed = TRUE)))
  # div1 still came back: an unresolved division skips itself, not the league.
  expect_equal(nrow(out), 132L)
  expect_true(all(out$division == "OD"))
})

test_that("the playoff deferral is loud on the real 2027 fetch path", {
  # Regression guard for the silent-drop risk this task exists to close.
  # Before season-keying, playoffs and cup rode the current-season table and
  # were fetched unconditionally; under the registry they resolve to NULL for
  # 2027. `data/facts/results` holds PO rows for 2026 only, so a silent skip
  # would be indistinguishable from ordinary off-season emptiness. The real
  # registry is used here -- no hsi_url mock -- so this fails the day someone
  # deletes the warning or the unresolved cells stop being reported.
  testthat::local_mocked_bindings(
    fetch_hsi_html = function(url, ...) {
      xml2::read_html("<html><body><p>empty</p></body></html>")
    }
  )
  warnings <- character()
  out <- withCallingHandlers(
    fetch_results_hsi(NULL, "male", seasons = 2027L, sleep_fn = .no_sleep),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("male/cup season=2027", warnings, fixed = TRUE)))
  expect_true(any(grepl("male/playoffs season=2027", warnings, fixed = TRUE)))
  # div1/div2 2027 DO resolve, so their absence here is a fetch/parse result,
  # not an unresolved-id skip.
  expect_false(any(grepl("male/div1 season=2027", warnings, fixed = TRUE) &
                     grepl("no tournament id", warnings, fixed = TRUE)))
  expect_equal(nrow(out), 0L)
})
