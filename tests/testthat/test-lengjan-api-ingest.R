# API odds ingest (spec 2026-09-23 WS2). Fixture events: handball 4616742
# (comp 9281, 9 rows), basketball 4412294 (comp 9445, 5 rows), football
# 4601670 (comp 10046, 25 rows); FH - Haukar 4602104 (comp 1269) has no market.
# `.fx()` (the fixture reader) lives in helper-lengjan-api.R.

.api_league <- function(comp_id, sport, sex = "male") {
  list(
    sport = sport, country = "iceland",
    lengjan = list(
      source = "api",
      competitions = list(list(id = comp_id, name = "x", sex = sex))
    )
  )
}

.t0 <- as.POSIXct("2026-09-23 17:17:00", tz = "UTC")

test_that("lengjan_api_odds_rows selects by competition and stamps sex + ids", {
  ev <- parse_lengjan_program(.fx("current-program.json"))
  mk <- parse_lengjan_markets(.fx("markets.json"))
  rows <- lengjan_api_odds_rows(ev, mk, .api_league("9281", "handball"), .t0)
  expect_equal(nrow(rows), 9L)
  expect_true(all(rows$sex == "male"))
  expect_true(all(rows$event_id == "4616742"))
  expect_true(all(rows$competition_id == "9281"))
  expect_equal(unique(rows$home_team), "Stuttgart")
  expect_equal(unique(rows$match_date), as.Date("2026-09-24"))
  expect_equal(unique(rows$kickoff_at), as.POSIXct("2026-09-24 17:00:00", tz = "UTC"))
  expect_true(all(rows$sport == "handball" & rows$country == "iceland"))
  # A competition id asked for under the wrong sport never matches.
  expect_equal(nrow(lengjan_api_odds_rows(ev, mk, .api_league("9281", "basketball"), .t0)), 0L)
})

test_that("ingest_lengjan_api writes schema-valid rows and asks only for open events", {
  root <- withr::local_tempdir()
  asked <- NULL
  n <- suppressMessages(ingest_lengjan_api(
    list(
      handball_x = .api_league("9281", "handball"),
      basketball_x = .api_league("9445", "basketball", sex = "female"),
      handball_is = .api_league("1269", "handball")
    ),
    scraped_at = .t0, root = root,
    fetch_program = function() .fx("current-program.json"),
    fetch_markets = function(ids) {
      asked <<- ids
      .fx("markets.json")
    }
  ))
  expect_equal(n, 14L) # 9 handball + 5 basketball; FH - Haukar has no market yet
  expect_setequal(asked, c("4616742", "4412294")) # 4602104 (marketCount 0) not requested
  back <- read_table("odds", root = root)
  expect_equal(nrow(back), 14L)
  expect_setequal(unique(back$sex), c("male", "female"))
  expect_setequal(unique(back$sport), c("handball", "basketball"))
})

test_that("ingest_lengjan_api makes no markets request when nothing is open", {
  called <- FALSE
  n <- suppressMessages(ingest_lengjan_api(
    list(handball_is = .api_league("1269", "handball")),
    root = withr::local_tempdir(),
    fetch_program = function() .fx("current-program.json"),
    fetch_markets = function(ids) {
      called <<- TRUE
      list()
    }
  ))
  expect_equal(n, 0L)
  expect_false(called)
})

test_that("ingest_one_lengjan dispatches on lengjan.source", {
  testthat::local_mocked_bindings(
    .is_league_active = function(active_path, key) TRUE,
    ingest_lengjan_api = function(...) 11L,
    ingest_lengjan_odds = function(...) 22L
  )
  one <- function(src) {
    lj <- list(competitions = list(list(id = "1269", name = "x", sex = "male")))
    if (!is.null(src)) lj$source <- src
    suppressMessages(ingest_one_lengjan(
      list(sport = "handball", country = "iceland"), lj,
      "handball_iceland", "active.json"
    ))
  }
  expect_identical(one("api"), 11L)
  expect_identical(one("dom"), 22L)
  expect_identical(one(NULL), 22L) # absent source keeps the DOM scraper
})

test_that("ingest_one_lengjan soft-fails an API fetch error to 0 rows", {
  testthat::local_mocked_bindings(
    .is_league_active = function(active_path, key) TRUE,
    ingest_lengjan_api = function(...) {
      stop(structure(
        class = c("lengjan_fetch_error", "error", "condition"),
        list(message = "Lengjan API /current-program: HTTP 503")
      ))
    },
    ingest_lengjan_odds = function(...) stop("DOM path taken")
  )
  res <- suppressMessages(ingest_one_lengjan(
    list(sport = "handball", country = "iceland"),
    list(source = "api", competitions = list(list(id = "1269", name = "x", sex = "male"))),
    "handball_iceland", "active.json"
  ))
  expect_identical(res, 0L)
})
