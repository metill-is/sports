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

# ---- Unparseable kickoff (final review F3) -----------------------------------
#
# .lengjan_event_row() parses datePlayed with ymd_hms(quiet = TRUE), so an
# unparseable value became an NA kickoff_at and the league's rows were written
# with match_date NA: never joinable, never reported. It must fail loudly
# instead; run_per_league() keeps that from taking the other leagues down.

.bad_kickoff_program <- function(event_id = "4616742", value = "Fimmtudagur 17:00") {
  prog <- .fx("current-program.json")
  for (a in c("events", "liveSoon", "popular", "liveNow")) {
    prog[[a]] <- lapply(prog[[a]], function(e) {
      if (identical(as.character(e$id), event_id)) e$datePlayed <- value
      e
    })
  }
  prog
}

test_that("lengjan_api_odds_rows fails loudly on a selected event with no parseable kickoff", {
  ev <- parse_lengjan_program(.bad_kickoff_program())
  mk <- parse_lengjan_markets(.fx("markets.json"))
  expect_error(
    lengjan_api_odds_rows(ev, mk, .api_league("9281", "handball"), .t0),
    "API may have changed"
  )
  expect_error(
    lengjan_api_odds_rows(ev, mk, .api_league("9281", "handball"), .t0),
    "4616742"
  )
  # Scoped to the league's own events: basketball's event is unaffected.
  expect_equal(nrow(lengjan_api_odds_rows(ev, mk, .api_league("9445", "basketball"), .t0)), 5L)
})

test_that("a plain error, not lengjan_fetch_error, so it is never soft-failed", {
  ev <- parse_lengjan_program(.bad_kickoff_program())
  mk <- parse_lengjan_markets(.fx("markets.json"))
  err <- tryCatch(
    lengjan_api_odds_rows(ev, mk, .api_league("9281", "handball"), .t0),
    error = function(e) e
  )
  expect_s3_class(err, "error")
  expect_false(inherits(err, "lengjan_fetch_error"))
})

test_that("ingest_lengjan_api writes nothing when a selected event's kickoff is unparseable", {
  root <- withr::local_tempdir()
  expect_error(
    suppressMessages(ingest_lengjan_api(
      list(handball_iceland = .api_league("9281", "handball")),
      scraped_at = .t0, root = root,
      fetch_program = function() .bad_kickoff_program(),
      fetch_markets = function(ids) .fx("markets.json")
    )),
    "API may have changed"
  )
  expect_equal(nrow(read_table("odds", root = root)), 0L)
  expect_equal(nrow(read_table("odds_markets", root = root)), 0L)
})

# ---- Every market: the odds_markets table (2026-11-24) -------------------------
#
# The fixture's handball event 4616742 has 30 open, priced selections and the
# basketball event 4412294 has 19 (test-lengjan-api.R).

test_that("lengjan_api_market_rows has the odds_markets columns and every selection", {
  ev <- parse_lengjan_program(.fx("current-program.json"))
  mk <- parse_lengjan_markets(.fx("markets.json"))
  rows <- lengjan_api_market_rows(ev, mk, .api_league("9281", "handball"), .t0)
  expect_named(rows, schemas()$odds_markets$names)
  expect_equal(nrow(rows), 30L)
  expect_true(all(rows$sex == "male"))
  expect_true(all(rows$event_id == "4616742" & rows$competition_id == "9281"))
  expect_equal(unique(rows$home_team), "Stuttgart")
  expect_equal(unique(rows$match_date), as.Date("2026-09-24"))
  expect_equal(unique(rows$scraped_at), .t0)
  expect_equal(nrow(dplyr::distinct(rows[, c("market_id", "selection")])), 30L)
  expect_equal(nrow(lengjan_api_market_rows(ev, mk, .api_league("9281", "basketball"), .t0)), 0L)
  none <- lengjan_api_market_rows(ev, mk, .api_league("1269", "handball"), .t0)
  expect_named(none, schemas()$odds_markets$names)
  expect_equal(nrow(none), 0L)
})

test_that("ingest_lengjan_api writes every market to odds_markets and returns the odds count", {
  root <- withr::local_tempdir()
  leagues <- list(
    handball_x = .api_league("9281", "handball"),
    basketball_x = .api_league("9445", "basketball", sex = "female")
  )
  scrape <- function(at) {
    suppressMessages(ingest_lengjan_api(leagues,
      scraped_at = at, root = root,
      fetch_program = function() .fx("current-program.json"),
      fetch_markets = function(ids) .fx("markets.json")
    ))
  }
  expect_equal(scrape(.t0), 14L)
  back <- read_table("odds_markets", root = root)
  expect_equal(nrow(back), 49L) # 30 handball + 19 basketball
  expect_equal(nrow(read_table("odds", root = root)), 14L)
  expect_setequal(unique(back$sex), c("male", "female"))
  expect_equal(sort(unique(back$market_code[back$sport == "basketball"])), c("1", "18", "234", "47"))
  # A later scrape the same day accretes; the same scrape again does not.
  scrape(.t0 + 3600)
  scrape(.t0 + 3600)
  back <- read_table("odds_markets", root = root)
  expect_equal(nrow(back), 98L)
  expect_equal(as.integer(table(format(back$scraped_at, "%H:%M", tz = "UTC"))), c(49L, 49L))
})

test_that("ingest_one_lengjan hands its program fetcher to the API scraper", {
  seen <- NULL
  testthat::local_mocked_bindings(
    .is_league_active = function(active_path, key) TRUE,
    ingest_lengjan_api = function(leagues, ..., fetch_program) {
      seen <<- fetch_program
      3L
    }
  )
  program <- function() "the run's program"
  res <- suppressMessages(ingest_one_lengjan(
    list(sport = "handball", country = "iceland"),
    list(source = "api", competitions = list(list(id = "1269", name = "x", sex = "male"))),
    "handball_iceland", "active.json",
    fetch_program = program
  ))
  expect_identical(res, 3L)
  expect_identical(seen, program)
})

test_that("two API leagues sharing lengjan_program_once make one program request", {
  calls <- 0L
  program <- lengjan_program_once(function() {
    calls <<- calls + 1L
    .fx("current-program.json")
  })
  root <- withr::local_tempdir()
  for (lg in list(handball_x = .api_league("9281", "handball"), basketball_x = .api_league("9445", "basketball"))) {
    suppressMessages(ingest_lengjan_api(list(lg),
      scraped_at = .t0, root = root,
      fetch_program = program, fetch_markets = function(ids) .fx("markets.json")
    ))
  }
  expect_equal(calls, 1L)
  expect_equal(nrow(read_table("odds", root = root)), 14L)
})

# ---- An unparseable kickoff must not reach the odds write via odds_markets ------
#
# Review of PR #119: the long table keeps markets the odds mapping drops, so an
# event whose only open market is one of those (double chance here) reached
# the kickoff check through odds_markets and aborted the league's odds write,
# which it never reached before the table existed.

.dc_only_event <- function() {
  prog <- .fx("current-program.json")
  ev <- prog$events[[1]] # handball 4616742, comp 9281
  ev$id <- "999"
  ev$datePlayed <- "soon"
  ev$marketCount <- 1L
  ev$market <- NULL
  ev$participants[[1]]$name <- "Kiel"
  ev$participants[[2]]$name <- "Flensburg"
  prog$events <- c(prog$events, list(ev))
  dc <- list(
    id = "99901", version = "1", eventId = "999", type = "2", typeName = "DC",
    externalId = "10", name = "x", primary = FALSE, live = FALSE, status = "open",
    dateClosed = "2026-09-24T16:55:00.000Z",
    selections = list(list(name = "1X", odds = 124), list(name = "12", odds = 107), list(name = "X2", odds = 237))
  )
  list(
    program = prog,
    markets = c(.fx("markets.json"), list(list(name = "single-99901", markets = list(dc))))
  )
}

test_that("an unparseable kickoff on an event with no odds-table market does not block the odds write", {
  fx <- .dc_only_event()
  root <- withr::local_tempdir()
  expect_warning(
    n <- suppressMessages(ingest_lengjan_api(
      list(handball_x = .api_league("9281", "handball")),
      scraped_at = .t0, root = root,
      fetch_program = function() fx$program,
      fetch_markets = function(ids) fx$markets
    )),
    "999"
  )
  expect_equal(n, 9L)
  od <- read_table("odds", root = root)
  expect_equal(nrow(od), 9L)
  expect_equal(unique(od$event_id), "4616742")
  om <- read_table("odds_markets", root = root)
  expect_equal(nrow(om), 30L)
  expect_false("999" %in% om$event_id)
})

test_that("lengjan_api_market_rows drops an event with no parseable kickoff, naming it", {
  fx <- .dc_only_event()
  ev <- parse_lengjan_program(fx$program)
  mk <- parse_lengjan_markets(fx$markets)
  expect_warning(
    rows <- lengjan_api_market_rows(ev, mk, .api_league("9281", "handball"), .t0),
    "999"
  )
  expect_equal(nrow(rows), 30L)
  expect_false(anyNA(rows$match_date))
  # The odds table still stops on such an event when it has an odds-table market.
  bad <- .bad_kickoff_program()
  expect_error(
    lengjan_api_odds_rows(parse_lengjan_program(bad), parse_lengjan_markets(.fx("markets.json")),
      .api_league("9281", "handball"), .t0),
    "4616742"
  )
})

# ---- odds_markets rows: lines and market fields, per row --------------------

test_that("odds_markets rows carry each handicap's signed line and each total's line", {
  ev <- parse_lengjan_program(.fx("current-program.json"))
  mk <- parse_lengjan_markets(.fx("markets.json"))
  fb <- lengjan_api_market_rows(ev, mk, .api_league("10046", "football"), .t0)
  expect_equal(nrow(fb), 151L)
  pick <- function(x, id, sel) x[x$market_id == id & x$selection == sel, ]
  h10 <- pick(fb, "14017594", "1") # "Forgjof 1-0": home starts a goal up
  expect_equal(nrow(h10), 1L)
  expect_equal(h10$special_value, "1")
  expect_equal(h10$line, 1)
  expect_equal(h10$odds, 1.24)
  expect_equal(h10$market_code, "14")
  expect_equal(h10$market_group, "HC_FT")
  h01 <- pick(fb, "14017618", "2") # "Forgjof 0-1": away starts a goal up
  expect_equal(h01$special_value, "-1")
  expect_equal(h01$line, -1)
  expect_equal(h01$odds, 1.72)
  hb <- lengjan_api_market_rows(ev, mk, .api_league("9281", "handball"), .t0)
  over <- pick(hb, "14027143", "Yfir")
  expect_equal(over$line, 56.5)
  expect_equal(over$odds, 1.60)
  expect_equal(over$market_code, "18")
  expect_equal(over$market_type, "OU")
  expect_equal(over$market_version, "1535476427")
  expect_equal(over$market_name, "Yfir/undir 56.5")
  expect_equal(over$closes_at, as.POSIXct("2026-09-24 16:55:00", tz = "UTC"))
  expect_false(over$is_primary)
  expect_false(over$is_live)
})

test_that("a stored odds_markets row keeps its line, version, name and close time", {
  root <- withr::local_tempdir()
  suppressMessages(ingest_lengjan_api(
    list(handball_x = .api_league("9281", "handball")),
    scraped_at = .t0, root = root,
    fetch_program = function() .fx("current-program.json"),
    fetch_markets = function(ids) .fx("markets.json")
  ))
  back <- read_table("odds_markets", root = root)
  under <- back[back$market_id == "14027147" & back$selection == "Undir", ]
  expect_equal(nrow(under), 1L)
  expect_equal(under$line, 58.5)
  expect_equal(under$odds, 1.59)
  expect_equal(under$market_version, "1535476431")
  expect_equal(under$market_name, "Yfir/undir 58.5")
  expect_equal(under$closes_at, as.POSIXct("2026-09-24 16:55:00", tz = "UTC"))
  expect_equal(under$scraped_at, .t0)
})
