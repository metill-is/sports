# Lengjan JSON API client (spec 2026-09-23 WS2). Fixtures captured live
# 2026-09-23 -- see docs/superpowers/plans/2026-09-23-hb-bb-lengjan-odds-milestone-a.md
# Task 5 for their contents. No network. `.fx()` lives in helper-lengjan-api.R.

test_that("parse_lengjan_program unions the arrays and de-duplicates events", {
  ev <- parse_lengjan_program(.fx("current-program.json"))
  # 3 in events + 3 in liveSoon; the football event repeated in popular once.
  expect_equal(nrow(ev), 6L)
  expect_equal(anyDuplicated(ev$event_id), 0L)
  fh <- ev[ev$event_id == "4602104", ]
  expect_equal(fh$sport_id, 6L)
  expect_equal(fh$competition_id, "1269")
  expect_equal(fh$country_code, "IS") # countryName fallback: no countryCode
  expect_equal(fh$home_team, "FH")
  expect_equal(fh$away_team, "Haukar")
  expect_equal(fh$market_count, 0L)
  expect_equal(fh$kickoff_at, as.POSIXct("2026-09-24 19:30:00", tz = "UTC"))
  expect_equal(ev$country_code[ev$event_id == "4616742"], "DE")
})

test_that("parse_lengjan_program fails loudly on an unexpected shape", {
  expect_error(
    parse_lengjan_program(list(data = list())),
    "unexpected current-program shape"
  )
})

test_that("parse_lengjan_markets fails loudly on a group without markets", {
  expect_error(
    parse_lengjan_markets(list(list(name = "OU_FT", rows = list()))),
    "without a `markets` array"
  )
})

test_that("parse_lengjan_markets divides the integer prices by 100", {
  mk <- parse_lengjan_markets(.fx("markets.json"))
  ml <- mk[mk$event_id == "4616742" & mk$primary, ]
  expect_equal(ml$odds[match(c("1", "X", "2"), ml$selection)], c(1.39, 8.83, 3.23))
})

test_that("lengjan_markets_to_odds keeps exactly 1X2, OU_FT totals and HC_FT spreads", {
  od <- lengjan_markets_to_odds(parse_lengjan_markets(.fx("markets.json")))
  n <- table(od$event_id)
  expect_equal(as.integer(n[["4616742"]]), 9L) # handball: 1X2 + 3 total lines
  expect_equal(as.integer(n[["4412294"]]), 5L) # basketball: regulation 1X2 + 1 total line
  expect_equal(as.integer(n[["4601670"]]), 25L) # football: 1X2 + 2 totals + 6 handicaps
  expect_setequal(unique(od$market[od$event_id == "4616742"]), c("moneyline", "total"))
  tot <- od[od$event_id == "4616742" & od$market == "total", ]
  expect_setequal(tot$line, c(56.5, 57.5, 58.5))
  expect_equal(tot$odds[tot$line == 56.5 & tot$outcome == "over"], 1.60)
  # The half-time 3WAY (type "2", not primary) must not become the moneyline.
  ml <- od[od$event_id == "4616742" & od$market == "moneyline", ]
  expect_equal(ml$odds[match(c("home", "draw", "away"), ml$outcome)], c(1.39, 8.83, 3.23))
})

test_that("HC_FT specialValue is home's signed handicap, as parse_handicap() encodes it", {
  od <- lengjan_markets_to_odds(parse_lengjan_markets(.fx("markets.json")))
  sp <- od[od$event_id == "4601670" & od$market == "spread", ]
  expect_setequal(unique(sp$line), c(1, -1, 2, -2, 3, -3))
  expect_equal(parse_handicap("1-0"), 1) # the DOM path's encoding of "Forgjof 1-0"
  expect_equal(sp$odds[sp$line == 1 & sp$outcome == "home"], 1.24)
})

test_that("closed markets and invalid prices are dropped row by row", {
  groups <- list(list(name = "single-1", markets = list(
    list(
      eventId = "1", type = "1", typeName = "3WAY", primary = TRUE, status = "open",
      selections = list(
        list(name = "1", odds = 100), # 1.00: not a valid decimal price
        list(name = "X", odds = NULL), # no price
        list(name = "2", odds = 250)
      )
    ),
    list(
      eventId = "2", type = "1", typeName = "3WAY", primary = TRUE, status = "suspended",
      selections = list(
        list(name = "1", odds = 180), list(name = "X", odds = 340), list(name = "2", odds = 400)
      )
    )
  )))
  od <- lengjan_markets_to_odds(parse_lengjan_markets(groups))
  expect_equal(nrow(od), 1L)
  expect_equal(od$outcome, "away")
  expect_equal(od$odds, 2.5)
})

test_that("lengjan_fetch_markets sends at most 20 ids per request", {
  seen <- list()
  testthat::local_mocked_bindings(lengjan_api_get = function(path, query = list()) {
    seen[[length(seen) + 1L]] <<- query
    list()
  })
  lengjan_fetch_markets(as.character(1:45), pause_s = 0)
  expect_equal(length(seen), 3L)
  n_ids <- vapply(seen, function(q) sum(startsWith(names(q), "eventIds[")), integer(1))
  expect_equal(n_ids, c(20L, 20L, 5L))
  expect_equal(names(seen[[2]])[[1]], "eventIds[0]") # indices restart per batch
  expect_true(all(vapply(seen, function(q) identical(q$live, "false"), logical(1))))
})

test_that("lengjan_fetch_markets makes no request for an empty id set", {
  testthat::local_mocked_bindings(lengjan_api_get = function(...) stop("no request expected"))
  expect_equal(lengjan_fetch_markets(character(0)), list())
})

test_that("lengjan_api_get raises lengjan_fetch_error on transport failure", {
  testthat::local_mocked_bindings(.lengjan_perform = function(req) stop("Could not resolve host"))
  expect_error(lengjan_api_get("current-program"), class = "lengjan_fetch_error")
})

test_that("lengjan_api_get retries transport failures and sends the pipeline UA", {
  captured <- NULL
  testthat::local_mocked_bindings(.lengjan_perform = function(req) {
    captured <<- req
    stop("offline")
  })
  expect_error(
    lengjan_api_get("markets", list(`eventIds[0]` = "1", live = "false")),
    class = "lengjan_fetch_error"
  )
  # httr2 retries only HTTP 429/503 unless retry_on_failure is set, so a
  # timeout or DNS blip would otherwise zero the league's odds for the run.
  expect_true(captured$policies$retry_on_failure)
  expect_equal(captured$policies$retry_max_tries, 3L)
  expect_identical(
    captured$options$useragent,
    "sports-pipeline (+https://github.com/metill-is/sports)"
  )
  expect_true(startsWith(captured$url, "https://games.lotto.is/api/proxy/lengjan/markets"))
  q <- httr2::url_parse(captured$url)$query
  expect_identical(q$live, "false")
  expect_identical(q[["eventIds[0]"]], "1")
})
