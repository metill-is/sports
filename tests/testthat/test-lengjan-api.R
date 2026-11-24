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

# ---- Every market: the odds_markets table (2026-11-24) -------------------------
#
# Counts of open selections priced above 1 in markets.json, read with a
# separate script: football 4601670 has 151 in 37 markets of 28 market codes,
# handball 4616742 has 30 in 9, basketball 4412294 has 19 in 4.

test_that("parse_lengjan_markets keeps each market's id, version, code, name, live flag and close", {
  mk <- parse_lengjan_markets(.fx("markets.json"))
  ml <- mk[mk$event_id == "4616742" & mk$primary, ]
  expect_equal(nrow(ml), 3L)
  expect_equal(unique(ml$market_id), "13904004")
  expect_equal(unique(ml$market_version), "1535685747")
  expect_equal(unique(ml$market_code), "1")
  expect_equal(unique(ml$market_name), "\u00darslit leiksins")
  expect_false(any(ml$live))
  expect_equal(unique(ml$closes_at), as.POSIXct("2026-09-24 16:55:00", tz = "UTC"))
  # `type` is 1 for the primary market and 2 for every other: not a market
  # type. The stable code is `externalId`, 18 for every full-time total.
  ou <- mk[mk$event_id == "4616742" & mk$group == "OU_FT", ]
  expect_equal(unique(ou$type), "2")
  expect_equal(unique(ou$market_code), "18")
  expect_equal(length(unique(ou$market_id)), 3L)
})

test_that("lengjan_markets_long keeps every open, priced selection of every market", {
  long <- lengjan_markets_long(parse_lengjan_markets(.fx("markets.json")))
  ev <- c("4601670", "4616742", "4412294")
  expect_equal(as.integer(table(long$event_id)[ev]), c(151L, 30L, 19L))
  n_markets <- tapply(long$market_id, long$event_id, function(x) length(unique(x)))
  expect_equal(as.integer(n_markets[ev]), c(37L, 9L, 4L))
  codes <- function(e) sort(as.integer(unique(long$market_code[long$event_id == e])))
  expect_equal(codes("4616742"), c(1L, 10L, 18L, 47L, 52L, 60L, 83L))
  expect_equal(codes("4412294"), c(1L, 18L, 47L, 234L))
  expect_length(codes("4601670"), 28L)
  expect_true(all(is.finite(long$odds) & long$odds > 1))
  expect_named(long, c(
    "event_id", "market_id", "market_version", "market_code", "market_type",
    "market_group", "market_name", "special_value", "line", "selection",
    "odds", "is_primary", "is_live", "closes_at"
  ))
  q <- long[long$event_id == "4412294" & long$market_code == "234", ]
  expect_equal(q$odds[q$selection == "Jafnt"], 11.23)
})

test_that("line is special_value as a number, NA for a market without one", {
  long <- lengjan_markets_long(parse_lengjan_markets(.fx("markets.json")))
  ah <- long[long$event_id == "4601670" & long$market_group == "AH_FT", ]
  expect_equal(unique(ah$market_code), "16")
  expect_equal(unique(ah$market_type), "AH")
  expect_setequal(unique(ah$line), c(0.5, -0.5, -1.5))
  expect_equal(ah$special_value[ah$line == -1.5][[1]], "-1.5")
  hc <- long[long$market_code == "14", ]
  expect_setequal(unique(hc$line), c(1, -1, 2, -2, 3, -3))
  dc <- long[long$event_id == "4616742" & long$market_code == "10", ]
  expect_setequal(dc$selection, c("1X", "12", "X2"))
  expect_true(all(is.na(dc$special_value) & is.na(dc$line)))
})

test_that("the long rows carry every price the odds mapping keeps", {
  mk <- parse_lengjan_markets(.fx("markets.json"))
  od <- lengjan_markets_to_odds(mk)
  long <- lengjan_markets_long(mk)
  ml <- long[long$is_primary & long$market_type == "3WAY", ]
  tot <- long[long$market_group == "OU_FT", ]
  hc <- long[long$market_group == "HC_FT", ]
  expect_equal(sort(ml$odds), sort(od$odds[od$market == "moneyline"]))
  expect_equal(sort(tot$odds), sort(od$odds[od$market == "total"]))
  expect_equal(sort(hc$odds), sort(od$odds[od$market == "spread"]))
  expect_equal(nrow(od), 39L)
  expect_equal(nrow(long), 200L)
})

test_that("lengjan_markets_long drops closed markets and invalid prices row by row", {
  groups <- list(list(name = "single-1", markets = list(
    list(
      id = "11", eventId = "1", typeName = "3WAY", primary = TRUE, status = "open",
      selections = list(list(name = "1", odds = 100), list(name = "X", odds = NULL), list(name = "2", odds = 250))
    ),
    list(
      id = "12", eventId = "2", typeName = "3WAY", primary = TRUE, status = "suspended",
      selections = list(list(name = "1", odds = 180), list(name = "X", odds = 340))
    )
  )))
  long <- lengjan_markets_long(parse_lengjan_markets(groups))
  expect_equal(nrow(long), 1L)
  expect_equal(long$selection, "2")
  expect_equal(long$odds, 2.5)
  expect_equal(long$market_id, "11")
  # A field the market lacks reads NA, not an error.
  expect_true(is.na(long$market_version) && is.na(long$closes_at) && !long$is_live)
  expect_equal(nrow(lengjan_markets_long(parse_lengjan_markets(list()))), 0L)
})

test_that("lengjan_api_get treats 429, 500, 502, 503 and 504 as transient", {
  for (st in c(429L, 500L, 502L, 503L, 504L)) {
    expect_true(.lengjan_is_transient(httr2::response(st)), info = st)
  }
  for (st in c(400L, 403L, 404L)) {
    expect_false(.lengjan_is_transient(httr2::response(st)), info = st)
  }
  expect_equal(.lengjan_retry_after(httr2::response(503L, headers = "Retry-After: 5")), 5)
  expect_equal(.lengjan_retry_after(httr2::response(503L, headers = "Retry-After: 3600")), 60)
  expect_true(is.na(.lengjan_retry_after(httr2::response(503L))))
})

# httr2 sleeps a server's Retry-After uncapped (httr2 1.2.2), and
# local_mocked_responses() answers before the retry loop, so the single attempt
# and the sleep are mocked inside httr2 instead.
test_that("a Retry-After of an hour is waited for 60 s, three tries in all", {
  tries <- 0L
  slept <- numeric()
  testthat::local_mocked_bindings(
    req_perform1 = function(req, ...) {
      tries <<- tries + 1L
      httr2::response(503L, headers = "Retry-After: 3600")
    },
    sys_sleep = function(seconds, ...) slept <<- c(slept, seconds),
    .package = "httr2"
  )
  expect_error(lengjan_api_get("current-program"), class = "lengjan_fetch_error")
  expect_equal(tries, 3L)
  # httr2 also calls its sleep with 0 s outside the retry wait; the two waits
  # between the three tries are the positive ones.
  expect_equal(slept[slept > 0], c(60, 60))
})

test_that("a 502 is retried and the success that follows is returned", {
  tries <- 0L
  testthat::local_mocked_bindings(
    req_perform1 = function(req, ...) {
      tries <<- tries + 1L
      if (tries < 3L) httr2::response(502L) else httr2::response_json(body = list(ok = TRUE))
    },
    sys_sleep = function(seconds, ...) NULL,
    .package = "httr2"
  )
  expect_equal(lengjan_api_get("current-program"), list(ok = TRUE))
  expect_equal(tries, 3L)
})

test_that("lengjan_program_once fetches once and does not keep a failed fetch", {
  calls <- 0L
  program <- lengjan_program_once(function() {
    calls <<- calls + 1L
    if (calls == 1L) stop("Lengjan blip")
    list(events = list(), liveSoon = list())
  })
  expect_error(program(), "Lengjan blip")
  expect_equal(program(), list(events = list(), liveSoon = list()))
  program()
  expect_equal(calls, 2L)
})
