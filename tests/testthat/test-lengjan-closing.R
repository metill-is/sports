# Closing odds from the raw Lengjan archive: lengjan_closing_odds(),
# lengjan_closing_results(), lengjan_archive_events() and
# lengjan_closing_coverage(). Market rows are built here in the shape of
# read_lengjan_archive(table = "odds_markets"); no archive or network needed
# except for lengjan_archive_events(), which uses .local_archive().

.utc <- function(x) as.POSIXct(x, tz = "UTC")
.as_of <- .utc("2026-12-01 00:00:00")

# One capture's rows of one market for one event, in the reader's shape.
.mrows <- function(event_id, scraped_at, kickoff_at, market_id = "m1",
                   selection = c("1", "X", "2"), odds = c(2.1, 3.4, 3.0),
                   line = NA_real_, market_code = "1",
                   closes_at = .utc(kickoff_at) - 300, is_live = FALSE,
                   home_team = "Valur kv", away_team = "Haukar kv",
                   sport = "basketball", sex = "female",
                   competition_id = "1291") {
  n <- length(selection)
  kickoff_at <- .utc(kickoff_at)
  tibble::tibble(
    sport = sport, country = "iceland", scraped_at = .utc(scraped_at),
    match_date = as.Date(kickoff_at, tz = "UTC"),
    home_team = home_team, away_team = away_team, sex = sex,
    event_id = event_id, competition_id = competition_id,
    kickoff_at = kickoff_at, market_id = market_id, market_version = "1",
    market_code = market_code, market_type = if (n == 3L) "3WAY" else "2WAY",
    market_group = "g", market_name = "x",
    special_value = if (is.na(line)) NA_character_ else format(line),
    line = line, selection = selection, odds = odds, is_primary = market_code == "1",
    is_live = is_live, closes_at = closes_at,
    sport_id = 2L, sport_name = "Karfa", country_code = "IS",
    competition_name = "c"
  )
}

.total <- function(event_id, scraped_at, kickoff_at, market_id, line, ...) {
  .mrows(event_id, scraped_at, kickoff_at,
    market_id = market_id,
    selection = c("over", "under"), odds = c(1.9, 1.9), line = line,
    market_code = "18", ...
  )
}

test_that("closing is each event's last capture before kick-off, with minutes_before", {
  ko <- "2026-11-23 19:15:00"
  rows <- dplyr::bind_rows(
    .mrows("E1", "2026-11-23 12:00:00", ko, odds = c(2.0, 3.5, 3.1)),
    .mrows("E1", "2026-11-23 18:45:00", ko, odds = c(2.2, 3.4, 2.9)),
    .mrows("E1", "2026-11-23 19:05:00", ko, odds = c(2.3, 3.3, 2.8))
  )
  got <- lengjan_closing_odds(rows, as_of = .as_of)
  expect_equal(nrow(got), 3L)
  expect_equal(unique(got$scraped_at), .utc("2026-11-23 19:05:00"))
  expect_equal(got$odds, c(2.3, 3.3, 2.8))
  expect_equal(unique(got$minutes_before), 10)
  expect_false(any(got$kickoff_changed))
})

test_that("a capture after kick-off is never the closing one", {
  ko <- "2026-11-23 19:15:00"
  rows <- dplyr::bind_rows(
    .mrows("E1", "2026-11-23 19:00:00", ko, odds = c(2.2, 3.4, 2.9)),
    .mrows("E1", "2026-11-23 19:20:00", ko,
      odds = c(1.1, 9.0, 15.0),
      closes_at = .utc("2026-11-23 21:00:00")
    )
  )
  got <- lengjan_closing_odds(rows, as_of = .utc("2026-11-24 00:00:00"))
  expect_equal(unique(got$scraped_at), .utc("2026-11-23 19:00:00"))
  expect_equal(got$odds, c(2.2, 3.4, 2.9))
})

test_that("a line withdrawn before the closing capture does not linger", {
  ko <- "2026-11-23 19:15:00"
  rows <- dplyr::bind_rows(
    .mrows("E1", "2026-11-23 18:00:00", ko),
    .total("E1", "2026-11-23 18:00:00", ko, "t1", 160.5),
    .total("E1", "2026-11-23 18:00:00", ko, "t2", 158.5),
    .mrows("E1", "2026-11-23 19:00:00", ko),
    .total("E1", "2026-11-23 19:00:00", ko, "t2", 158.5)
  )
  got <- lengjan_closing_odds(rows, as_of = .as_of)
  expect_equal(nrow(got), 5L)
  expect_equal(sort(unique(got$market_id)), c("m1", "t2"))
  expect_false(160.5 %in% got$line)
})

test_that("a kick-off moved later uses each capture's own kick-off", {
  rows <- dplyr::bind_rows(
    .mrows("E2", "2026-11-20 12:00:00", "2026-11-20 18:00:00"),
    .mrows("E2", "2026-11-20 19:00:00", "2026-11-21 16:00:00", odds = c(2.5, 3.5, 2.4)),
    .mrows("E2", "2026-11-21 15:30:00", "2026-11-21 16:00:00", odds = c(2.6, 3.5, 2.3))
  )
  got <- lengjan_closing_odds(rows, as_of = .as_of)
  expect_equal(unique(got$scraped_at), .utc("2026-11-21 15:30:00"))
  expect_equal(unique(got$kickoff_at), .utc("2026-11-21 16:00:00"))
  expect_equal(unique(got$match_date), as.Date("2026-11-21"))
  expect_equal(unique(got$minutes_before), 30)
  expect_true(all(got$kickoff_changed))
})

test_that("a kick-off moved earlier cuts off captures that still listed the old time", {
  rows <- dplyr::bind_rows(
    .mrows("E3", "2026-11-20 16:00:00", "2026-11-20 19:00:00", odds = c(2.0, 3.0, 4.0)),
    .mrows("E3", "2026-11-20 17:45:00", "2026-11-20 19:00:00", odds = c(1.2, 6.0, 9.0)),
    .mrows("E3", "2026-11-20 18:30:00", "2026-11-20 17:30:00",
      odds = c(1.1, 8.0, 12.0),
      closes_at = .utc("2026-11-20 20:00:00")
    )
  )
  got <- lengjan_closing_odds(rows, as_of = .as_of)
  expect_equal(unique(got$scraped_at), .utc("2026-11-20 16:00:00"))
  expect_equal(unique(got$kickoff_at), .utc("2026-11-20 17:30:00"))
  expect_equal(unique(got$minutes_before), 90)
  expect_true(all(got$kickoff_changed))
})

test_that("a match brought forward to the day before takes that day as its match_date", {
  rows <- dplyr::bind_rows(
    .mrows("E3", "2026-11-20 12:00:00", "2026-11-22 18:00:00"),
    .mrows("E3", "2026-11-21 19:00:00", "2026-11-21 18:00:00",
      closes_at = .utc("2026-11-21 20:00:00")
    )
  )
  got <- lengjan_closing_odds(rows, as_of = .as_of)
  expect_equal(unique(got$scraped_at), .utc("2026-11-20 12:00:00"))
  expect_equal(unique(got$match_date), as.Date("2026-11-21"))
})

test_that("a market already closed at capture time is dropped from the closing capture", {
  ko <- "2026-11-23 19:15:00"
  rows <- dplyr::bind_rows(
    .mrows("E1", "2026-11-23 19:05:00", ko),
    .total("E1", "2026-11-23 19:05:00", ko, "t1", 160.5,
      closes_at = .utc("2026-11-23 19:00:00")
    )
  )
  got <- lengjan_closing_odds(rows, as_of = .as_of)
  expect_equal(unique(got$market_id), "m1")
})

test_that("an in-play market row never passes for a closing price", {
  ko <- "2026-11-23 19:15:00"
  rows <- dplyr::bind_rows(
    .mrows("E1", "2026-11-23 18:30:00", ko, odds = c(2.2, 3.4, 2.9)),
    .mrows("E1", "2026-11-23 19:05:00", ko, odds = c(1.5, 4.0, 5.0), is_live = TRUE)
  )
  got <- lengjan_closing_odds(rows, as_of = .as_of)
  expect_equal(unique(got$scraped_at), .utc("2026-11-23 18:30:00"))
  expect_false(any(got$is_live))
})

test_that("an event that has not kicked off by as_of has no closing rows", {
  rows <- dplyr::bind_rows(
    .mrows("E1", "2026-11-23 18:00:00", "2026-11-23 19:15:00"),
    .mrows("E4", "2026-11-23 18:00:00", "2026-11-24 19:15:00", home_team = "KR kv")
  )
  expect_equal(unique(lengjan_closing_odds(rows, as_of = .utc("2026-11-23 18:00:00"))$event_id), character())
  got <- lengjan_closing_odds(rows, as_of = .utc("2026-11-23 20:00:00"))
  expect_equal(unique(got$event_id), "E1")
})

test_that("an event whose detail stopped hours early still closes, flagged by minutes_before", {
  rows <- .mrows("E5", "2026-11-20 10:28:00", "2026-11-20 19:30:00")
  got <- lengjan_closing_odds(rows, as_of = .utc("2026-11-21 00:00:00"))
  expect_equal(nrow(got), 3L)
  expect_equal(unique(got$minutes_before), 542)
})

test_that("zero input rows give zero closing rows with the added columns", {
  empty <- .mrows("E1", "2026-11-23 18:00:00", "2026-11-23 19:15:00")[0, ]
  got <- lengjan_closing_odds(empty, as_of = .as_of)
  expect_equal(nrow(got), 0L)
  expect_named(got, c(names(empty), "minutes_before", "kickoff_changed"))
  expect_error(lengjan_closing_odds(empty[, -1], as_of = .as_of), "sport")
  expect_error(lengjan_closing_odds(empty), "as_of")
})

# --- joining closing rows to results -----------------------------------------

.leagues <- function() {
  list(basketball_iceland = list(
    sport = "basketball", country = "iceland",
    lengjan = list(
      competitions = list(list(id = "1291", name = "x", sex = "female", division = "BD")),
      team_names = list(female = list(Valur = "Valur kv", Haukar = "Haukar kv", KR = "KR kv"))
    )
  ))
}

.results <- function(...) {
  base <- tibble::tibble(
    sport = "basketball", country = "iceland", sex = "female", season = 2027L,
    match_date = as.Date("2026-11-23"), home_team = "Valur", away_team = "Haukar",
    home_score = 80L, away_score = 75L, division = "BD", round = 8L,
    game_id = "g1", home_score_reg = 70L, away_score_reg = 70L
  )
  dplyr::bind_rows(base, tibble::tibble(...))
}

test_that("closing rows join to results by canonical names, with regulation scores", {
  closing <- lengjan_closing_odds(.mrows("E1", "2026-11-23 19:00:00", "2026-11-23 19:15:00"),
    as_of = .utc("2026-11-24 00:00:00")
  )
  got <- lengjan_closing_results(closing, .results()[1, ], leagues = .leagues())
  expect_equal(nrow(got), 3L)
  expect_equal(unique(got$home_team), "Valur")
  expect_equal(unique(got$lengjan_home_team), "Valur kv")
  expect_equal(unique(got$result_join), "exact")
  expect_equal(unique(got$home_score), 80L)
  expect_equal(unique(got$home_score_reg), 70L)
  expect_equal(unique(got$away_score_reg), 70L)
  expect_equal(unique(got$result_date), as.Date("2026-11-23"))
})

test_that("a result a day off the listed date joins through the date window", {
  closing <- lengjan_closing_odds(.mrows("E1", "2026-11-23 19:00:00", "2026-11-23 19:15:00"),
    as_of = .utc("2026-11-24 00:00:00")
  )
  res <- .results()[1, ]
  res$match_date <- as.Date("2026-11-24")
  got <- lengjan_closing_results(closing, res, leagues = .leagues())
  expect_equal(unique(got$result_join), "date_window")
  expect_equal(unique(got$result_date), as.Date("2026-11-24"))
  none <- lengjan_closing_results(closing, res, leagues = .leagues(), match_date_window_days = 0L)
  expect_equal(unique(none$result_join), "no_result")
  expect_true(all(is.na(none$home_score)))
})

test_that("two candidate results in the window leave the event unjoined as ambiguous", {
  closing <- lengjan_closing_odds(.mrows("E1", "2026-11-23 19:00:00", "2026-11-23 19:15:00"),
    as_of = .utc("2026-11-24 00:00:00")
  )
  res <- .results(
    match_date = as.Date(c("2026-11-21", "2026-11-25")),
    home_team = "Valur", away_team = "Haukar", sex = "female", sport = "basketball",
    country = "iceland", home_score = c(60L, 90L), away_score = c(50L, 70L)
  )[-1, ]
  got <- lengjan_closing_results(closing, res, leagues = .leagues())
  expect_equal(unique(got$result_join), "ambiguous")
  expect_true(all(is.na(got$home_score)))
})

test_that("an unwired competition and a missing result are named, not dropped", {
  rows <- dplyr::bind_rows(
    .mrows("E1", "2026-11-23 19:00:00", "2026-11-23 19:15:00", home_team = "KR kv"),
    .mrows("E9", "2026-11-23 19:00:00", "2026-11-23 19:15:00",
      sex = NA_character_,
      competition_id = "2306", sport = "handball", home_team = "FH kv", away_team = "HK kv"
    )
  )
  closing <- lengjan_closing_odds(rows, as_of = .utc("2026-11-24 00:00:00"))
  got <- suppressMessages(lengjan_closing_results(closing, .results()[1, ], leagues = .leagues()))
  expect_equal(nrow(got), 6L)
  by_event <- dplyr::distinct(got[, c("event_id", "result_join", "home_team")])
  expect_equal(by_event$result_join[by_event$event_id == "E1"], "no_result")
  expect_equal(by_event$home_team[by_event$event_id == "E1"], "KR")
  expect_equal(by_event$result_join[by_event$event_id == "E9"], "unwired")
  expect_equal(by_event$home_team[by_event$event_id == "E9"], "FH kv")
})

test_that("results without regulation columns still join, with NA regulation scores", {
  closing <- lengjan_closing_odds(.mrows("E1", "2026-11-23 19:00:00", "2026-11-23 19:15:00"),
    as_of = .utc("2026-11-24 00:00:00")
  )
  res <- .results()[1, setdiff(names(.results()), c("game_id", "home_score_reg", "away_score_reg"))]
  got <- lengjan_closing_results(closing, res, leagues = .leagues())
  expect_equal(unique(got$result_join), "exact")
  expect_true(all(is.na(got$home_score_reg)))
})

# --- program listings and coverage -------------------------------------------

test_that("lengjan_archive_events lists each capture's events of one country", {
  dir <- .local_archive(list(
    list(ts = "2026-09-23T12:00:00Z", program = "current-program.json", markets = character()),
    list(ts = "2026-09-23T12:30:00Z", program = "current-program.json", markets = character())
  ))
  is_ev <- lengjan_archive_events(dir, country_code = "IS")
  expect_equal(nrow(is_ev), 6L) # three liveSoon events x two captures
  expect_equal(sort(unique(is_ev$event_id)), c("4602104", "4622914", "4631057"))
  expect_equal(unique(is_ev$country_code), "IS")
  expect_equal(unique(is_ev$market_count), 0L)
  expect_true(all(is_ev$is_live))
  expect_equal(sort(unique(is_ev$sport)), c("football", "handball"))
  de <- lengjan_archive_events(dir, country_code = "DE", from = "2026-09-23T12:15:00Z")
  expect_equal(de$event_id, "4616742")
  expect_equal(de$market_count, 9L)
  expect_equal(format(de$scraped_at, "%H:%M", tz = "UTC"), "12:30")
  expect_named(de, c(
    "scraped_at", "event_id", "sport", "sport_id", "competition_id",
    "competition_name", "country_code", "kickoff_at", "home_team", "away_team",
    "market_count", "is_live"
  ))
})

.listing <- function(event_id, scraped_at, kickoff_at, market_count = 4L, is_live = FALSE,
                     home_team = "Valur kv", away_team = "Haukar kv") {
  tibble::tibble(
    scraped_at = .utc(scraped_at), event_id = event_id, sport = "basketball",
    sport_id = 2L, competition_id = "1291", competition_name = "c",
    country_code = "IS", kickoff_at = .utc(kickoff_at), home_team = home_team,
    away_team = away_team, market_count = market_count, is_live = is_live
  )
}

test_that("coverage counts closing captures within the threshold against listed events", {
  as_of <- .utc("2026-11-24 00:00:00")
  listings <- dplyr::bind_rows(
    .listing("E1", "2026-11-23 19:05:00", "2026-11-23 19:15:00"),
    .listing("E5", "2026-11-23 18:00:00", "2026-11-23 19:30:00", is_live = TRUE),
    .listing("E6", "2026-11-23 18:00:00", "2026-11-23 19:30:00", is_live = TRUE),
    .listing("E7", "2026-11-23 18:00:00", "2026-11-23 19:30:00", market_count = 0L),
    .listing("E8", "2026-11-23 18:00:00", "2026-11-24 19:30:00")
  )
  closing <- lengjan_closing_odds(dplyr::bind_rows(
    .mrows("E1", "2026-11-23 19:05:00", "2026-11-23 19:15:00"),
    .mrows("E5", "2026-11-23 10:30:00", "2026-11-23 19:30:00")
  ), as_of = as_of)
  cov <- lengjan_closing_coverage(listings, closing, as_of = as_of)
  expect_equal(sort(cov$event_id), c("E1", "E5", "E6")) # E7 never had markets, E8 not yet played
  status <- stats::setNames(cov$status, cov$event_id)
  expect_equal(status[["E1"]], "within")
  expect_equal(status[["E5"]], "stale")
  expect_equal(status[["E6"]], "no_detail")
  expect_equal(cov$minutes_before[cov$event_id == "E5"], 540)
  expect_true(cov$live_flagged[cov$event_id == "E6"])
  wide <- lengjan_closing_coverage(listings, closing, as_of = as_of, within_minutes = 600)
  expect_equal(wide$status[wide$event_id == "E5"], "within")
})

test_that("coverage carries the result join when the closing rows have one", {
  as_of <- .utc("2026-11-24 00:00:00")
  closing <- lengjan_closing_odds(.mrows("E1", "2026-11-23 19:05:00", "2026-11-23 19:15:00"), as_of = as_of)
  joined <- lengjan_closing_results(closing, .results()[1, ], leagues = .leagues())
  cov <- lengjan_closing_coverage(.listing("E1", "2026-11-23 19:05:00", "2026-11-23 19:15:00"),
    joined,
    as_of = as_of
  )
  expect_equal(cov$result_join, "exact")
  expect_equal(cov$home_team, "Valur kv") # coverage keeps Lengjan's names
})
