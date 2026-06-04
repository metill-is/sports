test_that("parse_competition_page extracts 1x2 odds in long form", {
  html_path <- testthat::test_path("fixtures/lengjan/competition_page.html")
  html <- rvest::read_html(html_path)

  out <- parse_competition_page(html, sport = "football", country = "iceland")

  expect_equal(nrow(out), 3L)
  expect_setequal(out$outcome, c("home", "draw", "away"))
  expect_equal(out$market, rep("moneyline", 3L))
  expect_equal(out$home_team, rep("KR", 3L))
  expect_equal(out$away_team, rep("Valur", 3L))
  expect_equal(
    out$odds[match(c("home", "draw", "away"), out$outcome)],
    c(2.10, 3.50, 3.20)
  )
  expect_true(all(is.na(out$line)))
  # Date parsing: "25. apr <year>" -> 25 April current year (or +/- 1 if past)
  expect_equal(format(out$match_date[[1]], "%m-%d"), "04-25")
})

test_that("parse_match_detail extracts handicap + totals", {
  html_path <- testthat::test_path("fixtures/lengjan/match_detail.html")
  html <- rvest::read_html(html_path)

  match_meta <- list(
    sport = "football", country = "iceland",
    home_team = "KR", away_team = "Valur",
    match_date = as.Date("2026-04-25")
  )
  out <- parse_match_detail(html, match_meta)

  spread_rows <- out[out$market == "spread", ]
  expect_equal(nrow(spread_rows), 2L)
  expect_setequal(spread_rows$outcome, c("home", "away"))
  expect_equal(unique(spread_rows$line), -1.0)
  expect_equal(
    spread_rows$odds[match(c("home", "away"), spread_rows$outcome)],
    c(2.65, 1.45)
  )

  total_rows <- out[out$market == "total", ]
  expect_equal(nrow(total_rows), 2L)
  expect_setequal(total_rows$outcome, c("over", "under"))
  expect_equal(unique(total_rows$line), 2.5)
  expect_equal(
    total_rows$odds[match(c("over", "under"), total_rows$outcome)],
    c(1.80, 2.00)
  )
})

test_that("parse_match_detail extracts 3-way football handicap with range lines", {
  # Football handicap rows on Lengjan are 3-way (home/draw/away) with
  # range-string lines like "0-1", "1-0", "2-0". Regression for a silent
  # parser drop that lost ~all football handicap data; legacy reference at
  # _legacy/lengjan-odds/data/football_iceland/odds_handicap.csv.
  html_path <- testthat::test_path("fixtures/lengjan/match_detail_football.html")
  html <- rvest::read_html(html_path)

  match_meta <- list(
    sport = "football", country = "iceland",
    home_team = "Fram", away_team = "ÍA",
    match_date = as.Date("2026-04-12")
  )
  out <- parse_match_detail(html, match_meta)

  # Silent-drop guard: both market types must be present.
  expect_true("spread" %in% out$market)
  expect_true("total" %in% out$market)

  # 3 handicap rows x 3 outcomes = 9 spread rows. parse_handicap maps
  # "0-1" -> -1, "1-0" -> 1, "2-0" -> 2 (signed home line).
  spread_rows <- out[out$market == "spread", ]
  expect_equal(nrow(spread_rows), 9L)
  expect_setequal(spread_rows$outcome, c("home", "draw", "away"))
  expect_setequal(unique(spread_rows$line), c(-1, 1, 2))

  # Per-line: "0-1" -> line -1, ÍA-favoured leg should have the lowest odds.
  hc_minus1 <- spread_rows[spread_rows$line == -1, ]
  expect_equal(
    hc_minus1$odds[match(c("home", "draw", "away"), hc_minus1$outcome)],
    c(3.42, 4.25, 1.61)
  )

  # 2 totals rows x 2 outcomes = 4 over/under rows; numeric lines preserved.
  total_rows <- out[out$market == "total", ]
  expect_equal(nrow(total_rows), 4L)
  expect_setequal(total_rows$outcome, c("over", "under"))
  expect_setequal(unique(total_rows$line), c(2.5, 3.5))
})

test_that("ingest_lengjan_odds skips chromote bits with skip_if", {
  testthat::skip_if_not(
    nzchar(Sys.getenv("LENGJAN_LIVE_TEST")),
    "set LENGJAN_LIVE_TEST=1 to run live scraper integration"
  )
  leagues <- load_leagues()
  active <- filter_leagues(leagues, active_only = TRUE, has_lengjan = TRUE)
  expect_gt(length(active), 0L)
})

# Fake chromote session helper for retry-behaviour tests. Mirrors the subset of
# the ChromoteSession API that .lengjan_fetch touches: $Page$navigate and
# $Runtime$evaluate. `fail_first_n` makes the first N navigate calls throw
# the same error message chromote uses on a Page.navigate timeout.
make_fake_chromote_session <- function(fail_first_n = 0L,
                                       html = "<html><body>OK</body></html>") {
  call_count <- 0L
  navigate <- function(url) {
    call_count <<- call_count + 1L
    if (call_count <= fail_first_n) {
      stop(
        "Chromote: timed out waiting for response to command Page.navigate"
      )
    }
    invisible()
  }
  evaluate <- function(expr) {
    list(result = list(value = html))
  }
  list(
    Page = list(navigate = navigate),
    Runtime = list(evaluate = evaluate),
    .navigate_calls = function() call_count
  )
}

test_that(".lengjan_fetch returns parsed HTML on the happy path", {
  session <- make_fake_chromote_session(fail_first_n = 0L)
  out <- .lengjan_fetch(
    session, "https://example.com",
    settle_s = 0, max_attempts = 3L, backoff_base_s = 0
  )
  expect_s3_class(out, "xml_document")
  expect_equal(session$.navigate_calls(), 1L)
})

test_that(".lengjan_fetch retries on transient errors and eventually succeeds", {
  session <- make_fake_chromote_session(fail_first_n = 2L)
  out <- suppressMessages(.lengjan_fetch(
    session, "https://example.com",
    settle_s = 0, max_attempts = 3L, backoff_base_s = 0
  ))
  expect_s3_class(out, "xml_document")
  expect_equal(session$.navigate_calls(), 3L)
})

test_that(".lengjan_fetch surfaces the original error after max_attempts", {
  session <- make_fake_chromote_session(fail_first_n = Inf)
  expect_error(
    suppressMessages(.lengjan_fetch(
      session, "https://example.com",
      settle_s = 0, max_attempts = 3L, backoff_base_s = 0
    )),
    "Page.navigate"
  )
  expect_equal(session$.navigate_calls(), 3L)
})

test_that(".lengjan_fetch classes an exhausted-retry failure as lengjan_fetch_error", {
  session <- make_fake_chromote_session(fail_first_n = Inf)
  expect_error(
    suppressMessages(.lengjan_fetch(
      session, "https://example.com",
      settle_s = 0, max_attempts = 2L, backoff_base_s = 0
    )),
    class = "lengjan_fetch_error"
  )
})

test_that("ingest_one_lengjan soft-fails a transient fetch timeout to 0 rows", {
  testthat::local_mocked_bindings(
    .is_league_active = function(active_path, key) TRUE,
    ingest_lengjan_odds = function(...) {
      stop(structure(
        class = c("lengjan_fetch_error", "error", "condition"),
        list(message = "Chromote: timed out waiting for response to command Page.navigate")
      ))
    }
  )
  res <- suppressMessages(ingest_one_lengjan(
    list(sport = "football", country = "iceland"), list(),
    "football_iceland", "active.json"
  ))
  expect_identical(res, 0L)
})

test_that("ingest_one_lengjan still aborts on a non-fetch (parse) error", {
  testthat::local_mocked_bindings(
    .is_league_active = function(active_path, key) TRUE,
    ingest_lengjan_odds = function(...) stop("parse_competition_page: unexpected DOM")
  )
  expect_error(
    suppressMessages(ingest_one_lengjan(
      list(sport = "football", country = "iceland"), list(),
      "football_iceland", "active.json"
    )),
    "parse_competition_page"
  )
})
