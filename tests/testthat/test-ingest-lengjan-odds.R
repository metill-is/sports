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
