# Shadow comparison of the Lengjan odds scrapers (spec 2026-09-23 WS3):
# compare_odds_sources(), the in-memory DOM scrape, the API snapshot and the
# team-name audit. `.fx()` (the API fixture reader) lives in
# helper-lengjan-api.R.

.cmp_row <- function(home = "A", away = "B", market = "moneyline",
                     outcome = "home", line = NA_real_, odds = 2.00,
                     match_date = as.Date("2030-05-01")) {
  tibble::tibble(
    match_date = match_date, home_team = home, away_team = away,
    market = market, outcome = outcome, line = line, odds = odds
  )
}

.cmp_cause <- function(cmp, side, outcome) {
  d <- if (side == "dom") cmp$dom else cmp$api_only
  d$cause[d$outcome == outcome]
}

test_that("a DOM row passes on the price of either bracketing snapshot", {
  dom <- dplyr::bind_rows(
    .cmp_row(outcome = "home", odds = 2.10),
    .cmp_row(outcome = "draw", odds = 3.40),
    .cmp_row(outcome = "away", odds = 3.00)
  )
  before <- dplyr::bind_rows(
    .cmp_row(outcome = "home", odds = 2.10), # same before
    .cmp_row(outcome = "draw", odds = 3.50), # moved: DOM saw the after price
    .cmp_row(outcome = "away", odds = 3.10)
  )
  after <- dplyr::bind_rows(
    .cmp_row(outcome = "home", odds = 2.05),
    .cmp_row(outcome = "draw", odds = 3.40),
    .cmp_row(outcome = "away", odds = 3.20) # DOM's 3.00 is in neither
  )
  cmp <- compare_odds_sources(dom, before, after)
  expect_equal(cmp$dom$verdict, c("pass", "pass", "fail"))
  expect_equal(.cmp_cause(cmp, "dom", "away"), "odds_mismatch")
  expect_equal(cmp$dom$odds_before, c(2.10, 3.50, 3.10))
  expect_equal(cmp$dom$odds_after, c(2.05, 3.40, 3.20))
  expect_false(cmp$summary$pass)
  expect_equal(cmp$summary$dom_fail, 1L)
  expect_equal(nrow(cmp$api_only), 0L)
})

test_that("prices compare to the cent, not by floating-point identity", {
  dom <- .cmp_row(odds = 1.39)
  api <- .cmp_row(odds = 139 / 100)
  api$odds <- api$odds + 1e-12
  cmp <- compare_odds_sources(dom, api, api)
  expect_equal(cmp$dom$verdict, "pass")
})

.cmp_visit <- function(detail, home = "A", away = "B", n_rows = 0L) {
  tibble::tibble(
    league = "x", competition_id = "1", position = seq_along(detail),
    event_id = as.character(seq_along(detail)), href = "",
    match_date = as.Date("2030-05-01"), home_team = home, away_team = away,
    detail = detail, n_rows = n_rows
  )
}

test_that("a run passes only with DOM rows and a compared detail page", {
  ml <- dplyr::bind_rows(
    .cmp_row(outcome = "home", odds = 2.1), .cmp_row(outcome = "draw", odds = 3.4),
    .cmp_row(outcome = "away", odds = 3.0)
  )
  hc <- .cmp_row(market = "spread", outcome = "home", line = -1, odds = 3.9)
  api <- dplyr::bind_rows(ml, hc)

  # No DOM rows at all: all() of nothing is TRUE, which must not read as PASS.
  none <- compare_odds_sources(ml[0, ], api, api, visits = .cmp_visit(character()))
  expect_false(none$summary$pass)
  expect_match(none$summary$fail_reason, "no_dom_rows")

  # Every detail page failed: the 1x2 rows match, the handicap is API-only
  # as dom_detail_failed (an allowed cause), yet nothing was compared.
  failed <- compare_odds_sources(ml, api, api, visits = .cmp_visit("failed"))
  expect_true(all(failed$dom$verdict == "pass"))
  expect_equal(failed$api_only$cause, "dom_detail_failed")
  expect_false(failed$summary$pass)
  expect_match(failed$summary$fail_reason, "no_detail_page_ok")
  expect_match(failed$summary$fail_reason, "no_shared_detail")

  # A detail page that loads but renders no rows (a detail-page layout change).
  blank <- compare_odds_sources(ml, api, api, visits = .cmp_visit("ok"))
  expect_false(blank$summary$pass)
  expect_equal(blank$summary$fail_reason, "no_shared_detail")

  # The control: 1x2 plus a handicap row from a detail page that loaded.
  good <- compare_odds_sources(api, api, api, visits = .cmp_visit("ok", n_rows = 1L))
  expect_true(good$summary$pass)
  expect_true(is.na(good$summary$fail_reason))
  expect_equal(good$summary$shared_detail_matches, 1L)
})

test_that("each failing DOM row gets the first cause that applies", {
  d0 <- as.Date("2030-05-01")
  dom <- dplyr::bind_rows(
    .cmp_row(market = "spread", outcome = "home", line = 1, odds = 1.5), # suspended
    .cmp_row(market = "spread", outcome = "draw", line = 2, odds = 4.0), # line not on api
    .cmp_row(market = "total", outcome = "over", line = 2.5, odds = 1.9), # market not on api
    .cmp_row(home = "C", away = "D", outcome = "draw", match_date = d0 + 1), # date differs
    .cmp_row(home = "Newcastle", away = "E", outcome = "away"), # whitespace
    .cmp_row(home = "X", away = "Y", outcome = "home") # not on api
  )
  api <- dplyr::bind_rows(
    .cmp_row(market = "moneyline", outcome = "draw", odds = 3.3),
    .cmp_row(market = "spread", outcome = "home", line = -1, odds = 1.8),
    .cmp_row(home = "C", away = "D", outcome = "draw", match_date = d0),
    .cmp_row(home = "Newcastle ", away = "E", outcome = "away")
  )
  closed <- .cmp_row(market = "spread", outcome = "home", line = 1, odds = NA_real_)
  cmp <- compare_odds_sources(dom, api, api, api_closed = closed)
  expect_equal(cmp$dom$cause, c(
    "suspended_on_api", "line_not_on_api", "market_not_on_api",
    "date_differs", "name_whitespace", "match_not_on_api"
  ))
  expect_true(all(cmp$dom$verdict == "fail"))
  # The API side of the same mismatches.
  api_only <- cmp$api_only
  expect_equal(api_only$cause[api_only$home_team == "C"], "date_differs")
  expect_equal(api_only$cause[api_only$home_team == "Newcastle "], "name_whitespace")
})

test_that("API rows without a DOM row are listed with a cause", {
  m1 <- list(home = "A", away = "B")
  dom <- dplyr::bind_rows(
    .cmp_row(outcome = "home", odds = 2.0),
    .cmp_row(market = "spread", outcome = "home", line = 1, odds = 1.5),
    .cmp_row(market = "spread", outcome = "away", line = 1, odds = 5.0),
    .cmp_row(home = "F", away = "G", outcome = "home", odds = 1.7),
    .cmp_row(home = "H", away = "I", outcome = "home", odds = 1.7),
    .cmp_row(home = "J", away = "K", outcome = "home", odds = 1.7),
    .cmp_row(home = "J", away = "K", market = "spread", outcome = "home", line = 1, odds = 2.2)
  )
  both <- dplyr::bind_rows(
    dom,
    .cmp_row(market = "spread", outcome = "draw", line = 1, odds = 4.0), # outcome not on dom
    .cmp_row(market = "spread", outcome = "home", line = 2, odds = 1.2), # extra line
    .cmp_row(home = "F", away = "G", market = "total", outcome = "over", line = 2.5, odds = 1.9), # not visited
    .cmp_row(home = "H", away = "I", market = "total", outcome = "over", line = 2.5, odds = 1.9), # failed
    .cmp_row(home = "J", away = "K", market = "total", outcome = "over", line = 2.5, odds = 1.9), # market not on dom
    .cmp_row(home = "L", away = "M", outcome = "home", odds = 1.4) # match not on dom
  )
  after <- dplyr::bind_rows(
    both,
    .cmp_row(market = "spread", outcome = "home", line = 3, odds = 1.1) # opened during run
  )
  visits <- tibble::tibble(
    league = "x", competition_id = "1", position = 1:3,
    event_id = c("10", "11", "12"), href = "", match_date = as.Date("2030-05-01"),
    home_team = c("F", "H", "J"), away_team = c("G", "I", "K"),
    detail = c("not_visited", "failed", "ok"), n_rows = c(0L, 0L, 3L)
  )
  cmp <- compare_odds_sources(dom, both, after, visits = visits)
  expect_true(cmp$summary$pass)
  a <- cmp$api_only
  cause_of <- function(home, market, line, outcome) {
    a$cause[a$home_team == home & a$market == market &
      (is.na(line) & is.na(a$line) | a$line %in% line) & a$outcome == outcome]
  }
  expect_equal(cause_of("A", "spread", 1, "draw"), "outcome_not_on_dom")
  expect_equal(cause_of("A", "spread", 2, "home"), "extra_line_on_api")
  expect_equal(cause_of("A", "spread", 3, "home"), "changed_during_run")
  expect_equal(cause_of("F", "total", 2.5, "over"), "dom_detail_not_visited")
  expect_equal(cause_of("H", "total", 2.5, "over"), "dom_detail_failed")
  expect_equal(cause_of("J", "total", 2.5, "over"), "market_not_on_dom")
  expect_equal(cause_of("L", "moneyline", NA, "home"), "match_not_on_dom")
  expect_equal(nrow(a), 7L)
  expect_true(all(a$cause %in% sports:::.API_ONLY_CAUSES))
})

test_that("a detail page whose link is another match is caught as mispaired", {
  # The DOM paired the 2nd link (event 22, C v D on the API) with A v B by
  # position, so its handicap rows were written under A v B.
  dom <- dplyr::bind_rows(
    .cmp_row(outcome = "home", odds = 2.0),
    .cmp_row(market = "spread", outcome = "home", line = 1, odds = 1.3)
  )
  api <- dplyr::bind_rows(
    .cmp_row(outcome = "home", odds = 2.0),
    .cmp_row(market = "spread", outcome = "home", line = 1, odds = 1.6)
  )
  visits <- tibble::tibble(
    league = "x", competition_id = "1", position = 2L, event_id = "22",
    href = "/leikur?id=22", match_date = as.Date("2030-05-01"),
    home_team = "A", away_team = "B", detail = "ok", n_rows = 1L
  )
  events <- tibble::tibble(
    event_id = c("21", "22"), home_team = c("A", "C"), away_team = c("B", "D"),
    kickoff_at = as.POSIXct("2030-05-01 12:00", tz = "UTC")
  )
  cmp <- compare_odds_sources(dom, api, api, visits = visits, api_events = events)
  expect_equal(cmp$dom$cause, c(NA, "dom_detail_mispaired"))
  expect_equal(nrow(cmp$mispaired), 1L)
  expect_equal(cmp$mispaired$api_home_team, "C")
  expect_equal(cmp$summary$mispaired_visits, 1L)
})

test_that("keys listed twice on one side are reported", {
  dom <- dplyr::bind_rows(.cmp_row(odds = 2), .cmp_row(odds = 2))
  cmp <- compare_odds_sources(dom, dom[1, ], dom[1, ])
  expect_equal(cmp$duplicates$side, "dom")
  expect_equal(cmp$summary$duplicate_keys, 1L)
})

test_that("odds_comparison_causes counts DOM failures and API-only rows apart", {
  dom <- .cmp_row(odds = 2.5)
  api <- dplyr::bind_rows(.cmp_row(odds = 2.4), .cmp_row(outcome = "draw", odds = 3))
  out <- odds_comparison_causes(compare_odds_sources(dom, api, api))
  expect_equal(out$side, c("dom_fail", "api_only"))
  expect_equal(out$cause, c("odds_mismatch", "outcome_not_on_dom"))
  expect_equal(out$n, c(1L, 1L))
})

# --- the in-memory DOM scrape --------------------------------------------------

.dom_league <- function() {
  list(x = list(
    sport = "football", country = "iceland",
    lengjan = list(competitions = list(list(id = "14", name = "c", sex = "male")))
  ))
}

.dom_fetch <- function(fail_detail = FALSE) {
  calls <- character()
  fetch <- function(session, url, expand_aria = character(0)) {
    calls <<- c(calls, url)
    if (grepl("marketTab=allMarkets", url, fixed = TRUE)) {
      if (fail_detail) stop("timed out")
      return(rvest::read_html(testthat::test_path("fixtures/lengjan/match_detail_football.html")))
    }
    rvest::read_html(testthat::test_path("fixtures/lengjan/competition_page.html"))
  }
  list(fetch = fetch, calls = function() calls)
}

test_that("scrape_lengjan_dom returns rows, visits and pages and writes nothing", {
  f <- .dom_fetch()
  t0 <- as.POSIXct("2026-04-10 12:00", tz = "UTC")
  out <- suppressMessages(scrape_lengjan_dom(
    .dom_league(),
    scraped_at = t0, chromote_session = "fake", fetch = f$fetch,
    delay_s = c(min = 0, max = 0)
  ))
  expect_equal(nrow(out$rows), 3L + 9L + 4L) # 1x2 + 3 handicap lines + 2 totals
  expect_true(all(out$rows$scraped_at == t0))
  expect_equal(out$visits$event_id, "42") # from href "/?sport=1&competition=14&match=42"
  expect_equal(out$visits$detail, "ok")
  expect_equal(out$visits$n_rows, 13L)
  expect_equal(out$visits$home_team, "KR")
  expect_equal(out$pages$n_links, 1L)
  expect_equal(out$pages$n_matches, 1L)
  expect_length(f$calls(), 2L)
})

test_that("scrape_lengjan_dom records skipped and failed detail pages", {
  f <- .dom_fetch()
  capped <- suppressMessages(scrape_lengjan_dom(
    .dom_league(),
    chromote_session = "fake", fetch = f$fetch, max_detail = 0,
    delay_s = c(min = 0, max = 0)
  ))
  expect_equal(capped$visits$detail, "not_visited")
  expect_equal(nrow(capped$rows), 3L)
  expect_length(f$calls(), 1L) # the competition page only

  g <- .dom_fetch(fail_detail = TRUE)
  failed <- suppressMessages(scrape_lengjan_dom(
    .dom_league(),
    chromote_session = "fake", fetch = g$fetch, delay_s = c(min = 0, max = 0)
  ))
  expect_equal(failed$visits$detail, "failed")
  expect_equal(nrow(failed$rows), 3L)
})

.dom_page <- function(odds = c("2.10", "3.40", "3.05"), containers = TRUE) {
  btns <- paste0(
    '<button class="uazl1c1"><div class="h7cub57"><p>', odds, "</p></div></button>",
    collapse = ""
  )
  body <- if (containers) {
    paste0(
      '<div class="lj1n6v0"><a class="lj1n6v1" href="/?sport=1&match=42"><p>10. apr.</p></a>',
      '<div class="lj1n6v9"><span>KR</span><span>-</span><span>Valur</span></div>',
      '<ol class="lj1n6vd">', btns, "</ol></div>"
    )
  } else {
    "<div>Engir leikir</div>"
  }
  rvest::read_html(paste0("<html><body>", body, "</body></html>"))
}

test_that("match containers that parse to nothing raise lengjan_parse_error", {
  # A layout or selector change leaves the containers but no numeric odds or
  # team spans. main crashed there ("Column `sport` doesn't exist"), turning
  # scrape-odds red; it must stay a loud failure, not a green 0-row scrape.
  good <- function(session, url, expand_aria = character(0)) .dom_page()
  ok <- suppressMessages(scrape_lengjan_dom(
    .dom_league(),
    chromote_session = "fake", fetch = good, delay_s = c(min = 0, max = 0)
  ))
  expect_equal(ok$rows$odds, c(2.10, 3.40, 3.05)) # the page itself is well formed

  broken <- function(session, url, expand_aria = character(0)) {
    .dom_page(odds = c("-", "-", "-"))
  }
  err <- expect_error(
    suppressMessages(scrape_lengjan_dom(
      .dom_league(),
      chromote_session = "fake", fetch = broken, delay_s = c(min = 0, max = 0)
    )),
    class = "lengjan_parse_error"
  )
  expect_false(inherits(err, "lengjan_fetch_error")) # not soft-failed upstream
  expect_match(conditionMessage(err), "1 match container")

  testthat::local_mocked_bindings(.lengjan_fetch = broken)
  expect_error(
    suppressMessages(ingest_lengjan_odds(
      .dom_league(),
      root = withr::local_tempdir(), chromote_session = "fake"
    )),
    class = "lengjan_parse_error"
  )
})

test_that("a competition page without match containers is still 0 rows, no error", {
  empty <- function(session, url, expand_aria = character(0)) {
    .dom_page(containers = FALSE)
  }
  out <- suppressMessages(scrape_lengjan_dom(
    .dom_league(),
    chromote_session = "fake", fetch = empty, delay_s = c(min = 0, max = 0)
  ))
  expect_equal(nrow(out$rows), 0L)
  expect_equal(out$pages$n_containers, 0L)
})

test_that("ingest_lengjan_odds writes exactly the rows scrape_lengjan_dom returns", {
  root <- withr::local_tempdir()
  rows <- .cmp_row(home = "KR", away = "Valur", odds = 2.1)
  rows <- dplyr::bind_cols(
    tibble::tibble(
      sport = "football", country = "iceland",
      scraped_at = as.POSIXct("2026-04-10 12:00", tz = "UTC")
    ),
    rows
  )
  rows <- rows[, c(
    "sport", "country", "scraped_at", "match_date", "home_team",
    "away_team", "market", "outcome", "line", "odds"
  )]
  testthat::local_mocked_bindings(
    scrape_lengjan_dom = function(...) list(rows = rows)
  )
  n <- suppressMessages(ingest_lengjan_odds(.dom_league(), root = root))
  expect_equal(n, 1L)
  back <- read_table("odds", root = root)
  expect_equal(back$home_team, "KR")
  expect_equal(back$odds, 2.1)
})

test_that("the href event id reads id= and match= parameters", {
  expect_equal(
    sports:::.lengjan_href_event_id(c(
      "/getraunaleikir/lengjan/leikur?id=4631057&sport=1", "/?sport=1&match=42", "/x"
    )),
    c("4631057", "42", NA)
  )
})

# --- the API snapshot ----------------------------------------------------------

.snap_league <- function() {
  list(fb = list(
    sport = "football", country = "england",
    lengjan = list(source = "api", competitions = list(
      list(id = "10046", name = "x", sex = "male")
    ))
  ))
}

test_that("lengjan_api_snapshot maps what the API scrape would write", {
  t0 <- as.POSIXct("2026-09-23 17:17:00", tz = "UTC")
  ev <- parse_lengjan_program(.fx("current-program.json"))
  snap <- lengjan_api_snapshot(
    .snap_league(),
    events = ev,
    fetch_program = function() stop("events were given"),
    fetch_markets = function(ids) {
      expect_equal(ids, "4601670")
      .fx("markets.json")
    },
    now = function() t0
  )
  expect_equal(snap$taken_at, t0)
  expect_equal(snap$events$event_id, "4601670")
  ref <- lengjan_api_odds_rows(
    ev, parse_lengjan_markets(.fx("markets.json")), .snap_league()$fb, t0
  )
  expect_equal(snap$odds, ref)
  expect_equal(nrow(snap$odds), 25L)
  expect_equal(nrow(snap$closed), 0L)
})

test_that("lengjan_api_snapshot lists suspended selections as closed", {
  groups <- .fx("markets.json")
  for (g in seq_along(groups)) {
    if (identical(groups[[g]]$name, "HC_FT")) {
      groups[[g]]$markets[[1]]$status <- "suspended"
      break
    }
  }
  ev <- parse_lengjan_program(.fx("current-program.json"))
  snap <- lengjan_api_snapshot(
    .snap_league(),
    events = ev, fetch_markets = function(ids) groups, now = Sys.time
  )
  expect_equal(nrow(snap$odds), 25L - 3L)
  expect_equal(nrow(snap$closed), 3L)
  expect_true(all(snap$closed$market == "spread"))
  # The closed rows key the selections the odds rows lost.
  full <- lengjan_api_odds_rows(ev, parse_lengjan_markets(.fx("markets.json")), .snap_league()$fb, Sys.time())
  lost <- dplyr::anti_join(full, snap$odds, by = c("market", "outcome", "line"))
  expect_setequal(paste(lost$outcome, lost$line), paste(snap$closed$outcome, snap$closed$line))
})

test_that("lengjan_api_snapshot makes no markets request when nothing is open", {
  ev <- parse_lengjan_program(.fx("current-program.json"))
  lg <- .snap_league()
  lg$fb$lengjan$competitions[[1]]$id <- "no-such-comp"
  snap <- lengjan_api_snapshot(lg, events = ev, fetch_markets = function(ids) stop("asked"))
  expect_equal(nrow(snap$odds), 0L)
  expect_equal(nrow(snap$events), 0L)
})

# --- team-name audit -------------------------------------------------------------

.audit_league <- function() {
  list(lengjan = list(team_names = list(
    male = list(Valur = "Valur", `Víkingur R.` = list("Víkingur Reykjavík", "Víkingur Rvk"), KA = "KA"),
    female = list(Valur = "Valur kv", FH = "FH kv")
  )))
}

test_that("audit_lengjan_team_names classifies names by their own sex", {
  names <- tibble::tibble(
    team = c("Valur kv", "Víkingur Rvk", "Valur ", "FH", "KA", "Nobody", "KA", "Valur"),
    sex = c("female", "male", "male", "female", "female", "male", NA, "female")
  )
  out <- audit_lengjan_team_names(names, .audit_league())
  expect_equal(out$status, c(
    "mapped", "mapped", "whitespace", "canonical_unlisted", "other_sex",
    "unmapped", "mapped",
    # A female canonical shown bare: decide passes it through to the right team.
    "canonical_unlisted"
  ))
  expect_equal(out$canonical[1:3], c("Valur", "Víkingur R.", "Valur"))
  expect_equal(out$sex_hit, c("female", "male", "male", "female", NA, NA, "male", "female"))
})

test_that("team_name_rendering_coverage marks primaries and unseen renderings", {
  cov <- team_name_rendering_coverage(.audit_league(), seen = c("Valur", "Víkingur Rvk"))
  vik <- cov[cov$canonical == "Víkingur R.", ]
  expect_equal(vik$rendering, c("Víkingur Reykjavík", "Víkingur Rvk"))
  expect_equal(vik$primary, c(TRUE, FALSE))
  expect_equal(vik$seen, c(FALSE, TRUE))
  expect_equal(nrow(cov), 6L)
})

# --- the scripts stay local and read-only ---------------------------------------

test_that("the compare and audit scripts write nothing to data/ and stay off CI", {
  scripts <- testthat::test_path(
    "..", "..", "scripts",
    c("0Nc_compare_odds_sources.R", "0Nc_audit_team_names.R")
  )
  for (s in scripts) {
    code <- readLines(s, warn = FALSE)
    code <- code[!grepl("^\\s*#", code)]
    expect_false(any(grepl(
      "write_table|upsert_table|ingest_lengjan|write_parquet|commit_",
      code
    )), info = basename(s))
  }
  wf <- list.files(testthat::test_path("..", "..", ".github", "workflows"),
    pattern = "[.]ya?ml$", full.names = TRUE
  )
  hits <- unlist(lapply(wf, function(f) grep("0Nc_", readLines(f, warn = FALSE), value = TRUE)))
  expect_length(hits, 0L)
})
