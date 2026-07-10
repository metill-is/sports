fixture <- function(name) {
  testthat::test_path("fixtures", "ksi_football", name)
}

test_that("parse_ksi_results_page extracts matches into canonical columns", {
  skip_if_not(
    file.exists(fixture("male_div1_2026.html")),
    "no ksi fixture captured"
  )

  html <- rvest::read_html(
    fixture("male_div1_2026.html"),
    encoding = "UTF-8"
  )
  parsed <- parse_ksi_results_page(
    html,
    sport = "football", country = "iceland", sex = "male",
    division = "BD", season = 2026L
  )

  expect_named(
    parsed,
    c(
      "sport", "country", "sex", "season", "match_date",
      "home_team", "away_team", "home_score", "away_score",
      "division", "round", "kickoff_time"
    )
  )
  expect_s3_class(parsed$match_date, "Date")
  expect_type(parsed$season, "integer")
  # The 2026 fixture captured in April contains only scheduled (unplayed)
  # matches — the season starts in May. Parser should include them with
  # NA scores.
  expect_gt(nrow(parsed), 5)
  # Football: sensible scores 0..12 typically
  expect_true(all(is.na(parsed$home_score) | parsed$home_score >= 0))
  expect_true(all(is.na(parsed$away_score) | parsed$away_score >= 0))
  expect_true(all(parsed$division == "BD"))
  expect_true(all(parsed$season == 2026L))
  # Dates must parse for every surviving row (including Icelandic month
  # names like "maí" / "júní" that appear in the fixture).
  expect_true(all(!is.na(parsed$match_date)))
  # kickoff_time is captured from the same date column (KSÍ posts "HH:MM"
  # after a double space); at least some fixture rows carry a time, and any
  # value present is a well-formed HH:MM string.
  expect_true(any(!is.na(parsed$kickoff_time)))
  expect_true(all(
    is.na(parsed$kickoff_time) |
      grepl("^[0-9]{1,2}:[0-9]{2}$", parsed$kickoff_time)
  ))
})

test_that("parse_ksi_date handles Icelandic month names", {
  # Confirms the month-name map covers at least the summer months which
  # dominate the Icelandic football season.
  dates <- c(
    "Sun 26. apr\u00edl  18:00",
    "Lau 2. ma\u00ed  16:00",
    "Lau 9. j\u00fan\u00ed  14:00",
    "Fim 4. j\u00fal\u00ed  18:00",
    "Mi\u00f0 4. \u00e1g\u00fast  19:15",
    "Lau 12. september  14:00"
  )
  Encoding(dates) <- "UTF-8"
  out <- parse_ksi_date(dates, 2026L)
  expect_s3_class(out, "Date")
  expect_true(all(!is.na(out)))
  expect_equal(lubridate::month(out), c(4L, 5L, 6L, 7L, 8L, 9L))
})

test_that("parse_ksi_time extracts HH:MM and returns NA when absent", {
  # KSI renders the kick-off time after a double space in the date column;
  # parse_ksi_date strips it, parse_ksi_time recovers it.
  raw <- c(
    "Sun 26. apr\u00edl  18:00",
    "Lau 2. ma\u00ed  16:00",
    "Mi\u00f0 4. \u00e1g\u00fast  19:15",
    "Sun 26. apr\u00edl",
    ""
  )
  Encoding(raw) <- "UTF-8"
  expect_equal(
    parse_ksi_time(raw),
    c("18:00", "16:00", "19:15", NA, NA)
  )
})

test_that("parse_ksi_results_page with played_only drops unplayed", {
  skip_if_not(
    file.exists(fixture("male_div1_2026.html")),
    "no ksi fixture captured"
  )

  html <- rvest::read_html(
    fixture("male_div1_2026.html"),
    encoding = "UTF-8"
  )
  parsed <- parse_ksi_results_page(
    html,
    sport = "football", country = "iceland", sex = "male",
    division = "BD", season = 2026L,
    played_only = TRUE
  )
  # 2026 fixture captured pre-season: no played matches.
  # All surviving rows must have both scores.
  if (nrow(parsed) > 0L) {
    expect_true(all(!is.na(parsed$home_score)))
    expect_true(all(!is.na(parsed$away_score)))
  }
})

test_that("ksi_football registers itself as a source module", {
  src <- get_ingest_source("ksi_football")
  expect_true(is.function(src$fetch_results))
  expect_true(is.function(src$fetch_schedule))
})

test_that("ksi_url includes toggle and page query params", {
  u <- ksi_url(190366L, page = 3L, toggle = "results")
  expect_match(u, "id=190366")
  expect_match(u, "toggle=results")
  expect_match(u, "page=3")
})

test_that("extract_ksi_page_count reads 'X af Y' label", {
  skip_if_not(
    file.exists(fixture("male_div1_2025_p1.html")),
    "no ksi historical fixture captured"
  )
  html <- rvest::read_html(
    fixture("male_div1_2025_p1.html"),
    encoding = "UTF-8"
  )
  n <- extract_ksi_page_count(html)
  expect_type(n, "integer")
  expect_gt(n, 1L)
})

test_that("parse_ksi_results_page extracts played matches from historical page", {
  skip_if_not(
    file.exists(fixture("male_div1_2025_p1.html")),
    "no ksi historical fixture captured"
  )
  html <- rvest::read_html(
    fixture("male_div1_2025_p1.html"),
    encoding = "UTF-8"
  )
  parsed <- parse_ksi_results_page(
    html,
    sport = "football", country = "iceland", sex = "male",
    division = "BD", season = 2025L,
    played_only = TRUE
  )
  expect_gt(nrow(parsed), 5L)
  expect_true(all(!is.na(parsed$home_score)))
  expect_true(all(!is.na(parsed$away_score)))
  # Historical page should be tagged with the requested season, not the
  # current year — dates come back as 2025 because season = 2025L.
  expect_true(all(lubridate::year(parsed$match_date) == 2025L))
})

test_that("KSI_IDS covers men's 2021-2026 top flight", {
  top_flight <- KSI_IDS$male$div1
  expect_true(all(c("2021", "2022", "2023", "2024", "2025", "2026") %in%
    names(top_flight)))
})

test_that("KSI_IDS exposes men's + women's 2026 Mj\u00f3lkurbikar cup IDs", {
  expect_equal(KSI_IDS$male$cup[["2026"]], 7059683L)
  expect_equal(KSI_IDS$female$cup[["2026"]], 7058831L)
})

test_that("KSI_IDS covers women's 2023-2026 top-flight split playoffs", {
  for (slug in c("div1_upper_playoffs", "div1_lower_playoffs")) {
    expect_true(all(c("2023", "2024", "2025", "2026") %in%
      names(KSI_IDS$female[[slug]])))
  }
  expect_equal(KSI_IDS$female$div1_upper_playoffs[["2025"]], 190360L)
  expect_equal(KSI_IDS$female$div1_lower_playoffs[["2025"]], 190355L)
})

test_that("parse_ksi_results_page drops bracket-header placeholder rows", {
  # Synthetic HTML mimicking a KSI cup page with one real match (Stjarnan vs
  # KR) and one bracket-header placeholder (home = round name, away = ".").
  # The filter at parse time must drop the placeholder.
  html_str <- paste0(
    "<html><body>",
    '<div><div class="flex flex-col"><div>Mi\u00f0 13. ma\u00ed  18:00</div></div>',
    "<div><a>Stjarnan</a>",
    '<span class="body-4 whitespace-nowrap">-</span>',
    "<a>KR</a></div></div>",
    '<div><div class="flex flex-col"><div>F\u00f6s 15. ma\u00ed  19:00</div></div>',
    "<div><a>16 Li\u00f0a \u00darslit</a>",
    '<span class="body-4 whitespace-nowrap">-</span>',
    "<a>.</a></div></div>",
    "</body></html>"
  )
  Encoding(html_str) <- "UTF-8"
  html <- rvest::read_html(html_str, encoding = "UTF-8")
  parsed <- parse_ksi_results_page(
    html,
    sport = "football", country = "iceland", sex = "male",
    division = "CUP", season = 2026L
  )
  expect_equal(nrow(parsed), 1L)
  expect_equal(parsed$home_team, "Stjarnan")
  expect_equal(parsed$away_team, "KR")
})
