test_that("is_positive_ev returns TRUE iff EV is strictly positive", {
  # p * (odds - 1) - (1 - p) > 0  <=>  p * odds - 1 > 0
  expect_true(is_positive_ev(p = 0.55, odds = 2.0)) # 0.55*1 - 0.45 = 0.10 > 0
  expect_false(is_positive_ev(p = 0.50, odds = 1.50)) # 0.50*0.5 - 0.50 = -0.25 < 0
  expect_false(is_positive_ev(p = 0.50, odds = 2.0)) # 0.50*1 - 0.50 = 0 (not > 0)
})

test_that("recalculate_kelly_amount returns original amount when odds unchanged", {
  out <- recalculate_kelly_amount(
    p = 0.55, actual_odds = 2.0, original_odds = 2.0, original_amount = 1000
  )
  expect_equal(out, 1000, tolerance = 1e-6)
})

test_that("recalculate_kelly_amount returns smaller stake when odds drop", {
  out <- recalculate_kelly_amount(
    p = 0.55, actual_odds = 1.85, original_odds = 2.00, original_amount = 1000
  )
  expect_lt(out, 1000)
  expect_gte(out, 0)
})

test_that("recalculate_kelly_amount returns 0 when new EV is negative", {
  # p=0.55, odds=1.50 → kelly fraction negative → clamped to 0
  out <- recalculate_kelly_amount(
    p = 0.55, actual_odds = 1.50, original_odds = 2.00, original_amount = 1000
  )
  expect_equal(out, 0)
})

test_that("handicap_to_lengjan_line: positive handicap gives '<h>-0' format", {
  # Legacy: if (handicap >= 0) paste0(handicap, "-0")
  expect_equal(handicap_to_lengjan_line(1.5), "1.5-0")
  expect_equal(handicap_to_lengjan_line(2), "2-0")
})

test_that("handicap_to_lengjan_line: negative handicap gives '0-<abs(h)>' format", {
  # Legacy: else paste0("0-", abs(handicap))
  expect_equal(handicap_to_lengjan_line(-1.5), "0-1.5")
  expect_equal(handicap_to_lengjan_line(-2), "0-2")
})

test_that("handicap_to_lengjan_line: zero handicap gives '0-0'", {
  expect_equal(handicap_to_lengjan_line(0), "0-0")
})

test_that("placer-place.R reads bet$line, not legacy bet$info (schema guard)", {
  src <- readLines(testthat::test_path("..", "..", "R", "placer-place.R"))
  expect_false(
    any(grepl("bet\\$info", src, fixed = FALSE)),
    info = paste(
      "Recommendations Parquet schema uses 'line' (numeric DOUBLE).",
      "Legacy CSV used 'info' (character) — that column does not exist on",
      "recommendation rows, and reading it returns NULL, which mangles the",
      "JS expression sent to chromote with",
      "'BINDINGS: string value expected at position 19'."
    )
  )
})

# ── parse_actual_odds_from_dom (Plan 5 Task 6 seam) ───────────────────────────

test_that("parse_actual_odds_from_dom: aria-label with stuðull (market button primary path)", {
  html <- '<button aria-label="1, stuðull: 2.28">2.28</button>'
  expect_equal(parse_actual_odds_from_dom(html), 2.28)
})

test_that("parse_actual_odds_from_dom: aria-label takes precedence over text content", {
  html <- '<button aria-label="2, stuðull: 1.50">99.99</button>'
  expect_equal(parse_actual_odds_from_dom(html), 1.50)
})

test_that("parse_actual_odds_from_dom: aria-label with home-team variant", {
  html <- '<button aria-label="Heimavinning, stuðull: 1.85">1.85</button>'
  expect_equal(parse_actual_odds_from_dom(html), 1.85)
})

test_that("parse_actual_odds_from_dom: text content with [1X2] prefix (market button fallback)", {
  expect_equal(parse_actual_odds_from_dom("<button>11.69</button>"), 1.69)
  expect_equal(parse_actual_odds_from_dom("<button>X7.80</button>"), 7.80)
  expect_equal(parse_actual_odds_from_dom("<button>22.39</button>"), 2.39)
})

test_that("parse_actual_odds_from_dom: bare numeric text content (table button)", {
  expect_equal(parse_actual_odds_from_dom("<button>1.65</button>"), 1.65)
  expect_equal(parse_actual_odds_from_dom("<button><p>1.87</p></button>"), 1.87)
})

test_that("parse_actual_odds_from_dom: returns NA on no parseable odds", {
  expect_true(is.na(parse_actual_odds_from_dom("<button>no odds here</button>")))
  expect_true(is.na(parse_actual_odds_from_dom("<button></button>")))
  expect_true(is.na(parse_actual_odds_from_dom("")))
})

test_that("parse_actual_odds_from_dom: defensive against NA/NULL/non-character input", {
  expect_true(is.na(parse_actual_odds_from_dom(NA_character_)))
  expect_true(is.na(parse_actual_odds_from_dom(NULL)))
  expect_true(is.na(parse_actual_odds_from_dom(character(0))))
})

test_that("parse_actual_odds_from_dom: nested elements parse to correct first decimal", {
  # Real-world Lengjan structure: <button> wraps <p class="..."> with the odds
  html <- '<button><div><p class="h7cub5">2.05</p></div></button>'
  expect_equal(parse_actual_odds_from_dom(html), 2.05)
})

test_that("parse_actual_odds_from_dom: aria-label without stuðull falls through to text", {
  html <- '<button aria-label="some other label">1.95</button>'
  expect_equal(parse_actual_odds_from_dom(html), 1.95)
})
