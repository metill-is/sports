mk_raw <- function() {
  tibble::tibble(
    date = as.Date(c("2026-06-26", "2026-06-26", "2026-06-25")),
    home_team = c("Algeria", "Jordan", "Spain"),
    away_team = c("Austria", "Argentina", "Brazil"),
    home_score = c(NA_integer_, NA_integer_, 1L),
    away_score = c(NA_integer_, NA_integer_, 2L),
    tournament = "FIFA World Cup",
    city = "x", country = "US", neutral = TRUE
  )
}

test_that("fills an NA-score row matched by key", {
  overlay <- tibble::tibble(
    date = as.Date("2026-06-26"), home_team = "Algeria",
    away_team = "Austria", home_score = 3L, away_score = 0L
  )
  out <- wc_apply_manual_results(mk_raw(), overlay)
  row <- out[out$home_team == "Algeria", ]
  expect_equal(row$home_score, 3L)
  expect_equal(row$away_score, 0L)
})

test_that("aborts on an overlay row that matches no fixture (typo)", {
  overlay <- tibble::tibble(
    date = as.Date("2026-06-26"), home_team = "Algerie", # typo
    away_team = "Austria", home_score = 3L, away_score = 0L
  )
  expect_error(wc_apply_manual_results(mk_raw(), overlay), "matches no martj42 fixture")
})

test_that("martj42 wins once it has a score; a differing overlay warns (drain)", {
  overlay <- tibble::tibble(
    date = as.Date("2026-06-25"), home_team = "Spain",
    away_team = "Brazil", home_score = 5L, away_score = 5L # differs from 1-2
  )
  expect_warning(out <- wc_apply_manual_results(mk_raw(), overlay), "already reports")
  row <- out[out$home_team == "Spain", ]
  expect_equal(row$home_score, 1L) # martj42 unchanged
  expect_equal(row$away_score, 2L)
})

test_that("no warning when overlay equals an already-scored martj42 row", {
  overlay <- tibble::tibble(
    date = as.Date("2026-06-25"), home_team = "Spain",
    away_team = "Brazil", home_score = 1L, away_score = 2L # same as martj42
  )
  expect_no_warning(out <- wc_apply_manual_results(mk_raw(), overlay))
  expect_equal(out$home_score[out$home_team == "Spain"], 1L)
})

test_that("empty overlay is a no-op", {
  empty <- tibble::tibble(
    date = as.Date(character()), home_team = character(),
    away_team = character(), home_score = integer(), away_score = integer()
  )
  expect_identical(wc_apply_manual_results(mk_raw(), empty), mk_raw())
})

test_that("an ambiguous (duplicate) key aborts", {
  raw <- dplyr::bind_rows(mk_raw(), mk_raw()[1, ]) # duplicate Algeria-Austria
  overlay <- tibble::tibble(
    date = as.Date("2026-06-26"), home_team = "Algeria",
    away_team = "Austria", home_score = 3L, away_score = 0L
  )
  expect_error(wc_apply_manual_results(raw, overlay), "ambiguous")
})
