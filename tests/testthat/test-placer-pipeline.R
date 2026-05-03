test_that("place_bets returns 0-row tibble when no recommendations match", {
  tmp <- withr::local_tempdir()
  out <- place_bets(target_date = as.Date("2050-01-01"), root = tmp)
  expect_equal(nrow(out), 0L)
})

test_that("place_bets dry-run with no bets skips chromote entirely", {
  testthat::local_mocked_bindings(
    chromote_login = function(...) stop("Should not have logged in"),
    .package = "sports"
  )
  tmp <- withr::local_tempdir()
  out <- place_bets(
    target_date = as.Date("2050-01-01"),
    root        = tmp,
    dry_run     = TRUE
  )
  expect_equal(nrow(out), 0L)
})

test_that("place_bets respects league filter", {
  tmp <- withr::local_tempdir()
  out <- place_bets(
    leagues     = "football_iceland",
    target_date = as.Date("2050-01-01"),
    root        = tmp
  )
  expect_equal(nrow(out), 0L)
})

test_that("place_bets returns a tibble with the documented status column", {
  tmp <- withr::local_tempdir()
  out <- place_bets(target_date = as.Date("2050-01-01"), root = tmp)
  expected_cols <- c(
    "sport", "country", "sex", "match_date",
    "home_team", "away_team", "market", "outcome",
    "odds", "bet_amount", "status"
  )
  expect_true(all(expected_cols %in% names(out)))
})

# ── resolve_match_ids_new return-shape contract (no_match_id disambiguation) ──

test_that("resolve_match_ids_new returns list(match_ids, n_total) shape", {
  league <- list(lengjan = list(competitions = list()))
  out <- resolve_match_ids_new(
    session = NULL, league = league, sport_id = 1L,
    pipeline_to_lengjan = character(0)
  )
  expect_named(out, c("match_ids", "n_total"), ignore.order = TRUE)
  expect_type(out$match_ids, "list")
  expect_type(out$n_total, "integer")
  expect_equal(out$n_total, 0L)
})

test_that("resolve_match_ids_new n_total reflects total matches across competitions", {
  league <- list(lengjan = list(competitions = list(
    list(id = "111", name = "Comp A", sex = "male"),
    list(id = "222", name = "Comp B", sex = "male")
  )))
  call_n <- 0L
  testthat::local_mocked_bindings(
    extract_matches = function(session, sport_id, competition_id, ...) {
      call_n <<- call_n + 1L
      if (call_n == 1L) {
        tibble::tibble(
          home = c("A", "B"), away = c("B", "C"),
          match_id = c("m1", "m2"),
          date = c("", ""),
          odds_home = c(1.5, 1.6),
          odds_draw = c(NA_real_, NA_real_),
          odds_away = c(2.5, 2.6)
        )
      } else {
        tibble::tibble(
          home = "X", away = "Y", match_id = "m3", date = "",
          odds_home = 1.7, odds_draw = NA_real_, odds_away = 2.7
        )
      }
    },
    .package = "sports"
  )
  out <- resolve_match_ids_new(
    session = NULL, league = league, sport_id = 1L,
    pipeline_to_lengjan = character(0)
  )
  expect_equal(out$n_total, 3L)
  expect_equal(out$match_ids[["A - B"]], "m1")
  expect_equal(out$match_ids[["X - Y"]], "m3")
})

test_that("resolve_match_ids_new n_total is 0 when all competitions return empty", {
  league <- list(lengjan = list(competitions = list(
    list(id = "111", name = "Comp A", sex = "male")
  )))
  testthat::local_mocked_bindings(
    extract_matches = function(...) {
      tibble::tibble(
        home = character(), away = character(), match_id = character(),
        date = character(), odds_home = numeric(),
        odds_draw = numeric(), odds_away = numeric()
      )
    },
    .package = "sports"
  )
  out <- resolve_match_ids_new(
    session = NULL, league = league, sport_id = 1L,
    pipeline_to_lengjan = character(0)
  )
  expect_equal(out$n_total, 0L)
  expect_length(out$match_ids, 0L)
})
