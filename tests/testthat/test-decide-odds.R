setup_odds_root <- function() {
  tmp <- withr::local_tempdir(.local_envir = parent.frame())
  odds <- arrow::read_parquet(testthat::test_path(
    "fixtures", "decide",
    "mini_odds.parquet"
  ))
  write_table(odds, "odds", root = tmp)
  tmp
}

# Reference "now" set just after the last fixture scrape (2026-04-25 12:00 UTC)
fixture_now <- as.POSIXct("2026-04-25 13:00:00", tz = "UTC")

test_that("prepare_odds returns the latest odds per (match, market, outcome, line)", {
  root <- setup_odds_root()
  league <- list(sport = "basketball", country = "iceland")

  # now is 1h after last scrape; large max_age_hours keeps all rows
  out <- prepare_odds(league,
    sex = "male",
    end_date = as.Date("2026-04-25"),
    max_age_hours = 24L * 365 * 100L, # effectively no age filter
    now = fixture_now,
    root = root
  )

  expect_named(out, c(
    "match_date", "home_team", "away_team",
    "market", "outcome", "line", "odds", "scraped_at"
  ),
  ignore.order = TRUE
  )
  expect_equal(nrow(out), 3L)
})

test_that("prepare_odds drops odds older than max_age_hours", {
  root <- setup_odds_root()
  league <- list(sport = "basketball", country = "iceland")

  # Fixture scrapes at 08:00 and 12:00 UTC.
  # now = 14:00, max_age_hours = 1 → cutoff = 13:00 → all scrapes are older.
  out <- prepare_odds(league,
    sex = "male",
    end_date = as.Date("2026-04-25"),
    max_age_hours = 1L,
    now = as.POSIXct("2026-04-25 14:00:00", tz = "UTC"),
    root = root
  )
  expect_equal(nrow(out), 0L)
})

test_that("prepare_odds drops matches before end_date", {
  root <- setup_odds_root()
  league <- list(sport = "basketball", country = "iceland")

  out <- prepare_odds(league,
    sex = "male",
    end_date = as.Date("2026-04-27"), # after fixture matches (2026-04-26)
    max_age_hours = 24L * 365 * 100L,
    now = fixture_now,
    root = root
  )
  expect_equal(nrow(out), 0L)
})

test_that("parse_handicap converts Lengjan score-style strings to signed numeric", {
  expect_equal(parse_handicap(c("0-1", "1-0", "0-2")), c(-1, 1, -2))
  expect_warning(parse_handicap("not-a-handicap"), "Could not parse")
})

test_that("prepare_odds returns empty tibble when no rows in (sport, country)", {
  tmp <- withr::local_tempdir()
  league <- list(sport = "football", country = "iceland")
  out <- prepare_odds(league, sex = "male", root = tmp)
  expect_equal(nrow(out), 0L)
  # Should still have all 8 columns
  expect_named(out, c(
    "match_date", "home_team", "away_team",
    "market", "outcome", "line", "odds", "scraped_at"
  ),
  ignore.order = TRUE
  )
})
