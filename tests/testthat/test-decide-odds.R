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
  # Fixture has 4 rows: moneyline/home appears twice (06:00 + 12:00).
  # Dedup keeps only the latest, so 3 unique groups remain.
  expect_equal(nrow(out), 3L)

  # The older moneyline/home scrape (1.95 at 06:00) should be dropped;
  # the newer one (1.85 at 12:00) survives.
  ml_home <- out[out$market == "moneyline" & out$outcome == "home", ]
  expect_equal(nrow(ml_home), 1L)
  expect_equal(ml_home$odds, 1.85)
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

test_that("prepare_odds keeps only the requested sex when odds carry one", {
  # Spec 2026-09-23 Review Focus 4: the API stamps each row's sex from its
  # competition; a men's and a women's fixture between the same clubs on the
  # same day must never price each other. NA rows (DOM scraper, history) stay
  # sex-agnostic.
  root <- withr::local_tempdir()
  now <- as.POSIXct("2100-01-01 12:00:00", tz = "UTC")
  row <- function(sex, home, away, odds) {
    tibble::tibble(
      sport = "handball", country = "iceland", scraped_at = now - 3600,
      match_date = as.Date("2100-01-02"), home_team = home, away_team = away,
      market = "moneyline", outcome = "home", line = NA_real_, odds = odds,
      sex = sex
    )
  }
  write_table(dplyr::bind_rows(
    row("male", "Valur", "Haukar", 1.80),
    row("female", "Valur", "Haukar", 2.60),
    row(NA_character_, "FH", "HK", 3.10)
  ), "odds", root = root)
  tn <- list(Valur = "Valur", Haukar = "Haukar", FH = "FH", HK = "HK")
  league <- list(
    sport = "handball", country = "iceland",
    lengjan = list(team_names = list(male = tn, female = tn))
  )
  get <- function(sex) {
    prepare_odds(league, sex,
      end_date = as.Date("2100-01-01"), max_age_hours = 48,
      now = now, root = root
    )
  }
  m <- get("male")
  f <- get("female")
  expect_equal(m$odds[m$home_team == "Valur"], 1.80)
  expect_equal(f$odds[f$home_team == "Valur"], 2.60)
  expect_equal(m$odds[m$home_team == "FH"], 3.10) # NA sex passes through
  expect_equal(f$odds[f$home_team == "FH"], 3.10)
  expect_equal(nrow(m), 2L)
  expect_equal(nrow(f), 2L)
})
