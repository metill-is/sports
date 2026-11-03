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

test_that("prepare_odds de-duplicates two Lengjan spellings of one fixture on canonical names", {
  # Audit 2026-11 finding A: a team_names list value (cf. the 2026-07-17
  # "Víkingur Rvk" -> "Víkingur Reykjavík" rename) maps two Lengjan spellings
  # to one canonical. De-duplicating on the raw strings kept both scrapes of
  # the fixture, which then collapsed into identical (market, outcome, line)
  # rows whose probabilities summed to ~2 in kelly_joint().
  root <- withr::local_tempdir()
  now <- as.POSIXct("2100-01-01 13:00:00", tz = "UTC")
  scrape <- function(away, at, odds) {
    tibble::tibble(
      sport = "football", country = "iceland", scraped_at = at,
      match_date = as.Date("2100-01-02"), home_team = "Thor Ak.",
      away_team = away, market = "moneyline",
      outcome = c("home", "draw", "away"), line = NA_real_, odds = odds
    )
  }
  write_table(dplyr::bind_rows(
    scrape("Vikingur Rvk", now - 7 * 3600, c(3.50, 3.40, 2.00)),
    scrape("Vikingur Reykjavik", now - 3600, c(3.60, 3.50, 1.90))
  ), "odds", root = root)
  league <- list(
    sport = "football", country = "iceland",
    lengjan = list(team_names = list(male = list(
      Thor = "Thor Ak.",
      "Vikingur R." = list("Vikingur Reykjavik", "Vikingur Rvk")
    )))
  )

  out <- prepare_odds(league, "male",
    end_date = as.Date("2100-01-01"), max_age_hours = 48,
    now = now, root = root
  )

  expect_equal(nrow(out), 3L)
  expect_equal(unique(out$away_team), "Vikingur R.")
  out <- out[order(out$outcome), ]
  expect_equal(out$outcome, c("away", "draw", "home"))
  expect_equal(out$odds, c(1.90, 3.50, 3.60))
})

test_that("prepare_odds prices each match from its latest scrape only", {
  # Audit 2026-11 finding B: the scrapers drop withdrawn lines and suspended
  # selections instead of writing them, so keeping the latest row per
  # (market, outcome, line) across the whole max_age_hours window left them
  # priceable at their last price. A match missing from the newest scrape run
  # keeps its own latest scrape.
  root <- withr::local_tempdir()
  now <- as.POSIXct("2100-01-01 13:00:00", tz = "UTC")
  rows <- function(home, away, at, market, outcome, line, odds) {
    tibble::tibble(
      sport = "football", country = "iceland", scraped_at = at,
      match_date = as.Date("2100-01-02"), home_team = home, away_team = away,
      market = market, outcome = outcome, line = line, odds = odds
    )
  }
  old <- now - 7 * 3600
  new <- now - 3600
  write_table(dplyr::bind_rows(
    rows("Thor", "Vikingur", old, "moneyline", c("home", "draw", "away"),
      NA_real_, c(2.10, 3.40, 3.00)),
    rows("Thor", "Vikingur", old, "spread", c("home", "draw", "away"),
      -1, c(4.00, 3.80, 1.60)),
    rows("Thor", "Vikingur", new, "moneyline", c("home", "away"),
      NA_real_, c(2.20, 2.90)),
    rows("Fram", "KR", old, "moneyline", c("home", "draw", "away"),
      NA_real_, c(2.50, 3.30, 2.60))
  ), "odds", root = root)
  tn <- list(Thor = "Thor", Vikingur = "Vikingur", Fram = "Fram", KR = "KR")
  league <- list(
    sport = "football", country = "iceland",
    lengjan = list(team_names = list(male = tn))
  )

  out <- prepare_odds(league, "male",
    end_date = as.Date("2100-01-01"), max_age_hours = 48,
    now = now, root = root
  )

  vik <- out[out$home_team == "Thor", ]
  vik <- vik[order(vik$outcome), ]
  expect_equal(vik$market, c("moneyline", "moneyline"))
  expect_equal(vik$outcome, c("away", "home"))
  expect_equal(vik$odds, c(2.90, 2.20))
  expect_equal(nrow(out[out$home_team == "Fram", ]), 3L)
})

test_that("prepare_odds collapses two Lengjan spellings within one scrape", {
  # The 2026-07-17 scrape carried one fixture under both Víkingur spellings
  # with the same scraped_at, so the latest-scrape filter keeps both and only
  # the canonical-key de-duplication collapses them.
  root <- withr::local_tempdir()
  now <- as.POSIXct("2100-01-01 13:00:00", tz = "UTC")
  write_table(tibble::tibble(
    sport = "football", country = "iceland", scraped_at = now - 3600,
    match_date = as.Date("2100-01-02"), home_team = "Thor Ak.",
    away_team = rep(c("Vikingur Rvk", "Vikingur Reykjavik"), each = 3),
    market = "moneyline", outcome = rep(c("home", "draw", "away"), 2),
    line = NA_real_, odds = c(3.50, 3.40, 2.00, 3.60, 3.50, 1.90)
  ), "odds", root = root)
  league <- list(
    sport = "football", country = "iceland",
    lengjan = list(team_names = list(male = list(
      Thor = "Thor Ak.",
      "Vikingur R." = list("Vikingur Reykjavik", "Vikingur Rvk")
    )))
  )

  out <- prepare_odds(league, "male",
    end_date = as.Date("2100-01-01"), max_age_hours = 48,
    now = now, root = root
  )

  expect_equal(nrow(out), 3L)
  expect_setequal(out$outcome, c("home", "draw", "away"))
})

test_that("prepare_odds prices each market from the primary rendering's listing", {
  # Two spellings of one fixture in one scrape: the canonical dedup kept the
  # first row per outcome in file order, so home and away could come from
  # different listings. Each (market, line) now comes from one listing, the
  # one earliest in tn_renderings() order; a selection missing from it (the
  # spread draw) is not borrowed from the other listing. The second order
  # makes the primary sort after the variant byte-wise, so the raw-name
  # tiebreak alone would pick the wrong listing.
  now <- as.POSIXct("2100-01-01 13:00:00", tz = "UTC")
  for (pair in list(
    c("Vikingur Reykjavik", "Vikingur Rvk"),
    c("Vikingur Rvk", "Vikingur Reykjavik")
  )) {
    root <- withr::local_tempdir()
    primary <- pair[1]
    variant <- pair[2]
    write_table(tibble::tibble(
      sport = "football", country = "iceland", scraped_at = now - 3600,
      match_date = as.Date("2100-01-02"), home_team = "Thor Ak.",
      away_team = c(
        variant, primary, primary, variant, primary, variant,
        variant, variant, variant, primary, primary
      ),
      market = rep(c("moneyline", "spread"), c(6, 5)),
      outcome = c(
        "home", "home", "draw", "draw", "away", "away",
        "home", "draw", "away", "away", "home"
      ),
      line = rep(c(NA_real_, -1), c(6, 5)),
      odds = c(
        3.50, 3.60, 3.30, 3.40, 1.90, 2.00,
        5.00, 4.20, 1.50, 1.45, 5.50
      )
    ), "odds", root = root)
    league <- list(
      sport = "football", country = "iceland",
      lengjan = list(team_names = list(male = list(
        Thor = "Thor Ak.",
        "Vikingur R." = list(primary, variant)
      )))
    )

    out <- prepare_odds(league, "male",
      end_date = as.Date("2100-01-01"), max_age_hours = 48,
      now = now, root = root
    )

    ml <- out[out$market == "moneyline", ]
    expect_equal(ml$odds[match(c("home", "draw", "away"), ml$outcome)],
      c(3.60, 3.30, 1.90))
    sp <- out[out$market == "spread", ]
    expect_setequal(sp$outcome, c("home", "away"))
    expect_equal(sp$odds[match(c("home", "away"), sp$outcome)], c(5.50, 1.45))
  }
})

test_that("prepare_odds keeps a women's fixture apart from the men's between the same clubs", {
  # Football's DOM-scraped rows carry no `sex`, so the female cell also sees
  # the men's listings. "FH" and "Valur" are no female rendering and pass
  # through unchanged, onto the canonical key of "FH kv" v "Valur kv"; the
  # canonical dedup then picked rows by file order. Before it, the duplicate
  # rows aborted kelly_joint().
  now <- as.POSIXct("2100-01-01 13:00:00", tz = "UTC")
  listing <- function(home, away, at, odds) {
    tibble::tibble(
      sport = "football", country = "iceland", scraped_at = at,
      match_date = as.Date("2100-01-02"), home_team = home, away_team = away,
      market = "moneyline", outcome = c("home", "draw", "away"),
      line = NA_real_, odds = odds
    )
  }
  league <- list(
    sport = "football", country = "iceland",
    lengjan = list(team_names = list(
      male = list(FH = "FH", Valur = "Valur"),
      female = list(FH = "FH kv", Valur = "Valur kv")
    ))
  )
  mens <- c(1.70, 3.80, 4.50)
  womens <- c(2.40, 3.60, 2.70)
  odds_of <- function(out, home) {
    out <- out[out$home_team == home, ]
    out$odds[match(c("home", "draw", "away"), out$outcome)]
  }
  # Both in one scrape; then the women's game delisted (earlier kickoff) so
  # only the men's is in the newest scrape.
  for (mens_lag in c(3600, 1800)) {
    root <- withr::local_tempdir()
    write_table(dplyr::bind_rows(
      listing("FH", "Valur", now - mens_lag, mens),
      listing("FH kv", "Valur kv", now - 3600, womens)
    ), "odds", root = root)
    get <- function(sex) {
      prepare_odds(league, sex,
        end_date = as.Date("2100-01-01"), max_age_hours = 48,
        now = now, root = root
      )
    }

    expect_warning(f <- get("female"), "2100-01-02 FH v Valur")
    expect_equal(nrow(f), 3L)
    expect_equal(odds_of(f, "FH"), womens)
    expect_no_warning(m <- get("male"))
    expect_equal(odds_of(m, "FH"), mens)
    expect_equal(odds_of(m, "FH kv"), womens)
  }
})

test_that("prepare_odds drops odds older than max_age_hours", {
  root <- setup_odds_root()
  league <- list(sport = "basketball", country = "iceland")

  # Fixture scrapes at 06:00 and 12:00 UTC.
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
