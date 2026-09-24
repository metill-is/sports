# Health on the betting ladder (spec 2026-09-23 WS7).

.hl_sched <- function(match_date, sex = "male", division = "OD",
                      home = "Valur", away = "FH") {
  tibble::tibble(
    sport = "handball", country = "iceland", sex = sex, season = 2027L,
    match_date = as.Date(match_date), home_team = home, away_team = away,
    division = division, round = 1L, kickoff_time = NA_character_
  )
}

.hl_league <- function(mode,
                       comps = list(list(id = "1269", name = "x", sex = "male", division = "OD"))) {
  list(handball_iceland = list(
    sport = "handball", country = "iceland", sexes = list("male", "female"),
    lengjan = list(competitions = comps), betting = list(mode = mode)
  ))
}

.hl_now <- as.POSIXct("2026-10-01 12:00", tz = "UTC")

test_that("odds_freshness caps a stall at WARN below betting.mode manual", {
  root <- withr::local_tempdir()
  write_table(.hl_sched("2026-10-01"), "schedules", root = root) # fixture today, no odds
  paper <- check_odds_freshness(.hl_league("paper"), root, .hl_now, health_thresholds())
  expect_equal(paper$status, "WARN")
  expect_match(paper$value, "capped at WARN")
  # Positive control: identical data with money at stake still FAILs.
  manual <- check_odds_freshness(.hl_league("manual"), root, .hl_now, health_thresholds())
  expect_equal(manual$status, "FAIL")
})

test_that("odds_freshness expects odds only in configured (sex, division) cells", {
  root <- withr::local_tempdir()
  # Today's only fixtures are women's OD and men's G66: no configured competition.
  write_table(dplyr::bind_rows(
    .hl_sched("2026-10-01", sex = "female"),
    .hl_sched("2026-10-01", division = "G66", home = "Hordur", away = "Fjolnir")
  ), "schedules", root = root)
  res <- check_odds_freshness(.hl_league("manual"), root, .hl_now, health_thresholds())
  expect_equal(res$status, "OK")
  expect_match(res$value, "no configured Lengjan competition")
})

test_that("odds_freshness is PAUSED for a league below manual with no competitions", {
  root <- withr::local_tempdir()
  write_table(.hl_sched("2026-10-01"), "schedules", root = root)
  res <- check_odds_freshness(.hl_league("scrape", comps = list()), root, .hl_now, health_thresholds())
  expect_equal(res$status, "PAUSED")
  expect_match(res$value, "no Lengjan competitions wired")
})

test_that("odds_freshness keeps the 'betting disabled' wording at mode off", {
  root <- withr::local_tempdir()
  write_table(.hl_sched("2026-10-01"), "schedules", root = root)
  res <- check_odds_freshness(.hl_league("off"), root, .hl_now, health_thresholds())
  expect_equal(res$status, "PAUSED")
  expect_match(res$value, "betting disabled")
})

test_that("capture_rate ignores recommendations from leagues below manual", {
  root <- withr::local_tempdir()
  write_table(tibble::tibble(
    run_id = as.POSIXct("2026-05-20", tz = "UTC"),
    sport = "handball", country = "iceland", sex = "male",
    match_date = as.Date("2026-05-22"),
    home_team = paste0("H", 1:25), away_team = paste0("A", 1:25),
    market = "moneyline", outcome = "home", line = NA_real_,
    p = 0.55, odds = 2.0, ev = 0.1, kelly = 0.02, bet_amount = 250
  ), "recommendations", root = root)
  at <- as.POSIXct("2026-05-30", tz = "UTC")
  paper <- check_capture_rate(root, at, health_thresholds(), leagues = .hl_league("paper"))
  expect_equal(paper$status, "OK")
  expect_match(paper$value, "no settled-window recs")
  # Positive control: the same 25 unplaced recs FAIL once money is at stake.
  manual <- check_capture_rate(root, at, health_thresholds(), leagues = .hl_league("manual"))
  expect_equal(manual$status, "FAIL")
})
