# WS1 -- betting interlock (spec section 3, decision D2).
# One predicate, four enforcement points: odds ingest, decide, the placer's
# recommendation loader, and the placer's pre-flight validator.

test_that("betting_enabled() defaults to TRUE when the key is absent", {
  expect_true(betting_enabled(list(betting = list(kelly_frac = 0.1))))
})

test_that("betting_enabled() defaults to TRUE when there is no betting block", {
  expect_true(betting_enabled(list(sport = "football", country = "iceland")))
})

test_that("betting_enabled() is FALSE only for an explicit FALSE", {
  expect_false(betting_enabled(list(betting = list(enabled = FALSE))))
  expect_true(betting_enabled(list(betting = list(enabled = TRUE))))
})

test_that("betting_enabled() treats a NULL betting slice as enabled", {
  # decide_one() forwards `betting` verbatim; a NULL slice must not silently
  # disarm a league that meant to bet.
  expect_true(betting_enabled(list(betting = NULL)))
})

# --- the shipped config is disarmed (D2) --------------------------------------

test_that("basketball and handball ship betting-disabled, football does not", {
  lg <- load_leagues()
  expect_false(betting_enabled(lg$basketball_iceland))
  expect_false(betting_enabled(lg$handball_iceland))
  expect_true(betting_enabled(lg$football_iceland))
})

test_that("the disarmed leagues have no Lengjan competitions to scrape", {
  lg <- load_leagues()
  expect_length(lg$basketball_iceland$lengjan$competitions, 0L)
  expect_length(lg$handball_iceland$lengjan$competitions, 0L)
  # Football must still have its full slate.
  expect_gt(length(lg$football_iceland$lengjan$competitions), 0L)
})

test_that("disarming did not clobber the team_names maps", {
  # ID-5: the competitions edit abuts `team_names:`. An off-by-one replacement
  # yields two `team_names:` keys under `lengjan:`; yaml::yaml.load() keeps the
  # last, silently discarding the maps. Guard the content, not the line numbers.
  lg <- load_leagues()
  expect_gt(length(lg$basketball_iceland$lengjan$team_names$male), 0L)
  expect_gt(length(lg$basketball_iceland$lengjan$team_names$female), 0L)
  expect_gt(length(lg$handball_iceland$lengjan$team_names$male), 0L)
})

# --- the decide-layer guard ---------------------------------------------------

interlock_decide_setup <- function(root, match_date = Sys.Date() + 1L) {
  set.seed(11)
  write_table(tibble::tibble(
    sport = "handball", country = "iceland", sex = "male",
    fit_date = Sys.Date(), match_date = match_date,
    home_team = "Valur", away_team = "FH",
    draw_id = 1:1000L,
    home_goals = rpois(1000, 30), away_goals = rpois(1000, 26)
  ), "beliefs_latest", root = root)

  write_table(tibble::tibble(
    sport = "handball", country = "iceland",
    scraped_at = Sys.time(), match_date = match_date,
    home_team = "Valur", away_team = "FH",
    market = "moneyline", outcome = c("home", "away"),
    line = NA_real_, odds = c(2.60, 2.20)
  ), "odds", root = root)
}

interlock_league <- function(enabled = NULL) {
  betting <- list(
    kelly_frac = 0.10, ev_threshold = 0.0,
    markets = list(moneyline = TRUE, spread = FALSE, total = FALSE),
    scoring = list(has_ties = TRUE, tie_threshold = 0.5),
    min_bet = 1L, max_age_hours = 999999L
  )
  if (!is.null(enabled)) betting$enabled <- enabled
  list(
    sport = "handball", country = "iceland", sexes = "male",
    active = TRUE, stan_model = "x.stan", betting = betting
  )
}

interlock_bankroll <- function() {
  list(
    initial_pool = 23610, current_pool = 23610,
    daily_budget_frac = 0.5, daily_budget_min_isk = 1000
  )
}

interlock_decide <- function(root, enabled) {
  suppressMessages(decide_league(
    league = interlock_league(enabled), sex = "male",
    root = root, bankroll = interlock_bankroll(), return_candidates = TRUE
  ))
}

test_that("an ENABLED league produces candidates from this fixture", {
  # Positive control. Without it, the disabled-league assertion below could
  # pass because the fixture yields nothing, not because the guard fired --
  # a test that proves the guard works when it does not.
  root <- withr::local_tempdir()
  interlock_decide_setup(root)
  expect_gt(nrow(interlock_decide(root, enabled = NULL)), 0L)
})

test_that("decide_league returns nothing for a betting-disabled league", {
  root <- withr::local_tempdir()
  interlock_decide_setup(root)

  expect_equal(nrow(interlock_decide(root, enabled = FALSE)), 0L)

  # decide_write_empty() is a no-op (write_table() returns early on a zero-row
  # frame), so no partition is created and read_table() sees nothing.
  expect_equal(nrow(read_table("candidates", root = root)), 0L)
  expect_equal(nrow(read_table("recommendations", root = root)), 0L)
})

test_that("the decide guard fires before any odds are read", {
  # A disabled league with NO odds store at all must still exit cleanly rather
  # than erroring -- proving the guard precedes the odds read.
  root <- withr::local_tempdir()
  set.seed(11)
  write_table(tibble::tibble(
    sport = "handball", country = "iceland", sex = "male",
    fit_date = Sys.Date(), match_date = Sys.Date() + 1L,
    home_team = "Valur", away_team = "FH", draw_id = 1:100L,
    home_goals = rpois(100, 30), away_goals = rpois(100, 26)
  ), "beliefs_latest", root = root)

  expect_equal(nrow(interlock_decide(root, enabled = FALSE)), 0L)
})

# --- the odds-ingest guard ----------------------------------------------------

test_that("ingest_one_lengjan scrapes nothing for a betting-disabled league", {
  testthat::local_mocked_bindings(
    .is_league_active = function(active_path, key) TRUE,
    ingest_lengjan_odds = function(...) stop("scraper must not be reached")
  )
  res <- suppressMessages(ingest_one_lengjan(
    list(sport = "handball", country = "iceland"),
    list(competitions = list(list(id = "1269", name = "x", sex = "male"))),
    "handball_iceland", "active.json",
    betting = list(enabled = FALSE)
  ))
  expect_identical(res, 0L)
})

test_that("ingest_one_lengjan is unaffected when betting is absent", {
  # Locks the existing four-arg contract: `betting` is trailing and defaulted.
  testthat::local_mocked_bindings(
    .is_league_active = function(active_path, key) TRUE,
    ingest_lengjan_odds = function(...) 7L
  )
  res <- suppressMessages(ingest_one_lengjan(
    list(sport = "football", country = "iceland"), list(),
    "football_iceland", "active.json"
  ))
  expect_identical(res, 7L)
})

test_that("the odds guard fires before the activation gate", {
  # A disabled league must not even consult active_competitions.json -- a
  # league we will never bet should not launch a browser on a fixture day.
  testthat::local_mocked_bindings(
    .is_league_active = function(active_path, key) stop("gate must not be reached"),
    ingest_lengjan_odds = function(...) stop("scraper must not be reached")
  )
  expect_identical(
    suppressMessages(ingest_one_lengjan(
      list(sport = "handball", country = "iceland"), list(),
      "handball_iceland", "active.json", betting = list(enabled = FALSE)
    )),
    0L
  )
})
