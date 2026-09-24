# Betting ladder (spec 2026-09-23 WS1): off < scrape < paper < manual < auto.

test_that("betting_mode reads betting.mode when set", {
  for (m in c("off", "scrape", "paper", "manual", "auto")) {
    expect_equal(betting_mode(list(betting = list(mode = m))), m)
  }
})

test_that("betting_mode falls back to the legacy enabled flag", {
  expect_equal(betting_mode(list(betting = list(enabled = FALSE))), "off")
  expect_equal(betting_mode(list(betting = list(enabled = TRUE))), "auto")
  expect_equal(betting_mode(list(betting = list(kelly_frac = 0.1))), "auto")
  expect_equal(betting_mode(list(sport = "football")), "auto")
  expect_equal(betting_mode(list(betting = NULL)), "auto")
})

test_that("betting_mode rejects an unknown stage", {
  expect_error(betting_mode(list(betting = list(mode = "live"))), "betting.mode")
})

test_that("betting_mode reads a YAML-boolean FALSE mode as 'off'", {
  # YAML 1.1: an unquoted `mode: off` parses as FALSE.
  expect_equal(betting_mode(list(betting = list(mode = FALSE))), "off")
})

test_that("betting_mode_at_least orders the ladder", {
  modes <- c("off", "scrape", "paper", "manual", "auto")
  for (i in seq_along(modes)) {
    for (j in seq_along(modes)) {
      expect_identical(
        betting_mode_at_least(list(betting = list(mode = modes[[i]])), modes[[j]]),
        i >= j,
        info = paste(modes[[i]], ">=", modes[[j]])
      )
    }
  }
})

test_that("betting_enabled is exactly 'mode at least paper'", {
  expect_false(betting_enabled(list(betting = list(mode = "scrape"))))
  expect_true(betting_enabled(list(betting = list(mode = "paper"))))
  expect_false(betting_enabled(list(betting = list(enabled = FALSE))))
  expect_true(betting_enabled(list(sport = "football")))
})

test_that("betting_mode_at_least fails closed on an invalid stage", {
  # match.arg() would map NULL to "off" (TRUE for every league) and
  # partial-match "p" to "paper"; the placer gates must never do either.
  auto <- list(betting = list(mode = "auto"))
  bad <- list(NULL, "p", character(0), NA_character_, c("paper", "auto"))
  for (stage in bad) {
    expect_error(
      betting_mode_at_least(auto, stage),
      "stage must be one of",
      info = deparse(stage)
    )
  }
})

# --- one gate per layer -------------------------------------------------------

.ladder_recs <- function() {
  tibble::tibble(
    sport = "handball", country = "iceland", sex = "male",
    run_id = as.POSIXct("2026-09-23 10:00:00", tz = "UTC"),
    match_date = as.Date("2100-01-05"), home_team = "Valur", away_team = "FH",
    market = "moneyline", outcome = "home", line = NA_real_,
    p = 0.6, odds = 2.0, ev = 0.2, kelly = 0.05, bet_amount = 500
  )
}

.ladder_cfg <- function(mode) {
  list(handball_iceland = list(
    sport = "handball", country = "iceland", betting = list(mode = mode)
  ))
}

test_that("odds ingest runs from 'scrape' up and never below", {
  called <- 0L
  testthat::local_mocked_bindings(
    .is_league_active = function(active_path, key) TRUE,
    ingest_lengjan_odds = function(...) {
      called <<- called + 1L
      3L
    }
  )
  for (m in c("off", "scrape", "paper", "manual", "auto")) {
    called <- 0L
    suppressMessages(ingest_one_lengjan(
      list(sport = "handball", country = "iceland"),
      list(competitions = list(list(id = "1269", name = "x", sex = "male"))),
      "handball_iceland", "active.json",
      betting = list(mode = m)
    ))
    expect_identical(called, if (m == "off") 0L else 1L, info = m)
  }
})

test_that("decide produces candidates from 'paper' up and nothing below", {
  run <- function(mode) {
    root <- withr::local_tempdir()
    md <- Sys.Date() + 1L
    set.seed(11)
    write_table(tibble::tibble(
      sport = "handball", country = "iceland", sex = "male",
      fit_date = Sys.Date(), match_date = md,
      home_team = "Valur", away_team = "FH", draw_id = 1:1000L,
      home_goals = rpois(1000, 30), away_goals = rpois(1000, 26)
    ), "beliefs_latest", root = root)
    write_table(tibble::tibble(
      sport = "handball", country = "iceland", scraped_at = Sys.time(),
      match_date = md, home_team = "Valur", away_team = "FH",
      market = "moneyline", outcome = c("home", "away"),
      line = NA_real_, odds = c(2.60, 2.20)
    ), "odds", root = root)
    league <- list(
      sport = "handball", country = "iceland", sexes = "male",
      active = TRUE, stan_model = "x.stan",
      betting = list(
        mode = mode, kelly_frac = 0.10, ev_threshold = 0.0,
        markets = list(moneyline = TRUE, spread = FALSE, total = FALSE),
        scoring = list(has_ties = TRUE, tie_threshold = 0.5),
        min_bet = 1L, max_age_hours = 999999L
      )
    )
    nrow(suppressMessages(decide_league(
      league = league, sex = "male", root = root, return_candidates = TRUE,
      bankroll = list(
        initial_pool = 23610, current_pool = 23610,
        daily_budget_frac = 0.5, daily_budget_min_isk = 1000
      )
    )))
  }
  expect_gt(run("paper"), 0L) # positive control: the fixture does yield candidates
  expect_gt(run("auto"), 0L)
  expect_equal(run("scrape"), 0L)
  expect_equal(run("off"), 0L)
})

test_that("the placer loader keeps recommendations only from 'manual' up", {
  for (m in c("off", "scrape", "paper", "manual", "auto")) {
    out <- suppressMessages(
      drop_betting_disabled(.ladder_recs(), leagues_cfg = .ladder_cfg(m))
    )
    expect_equal(nrow(out), if (m %in% c("manual", "auto")) 1L else 0L, info = m)
  }
})

test_that("the placer pre-flight refuses leagues below 'manual'", {
  expect_error(
    validate_betting_enabled(.ladder_cfg("paper"), .ladder_recs()),
    "handball_iceland"
  )
  expect_true(validate_betting_enabled(.ladder_cfg("manual"), .ladder_recs()))
})

test_that("fit priority follows the ladder: money first, then paper, then scrape", {
  # Config order is basketball, handball, football. A two-tier sort on
  # betting_enabled() would fit paper handball BEFORE auto football, so the
  # fit.yml timeout would cut the money fit.
  targets <- tibble::tibble(
    key = c("basketball_iceland", "handball_iceland", "football_iceland"),
    sex = "male"
  )
  leagues <- list(
    basketball_iceland = list(betting = list(mode = "scrape")),
    handball_iceland = list(betting = list(mode = "paper")),
    football_iceland = list(betting = NULL)
  )
  expect_equal(
    order_fit_targets(targets, leagues)$key,
    c("football_iceland", "handball_iceland", "basketball_iceland")
  )
})
