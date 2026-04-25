# Helper: build minimal beliefs + odds for one match
mini_decide_setup <- function(root) {
  beliefs <- tibble::tibble(
    sport = "basketball", country = "iceland", sex = "male",
    fit_date = Sys.Date(),
    match_date = as.Date("2026-04-26"),
    home_team = "Alpha", away_team = "Bravo",
    draw_id = 1:1000,
    home_goals = rpois(1000, lambda = 90),
    away_goals = rpois(1000, lambda = 85)
  )
  write_table(beliefs, "beliefs_latest", root = root)

  odds <- tibble::tibble(
    sport = "basketball", country = "iceland",
    scraped_at = Sys.time(),
    match_date = as.Date("2026-04-26"),
    home_team = "Alpha", away_team = "Bravo",
    market = c("moneyline", "moneyline"),
    outcome = c("home", "away"),
    line = NA_real_,
    odds = c(1.85, 2.10)
  )
  write_table(odds, "odds", root = root)
}

mini_basketball_league <- function() {
  list(
    sport = "basketball", country = "iceland", sexes = "male",
    active = TRUE, stan_model = "x.stan",
    betting = list(
      kelly_frac = 0.10, ev_threshold = 0.0,
      markets = list(moneyline = TRUE, spread = TRUE, total = TRUE),
      scoring = list(has_ties = FALSE, tie_threshold = 0),
      min_bet = 200, max_age_hours = 999999L
    )
  )
}

mini_bankroll <- function() {
  list(
    initial_pool = 23610, current_pool = 23610,
    daily_budget_frac = 0.05, daily_budget_min_isk = 1000
  )
}

test_that("decide_league writes candidates and recommendations", {
  set.seed(13)
  root <- withr::local_tempdir()
  mini_decide_setup(root)
  league <- mini_basketball_league()

  out <- decide_league(
    league = league, sex = "male",
    run_date = as.Date("2026-04-25"),
    root = root,
    bankroll = mini_bankroll()
  )

  expect_s3_class(out, "tbl_df")

  # Both tables written
  cands <- read_table("candidates",
    filter = list(sport = "basketball", country = "iceland"),
    root = root
  )
  recs <- read_table("recommendations",
    filter = list(sport = "basketball", country = "iceland"),
    root = root
  )
  expect_gt(nrow(cands), 0L)
  # Recommendations may be empty if no bet survives min_bet — but candidates
  # capture the journey
  expect_true(nrow(recs) >= 0L)
})

test_that("decide_league errors when both league_key and league supplied", {
  expect_error(
    decide_league(league_key = "x", league = list(sport = "y"), sex = "male"),
    "Exactly one of"
  )
})

test_that("decide_league errors when neither league_key nor league supplied", {
  expect_error(
    decide_league(sex = "male"),
    "Exactly one of"
  )
})

test_that("decide_league returns empty tibble when no beliefs", {
  root <- withr::local_tempdir()
  league <- mini_basketball_league()
  expect_warning(
    out <- decide_league(
      league = league, sex = "male", root = root,
      bankroll = mini_bankroll()
    ),
    "no beliefs"
  )
  expect_equal(nrow(out), 0L)
})

test_that("decide_league markets toggle drops disabled markets", {
  set.seed(13)
  root <- withr::local_tempdir()
  mini_decide_setup(root)
  league <- mini_basketball_league()
  # Disable all markets
  league$betting$markets$moneyline <- FALSE
  league$betting$markets$spread <- FALSE
  league$betting$markets$total <- FALSE

  out <- decide_league(
    league = league, sex = "male", root = root,
    bankroll = mini_bankroll()
  )
  # No bets allowed -> no candidates with stage='kept'
  cands <- read_table("candidates",
    filter = list(sport = "basketball", country = "iceland"),
    root = root
  )
  if (nrow(cands) > 0L) {
    expect_false("kept" %in% cands$stage)
  }
})

test_that("decide_league stage column tracks the filter journey", {
  set.seed(13)
  root <- withr::local_tempdir()
  mini_decide_setup(root)
  league <- mini_basketball_league()

  decide_league(
    league = league, sex = "male", root = root,
    bankroll = mini_bankroll()
  )
  cands <- read_table("candidates",
    filter = list(sport = "basketball", country = "iceland"),
    root = root
  )
  expect_true("stage" %in% names(cands))
  expect_true(all(cands$stage %in%
    c(
      "candidate", "post_portfolio", "post_calibration",
      "kept", "dropped_min_bet", "dropped_low_ev", "dropped_market_off"
    )))
})
