# Helper: build minimal beliefs + odds for one match.
# `match_date` is kept relative to Sys.Date() rather than hardcoded -- callers
# that don't pass an explicit run_date to decide_league() default to today, and
# any fixed past date silently turns the match into a no-op once Sys.Date()
# passes it.
mini_decide_setup <- function(root, match_date = Sys.Date() + 1L) {
  beliefs <- tibble::tibble(
    sport = "basketball", country = "iceland", sex = "male",
    fit_date = Sys.Date(),
    match_date = match_date,
    home_team = "Alpha", away_team = "Bravo",
    draw_id = 1:1000,
    home_goals = rpois(1000, lambda = 90),
    away_goals = rpois(1000, lambda = 85)
  )
  write_table(beliefs, "beliefs_latest", root = root)

  odds <- tibble::tibble(
    sport = "basketball", country = "iceland",
    scraped_at = Sys.time(),
    match_date = match_date,
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
  cands <- read_table("candidates",
    filter = list(sport = "basketball", country = "iceland"),
    root = root
  )
  # Markets disabled -> all rows tagged dropped_market_off, none kept
  expect_gt(nrow(cands), 0L)
  expect_false("kept" %in% cands$stage)
  expect_true(all(cands$stage == "dropped_market_off"))
})

test_that("decide_league stage column emits only terminal classifications", {
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
      "kept", "dropped_min_bet", "dropped_low_ev",
      "dropped_market_off", "dropped_invalid_input"
    )))
})

test_that("decide_league tags a non-finite posterior as dropped_invalid_input", {
  set.seed(13)
  root <- withr::local_tempdir()
  beliefs <- tibble::tibble(
    sport = "basketball", country = "iceland", sex = "male",
    fit_date = Sys.Date(),
    match_date = Sys.Date() + 1L,
    home_team = "Alpha", away_team = "Bravo",
    draw_id = 1:1000,
    home_goals = c(NaN, rpois(999, lambda = 90)),
    away_goals = rpois(1000, lambda = 85)
  )
  write_table(beliefs, "beliefs_latest", root = root)
  odds <- tibble::tibble(
    sport = "basketball", country = "iceland",
    scraped_at = Sys.time(),
    match_date = Sys.Date() + 1L,
    home_team = "Alpha", away_team = "Bravo",
    market = c("moneyline", "moneyline"),
    outcome = c("home", "away"),
    line = NA_real_,
    odds = c(1.85, 2.10)
  )
  write_table(odds, "odds", root = root)

  decide_league(
    league = mini_basketball_league(), sex = "male", root = root,
    bankroll = mini_bankroll()
  )
  cands <- read_table("candidates",
    filter = list(sport = "basketball", country = "iceland"),
    root = root
  )
  expect_true(all(cands$stage == "dropped_invalid_input"))
  recs <- read_table("recommendations",
    filter = list(sport = "basketball", country = "iceland"),
    root = root
  )
  expect_equal(nrow(recs), 0L)
})

test_that("decide_league handles per-sex kelly_frac (object form)", {
  set.seed(13)
  root <- withr::local_tempdir()
  mini_decide_setup(root)
  league <- mini_basketball_league()
  # Switch to per-sex object form (mimics football_iceland)
  league$betting$kelly_frac <- list(male = 0.20, female = 0.05)

  out <- decide_league(
    league = league, sex = "male", root = root,
    bankroll = mini_bankroll()
  )
  expect_s3_class(out, "tbl_df")
  # No error means the per-sex resolution worked.
})

test_that("decide_league errors on missing kelly_frac for the requested sex", {
  set.seed(13)
  root <- withr::local_tempdir()
  mini_decide_setup(root)
  # Add female beliefs so the function reaches the kelly_frac check (otherwise
  # it short-circuits at the empty-beliefs warning). match_date relative to
  # Sys.Date() so the fixture stays in the future.
  female_beliefs <- tibble::tibble(
    sport = "basketball", country = "iceland", sex = "female",
    fit_date = Sys.Date(),
    match_date = Sys.Date() + 1L,
    home_team = "Alpha", away_team = "Bravo",
    draw_id = 1:1000,
    home_goals = rpois(1000, lambda = 90),
    away_goals = rpois(1000, lambda = 85)
  )
  write_table(female_beliefs, "beliefs_latest", root = root)

  league <- mini_basketball_league()
  league$betting$kelly_frac <- list(male = 0.10) # no female key

  expect_error(
    decide_league(
      league = league, sex = "female", root = root,
      bankroll = mini_bankroll()
    ),
    "kelly_frac"
  )
})

test_that("decide_league with write = FALSE does not write tables", {
  set.seed(13)
  root <- withr::local_tempdir()
  mini_decide_setup(root)
  league <- mini_basketball_league()

  out <- decide_league(
    league = league, sex = "male", root = root,
    bankroll = mini_bankroll(), write = FALSE
  )
  expect_s3_class(out, "tbl_df")
  expect_false(dir.exists(file.path(root, "decisions", "candidates")))
  expect_false(dir.exists(file.path(root, "decisions", "recommendations")))
})

test_that("decide_league recommendations contain only kept rows", {
  set.seed(13)
  root <- withr::local_tempdir()
  mini_decide_setup(root)
  league <- mini_basketball_league()
  # Drop min_bet so all positive-EV bets survive
  league$betting$min_bet <- 0

  out <- decide_league(
    league = league, sex = "male", root = root,
    bankroll = mini_bankroll()
  )
  recs <- read_table("recommendations",
    filter = list(sport = "basketball", country = "iceland"),
    root = root
  )
  # All recs match `kept` stage from candidates
  if (nrow(recs) > 0L) {
    expect_true(all(recs$bet_amount > 0))
    expect_true(all(c("kelly", "bet_amount") %in% names(recs)))
  }
  expect_equal(nrow(recs), nrow(out))
})

test_that("decide_league applies kelly_frac as a multiplicative shrinkage", {
  set.seed(13)
  root <- withr::local_tempdir()
  mini_decide_setup(root)
  league <- mini_basketball_league()
  league$betting$kelly_frac <- 0.10
  league$betting$min_bet <- 0

  # Generous daily budget so portfolio never scales down — isolates the
  # multiplicative chain from the portfolio-scaling step.
  br <- mini_bankroll()
  br$daily_budget_frac <- 1.0

  out <- decide_league(
    league = league, sex = "male",
    run_date = as.Date("2026-04-25"),
    root = root,
    bankroll = br
  )
  cands <- read_table("candidates",
    filter = list(sport = "basketball", country = "iceland"),
    root = root
  )
  kept <- cands[cands$stage == "kept", , drop = FALSE]
  expect_gt(nrow(kept), 0L)

  # No ledger -> calibration multiplier = prior_ratio = 1.0
  # Single match + generous budget -> portfolio lambda = 1.0
  # kelly_frac = 0.10, ceiling = 0.25 -> shrink_eff = 0.10
  ratio <- out$kelly[1] / kept$kelly_raw[1]
  expect_lt(ratio, 0.105)
  expect_gt(ratio, 0.095)
})

test_that("decide_league clamps kelly_frac at kelly_ceiling (K5 invariant)", {
  set.seed(13)
  root <- withr::local_tempdir()
  mini_decide_setup(root)
  league <- mini_basketball_league()
  # kelly_frac > kelly_ceiling: 0.30 > 0.25 should clamp to 0.25
  league$betting$kelly_frac <- 0.30
  league$betting$min_bet <- 0

  br <- mini_bankroll()
  br$daily_budget_frac <- 1.0

  out <- decide_league(
    league = league, sex = "male",
    run_date = as.Date("2026-04-25"),
    root = root,
    bankroll = br
  )
  cands <- read_table("candidates",
    filter = list(sport = "basketball", country = "iceland"),
    root = root
  )
  kept <- cands[cands$stage == "kept", , drop = FALSE]
  expect_gt(nrow(kept), 0L)

  # Without the ceiling clamp, ratio would be 0.30. Clamped at 0.25.
  ratio <- out$kelly[1] / kept$kelly_raw[1]
  expect_lt(ratio, 0.255)
  expect_gt(ratio, 0.245)
})


# --- per-sex runs share a partition -----------------------------------------
# candidates/recommendations partition on (sport, country, run_date) with no
# sex, and 04_decide.R runs decide_league() once per sex. Before the fix the
# second sex's write replaced the partition and wiped the first (audit
# 2026-11-03: football_iceland 2026-08-10..19 held a single sex per day).

.two_sex_setup <- function(root, sexes = c("male", "female"),
                           home_odds = c(male = 1.85, female = 1.85)) {
  md <- Sys.Date() + 1L
  beliefs <- dplyr::bind_rows(lapply(c("male", "female"), function(s) {
    tibble::tibble(
      sport = "basketball", country = "iceland", sex = s,
      fit_date = Sys.Date(), match_date = md,
      home_team = paste0("Home_", s), away_team = paste0("Away_", s),
      draw_id = 1:1000,
      home_goals = rpois(1000, lambda = 90),
      away_goals = rpois(1000, lambda = 85)
    )
  }))
  write_table(beliefs, "beliefs_latest", root = root)
  odds <- dplyr::bind_rows(lapply(sexes, function(s) {
    tibble::tibble(
      sport = "basketball", country = "iceland", sex = s,
      scraped_at = Sys.time(), match_date = md,
      home_team = paste0("Home_", s), away_team = paste0("Away_", s),
      market = "moneyline", outcome = c("home", "away"),
      line = NA_real_, odds = c(home_odds[[s]], 2.10)
    )
  }))
  write_table(odds, "odds", root = root)
}

.two_sex_decide <- function(root, sex, markets_on = TRUE) {
  league <- mini_basketball_league()
  league$betting$min_bet <- 0
  if (!markets_on) league$betting$markets[] <- list(FALSE)
  decide_league(
    league = league, sex = sex, run_date = Sys.Date(),
    root = root, bankroll = mini_bankroll()
  )
}

.read_decided <- function(table, root) {
  out <- read_table(table,
    filter = list(sport = "basketball", country = "iceland"), root = root
  )
  if (nrow(out) == 0L) {
    return(out)
  }
  out[order(out$sex, out$home_team, out$market, out$outcome), , drop = FALSE]
}

test_that("decide_league keeps both sexes when they share a run_date", {
  set.seed(13)
  root <- withr::local_tempdir()
  .two_sex_setup(root)

  .two_sex_decide(root, "male")
  .two_sex_decide(root, "female")

  cands <- .read_decided("candidates", root)
  recs <- .read_decided("recommendations", root)
  expect_setequal(unique(cands$sex), c("male", "female"))
  expect_setequal(unique(recs$sex), c("male", "female"))
  expect_equal(length(unique(as.character(cands$run_date))), 1L)
})

test_that("rerunning one sex's decide replaces only that sex's rows", {
  set.seed(13)
  root <- withr::local_tempdir()
  .two_sex_setup(root)
  .two_sex_decide(root, "male")
  .two_sex_decide(root, "female")
  female_cands <- .read_decided("candidates", root)
  female_cands <- female_cands[female_cands$sex == "female", , drop = FALSE]
  female_recs <- .read_decided("recommendations", root)
  female_recs <- female_recs[female_recs$sex == "female", , drop = FALSE]

  .two_sex_setup(root, home_odds = c(male = 1.95, female = 1.85))
  .two_sex_decide(root, "male")

  cands <- .read_decided("candidates", root)
  male <- cands[cands$sex == "male", , drop = FALSE]
  expect_equal(nrow(male), 2L)
  expect_equal(male$odds[male$outcome == "home"], 1.95)
  expect_equal(cands[cands$sex == "female", , drop = FALSE], female_cands)
  recs <- .read_decided("recommendations", root)
  expect_equal(sum(recs$sex == "male"), 1L)
  expect_equal(recs$odds[recs$sex == "male"], 1.95)
  expect_equal(recs[recs$sex == "female", , drop = FALSE], female_recs)
})

test_that("a zero-row decide run for one sex leaves the other untouched", {
  set.seed(13)
  root <- withr::local_tempdir()
  .two_sex_setup(root, sexes = "male")
  .two_sex_decide(root, "male")
  cands_before <- .read_decided("candidates", root)
  recs_before <- .read_decided("recommendations", root)
  expect_gt(nrow(recs_before), 0L)

  out <- .two_sex_decide(root, "female")

  expect_equal(nrow(out), 0L)
  expect_equal(.read_decided("candidates", root), cands_before)
  expect_equal(.read_decided("recommendations", root), recs_before)
})

test_that("a rerun with candidates but no recommendations clears that sex's", {
  set.seed(13)
  root <- withr::local_tempdir()
  .two_sex_setup(root)
  .two_sex_decide(root, "male")
  .two_sex_decide(root, "female")
  recs_before <- .read_decided("recommendations", root)
  expect_setequal(unique(recs_before$sex), c("male", "female"))

  # Female home odds of 1.30 leave both female bets below EV.
  .two_sex_setup(root, home_odds = c(male = 1.85, female = 1.30))
  out <- .two_sex_decide(root, "female")

  expect_equal(nrow(out), 0L)
  cands <- .read_decided("candidates", root)
  expect_equal(unique(cands$stage[cands$sex == "female"]), "dropped_low_ev")
  expect_equal(
    .read_decided("recommendations", root),
    recs_before[recs_before$sex == "male", , drop = FALSE]
  )
})

test_that("a market-off run keeps the other sex and clears its own recs", {
  set.seed(13)
  root <- withr::local_tempdir()
  .two_sex_setup(root)
  # Female first, so the market-off run is the only write that could wipe the
  # male rows.
  .two_sex_decide(root, "female")
  expect_gt(nrow(.read_decided("recommendations", root)), 0L)
  .two_sex_decide(root, "male")
  cands_before <- .read_decided("candidates", root)
  recs_before <- .read_decided("recommendations", root)
  expect_gt(sum(recs_before$sex == "male"), 0L)

  out <- .two_sex_decide(root, "female", markets_on = FALSE)

  expect_equal(nrow(out), 0L)
  cands <- .read_decided("candidates", root)
  expect_equal(
    cands[cands$sex == "male", , drop = FALSE],
    cands_before[cands_before$sex == "male", , drop = FALSE]
  )
  female <- cands[cands$sex == "female", , drop = FALSE]
  expect_equal(nrow(female), 2L)
  expect_equal(unique(female$stage), "dropped_market_off")
  expect_equal(
    .read_decided("recommendations", root),
    recs_before[recs_before$sex == "male", , drop = FALSE]
  )
})

test_that("a market-off rerun clears the only sex's recommendations", {
  set.seed(13)
  root <- withr::local_tempdir()
  .two_sex_setup(root, sexes = "male")
  .two_sex_decide(root, "male")
  expect_gt(nrow(.read_decided("recommendations", root)), 0L)

  .two_sex_decide(root, "male", markets_on = FALSE)

  expect_equal(nrow(.read_decided("recommendations", root)), 0L)
  expect_equal(
    unique(.read_decided("candidates", root)$stage), "dropped_market_off"
  )
})

test_that("decide_league aborts when the shared partition cannot be read", {
  set.seed(13)
  root <- withr::local_tempdir()
  .two_sex_setup(root)
  .two_sex_decide(root, "male")
  part <- list.files(file.path(root, "decisions", "candidates"),
    pattern = "[.]parquet$", recursive = TRUE, full.names = TRUE
  )
  expect_length(part, 1L)
  writeLines("not parquet", part)

  expect_error(.two_sex_decide(root, "female"))
  expect_identical(readLines(part), "not parquet")
})


# --- division exclusions --------------------------------------------------
# Some divisions are modelled but must never be priced. LD1_PO (the Lengjudeild
# promotion umspil) is two-legged, so leg 2 is played conditional on the first
# leg's aggregate while the bivariate-Poisson prices each fixture independently;
# its final is also at a neutral venue. No Lengjan comp id is wired for it
# today, so the guard is a tripwire against a future discovery run silently
# turning mispriced bets on.

.excl_odds <- function() {
  tibble::tibble(
    match_date = as.Date(c("2026-09-09", "2026-09-09", "2026-09-12")),
    home_team  = c("Njarðvík", "HK", "Selfoss"),
    away_team  = c("Þróttur R.", "Fylkir", "KFG"),
    market     = "moneyline",
    outcome    = "home",
    odds       = 2.0
  )
}

.excl_root <- function(env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  write_table(
    tibble::tibble(
      sport = "football", country = "iceland", sex = "male", season = 2026L,
      match_date = as.Date(c("2026-09-09", "2026-09-09", "2026-09-12")),
      home_team = c("Njarðvík", "HK", "Selfoss"),
      away_team = c("Þróttur R.", "Fylkir", "KFG"),
      division = c("LD1_PO", "LD1_PO", "LD2"),
      round = NA_integer_,
      kickoff_time = NA_character_
    ),
    "schedules",
    root = root
  )
  root
}

test_that("drop_excluded_divisions removes only the excluded division's rows", {
  league <- list(
    sport = "football", country = "iceland",
    betting = list(exclude_divisions = "LD1_PO")
  )
  out <- drop_excluded_divisions(
    .excl_odds(), league, "male",
    root = .excl_root()
  )
  expect_equal(nrow(out), 1L)
  expect_equal(out$home_team, "Selfoss")
})

test_that("drop_excluded_divisions is a byte-identical no-op when unset", {
  odds <- .excl_odds()
  league <- list(sport = "football", country = "iceland", betting = list())
  # No exclude_divisions -> must not even read schedules, and must return the
  # input untouched. Every other league in config/leagues.yml relies on this.
  expect_identical(
    drop_excluded_divisions(odds, league, "male", root = .excl_root()),
    odds
  )
})

test_that("drop_excluded_divisions fails OPEN on a fixture missing from schedules", {
  # A rescheduled or renamed fixture has no schedules row, so no division can be
  # joined. Keeping it is deliberate: the guard excludes a known-bad division,
  # it does not require schedule coverage. Failing closed here would silently
  # stop betting a whole league the first time a fixture key drifted.
  odds <- tibble::tibble(
    match_date = as.Date("2026-09-30"),
    home_team = "Vestri", away_team = "Grótta",
    market = "moneyline", outcome = "home", odds = 2.0
  )
  league <- list(
    sport = "football", country = "iceland",
    betting = list(exclude_divisions = "LD1_PO")
  )
  expect_equal(
    nrow(drop_excluded_divisions(odds, league, "male", root = .excl_root())),
    1L
  )
})

test_that("football_iceland excludes LD1_PO and declares no LD1 split", {
  # Tripwire, not a restatement of config. Two independent ways a future
  # session could re-arm this defect:
  #   1. dropping exclude_divisions, which would let a newly-discovered Lengjan
  #      umspil comp id start placing two-legged bets the model cannot price;
  #   2. adding a `split:` block to LD1 to fix its empty next_games.json, which
  #      would fabricate ~30 fixtures via the split template and publish a
  #      simulated split season for a league that finished at 132 matches.
  lg <- load_leagues()[["football_iceland"]]
  expect_true("LD1_PO" %in% lg$betting$exclude_divisions)

  for (sx in c("male", "female")) {
    ld1 <- Filter(function(d) d$code == "LD1", lg$publish_divisions[[sx]])
    expect_length(ld1, 1L)
    expect_null(ld1[[1]]$split)
  }
})
