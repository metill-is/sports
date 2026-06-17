# Tests for the Lengjan->canonical team-name normalisation that lets
# decide_league join odds with `kv`-suffixed Lengjan names against beliefs
# with bare federation-side names.
#
# Bug context (2026-05-11): women's leagues produced zero recommendations
# lifetime because odds Parquet had "Fram kv" / "Grindavik kv" while
# beliefs had "Fram" / "Grindavik", and the per-match join in
# decide_league() silently dropped non-matching rows.

test_that("normalise_lengjan_team_names rewrites Lengjan-side teams to canonical", {
  odds <- tibble::tibble(
    match_date = as.Date("2026-05-12"),
    home_team = "Grindav\u00edk kv",
    away_team = "Valur kv",
    market = "moneyline", outcome = "home",
    line = NA_real_, odds = 1.80,
    scraped_at = as.POSIXct("2026-05-12 09:00:00", tz = "UTC")
  )
  league <- list(
    sport = "basketball", country = "iceland",
    lengjan = list(team_names = list(
      female = list(
        "Grindav\u00edk" = "Grindav\u00edk kv",
        Valur = "Valur kv"
      )
    ))
  )

  out <- normalise_lengjan_team_names(odds, league, sex = "female")

  expect_equal(out$home_team, "Grindav\u00edk")
  expect_equal(out$away_team, "Valur")
})

test_that("normalise_lengjan_team_names maps every rendering of a list-valued entry to canonical", {
  # Lengjan renders this women's team under two byte-distinct forms (verified
  # against data/facts/odds): accented + spaced slash, and plain-i + bare slash.
  # A list-valued team_names entry must decode both to the one canonical name.
  odds <- tibble::tibble(
    match_date = as.Date("2026-05-20"),
    home_team = c(
      "Grindav\u00edk / Njar\u00f0v\u00edk kv",
      "Grindavik/Njar\u00f0v\u00edk kv"
    ),
    away_team = c("Valur kv", "Stjarnan kv"),
    market = "moneyline", outcome = "home",
    line = NA_real_, odds = 1.80,
    scraped_at = as.POSIXct("2026-05-20 09:00:00", tz = "UTC")
  )
  league <- list(
    sport = "football", country = "iceland",
    lengjan = list(team_names = list(
      female = list(
        "Grindav\u00edk/Njar\u00f0v\u00edk" = list(
          "Grindav\u00edk / Njar\u00f0v\u00edk kv",
          "Grindavik/Njar\u00f0v\u00edk kv"
        ),
        Valur = "Valur kv",
        Stjarnan = "Stjarnan kv"
      )
    ))
  )

  out <- normalise_lengjan_team_names(odds, league, sex = "female")

  expect_equal(
    out$home_team,
    c("Grindav\u00edk/Njar\u00f0v\u00edk", "Grindav\u00edk/Njar\u00f0v\u00edk")
  )
  expect_equal(out$away_team, c("Valur", "Stjarnan"))
})

test_that("normalise_lengjan_team_names errors when a rendering is shared across list entries", {
  odds <- tibble::tibble(
    match_date = as.Date("2026-05-12"),
    home_team = "X", away_team = "Y",
    market = "moneyline", outcome = "home",
    line = NA_real_, odds = 1.80,
    scraped_at = as.POSIXct("2026-05-12 09:00:00", tz = "UTC")
  )
  # A rendering listed under A also appears under B -> ambiguous inverse.
  league <- list(
    sport = "football", country = "iceland",
    lengjan = list(team_names = list(
      female = list(A = list("A kv", "Shared kv"), B = "Shared kv")
    ))
  )

  expect_error(
    normalise_lengjan_team_names(odds, league, sex = "female"),
    "non-injective"
  )
})

test_that("normalise_lengjan_team_names is identity (with warning) when team_names is empty", {
  odds <- tibble::tibble(
    match_date = as.Date("2026-05-12"),
    home_team = "Fram",
    away_team = "KR",
    market = "moneyline", outcome = "home",
    line = NA_real_, odds = 1.80,
    scraped_at = as.POSIXct("2026-05-12 09:00:00", tz = "UTC")
  )
  league <- list(
    sport = "football", country = "iceland",
    lengjan = list(team_names = list(female = list()))
  )

  expect_message(
    out <- normalise_lengjan_team_names(odds, league, sex = "female"),
    "no team_names"
  )
  expect_equal(out$home_team, "Fram")
  expect_equal(out$away_team, "KR")
})

test_that("normalise_lengjan_team_names warns + passes through unmapped names", {
  odds <- tibble::tibble(
    match_date = as.Date("2026-05-12"),
    home_team = "Unknown Combined kv",
    away_team = "Haukar kv",
    market = "moneyline", outcome = "home",
    line = NA_real_, odds = 1.80,
    scraped_at = as.POSIXct("2026-05-12 09:00:00", tz = "UTC")
  )
  league <- list(
    sport = "basketball", country = "iceland",
    lengjan = list(team_names = list(
      female = list(Haukar = "Haukar kv")
    ))
  )

  expect_message(
    out <- normalise_lengjan_team_names(odds, league, sex = "female"),
    "no team_names mapping"
  )
  # Mapped name normalises; unmapped passes through unchanged so the loud
  # warning in decide-pipeline catches the resulting empty-beliefs join.
  expect_equal(out$home_team, "Unknown Combined kv")
  expect_equal(out$away_team, "Haukar")
})

test_that("normalise_lengjan_team_names errors when the inverse map is not injective", {
  odds <- tibble::tibble(
    match_date = as.Date("2026-05-12"),
    home_team = "X", away_team = "Y",
    market = "moneyline", outcome = "home",
    line = NA_real_, odds = 1.80,
    scraped_at = as.POSIXct("2026-05-12 09:00:00", tz = "UTC")
  )
  # Two canonical names mapping to the same Lengjan rendering -- ambiguous
  # inverse; would silently corrupt the join.
  league <- list(
    sport = "basketball", country = "iceland",
    lengjan = list(team_names = list(
      female = list(A = "Shared", B = "Shared")
    ))
  )

  expect_error(
    normalise_lengjan_team_names(odds, league, sex = "female"),
    "non-injective"
  )
})

test_that("prepare_odds applies team-name normalisation when league$lengjan$team_names is set", {
  root <- withr::local_tempdir()
  odds <- tibble::tibble(
    sport = "basketball", country = "iceland",
    scraped_at = as.POSIXct("2026-05-12 09:00:00", tz = "UTC"),
    match_date = Sys.Date() + 1L,
    home_team = "Grindav\u00edk kv",
    away_team = "Valur kv",
    market = "moneyline", outcome = "home",
    line = NA_real_, odds = 1.80
  )
  write_table(odds, "odds", root = root)

  league <- list(
    sport = "basketball", country = "iceland",
    lengjan = list(team_names = list(
      female = list(
        "Grindav\u00edk" = "Grindav\u00edk kv",
        Valur = "Valur kv"
      )
    ))
  )

  out <- prepare_odds(league,
    sex = "female",
    end_date = Sys.Date(),
    max_age_hours = 24L * 365 * 100L,
    now = as.POSIXct("2026-05-12 10:00:00", tz = "UTC"),
    root = root
  )

  expect_equal(nrow(out), 1L)
  expect_equal(out$home_team, "Grindav\u00edk")
  expect_equal(out$away_team, "Valur")
})

test_that("decide_league produces candidates for women's matches when odds use kv suffix and beliefs do not", {
  set.seed(13)
  root <- withr::local_tempdir()

  match_date <- Sys.Date() + 1L
  beliefs <- tibble::tibble(
    sport = "basketball", country = "iceland", sex = "female",
    fit_date = Sys.Date(),
    match_date = match_date,
    home_team = "Grindav\u00edk", away_team = "Valur",
    draw_id = 1:1000,
    home_goals = rpois(1000, lambda = 90),
    away_goals = rpois(1000, lambda = 85)
  )
  write_table(beliefs, "beliefs_latest", root = root)

  odds <- tibble::tibble(
    sport = "basketball", country = "iceland",
    scraped_at = Sys.time(),
    match_date = match_date,
    home_team = "Grindav\u00edk kv",
    away_team = "Valur kv",
    market = c("moneyline", "moneyline"),
    outcome = c("home", "away"),
    line = NA_real_,
    odds = c(1.85, 2.10)
  )
  write_table(odds, "odds", root = root)

  league <- list(
    sport = "basketball", country = "iceland", sexes = "female",
    active = TRUE, stan_model = "x.stan",
    lengjan = list(team_names = list(
      female = list(
        "Grindav\u00edk" = "Grindav\u00edk kv",
        Valur = "Valur kv"
      )
    )),
    betting = list(
      kelly_frac = list(female = 0.10),
      ev_threshold = 0.0,
      markets = list(moneyline = TRUE, spread = TRUE, total = TRUE),
      scoring = list(has_ties = FALSE, tie_threshold = 0),
      min_bet = 200, max_age_hours = 999999L
    )
  )

  bankroll <- list(
    initial_pool = 23610, current_pool = 23610,
    daily_budget_frac = 0.05, daily_budget_min_isk = 1000
  )

  out <- decide_league(
    league = league, sex = "female",
    run_date = as.Date("2026-05-12"),
    root = root,
    bankroll = bankroll
  )

  cands <- read_table("candidates",
    filter = list(sport = "basketball", country = "iceland"),
    root = root
  )
  # The whole point: candidates exist (silent skip is gone), the join found
  # beliefs for this match, and at least one non-market-off candidate appears.
  expect_gt(nrow(cands), 0L)
  expect_true(any(cands$stage != "dropped_market_off"))
})
