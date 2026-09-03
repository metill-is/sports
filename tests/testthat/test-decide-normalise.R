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

# ── Lengjan mid-season rename: "Víkingur Rvk" -> "Víkingur Reykjavík" ────────
#
# Bug context (found 2026-09-03; live since 2026-07-17). Lengjan renamed the
# men's Besta deildin side's display string mid-season. The 2026-07-17 scrape
# carries the SAME fixture (Þór Ak. v Víkingur, kickoff 2026-07-18) under BOTH
# "Víkingur Rvk" and "Víkingur Reykjavík"; every scrape since shows only the
# latter. team_names.male mapped canonical "Víkingur R." to "Víkingur Rvk"
# alone, so the decode inverse map had no entry for the new rendering,
# decide_league() warn-and-skipped every Víkingur fixture, and 8 Besta
# deildin matches (2026-07-18 .. 2026-09-06) could never be bet —
# football_iceland being the only betting-enabled league.
#
# These bind to the REAL config rather than a synthetic fixture, because what
# regressed is config *content*: a later tidy-up that collapses the list back
# to a scalar, or that replaces the historical rendering instead of appending
# to it, must fail here. test_path() (not here::here()) so the repo root also
# resolves correctly inside a git worktree.

real_football_iceland <- function() {
  cfg <- load_leagues(
    path = testthat::test_path("..", "..", "config", "leagues.yml"),
    schema_path = testthat::test_path("..", "..", "config", "leagues.schema.json")
  )
  cfg$football_iceland
}

test_that("real config decodes both Lengjan renderings of V\u00edkingur R. (male)", {
  league <- real_football_iceland()

  # Row 1 is the live fixture that was being dropped; row 2 is the pre-rename
  # rendering that historical odds (2026-04-15 .. 2026-07-17) and the
  # backtest/replay path still read.
  odds <- tibble::tibble(
    match_date = as.Date(c("2026-09-06", "2026-07-18")),
    home_team = c("V\u00edkingur Reykjav\u00edk", "\u00de\u00f3r Ak."),
    away_team = c("Fram", "V\u00edkingur Rvk"),
    market = "moneyline", outcome = "home",
    line = NA_real_, odds = 1.80,
    scraped_at = as.POSIXct("2026-09-02 09:00:00", tz = "UTC")
  )

  out <- normalise_lengjan_team_names(odds, league, sex = "male")

  # Both renderings must collapse onto the one canonical name that
  # data/facts/results and data/beliefs/latest carry.
  expect_equal(out$home_team, c("V\u00edkingur R.", "\u00de\u00f3r"))
  expect_equal(out$away_team, c("Fram", "V\u00edkingur R."))
})

test_that("real config keeps the women's V\u00edkingur rendering decoding (female)", {
  league <- real_football_iceland()

  # The rename hit the men's side only — Lengjan still showed
  # "Víkingur Rvk kv" on 2026-08-29. team_names is per-sex, so the female
  # sub-map must keep decoding its own rendering independently of the male fix.
  odds <- tibble::tibble(
    match_date = as.Date("2026-09-07"),
    home_team = "Stjarnan kv",
    away_team = "V\u00edkingur Rvk kv",
    market = "moneyline", outcome = "home",
    line = NA_real_, odds = 2.10,
    scraped_at = as.POSIXct("2026-09-02 09:00:00", tz = "UTC")
  )

  out <- normalise_lengjan_team_names(odds, league, sex = "female")

  expect_equal(out$home_team, "Stjarnan")
  expect_equal(out$away_team, "V\u00edkingur R.")
})
