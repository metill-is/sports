# tests/testthat/test-backtest-walkforward.R

test_that("bt_oos_scores computes Brier and log-loss over (p, win)", {
  settled <- tibble::tibble(
    p   = c(0.9, 0.1, 0.5, 0.8),
    win = c(TRUE, FALSE, TRUE, FALSE)
  )
  s <- bt_oos_scores(settled)
  expect_equal(s$n, 4L)
  expect_equal(
    s$brier,
    mean((c(0.9, 0.1, 0.5, 0.8) - c(1, 0, 1, 0))^2)
  )
  expect_equal(
    s$log_loss,
    -mean(c(log(0.9), log(0.9), log(0.5), log(0.2)))
  )
})

test_that("bt_oos_scores clamps p away from 0/1 so log-loss is finite", {
  settled <- tibble::tibble(p = c(0, 1), win = c(FALSE, TRUE))
  s <- bt_oos_scores(settled)
  expect_true(is.finite(s$log_loss))
  expect_true(is.finite(s$brier))
})

test_that("bt_oos_scores returns NA-row for empty input", {
  s <- bt_oos_scores(tibble::tibble(p = numeric(), win = logical()))
  expect_equal(s$n, 0L)
  expect_true(is.na(s$brier))
  expect_true(is.na(s$log_loss))
})

test_that("bt_oos_scores drops rows with NA win (unsettled) before scoring", {
  settled <- tibble::tibble(p = c(0.7, 0.4), win = c(TRUE, NA))
  s <- bt_oos_scores(settled)
  expect_equal(s$n, 1L)
  expect_equal(s$brier, (0.7 - 1)^2)
})

test_that("bt_oos_scores drops rows with NA/non-finite p (e.g. market-off candidates) before scoring", {
  # Scoring ALL candidates means market-off rows (p = NA) and invalid-input rows
  # (p = NaN) reach the scorer; they carry no model probability and must not
  # contaminate the calibration verdict.
  settled <- tibble::tibble(p = c(0.7, NA, NaN, 0.3), win = c(TRUE, TRUE, FALSE, FALSE))
  s <- bt_oos_scores(settled)
  expect_equal(s$n, 2L)
  expect_equal(s$brier, mean((c(0.7, 0.3) - c(1, 0))^2))
})

test_that("bt_wf_slice_odds cuts each match at its OWN match day, independent of any single cutoff (ASSERT-ODDS-2)", {
  # Two matches on different days. The leak-free boundary is each match's own
  # kickoff day -- a snapshot scraped any day BEFORE the match cannot embed the
  # result; a snapshot on/after the match day can. This is per-row, so a match
  # 12 days after a cutoff still keeps its (post-cutoff) pre-match snapshots.
  odds <- tibble::tibble(
    sport = "football", country = "iceland",
    match_date = as.Date(c("2026-05-22", "2026-05-22", "2026-05-29", "2026-05-29")),
    scraped_at = as.POSIXct(
      c(
        "2026-05-21 09:00:00", # day before its match -> keep
        "2026-05-22 09:00:00", # ON its match day -> drop (may embed result)
        "2026-05-28 09:00:00", # day before its match -> keep
        "2026-05-29 18:00:00" # ON its match day -> drop
      ),
      tz = "UTC"
    ),
    home_team = "A", away_team = "B",
    market = "moneyline", outcome = "home",
    line = NA_real_, odds = c(2.50, 1.90, 2.10, 1.70)
  )
  out <- bt_wf_slice_odds(odds)
  expect_equal(nrow(out), 2L)
  expect_true(all(as.Date(out$scraped_at, tz = "UTC") < out$match_date))
  expect_setequal(out$odds, c(2.50, 2.10))
})

test_that("bt_wf_decide selects the latest pre-match-day snapshot, never the match-day/post-result one (ASSERT-ODDS-1, the mandated regression test)", {
  # Three snapshots for one match: two pre-match-day (admissible) and one on the
  # match day after kickoff (embeds the result). The slice must drop the last;
  # prepare_odds' slice_max then takes the latest survivor = closing-ish line.
  odds <- tibble::tibble(
    sport = "football", country = "iceland",
    match_date = as.Date("2026-05-22"),
    scraped_at = as.POSIXct(
      c("2026-05-19 12:00:00", "2026-05-21 12:00:00", "2026-05-22 20:00:00"),
      tz = "UTC"
    ),
    home_team = "A", away_team = "B",
    market = "moneyline", outcome = "home",
    line = NA_real_, odds = c(2.70, 2.50, 1.40)
  )
  wf_root <- withr::local_tempdir()
  write_table(bt_wf_slice_odds(odds), "odds", root = wf_root)
  seen <- prepare_odds(
    list(sport = "football", country = "iceland"), "male",
    end_date = as.Date("2026-05-01"), max_age_hours = bt_wf_max_age_hours(), root = wf_root
  )
  expect_equal(nrow(seen), 1L)
  expect_equal(seen$odds, 2.50)
  expect_false(any(seen$odds == 1.40))
  expect_true(all(as.Date(seen$scraped_at, tz = "UTC") < seen$match_date))
})

test_that("bt_wf_slice_odds + huge max_age_hours still surfaces weeks-old pre-match snapshots (ASSERT-ODDS-3, guards the silent-empty trap)", {
  # A snapshot scraped 4 days before the match must survive BOTH the slice and
  # prepare_odds' lower bound. The bug stacked a same-day-as-cutoff slice on top
  # of a 48h wall-clock lower bound, silently emptying the OOS odds set.
  odds <- tibble::tibble(
    sport = "football", country = "iceland",
    match_date = as.Date("2026-05-22"),
    scraped_at = as.POSIXct("2026-05-18 09:00:00", tz = "UTC"),
    home_team = "A", away_team = "B",
    market = "moneyline", outcome = "home",
    line = NA_real_, odds = 2.50
  )
  wf_root <- withr::local_tempdir()
  write_table(bt_wf_slice_odds(odds), "odds", root = wf_root)
  seen <- prepare_odds(
    list(sport = "football", country = "iceland"), "male",
    end_date = as.Date("2026-05-01"), max_age_hours = bt_wf_max_age_hours(), root = wf_root
  )
  expect_equal(nrow(seen), 1L)
  expect_equal(seen$odds, 2.50)
})

test_that("decide_league forwards max_age_hours so a historical decide can see weeks-old odds (cause-b regression)", {
  # The second stacked cause of the 0-bets bug: prepare_odds drops scrapes older
  # than `now - max_age_hours` with now = Sys.time(). For a historical decide the
  # odds are genuinely weeks old, so the default 48h window empties them. The
  # harness must pass a huge max_age_hours through decide_league -> prepare_odds.
  set.seed(7)
  root <- withr::local_tempdir()
  match_date <- Sys.Date() + 5L
  beliefs <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male",
    fit_date = Sys.Date(),
    match_date = match_date,
    home_team = "Alpha", away_team = "Bravo",
    draw_id = 1:1000,
    home_goals = rpois(1000, 1.6),
    away_goals = rpois(1000, 1.1)
  )
  write_table(beliefs, "beliefs_latest", root = root)
  odds <- tibble::tibble(
    sport = "football", country = "iceland",
    scraped_at = Sys.time() - lubridate::ddays(20),
    match_date = match_date,
    home_team = "Alpha", away_team = "Bravo",
    market = c("moneyline", "moneyline"),
    outcome = c("home", "away"),
    line = NA_real_,
    odds = c(2.40, 3.20)
  )
  write_table(odds, "odds", root = root)

  league <- list(
    sport = "football", country = "iceland", sexes = "male",
    betting = list(
      kelly_frac = list(male = 0.10), ev_threshold = 0.0,
      markets = list(moneyline = TRUE, spread = TRUE, total = TRUE),
      scoring = list(has_ties = TRUE, tie_threshold = 0),
      min_bet = 200, max_age_hours = 48
    )
  )
  bankroll <- list(
    initial_pool = 14909, current_pool = 14909,
    daily_budget_frac = 0.10, daily_budget_min_isk = 1000,
    kelly_ceiling = 0.25
  )

  # Default config window (48h): the 20-day-old scrape is dropped -> no odds ->
  # decide_league early-returns and writes EMPTY candidates.
  decide_league(
    league = league, sex = "male", run_date = Sys.Date(),
    root = root, bankroll = bankroll, write = TRUE
  )
  cands_default <- read_table("candidates",
    filter = list(sport = "football", country = "iceland"), root = root
  )
  expect_equal(nrow(cands_default), 0L)

  # Override: the historical odds survive the age filter -> candidates appear.
  decide_league(
    league = league, sex = "male", run_date = Sys.Date(),
    root = root, bankroll = bankroll, write = TRUE,
    max_age_hours = bt_wf_max_age_hours()
  )
  cands_override <- read_table("candidates",
    filter = list(sport = "football", country = "iceland"), root = root
  )
  expect_gt(nrow(cands_override), 0L)
})

test_that("bt_wf_filter_oos drops candidates on or before the cutoff and beyond the horizon (G2)", {
  d <- as.Date("2026-05-20")
  cands <- tibble::tibble(
    match_date = as.Date(c("2026-05-19", "2026-05-20", "2026-05-21", "2026-06-10")),
    home_team = c("A", "C", "E", "G"),
    away_team = c("B", "D", "F", "H"),
    p = 0.5, odds = 2.0, market = "moneyline", outcome = "home"
  )
  out <- bt_wf_filter_oos(cands, d, horizon_days = 14L)
  expect_equal(out$home_team, "E")
  expect_true(all(out$match_date > d))
  expect_true(all(out$match_date <= d + 14L))
})

test_that("every OOS candidate has match_date strictly after the training cutoff (ASSERT-SAMEDAY-2)", {
  d <- as.Date("2026-05-20")
  cands <- tibble::tibble(
    match_date = as.Date(c("2026-05-20", "2026-05-22")),
    home_team = c("A", "C"), away_team = c("B", "D"),
    p = 0.5, odds = 2.0, market = "moneyline", outcome = "home"
  )
  out <- bt_wf_filter_oos(cands, d, horizon_days = 14L)
  expect_true(all(out$match_date > d))
})

test_that("bt_wf_training_disjoint is TRUE when OOS and training match-sets do not overlap, FALSE otherwise (ASSERT-SAMEDAY-3)", {
  d <- as.Date("2026-05-20")
  results <- tibble::tibble(
    match_date = as.Date(c("2026-05-18", "2026-05-20")),
    home_team = c("A", "C"), away_team = c("B", "D")
  )
  oos_clean <- tibble::tibble(
    match_date = as.Date("2026-05-22"), home_team = "E", away_team = "F"
  )
  oos_leaky <- tibble::tibble(
    match_date = as.Date("2026-05-20"), home_team = "C", away_team = "D"
  )
  expect_true(bt_wf_training_disjoint(oos_clean, results, d))
  expect_false(bt_wf_training_disjoint(oos_leaky, results, d))
})

test_that("bt_wf_ledger_asof keeps only rows whose match settled strictly before d (ASSERT-CALIB-1)", {
  d <- as.Date("2026-05-20")
  led <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male",
    match_date = as.Date(c("2026-05-15", "2026-05-25")),
    home_team = c("A", "C"), away_team = c("B", "D"),
    settled = c(TRUE, TRUE), win = c(TRUE, FALSE), p = c(0.6, 0.4)
  )
  out <- bt_wf_ledger_asof(led, d)
  expect_equal(nrow(out), 1L)
  expect_equal(out$home_team, "A")
})

test_that("bt_wf_ledger_asof drops unsettled rows and a NULL/empty ledger returns empty", {
  d <- as.Date("2026-05-20")
  led <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male",
    match_date = as.Date("2026-05-10"), home_team = "A", away_team = "B",
    settled = FALSE, win = NA, p = 0.5
  )
  expect_equal(nrow(bt_wf_ledger_asof(led, d)), 0L)
  expect_equal(nrow(bt_wf_ledger_asof(led[0, ], d)), 0L)
})

test_that("bt_wf_require_pre_cutoff_odds drops candidates with no pre-cutoff odds snapshot (G8, ASSERT-RESCHEDULE-1)", {
  cands <- tibble::tibble(
    match_date = as.Date(c("2026-05-22", "2026-05-23")),
    home_team = c("A", "C"), away_team = c("B", "D"),
    market = "moneyline", outcome = "home", line = NA_real_,
    p = 0.5, odds = 2.0
  )
  sliced_odds <- tibble::tibble(
    match_date = as.Date("2026-05-22"),
    home_team = "A", away_team = "B",
    market = "moneyline", outcome = "home", line = NA_real_,
    odds = 2.0, scraped_at = as.POSIXct("2026-05-19 12:00:00", tz = "UTC")
  )
  out <- bt_wf_require_pre_cutoff_odds(cands, sliced_odds)
  expect_equal(out$home_team, "A")
  expect_equal(nrow(out), 1L)
})

test_that("bt_walkforward_cutoff seeds an isolated wf_root and never writes the live root (ASSERT-OUTPUT-ISOLATION-1, G6)", {
  d <- as.Date("2026-05-20")
  live_root <- withr::local_tempdir()
  dir.create(file.path(live_root, "beliefs", "latest"), recursive = TRUE)
  sentinel <- file.path(live_root, "beliefs", "latest", "SENTINEL")
  writeLines("untouched", sentinel)

  results <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male", season = 2026L,
    match_date = as.Date(c("2026-05-18", "2026-05-22")),
    home_team = c("A", "A"), away_team = c("B", "B"),
    home_score = c(1L, 2L), away_score = c(0L, 1L),
    division = "BD", round = c(1L, 2L)
  )
  odds <- tibble::tibble(
    sport = "football", country = "iceland",
    scraped_at = as.POSIXct(c("2026-05-19 12:00:00", "2026-05-23 12:00:00"), tz = "UTC"),
    match_date = as.Date("2026-05-22"),
    home_team = "A", away_team = "B",
    market = "moneyline", outcome = "home", line = NA_real_, odds = c(2.50, 1.50)
  )

  fake_decide <- function(root, run_date, ...) {
    tibble::tibble(
      match_date = as.Date("2026-05-22"), home_team = "A", away_team = "B",
      market = "moneyline", outcome = "home", line = NA_real_,
      p = 0.7, odds = 2.50, ev = 0.1, kelly_raw = 0.1
    )
  }

  scored <- bt_walkforward_cutoff(
    sex = "male", d = d, horizon_days = 14L,
    results = results, odds = odds, ledger = NULL,
    live_root = live_root, decide_fn = fake_decide, tie_threshold = 0
  )

  expect_equal(readLines(sentinel), "untouched")
  expect_equal(nrow(scored), 1L)
  expect_equal(scored$odds, 2.50)
  expect_true(scored$win)
  expect_equal(scored$cutoff, d)
  expect_true(all(scored$match_date > d))
})

test_that("bt_walkforward_cutoff drops a match settled on the cutoff day (same-day leak end-to-end, ASSERT-SAMEDAY-1)", {
  d <- as.Date("2026-05-20")
  live_root <- withr::local_tempdir()
  results <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male", season = 2026L,
    match_date = as.Date("2026-05-20"),
    home_team = "A", away_team = "B",
    home_score = 3L, away_score = 0L, division = "BD", round = 1L
  )
  odds <- tibble::tibble(
    sport = "football", country = "iceland",
    scraped_at = as.POSIXct("2026-05-19 12:00:00", tz = "UTC"),
    match_date = as.Date("2026-05-20"),
    home_team = "A", away_team = "B",
    market = "moneyline", outcome = "home", line = NA_real_, odds = 2.50
  )
  fake_decide <- function(root, run_date, ...) {
    tibble::tibble(
      match_date = as.Date("2026-05-20"), home_team = "A", away_team = "B",
      market = "moneyline", outcome = "home", line = NA_real_,
      p = 0.7, odds = 2.50, ev = 0.1, kelly_raw = 0.1
    )
  }
  scored <- bt_walkforward_cutoff(
    sex = "male", d = d, horizon_days = 14L,
    results = results, odds = odds, ledger = NULL,
    live_root = live_root, decide_fn = fake_decide, tie_threshold = 0
  )
  expect_equal(nrow(scored), 0L)
})

test_that("bt_walkforward binds cutoffs and reports primary OOS scores + secondary PnL", {
  live_root <- withr::local_tempdir()
  results <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male", season = 2026L,
    match_date = as.Date(c("2026-05-22", "2026-05-29")),
    home_team = "A", away_team = "B",
    home_score = c(2L, 0L), away_score = c(1L, 1L),
    division = "BD", round = c(2L, 3L)
  )
  odds <- tibble::tibble(
    sport = "football", country = "iceland",
    scraped_at = as.POSIXct(c("2026-05-19 12:00:00", "2026-05-26 12:00:00"), tz = "UTC"),
    match_date = as.Date(c("2026-05-22", "2026-05-29")),
    home_team = "A", away_team = "B",
    market = "moneyline", outcome = "home", line = NA_real_, odds = 2.0
  )
  fake_decide <- function(root, run_date, sex, ledger_asof = NULL) {
    md <- if (run_date < as.Date("2026-05-25")) as.Date("2026-05-22") else as.Date("2026-05-29")
    tibble::tibble(
      match_date = md, home_team = "A", away_team = "B",
      market = "moneyline", outcome = "home", line = NA_real_,
      p = 0.6, odds = 2.0, ev = 0.1, kelly_raw = 0.1
    )
  }
  wf <- bt_walkforward(
    sex = "male", cutoffs = as.Date(c("2026-05-20", "2026-05-27")),
    horizon_days = 14L, results = results, odds = odds, ledger = NULL,
    live_root = live_root, decide_fn = fake_decide, tie_threshold = 0
  )
  expect_equal(wf$scores$n, 2L)
  expect_true(is.finite(wf$scores$brier))
  expect_equal(nrow(wf$bets), 2L)
})

test_that("bt_walkforward caps each cutoff's window at the next cutoff so a match is scored once, under its freshest fit", {
  # Per-round cutoffs sit closer than horizon_days, so two consecutive 14-day
  # windows BOTH cover the 2026-05-20 match -> a fixed horizon double-counts it.
  # Capping cutoff d1's window at d2 leaves the match only in d2's (freshest) one.
  live_root <- withr::local_tempdir()
  results <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male", season = 2026L,
    match_date = as.Date("2026-05-20"),
    home_team = "A", away_team = "B",
    home_score = 2L, away_score = 0L, division = "BD", round = 3L
  )
  odds <- tibble::tibble(
    sport = "football", country = "iceland",
    scraped_at = as.POSIXct("2026-05-19 09:00:00", tz = "UTC"),
    match_date = as.Date("2026-05-20"),
    home_team = "A", away_team = "B",
    market = "moneyline", outcome = "home", line = NA_real_, odds = 1.8
  )
  fake_decide <- function(root, run_date, sex, ledger_asof = NULL) {
    tibble::tibble(
      match_date = as.Date("2026-05-20"), home_team = "A", away_team = "B",
      market = "moneyline", outcome = "home", line = NA_real_,
      p = 0.6, odds = 1.8, ev = 0.08, kelly_raw = 0.1
    )
  }
  # d1 = 05-10, d2 = 05-17 (7 days apart); both fixed-14d windows cover 05-20.
  wf <- bt_walkforward(
    sex = "male", cutoffs = as.Date(c("2026-05-10", "2026-05-17")),
    horizon_days = 14L, results = results, odds = odds, ledger = NULL,
    live_root = live_root, decide_fn = fake_decide, tie_threshold = 0
  )
  expect_equal(wf$scores$n, 1L)
  expect_equal(nrow(wf$bets), 1L)
  expect_equal(wf$bets$cutoff, as.Date("2026-05-17"))
})

test_that("bt_walkforward's LAST cutoff keeps the full horizon (no next cutoff to cap against)", {
  live_root <- withr::local_tempdir()
  results <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male", season = 2026L,
    match_date = as.Date("2026-05-29"),
    home_team = "A", away_team = "B",
    home_score = 2L, away_score = 0L, division = "BD", round = 4L
  )
  odds <- tibble::tibble(
    sport = "football", country = "iceland",
    scraped_at = as.POSIXct("2026-05-28 09:00:00", tz = "UTC"),
    match_date = as.Date("2026-05-29"),
    home_team = "A", away_team = "B",
    market = "moneyline", outcome = "home", line = NA_real_, odds = 1.8
  )
  fake_decide <- function(root, run_date, sex, ledger_asof = NULL) {
    tibble::tibble(
      match_date = as.Date("2026-05-29"), home_team = "A", away_team = "B",
      market = "moneyline", outcome = "home", line = NA_real_,
      p = 0.6, odds = 1.8, ev = 0.08, kelly_raw = 0.1
    )
  }
  # Single (last) cutoff 05-17; the 05-29 match (12 days out) needs the full 14d.
  wf <- bt_walkforward(
    sex = "male", cutoffs = as.Date("2026-05-17"),
    horizon_days = 14L, results = results, odds = odds, ledger = NULL,
    live_root = live_root, decide_fn = fake_decide, tie_threshold = 0
  )
  expect_equal(wf$scores$n, 1L)
  expect_equal(wf$bets$cutoff, as.Date("2026-05-17"))
})

test_that("bt_walkforward tolerates a per-cutoff fit failure: records it in $skipped and continues", {
  live_root <- withr::local_tempdir()
  results <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male", season = 2026L,
    match_date = as.Date("2026-05-22"),
    home_team = "A", away_team = "B",
    home_score = 2L, away_score = 1L, division = "BD", round = 2L
  )
  odds <- tibble::tibble(
    sport = "football", country = "iceland",
    scraped_at = as.POSIXct("2026-05-19 12:00:00", tz = "UTC"),
    match_date = as.Date("2026-05-22"),
    home_team = "A", away_team = "B",
    market = "moneyline", outcome = "home", line = NA_real_, odds = 2.0
  )
  flaky_decide <- function(root, run_date, sex, ledger_asof = NULL) {
    if (run_date >= as.Date("2026-05-25")) {
      stop("Stan diagnostic gate: 51 divergent transitions > 1.00% threshold", call. = FALSE)
    }
    tibble::tibble(
      match_date = as.Date("2026-05-22"), home_team = "A", away_team = "B",
      market = "moneyline", outcome = "home", line = NA_real_,
      p = 0.6, odds = 2.0, ev = 0.1, kelly_raw = 0.1
    )
  }
  wf <- bt_walkforward(
    sex = "male", cutoffs = as.Date(c("2026-05-20", "2026-05-27")),
    horizon_days = 14L, results = results, odds = odds, ledger = NULL,
    live_root = live_root, decide_fn = flaky_decide, tie_threshold = 0
  )
  expect_equal(nrow(wf$bets), 1L)
  expect_equal(nrow(wf$skipped), 1L)
  expect_equal(wf$skipped$cutoff, as.Date("2026-05-27"))
  expect_match(wf$skipped$error, "divergent")
})

test_that("bt_walkforward scores ALL candidates for calibration but PnL only over kept bets", {
  # The primary OOS calibration verdict scores every bettable candidate the
  # decider produced (kept + dropped-but-priced); the secondary PnL arm scores
  # only the bets that would actually be placed (stage == "kept").
  live_root <- withr::local_tempdir()
  results <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male", season = 2026L,
    match_date = as.Date("2026-05-22"),
    home_team = "A", away_team = "B",
    home_score = 2L, away_score = 0L, division = "BD", round = 2L
  )
  odds <- tibble::tibble(
    sport = "football", country = "iceland",
    scraped_at = as.POSIXct("2026-05-21 09:00:00", tz = "UTC"),
    match_date = as.Date("2026-05-22"),
    home_team = "A", away_team = "B",
    market = c("moneyline", "moneyline", "total"),
    outcome = c("home", "away", "over"),
    line = c(NA_real_, NA_real_, 2.5),
    odds = c(1.80, 4.5, 2.0)
  )
  # Real decide returns the FULL candidates tibble (all stages) when the harness
  # asks for it; only stage == "kept" would be placed. Two of these three are
  # dropped by staking filters but still carry a model p -> scored for calibration.
  fake_decide <- function(root, run_date, sex, ledger_asof = NULL) {
    tibble::tibble(
      match_date = as.Date("2026-05-22"), home_team = "A", away_team = "B",
      market = c("moneyline", "moneyline", "total"),
      outcome = c("home", "away", "over"),
      line = c(NA_real_, NA_real_, 2.5),
      p = c(0.65, 0.12, 0.55), odds = c(1.80, 4.5, 2.0),
      ev = c(0.17, -0.46, 0.10), kelly_raw = c(0.2, 0, 0.05),
      stage = c("kept", "dropped_low_ev", "dropped_min_bet")
    )
  }
  wf <- bt_walkforward(
    sex = "male", cutoffs = as.Date("2026-05-10"), horizon_days = 14L,
    results = results, odds = odds, ledger = NULL,
    live_root = live_root, decide_fn = fake_decide, tie_threshold = 0
  )
  # Calibration over all 3 settled candidates.
  expect_equal(wf$scores$n, 3L)
  # PnL only over the single kept bet.
  expect_equal(wf$pnl$n_bets, 1L)
  expect_equal(sum(wf$bets$kept), 1L)
  # The kept bet is moneyline-home @1.80; A won 2-0 -> a win.
  kept_row <- wf$bets[wf$bets$kept, , drop = FALSE]
  expect_equal(kept_row$market, "moneyline")
  expect_equal(kept_row$outcome, "home")
  expect_true(kept_row$win)
})

test_that("walk-forward yields OOS candidates and scores >= 1 bet with a real as-of fit (integration; opt-in)", {
  # Every test above injects a fake decide_fn, so none exercised the real
  # fit_league -> decide_league path the slice rule + max_age_hours forwarding
  # actually fix -- which is exactly why the 0-bets bug shipped green. This runs
  # ONE real reduced-iter as-of fit against the live football_iceland store.
  # Opt-in (Stan fit, ~minutes) and never on CI; the CI-isolation test forbids
  # the harness symbols in any workflow regardless.
  skip_on_cran()
  skip_on_ci()
  skip_if_not(
    nzchar(Sys.getenv("SPORTS_RUN_WF_INTEGRATION")),
    "set SPORTS_RUN_WF_INTEGRATION=1 to run the Stan-backed walk-forward integration test"
  )
  skip_if_not_installed("cmdstanr")

  withr::local_envvar(
    SPORTS_FIT_ITER_WARMUP = "400",
    SPORTS_FIT_ITER_SAMPLING = "400"
  )

  league <- load_leagues()[["football_iceland"]]
  results <- read_table("results",
    filter = list(sport = "football", country = "iceland", sex = "male")
  )
  odds <- read_table("odds",
    filter = list(sport = "football", country = "iceland")
  )
  skip_if(
    nrow(results) == 0L || nrow(odds) == 0L,
    "no football_iceland results/odds in this checkout"
  )

  wf <- bt_walkforward(
    sex = "male", cutoffs = as.Date("2026-05-03"), horizon_days = 14L,
    results = results, odds = odds, ledger = NULL,
    tie_threshold = league$betting$scoring$tie_threshold %||% 0
  )

  # A reduced-iter fit can trip the Stan divergence gate -- a tolerated skip, not
  # the regression we guard. Only assert the OOS pipeline when the fit survived.
  if (nrow(wf$skipped) > 0L) {
    skip(paste("reduced-iter fit tripped the Stan gate:", wf$skipped$error[1]))
  }
  # Calibration scores ALL bettable OOS candidates -> many per cutoff. A regress
  # to placed-bets-only (the original n=1 bug) would fail this floor.
  expect_gt(nrow(wf$bets), 5L)
  expect_gt(wf$scores$n, 5L)
  # PnL is the placed subset of the scored candidates.
  if (!is.null(wf$pnl$n_bets)) expect_lte(wf$pnl$n_bets, wf$scores$n)
})
