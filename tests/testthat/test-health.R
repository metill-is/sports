seed_health_pending_rec <- function(root) {
  recs <- tibble::tibble(
    run_id = as.POSIXct("2026-06-01 10:00:00", tz = "UTC"),
    sex = "male",
    match_date = as.Date("2100-01-01"),
    home_team = "A", away_team = "B",
    market = "moneyline", outcome = "home", line = NA_real_,
    p = 0.6, odds = 2.1, ev = 0.26, kelly = 0.02, bet_amount = 1500,
    sport = "football", country = "iceland"
  )
  write_table(recs, "recommendations", root = root)
}

.mini_ledger <- function(match_date, settled = FALSE, pnl = 0) {
  tibble::tibble(
    placed_at = as.POSIXct("2026-05-01", tz = "UTC"),
    match_date = as.Date(match_date),
    sport = "football", country = "iceland", sex = "male",
    home_team = "A", away_team = "B",
    market = "moneyline", outcome = "home", line = NA_real_,
    odds_placed = 2.0, p = 0.55, kelly = 0.01, bet_amount = 200,
    settled = settled, win = if (settled) TRUE else NA, pnl = pnl
  )
}

test_that("check_orphaned_bets flags an old unsettled bet", {
  root <- withr::local_tempdir()
  write_table(.mini_ledger("2026-04-01", settled = FALSE), "ledger", root = root)
  res <- check_orphaned_bets(root, as.POSIXct("2026-05-30", tz = "UTC"), health_thresholds())
  expect_equal(res$status, "WARN")
})

test_that("check_orphaned_bets is OK with a recent unsettled bet", {
  root <- withr::local_tempdir()
  write_table(.mini_ledger("2026-05-29", settled = FALSE), "ledger", root = root)
  res <- check_orphaned_bets(root, as.POSIXct("2026-05-30", tz = "UTC"), health_thresholds())
  expect_equal(res$status, "OK")
})

test_that("pipeline_health returns the canonical health tibble shape", {
  root <- withr::local_tempdir()
  out <- pipeline_health(root = root, now = as.POSIXct("2026-05-30", tz = "UTC"), leagues = list())
  expect_true(all(c("check", "scope", "status", "value", "threshold") %in% names(out)))
  expect_true(all(out$status %in% c("OK", "WARN", "FAIL", "PAUSED")))
})

test_that("pipeline_health does not mutate the ledger (read-only money path)", {
  root <- withr::local_tempdir()
  write_table(.mini_ledger("2026-05-20", settled = TRUE, pnl = 100), "ledger", root = root)
  led_dir <- file.path(root, "decisions", "ledger")
  before <- tools::md5sum(list.files(led_dir, recursive = TRUE, full.names = TRUE, pattern = "parquet$"))
  pipeline_health(root = root, now = as.POSIXct("2026-05-30", tz = "UTC"), leagues = list())
  after <- tools::md5sum(list.files(led_dir, recursive = TRUE, full.names = TRUE, pattern = "parquet$"))
  expect_identical(before, after)
})

test_that("fit_freshness is PAUSED when a cell has no upcoming games", {
  root <- withr::local_tempdir()
  leagues <- list(
    football_iceland = list(sport = "football", country = "iceland", sexes = list("male"))
  )
  out <- pipeline_health(root = root, now = as.POSIXct("2026-05-30", tz = "UTC"), leagues = leagues)
  ff <- out[out$check == "fit_freshness", ]
  expect_true(any(ff$status == "PAUSED"))
})

test_that("write_health_status writes a readable status json with overall", {
  path <- withr::local_tempfile(fileext = ".json")
  h <- tibble::tibble(
    check = c("a", "b"), scope = c("x", "y"), status = c("OK", "FAIL"),
    value = c("1", "2"), threshold = c("t", "t")
  )
  write_health_status(h, path, now = as.POSIXct("2026-05-30 12:00", tz = "UTC"))
  p <- jsonlite::read_json(path)
  expect_equal(p$overall, "FAIL")
  expect_equal(p$n_fail, 1L)
  expect_equal(length(p$checks), 2L)
  expect_equal(p$checks[[2]]$status, "FAIL")
})

test_that("overall_health_status escalates to the worst row", {
  expect_equal(overall_health_status(tibble::tibble(status = c("OK", "WARN", "OK"))), "WARN")
  expect_equal(overall_health_status(tibble::tibble(status = c("OK", "WARN", "FAIL"))), "FAIL")
  expect_equal(overall_health_status(tibble::tibble(status = c("OK", "PAUSED"))), "OK")
})

.mini_recs <- function(n, match_date = "2026-05-22", run_date = "2026-05-20") {
  tibble::tibble(
    run_id = as.POSIXct(run_date, tz = "UTC"), run_date = as.Date(run_date),
    sport = "football", country = "iceland", sex = "male",
    match_date = as.Date(match_date),
    home_team = paste0("H", seq_len(n)), away_team = paste0("A", seq_len(n)),
    market = "moneyline", outcome = "home", line = NA_real_,
    p = 0.55, odds = 2.0, ev = 0.1, kelly = 0.02, bet_amount = 250
  )
}

test_that("check_capture_rate WARNs when few recommendations were placed", {
  root <- withr::local_tempdir()
  recs <- .mini_recs(25L)
  write_table(recs, "recommendations", root = root)
  placed <- recs[1:10, ] # 10/25 = 40% -> WARN (below 70%, above 30%)
  led <- tibble::tibble(
    placed_at = as.POSIXct("2026-05-21", tz = "UTC"),
    match_date = placed$match_date, sport = "football", country = "iceland",
    sex = "male", home_team = placed$home_team, away_team = placed$away_team,
    market = "moneyline", outcome = "home", line = NA_real_,
    odds_placed = 2.0, p = 0.55, kelly = 0.02, bet_amount = 250,
    settled = TRUE, win = TRUE, pnl = 250
  )
  write_table(led, "ledger", root = root)
  res <- check_capture_rate(root, as.POSIXct("2026-05-30", tz = "UTC"), health_thresholds())
  expect_equal(res$status, "WARN")
  expect_match(res$value, "10/25")
})

.mini_sched <- function(match_date, sex = "male", home = "A", away = "B") {
  tibble::tibble(
    sport = "football", country = "iceland", sex = sex, season = 2026L,
    match_date = as.Date(match_date), home_team = home, away_team = away,
    division = "besta", round = 1L, kickoff_time = NA_character_
  )
}

.mini_odds <- function(match_date, scraped_at) {
  tibble::tibble(
    sport = "football", country = "iceland",
    scraped_at = as.POSIXct(scraped_at, tz = "UTC"),
    match_date = as.Date(match_date), home_team = "A", away_team = "B",
    market = "moneyline", outcome = "home", line = NA_real_, odds = 2.0
  )
}

.mini_candidate <- function(match_date, sex = "male", home = "A", away = "B") {
  tibble::tibble(
    run_id = as.POSIXct("2026-05-19 10:00:00", tz = "UTC"),
    sport = "football", country = "iceland", sex = sex,
    match_date = as.Date(match_date), home_team = home, away_team = away,
    market = "moneyline", outcome = "home", line = NA_real_,
    p = 0.55, odds = 2.0, ev = 0.1, kelly_raw = 0.02, stage = "kept"
  )
}

.fb_league <- list(
  football_iceland = list(sport = "football", country = "iceland", sexes = list("male"))
)

test_that("odds_freshness is OK during a between-match lull (next match beyond the lead window)", {
  root <- withr::local_tempdir()
  now <- as.POSIXct("2026-06-03 12:00", tz = "UTC")
  # Next match is 5 days out; only stale odds for already-played matches exist.
  write_table(.mini_sched("2026-06-08"), "schedules", root = root)
  write_table(.mini_odds("2026-06-01", "2026-06-01 13:00"), "odds", root = root)
  res <- check_odds_freshness(.fb_league, root, now, health_thresholds())
  expect_equal(res$status, "OK")
})

test_that("odds_freshness is OK when odds cover an upcoming match, even if scraped days ago", {
  root <- withr::local_tempdir()
  now <- as.POSIXct("2026-06-03 12:00", tz = "UTC")
  write_table(.mini_sched("2026-06-05"), "schedules", root = root)
  # Odds for the upcoming 06-05 match, scraped two days ago.
  write_table(.mini_odds("2026-06-05", "2026-06-01 13:00"), "odds", root = root)
  res <- check_odds_freshness(.fb_league, root, now, health_thresholds())
  expect_equal(res$status, "OK")
})

test_that("odds_freshness WARNs when a match is imminent but no upcoming odds are scraped", {
  root <- withr::local_tempdir()
  now <- as.POSIXct("2026-06-03 12:00", tz = "UTC")
  # Match tomorrow, but only stale odds for past matches.
  write_table(.mini_sched("2026-06-04"), "schedules", root = root)
  write_table(.mini_odds("2026-06-01", "2026-06-01 13:00"), "odds", root = root)
  res <- check_odds_freshness(.fb_league, root, now, health_thresholds())
  expect_equal(res$status, "WARN")
})

test_that("odds_freshness FAILs when a match is today and no upcoming odds exist", {
  root <- withr::local_tempdir()
  now <- as.POSIXct("2026-06-03 12:00", tz = "UTC")
  write_table(.mini_sched("2026-06-03"), "schedules", root = root)
  write_table(.mini_odds("2026-06-01", "2026-06-01 13:00"), "odds", root = root)
  res <- check_odds_freshness(.fb_league, root, now, health_thresholds())
  expect_equal(res$status, "FAIL")
  expect_match(res$value, "today")
})

test_that("odds_freshness produces no row off-season (no upcoming games)", {
  root <- withr::local_tempdir()
  now <- as.POSIXct("2026-06-03 12:00", tz = "UTC")
  # Only past fixtures on the schedule -> not active.
  write_table(.mini_sched("2026-05-01"), "schedules", root = root)
  write_table(.mini_odds("2026-05-01", "2026-05-01 13:00"), "odds", root = root)
  res <- check_odds_freshness(.fb_league, root, now, health_thresholds())
  expect_equal(nrow(res), 0L)
})

# --- division-coverage scoping (2026-06-09 LD4 false-FAIL) --------------------
#
# KSI schedules span divisions Lengjan never prices. The lone fixture on
# 2026-06-09 was 4. deild (Alftanes v Hafnir); odds_freshness FAILed all day
# and the Health Check workflow alert-emailed twice for a non-event. Odds are
# only *expected* in (sex, division) cells with candidate history.

test_that("odds_freshness ignores a today-fixture in a division without candidate history", {
  root <- withr::local_tempdir()
  now <- as.POSIXct("2026-06-03 12:00", tz = "UTC")
  # Candidate history in 'besta' only; today's sole fixture is '4deild'.
  write_table(
    dplyr::bind_rows(
      .mini_sched("2026-05-20"),
      .mini_sched("2026-06-03", home = "C", away = "D") |>
        dplyr::mutate(division = "4deild")
    ),
    "schedules",
    root = root
  )
  write_table(.mini_candidate("2026-05-20"), "candidates", root = root)
  write_table(.mini_odds("2026-05-20", "2026-05-18 13:00"), "odds", root = root)
  res <- check_odds_freshness(.fb_league, root, now, health_thresholds())
  expect_equal(res$status, "OK")
  expect_match(res$value, "never priced")
})

test_that("odds_freshness still FAILs for a today-fixture in a division with candidate history", {
  root <- withr::local_tempdir()
  now <- as.POSIXct("2026-06-03 12:00", tz = "UTC")
  write_table(
    dplyr::bind_rows(
      .mini_sched("2026-05-20"),
      .mini_sched("2026-06-03", home = "C", away = "D")
    ),
    "schedules",
    root = root
  )
  write_table(.mini_candidate("2026-05-20"), "candidates", root = root)
  write_table(.mini_odds("2026-05-20", "2026-05-18 13:00"), "odds", root = root)
  res <- check_odds_freshness(.fb_league, root, now, health_thresholds())
  expect_equal(res$status, "FAIL")
  expect_match(res$value, "today")
})

test_that("odds_freshness coverage is per sex, not per division name", {
  root <- withr::local_tempdir()
  now <- as.POSIXct("2026-06-03 12:00", tz = "UTC")
  leagues <- list(
    football_iceland = list(
      sport = "football", country = "iceland", sexes = list("male", "female")
    )
  )
  # Male 'besta' has candidate history; the only upcoming fixture is FEMALE
  # 'besta' -- same division string, different cell, never priced.
  write_table(
    dplyr::bind_rows(
      .mini_sched("2026-05-20"),
      .mini_sched("2026-06-03", sex = "female", home = "C", away = "D")
    ),
    "schedules",
    root = root
  )
  write_table(.mini_candidate("2026-05-20"), "candidates", root = root)
  write_table(.mini_odds("2026-05-20", "2026-05-18 13:00"), "odds", root = root)
  res <- check_odds_freshness(leagues, root, now, health_thresholds())
  expect_equal(res$status, "OK")
  expect_match(res$value, "never priced")
})

test_that("odds_freshness falls back to expecting odds everywhere with no candidate history", {
  root <- withr::local_tempdir()
  now <- as.POSIXct("2026-06-03 12:00", tz = "UTC")
  # Cold start: schedule but no candidates at all -> conservative FAIL.
  write_table(
    .mini_sched("2026-06-03") |> dplyr::mutate(division = "4deild"),
    "schedules",
    root = root
  )
  res <- check_odds_freshness(.fb_league, root, now, health_thresholds())
  expect_equal(res$status, "FAIL")
})

# Seed the on-disk state check_fit_freshness reads: a completed result, a
# beliefs/latest partition carrying fit_date (latest_fit_date + needs_refit's
# wiped-latest guard), and an archive fit_date= partition (needs_refit's
# last_fit source). result_date <= fit_date => fit current; result_date >
# fit_date => fit behind the data.
.seed_fit <- function(root, fit_date, result_date, sex = "male") {
  rd <- fs::path(
    root, "facts", "results",
    "sport=football", "country=iceland", paste0("sex=", sex), "season=2026"
  )
  fs::dir_create(rd)
  arrow::write_parquet(
    tibble::tibble(
      home_team = "A", away_team = "B",
      match_date = as.Date(result_date), home_score = 1L, away_score = 0L,
      division = NA_character_, round = NA_integer_
    ),
    fs::path(rd, "part-0.parquet")
  )
  write_table(
    tibble::tibble(
      sport = "football", country = "iceland", sex = sex,
      fit_date = as.Date(fit_date), match_date = as.Date(fit_date),
      home_team = "A", away_team = "B", draw_id = 1L,
      home_goals = 1, away_goals = 0
    ),
    "beliefs_latest",
    root = root
  )
  arch <- fs::path(
    root, "beliefs", "archive",
    "sport=football", "country=iceland", paste0("sex=", sex),
    paste0("fit_date=", fit_date)
  )
  fs::dir_create(arch)
  fs::file_create(fs::path(arch, "beliefs.parquet"))
}

test_that("fit_freshness is OK during an inter-match lull (fit current with results)", {
  root <- withr::local_tempdir()
  now <- as.POSIXct("2026-06-04 12:00", tz = "UTC")
  # Upcoming fixture five days out (has_upcoming_games uses the real clock, so
  # this must be relative, not a fixed near date).
  write_table(.mini_sched(Sys.Date() + 5L), "schedules", root = root)
  # Fit is five days old, but no result postdates it: nothing to refit.
  .seed_fit(root, fit_date = "2026-05-30", result_date = "2026-05-30")
  res <- check_fit_freshness(.fb_league, root, now, health_thresholds())
  expect_equal(res$status, "OK")
})

test_that("fit_freshness FAILs when results have moved past an old fit", {
  root <- withr::local_tempdir()
  now <- as.POSIXct("2026-06-04 12:00", tz = "UTC")
  write_table(.mini_sched(Sys.Date() + 5L), "schedules", root = root)
  # A completed result on 06-02 postdates the 05-30 fit: genuinely behind, and
  # the fit is 5 days old (> fail threshold).
  .seed_fit(root, fit_date = "2026-05-30", result_date = "2026-06-02")
  res <- check_fit_freshness(.fb_league, root, now, health_thresholds())
  expect_equal(res$status, "FAIL")
})

test_that("fit_freshness WARNs when behind but only mildly stale", {
  root <- withr::local_tempdir()
  now <- as.POSIXct("2026-06-04 12:00", tz = "UTC")
  write_table(.mini_sched(Sys.Date() + 5L), "schedules", root = root)
  # Behind (result 06-02 > fit 06-01) but the fit is only 3 days old (warn band).
  .seed_fit(root, fit_date = "2026-06-01", result_date = "2026-06-02")
  res <- check_fit_freshness(.fb_league, root, now, health_thresholds())
  expect_equal(res$status, "WARN")
})

test_that("check_capture_rate is OK at high capture and OK on thin data", {
  root <- withr::local_tempdir()
  recs <- .mini_recs(25L)
  write_table(recs, "recommendations", root = root)
  placed <- recs[1:23, ] # 23/25 = 92% -> OK
  led <- tibble::tibble(
    placed_at = as.POSIXct("2026-05-21", tz = "UTC"),
    match_date = placed$match_date, sport = "football", country = "iceland",
    sex = "male", home_team = placed$home_team, away_team = placed$away_team,
    market = "moneyline", outcome = "home", line = NA_real_,
    odds_placed = 2.0, p = 0.55, kelly = 0.02, bet_amount = 250,
    settled = TRUE, win = TRUE, pnl = 250
  )
  write_table(led, "ledger", root = root)
  expect_equal(
    check_capture_rate(root, as.POSIXct("2026-05-30", tz = "UTC"), health_thresholds())$status,
    "OK"
  )

  # Thin data (< capture_min_n recs in window) never escalates past OK.
  root2 <- withr::local_tempdir()
  write_table(.mini_recs(5L), "recommendations", root = root2)
  expect_equal(
    check_capture_rate(root2, as.POSIXct("2026-05-30", tz = "UTC"), health_thresholds())$status,
    "OK"
  )
})

test_that("check_placement_health is OK when nothing is pending", {
  root <- withr::local_tempdir()
  th <- health_thresholds()
  row <- check_placement_health(root, Sys.time(), th)
  expect_equal(row$status, "OK")
})

test_that("check_placement_health WARNs on a failed last run even with nothing pending", {
  # The 2026-06-13 silent freeze: the agent's sync died for 3 days but the
  # check read OK because the queue was empty. A broken agent must be visible
  # even at zero pending (WARN, not FAIL -- empty queue means no money lost yet).
  root <- withr::local_tempdir()
  th <- health_thresholds()
  record_placement_status("sync_failed", n_pending = 0L, run_at = Sys.time(), root = root)
  row <- check_placement_health(root, Sys.time(), th)
  expect_equal(row$status, "WARN")
})

test_that("check_placement_health FAILs on a failed last run with pending bets", {
  root <- withr::local_tempdir()
  th <- health_thresholds()
  seed_health_pending_rec(root)
  record_placement_status("failed:boom",
    n_pending = 1L,
    run_at = Sys.time(), root = root
  )
  row <- check_placement_health(root, Sys.time(), th)
  expect_equal(row$status, "FAIL")
})

test_that("check_placement_health is OK after a healthy run cleared the queue", {
  root <- withr::local_tempdir()
  th <- health_thresholds()
  record_placement_status("placed", n_placed = 2L, run_at = Sys.time(), root = root)
  row <- check_placement_health(root, Sys.time(), th)
  expect_equal(row$status, "OK")
})

test_that("check_placement_health WARNs when last healthy run is stale", {
  root <- withr::local_tempdir()
  th <- health_thresholds()
  seed_health_pending_rec(root)
  record_placement_status("placed",
    run_at = Sys.time() - 3600 * (th$placement_stale_warn_hours + 1), root = root
  )
  expect_equal(check_placement_health(root, Sys.time(), th)$status, "WARN")
})

test_that("check_placement_health FAILs when last healthy run is very stale", {
  root <- withr::local_tempdir()
  th <- health_thresholds()
  seed_health_pending_rec(root)
  record_placement_status("placed",
    run_at = Sys.time() - 3600 * (th$placement_stale_fail_hours + 1), root = root
  )
  expect_equal(check_placement_health(root, Sys.time(), th)$status, "FAIL")
})

# ---- WS12 T5: the publish and season checks are composed in ----------------
#
# Nine checks were composed before this and NOT ONE read data/publish/, which
# is how basketball and handball stayed dark for months behind a green
# pipeline.
#
# The plan claimed the proof was an overall OK -> FAIL delta on this fixture.
# MEASURED, and it is not: pre-composition this tempdir already returns FAIL,
# because fit_freshness fires on a root with no fit at all. The load-bearing
# assertion is therefore the publish_freshness row EXISTING and reporting FAIL
# with a per-cell scope -- overall is asserted only as a shape check.

.bb_health_league <- function() {
  list(basketball_iceland = list(
    sport = "basketball", country = "iceland", sexes = list("male"),
    active = TRUE,
    data_source = list(results = "kki_basketball"),
    publish_divisions = list(male = list(
      list(code = "BD", slug = "bd", label_is = "Bonusdeild", is_cup = FALSE)
    ))
  ))
}

test_that("pipeline_health composes the publish and season checks", {
  root <- withr::local_tempdir()
  # In-season: has_upcoming_games() reads Sys.Date() internally and ignores the
  # injected clock, so the fixture must use a real future date.
  write_table(
    tibble::tibble(
      sport = "basketball", country = "iceland", sex = "male", season = 2027L,
      match_date = Sys.Date() + 3L, home_team = "A", away_team = "B",
      division = "BD", round = 1L, kickoff_time = NA_character_
    ),
    "schedules",
    root = root
  )

  out <- suppressWarnings(pipeline_health(
    root = root, now = Sys.time(), leagues = .bb_health_league()
  ))

  expect_true(any(out$check == "publish_freshness"))
  expect_true(any(out$check == "season_resolution"))
  # publish_format emits no row here (nothing published to compare), which is
  # correct -- the composition is proved by the check running without erroring
  # into a check_error row rather than by it manufacturing one.
  expect_false(any(out$check == "check_error"))
  expect_equal(overall_health_status(out), "FAIL")
  expect_equal(
    out$status[out$check == "publish_freshness"], "FAIL"
  )
  # The existing shape invariant still holds.
  expect_true(all(out$status %in% c("OK", "WARN", "FAIL", "PAUSED")))
})

test_that("pipeline_health emits a publish_format row when there is one to emit", {
  root <- withr::local_tempdir()
  write_table(
    tibble::tibble(
      sport = "basketball", country = "iceland", sex = "male", season = 2027L,
      match_date = Sys.Date() + 3L, home_team = "A", away_team = "B",
      division = "BD", round = 1L, kickoff_time = NA_character_
    ),
    "schedules",
    root = root
  )
  cell <- .publish_cell_dir(root, "basketball", "male", "bd")
  dir.create(cell, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(
    list(
      sport = "basketball", sex = "male", division = "BD", is_cup = FALSE,
      generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
      n_rounds = 6L, n_rounds_source = "schedule"
    ),
    file.path(cell, "meta.json"),
    auto_unbox = TRUE
  )
  jsonlite::write_json(
    list(season = 2027L, rows = list(
      list(team = "A"), list(team = "B"), list(team = "C"), list(team = "D")
    )),
    file.path(cell, "standings.json"),
    auto_unbox = TRUE
  )
  leagues <- .bb_health_league()
  leagues$basketball_iceland$publish_divisions$male[[1]]$expected_meetings <- 2L

  out <- suppressWarnings(pipeline_health(
    root = root, now = Sys.time(), leagues = leagues
  ))
  expect_true(any(out$check == "publish_format"))
  expect_false(any(out$check == "check_error"))
})
