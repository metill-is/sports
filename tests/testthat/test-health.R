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
