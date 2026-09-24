setup_placer_root <- function() {
  tmp <- withr::local_tempdir(.local_envir = parent.frame())
  recs <- arrow::read_parquet(testthat::test_path(
    "fixtures", "placer",
    "recs_sample.parquet"
  ))
  led <- arrow::read_parquet(testthat::test_path(
    "fixtures", "placer",
    "ledger_sample.parquet"
  ))
  write_table(recs, "recommendations", root = tmp)
  write_table(led, "ledger", root = tmp)
  tmp
}

test_that("load_recommendations returns rows for matching target_date", {
  root <- setup_placer_root()
  out <- load_recommendations(root, target_date = as.Date("2026-04-26"))
  expect_equal(nrow(out), 2L)
  expect_true(all(out$match_date == as.Date("2026-04-26")))
})

test_that("load_recommendations honours league filter", {
  root <- setup_placer_root()
  # Filter-mechanism test: pin an explicit league config so it exercises the
  # filter rather than the shipped betting policy. basketball_iceland is
  # betting-disabled in production (D2), and load_recommendations() drops
  # disabled leagues -- without this pin the assertion would pass for the
  # wrong reason and stop testing the filter at all.
  cfg <- list(basketball_iceland = list(betting = list(kelly_frac = 0.05)))
  out <- load_recommendations(root,
    leagues = "basketball_iceland",
    target_date = as.Date("2026-04-27"),
    leagues_cfg = cfg
  )
  expect_equal(nrow(out), 1L)
  expect_equal(out$sport, "basketball")
})

test_that("dedup_against_ledger drops already-placed bets", {
  root <- setup_placer_root()
  recs <- load_recommendations(root, target_date = as.Date("2026-04-26"))
  out <- dedup_against_ledger(recs, root)
  # KR vs FH moneyline/home is in the ledger; should be removed.
  expect_equal(nrow(out), 1L)
  expect_equal(out$home_team, "Fram")
})

test_that("dedup_against_ledger is a no-op when ledger is empty", {
  tmp <- withr::local_tempdir()
  recs <- arrow::read_parquet(testthat::test_path(
    "fixtures", "placer",
    "recs_sample.parquet"
  ))
  write_table(recs, "recommendations", root = tmp)
  loaded <- load_recommendations(tmp, target_date = as.Date("2026-04-26"))
  out <- dedup_against_ledger(loaded, tmp)
  expect_equal(nrow(out), nrow(loaded))
})

test_that("load_recommendations returns 0 rows + correct cols when nothing matches", {
  root <- setup_placer_root()
  out <- load_recommendations(root, target_date = as.Date("2050-01-01"))
  expect_equal(nrow(out), 0L)
  expect_true(all(c("match_date", "home_team", "market") %in% names(out)))
})

test_that("load_recommendations(run_date = NULL) returns only the most recent partition", {
  tmp <- withr::local_tempdir()

  base <- arrow::read_parquet(testthat::test_path(
    "fixtures", "placer", "recs_sample.parquet"
  ))
  # Older run with the same matches as the fixture
  older <- base
  older$run_id <- as.POSIXct("2026-04-24 09:00:00", tz = "UTC")

  write_table(base, "recommendations", root = tmp)
  write_table(older, "recommendations", root = tmp)

  out <- load_recommendations(tmp, target_date = as.Date("2026-04-26"))
  expect_true(all(out$run_id == as.POSIXct("2026-04-25 09:00:00", tz = "UTC")))
})

# ---- Per-league latest run (final review F2) --------------------------------
#
# With run_date = NULL the loader kept only the GLOBAL max(run_date) before the
# league filter, so a handball paper partition written on D+1 hid football's
# recommendations from D: autoplace saw 0 pending instead of 1. Each league now
# keeps its own most recent run, exactly as when it was the only writer.

.f2_rec <- function(sport, run_id, home) {
  tibble::tibble(
    run_id = as.POSIXct(run_id, tz = "UTC"),
    sport = sport, country = "iceland", sex = "male",
    match_date = as.Date("2100-06-01"),
    home_team = home, away_team = "B",
    market = "moneyline", outcome = "home", line = NA_real_,
    p = 0.6, odds = 2.0, ev = 0.2, kelly = 0.05, bet_amount = 500
  )
}

.f2_root <- function(env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  # Football decided on D-1 and D; handball (paper) decided on D+1.
  write_table(.f2_rec("football", "2026-09-22 10:00:00", "Fram"), "recommendations", root = root)
  write_table(.f2_rec("football", "2026-09-23 10:00:00", "KR"), "recommendations", root = root)
  write_table(.f2_rec("handball", "2026-09-24 10:00:00", "Valur"), "recommendations", root = root)
  root
}

test_that("a later paper-league run does not hide football's latest recommendations", {
  root <- .f2_root()
  cfg <- list(
    football_iceland = list(betting = list(kelly_frac = 0.1)),
    handball_iceland = list(betting = list(mode = "paper"))
  )
  out <- suppressMessages(load_recommendations(root, leagues_cfg = cfg))
  expect_equal(nrow(out), 1L)
  expect_equal(out$sport, "football")
  expect_equal(out$home_team, "KR") # football's own latest run (D), not D-1
})

test_that("with run_date = NULL each league keeps its own most recent run", {
  root <- .f2_root()
  cfg <- list(
    football_iceland = list(betting = list(kelly_frac = 0.1)),
    handball_iceland = list(betting = list(mode = "manual"))
  )
  out <- suppressMessages(load_recommendations(root, leagues_cfg = cfg))
  expect_setequal(paste(out$sport, out$home_team), c("football KR", "handball Valur"))
})

test_that("an explicit run_date still selects exactly that partition", {
  root <- .f2_root()
  cfg <- list(
    football_iceland = list(betting = list(kelly_frac = 0.1)),
    handball_iceland = list(betting = list(mode = "manual"))
  )
  out <- suppressMessages(load_recommendations(
    root, run_date = as.Date("2026-09-22"), leagues_cfg = cfg
  ))
  expect_equal(paste(out$sport, out$home_team), "football Fram")
})
