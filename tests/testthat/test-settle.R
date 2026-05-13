make_bet <- function(market, outcome, line = NA_real_,
                     odds = 2.0, bet_amount = 100,
                     home = "A", away = "B",
                     match_date = as.Date("2026-05-01"),
                     sport = "football", country = "iceland", sex = "male") {
  tibble::tibble(
    placed_at   = as.POSIXct("2026-05-01 12:00:00", tz = "UTC"),
    match_date  = match_date,
    sport       = sport,
    country     = country,
    sex         = sex,
    home_team   = home,
    away_team   = away,
    market      = market,
    outcome     = outcome,
    line        = line,
    odds_placed = odds,
    p           = 0.5,
    kelly       = 0.05,
    bet_amount  = bet_amount,
    settled     = FALSE,
    win         = NA,
    pnl         = NA_real_
  )
}

make_result <- function(home_score, away_score,
                        home = "A", away = "B",
                        match_date = as.Date("2026-05-01"),
                        sport = "football", country = "iceland", sex = "male") {
  tibble::tibble(
    sport      = sport,
    country    = country,
    sex        = sex,
    season     = 2026L,
    match_date = match_date,
    home_team  = home,
    away_team  = away,
    home_score = as.integer(home_score),
    away_score = as.integer(away_score)
  )
}

test_that("compute_settlement: moneyline home wins when home > away", {
  bets <- make_bet("moneyline", "home", odds = 2.5)
  res <- make_result(2, 1)
  out <- compute_settlement(bets, res)
  expect_true(out$win)
  expect_equal(out$pnl, 100 * (2.5 - 1))
})

test_that("compute_settlement: moneyline away wins when away > home", {
  bets <- make_bet("moneyline", "away", odds = 3.0)
  res <- make_result(1, 2)
  out <- compute_settlement(bets, res)
  expect_true(out$win)
  expect_equal(out$pnl, 200)
})

test_that("compute_settlement: moneyline draw wins on tie", {
  bets <- make_bet("moneyline", "draw", odds = 3.5)
  res <- make_result(1, 1)
  out <- compute_settlement(bets, res)
  expect_true(out$win)
  expect_equal(out$pnl, 250)
})

test_that("compute_settlement: moneyline home loses when away wins", {
  bets <- make_bet("moneyline", "home", odds = 2.5)
  res <- make_result(0, 1)
  out <- compute_settlement(bets, res)
  expect_false(out$win)
  expect_equal(out$pnl, -100)
})

test_that("compute_settlement: spread home wins when adjusted margin > 0", {
  bets <- make_bet("spread", "home", line = 1.5, odds = 1.9)
  res <- make_result(0, 1)
  out <- compute_settlement(bets, res)
  expect_true(out$win)
  expect_equal(out$pnl, 90)
})

test_that("compute_settlement: spread integer-line push counts as loss for home", {
  bets <- make_bet("spread", "home", line = 1.0, odds = 1.9)
  res <- make_result(0, 1)
  out <- compute_settlement(bets, res)
  expect_false(out$win)
  expect_equal(out$pnl, -100)
})

test_that("compute_settlement: spread away with positive line means away covers -line", {
  # Lengjan convention: same line shared by home/away outcomes; line is the
  # home team's signed handicap. line=+0.5 means home gets +0.5 head start
  # (away effectively -0.5). With score 1-1, home covers (loses by < 0.5).
  # Away does NOT cover.
  bets <- make_bet("spread", "away", line = 0.5, odds = 1.9)
  res <- make_result(1, 1)
  out <- compute_settlement(bets, res)
  expect_false(out$win)
})

test_that("compute_settlement: spread away wins by covering -line", {
  # line=+3 means away effectively -3; away covers when away wins by 4+.
  bets <- make_bet("spread", "away", line = 3, odds = 15.73)
  res <- make_result(0, 4)
  out <- compute_settlement(bets, res)
  expect_true(out$win)
  expect_equal(out$pnl, 100 * (15.73 - 1))
})

test_that("compute_settlement: spread away with positive line does NOT cover on home win", {
  # Pre-fix bug surface: previously (ag + line) > hg returned TRUE here
  # (home does not win by 3+) and graded the bet as a "win" inside the
  # ledger -- but Lengjan would have settled it as a loss (away didn't win
  # by 4+). This guards against re-introducing the sign flip.
  bets <- make_bet("spread", "away", line = 3, odds = 15.73)
  res <- make_result(2, 1) # home wins by 1; away does not cover -3
  out <- compute_settlement(bets, res)
  expect_false(out$win)
})

test_that("compute_settlement: spread away with negative line wins on tie", {
  # line=-0.5 means home gives 0.5 (away gets +0.5); with score 1-1 the away
  # side has the tie + half-point cushion and wins the spread.
  bets <- make_bet("spread", "away", line = -0.5, odds = 1.9)
  res <- make_result(1, 1)
  out <- compute_settlement(bets, res)
  expect_true(out$win)
})

test_that("compute_settlement: total over wins on strict greater-than", {
  bets <- make_bet("total", "over", line = 2.5, odds = 1.85)
  res <- make_result(2, 1)
  out <- compute_settlement(bets, res)
  expect_true(out$win)
  expect_equal(out$pnl, 85)
})

test_that("compute_settlement: total under wins on strict less-than", {
  bets <- make_bet("total", "under", line = 2.5, odds = 1.85)
  res <- make_result(1, 1)
  out <- compute_settlement(bets, res)
  expect_true(out$win)
})

test_that("compute_settlement: integer-line total push counts as loss both sides", {
  over <- make_bet("total", "over", line = 5.0, odds = 1.85)
  under <- make_bet("total", "under", line = 5.0, odds = 1.85)
  res <- make_result(2, 3)
  expect_false(compute_settlement(over, res)$win)
  expect_false(compute_settlement(under, res)$win)
})

test_that("compute_settlement: missing result leaves win NA", {
  bets <- make_bet("moneyline", "home",
    home = "A", away = "B",
    match_date = as.Date("2026-05-02")
  )
  res <- make_result(1, 0,
    home = "C", away = "D",
    match_date = as.Date("2026-05-02")
  )
  out <- compute_settlement(bets, res)
  expect_true(is.na(out$win))
  expect_true(is.na(out$pnl))
})

test_that("compute_settlement: window=0 (default) ignores nearby off-date results", {
  bets <- make_bet("moneyline", "home",
    home = "Fylkir", away = "Vestri",
    match_date = as.Date("2026-05-02"), odds = 2.5
  )
  res <- make_result(2, 0,
    home = "Fylkir", away = "Vestri",
    match_date = as.Date("2026-05-04")
  )
  out <- compute_settlement(bets, res)
  expect_true(is.na(out$win))
  expect_true(is.na(out$pnl))
  expect_false(out$settled)
})

test_that("compute_settlement: rescheduled fixture settles against nearby result within window", {
  bets <- dplyr::bind_rows(
    make_bet("total", "under",
      line = 2.5, odds = 2.15, bet_amount = 290,
      home = "Fylkir", away = "Vestri",
      match_date = as.Date("2026-05-02")
    ),
    make_bet("total", "under",
      line = 3.5, odds = 1.46, bet_amount = 304,
      home = "Fylkir", away = "Vestri",
      match_date = as.Date("2026-05-02")
    )
  )
  res <- make_result(2, 0,
    home = "Fylkir", away = "Vestri",
    match_date = as.Date("2026-05-04")
  )

  out <- compute_settlement(bets, res, match_date_window_days = 3L)
  expect_true(all(out$settled))
  expect_equal(out$win, c(TRUE, TRUE))
  expect_equal(out$pnl, c(290 * (2.15 - 1), 304 * (1.46 - 1)))
  # match_date itself must not be mutated (L3/L4 spirit)
  expect_equal(out$match_date, rep(as.Date("2026-05-02"), 2))
})

test_that("compute_settlement: fallback respects window bounds", {
  bets <- make_bet("moneyline", "home",
    home = "A", away = "B",
    match_date = as.Date("2026-05-01"), odds = 2.0
  )
  res <- make_result(2, 0,
    home = "A", away = "B",
    match_date = as.Date("2026-05-10")
  )
  # 9-day gap > 3-day window -> still unsettled
  out <- compute_settlement(bets, res, match_date_window_days = 3L)
  expect_true(is.na(out$win))
  expect_false(out$settled)
})

test_that("compute_settlement: unique-pairing guard leaves ambiguous orphans unsettled", {
  bets <- make_bet("moneyline", "home",
    home = "A", away = "B",
    match_date = as.Date("2026-05-02"), odds = 2.0
  )
  # Two genuine fixtures of the same pairing inside the window:
  # ambiguity → guard must refuse to settle.
  res <- dplyr::bind_rows(
    make_result(2, 0, home = "A", away = "B", match_date = as.Date("2026-05-01")),
    make_result(0, 1, home = "A", away = "B", match_date = as.Date("2026-05-04"))
  )
  out <- compute_settlement(bets, res, match_date_window_days = 5L)
  expect_true(is.na(out$win))
  expect_false(out$settled)
})

test_that("compute_settlement: fallback co-exists with strict pass on the same teams", {
  # Bet1 is an orphan from a reschedule; Bet2 placed at the correct date for
  # the same teams. Strict pass should settle Bet2; fallback should settle
  # Bet1 against the same result.
  bets <- dplyr::bind_rows(
    make_bet("total", "under",
      line = 2.5, odds = 2.0, bet_amount = 100,
      home = "Fylkir", away = "Vestri",
      match_date = as.Date("2026-05-02")
    ),
    make_bet("total", "under",
      line = 3.5, odds = 1.5, bet_amount = 200,
      home = "Fylkir", away = "Vestri",
      match_date = as.Date("2026-05-04")
    )
  )
  res <- make_result(2, 0,
    home = "Fylkir", away = "Vestri",
    match_date = as.Date("2026-05-04")
  )
  out <- compute_settlement(bets, res, match_date_window_days = 3L)
  expect_true(all(out$settled))
  expect_equal(out$win, c(TRUE, TRUE))
  expect_equal(out$pnl, c(100 * 1.0, 200 * 0.5))
})

test_that("compute_settlement: fallback does not match wrong teams within window", {
  bets <- make_bet("moneyline", "home",
    home = "A", away = "B",
    match_date = as.Date("2026-05-02"), odds = 2.0
  )
  res <- make_result(2, 0,
    home = "C", away = "D",
    match_date = as.Date("2026-05-03")
  )
  out <- compute_settlement(bets, res, match_date_window_days = 3L)
  expect_true(is.na(out$win))
  expect_false(out$settled)
})

test_that("compute_settlement: empty bets returns empty tibble", {
  bets <- make_bet("moneyline", "home")[0, ]
  res <- make_result(1, 0)
  out <- compute_settlement(bets, res)
  expect_equal(nrow(out), 0)
})

test_that("compute_settlement: unknown market raises clear error", {
  bets <- make_bet("moneyline", "home")
  bets$market <- "asianhandicap"
  res <- make_result(1, 0)
  expect_error(compute_settlement(bets, res), "Unknown market")
})

test_that("settle_ledger: flips only settled/win/pnl on newly resolvable rows (L4)", {
  tmp <- withr::local_tempdir()
  led <- dplyr::bind_rows(
    make_bet("moneyline", "home",
      odds = 2.0, bet_amount = 100,
      home = "A", away = "B", match_date = as.Date("2026-05-01")
    ),
    make_bet("total", "over",
      line = 2.5, odds = 1.85, bet_amount = 200,
      home = "C", away = "D", match_date = as.Date("2026-05-01")
    )
  )
  write_table(led, "ledger", root = tmp)

  res <- dplyr::bind_rows(
    make_result(2, 1, home = "A", away = "B"),
    make_result(0, 1, home = "C", away = "D")
  )
  res_root <- file.path(tmp, "facts", "results")
  fs::dir_create(res_root, recurse = TRUE)
  arrow::write_dataset(
    res,
    path = res_root, format = "parquet",
    partitioning = c("sport", "country", "sex", "season"),
    existing_data_behavior = "overwrite"
  )

  n <- settle_ledger(root = tmp)
  expect_equal(n, 2L)

  out <- read_table("ledger", root = tmp)
  out <- out[order(out$home_team), ]
  expect_true(all(out$settled))
  expect_equal(out$win, c(TRUE, FALSE))
  expect_equal(out$pnl, c(100, -200))
  expect_equal(out$bet_amount, c(100, 200))
  expect_equal(out$odds_placed, c(2.0, 1.85))
})

test_that("settle_ledger: leaves already-settled rows untouched", {
  tmp <- withr::local_tempdir()
  led <- make_bet("moneyline", "home", odds = 2.0, bet_amount = 100)
  led$settled <- TRUE
  led$win <- TRUE
  led$pnl <- 99999
  write_table(led, "ledger", root = tmp)

  res <- make_result(2, 1)
  res_root <- file.path(tmp, "facts", "results")
  fs::dir_create(res_root, recurse = TRUE)
  arrow::write_dataset(
    res,
    path = res_root, format = "parquet",
    partitioning = c("sport", "country", "sex", "season"),
    existing_data_behavior = "overwrite"
  )

  n <- settle_ledger(root = tmp)
  expect_equal(n, 0L)
  out <- read_table("ledger", root = tmp)
  expect_equal(out$pnl, 99999)
})

test_that("settle_ledger: returns 0 when no results match unsettled rows", {
  tmp <- withr::local_tempdir()
  led <- make_bet("moneyline", "home",
    home = "A", away = "B",
    match_date = as.Date("2026-05-02")
  )
  write_table(led, "ledger", root = tmp)

  res <- make_result(1, 0,
    home = "X", away = "Y",
    match_date = as.Date("2026-05-02")
  )
  res_root <- file.path(tmp, "facts", "results")
  fs::dir_create(res_root, recurse = TRUE)
  arrow::write_dataset(
    res,
    path = res_root, format = "parquet",
    partitioning = c("sport", "country", "sex", "season"),
    existing_data_behavior = "overwrite"
  )

  n <- settle_ledger(root = tmp)
  expect_equal(n, 0L)

  out <- read_table("ledger", root = tmp)
  expect_false(out$settled)
  expect_true(is.na(out$win))
  expect_true(is.na(out$pnl))
})

test_that("settle_ledger: empty ledger is a no-op", {
  tmp <- withr::local_tempdir()
  expect_equal(settle_ledger(root = tmp), 0L)
})

test_that("settle_ledger: back-settles rescheduled-fixture orphans via window default", {
  tmp <- withr::local_tempdir()
  led <- dplyr::bind_rows(
    make_bet("total", "under",
      line = 2.5, odds = 2.15, bet_amount = 290,
      home = "Fylkir", away = "Vestri",
      match_date = as.Date("2026-05-02")
    ),
    make_bet("total", "under",
      line = 3.5, odds = 1.46, bet_amount = 304,
      home = "Fylkir", away = "Vestri",
      match_date = as.Date("2026-05-02")
    )
  )
  write_table(led, "ledger", root = tmp)

  res <- make_result(2, 0,
    home = "Fylkir", away = "Vestri",
    match_date = as.Date("2026-05-04")
  )
  res_root <- file.path(tmp, "facts", "results")
  fs::dir_create(res_root, recurse = TRUE)
  arrow::write_dataset(
    res,
    path = res_root, format = "parquet",
    partitioning = c("sport", "country", "sex", "season"),
    existing_data_behavior = "overwrite"
  )

  n <- settle_ledger(root = tmp)
  expect_equal(n, 2L)

  out <- read_table("ledger", root = tmp)
  out <- out[order(out$line), ]
  expect_true(all(out$settled))
  expect_equal(out$win, c(TRUE, TRUE))
  expect_equal(out$pnl, c(290 * (2.15 - 1), 304 * (1.46 - 1)))
  # L3 spirit: match_date frozen at placement, never mutated
  expect_equal(out$match_date, rep(as.Date("2026-05-02"), 2))
})

test_that("settle_ledger: window=0 disables fallback so orphans stay unsettled", {
  tmp <- withr::local_tempdir()
  led <- make_bet("total", "under",
    line = 2.5, odds = 2.15, bet_amount = 290,
    home = "Fylkir", away = "Vestri",
    match_date = as.Date("2026-05-02")
  )
  write_table(led, "ledger", root = tmp)

  res <- make_result(2, 0,
    home = "Fylkir", away = "Vestri",
    match_date = as.Date("2026-05-04")
  )
  res_root <- file.path(tmp, "facts", "results")
  fs::dir_create(res_root, recurse = TRUE)
  arrow::write_dataset(
    res,
    path = res_root, format = "parquet",
    partitioning = c("sport", "country", "sex", "season"),
    existing_data_behavior = "overwrite"
  )

  n <- settle_ledger(root = tmp, match_date_window_days = 0L)
  expect_equal(n, 0L)
  out <- read_table("ledger", root = tmp)
  expect_false(out$settled)
  expect_true(is.na(out$win))
})

test_that("settle_ledger: leaves Parquet files of untouched partitions alone", {
  tmp <- withr::local_tempdir()
  led <- dplyr::bind_rows(
    make_bet("moneyline", "home",
      odds = 2.0, bet_amount = 100,
      sport = "football", country = "iceland",
      home = "A", away = "B", match_date = as.Date("2026-05-01")
    ),
    make_bet("moneyline", "home",
      odds = 2.0, bet_amount = 100,
      sport = "handball", country = "denmark",
      home = "X", away = "Y", match_date = as.Date("2026-05-01")
    )
  )
  write_table(led, "ledger", root = tmp)

  res <- make_result(2, 1,
    sport = "football", country = "iceland",
    home = "A", away = "B"
  )
  res_root <- file.path(tmp, "facts", "results")
  fs::dir_create(res_root, recurse = TRUE)
  arrow::write_dataset(
    res,
    path = res_root, format = "parquet",
    partitioning = c("sport", "country", "sex", "season"),
    existing_data_behavior = "overwrite"
  )

  untouched <- file.path(
    tmp, "decisions", "ledger",
    "sport=handball", "country=denmark", "part-0.parquet"
  )
  mtime_before <- file.info(untouched)$mtime

  Sys.sleep(1.05)
  n <- settle_ledger(root = tmp)

  mtime_after <- file.info(untouched)$mtime
  expect_equal(n, 1L)
  expect_equal(mtime_before, mtime_after)
})
