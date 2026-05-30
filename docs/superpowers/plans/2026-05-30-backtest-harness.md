# Backtest Harness Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build a reusable engine + Quarto report that replays historical betting decisions against real results and decomposes the outcome ("what would have been working, and how well").

**Architecture:** A linear pipeline of pure R functions — `bt_load_universe` (read leak-free `candidates`, attach recorded stakes) → stake rules (`stake_rolling` / `stake_fixed`) → `bt_run` (reuses `compute_settlement()` for win/pnl) → `bt_metrics`/`bt_calibration`/`bt_baselines`. A CLI (`scripts/0Nb_backtest.R`) materialises tidy Parquet under `data/backtest/`; a Quarto report (`docs/reports/2026-backtest.qmd`) renders the analysis.

**Tech Stack:** R package (devtools/testthat ed.3/roxygen2), arrow, dplyr, ggplot2, gt, Quarto. Reuses `R/settle.R::compute_settlement`, `R/storage.R::read_table`, `R/config.R::load_bankroll`/`load_leagues`.

**Spec:** `docs/superpowers/specs/2026-05-30-backtest-harness-design.md`

**Conventions (from `.claude/rules/`):** base pipe `|>`; explicit namespacing (`dplyr::`, `stats::`); roxygen `@export` + `devtools::document()`; testthat ed.3; ASCII-only fixtures (no Icelandic team names needed); `here::here()` for paths.

---

## Task 1: Bet universe loader

**Files:**
- Create: `R/backtest-universe.R`
- Test: `tests/testthat/test-backtest-universe.R`

- [ ] **Step 1: Write the failing test**

```r
# tests/testthat/test-backtest-universe.R
make_cand <- function(stage, ev, market = "moneyline", outcome = "home",
                      line = NA_real_, odds = 2.0, p = 0.55,
                      kelly_raw = 0.1, run_id = "2026-05-01",
                      home = "A", away = "B",
                      sport = "football", country = "iceland", sex = "male") {
  tibble::tibble(
    run_id = run_id, run_date = as.Date(run_id),
    sport = sport, country = country, sex = sex,
    match_date = as.Date("2026-05-02"),
    home_team = home, away_team = away,
    market = market, outcome = outcome, line = line,
    p = p, odds = odds, ev = ev, kelly_raw = kelly_raw, stage = stage
  )
}

with_universe_fixture <- function(cand, recs = NULL, code) {
  root <- withr::local_tempdir(.local_envir = parent.frame())
  write_table(cand, "candidates", root = root)
  if (!is.null(recs)) write_table(recs, "recommendations", root = root)
  code(root)
}

test_that("bt_load_universe strategy='kept' keeps only kept rows", {
  cand <- dplyr::bind_rows(
    make_cand("kept", ev = 0.30),
    make_cand("dropped_low_ev", ev = 0.01, outcome = "away"),
    make_cand("dropped_min_bet", ev = 0.05, outcome = "draw")
  )
  with_universe_fixture(cand, code = function(root) {
    u <- bt_load_universe(root = root, strategy = "kept")
    expect_equal(nrow(u), 1L)
    expect_equal(u$stage, "kept")
    expect_equal(u$strategy, "kept")
  })
})

test_that("bt_load_universe strategy='positive_ev' keeps all ev>0 regardless of stage", {
  cand <- dplyr::bind_rows(
    make_cand("kept", ev = 0.30),
    make_cand("dropped_min_bet", ev = 0.05, outcome = "draw"),
    make_cand("dropped_low_ev", ev = -0.02, outcome = "away")
  )
  with_universe_fixture(cand, code = function(root) {
    u <- bt_load_universe(root = root, strategy = "positive_ev")
    expect_equal(nrow(u), 2L)
    expect_true(all(u$ev > 0))
  })
})

test_that("bt_load_universe attaches recorded kelly/bet_amount only to kept bets", {
  cand <- dplyr::bind_rows(
    make_cand("kept", ev = 0.30),
    make_cand("dropped_min_bet", ev = 0.05, outcome = "draw")
  )
  recs <- tibble::tibble(
    run_id = "2026-05-01", run_date = as.Date("2026-05-01"),
    sport = "football", country = "iceland", sex = "male",
    match_date = as.Date("2026-05-02"), home_team = "A", away_team = "B",
    market = "moneyline", outcome = "home", line = NA_real_,
    p = 0.55, odds = 2.0, ev = 0.30, kelly = 0.04, bet_amount = 250
  )
  with_universe_fixture(cand, recs, code = function(root) {
    u <- bt_load_universe(root = root, strategy = "positive_ev")
    kept_row <- u[u$stage == "kept", ]
    drop_row <- u[u$stage == "dropped_min_bet", ]
    expect_equal(kept_row$kelly, 0.04)
    expect_equal(kept_row$bet_amount_recorded, 250)
    expect_true(is.na(drop_row$kelly))
    expect_true(is.na(drop_row$bet_amount_recorded))
  })
})

test_that("bt_load_universe returns empty-with-columns when no candidates", {
  root <- withr::local_tempdir()
  u <- bt_load_universe(root = root)
  expect_equal(nrow(u), 0L)
  expect_true(all(c("run_date", "p", "odds", "kelly", "strategy") %in% names(u)))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-backtest-universe.R")'`
Expected: FAIL — `could not find function "bt_load_universe"`.

- [ ] **Step 3: Write minimal implementation**

```r
# R/backtest-universe.R
#' @include storage.R
NULL

bt_universe_cols <- function() {
  c("run_date", "run_id", "sport", "country", "sex", "match_date",
    "home_team", "away_team", "market", "outcome", "line",
    "p", "odds", "ev", "kelly_raw", "kelly", "bet_amount_recorded",
    "stage", "strategy")
}

bt_empty_universe <- function() {
  tibble::tibble(
    run_date = as.Date(character()), run_id = character(),
    sport = character(), country = character(), sex = character(),
    match_date = as.Date(character()), home_team = character(),
    away_team = character(), market = character(), outcome = character(),
    line = numeric(), p = numeric(), odds = numeric(), ev = numeric(),
    kelly_raw = numeric(), kelly = numeric(),
    bet_amount_recorded = numeric(), stage = character(),
    strategy = character()
  )
}

#' Load the backtest bet universe from stored decisions.
#'
#' Reads the leak-free `candidates` store (every evaluated market per decide
#' run, with `p`/`odds`/`ev`/`kelly_raw` frozen at decide-time) and left-joins
#' `recommendations` to attach the recorded effective Kelly fraction (`kelly`)
#' and stake (`bet_amount`) for bets that were actually kept. The `strategy`
#' argument selects the bet subset to evaluate -- the strategy *is* the filter.
#'
#' @param root Data root (default `here::here("data")`).
#' @param strategy One of `"kept"` (our actual picks; default), `"positive_ev"`
#'   (every candidate with `ev > 0`), `"all"` (every candidate).
#' @param leagues Optional character vector of `sport` values to keep.
#' @param sex Optional character vector of `sex` values to keep.
#' @param from,to Optional `Date` or `YYYY-MM-DD` bounds on `run_date`.
#' @return Tibble, one row per bet (see `bt_universe_cols()`); empty-with-columns
#'   if nothing matches.
#' @export
bt_load_universe <- function(root = here::here("data"),
                             strategy = c("kept", "positive_ev", "all"),
                             leagues = NULL, sex = NULL,
                             from = NULL, to = NULL) {
  strategy <- match.arg(strategy)

  cand <- tryCatch(read_table("candidates", root = root),
                   error = function(e) NULL)
  if (is.null(cand) || nrow(cand) == 0L) return(bt_empty_universe())

  recs <- tryCatch(read_table("recommendations", root = root),
                   error = function(e) NULL)

  join_key <- c("run_id", "sport", "country", "sex", "match_date",
                "home_team", "away_team", "market", "outcome", "line")
  if (!is.null(recs) && nrow(recs) > 0L) {
    rec_slim <- dplyr::rename(
      recs[, c(join_key, "kelly", "bet_amount")],
      bet_amount_recorded = "bet_amount"
    )
    cand <- dplyr::left_join(cand, rec_slim, by = join_key)
  } else {
    cand$kelly <- NA_real_
    cand$bet_amount_recorded <- NA_real_
  }

  cand <- switch(strategy,
    kept = cand[cand$stage == "kept", , drop = FALSE],
    positive_ev = cand[cand$ev > 0, , drop = FALSE],
    all = cand
  )
  cand$strategy <- strategy

  if (!is.null(leagues)) cand <- cand[cand$sport %in% leagues, , drop = FALSE]
  if (!is.null(sex)) cand <- cand[cand$sex %in% sex, , drop = FALSE]
  if (!is.null(from)) cand <- cand[cand$run_date >= as.Date(from), , drop = FALSE]
  if (!is.null(to)) cand <- cand[cand$run_date <= as.Date(to), , drop = FALSE]

  if (nrow(cand) == 0L) return(bt_empty_universe())
  cand[, bt_universe_cols(), drop = FALSE]
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-backtest-universe.R")'`
Expected: PASS (4 tests).

- [ ] **Step 5: Commit**

```bash
git add R/backtest-universe.R tests/testthat/test-backtest-universe.R
git commit -m "feat(backtest): bet universe loader with strategy filters"
```

---

## Task 2: Effective-fraction helper + fixed-pool stake rule

**Files:**
- Create: `R/backtest-stake.R`
- Test: `tests/testthat/test-backtest-stake.R`

- [ ] **Step 1: Write the failing test**

```r
# tests/testthat/test-backtest-stake.R
make_settled_bet <- function(win, odds = 2.0, kelly = NA_real_,
                             kelly_raw = 0.1, run_id = "2026-05-01",
                             match_date = as.Date("2026-05-02")) {
  tibble::tibble(
    run_id = run_id, run_date = as.Date(run_id),
    match_date = match_date, odds = odds,
    kelly_raw = kelly_raw, kelly = kelly, win = win
  )
}

test_that("bt_effective_fraction uses recorded kelly for kept bets", {
  u <- make_settled_bet(win = TRUE, kelly = 0.04, kelly_raw = 0.2)
  expect_equal(bt_effective_fraction(u), 0.04)
})

test_that("bt_effective_fraction estimates counterfactual frac via per-run shrink", {
  u <- dplyr::bind_rows(
    make_settled_bet(win = TRUE, kelly = 0.05, kelly_raw = 0.10),  # shrink 0.5
    make_settled_bet(win = FALSE, kelly = NA_real_, kelly_raw = 0.20)
  )
  fr <- bt_effective_fraction(u)
  expect_equal(fr[1], 0.05)
  expect_equal(fr[2], 0.20 * 0.5)  # kelly_raw * per-run median(kelly/kelly_raw)
})

test_that("stake_fixed sizes off a constant pool and computes pnl from win", {
  u <- dplyr::bind_rows(
    make_settled_bet(win = TRUE, kelly = 0.04, odds = 3.0),
    make_settled_bet(win = FALSE, kelly = 0.04, odds = 2.0)
  )
  out <- stake_fixed(u, ref_pool = 10000)
  expect_equal(out$stake, c(400, 400))           # 0.04 * 10000
  expect_equal(out$pnl, c(400 * (3.0 - 1), -400)) # win: stake*(odds-1); loss: -stake
  expect_true(all(out$pool_before == 10000))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-backtest-stake.R")'`
Expected: FAIL — `could not find function "bt_effective_fraction"`.

- [ ] **Step 3: Write minimal implementation**

```r
# R/backtest-stake.R
NULL

#' Effective Kelly stake fraction per bet.
#'
#' Kept bets use their recorded effective fraction (`kelly`). Counterfactual
#' bets (no recorded `kelly`) are estimated as `kelly_raw * shrink`, where
#' `shrink` is the median `kelly / kelly_raw` over kept bets in the same run
#' (fallback: global median over all kept bets; final fallback: 0.05, near the
#' current operational `kelly_frac`).
#' @param universe Tibble with `run_id`, `kelly`, `kelly_raw`.
#' @return Numeric vector, one fraction per row.
#' @noRd
bt_effective_fraction <- function(universe) {
  kept <- !is.na(universe$kelly)
  pos <- universe$kelly_raw > 0
  shrink_global <- stats::median(
    (universe$kelly / universe$kelly_raw)[kept & pos], na.rm = TRUE
  )
  if (!is.finite(shrink_global)) shrink_global <- 0.05

  frac <- numeric(nrow(universe))
  for (r in unique(universe$run_id)) {
    rm <- universe$run_id == r
    rk <- rm & kept & pos
    shrink_r <- if (any(rk)) {
      stats::median(universe$kelly[rk] / universe$kelly_raw[rk], na.rm = TRUE)
    } else {
      shrink_global
    }
    frac[rm & kept] <- universe$kelly[rm & kept]
    frac[rm & !kept] <- universe$kelly_raw[rm & !kept] * shrink_r
  }
  frac
}

#' Fixed-reference-pool stake rule (no compounding).
#'
#' Sizes every bet off a constant `ref_pool` so cross-strategy / cross-market
#' ROI is comparable without compounding variance. `pnl` is derived from the
#' bet's `win` flag (set upstream by [bt_run()]).
#' @param universe Tibble with `win`, `odds`, plus the columns
#'   [bt_effective_fraction()] needs.
#' @param ref_pool Constant pool in ISK.
#' @param ... Absorbs `initial_pool` (ignored) for a uniform stake-rule signature.
#' @return `universe` plus `stake`, `pnl`, `pool_before`.
#' @export
stake_fixed <- function(universe, ref_pool = NULL, ...) {
  if (is.null(ref_pool)) {
    dots <- list(...)
    ref_pool <- dots$initial_pool %||% load_bankroll()$initial_pool
  }
  frac <- bt_effective_fraction(universe)
  universe$stake <- round(frac * ref_pool)
  universe$pnl <- ifelse(universe$win,
                         universe$stake * (universe$odds - 1),
                         -universe$stake)
  universe$pool_before <- ref_pool
  universe
}
```

> Note: `%||%` is already defined at package scope in `R/settle.R`; do not
> redefine it here.

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-backtest-stake.R")'`
Expected: PASS (3 tests).

- [ ] **Step 5: Commit**

```bash
git add R/backtest-stake.R tests/testthat/test-backtest-stake.R
git commit -m "feat(backtest): effective-fraction helper + fixed-pool stake rule"
```

---

## Task 3: Rolling-bankroll stake rule

**Files:**
- Modify: `R/backtest-stake.R` (append `stake_rolling`)
- Test: `tests/testthat/test-backtest-stake.R` (append)

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-backtest-stake.R`:

```r
test_that("stake_rolling compounds the pool across runs by match settlement date", {
  # Run 1 (run_date 05-01, match 05-02): one kept bet, kelly 0.10, odds 2.0, WIN.
  #   pool_1 = 10000 -> stake 1000 -> pnl +1000 (settles 05-02).
  # Run 2 (run_date 05-03, match 05-04): one kept bet, kelly 0.10, odds 2.0, LOSS.
  #   pool_2 = 10000 + 1000 (05-02 settled before 05-03) = 11000 -> stake 1100 -> pnl -1100.
  u <- dplyr::bind_rows(
    make_settled_bet(win = TRUE,  kelly = 0.10, odds = 2.0,
                     run_id = "2026-05-01", match_date = as.Date("2026-05-02")),
    make_settled_bet(win = FALSE, kelly = 0.10, odds = 2.0,
                     run_id = "2026-05-03", match_date = as.Date("2026-05-04"))
  )
  out <- stake_rolling(u, initial_pool = 10000,
                       daily_budget_frac = 1.0, daily_budget_min_isk = 0)
  expect_equal(out$pool_before, c(10000, 11000))
  expect_equal(out$stake, c(1000, 1100))
  expect_equal(out$pnl, c(1000, -1100))
})

test_that("stake_rolling applies the daily-budget cap to an over-budget slate", {
  # Two kept bets same run, each kelly 0.10 of 10000 = 1000, total 2000.
  # Cap = max(0.05*10000, 1000) = 1000 -> scale both by 1000/2000 = 0.5 -> 500 each.
  u <- dplyr::bind_rows(
    make_settled_bet(win = TRUE, kelly = 0.10, odds = 2.0, run_id = "2026-05-01"),
    make_settled_bet(win = TRUE, kelly = 0.10, odds = 2.0, run_id = "2026-05-01")
  )
  out <- stake_rolling(u, initial_pool = 10000,
                       daily_budget_frac = 0.05, daily_budget_min_isk = 1000)
  expect_equal(out$stake, c(500, 500))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-backtest-stake.R")'`
Expected: FAIL — `could not find function "stake_rolling"`.

- [ ] **Step 3: Write minimal implementation**

Append to `R/backtest-stake.R`:

```r
#' Rolling-bankroll stake rule (path-dependent; the money story).
#'
#' Walks decide runs in chronological order. For each run, the pool is
#' `initial_pool + sum(pnl)` over bets whose match settled *strictly before*
#' that run's `run_date`, mirroring the live `current_pool`. The run's bets are
#' sized off that pool (slate co-sizing, as joint Kelly does), then scaled down
#' if the slate total exceeds the daily-budget cap
#' `max(daily_budget_frac * pool, daily_budget_min_isk)`. `pnl` comes from each
#' bet's `win` flag.
#' @param universe Tibble with `run_id`, `run_date`, `match_date`, `win`,
#'   `odds`, plus the columns [bt_effective_fraction()] needs.
#' @param initial_pool Starting bankroll in ISK.
#' @param daily_budget_frac,daily_budget_min_isk Daily-budget cap parameters
#'   (defaults from `bankroll.yml`).
#' @param ... Absorbs `ref_pool` (ignored) for a uniform stake-rule signature.
#' @return `universe` plus `stake`, `pnl`, `pool_before`.
#' @export
stake_rolling <- function(universe, initial_pool = NULL,
                          daily_budget_frac = 0.05,
                          daily_budget_min_isk = 1000, ...) {
  if (is.null(initial_pool)) initial_pool <- load_bankroll()$initial_pool
  frac <- bt_effective_fraction(universe)
  u <- universe
  u$stake <- NA_real_
  u$pnl <- NA_real_
  u$pool_before <- NA_real_

  settled_md <- as.Date(character())
  settled_pnl <- numeric()

  for (r in sort(unique(u$run_id))) {
    rm <- which(u$run_id == r)
    rd <- u$run_date[rm][[1]]
    pool_r <- initial_pool + sum(settled_pnl[settled_md < rd], na.rm = TRUE)

    stake_r <- round(frac[rm] * pool_r)
    cap <- max(daily_budget_frac * pool_r, daily_budget_min_isk)
    tot <- sum(stake_r)
    if (is.finite(tot) && tot > cap && tot > 0) {
      stake_r <- round(stake_r * cap / tot)
    }
    pnl_r <- ifelse(u$win[rm], stake_r * (u$odds[rm] - 1), -stake_r)

    u$stake[rm] <- stake_r
    u$pnl[rm] <- pnl_r
    u$pool_before[rm] <- pool_r
    settled_md <- c(settled_md, u$match_date[rm])
    settled_pnl <- c(settled_pnl, pnl_r)
  }
  u
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-backtest-stake.R")'`
Expected: PASS (5 tests total).

- [ ] **Step 5: Commit**

```bash
git add R/backtest-stake.R tests/testthat/test-backtest-stake.R
git commit -m "feat(backtest): rolling-bankroll stake rule with daily-budget cap"
```

---

## Task 4: Backtest engine (reuses compute_settlement)

**Files:**
- Create: `R/backtest-engine.R`
- Test: `tests/testthat/test-backtest-engine.R`

- [ ] **Step 1: Write the failing test**

```r
# tests/testthat/test-backtest-engine.R
bt_eng_universe <- function() {
  tibble::tibble(
    run_id = "2026-05-01", run_date = as.Date("2026-05-01"),
    sport = "football", country = "iceland", sex = "male",
    match_date = as.Date("2026-05-02"),
    home_team = c("A", "C"), away_team = c("B", "D"),
    market = "moneyline", outcome = c("home", "away"),
    line = NA_real_, p = 0.5, odds = c(2.0, 3.0),
    ev = 0.1, kelly_raw = 0.1, kelly = 0.10,
    bet_amount_recorded = NA_real_, stage = "kept", strategy = "kept"
  )
}
bt_eng_results <- function() {
  tibble::tibble(
    sport = "football", country = "iceland", sex = "male", season = 2026L,
    match_date = as.Date("2026-05-02"),
    home_team = c("A", "C"), away_team = c("B", "D"),
    home_score = c(2L, 0L), away_score = c(1L, 1L)  # A home win; D away win
  )
}

test_that("bt_run settles via compute_settlement and applies fixed stakes", {
  out <- bt_run(bt_eng_universe(), bt_eng_results(),
                stake_rule = stake_fixed, initial_pool = 10000)
  expect_equal(nrow(out), 2L)
  expect_true(all(out$win))                       # both bets win
  # stake = 0.10 * 10000 = 1000 each; pnl = 1000*(odds-1)
  expect_equal(sort(out$pnl), sort(c(1000 * 1.0, 1000 * 2.0)))
  expect_equal(max(out$pool_after), 10000 + 3000) # cumulative
})

test_that("bt_run excludes bets with no matching result (pending)", {
  u <- bt_eng_universe()
  res <- bt_eng_results()[1, ]                     # only A vs B has a result
  out <- bt_run(u, res, stake_rule = stake_fixed, initial_pool = 10000)
  expect_equal(nrow(out), 1L)
  expect_equal(attr(out, "pending"), 1L)
})

test_that("bt_run returns empty-with-columns for an empty universe", {
  out <- bt_run(bt_load_universe(root = withr::local_tempdir()),
                bt_eng_results(), initial_pool = 10000)
  expect_equal(nrow(out), 0L)
  expect_true(all(c("stake", "pnl", "pool_after", "cum_pnl") %in% names(out)))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-backtest-engine.R")'`
Expected: FAIL — `could not find function "bt_run"`.

- [ ] **Step 3: Write minimal implementation**

```r
# R/backtest-engine.R
#' @include settle.R config.R backtest-stake.R
NULL

bt_empty_settled <- function() {
  u <- bt_empty_universe()
  u$win <- logical()
  u$stake <- numeric()
  u$pnl <- numeric()
  u$pool_before <- numeric()
  u$cum_pnl <- numeric()
  u$pool_after <- numeric()
  u
}

#' Run a backtest: settle a bet universe and size stakes.
#'
#' Win/loss is stake-independent, so the engine first resolves `win` per bet by
#' reusing [compute_settlement()] (per `(sport, country)` with the same
#' `tie_threshold` as `settle_ledger()`, so push/boundary semantics match the
#' live decider). Bets with no matching result are dropped (counted in the
#' `"pending"` attribute). The remaining settled bets pass to `stake_rule`,
#' which computes `stake` and `pnl`; the engine then accumulates `cum_pnl` and
#' `pool_after` in match-settlement order.
#' @param universe Tibble from [bt_load_universe()].
#' @param results Results tibble (`read_table("results")`).
#' @param stake_rule [stake_rolling()] (default) or [stake_fixed()].
#' @param initial_pool Starting bankroll; default from `bankroll.yml`.
#' @param match_date_window_days Reschedule fallback window; default `3L`
#'   (matches `settle_ledger()`).
#' @param ... Forwarded to `stake_rule`.
#' @return Per-bet tibble with `win, stake, pnl, pool_before, cum_pnl,
#'   pool_after`; `attr(., "pending")` is the count of unsettled bets dropped.
#' @export
bt_run <- function(universe, results, stake_rule = stake_rolling,
                   initial_pool = NULL, match_date_window_days = 3L, ...) {
  if (is.null(initial_pool)) initial_pool <- load_bankroll()$initial_pool
  if (nrow(universe) == 0L) return(bt_empty_settled())

  bets <- universe
  bets$odds_placed <- bets$odds
  bets$bet_amount <- 1
  bets$settled <- FALSE
  bets$win <- NA
  bets$pnl <- NA_real_

  leagues_cfg <- tryCatch(load_leagues(), error = function(e) list())
  tt_for <- function(sport, country) {
    for (lg in leagues_cfg) {
      if (identical(lg$sport, sport) && identical(lg$country, country)) {
        return(lg$betting$scoring$tie_threshold %||% 0)
      }
    }
    0
  }

  groups <- dplyr::distinct(bets[, c("sport", "country")])
  win <- rep(NA, nrow(bets))
  for (gi in seq_len(nrow(groups))) {
    sp <- groups$sport[[gi]]
    co <- groups$country[[gi]]
    gm <- which(bets$sport == sp & bets$country == co)
    g <- compute_settlement(bets[gm, , drop = FALSE], results,
                            match_date_window_days = match_date_window_days,
                            tie_threshold = tt_for(sp, co))
    win[gm] <- g$win
  }

  universe$win <- win
  pending <- sum(is.na(universe$win))
  settled <- universe[!is.na(universe$win), , drop = FALSE]
  if (nrow(settled) == 0L) {
    out <- bt_empty_settled()
    attr(out, "pending") <- pending
    return(out)
  }

  staked <- stake_rule(settled, initial_pool = initial_pool,
                       ref_pool = initial_pool, ...)
  staked <- staked[order(staked$match_date, staked$run_date), , drop = FALSE]
  staked$cum_pnl <- cumsum(staked$pnl)
  staked$pool_after <- initial_pool + staked$cum_pnl
  attr(staked, "pending") <- pending
  staked
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-backtest-engine.R")'`
Expected: PASS (3 tests).

- [ ] **Step 5: Commit**

```bash
git add R/backtest-engine.R tests/testthat/test-backtest-engine.R
git commit -m "feat(backtest): engine reusing compute_settlement for win/pnl"
```

---

## Task 5: Metrics, calibration, baselines

**Files:**
- Create: `R/backtest-metrics.R`
- Test: `tests/testthat/test-backtest-metrics.R`

- [ ] **Step 1: Write the failing test**

```r
# tests/testthat/test-backtest-metrics.R
settled_fixture <- function() {
  tibble::tibble(
    sport = "football", market = c("moneyline", "moneyline", "total"),
    sex = "male", p = c(0.6, 0.4, 0.5),
    match_date = as.Date(c("2026-05-02", "2026-05-03", "2026-05-04")),
    odds = c(2.0, 3.0, 1.9),
    win = c(TRUE, FALSE, TRUE),
    stake = c(1000, 1000, 1000),
    pnl = c(1000, -1000, 900)
  )
}

test_that("bt_metrics computes ROI, hit-rate, yield, drawdown", {
  m <- bt_metrics(settled_fixture())
  expect_equal(m$n_bets, 3L)
  expect_equal(m$total_staked, 3000)
  expect_equal(m$total_pnl, 900)
  expect_equal(m$roi, 900 / 3000)
  expect_equal(m$yield, 900 / 3)
  expect_equal(m$hit_rate, 2 / 3)
  expect_equal(m$max_drawdown, -1000)  # after +1000 then -1000 trough
})

test_that("bt_metrics groups by a dimension", {
  m <- bt_metrics(settled_fixture(), by = "market")
  expect_setequal(m$market, c("moneyline", "total"))
  ml <- m[m$market == "moneyline", ]
  expect_equal(ml$n_bets, 2L)
  expect_equal(ml$total_pnl, 0)
})

test_that("bt_calibration bins predicted p against realised frequency", {
  cal <- bt_calibration(settled_fixture(), n_bins = 2)
  expect_true(all(c("mean_p", "realised_freq", "n") %in% names(cal)))
  expect_equal(sum(cal$n), 3L)
})

test_that("bt_metrics returns empty tibble on empty input", {
  expect_equal(nrow(bt_metrics(settled_fixture()[0, ])), 0L)
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-backtest-metrics.R")'`
Expected: FAIL — `could not find function "bt_metrics"`.

- [ ] **Step 3: Write minimal implementation**

```r
# R/backtest-metrics.R
#' @include backtest-engine.R
NULL

bt_metrics_one <- function(d) {
  n <- nrow(d)
  staked <- sum(d$stake)
  pnl <- sum(d$pnl)
  o <- order(d$match_date)
  cp <- cumsum(d$pnl[o])
  peak <- cummax(c(0, cp))[-1]
  ret <- d$pnl / pmax(d$stake, 1e-9)
  tibble::tibble(
    n_bets = n,
    total_staked = staked,
    total_pnl = pnl,
    roi = if (staked > 0) pnl / staked else NA_real_,
    yield = pnl / n,
    hit_rate = mean(d$win),
    avg_odds = mean(d$odds),
    max_drawdown = if (n > 0) min(cp - peak) else NA_real_,
    sharpe_like = if (stats::sd(ret) > 0) mean(ret) / stats::sd(ret) else NA_real_
  )
}

#' Backtest performance metrics, optionally grouped.
#' @param settled Per-bet tibble from [bt_run()].
#' @param by Optional character vector of grouping columns (e.g. `"market"`,
#'   `c("sport", "sex")`).
#' @return One-row (or grouped) KPI tibble. Empty input -> empty tibble.
#' @export
bt_metrics <- function(settled, by = NULL) {
  if (nrow(settled) == 0L) return(tibble::tibble())
  if (is.null(by)) return(bt_metrics_one(settled))
  settled |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::group_modify(~ bt_metrics_one(.x)) |>
    dplyr::ungroup()
}

#' Calibration reliability: predicted `p` vs realised win frequency.
#' @param settled Per-bet tibble from [bt_run()].
#' @param n_bins Number of equal-width probability bins.
#' @param by Optional grouping columns for decomposition.
#' @return Tibble of `(.by.., bin, mean_p, realised_freq, n)`.
#' @export
bt_calibration <- function(settled, n_bins = 10, by = NULL) {
  if (nrow(settled) == 0L) return(tibble::tibble())
  settled$bin <- cut(settled$p, breaks = seq(0, 1, length.out = n_bins + 1),
                     include.lowest = TRUE)
  settled |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(by, "bin")))) |>
    dplyr::summarise(mean_p = mean(.data$p),
                     realised_freq = mean(.data$win),
                     n = dplyr::n(), .groups = "drop")
}

#' Compare strategies under both stake models.
#'
#' Runs the engine for `our_picks` (kept), `all_positive_ev`, `flat_stake`
#' (kept bets at a constant ISK stake -- isolates selection from sizing), and
#' `favourites` (lowest-odds moneyline outcome per match -- naive baseline),
#' under both rolling and fixed stake models, and returns a tidy comparison.
#' @param root Data root.
#' @param results Optional results tibble (read from `root` if NULL).
#' @param initial_pool Starting bankroll; default from `bankroll.yml`.
#' @param flat_isk Constant stake for the `flat_stake` strategy.
#' @param ... Forwarded to [bt_load_universe()] (e.g. `leagues`, `from`, `to`).
#' @return Tibble of metrics with `strategy` + `stake_model` columns.
#' @export
bt_baselines <- function(root = here::here("data"), results = NULL,
                         initial_pool = NULL, flat_isk = 500, ...) {
  if (is.null(results)) results <- read_table("results", root = root)
  if (is.null(initial_pool)) initial_pool <- load_bankroll()$initial_pool

  picks <- bt_load_universe(root, "kept", ...)
  posev <- bt_load_universe(root, "positive_ev", ...)
  allc  <- bt_load_universe(root, "all", ...)

  favs <- allc[allc$market == "moneyline", , drop = FALSE]
  if (nrow(favs) > 0L) {
    favs <- favs |>
      dplyr::group_by(.data$run_id, .data$sport, .data$country, .data$sex,
                      .data$match_date, .data$home_team, .data$away_team) |>
      dplyr::slice_min(.data$odds, n = 1, with_ties = FALSE) |>
      dplyr::ungroup()
  }

  flat_rule <- function(universe, ...) {
    universe$stake <- flat_isk
    universe$pnl <- ifelse(universe$win,
                           universe$stake * (universe$odds - 1),
                           -universe$stake)
    universe$pool_before <- initial_pool
    universe
  }

  jobs <- list(
    list(name = "our_picks", u = picks, rules = c("rolling", "fixed")),
    list(name = "all_positive_ev", u = posev, rules = c("rolling", "fixed")),
    list(name = "favourites", u = favs, rules = c("rolling", "fixed")),
    list(name = "flat_stake", u = picks, rules = "flat")
  )
  out <- list()
  for (j in jobs) {
    if (nrow(j$u) == 0L) next
    for (sm in j$rules) {
      rule <- switch(sm, rolling = stake_rolling, fixed = stake_fixed,
                     flat = flat_rule)
      r <- bt_run(j$u, results, stake_rule = rule, initial_pool = initial_pool)
      if (nrow(r) == 0L) next
      m <- bt_metrics(r)
      m$strategy <- j$name
      m$stake_model <- sm
      out[[paste(j$name, sm)]] <- m
    }
  }
  dplyr::bind_rows(out)
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-backtest-metrics.R")'`
Expected: PASS (4 tests).

- [ ] **Step 5: Commit**

```bash
git add R/backtest-metrics.R tests/testthat/test-backtest-metrics.R
git commit -m "feat(backtest): metrics, calibration, strategy baselines"
```

---

## Task 6: CLI driver

**Files:**
- Create: `scripts/0Nb_backtest.R`
- Modify: `.gitignore` (add `data/backtest/`)

- [ ] **Step 1: Add the gitignore entry**

Append to `.gitignore`:

```
data/backtest/
```

- [ ] **Step 2: Write the CLI**

```r
#!/usr/bin/env Rscript
# scripts/0Nb_backtest.R --
# Replay historical betting decisions against results and write tidy
# backtest artefacts to data/backtest/ for the Quarto report. Read-only on
# data/decisions/ -- never touches the ledger or the money path.
#
# Usage:
#   Rscript scripts/0Nb_backtest.R                       # all strategies, both stake models
#   Rscript scripts/0Nb_backtest.R --strategy kept --stake rolling
#   Rscript scripts/0Nb_backtest.R --league football_iceland --from 2026-04-25
invisible(Sys.setlocale("LC_ALL", "is_IS.UTF-8"))
options(width = 120)
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
get_flag <- function(name, default = NULL) {
  i <- which(args == paste0("--", name))
  if (length(i) == 0L) default else args[[i + 1L]]
}
strategy <- get_flag("strategy", "kept")
stake <- get_flag("stake", "both")
league <- get_flag("league")
from <- get_flag("from")
to <- get_flag("to")

sport_filter <- if (!is.null(league)) sub("_.*$", "", league) else NULL

results <- sports::read_table("results")
initial_pool <- sports::load_bankroll()$initial_pool

universe <- sports::bt_load_universe(
  strategy = strategy, leagues = sport_filter, from = from, to = to
)
if (nrow(universe) == 0L) {
  cli::cli_alert_warning("No bets in universe for the given filters; nothing to do.")
  quit(status = 0L)
}

stake_models <- if (stake == "both") c("rolling", "fixed") else stake
per_bet <- list()
for (sm in stake_models) {
  rule <- if (sm == "rolling") sports::stake_rolling else sports::stake_fixed
  r <- sports::bt_run(universe, results, stake_rule = rule,
                      initial_pool = initial_pool)
  r$stake_model <- sm
  per_bet[[sm]] <- r
}
per_bet <- dplyr::bind_rows(per_bet)

out_dir <- here::here("data", "backtest")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
arrow::write_parquet(per_bet, file.path(out_dir, "per_bet.parquet"))

metrics <- dplyr::bind_rows(lapply(stake_models, function(sm) {
  d <- per_bet[per_bet$stake_model == sm, ]
  m <- sports::bt_metrics(d)
  m$stake_model <- sm
  m
}))
arrow::write_parquet(metrics, file.path(out_dir, "metrics.parquet"))

calib <- sports::bt_calibration(per_bet[per_bet$stake_model == stake_models[[1]], ])
arrow::write_parquet(calib, file.path(out_dir, "calibration.parquet"))

baselines <- sports::bt_baselines(leagues = sport_filter, from = from, to = to)
arrow::write_parquet(baselines, file.path(out_dir, "baselines.parquet"))

cli::cli_h2("Backtest summary")
print(metrics)
cli::cli_alert_info("Wrote artefacts to {.path {out_dir}}")
```

- [ ] **Step 3: Smoke-run against real data**

Run: `Rscript scripts/0Nb_backtest.R --strategy kept`
Expected: prints a metrics table (n_bets > 0, an ROI value); creates
`data/backtest/per_bet.parquet`, `metrics.parquet`, `calibration.parquet`,
`baselines.parquet`. No error.

- [ ] **Step 4: Verify outputs are readable**

Run: `Rscript -e 'a <- arrow::read_parquet("data/backtest/metrics.parquet"); print(a)'`
Expected: a tibble with `n_bets, total_pnl, roi, ...` and a `stake_model` column.

- [ ] **Step 5: Commit**

```bash
git add scripts/0Nb_backtest.R .gitignore
git commit -m "feat(backtest): CLI driver writing tidy artefacts to data/backtest"
```

---

## Task 7: Quarto report

**Files:**
- Create: `docs/reports/2026-backtest.qmd`

- [ ] **Step 1: Write the report**

```markdown
---
title: "Betting Backtest — Iceland 2026"
subtitle: "What would have been working, and how well"
author: "Brynjólfur Gauti Jónsson"
date: today
date-format: "YYYY-MM-DD"
format:
  html:
    toc: true
    toc-depth: 3
    toc-location: left
    code-fold: true
    code-tools: true
    theme: cosmo
    fig-width: 9
    fig-height: 5.5
    fig-dpi: 144
    embed-resources: true
    smooth-scroll: true
execute:
  echo: false
  warning: false
  message: false
---

```{r}
#| label: setup
#| include: false
invisible(Sys.setlocale("LC_ALL", "is_IS.UTF-8"))
options(width = 110)
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(gt)
  library(scales)
  devtools::load_all(here::here(), quiet = TRUE)
})

results <- read_table("results")
initial_pool <- load_bankroll()$initial_pool
picks <- bt_load_universe(strategy = "kept")
roll <- bt_run(picks, results, stake_rule = stake_rolling, initial_pool = initial_pool)
fixed <- bt_run(picks, results, stake_rule = stake_fixed, initial_pool = initial_pool)
```

## Headline — our actual picks

```{r}
#| label: kpis
m <- bt_metrics(roll)
gt(m) |>
  fmt_number(c(total_staked, total_pnl, max_drawdown), decimals = 0) |>
  fmt_percent(c(roi, hit_rate), decimals = 1) |>
  tab_header("Rolling-bankroll performance (kept bets)")
```

```{r}
#| label: pnl-curve
#| fig-alt: "Cumulative profit-and-loss of the bankroll over the backtest window."
roll |>
  arrange(match_date) |>
  ggplot(aes(match_date, pool_after)) +
  geom_hline(yintercept = initial_pool, linetype = 2, colour = "grey50") +
  geom_step() +
  labs(x = NULL, y = "Bankroll (ISK)",
       title = "Rolling bankroll — kept bets") +
  theme_minimal()
```

## Where the edge is

```{r}
#| label: by-dimension
bind_rows(
  bt_metrics(fixed, by = "market") |> rename(dim = market) |> mutate(facet = "market"),
  bt_metrics(fixed, by = "sport")  |> rename(dim = sport)  |> mutate(facet = "sport"),
  bt_metrics(fixed, by = "sex")    |> rename(dim = sex)    |> mutate(facet = "sex")
) |>
  select(facet, dim, n_bets, total_pnl, roi, hit_rate, avg_odds) |>
  gt(groupname_col = "facet") |>
  fmt_number(total_pnl, decimals = 0) |>
  fmt_percent(c(roi, hit_rate), decimals = 1) |>
  tab_header("Fixed-fraction ROI by dimension")
```

## Calibration

```{r}
#| label: calibration
#| fig-alt: "Reliability plot: predicted probability versus realised win frequency."
bt_calibration(fixed, n_bins = 8) |>
  filter(n > 0) |>
  ggplot(aes(mean_p, realised_freq, size = n)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "grey50") +
  geom_point(alpha = 0.7) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(x = "Predicted p", y = "Realised frequency",
       title = "Calibration (kept bets)") +
  theme_minimal()
```

## Strategy comparison

```{r}
#| label: baselines
bt_baselines() |>
  select(strategy, stake_model, n_bets, total_pnl, roi, hit_rate) |>
  arrange(desc(roi)) |>
  gt() |>
  fmt_number(total_pnl, decimals = 0) |>
  fmt_percent(c(roi, hit_rate), decimals = 1) |>
  tab_header("Strategy x stake-model comparison")
```

## Caveats

- **Leak-free** picks: `candidates.p` was frozen pre-match; settlement reuses
  `compute_settlement()` so win/push boundaries match the live decider.
- **Obtainability:** odds are decide-time prices; some bets were never
  placeable (early-kickoff delisting). Pending/unsettled bets are excluded.
- **Counterfactual stakes are approximate** for non-kept strategies
  (`portfolio_lambda` / calibration are not stored in `candidates`).
- **Window** is short (candidates begin 2026-04-25) — per-cell slices with low
  `n_bets` are noisy.
```

- [ ] **Step 2: Render to verify it builds**

Run: `quarto render docs/reports/2026-backtest.qmd`
Expected: produces `docs/reports/2026-backtest.html` with no error. (Requires
`data/backtest/` artefacts from Task 6, or the chunks regenerate from
`candidates`/`results` directly — they call the engine inline, so a prior CLI
run is not strictly required.)

- [ ] **Step 3: Commit**

```bash
git add docs/reports/2026-backtest.qmd docs/reports/2026-backtest.html
git commit -m "feat(backtest): Quarto report — PnL, edge breakdown, calibration, comparison"
```

---

## Task 8: Docs, NAMESPACE, full verification

**Files:**
- Create: `.claude/rules/backtest.md`
- Modify: `CLAUDE.md` (add a backtest line under Conventions)
- Modify: `NAMESPACE` (via `devtools::document()`)

- [ ] **Step 1: Write the rule file**

```markdown
# Backtest harness

Loads when working on `R/backtest-*.R`, `scripts/0Nb_backtest.R`, or
`docs/reports/2026-backtest.qmd`.

- **Read-only on the money path.** The harness never writes the ledger and is
  never wired into CI. It reads `candidates` / `recommendations` / `results`.
- **Leak-free contract.** `candidates.p` is frozen at decide-time (pre-match);
  joining to `results` introduces no look-ahead. Do not re-derive `p` from
  belief stores in the engine.
- **Reuse `compute_settlement()`** for win/pnl — never reimplement settlement
  boundaries. The engine looks up `tie_threshold` per `(sport, country)` the
  same way `settle_ledger()` does, so backtested settlement is self-consistent
  with the live decider.
- **Stake reconstruction.** Kept bets use the recorded effective `kelly`
  (faithful rolling Kelly — compounding is baked into recorded stakes).
  Counterfactual bets use `kelly_raw * median(kelly/kelly_raw)` per run; these
  are flagged approximate in the report.
- **Output** `data/backtest/` is gitignored and regenerable via
  `Rscript scripts/0Nb_backtest.R`.
- Phase 2 (separate spec): extend history via `0Nr_replay.R` re-fits (football
  only); persist `portfolio_lambda` + calibration into `candidates`.
```

- [ ] **Step 2: Add the CLAUDE.md convention line**

In `CLAUDE.md`, under the "Conventions" section, add after the "Settle layer" block:

```markdown
### Backtest harness (2026-05-30)

`R/backtest-*.R` + `scripts/0Nb_backtest.R` + `docs/reports/2026-backtest.qmd`
replay historical decisions against results to analyse strategy performance
(PnL/ROI/calibration, by market/league/sex). Read-only, never on CI; reuses
`compute_settlement()`. See `.claude/rules/backtest.md`.
```

- [ ] **Step 3: Regenerate NAMESPACE**

Run: `Rscript -e 'devtools::document()'`
Expected: NAMESPACE gains `export(bt_load_universe)`, `export(stake_fixed)`,
`export(stake_rolling)`, `export(bt_run)`, `export(bt_metrics)`,
`export(bt_calibration)`, `export(bt_baselines)`.

- [ ] **Step 4: Run the full backtest test suite**

Run: `Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_dir("tests/testthat", filter = "backtest")'`
Expected: all backtest tests PASS (universe 4, stake 5, engine 3, metrics 4).

- [ ] **Step 5: Run the full package test suite (no regressions)**

Run: `Rscript -e 'devtools::test()'`
Expected: 0 failures across the whole suite (existing 1120+ assertions + new).

- [ ] **Step 6: Commit**

```bash
git add .claude/rules/backtest.md CLAUDE.md NAMESPACE
git commit -m "docs(backtest): rule file, CLAUDE.md note, regenerate NAMESPACE"
```

---

## Self-review notes (author)

- **Spec coverage:** §4.1→T1, §4.2/§5→T2-T3, §4.3→T4, §4.4/§7/§8→T5, §4.5→T6,
  §4.6→T7, §12 (rule file, layout)→T8. §6 caveats surfaced in the report (T7)
  and rule file (T8). §9 output format→T6. §10 testing→T1-T5.
- **`favourites` baseline** (§8) is implemented in T5 via a per-match min-odds
  moneyline slice — covered.
- **Type consistency:** `bt_load_universe` output columns (`bt_universe_cols`)
  flow unchanged into `stake_*` (add `stake/pnl/pool_before`) and `bt_run`
  (adds `win/cum_pnl/pool_after`); `compute_settlement` is fed `odds_placed` +
  `bet_amount` (notional) and only its `win` column is read back.
- **Realisable-subset view** (§6) is data already available (intersect universe
  with `ledger`); the report's caveats note it. A dedicated `realisable`
  strategy filter is a small follow-up, not blocking Phase 1.
