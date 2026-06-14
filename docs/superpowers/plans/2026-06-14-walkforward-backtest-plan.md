# Walk-Forward Backtest Implementation Plan (Subsystem 1)

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build a leak-free walk-forward (out-of-sample) backtest for football_iceland that, for each historical cutoff date `d`, re-fits the model as-of `d`, re-decides from only-pre-`d` odds, and scores the model's OOS probability calibration (Brier + log-loss) against realised results. This is the orthogonal complement to the existing stored-decision backtest (`R/backtest-{universe,engine,...}.R`), which never re-fits. Read-only on the money path; NEVER wired into CI.

**Architecture:** A loop `bt_walkforward(sex, cutoffs, horizon_days, candidate, root)` over cutoff dates. For each `d`: (1) build an isolated `wf_root` tempdir; (2) copy pre-`d` results + a STRICTLY-pre-`d` odds pre-slice + (after the fit) the as-of beliefs into `wf_root`; (3) `fit_league(end_date = d, fit_date = d, seed = as.integer(format(d, "%Y%m%d")), root = wf_root)` (leak boundary `results$match_date <= end_date` already enforced at `model-prepare.R:103`); (4) `decide_league(league_key = "football_iceland", sex, run_date = d, root = wf_root, write = FALSE)` to regenerate candidates from fresh as-of beliefs; (5) restrict the OOS bet set to matches STRICTLY after `d` and within `(d, d + horizon_days]`; (6) settle with `compute_settlement(..., match_date_window_days = 0L, tie_threshold)`; (7) score OOS Brier/log-loss (primary) + reuse `bt_run`/`bt_metrics` for the secondary PnL arm. The two load-bearing NEW pieces are the odds pre-slice (no existing function bounds `scraped_at` from above) and the OOS Brier/log-loss scorers.

**Tech Stack:** R package (devtools / testthat ed.3 / roxygen2), arrow, dplyr, lubridate, cmdstanr (fit), cli. Reuses `R/model-league.R::fit_league`, `R/decide-pipeline.R::decide_league`, `R/decide-odds.R::prepare_odds`, `R/settle.R::compute_settlement`, `R/backtest-engine.R::bt_run`, `R/backtest-metrics.R::bt_metrics`/`bt_calibration`, `R/storage.R::write_table`/`read_table`, `R/round-cutoff.R::compute_round_cutoff_date`, `R/config.R::load_leagues`.

**Spec grounding:** Component C §5.1 (odds pre-slice to `scraped_at <= d + 12h`; ledger_asof; OOS Brier/log-loss primary, PnL secondary). Leak-check guards G1-G11 and ASSERT-* assertions folded in below.

**Conventions (from `.claude/rules/`):** base pipe `|>`; explicit namespacing (`dplyr::`, `lubridate::`, `stats::`); roxygen `@export` + `devtools::document()`; testthat ed.3; `here::here()` for paths; ASCII-only fixtures (football iceland needs no Icelandic strings in fixtures; any non-ASCII uses `\uXXXX`); no comments except roxygen and one-line `# WHY:`; output under `data/backtest/walkforward/` (already gitignored via `data/backtest/`).

---

## File Structure

- Create: `R/backtest-walkforward.R` (engine + scorers + loop)
- Create: `scripts/0Nb_walkforward.R` (CLI; mirrors `scripts/0Nr_replay.R` flag parser + football-only hard stop)
- Test: `tests/testthat/test-backtest-walkforward.R` (pure-function + leak-guard assertions; no Stan)
- Test: `tests/testthat/test-backtest-walkforward-ci-isolation.R` (workflow grep, mirrors `test-placer-ci-isolation.R`)

Names confirmed against conventions: existing engine files are `R/backtest-{engine,metrics,stake,universe}.R`, existing CLI is `scripts/0Nb_backtest.R`, existing isolation tests are `tests/testthat/test-{placer,healthcheck}-ci-isolation.R`. `R/backtest-walkforward.R` + `scripts/0Nb_walkforward.R` + `tests/testthat/test-backtest-walkforward.R` follow these exactly.

---

## Task 1: OOS scoring primitives (Brier + log-loss) — pure, no Stan

**Files:** Create `R/backtest-walkforward.R`; Test `tests/testthat/test-backtest-walkforward.R`

- [ ] **Step 1: Write the failing test**

```r
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
```

- [ ] **Step 2: Run it — expect FAIL (function undefined)**

```
Rscript -e 'devtools::load_all(here::here(), quiet=TRUE); testthat::test_file("tests/testthat/test-backtest-walkforward.R")'
```
Expected: `Error ... could not find function "bt_oos_scores"` / all four FAIL.

- [ ] **Step 3: Minimal implementation**

```r
# R/backtest-walkforward.R
#' @include settle.R config.R decide-pipeline.R model-league.R backtest-engine.R round-cutoff.R
NULL

#' Out-of-sample probability scores: Brier and log-loss over frozen as-of `p`.
#'
#' The PRIMARY walk-forward verdict. Rows with `NA` win (bets that never
#' settled within the horizon) are dropped before scoring. `p` is clamped to
#' `[eps, 1-eps]` so a degenerate 0/1 forecast cannot send log-loss to Inf.
#' @param settled Tibble with numeric `p` and logical `win`.
#' @param eps Clamp bound. Default `1e-6`.
#' @return One-row tibble `(n, brier, log_loss)`; `n = 0` -> NA scores.
#' @export
bt_oos_scores <- function(settled, eps = 1e-6) {
  d <- settled[!is.na(settled$win), , drop = FALSE]
  if (nrow(d) == 0L) {
    return(tibble::tibble(n = 0L, brier = NA_real_, log_loss = NA_real_))
  }
  p <- pmin(pmax(d$p, eps), 1 - eps)
  y <- as.numeric(d$win)
  tibble::tibble(
    n = nrow(d),
    brier = mean((p - y)^2),
    log_loss = -mean(y * log(p) + (1 - y) * log(1 - p))
  )
}
```

- [ ] **Step 4: Run it — expect PASS**

```
Rscript -e 'devtools::load_all(here::here(), quiet=TRUE); testthat::test_file("tests/testthat/test-backtest-walkforward.R")'
```

- [ ] **Step 5: Commit**

```
git -C /Users/brynjolfurjonsson/sports add R/backtest-walkforward.R tests/testthat/test-backtest-walkforward.R
git -C /Users/brynjolfurjonsson/sports commit -m "feat(backtest): OOS Brier + log-loss scorers for walk-forward validator"
```

---

## Task 2: Odds pre-slice — the single load-bearing leak guard (G3/G4, ASSERT-ODDS-1/2/3)

**Files:** Edit `R/backtest-walkforward.R`; Edit `tests/testthat/test-backtest-walkforward.R`

`prepare_odds` (decide-odds.R:56-70) has only a LOWER `scraped_at` bound + `slice_max(scraped_at)` with NO upper bound. `bt_wf_slice_odds(odds, d)` keeps `scraped_at <= as.POSIXct(d) + lubridate::dhours(12)`. Writing the result into `wf_root` makes `decide_league(root = wf_root)` structurally incapable of seeing a post-cutoff snapshot. The cutoff and timestamps are PINNED in the test (not wall-clock) — this is the explicit exception to the time-bomb-date rule.

- [ ] **Step 1: Write the failing test**

```r
test_that("bt_wf_slice_odds keeps only snapshots at or before d + 12h (ASSERT-ODDS-2)", {
  d <- as.Date("2026-05-20")
  odds <- tibble::tibble(
    sport = "football", country = "iceland",
    scraped_at = as.POSIXct(
      c("2026-05-19 14:00:00", "2026-05-20 10:00:00", "2026-05-21 09:00:00"),
      tz = "UTC"
    ),
    match_date = as.Date("2026-05-22"),
    home_team = "A", away_team = "B",
    market = "moneyline", outcome = "home",
    line = NA_real_, odds = c(2.50, 2.40, 1.90)
  )
  out <- bt_wf_slice_odds(odds, d)
  expect_true(all(out$scraped_at <= as.POSIXct("2026-05-20", tz = "UTC") + lubridate::dhours(12)))
  expect_false(any(out$odds == 1.90))
  expect_equal(nrow(out), 2L)
})

test_that("bt_wf_decide selects the pre-cutoff snapshot, never the post-result one (ASSERT-ODDS-1, the mandated regression test)", {
  d <- as.Date("2026-05-20")
  odds <- tibble::tibble(
    sport = "football", country = "iceland",
    scraped_at = as.POSIXct(
      c("2026-05-19 12:00:00", "2026-05-21 12:00:00"),
      tz = "UTC"
    ),
    match_date = as.Date("2026-05-22"),
    home_team = "A", away_team = "B",
    market = "moneyline", outcome = "home",
    line = NA_real_, odds = c(2.50, 1.90)
  )
  wf_root <- withr::local_tempdir()
  write_table(bt_wf_slice_odds(odds, d), "odds", root = wf_root)
  seen <- prepare_odds(
    list(sport = "football", country = "iceland"), "male",
    end_date = d, max_age_hours = 24 * 365 * 10, root = wf_root
  )
  expect_equal(seen$odds, 2.50)
  expect_false(any(seen$odds == 1.90))
  expect_true(all(seen$scraped_at <= as.POSIXct("2026-05-20", tz = "UTC") + lubridate::dhours(12)))
})

test_that("bt_wf_slice_odds + huge max_age_hours still returns old historical snapshots (ASSERT-ODDS-3, guards the silent-empty trap)", {
  d <- as.Date("2026-05-20")
  odds <- tibble::tibble(
    sport = "football", country = "iceland",
    scraped_at = as.POSIXct("2026-05-18 09:00:00", tz = "UTC"),
    match_date = as.Date("2026-05-22"),
    home_team = "A", away_team = "B",
    market = "moneyline", outcome = "home",
    line = NA_real_, odds = 2.50
  )
  wf_root <- withr::local_tempdir()
  write_table(bt_wf_slice_odds(odds, d), "odds", root = wf_root)
  seen <- prepare_odds(
    list(sport = "football", country = "iceland"), "male",
    end_date = d, max_age_hours = bt_wf_max_age_hours(), root = wf_root
  )
  expect_equal(nrow(seen), 1L)
  expect_equal(seen$odds, 2.50)
})
```

- [ ] **Step 2: Run it — expect FAIL**

```
Rscript -e 'devtools::load_all(here::here(), quiet=TRUE); testthat::test_file("tests/testthat/test-backtest-walkforward.R")'
```
Expected: `could not find function "bt_wf_slice_odds"` / `bt_wf_max_age_hours`.

- [ ] **Step 3: Minimal implementation**

```r
#' Upper-bound the odds snapshots a decision at cutoff `d` may see.
#'
#' G3/G4: `prepare_odds` bounds `scraped_at` from below only and takes
#' `slice_max(scraped_at)`. Pre-slicing here to `scraped_at <= d + 12h` and
#' writing the result into the isolated `wf_root` makes the decider unable to
#' select a closing/post-result snapshot. Spec Component C §5.1 step 3.
#' @param odds Tibble from `read_table("odds")` (has `scraped_at`).
#' @param d Cutoff date.
#' @return `odds` restricted to pre-cutoff snapshots.
#' @export
bt_wf_slice_odds <- function(odds, d) {
  if (nrow(odds) == 0L) return(odds)
  cutoff <- as.POSIXct(format(as.Date(d)), tz = "UTC") + lubridate::dhours(12)
  odds[odds$scraped_at <= cutoff, , drop = FALSE]
}

#' max_age_hours large enough that prepare_odds' lower bound never drops a
#' legitimately-old historical snapshot once the upper bound is enforced by
#' bt_wf_slice_odds (G3). ~10 years.
#' @return Numeric hours.
#' @export
bt_wf_max_age_hours <- function() 24 * 365 * 10
```

- [ ] **Step 4: Run it — expect PASS**

```
Rscript -e 'devtools::load_all(here::here(), quiet=TRUE); testthat::test_file("tests/testthat/test-backtest-walkforward.R")'
```

- [ ] **Step 5: Commit**

```
git -C /Users/brynjolfurjonsson/sports add R/backtest-walkforward.R tests/testthat/test-backtest-walkforward.R
git -C /Users/brynjolfurjonsson/sports commit -m "feat(backtest): odds pre-slice + regression test that post-cutoff snapshot is never selected"
```

---

## Task 3: OOS window restriction — strict same-day leak guard (G1/G2, ASSERT-SAMEDAY-1/2/3)

**Files:** Edit `R/backtest-walkforward.R`; Edit `tests/testthat/test-backtest-walkforward.R`

`compute_round_cutoff_date` returns the date round N COMPLETES — round-N matches are played ON the cutoff. `prepare_data` trains on `results$match_date <= end_date` (inclusive) while the prediction/odds windows are `>= end_date` (inclusive), so a day-`d` match leaks into both training and the bet set. `bt_wf_filter_oos(candidates, d, horizon_days)` keeps only `match_date > d & match_date <= d + horizon_days` (STRICT lower bound), and `bt_wf_training_disjoint(candidates, results, d)` asserts the OOS bet match-set is disjoint from the training match-set.

- [ ] **Step 1: Write the failing test**

```r
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
```

- [ ] **Step 2: Run it — expect FAIL**

```
Rscript -e 'devtools::load_all(here::here(), quiet=TRUE); testthat::test_file("tests/testthat/test-backtest-walkforward.R")'
```

- [ ] **Step 3: Minimal implementation**

```r
#' Restrict OOS candidates to matches STRICTLY after the cutoff, within horizon.
#'
#' G1/G2: neutralises the non-strict `schedules >= d` / `odds match_date >= d`
#' operators even if a cutoff coincides with a match day. A day-`d` match is in
#' the (inclusive) training set, so it must never be a bet.
#' @param candidates Tibble with `match_date`.
#' @param d Training cutoff date.
#' @param horizon_days OOS window length.
#' @return Candidates with `match_date > d & match_date <= d + horizon_days`.
#' @export
bt_wf_filter_oos <- function(candidates, d, horizon_days) {
  if (nrow(candidates) == 0L) return(candidates)
  d <- as.Date(d)
  hi <- d + as.integer(horizon_days)
  candidates[candidates$match_date > d & candidates$match_date <= hi, , drop = FALSE]
}

#' Assert the OOS bet match-set is disjoint from the training match-set (G1).
#'
#' Training = results with `match_date <= d` (what prepare_data trained on).
#' @param oos OOS candidates (`match_date`, `home_team`, `away_team`).
#' @param results Full results store.
#' @param d Training cutoff.
#' @return `TRUE` if disjoint.
#' @export
bt_wf_training_disjoint <- function(oos, results, d) {
  d <- as.Date(d)
  trn <- results[results$match_date <= d, , drop = FALSE]
  tkey <- paste(trn$match_date, trn$home_team, trn$away_team, sep = "\r")
  okey <- paste(oos$match_date, oos$home_team, oos$away_team, sep = "\r")
  length(intersect(tkey, okey)) == 0L
}
```

- [ ] **Step 4: Run it — expect PASS**

```
Rscript -e 'devtools::load_all(here::here(), quiet=TRUE); testthat::test_file("tests/testthat/test-backtest-walkforward.R")'
```

- [ ] **Step 5: Commit**

```
git -C /Users/brynjolfurjonsson/sports add R/backtest-walkforward.R tests/testthat/test-backtest-walkforward.R
git -C /Users/brynjolfurjonsson/sports commit -m "feat(backtest): strict OOS window + training-disjointness guard (same-day leak)"
```

---

## Task 4: ledger_asof view + reschedule/no-odds guard (G5/G8, ASSERT-CALIB-1, ASSERT-RESCHEDULE-1)

**Files:** Edit `R/backtest-walkforward.R`; Edit `tests/testthat/test-backtest-walkforward.R`

`compute_calibrations` reads the FULL settled ledger with no date filter — at cutoff `d` it would peek at bets that settled after `d`. `bt_wf_ledger_asof(ledger, d)` keeps only rows whose match resolved strictly before `d`. G8: `bt_wf_require_pre_cutoff_odds(candidates, sliced_odds)` drops any OOS candidate with no pre-cutoff odds snapshot, so a phantom rescheduled fixture (whose only stored date is a post-`d` revision) cannot enter the bet set.

- [ ] **Step 1: Write the failing test**

```r
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
```

- [ ] **Step 2: Run it — expect FAIL**

```
Rscript -e 'devtools::load_all(here::here(), quiet=TRUE); testthat::test_file("tests/testthat/test-backtest-walkforward.R")'
```

- [ ] **Step 3: Minimal implementation**

```r
#' As-of ledger view: rows whose match resolved strictly before `d` (G5).
#'
#' Feeds calibration so `compute_calibrations` cannot peek at bets that
#' settled after the cutoff. Approximates settle-date by `match_date` (the
#' ledger has no settle timestamp); `match_date < d` is the conservative cut.
#' @param ledger Ledger tibble (or empty).
#' @param d Cutoff date.
#' @return Settled ledger rows with `match_date < d`.
#' @export
bt_wf_ledger_asof <- function(ledger, d) {
  if (is.null(ledger) || nrow(ledger) == 0L) return(ledger[0, , drop = FALSE])
  d <- as.Date(d)
  keep <- !is.na(ledger$settled) & ledger$settled & ledger$match_date < d
  ledger[keep, , drop = FALSE]
}

#' Drop OOS candidates with no pre-cutoff odds snapshot (G8).
#'
#' A fixture whose only stored date is a post-`d` schedule revision has no
#' pre-cutoff odds and is not a bettable as-of match. Keying on the selection
#' identity sidesteps phantom rescheduled fixtures.
#' @param candidates OOS candidates.
#' @param sliced_odds Output of `bt_wf_slice_odds`.
#' @return Candidates that have a matching pre-cutoff odds snapshot.
#' @export
bt_wf_require_pre_cutoff_odds <- function(candidates, sliced_odds) {
  if (nrow(candidates) == 0L || nrow(sliced_odds) == 0L) {
    return(candidates[0, , drop = FALSE])
  }
  ok <- paste(sliced_odds$match_date, sliced_odds$home_team,
    sliced_odds$away_team, sliced_odds$market, sliced_odds$outcome,
    sliced_odds$line,
    sep = "\r"
  )
  ck <- paste(candidates$match_date, candidates$home_team,
    candidates$away_team, candidates$market, candidates$outcome,
    candidates$line,
    sep = "\r"
  )
  candidates[ck %in% ok, , drop = FALSE]
}
```

- [ ] **Step 4: Run it — expect PASS**

```
Rscript -e 'devtools::load_all(here::here(), quiet=TRUE); testthat::test_file("tests/testthat/test-backtest-walkforward.R")'
```

- [ ] **Step 5: Commit**

```
git -C /Users/brynjolfurjonsson/sports add R/backtest-walkforward.R tests/testthat/test-backtest-walkforward.R
git -C /Users/brynjolfurjonsson/sports commit -m "feat(backtest): as-of ledger calibration view + pre-cutoff-odds requirement (G5/G8)"
```

---

## Task 5: Per-cutoff orchestrator `bt_walkforward_cutoff` — wf_root isolation (G6/G7/G9, ASSERT-UNIVERSE-1, ASSERT-OUTPUT-ISOLATION-1)

**Files:** Edit `R/backtest-walkforward.R`; Edit `tests/testthat/test-backtest-walkforward.R`

This wires the guards together. The fit (`fit_league`) is gated behind `skip_on_ci()` and a cmdstan-availability skip in tests; the test injects a fake `decide_fn` so the pure orchestration is testable WITHOUT Stan. The orchestrator MUST: build a fresh `wf_root` tempdir (G6 — never the live root); seed pre-`d` results + pre-sliced odds + (the caller-provided) beliefs into it; call `decide_fn(root = wf_root, write = FALSE)`; apply `bt_wf_filter_oos` + `bt_wf_require_pre_cutoff_odds`; settle with `compute_settlement(match_date_window_days = 0L, tie_threshold)` (G9); return the scored frame with `cutoff` + `run_id` columns (ASSERT-UNIVERSE-1: `run_id` derives from the cutoff, not a persisted decision).

- [ ] **Step 1: Write the failing test**

```r
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
```

- [ ] **Step 2: Run it — expect FAIL**

```
Rscript -e 'devtools::load_all(here::here(), quiet=TRUE); testthat::test_file("tests/testthat/test-backtest-walkforward.R")'
```

- [ ] **Step 3: Minimal implementation**

```r
#' Score one walk-forward cutoff into an isolated tempdir.
#'
#' G6: builds a fresh `wf_root` tempdir, seeds pre-`d` results + pre-sliced
#' odds into it; the live root is never written. `decide_fn` defaults to the
#' real fit+decide closure (`bt_wf_default_decide`), but tests inject a fake to
#' exercise the pure orchestration without Stan. G7: the universe is the
#' decide return, never `bt_load_universe`. G9: settlement uses
#' `match_date_window_days = 0L`.
#' @param sex,d,horizon_days Cutoff parameters.
#' @param results,odds,ledger Pre-loaded stores (`ledger` may be NULL).
#' @param live_root The production data root (read-only here; existence guard).
#' @param decide_fn Closure `(root, run_date, sex, ledger_asof)` -> candidate
#'   tibble. Default fits as-of then decides.
#' @param tie_threshold Per-(sport,country) push band (football = 0).
#' @return Scored OOS tibble (`cutoff`, `run_id`, candidate cols, `win`).
#' @export
bt_walkforward_cutoff <- function(sex, d, horizon_days,
                                  results, odds, ledger = NULL,
                                  live_root = here::here("data"),
                                  decide_fn = bt_wf_default_decide,
                                  tie_threshold = 0) {
  d <- as.Date(d)
  wf_root <- withr::local_tempdir(.local_envir = parent.frame())

  pre_results <- results[results$match_date <= (d + as.integer(horizon_days)), , drop = FALSE]
  bt_wf_seed_results(pre_results, wf_root)

  sliced <- bt_wf_slice_odds(odds, d)
  if (nrow(sliced) > 0L) write_table(sliced, "odds", root = wf_root)

  led_asof <- bt_wf_ledger_asof(ledger, d)

  cands <- decide_fn(
    root = wf_root, run_date = d, sex = sex, ledger_asof = led_asof
  )
  cands <- bt_wf_filter_oos(cands, d, horizon_days)
  cands <- bt_wf_require_pre_cutoff_odds(cands, sliced)
  if (nrow(cands) == 0L) {
    out <- cands
    out$cutoff <- as.Date(character())
    out$run_id <- as.POSIXct(character(), tz = "UTC")
    out$win <- logical()
    return(out)
  }

  bets <- cands
  bets$sport <- "football"; bets$country <- "iceland"; bets$sex <- sex
  bets$odds_placed <- bets$odds
  bets$bet_amount <- 1
  bets$settled <- FALSE
  bets$win <- NA
  bets$pnl <- NA_real_
  settled <- compute_settlement(bets, results,
    match_date_window_days = 0L, tie_threshold = tie_threshold
  )
  cands$win <- settled$win
  cands$cutoff <- d
  cands$run_id <- as.POSIXct(format(d), tz = "UTC")
  cands
}

#' Seed a results store into an isolated tempdir for prepare_data.
#' @noRd
bt_wf_seed_results <- function(results, root) {
  if (nrow(results) == 0L) return(invisible(NULL))
  res_root <- file.path(root, "facts", "results")
  fs::dir_create(res_root, recurse = TRUE)
  arrow::write_dataset(results,
    path = res_root, format = "parquet",
    partitioning = c("sport", "country", "sex", "season"),
    existing_data_behavior = "overwrite"
  )
  invisible(NULL)
}

#' Default decide closure: fit as-of `d` into `wf_root`, then decide (G7).
#' Never called by the pure tests (which inject a fake). Skipped on CI.
#' @noRd
bt_wf_default_decide <- function(root, run_date, sex, ledger_asof = NULL) {
  fit_league(
    league_key = "football_iceland", sex = sex,
    fit_date = run_date, end_date = run_date,
    seed = as.integer(format(run_date, "%Y%m%d")),
    schedule_horizon_days = 200L, root = root
  )
  recs <- decide_league(
    league_key = "football_iceland", sex = sex,
    run_date = run_date, root = root, write = FALSE
  )
  recs
}
```

- [ ] **Step 4: Run it — expect PASS**

```
Rscript -e 'devtools::load_all(here::here(), quiet=TRUE); testthat::test_file("tests/testthat/test-backtest-walkforward.R")'
```

- [ ] **Step 5: Commit**

```
git -C /Users/brynjolfurjonsson/sports add R/backtest-walkforward.R tests/testthat/test-backtest-walkforward.R
git -C /Users/brynjolfurjonsson/sports commit -m "feat(backtest): per-cutoff orchestrator with wf_root isolation + same-day leak guard end-to-end"
```

---

## Task 6: `bt_walkforward` loop + Brier/PnL aggregation

**Files:** Edit `R/backtest-walkforward.R`; Edit `tests/testthat/test-backtest-walkforward.R`

Loops cutoffs, binds the scored frames, computes OOS Brier/log-loss (primary, via `bt_oos_scores`) and reuses `bt_metrics` on the secondary PnL arm. Tested with the injected fake decide so no Stan runs.

- [ ] **Step 1: Write the failing test**

```r
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
```

- [ ] **Step 2: Run it — expect FAIL**

```
Rscript -e 'devtools::load_all(here::here(), quiet=TRUE); testthat::test_file("tests/testthat/test-backtest-walkforward.R")'
```

- [ ] **Step 3: Minimal implementation**

```r
#' Walk-forward OOS validator over a set of cutoff dates.
#'
#' For each `d` in `cutoffs`, re-fit as-of `d`, decide from pre-`d` odds, and
#' score the OOS matches in `(d, d + horizon_days]`. PRIMARY verdict is OOS
#' Brier/log-loss (`bt_oos_scores`); secondary is PnL via `bt_metrics`.
#' football_iceland only (the engine stays general — widen when handball
#' resumes). Each cutoff is a full Stan fit: run detached.
#' @param sex "male" or "female".
#' @param cutoffs Vector of cutoff Dates (e.g. `compute_round_cutoff_date` per
#'   round, offset to be strictly pre-round per G1).
#' @param horizon_days OOS window length. Default 14.
#' @param results,odds,ledger Pre-loaded stores (NULL ledger -> neutral calib).
#' @param live_root Production data root (read-only).
#' @param decide_fn Injected for tests; default fits+decides.
#' @param tie_threshold Per-(sport,country) push band.
#' @return `list(bets = <scored OOS tibble>, scores = <bt_oos_scores>,
#'   pnl = <bt_metrics>)`.
#' @export
bt_walkforward <- function(sex, cutoffs, horizon_days = 14L,
                           results, odds, ledger = NULL,
                           live_root = here::here("data"),
                           decide_fn = bt_wf_default_decide,
                           tie_threshold = 0) {
  scored <- list()
  for (d in as.list(as.Date(cutoffs))) {
    scored[[length(scored) + 1L]] <- bt_walkforward_cutoff(
      sex = sex, d = d, horizon_days = horizon_days,
      results = results, odds = odds, ledger = ledger,
      live_root = live_root, decide_fn = decide_fn,
      tie_threshold = tie_threshold
    )
  }
  bets <- dplyr::bind_rows(scored)
  pnl <- if (nrow(bets) > 0L) {
    b <- bets
    b$stake <- b$bet_amount %||% 1
    b$pnl <- ifelse(b$win, b$stake * (b$odds - 1), -b$stake)
    bt_metrics(b)
  } else {
    tibble::tibble()
  }
  list(bets = bets, scores = bt_oos_scores(bets), pnl = pnl)
}

`%||%` <- function(x, y) if (is.null(x)) y else x
```

- [ ] **Step 4: Run it — expect PASS**

```
Rscript -e 'devtools::load_all(here::here(), quiet=TRUE); testthat::test_file("tests/testthat/test-backtest-walkforward.R")'
```

- [ ] **Step 5: Commit**

```
git -C /Users/brynjolfurjonsson/sports add R/backtest-walkforward.R tests/testthat/test-backtest-walkforward.R
git -C /Users/brynjolfurjonsson/sports commit -m "feat(backtest): bt_walkforward loop + primary OOS Brier/log-loss aggregation"
```

---

## Task 7: CI-isolation test (G10, ASSERT-CI-ISOLATION-1)

**Files:** Create `tests/testthat/test-backtest-walkforward-ci-isolation.R`

Mirrors `test-placer-ci-isolation.R`: fixed-string grep over every workflow yml, `fail()` on any walk-forward symbol — the multi-hour Stan sweep must never run on CI.

- [ ] **Step 1: Write the test (passes immediately — no workflow references the harness yet; this is a guard, not a red-then-green)**

```r
# tests/testthat/test-backtest-walkforward-ci-isolation.R
#
# G10: the walk-forward harness is a multi-hour Stan sweep, read-only, never
# on CI. Fails the build if any workflow references its symbols.

test_that("no GitHub Actions workflow references the walk-forward harness", {
  workflow_dir <- here::here(".github", "workflows")
  if (!dir.exists(workflow_dir)) {
    testthat::skip("no .github/workflows yet")
  }
  yml_files <- list.files(workflow_dir, pattern = "\\.ya?ml$", full.names = TRUE)
  if (length(yml_files) == 0L) {
    testthat::skip("no workflow files yet")
  }
  forbidden <- c(
    "R/backtest-walkforward",
    "bt_walkforward",
    "0Nb_walkforward",
    "bt_walkforward_cutoff",
    "bt_wf_default_decide"
  )
  failures <- character(0)
  for (f in yml_files) {
    contents <- readLines(f, warn = FALSE)
    for (token in forbidden) {
      hit <- grep(token, contents, fixed = TRUE)
      if (length(hit) > 0L) {
        failures <- c(failures, sprintf(
          "%s: line %d references forbidden token %s",
          basename(f), hit[1L], shQuote(token)
        ))
      }
    }
  }
  if (length(failures) > 0L) {
    fail(paste(
      "CI workflow(s) reference the walk-forward harness.",
      "It is a multi-hour Stan sweep and must remain local-only.",
      paste("  -", failures, collapse = "\n"),
      sep = "\n"
    ))
  }
  expect_true(TRUE)
})
```

- [ ] **Step 2: Run it — expect PASS (no workflow references the harness)**

```
Rscript -e 'devtools::load_all(here::here(), quiet=TRUE); testthat::test_file("tests/testthat/test-backtest-walkforward-ci-isolation.R")'
```

- [ ] **Step 3: Commit**

```
git -C /Users/brynjolfurjonsson/sports add tests/testthat/test-backtest-walkforward-ci-isolation.R
git -C /Users/brynjolfurjonsson/sports commit -m "test(backtest): CI-isolation guard for the walk-forward Stan sweep"
```

---

## Task 8: CLI `scripts/0Nb_walkforward.R` (mirrors 0Nr_replay.R)

**Files:** Create `scripts/0Nb_walkforward.R`

Football-only hard stop (replay.R / 0Nr_replay.R:56-63 pattern). Flags `--sex --season --per-round --as-of --horizon`. Cutoffs come from `compute_round_cutoff_date` per round (offset strictly pre-round per G1) or a single `--as-of`. Loads results/odds/ledger from the live root (read-only), passes them to `bt_walkforward`, writes `data/backtest/walkforward/` (gitignored). Detached run per the multi-hour fit.

- [ ] **Step 1: Write the script**

```r
#!/usr/bin/env Rscript
# scripts/0Nb_walkforward.R --
# Leak-free walk-forward OOS validator for football iceland. Re-fits the model
# as-of each cutoff, re-decides from pre-cutoff odds, scores OOS Brier/log-loss
# (primary) + PnL (secondary). Read-only on the money path; NEVER on CI.
#
# Each cutoff is a full Stan fit (~hours). Run detached:
#   nohup Rscript scripts/0Nb_walkforward.R --sex male --season 2026 --per-round \
#       > /tmp/wf.log 2>&1 & disown

invisible(Sys.setlocale("LC_ALL", "en_US.UTF-8"))
options(width = 120)
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
get_flag <- function(name, default = NULL) {
  i <- which(args == paste0("--", name))
  if (length(i) == 0L) default else args[[i + 1L]]
}
has_flag <- function(name) paste0("--", name) %in% args

sex <- get_flag("sex")
season_str <- get_flag("season")
as_of_str <- get_flag("as-of")
per_round <- has_flag("per-round")
horizon_days <- as.integer(get_flag("horizon", "14"))

if (is.null(sex)) stop("--sex required (male or female)", call. = FALSE)

# Football iceland only (engine stays general; handball is a deliberate later
# scope flip per spec Component C / plan P2).
league_key <- "football_iceland"
leagues <- load_leagues()
league <- leagues[[league_key]]

results <- read_table("results",
  filter = list(sport = league$sport, country = league$country, sex = sex)
)
odds <- read_table("odds",
  filter = list(sport = league$sport, country = league$country)
)
ledger <- tryCatch(read_table("ledger"), error = function(e) NULL)
tie_threshold <- league$betting$scoring$tie_threshold %||% 0

# G1: cutoffs are STRICTLY pre-round. For round N, fit through the day BEFORE
# round N's first kickoff so round-N matches are scored OOS, never trained on.
cutoffs <- if (isTRUE(per_round)) {
  if (is.null(season_str)) stop("--per-round requires --season YYYY", call. = FALSE)
  season <- as.integer(season_str)
  out <- list()
  for (n in seq_len(50L)) {
    d_complete <- suppressWarnings(suppressMessages(
      compute_round_cutoff_date(results, season = season, round_cutoff = n, quiet = TRUE)
    ))
    if (is.null(d_complete)) break
    round_dates <- results$match_date[results$season == season]
    first_kick_n <- min(round_dates[round_dates > (if (n == 1L) as.Date("1900-01-01") else out_prev %||% as.Date("1900-01-01"))], na.rm = TRUE)
    out[[length(out) + 1L]] <- d_complete - 1L
    out_prev <- d_complete
  }
  if (length(out) == 0L) stop("No completed rounds for season ", season, call. = FALSE)
  do.call(c, out)
} else {
  if (is.null(as_of_str)) stop("--as-of YYYY-MM-DD required (or --per-round --season YYYY)", call. = FALSE)
  d <- as.Date(as_of_str)
  if (is.na(d)) stop("--as-of: could not parse '", as_of_str, "'", call. = FALSE)
  d
}

cli::cli_h1("Walk-forward {league_key}/{sex}: {length(cutoffs)} cutoff(s), horizon={horizon_days}d")
wf <- bt_walkforward(
  sex = sex, cutoffs = cutoffs, horizon_days = horizon_days,
  results = results, odds = odds, ledger = ledger,
  tie_threshold = tie_threshold
)

out_dir <- here::here("data", "backtest", "walkforward")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
arrow::write_parquet(wf$bets, file.path(out_dir, paste0("bets_", sex, ".parquet")))
arrow::write_parquet(wf$scores, file.path(out_dir, paste0("scores_", sex, ".parquet")))
print(wf$scores)
print(wf$pnl)
cli::cli_alert_success("Walk-forward complete: {nrow(wf$bets)} OOS bets scored")
```

- [ ] **Step 2: Smoke-check the script parses + the CLI hard-stop on a missing flag (no Stan run)**

```
Rscript -e 'parse(file = here::here("scripts", "0Nb_walkforward.R"))' && echo "parses OK"
Rscript scripts/0Nb_walkforward.R 2>&1 | grep -q "\-\-sex required" && echo "hard-stop OK"
```
Expected: `parses OK` and `hard-stop OK`.

- [ ] **Step 3: Run the full suite + CI-isolation to confirm nothing regressed**

```
Rscript -e 'devtools::load_all(here::here(), quiet=TRUE); testthat::test_file("tests/testthat/test-backtest-walkforward.R"); testthat::test_file("tests/testthat/test-backtest-walkforward-ci-isolation.R"); testthat::test_file("tests/testthat/test-placer-ci-isolation.R")'
```

- [ ] **Step 4: Commit**

```
git -C /Users/brynjolfurjonsson/sports add scripts/0Nb_walkforward.R
git -C /Users/brynjolfurjonsson/sports commit -m "feat(backtest): 0Nb_walkforward CLI (football-only, detached, gitignored output)"
```

---

## Task 9: Roxygen + final verification

**Files:** regenerate NAMESPACE/man.

- [ ] **Step 1: Document**

```
Rscript -e 'devtools::document()'
```

- [ ] **Step 2: Full test suite (confirm no regression across the package)**

```
Rscript -e 'devtools::load_all(here::here(), quiet=TRUE); testthat::test_dir("tests/testthat", filter = "backtest")'
```
Expected: all walk-forward + existing backtest tests pass; 0 failures.

- [ ] **Step 3: Confirm the harness wrote nothing under data/decisions during the test run (ASSERT-OUTPUT-ISOLATION-1, belt-and-braces)**

```
git -C /Users/brynjolfurjonsson/sports status --porcelain data/decisions/
```
Expected: empty output (no decisions/ changes from the test run).

- [ ] **Step 4: Commit docs**

```
git -C /Users/brynjolfurjonsson/sports add NAMESPACE man/
git -C /Users/brynjolfurjonsson/sports commit -m "docs(backtest): roxygen for walk-forward validator exports"
```
