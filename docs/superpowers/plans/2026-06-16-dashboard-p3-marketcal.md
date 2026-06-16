# Dashboard P3 — Line-Softness / Market-Calibration Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: superpowers:executing-plans. Steps use checkbox (`- [ ]`) syntax.

**Goal:** Build the "where are the *bad odds*" engine — treat the de-vigged Lengjan line as a competing forecaster and map where it is biased (the CLV replacement for a static monopoly line). New `R/backtest-marketcal.R`: market calibration, directional bias, model-vs-line disagreement (stratified), and a line-stability monitor confirming the line doesn't move.

**Architecture:** Pure, read-only functions over the de-vigged `bt_devig()` output (`p`, `q_market`, `y`, `outcome`, `market`) and the raw `odds` store. Reuses P2's `bt_calibration_bands`. TDD.

**Tech Stack:** R (base pipe), `dplyr`, `tibble`, `testthat` 3, roxygen2.

**Reference (spec):** `docs/superpowers/specs/2026-06-16-model-quality-dashboard-design.md` §6.4.

**Verified:** `bt_devig()` returns `(p, q_market, y, overround, market, outcome, line, sex, match keys)`. `odds` store columns: `scraped_at, match_date, home_team, away_team, market, outcome, line, odds, sport, country, scraped_date` (no `sex`). `bt_calibration_bands(settled, n_bins, by, conf)` exists (P2).

---

## File structure
- **Create** `R/backtest-marketcal.R` — `bt_market_calibration`, `bt_market_bias`, `bt_disagreement`, `bt_line_stability`.
- **Create** `tests/testthat/test-backtest-marketcal.R`.

---

## Task 1: Market calibration (is the de-vigged line itself calibrated?)

**Files:** Create `R/backtest-marketcal.R`, `tests/testthat/test-backtest-marketcal.R`.

- [ ] **Step 1: Failing test**

```r
# tests/testthat/test-backtest-marketcal.R

mc_fixture <- function() {
  # de-vig output shape: q_market is the margin-free line prob, y the outcome.
  tibble::tibble(
    sex = "male", market = "moneyline",
    match_date = as.Date("2026-05-01") + rep(0:3, each = 3),
    home_team = rep(c("A", "C", "E", "G"), each = 3),
    away_team = rep(c("B", "D", "F", "H"), each = 3),
    outcome = rep(c("home", "draw", "away"), 4),
    p = rep(c(0.5, 0.3, 0.2), 4),
    q_market = rep(c(0.55, 0.25, 0.2), 4),
    y = c(1,0,0, 0,1,0, 1,0,0, 0,0,1),
    overround = 1.05
  )
}

test_that("bt_market_calibration reports the reliability of q_market (not the model p)", {
  cal <- bt_market_calibration(mc_fixture(), n_bins = 5)
  expect_true(all(c("mean_p", "realised_freq", "n", "lo", "hi", "band_lo", "band_hi") %in% names(cal)))
  # mean_p here is the binned q_market, so it must come from q_market's support,
  # not the model p (which has a 0.5 mass the market lacks).
  expect_true(all(cal$mean_p %in% round(unique(mc_fixture()$q_market), 6) |
    abs(cal$mean_p - 0.55) < 1e-6 | abs(cal$mean_p - 0.25) < 1e-6 | abs(cal$mean_p - 0.2) < 1e-6))
  expect_equal(sum(cal$n), 12L)
})
```

- [ ] **Step 2: Run — expect FAIL** (`could not find function "bt_market_calibration"`)

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-backtest-marketcal.R")'`

- [ ] **Step 3: Implement**

```r
# R/backtest-marketcal.R
#' Line-softness diagnostics: treat the de-vigged Lengjan line as a competing
#' forecaster and map where it is biased (the CLV replacement for a static line).
#' @importFrom rlang .data
NULL

#' Calibration reliability of the de-vigged market probability `q_market`.
#'
#' The CLV-replacement for a non-moving monopoly line: a static line cannot show
#' closing-line value, but a *biased* one shows up as miscalibration of `q_market`
#' against outcomes. Reuses [bt_calibration_bands()] on `(p = q_market, win = y)`.
#' @param scored Output of [bt_devig()] (`q_market`, `y`).
#' @param n_bins Probability bins. Default 10.
#' @param by Optional grouping columns.
#' @param conf Central mass for the intervals. Default 0.9.
#' @return [bt_calibration_bands()] columns; `mean_p` is the binned `q_market`.
#' @export
bt_market_calibration <- function(scored, n_bins = 10, by = NULL, conf = 0.9) {
  if (nrow(scored) == 0L) {
    return(tibble::tibble())
  }
  remap <- scored
  remap$p <- scored$q_market
  remap$win <- scored$y
  bt_calibration_bands(remap, n_bins = n_bins, by = by, conf = conf)
}
```

- [ ] **Step 4: Run — expect PASS**
- [ ] **Step 5: Commit** `git commit R/backtest-marketcal.R tests/testthat/test-backtest-marketcal.R -m "feat(marketcal): calibration of the de-vigged line"`

---

## Task 2: Directional market bias (draw / over-under / favourite under-pricing)

**Files:** Modify `R/backtest-marketcal.R`, extend test file.

- [ ] **Step 1: Failing test**

```r
# append to tests/testthat/test-backtest-marketcal.R

test_that("bt_market_bias aggregates realised-minus-q_market bias per outcome", {
  # Draw outcome priced at q=0.25 but realised 0.5 (2 of 4) -> the line under-prices draws.
  scored <- tibble::tibble(
    market = "moneyline", outcome = rep(c("home", "draw", "away"), 4),
    q_market = rep(c(0.5, 0.25, 0.25), 4),
    y = c(1,0,0, 0,1,0, 1,0,0, 0,1,0)
  )
  bias <- bt_market_bias(scored, by = "market")
  draw <- bias[bias$outcome == "draw", ]
  expect_equal(draw$mean_q, 0.25)
  expect_equal(draw$realised, 0.5)
  expect_equal(draw$bias, 0.25) # realised - mean_q; >0 == line under-prices draws
  expect_equal(draw$n, 4L)
})
```

- [ ] **Step 2: Run — expect FAIL** (`could not find function "bt_market_bias"`)

- [ ] **Step 3: Implement** (append):

```r
#' Directional market bias: realised rate minus the line's implied probability,
#' per outcome type.
#'
#' `bias > 0` means the outcome happens MORE often than the de-vigged line implies
#' (the line under-prices it -- a value pocket). Surfaces the classic soft-line
#' tells: draw under-pricing, over/under skew, favourite-longshot bias.
#' @param scored Output of [bt_devig()].
#' @param by Grouping columns (the outcome is always added). Default "market".
#' @return `(<by..>, outcome, n, mean_q, realised, bias)`.
#' @export
bt_market_bias <- function(scored, by = "market") {
  if (nrow(scored) == 0L) {
    return(tibble::tibble())
  }
  scored |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(by, "outcome")))) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean_q = mean(.data$q_market),
      realised = mean(.data$y),
      bias = mean(.data$y) - mean(.data$q_market),
      .groups = "drop"
    )
}
```

- [ ] **Step 4: Run — expect PASS**
- [ ] **Step 5: Commit** `git commit R/backtest-marketcal.R tests/testthat/test-backtest-marketcal.R -m "feat(marketcal): directional market bias per outcome"`

---

## Task 3: Model-vs-line disagreement, stratified (who is right where they diverge?)

**Files:** Modify `R/backtest-marketcal.R`, extend test file.

- [ ] **Step 1: Failing test**

```r
# append to tests/testthat/test-backtest-marketcal.R

test_that("bt_disagreement bands gap = p - q_market and reports who is right", {
  scored <- tibble::tibble(
    market = "moneyline",
    p =        c(0.80, 0.50, 0.20, 0.51),
    q_market = c(0.50, 0.50, 0.50, 0.50),
    y =        c(1,    0,    0,    1)
  )
  d <- bt_disagreement(scored)
  expect_true(all(c("band", "n", "mean_p", "mean_q", "realised") %in% names(d)))
  # gap +0.30 -> "model>>mkt"; gaps 0 and +0.01 -> "agree"; -0.30 -> "model<<mkt".
  expect_true("model>>mkt" %in% d$band)
  expect_true("model<<mkt" %in% d$band)
  agree <- d[d$band == "agree", ]
  expect_equal(agree$n, 2L)
})

test_that("bt_disagreement accepts a `by` stratifier", {
  scored <- tibble::tibble(
    market = c("moneyline", "total"),
    p = c(0.8, 0.2), q_market = c(0.5, 0.5), y = c(1, 0)
  )
  d <- bt_disagreement(scored, by = "market")
  expect_true(all(c("market", "band") %in% names(d)))
})
```

- [ ] **Step 2: Run — expect FAIL** (`could not find function "bt_disagreement"`)

- [ ] **Step 3: Implement** (append):

```r
#' Band the model-vs-line gap `p - q_market` and report who is right per band.
#'
#' Where the model and the de-vigged line disagree most, whose probability tracks
#' the realised rate? If the market is right in a regime where they diverge, that
#' regime is a model weakness with an external second opinion.
#' @param scored Output of [bt_devig()].
#' @param by Optional grouping columns (the band is always added).
#' @param breaks Gap cut points. Default `c(-1, -.1, -.03, .03, .1, 1)`.
#' @return `(<by..>, band, n, mean_p, mean_q, realised)`.
#' @export
bt_disagreement <- function(scored, by = NULL,
                            breaks = c(-1, -0.1, -0.03, 0.03, 0.1, 1)) {
  if (nrow(scored) == 0L) {
    return(tibble::tibble())
  }
  d <- scored
  d$gap <- d$p - d$q_market
  d$band <- cut(d$gap,
    breaks = breaks, include.lowest = TRUE,
    labels = c("model<<mkt", "model<mkt", "agree", "model>mkt", "model>>mkt")
  )
  d |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(by, "band")))) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean_p = mean(.data$p),
      mean_q = mean(.data$q_market),
      realised = mean(.data$y),
      .groups = "drop"
    )
}
```

- [ ] **Step 4: Run — expect PASS**
- [ ] **Step 5: Commit** `git commit R/backtest-marketcal.R tests/testthat/test-backtest-marketcal.R -m "feat(marketcal): stratified model-vs-line disagreement"`

---

## Task 4: Line-stability monitor (does Lengjan actually move prices?)

**Files:** Modify `R/backtest-marketcal.R`, extend test file.

- [ ] **Step 1: Failing test**

```r
# append to tests/testthat/test-backtest-marketcal.R

test_that("bt_line_stability measures how often a price moves across snapshots", {
  odds <- tibble::tibble(
    match_date = as.Date("2026-05-20"),
    home_team = "A", away_team = "B",
    market = "moneyline", outcome = c("home", "home", "away", "away"),
    line = NA_real_,
    odds = c(2.10, 2.30, 1.70, 1.70), # home moved (2 prices), away static (1 price)
    scraped_at = as.POSIXct(c("2026-05-18 09:00", "2026-05-19 09:00",
                              "2026-05-18 09:00", "2026-05-19 09:00"), tz = "UTC")
  )
  s <- bt_line_stability(odds)
  expect_equal(s$n_series, 2L)         # (home), (away)
  expect_equal(s$pct_moved, 0.5)       # 1 of 2 series moved
  expect_equal(s$mean_distinct_prices, 1.5)
})
```

- [ ] **Step 2: Run — expect FAIL** (`could not find function "bt_line_stability"`)

- [ ] **Step 3: Implement** (append):

```r
#' How often does Lengjan actually move a price before kickoff?
#'
#' Confirms (or refutes) the static-line hypothesis empirically: per
#' `(match, market, line, outcome)` series, counts distinct prices across the
#' pre-kickoff snapshots. A low `pct_moved` justifies dropping CLV in favour of
#' the bias map.
#' @param odds Odds store (`scraped_at`, match keys, `market`, `outcome`, `line`,
#'   `odds`).
#' @return One-row tibble `(n_series, pct_moved, mean_distinct_prices,
#'   mean_snapshots)`.
#' @export
bt_line_stability <- function(odds) {
  if (nrow(odds) == 0L) {
    return(tibble::tibble(
      n_series = 0L, pct_moved = NA_real_,
      mean_distinct_prices = NA_real_, mean_snapshots = NA_real_
    ))
  }
  key <- c("match_date", "home_team", "away_team", "market", "outcome", "line")
  per <- odds |>
    dplyr::group_by(dplyr::across(dplyr::all_of(key))) |>
    dplyr::summarise(
      n_snapshots = dplyr::n(),
      n_distinct_prices = dplyr::n_distinct(.data$odds),
      .groups = "drop"
    )
  tibble::tibble(
    n_series = nrow(per),
    pct_moved = mean(per$n_distinct_prices > 1L),
    mean_distinct_prices = mean(per$n_distinct_prices),
    mean_snapshots = mean(per$n_snapshots)
  )
}
```

- [ ] **Step 4: Run — expect PASS**
- [ ] **Step 5: Commit** `git commit R/backtest-marketcal.R tests/testthat/test-backtest-marketcal.R -m "feat(marketcal): line-stability monitor"`

---

## Task 5: Document, verify, integration smoke

- [ ] **Step 1:** `Rscript -e 'devtools::document()'` — NAMESPACE gains the 4 exports; DESCRIPTION Collate gains `backtest-marketcal.R`. Stage NAMESPACE + DESCRIPTION + man/.
- [ ] **Step 2:** Run `test-backtest-marketcal.R` + full backtest suite — all PASS.
- [ ] **Step 3: Integration smoke** — the real line-softness map + stability:

```bash
Rscript -e '
devtools::load_all(quiet=TRUE)
wf <- bt_walkforward_reuse("male", season = 2026L)$bets
mkt <- bt_devig(wf)
cat("*** Is the line calibrated? market reliability (q_market vs realised): ***\n")
print(as.data.frame(bt_market_calibration(mkt, n_bins = 5)))
cat("\n*** Directional bias per outcome (bias>0 = line under-prices it): ***\n")
print(as.data.frame(bt_market_bias(mkt, by = "market")))
cat("\n*** Disagreement: who is right where model and line diverge? ***\n")
print(as.data.frame(bt_disagreement(mkt)))
odds <- read_table("odds", filter = list(sport="football", country="iceland"))
cat("\n*** Does Lengjan move prices? ***\n")
print(as.data.frame(bt_line_stability(odds)))
'
```
Expected: market reliability table, per-outcome bias (the bad-odds map), disagreement bands, and a `pct_moved` confirming the static-line hypothesis.

- [ ] **Step 4: Commit** `git commit NAMESPACE DESCRIPTION man/ -m "docs(marketcal): exports + Collate for line-softness diagnostics"`

---

## Self-Review

**Spec coverage (§6.4):** market reliability → Task 1 ✅; favourite-longshot/draw/over-under bias → Task 2 ✅; disagreement→who's-right, stratified → Task 3 ✅; line-stability monitor → Task 4 ✅.

**Placeholder scan:** none.

**Type consistency:** all functions consume the `bt_devig()` frame (`p`, `q_market`, `y`, `outcome`, `market`); `by=` semantics match P2; `bt_market_calibration` reuses `bt_calibration_bands` (P2) by remapping `q_market`→`p`, `y`→`win`.
