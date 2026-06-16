# Dashboard P2 — Stratification Primitives Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: superpowers:executing-plans. Steps use checkbox (`- [ ]`) syntax.

**Goal:** Make the model-arm calibration/skill machinery stratifiable and decomposable — division/round attachment, per-stratum bootstrap CIs, Murphy (REL/RES/UNC) decomposition, and calibration consistency-bands + Jeffreys intervals — so every dashboard diagnostic can be cut by sex × division × market with honest uncertainty.

**Architecture:** One new file `R/backtest-divisions.R` (attach division/round to a bets tibble from `results` on the clean federation-name key) and four extensions to `R/backtest-metrics.R` (`by=` on `bt_skill_ci`; `bt_brier_decomp`; `bt_calibration_bands`). Pure, read-only, TDD.

**Tech Stack:** R (base pipe), `dplyr`, `tibble`, `withr`, `stats` (`qbeta`/`qbinom`/`ks`), `testthat` 3, roxygen2.

**Reference (spec):** `docs/superpowers/specs/2026-06-16-model-quality-dashboard-design.md` §6.3, §6.5, §7.1, §7.3.

**Verified signatures:** `bt_calibration(settled, n_bins=10, by=NULL)`, `bt_skill(scored, by=NULL, eps=1e-6)`, `bt_skill_ci(scored, R=2000, probs=c(0.05,0.5,0.95), seed=1L)` (returns a bare numeric vector — must stay backward-compatible). `results` schema carries `division` (string) + `round` (int32).

---

## File structure

- **Create** `R/backtest-divisions.R` — `bt_attach_division()`.
- **Modify** `R/backtest-metrics.R` — `bt_skill_ci(by=)`, `bt_brier_decomp()`, `bt_calibration_bands()`.
- **Create** `tests/testthat/test-backtest-divisions.R`, extend `tests/testthat/test-backtest-metrics.R`.

---

## Task 1: Attach division + round (model-arm universe)

**Files:** Create `R/backtest-divisions.R`, `tests/testthat/test-backtest-divisions.R`.

- [ ] **Step 1: Failing test**

```r
# tests/testthat/test-backtest-divisions.R

test_that("bt_attach_division joins division + round from results on the match key", {
  bets <- tibble::tibble(
    sex = "male",
    match_date = as.Date(c("2026-05-20", "2026-05-21")),
    home_team = c("A", "C"), away_team = c("B", "D"),
    p = c(0.6, 0.4)
  )
  results <- tibble::tibble(
    sex = "male", match_date = as.Date("2026-05-20"),
    home_team = "A", away_team = "B",
    division = "BD", round = 7L, home_score = 1L, away_score = 0L
  )
  out <- bt_attach_division(bets, results)
  expect_equal(out$division, c("BD", "unknown")) # 2nd match absent from results
  expect_equal(out$round[1], 7L)
  expect_true(is.na(out$round[2]))
  expect_equal(nrow(out), 2L) # no fan-out
})

test_that("bt_attach_division de-duplicates results to one division per match", {
  bets <- tibble::tibble(
    sex = "male", match_date = as.Date("2026-05-20"),
    home_team = "A", away_team = "B", p = 0.5
  )
  results <- tibble::tibble(
    sex = "male", match_date = as.Date("2026-05-20"),
    home_team = "A", away_team = "B",
    division = c("BD", "BD"), round = c(7L, 7L),
    home_score = c(1L, 1L), away_score = c(0L, 0L)
  )
  expect_equal(nrow(bt_attach_division(bets, results)), 1L)
})
```

- [ ] **Step 2: Run — expect FAIL** (`could not find function "bt_attach_division"`)

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-backtest-divisions.R")'`

- [ ] **Step 3: Implement**

```r
# R/backtest-divisions.R
#' Attach division + round to a model-arm bet/candidate tibble.
#' @importFrom rlang .data
NULL

#' Attach `division` + `round` from `results` on the federation-name match key.
#'
#' The betting stores carry no `division`; the model arm recovers it from
#' `results` on `(sex, match_date, home_team, away_team)` -- clean federation
#' names on both sides (no Lengjan name-join). Unmatched fixtures get
#' `division = "unknown"` (never dropped); `round` stays `NA`.
#' @param bets Tibble with the match key columns.
#' @param results Results store (`division`, `round`, key cols).
#' @return `bets` with `division` (NA -> "unknown") and `round` columns added.
#' @export
bt_attach_division <- function(bets, results) {
  key <- c("sex", "match_date", "home_team", "away_team")
  div <- results |>
    dplyr::group_by(dplyr::across(dplyr::all_of(key))) |>
    dplyr::summarise(
      division = dplyr::first(.data$division),
      round = dplyr::first(.data$round),
      .groups = "drop"
    )
  out <- dplyr::left_join(bets, div, by = key)
  out$division <- dplyr::coalesce(out$division, "unknown")
  out
}
```

- [ ] **Step 4: Run — expect PASS**
- [ ] **Step 5: Commit** `git commit R/backtest-divisions.R tests/testthat/test-backtest-divisions.R -m "feat(backtest): attach division+round to the model-arm universe"`

---

## Task 2: `by=` on the match-clustered bootstrap CI

**Files:** Modify `R/backtest-metrics.R`, extend `tests/testthat/test-backtest-metrics.R`.

- [ ] **Step 1: Failing test**

```r
# append to tests/testthat/test-backtest-metrics.R

devig_fixture <- function() {
  # Two markets, two matches each; complete 2-way books, one winner per book.
  tibble::tibble(
    sport = "football", country = "iceland", sex = "male",
    match_date = as.Date(c("2026-05-01","2026-05-01","2026-05-02","2026-05-02",
                           "2026-05-03","2026-05-03","2026-05-04","2026-05-04")),
    home_team = c("A","A","C","C","E","E","G","G"),
    away_team = c("B","B","D","D","F","F","H","H"),
    market = c("total","total","total","total","moneyline","moneyline","moneyline","moneyline"),
    line = c(2.5,2.5,2.5,2.5,NA,NA,NA,NA),
    outcome = c("over","under","over","under","home","away","home","away"),
    p = c(0.6,0.4,0.55,0.45,0.7,0.3,0.65,0.35),
    q_market = c(0.58,0.42,0.52,0.48,0.68,0.32,0.6,0.4),
    y = c(1,0,0,1,1,0,0,1),
    overround = 1.05
  )
}

test_that("bt_skill_ci stays backward-compatible (numeric vector) with no `by`", {
  ci <- bt_skill_ci(devig_fixture(), R = 200, seed = 1L)
  expect_type(ci, "double")
  expect_length(ci, 3L)
  expect_true(ci[1] <= ci[2] && ci[2] <= ci[3])
})

test_that("bt_skill_ci returns a per-stratum tibble with `by`", {
  out <- bt_skill_ci(devig_fixture(), by = "market", R = 200, seed = 1L)
  expect_setequal(out$market, c("total", "moneyline"))
  expect_true(all(c("skill_lo", "skill_mid", "skill_hi") %in% names(out)))
  expect_true(all(out$skill_lo <= out$skill_mid & out$skill_mid <= out$skill_hi))
})
```

- [ ] **Step 2: Run — expect FAIL** (`unused argument (by = ...)`)

- [ ] **Step 3: Implement** — replace the `bt_skill_ci` definition's signature + add the grouped branch (keep the existing body as the `by = NULL` path):

```r
# R/backtest-metrics.R -- change the signature line and add the grouped branch
bt_skill_ci <- function(scored, by = NULL, R = 2000, probs = c(0.05, 0.5, 0.95), seed = 1L) {
  if (!is.null(by)) {
    return(
      scored |>
        dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
        dplyr::group_modify(~ {
          q <- bt_skill_ci(.x, by = NULL, R = R, probs = probs, seed = seed)
          tibble::tibble(skill_lo = q[1], skill_mid = q[2], skill_hi = q[3])
        }) |>
        dplyr::ungroup()
    )
  }
  if (nrow(scored) == 0L) {
    return(rep(NA_real_, length(probs)))
  }
  mid <- paste(scored$match_date, scored$home_team, scored$away_team)
  by_match <- split(seq_len(nrow(scored)), mid)
  matches <- names(by_match)
  withr::with_seed(seed, {
    reps <- vapply(seq_len(R), function(i) {
      idx <- unlist(by_match[sample(matches, replace = TRUE)], use.names = FALSE)
      d <- scored[idx, , drop = FALSE]
      1 - mean((d$p - d$y)^2) / mean((d$q_market - d$y)^2)
    }, numeric(1))
  })
  stats::quantile(reps, probs, names = FALSE)
}
```

(Note: the grouped branch requires `probs` length 3; the default satisfies it.)

- [ ] **Step 4: Run — expect PASS** (also re-run existing metrics tests for no regression)
- [ ] **Step 5: Commit** `git commit R/backtest-metrics.R tests/testthat/test-backtest-metrics.R -m "feat(backtest): per-stratum match-clustered skill CI (bt_skill_ci by=)"`

---

## Task 3: Murphy decomposition (REL/RES/UNC)

**Files:** Modify `R/backtest-metrics.R`, extend `tests/testthat/test-backtest-metrics.R`.

- [ ] **Step 1: Failing test**

```r
# append to tests/testthat/test-backtest-metrics.R

test_that("bt_brier_decomp splits Brier into reliability/resolution/uncertainty", {
  # constant-within-bin p so the Murphy identity holds exactly.
  scored <- tibble::tibble(p = c(0.2, 0.2, 0.8, 0.8), y = c(0, 1, 1, 1))
  d <- bt_brier_decomp(scored, n_bins = 2)
  expect_equal(d$uncertainty, 0.75 * 0.25)
  expect_equal(d$reliability, 0.065)
  expect_equal(d$resolution, 0.0625)
  expect_equal(d$brier, 0.19)
  # identity: BS = REL - RES + UNC
  expect_equal(d$brier, d$reliability - d$resolution + d$uncertainty, tolerance = 1e-9)
})

test_that("bt_brier_decomp groups by a dimension", {
  scored <- tibble::tibble(
    market = c("a", "a", "b", "b"),
    p = c(0.2, 0.8, 0.3, 0.7), y = c(0, 1, 0, 1)
  )
  d <- bt_brier_decomp(scored, n_bins = 2, by = "market")
  expect_setequal(d$market, c("a", "b"))
  expect_equal(nrow(d), 2L)
})
```

- [ ] **Step 2: Run — expect FAIL** (`could not find function "bt_brier_decomp"`)

- [ ] **Step 3: Implement** (append to `R/backtest-metrics.R`):

```r
#' Murphy (1973) decomposition of the Brier score into reliability, resolution,
#' and uncertainty. Compare REL/RES across strata (not raw Brier -- UNC, the
#' base-rate variance, differs by cell so raw Brier is not cross-stratum
#' comparable). Lower REL = better calibrated; higher RES = better discrimination.
#' @param scored Tibble with numeric `p` and binary `y`.
#' @param n_bins Forecast bins. Default 10.
#' @param by Optional grouping columns.
#' @return `(<by..>, n, reliability, resolution, uncertainty, brier)`.
#' @export
bt_brier_decomp <- function(scored, n_bins = 10, by = NULL) {
  one <- function(d) {
    n <- nrow(d)
    o_bar <- mean(d$y)
    bin <- cut(d$p, breaks = seq(0, 1, length.out = n_bins + 1), include.lowest = TRUE)
    agg <- tibble::tibble(p = d$p, y = d$y, bin = bin) |>
      dplyr::group_by(.data$bin) |>
      dplyr::summarise(nk = dplyr::n(), pbar = mean(.data$p), obar = mean(.data$y), .groups = "drop")
    tibble::tibble(
      n = n,
      reliability = sum(agg$nk * (agg$pbar - agg$obar)^2) / n,
      resolution = sum(agg$nk * (agg$obar - o_bar)^2) / n,
      uncertainty = o_bar * (1 - o_bar),
      brier = mean((d$p - d$y)^2)
    )
  }
  if (nrow(scored) == 0L) {
    return(tibble::tibble())
  }
  if (is.null(by)) {
    return(one(scored))
  }
  scored |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::group_modify(~ one(.x)) |>
    dplyr::ungroup()
}
```

- [ ] **Step 4: Run — expect PASS**
- [ ] **Step 5: Commit** `git commit R/backtest-metrics.R tests/testthat/test-backtest-metrics.R -m "feat(backtest): Murphy Brier decomposition (REL/RES/UNC)"`

---

## Task 4: Calibration consistency-bands + Jeffreys intervals

**Files:** Modify `R/backtest-metrics.R`, extend `tests/testthat/test-backtest-metrics.R`.

- [ ] **Step 1: Failing test**

```r
# append to tests/testthat/test-backtest-metrics.R

test_that("bt_calibration_bands adds Jeffreys intervals + a consistency band", {
  settled <- tibble::tibble(p = rep(0.5, 100), win = rep(c(TRUE, FALSE), 50))
  cal <- bt_calibration_bands(settled, n_bins = 1)
  expect_equal(cal$n, 100L)
  expect_equal(cal$mean_p, 0.5)
  expect_equal(cal$realised_freq, 0.5)
  # Jeffreys interval brackets the realised frequency, strictly inside (0,1).
  expect_true(cal$lo > 0 && cal$lo < 0.5 && cal$hi > 0.5 && cal$hi < 1)
  # Consistency band (binomial null at mean_p) brackets 0.5.
  expect_true(cal$band_lo <= 0.5 && cal$band_hi >= 0.5)
})

test_that("bt_calibration_bands flags a miscalibrated bin (realised outside the band)", {
  # 100 forecasts of p~0.9 (in bin 10) but only half win.
  settled <- tibble::tibble(p = rep(0.92, 100), win = rep(c(TRUE, FALSE), 50))
  cal <- bt_calibration_bands(settled, n_bins = 10)
  cal <- cal[cal$n > 0, ]
  expect_true(cal$realised_freq < cal$band_lo) # 0.5 well below the ~0.9 null band
})
```

- [ ] **Step 2: Run — expect FAIL** (`could not find function "bt_calibration_bands"`)

- [ ] **Step 3: Implement** (append to `R/backtest-metrics.R`):

```r
#' Calibration reliability with Jeffreys per-bin intervals and a binomial
#' consistency band.
#'
#' Extends [bt_calibration()]: `lo`/`hi` are the Jeffreys (Beta(k+.5, n-k+.5))
#' interval on each bin's realised frequency; `band_lo`/`band_hi` are the
#' consistency band -- the central interval of the realised frequency UNDER the
#' null that the bin is perfectly calibrated (Binomial(n, mean_p)/n). A bin whose
#' `realised_freq` sits outside `[band_lo, band_hi]` is miscalibrated beyond
#' sampling noise.
#' @param settled Per-bet tibble (`p`, logical `win`).
#' @param n_bins Probability bins. Default 10.
#' @param by Optional grouping columns.
#' @param conf Central mass for both intervals. Default 0.9.
#' @return [bt_calibration()] columns plus `lo, hi, band_lo, band_hi`.
#' @export
bt_calibration_bands <- function(settled, n_bins = 10, by = NULL, conf = 0.9) {
  cal <- bt_calibration(settled, n_bins = n_bins, by = by)
  if (nrow(cal) == 0L) {
    return(cal)
  }
  a <- (1 - conf) / 2
  b <- 1 - a
  k <- round(cal$realised_freq * cal$n)
  cal$lo <- stats::qbeta(a, k + 0.5, cal$n - k + 0.5)
  cal$hi <- stats::qbeta(b, k + 0.5, cal$n - k + 0.5)
  cal$band_lo <- stats::qbinom(a, cal$n, cal$mean_p) / cal$n
  cal$band_hi <- stats::qbinom(b, cal$n, cal$mean_p) / cal$n
  cal
}
```

- [ ] **Step 4: Run — expect PASS**
- [ ] **Step 5: Commit** `git commit R/backtest-metrics.R tests/testthat/test-backtest-metrics.R -m "feat(backtest): calibration consistency-bands + Jeffreys intervals"`

---

## Task 5: Document, full-suite verification, integration smoke

- [ ] **Step 1:** `Rscript -e 'devtools::document()'` — NAMESPACE gains `bt_attach_division`, `bt_brier_decomp`, `bt_calibration_bands`; DESCRIPTION Collate gains `backtest-divisions.R`. **Stage NAMESPACE + man/ + DESCRIPTION** (the Collate is part of the change — see P1's lesson).
- [ ] **Step 2:** Run `test-backtest-divisions.R` + `test-backtest-metrics.R` + the full backtest suite — all PASS, no regressions.
- [ ] **Step 3: Integration smoke** — stratified skill CI + Murphy on real de-vigged data:

```bash
Rscript -e '
devtools::load_all(quiet=TRUE)
wf <- bt_walkforward_reuse("male", season = 2026L)$bets
res <- read_table("results", filter=list(sport="football", country="iceland"))
wf <- bt_attach_division(wf, res)
mkt <- bt_devig(wf)
cat("de-vigged rows:", nrow(mkt), "\n")
print(bt_skill_ci(mkt, by = "market", R = 500))
print(as.data.frame(bt_brier_decomp(mkt, by = c("market"))))
'
```
Expected: per-market skill CIs + REL/RES/UNC. (REUSE-mode walk-forward ~70s.)

- [ ] **Step 4: Commit** `git commit NAMESPACE DESCRIPTION man/ -m "docs(backtest): exports for stratification primitives"`

---

## Self-Review

**Spec coverage:** §7.1 `bt_skill_ci(by=)` → Task 2 ✅; §6.5 Murphy → Task 3 ✅; §6.3 consistency bands + Jeffreys → Task 4 ✅; §7.2 division attachment (model arm, federation-name key, "unknown" on miss) → Task 1 ✅ (sourced from `results`, which carries division+round; equivalent to the extract source and simpler).

**Placeholder scan:** none.

**Type consistency:** match key `c("sex","match_date","home_team","away_team")` matches P1; `bt_skill_ci(by=)` returns a tibble with `skill_lo/skill_mid/skill_hi` while `by=NULL` stays a numeric vector (backward-compatible with the qmd's `bt_skill_ci(mkt)` call); `bt_brier_decomp`/`bt_calibration_bands` take `by=` like the existing `bt_metrics`/`bt_calibration`.
