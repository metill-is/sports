# Dashboard P1 — Joint-Distribution Diagnostics Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add `R/backtest-pit.R` — randomised-PIT, predicted-vs-observed draw rate, and scoreline-residual diagnostics computed from the joint posterior-predictive (`predicted_matches.parquet`) + `results`, as the substrate for the model-quality dashboard's "Is the model right?" page.

**Architecture:** Pure, read-only functions over existing parquet. A deterministic core (`bt_pit_bounds`, `bt_marginal_value`, `bt_rpit`) is composed by table-level functions that load all extract fit_dates (`bt_load_predicted`), pick each match's leak-free **as-of** fit (`bt_pit_asof`), and emit tidy per-match / per-stratum diagnostics (`bt_pit_values`, `bt_pit_uniformity`, `bt_draw_rate`, `bt_scoreline_residuals`). No Stan, no writes — self-updating on render like the existing walk-forward REUSE arm.

**Tech Stack:** R (base pipe), `arrow`, `dplyr`, `tibble`, `withr`, `testthat` edition 3, roxygen2. Conventions per `.claude/rules/r-conventions.md` + `r-package-conventions.md`: `#' @export` on public fns, `@noRd` on micro-helpers, explicit `dplyr::` namespacing, `here::here()`, no comments beyond roxygen + `# WHY:`.

**Reference (spec):** `docs/superpowers/specs/2026-06-16-model-quality-dashboard-design.md` §6.1, §6.2.

**Data facts (verified):** each `predicted_matches.parquet` holds `(home_team, away_team, match_date<Date>, home_goals<int>, away_goals<int>, count<int>, division<chr>)` with `sum(count) == 4000` per match; `sex`/`fit_date` live in the hive path `beliefs/extracts/sport=football/country=iceland/sex=<s>/fit_date=<F>/`. `results` holds `(match_date, home_team, away_team, home_score, away_score, division, round, sport, country, sex, season)` on the same **federation-name** key — so the predicted↔results join is clean (no Lengjan name mismatch).

---

## File structure

- **Create** `R/backtest-pit.R` — all diagnostic functions for this phase.
- **Create** `tests/testthat/test-backtest-pit.R` — tests, mirroring `test-backtest-metrics.R` style.
- **Modify** `NAMESPACE` — via `devtools::document()` only (never by hand).

---

## Task 1: Deterministic PIT core

**Files:**
- Create: `R/backtest-pit.R`
- Test: `tests/testthat/test-backtest-pit.R`

- [ ] **Step 1: Write the failing test**

```r
# tests/testthat/test-backtest-pit.R

test_that("bt_marginal_value derives total, diff, home, away", {
  expect_equal(bt_marginal_value(2L, 1L, "total"), 3)
  expect_equal(bt_marginal_value(2L, 1L, "diff"), 1)
  expect_equal(bt_marginal_value(2L, 1L, "home"), 2)
  expect_equal(bt_marginal_value(2L, 1L, "away"), 1)
})

test_that("bt_pit_bounds returns the discrete CDF jump [F(y-1), F(y)]", {
  # Uniform predictive over totals {0,1,2,3}: F(1)=0.5, F(2)=0.75.
  values <- c(0L, 1L, 2L, 3L)
  weights <- c(1000, 1000, 1000, 1000)
  b <- bt_pit_bounds(values, weights, y = 2L)
  expect_equal(b, c(0.5, 0.75))
})

test_that("bt_pit_bounds degenerates to a point when the observed value has no mass", {
  # No draw ever landed total == 5: F(4) == F(5) == 1 -> a zero-width band.
  b <- bt_pit_bounds(c(0L, 1L, 2L), c(10, 20, 70), y = 5L)
  expect_equal(b, c(1, 1))
})

test_that("bt_rpit places u inside the [F(y-1), F(y)] band", {
  values <- c(0L, 1L, 2L, 3L)
  weights <- c(1000, 1000, 1000, 1000)
  expect_equal(bt_rpit(values, weights, y = 2L, u = 0), 0.5)
  expect_equal(bt_rpit(values, weights, y = 2L, u = 1), 0.75)
  expect_equal(bt_rpit(values, weights, y = 2L, u = 0.5), 0.625)
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-backtest-pit.R")'`
Expected: FAIL — `could not find function "bt_marginal_value"`.

- [ ] **Step 3: Write minimal implementation**

```r
# R/backtest-pit.R
#' Joint-distribution forecast diagnostics (randomised PIT, draw rate,
#' scoreline residuals) over the saved posterior-predictive extracts.
#' @importFrom rlang .data
NULL

#' Derive a scalar match marginal from a home/away goal pair.
#' @param home,away Integer (or numeric) goal counts (vectorised).
#' @param marginal One of "total" (h+a), "diff" (h-a, the Skellam), "home", "away".
#' @return Numeric vector of the chosen marginal.
#' @noRd
bt_marginal_value <- function(home, away, marginal = c("total", "diff", "home", "away")) {
  marginal <- match.arg(marginal)
  switch(marginal,
    total = home + away,
    diff = home - away,
    home = home,
    away = away
  )
}

#' Discrete PIT band [F(y-1), F(y)] of an integer outcome under a pmf.
#' @param values Integer support points of the predictive pmf.
#' @param weights Non-negative weights (e.g. posterior-draw counts) per value.
#' @param y Observed integer outcome.
#' @return Length-2 numeric `c(lo, hi)`; `c(NA, NA)` if total weight is 0.
#' @noRd
bt_pit_bounds <- function(values, weights, y) {
  tot <- sum(weights)
  if (!isTRUE(tot > 0)) {
    return(c(NA_real_, NA_real_))
  }
  lo <- sum(weights[values <= y - 1]) / tot
  hi <- sum(weights[values <= y]) / tot
  c(lo, hi)
}

#' Randomised PIT for a discrete outcome (Czado-Gneiting-Held 2009).
#'
#' `u = F(y-1) + U * [F(y) - F(y-1)]`. Under a correctly specified predictive,
#' `u ~ Uniform(0, 1)`; a U-shaped histogram of `u` over many matches signals an
#' under-dispersed (over-confident) predictive, a hump signals over-dispersion.
#' @param values,weights Predictive pmf (support + draw counts).
#' @param y Observed integer outcome.
#' @param u Uniform(0,1) draw for the randomisation; injectable for tests.
#' @return Scalar randomised PIT value in `[0, 1]`.
#' @export
bt_rpit <- function(values, weights, y, u = stats::runif(1)) {
  b <- bt_pit_bounds(values, weights, y)
  b[1] + u * (b[2] - b[1])
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-backtest-pit.R")'`
Expected: PASS (4 tests).

- [ ] **Step 5: Commit**

```bash
git add R/backtest-pit.R tests/testthat/test-backtest-pit.R
git commit -m "feat(backtest-pit): deterministic randomised-PIT core"
```

---

## Task 2: Load all predicted-match extracts for a sex

**Files:**
- Modify: `R/backtest-pit.R`
- Test: `tests/testthat/test-backtest-pit.R`

- [ ] **Step 1: Write the failing test**

```r
# append to tests/testthat/test-backtest-pit.R

seed_predicted <- function(root, sex, fit_date, pm) {
  dir <- file.path(
    root, "beliefs", "extracts", "sport=football", "country=iceland",
    paste0("sex=", sex), paste0("fit_date=", fit_date)
  )
  fs::dir_create(dir, recurse = TRUE)
  arrow::write_parquet(pm, file.path(dir, "predicted_matches.parquet"))
}

pm_fixture <- function(match_date = "2026-05-20", home = "A", away = "B",
                       division = "BD") {
  # Uniform pmf over the (0..1)x(0..1) score grid: 1000 draws per cell, 4000 total.
  tidyr::expand_grid(home_goals = 0:1, away_goals = 0:1) |>
    dplyr::mutate(
      home_team = home, away_team = away,
      match_date = as.Date(match_date), count = 1000L, division = division
    )
}

test_that("bt_load_predicted reads every fit_date partition with sex + fit_date attached", {
  root <- withr::local_tempdir()
  seed_predicted(root, "male", "2026-05-10", pm_fixture(match_date = "2026-05-12"))
  seed_predicted(root, "male", "2026-05-17", pm_fixture(match_date = "2026-05-20"))
  seed_predicted(root, "female", "2026-05-17", pm_fixture(match_date = "2026-05-20"))

  all <- bt_load_predicted(root, sex = c("male", "female"))
  expect_setequal(unique(all$fit_date), as.Date(c("2026-05-10", "2026-05-17")))
  expect_setequal(unique(all$sex), c("male", "female"))
  expect_true(all(c("home_goals", "away_goals", "count", "division") %in% names(all)))
  expect_equal(sum(all$count), 4L * 4000L)
})

test_that("bt_load_predicted filters to a season and returns the empty schema when absent", {
  root <- withr::local_tempdir()
  seed_predicted(root, "male", "2025-09-01", pm_fixture(match_date = "2025-09-03"))
  seed_predicted(root, "male", "2026-05-17", pm_fixture(match_date = "2026-05-20"))

  out <- bt_load_predicted(root, sex = "male", season = 2026L)
  expect_equal(unique(out$fit_date), as.Date("2026-05-17"))

  empty <- bt_load_predicted(withr::local_tempdir(), sex = "male")
  expect_equal(nrow(empty), 0L)
  expect_true(all(c("sex", "fit_date", "home_goals", "count") %in% names(empty)))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-backtest-pit.R")'`
Expected: FAIL — `could not find function "bt_load_predicted"`.

- [ ] **Step 3: Write minimal implementation**

```r
# append to R/backtest-pit.R

#' Empty predicted-matches tibble (loader schema).
#' @noRd
bt_predicted_empty <- function() {
  tibble::tibble(
    home_team = character(), away_team = character(),
    match_date = as.Date(character()),
    home_goals = integer(), away_goals = integer(), count = integer(),
    division = character(), sex = character(), fit_date = as.Date(character())
  )
}

#' Load every saved football_iceland predicted-matches extract for a sex.
#'
#' Reads each `beliefs/extracts/.../sex=<s>/fit_date=<F>/predicted_matches.parquet`
#' (the posterior-predictive score histogram) and row-binds them, attaching `sex`
#' and `fit_date` from the hive path. Read-only.
#' @param root Data root holding `beliefs/extracts/`.
#' @param sex Character vector of sexes to load. Default both.
#' @param season Optional integer year; filters fit_dates to that season.
#' @return Tibble of all fit_dates' predicted matches, or the empty schema.
#' @export
bt_load_predicted <- function(root = here::here("data"),
                              sex = c("male", "female"), season = NULL) {
  base <- file.path(root, "beliefs", "extracts", "sport=football", "country=iceland")
  out <- list()
  for (s in sex) {
    ext_dir <- file.path(base, paste0("sex=", s))
    if (!dir.exists(ext_dir)) next
    fds <- sub("fit_date=", "", list.files(ext_dir))
    fds <- fds[grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", fds)]
    if (!is.null(season)) fds <- fds[substr(fds, 1, 4) == as.character(season)]
    for (fd in fds) {
      p <- file.path(ext_dir, paste0("fit_date=", fd), "predicted_matches.parquet")
      if (!file.exists(p)) next
      pm <- arrow::read_parquet(p)
      if (nrow(pm) == 0L) next
      pm$sex <- s
      pm$fit_date <- as.Date(fd)
      out[[length(out) + 1L]] <- pm
    }
  }
  if (length(out) == 0L) {
    return(bt_predicted_empty())
  }
  dplyr::bind_rows(out)
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-backtest-pit.R")'`
Expected: PASS (all tests so far).

- [ ] **Step 5: Commit**

```bash
git add R/backtest-pit.R tests/testthat/test-backtest-pit.R
git commit -m "feat(backtest-pit): load predicted-match extracts across fit_dates"
```

---

## Task 3: Leak-free as-of fit selection

**Files:**
- Modify: `R/backtest-pit.R`
- Test: `tests/testthat/test-backtest-pit.R`

- [ ] **Step 1: Write the failing test**

```r
# append to tests/testthat/test-backtest-pit.R

test_that("bt_pit_asof keeps only the most recent fit STRICTLY before each match", {
  # Three fits predict the same 2026-05-20 match. As-of must pick 05-17 (latest
  # pre-match) and exclude 05-21 (after the match -> would leak the result).
  predicted <- dplyr::bind_rows(
    dplyr::mutate(pm_fixture(match_date = "2026-05-20"), sex = "male", fit_date = as.Date("2026-05-10")),
    dplyr::mutate(pm_fixture(match_date = "2026-05-20"), sex = "male", fit_date = as.Date("2026-05-17")),
    dplyr::mutate(pm_fixture(match_date = "2026-05-20"), sex = "male", fit_date = as.Date("2026-05-21"))
  )
  asof <- bt_pit_asof(predicted)
  expect_equal(unique(asof$fit_date), as.Date("2026-05-17"))
  expect_equal(nrow(asof), 4L) # one fit's 2x2 score grid
})

test_that("bt_pit_asof drops a match with no pre-match fit", {
  predicted <- dplyr::mutate(
    pm_fixture(match_date = "2026-05-20"),
    sex = "male", fit_date = as.Date("2026-05-25")
  )
  expect_equal(nrow(bt_pit_asof(predicted)), 0L)
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-backtest-pit.R")'`
Expected: FAIL — `could not find function "bt_pit_asof"`.

- [ ] **Step 3: Write minimal implementation**

```r
# append to R/backtest-pit.R

#' Restrict predicted matches to each match's leak-free as-of fit.
#'
#' Per `(sex, match)`, keeps only the rows from the most recent `fit_date`
#' STRICTLY before `match_date` — the freshest forecast that could not have seen
#' the result. Matches with no pre-match fit are dropped.
#' @param predicted Output of [bt_load_predicted()].
#' @return `predicted` filtered to the as-of fit per match.
#' @noRd
bt_pit_asof <- function(predicted) {
  if (nrow(predicted) == 0L) {
    return(predicted)
  }
  key <- c("sex", "home_team", "away_team", "match_date")
  chosen <- predicted |>
    dplyr::filter(.data$fit_date < .data$match_date) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(key))) |>
    dplyr::summarise(fit_date = max(.data$fit_date), .groups = "drop")
  predicted |>
    dplyr::inner_join(chosen, by = c(key, "fit_date"))
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-backtest-pit.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/backtest-pit.R tests/testthat/test-backtest-pit.R
git commit -m "feat(backtest-pit): leak-free as-of fit selection per match"
```

---

## Task 4: Per-match randomised-PIT table

**Files:**
- Modify: `R/backtest-pit.R`
- Test: `tests/testthat/test-backtest-pit.R`

- [ ] **Step 1: Write the failing test**

```r
# append to tests/testthat/test-backtest-pit.R

results_fixture <- function(match_date = "2026-05-20", home = "A", away = "B",
                            home_score = 1L, away_score = 1L, division = "BD",
                            sex = "male") {
  tibble::tibble(
    sport = "football", country = "iceland", sex = sex,
    match_date = as.Date(match_date), home_team = home, away_team = away,
    home_score = home_score, away_score = away_score,
    division = division, round = 1L, season = 2026L
  )
}

test_that("bt_pit_values computes the as-of randomised PIT per match for a marginal", {
  predicted <- dplyr::mutate(
    pm_fixture(match_date = "2026-05-20"), sex = "male", fit_date = as.Date("2026-05-17")
  )
  # Uniform 2x2 grid -> totals {0,1,2} with weights {1000, 2000, 1000} (tot 4000):
  # F(total) for observed total 2 = c(F(1), F(2)) = c(0.75, 1.0).
  results <- results_fixture(home_score = 1L, away_score = 1L) # total = 2
  pit <- bt_pit_values(predicted, results, marginal = "total", seed = 1L)
  expect_equal(nrow(pit), 1L)
  expect_true(pit$u >= 0.75 && pit$u <= 1.0)
  expect_true(all(c("sex", "division", "u") %in% names(pit)))
})

test_that("bt_pit_values carries division for stratification and returns empty on no overlap", {
  predicted <- dplyr::mutate(
    pm_fixture(match_date = "2026-05-20", division = "LD1"),
    sex = "male", fit_date = as.Date("2026-05-17")
  )
  results <- results_fixture(division = "LD1")
  pit <- bt_pit_values(predicted, results, marginal = "diff", seed = 1L)
  expect_equal(pit$division, "LD1")

  # A results set that shares no match key -> no rows.
  other <- results_fixture(home = "X", away = "Y")
  expect_equal(nrow(bt_pit_values(predicted, other, marginal = "total")), 0L)
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-backtest-pit.R")'`
Expected: FAIL — `could not find function "bt_pit_values"`.

- [ ] **Step 3: Write minimal implementation**

```r
# append to R/backtest-pit.R

#' Empty PIT-values tibble.
#' @noRd
bt_pit_empty <- function() {
  tibble::tibble(
    sex = character(), division = character(),
    match_date = as.Date(character()),
    home_team = character(), away_team = character(),
    marginal = character(), observed = numeric(), u = numeric()
  )
}

#' Per-match leak-free randomised PIT over a chosen score marginal.
#'
#' For each match, builds the as-of predictive pmf of the marginal (`total`,
#' `diff`, `home`, `away`) from the posterior-draw counts, looks up the observed
#' value from `results`, and computes the randomised PIT ([bt_rpit()]). The match
#' key is the federation-name `(sex, match_date, home_team, away_team)`, clean
#' between extracts and results.
#' @param predicted Output of [bt_load_predicted()].
#' @param results Results store (`home_score`, `away_score`, key cols).
#' @param marginal Score marginal to transform.
#' @param seed RNG seed for the PIT randomisation (reproducible).
#' @return Tibble `(sex, division, match_date, home_team, away_team, marginal,
#'   observed, u)`, one row per scored match.
#' @export
bt_pit_values <- function(predicted, results,
                          marginal = c("total", "diff", "home", "away"),
                          seed = 1L) {
  marginal <- match.arg(marginal)
  asof <- bt_pit_asof(predicted)
  if (nrow(asof) == 0L) {
    return(bt_pit_empty())
  }
  key <- c("sex", "match_date", "home_team", "away_team")
  asof$mval <- bt_marginal_value(asof$home_goals, asof$away_goals, marginal)
  pmf <- asof |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(key, "division", "mval")))) |>
    dplyr::summarise(weight = sum(.data$count), .groups = "drop")

  obs <- results
  obs$observed <- bt_marginal_value(obs$home_score, obs$away_score, marginal)
  obs <- obs[, c(key, "observed"), drop = FALSE]

  matches <- pmf |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c(key, "division")))) |>
    dplyr::inner_join(obs, by = key)
  if (nrow(matches) == 0L) {
    return(bt_pit_empty())
  }

  pmf_by <- split(pmf, interaction(pmf$sex, pmf$match_date, pmf$home_team, pmf$away_team, drop = TRUE))
  mkey <- function(r) as.character(interaction(r$sex, r$match_date, r$home_team, r$away_team, drop = TRUE))
  withr::with_seed(seed, {
    matches$u <- vapply(seq_len(nrow(matches)), function(i) {
      sub <- pmf_by[[mkey(matches[i, ])]]
      bt_rpit(sub$mval, sub$weight, matches$observed[i])
    }, numeric(1))
  })
  matches$marginal <- marginal
  tibble::as_tibble(matches[, c(key, "division", "marginal", "observed", "u")])
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-backtest-pit.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/backtest-pit.R tests/testthat/test-backtest-pit.R
git commit -m "feat(backtest-pit): per-match as-of randomised PIT table"
```

---

## Task 5: PIT uniformity summary

**Files:**
- Modify: `R/backtest-pit.R`
- Test: `tests/testthat/test-backtest-pit.R`

- [ ] **Step 1: Write the failing test**

```r
# append to tests/testthat/test-backtest-pit.R

test_that("bt_pit_uniformity summarises u with a KS test, grouped", {
  pit <- tibble::tibble(
    sex = rep(c("male", "female"), each = 50),
    u = c(seq(0.01, 0.99, length.out = 50), seq(0.01, 0.99, length.out = 50))
  )
  s <- bt_pit_uniformity(pit, by = "sex")
  expect_setequal(s$sex, c("male", "female"))
  expect_true(all(c("n", "ks_stat", "ks_p", "mean_u") %in% names(s)))
  expect_equal(s$n, c(50L, 50L))
  expect_true(all(s$mean_u > 0.4 & s$mean_u < 0.6))
})

test_that("bt_pit_uniformity flags a U-shaped (under-dispersed) histogram with low KS p", {
  # Mass piled at the extremes -> far from uniform.
  u <- c(rep(0.02, 60), rep(0.98, 60))
  s <- bt_pit_uniformity(tibble::tibble(u = u))
  expect_true(s$ks_p < 0.05)
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-backtest-pit.R")'`
Expected: FAIL — `could not find function "bt_pit_uniformity"`.

- [ ] **Step 3: Write minimal implementation**

```r
# append to R/backtest-pit.R

#' Uniformity summary of PIT values (KS test against Uniform(0,1)).
#'
#' A calibrated predictive yields `u ~ Uniform(0,1)`; a small `ks_p` (or a
#' visibly U-/hump-shaped histogram) flags miscalibration of the predictive
#' distribution's shape.
#' @param pit Tibble with a numeric `u` column (e.g. from [bt_pit_values()]).
#' @param by Optional grouping columns.
#' @return One row (or per group) of `(n, ks_stat, ks_p, mean_u)`.
#' @export
bt_pit_uniformity <- function(pit, by = NULL) {
  one <- function(d) {
    u <- d$u[is.finite(d$u)]
    if (length(u) < 2L) {
      return(tibble::tibble(n = length(u), ks_stat = NA_real_, ks_p = NA_real_, mean_u = mean(u)))
    }
    k <- suppressWarnings(stats::ks.test(u, "punif"))
    tibble::tibble(n = length(u), ks_stat = unname(k$statistic), ks_p = k$p.value, mean_u = mean(u))
  }
  if (nrow(pit) == 0L) {
    return(tibble::tibble(n = integer(), ks_stat = numeric(), ks_p = numeric(), mean_u = numeric()))
  }
  if (is.null(by)) {
    return(one(pit))
  }
  pit |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::group_modify(~ one(.x)) |>
    dplyr::ungroup()
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-backtest-pit.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/backtest-pit.R tests/testthat/test-backtest-pit.R
git commit -m "feat(backtest-pit): PIT uniformity (KS) summary"
```

---

## Task 6: Predicted-vs-observed draw rate

**Files:**
- Modify: `R/backtest-pit.R`
- Test: `tests/testthat/test-backtest-pit.R`

- [ ] **Step 1: Write the failing test**

```r
# append to tests/testthat/test-backtest-pit.R

test_that("bt_draw_rate compares predicted vs observed draw rate with a gap", {
  # Predictive: uniform 2x2 grid -> P(draw) = P(0-0) + P(1-1) = 0.25 + 0.25 = 0.5.
  predicted <- dplyr::bind_rows(
    dplyr::mutate(pm_fixture(match_date = "2026-05-20", home = "A", away = "B"),
      sex = "male", fit_date = as.Date("2026-05-17")),
    dplyr::mutate(pm_fixture(match_date = "2026-05-20", home = "C", away = "D"),
      sex = "male", fit_date = as.Date("2026-05-17"))
  )
  results <- dplyr::bind_rows(
    results_fixture(home = "A", away = "B", home_score = 1L, away_score = 1L), # draw
    results_fixture(home = "C", away = "D", home_score = 2L, away_score = 0L)  # not
  )
  dr <- bt_draw_rate(predicted, results, by = "sex")
  expect_equal(dr$predicted_draw_rate, 0.5)
  expect_equal(dr$observed_draw_rate, 0.5) # 1 of 2 matches drawn
  expect_equal(dr$gap, 0) # observed - predicted
  expect_equal(dr$n, 2L)
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-backtest-pit.R")'`
Expected: FAIL — `could not find function "bt_draw_rate"`.

- [ ] **Step 3: Write minimal implementation**

```r
# append to R/backtest-pit.R

#' Predicted vs observed draw rate, with the (observed - predicted) gap.
#'
#' Predicted draw probability per match = as-of `P(home_goals == away_goals)`
#' from the draw counts; observed = the realised draw indicator. A persistent
#' positive gap (model under-predicts draws) is the canonical signal for a
#' Dixon-Coles low-score correction or a bivariate-Poisson correlation term.
#' @param predicted Output of [bt_load_predicted()].
#' @param results Results store.
#' @param by Optional grouping columns (e.g. `c("sex", "division")`).
#' @return `(<by..>, n, predicted_draw_rate, observed_draw_rate, gap)`.
#' @export
bt_draw_rate <- function(predicted, results, by = "sex") {
  asof <- bt_pit_asof(predicted)
  if (nrow(asof) == 0L) {
    return(tibble::tibble())
  }
  key <- c("sex", "match_date", "home_team", "away_team")
  per_match <- asof |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(key, "division")))) |>
    dplyr::summarise(
      p_draw = sum(.data$count[.data$home_goals == .data$away_goals]) / sum(.data$count),
      .groups = "drop"
    )
  obs <- results
  obs$obs_draw <- as.numeric(obs$home_score == obs$away_score)
  obs <- obs[, c(key, "obs_draw"), drop = FALSE]
  joined <- dplyr::inner_join(per_match, obs, by = key)
  if (nrow(joined) == 0L) {
    return(tibble::tibble())
  }
  joined |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::summarise(
      n = dplyr::n(),
      predicted_draw_rate = mean(.data$p_draw),
      observed_draw_rate = mean(.data$obs_draw),
      gap = mean(.data$obs_draw) - mean(.data$p_draw),
      .groups = "drop"
    )
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-backtest-pit.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/backtest-pit.R tests/testthat/test-backtest-pit.R
git commit -m "feat(backtest-pit): predicted-vs-observed draw-rate gap"
```

---

## Task 7: Scoreline residual grid

**Files:**
- Modify: `R/backtest-pit.R`
- Test: `tests/testthat/test-backtest-pit.R`

- [ ] **Step 1: Write the failing test**

```r
# append to tests/testthat/test-backtest-pit.R

test_that("bt_scoreline_residuals returns observed-minus-predicted cell frequencies", {
  # One match, uniform 2x2 predictive (each cell predicted freq 0.25). The match
  # landed 1-1, so observed freq is 1 at (1,1) and 0 elsewhere.
  predicted <- dplyr::mutate(
    pm_fixture(match_date = "2026-05-20"), sex = "male", fit_date = as.Date("2026-05-17")
  )
  results <- results_fixture(home_score = 1L, away_score = 1L)
  grid <- bt_scoreline_residuals(predicted, results, by = "sex")
  cell_11 <- grid[grid$home_goals == 1 & grid$away_goals == 1, ]
  expect_equal(cell_11$predicted_freq, 0.25)
  expect_equal(cell_11$observed_freq, 1)
  expect_equal(cell_11$residual, 0.75)
  # Residuals over the full grid sum to ~0 (both are proper frequencies).
  expect_equal(sum(grid$residual), 0, tolerance = 1e-8)
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-backtest-pit.R")'`
Expected: FAIL — `could not find function "bt_scoreline_residuals"`.

- [ ] **Step 3: Write minimal implementation**

```r
# append to R/backtest-pit.R

#' Observed-minus-predicted scoreline frequencies on the (home, away) goal grid.
#'
#' Predicted cell frequency = mean over matches of `count / 4000`; observed =
#' the share of matches that landed exactly on that cell. Off-diagonal vs
#' diagonal structure in the residual separates a correlation fault (Dixon-Coles
#' / lambda3) from a marginal/dispersion one.
#' @param predicted Output of [bt_load_predicted()].
#' @param results Results store.
#' @param by Optional grouping columns.
#' @param max_goals Cap the grid (scores above fold into the top cell). Default 6.
#' @return `(<by..>, home_goals, away_goals, predicted_freq, observed_freq, residual)`.
#' @export
bt_scoreline_residuals <- function(predicted, results, by = "sex", max_goals = 6L) {
  asof <- bt_pit_asof(predicted)
  if (nrow(asof) == 0L) {
    return(tibble::tibble())
  }
  key <- c("sex", "match_date", "home_team", "away_team")
  cap <- function(x) pmin(as.integer(x), max_goals)
  matched_keys <- dplyr::inner_join(
    dplyr::distinct(asof, dplyr::across(dplyr::all_of(key))),
    dplyr::distinct(results[, key, drop = FALSE]),
    by = key
  )
  pm <- dplyr::semi_join(asof, matched_keys, by = key)
  res <- dplyr::semi_join(results, matched_keys, by = key)
  if (nrow(pm) == 0L) {
    return(tibble::tibble())
  }
  pm$home_goals <- cap(pm$home_goals)
  pm$away_goals <- cap(pm$away_goals)
  pred <- pm |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by)), .data$home_team, .data$away_team,
      .data$match_date, .data$home_goals, .data$away_goals) |>
    dplyr::summarise(cell_p = sum(.data$count), .groups = "drop_last") |>
    dplyr::mutate(cell_p = .data$cell_p / sum(.data$cell_p)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(by, "home_goals", "away_goals")))) |>
    dplyr::summarise(predicted_freq = mean(.data$cell_p), .groups = "drop")
  res$home_goals <- cap(res$home_score)
  res$away_goals <- cap(res$away_score)
  obs <- res |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::mutate(.n = dplyr::n()) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(by, "home_goals", "away_goals")))) |>
    dplyr::summarise(observed_freq = dplyr::n() / dplyr::first(.data$.n), .groups = "drop")
  dplyr::full_join(pred, obs, by = c(by, "home_goals", "away_goals")) |>
    dplyr::mutate(
      predicted_freq = dplyr::coalesce(.data$predicted_freq, 0),
      observed_freq = dplyr::coalesce(.data$observed_freq, 0),
      residual = .data$observed_freq - .data$predicted_freq
    )
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-backtest-pit.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/backtest-pit.R tests/testthat/test-backtest-pit.R
git commit -m "feat(backtest-pit): scoreline residual grid"
```

---

## Task 8: Document, full-suite verification, integration smoke

**Files:**
- Modify: `NAMESPACE` (via `devtools::document()`)

- [ ] **Step 1: Regenerate docs/NAMESPACE**

Run: `Rscript -e 'devtools::document()'`
Expected: `NAMESPACE` gains `export(bt_rpit)`, `export(bt_load_predicted)`, `export(bt_pit_values)`, `export(bt_pit_uniformity)`, `export(bt_draw_rate)`, `export(bt_scoreline_residuals)`; new `man/*.Rd`.

- [ ] **Step 2: Run the full PIT test file + the existing backtest suite**

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-backtest-pit.R"); testthat::test_file("tests/testthat/test-backtest-metrics.R"); testthat::test_file("tests/testthat/test-backtest-walkforward.R")'`
Expected: all PASS, 0 failures.

- [ ] **Step 3: Integration smoke on real data** (verifies the federation-name join + as-of selection actually produce rows)

Run:
```bash
Rscript -e '
devtools::load_all(quiet=TRUE)
pred <- bt_load_predicted(sex = c("male","female"), season = 2026L)
res  <- read_table("results", filter = list(sport="football", country="iceland"))
pit  <- bt_pit_values(pred, res, marginal = "total")
cat("PIT rows:", nrow(pit), "by sex:\n"); print(table(pit$sex))
print(bt_pit_uniformity(pit, by = "sex"))
print(bt_draw_rate(pred, res, by = c("sex","division")))
'
```
Expected: non-zero PIT rows for at least `male`; a draw-rate table with `predicted_draw_rate`, `observed_draw_rate`, `gap` per (sex, division). (This is the first real "model under-predicts draws by X%" readout.)

- [ ] **Step 4: Commit**

```bash
git add NAMESPACE man/
git commit -m "docs(backtest-pit): roxygen exports for joint-distribution diagnostics"
```

---

## Self-Review

**Spec coverage (§6.1, §6.2):**
- §6.1 randomised PIT (total/diff/home/away marginals, as-of leak-free, KS summary) → Tasks 1, 3, 4, 5. ✅
- §6.2 predicted-vs-observed draw rate → Task 6. ✅
- §6.2 scoreline residual grid (correlation-vs-marginal separation) → Task 7. ✅
- §6.2 total-goals / Skellam *law overlay* → emerges from `bt_pit_values(marginal="total"/"diff")` + the export layer's histogram (P4); the per-match values are produced here, the pooled-overlay plot is a P4 dashboard concern. No engine gap.
- Stratification by sex/division → every table function takes `by=` and carries `division`. ✅ (Wider sex×division×market plumbing + `bt_skill_ci(by=)` is P2, per the spec phasing — out of scope here.)

**Placeholder scan:** none — every step has complete code and exact commands.

**Type consistency:** match key `c("sex","match_date","home_team","away_team")` used identically across `bt_pit_asof`/`bt_pit_values`/`bt_draw_rate`/`bt_scoreline_residuals`; `bt_marginal_value`/`bt_pit_bounds`/`bt_rpit` signatures stable; `division` carried through every table fn.

**Note:** this plan delivers engine functions only. They surface in the dashboard in P4 (`R/dashboard-export.R` + `docs/dashboard/experiment.qmd`); P1 is independently shippable and testable (the Task 8 smoke is the human-visible payoff — the first quantified draw-deficit number).
