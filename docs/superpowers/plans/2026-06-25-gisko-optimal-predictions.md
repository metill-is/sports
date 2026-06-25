# GISKÓ Optimal Match Predictions Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A reusable R tool that turns the WC-2026 posterior into GISKÓ-optimal predicted scorelines + per-round joker recommendations.

**Architecture:** A posterior-first engine in `R/gisko.R`. The substrate is a per-posterior-draw analytic bivariate-Poisson scoreline matrix (rates from the model's own `.wc_match_lambdas()`), averaged into an integrated predictive `pbar`. The optimal scoreline maximises expected GISKÓ points over `pbar`; the joker doubles the highest-expected-points match per round; the round-total *distribution* comes from per-draw within-draw convolutions (matches are conditionally independent given a draw). A thin driver `scripts/wc/gisko.R` runs it on the live cache and prints/saves results.

**Tech Stack:** R package (`devtools`/`testthat` 3 / `roxygen2`), reuses `R/wc-simulate.R` internals, `data/wc/fit/sim_inputs.rds`.

## Global Constraints

- testthat edition 3; tests in `tests/testthat/`. Daily drivers: `devtools::load_all()`, `devtools::test()`.
- Public functions get `#' @export` roxygen; internal helpers `#' @noRd`. Regenerate `NAMESPACE` with `devtools::document()` after adding exports.
- Base pipe `|>`; `snake_case`; verbs for functions. Use `here::here()` for paths; never hardcode absolute paths.
- Non-ASCII in R source must use `\uXXXX` escapes (R CMD check). Files with Icelandic *string literals* are written via Python, not the Write tool. Comments may be non-ASCII.
- Read-only on the money path: `R/gisko.R` and `scripts/wc/gisko.R` never touch the ledger and are never wired into CI.
- Scoreline orientation is fixed everywhere: a matrix `P` has **rows = home goals (0..max)**, **cols = away goals (0..max)**; `P[h+1, a+1] = P(home = h, away = a)`.
- Achievable per-match scores are `{0, 1, 2, 3, 5}` — never 4.
- `max_goals` default `8L`.

---

### Task 1: Exact scoring rule + score matrix

**Files:**
- Create: `R/gisko.R`
- Test: `tests/testthat/test-gisko.R`

**Interfaces:**
- Produces: `gisko_match_points(pred_home, pred_away, act_home, act_away)` → integer score 0..5 (vectorised over equal-length args). `gisko_score_matrix(pred_home, pred_away, max_goals = 8L)` → `(max+1)x(max+1)` integer matrix; entry `[h+1, a+1]` is the score if the actual result is `(home=h, away=a)`.

- [ ] **Step 1: Write the failing test**

```r
# tests/testthat/test-gisko.R
test_that("gisko_match_points matches the rules examples", {
  expect_equal(gisko_match_points(2, 1, 2, 1), 5L)   # exact
  expect_equal(gisko_match_points(3, 1, 2, 1), 3L)   # outcome + away goals
  expect_equal(gisko_match_points(1, 1, 2, 2), 3L)   # draw outcome + goal diff
  expect_equal(gisko_match_points(2, 0, 0, 2), 0L)   # wrong everything
  expect_equal(gisko_match_points(2, 0, 3, 1), 3L)   # outcome + goal diff
})

test_that("gisko_match_points is vectorised", {
  expect_equal(gisko_match_points(c(2, 3), c(1, 1), c(2, 2), c(1, 1)), c(5L, 3L))
})

test_that("a per-match score of 4 is impossible", {
  g <- expand.grid(ph = 0:6, pa = 0:6, ah = 0:6, aa = 0:6)
  s <- gisko_match_points(g$ph, g$pa, g$ah, g$aa)
  expect_true(all(s %in% c(0L, 1L, 2L, 3L, 5L)))
  expect_false(any(s == 4L))
})

test_that("gisko_score_matrix orientation is rows=home, cols=away", {
  sm <- gisko_score_matrix(2, 1, max_goals = 4L)
  expect_equal(dim(sm), c(5L, 5L))
  expect_equal(sm[3, 2], 5L)   # actual home=2 (row 3), away=1 (col 2)
  expect_equal(sm[4, 2], 3L)   # actual 3-1: outcome + away goals
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-gisko.R")'`
Expected: FAIL — could not find function `gisko_match_points`.

- [ ] **Step 3: Write minimal implementation**

```r
# R/gisko.R
# GISKO (gisko.is) optimal-prediction engine. Posterior-first: the substrate is
# a per-draw bivariate-Poisson scoreline matrix from the WC model's own rate
# function. See docs/superpowers/specs/2026-06-25-gisko-optimal-predictions-design.md
#
# Scoreline matrices are oriented rows = home goals, cols = away goals.

#' GISKO points for one predicted scoreline against an actual result
#'
#' Scoring (rules section 06): 2 for correct outcome, 1 for exact home goals,
#' 1 for exact away goals, 1 for correct goal difference. Max 5; 4 is
#' unreachable. Vectorised over equal-length arguments.
#'
#' @param pred_home,pred_away Predicted goals.
#' @param act_home,act_away Actual goals.
#' @return Integer vector of scores in {0,1,2,3,5}.
#' @export
gisko_match_points <- function(pred_home, pred_away, act_home, act_away) {
  outcome <- 2L * (sign(pred_home - pred_away) == sign(act_home - act_away))
  gh <- 1L * (pred_home == act_home)
  ga <- 1L * (pred_away == act_away)
  gd <- 1L * ((pred_home - pred_away) == (act_home - act_away))
  as.integer(outcome + gh + ga + gd)
}

#' Score of a fixed prediction against every actual scoreline on a grid
#'
#' @param pred_home,pred_away Predicted goals.
#' @param max_goals Grid upper bound.
#' @return `(max+1) x (max+1)` integer matrix; `[h+1, a+1]` is the score when
#'   the actual result is home `h`, away `a`.
#' @export
gisko_score_matrix <- function(pred_home, pred_away, max_goals = 8L) {
  g <- 0:max_goals
  ah <- matrix(g, length(g), length(g))
  aa <- matrix(g, length(g), length(g), byrow = TRUE)
  matrix(
    gisko_match_points(pred_home, pred_away, as.vector(ah), as.vector(aa)),
    length(g), length(g)
  )
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e 'devtools::document(); devtools::load_all(); testthat::test_file("tests/testthat/test-gisko.R")'`
Expected: PASS (4 passing tests).

- [ ] **Step 5: Commit**

```bash
git add R/gisko.R tests/testthat/test-gisko.R NAMESPACE man/gisko_match_points.Rd man/gisko_score_matrix.Rd
git commit -m "feat(gisko): exact GISKO scoring rule + score matrix"
```

---

### Task 2: Analytic bivariate-Poisson PMF

**Files:**
- Modify: `R/gisko.R`
- Test: `tests/testthat/test-gisko.R`

**Interfaces:**
- Consumes: nothing from prior tasks.
- Produces: `.gisko_bvpois_pmf(lambdas, max_goals = 8L)` → `(max+1)x(max+1)` numeric matrix `P(home=h, away=a)` for rates `lambdas = c(lambda_h, lambda_a, lambda3)`, truncated to the grid and renormalised. This is the noise-free analogue of `.wc_rbvpois()`; note the realised goals are `H = Pois(lambda_h) + X3`, so `E[H] = lambda_h + lambda3`.

- [ ] **Step 1: Write the failing test**

```r
test_that(".gisko_bvpois_pmf sums to one and has the right means", {
  l <- c(1.4, 0.9, 0.3)
  P <- sports:::.gisko_bvpois_pmf(l, max_goals = 15L)
  expect_equal(sum(P), 1)
  g <- 0:15
  expect_equal(sum(rowSums(P) * g), l[1] + l[3], tolerance = 1e-4)  # E[H]=lh+l3
  expect_equal(sum(colSums(P) * g), l[2] + l[3], tolerance = 1e-4)  # E[A]=la+l3
})

test_that(".gisko_bvpois_pmf factorises when lambda3 = 0", {
  l <- c(1.2, 0.8, 0)
  P <- sports:::.gisko_bvpois_pmf(l, max_goals = 20L)
  g <- 0:20
  indep <- outer(stats::dpois(g, l[1]), stats::dpois(g, l[2]))
  indep <- indep / sum(indep)
  expect_equal(P, indep, tolerance = 1e-9)
})

test_that("positive lambda3 raises the draw probability vs independence", {
  corr <- sports:::.gisko_bvpois_pmf(c(1, 1, 0.6), max_goals = 20L)
  indep <- sports:::.gisko_bvpois_pmf(c(1, 1, 0), max_goals = 20L)
  expect_gt(sum(diag(corr)), sum(diag(indep)))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-gisko.R")'`
Expected: FAIL — `.gisko_bvpois_pmf` not found.

- [ ] **Step 3: Write minimal implementation**

```r
# append to R/gisko.R

#' Analytic bivariate-Poisson PMF on a goal grid
#'
#' P(H=h, A=a) = sum_k dpois(h-k, l1) dpois(a-k, l2) dpois(k, l3), the
#' trivariate-reduction bivariate Poisson the WC model samples from
#' (`.wc_rbvpois`). Truncated to `0..max_goals` and renormalised.
#'
#' @param lambdas `c(lambda_h, lambda_a, lambda3)`.
#' @param max_goals Grid upper bound.
#' @return `(max+1) x (max+1)` matrix; rows = home, cols = away.
#' @noRd
.gisko_bvpois_pmf <- function(lambdas, max_goals = 8L) {
  l1 <- lambdas[1]
  l2 <- lambdas[2]
  l3 <- lambdas[3]
  g <- 0:max_goals
  P <- matrix(0, length(g), length(g))
  for (k in 0:max_goals) {
    pk <- stats::dpois(k, l3)
    if (pk == 0) next
    vh <- ifelse(g - k >= 0, stats::dpois(g - k, l1), 0)
    va <- ifelse(g - k >= 0, stats::dpois(g - k, l2), 0)
    P <- P + pk * outer(vh, va)
  }
  P / sum(P)
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-gisko.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/gisko.R tests/testthat/test-gisko.R
git commit -m "feat(gisko): analytic bivariate-Poisson scoreline PMF"
```

---

### Task 3: Marginals, expected points, optimal scoreline

**Files:**
- Modify: `R/gisko.R`
- Test: `tests/testthat/test-gisko.R`

**Interfaces:**
- Consumes: `gisko_score_matrix()` (Task 1).
- Produces:
  - `gisko_marginals_from_pbar(pbar)` → list `p_outcome = c(home, draw, away)`, `home_pmf` (named by goals), `away_pmf`, `gd_pmf` (named by signed diff).
  - `gisko_expected_points(pred_home, pred_away, pbar)` → scalar `sum(pbar * score_matrix)`.
  - `gisko_expected_points_marginal(pred_home, pred_away, marg)` → scalar via the Lemma-1 formula (cross-check / `predictions.json` path).
  - `gisko_optimal_scoreline(pbar, max_goals = nrow(pbar) - 1L)` → list `home, away, exp_points, p_exact, modal_home, modal_away, optimal_differs_from_modal, top` (top-6 candidate tibble).

- [ ] **Step 1: Write the failing test**

```r
test_that("expected points: grid sum equals the marginal (Lemma 1) formula", {
  set.seed(1)
  pbar <- matrix(runif(36), 6, 6); pbar <- pbar / sum(pbar)
  marg <- gisko_marginals_from_pbar(pbar)
  for (h in 0:3) for (a in 0:3) {
    expect_equal(
      gisko_expected_points(h, a, pbar),
      gisko_expected_points_marginal(h, a, marg),
      tolerance = 1e-9
    )
  }
})

test_that("optimal scoreline can differ from the modal scoreline and never scores worse", {
  pbar <- matrix(0, 4, 4)               # rows=home, cols=away, goals 0..3
  pbar[2, 2] <- 0.30                    # 1-1 draw is the single modal cell
  pbar[3, 2] <- 0.22                    # 2-1
  pbar[2, 1] <- 0.20                    # 1-0
  pbar[3, 1] <- 0.13                    # 2-0
  pbar[1, 2] <- 0.08                    # 0-1
  pbar[2, 3] <- 0.07                    # 1-2
  opt <- gisko_optimal_scoreline(pbar)
  expect_true(opt$optimal_differs_from_modal)
  expect_gte(opt$exp_points, gisko_expected_points(1, 1, pbar))
})

test_that("a degenerate posterior is optimised by its point mass", {
  pbar <- matrix(0, 5, 5); pbar[3, 2] <- 1   # all mass on 2-1
  opt <- gisko_optimal_scoreline(pbar)
  expect_equal(c(opt$home, opt$away), c(2, 1))
  expect_equal(opt$exp_points, 5)
  expect_false(opt$optimal_differs_from_modal)
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-gisko.R")'`
Expected: FAIL — functions not found.

- [ ] **Step 3: Write minimal implementation**

```r
# append to R/gisko.R

#' Marginal summaries of an integrated scoreline PMF
#' @param pbar `(n) x (n)` matrix, rows = home, cols = away.
#' @return list `p_outcome`, `home_pmf`, `away_pmf`, `gd_pmf`.
#' @export
gisko_marginals_from_pbar <- function(pbar) {
  n <- nrow(pbar)
  g <- 0:(n - 1L)
  hh <- matrix(g, n, n)
  aa <- matrix(g, n, n, byrow = TRUE)
  gd <- tapply(as.vector(pbar), as.vector(hh - aa), sum)
  list(
    p_outcome = c(
      home = sum(pbar[lower.tri(pbar)]),
      draw = sum(diag(pbar)),
      away = sum(pbar[upper.tri(pbar)])
    ),
    home_pmf = stats::setNames(rowSums(pbar), g),
    away_pmf = stats::setNames(colSums(pbar), g),
    gd_pmf = stats::setNames(as.numeric(gd), names(gd))
  )
}

#' Expected GISKO points for a predicted scoreline over an integrated PMF
#' @export
gisko_expected_points <- function(pred_home, pred_away, pbar) {
  sum(pbar * gisko_score_matrix(pred_home, pred_away, nrow(pbar) - 1L))
}

.gisko_lookup <- function(v, nm) {
  x <- v[as.character(nm)]
  if (length(x) == 0L || is.na(x)) 0 else unname(x)
}

#' Expected GISKO points via the marginal (Lemma 1) decomposition
#'
#' Equivalent to [gisko_expected_points()] but takes only marginals — used to
#' cross-check and to score from `predictions.json` (which ships marginals).
#' @export
gisko_expected_points_marginal <- function(pred_home, pred_away, marg) {
  out <- if (pred_home > pred_away) {
    "home"
  } else if (pred_home < pred_away) {
    "away"
  } else {
    "draw"
  }
  2 * unname(marg$p_outcome[[out]]) +
    .gisko_lookup(marg$home_pmf, pred_home) +
    .gisko_lookup(marg$away_pmf, pred_away) +
    .gisko_lookup(marg$gd_pmf, pred_home - pred_away)
}

#' Bayes-optimal GISKO scoreline (maximise expected points)
#' @export
gisko_optimal_scoreline <- function(pbar, max_goals = nrow(pbar) - 1L) {
  g <- 0:max_goals
  cand <- expand.grid(home = g, away = g)
  cand$exp_points <- mapply(
    function(h, a) gisko_expected_points(h, a, pbar), cand$home, cand$away
  )
  cand <- cand[order(-cand$exp_points), , drop = FALSE]
  best <- cand[1, ]
  modal <- which(pbar == max(pbar), arr.ind = TRUE)[1, ]
  modal_home <- unname(modal[["row"]]) - 1L
  modal_away <- unname(modal[["col"]]) - 1L
  list(
    home = best$home, away = best$away,
    exp_points = best$exp_points,
    p_exact = pbar[best$home + 1L, best$away + 1L],
    modal_home = modal_home, modal_away = modal_away,
    optimal_differs_from_modal =
      !(best$home == modal_home && best$away == modal_away),
    top = tibble::as_tibble(utils::head(cand, 6L))
  )
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e 'devtools::document(); devtools::load_all(); testthat::test_file("tests/testthat/test-gisko.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/gisko.R tests/testthat/test-gisko.R NAMESPACE man/
git commit -m "feat(gisko): marginals, expected points, optimal scoreline"
```

---

### Task 4: Posterior predictive matrix from the WC model

**Files:**
- Modify: `R/gisko.R`
- Test: `tests/testthat/test-gisko.R`

**Interfaces:**
- Consumes: `.gisko_bvpois_pmf()` (Task 2); the package-internal `.wc_match_lambdas()` from `R/wc-simulate.R`.
- Produces: `gisko_predictive_matrix(home, away, sim_inputs, venue = "neutral", max_goals = 8L)` → list `pbar` (integrated `(max+1)x(max+1)` PMF), `lambdas` (`n_draws x 3` per-draw rate matrix), `n_draws`. `sim_inputs` is the `list(team=, scalar=)` from `data/wc/fit/sim_inputs.rds`.

- [ ] **Step 1: Write the failing test**

```r
test_that("gisko_predictive_matrix integrates per-draw PMFs over the posterior", {
  team <- tibble::tibble(
    team = rep(c("A", "B"), times = 3),
    .draw = rep(1:3, each = 2),
    cur_offense = c(0.4, -0.1, 0.5, 0.0, 0.3, -0.2),
    cur_defense = c(0.2, -0.3, 0.1, -0.2, 0.25, -0.35),
    home_advantage_off = 0, home_advantage_def = 0
  )
  scalar <- tibble::tibble(
    .draw = 1:3,
    mean_log_goals = c(0.1, 0.05, 0.12),
    alpha_mu3 = c(-1, -1.1, -0.9),
    beta_mu3_strength_diff = c(0.2, 0.25, 0.15)
  )
  si <- list(team = team, scalar = scalar)
  res <- gisko_predictive_matrix("A", "B", si, venue = "neutral", max_goals = 10L)
  expect_equal(res$n_draws, 3L)
  expect_equal(dim(res$lambdas), c(3L, 3L))
  expect_equal(sum(res$pbar), 1, tolerance = 1e-9)
  manual <- (sports:::.gisko_bvpois_pmf(res$lambdas[1, ], 10L) +
    sports:::.gisko_bvpois_pmf(res$lambdas[2, ], 10L) +
    sports:::.gisko_bvpois_pmf(res$lambdas[3, ], 10L)) / 3
  expect_equal(res$pbar, manual, tolerance = 1e-9)
})

test_that("gisko_predictive_matrix errors on an unknown team", {
  si <- list(
    team = tibble::tibble(
      team = "A", .draw = 1L, cur_offense = 0, cur_defense = 0,
      home_advantage_off = 0, home_advantage_def = 0
    ),
    scalar = tibble::tibble(
      .draw = 1L, mean_log_goals = 0.1, alpha_mu3 = -1, beta_mu3_strength_diff = 0.2
    )
  )
  expect_error(gisko_predictive_matrix("A", "Z", si), "not in sim_inputs")
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-gisko.R")'`
Expected: FAIL — `gisko_predictive_matrix` not found.

- [ ] **Step 3: Write minimal implementation**

```r
# append to R/gisko.R

#' Integrated posterior-predictive scoreline matrix for a pairing
#'
#' Reuses the WC model's rate function `.wc_match_lambdas()` (single source of
#' truth) so this can never drift from the forecast. Builds the analytic
#' bivariate-Poisson PMF per posterior draw and averages.
#'
#' @param home,away Team names (martj42 convention, as in `sim_inputs`).
#' @param sim_inputs `list(team=, scalar=)` from `data/wc/fit/sim_inputs.rds`.
#' @param venue One of "neutral" (all knockouts), "home", "away".
#' @param max_goals Grid upper bound.
#' @return list `pbar`, `lambdas` (`n_draws x 3`), `n_draws`.
#' @export
gisko_predictive_matrix <- function(home, away, sim_inputs,
                                    venue = "neutral", max_goals = 8L) {
  sit <- sim_inputs$team
  sca <- sim_inputs$scalar
  missing <- setdiff(c(home, away), sit$team)
  if (length(missing) > 0L) {
    cli::cli_abort("gisko_predictive_matrix: team(s) not in sim_inputs: {.val {missing}}")
  }
  dt_by <- split(sit, sit$.draw)
  ds_by <- split(sca, sca$.draw)
  keys <- intersect(names(dt_by), names(ds_by))
  if (length(keys) == 0L) {
    cli::cli_abort("gisko_predictive_matrix: no overlapping .draw keys.")
  }
  nd <- length(keys)
  n <- max_goals + 1L
  pbar <- matrix(0, n, n)
  lambdas <- matrix(0, nd, 3L)
  for (i in seq_along(keys)) {
    dt <- dt_by[[keys[i]]]
    off <- stats::setNames(dt$cur_offense, dt$team)
    def <- stats::setNames(dt$cur_defense, dt$team)
    ha_off <- stats::setNames(dt$home_advantage_off, dt$team)
    ha_def <- stats::setNames(dt$home_advantage_def, dt$team)
    ds <- ds_by[[keys[i]]]
    l <- .wc_match_lambdas(
      home, away, venue, off, def, ha_off, ha_def,
      ds$mean_log_goals, ds$alpha_mu3, ds$beta_mu3_strength_diff
    )
    lambdas[i, ] <- l
    pbar <- pbar + .gisko_bvpois_pmf(l, max_goals)
  }
  list(pbar = pbar / nd, lambdas = lambdas, n_draws = nd)
}

#' Assemble marginals from a published `predictions.json` match (cross-check)
#'
#' @param match One element of `predictions.json$matches`, parsed by jsonlite.
#' @return A `marg` list as produced by [gisko_marginals_from_pbar()].
#' @export
gisko_marginals_from_predictions <- function(match) {
  pull <- function(dist, value_key) {
    vals <- vapply(dist, function(e) as.numeric(e[[value_key]]), numeric(1))
    ps <- vapply(dist, function(e) as.numeric(e$p), numeric(1))
    stats::setNames(ps, vals)
  }
  list(
    p_outcome = c(
      home = as.numeric(match$p_home),
      draw = as.numeric(match$p_draw),
      away = as.numeric(match$p_away)
    ),
    home_pmf = pull(match$home_goal_distribution, "goals"),
    away_pmf = pull(match$away_goal_distribution, "goals"),
    gd_pmf = pull(match$goal_diff_distribution, "diff")
  )
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e 'devtools::document(); devtools::load_all(); testthat::test_file("tests/testthat/test-gisko.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/gisko.R tests/testthat/test-gisko.R NAMESPACE man/
git commit -m "feat(gisko): posterior-predictive scoreline matrix + predictions.json adapter"
```

---

### Task 5: Round optimisation + joker + round-total distribution

**Files:**
- Modify: `R/gisko.R`
- Test: `tests/testthat/test-gisko.R`

**Interfaces:**
- Consumes: `gisko_predictive_matrix()` (Task 4), `gisko_optimal_scoreline()` (Task 3), `gisko_score_matrix()` (Task 1).
- Produces: `gisko_optimise_round(matches, sim_inputs, max_goals = 8L)`. `matches` is a tibble with columns `home`, `away`, `venue`, `label` (all rows are one GISKÓ round). Returns list:
  - `picks`: tibble `label, home, away, opt_home, opt_away, exp_points, p_exact, differs_from_modal`.
  - `joker`: integer row index of the recommended joker match (max `exp_points`).
  - `total_pmf`, `total_pmf_joker`: numeric probability vectors indexed from 0 (round total without / with the joker on the recommended match).
  - `summary`: tibble `scenario ("base"/"joker"), mean, sd, q05, q50, q95`.

- [ ] **Step 1: Write the failing test**

```r
test_that("round total mean equals the sum of per-match expected points", {
  team <- tibble::tibble(
    team = rep(c("A", "B", "C", "D"), times = 2),
    .draw = rep(1:2, each = 4),
    cur_offense = c(0.5, 0.1, -0.2, -0.4, 0.45, 0.05, -0.25, -0.35),
    cur_defense = c(0.3, 0.0, -0.1, -0.3, 0.28, 0.02, -0.12, -0.28),
    home_advantage_off = 0, home_advantage_def = 0
  )
  scalar <- tibble::tibble(
    .draw = 1:2, mean_log_goals = c(0.1, 0.12),
    alpha_mu3 = c(-1, -0.9), beta_mu3_strength_diff = c(0.2, 0.18)
  )
  si <- list(team = team, scalar = scalar)
  matches <- tibble::tibble(
    home = c("A", "C"), away = c("B", "D"),
    venue = "neutral", label = c("A v B", "C v D")
  )
  res <- gisko_optimise_round(matches, si, max_goals = 10L)
  support <- seq_along(res$total_pmf) - 1L
  expect_equal(sum(support * res$total_pmf), sum(res$picks$exp_points),
    tolerance = 1e-6)
  jl <- res$joker
  supp_j <- seq_along(res$total_pmf_joker) - 1L
  expect_equal(sum(supp_j * res$total_pmf_joker),
    sum(res$picks$exp_points) + res$picks$exp_points[jl], tolerance = 1e-6)
})

test_that("the joker is the highest-expected-points match", {
  team <- tibble::tibble(
    team = rep(c("A", "B", "C", "D"), times = 2),
    .draw = rep(1:2, each = 4),
    cur_offense = c(1.2, -0.8, 0.1, 0.0, 1.1, -0.7, 0.05, 0.02),
    cur_defense = c(0.6, -0.6, 0.0, 0.0, 0.55, -0.55, 0.0, 0.0),
    home_advantage_off = 0, home_advantage_def = 0
  )
  scalar <- tibble::tibble(
    .draw = 1:2, mean_log_goals = c(0.1, 0.1),
    alpha_mu3 = c(-1, -1), beta_mu3_strength_diff = c(0.2, 0.2)
  )
  si <- list(team = team, scalar = scalar)
  matches <- tibble::tibble(
    home = c("A", "C"), away = c("B", "D"),
    venue = "neutral", label = c("mismatch", "even")
  )
  res <- gisko_optimise_round(matches, si, max_goals = 10L)
  expect_equal(res$joker, which.max(res$picks$exp_points))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-gisko.R")'`
Expected: FAIL — `gisko_optimise_round` not found.

- [ ] **Step 3: Write minimal implementation**

```r
# append to R/gisko.R

.gisko_score_support <- c(0L, 1L, 2L, 3L, 5L)

# Convolve two 0-indexed probability vectors.
.gisko_conv <- function(p, q) {
  r <- numeric(length(p) + length(q) - 1L)
  for (i in seq_along(p)) {
    r[i:(i + length(q) - 1L)] <- r[i:(i + length(q) - 1L)] + p[i] * q
  }
  r
}

# Per-draw conditional score PMF -> 0..5 indexed vector (index 4 = score 4 = 0).
.gisko_support_vec <- function(pmf_on_support) {
  v <- numeric(6L)
  v[.gisko_score_support + 1L] <- pmf_on_support
  v
}

.gisko_dist_summary <- function(total_pmf, scenario) {
  support <- seq_along(total_pmf) - 1L
  cdf <- cumsum(total_pmf)
  q <- function(p) support[which(cdf >= p)[1]]
  mu <- sum(support * total_pmf)
  tibble::tibble(
    scenario = scenario,
    mean = mu,
    sd = sqrt(sum((support - mu)^2 * total_pmf)),
    q05 = q(0.05), q50 = q(0.5), q95 = q(0.95)
  )
}

#' Optimise one GISKO round: scorelines, joker, and the round-total distribution
#'
#' Expected-points optimal per match (Lemma 1); joker doubles the highest-
#' expected-points match (Lemma 2). The round-total distribution is the
#' posterior-predictive mixture over draws of the within-draw convolution of
#' per-match score PMFs — matches are conditionally independent given a draw.
#'
#' @param matches Tibble `home`, `away`, `venue`, `label` (one round).
#' @param sim_inputs `list(team=, scalar=)` from the WC fit cache.
#' @param max_goals Grid upper bound.
#' @return See the task interface block.
#' @export
gisko_optimise_round <- function(matches, sim_inputs, max_goals = 8L) {
  nm <- nrow(matches)
  picks <- vector("list", nm)
  per_draw <- vector("list", nm)
  nd <- NULL
  for (m in seq_len(nm)) {
    pm <- gisko_predictive_matrix(
      matches$home[m], matches$away[m], sim_inputs,
      venue = matches$venue[m], max_goals = max_goals
    )
    opt <- gisko_optimal_scoreline(pm$pbar, max_goals)
    picks[[m]] <- tibble::tibble(
      label = matches$label[m], home = matches$home[m], away = matches$away[m],
      opt_home = opt$home, opt_away = opt$away,
      exp_points = opt$exp_points, p_exact = opt$p_exact,
      differs_from_modal = opt$optimal_differs_from_modal
    )
    nd <- pm$n_draws
    smv <- as.vector(gisko_score_matrix(opt$home, opt$away, max_goals))
    fac <- factor(smv, levels = .gisko_score_support)
    mat <- matrix(0, nd, 6L)
    for (d in seq_len(nd)) {
      Pd <- .gisko_bvpois_pmf(pm$lambdas[d, ], max_goals)
      agg <- tapply(as.vector(Pd), fac, sum)
      agg[is.na(agg)] <- 0
      mat[d, ] <- .gisko_support_vec(as.numeric(agg))
    }
    per_draw[[m]] <- mat
  }
  picks <- do.call(rbind, picks)
  joker <- which.max(picks$exp_points)

  total_for <- function(joker_idx) {
    total <- 0
    for (d in seq_len(nd)) {
      acc <- 1
      for (m in seq_len(nm)) {
        sp <- per_draw[[m]][d, ]
        acc <- .gisko_conv(acc, sp)
        if (!is.na(joker_idx) && joker_idx == m) acc <- .gisko_conv(acc, sp)
      }
      if (length(total) < length(acc)) total <- c(total, numeric(length(acc) - length(total)))
      total[seq_along(acc)] <- total[seq_along(acc)] + acc
    }
    total / nd
  }
  total_pmf <- total_for(NA_integer_)
  total_pmf_joker <- total_for(joker)

  list(
    picks = picks, joker = joker,
    total_pmf = total_pmf, total_pmf_joker = total_pmf_joker,
    summary = rbind(
      .gisko_dist_summary(total_pmf, "base"),
      .gisko_dist_summary(total_pmf_joker, "joker")
    )
  )
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e 'devtools::document(); devtools::load_all(); testthat::test_file("tests/testthat/test-gisko.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/gisko.R tests/testthat/test-gisko.R NAMESPACE man/
git commit -m "feat(gisko): round optimiser, joker recommendation, round-total distribution"
```

---

### Task 6: Driver script + live run

**Files:**
- Create: `scripts/wc/gisko.R`
- Modify: `.gitignore` (add `data/wc/gisko/`)

**Interfaces:**
- Consumes: all public `gisko_*` from `R/gisko.R`; `wc_structure()`, `wc_group_fixtures()`, `wc_schedule()` from the WC code; `data/wc/fit/sim_inputs.rds`.
- Produces: console table + `data/wc/gisko/optimal_picks.json`.

- [ ] **Step 1: Write the driver**

```r
# scripts/wc/gisko.R
# GISKO optimal predictions. Reads the WC posterior cache and emits, per
# upcoming match, the expected-points-optimal scoreline + a per-round joker.
# Read-only; never on CI. Usage:
#   Rscript scripts/wc/gisko.R                 # upcoming group fixtures
#   Rscript scripts/wc/gisko.R --match "Spain vs France @neutral #R16" \
#                              --match "Brazil vs Argentina @neutral #R16"
suppressMessages(devtools::load_all(here::here()))
options(width = 120)

args <- commandArgs(trailingOnly = TRUE)
max_goals <- 8L

parse_match <- function(s) {
  rnd <- sub(".*#", "", s); rnd <- if (grepl("#", s)) rnd else "KO"
  s2 <- sub("\\s*#.*$", "", s)
  venue <- if (grepl("@", s2)) sub(".*@\\s*", "", s2) else "neutral"
  s3 <- sub("\\s*@.*$", "", s2)
  parts <- trimws(strsplit(s3, "\\s+vs\\s+")[[1]])
  tibble::tibble(home = parts[1], away = parts[2], venue = venue, round = rnd,
    label = paste(parts[1], "v", parts[2]))
}

explicit <- args[args == "--match"]
if (length(explicit) > 0L) {
  vals <- args[which(args == "--match") + 1L]
  matches <- do.call(rbind, lapply(vals, parse_match))
} else {
  s <- wc_structure()
  fx <- wc_group_fixtures(s)
  up <- fx[!fx$played, , drop = FALSE]
  sched <- wc_schedule()
  rn <- sched$round[match(
    paste(up$home_team, up$away_team), paste(sched$home_team, sched$away_team)
  )]
  matches <- tibble::tibble(
    home = up$home_team, away = up$away_team, venue = up$venue,
    round = ifelse(is.na(rn), "G", paste0("G", rn)),
    label = paste(up$home_team, "v", up$away_team)
  )
}

si <- readRDS(here::here("data", "wc", "fit", "sim_inputs.rds"))

out <- lapply(split(matches, matches$round), function(mr) {
  res <- gisko_optimise_round(mr, si, max_goals = max_goals)
  res$round <- mr$round[1]
  res
})

for (res in out) {
  cli::cli_h2("Round {res$round}")
  tab <- res$picks
  tab$pick <- paste0(tab$opt_home, "-", tab$opt_away)
  print(tab[, c("label", "pick", "exp_points", "p_exact", "differs_from_modal")])
  cli::cli_alert_info(
    "Joker: {tab$label[res$joker]} (E[points] {round(tab$exp_points[res$joker], 2)})"
  )
  print(res$summary)
}

dir.create(here::here("data", "wc", "gisko"), showWarnings = FALSE, recursive = TRUE)
payload <- lapply(out, function(res) {
  list(round = res$round, joker_label = res$picks$label[res$joker],
    picks = res$picks, summary = res$summary)
})
jsonlite::write_json(payload, here::here("data", "wc", "gisko", "optimal_picks.json"),
  auto_unbox = TRUE, pretty = TRUE, dataframe = "rows")
cli::cli_alert_success("Wrote data/wc/gisko/optimal_picks.json")
```

- [ ] **Step 2: Add the scratch output dir to .gitignore**

Append to `.gitignore`:

```
data/wc/gisko/
```

- [ ] **Step 3: Run the driver end-to-end on the live cache**

Run: `Rscript scripts/wc/gisko.R`
Expected: a per-round table of optimal scorelines with `exp_points` in roughly `2.0–3.5`, a joker line per round, a base/joker summary, and "Wrote data/wc/gisko/optimal_picks.json". (If `data/wc/fit/sim_inputs.rds` is absent, run `Rscript scripts/wc/forecast.R` first or fit per `scripts/wc/fit.R`.)

- [ ] **Step 4: Sanity-check a known mismatch by hand**

Run a lopsided explicit pairing and confirm the optimal pick is a clear favourite win with `exp_points` toward the high end:

Run: `Rscript scripts/wc/gisko.R --match "Spain vs <weakest available> @neutral #R16"`
Expected: optimal scoreline is a multi-goal home win; `exp_points` notably above an even fixture's.

- [ ] **Step 5: Commit**

```bash
git add scripts/wc/gisko.R .gitignore
git commit -m "feat(gisko): driver script for optimal picks + per-round joker"
```

---

## Self-Review

**Spec coverage:**
- §2 scoring rule → Task 1 (`gisko_match_points`, the {0,1,2,3,5} invariant).
- §3 Lemma 1 (optimal scoreline) → Task 3; the marginal-vs-grid cross-check test ties the two derivations.
- §3 Lemma 2 (joker) → Task 5 (`joker = which.max(exp_points)`, mean-of-total test).
- §3 full-posterior round distribution → Task 5 (`total_pmf` via per-draw convolution).
- §4 substrate (`.gisko_bvpois_pmf`, `gisko_predictive_matrix` reusing `.wc_match_lambdas`) → Tasks 2, 4.
- §4 adapters → Task 3 (`gisko_marginals_from_pbar`), Task 4 (`gisko_marginals_from_predictions`).
- §5 round/joker → Task 5. §6 driver → Task 6. §7 tests → spread across all tasks.
- §8 out-of-scope (structural, rank-aware, HTML) → not implemented, by design.

**Placeholder scan:** No TBD/TODO; every code step shows full code; commands have expected output.

**Type consistency:** Scoreline matrices are uniformly rows=home/cols=away (Tasks 1–5). `marg` shape (`p_outcome`/`home_pmf`/`away_pmf`/`gd_pmf`) is identical in Task 3 (`gisko_marginals_from_pbar`) and Task 4 (`gisko_marginals_from_predictions`). `gisko_predictive_matrix` returns `pbar`/`lambdas`/`n_draws`, consumed under those names in Task 5. `sim_inputs` is `list(team=, scalar=)` in Tasks 4–6, matching `data/wc/fit/sim_inputs.rds`.

**Open verification points for the executor (not blockers):**
- Confirm the `predictions.json` cross-check tolerance empirically (analytic vs MC histogram); fold into Task 4 as a `skip_if(!file.exists(...))` test if the published file is present.
- Confirm `wc_schedule()` column names (`round` vs `Round Number`) when mapping group matchdays in Task 6; adjust the `sched$round` access accordingly.
