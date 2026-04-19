#' Unit tests for build_return_matrix and collect_match_bets
#'
#' Run from Sports/ directory:
#'   Rscript R/bets/test_return_matrix.R
#'
#' Covers every (sport, handicap variant, line type) cell the production
#' pipeline can produce.  Uses a simple inline assertion runner (not testthat)
#' to side-step reporter-output buffering in Rscript sessions.

Sys.setlocale("LC_ALL", "is_IS.UTF-8")

box::use(
  . / kelly_joint[
    build_return_matrix, collect_match_bets, get_kelly_joint,
    fractional_growth_curve
  ],
  dplyr[tibble]
)

# ─── Inline assertion runner ───────────────────────────────────────────────

.fail_count <- 0
.pass_count <- 0

expect_equal_named <- function(R, bet_types, expected, tol = 1e-10, ctx = "") {
  R_named <- stats::setNames(as.vector(R), bet_types)
  for (name in names(expected)) {
    if (!name %in% names(R_named)) {
      .fail_count <<- .fail_count + 1
      cat(sprintf("  FAIL [%s]: bet_type '%s' missing\n", ctx, name))
      next
    }
    actual <- R_named[[name]]
    exp <- expected[[name]]
    ok <- isTRUE(all.equal(actual, exp, tolerance = tol))
    if (ok) {
      .pass_count <<- .pass_count + 1
    } else {
      .fail_count <<- .fail_count + 1
      cat(sprintf(
        "  FAIL [%s]: %s got %s, expected %s\n",
        ctx, name, format(actual), format(exp)
      ))
    }
  }
}

expect_row <- function(R, row_idx, bet_types, expected, tol = 1e-10,
                       ctx = "") {
  for (name in names(expected)) {
    j <- match(name, bet_types)
    if (is.na(j)) {
      .fail_count <<- .fail_count + 1
      cat(sprintf("  FAIL [%s]: bet_type '%s' missing\n", ctx, name))
      next
    }
    actual <- R[row_idx, j]
    exp <- expected[[name]]
    ok <- isTRUE(all.equal(actual, exp, tolerance = tol))
    if (ok) {
      .pass_count <<- .pass_count + 1
    } else {
      .fail_count <<- .fail_count + 1
      cat(sprintf(
        "  FAIL [%s row=%d]: %s got %s, expected %s\n",
        ctx, row_idx, name, format(actual), format(exp)
      ))
    }
  }
}

expect_absent <- function(bets, bet_type, ctx = "") {
  if (bet_type %in% bets$bet_type) {
    .fail_count <<- .fail_count + 1
    cat(sprintf(
      "  FAIL [%s]: expected '%s' absent but it is present\n",
      ctx, bet_type
    ))
  } else {
    .pass_count <<- .pass_count + 1
  }
}

section <- function(name) {
  cat(sprintf("\n── %s ──\n", name))
}

# Shared configs ─────────────────────────────────────────────────────────────

cfg_football <- list(
  scoring = list(has_ties = TRUE, tie_threshold = 0),
  markets = list(outcome = TRUE, handicap = TRUE, totals = TRUE)
)
cfg_handball <- list(
  scoring = list(has_ties = TRUE, tie_threshold = 0.5),
  markets = list(outcome = TRUE, handicap = TRUE, totals = TRUE)
)
cfg_basketball <- list(
  scoring = list(has_ties = FALSE, tie_threshold = 0),
  markets = list(outcome = TRUE, handicap = TRUE, totals = TRUE)
)

# Minimal odds row constructor used throughout tests.
odds_row <- function(date = as.Date("2026-04-20"), booker = "Lengjan",
                     home = "A", away = "B", ...) {
  tibble(date = date, booker = booker, home = home, away = away, ...)
}

# ═══════════════════════════════════════════════════════════════════════════
# GROUP 1: European 3-way HC — football (tie_threshold = 0)
# ═══════════════════════════════════════════════════════════════════════════

section("Football European 3-way HC (THE BUG)")

# Change = +1 integer HC; match 2-3 → diff=-1 → adj=0 (tie zone)
match_hc <- odds_row(change = 1, o_home = 2.5, o_draw = 3.5, o_away = 2.5)
draws <- tibble(home_goals = 2L, away_goals = 3L)

bets <- collect_match_bets(NULL, match_hc, NULL, cfg_football)
R <- build_return_matrix(draws, bets)
expect_equal_named(
  R, bets$bet_type,
  list(hc_home = -1, hc_tie = 2.5, hc_away = -1),
  ctx = "football adj=0 Euro 3-way"
)

# adj > 0: home wins, tie/away lose
draws <- tibble(home_goals = 3L, away_goals = 1L) # adj = 2+1 = 3
bets <- collect_match_bets(NULL, match_hc, NULL, cfg_football)
R <- build_return_matrix(draws, bets)
expect_equal_named(
  R, bets$bet_type,
  list(hc_home = 1.5, hc_tie = -1, hc_away = -1),
  ctx = "football adj>0 Euro 3-way"
)

# adj < 0: away wins
draws <- tibble(home_goals = 0L, away_goals = 3L) # adj = -3+1 = -2
bets <- collect_match_bets(NULL, match_hc, NULL, cfg_football)
R <- build_return_matrix(draws, bets)
expect_equal_named(
  R, bets$bet_type,
  list(hc_home = -1, hc_tie = -1, hc_away = 1.5),
  ctx = "football adj<0 Euro 3-way"
)

# ═══════════════════════════════════════════════════════════════════════════
# GROUP 2: European 3-way HC — handball (regression guard)
# ═══════════════════════════════════════════════════════════════════════════

section("Handball European 3-way HC (regression)")

match_hc <- odds_row(change = 1, o_home = 2.5, o_draw = 8.0, o_away = 2.5)
draws <- tibble(
  home_goals = c(29L, 28L, 30L, 28L),
  away_goals = c(27L, 30L, 30L, 29L)
) # diffs: +2, -2, 0, -1  adj: +3, -1, +1, 0
bets <- collect_match_bets(NULL, match_hc, NULL, cfg_handball)
R <- build_return_matrix(draws, bets)

# Row 1: adj=+3 > 0.5 → home wins
expect_row(R, 1, bets$bet_type,
  list(hc_home = 1.5, hc_tie = -1, hc_away = -1),
  ctx = "handball adj=+3"
)
# Row 2: adj=-1 < -0.5 → away wins
expect_row(R, 2, bets$bet_type,
  list(hc_home = -1, hc_tie = -1, hc_away = 1.5),
  ctx = "handball adj=-1"
)
# Row 3: adj=+1 > 0.5 → home wins (one-goal win on +1 HC)
expect_row(R, 3, bets$bet_type,
  list(hc_home = 1.5, hc_tie = -1, hc_away = -1),
  ctx = "handball adj=+1"
)
# Row 4: adj=0 inside tie zone → tie wins, home/away lose
expect_row(R, 4, bets$bet_type,
  list(hc_home = -1, hc_tie = 7.0, hc_away = -1),
  ctx = "handball adj=0 (tie zone)"
)

# ═══════════════════════════════════════════════════════════════════════════
# GROUP 3: Asian 2-way HC — no o_draw offered
# ═══════════════════════════════════════════════════════════════════════════

section("Asian 2-way HC")

# Football integer HC without o_draw → Lengjan Asian 2-way
match_hc <- odds_row(change = 1, o_home = 1.9, o_draw = NA_real_, o_away = 1.9)
draws <- tibble(home_goals = 2L, away_goals = 3L) # adj = 0
bets <- collect_match_bets(NULL, match_hc, NULL, cfg_football)
expect_absent(bets, "hc_tie", ctx = "football Asian 2-way no o_draw")
R <- build_return_matrix(draws, bets)
expect_equal_named(
  R, bets$bet_type,
  list(hc_home = 0, hc_away = 0),
  ctx = "football Asian 2-way adj=0"
)

# Football half-point HC → Asian (no push ever)
match_hc <- odds_row(
  change = 0.5, o_home = 1.9, o_draw = NA_real_,
  o_away = 1.9
)
draws <- tibble(home_goals = 1L, away_goals = 1L) # adj = +0.5
bets <- collect_match_bets(NULL, match_hc, NULL, cfg_football)
R <- build_return_matrix(draws, bets)
expect_equal_named(
  R, bets$bet_type,
  list(hc_home = 0.9, hc_away = -1),
  ctx = "football Asian half-point"
)

# Basketball integer HC (no ties sport) → always Asian 2-way
match_hc <- odds_row(change = 3, o_home = 1.9, o_draw = NA_real_, o_away = 1.9)
draws <- tibble(home_goals = 97L, away_goals = 100L) # adj = 0
bets <- collect_match_bets(NULL, match_hc, NULL, cfg_basketball)
expect_absent(bets, "hc_tie", ctx = "basketball integer HC")
R <- build_return_matrix(draws, bets)
expect_equal_named(
  R, bets$bet_type,
  list(hc_home = 0, hc_away = 0),
  ctx = "basketball Asian 2-way adj=0"
)

# ═══════════════════════════════════════════════════════════════════════════
# GROUP 4: Totals push handling
# ═══════════════════════════════════════════════════════════════════════════

section("Totals push handling")

match_tot <- odds_row(limit = 3, o_over = 1.8, o_under = 2.0)
draws <- tibble(
  home_goals = c(1L, 2L, 0L), # totals: 3, 4, 1
  away_goals = c(2L, 2L, 1L)
)
bets <- collect_match_bets(NULL, NULL, match_tot, cfg_football)
R <- build_return_matrix(draws, bets)
# Integer line: push when total == 3
expect_row(R, 1, bets$bet_type, list(over = 0, under = 0), ctx = "totals int push")
expect_row(R, 2, bets$bet_type, list(over = 0.8, under = -1), ctx = "totals int over")
expect_row(R, 3, bets$bet_type, list(over = -1, under = 1.0), ctx = "totals int under")

match_tot <- odds_row(limit = 2.5, o_over = 1.8, o_under = 2.0)
draws <- tibble(home_goals = c(1L, 2L), away_goals = c(1L, 1L)) # totals 2, 3
bets <- collect_match_bets(NULL, NULL, match_tot, cfg_football)
R <- build_return_matrix(draws, bets)
expect_row(R, 1, bets$bet_type, list(over = -1, under = 1.0), ctx = "totals half under")
expect_row(R, 2, bets$bet_type, list(over = 0.8, under = -1), ctx = "totals half over")

# ═══════════════════════════════════════════════════════════════════════════
# GROUP 5: 1x2 — football, handball, basketball
# ═══════════════════════════════════════════════════════════════════════════

section("1x2 outcome market")

match_1x2 <- odds_row(o_home = 2.0, o_draw = 3.4, o_away = 3.5)
draws <- tibble(
  home_goals = c(2L, 1L, 1L, 0L),
  away_goals = c(1L, 1L, 2L, 0L)
) # diffs: +1, 0, -1, 0
bets <- collect_match_bets(match_1x2, NULL, NULL, cfg_football)
R <- build_return_matrix(draws, bets)
expect_row(R, 1, bets$bet_type, list(`1x2_home` = 1.0, `1x2_tie` = -1, `1x2_away` = -1), ctx = "1x2 fb diff=+1")
expect_row(R, 2, bets$bet_type, list(`1x2_home` = -1, `1x2_tie` = 2.4, `1x2_away` = -1), ctx = "1x2 fb diff= 0")
expect_row(R, 3, bets$bet_type, list(`1x2_home` = -1, `1x2_tie` = -1, `1x2_away` = 2.5), ctx = "1x2 fb diff=-1")
expect_row(R, 4, bets$bet_type, list(`1x2_home` = -1, `1x2_tie` = 2.4, `1x2_away` = -1), ctx = "1x2 fb diff= 0 (2)")

# Basketball 1x2 without draw
match_1x2 <- odds_row(o_home = 1.9, o_away = 1.9)
draws <- tibble(home_goals = c(100L, 99L), away_goals = c(95L, 101L))
bets <- collect_match_bets(match_1x2, NULL, NULL, cfg_basketball)
expect_absent(bets, "1x2_tie", ctx = "basketball no draw")
R <- build_return_matrix(draws, bets)
expect_row(R, 1, bets$bet_type, list(`1x2_home` = 0.9, `1x2_away` = -1), ctx = "bball diff=+5")
expect_row(R, 2, bets$bet_type, list(`1x2_home` = -1, `1x2_away` = 0.9), ctx = "bball diff=-2")

# ═══════════════════════════════════════════════════════════════════════════
# Summary
# ═══════════════════════════════════════════════════════════════════════════


# ═══════════════════════════════════════════════════════════════════════════
# GROUP 6: End-to-end smoke test via get_kelly_joint
# Ensures the market_variant column flows correctly through the optimiser
# boundary.  Regression guard: if build_return_matrix ever drops the new
# column, we want a fast signal rather than a wrong bet in production.
# ═══════════════════════════════════════════════════════════════════════════

section("Smoke: collect -> build -> get_kelly_joint")

# Football-style 3-way 1x2 with a small posterior.  Build 4000 synthetic
# draws from a Poisson(1.3)-Poisson(1.2) score process; the true Kelly
# fractions should be small positive numbers on each positive-EV outcome.
set.seed(101)
S <- 4000
draws <- tibble(
  home_goals = rpois(S, 1.3),
  away_goals = rpois(S, 1.2)
)
match_1x2 <- odds_row(o_home = 2.4, o_draw = 3.5, o_away = 3.2)
bets <- collect_match_bets(match_1x2, NULL, NULL, cfg_football)
R <- build_return_matrix(draws, bets)
fit <- get_kelly_joint(net_return = R, max_stake = 1.0)
{
  ok <- length(fit$solution) == nrow(bets) &&
    all(fit$solution >= -1e-9) &&
    sum(fit$solution) <= 1 + 1e-6
  if (ok) {
    .pass_count <<- .pass_count + 1
    cat(sprintf(
      "  PASS [smoke-1x2]: f = (%s), sum = %.4f\n",
      paste(sprintf("%.4f", fit$solution), collapse = ", "),
      sum(fit$solution)
    ))
  } else {
    .fail_count <<- .fail_count + 1
    cat(sprintf(
      "  FAIL [smoke-1x2]: f = %s, sum = %.4f\n",
      paste(fit$solution, collapse = ", "), sum(fit$solution)
    ))
  }
}

# Football integer HC (the bug site) — verify the new path works end-to-end.
match_hc <- odds_row(change = 1, o_home = 2.5, o_draw = 3.5, o_away = 2.5)
bets <- collect_match_bets(NULL, match_hc, NULL, cfg_football)
stopifnot("market_variant" %in% names(bets))
R <- build_return_matrix(draws, bets)
fit <- get_kelly_joint(net_return = R, max_stake = 1.0)
{
  ok <- length(fit$solution) == nrow(bets) &&
    all(fit$solution >= -1e-9) &&
    sum(fit$solution) <= 1 + 1e-6
  if (ok) {
    .pass_count <<- .pass_count + 1
    cat(sprintf(
      "  PASS [smoke-hc-euro]: f = (%s), sum = %.4f\n",
      paste(sprintf("%.4f", fit$solution), collapse = ", "),
      sum(fit$solution)
    ))
  } else {
    .fail_count <<- .fail_count + 1
    cat(sprintf(
      "  FAIL [smoke-hc-euro]: f = %s, sum = %.4f\n",
      paste(fit$solution, collapse = ", "), sum(fit$solution)
    ))
  }
}

# ═══════════════════════════════════════════════════════════════════════════
# GROUP 7: Fractional-growth curve
# ═══════════════════════════════════════════════════════════════════════════

section("Fractional-growth curve")

# Binary bet: α(2-α) should be exact (up to SAA noise at large S).
set.seed(201)
S <- 200000
win <- rbinom(S, 1, 0.60)
R_bin <- matrix(win * 2.0 - 1, ncol = 1)
f_star <- get_kelly_joint(net_return = R_bin, max_stake = 1.0)$solution
curve_bin <- fractional_growth_curve(R_bin, f_star, c(0.25, 0.5, 0.75, 1.0))
for (i in seq_len(nrow(curve_bin))) {
  a <- curve_bin$alpha[i]
  pred <- a * (2 - a)
  diff <- abs(curve_bin$ratio_to_full[i] - pred)
  ok <- diff < 5e-3 # α(2-α) should be exact for binary ± SAA noise
  if (ok) {
    .pass_count <- .pass_count + 1
  } else {
    .fail_count <- .fail_count + 1
    cat(sprintf(
      "  FAIL [binary α=%.2f]: ratio=%.4f, α(2-α)=%.4f, diff=%.4f\n",
      a, curve_bin$ratio_to_full[i], pred, diff
    ))
  }
}

# Ratio at α=1 is exactly 1 by definition
{
  r1 <- curve_bin$ratio_to_full[curve_bin$alpha == 1]
  if (abs(r1 - 1) < 1e-9) {
    .pass_count <- .pass_count + 1
  } else {
    .fail_count <- .fail_count + 1
    cat(sprintf("  FAIL [ratio at α=1]: got %.6f, expected 1\n", r1))
  }
}

# 3-way market: ratio at α=0.5 should be materially less than 0.75
# (per audit V12 finding, empirically ~0.56).
set.seed(202)
p <- c(0.55, 0.25, 0.20)
o <- c(2.0, 3.5, 5.0)
R_exact <- diag(o - 1) - (1 - diag(3))
idx <- sample(3, S, replace = TRUE, prob = p)
R_3way <- R_exact[idx, , drop = FALSE]
f_star_3 <- get_kelly_joint(net_return = R_3way, max_stake = 1.0)$solution
curve_3 <- fractional_growth_curve(R_3way, f_star_3, c(0.5, 1.0))
{
  r50 <- curve_3$ratio_to_full[curve_3$alpha == 0.5]
  if (r50 < 0.70 && r50 > 0.40) {
    # Within the regime where the α(2-α) heuristic is noticeably wrong.
    .pass_count <- .pass_count + 1
    cat(sprintf(
      "  PASS [3-way α=0.5]: ratio=%.3f (< 0.75 binary prediction)\n", r50
    ))
  } else {
    .fail_count <- .fail_count + 1
    cat(sprintf(
      "  FAIL [3-way α=0.5]: ratio=%.3f, expected 0.40 < r < 0.70\n", r50
    ))
  }
}

# Schema: should have alpha, G_at_alpha_f, ratio_to_full, binary_prediction
{
  req <- c("alpha", "G_at_alpha_f", "ratio_to_full", "binary_prediction")
  if (all(req %in% names(curve_bin))) {
    .pass_count <- .pass_count + 1
  } else {
    .fail_count <- .fail_count + 1
    cat(sprintf(
      "  FAIL [schema]: missing %s\n",
      paste(setdiff(req, names(curve_bin)), collapse = ", ")
    ))
  }
}

cat(sprintf(
  "\n──────────────────────────────────────\n  %d passed, %d failed\n",
  .pass_count, .fail_count
))
if (.fail_count > 0) quit(status = 1)
