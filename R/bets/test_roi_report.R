#' Unit tests for roi_report.R
#'
#' Run from Sports/ directory:
#'   Rscript R/bets/test_roi_report.R
#'
#' Covers the core ROI/PnL helpers that back the `/bet-roi` tool.

Sys.setlocale("LC_ALL", "is_IS.UTF-8")

box::use(
  . / roi_report[compute_roi, summarise_roi, load_bets_logs],
  dplyr[tibble],
  fs[dir_create, path, file_create]
)

# ─── Inline assertion runner ───────────────────────────────────────────────

.fail_count <- 0
.pass_count <- 0

expect_near <- function(actual, expected, ctx, tol = 1e-9) {
  ok <- !is.null(actual) && !is.null(expected) &&
    !is.na(actual) && !is.na(expected) &&
    isTRUE(all.equal(actual, expected, tolerance = tol))
  if (ok) {
    .pass_count <<- .pass_count + 1
  } else {
    .fail_count <<- .fail_count + 1
    cat(sprintf(
      "  FAIL [%s]: got %s, expected %s\n",
      ctx, format(actual), format(expected)
    ))
  }
}

expect_equal <- function(actual, expected, ctx) {
  if (identical(actual, expected)) {
    .pass_count <<- .pass_count + 1
  } else {
    .fail_count <<- .fail_count + 1
    cat(sprintf(
      "  FAIL [%s]: got %s, expected %s\n",
      ctx, format(actual), format(expected)
    ))
  }
}

expect_true <- function(cond, ctx) {
  if (isTRUE(cond)) {
    .pass_count <<- .pass_count + 1
  } else {
    .fail_count <<- .fail_count + 1
    cat(sprintf("  FAIL [%s]: expected TRUE\n", ctx))
  }
}

section <- function(name) cat(sprintf("\n── %s ──\n", name))

# ═══════════════════════════════════════════════════════════════════════════
# GROUP 1: compute_roi — core summary on one block of bets
# ═══════════════════════════════════════════════════════════════════════════

section("compute_roi — basic settled bets")

bets <- tibble(
  bet_amount = c(100, 100, 50),
  win = c(TRUE, FALSE, TRUE),
  pnl = c(90, -100, 40),
  probability = c(0.7, 0.4, 0.6)
)
r <- compute_roi(bets)

expect_equal(r$n, 3L, "n counts rows")
expect_equal(r$wins, 2L, "wins counts TRUE")
expect_near(r$exp_wins, 1.7, "exp_wins sums probability")
expect_near(r$calibration, 2 / 1.7, "calibration = wins / exp_wins")
expect_near(r$turnover, 250, "turnover sums bet_amount")
expect_near(r$pnl, 30, "pnl sums pnl column")
expect_near(r$roi_pct, 12, "roi_pct = 100 * pnl / turnover")

section("compute_roi — empty frame returns NA-safe summary")

empty_bets <- tibble(
  bet_amount = numeric(0),
  win = logical(0),
  pnl = numeric(0),
  probability = numeric(0)
)
re <- compute_roi(empty_bets)
expect_equal(re$n, 0L, "empty: n = 0")
expect_equal(re$wins, 0L, "empty: wins = 0")
expect_true(is.na(re$calibration), "empty: calibration NA")
expect_true(is.na(re$roi_pct), "empty: roi_pct NA")

section("compute_roi — NA pnl rows are treated as unsettled and excluded")

mixed <- tibble(
  bet_amount = c(100, 200, 100),
  win = c(TRUE, NA, FALSE),
  pnl = c(90, NA, -100),
  probability = c(0.7, 0.5, 0.4)
)
rm <- compute_roi(mixed)
expect_equal(rm$n, 2L, "drops NA-win rows")
expect_equal(rm$wins, 1L, "wins after drop = 1")
expect_near(rm$turnover, 200, "turnover after drop = 200")
expect_near(rm$pnl, -10, "pnl after drop = -10")
expect_near(rm$roi_pct, -5, "roi_pct after drop = -5 %")

# ═══════════════════════════════════════════════════════════════════════════
# GROUP 2: summarise_roi — group by arbitrary columns
# ═══════════════════════════════════════════════════════════════════════════

section("summarise_roi — by country")

multi <- tibble(
  country = c("iceland", "iceland", "denmark", "denmark"),
  bet_amount = c(100, 100, 50, 50),
  win = c(TRUE, FALSE, TRUE, TRUE),
  pnl = c(90, -100, 40, 40),
  probability = c(0.6, 0.45, 0.5, 0.5)
)
by_country <- summarise_roi(multi, by = "country")
expect_equal(nrow(by_country), 2L, "one row per country")
isl <- by_country[by_country$country == "iceland", ]
den <- by_country[by_country$country == "denmark", ]
expect_near(isl$roi_pct, -5, "iceland roi = -5 %")
expect_near(den$roi_pct, 80, "denmark roi = 80 %")
expect_equal(isl$n, 2L, "iceland has 2 bets")
expect_equal(den$n, 2L, "denmark has 2 bets")

section("summarise_roi — by sport + country + sex (multi-key)")

multi2 <- tibble(
  sport = c("handball", "handball", "handball"),
  country = c("iceland", "iceland", "iceland"),
  sex = c("male", "female", "female"),
  bet_amount = c(100, 100, 100),
  win = c(TRUE, TRUE, FALSE),
  pnl = c(90, 80, -100),
  probability = c(0.6, 0.55, 0.45)
)
by_triple <- summarise_roi(multi2, by = c("sport", "country", "sex"))
expect_equal(nrow(by_triple), 2L, "two unique (sport,country,sex) cells")
fem <- by_triple[by_triple$sex == "female", ]
expect_equal(fem$n, 2L, "female cell has 2 bets")
expect_near(fem$pnl, -20, "female pnl = -20")

# ═══════════════════════════════════════════════════════════════════════════
# GROUP 3: load_bets_logs — walks */history/bets_log.csv
# ═══════════════════════════════════════════════════════════════════════════

section("load_bets_logs — reads nested bets_log.csv files")

tmp <- tempfile("roi_report_")
dir_create(tmp)
leagues <- c("handball/iceland", "football/iceland", "handball/denmark")
for (lg in leagues) {
  dir_create(path(tmp, lg, "history"))
  writeLines(c(
    "date_recommended,date_match,sport,country,sex,market,home,away,outcome,odds,probability,ev,kelly_frac,bet_amount,info,win,pnl,source",
    sprintf(
      "2026-04-18,2026-04-18,%s,%s,male,outcome,A,B,home,1.8,0.6,0.08,0.1,100,%s,TRUE,80,lengjan",
      strsplit(lg, "/")[[1]][1], strsplit(lg, "/")[[1]][2],
      if (lg == "football/iceland") "{\"note\":\"mixed\"}" else NA_character_
    )
  ), path(tmp, lg, "history", "bets_log.csv"))
}
# Also place an empty / zero-row file — should be skipped without crashing
empty_dir <- path(tmp, "basketball/iceland/history")
dir_create(empty_dir)
writeLines(
  "date_recommended,date_match,sport,country,sex,market,home,away,outcome,odds,probability,ev,kelly_frac,bet_amount,info,win,pnl,source",
  path(empty_dir, "bets_log.csv")
)

loaded <- load_bets_logs(tmp)
expect_equal(nrow(loaded), 3L, "picks up 3 non-empty logs")
expect_true(is.character(loaded$info), "info coerced to character (mixed types safe)")
expect_true(inherits(loaded$date_match, "Date"), "date_match parsed to Date")
expect_true(is.logical(loaded$win), "win parsed to logical")
expect_true(is.numeric(loaded$pnl), "pnl parsed to numeric")

# ═══════════════════════════════════════════════════════════════════════════
# Report
# ═══════════════════════════════════════════════════════════════════════════

cat(sprintf(
  "\n──────────────────────────────────────\n  %d passed, %d failed\n",
  .pass_count, .fail_count
))
if (.fail_count > 0) quit(status = 1)
