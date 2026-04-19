#' Unit tests for clv_tracker.R
#'
#' Run from Sports/ directory:
#'   Rscript R/bets/test_clv_tracker.R

Sys.setlocale("LC_ALL", "is_IS.UTF-8")

box::use(
  . / clv_tracker[
    normalise_odds_long, compute_closing_odds, compute_clv, summarise_clv
  ],
  dplyr[tibble, filter, arrange]
)

# ─── runner ───────────────────────────────────────────────────────────────

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
# GROUP 1: normalise_odds_long — wide → long
# ═══════════════════════════════════════════════════════════════════════════

section("normalise_odds_long — 1x2 market")

raw_1x2 <- tibble(
  date = as.Date(c("2026-04-18", "2026-04-18")),
  home = c("A", "A"),
  away = c("B", "B"),
  o_home = c(1.80, 1.75),
  o_draw = c(3.40, 3.50),
  o_away = c(4.20, 4.40),
  scraped_at = as.POSIXct(c("2026-04-17 08:00:00", "2026-04-18 07:00:00"),
    tz = "UTC"
  )
)
long_1x2 <- normalise_odds_long(raw_1x2, market = "outcome")
expect_equal(nrow(long_1x2), 6L, "1x2: 2 scrapes × 3 outcomes = 6")
expect_equal(
  sort(unique(long_1x2$outcome)), c("away", "home", "tie"),
  "1x2 outcomes expanded, draw normalised to tie (Sports bets_log convention)"
)
expect_true(all(is.na(long_1x2$line)), "1x2 has no line value")

section("normalise_odds_long — totals")

raw_tot <- tibble(
  date = as.Date("2026-04-18"),
  home = "A", away = "B",
  limit = c(58.5, 59.5),
  o_over = c(1.90, 1.95),
  o_under = c(1.85, 1.80),
  scraped_at = as.POSIXct("2026-04-18 09:00:00", tz = "UTC")
)
long_tot <- normalise_odds_long(raw_tot, market = "totals")
expect_equal(nrow(long_tot), 4L, "totals: 2 limits × 2 sides = 4 rows")
expect_equal(
  sort(unique(long_tot$outcome)), c("over", "under"),
  "totals outcomes over/under"
)
expect_true(
  all(long_tot$line %in% c(58.5, 59.5)),
  "totals line populated from limit"
)

section("normalise_odds_long — handicap (Lengjan string format)")

raw_hc <- tibble(
  date = as.Date("2026-04-18"),
  home = "A", away = "B",
  change = c("0-1", "1-0"),
  o_home = c(1.40, 2.80),
  o_draw = c(4.60, 4.80),
  o_away = c(5.80, 2.50),
  scraped_at = as.POSIXct("2026-04-18 09:00:00", tz = "UTC")
)
long_hc <- normalise_odds_long(raw_hc, market = "handicap")
expect_equal(
  nrow(long_hc), 6L,
  "handicap: 2 lines × 3 outcomes (home/tie/away) = 6"
)
expect_true(
  all(long_hc$line %in% c(-1, 1)),
  "change parsed via parse_handicap"
)

# ═══════════════════════════════════════════════════════════════════════════
# GROUP 2: compute_closing_odds — last scrape per match × line × outcome
# ═══════════════════════════════════════════════════════════════════════════

section("compute_closing_odds — takes latest scraped_at per key")

long <- tibble(
  date = as.Date("2026-04-18"),
  home = "A", away = "B",
  market = "outcome",
  outcome = c("home", "home", "home", "away"),
  line = NA_real_,
  odds = c(1.80, 1.75, 1.70, 4.40),
  scraped_at = as.POSIXct(c(
    "2026-04-17 08:00:00", # early scrape, home
    "2026-04-18 07:00:00", # later scrape, home
    "2026-04-18 14:00:00", # latest, home  ← should win
    "2026-04-18 14:00:00" # away, only one scrape
  ), tz = "UTC")
)
close <- compute_closing_odds(long)
home_row <- close[close$outcome == "home", ]
expect_equal(nrow(home_row), 1L, "one row per (match, outcome, line)")
expect_near(home_row$odds, 1.70, "latest scrape wins")

# ═══════════════════════════════════════════════════════════════════════════
# GROUP 3: compute_clv — merge bets × closing_odds, compute CLV
# ═══════════════════════════════════════════════════════════════════════════

section("compute_clv — basic per-bet CLV")

bets <- tibble(
  date_match = as.Date("2026-04-18"),
  home = c("A", "C"),
  away = c("B", "D"),
  market = c("outcome", "outcome"),
  outcome = c("home", "home"),
  line = NA_real_,
  odds = c(1.80, 2.00) # placement odds
)
close <- tibble(
  date = as.Date("2026-04-18"),
  home = c("A", "C"),
  away = c("B", "D"),
  market = c("outcome", "outcome"),
  outcome = c("home", "home"),
  line = NA_real_,
  odds = c(1.70, 2.10) # closing odds
)
with_clv <- compute_clv(bets, close)
expect_equal(nrow(with_clv), 2L, "one row per bet preserved")
# Bet 1: we got 1.80, closed 1.70 → CLV = 1.70/1.80 - 1 = -0.05555…
expect_near(
  with_clv$clv[1], 1.70 / 1.80 - 1,
  "CLV negative when placement > close"
)
# Bet 2: we got 2.00, closed 2.10 → CLV = 2.10/2.00 - 1 = +0.05
expect_near(
  with_clv$clv[2], 2.10 / 2.00 - 1,
  "CLV positive when placement < close"
)

section("compute_clv — unmatched bets get NA clv")

bets2 <- tibble(
  date_match = as.Date("2026-04-18"),
  home = "E", away = "F", # no closing odds row for this match
  market = "outcome", outcome = "home", line = NA_real_,
  odds = 1.90
)
res2 <- compute_clv(bets2, close)
expect_equal(nrow(res2), 1L, "unmatched bet still returned")
expect_true(is.na(res2$clv), "unmatched bet: clv is NA")

section("compute_clv — line match for totals")

bets_tot <- tibble(
  date_match = as.Date("2026-04-18"),
  home = "A", away = "B",
  market = c("totals", "totals"),
  outcome = c("over", "over"),
  line = c(58.5, 60.5),
  odds = c(1.90, 2.00)
)
close_tot <- tibble(
  date = as.Date("2026-04-18"),
  home = "A", away = "B",
  market = c("totals", "totals"),
  outcome = c("over", "over"),
  line = c(58.5, 60.5),
  odds = c(1.85, 2.10)
)
res3 <- compute_clv(bets_tot, close_tot)
expect_near(res3$clv[1], 1.85 / 1.90 - 1, "totals line=58.5 CLV")
expect_near(res3$clv[2], 2.10 / 2.00 - 1, "totals line=60.5 CLV")

# ═══════════════════════════════════════════════════════════════════════════
# GROUP 4: summarise_clv — mean CLV grouped
# ═══════════════════════════════════════════════════════════════════════════

section("summarise_clv — by country")

bets_multi <- tibble(
  country = c("iceland", "iceland", "denmark", "denmark"),
  clv = c(0.02, 0.00, -0.03, -0.01)
)
by_c <- summarise_clv(bets_multi, by = "country")
expect_equal(nrow(by_c), 2L, "one row per country")
isl <- by_c[by_c$country == "iceland", ]
den <- by_c[by_c$country == "denmark", ]
expect_near(isl$mean_clv, 0.01, "iceland mean CLV")
expect_near(den$mean_clv, -0.02, "denmark mean CLV")
expect_equal(isl$n, 2L, "iceland n")

section("summarise_clv — NA clv rows are dropped from the mean")

bets_with_na <- tibble(
  country = c("iceland", "iceland", "iceland"),
  clv = c(0.02, NA_real_, 0.04)
)
s <- summarise_clv(bets_with_na, by = "country")
expect_equal(s$n, 2L, "NA clv rows excluded from n")
expect_near(s$mean_clv, 0.03, "mean of non-NA clv")

# ═══════════════════════════════════════════════════════════════════════════
# Report
# ═══════════════════════════════════════════════════════════════════════════

cat(sprintf(
  "\n──────────────────────────────────────\n  %d passed, %d failed\n",
  .pass_count, .fail_count
))
if (.fail_count > 0) quit(status = 1)
