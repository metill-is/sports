# Shared fixtures for betting_pnl tests.
# Source this from individual test files:
#   source(here::here("R", "backtest", "betting_pnl", "tests", "helper.R"))

suppressPackageStartupMessages({
  library(tibble)
  library(dplyr)
})

# Fixture 1: posterior draws for two matches, two variants.
# 4 iterations x 2 matches x 2 variants = 16 rows.
fixture_posterior_draws <- tibble::tribble(
  ~variant, ~match_id, ~iteration, ~total_goals, ~goal_diff,
  "bvp", "m1_FramIA", 1L, 3, 1,
  "bvp", "m1_FramIA", 2L, 2, 0,
  "bvp", "m1_FramIA", 3L, 4, 2,
  "bvp", "m1_FramIA", 4L, 1, -1,
  "bvp", "m2_KeflBB", 1L, 5, -1,
  "bvp", "m2_KeflBB", 2L, 3, 1,
  "bvp", "m2_KeflBB", 3L, 4, 0,
  "bvp", "m2_KeflBB", 4L, 2, -2,
  "v3", "m1_FramIA", 1L, 3, 0.8,
  "v3", "m1_FramIA", 2L, 2, -0.2,
  "v3", "m1_FramIA", 3L, 4, 1.6,
  "v3", "m1_FramIA", 4L, 1, -0.7,
  "v3", "m2_KeflBB", 1L, 5, -1.3,
  "v3", "m2_KeflBB", 2L, 3, 0.4,
  "v3", "m2_KeflBB", 3L, 4, 0.1,
  "v3", "m2_KeflBB", 4L, 2, -1.9
)

# Fixture 2: real Lengjan odds rows (shape matches production CSV).
# One match, two scrapes (to test dedup), one market (1x2).
fixture_odds_1x2_raw <- tibble::tribble(
  ~date,         ~league,         ~home,  ~away, ~o_home, ~o_draw, ~o_away, ~scraped_at,
  "2026-04-12",  "Besta Deildin", "Fram", "ÍA",  2.08,    3.77,    2.74,    "2026-04-10T20:41:26Z",
  "2026-04-12",  "Besta Deildin", "Fram", "ÍA",  2.17,    3.71,    2.63,    "2026-04-12T14:43:31Z"
)

# Fixture 3: known outcomes for the fixture matches.
fixture_results <- tibble::tribble(
  ~match_id, ~date, ~home, ~away, ~home_goals, ~away_goals,
  "m1_FramIA", "2026-04-12", "Fram", "ÍA", 2, 1,
  "m2_KeflBB", "2026-04-17", "Keflavík", "Breiðablik", 1, 1
)
