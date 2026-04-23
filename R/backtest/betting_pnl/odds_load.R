# Load Lengjan odds CSVs, deduplicate to closest-to-kickoff scrape, join results.
#
# Market semantics:
#   1x2       - one row per outcome (H/D/A) per match; no line.
#   totals    - one row per (over, under) per line; `line` is the O/U line.
#   handicap  - one row per (home_cover, away_cover) per change; `line` is the signed
#               head start for HOME. "0-1" -> line = -1 (home disadvantaged by 1),
#               "1-0" -> line = +1, "2-0" -> line = +2. The Lengjan CSV also carries
#               o_draw (the push market at integer lines) - ignored in Phase 1.
#
# Push handling: at integer handicap lines, goal_diff + line == 0 is a push (stake
# refunded). Phase 1 marks these as won = NA (same as unplayed) so they are excluded
# from PnL aggregation.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
})

canonical_match_id <- function(date, home, away) {
  paste(date, home, away, sep = "|")
}

# Returns the latest scrape per (date, home, away[, ...]) - closest to kickoff.
keep_closest_scrape <- function(df, extra_keys = character(0)) {
  keys <- c("date", "home", "away", extra_keys)
  df |>
    group_by(across(all_of(keys))) |>
    slice_max(order_by = scraped_at, n = 1, with_ties = FALSE) |>
    ungroup()
}

#' Load 1x2 odds, dedup to closest-to-kickoff, pivot to long (one row per outcome).
load_odds_1x2 <- function(path) {
  readr::read_csv(path, show_col_types = FALSE) |>
    keep_closest_scrape() |>
    mutate(match_id = canonical_match_id(date, home, away)) |>
    pivot_longer(
      cols = c(o_home, o_draw, o_away),
      names_to = "outcome",
      values_to = "odds"
    ) |>
    mutate(
      outcome = dplyr::case_when(
        outcome == "o_home" ~ "H",
        outcome == "o_draw" ~ "D",
        outcome == "o_away" ~ "A"
      ),
      market = "1x2",
      line = NA_real_
    ) |>
    select(match_id, date, home, away, market, outcome, line, odds)
}

#' Load totals odds. Each (match, line) has two rows (over, under) in the source CSV.
load_odds_totals <- function(path) {
  readr::read_csv(path, show_col_types = FALSE) |>
    keep_closest_scrape(extra_keys = "limit") |>
    mutate(match_id = canonical_match_id(date, home, away)) |>
    pivot_longer(
      cols = c(o_over, o_under),
      names_to = "outcome",
      values_to = "odds"
    ) |>
    mutate(
      outcome = sub("^o_", "", outcome),
      market = "totals",
      line = limit
    ) |>
    select(match_id, date, home, away, market, outcome, line, odds)
}

#' Load handicap odds. `change` is "H-A" (starting scoreline); home's head start line = H - A.
#' Draw-push column (o_draw) is ignored - see header for push semantics.
load_odds_handicap <- function(path) {
  readr::read_csv(path, show_col_types = FALSE) |>
    keep_closest_scrape(extra_keys = "change") |>
    mutate(
      home_h = as.numeric(sub("-.*$", "", change)),
      away_h = as.numeric(sub("^.*-", "", change)),
      line = home_h - away_h,
      match_id = canonical_match_id(date, home, away)
    ) |>
    pivot_longer(
      cols = c(o_home, o_away),
      names_to = "outcome",
      values_to = "odds"
    ) |>
    mutate(
      outcome = dplyr::case_when(
        outcome == "o_home" ~ "home_cover",
        outcome == "o_away" ~ "away_cover"
      ),
      market = "handicap"
    ) |>
    select(match_id, date, home, away, market, outcome, line, odds)
}

#' Bind market frames and attach actual outcomes (won per bet row).
#'
#' @param odds_long Long frame from load_odds_* (bound via bind_rows).
#' @param results Frame with match_id, home_goals, away_goals.
join_results <- function(odds_long, results) {
  results_keyed <- results |>
    select(match_id, home_goals, away_goals)

  odds_long |>
    left_join(results_keyed, by = "match_id") |>
    mutate(
      played = !is.na(home_goals),
      total_goals = home_goals + away_goals,
      goal_diff = home_goals - away_goals,
      push_handicap = market == "handicap" & played & (goal_diff + line == 0),
      won = dplyr::case_when(
        !played ~ NA,
        market == "handicap" & push_handicap ~ NA,
        market == "1x2" & outcome == "H" & goal_diff > 0 ~ TRUE,
        market == "1x2" & outcome == "D" & goal_diff == 0 ~ TRUE,
        market == "1x2" & outcome == "A" & goal_diff < 0 ~ TRUE,
        market == "1x2" ~ FALSE,
        market == "totals" & outcome == "over" & total_goals > line ~ TRUE,
        market == "totals" & outcome == "under" & total_goals < line ~ TRUE,
        market == "totals" ~ FALSE,
        market == "handicap" & outcome == "home_cover" & (goal_diff + line) > 0 ~ TRUE,
        market == "handicap" & outcome == "away_cover" & (goal_diff + line) < 0 ~ TRUE,
        market == "handicap" ~ FALSE,
        TRUE ~ NA
      )
    )
}

#' End-to-end odds loader.
#'
#' @param football_iceland_dir Path to lengjan-odds/data/football_iceland/.
#' @param results Frame with match_id, home_goals, away_goals.
load_all_odds <- function(football_iceland_dir, results) {
  bind_rows(
    load_odds_1x2(file.path(football_iceland_dir, "odds_1x2.csv")),
    load_odds_totals(file.path(football_iceland_dir, "odds_totals.csv")),
    load_odds_handicap(file.path(football_iceland_dir, "odds_handicap.csv"))
  ) |>
    join_results(results)
}
