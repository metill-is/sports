# Extract market probabilities from posterior (total_goals, goal_diff) draws.
#
# Input contract: tidy frame with columns
#   variant      chr   - variant tag (e.g. "bvp", "v3")
#   match_id     chr   - match identifier
#   iteration    int   - posterior draw index
#   total_goals  num   - S = home_goals + away_goals
#   goal_diff    num   - D = home_goals - away_goals (continuous for Student-t)
#
# Output contract: tidy frame with columns
#   variant chr, match_id chr, market chr, outcome chr, line num (NA for 1x2), p_model num

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

#' Compute 1x2 market probabilities from goal_diff draws.
#' @param draws Posterior draws frame.
#' @param draw_threshold |D| <= threshold counts as a draw (default 0.5).
compute_1x2_probs <- function(draws, draw_threshold = 0.5) {
  draws |>
    group_by(variant, match_id) |>
    summarise(
      H = mean(goal_diff > draw_threshold),
      D = mean(abs(goal_diff) <= draw_threshold),
      A = mean(goal_diff < -draw_threshold),
      .groups = "drop"
    ) |>
    pivot_longer(cols = c(H, D, A), names_to = "outcome", values_to = "p_model") |>
    mutate(market = "1x2", line = NA_real_) |>
    select(variant, match_id, market, outcome, line, p_model)
}

#' Compute totals-market over/under probabilities for each requested line.
compute_totals_probs <- function(draws, lines = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5)) {
  keys <- draws |> distinct(variant, match_id)
  grid <- tidyr::crossing(keys, line = lines)

  # Vectorised: for each (variant, match_id, line) compute P(S > line) by joining
  # back to the draws and aggregating.
  draws_per_key <- draws |>
    select(variant, match_id, iteration, total_goals)

  grid |>
    left_join(draws_per_key,
      by = c("variant", "match_id"),
      relationship = "many-to-many"
    ) |>
    group_by(variant, match_id, line) |>
    summarise(
      p_over = mean(total_goals > line),
      .groups = "drop"
    ) |>
    mutate(p_under = 1 - p_over) |>
    pivot_longer(cols = c(p_over, p_under), names_to = "outcome", values_to = "p_model") |>
    mutate(
      outcome = sub("^p_", "", outcome),
      market = "totals"
    ) |>
    select(variant, match_id, market, outcome, line, p_model)
}

#' Compute handicap probabilities for each requested line.
#' Convention: `line` is the handicap added to home goals.
#'   home_cover when (goal_diff + line) > 0; away_cover when (goal_diff + line) < 0.
#'   Push (== 0) treated as a loss for both sides - rare at half-line handicaps.
compute_handicap_probs <- function(draws, lines = c(-1.5, -0.5, 0.5, 1.5)) {
  keys <- draws |> distinct(variant, match_id)
  grid <- tidyr::crossing(keys, line = lines)

  draws_per_key <- draws |>
    select(variant, match_id, iteration, goal_diff)

  grid |>
    left_join(draws_per_key,
      by = c("variant", "match_id"),
      relationship = "many-to-many"
    ) |>
    group_by(variant, match_id, line) |>
    summarise(
      p_home_cover = mean(goal_diff + line > 0),
      p_away_cover = mean(goal_diff + line < 0),
      .groups = "drop"
    ) |>
    pivot_longer(
      cols = c(p_home_cover, p_away_cover),
      names_to = "outcome",
      values_to = "p_model"
    ) |>
    mutate(
      outcome = sub("^p_", "", outcome),
      market = "handicap"
    ) |>
    select(variant, match_id, market, outcome, line, p_model)
}

#' Compute all market probabilities in one go.
compute_all_market_probs <- function(draws,
                                     totals_lines = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5),
                                     handicap_lines = c(-1.5, -0.5, 0.5, 1.5),
                                     draw_threshold = 0.5) {
  bind_rows(
    compute_1x2_probs(draws, draw_threshold = draw_threshold),
    compute_totals_probs(draws, lines = totals_lines),
    compute_handicap_probs(draws, lines = handicap_lines)
  )
}
