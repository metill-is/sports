# Dispatch between in-sample and OOS prediction loading, normalise to a tidy
# (variant, match_id, market, outcome, line, p_model) frame.
#
# Phase 1 scope: OOS mode only. The predictions_archive snapshots only upcoming
# matches at fit time, so every archived row is by construction OOS (fit_date
# precedes match_date). In-sample mode is a stub - returns an empty frame with
# a warning - until model_d.csv is added to the prep pipeline (deferred Phase 2).

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
})

source(here::here("R", "storage", "store.R"))
source(here::here("R", "backtest", "betting_pnl", "market_probs.R"))

#' Load variant predictions from the archive and compute market probabilities.
#'
#' @param sports_dir Absolute path to Sports/ root.
#' @param variants Character vector of variant tags (e.g. c("bvp_noinflation", "v3_free_nu")).
#' @param mode Either "oos" (use fit_date < match_date archive rows) or "in_sample" (stub).
#' @param totals_lines Numeric lines for the totals market.
#' @param handicap_lines Numeric lines for the handicap market.
#' @param draw_threshold Threshold for |goal_diff| to count as 1x2 draw.
load_variant_predictions <- function(sports_dir,
                                     variants,
                                     mode = c("oos", "in_sample"),
                                     totals_lines = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5),
                                     handicap_lines = c(-1.5, -0.5, 0.5, 1.5),
                                     draw_threshold = 0.5) {
  mode <- match.arg(mode)

  if (mode == "in_sample") {
    warning(
      "in_sample mode is a Phase 1 stub - archive does not yet contain ",
      "in-sample posterior draws. Returning empty frame."
    )
    return(empty_prob_frame())
  }

  frames <- list()
  for (v in variants) {
    archive <- read_predictions_archive(
      sports_dir = sports_dir,
      sport = "football", country = "iceland", sex = "male",
      variant = v
    )
    if (is.null(archive) || nrow(archive) == 0) {
      warning("No archive rows found for variant ", v, " - skipping.")
      next
    }

    # OOS guard: keep only rows where the fit predates the match.
    archive <- archive |>
      mutate(
        fit_date_d = as.Date(fit_date),
        match_date_d = as.Date(date)
      ) |>
      filter(fit_date_d < match_date_d)

    if (nrow(archive) == 0) {
      warning("Variant ", v, " has no OOS rows after fit_date < match_date filter.")
      next
    }

    draws <- archive |>
      mutate(
        variant = v,
        match_id = paste(as.character(date), home, away, sep = "|"),
        total_goals = home_goals + away_goals,
        goal_diff = home_goals - away_goals
      ) |>
      select(variant, match_id, iteration, total_goals, goal_diff)

    frames[[v]] <- compute_all_market_probs(
      draws,
      totals_lines = totals_lines,
      handicap_lines = handicap_lines,
      draw_threshold = draw_threshold
    )
  }

  if (length(frames) == 0) {
    return(empty_prob_frame())
  }

  bind_rows(frames)
}

empty_prob_frame <- function() {
  tibble::tibble(
    variant = character(),
    match_id = character(),
    market = character(),
    outcome = character(),
    line = numeric(),
    p_model = numeric()
  )
}
