#' Out-of-sample probability scores: Brier and log-loss over frozen as-of `p`.
#'
#' The PRIMARY walk-forward verdict. Rows with `NA` win (bets that never
#' settled within the horizon) are dropped before scoring. `p` is clamped to
#' `[eps, 1-eps]` so a degenerate 0/1 forecast cannot send log-loss to Inf.
#' @param settled Tibble with numeric `p` and logical `win`.
#' @param eps Clamp bound. Default `1e-6`.
#' @return One-row tibble `(n, brier, log_loss)`; `n = 0` -> NA scores.
#' @export
bt_oos_scores <- function(settled, eps = 1e-6) {
  d <- settled[!is.na(settled$win), , drop = FALSE]
  if (nrow(d) == 0L) {
    return(tibble::tibble(n = 0L, brier = NA_real_, log_loss = NA_real_))
  }
  p <- pmin(pmax(d$p, eps), 1 - eps)
  y <- as.numeric(d$win)
  tibble::tibble(
    n = nrow(d),
    brier = mean((p - y)^2),
    log_loss = -mean(y * log(p) + (1 - y) * log(1 - p))
  )
}

#' Upper-bound the odds snapshots a decision at cutoff `d` may see.
#'
#' G3/G4: `prepare_odds` bounds `scraped_at` from below only and takes
#' `slice_max(scraped_at)`. Pre-slicing here to `scraped_at <= d + 12h` and
#' writing the result into the isolated `wf_root` makes the decider unable to
#' select a closing/post-result snapshot.
#' @param odds Tibble from `read_table("odds")` (has `scraped_at`).
#' @param d Cutoff date.
#' @return `odds` restricted to pre-cutoff snapshots.
#' @export
bt_wf_slice_odds <- function(odds, d) {
  if (nrow(odds) == 0L) {
    return(odds)
  }
  cutoff <- as.POSIXct(format(as.Date(d)), tz = "UTC") + lubridate::dhours(12)
  odds[odds$scraped_at <= cutoff, , drop = FALSE]
}

#' Large `max_age_hours` so prepare_odds' lower bound never drops a
#' legitimately-old historical snapshot once the upper bound is enforced by
#' [bt_wf_slice_odds()] (G3). ~10 years.
#' @return Numeric hours.
#' @export
bt_wf_max_age_hours <- function() 24 * 365 * 10

#' Restrict OOS candidates to matches STRICTLY after the cutoff, within horizon.
#'
#' G1/G2: neutralises the non-strict `schedules >= d` / `odds match_date >= d`
#' operators even if a cutoff coincides with a match day. A day-`d` match is in
#' the (inclusive) training set, so it must never be a bet.
#' @param candidates Tibble with `match_date`.
#' @param d Training cutoff date.
#' @param horizon_days OOS window length.
#' @return Candidates with `match_date > d & match_date <= d + horizon_days`.
#' @export
bt_wf_filter_oos <- function(candidates, d, horizon_days) {
  if (nrow(candidates) == 0L) {
    return(candidates)
  }
  d <- as.Date(d)
  hi <- d + as.integer(horizon_days)
  candidates[candidates$match_date > d & candidates$match_date <= hi, , drop = FALSE]
}

#' Assert the OOS bet match-set is disjoint from the training match-set (G1).
#'
#' Training = results with `match_date <= d` (what prepare_data trained on).
#' @param oos OOS candidates (`match_date`, `home_team`, `away_team`).
#' @param results Full results store.
#' @param d Training cutoff.
#' @return `TRUE` if disjoint.
#' @export
bt_wf_training_disjoint <- function(oos, results, d) {
  d <- as.Date(d)
  trn <- results[results$match_date <= d, , drop = FALSE]
  tkey <- paste(trn$match_date, trn$home_team, trn$away_team, sep = "\r")
  okey <- paste(oos$match_date, oos$home_team, oos$away_team, sep = "\r")
  length(intersect(tkey, okey)) == 0L
}
