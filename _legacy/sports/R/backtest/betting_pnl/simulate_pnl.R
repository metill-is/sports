# Per-bet PnL and aggregation by (variant, market, mode).

suppressPackageStartupMessages({
  library(dplyr)
})

#' Compute per-bet PnL.
#'
#' @param df Frame with stake, odds, won columns. won is TRUE/FALSE/NA (NA = unplayed).
#' @return Same frame plus a pnl column.
compute_pnl <- function(df) {
  df |>
    mutate(
      pnl = dplyr::case_when(
        stake == 0 ~ 0,
        is.na(won) ~ NA_real_,
        won ~ stake * (odds - 1),
        TRUE ~ -as.numeric(stake)
      )
    )
}

#' Aggregate bets by (variant, market, mode). Unplayed bets (pnl == NA) are excluded.
#'
#' @param bets Frame with variant, market, mode, stake, pnl columns (pnl from compute_pnl).
summarise_pnl <- function(bets) {
  bets |>
    filter(!is.na(pnl), stake > 0) |>
    group_by(variant, market, mode) |>
    summarise(
      n_bets = dplyr::n(),
      total_stake = sum(stake),
      total_pnl = sum(pnl),
      roi = total_pnl / total_stake,
      hit_rate = mean(pnl > 0),
      .groups = "drop"
    )
}
