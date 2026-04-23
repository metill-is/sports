# EV, Kelly fraction, stake, and bet-pass filter (pure math).
#
# Input contract: frame with columns p_model (num in [0,1]) and odds (num > 1).
# Output contract: same frame plus ev, kelly_f, stake, pass.

suppressPackageStartupMessages({
  library(dplyr)
})

#' Compute EV, Kelly fraction, stake, and bet-pass filter per row.
#'
#' @param df Frame with p_model and odds columns.
#' @param bankroll Notional bankroll for stake sizing.
#' @param kelly_frac Fractional-Kelly multiplier in [0, 1].
#' @param min_ev Minimum EV to pass the bet filter (default 0.03).
#' @param min_bet Minimum stake to pass the bet filter (default 200 - Lengjan minimum).
compute_ev_kelly <- function(df, bankroll, kelly_frac,
                             min_ev = 0.03, min_bet = 200) {
  df |>
    mutate(
      ev = p_model * odds - 1,
      kelly_f = pmax(0, pmin(1, ev / (odds - 1))),
      stake = round(bankroll * kelly_frac * kelly_f),
      pass = ev >= min_ev & stake >= min_bet
    )
}
