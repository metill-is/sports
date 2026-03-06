#' Kelly criterion utilities
#'
#' Shared formatting for bet output.

box::use(
  glue[glue],
  dplyr[mutate, filter, if_else]
)

#' Apply EV, scaled Kelly, bet amount and display text to results
#'
#' Expects input tibble to have columns: p, o, kelly (from joint optimiser).
#' Adds: ev, bet_amount, pred, p_o, text. Overwrites kelly with scaled value.
#'
#' @param d Tibble with p, o, kelly columns
#' @param cfg Config list (from bets.yml)
#' @return Mutated tibble with betting text and amounts
#' @export
format_bet_text <- function(d, cfg) {
  b <- cfg$bankroll
  d |>
    mutate(
      ev = round(p * (o - 1) - (1 - p), 2),
      kelly = kelly * b$kelly_frac,
      bet_amount = round(kelly * b$cur_pool, b$bet_digits),
      kelly = round(kelly, 2),
      pred = round(p, 3),
      p_o = round(1 / o, 3),
      text = glue(
        "@{o} {b$currency}={bet_amount} (f={kelly}, ev={ev})"
      ),
      text = if_else(bet_amount < b$min_bet_amount, "", text)
    )
}
