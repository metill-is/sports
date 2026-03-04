#' Kelly criterion optimisation and bet formatting
#'
#' Shared utilities for all market modules.

box::use(
  nloptr[nloptr],
  glue[glue],
  dplyr[mutate, if_else]
)

#' Optimal Kelly criterion allocation across outcomes
#'
#' @param p Probability vector (one per outcome)
#' @param o Odds vector (decimal, one per outcome)
#' @param p0 Optional counter-probability vector (default: 1 - p)
#' @return Numeric vector of optimal fractions to bet per outcome
#' @export
get_kelly <- function(p, o, p0 = NULL) {
  if (is.null(p0)) {
    p0 <- 1 - p
  }

  R <- o - 1

  kelly_objective <- function(f) {
    sum(p * log(1 + f * R) + p0 * log(1 - f))
  }

  f_init <- rep(1 / length(p), length(p))

  constraint <- function(f) {
    sum(f) - 1
  }

  lb <- rep(0, length(p))
  ub <- rep(1, length(p))

  result <- nloptr(
    x0 = f_init,
    eval_f = function(f) -kelly_objective(f),
    eval_g_ineq = function(f) constraint(f),
    lb = lb,
    ub = ub,
    opts = list("algorithm" = "NLOPT_LN_COBYLA", "xtol_rel" = 1e-8)
  )

  result$solution
}

#' Apply EV, scaled Kelly, bet amount and display text to results
#'
#' Expects input tibble to have columns: p, o, kelly (from second Kelly pass).
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
