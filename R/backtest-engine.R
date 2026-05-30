# R/backtest-engine.R
#' @include settle.R config.R backtest-stake.R
NULL

bt_empty_settled <- function() {
  u <- bt_empty_universe()
  u$win <- logical()
  u$stake <- numeric()
  u$pnl <- numeric()
  u$pool_before <- numeric()
  u$cum_pnl <- numeric()
  u$pool_after <- numeric()
  u
}

#' Run a backtest: settle a bet universe and size stakes.
#'
#' Win/loss is stake-independent, so the engine first resolves `win` per bet by
#' reusing [compute_settlement()] (per `(sport, country)` with the same
#' `tie_threshold` as `settle_ledger()`, so push/boundary semantics match the
#' live decider). Bets with no matching result are dropped (counted in the
#' `"pending"` attribute). The remaining settled bets pass to `stake_rule`,
#' which computes `stake` and `pnl`; the engine then accumulates `cum_pnl` and
#' `pool_after` in match-settlement order.
#' @param universe Tibble from [bt_load_universe()].
#' @param results Results tibble (`read_table("results")`).
#' @param stake_rule [stake_rolling()] (default) or [stake_fixed()].
#' @param initial_pool Starting bankroll; default from `bankroll.yml`.
#' @param match_date_window_days Reschedule fallback window; default `3L`
#'   (matches `settle_ledger()`).
#' @param ... Forwarded to `stake_rule`.
#' @return Per-bet tibble with `win, stake, pnl, pool_before, cum_pnl,
#'   pool_after`; `attr(., "pending")` is the count of unsettled bets dropped.
#' @export
bt_run <- function(universe, results, stake_rule = stake_rolling,
                   initial_pool = NULL, match_date_window_days = 3L, ...) {
  if (is.null(initial_pool)) initial_pool <- load_bankroll()$initial_pool
  if (nrow(universe) == 0L) {
    return(bt_empty_settled())
  }

  bets <- universe
  bets$odds_placed <- bets$odds
  bets$bet_amount <- 1
  bets$settled <- FALSE
  bets$win <- NA
  bets$pnl <- NA_real_

  leagues_cfg <- tryCatch(load_leagues(), error = function(e) list())
  tt_for <- function(sport, country) {
    for (lg in leagues_cfg) {
      if (identical(lg$sport, sport) && identical(lg$country, country)) {
        return(lg$betting$scoring$tie_threshold %||% 0)
      }
    }
    0
  }

  groups <- dplyr::distinct(bets[, c("sport", "country")])
  win <- rep(NA, nrow(bets))
  for (gi in seq_len(nrow(groups))) {
    sp <- groups$sport[[gi]]
    co <- groups$country[[gi]]
    gm <- which(bets$sport == sp & bets$country == co)
    g <- compute_settlement(bets[gm, , drop = FALSE], results,
      match_date_window_days = match_date_window_days,
      tie_threshold = tt_for(sp, co)
    )
    win[gm] <- g$win
  }

  universe$win <- win
  pending <- sum(is.na(universe$win))
  settled <- universe[!is.na(universe$win), , drop = FALSE]
  if (nrow(settled) == 0L) {
    out <- bt_empty_settled()
    attr(out, "pending") <- pending
    return(out)
  }

  staked <- stake_rule(settled,
    initial_pool = initial_pool,
    ref_pool = initial_pool, ...
  )
  staked <- staked[order(staked$match_date, staked$run_date), , drop = FALSE]
  staked$cum_pnl <- cumsum(staked$pnl)
  staked$pool_after <- initial_pool + staked$cum_pnl
  attr(staked, "pending") <- pending
  staked
}
