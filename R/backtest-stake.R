# R/backtest-stake.R
NULL

#' Effective Kelly stake fraction per bet.
#'
#' Kept bets use their recorded effective fraction (`kelly`). Counterfactual
#' bets (no recorded `kelly`) are estimated as `kelly_raw * shrink`, where
#' `shrink` is the median `kelly / kelly_raw` over kept bets in the same run
#' (fallback: global median over all kept bets; final fallback: 0.05, near the
#' current operational `kelly_frac`).
#' @param universe Tibble with `run_id`, `kelly`, `kelly_raw`.
#' @return Numeric vector, one fraction per row.
#' @noRd
bt_effective_fraction <- function(universe) {
  kept <- !is.na(universe$kelly)
  pos <- universe$kelly_raw > 0
  shrink_global <- stats::median(
    (universe$kelly / universe$kelly_raw)[kept & pos],
    na.rm = TRUE
  )
  if (!is.finite(shrink_global)) shrink_global <- 0.05

  frac <- numeric(nrow(universe))
  for (r in unique(universe$run_id)) {
    rm <- universe$run_id == r
    rk <- rm & kept & pos
    shrink_r <- if (any(rk)) {
      stats::median(universe$kelly[rk] / universe$kelly_raw[rk], na.rm = TRUE)
    } else {
      shrink_global
    }
    frac[rm & kept] <- universe$kelly[rm & kept]
    frac[rm & !kept] <- universe$kelly_raw[rm & !kept] * shrink_r
  }
  frac
}

#' Fixed-reference-pool stake rule (no compounding).
#'
#' Sizes every bet off a constant `ref_pool` so cross-strategy / cross-market
#' ROI is comparable without compounding variance. `pnl` is derived from the
#' bet's `win` flag (set upstream by [bt_run()]).
#' @param universe Tibble with `win`, `odds`, plus the columns
#'   [bt_effective_fraction()] needs.
#' @param ref_pool Constant pool in ISK.
#' @param min_bet Bookmaker minimum stake in ISK. Bets whose computed stake
#'   falls below it are dropped (rows removed) — never placed — mirroring the
#'   live decider's `dropped_min_bet` stage. Default `0` = no floor.
#' @param ... Absorbs `initial_pool` (ignored) for a uniform stake-rule signature.
#' @return `universe` plus `stake`, `pnl`, `pool_before`; rows below `min_bet`
#'   removed when a floor is set.
#' @export
stake_fixed <- function(universe, ref_pool = NULL, min_bet = 0, ...) {
  if (is.null(ref_pool)) {
    dots <- list(...)
    ref_pool <- dots$initial_pool %||% load_bankroll()$initial_pool
  }
  frac <- bt_effective_fraction(universe)
  universe$stake <- round(frac * ref_pool)
  universe$pnl <- ifelse(universe$win,
    universe$stake * (universe$odds - 1),
    -universe$stake
  )
  universe$pool_before <- ref_pool
  universe[universe$stake >= min_bet, , drop = FALSE]
}

#' Rolling-bankroll stake rule (path-dependent; the money story).
#'
#' Walks decide runs in chronological order. For each run, the pool is
#' `initial_pool + sum(pnl)` over bets whose match settled *strictly before*
#' that run's `run_date`, mirroring the live `current_pool`. The run's bets are
#' sized off that pool (slate co-sizing, as joint Kelly does), then scaled down
#' if the slate total exceeds the daily-budget cap
#' `max(daily_budget_frac * pool, daily_budget_min_isk)`. `pnl` comes from each
#' bet's `win` flag.
#' @param universe Tibble with `run_id`, `run_date`, `match_date`, `win`,
#'   `odds`, plus the columns [bt_effective_fraction()] needs.
#' @param initial_pool Starting bankroll in ISK.
#' @param daily_budget_frac,daily_budget_min_isk Daily-budget cap parameters
#'   (defaults from `bankroll.yml`).
#' @param min_bet Bookmaker minimum stake in ISK. A bet whose stake — after the
#'   daily-budget cap — is below it is dropped (never placed), so it contributes
#'   no stake and no pnl to the rolling pool, mirroring the live decider's
#'   `dropped_min_bet` stage. The cap is applied over the full slate first (as
#'   live portfolio scaling precedes the min-bet filter), then the floor. Default
#'   `0` = no floor.
#' @param ... Absorbs `ref_pool` (ignored) for a uniform stake-rule signature.
#' @return `universe` plus `stake`, `pnl`, `pool_before`; rows below `min_bet`
#'   removed when a floor is set.
#' @export
stake_rolling <- function(universe, initial_pool = NULL,
                          daily_budget_frac = 0.05,
                          daily_budget_min_isk = 1000, min_bet = 0, ...) {
  if (is.null(initial_pool)) initial_pool <- load_bankroll()$initial_pool
  frac <- bt_effective_fraction(universe)
  u <- universe
  u$stake <- NA_real_
  u$pnl <- NA_real_
  u$pool_before <- NA_real_
  keep <- rep(TRUE, nrow(u))

  settled_md <- as.Date(character())
  settled_pnl <- numeric()

  for (r in sort(unique(u$run_id))) {
    rm <- which(u$run_id == r)
    rd <- u$run_date[rm][[1]]
    pool_r <- initial_pool + sum(settled_pnl[settled_md < rd], na.rm = TRUE)

    stake_r <- round(frac[rm] * pool_r)
    cap <- max(daily_budget_frac * pool_r, daily_budget_min_isk)
    tot <- sum(stake_r)
    if (is.finite(tot) && tot > cap && tot > 0) {
      stake_r <- round(stake_r * cap / tot)
    }
    pnl_r <- ifelse(u$win[rm], stake_r * (u$odds[rm] - 1), -stake_r)

    u$stake[rm] <- stake_r
    u$pnl[rm] <- pnl_r
    u$pool_before[rm] <- pool_r

    below <- stake_r < min_bet
    keep[rm[below]] <- FALSE
    placed <- rm[!below]
    settled_md <- c(settled_md, u$match_date[placed])
    settled_pnl <- c(settled_pnl, u$pnl[placed])
  }
  u[keep, , drop = FALSE]
}
