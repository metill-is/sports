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
#' @param ... Absorbs `initial_pool` (ignored) for a uniform stake-rule signature.
#' @return `universe` plus `stake`, `pnl`, `pool_before`.
#' @export
stake_fixed <- function(universe, ref_pool = NULL, ...) {
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
  universe
}
