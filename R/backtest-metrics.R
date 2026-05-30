# R/backtest-metrics.R
#' @include backtest-engine.R
NULL

bt_metrics_one <- function(d) {
  n <- nrow(d)
  staked <- sum(d$stake)
  pnl <- sum(d$pnl)
  o <- order(d$match_date)
  cp <- cumsum(d$pnl[o])
  peak <- cummax(c(0, cp))[-1]
  ret <- d$pnl / pmax(d$stake, 1e-9)
  tibble::tibble(
    n_bets = n,
    total_staked = staked,
    total_pnl = pnl,
    roi = if (staked > 0) pnl / staked else NA_real_,
    yield = pnl / n,
    hit_rate = mean(d$win),
    avg_odds = mean(d$odds),
    max_drawdown = if (n > 0) min(cp - peak) else NA_real_,
    sharpe_like = if (isTRUE(stats::sd(ret) > 0)) mean(ret) / stats::sd(ret) else NA_real_
  )
}

#' Backtest performance metrics, optionally grouped.
#' @param settled Per-bet tibble from [bt_run()].
#' @param by Optional character vector of grouping columns (e.g. `"market"`,
#'   `c("sport", "sex")`).
#' @return One-row (or grouped) KPI tibble. Empty input -> empty tibble.
#' @export
bt_metrics <- function(settled, by = NULL) {
  if (nrow(settled) == 0L) {
    return(tibble::tibble())
  }
  if (is.null(by)) {
    return(bt_metrics_one(settled))
  }
  settled |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::group_modify(~ bt_metrics_one(.x)) |>
    dplyr::ungroup()
}

#' Calibration reliability: predicted `p` vs realised win frequency.
#' @param settled Per-bet tibble from [bt_run()].
#' @param n_bins Number of equal-width probability bins.
#' @param by Optional grouping columns for decomposition.
#' @return Tibble of `(.by.., bin, mean_p, realised_freq, n)`.
#' @export
bt_calibration <- function(settled, n_bins = 10, by = NULL) {
  if (nrow(settled) == 0L) {
    return(tibble::tibble())
  }
  settled$bin <- cut(settled$p,
    breaks = seq(0, 1, length.out = n_bins + 1),
    include.lowest = TRUE
  )
  settled |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(by, "bin")))) |>
    dplyr::summarise(
      mean_p = mean(.data$p),
      realised_freq = mean(.data$win),
      n = dplyr::n(), .groups = "drop"
    )
}

#' Compare strategies under both stake models.
#'
#' Runs the engine for `our_picks` (kept), `all_positive_ev`, `flat_stake`
#' (kept bets at a constant ISK stake -- isolates selection from sizing), and
#' `favourites` (lowest-odds moneyline outcome per match -- naive baseline),
#' under both rolling and fixed stake models, and returns a tidy comparison.
#' @param root Data root.
#' @param results Optional results tibble (read from `root` if NULL).
#' @param initial_pool Starting bankroll; default from `bankroll.yml`.
#' @param flat_isk Constant stake for the `flat_stake` strategy.
#' @param ... Forwarded to [bt_load_universe()] (e.g. `leagues`, `from`, `to`).
#' @return Tibble of metrics with `strategy` + `stake_model` columns.
#' @export
bt_baselines <- function(root = here::here("data"), results = NULL,
                         initial_pool = NULL, flat_isk = 500, ...) {
  if (is.null(results)) results <- read_table("results", root = root)
  if (is.null(initial_pool)) initial_pool <- load_bankroll()$initial_pool

  picks <- bt_load_universe(root, "kept", ...)
  posev <- bt_load_universe(root, "positive_ev", ...)
  allc <- bt_load_universe(root, "all", ...)

  favs <- allc[allc$market == "moneyline", , drop = FALSE]
  if (nrow(favs) > 0L) {
    favs <- favs |>
      dplyr::group_by(
        .data$run_id, .data$sport, .data$country, .data$sex,
        .data$match_date, .data$home_team, .data$away_team
      ) |>
      dplyr::slice_min(.data$odds, n = 1, with_ties = FALSE) |>
      dplyr::ungroup()
  }

  flat_rule <- function(universe, ...) {
    universe$stake <- flat_isk
    universe$pnl <- ifelse(universe$win,
      universe$stake * (universe$odds - 1),
      -universe$stake
    )
    universe$pool_before <- initial_pool
    universe
  }

  jobs <- list(
    list(name = "our_picks", u = picks, rules = c("rolling", "fixed")),
    list(name = "all_positive_ev", u = posev, rules = c("rolling", "fixed")),
    list(name = "favourites", u = favs, rules = c("rolling", "fixed")),
    list(name = "flat_stake", u = picks, rules = "flat")
  )
  out <- list()
  for (j in jobs) {
    if (nrow(j$u) == 0L) next
    for (sm in j$rules) {
      rule <- switch(sm,
        rolling = stake_rolling,
        fixed = stake_fixed,
        flat = flat_rule
      )
      r <- bt_run(j$u, results, stake_rule = rule, initial_pool = initial_pool)
      if (nrow(r) == 0L) next
      m <- bt_metrics(r)
      m$strategy <- j$name
      m$stake_model <- sm
      out[[paste(j$name, sm)]] <- m
    }
  }
  dplyr::bind_rows(out)
}
