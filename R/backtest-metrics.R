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

#' De-vig a candidate set: add the bookmaker's margin-free implied probability.
#'
#' The market's quoted `1/odds` sum to >1 within a market (the overround/vig).
#' Normalising them to sum to 1 within each `(match, market, line)` group yields a
#' fair market probability `q_market` to score against the model's `p` on the same
#' rows. Two data-driven guards keep the comparison honest:
#'   * `sum(p) ~= 1` per group -> the group is COMPLETE (every outcome present); an
#'     incomplete book (missing moneyline draw, one-sided scrape) cannot be
#'     de-vigged and is dropped.
#'   * exactly one winner per group -> SETTLED and not a push; an integer
#'     spread/total line landing exactly has no winner (`sum(win) == 0`) and is
#'     dropped (a real push refunds the stake; neither side forecasts that).
#' Markets here are 3-way for moneyline AND spread (the handicap tie is a `draw`
#' outcome) and 2-way for total -- the guards adapt without hard-coding counts.
#' @param bets Tibble with `p`, `odds`, logical `win`, `market`, `outcome`, `line`
#'   and the match keys (`sport`, `country`, `sex`, `match_date`, `home_team`,
#'   `away_team`).
#' @param p_tol Completeness tolerance on `sum(p)`. Default 0.02.
#' @return The settled, complete, non-push rows with added `y` (0/1), `overround`,
#'   and `q_market`. Empty tibble if nothing qualifies.
#' @importFrom rlang .data
#' @export
bt_devig <- function(bets, p_tol = 0.02) {
  keys <- c(
    "sport", "country", "sex", "match_date",
    "home_team", "away_team", "market", "line"
  )
  d <- bets[is.finite(bets$p) & is.finite(bets$odds) &
    bets$odds > 1 & !is.na(bets$win), , drop = FALSE]
  if (nrow(d) == 0L) {
    return(tibble::tibble())
  }
  d$y <- as.numeric(d$win)
  d |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::mutate(
      overround = sum(1 / .data$odds),
      q_market = (1 / .data$odds) / sum(1 / .data$odds),
      grp_p = sum(.data$p),
      grp_win = sum(.data$y)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(abs(.data$grp_p - 1) < p_tol, .data$grp_win == 1) |>
    dplyr::select(-"grp_p", -"grp_win")
}

#' Model-vs-market prediction-quality scores + Brier/log-loss skill.
#'
#' On the de-vigged rows from [bt_devig()], scores the model `p` and the market
#' `q_market` against realised `y` with Brier + log-loss, and reports the skill
#' scores `1 - score_model/score_market` (> 0 == the model beats the margin-free
#' line). `n_matches` is the count of distinct fixtures -- the honest unit, since
#' outcome rows within a match are mechanically dependent.
#' @param scored Output of [bt_devig()] (`p`, `q_market`, `y`, `overround`).
#' @param by Optional grouping columns (e.g. `"market"`).
#' @param eps Log-loss clamp. Default 1e-6.
#' @return One row (or one per `by` group) of model/market Brier + log-loss, their
#'   skill scores, `n`, `n_matches`, and mean `overround`.
#' @export
bt_skill <- function(scored, by = NULL, eps = 1e-6) {
  one <- function(d) {
    clip <- function(x) pmin(pmax(x, eps), 1 - eps)
    ll <- function(prob) {
      -mean(d$y * log(clip(prob)) + (1 - d$y) * log(clip(1 - prob)))
    }
    bm <- mean((d$p - d$y)^2)
    bk <- mean((d$q_market - d$y)^2)
    lm <- ll(d$p)
    lk <- ll(d$q_market)
    tibble::tibble(
      n = nrow(d),
      n_matches = dplyr::n_distinct(paste(d$match_date, d$home_team, d$away_team)),
      brier_model = bm, brier_market = bk, brier_skill = 1 - bm / bk,
      logloss_model = lm, logloss_market = lk, logloss_skill = 1 - lm / lk,
      overround = mean(d$overround)
    )
  }
  if (nrow(scored) == 0L) {
    return(tibble::tibble())
  }
  if (is.null(by)) {
    return(one(scored))
  }
  scored |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::group_modify(~ one(.x)) |>
    dplyr::ungroup()
}

#' Match-clustered bootstrap CI on the Brier skill score (model vs market).
#'
#' Outcome rows within a match are dependent, so the resample unit is the MATCH,
#' not the row (index-expansion, so a match drawn twice contributes twice). This
#' is what keeps the "is the edge real?" interval honest at small effective n.
#' @param scored Output of [bt_devig()].
#' @param by Optional grouping columns. With `by`, returns a per-group tibble of
#'   `(<by..>, skill_lo, skill_mid, skill_hi)` (requires length-3 `probs`); with
#'   `NULL` (default) returns the bare numeric vector for backward compatibility.
#' @param R Bootstrap replicates. Default 2000.
#' @param probs Quantiles to return. Default `c(0.05, 0.5, 0.95)`.
#' @param seed RNG seed for reproducibility.
#' @return Numeric vector of the `probs` quantiles, or a per-group tibble (`by`).
#' @export
bt_skill_ci <- function(scored, by = NULL, R = 2000, probs = c(0.05, 0.5, 0.95), seed = 1L) {
  if (!is.null(by)) {
    return(
      scored |>
        dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
        dplyr::group_modify(~ {
          q <- bt_skill_ci(.x, by = NULL, R = R, probs = probs, seed = seed)
          tibble::tibble(skill_lo = q[1], skill_mid = q[2], skill_hi = q[3])
        }) |>
        dplyr::ungroup()
    )
  }
  if (nrow(scored) == 0L) {
    return(rep(NA_real_, length(probs)))
  }
  mid <- paste(scored$match_date, scored$home_team, scored$away_team)
  by_match <- split(seq_len(nrow(scored)), mid)
  matches <- names(by_match)
  withr::with_seed(seed, {
    reps <- vapply(seq_len(R), function(i) {
      idx <- unlist(by_match[sample(matches, replace = TRUE)], use.names = FALSE)
      d <- scored[idx, , drop = FALSE]
      1 - mean((d$p - d$y)^2) / mean((d$q_market - d$y)^2)
    }, numeric(1))
  })
  stats::quantile(reps, probs, names = FALSE)
}

#' Murphy (1973) decomposition of the Brier score.
#'
#' Splits Brier into reliability (calibration; lower better), resolution
#' (discrimination; higher better), and uncertainty (base-rate variance, a
#' property of the outcomes). Compare REL/RES across strata, NOT raw Brier --
#' UNC differs by cell, so raw Brier is not cross-stratum comparable. The binned
#' identity `BS = REL - RES + UNC` holds exactly when forecasts are constant
#' within a bin.
#' @param scored Tibble with numeric `p` and binary `y`.
#' @param n_bins Forecast bins. Default 10.
#' @param by Optional grouping columns.
#' @return `(<by..>, n, reliability, resolution, uncertainty, brier)`.
#' @export
bt_brier_decomp <- function(scored, n_bins = 10, by = NULL) {
  one <- function(d) {
    n <- nrow(d)
    o_bar <- mean(d$y)
    bin <- cut(d$p, breaks = seq(0, 1, length.out = n_bins + 1), include.lowest = TRUE)
    agg <- tibble::tibble(p = d$p, y = d$y, bin = bin) |>
      dplyr::group_by(.data$bin) |>
      dplyr::summarise(
        nk = dplyr::n(), pbar = mean(.data$p), obar = mean(.data$y),
        .groups = "drop"
      )
    tibble::tibble(
      n = n,
      reliability = sum(agg$nk * (agg$pbar - agg$obar)^2) / n,
      resolution = sum(agg$nk * (agg$obar - o_bar)^2) / n,
      uncertainty = o_bar * (1 - o_bar),
      brier = mean((d$p - d$y)^2)
    )
  }
  if (nrow(scored) == 0L) {
    return(tibble::tibble())
  }
  if (is.null(by)) {
    return(one(scored))
  }
  scored |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::group_modify(~ one(.x)) |>
    dplyr::ungroup()
}

#' Calibration reliability with Jeffreys intervals and a binomial consistency band.
#'
#' Extends [bt_calibration()]: `lo`/`hi` are the Jeffreys (Beta(k+.5, n-k+.5))
#' interval on each bin's realised frequency; `band_lo`/`band_hi` are the
#' consistency band -- the central interval of the realised frequency UNDER the
#' null that the bin is perfectly calibrated (Binomial(n, mean_p)/n). A bin whose
#' `realised_freq` sits outside `[band_lo, band_hi]` is miscalibrated beyond
#' sampling noise.
#' @param settled Per-bet tibble (`p`, logical `win`).
#' @param n_bins Probability bins. Default 10.
#' @param by Optional grouping columns.
#' @param conf Central mass for both intervals. Default 0.9.
#' @return [bt_calibration()] columns plus `lo, hi, band_lo, band_hi`.
#' @export
bt_calibration_bands <- function(settled, n_bins = 10, by = NULL, conf = 0.9) {
  cal <- bt_calibration(settled, n_bins = n_bins, by = by)
  if (nrow(cal) == 0L) {
    return(cal)
  }
  a <- (1 - conf) / 2
  b <- 1 - a
  k <- round(cal$realised_freq * cal$n)
  cal$lo <- stats::qbeta(a, k + 0.5, cal$n - k + 0.5)
  cal$hi <- stats::qbeta(b, k + 0.5, cal$n - k + 0.5)
  cal$band_lo <- stats::qbinom(a, cal$n, cal$mean_p) / cal$n
  cal$band_hi <- stats::qbinom(b, cal$n, cal$mean_p) / cal$n
  cal
}
