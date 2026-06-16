# R/backtest-marketcal.R
#' Line-softness diagnostics: treat the de-vigged Lengjan line as a competing
#' forecaster and map where it is biased (the CLV replacement for a static line).
#' @importFrom rlang .data
NULL

#' Calibration reliability of the de-vigged market probability `q_market`.
#'
#' The CLV-replacement for a non-moving monopoly line: a static line cannot show
#' closing-line value, but a *biased* one shows up as miscalibration of `q_market`
#' against outcomes. Reuses [bt_calibration_bands()] on `(p = q_market, win = y)`.
#' @param scored Output of [bt_devig()] (`q_market`, `y`).
#' @param n_bins Probability bins. Default 10.
#' @param by Optional grouping columns.
#' @param conf Central mass for the intervals. Default 0.9.
#' @return [bt_calibration_bands()] columns; `mean_p` is the binned `q_market`.
#' @export
bt_market_calibration <- function(scored, n_bins = 10, by = NULL, conf = 0.9) {
  if (nrow(scored) == 0L) {
    return(tibble::tibble())
  }
  remap <- scored
  remap$p <- scored$q_market
  remap$win <- scored$y
  bt_calibration_bands(remap, n_bins = n_bins, by = by, conf = conf)
}

#' Directional market bias: realised rate minus the line's implied probability,
#' per outcome type.
#'
#' `bias > 0` means the outcome happens MORE often than the de-vigged line implies
#' (the line under-prices it -- a value pocket). Surfaces the classic soft-line
#' tells: draw under-pricing, over/under skew, favourite-longshot bias.
#' @param scored Output of [bt_devig()].
#' @param by Grouping columns (the outcome is always added). Default "market".
#' @return `(<by..>, outcome, n, mean_q, realised, bias)`.
#' @export
bt_market_bias <- function(scored, by = "market") {
  if (nrow(scored) == 0L) {
    return(tibble::tibble())
  }
  scored |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(by, "outcome")))) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean_q = mean(.data$q_market),
      realised = mean(.data$y),
      bias = mean(.data$y) - mean(.data$q_market),
      .groups = "drop"
    )
}

#' Band the model-vs-line gap `p - q_market` and report who is right per band.
#'
#' Where the model and the de-vigged line disagree most, whose probability tracks
#' the realised rate? If the market is right in a regime where they diverge, that
#' regime is a model weakness with an external second opinion.
#' @param scored Output of [bt_devig()].
#' @param by Optional grouping columns (the band is always added).
#' @param breaks Gap cut points. Default `c(-1, -.1, -.03, .03, .1, 1)`.
#' @return `(<by..>, band, n, mean_p, mean_q, realised)`.
#' @export
bt_disagreement <- function(scored, by = NULL,
                            breaks = c(-1, -0.1, -0.03, 0.03, 0.1, 1)) {
  if (nrow(scored) == 0L) {
    return(tibble::tibble())
  }
  d <- scored
  d$gap <- d$p - d$q_market
  d$band <- cut(d$gap,
    breaks = breaks, include.lowest = TRUE,
    labels = c("model<<mkt", "model<mkt", "agree", "model>mkt", "model>>mkt")
  )
  d |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(by, "band")))) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean_p = mean(.data$p),
      mean_q = mean(.data$q_market),
      realised = mean(.data$y),
      .groups = "drop"
    )
}

#' How often does Lengjan actually move a price before kickoff?
#'
#' Confirms (or refutes) the static-line hypothesis empirically: per
#' `(match, market, line, outcome)` series, counts distinct prices across the
#' pre-kickoff snapshots. A low `pct_moved` justifies dropping CLV in favour of
#' the bias map.
#' @param odds Odds store (`scraped_at`, match keys, `market`, `outcome`, `line`,
#'   `odds`).
#' @return One-row tibble `(n_series, pct_moved, mean_distinct_prices,
#'   mean_snapshots)`.
#' @export
bt_line_stability <- function(odds) {
  if (nrow(odds) == 0L) {
    return(tibble::tibble(
      n_series = 0L, pct_moved = NA_real_,
      mean_distinct_prices = NA_real_, mean_snapshots = NA_real_
    ))
  }
  key <- c("match_date", "home_team", "away_team", "market", "outcome", "line")
  per <- odds |>
    dplyr::group_by(dplyr::across(dplyr::all_of(key))) |>
    dplyr::summarise(
      n_snapshots = dplyr::n(),
      n_distinct_prices = dplyr::n_distinct(.data$odds),
      .groups = "drop"
    )
  tibble::tibble(
    n_series = nrow(per),
    pct_moved = mean(per$n_distinct_prices > 1L),
    mean_distinct_prices = mean(per$n_distinct_prices),
    mean_snapshots = mean(per$n_snapshots)
  )
}
