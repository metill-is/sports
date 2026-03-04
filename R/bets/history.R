#' Betting history analysis module
#'
#' Two core purposes:
#' 1. Plot expected vs observed cumulative profit (with simulation bands)
#' 2. Estimate optimal kelly_frac per sport/country/sex from calibration
#'
#' Usage:
#'   box::use(../../R/bets/history[...])
#'   bets <- load_all_history(c("basketball/iceland", "handball/iceland"))
#'   plot_cumulative_pnl(bets)
#'   recommend_kelly(bets)

box::use(
  readr[read_csv],
  dplyr[
    mutate, filter, summarise, arrange, select, group_by, ungroup,
    n, bind_rows, across, any_of, ntile, desc, inner_join,
    distinct, pull, slice_max, tibble, rename
  ],
  tidyr[drop_na, pivot_wider, crossing],
  ggplot2[
    ggplot, aes, geom_ribbon, geom_line, geom_hline, geom_point,
    geom_abline, geom_segment,
    scale_x_date, scale_y_continuous,
    facet_wrap, labs, theme, element_text,
    guide_axis, vars
  ],
  scales[
    label_date_short, label_percent, label_currency,
    breaks_pretty, breaks_width
  ]
)

# ---------------------------------------------------------------------------
# Data loading
# ---------------------------------------------------------------------------

#' Load bets_log.csv from a single sport directory
#'
#' @param sport_dir Path to sport directory (e.g., "basketball/iceland")
#' @return Tibble with standardised columns, or NULL
#' @export
load_history <- function(sport_dir) {
  log_path <- file.path(sport_dir, "history", "bets_log.csv")
  if (!file.exists(log_path)) {
    cat("  No history at", log_path, "\n")
    return(NULL)
  }
  d <- read_csv(log_path, show_col_types = FALSE)

  if (!"win" %in% names(d)) d$win <- NA
  if (!"pnl" %in% names(d)) d$pnl <- NA_real_
  if (!"source" %in% names(d)) d$source <- "unknown"

  d |>
    mutate(
      date_match = as.Date(date_match),
      date_recommended = as.Date(date_recommended)
    )
}

#' Load and combine history from multiple sport directories
#' @export
load_all_history <- function(sport_dirs) {
  lapply(sport_dirs, load_history) |> bind_rows()
}

# ---------------------------------------------------------------------------
# Cumulative P&L data prep (for plotting)
# ---------------------------------------------------------------------------

#' Compute cumulative expected and observed P&L
#'
#' Returns a tibble ready for ggplot with columns:
#'   date_match, cumul_expected, cumul_observed, cumul_wagered
#'
#' @param bets Tibble from load_history
#' @param by Character vector of grouping columns (e.g., c("sport", "market"))
#' @return Tibble with cumulative columns
#' @export
cumulative_pnl <- function(bets, by = NULL) {
  d <- bets |>
    filter(!is.na(pnl), !is.na(ev), !is.na(bet_amount)) |>
    arrange(date_match)

  if (!is.null(by)) {
    d <- d |> group_by(across(any_of(by)))
  }

  d |>
    mutate(
      cumul_expected = cumsum(ev * bet_amount),
      cumul_observed = cumsum(pnl),
      cumul_wagered  = cumsum(bet_amount)
    ) |>
    ungroup()
}

#' Simulate cumulative P&L paths under model probabilities
#'
#' For each bet, simulates win/loss from Bernoulli(probability) and computes
#' cumulative simulated profit. Returns per-date quantile bands.
#'
#' @param bets Tibble from load_history (resolved bets only)
#' @param n_sims Number of simulation paths (default 4000)
#' @param by Character vector of grouping columns
#' @param probs Quantiles for bands (default 2.5% and 97.5%)
#' @return Tibble with date_match, lower, upper (plus grouping cols)
#' @export
simulate_pnl_bands <- function(bets, n_sims = 4000, by = NULL,
                               probs = c(0.025, 0.975)) {
  d <- bets |>
    filter(!is.na(probability), !is.na(bet_amount), !is.na(odds)) |>
    arrange(date_match) |>
    select(any_of(c("date_match", "probability", "bet_amount", "odds", by)))

  if (nrow(d) == 0) return(NULL)

  # Expand: each bet × n_sims iterations
  sims <- d |>
    crossing(iter = seq_len(n_sims)) |>
    mutate(
      sim_win    = stats::rbinom(n(), size = 1, prob = probability),
      sim_pnl    = sim_win * (odds * bet_amount) - bet_amount
    )

  group_cols <- c(by, "iter")
  sims <- sims |>
    group_by(across(any_of(group_cols))) |>
    arrange(date_match, .by_group = TRUE) |>
    mutate(cumul_sim = cumsum(sim_pnl)) |>
    ungroup()

  # Per date × iter: take the last cumulative value (handles multiple bets/date)
  date_iter_cols <- c("date_match", by, "iter")
  summary_cols <- c("date_match", by)

  sims |>
    group_by(across(any_of(date_iter_cols))) |>
    summarise(cumul_sim = max(cumul_sim), .groups = "drop") |>
    group_by(across(any_of(summary_cols))) |>
    summarise(
      lower = stats::quantile(cumul_sim, probs[1]),
      upper = stats::quantile(cumul_sim, probs[2]),
      .groups = "drop"
    )
}

# ---------------------------------------------------------------------------
# Plotting
# ---------------------------------------------------------------------------

#' Plot cumulative expected vs observed P&L with simulation bands
#'
#' @param bets Tibble from load_history
#' @param by Character vector of faceting columns (e.g., c("sport", "country"))
#' @param n_sims Number of simulation paths for bands (default 4000)
#' @param currency_suffix Currency label (default "kr")
#' @return ggplot object
#' @export
plot_cumulative_pnl <- function(bets, by = NULL, n_sims = 4000,
                                currency_suffix = "kr") {
  cumul <- cumulative_pnl(bets, by = by)
  bands <- simulate_pnl_bands(bets, n_sims = n_sims, by = by)

  # Collapse to one row per date (per group) for plotting
  summary_cols <- c("date_match", by)
  plot_d <- cumul |>
    group_by(across(any_of(summary_cols))) |>
    summarise(
      cumul_expected = max(cumul_expected),
      cumul_observed = max(cumul_observed),
      .groups = "drop"
    )

  if (!is.null(bands)) {
    plot_d <- plot_d |> inner_join(bands, by = summary_cols)
  }

  p <- ggplot(plot_d, aes(date_match)) +
    geom_hline(yintercept = 0, lty = 2, alpha = 0.3)

  if (!is.null(bands)) {
    p <- p + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1)
  }

  p <- p +
    geom_line(
      aes(y = cumul_expected, colour = "Expected"),
      linewidth = 0.8
    ) +
    geom_line(
      aes(y = cumul_observed, colour = "Observed"),
      linewidth = 1
    ) +
    scale_x_date(
      guide = guide_axis(cap = "both"),
      breaks = breaks_pretty(8),
      labels = label_date_short()
    ) +
    scale_y_continuous(
      guide = guide_axis(cap = "both"),
      breaks = breaks_pretty(8),
      labels = label_currency(prefix = "", suffix = currency_suffix)
    ) +
    labs(
      x = NULL, y = NULL,
      colour = NULL,
      title = "Cumulative expected vs observed profit"
    ) +
    theme(legend.position = "bottom")

  if (!is.null(by) && length(by) > 0) {
    p <- p + facet_wrap(vars(!!!rlang::syms(by)), scales = "free")
  }

  p
}

#' Plot probability calibration (predicted prob vs observed win rate)
#'
#' @param bets Tibble from load_history
#' @param n_bins Number of bins (default 10)
#' @return ggplot object
#' @export
plot_calibration <- function(bets, n_bins = 10) {
  cal <- bets |>
    filter(!is.na(probability), !is.na(win)) |>
    mutate(bin = ntile(probability, n_bins)) |>
    group_by(bin) |>
    summarise(
      mean_prob     = mean(probability),
      observed_rate = mean(win),
      n             = n(),
      .groups = "drop"
    )

  ggplot(cal, aes(mean_prob, observed_rate)) +
    geom_abline(intercept = 0, slope = 1, lty = 2, alpha = 0.5) +
    geom_point(aes(size = n)) +
    scale_x_continuous(
      limits = c(0, 1),
      guide = guide_axis(cap = "both"),
      labels = label_percent(),
      breaks = breaks_width(0.2)
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      guide = guide_axis(cap = "both"),
      labels = label_percent(),
      breaks = breaks_width(0.2)
    ) +
    labs(
      x = "Model probability",
      y = "Observed win rate",
      size = "n",
      title = "Probability calibration"
    )
}

#' Plot EV calibration (predicted EV vs observed ROI)
#'
#' @param bets Tibble from load_history
#' @param n_bins Number of bins (default 10)
#' @return ggplot object
#' @export
plot_ev_calibration <- function(bets, n_bins = 10) {
  cal <- bets |>
    filter(!is.na(ev), !is.na(pnl), !is.na(bet_amount), bet_amount > 0) |>
    mutate(bin = ntile(ev, n_bins)) |>
    group_by(bin) |>
    summarise(
      mean_ev      = mean(ev),
      observed_roi = sum(pnl) / sum(bet_amount),
      n            = n(),
      .groups = "drop"
    )

  ggplot(cal, aes(mean_ev, observed_roi)) +
    geom_hline(yintercept = 0, alpha = 0.2) +
    geom_vline(xintercept = 0, alpha = 0.2) +
    geom_abline(intercept = 0, slope = 1, lty = 2, alpha = 0.5) +
    geom_point(aes(size = n)) +
    scale_x_continuous(
      guide = guide_axis(cap = "both"),
      labels = label_percent()
    ) +
    scale_y_continuous(
      guide = guide_axis(cap = "both"),
      labels = label_percent()
    ) +
    labs(
      x = "Predicted EV",
      y = "Observed ROI",
      size = "n",
      title = "EV calibration"
    )
}

# ---------------------------------------------------------------------------
# Kelly fraction estimation
# ---------------------------------------------------------------------------

#' Estimate optimal kelly_frac from historical performance
#'
#' The model predicts edge (EV) per bet. If the model is perfectly calibrated,
#' kelly_frac = 1.0 is optimal. If overconfident, a fraction < 1 is better.
#'
#' We estimate the "calibration slope" — the ratio of true edge to predicted
#' edge — via two complementary methods:
#'
#' 1. **ROI ratio**: observed_ROI / expected_ROI (simple, noisy)
#' 2. **EV regression slope**: regress observed_roi ~ predicted_ev across bins
#'    (the slope estimates how much of the predicted edge materialises)
#'
#' Returns recommended kelly_frac per group, with the regression slope as
#' the primary estimate (clipped to [0.01, 1.0]).
#'
#' @param bets Tibble from load_history
#' @param by Character vector of grouping columns (default: sport, country, sex)
#' @param min_bets Minimum resolved bets to produce a recommendation (default 20)
#' @param n_bins Number of EV bins for regression (default 5)
#' @return Tibble with group columns + recommended_kelly, roi_ratio, ev_slope, n
#' @export
recommend_kelly <- function(bets, by = c("sport", "country", "sex"),
                            min_bets = 20, n_bins = 5) {
  resolved <- bets |>
    filter(
      !is.na(win), !is.na(pnl), !is.na(ev),
      !is.na(bet_amount), bet_amount > 0
    )

  groups <- rlang::syms(by)

  # Per-group metrics
  group_stats <- resolved |>
    group_by(!!!groups) |>
    summarise(
      n_bets         = n(),
      total_wagered  = sum(bet_amount),
      total_pnl      = sum(pnl),
      observed_roi   = total_pnl / total_wagered,
      expected_roi   = sum(ev * bet_amount) / total_wagered,
      roi_ratio      = ifelse(expected_roi > 0, observed_roi / expected_roi, NA_real_),
      hit_rate       = mean(win),
      mean_ev        = mean(ev),
      mean_odds      = mean(odds, na.rm = TRUE),
      .groups = "drop"
    )

  # EV regression slope per group
  ev_slopes <- resolved |>
    group_by(!!!groups) |>
    filter(n() >= min_bets) |>
    mutate(bin = ntile(ev, min(n_bins, n()))) |>
    group_by(!!!groups, bin) |>
    summarise(
      mean_ev = mean(ev),
      obs_roi = sum(pnl) / sum(bet_amount),
      .groups = "drop"
    ) |>
    group_by(!!!groups) |>
    summarise(
      # Slope of obs_roi ~ mean_ev (through origin)
      ev_slope = ifelse(
        sum(mean_ev^2) > 0,
        sum(mean_ev * obs_roi) / sum(mean_ev^2),
        NA_real_
      ),
      .groups = "drop"
    )

  group_stats |>
    inner_join(ev_slopes, by = as.character(by)) |>
    mutate(
      # Primary recommendation: EV slope, clipped to sensible range
      # Fallback to roi_ratio if slope is NA
      recommended_kelly = ifelse(
        !is.na(ev_slope),
        pmin(pmax(ev_slope, 0.01), 1.0),
        ifelse(!is.na(roi_ratio), pmin(pmax(roi_ratio, 0.01), 1.0), NA_real_)
      ),
      # Flag: is the sample large enough to trust?
      confidence = ifelse(
        n_bets >= 100, "high",
        ifelse(n_bets >= min_bets, "moderate", "low")
      )
    ) |>
    filter(n_bets >= min_bets) |>
    select(
      !!!groups,
      n_bets, hit_rate,
      observed_roi, expected_roi,
      roi_ratio, ev_slope,
      recommended_kelly, confidence,
      mean_ev, mean_odds
    ) |>
    arrange(desc(n_bets))
}

#' Summary table: performance by group
#'
#' @param bets Tibble from load_history
#' @param by Character vector of grouping columns
#' @return Summary tibble
#' @export
summary_table <- function(bets, by = c("sport", "country", "market")) {
  groups <- rlang::syms(by)

  bets |>
    group_by(!!!groups) |>
    summarise(
      n_bets        = n(),
      n_resolved    = sum(!is.na(win)),
      n_won         = sum(win == TRUE, na.rm = TRUE),
      hit_rate      = ifelse(n_resolved > 0, n_won / n_resolved, NA_real_),
      total_wagered = sum(bet_amount, na.rm = TRUE),
      total_pnl     = sum(pnl, na.rm = TRUE),
      roi           = ifelse(total_wagered > 0, total_pnl / total_wagered, NA_real_),
      mean_ev       = mean(ev, na.rm = TRUE),
      mean_odds     = mean(odds, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(desc(n_resolved))
}
