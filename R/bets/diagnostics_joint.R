#' Diagnostics for joint Kelly optimisation
#'
#' Computes per-match diagnostics comparing joint vs independent Kelly,
#' and produces summary statistics for evaluating the joint approach.
#'
#' Intended for interactive analysis. Returns diagnostics as a list of tibbles.
#'
#' Usage:
#'   box::use(R/bets/diagnostics_joint[run_diagnostics])
#'   diag <- run_diagnostics(post, odds, cfg)

box::use(
  ./kelly_joint[build_return_matrix, get_kelly_joint, collect_match_bets, parse_handicap],
  dplyr[filter, mutate, distinct, select, bind_rows, summarise, arrange, tibble,
        group_by, ungroup, across, any_of, n, row_number],
  tidyr[pivot_wider],
  stats[cor]
)

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Per-match joint Kelly diagnostics
#'
#' For each match with available odds, computes:
#' - All positive-EV bets and their joint Kelly allocations
#' - Indicator correlation matrix (pairwise between bets)
#' - Posterior expected log-growth rate
#' - Worst-case wealth under any posterior draw
#' - Effective number of bets (nonzero allocation)
#'
#' @param post Posterior draws (date, division, home, away, home_goals, away_goals)
#' @param odds List with outcome, handicap, totals tibbles (from load_odds)
#' @param cfg Config list from bets.yml (with global bankroll merged)
#' @return List with:
#'   - match_summary: tibble with one row per match
#'   - bet_details: tibble with one row per bet (all matches)
#'   - comparison: tibble comparing joint vs independent allocations
#' @export
run_diagnostics <- function(post, odds, cfg) {
  divisions <- cfg$predictions$divisions
  if (!is.null(divisions)) {
    post <- post |> filter(division %in% divisions)
  }

  odds_hc <- odds$handicap
  if (!is.null(odds_hc) && nrow(odds_hc) > 0 && is.character(odds_hc$change)) {
    odds_hc <- odds_hc |> mutate(change = parse_handicap(change))
  }

  matches <- post |> distinct(date, division, home, away)
  max_match_stake <- cfg$bankroll$max_match_stake %||% 0.50
  ev_threshold <- cfg$bankroll$ev_threshold %||% 0.00
  kelly_frac <- cfg$bankroll$kelly_frac
  cur_pool <- cfg$bankroll$cur_pool
  min_bet <- cfg$bankroll$min_bet_amount

  match_rows <- list()
  bet_rows <- list()

  for (m_idx in seq_len(nrow(matches))) {
    match <- matches[m_idx, ]
    draws <- post |>
      filter(date == match$date, home == match$home, away == match$away)
    if (nrow(draws) == 0) next

    # Collect odds for this match
    m_1x2 <- get_match_odds(odds$outcome, match, cfg$markets$outcome)
    m_hc   <- get_match_odds(odds_hc, match, cfg$markets$handicap)
    m_tot  <- get_match_odds(odds$totals, match, cfg$markets$totals)

    bets <- collect_match_bets(m_1x2, m_hc, m_tot, cfg)
    if (is.null(bets) || nrow(bets) == 0) next

    ret_mat <- build_return_matrix(draws, bets)
    bets$p <- colMeans(ret_mat > 0)
    bets$implied_p <- 1 / bets$o
    bets$ev_edge <- bets$p - bets$implied_p
    bets$ev <- bets$p * (bets$o - 1) - (1 - bets$p)

    # Pre-filter
    keep <- bets$ev_edge > ev_threshold
    if (!any(keep)) next

    bets_filt <- bets[keep, ]
    ret_filt <- ret_mat[, keep, drop = FALSE]
    ind_filt <- matrix(as.integer(ret_filt > 0), nrow = nrow(ret_filt))
    B <- nrow(bets_filt)

    # ── Joint Kelly ──
    kelly_result <- get_kelly_joint(net_return = ret_filt, max_stake = max_match_stake)
    fracs_joint <- kelly_result$solution

    # ── Wealth diagnostics ──
    net_return <- ret_filt
    wealth_joint <- 1 + as.vector(net_return %*% fracs_joint)
    growth_joint <- mean(log(pmax(wealth_joint, 1e-10)))
    worst_joint <- min(wealth_joint)

    # ── Correlation ──
    max_cor <- NA_real_
    if (B > 1) {
      cor_mat <- cor(ind_filt)
      max_cor <- max(abs(cor_mat[upper.tri(cor_mat)]))
    }

    # ── Match summary ──
    match_rows <- c(match_rows, list(tibble(
      date = match$date,
      division = match$division,
      home = match$home,
      away = match$away,
      n_draws = nrow(draws),
      n_bets_total = nrow(bets),
      n_bets_ev = B,
      n_bets_effective = sum(fracs_joint > 1e-4),
      total_stake = sum(fracs_joint),
      growth_rate = growth_joint,
      worst_wealth = worst_joint,
      max_indicator_cor = max_cor
    )))

    # ── Per-bet details ──
    for (j in seq_len(B)) {
      b <- bets_filt[j, ]
      bet_rows <- c(bet_rows, list(tibble(
        date = match$date,
        home = match$home,
        away = match$away,
        bet_type = b$bet_type,
        market = b$market,
        outcome = b$outcome,
        odds = b$o,
        change = b$change,
        limit = b$limit,
        p_posterior = b$p,
        p_implied = b$implied_p,
        ev_edge = b$ev_edge,
        ev = b$ev,
        f_kelly = fracs_joint[j],
        amt_raw = round(fracs_joint[j] * cur_pool, 0),
        amt_scaled = round(fracs_joint[j] * kelly_frac * cur_pool, 0)
      )))
    }
  }

  match_summary <- if (length(match_rows) > 0) bind_rows(match_rows) else tibble()
  bet_details <- if (length(bet_rows) > 0) bind_rows(bet_rows) else tibble()

  list(
    match_summary = match_summary,
    bet_details = bet_details
  )
}

#' Print a formatted diagnostics report
#'
#' @param diag Output from run_diagnostics()
#' @param cfg Config list
#' @export
print_diagnostics <- function(diag, cfg) {
  ms <- diag$match_summary
  bd <- diag$bet_details

  if (nrow(ms) == 0) {
    cat("No matches with positive-EV bets found.\n")
    return(invisible(NULL))
  }

  kelly_frac <- cfg$bankroll$kelly_frac
  cur_pool <- cfg$bankroll$cur_pool
  min_bet <- cfg$bankroll$min_bet_amount

  cat("\n══════════════════════════════════════════════════════════\n")
  cat(" JOINT KELLY — DIAGNOSTICS REPORT\n")
  cat("══════════════════════════════════════════════════════════\n\n")

  cat(sprintf("  kelly_frac: %.2f | pool: %d | min_bet: %d\n",
              kelly_frac, cur_pool, min_bet))
  cat(sprintf("  Raw f needed (at kelly_frac=%.2f): %.4f\n",
              kelly_frac, min_bet / (cur_pool * kelly_frac)))
  cat(sprintf("  Matches with +EV bets: %d\n\n", nrow(ms)))

  # Aggregate
  cat("── Aggregate ──\n")
  cat(sprintf("  %-30s  %8d\n", "Total effective bets", sum(ms$n_bets_effective)))
  cat(sprintf("  %-30s  %8.4f\n", "Mean log-growth per match", mean(ms$growth_rate)))
  cat(sprintf("  %-30s  %8.4f\n", "Total log-growth", sum(ms$growth_rate)))
  cat(sprintf("  %-30s  %8.4f\n", "Min worst-case wealth", min(ms$worst_wealth)))
  cat(sprintf("  %-30s  %8.4f\n", "Mean worst-case wealth", mean(ms$worst_wealth)))

  # Bets that would pass at various kelly_frac levels
  if (nrow(bd) > 0) {
    for (kf in c(0.10, 0.25, 0.50, 0.75, 1.00)) {
      n_pass <- sum(bd$f_kelly * kf * cur_pool >= min_bet)
      cat(sprintf("  Bets passing at kelly_frac=%.2f:  %d\n", kf, n_pass))
    }
  }

  cat("\n── Per-match ──\n")
  for (i in seq_len(nrow(ms))) {
    m <- ms[i, ]
    cat(sprintf("\n  %s vs %s (%s, div %s)\n",
                m$home, m$away, m$date, m$division))
    cat(sprintf("    +EV bets: %d | Effective: %d | Stake: %.3f | Max corr: %.3f\n",
                m$n_bets_ev, m$n_bets_effective, m$total_stake,
                ifelse(is.na(m$max_indicator_cor), 0, m$max_indicator_cor)))
    cat(sprintf("    Growth: %.6f | Worst-case wealth: %.4f\n",
                m$growth_rate, m$worst_wealth))

    # Show bets for this match
    match_bets <- bd |>
      filter(date == m$date, home == m$home, away == m$away) |>
      filter(f_kelly > 1e-4)

    if (nrow(match_bets) > 0) {
      for (j in seq_len(nrow(match_bets))) {
        b <- match_bets[j, ]
        label <- paste0(b$bet_type,
                        if (!is.na(b$change)) paste0("[", b$change, "]") else "",
                        if (!is.na(b$limit)) paste0("[", b$limit, "]") else "")
        cat(sprintf("      %-18s odds=%5.2f  edge=%+.3f  f=%.4f  amt=%d\n",
                    label, b$odds, b$ev_edge, b$f_kelly, b$amt_scaled))
      }
    }
  }

  cat("\n══════════════════════════════════════════════════════════\n")
  invisible(diag)
}

# ── Helper ──

get_match_odds <- function(odds_tbl, match, enabled) {
  if (!isTRUE(enabled) || is.null(odds_tbl) || nrow(odds_tbl) == 0) return(NULL)
  out <- odds_tbl |>
    filter(date == match$date, home == match$home, away == match$away)
  if (nrow(out) == 0) NULL else out
}
