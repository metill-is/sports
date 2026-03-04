#' Joint Kelly criterion across all markets per match
#'
#' Replaces the three independent per-market Kelly runs with a single
#' optimiser that sees all bets simultaneously. This correctly handles:
#' - Mutually exclusive outcomes within a market (1x2)
#' - Cross-market correlation (home win + home -0.5 handicap)
#' - Full posterior uncertainty (no collapse to point probabilities)
#'
#' Activated by setting `kelly_mode: joint` in bets.yml.

box::use(
  ./kelly[format_bet_text],
  ./market_handicap[parse_handicap],
  nloptr[nloptr],
  dplyr[filter, mutate, select, any_of, bind_rows, inner_join, distinct,
        summarise, arrange, tibble, rename],
  tidyr[crossing]
)

# ── Indicator matrix construction ────────────────────────────────────────────

#' Build binary indicator matrix from posterior draws
#'
#' @param draws Tibble of posterior draws for one match (home_goals, away_goals).
#'   Must have S rows (one per draw).
#' @param bets Tibble describing each bet with columns:
#'   bet_type ("1x2_home", "hc_home", "over", etc.),
#'   change (for handicap bets, NA otherwise),
#'   limit (for totals bets, NA otherwise),
#'   tie_threshold, hc_threshold (for European HC).
#' @return Integer matrix S x B: 1 if bet j wins in draw s, 0 otherwise
#' @export
build_indicators <- function(draws, bets) {
  S <- nrow(draws)
  B <- nrow(bets)
  indicators <- matrix(0L, nrow = S, ncol = B)

  hg <- draws$home_goals
  ag <- draws$away_goals
  diff <- hg - ag
  total <- hg + ag

  for (j in seq_len(B)) {
    bt <- bets$bet_type[j]
    tt <- bets$tie_threshold[j]
    indicators[, j] <- switch(bt,
      "1x2_home"  = as.integer(diff > tt),
      "1x2_tie"   = as.integer(abs(diff) <= tt),
      "1x2_away"  = as.integer(diff < -tt),
      "hc_home"   = {
        adj <- diff + bets$change[j]
        ht <- bets$hc_threshold[j]
        as.integer(adj > ht)
      },
      "hc_tie"    = {
        adj <- diff + bets$change[j]
        ht <- bets$hc_threshold[j]
        as.integer(abs(adj) <= ht)
      },
      "hc_away"   = {
        adj <- diff + bets$change[j]
        ht <- bets$hc_threshold[j]
        as.integer(adj < -ht)
      },
      "over"      = as.integer(total > bets$limit[j]),
      "under"     = as.integer(total <= bets$limit[j]),
      stop("Unknown bet_type: ", bt)
    )
  }

  indicators
}

# ── Joint optimiser ──────────────────────────────────────────────────────────

#' Joint Kelly criterion optimisation over the full posterior
#'
#' Maximises posterior expected log-growth:
#'   G(f) = (1/S) * sum_s log(1 + sum_j net_return_sj * f_j)
#' where net_return_sj = indicators_sj * (odds_j - 1) - (1 - indicators_sj)
#'                     = indicators_sj * odds_j - 1
#'
#' Uses SLSQP with analytic gradients.
#'
#' @param indicators S x B integer matrix (from build_indicators)
#' @param odds Numeric vector of length B (decimal odds)
#' @param max_stake Max total fraction to wager (default 0.50)
#' @return Numeric vector of length B: optimal raw fractions
#' @export
get_kelly_joint <- function(indicators, odds, max_stake = 0.50) {
  S <- nrow(indicators)
  B <- ncol(indicators)

  if (B == 0) return(numeric(0))

  # Net return matrix: S x B
  # If bet j wins: return = odds_j - 1 (net profit per unit)
  # If bet j loses: return = -1 (lose stake)
  # net_return_sj = indicators_sj * odds_j - 1
  # But we only lose what we staked on bet j, so:
  # Wealth = 1 + sum_j f_j * (indicators_sj * odds_j - 1)
  #        = 1 + sum_j f_j * indicators_sj * odds_j - sum_j f_j
  #        = 1 - sum(f) + sum_j f_j * indicators_sj * odds_j
  net_return <- sweep(indicators, 2, odds, `*`) - 1  # S x B

  # Objective: negative expected log-growth (minimise)
  eval_f <- function(f) {
    wealth <- 1 + as.vector(net_return %*% f)  # length S
    wealth <- pmax(wealth, 1e-10)
    -mean(log(wealth))
  }

  # Analytic gradient
  eval_grad_f <- function(f) {
    wealth <- 1 + as.vector(net_return %*% f)
    wealth <- pmax(wealth, 1e-10)
    inv_w <- 1 / wealth  # length S
    -as.vector(crossprod(net_return, inv_w)) / S  # length B
  }

  # Constraint: sum(f) <= max_stake
  eval_g_ineq <- function(f) sum(f) - max_stake

  eval_jac_g_ineq <- function(f) rep(1, B)

  # Start from small positive fractions
  f_init <- rep(min(0.01, max_stake / B), B)

  result <- nloptr(
    x0 = f_init,
    eval_f = eval_f,
    eval_grad_f = eval_grad_f,
    eval_g_ineq = eval_g_ineq,
    eval_jac_g_ineq = eval_jac_g_ineq,
    lb = rep(0, B),
    ub = rep(max_stake, B),
    opts = list(
      algorithm = "NLOPT_LD_SLSQP",
      xtol_rel = 1e-8,
      maxeval = 1000
    )
  )

  result$solution
}

# ── Bet collection ───────────────────────────────────────────────────────────

#' Collect all available bets for one match across enabled markets
#'
#' @param match_odds_1x2 Filtered 1x2 odds for this match (or NULL)
#' @param match_odds_hc Filtered handicap odds for this match (or NULL)
#' @param match_odds_tot Filtered totals odds for this match (or NULL)
#' @param cfg Config list
#' @return Tibble with one row per bet: bet_type, outcome, market, odds,
#'   booker, change, limit, tie_threshold, hc_threshold
#' @export
collect_match_bets <- function(match_odds_1x2, match_odds_hc, match_odds_tot, cfg) {
  has_ties <- cfg$scoring$has_ties
  tie_threshold <- cfg$scoring$tie_threshold

  bets <- list()

  # 1x2 bets
  if (!is.null(match_odds_1x2) && nrow(match_odds_1x2) > 0) {
    for (i in seq_len(nrow(match_odds_1x2))) {
      row <- match_odds_1x2[i, ]
      bets <- c(bets, list(
        tibble(
          bet_type = "1x2_home", outcome = "home", market = "outcome",
          o = row$o_home, booker = row$booker,
          change = NA_real_, limit = NA_real_,
          tie_threshold = tie_threshold, hc_threshold = NA_real_
        )
      ))
      if (has_ties && "o_draw" %in% names(row) && !is.na(row$o_draw)) {
        bets <- c(bets, list(
          tibble(
            bet_type = "1x2_tie", outcome = "tie", market = "outcome",
            o = row$o_draw, booker = row$booker,
            change = NA_real_, limit = NA_real_,
            tie_threshold = tie_threshold, hc_threshold = NA_real_
          )
        ))
      }
      bets <- c(bets, list(
        tibble(
          bet_type = "1x2_away", outcome = "away", market = "outcome",
          o = row$o_away, booker = row$booker,
          change = NA_real_, limit = NA_real_,
          tie_threshold = tie_threshold, hc_threshold = NA_real_
        )
      ))
    }
  }

  # Handicap bets
  if (!is.null(match_odds_hc) && nrow(match_odds_hc) > 0) {
    for (i in seq_len(nrow(match_odds_hc))) {
      row <- match_odds_hc[i, ]
      change_val <- row$change
      is_whole <- (change_val == round(change_val))
      is_european <- has_ties && is_whole

      if (is_european) {
        ht <- tie_threshold
      } else {
        ht <- 0
      }

      bets <- c(bets, list(
        tibble(
          bet_type = "hc_home", outcome = "home", market = "handicap",
          o = row$o_home, booker = row$booker,
          change = change_val, limit = NA_real_,
          tie_threshold = tie_threshold, hc_threshold = ht
        )
      ))

      if (is_european && "o_draw" %in% names(row) && !is.na(row$o_draw)) {
        bets <- c(bets, list(
          tibble(
            bet_type = "hc_tie", outcome = "tie", market = "handicap",
            o = row$o_draw, booker = row$booker,
            change = change_val, limit = NA_real_,
            tie_threshold = tie_threshold, hc_threshold = ht
          )
        ))
      }

      bets <- c(bets, list(
        tibble(
          bet_type = "hc_away", outcome = "away", market = "handicap",
          o = row$o_away, booker = row$booker,
          change = change_val, limit = NA_real_,
          tie_threshold = tie_threshold, hc_threshold = ht
        )
      ))
    }
  }

  # Totals bets
  if (!is.null(match_odds_tot) && nrow(match_odds_tot) > 0) {
    for (i in seq_len(nrow(match_odds_tot))) {
      row <- match_odds_tot[i, ]
      bets <- c(bets, list(
        tibble(
          bet_type = "over", outcome = "over", market = "totals",
          o = row$o_over, booker = row$booker,
          change = NA_real_, limit = row$limit,
          tie_threshold = tie_threshold, hc_threshold = NA_real_
        ),
        tibble(
          bet_type = "under", outcome = "under", market = "totals",
          o = row$o_under, booker = row$booker,
          change = NA_real_, limit = row$limit,
          tie_threshold = tie_threshold, hc_threshold = NA_real_
        )
      ))
    }
  }

  if (length(bets) == 0) return(NULL)
  bind_rows(bets)
}

# ── Per-match orchestrator ───────────────────────────────────────────────────

#' Run joint Kelly optimisation for all matches
#'
#' @param post Posterior draws (date, division, home, away, home_goals, away_goals)
#' @param odds_1x2 Outcome odds tibble (or NULL)
#' @param odds_hc Handicap odds tibble (or NULL)
#' @param odds_tot Totals odds tibble (or NULL)
#' @param cfg Config list from bets.yml
#' @return Tibble with same schema as individual market modules, plus market column
#' @export
run_joint_kelly <- function(post, odds_1x2, odds_hc, odds_tot, cfg) {
  leagues <- cfg$leagues
  divisions <- cfg$predictions$divisions
  max_match_stake <- cfg$bankroll$max_match_stake %||% 0.50
  ev_threshold <- cfg$bankroll$ev_threshold %||% 0.00

  # Filter posterior to configured divisions
  post_filtered <- post
  if (!is.null(divisions)) {
    post_filtered <- post_filtered |> filter(division %in% divisions)
  }

  # Parse handicap if character
  if (!is.null(odds_hc) && nrow(odds_hc) > 0 && is.character(odds_hc$change)) {
    odds_hc <- odds_hc |> mutate(change = parse_handicap(change))
  }

  # Rename odds columns to match posterior (home/away)
  # Odds already have home/away from load_odds

  # Get distinct future matches from posterior
  matches <- post_filtered |>
    distinct(date, division, home, away)

  # Map division numbers to league names
  if (!is.null(leagues)) {
    league_vec <- unlist(leagues)
    matches <- matches |>
      mutate(division = {
        d <- as.character(division)
        ifelse(d %in% names(league_vec), league_vec[d], d)
      })
  }

  all_results <- list()

  for (m in seq_len(nrow(matches))) {
    match <- matches[m, ]
    match_home <- match$home
    match_away <- match$away
    match_date <- match$date
    match_div <- match$division

    # Get posterior draws for this match
    draws <- post_filtered |>
      filter(date == match_date, home == match_home, away == match_away)

    if (nrow(draws) == 0) next

    # Get odds for this match from each market
    m_1x2 <- NULL
    m_hc <- NULL
    m_tot <- NULL

    if (isTRUE(cfg$markets$outcome) && !is.null(odds_1x2) && nrow(odds_1x2) > 0) {
      m_1x2 <- odds_1x2 |>
        filter(date == match_date, home == match_home, away == match_away)
      if (nrow(m_1x2) == 0) m_1x2 <- NULL
    }

    if (isTRUE(cfg$markets$handicap) && !is.null(odds_hc) && nrow(odds_hc) > 0) {
      m_hc <- odds_hc |>
        filter(date == match_date, home == match_home, away == match_away)
      if (nrow(m_hc) == 0) m_hc <- NULL
    }

    if (isTRUE(cfg$markets$totals) && !is.null(odds_tot) && nrow(odds_tot) > 0) {
      m_tot <- odds_tot |>
        filter(date == match_date, home == match_home, away == match_away)
      if (nrow(m_tot) == 0) m_tot <- NULL
    }

    # Collect all bets for this match
    bets <- collect_match_bets(m_1x2, m_hc, m_tot, cfg)
    if (is.null(bets) || nrow(bets) == 0) next

    # Compute posterior probabilities for pre-filtering
    indicators <- build_indicators(draws, bets)
    bets$p <- colMeans(indicators)
    bets$implied_p <- 1 / bets$o

    # Pre-filter: only keep positive-EV bets (posterior prob > implied + threshold)
    ev_edge <- bets$p - bets$implied_p
    keep <- ev_edge > ev_threshold
    if (!any(keep)) next

    bets <- bets[keep, ]
    indicators <- indicators[, keep, drop = FALSE]

    # Optimise joint Kelly
    fracs <- get_kelly_joint(indicators, bets$o, max_stake = max_match_stake)

    # Filter to non-trivial allocations
    nontrivial <- fracs > 1e-4
    if (!any(nontrivial)) next

    bets <- bets[nontrivial, ]
    fracs <- fracs[nontrivial]

    # Build output rows matching market module schema
    result <- bets |>
      mutate(
        date = match_date,
        division = match_div,
        heima = match_home,
        gestir = match_away,
        kelly = fracs
      ) |>
      select(
        date, division, booker, heima, gestir,
        market, outcome, change, limit,
        p, o, kelly
      )

    all_results <- c(all_results, list(result))
  }

  if (length(all_results) == 0) return(NULL)

  combined <- bind_rows(all_results)

  # Apply format_bet_text (scales kelly by kelly_frac, adds ev/bet_amount/text)
  combined <- combined |>
    format_bet_text(cfg) |>
    filter(bet_amount >= cfg$bankroll$min_bet_amount)

  if (nrow(combined) == 0) return(NULL)
  combined
}
