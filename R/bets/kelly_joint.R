#' Joint Kelly criterion across all markets per match
#'
#' Replaces the three independent per-market Kelly runs with a single
#' optimiser that sees all bets simultaneously. This correctly handles:
#' - Mutually exclusive outcomes within a market (1x2)
#' - Cross-market correlation (home win + home -0.5 handicap)
#' - Full posterior uncertainty (no collapse to point probabilities)

box::use(
  nloptr[nloptr],
  dplyr[
    filter, mutate, select, any_of, bind_rows, inner_join, distinct,
    summarise, arrange, tibble, rename
  ],
  tidyr[crossing],
  stringr[str_split_fixed]
)

#' Parse Lengjan handicap strings to signed numeric
#'
#' "0-1" -> -1 (away gets 1-goal head start)
#' "1-0" -> +1 (home gets 1-goal head start)
#' @export
parse_handicap <- function(change_str) {
  parts <- str_split_fixed(change_str, "-", n = 2)
  result <- as.numeric(parts[, 1]) - as.numeric(parts[, 2])
  if (any(is.na(result))) {
    warning(
      "Could not parse handicap values: ",
      paste(change_str[is.na(result)], collapse = ", ")
    )
  }
  result
}

# ── Return matrix construction ───────────────────────────────────────────────

#' Build net return matrix from posterior draws
#'
#' Produces an S x B matrix of net returns per unit staked:
#'   win  -> odds - 1 (net profit)
#'   push -> 0        (stake returned)
#'   loss -> -1       (stake lost)
#'
#' Handles half-point lines (no push), integer lines (push at exact value),
#' and quarter lines (half-push via split bet).
#'
#' For backward compatibility, build_indicators() is retained as a wrapper
#' that returns the binary win indicator (1/0) for probability estimation.
#'
#' @param draws Tibble of posterior draws for one match (home_goals, away_goals).
#' @param bets Tibble describing each bet with columns:
#'   bet_type, change, limit, tie_threshold, hc_threshold, o (decimal odds).
#' @return Numeric matrix S x B of net returns
#' @export
build_return_matrix <- function(draws, bets) {
  S <- nrow(draws)
  B <- nrow(bets)
  returns <- matrix(-1, nrow = S, ncol = B)

  # Accept either (home_goals, away_goals) or (goal_diff, total_goals) directly
  if ("goal_diff" %in% names(draws) && "total_goals" %in% names(draws)) {
    diff <- draws$goal_diff
    total <- draws$total_goals
  } else {
    diff <- draws$home_goals - draws$away_goals
    total <- draws$home_goals + draws$away_goals
  }

  for (j in seq_len(B)) {
    bt <- bets$bet_type[j]
    tt <- bets$tie_threshold[j]
    o <- bets$o[j]

    # Compute win/push/loss state for each draw
    # state: 1 = win, 0 = push, -1 = loss
    state <- switch(bt,
      "1x2_home" = ifelse(diff > tt, 1L, -1L),
      "1x2_tie" = ifelse(abs(diff) <= tt, 1L, -1L),
      "1x2_away" = ifelse(diff < -tt, 1L, -1L),
      "hc_home" = {
        adj <- diff + bets$change[j]
        ht <- bets$hc_threshold[j]
        is_half <- (bets$change[j] != round(bets$change[j]))
        variant <- if ("market_variant" %in% names(bets)) bets$market_variant[j] else NA_character_
        if (isTRUE(variant == "european_3way") || is_half) {
          # European 3-way (tie zone bettable separately) OR half-point:
          # hc_home loses inside tie zone, no push on this side.
          ifelse(adj > ht, 1L, -1L)
        } else {
          # Asian 2-way integer: push when adj == 0.
          ifelse(adj > 0, 1L, ifelse(adj == 0, 0L, -1L))
        }
      },
      "hc_tie" = {
        adj <- diff + bets$change[j]
        ht <- bets$hc_threshold[j]
        # European 3-way HC tie: wins iff adj within tie zone |adj| <= ht.
        ifelse(abs(adj) <= ht, 1L, -1L)
      },
      "hc_away" = {
        adj <- diff + bets$change[j]
        ht <- bets$hc_threshold[j]
        is_half <- (bets$change[j] != round(bets$change[j]))
        variant <- if ("market_variant" %in% names(bets)) bets$market_variant[j] else NA_character_
        if (isTRUE(variant == "european_3way") || is_half) {
          ifelse(adj < -ht, 1L, -1L)
        } else {
          ifelse(adj < 0, 1L, ifelse(adj == 0, 0L, -1L))
        }
      },
      "over" = {
        lim <- bets$limit[j]
        is_half <- (lim != round(lim))
        if (is_half) {
          ifelse(total > lim, 1L, -1L)
        } else {
          # Integer line: push when total == limit
          ifelse(total > lim, 1L, ifelse(total == lim, 0L, -1L))
        }
      },
      "under" = {
        lim <- bets$limit[j]
        is_half <- (lim != round(lim))
        if (is_half) {
          ifelse(total < lim, 1L, -1L)
        } else {
          ifelse(total < lim, 1L, ifelse(total == lim, 0L, -1L))
        }
      },
      stop("Unknown bet_type: ", bt)
    )

    # Convert state to net return
    returns[, j] <- ifelse(state == 1L, o - 1, ifelse(state == 0L, 0, -1))
  }

  returns
}

#' Build binary indicator matrix (backward-compatible wrapper)
#'
#' Returns 1 for wins, 0 for push/loss. Used for probability estimation
#' (colMeans gives posterior win probability).
#'
#' @inheritParams build_return_matrix
#' @return Integer matrix S x B: 1 if bet j wins in draw s, 0 otherwise
#' @export
build_indicators <- function(draws, bets) {
  returns <- build_return_matrix(draws, bets)
  # Win = positive return, push/loss = 0
  matrix(as.integer(returns > 0), nrow = nrow(returns), ncol = ncol(returns))
}

# ── Joint optimiser ──────────────────────────────────────────────────────────

#' Joint Kelly criterion optimisation over the full posterior
#'
#' Maximises posterior expected log-growth:
#'   G(f) = (1/S) * sum_s log(1 + sum_j net_return_sj * f_j)
#'
#' The net_return matrix encodes win/push/loss per draw per bet:
#'   win  -> odds - 1, push -> 0, loss -> -1
#'
#' Uses SLSQP with analytic gradients.
#'
#' @param net_return S x B numeric matrix of net returns (from build_return_matrix).
#'   If NULL, computed from indicators and odds (backward compat).
#' @param indicators S x B integer matrix (deprecated, use net_return).
#' @param odds Numeric vector of length B (deprecated, use net_return).
#' @param max_stake Max total fraction to wager (default 0.50)
#' @return List with `solution` (numeric vector of optimal fractions) and
#'   `diagnostics` (growth_rate, worst_case_wealth, n_effective_bets).
#' @export
get_kelly_joint <- function(net_return = NULL, indicators = NULL, odds = NULL,
                            max_stake = 1.0) {
  # Backward compatibility: compute net_return from indicators + odds

  if (is.null(net_return)) {
    if (is.null(indicators) || is.null(odds)) {
      stop("Either net_return or both indicators and odds must be provided")
    }
    net_return <- sweep(indicators, 2, odds, `*`) - 1
  }

  S <- nrow(net_return)
  B <- ncol(net_return)

  if (B == 0) {
    return(list(solution = numeric(0), diagnostics = list(
      growth_rate = 0, worst_case_wealth = 1, n_effective_bets = 0
    )))
  }

  # Objective: negative expected log-growth (minimise)
  eval_f <- function(f) {
    wealth <- 1 + as.vector(net_return %*% f) # length S
    wealth <- pmax(wealth, 1e-10)
    -mean(log(wealth))
  }

  # Analytic gradient
  eval_grad_f <- function(f) {
    wealth <- 1 + as.vector(net_return %*% f)
    wealth <- pmax(wealth, 1e-10)
    inv_w <- 1 / wealth # length S
    -as.vector(crossprod(net_return, inv_w)) / S # length B
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

  f_opt <- result$solution

  # Diagnostics: growth rate, worst-case wealth, effective bet count
  wealth_opt <- 1 + as.vector(net_return %*% f_opt)
  diagnostics <- list(
    growth_rate = mean(log(pmax(wealth_opt, 1e-10))),
    worst_case_wealth = min(wealth_opt),
    n_effective_bets = sum(f_opt > 1e-6)
  )

  list(solution = f_opt, diagnostics = diagnostics)
}

# ── Fractional-Kelly growth curve ────────────────────────────────────────────

#' Fractional-Kelly growth curve G(alpha * f*) / G(f*)
#'
#' For each alpha, computes the posterior expected log-growth at fraction
#' alpha of the optimal Kelly stakes, plus the ratio to full-Kelly growth
#' and the alpha*(2-alpha) "binary Kelly" prediction.
#'
#' The alpha*(2-alpha) rule (often quoted as "half-Kelly gives 3/4 of
#' full-Kelly growth") is the second-order expansion of G around the
#' optimum and holds *exactly* only for a single binary bet. In joint
#' multi-bet settings it is a loose upper bound: the actual ratio drops
#' faster with decreasing alpha. See the 2026-04-19 numerical audit (V12)
#' for quantitative examples.
#'
#' @param net_return S x B net-return matrix (same form as get_kelly_joint).
#' @param f_star Numeric vector of length B with the optimal Kelly stakes.
#' @param alphas Numeric vector of fractions to evaluate.
#' @return Tibble with columns: alpha, G_at_alpha_f, ratio_to_full,
#'   binary_prediction, deviation.
#' @export
fractional_growth_curve <- function(
  net_return, f_star,
  alphas = c(0.1, 0.25, 0.5, 0.75, 1.0, 1.25)
) {
  G <- function(a) {
    w <- 1 + as.vector(net_return %*% (a * f_star))
    mean(log(pmax(w, 1e-10)))
  }
  Gs <- vapply(alphas, G, numeric(1))
  G_star <- G(1)
  ratio <- if (abs(G_star) > 1e-12) Gs / G_star else rep(NA_real_, length(alphas))
  binary <- alphas * (2 - alphas)
  tibble(
    alpha = alphas,
    G_at_alpha_f = Gs,
    ratio_to_full = ratio,
    binary_prediction = binary,
    deviation = ratio - binary
  )
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
      if (!is.na(row$o_home)) {
        bets <- c(bets, list(
          tibble(
            bet_type = "1x2_home", outcome = "home", market = "outcome",
            o = row$o_home, booker = row$booker,
            change = NA_real_, limit = NA_real_,
            tie_threshold = tie_threshold, hc_threshold = NA_real_
          )
        ))
      }
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
      if (!is.na(row$o_away)) {
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
  }

  # Handicap bets
  #
  # `market_variant` disambiguates settlement semantics on integer lines:
  #   - european_3way: tie zone is bettable AND priced by Lengjan (o_draw
  #     present). hc_home/hc_away LOSE inside the tie zone, hc_tie wins.
  #   - asian_2way:    no tie bet priced. hc_home/hc_away PUSH when adj == 0
  #     on integer lines and have no push on half-point lines.
  # Using `o_draw` presence (rather than has_ties alone) keeps the pipeline
  # faithful to what Lengjan actually offers on each row.
  if (!is.null(match_odds_hc) && nrow(match_odds_hc) > 0) {
    for (i in seq_len(nrow(match_odds_hc))) {
      row <- match_odds_hc[i, ]
      change_val <- row$change
      is_whole <- (change_val == round(change_val))
      is_european <- has_ties && is_whole
      hc_tie_priced <- is_european &&
        "o_draw" %in% names(row) && !is.na(row$o_draw)
      variant <- if (hc_tie_priced) "european_3way" else "asian_2way"

      if (hc_tie_priced) {
        ht <- tie_threshold
      } else {
        ht <- 0
      }

      if (!is.na(row$o_home)) {
        bets <- c(bets, list(
          tibble(
            bet_type = "hc_home", outcome = "home", market = "handicap",
            market_variant = variant,
            o = row$o_home, booker = row$booker,
            change = change_val, limit = NA_real_,
            tie_threshold = tie_threshold, hc_threshold = ht
          )
        ))
      }

      if (hc_tie_priced) {
        bets <- c(bets, list(
          tibble(
            bet_type = "hc_tie", outcome = "tie", market = "handicap",
            market_variant = variant,
            o = row$o_draw, booker = row$booker,
            change = change_val, limit = NA_real_,
            tie_threshold = tie_threshold, hc_threshold = ht
          )
        ))
      }

      if (!is.na(row$o_away)) {
        bets <- c(bets, list(
          tibble(
            bet_type = "hc_away", outcome = "away", market = "handicap",
            market_variant = variant,
            o = row$o_away, booker = row$booker,
            change = change_val, limit = NA_real_,
            tie_threshold = tie_threshold, hc_threshold = ht
          )
        ))
      }
    }
  }

  # Totals bets
  if (!is.null(match_odds_tot) && nrow(match_odds_tot) > 0) {
    for (i in seq_len(nrow(match_odds_tot))) {
      row <- match_odds_tot[i, ]
      if (!is.na(row$o_over)) {
        bets <- c(bets, list(
          tibble(
            bet_type = "over", outcome = "over", market = "totals",
            o = row$o_over, booker = row$booker,
            change = NA_real_, limit = row$limit,
            tie_threshold = tie_threshold, hc_threshold = NA_real_
          )
        ))
      }
      if (!is.na(row$o_under)) {
        bets <- c(bets, list(
          tibble(
            bet_type = "under", outcome = "under", market = "totals",
            o = row$o_under, booker = row$booker,
            change = NA_real_, limit = row$limit,
            tie_threshold = tie_threshold, hc_threshold = NA_real_
          )
        ))
      }
    }
  }

  if (length(bets) == 0) {
    return(NULL)
  }
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
  max_match_stake <- cfg$bankroll$max_match_kelly %||% 1.0
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

  # Map division numbers to league names (always character)
  matches <- matches |> mutate(division = as.character(division))
  if (!is.null(leagues)) {
    league_vec <- unlist(leagues)
    matches <- matches |>
      mutate(division = ifelse(
        division %in% names(league_vec), league_vec[division], division
      ))
  }

  all_results <- list()
  all_packages <- list()

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

    # Build return matrix (handles push/void correctly)
    net_return <- build_return_matrix(draws, bets)

    # Compute posterior win probabilities for pre-filtering and display
    bets$p <- colMeans(net_return > 0)
    bets$implied_p <- 1 / bets$o

    # Pre-filter: only keep positive-EV bets (posterior prob > implied + threshold)
    ev_edge <- bets$p - bets$implied_p
    keep <- ev_edge > ev_threshold
    keep[is.na(keep)] <- FALSE
    if (!any(keep)) next

    bets <- bets[keep, ]
    net_return <- net_return[, keep, drop = FALSE]

    # Optimise joint Kelly using return matrix directly
    kelly_result <- get_kelly_joint(net_return = net_return, max_stake = max_match_stake)
    fracs <- kelly_result$solution
    diag <- kelly_result$diagnostics

    cat(sprintf(
      "  %s v %s: G=%.4f, W_min=%.3f, n_bets=%d\n",
      match_home, match_away,
      diag$growth_rate, diag$worst_case_wealth, diag$n_effective_bets
    ))

    # Filter to non-trivial allocations
    nontrivial <- fracs > 1e-4
    if (!any(nontrivial)) next

    bets <- bets[nontrivial, ]
    fracs <- fracs[nontrivial]

    # Compute match PnL vector for portfolio optimisation (Stage 2)
    net_return_kept <- net_return[, nontrivial, drop = FALSE]
    match_pnl <- as.vector(net_return_kept %*% fracs)

    # Build output rows matching market module schema
    result <- bets |>
      mutate(
        date = match_date,
        division = match_div,
        heima = match_home,
        gestir = match_away,
        kelly = fracs,
        ev = round(p * (o - 1) - (1 - p), 2),
        pred = round(p, 3),
        p_o = round(1 / o, 3),
        text = sprintf("@%.2f (f=%.4f, ev=%.2f)", o, kelly, ev)
      ) |>
      select(
        date, division, booker, heima, gestir,
        market, outcome, change, limit,
        p, o, kelly, ev, pred, p_o, text
      )

    # Build bet package for portfolio optimisation
    bet_package <- list(
      date = match_date,
      home = match_home,
      away = match_away,
      match_pnl = match_pnl,
      raw_kelly_sum = sum(fracs),
      growth_rate = diag$growth_rate
    )

    all_results <- c(all_results, list(result))
    all_packages <- c(all_packages, list(bet_package))
  }

  if (length(all_results) == 0) {
    return(NULL)
  }

  combined <- bind_rows(all_results)
  if (nrow(combined) == 0) {
    return(NULL)
  }

  list(recommendations = combined, bet_packages = all_packages)
}
