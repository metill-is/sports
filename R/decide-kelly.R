#' @include storage.R
NULL

#' Build the S x B net-return matrix from posterior draws and bet table.
#'
#' For each posterior draw s and each bet j, returns the bet's net return:
#' (odds_j - 1) if bet j wins, else -1.
#'
#' Bet-resolution by market:
#' - moneyline: outcome == "home" wins iff home_goals > away_goals
#' - spread:    outcome == "home" wins iff (home_goals + line) > away_goals
#'              outcome == "away" wins iff (away_goals + line) > home_goals
#'              outcome == "draw" wins iff (home_goals + line) == away_goals (push)
#' - total:     outcome == "over"  wins iff (home_goals + away_goals) > line
#'              outcome == "under" wins iff (home_goals + away_goals) < line
#'
#' @keywords internal
#' @noRd
build_return_matrix <- function(beliefs, bets) {
  S <- nrow(beliefs)
  B <- nrow(bets)
  R <- matrix(NA_real_, nrow = S, ncol = B)
  hg <- beliefs$home_goals
  ag <- beliefs$away_goals
  total <- hg + ag

  for (j in seq_len(B)) {
    win <- switch(bets$market[[j]],
      moneyline = switch(bets$outcome[[j]],
        home = hg > ag,
        draw = hg == ag,
        away = hg < ag
      ),
      spread = {
        if (!bets$outcome[[j]] %in% c("home", "away", "draw")) {
          stop(
            "Spread outcome must be 'home', 'away', or 'draw', got: ",
            bets$outcome[[j]],
            call. = FALSE
          )
        }
        line <- bets$line[[j]]
        if (bets$outcome[[j]] == "home") {
          (hg + line) > ag
        } else if (bets$outcome[[j]] == "away") {
          (ag + line) > hg
        } else {
          # spread/draw: wins when the handicap-adjusted margin is exactly zero
          (hg + line) == ag
        }
      },
      total = {
        if (!bets$outcome[[j]] %in% c("over", "under")) {
          stop(
            "Total outcome must be 'over' or 'under', got: ",
            bets$outcome[[j]],
            call. = FALSE
          )
        }
        if (bets$outcome[[j]] == "over") {
          total > bets$line[[j]]
        } else {
          total < bets$line[[j]]
        }
      },
      stop("Unknown market: ", bets$market[[j]], call. = FALSE)
    )
    R[, j] <- ifelse(win, bets$odds[[j]] - 1, -1)
  }
  R
}

#' Convex-optimise joint Kelly stakes.
#'
#' Solves: maximise E[log(1 + R %*% f)] subject to f >= 0 and sum(f) <= max_stake.
#'
#' @keywords internal
#' @noRd
solve_kelly_joint <- function(net_return, max_stake) {
  B <- ncol(net_return)
  if (B == 0L) {
    return(numeric(0))
  }

  # Negative log-growth (we minimise; nloptr seeks min)
  obj <- function(f) {
    g <- log1p(as.numeric(net_return %*% f))
    if (any(!is.finite(g))) {
      return(Inf)
    }
    -mean(g)
  }

  # Analytical gradient: d(-mean(log(1 + R%*%f))) / df_j = -mean(R[,j] / (1 + R%*%f))
  # Project the denominator away from -1 to avoid passing NA into the C++
  # nlopt layer (which silently returns NLOPT_ROUNDOFF_LIMITED on bad gradient).
  grad <- function(f) {
    Rf <- as.numeric(net_return %*% f)
    denom <- pmax(1 + Rf, 1e-8)
    -as.numeric(crossprod(net_return, 1 / denom)) / nrow(net_return)
  }

  res <- nloptr::nloptr(
    x0 = rep(0, B),
    eval_f = obj,
    eval_grad_f = grad,
    lb = rep(0, B),
    ub = rep(max_stake, B),
    eval_g_ineq = function(f) sum(f) - max_stake,
    eval_jac_g_ineq = function(f) matrix(1, nrow = 1, ncol = B),
    opts = list(
      algorithm = "NLOPT_LD_SLSQP",
      xtol_rel  = 1e-7,
      maxeval   = 500L
    )
  )
  list(
    solution = pmax(res$solution, 0), status = res$status,
    iterations = res$iterations
  )
}

#' Joint Kelly criterion across all bets in one match.
#'
#' Replaces three independent per-market Kelly runs with a single convex
#' optimiser that sees all bets simultaneously. Correctly handles:
#' - Mutually exclusive outcomes within a market (1x2)
#' - Cross-market correlation (home win + home -0.5 spread)
#' - Full posterior uncertainty (no point-probability collapse)
#'
#' @param beliefs Tibble with `draw_id`, `home_goals`, `away_goals` for
#'   one match. Typically S = 4000 rows from a Stan fit.
#' @param bets Tibble with `market` (moneyline/spread/total), `outcome`
#'   (home/draw/away/over/under), `line` (numeric or NA), `odds` (decimal).
#' @param max_match_stake Per-match solver cap (`Σ f_j ≤ max_match_stake`).
#'   Default 1.0 = unconstrained joint Kelly. The §7.2 fractional-Kelly
#'   shrinkage `kelly_frac` is applied by the caller (`decide_league()`)
#'   after the optimiser returns, *not* via this cap.
#' @param ev_threshold Minimum EV per bet to consider. Bets below threshold
#'   are zeroed before optimisation.
#' @param tie_threshold Reserved for future use (tie-handling in handball
#'   scoring); currently unused.
#' @return List with `bets` (input cols + p, ev, kelly_raw), `match_pnl`
#'   (numeric S), `match_kelly_sum` (sum of kelly_raw), `diagnostics` (list).
#' @export
kelly_joint <- function(beliefs, bets,
                        max_match_stake = 1.0,
                        ev_threshold = 0.0,
                        tie_threshold = 0) {
  R <- build_return_matrix(beliefs, bets)
  p <- as.numeric(colMeans(R > 0))
  ev <- p * (bets$odds - 1) - (1 - p)

  keep <- ev >= ev_threshold
  R_keep <- R[, keep, drop = FALSE]

  f <- numeric(nrow(bets))
  solver_status <- NA_integer_
  solver_iterations <- NA_integer_
  if (sum(keep) > 0L) {
    sol <- solve_kelly_joint(R_keep, max_stake = max_match_stake)
    f[keep] <- sol$solution
    solver_status <- sol$status
    solver_iterations <- sol$iterations
  }

  match_pnl <- as.numeric(R %*% f)

  list(
    bets = tibble::tibble(
      market      = bets$market,
      outcome     = bets$outcome,
      line        = bets$line,
      odds        = bets$odds,
      p           = p,
      ev          = ev,
      kelly_raw   = f
    ),
    match_pnl = match_pnl,
    match_kelly_sum = sum(f),
    diagnostics = list(
      n_bets          = nrow(bets),
      n_kept          = sum(keep),
      max_match_stake = max_match_stake,
      nloptr_status   = solver_status,
      nloptr_iter     = solver_iterations
    )
  )
}
