#' @include storage.R
NULL

#' Build the S x B net-return matrix from posterior draws and bet table.
#'
#' For each posterior draw s and each bet j, returns the bet's net return:
#' (odds_j - 1) if bet j wins, else -1.
#'
#' Bet-resolution by market (with `tie_threshold = T` from league config):
#' - moneyline: home wins iff (hg - ag)  >  T
#'              draw wins iff |hg - ag|  <= T
#'              away wins iff (ag - hg)  >  T
#' - spread:    `line` is the home team's signed handicap shared across
#'              outcomes of the row (positive = home gets head start). Define
#'              `adj = (home_goals + line) - away_goals`. Then
#'              home wins iff adj      >  T
#'              away wins iff -adj     >  T
#'              draw wins iff |adj|    <= T  (push band)
#' - total:     over  wins iff (hg + ag) > line
#'              under wins iff (hg + ag) < line
#'
#' For sports with integer scoring and `T = 0` (football basketball default)
#' the equality-as-tie behaviour is preserved exactly. For handball
#' (`T = 0.5` per `config/leagues.yml`), continuous Student-t goal draws
#' have `hg == ag` of measure zero — the `T = 0.5` band captures the
#' probability mass that would round to an integer tie at game time. This
#' unlocks the handball moneyline draw market that was previously P = 0.
#'
#' @param beliefs Tibble with `home_goals` and `away_goals` columns
#'   (one row per posterior draw per match).
#' @param bets Tibble with `market`, `outcome`, `line`, `odds`.
#' @param tie_threshold Numeric \eqn{\ge 0}. See description. Default 0.
#' @return Numeric matrix, `nrow(beliefs)` x `nrow(bets)`. Each cell is
#'   `odds_j - 1` (win) or `-1` (loss).
#' @keywords internal
#' @noRd
build_return_matrix <- function(beliefs, bets, tie_threshold = 0) {
  S <- nrow(beliefs)
  B <- nrow(bets)
  R <- matrix(NA_real_, nrow = S, ncol = B)
  hg <- beliefs$home_goals
  ag <- beliefs$away_goals
  diff <- hg - ag
  total <- hg + ag

  for (j in seq_len(B)) {
    win <- switch(bets$market[[j]],
      moneyline = switch(bets$outcome[[j]],
        home = diff > tie_threshold,
        draw = abs(diff) <= tie_threshold,
        away = -diff > tie_threshold,
        stop("Moneyline outcome must be 'home','draw','away', got: ",
          bets$outcome[[j]],
          call. = FALSE
        )
      ),
      spread = {
        if (!bets$outcome[[j]] %in% c("home", "away", "draw")) {
          stop(
            "Spread outcome must be 'home', 'away', or 'draw', got: ",
            bets$outcome[[j]],
            call. = FALSE
          )
        }
        # WHY: `line` is the home team's signed handicap shared across all
        # outcomes of a parse_match_detail() row. Home and away are mirror
        # images under one adjusted margin `adj = (hg + line) - ag`; they are
        # NOT separate handicaps applied symmetrically to each side. The
        # pre-2026-05-13 away formula `(ag + line) > hg` flipped the sign of
        # the away branch and produced implausibly-high spread-away EVs on
        # cup matches with |line| >= 2. The `tie_threshold` band keeps the
        # spread push semantics consistent with moneyline draws for sports
        # whose continuous-goal model would otherwise hit `adj == 0` only
        # with measure zero.
        line <- bets$line[[j]]
        adj <- (hg + line) - ag
        if (bets$outcome[[j]] == "home") {
          adj > tie_threshold
        } else if (bets$outcome[[j]] == "away") {
          -adj > tie_threshold
        } else {
          abs(adj) <= tie_threshold
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
#' @param tie_threshold Numeric \eqn{\ge 0}. Tie/push band — see
#'   `build_return_matrix()` for resolution semantics. Sourced per-league
#'   from `config/leagues.yml::*.betting.scoring.tie_threshold` by
#'   `decide_league()`. Default 0 preserves equality-as-tie behaviour for
#'   integer-scoring sports.
#' @return List with `bets` (input cols + p, ev, kelly_raw), `match_pnl`
#'   (numeric S), `match_kelly_sum` (sum of kelly_raw), `diagnostics` (list).
#' @export
kelly_joint <- function(beliefs, bets,
                        max_match_stake = 1.0,
                        ev_threshold = 0.0,
                        tie_threshold = 0) {
  R <- build_return_matrix(beliefs, bets, tie_threshold = tie_threshold)
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
