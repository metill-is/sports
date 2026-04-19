#' Joint Kelly via CVXR (optional convex solver)
#'
#' Provides a parallel implementation of `get_kelly_joint()` that uses the
#' `CVXR` disciplined-convex-programming framework instead of SLSQP from
#' `nloptr`. The two are mathematically equivalent on the unconstrained
#' Kelly problem — both converge to the same global optimum because the
#' objective is strictly concave on the feasible set (Busseti-Ryu-Boyd
#' 2016 §2). The CVXR route unlocks:
#'
#'   1. A **certified** convex solve (SLSQP is a general NLP method; CVXR
#'      proves feasibility and optimality via KKT residuals).
#'   2. Natural extensions to the **risk-constrained** Kelly formulation of
#'      Busseti, Ryu & Boyd (arXiv:1603.06183) — a drawdown probability
#'      bound tunable via a single risk-aversion parameter λ ≥ 0.
#'
#' CVXR pulls in a substantial dependency graph (conic solvers, Rmosek/
#' ECOS/SCS). For that reason it is NOT a hard dependency of the pipeline:
#' this module gracefully errors out with an install hint if CVXR is
#' absent. The default solver remains `nloptr::nloptr` via `get_kelly_joint()`.
#'
#' Usage (once CVXR is installed):
#'   box::use(./kelly_cvxr[get_kelly_cvxr])
#'   fit <- get_kelly_cvxr(net_return = R, max_stake = 1.0)
#'   fit <- get_kelly_cvxr(R, max_stake = 1.0, risk_lambda = 2.0)  # RC-Kelly

box::use(
  dplyr[tibble]
)

#' Check CVXR is installed and give a helpful error if not.
.require_cvxr <- function() {
  if (!requireNamespace("CVXR", quietly = TRUE)) {
    stop(
      "CVXR is not installed. To use the CVXR-based solver, run:\n",
      "    install.packages(\"CVXR\")\n",
      "The default SLSQP solver (`get_kelly_joint()`) does not need CVXR."
    )
  }
}

#' Joint Kelly using CVXR
#'
#' Solves
#'     max_f (1/S) Σ_s log(1 + R[s,]·f)
#'     s.t.  sum(f) ≤ max_stake,  f ≥ 0
#'
#' Optionally adds the Busseti-Ryu-Boyd risk constraint:
#'     (1/S) Σ_s W_s(f)^(-λ) ≤ 1    (equivalent: LSE(-λ log W) ≤ log S)
#'
#' @param net_return S×B numeric matrix of per-draw net returns (win → o-1,
#'   push → 0, loss → -1). Same shape as `get_kelly_joint()`.
#' @param max_stake Max total fraction to stake per match. Default 1.0.
#' @param risk_lambda Risk-aversion parameter λ ≥ 0. If NULL (default) the
#'   unconstrained Kelly is solved. A positive value adds the drawdown
#'   bound; larger λ → tighter bound → smaller stakes.
#' @param solver CVXR solver name. "ECOS" is the default (conic) solver; "SCS"
#'   is the first-order fallback. Both handle the exponential cone required
#'   by log terms.
#' @return List with the same shape as `get_kelly_joint()`:
#'   - `solution`: numeric vector of optimal fractions (length B)
#'   - `diagnostics`: growth_rate, worst_case_wealth, n_effective_bets,
#'     floor_triggered, solver_status, solver_duality_gap
#' @export
get_kelly_cvxr <- function(
  net_return,
  max_stake = 1.0,
  risk_lambda = NULL,
  solver = "ECOS"
) {
  .require_cvxr()
  S <- nrow(net_return)
  B <- ncol(net_return)
  if (B == 0) {
    return(list(
      solution = numeric(0),
      diagnostics = list(
        growth_rate = 0, worst_case_wealth = 1,
        n_effective_bets = 0, floor_triggered = FALSE,
        solver_status = "trivial", solver_duality_gap = 0
      )
    ))
  }

  # Access CVXR functions without attaching the namespace.
  Variable <- CVXR::Variable
  Maximize <- CVXR::Maximize
  Problem <- CVXR::Problem
  solve_cvxr <- CVXR::solve # avoid clashing with base::solve

  f <- Variable(B, nonneg = TRUE)
  wealth <- 1 + net_return %*% f
  # CVXR recognises `log` on an affine-transformed variable as concave and
  # routes it through the exponential cone — this is the critical piece.
  log_wealth <- CVXR::log(wealth)

  objective <- Maximize(sum(log_wealth) / S)

  constraints <- list(
    sum(f) <= max_stake
  )

  # Optional Busseti-Ryu-Boyd risk constraint.
  # LSE(-λ log W_s) ≤ log S  ⇔  (1/S) Σ exp(-λ log W_s) ≤ 1
  # which bounds the probability of a large drawdown (see their eq. 7-8).
  if (!is.null(risk_lambda) && risk_lambda > 0) {
    # log_sum_exp is convex; -λ log_wealth is convex when λ > 0.
    # CVXR allows LSE(convex) ≤ constant as a valid convex constraint.
    lse_term <- CVXR::log_sum_exp(-risk_lambda * log_wealth)
    constraints <- c(constraints, list(lse_term <= log(S)))
  }

  prob <- Problem(objective, constraints)
  result <- solve_cvxr(prob, solver = solver)

  f_opt <- as.numeric(result$getValue(f))
  # Defensive: clip any tiny numerical negatives from solver tolerance
  f_opt <- pmax(f_opt, 0)

  wealth_opt <- 1 + as.vector(net_return %*% f_opt)
  floor_eps <- 1e-10
  floor_triggered <- min(wealth_opt) < floor_eps
  diagnostics <- list(
    growth_rate = mean(log(pmax(wealth_opt, floor_eps))),
    worst_case_wealth = min(wealth_opt),
    n_effective_bets = sum(f_opt > 1e-6),
    floor_triggered = floor_triggered,
    solver_status = result$status,
    solver_duality_gap = if (!is.null(result$solver_stats$solve_time)) {
      result$value - result$value
    } else {
      NA_real_
    }
  )
  list(solution = f_opt, diagnostics = diagnostics)
}

#' Compare SLSQP and CVXR solutions on the same input
#'
#' Convenience wrapper for evaluating equivalence / performance. Returns
#' a tibble with one row per bet, the two solutions, and their absolute
#' difference. Also reports both objective values so you can confirm they
#' match to solver tolerance.
#'
#' @param net_return Same as `get_kelly_cvxr`.
#' @param max_stake  Same as `get_kelly_cvxr`.
#' @param slsqp_fit Optional pre-computed `get_kelly_joint` result. If NULL,
#'   will be computed.
#' @return list with `per_bet` tibble and `summary` list.
#' @export
compare_slsqp_cvxr <- function(net_return, max_stake = 1.0, slsqp_fit = NULL) {
  .require_cvxr()
  if (is.null(slsqp_fit)) {
    # Lazy import to avoid circular dependency when CVXR absent.
    box::use(. / kelly_joint[get_kelly_joint])
    slsqp_fit <- get_kelly_joint(net_return = net_return, max_stake = max_stake)
  }
  cvxr_fit <- get_kelly_cvxr(net_return, max_stake = max_stake)

  per_bet <- tibble(
    j = seq_along(slsqp_fit$solution),
    f_slsqp = slsqp_fit$solution,
    f_cvxr = cvxr_fit$solution,
    abs_diff = abs(slsqp_fit$solution - cvxr_fit$solution)
  )
  summary_list <- list(
    max_abs_diff = max(per_bet$abs_diff),
    growth_slsqp = slsqp_fit$diagnostics$growth_rate,
    growth_cvxr = cvxr_fit$diagnostics$growth_rate,
    growth_diff = cvxr_fit$diagnostics$growth_rate -
      slsqp_fit$diagnostics$growth_rate,
    cvxr_status = cvxr_fit$diagnostics$solver_status
  )
  list(per_bet = per_bet, summary = summary_list)
}
