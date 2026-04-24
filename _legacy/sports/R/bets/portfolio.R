#' Cross-match portfolio optimisation (Stage 2)
#'
#' Given per-match bet packages from Stage 1 (joint Kelly), finds optimal
#' per-match scaling factors that maximise total expected log-growth subject
#' to a daily budget constraint.
#'
#' Uses the joint wealth formulation with random draw pairing to correctly
#' handle simultaneous bets sharing the same bankroll.

box::use(
  nloptr[nloptr]
)

#' Optimise per-match scaling factors for a single day's matches
#'
#' @param packages List of bet packages. Each must have:
#'   match_key (character), match_pnl (numeric vector),
#'   raw_kelly_sum (numeric), kelly_frac (numeric).
#' @param max_daily Max total fraction of bankroll to wager per day.
#' @param mode "proportional" (uniform scaling) or "optimal" (convex optimisation).
#' @return List with lambdas (named numeric) and diagnostics (list).
#' @export
portfolio_optimize <- function(packages, max_daily, mode = "proportional") {
  M <- length(packages)
  if (M == 0) return(list(lambdas = numeric(0), diagnostics = list()))

  match_keys <- vapply(packages, `[[`, character(1), "match_key")

  # Effective stakes = kelly_frac * raw_kelly_sum (actual bankroll fraction)
  eff_stakes <- vapply(packages, function(p) {
    p$kelly_frac * p$raw_kelly_sum
  }, numeric(1))
  total_eff <- sum(eff_stakes)

  # Under budget: no scaling needed
  if (total_eff <= max_daily) {
    return(list(
      lambdas = stats::setNames(rep(1.0, M), match_keys),
      diagnostics = list(budget_binding = FALSE, total_stake = total_eff)
    ))
  }

  # Proportional mode: uniform scaling (matches legacy behaviour)
  if (mode == "proportional") {
    scale <- max_daily / total_eff
    cat(sprintf(
      "  Portfolio: proportional scale=%.3f (total=%.3f > budget=%.3f)\n",
      scale, total_eff, max_daily
    ))
    return(list(
      lambdas = stats::setNames(rep(scale, M), match_keys),
      diagnostics = list(
        budget_binding = TRUE, total_stake = max_daily, scale_factor = scale
      )
    ))
  }

  # Optimal mode: convex portfolio optimisation
  # Build S x M effective PnL matrix with random draw pairing
  raw_pnls <- lapply(packages, `[[`, "match_pnl")
  kelly_fracs <- vapply(packages, `[[`, numeric(1), "kelly_frac")
  S_max <- max(vapply(raw_pnls, length, integer(1)))

  set.seed(42)
  pnl_matrix <- matrix(0, nrow = S_max, ncol = M)
  for (m in seq_len(M)) {
    v <- raw_pnls[[m]] * kelly_fracs[m]
    if (length(v) < S_max) {
      v <- v[sample.int(length(v), S_max, replace = TRUE)]
    }
    pnl_matrix[, m] <- v[sample.int(S_max)]
  }

  # Objective: minimise -mean(log(1 + pnl_matrix %*% lambda))
  eval_f <- function(lambda) {
    wealth <- 1 + as.vector(pnl_matrix %*% lambda)
    wealth <- pmax(wealth, 1e-10)
    -mean(log(wealth))
  }

  eval_grad_f <- function(lambda) {
    wealth <- 1 + as.vector(pnl_matrix %*% lambda)
    wealth <- pmax(wealth, 1e-10)
    -as.vector(crossprod(pnl_matrix, 1 / wealth)) / S_max
  }

  # Budget constraint: sum(lambda * eff_stakes) <= max_daily
  eval_g_ineq <- function(lambda) sum(lambda * eff_stakes) - max_daily
  eval_jac_g_ineq <- function(lambda) eff_stakes

  lambda0 <- rep(min(1, max_daily / total_eff), M)

  result <- tryCatch(
    nloptr(
      x0 = lambda0,
      eval_f = eval_f,
      eval_grad_f = eval_grad_f,
      eval_g_ineq = eval_g_ineq,
      eval_jac_g_ineq = eval_jac_g_ineq,
      lb = rep(0, M),
      ub = rep(1, M),
      opts = list(
        algorithm = "NLOPT_LD_SLSQP",
        xtol_rel = 1e-8,
        maxeval = 500
      )
    ),
    error = function(e) {
      warning("Portfolio SLSQP failed: ", conditionMessage(e),
              ". Falling back to proportional.")
      NULL
    }
  )

  if (is.null(result)) {
    scale <- max_daily / total_eff
    return(list(
      lambdas = stats::setNames(rep(scale, M), match_keys),
      diagnostics = list(budget_binding = TRUE, fallback = TRUE)
    ))
  }

  lambdas <- stats::setNames(result$solution, match_keys)
  optimal_growth <- -result$objective

  # Compare to proportional baseline
  prop_lambda <- max_daily / total_eff
  prop_wealth <- pmax(1 + prop_lambda * rowSums(pnl_matrix), 1e-10)
  prop_growth <- mean(log(prop_wealth))

  for (m in seq_len(M)) {
    cat(sprintf(
      "  Portfolio: %s: lambda=%.3f, stake=%.4f->%.4f\n",
      match_keys[m], lambdas[m], eff_stakes[m], lambdas[m] * eff_stakes[m]
    ))
  }

  improvement <- if (abs(prop_growth) > 1e-10) {
    100 * (optimal_growth - prop_growth) / abs(prop_growth)
  } else {
    0
  }

  cat(sprintf(
    "  Portfolio growth: %.6f (vs proportional %.6f, %+.2f%%)\n",
    optimal_growth, prop_growth, improvement
  ))

  list(
    lambdas = lambdas,
    diagnostics = list(
      budget_binding = TRUE,
      total_stake = sum(lambdas * eff_stakes),
      portfolio_growth = optimal_growth,
      proportional_growth = prop_growth,
      solver_status = result$status
    )
  )
}
