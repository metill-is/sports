#' Cross-match portfolio optimisation (Stage 2).
#'
#' Given per-match bet packages from Stage 1 (joint Kelly), finds optimal
#' per-match scaling factors that maximise total expected log-growth subject
#' to a daily budget constraint.
#'
#' Uses the joint wealth formulation with random draw pairing across matches.
#'
#' @param packages List of bet packages. Each must have:
#'   match_key (character), match_pnl (numeric vector),
#'   match_kelly_sum (numeric), kelly_frac (numeric).
#' @param max_daily Max total fraction of bankroll to wager per day.
#' @param mode "proportional" (uniform scaling) or "optimal" (convex
#'   optimisation per match).
#' @param seed Integer seed for the draw-pairing random sampling. Reproducible
#'   across runs when set.
#' @return List with `lambdas` (named numeric, names = match_key) and
#'   `diagnostics` (list with budget_binding, total_stake, optionally
#'   portfolio_growth, proportional_growth, solver_status, fallback).
#' @export
portfolio_optimise <- function(packages, max_daily,
                               mode = c("optimal", "proportional"),
                               seed = 42L) {
  mode <- match.arg(mode)
  M <- length(packages)
  if (M == 0L) {
    return(list(lambdas = numeric(0), diagnostics = list()))
  }

  match_keys <- vapply(packages, `[[`, character(1), "match_key")
  eff_stakes <- vapply(
    packages, function(p) p$kelly_frac * p$match_kelly_sum,
    numeric(1)
  )
  total_eff <- sum(eff_stakes)

  # Under budget: no scaling needed
  if (total_eff <= max_daily) {
    return(list(
      lambdas = stats::setNames(rep(1.0, M), match_keys),
      diagnostics = list(budget_binding = FALSE, total_stake = total_eff)
    ))
  }

  if (mode == "proportional") {
    scale <- max_daily / total_eff
    return(list(
      lambdas = stats::setNames(rep(scale, M), match_keys),
      diagnostics = list(
        budget_binding = TRUE,
        total_stake    = max_daily,
        scale_factor   = scale
      )
    ))
  }

  # Optimal mode: convex portfolio optimisation
  raw_pnls <- lapply(packages, `[[`, "match_pnl")
  kelly_fracs <- vapply(packages, `[[`, numeric(1), "kelly_frac")
  S_max <- max(vapply(raw_pnls, length, integer(1)))

  # Draw-pairing uses random sampling for reproducibility; isolate the global
  # RNG state so the caller's stream isn't perturbed.
  pnl_matrix <- withr::with_seed(seed, {
    mat <- matrix(0, nrow = S_max, ncol = M)
    for (m in seq_len(M)) {
      v <- raw_pnls[[m]] * kelly_fracs[m]
      if (length(v) < S_max) {
        v <- v[sample.int(length(v), S_max, replace = TRUE)]
      }
      mat[, m] <- v[sample.int(S_max)]
    }
    mat
  })

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
  eval_g_ineq <- function(lambda) sum(lambda * eff_stakes) - max_daily
  eval_jac_g_ineq <- function(lambda) eff_stakes

  lambda0 <- rep(min(1, max_daily / total_eff), M)

  result <- tryCatch(
    nloptr::nloptr(
      x0 = lambda0,
      eval_f = eval_f,
      eval_grad_f = eval_grad_f,
      eval_g_ineq = eval_g_ineq,
      eval_jac_g_ineq = eval_jac_g_ineq,
      lb = rep(0, M),
      ub = rep(1, M),
      opts = list(
        algorithm = "NLOPT_LD_SLSQP",
        xtol_rel  = 1e-8,
        maxeval   = 500L
      )
    ),
    error = function(e) {
      warning(
        "Portfolio SLSQP failed: ", conditionMessage(e),
        ". Falling back to proportional.",
        call. = FALSE
      )
      NULL
    }
  )

  # nloptr can return negative status without throwing (e.g. -4
  # NLOPT_ROUNDOFF_LIMITED). Treat those as silent failures and route through
  # the same fallback as a thrown error.
  if (!is.null(result) && !is.null(result$status) && result$status < 0) {
    warning(
      "Portfolio SLSQP returned failure status ", result$status,
      ". Falling back to proportional.",
      call. = FALSE
    )
    result <- NULL
  }

  if (is.null(result)) {
    scale <- max_daily / total_eff
    return(list(
      lambdas = stats::setNames(rep(scale, M), match_keys),
      diagnostics = list(
        budget_binding = TRUE,
        fallback       = TRUE,
        scale_factor   = scale,
        total_stake    = max_daily
      )
    ))
  }

  lambdas <- stats::setNames(result$solution, match_keys)
  optimal_growth <- -result$objective

  prop_lambda <- max_daily / total_eff
  prop_wealth <- pmax(1 + prop_lambda * rowSums(pnl_matrix), 1e-10)
  prop_growth <- mean(log(prop_wealth))

  list(
    lambdas = lambdas,
    diagnostics = list(
      budget_binding      = TRUE,
      total_stake         = sum(lambdas * eff_stakes),
      portfolio_growth    = optimal_growth,
      proportional_growth = prop_growth,
      solver_status       = result$status
    )
  )
}
