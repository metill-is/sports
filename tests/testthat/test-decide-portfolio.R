make_pkg <- function(match_key, match_pnl, match_kelly_sum, kelly_frac = 0.10) {
  list(
    match_key       = match_key,
    match_pnl       = match_pnl,
    match_kelly_sum = match_kelly_sum,
    kelly_frac      = kelly_frac
  )
}

test_that("portfolio_optimise returns lambdas = 1 when total stake under budget", {
  set.seed(11)
  pkgs <- list(
    make_pkg("g1", rnorm(500, mean = 0.05), match_kelly_sum = 0.30),
    make_pkg("g2", rnorm(500, mean = 0.04), match_kelly_sum = 0.20)
  )
  # eff stake = 0.10 * (0.30 + 0.20) = 0.05; budget 0.10 -> under, all 1
  out <- portfolio_optimise(pkgs, max_daily = 0.10, mode = "optimal")
  expect_equal(out$lambdas, c(g1 = 1, g2 = 1))
  expect_false(out$diagnostics$budget_binding)
})

test_that("portfolio_optimise (proportional) scales uniformly when over budget", {
  set.seed(11)
  pkgs <- list(
    make_pkg("g1", rnorm(500, mean = 0.05), match_kelly_sum = 1.0),
    make_pkg("g2", rnorm(500, mean = 0.04), match_kelly_sum = 0.5)
  )
  # eff = 0.10 * (1 + 0.5) = 0.15; budget 0.05 -> scale = 0.05/0.15 = 0.333
  out <- portfolio_optimise(pkgs, max_daily = 0.05, mode = "proportional")
  expect_true(out$diagnostics$budget_binding)
  expect_equal(unname(out$lambdas[1]), 0.05 / 0.15, tolerance = 1e-6)
  expect_equal(unname(out$lambdas[1]), unname(out$lambdas[2]))
})

test_that("portfolio_optimise (optimal) finds non-uniform lambdas when over budget", {
  set.seed(11)
  # Two matches: one is much higher EV than the other.
  # Optimal should scale the high-EV match higher.
  pkgs <- list(
    make_pkg("g1", c(rep(0.20, 400), rep(-0.05, 100)), match_kelly_sum = 1.0),
    make_pkg("g2", c(rep(0.04, 250), rep(-0.04, 250)), match_kelly_sum = 1.0)
  )
  out <- portfolio_optimise(pkgs, max_daily = 0.10, mode = "optimal", seed = 7L)
  expect_true(out$diagnostics$budget_binding)
  # g1 has clearly better risk-adjusted profile -> larger lambda
  expect_gt(out$lambdas[["g1"]], out$lambdas[["g2"]])
  # Budget respected
  eff <- 0.10 * c(1, 1) # kelly_frac * match_kelly_sum
  expect_lte(sum(out$lambdas * eff), 0.10 + 1e-6)
})

test_that("portfolio_optimise handles empty package list", {
  out <- portfolio_optimise(list(), max_daily = 0.05)
  expect_equal(length(out$lambdas), 0L)
})

test_that("portfolio_optimise errors on invalid mode", {
  pkgs <- list(make_pkg("g1", rep(0.01, 100), 0.5))
  expect_error(portfolio_optimise(pkgs, max_daily = 0.01, mode = "bogus"))
})

test_that("portfolio_optimise falls back to proportional on solver error", {
  # Inject a solver failure via local mock — testthat ed3 supports this for
  # any namespaced function call.
  testthat::local_mocked_bindings(
    nloptr = function(...) stop("Injected solver failure"),
    .package = "nloptr"
  )

  pkgs <- list(
    make_pkg("g1", c(rep(0.10, 500), rep(-0.05, 500)), match_kelly_sum = 1.0),
    make_pkg("g2", c(rep(0.05, 500), rep(-0.03, 500)), match_kelly_sum = 1.0)
  )
  expect_warning(
    out <- portfolio_optimise(pkgs, max_daily = 0.05, mode = "optimal"),
    "Falling back"
  )
  expect_true(out$diagnostics$fallback)
  expect_true(out$diagnostics$budget_binding)
  expect_equal(out$diagnostics$total_stake, 0.05)
  # Uniform proportional lambdas
  expect_equal(unname(out$lambdas[1]), unname(out$lambdas[2]))
})

test_that("portfolio_optimise does NOT mutate the global RNG state", {
  set.seed(2024L)
  pkgs <- list(
    make_pkg("g1", rnorm(200), match_kelly_sum = 1.0),
    make_pkg("g2", rnorm(200), match_kelly_sum = 1.0)
  )
  # Capture state immediately before the call so any drift inside is visible.
  before <- .Random.seed
  portfolio_optimise(pkgs, max_daily = 0.05, mode = "optimal", seed = 99L)
  after <- .Random.seed
  expect_identical(before, after)
})
