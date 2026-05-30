test_that("fit_model returns a CmdStanMCMC for MCMC method", {
  skip_on_cran()
  skip_if_not_installed("cmdstanr")
  skip_if(
    is.null(tryCatch(cmdstanr::cmdstan_path(), error = function(e) NULL)),
    "cmdstan not installed"
  )

  tmp <- withr::local_tempfile(fileext = ".stan")
  writeLines(
    c(
      "data {",
      "  int<lower=0> N;",
      "  vector[N] y;",
      "  vector<lower=0>[N] sigma;",
      "}",
      "parameters {",
      "  real mu;",
      "  real<lower=0> tau;",
      "  vector[N] theta;",
      "}",
      "model {",
      "  tau ~ cauchy(0, 5);",
      "  theta ~ normal(mu, tau);",
      "  y ~ normal(theta, sigma);",
      "}"
    ),
    tmp
  )

  # 8 schools centered (theta ~ normal(mu, tau)) is the textbook
  # divergence-prone setup. The wrapper test is asserting the API shape,
  # not model quality, so disable the post-sample diagnostic gate.
  fit <- fit_model(
    stan_data = list(
      N = 8L,
      y = c(28, 8, -3, 7, -1, 1, 18, 12),
      sigma = c(15, 10, 16, 11, 9, 11, 10, 18)
    ),
    stan_model_path = tmp,
    method = "sample",
    chains = 2L,
    iter_warmup = 200L,
    iter_sampling = 200L,
    seed = 42L,
    show_progress = FALSE,
    check_diagnostics = FALSE
  )

  expect_s3_class(fit, "CmdStanMCMC")
  expect_equal(fit$num_chains(), 2L)
  # Variable names should include mu + tau + theta[1..8]
  vars <- fit$metadata()$variables
  expect_true("mu" %in% vars)
  expect_true("tau" %in% vars)
})

test_that("fit_model errors clearly on unknown method", {
  skip_if_not_installed("cmdstanr")
  expect_error(
    fit_model(stan_data = list(), stan_model_path = "ignored", method = "foo"),
    regexp = "should be one of"
  )
})

# ── check_stan_diagnostics: post-sample gate (audit 2026-05-15 §I) ────────────

# Build a fake CmdStanMCMC-like object that responds to $diagnostic_summary,
# $metadata, and $summary. The helper only calls these three methods.
.fake_fit <- function(num_divergent = c(0L, 0L, 0L, 0L),
                      iter_sampling = 1000L,
                      rhat = 1.01, ess_bulk = 500, ess_tail = 500,
                      variable = "mu",
                      num_max_treedepth = c(0L, 0L, 0L, 0L),
                      ebfmi = c(1, 1, 1, 1)) {
  list(
    diagnostic_summary = function(quiet = TRUE) {
      list(
        num_divergent = num_divergent,
        num_max_treedepth = num_max_treedepth,
        ebfmi = ebfmi
      )
    },
    metadata = function() list(iter_sampling = iter_sampling),
    summary = function(...) {
      tibble::tibble(
        variable = variable, rhat = rhat,
        ess_bulk = ess_bulk, ess_tail = ess_tail
      )
    }
  )
}

test_that("check_stan_diagnostics passes a clean fit silently", {
  fit <- .fake_fit()
  expect_invisible(check_stan_diagnostics(fit))
})

test_that("check_stan_diagnostics stops on divergent fraction above threshold", {
  # 50 divergent per chain × 4 chains = 200 / 4000 = 5% > 1% default
  fit <- .fake_fit(num_divergent = rep(50L, 4L))
  expect_error(
    check_stan_diagnostics(fit),
    "200 divergent transitions in 4000.*5\\.00%.*1\\.00%"
  )
})

test_that("check_stan_diagnostics stops on bad R-hat", {
  fit <- .fake_fit(rhat = 1.20, variable = "home_advantage")
  expect_error(
    check_stan_diagnostics(fit),
    "max R-hat 1.200 on parameter home_advantage exceeds 1.05"
  )
})

test_that("check_stan_diagnostics stops on low bulk ESS", {
  fit <- .fake_fit(ess_bulk = 50, variable = "tau")
  expect_error(
    check_stan_diagnostics(fit),
    "min bulk ESS 50 on parameter tau below 100"
  )
})

test_that("check_stan_diagnostics tolerates threshold overrides", {
  # 5% divergence — would fail at default but passes at relaxed threshold
  fit <- .fake_fit(num_divergent = rep(50L, 4L))
  expect_invisible(check_stan_diagnostics(fit, max_divergent_frac = 0.1))
})

test_that("check_stan_diagnostics survives diagnostic_summary failure", {
  # If cmdstanr's diagnostic_summary throws (older versions, partial fits),
  # the gate should fall through to R-hat / ESS checks rather than abort.
  fit <- list(
    diagnostic_summary = function(quiet = TRUE) stop("legacy fit"),
    metadata = function() list(iter_sampling = 1000L),
    summary = function(...) {
      tibble::tibble(variable = "mu", rhat = 1.01, ess_bulk = 500)
    }
  )
  expect_invisible(check_stan_diagnostics(fit))
})

test_that("check_stan_diagnostics returns the metric list invisibly", {
  m <- check_stan_diagnostics(.fake_fit())
  expect_equal(m$div_frac, 0)
  expect_equal(m$treedepth_frac, 0)
  expect_equal(m$max_rhat, 1.01)
  expect_equal(m$min_ess_bulk, 500)
  expect_equal(m$min_ess_tail, 500)
  expect_equal(m$min_ebfmi, 1)
})

test_that("check_stan_diagnostics stops on treedepth saturation above threshold", {
  # 300 per chain x 4 = 1200 / 4000 = 30% > 20% default
  fit <- .fake_fit(num_max_treedepth = rep(300L, 4L))
  expect_error(check_stan_diagnostics(fit), "treedepth")
})

test_that("check_stan_diagnostics stops on low E-BFMI", {
  fit <- .fake_fit(ebfmi = c(0.1, 0.9, 0.9, 0.9))
  expect_error(check_stan_diagnostics(fit), "E-BFMI")
})

test_that("check_stan_diagnostics stops on low tail ESS", {
  fit <- .fake_fit(ess_tail = 50, variable = "tau")
  expect_error(check_stan_diagnostics(fit), "tail ESS 50 on parameter tau")
})

test_that("persist_fit_diagnostics writes a readable one-row record", {
  root <- withr::local_tempdir()
  fit <- .fake_fit(num_divergent = rep(2L, 4L), ess_tail = 250)
  league <- list(sport = "basketball", country = "iceland")
  persist_fit_diagnostics(
    fit, league,
    sex = "male", fit_date = as.Date("2026-05-30"),
    n_obs = 120L, adapt_delta = 0.95, iter_sampling = 1000L, chains = 4L,
    root = root
  )
  d <- read_table("fit_diagnostics",
    filter = list(sport = "basketball", country = "iceland", sex = "male"),
    root = root
  )
  expect_equal(nrow(d), 1L)
  expect_equal(d$n_divergent, 8L)
  expect_equal(d$div_frac, 8 / 4000)
  expect_equal(d$min_ess_tail, 250)
  expect_equal(d$n_obs, 120L)
  expect_true(d$passed)
})

test_that("evaluate_stan_diagnostics is silent when clean and flags each fault", {
  clean <- list(
    div_frac = 0, n_divergent = 0L, total_iter = 4000L,
    treedepth_frac = 0, n_max_treedepth = 0L, min_ebfmi = 1,
    max_rhat = 1.0, min_ess_bulk = 500, min_ess_tail = 500,
    worst_rhat_var = "mu", worst_ess_bulk_var = "mu", worst_ess_tail_var = "mu"
  )
  expect_length(evaluate_stan_diagnostics(clean), 0L)

  bad <- clean
  bad$div_frac <- 0.05
  bad$n_divergent <- 200L
  bad$treedepth_frac <- 0.30
  bad$n_max_treedepth <- 1200L
  bad$min_ebfmi <- 0.1
  bad$max_rhat <- 1.2
  bad$min_ess_bulk <- 50
  bad$min_ess_tail <- 40
  expect_length(evaluate_stan_diagnostics(bad), 6L)
})
