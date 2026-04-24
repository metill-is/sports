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
    show_progress = FALSE
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
    regexp = "method"
  )
})
