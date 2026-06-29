test_that("2d_gaussian_sd.stan compiles", {
  skip_on_ci()
  skip_if_not(nzchar(Sys.getenv("SPORTS_TEST_STAN")), "set SPORTS_TEST_STAN=1 to run Stan compile")
  stan_path <- here::here("Stan", "football_iceland", "2d_gaussian_sd.stan")
  expect_true(file.exists(stan_path))
  mod <- cmdstanr::cmdstan_model(stan_path)
  expect_s3_class(mod, "CmdStanModel")
})
