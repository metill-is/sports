test_that("2d_gaussian_sd.stan compiles", {
  skip_on_ci()
  skip_if_not(nzchar(Sys.getenv("SPORTS_TEST_STAN")), "set SPORTS_TEST_STAN=1 to run Stan compile")
  stan_path <- here::here("Stan", "football_iceland", "2d_gaussian_sd.stan")
  expect_true(file.exists(stan_path))
  mod <- cmdstanr::cmdstan_model(stan_path)
  expect_s3_class(mod, "CmdStanModel")
})

test_that("bt_wf_sd_league overrides only the stan_model", {
  base <- load_leagues()[["football_iceland"]]
  lg <- bt_wf_sd_league()
  expect_equal(lg$stan_model, "football_iceland/2d_gaussian_sd.stan")
  expect_equal(lg$sport, base$sport)
  expect_equal(lg$country, base$country)
})

test_that("bt_wf_sd_decide fits the (S,D) model and returns candidates", {
  captured <- new.env()
  testthat::local_mocked_bindings(
    fit_league = function(league, sex, ...) {
      captured$stan_model <- league$stan_model
      captured$sex <- sex
      invisible(NULL)
    },
    decide_league = function(...) tibble::tibble(stage = "kept", p = 0.5, odds = 2.0)
  )
  fn <- bt_wf_sd_decide()
  out <- fn(root = withr::local_tempdir(), run_date = as.Date("2026-05-15"), sex = "male")
  expect_equal(captured$stan_model, "football_iceland/2d_gaussian_sd.stan")
  expect_equal(captured$sex, "male")
  expect_true(all(c("p", "odds") %in% names(out)))
})

test_that("wf_select_decide_fn picks the right closure", {
  expect_identical(wf_select_decide_fn("bvp", reuse = FALSE), bt_wf_default_decide)
  expect_true(is.function(wf_select_decide_fn("bvp", reuse = TRUE)))
  expect_true(is.function(wf_select_decide_fn("sd", reuse = FALSE)))
  expect_error(wf_select_decide_fn("sd", reuse = TRUE), "reuse")
})
