test_that("all three league Stan models compile", {
  skip_on_cran()
  skip_if_not_installed("cmdstanr")
  skip_if(
    is.null(tryCatch(cmdstanr::cmdstan_path(), error = function(e) NULL)),
    "cmdstan not installed"
  )

  leagues <- load_leagues()
  for (k in names(leagues)) {
    stan_path <- here::here("Stan", leagues[[k]]$stan_model)
    expect_true(file.exists(stan_path),
      info = paste0("missing Stan file for ", k, ": ", stan_path)
    )

    mod <- tryCatch(
      cmdstanr::cmdstan_model(stan_path, compile = TRUE, quiet = TRUE),
      error = function(e) {
        fail(paste0("compile failed for ", k, ": ", conditionMessage(e)))
        NULL
      }
    )
    expect_true(
      inherits(mod, "CmdStanModel"),
      info = paste0("compiled model for ", k, " should be CmdStanModel")
    )
  }
})
