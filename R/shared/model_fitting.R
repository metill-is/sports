#### Model Fitting Utilities ####
box::use(
  cmdstanr[cmdstan_model],
  here[here],
  metill[theme_metill],
  ggplot2[theme_set],
  R / shared / prep_data[prepare_data]
)

theme_set(theme_metill())
Sys.setlocale("LC_ALL", "is_IS.UTF-8")

#' Fit model for specified sport and sex
#'
#' @param config Config list from sport-specific config file
#' @param sex Character string, either "male" or "female"
#' @param end_date Date for filtering data
#' @param chains Number of MCMC chains (default: 4)
#' @param parallel_chains Number of parallel chains (default: 4)
#' @param iter_warmup Number of warmup iterations (default: 1000)
#' @param iter_sampling Number of sampling iterations (default: 1000)
#' @param init Initial values (default: 0)
#' @param refresh Refresh interval for progress (default: 100)
#' @param show_stan_output Logical. If FALSE (default), suppresses Stan's
#'   per-iteration output and informational messages. Set TRUE for debugging.
#'
#' @export
fit_model <- function(
  config,
  sex = "male",
  end_date = Sys.Date(),
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  init = 0,
  refresh = 100,
  show_stan_output = FALSE
) {
  if (!sex %in% c("male", "female")) {
    stop("Sex must be either 'male' or 'female'")
  }

  sport_dir <- config$sport_dir
  model_name <- config$stan_model

  # Prepare data (suppress readr column messages)
  stan_data <- suppressMessages(prepare_data(config, sex, end_date))

  # Compile model
  model <- cmdstan_model(
    here(sport_dir, "Stan", model_name)
  )

  # Fit model
  if (show_stan_output) {
    results <- model$sample(
      data = stan_data,
      chains = chains,
      parallel_chains = parallel_chains,
      iter_warmup = iter_warmup,
      iter_sampling = iter_sampling,
      init = init,
      refresh = refresh
    )
  } else {
    results <- model$sample(
      data = stan_data,
      chains = chains,
      parallel_chains = parallel_chains,
      iter_warmup = iter_warmup,
      iter_sampling = iter_sampling,
      init = init,
      refresh = 0,
      show_messages = FALSE,
      show_exceptions = FALSE
    )
  }

  # Save results
  results$save_object(
    file = here(sport_dir, "results", sex, end_date, "fit.rds")
  )
}
