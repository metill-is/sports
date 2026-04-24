#### Model Fitting Utilities ####
box::use(
  cmdstanr[cmdstan_model],
  here[here],
  metill[theme_metill],
  ggplot2[theme_set],
  R / common / prep_data[prepare_football_data]
)

theme_set(theme_metill())
Sys.setlocale("LC_ALL", "is_IS.UTF-8")

#' Fit football model for specified sex
#'
#' @param sex Character string, either "male" or "female"
#' @param chains Number of MCMC chains (default: 4)
#' @param iter_warmup Number of warmup iterations (default: 1000)
#' @param iter_sampling Number of sampling iterations (default: 1000)
#' @param parallel_chains Number of parallel chains (default: 4)
#' @param init Initial values (default: 0)
#'
#' @return Fitted cmdstanr model object
#'
#' @examples
#' fit_football_model("male")
#' fit_football_model("female")
fit_football_model <- function(
  sex = "male",
  model_name = "2d_student_t.stan",
  end_date = Sys.Date(),
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  init = 0,
  refresh = 100
) {
  # Validate input
  if (!sex %in% c("male", "female")) {
    stop("Sex must be either 'male' or 'female'")
  }

  # Prepare data for the specified sex
  stan_data <- prepare_football_data(sex, end_date)

  # Compile model
  model <- cmdstan_model(
    here("Stan", model_name)
  )

  # Fit model
  results <- model$sample(
    data = stan_data,
    chains = chains,
    parallel_chains = parallel_chains,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    init = init,
    refresh = refresh
  )

  # Save results
  results$save_object(
    file = here("results", sex, end_date, "fit.rds")
  )
}
