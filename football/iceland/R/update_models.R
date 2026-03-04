source(here::here("R", "utils", "model_fitting.R"))

fit_football_model(
  "male",
  model_name = "bivariate_poisson_inflated_diagonal_corrmodel.stan"
)
fit_football_model(
  "female",
  model_name = "bivariate_poisson_inflated_diagonal_corrmodel.stan"
)
