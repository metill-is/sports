# Data
source(here::here("R", "male", "update_data.R"))
source(here::here("R", "female", "update_data.R"))

# Models
source(here::here("R", "utils", "model_fitting.R"))

fit_football_model(
  "male",
  model_name = "bivariate_poisson_inflated_diagonal_corrmodel.stan"
)
fit_football_model(
  "female",
  model_name = "bivariate_poisson_inflated_diagonal_corrmodel.stan"
)

# Results
source(here::here("R", "male", "get_model_results.R"))
source(here::here("R", "female", "get_model_results.R"))
