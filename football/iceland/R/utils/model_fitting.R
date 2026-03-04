#### Model Fitting Utilities ####
library(tidyverse)
library(scales)
library(cmdstanr)
library(posterior)
library(metill)
library(geomtextpath)
library(ggtext)
library(glue)
library(here)
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
  sex,
  model_name = "bivariate_poisson_inflated_diagonal_corrmodel.stan",
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  init = 0
) {
  # Validate input
  if (!sex %in% c("male", "female")) {
    stop("Sex must be either 'male' or 'female'")
  }

  # Source the prep_data script for the specified sex
  source(here("R", sex, "prep_data.R"))

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
    init = init
  )

  # Save results
  results$save_object(
    file = here("results", sex, "fit.rds")
  )
}
