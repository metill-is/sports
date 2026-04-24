#### Packages ####
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

source(
  here("R", "female", "prep_data.R")
)

#### Compile and fit model ####
model <- cmdstan_model(
  here(
    "Stan",
    "bivariate_poisson_inflated_diagonal_corrmodel.stan"
  )
)

# Fit model
results <- model$sample(
  data = stan_data,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  init = 0
)


results$save_object(
  file = here("results", "female", "fit.rds")
)
