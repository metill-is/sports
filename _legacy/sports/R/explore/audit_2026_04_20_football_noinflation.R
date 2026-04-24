#!/usr/bin/env Rscript
# Football Iceland male — formal loo::loo comparison of inflated-diagonal BVP
# vs no-inflation BVP. Mirrors the basketball/handball production-length
# comparison pattern.
#
# Motivation: 2026-04-20 audit (compare Iceland p ~ 0.02, England p ~ 0.014)
# suggests diagonal inflation is dead weight globally. This script does the
# formal elpd test.
#
# Outputs:
#   football/iceland/results/male/fit_inflated.rds
#   football/iceland/results/male/fit_noinflation.rds
#   football/iceland/results/male/compare_2026_04_20_noinflation.txt

suppressPackageStartupMessages({
  library(cmdstanr)
  library(posterior)
  library(loo)
  library(dplyr)
  library(here)
  library(withr)
})

stopifnot(basename(getwd()) %in% c("Sports", "Sports-audit"))
sports_dir <- getwd()

cat("--- Preparing Stan data (football/iceland male) ---\n")
# Mirror fit_football() in R/pipeline/step_fit.R:130
league_dir <- file.path(sports_dir, "football", "iceland")

stan_data <- with_dir(league_dir, {
  here::i_am(".here")
  env <- new.env(parent = globalenv())
  source(file.path(sports_dir, "R", "shared", "prep_data_football.R"),
    local = env
  )
  suppressMessages(env$prepare_football_data(sex = "male"))
})

cat(sprintf(
  "  K=%d  N=%d  N_rounds=%d  N_pred=%d\n",
  stan_data$K, stan_data$N, stan_data$N_rounds, stan_data$N_pred
))

results_dir <- file.path(league_dir, "results", "male")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
out_inflated <- file.path(results_dir, "fit_inflated.rds")
out_noinflation <- file.path(results_dir, "fit_noinflation.rds")
log_path <- file.path(results_dir, "compare_2026_04_20_noinflation.txt")

# Reset here() back to Sports/ root for Stan path resolution
here::i_am("run.R")

fit_variant <- function(name, stan_path) {
  cat(sprintf("\n--- %s: compiling + fitting %s ---\n", name, stan_path))
  mod <- cmdstan_model(stan_path)
  t0 <- Sys.time()
  fit <- mod$sample(
    data = stan_data,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 1000,
    iter_sampling = 1000,
    refresh = 200,
    seed = 20260420,
    init = 0,
    show_messages = TRUE,
    show_exceptions = TRUE
  )
  wall <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  cat(sprintf("  %s wall time: %.1f min\n", name, wall / 60))
  list(name = name, fit = fit, wall = wall)
}

res_inflated <- fit_variant(
  "inflated",
  "football/iceland/Stan/bivariate_poisson_inflated_diagonal_corrmodel.stan"
)
cat("\n--- Saving inflated fit ---\n")
res_inflated$fit$save_object(file = out_inflated)

res_noinflation <- fit_variant(
  "noinflation",
  "football/iceland/Stan/bivariate_poisson_no_inflation.stan"
)
cat("\n--- Saving no-inflation fit ---\n")
res_noinflation$fit$save_object(file = out_noinflation)

collect_diag <- function(r) {
  diag <- r$fit$diagnostic_summary(quiet = TRUE)
  summ <- r$fit$summary()
  list(
    wall = r$wall,
    divergences = sum(diag$num_divergent),
    max_treedepth = sum(diag$num_max_treedepth),
    max_rhat = max(summ$rhat, na.rm = TRUE),
    min_ess_bulk = min(summ$ess_bulk, na.rm = TRUE),
    min_ess_tail = min(summ$ess_tail, na.rm = TRUE),
    n_vars = length(r$fit$metadata()$variables)
  )
}
d_inflated <- collect_diag(res_inflated)
d_noinflation <- collect_diag(res_noinflation)

cat("\n--- Computing loo on inflated ---\n")
ll_inflated <- res_inflated$fit$draws(
  variables = "log_lik",
  format = "draws_matrix"
)
loo_inflated <- loo::loo(ll_inflated)
rm(ll_inflated)
gc()

cat("\n--- Computing loo on noinflation ---\n")
ll_noinflation <- res_noinflation$fit$draws(
  variables = "log_lik",
  format = "draws_matrix"
)
loo_noinflation <- loo::loo(ll_noinflation)
rm(ll_noinflation)
gc()

cat("\n--- loo comparison ---\n")
loo_comp <- loo::loo_compare(list(
  inflated = loo_inflated,
  noinflation = loo_noinflation
))
print(loo_comp)

cat("\n--- Key parameters ---\n")
key_vars_shared <- c(
  "mean_log_goals", "alpha_mu3", "beta_mu3_strength_diff",
  "scale_sigma_off", "scale_sigma_def",
  "home_advantage_off[1]", "cur_strength[1]"
)
key_vars_inflated_only <- c(
  "logit_p0", "beta_logit_p_strength_diff", "tie_alpha", "tie_beta"
)

print_summary <- function(r, vars) {
  for (v in vars) {
    d <- tryCatch(
      as.numeric(r$fit$draws(variables = v, format = "draws_matrix")),
      error = function(e) NULL
    )
    if (is.null(d)) {
      cat(sprintf("  %-30s  (missing)\n", v))
    } else {
      cat(sprintf(
        "  %-30s  mean %+8.4f  sd %.4f  q[5,50,95] %+.4f / %+.4f / %+.4f\n",
        v, mean(d), sd(d),
        quantile(d, 0.05), quantile(d, 0.5), quantile(d, 0.95)
      ))
    }
  }
}

cat("\n[inflated]\n")
print_summary(res_inflated, c(key_vars_shared, key_vars_inflated_only))
cat("\n[noinflation]\n")
print_summary(res_noinflation, key_vars_shared)

sink(log_path)
cat("# Football Iceland male — diagonal-inflation loo comparison\n")
cat(sprintf(
  "# Fitted %s, 4 chains x (1000 warmup + 1000 sampling), init=0\n",
  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
))
cat(sprintf(
  "# Data: K=%d, N=%d, N_rounds=%d\n",
  stan_data$K, stan_data$N, stan_data$N_rounds
))
cat("\n## Diagnostics\n\n")
cat(sprintf(
  "%-13s %-9s %-12s %-14s %-10s %-13s %-13s %s\n",
  "variant", "wall_min", "divergences", "max_treedepth",
  "max_rhat", "min_ess_bulk", "min_ess_tail", "n_vars"
))
for (pair in list(
  list("inflated", d_inflated),
  list("noinflation", d_noinflation)
)) {
  n <- pair[[1]]
  d <- pair[[2]]
  cat(sprintf(
    "%-13s %-9.1f %-12d %-14d %-10.4f %-13.0f %-13.0f %d\n",
    n, d$wall / 60, d$divergences, d$max_treedepth,
    d$max_rhat, d$min_ess_bulk, d$min_ess_tail, d$n_vars
  ))
}
cat("\n## loo::loo output\n\n")
cat("### inflated\n")
print(loo_inflated)
cat("\n### noinflation\n")
print(loo_noinflation)
cat("\n### loo_compare\n")
print(loo_comp)
cat("\n## Key parameters\n\n[inflated]\n")
print_summary(res_inflated, c(key_vars_shared, key_vars_inflated_only))
cat("\n[noinflation]\n")
print_summary(res_noinflation, key_vars_shared)
sink()

cat(sprintf("\nDone. Log: %s\n", log_path))
cat(sprintf(
  "Wall: inflated %.1f min, noinflation %.1f min, total %.1f min\n",
  d_inflated$wall / 60,
  d_noinflation$wall / 60,
  (d_inflated$wall + d_noinflation$wall) / 60
))
