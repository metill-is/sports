#!/usr/bin/env Rscript
# Production-length fit of tier1 baseline vs scalarsigma variant (handball).
#
# Mirrors audit_2026_04_20_production_fit_basketball.R for handball/iceland.
# Handball uses single rho (not match-dependent like basketball); otherwise
# the two models share structure.
#
# Outputs:
#   handball/iceland/results/male/fit_tier1.rds
#   handball/iceland/results/male/fit_scalarsigma.rds
#   handball/iceland/results/male/compare_2026_04_20.txt

suppressPackageStartupMessages({
  library(cmdstanr)
  library(posterior)
  library(loo)
  library(dplyr)
  library(here)
})

stopifnot(basename(getwd()) == "Sports-audit")

cat("--- Preparing Stan data ---\n")
box::use(
  conf = R / config / handball_iceland[get_config],
  prep = R / shared / prep_data[prepare_data]
)
cfg <- conf$get_config()
stan_data <- prep$prepare_data(cfg, sex = "male", end_date = Sys.Date())
cat(sprintf(
  "  K=%d  N=%d  N_rounds=%d  N_seasons=%d  N_pred=%d\n",
  stan_data$K, stan_data$N, stan_data$N_rounds,
  stan_data$N_seasons, stan_data$N_pred
))

results_dir <- here("handball", "iceland", "results", "male")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
out_tier1 <- file.path(results_dir, "fit_tier1.rds")
out_scalar <- file.path(results_dir, "fit_scalarsigma.rds")
log_path <- file.path(results_dir, "compare_2026_04_20.txt")

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
    refresh = 100,
    seed = 20260420,
    init = 0,
    show_messages = TRUE,
    show_exceptions = TRUE
  )
  wall <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  cat(sprintf("  %s wall time: %.1f min\n", name, wall / 60))
  list(name = name, fit = fit, wall = wall)
}

res_tier1 <- fit_variant(
  "tier1",
  "handball/iceland/Stan/2d_student_t.stan"
)
cat("\n--- Saving tier1 fit ---\n")
res_tier1$fit$save_object(file = out_tier1)

res_scalar <- fit_variant(
  "scalarsig",
  "handball/iceland/Stan/2d_student_t_scalarsigma.stan"
)
cat("\n--- Saving scalarsigma fit ---\n")
res_scalar$fit$save_object(file = out_scalar)

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
d_tier1 <- collect_diag(res_tier1)
d_scalar <- collect_diag(res_scalar)

cat("\n--- Computing loo::loo on tier1 ---\n")
ll_tier1 <- res_tier1$fit$draws(variables = "log_lik", format = "draws_matrix")
loo_tier1 <- loo::loo(ll_tier1)
rm(ll_tier1)
gc()

cat("\n--- Computing loo::loo on scalarsigma ---\n")
ll_scalar <- res_scalar$fit$draws(variables = "log_lik", format = "draws_matrix")
loo_scalar <- loo::loo(ll_scalar)
rm(ll_scalar)
gc()

cat("\n--- loo comparison ---\n")
loo_comp <- loo::loo_compare(list(tier1 = loo_tier1, scalarsig = loo_scalar))
print(loo_comp)

cat("\n--- Key parameters: posterior mean (SD) per variant ---\n")
# Handball uses single rho (not alpha_rho/beta_rho)
key_vars <- c(
  "mean_goals0", "nu", "rho",
  "home_advantage_off[1]", "cur_strength[1]"
)
summary_rows <- list()
for (v in key_vars) {
  row <- list(variable = v)
  for (r in list(res_tier1, res_scalar)) {
    draws <- tryCatch(
      r$fit$draws(variables = v, format = "draws_matrix"),
      error = function(e) NULL
    )
    if (is.null(draws)) {
      row[[r$name]] <- "(missing)"
    } else {
      x <- as.numeric(draws)
      row[[r$name]] <- sprintf("%+8.3f (sd %.3f)", mean(x), sd(x))
    }
  }
  summary_rows[[v]] <- row
}
for (row in summary_rows) {
  cat(sprintf(
    "  %-25s  tier1 %-24s  scalarsig %-24s\n",
    row$variable, row$tier1, row$scalarsig
  ))
}

sink(log_path)
cat("# Handball Iceland male — production-length fit comparison\n")
cat(sprintf("# Fitted 2026-04-20, 4 chains x (1000 warmup + 1000 sampling)\n"))
cat(sprintf(
  "# Data: K=%d, N=%d, N_rounds=%d, N_seasons=%d\n",
  stan_data$K, stan_data$N, stan_data$N_rounds, stan_data$N_seasons
))
cat("\n## Diagnostics\n\n")
cat(sprintf(
  "%-12s %-9s %-12s %-14s %-10s %-13s %-13s %s\n",
  "variant", "wall_min", "divergences", "max_treedepth",
  "max_rhat", "min_ess_bulk", "min_ess_tail", "n_vars"
))
for (pair in list(list("tier1", d_tier1), list("scalarsig", d_scalar))) {
  n <- pair[[1]]
  d <- pair[[2]]
  cat(sprintf(
    "%-12s %-9.1f %-12d %-14d %-10.4f %-13.0f %-13.0f %d\n",
    n, d$wall / 60, d$divergences, d$max_treedepth,
    d$max_rhat, d$min_ess_bulk, d$min_ess_tail, d$n_vars
  ))
}
cat("\n## loo::loo output\n\n")
cat("### tier1\n")
print(loo_tier1)
cat("\n### scalarsigma\n")
print(loo_scalar)
cat("\n### loo_compare\n")
print(loo_comp)
cat("\n## Key parameters\n\n")
for (row in summary_rows) {
  cat(sprintf(
    "  %-25s  tier1 %-24s  scalarsig %-24s\n",
    row$variable, row$tier1, row$scalarsig
  ))
}
sink()

cat(sprintf("\nDone. Log written to %s\n", log_path))
cat(sprintf(
  "Wall: tier1 %.1f min, scalarsig %.1f min, total %.1f min\n",
  d_tier1$wall / 60, d_scalar$wall / 60, (d_tier1$wall + d_scalar$wall) / 60
))
