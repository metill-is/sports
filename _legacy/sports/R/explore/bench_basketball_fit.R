#!/usr/bin/env Rscript
# Short-fit benchmark — compares three Stan models for basketball male Iceland:
#   baseline   : pre-audit file (original mean_goals0 prior, per-team sigma)
#   tier1      : re-centred mean_goals0 prior only, per-team sigma unchanged
#   scalarsig  : re-centred prior + scalar sigma replaces per-team sigma_team
#
# Uses 2 chains, 300 warmup + 300 sampling. Reports wall time, divergences,
# max Rhat, min bulk ESS, posterior summaries of shared parameters.

suppressPackageStartupMessages({
  library(cmdstanr)
  library(posterior)
  library(dplyr)
  library(here)
})

stopifnot(basename(getwd()) == "Sports")

# -------- Prep data via box entry points (same as step_fit.R pattern) ----
cat("--- Preparing Stan data via shared pipeline ---\n")
box::use(
  conf = R / config / basketball_iceland[get_config],
  prep = R / shared / prep_data[prepare_data]
)
cfg <- conf$get_config()
stan_data <- prep$prepare_data(cfg, sex = "male", end_date = Sys.Date())
cat(sprintf(
  "  K=%d  N=%d  N_rounds=%d  N_seasons=%d  N_pred=%d\n",
  stan_data$K, stan_data$N, stan_data$N_rounds,
  stan_data$N_seasons, stan_data$N_pred
))

# -------- Fit each variant ----------------------------------------------
fit_variant <- function(name, path) {
  cat(sprintf("\n--- %s: %s ---\n", name, path))
  mod <- cmdstan_model(path)
  t0 <- Sys.time()
  fit <- mod$sample(
    data = stan_data,
    chains = 2,
    parallel_chains = 2,
    iter_warmup = 300,
    iter_sampling = 300,
    refresh = 0,
    seed = 20260419,
    init = 0, # matches production — random init can make rho near ±1
    show_messages = FALSE,
    show_exceptions = FALSE
  )
  wall <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  cat(sprintf("  wall time: %.1fs\n", wall))
  list(name = name, fit = fit, wall = wall)
}

results <- list(
  baseline  = fit_variant("baseline", "/tmp/basketball_baseline.stan"),
  tier1     = fit_variant("tier1", "basketball/iceland/Stan/2d_student_t.stan"),
  scalarsig = fit_variant("scalarsig", "basketball/iceland/Stan/2d_student_t_scalarsigma.stan")
)

# -------- Diagnostics summary -------------------------------------------
cat("\n============================================================\n")
cat("Diagnostics summary\n")
cat("============================================================\n")
cat(sprintf(
  "%-12s %-10s %-12s %-12s %-14s %s\n",
  "variant", "wall(s)", "divergences", "max_treed.", "max_rhat", "min_ess_bulk"
))

for (r in results) {
  diag <- r$fit$diagnostic_summary(quiet = TRUE)
  summ_all <- r$fit$summary()
  mx_rhat <- max(summ_all$rhat, na.rm = TRUE)
  mn_ess <- min(summ_all$ess_bulk, na.rm = TRUE)
  cat(sprintf(
    "%-12s %-10.1f %-12d %-12d %-14.3f %.0f\n",
    r$name, r$wall,
    sum(diag$num_divergent),
    sum(diag$num_max_treedepth),
    mx_rhat, mn_ess
  ))
}

# -------- Key parameter posterior means ---------------------------------
cat("\n--- Key parameters — posterior mean (SD) per variant ---\n")
key_vars <- c(
  "mean_goals0", "nu", "alpha_rho",
  "home_advantage_off[1]", "cur_strength[1]"
)
for (v in key_vars) {
  cat(sprintf("\n  %s\n", v))
  for (r in results) {
    draws <- tryCatch(
      r$fit$draws(variables = v, format = "draws_matrix"),
      error = function(e) NULL
    )
    if (is.null(draws)) {
      cat(sprintf("    %-11s  (not in variant)\n", r$name))
    } else {
      x <- as.numeric(draws)
      cat(sprintf("    %-11s  %+8.3f  (sd %.3f)\n", r$name, mean(x), sd(x)))
    }
  }
}

# -------- Total variable count ------------------------------------------
cat("\n--- Total draws variable count ---\n")
for (r in results) {
  n <- length(r$fit$metadata()$variables)
  cat(sprintf("  %-11s  %d variables in output\n", r$name, n))
}

cat("\nDone.\n")
