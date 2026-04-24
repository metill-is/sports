#!/usr/bin/env Rscript
# Quick check: is the football diagonal-inflation "dead weight" finding
# (Iceland male: fitted p ~ 0.02) Iceland-specific, or does it hold for
# England too? If England also fits p ~ 0, the Karlis-Ntzoufras diagonal
# inflation is dead everywhere and can be removed from the shared Stan file.
#
# Mirrors audit_2026_04_19_posteriors.R §3 but on football/england/male.
#
# Outputs to stdout only.

suppressPackageStartupMessages({
  library(posterior)
  library(dplyr)
  library(here)
})

stopifnot(basename(getwd()) %in% c("Sports", "Sports-audit"))

path_eng <- here("football", "england", "results", "male", "fit.rds")
cat(sprintf("Loading %s ...\n", path_eng))
t0 <- Sys.time()
fit <- readRDS(path_eng)
cat(sprintf("  loaded in %.1fs\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

draws <- fit$draws(
  variables = c(
    "alpha_mu3", "beta_mu3_strength_diff",
    "logit_p0", "beta_logit_p_strength_diff",
    "tie_alpha", "tie_beta",
    "mean_log_goals"
  ),
  format = "draws_matrix"
)

alpha <- as.numeric(draws[, "alpha_mu3"])
beta_sd <- as.numeric(draws[, "beta_mu3_strength_diff"])
logit_p0 <- as.numeric(draws[, "logit_p0"])
beta_p <- as.numeric(draws[, "beta_logit_p_strength_diff"])
mlg <- mean(as.numeric(draws[, "mean_log_goals"]))

cat("\n================================================\n")
cat("Football England (male) — BVP rho + diagonal-inflation\n")
cat("================================================\n\n")

cat(sprintf(
  "  alpha_mu3                  posterior mean = %+.3f  (sd %.3f)\n",
  mean(alpha), sd(alpha)
))
cat(sprintf(
  "  beta_mu3_strength_diff     posterior mean = %+.3f  (sd %.3f)\n",
  mean(beta_sd), sd(beta_sd)
))
cat(sprintf(
  "  logit_p0                   posterior mean = %+.3f  (sd %.3f)\n",
  mean(logit_p0), sd(logit_p0)
))
cat(sprintf(
  "  beta_logit_p_strength_diff posterior mean = %+.3f  (sd %.3f)\n",
  mean(beta_p), sd(beta_p)
))
cat(sprintf(
  "  mean_log_goals             posterior mean = %+.3f  (lambda = %.2f)\n",
  mlg, exp(mlg)
))

cat("\n--- Construction rho at various strength_diff ---\n")
for (sdf in c(0, 0.3, 0.7, 1.5)) {
  logit_rho <- alpha + beta_sd * sdf
  rho_c <- plogis(logit_rho)
  cat(sprintf(
    "  strength_diff=%.1f  construction rho = %.4f (post mean)\n",
    sdf, mean(rho_c)
  ))
}

cat("\n--- Diagonal inflation weight p at various strength_diff ---\n")
cat("  (This is the headline test. If p ~ 0 everywhere, inflation is dead weight.)\n")
for (sdf in c(0, 0.3, 0.7, 1.5)) {
  logit_p <- logit_p0 + beta_p * sdf
  p <- plogis(logit_p)
  cat(sprintf(
    "  strength_diff=%.1f  p = %.4f (post mean)\n",
    sdf, mean(p)
  ))
}

cat("\n--- For comparison: Iceland (male) values from audit_2026_04_19 ---\n")
cat("  alpha_mu3 = -3.46, beta_mu3_strength_diff = -1.11\n")
cat("  logit_p0  = -3.97 -> p ~ 0.02 at typical match\n")
cat("  observed Iceland draw rate 0.188 (below indep-Poisson 0.211 -> NEGATIVE deficit)\n")

cat("\n--- Observed draw rate in England data ---\n")
# Try to read raw data for direct comparison
tryCatch(
  {
    data_paths <- Sys.glob(here("football", "england", "data", "male", "*", "*", "results.csv"))
    cat(sprintf("  (%d per-season CSVs found)\n", length(data_paths)))
    if (length(data_paths) > 0) {
      all_data <- do.call(rbind, lapply(data_paths, function(p) {
        suppressMessages(read.csv(p))
      }))
      obs_draws <- mean(all_data$home_goals == all_data$away_goals, na.rm = TRUE)
      obs_mean <- mean((all_data$home_goals + all_data$away_goals) / 2, na.rm = TRUE)
      # indep-Poisson implied draw rate at Poisson(obs_mean) for both teams
      poisson_draw <- sum(dpois(0:30, obs_mean) * dpois(0:30, obs_mean))
      cat(sprintf("  N matches = %d\n", nrow(all_data)))
      cat(sprintf("  mean goals per team = %.3f\n", obs_mean))
      cat(sprintf("  observed draw rate = %.3f\n", obs_draws))
      cat(sprintf("  indep-Poisson draw rate at mean = %.3f\n", poisson_draw))
      cat(sprintf(
        "  draw deficit = %+.3f  (positive -> more draws than Poisson, KN motivation)\n",
        obs_draws - poisson_draw
      ))
    } else {
      cat("  (no per-season results.csv files found; inspect manually)\n")
    }
  },
  error = function(e) {
    cat(sprintf("  (couldn't read England data: %s)\n", conditionMessage(e)))
  }
)

cat("\nDone.\n")
