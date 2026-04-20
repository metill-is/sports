#!/usr/bin/env Rscript
#
# Quick diagnostic pass on the post-swap production fit.rds for
# football_iceland male. Compares against the audit's fit_noinflation.rds
# (same model, but 2000+2000 iter vs production's 1000+1000).
#
# Writes to football/iceland/results/male/swap_2026_04_20_production.txt.

suppressPackageStartupMessages({
  library(cmdstanr)
  library(dplyr)
  library(tidyr)
})

Sys.setlocale("LC_ALL", "is_IS.UTF-8")

results_dir <- here::here("football", "iceland", "results", "male")
prod_path <- file.path(results_dir, "fit.rds")
audit_path <- file.path(results_dir, "fit_noinflation.rds")
out_path <- file.path(results_dir, "swap_2026_04_20_production.txt")

sink(out_path, split = TRUE)

cat("Football Iceland — production refit diagnostics (post-swap)\n")
cat("Date: 2026-04-20\n")
cat("Commit: ", system("git rev-parse --short HEAD", intern = TRUE), "\n")
cat(strrep("-", 72), "\n\n")

summarise_fit <- function(path, label) {
  cat(sprintf("=== %s (%s) ===\n", label, basename(path)))
  fit <- readRDS(path)

  diag <- fit$diagnostic_summary(quiet = TRUE)
  n_div <- sum(diag$num_divergent)
  n_iter_per_chain <- fit$metadata()$iter_sampling
  n_chains <- length(diag$num_divergent)
  total_transitions <- n_iter_per_chain * n_chains
  cat(sprintf(
    "  chains: %d, sampling iter/chain: %d, total transitions: %d\n",
    n_chains, n_iter_per_chain, total_transitions
  ))
  cat(sprintf(
    "  divergences: %d (%.2f%%)\n",
    n_div, 100 * n_div / total_transitions
  ))
  cat(sprintf(
    "  E-BFMI per chain: %s\n",
    paste(sprintf("%.3f", diag$ebfmi), collapse = ", ")
  ))
  cat(sprintf(
    "  num_max_treedepth: %s\n",
    paste(diag$num_max_treedepth, collapse = ", ")
  ))

  summ <- fit$summary()
  summ_finite <- summ |> dplyr::filter(!is.na(rhat), is.finite(rhat))
  cat(sprintf(
    "  max Rhat: %.4f (on %s)\n",
    max(summ_finite$rhat),
    summ_finite$variable[which.max(summ_finite$rhat)]
  ))
  cat(sprintf(
    "  Rhat > 1.01: %d / %d params\n",
    sum(summ_finite$rhat > 1.01), nrow(summ_finite)
  ))
  cat(sprintf(
    "  min ESS_bulk: %.0f (on %s)\n",
    min(summ_finite$ess_bulk, na.rm = TRUE),
    summ_finite$variable[which.min(summ_finite$ess_bulk)]
  ))
  cat(sprintf(
    "  min ESS_tail: %.0f (on %s)\n",
    min(summ_finite$ess_tail, na.rm = TRUE),
    summ_finite$variable[which.min(summ_finite$ess_tail)]
  ))

  # Key posterior summaries
  cat("\n  Key posterior means (±MC SE):\n")
  keys <- c(
    "mean_goals0", "home_advantage_off", "home_advantage_def",
    "alpha_rho", "alpha_mu3", "beta_mu3_strength_diff"
  )
  summ_keys <- summ |> dplyr::filter(variable %in% keys)
  for (i in seq_len(nrow(summ_keys))) {
    r <- summ_keys[i, ]
    cat(sprintf(
      "    %-30s %8.4f  sd=%.4f  ess_bulk=%.0f  rhat=%.4f\n",
      r$variable, r$mean, r$sd, r$ess_bulk, r$rhat
    ))
  }

  cat("\n")
  invisible(fit)
}

summarise_fit(prod_path, "PRODUCTION REFIT (1000 warmup + 1000 sampling)")
if (file.exists(audit_path)) {
  summarise_fit(audit_path, "AUDIT BASELINE (2000 warmup + 2000 sampling)")
}

cat("Notes:\n")
cat("- Gates: max Rhat < 1.01 (strict) or 1.02 (production-reasonable for tail artifacts),\n")
cat("  min ESS_bulk > 400, min ESS_tail > 400.\n")
cat("- Divergence rate > 2%% is a sign that warmup adaptation is insufficient;\n")
cat("  typical remedy at production iter is to bump iter_warmup.\n")

sink()
cat(sprintf("\nWrote %s\n", out_path))
