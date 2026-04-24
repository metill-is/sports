#!/usr/bin/env Rscript
#
# Student-t football audit — Phase 1 orchestrator.
#
# Fits 2d_student_t_direct_SD.stan at fixed seed 20260420 on Iceland male
# data, reuses the already-saved fit_noinflation.rds (BVP no-inflation
# at the same seed), then runs formal loo_compare + diagnostics + a
# first-cut draw-rate calibration.
#
# Outputs:
#   football/iceland/results/male/audits/student_t/
#     fit_direct_SD.rds
#     compare_phase1.txt
#
# Usage (from Sports/ root):
#   Rscript R/backtest/student_t_audit/run_phase1.R
#
# Notes:
#   - Uses `bvp_no_inflation` baseline already fitted tonight at the same
#     seed (football/iceland/results/male/fit_noinflation.rds, 1.1 GB).
#   - Does NOT refit BVP — we have a clean reference fit ready.
#   - The LOO cross-class caveat (log density vs log probability) is noted
#     in compare_phase1.txt.

suppressPackageStartupMessages({
  library(cmdstanr)
  library(posterior)
  library(loo)
  library(here)
})

Sys.setlocale("LC_ALL", "is_IS.UTF-8")

stopifnot(basename(getwd()) %in% c("Sports", "Sports-student-t-audit"))
sports_dir <- getwd()
here::i_am("run.R")

source(file.path(sports_dir, "R", "backtest", "student_t_audit", "prep.R"))
source(file.path(sports_dir, "R", "backtest", "student_t_audit", "fit_variant.R"))
source(file.path(sports_dir, "R", "backtest", "student_t_audit", "compare.R"))
source(file.path(sports_dir, "R", "backtest", "student_t_audit", "calibration.R"))

cat("========================================\n")
cat(" Student-t audit Phase 1 (Iceland male)\n")
cat("========================================\n")

# --- Prep -----------------------------------------------------------------

cat("\n[prep] Loading football/iceland male data...\n")
stan_data <- prepare_student_t_audit_data(sports_dir, "football/iceland", "male")
cat(sprintf(
  "  K=%d  N=%d  N_rounds=%d  N_pred=%d\n",
  stan_data$K, stan_data$N, stan_data$N_rounds, stan_data$N_pred
))

# --- Output dir -----------------------------------------------------------

results_dir <- file.path(sports_dir, "football", "iceland", "results", "male")
audit_dir <- file.path(results_dir, "audits", "student_t")
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)

direct_SD_rds <- file.path(audit_dir, "fit_direct_SD.rds")
bvp_rds <- file.path(results_dir, "fit_noinflation.rds") # already saved
compare_txt <- file.path(audit_dir, "compare_phase1.txt")

# --- Fit direct_SD --------------------------------------------------------

if (file.exists(direct_SD_rds)) {
  cat("\n[fit] Reusing existing direct_SD fit at\n  ", direct_SD_rds, "\n")
  fit_direct <- readRDS(direct_SD_rds)
  # Reconstruct the result list scaffold for our helpers.
  res_direct <- list(
    name = "direct_SD",
    stan_path = file.path("football/iceland/Stan/2d_student_t_direct_SD.stan"),
    fit = fit_direct,
    wall_secs = NA_real_,
    seed = 20260420
  )
} else {
  res_direct <- fit_variant(
    stan_path = file.path(
      sports_dir,
      "football", "iceland", "Stan", "2d_student_t_direct_SD.stan"
    ),
    stan_data = stan_data,
    label = "direct_SD"
  )
  res_direct$fit$save_object(file = direct_SD_rds)
  cat(sprintf("[fit] Saved direct_SD fit -> %s\n", direct_SD_rds))
}

# --- Load BVP baseline ----------------------------------------------------

stopifnot(file.exists(bvp_rds))
cat(sprintf("\n[fit] Loading BVP no-inflation baseline (%s)\n", basename(bvp_rds)))
fit_bvp <- readRDS(bvp_rds)
res_bvp <- list(
  name = "bvp_no_inflation",
  stan_path = file.path("football/iceland/Stan/bivariate_poisson_no_inflation.stan"),
  fit = fit_bvp,
  wall_secs = NA_real_,
  seed = 20260420
)

# --- Diagnostics ----------------------------------------------------------

cat("\n[diag] Collecting diagnostic snapshots...\n")
res_direct$diag <- collect_diagnostics(res_direct)
res_bvp$diag <- collect_diagnostics(res_bvp)

# --- Shared posteriors ----------------------------------------------------

# Variables present in BOTH models (same architectural layer: team-strength
# random walk, home advantage, random-walk volatility hyperparameters).
shared_vars <- c(
  "home_advantage_off[1]", "home_advantage_def[1]",
  "mean_sigma_off", "mean_sigma_def", "scale_sigma_off", "scale_sigma_def"
)
shared_df <- shared_param_summary(list(res_direct, res_bvp), shared_vars)

# Model-specific parameters.
direct_specific <- per_fit_param_summary(
  res_direct$fit,
  c(
    "mean_goals0", "delta_mean_goals", "sigma_mean_goals",
    "sigma_S", "sigma_D", "rho_SD", "nu"
  )
)
bvp_specific <- per_fit_param_summary(
  res_bvp$fit,
  c("mean_log_goals", "alpha_mu3", "beta_mu3_strength_diff")
)

# --- LOO compare ----------------------------------------------------------

cat("\n[loo] Running loo on both fits (cross-class caveat applies)...\n")
fits_looed <- add_loo(list(res_direct, res_bvp))
compare_mat <- loo_compare_fits(fits_looed)
cat("\n--- loo_compare table ---\n")
print(compare_mat)

# --- Calibration ----------------------------------------------------------

cat("\n[cal] Overall draw-rate estimates...\n")
obs <- list(
  rate = observed_draw_rate(stan_data$goals1, stan_data$goals2),
  n_draws = sum(stan_data$goals1 == stan_data$goals2),
  n_matches = stan_data$N
)
direct_SD_draw <- implied_draw_rate_direct_SD(
  fit = res_direct$fit, stan_data = stan_data, threshold = 0.5
)
cal_summary <- format_draw_calibration(obs, direct_SD_draw)
cat(cal_summary)

# --- Write compare_phase1.txt --------------------------------------------

cat(sprintf("\n[report] Writing %s\n", compare_txt))

# Caveat block to embed in the report
loo_caveat <- paste0(
  "NOTE on loo_compare interpretation:\n",
  "  direct_SD's log_lik is a LOG DENSITY (continuous Student-t at the\n",
  "  observed integer (S, D)); bvp_no_inflation's log_lik is a LOG\n",
  "  PROBABILITY (poisson_2d_log_lpmf at integer (Y_h, Y_a)). Comparing\n",
  "  via loo_compare uses an implicit cell-volume-of-1 convention.\n",
  "  Treat the elpd_diff as directional evidence only; a full\n",
  "  common-observable check (1x2/totals/handicap Brier + log-score)\n",
  "  is queued as Phase 3 work.\n"
)

# Build the extra-body with model-specific param sections appended.
extra_body <- paste0(
  cal_summary, "\n",
  loo_caveat, "\n\n",
  "=== direct_SD model-specific posteriors ===\n",
  paste(capture.output(print(direct_specific, row.names = FALSE, digits = 4)),
    collapse = "\n"
  ), "\n\n",
  "=== bvp_no_inflation model-specific posteriors ===\n",
  paste(capture.output(print(bvp_specific, row.names = FALSE, digits = 4)),
    collapse = "\n"
  ), "\n"
)

write_compare_report(
  fits = fits_looed,
  compare_mat = compare_mat,
  shared_params_df = shared_df,
  extra = extra_body,
  out_path = compare_txt,
  title = "Student-t audit Phase 1 — Iceland male"
)

cat("\n[report] Done.\n")
cat(sprintf("  -> %s\n", compare_txt))
cat(sprintf("  -> %s\n", direct_SD_rds))
