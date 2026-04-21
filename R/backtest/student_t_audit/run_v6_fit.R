#!/usr/bin/env Rscript
#
# Fit + score v6 (match-dep sigma, scalar rho) and compare to v3 + BVP.
#
# Gates (per design doc):
#   MUST: divergences < 1%, max Rhat < 1.01, min ESS > 400
#   Improvement: 1x2 log-score > v3 (-0.908) by > 0.2%, OR
#                totals mean log-score > v3 (-0.451) by > 0.3%
#   Stretch   : within 0.5% of BVP's 1x2 log-score (-0.898).

suppressPackageStartupMessages({
  library(cmdstanr)
  library(posterior)
  library(dplyr)
})
Sys.setlocale("LC_ALL", "is_IS.UTF-8")
stopifnot(basename(getwd()) %in% c("Sports", "Sports-student-t-audit"))
sports_dir <- getwd()
here::i_am("run.R")

source(file.path(sports_dir, "R", "backtest", "student_t_audit", "prep.R"))
source(file.path(sports_dir, "R", "backtest", "student_t_audit", "fit_variant.R"))
source(file.path(sports_dir, "R", "backtest", "student_t_audit", "outcome_calibration.R"))
source(file.path(sports_dir, "R", "backtest", "student_t_audit", "totals_calibration.R"))

cat("==============================================\n")
cat(" v6 (match-dep sigma, scalar rho) audit\n")
cat("==============================================\n")

# ---- Data ----------------------------------------------------------------
stan_data <- prepare_student_t_audit_data(sports_dir, "football/iceland", "male")
cat(sprintf(
  "  K=%d  N=%d  N_rounds=%d\n",
  stan_data$K, stan_data$N, stan_data$N_rounds
))

# ---- Fit -----------------------------------------------------------------
stan_path <- file.path(
  sports_dir, "football", "iceland", "Stan",
  "2d_student_t_SD_v6_matchdep_sigma.stan"
)
audit_dir <- file.path(
  sports_dir, "football", "iceland", "results", "male",
  "audits", "student_t"
)
fit_path <- file.path(audit_dir, "fit_student_t_v6_matchdep_sigma.rds")

res <- fit_variant(
  stan_path = stan_path,
  stan_data = stan_data,
  label = "v6_matchdep_sigma",
  seed = 20260420,
  iter_warmup = 1000,
  iter_sampling = 1000,
  chains = 4,
  parallel_chains = 4,
  adapt_delta = 0.95,
  init = 0,
  refresh = 200
)

res$fit$save_object(file = fit_path)
cat(sprintf("\n[save] %s\n", fit_path))

d <- collect_diagnostics(res)

# ---- 1x2 calibration (analytical from per-match mu_D_fit, sigma_D_fit) ---
# v6 emits per-match mu_D_fit, sigma_D_fit directly — mirrors v5's approach.
v6_outcome_probs <- function(fit, stan_data,
                             threshold = 0.5, n_draws = 200) {
  mu_D_mat <- fit$draws("mu_D_fit", format = "draws_matrix")
  sd_D_mat <- fit$draws("sigma_D_fit", format = "draws_matrix")
  nu_vec <- as.numeric(fit$draws("nu", format = "draws_matrix"))

  total_draws <- nrow(mu_D_mat)
  draw_idx <- round(seq(1, total_draws, length.out = min(n_draws, total_draws)))
  mu_D_mat <- mu_D_mat[draw_idx, , drop = FALSE]
  sd_D_mat <- sd_D_mat[draw_idx, , drop = FALSE]
  nu_vec <- nu_vec[draw_idx]

  N <- ncol(mu_D_mat)
  p_home <- p_draw <- p_away <- numeric(N)
  for (n in seq_len(N)) {
    z_up <- (threshold - mu_D_mat[, n]) / sd_D_mat[, n]
    z_dn <- (-threshold - mu_D_mat[, n]) / sd_D_mat[, n]
    p_home[n] <- mean(1 - pt(z_up, df = nu_vec))
    p_draw[n] <- mean(pt(z_up, df = nu_vec) - pt(z_dn, df = nu_vec))
    p_away[n] <- mean(pt(z_dn, df = nu_vec))
  }
  data.frame(home_win = p_home, draw = p_draw, away_win = p_away)
}

obs_g1 <- stan_data$goals1
obs_g2 <- stan_data$goals2

cat(sprintf("\n[v6] analytical Student-t 1x2 (per-match sigma_D)...\n"))
t0 <- Sys.time()
probs_v6 <- v6_outcome_probs(res$fit, stan_data, threshold = 0.5, n_draws = 200)
cat(sprintf("  wall: %.1f s\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
s_v6_1x2 <- score_outcome(probs_v6, obs_g1, obs_g2, "v6_matchdep_sigma")

# ---- Totals calibration (reuse direct_SD_totals_probs via total_goals_rep) ---
cat(sprintf("\n[v6] totals from total_goals_rep...\n"))
t0 <- Sys.time()
p_v6 <- direct_SD_totals_probs(res$fit, stan_data,
  lines = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), n_draws = 200
)
cat(sprintf("  wall: %.1f s\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
s_v6_totals <- score_totals(p_v6, obs_g1, obs_g2, "v6_matchdep_sigma")

# ---- Comparison rows (from existing artefacts, don't refit) --------------
v3_1x2_log_score <- -0.908
v3_totals_mean_log <- -0.451
bvp_1x2_log_score <- -0.898
bvp_totals_mean_log <- -0.441

# ---- Report --------------------------------------------------------------
out_path <- file.path(audit_dir, "compare_v6.txt")
con <- file(out_path, open = "wt")
on.exit(close(con), add = TRUE)
w <- function(...) cat(sprintf(...), file = con)

w("v6 (match-dep sigma, scalar rho) — Iceland male\n")
w("Date: %s\n", format(Sys.Date()))
w(
  "Commit: %s\n",
  tryCatch(system("git rev-parse --short HEAD", intern = TRUE),
    error = function(e) "(unknown)"
  )
)
w("%s\n\n", strrep("=", 72))

w("--- Diagnostics ---\n")
print_diagnostics(d, "v6_matchdep_sigma", con = con)
w("\n")

summ <- res$fit$summary(
  variables = c(
    "mean_goals0", "rho_SD", "nu",
    "alpha_S", "alpha_D",
    "beta_S_q", "beta_S_g", "beta_D_q", "beta_D_g"
  )
)
w("--- Posterior summaries (scalars + new coefficients) ---\n")
capture.output(print(summ, digits = 3), file = con, append = TRUE)

# --- Sign probabilities for the four betas ---
beta_draws <- res$fit$draws(
  variables = c("beta_S_q", "beta_S_g", "beta_D_q", "beta_D_g"),
  format = "draws_matrix"
)
w("\n--- Sign probabilities ---\n")
for (nm in colnames(beta_draws)) {
  p_pos <- mean(beta_draws[, nm] > 0)
  w("  %-12s: P(beta > 0) = %.3f\n", nm, p_pos)
}

# --- sigma_S / sigma_D per-match summary (to spot exp-link blowups) ---
sig_S_mat <- res$fit$draws("sigma_S_fit", format = "draws_matrix")
sig_D_mat <- res$fit$draws("sigma_D_fit", format = "draws_matrix")
sig_S_means <- colMeans(sig_S_mat)
sig_D_means <- colMeans(sig_D_mat)
w("\n--- sigma_S(n) / sigma_D(n) posterior-mean summary over matches ---\n")
w(
  "  sigma_S:  min %.3f   median %.3f   max %.3f   sd %.3f\n",
  min(sig_S_means), median(sig_S_means), max(sig_S_means), sd(sig_S_means)
)
w(
  "  sigma_D:  min %.3f   median %.3f   max %.3f   sd %.3f\n",
  min(sig_D_means), median(sig_D_means), max(sig_D_means), sd(sig_D_means)
)

w("\n--- 1x2 log-score comparison ---\n")
cmp_1x2 <- data.frame(
  model = c("v6_matchdep_sigma", "v3_free_nu (baseline)", "BVP_no_inflation (target)"),
  log_score = c(s_v6_1x2$log_score, v3_1x2_log_score, bvp_1x2_log_score),
  rel_v3 = c(s_v6_1x2$log_score - v3_1x2_log_score, 0, bvp_1x2_log_score - v3_1x2_log_score),
  rel_bvp = c(s_v6_1x2$log_score - bvp_1x2_log_score, v3_1x2_log_score - bvp_1x2_log_score, 0)
)
capture.output(print(cmp_1x2, row.names = FALSE, digits = 5),
  file = con, append = TRUE
)

w("\n--- 1x2 detailed scores (v6) ---\n")
print_outcome_scores(s_v6_1x2, con)

w("\n--- Totals log-score comparison ---\n")
cmp_tot <- data.frame(
  model = c("v6_matchdep_sigma", "v3_free_nu (baseline)", "BVP_no_inflation (target)"),
  mean_log = c(
    attr(s_v6_totals, "mean_log_score"),
    v3_totals_mean_log,
    bvp_totals_mean_log
  ),
  rel_v3 = c(
    attr(s_v6_totals, "mean_log_score") - v3_totals_mean_log,
    0,
    bvp_totals_mean_log - v3_totals_mean_log
  ),
  rel_bvp = c(
    attr(s_v6_totals, "mean_log_score") - bvp_totals_mean_log,
    v3_totals_mean_log - bvp_totals_mean_log,
    0
  )
)
capture.output(print(cmp_tot, row.names = FALSE, digits = 5),
  file = con, append = TRUE
)

w("\n--- Totals per-line (v6) ---\n")
print_totals_scores(s_v6_totals, label = "v6_matchdep_sigma", con = con)

# ---- Gate assessment ----
w("\n--- Gate assessment ---\n")
gate_div <- d$div_rate < 0.01
gate_rhat <- d$max_rhat < 1.01
gate_ess <- d$min_ess_bulk > 400
improvement_1x2 <- s_v6_1x2$log_score - v3_1x2_log_score
improvement_totals <- attr(s_v6_totals, "mean_log_score") - v3_totals_mean_log
w(
  "  MUST: divergences %.2f%% < 1%%:    %s\n",
  100 * d$div_rate, if (gate_div) "PASS" else "FAIL"
)
w(
  "  MUST: max Rhat %.4f < 1.01:       %s\n",
  d$max_rhat, if (gate_rhat) "PASS" else "FAIL"
)
w(
  "  MUST: min ESS %.0f > 400:           %s\n",
  d$min_ess_bulk, if (gate_ess) "PASS" else "FAIL"
)
w(
  "  1x2 improvement over v3: %+.5f (%.2f%% of |v3|)   — threshold 0.2%%\n",
  improvement_1x2, 100 * improvement_1x2 / abs(v3_1x2_log_score)
)
w(
  "  Totals improvement over v3: %+.5f (%.2f%% of |v3|) — threshold 0.3%%\n",
  improvement_totals, 100 * improvement_totals / abs(v3_totals_mean_log)
)

# ---- Also stdout summary ----
cat("\n==============================================\n")
cat(" v6 summary\n")
cat("==============================================\n")
cat(sprintf("  wall:                 %.1f min\n", d$wall_min))
cat(sprintf(
  "  divergences:          %d (%.2f%%)\n",
  d$divergences, 100 * d$div_rate
))
cat(sprintf(
  "  max Rhat / min ESS:   %.4f / %.0f\n",
  d$max_rhat, d$min_ess_bulk
))
cat(sprintf(
  "  1x2 log-score:        %.4f (v3 %.4f, BVP %.4f)\n",
  s_v6_1x2$log_score, v3_1x2_log_score, bvp_1x2_log_score
))
cat(sprintf(
  "  Totals mean log:      %.4f (v3 %.4f, BVP %.4f)\n",
  attr(s_v6_totals, "mean_log_score"), v3_totals_mean_log, bvp_totals_mean_log
))
cat(sprintf("\n[report] Wrote %s\n", out_path))
