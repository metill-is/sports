#!/usr/bin/env Rscript
#
# Totals (over/under) calibration — does season-varying mean_goals (v4)
# actually beat the scalar mean_goals (v3) where it should?
#
# Compares v3, v4, bvp on in-sample totals calibration.

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
source(file.path(sports_dir, "R", "backtest", "student_t_audit", "totals_calibration.R"))

cat("==============================================\n")
cat(" Totals calibration: v3 vs v4 vs BVP\n")
cat("==============================================\n")

stan_data <- prepare_student_t_audit_data(sports_dir, "football/iceland", "male")
obs_g1 <- stan_data$goals1
obs_g2 <- stan_data$goals2
cat(sprintf(
  "  N=%d  mean_S_obs=%.2f  var_S_obs=%.2f\n",
  stan_data$N, mean(obs_g1 + obs_g2), var(obs_g1 + obs_g2)
))

audit_dir <- file.path(sports_dir, "football", "iceland", "results", "male", "audits", "student_t")

fits <- list(
  v3  = readRDS(file.path(audit_dir, "fit_student_t_v3_free_nu.rds")),
  v4  = readRDS(file.path(audit_dir, "fit_student_t_v4_season_rw.rds")),
  bvp = readRDS(file.path(sports_dir, "football", "iceland", "results", "male", "fit_noinflation.rds"))
)

lines <- c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5)

cat("\n[v3] Totals probs from in-sample total_goals_rep...\n")
t0 <- Sys.time()
p_v3 <- direct_SD_totals_probs(fits$v3, stan_data, lines = lines, n_draws = 200)
cat(sprintf("  wall: %.1f s\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

cat("\n[v4] Totals probs from in-sample total_goals_rep...\n")
t0 <- Sys.time()
p_v4 <- direct_SD_totals_probs(fits$v4, stan_data, lines = lines, n_draws = 200)
cat(sprintf("  wall: %.1f s\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

cat("\n[bvp] Totals probs via MC (100 draws x 200 sims)...\n")
t0 <- Sys.time()
p_bvp <- bvp_totals_probs(fits$bvp, stan_data, lines = lines, n_draws = 100, M = 200)
cat(sprintf("  wall: %.1f s\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

s_v3 <- score_totals(p_v3, obs_g1, obs_g2, "student_t_v3_free_nu")
s_v4 <- score_totals(p_v4, obs_g1, obs_g2, "student_t_v4_season_rw")
s_bvp <- score_totals(p_bvp, obs_g1, obs_g2, "bvp_no_inflation")

# --- Report --------------------------------------------------------------
out_path <- file.path(audit_dir, "totals_compare.txt")
con <- file(out_path, open = "wt")
on.exit(close(con))

cat("Totals calibration compare — Iceland male\n", file = con)
cat(sprintf("Date: %s\n", format(Sys.Date())), file = con)
cat(sprintf(
  "Commit: %s\n",
  tryCatch(system("git rev-parse --short HEAD", intern = TRUE),
    error = function(e) "(unknown)"
  )
), file = con)
cat(sprintf(
  "N_matches: %d, observed mean_S = %.2f\n\n",
  stan_data$N, mean(obs_g1 + obs_g2)
), file = con)

for (nm in c("v3", "v4", "bvp")) {
  s <- list(v3 = s_v3, v4 = s_v4, bvp = s_bvp)[[nm]]
  print_totals_scores(s, label = s$label[1], con = con)
}

cat("\n=== Mean log-score by model (higher better) ===\n", file = con)
capture.output(
  print(data.frame(
    model = c("v3 free_nu", "v4 season_rw", "bvp"),
    mean_log_score = c(
      attr(s_v3, "mean_log_score"),
      attr(s_v4, "mean_log_score"),
      attr(s_bvp, "mean_log_score")
    ),
    mean_brier = c(
      attr(s_v3, "mean_brier"),
      attr(s_v4, "mean_brier"),
      attr(s_bvp, "mean_brier")
    )
  ), row.names = FALSE, digits = 5),
  file = con, append = TRUE
)

cat(sprintf("\n[report] Wrote %s\n", out_path))
cat("\n==============================================\n Summary (mean across O/U lines)\n==============================================\n")
print(data.frame(
  model = c("v3 free_nu", "v4 season_rw", "bvp"),
  mean_log_score = c(
    attr(s_v3, "mean_log_score"),
    attr(s_v4, "mean_log_score"),
    attr(s_bvp, "mean_log_score")
  ),
  mean_brier = c(
    attr(s_v3, "mean_brier"),
    attr(s_v4, "mean_brier"),
    attr(s_bvp, "mean_brier")
  )
), row.names = FALSE, digits = 5)
