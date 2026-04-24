#!/usr/bin/env Rscript
#
# Phase 4 common-observable comparison: direct_SD (orig) vs
# gaussian_v1_barebones vs bvp_no_inflation on 1x2.
#
# Writes compare_phase4.txt under audits/student_t/.

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
source(file.path(sports_dir, "R", "backtest", "student_t_audit", "outcome_calibration.R"))

cat("========================================\n")
cat(" Phase 4 — three-way outcome compare\n")
cat("========================================\n")

stan_data <- prepare_student_t_audit_data(sports_dir, "football/iceland", "male")
cat(sprintf(
  "  K=%d  N=%d  N_rounds=%d\n",
  stan_data$K, stan_data$N, stan_data$N_rounds
))

obs_goals1 <- stan_data$goals1
obs_goals2 <- stan_data$goals2
obs_outcome <- ifelse(obs_goals1 > obs_goals2, "home_win",
  ifelse(obs_goals1 == obs_goals2, "draw", "away_win")
)

audit_dir <- file.path(sports_dir, "football", "iceland", "results", "male", "audits", "student_t")

cat("\n[load] direct_SD_original (student_t, K-dim home_adv, adapt_delta=0.8)\n")
fit_direct <- readRDS(file.path(audit_dir, "fit_direct_SD.rds"))

cat("[load] gaussian_v1_barebones (scalar home_adv, scalar sigma_off/def, adapt_delta=0.95)\n")
fit_bare <- readRDS(file.path(audit_dir, "fit_gaussian_v1_barebones.rds"))

cat("[load] bvp_no_inflation\n")
fit_bvp <- readRDS(file.path(sports_dir, "football", "iceland", "results", "male", "fit_noinflation.rds"))

N_DRAWS <- 100L
BVP_MC_M <- 100L

cat(sprintf("\n[direct_SD] analytical Student-t 1x2 (%d draws)...\n", N_DRAWS))
t0 <- Sys.time()
probs_direct <- direct_SD_outcome_probs(fit_direct, stan_data,
  threshold = 0.5, n_draws = N_DRAWS, family = "student_t",
  scalar_home_adv = FALSE
)
cat(sprintf("  wall: %.1f s\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

cat(sprintf("\n[gaussian_v1_barebones] analytical Gaussian 1x2 (%d draws)...\n", N_DRAWS))
t0 <- Sys.time()
probs_bare <- direct_SD_outcome_probs(fit_bare, stan_data,
  threshold = 0.5, n_draws = N_DRAWS, family = "gaussian",
  scalar_home_adv = TRUE
)
cat(sprintf("  wall: %.1f s\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

cat(sprintf("\n[bvp] MC 1x2 (%d draws x %d sims)...\n", N_DRAWS, BVP_MC_M))
t0 <- Sys.time()
probs_bvp <- bvp_outcome_probs(fit_bvp, stan_data, n_draws = N_DRAWS, M = BVP_MC_M)
cat(sprintf("  wall: %.1f s\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

s_direct <- score_outcome(probs_direct, obs_goals1, obs_goals2, "direct_SD")
s_bare <- score_outcome(probs_bare, obs_goals1, obs_goals2, "gaussian_v1_barebones")
s_bvp <- score_outcome(probs_bvp, obs_goals1, obs_goals2, "bvp_no_inflation")

# Report
out_path <- file.path(audit_dir, "compare_phase4_threeway.txt")
con <- file(out_path, open = "wt")
on.exit(close(con))
w <- function(...) cat(sprintf(...), file = con)

w("Phase 4 three-way outcome compare — Iceland male\n")
w("Date: %s\n", format(Sys.Date()))
w(
  "Commit: %s\n",
  tryCatch(system("git rev-parse --short HEAD", intern = TRUE),
    error = function(e) "(unknown)"
  )
)
w("%s\n\n", strrep("=", 72))

for (s in list(s_direct, s_bare, s_bvp)) {
  w("=== %s ===\n", s$label)
  print_outcome_scores(s, con)
  w("\n")
}

scores <- data.frame(
  model = c(s_direct$label, s_bare$label, s_bvp$label),
  log_score = c(s_direct$log_score, s_bare$log_score, s_bvp$log_score),
  brier = c(s_direct$brier, s_bare$brier, s_bvp$brier),
  rps = c(s_direct$rps, s_bare$rps, s_bvp$rps)
)
scores <- scores[order(-scores$log_score), ]
w("=== Rank by log-score (higher is better) ===\n")
capture.output(print(scores, row.names = FALSE, digits = 5), file = con, append = TRUE)

cat(sprintf("\n[report] Wrote %s\n", out_path))
cat("\n========================================\n Summary\n========================================\n")
print(scores, row.names = FALSE, digits = 5)
