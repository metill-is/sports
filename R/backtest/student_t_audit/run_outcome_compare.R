#!/usr/bin/env Rscript
#
# Common-observable 1x2 comparison: project each fit's posterior predictive
# into home-win / draw / away-win probabilities per match, then compute
# log-score, Brier, and RPS on the observed outcomes. Scale-equivalent
# across continuous (Student-t) and discrete (BVP) models — resolves the
# loo cross-class confound from Phase 1.
#
# Uses 100 posterior draws by default per fit to keep runtime reasonable.

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
cat(" Common-observable 1x2 calibration compare\n")
cat("========================================\n")

# --- Prep ---------------------------------------------------------------
cat("\n[prep] Loading football/iceland male data...\n")
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
cat(sprintf(
  "  Observed H/D/A: %.3f / %.3f / %.3f\n",
  mean(obs_outcome == "home_win"),
  mean(obs_outcome == "draw"),
  mean(obs_outcome == "away_win")
))

# --- Load fits ---------------------------------------------------------
audit_dir <- file.path(sports_dir, "football", "iceland", "results", "male", "audits", "student_t")

cat("\n[load] direct_SD_original (adapt_delta=0.8)\n")
fit_direct <- readRDS(file.path(audit_dir, "fit_direct_SD.rds"))

# NOTE: the lean variant was saved via saveRDS (no save_object) — its
# CSVs are in the /var/folders temp dir and have been cleaned up, so
# the draws can't be reloaded. Skipped here; re-save via save_object()
# on next refit. The original direct_SD is apples-to-apples enough for
# the core "is direct_SD competitive with BVP?" question.

cat("[load] bvp_no_inflation (production baseline)\n")
fit_bvp <- readRDS(file.path(
  sports_dir, "football", "iceland", "results", "male",
  "fit_noinflation.rds"
))

# --- Compute per-match outcome probs -----------------------------------
N_DRAWS <- 100L # posterior subsample
BVP_MC_M <- 100L # MC draws per (match, posterior draw)

cat(sprintf(
  "\n[direct_SD] Computing analytical 1x2 probs (%d posterior draws)...\n",
  N_DRAWS
))
t0 <- Sys.time()
probs_direct <- direct_SD_outcome_probs(fit_direct, stan_data,
  threshold = 0.5, n_draws = N_DRAWS
)
cat(sprintf("  wall: %.1f s\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

cat(sprintf(
  "\n[bvp] Computing MC 1x2 probs (%d draws x %d sims)...\n",
  N_DRAWS, BVP_MC_M
))
t0 <- Sys.time()
probs_bvp <- bvp_outcome_probs(fit_bvp, stan_data,
  n_draws = N_DRAWS, M = BVP_MC_M
)
cat(sprintf("  wall: %.1f s\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

# --- Score ----------------------------------------------------------------
s_direct <- score_outcome(probs_direct, obs_goals1, obs_goals2, "direct_SD")
s_bvp <- score_outcome(probs_bvp, obs_goals1, obs_goals2, "bvp_no_inflation")

# --- Write report --------------------------------------------------------
out_path <- file.path(audit_dir, "outcome_compare_phase1b.txt")
con <- file(out_path, open = "wt")
on.exit(close(con))
w <- function(...) cat(sprintf(...), file = con)

w("Common-observable 1x2 calibration — Iceland male\n")
w("Date: %s\n", format(Sys.Date()))
w(
  "Commit: %s\n",
  tryCatch(system("git rev-parse --short HEAD", intern = TRUE),
    error = function(e) "(unknown)"
  )
)
w(
  "Source: football/iceland training data (%d matches, %d teams, %d rounds)\n",
  stan_data$N, stan_data$K, stan_data$N_rounds
)
w("Posterior draws per fit: %d (subsampled for speed)\n", N_DRAWS)
w("BVP MC draws per (match, posterior sample): %d\n", BVP_MC_M)
w("%s\n\n", strrep("=", 72))

for (s in list(s_direct, s_bvp)) {
  w("=== %s ===\n", s$label)
  print_outcome_scores(s, con)
  w("\n")
}

w("=== Rank by log-score (higher is better) ===\n")
scores <- data.frame(
  model = c(s_direct$label, s_bvp$label),
  log_score = c(s_direct$log_score, s_bvp$log_score),
  brier = c(s_direct$brier, s_bvp$brier),
  rps = c(s_direct$rps, s_bvp$rps)
)
scores <- scores[order(-scores$log_score), ]
capture.output(print(scores, row.names = FALSE, digits = 5),
  file = con, append = TRUE
)
w("\n\nNotes:\n")
w("  - direct_SD analytical probs via univariate t-CDF on the marginal D.\n")
w("  - BVP MC probs via trivariate-reduction sampling in R.\n")
w("  - Scale is comparable: all three produce probabilities on (H/D/A).\n")
w("  - log-score and Brier are the headline metrics; RPS captures ordinality.\n")

cat(sprintf("\n[report] Wrote %s\n", out_path))

# --- Print summary to stdout too -----------------------------------------
cat("\n========================================\n")
cat(" Summary (higher log-score = better)\n")
cat("========================================\n")
print(scores, row.names = FALSE, digits = 5)
cat("\n")
