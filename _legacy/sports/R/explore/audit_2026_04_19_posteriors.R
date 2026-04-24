#!/usr/bin/env Rscript
# Stan model audit — posterior-based verifications (2026-04-19).
#
# Only loads variables of interest from each fit to avoid the full
# cmdstanr object cost. Each fit is ~500MB-1GB on disk.

suppressPackageStartupMessages({
  library(posterior)
  library(dplyr)
  library(here)
})

section <- function(n, title) {
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat("§", n, " ", title, "\n", sep = "")
  cat(strrep("=", 70), "\n", sep = "")
}

load_draws <- function(path, vars) {
  cat(sprintf("  [loading %s]\n", path))
  t0 <- Sys.time()
  fit <- readRDS(path)
  d <- fit$draws(variables = vars, format = "draws_df")
  cat(sprintf(
    "  [loaded + extracted in %.1fs, %d draws]\n",
    as.numeric(difftime(Sys.time(), t0, units = "secs")),
    nrow(d)
  ))
  rm(fit)
  gc(verbose = FALSE)
  d
}

# =============================================================================
section(1, "Basketball Iceland (male) — mean_goals, sigma_team CV, drift")
# =============================================================================
# Expected: prior mean_goals0 ~ N(30, 10). Data mean per team is 90.
# So posterior should be very far from prior (> 5 sigma).

path_bm <- here("basketball", "iceland", "results", "male", "fit.rds")
draws_bm <- load_draws(path_bm,
  vars = c(
    "mean_goals0", "delta_mean_goals",
    "sigma_team", "sigma_off", "sigma_def",
    "nu", "alpha_rho", "beta_rho",
    "mean_sigma_off", "mean_sigma_def"
  )
)

mean_goals0 <- as.numeric(draws_bm$mean_goals0)
cat(sprintf(
  "  mean_goals0  posterior mean = %.2f   sd = %.2f\n",
  mean(mean_goals0), sd(mean_goals0)
))
cat(sprintf(
  "  Prior was N(30, 10). Posterior is %.1f prior-sigma away from prior mean.\n",
  (mean(mean_goals0) - 30) / 10
))

# sigma_team CV — is this parameter doing work, or can we collapse to scalar?
st <- draws_bm |>
  posterior::subset_draws(variable = "sigma_team") |>
  as_draws_matrix()
team_means <- apply(st, 2, mean)
team_lo <- apply(st, 2, quantile, 0.05)
team_hi <- apply(st, 2, quantile, 0.95)
cv <- sd(team_means) / mean(team_means)
cat(sprintf("\n  sigma_team[k] across K=%d teams:\n", length(team_means)))
cat(sprintf("    mean(posterior means)      = %.3f\n", mean(team_means)))
cat(sprintf("    sd(posterior means)        = %.3f\n", sd(team_means)))
cat(sprintf("    CV across teams            = %.3f  (%.1f%%)\n", cv, cv * 100))
cat(sprintf(
  "    min / max posterior mean   = %.3f / %.3f\n",
  min(team_means), max(team_means)
))
# Typical within-team 90%% CI width (sampling uncertainty for a single team)
widths <- team_hi - team_lo
cat(sprintf("    median within-team 90%% CI width  = %.3f\n", median(widths)))
cat(sprintf(
  "    between-team variation / within = %.2f\n",
  sd(team_means) / median(widths)
))
# If between-team SD is small vs within-team CI width, the teams are effectively
# the same — scalar sigma_team would suffice.

# nu — are tails actually heavy?
nu_mean <- mean(as.numeric(draws_bm$nu))
cat(sprintf("\n  nu (Student-t df): posterior mean = %.1f\n", nu_mean))
if (nu_mean > 30) cat("    -> nearly Gaussian; heavy tails not doing much\n")
if (nu_mean < 10) cat("    -> materially heavy tails\n")

# =============================================================================
section(2, "Basketball Iceland (male) — sum-to-zero drift from real posterior")
# =============================================================================
# Extract offense[t, k], check sum over k
cat("  loading offense array (expect several GB RAM)...\n")
fit_bm <- readRDS(path_bm)
# Just pull posterior means of offense[t, k]
off_summary <- fit_bm$summary(variables = "offense") |>
  dplyr::mutate(
    idx = gsub("offense\\[|\\]", "", variable)
  ) |>
  tidyr::separate(idx, into = c("t", "k"), sep = ",", convert = TRUE)
rm(fit_bm)
gc(verbose = FALSE)

drift_by_t <- off_summary |>
  group_by(t) |>
  summarise(
    sum_mean = sum(mean),
    cross_team_sd = sd(mean),
    .groups = "drop"
  )
N_rounds <- max(drift_by_t$t)
K <- length(unique(off_summary$k))
cat(sprintf("  K=%d teams, N_rounds=%d\n", K, N_rounds))
cat(sprintf("  Round   sum_k offense[t,k]   cross-team SD    drift%%\n"))
for (tt in unique(c(1, 10, 50, 100, round(N_rounds / 2), N_rounds))) {
  if (tt <= N_rounds) {
    r <- drift_by_t[drift_by_t$t == tt, ]
    cat(sprintf(
      "  %-5d   %+.4f             %.3f            %.1f%%\n",
      tt, r$sum_mean, r$cross_team_sd,
      100 * abs(r$sum_mean) / r$cross_team_sd
    ))
  }
}
cat(sprintf(
  "\n  Maximum |drift%%| across all rounds: %.1f%%\n",
  100 * max(abs(drift_by_t$sum_mean) / drift_by_t$cross_team_sd)
))

# =============================================================================
section(3, "Football Iceland (male) — BVP rho, implied Pearson correlation")
# =============================================================================
# Extract logit-rho parameters; compute typical logit-rho, implied construction rho,
# then implied Pearson correlation at typical mean goals.

path_fm <- here("football", "iceland", "results", "male", "fit.rds")
draws_fm <- load_draws(path_fm,
  vars = c(
    "alpha_mu3", "beta_mu3_strength_diff",
    "logit_p0", "beta_logit_p_strength_diff",
    "tie_alpha", "tie_beta",
    "mean_log_goals",
    "mean_sigma_off", "mean_sigma_def",
    "home_advantage_off", "home_advantage_def"
  )
)

alpha <- as.numeric(draws_fm$alpha_mu3)
beta_sd <- as.numeric(draws_fm$beta_mu3_strength_diff)
cat(sprintf(
  "  alpha_mu3                  posterior mean = %+.3f  (%.3f)\n",
  mean(alpha), sd(alpha)
))
cat(sprintf(
  "  beta_mu3_strength_diff     posterior mean = %+.3f  (%.3f)\n",
  mean(beta_sd), sd(beta_sd)
))
# Typical match has strength_diff ~ 0-2 on log-goals scale (small in Iceland)
for (sdf in c(0, 0.3, 0.7, 1.5)) {
  logit_rho <- alpha + beta_sd * sdf
  rho_c <- plogis(logit_rho)
  cat(sprintf(
    "  strength_diff=%.1f  construction rho = %.4f (post mean)\n",
    sdf, mean(rho_c)
  ))
}

# Combined with typical lambdas
mlg <- mean(as.numeric(draws_fm$mean_log_goals))
lam_typ <- exp(mlg)
cat(sprintf(
  "\n  mean_log_goals posterior mean = %.2f  -> typical lambda = %.2f\n",
  mlg, lam_typ
))
# For typical match strength_diff = 0.3, compute Pearson
rho_c_typ <- mean(plogis(alpha + beta_sd * 0.3))
lam3 <- rho_c_typ * sqrt(lam_typ * lam_typ)
pearson <- lam3 / sqrt((lam_typ + lam3) * (lam_typ + lam3))
cat(sprintf("  At strength_diff=0.3, typical match:\n"))
cat(sprintf("    construction rho = %.4f\n", rho_c_typ))
cat(sprintf("    implied Pearson corr(Y1, Y2) = %.4f\n", pearson))
cat(sprintf(
  "    difference = %.1f%% smaller\n",
  100 * (rho_c_typ - pearson) / rho_c_typ
))

# Mixing probability p
logit_p0 <- as.numeric(draws_fm$logit_p0)
beta_p <- as.numeric(draws_fm$beta_logit_p_strength_diff)
cat(sprintf(
  "\n  logit_p0                   posterior mean = %+.3f  (%.3f)\n",
  mean(logit_p0), sd(logit_p0)
))
cat(sprintf(
  "  beta_logit_p_strength_diff posterior mean = %+.3f  (%.3f)\n",
  mean(beta_p), sd(beta_p)
))
for (sdf in c(0, 0.3, 0.7, 1.5)) {
  logit_p <- logit_p0 + beta_p * sdf
  p <- plogis(logit_p)
  cat(sprintf(
    "  strength_diff=%.1f  diagonal inflation weight p = %.4f (post mean)\n",
    sdf, mean(p)
  ))
}
cat("  (observed draw rate in male Iceland football was 0.188, indep-Poisson implied 0.211)\n")
cat("  (so diagonal inflation should be NEAR ZERO if the data is honestly reflected)\n")

# tie_beta — is the implicit +1 slope doing work?
tb <- as.numeric(draws_fm$tie_beta)
cat(sprintf("\n  tie_beta posterior mean = %+.3f  sd = %.3f\n", mean(tb), sd(tb)))
cat(sprintf(
  "  (Prior N(0,1). In code, slope is (1 + tie_beta), so posterior slope ≈ %.3f)\n",
  1 + mean(tb)
))

# home advantage — check if the <lower=0> is binding
hoff_summary <- draws_fm |>
  posterior::subset_draws(variable = "home_advantage_off") |>
  as_draws_matrix() |>
  apply(2, mean)
hdef_summary <- draws_fm |>
  posterior::subset_draws(variable = "home_advantage_def") |>
  as_draws_matrix() |>
  apply(2, mean)
cat(sprintf(
  "\n  home_advantage_off across K teams:  min=%.3f  median=%.3f  max=%.3f\n",
  min(hoff_summary), median(hoff_summary), max(hoff_summary)
))
cat(sprintf(
  "  home_advantage_def across K teams:  min=%.3f  median=%.3f  max=%.3f\n",
  min(hdef_summary), median(hdef_summary), max(hdef_summary)
))
cat(sprintf(
  "  Fraction of teams with home_off posterior mean < 0.02 (near boundary): %.2f\n",
  mean(hoff_summary < 0.02)
))

# =============================================================================
section(4, "Handball Iceland (male) — sigma_team, rho posterior, nu")
# =============================================================================
path_hm <- here("handball", "iceland", "results", "male", "2026-03-04", "fit.rds")
draws_hm <- load_draws(path_hm,
  vars = c(
    "rho", "nu", "sigma_team",
    "mean_goals0", "mean_sigma_off"
  )
)
cat(sprintf(
  "  rho        posterior mean = %+.4f  (%.4f)\n",
  mean(as.numeric(draws_hm$rho)), sd(as.numeric(draws_hm$rho))
))
cat(sprintf("  nu         posterior mean = %.2f\n", mean(as.numeric(draws_hm$nu))))
cat(sprintf(
  "  mean_goals0 posterior mean = %.2f  (Iceland male handball mean ≈ 29.3)\n",
  mean(as.numeric(draws_hm$mean_goals0))
))
st <- draws_hm |>
  posterior::subset_draws(variable = "sigma_team") |>
  as_draws_matrix()
tm <- apply(st, 2, mean)
cv <- sd(tm) / mean(tm)
cat(sprintf(
  "  sigma_team across K=%d teams: mean=%.3f  sd=%.3f  CV=%.1f%%\n",
  length(tm), mean(tm), sd(tm), 100 * cv
))

cat("\n", strrep("=", 70), "\n", sep = "")
cat("Done.\n")
