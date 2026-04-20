#' Common-observable totals (over/under) calibration.
#'
#' For each match, compute per-model P(S > L) at a set of totals lines
#' L (0.5, 1.5, 2.5, 3.5, 4.5, 5.5), then log-score and Brier on the
#' observed indicator (S_obs > L).
#'
#' For direct_SD variants: use the `total_goals_rep` posterior predictive
#' emitted in generated quantities (already rounded to integer with
#' `max(0, .)` floor). For BVP: MC simulate from the trivariate-reduction
#' structure in R (same mechanism as bvp_outcome_probs).

suppressPackageStartupMessages({
  library(posterior)
  library(dplyr)
})

#' Per-match P(S > L) from direct_SD's in-sample posterior predictive
#' `total_goals_rep`. Works for v3, v4, v1, v2, direct_SD, direct_SD_lean
#' — all emit `total_goals_rep[N]` as integer.
direct_SD_totals_probs <- function(fit, stan_data, lines = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5),
                                   n_draws = 200) {
  rep <- fit$draws("total_goals_rep", format = "draws_matrix")
  total_draws <- nrow(rep)
  draw_idx <- round(seq(1, total_draws, length.out = min(n_draws, total_draws)))
  rep <- rep[draw_idx, , drop = FALSE]

  N <- stan_data$N
  stopifnot(ncol(rep) == N)

  out <- matrix(NA_real_,
    nrow = N, ncol = length(lines),
    dimnames = list(NULL, as.character(lines))
  )
  for (j in seq_along(lines)) {
    out[, j] <- colMeans(rep > lines[j])
  }
  out
}

#' BVP totals via MC in R — reconstruct lambda_1/2/3 per match per draw
#' (like bvp_outcome_probs) and sample (y_h, y_a) via trivariate reduction.
bvp_totals_probs <- function(fit, stan_data,
                             lines = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5),
                             n_draws = 100, M = 200) {
  offense <- fit$draws("offense", format = "draws_matrix")
  defense <- fit$draws("defense", format = "draws_matrix")
  ha_off <- fit$draws("home_advantage_off", format = "draws_matrix")
  ha_def <- fit$draws("home_advantage_def", format = "draws_matrix")
  mlg <- as.numeric(fit$draws("mean_log_goals", format = "draws_matrix"))
  a_mu3 <- as.numeric(fit$draws("alpha_mu3", format = "draws_matrix"))
  b_mu3 <- as.numeric(fit$draws("beta_mu3_strength_diff", format = "draws_matrix"))

  total_draws <- length(mlg)
  draw_idx <- round(seq(1, total_draws, length.out = min(n_draws, total_draws)))
  ha_off <- ha_off[draw_idx, , drop = FALSE]
  ha_def <- ha_def[draw_idx, , drop = FALSE]
  mlg <- mlg[draw_idx]
  a_mu3 <- a_mu3[draw_idx]
  b_mu3 <- b_mu3[draw_idx]
  off_flat <- offense[draw_idx, , drop = FALSE]
  def_flat <- defense[draw_idx, , drop = FALSE]

  off_names <- colnames(off_flat)
  rt <- regmatches(off_names, regexec("offense\\[(\\d+),(\\d+)\\]", off_names))
  off_round <- as.integer(vapply(rt, `[`, character(1), 2L))
  off_team <- as.integer(vapply(rt, `[`, character(1), 3L))

  N <- stan_data$N
  K <- stan_data$K
  N_rounds <- stan_data$N_rounds
  round1 <- stan_data$round1
  round2 <- stan_data$round2
  team1 <- stan_data$team1
  team2 <- stan_data$team2

  n_used <- length(draw_idx)
  counts_over <- matrix(0L, nrow = N, ncol = length(lines))

  for (s in seq_len(n_used)) {
    off_s <- matrix(NA_real_, N_rounds, K)
    def_s <- matrix(NA_real_, N_rounds, K)
    for (i in seq_along(off_round)) {
      off_s[off_round[i], off_team[i]] <- off_flat[s, i]
      def_s[off_round[i], off_team[i]] <- def_flat[s, i]
    }
    for (n in seq_len(N)) {
      off_h <- off_s[round1[n], team1[n]] + ha_off[s, team1[n]]
      def_h <- def_s[round1[n], team1[n]] + ha_def[s, team1[n]]
      off_a <- off_s[round2[n], team2[n]]
      def_a <- def_s[round2[n], team2[n]]

      mu1 <- mlg[s] + off_h - def_a
      mu2 <- mlg[s] + off_a - def_h
      strength_diff <- (off_h + def_h) - (off_a + def_a)
      mu3 <- a_mu3[s] + b_mu3[s] * abs(strength_diff)

      lam1 <- exp(mu1)
      lam2 <- exp(mu2)
      lam3 <- exp(mu3)

      u1 <- rpois(M, lam1)
      u2 <- rpois(M, lam2)
      u3 <- rpois(M, lam3)
      total <- (u1 + u3) + (u2 + u3)
      for (j in seq_along(lines)) {
        counts_over[n, j] <- counts_over[n, j] + sum(total > lines[j])
      }
    }
  }
  total_sims <- n_used * M
  mat <- counts_over / total_sims
  colnames(mat) <- as.character(lines)
  mat
}

#' Log-score and Brier on totals at each line, given per-match P(over)
#' and observed totals.
score_totals <- function(probs_mat, goals1, goals2, label = "") {
  observed_total <- goals1 + goals2
  lines <- as.numeric(colnames(probs_mat))
  out <- data.frame(
    line = lines,
    log_score = NA_real_,
    brier = NA_real_,
    pred_freq_over = NA_real_,
    obs_freq_over = NA_real_
  )
  for (j in seq_along(lines)) {
    p <- probs_mat[, j]
    obs_over <- observed_total > lines[j]
    p_obs <- ifelse(obs_over, p, 1 - p)
    p_obs <- pmax(p_obs, 1e-10)
    out$log_score[j] <- mean(log(p_obs))
    out$brier[j] <- mean((p - obs_over)^2)
    out$pred_freq_over[j] <- mean(p)
    out$obs_freq_over[j] <- mean(obs_over)
  }
  out$label <- label
  # average log-score across lines (equal weight)
  attr(out, "mean_log_score") <- mean(out$log_score)
  attr(out, "mean_brier") <- mean(out$brier)
  out
}

print_totals_scores <- function(tbl, label = NULL, con = stdout()) {
  if (!is.null(label)) cat(sprintf("\n=== %s ===\n", label), file = con)
  cat(sprintf(
    "  mean log-score across lines: %.4f\n",
    attr(tbl, "mean_log_score")
  ), file = con)
  cat(sprintf(
    "  mean Brier across lines:     %.4f\n",
    attr(tbl, "mean_brier")
  ), file = con)
  capture.output(
    print(tbl[, c("line", "log_score", "brier", "pred_freq_over", "obs_freq_over")],
      row.names = FALSE, digits = 4
    ),
    file = con, append = TRUE
  )
}
