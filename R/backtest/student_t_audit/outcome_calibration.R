#' Common-observable calibration: per-match posterior predictive 1x2 probabilities.
#'
#' Both models are projected into the same 3-category outcome space
#' (home-win / draw / away-win). Metrics computed on the in-sample
#' observed outcomes:
#'   - log-score   = mean_over_matches log p(observed_outcome)
#'   - Brier       = mean_over_matches || p - one_hot(observed) ||^2
#'   - draw rate   = mean_over_matches p(draw), averaged against observed
#'
#' These are unit-equivalent across continuous (Student-t) and discrete
#' (BVP) models — the confound from log-density vs log-probability in
#' the raw loo comparison is resolved.
#'
#' Strategy: per-match, per posterior draw, simulate M samples of the
#' outcome from the model's posterior predictive and count. Average
#' probabilities across posterior draws (subsample to n_posterior_draws
#' per match for speed).
#'
#' Handles two distinct model forms:
#'   direct_SD : continuous (S, D) from multi_student_t; outcome by
#'               thresholding goal diff.
#'   bvp       : discrete (Y_h, Y_a) from trivariate-reduction BVP;
#'               outcome directly from Y_h vs Y_a.

suppressPackageStartupMessages({
  library(posterior)
  library(dplyr)
})

# ------------------------------------------------------------------
# direct_SD: analytical 1x2 probabilities.
#
# For each match n and posterior draw s, we have
#   mu_D_ns = (off_h + def_h) - (off_a + def_a)  (computable from team
#             strengths + home advantages at the match's round)
#   sigma_D_s, nu_s
# Then:
#   P(home_win)  = 1 - pt((+threshold - mu_D) / sigma_D, nu)
#   P(draw)      = pt((+threshold - mu_D) / sigma_D, nu)
#                  - pt((-threshold - mu_D) / sigma_D, nu)
#   P(away_win)  = pt((-threshold - mu_D) / sigma_D, nu)
#
# This is analytical — no MC simulation needed.
# ------------------------------------------------------------------

#' Extract per-match, per-draw mu_D values from a direct_SD fit.
#' Uses transformed parameters `offense[round, team]`, `defense[...]`,
#' `home_advantage_off/def[team]`. Returns a [n_draws x N] matrix.
direct_SD_mu_D_matrix <- function(fit, stan_data, n_draws = 200) {
  offense <- fit$draws("offense", format = "draws_array")
  defense <- fit$draws("defense", format = "draws_array")
  ha_off <- fit$draws("home_advantage_off", format = "draws_matrix")
  ha_def <- fit$draws("home_advantage_def", format = "draws_matrix")

  total_draws <- dim(ha_off)[1]
  draw_idx <- round(seq(1, total_draws, length.out = min(n_draws, total_draws)))
  ha_off <- ha_off[draw_idx, , drop = FALSE]
  ha_def <- ha_def[draw_idx, , drop = FALSE]

  # offense / defense come back as [iteration, chain, variable]. The
  # variable ordering is offense[1,1], offense[1,2], ..., offense[round, team].
  # Flatten to [draw, round, team]. Each saved fit has chains x iter_sampling
  # draws; we already sub-sampled ha_off / ha_def along the concatenated
  # axis; apply the same index to offense/defense.
  off_flat <- posterior::as_draws_matrix(offense)[draw_idx, , drop = FALSE]
  def_flat <- posterior::as_draws_matrix(defense)[draw_idx, , drop = FALSE]

  # Map variable names to [round, team] indices.
  off_names <- colnames(off_flat)
  rt <- regmatches(off_names, regexec("offense\\[(\\d+),(\\d+)\\]", off_names))
  off_round <- as.integer(vapply(rt, `[`, character(1), 2L))
  off_team <- as.integer(vapply(rt, `[`, character(1), 3L))

  N <- stan_data$N
  round1 <- stan_data$round1
  round2 <- stan_data$round2
  team1 <- stan_data$team1
  team2 <- stan_data$team2

  # Build mu_D for each (draw, match).
  n_used <- length(draw_idx)
  mu_D <- matrix(NA_real_, nrow = n_used, ncol = N)
  for (s in seq_len(n_used)) {
    # Reshape off/def to [round x team] for this draw.
    off_s <- matrix(NA_real_, stan_data$N_rounds, stan_data$K)
    def_s <- matrix(NA_real_, stan_data$N_rounds, stan_data$K)
    for (i in seq_along(off_round)) {
      off_s[off_round[i], off_team[i]] <- off_flat[s, i]
      def_s[off_round[i], off_team[i]] <- def_flat[s, i]
    }
    for (n in seq_len(N)) {
      off_h <- off_s[round1[n], team1[n]] + ha_off[s, team1[n]]
      def_h <- def_s[round1[n], team1[n]] + ha_def[s, team1[n]]
      off_a <- off_s[round2[n], team2[n]]
      def_a <- def_s[round2[n], team2[n]]
      mu_D[s, n] <- (off_h + def_h) - (off_a + def_a)
    }
  }
  list(mu_D = mu_D, draw_idx = draw_idx)
}

#' Compute analytical per-match 1x2 probabilities for direct_SD.
#' Returns a data.frame with columns home_win, draw, away_win.
direct_SD_outcome_probs <- function(fit, stan_data, threshold = 0.5, n_draws = 200) {
  mu_info <- direct_SD_mu_D_matrix(fit, stan_data, n_draws = n_draws)
  mu_D <- mu_info$mu_D

  sigma_D <- as.numeric(fit$draws("sigma_D", format = "draws_matrix")[mu_info$draw_idx, ])
  nu <- as.numeric(fit$draws("nu", format = "draws_matrix")[mu_info$draw_idx, ])

  # Per-match probability, averaged over posterior draws.
  N <- ncol(mu_D)
  n_used <- nrow(mu_D)
  p_home <- p_draw <- p_away <- numeric(N)
  for (n in seq_len(N)) {
    z_up <- (threshold - mu_D[, n]) / sigma_D
    z_dn <- (-threshold - mu_D[, n]) / sigma_D
    p_home[n] <- mean(1 - pt(z_up, df = nu))
    p_draw[n] <- mean(pt(z_up, df = nu) - pt(z_dn, df = nu))
    p_away[n] <- mean(pt(z_dn, df = nu))
  }
  data.frame(home_win = p_home, draw = p_draw, away_win = p_away)
}

# ------------------------------------------------------------------
# BVP: Monte-Carlo 1x2 probabilities.
#
# BVP has goals1_pred / goals2_pred only for N_pred matches. For
# in-sample calibration we MC-sample (Y_h, Y_a) per match per draw
# in R using the Karlis-Ntzoufras trivariate reduction:
#   U_1 ~ Poisson(lambda_1),  U_2 ~ Poisson(lambda_2),  U_3 ~ Poisson(lambda_3)
#   Y_h = U_1 + U_3, Y_a = U_2 + U_3
# where lambda_i are reconstructed from per-match team strengths.
#
# For each match and posterior draw, simulate M outcomes and count.
# ------------------------------------------------------------------

bvp_outcome_probs <- function(fit, stan_data, n_draws = 200, M = 200) {
  # Parameters to extract.
  vars <- c(
    "mean_log_goals", "alpha_mu3", "beta_mu3_strength_diff",
    "home_advantage_off", "home_advantage_def", "offense", "defense"
  )
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

  # Map offense/defense columns to [round, team].
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
  p_home <- p_draw <- p_away <- numeric(N)
  counts_home <- counts_draw <- counts_away <- numeric(N)

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

      # log-mean for home and away goals
      mu1 <- mlg[s] + off_h - def_a
      mu2 <- mlg[s] + off_a - def_h

      strength_diff <- (off_h + def_h) - (off_a + def_a)
      mu3 <- a_mu3[s] + b_mu3[s] * abs(strength_diff)

      lam1 <- exp(mu1)
      lam2 <- exp(mu2)
      lam3 <- exp(mu3)

      # MC simulate M outcomes
      u1 <- rpois(M, lam1)
      u2 <- rpois(M, lam2)
      u3 <- rpois(M, lam3)
      y_h <- u1 + u3
      y_a <- u2 + u3
      counts_home[n] <- counts_home[n] + sum(y_h > y_a)
      counts_draw[n] <- counts_draw[n] + sum(y_h == y_a)
      counts_away[n] <- counts_away[n] + sum(y_h < y_a)
    }
  }
  total_sims <- n_used * M
  data.frame(
    home_win = counts_home / total_sims,
    draw = counts_draw / total_sims,
    away_win = counts_away / total_sims
  )
}

# ------------------------------------------------------------------
# Metrics
# ------------------------------------------------------------------

#' Given per-match outcome probs (data.frame with home_win/draw/away_win)
#' and observed goals1 / goals2, compute calibration metrics.
score_outcome <- function(probs, goals1, goals2, label = "") {
  observed <- ifelse(goals1 > goals2, "home_win",
    ifelse(goals1 == goals2, "draw", "away_win")
  )
  # log-score: mean(log p(observed)), with tiny floor to avoid -Inf.
  p_obs <- mapply(
    function(r, o) max(probs[[o]][r], 1e-10),
    seq_along(observed), observed
  )
  log_score <- mean(log(p_obs))

  # Brier = mean || p - one_hot ||^2
  brier <- mean((probs$home_win - (observed == "home_win"))^2 +
    (probs$draw - (observed == "draw"))^2 +
    (probs$away_win - (observed == "away_win"))^2)

  # RPS (ordinal Brier)
  cum_p <- cbind(
    probs$home_win,
    probs$home_win + probs$draw,
    probs$home_win + probs$draw + probs$away_win
  )
  cum_o <- cbind(
    as.integer(observed == "home_win"),
    as.integer(observed == "home_win" | observed == "draw"),
    1
  )
  rps <- mean(rowMeans((cum_p - cum_o)^2))

  # Aggregate predicted vs observed frequency by outcome class
  freq_predicted <- colMeans(probs)
  freq_observed <- c(
    home_win = mean(observed == "home_win"),
    draw = mean(observed == "draw"),
    away_win = mean(observed == "away_win")
  )

  list(
    label = label,
    n = nrow(probs),
    log_score = log_score,
    brier = brier,
    rps = rps,
    freq_predicted = freq_predicted,
    freq_observed = freq_observed
  )
}

#' Pretty-print a score_outcome result.
print_outcome_scores <- function(s, con = stdout()) {
  w <- function(...) cat(sprintf(...), file = con)
  w("  label:                %s\n", s$label)
  w("  N matches:            %d\n", s$n)
  w("  log-score:            %.4f  (higher = better)\n", s$log_score)
  w("  Brier:                %.4f  (lower = better, 0..2)\n", s$brier)
  w("  RPS:                  %.4f  (lower = better)\n", s$rps)
  w(
    "  predicted freq (H/D/A): %.3f / %.3f / %.3f\n",
    s$freq_predicted["home_win"], s$freq_predicted["draw"], s$freq_predicted["away_win"]
  )
  w(
    "  observed  freq (H/D/A): %.3f / %.3f / %.3f\n",
    s$freq_observed["home_win"], s$freq_observed["draw"], s$freq_observed["away_win"]
  )
}
