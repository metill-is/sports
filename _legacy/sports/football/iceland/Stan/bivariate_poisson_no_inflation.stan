/**
 * Bivariate Poisson football model — NO diagonal inflation.
 *
 * Strips the logit_p / tie_alpha / tie_beta machinery from
 * bivariate_poisson_inflated_diagonal_corrmodel.stan. Keeps the
 * trivariate-reduction correlation structure (alpha_mu3 / beta_mu3_strength_diff)
 * — that's the bivariate Poisson's intrinsic ρ, separate from diagonal inflation.
 *
 * Motivation: 2026-04-20 audits showed fitted mixing weight p ≈ 0.014 (England)
 * and p ≈ 0.02 (Iceland) at typical matches — diagonal inflation appears to
 * be dead weight. This file enables a formal loo::loo elpd comparison.
 *
 * Companion to: Knowledge/Sports Models/next-actions.md (item 2 in
 * 2026-04-20 evening handoff).
 */

functions {
  /**
   * Bivariate Poisson log-PMF with log-scale parameters
   * (trivariate reduction: Y1 = X1 + X3, Y2 = X2 + X3).
   */
  real poisson_2d_log_lpmf(
    array[] int x,
    real log_lambda1,
    real log_lambda2,
    real log_lambda3
  ) {
    int x1 = x[1];
    int x2 = x[2];
    if (x1 < 0 || x2 < 0) return negative_infinity();

    real lambda1 = exp(log_lambda1);
    real lambda2 = exp(log_lambda2);
    real lambda3 = exp(log_lambda3);

    if (lambda3 <= 1e-4) {
      return poisson_lpmf(x1 | lambda1) + poisson_lpmf(x2 | lambda2);
    }

    int K = min(x1, x2);
    vector[K + 1] log_terms;
    for (k in 0:K) {
      log_terms[k + 1] =
        k * log_lambda3
        + (x1 - k) * log_lambda1
        + (x2 - k) * log_lambda2
        - (lgamma(k + 1) + lgamma(x1 - k + 1) + lgamma(x2 - k + 1));
    }
    return -(lambda1 + lambda2 + lambda3) + log_sum_exp(log_terms);
  }

  array[] int poisson_2d_log_rng(real lambda1, real lambda2, real lambda3) {
    int y1 = poisson_log_rng(lambda1);
    int y2 = poisson_log_rng(lambda2);
    int y3 = poisson_log_rng(lambda3);
    array[2] int out;
    out[1] = y1 + y3;
    out[2] = y2 + y3;
    return out;
  }
}

data {
  int<lower=0> K;
  int<lower=0> N;
  int<lower=0> N_rounds;
  array[N] int<lower=1, upper=K> team1;
  array[N] int<lower=1, upper=K> team2;
  array[N] int<lower=1, upper = N_rounds> round1;
  array[N] int<lower=1, upper = N_rounds> round2;
  matrix<lower = 0>[K, N_rounds] time_between_matches;
  array[N] int<lower=0> goals1;
  array[N] int<lower=0> goals2;

  // Prediction data
  int<lower = 0> N_top_teams;
  array[N_top_teams] int<lower=0> top_teams;
  vector[N_top_teams] time_to_next_games;

  int<lower=0> N_pred;
  array[N_pred] int<lower=1, upper=K> team1_pred;
  array[N_pred] int<lower=1, upper=K> team2_pred;
  vector[N_pred] pred_timediff1;
  vector[N_pred] pred_timediff2;
}

transformed data {
  array[2, N] int goals1_2;
  for (n in 1:N) {
    goals1_2[1, n] = goals1[n];
    goals1_2[2, n] = goals2[n];
  }
  matrix[K, N_rounds] delta_t;
  for (k in 1:K)
    for (n in 1:N_rounds)
      delta_t[k, n] = sqrt(time_between_matches[k, n]);

  vector[N_top_teams] delta_t_top = sqrt(time_to_next_games);
  vector[N_pred] pred_delta_t1 = sqrt(pred_timediff1);
  vector[N_pred] pred_delta_t2 = sqrt(pred_timediff2);
}

parameters {
  // Offensive
  sum_to_zero_vector[K] off0;
  array[N_rounds] sum_to_zero_vector[K] z_off;
  vector[K] z_sigma_off;
  real<lower = 0> scale_sigma_off;
  real mean_sigma_off;

  // Defensive
  sum_to_zero_vector[K] def0;
  array[N_rounds] sum_to_zero_vector[K] z_def;
  vector[K] z_sigma_def;
  real<lower = 0> scale_sigma_def;
  real mean_sigma_def;

  // Mean log-goals
  real mean_log_goals;

  // Home advantage
  vector<lower = 0>[K] home_advantage_off;
  vector<lower = 0>[K] home_advantage_def;

  // Bivariate Poisson correlation (trivariate-reduction shared rate)
  real alpha_mu3;
  real beta_mu3_strength_diff;
}

transformed parameters {
  array[N_rounds] vector[K] offense;
  vector<lower = 0>[K] sigma_off = exp(mean_sigma_off + z_sigma_off * scale_sigma_off);

  array[N_rounds] vector[K] defense;
  vector<lower = 0>[K] sigma_def = exp(mean_sigma_def + z_sigma_def * scale_sigma_def);

  offense[1] = off0;
  defense[1] = def0;

  for (i in 2:N_rounds) {
    offense[i] = offense[i - 1] + delta_t[, i] .* sigma_off .* z_off[i];
    defense[i] = defense[i - 1] + delta_t[, i] .* sigma_def .* z_def[i];
  }
}

model {
  off0 ~ std_normal();
  def0 ~ std_normal();
  for (i in 1:N_rounds) {
    z_off[i] ~ std_normal();
    z_def[i] ~ std_normal();
  }

  z_sigma_off ~ std_normal();
  scale_sigma_off ~ exponential(2);
  mean_sigma_off ~ normal(-4, 2);

  z_sigma_def ~ std_normal();
  scale_sigma_def ~ exponential(2);
  mean_sigma_def ~ normal(-5, 2);

  home_advantage_off ~ std_normal();
  home_advantage_def ~ std_normal();

  alpha_mu3 ~ std_normal();
  beta_mu3_strength_diff ~ std_normal();

  mean_log_goals ~ normal(log(1.5), 1);

  for (n in 1:N) {
    vector[2] off;
    vector[2] def;
    vector[2] mu;

    off[1] = offense[round1[n], team1[n]] + home_advantage_off[team1[n]];
    def[1] = defense[round1[n], team1[n]] + home_advantage_def[team1[n]];
    off[2] = offense[round2[n], team2[n]];
    def[2] = defense[round2[n], team2[n]];

    mu[1] = mean_log_goals + off[1] - def[2];
    mu[2] = mean_log_goals + off[2] - def[1];

    real strength_diff = abs(off[1] + def[1] - off[2] - def[2]);
    real logit_rho = alpha_mu3 + beta_mu3_strength_diff * strength_diff;
    real mu3 = log_inv_logit(logit_rho) + 0.5 * (mu[1] + mu[2]);

    goals1_2[, n] ~ poisson_2d_log(mu[1], mu[2], mu3);
  }
}

generated quantities {
  // Per-match log-likelihood for loo::loo / PSIS-LOO
  vector[N] log_lik;
  for (n in 1:N) {
    vector[2] off_ll;
    vector[2] def_ll;
    vector[2] mu_ll;
    off_ll[1] = offense[round1[n], team1[n]] + home_advantage_off[team1[n]];
    def_ll[1] = defense[round1[n], team1[n]] + home_advantage_def[team1[n]];
    off_ll[2] = offense[round2[n], team2[n]];
    def_ll[2] = defense[round2[n], team2[n]];
    mu_ll[1] = mean_log_goals + off_ll[1] - def_ll[2];
    mu_ll[2] = mean_log_goals + off_ll[2] - def_ll[1];
    real strength_diff_ll = abs(off_ll[1] + def_ll[1] - off_ll[2] - def_ll[2]);
    real logit_rho_ll = alpha_mu3 + beta_mu3_strength_diff * strength_diff_ll;
    real mu3_ll = log_inv_logit(logit_rho_ll) + 0.5 * (mu_ll[1] + mu_ll[2]);
    log_lik[n] = poisson_2d_log_lpmf(goals1_2[, n] | mu_ll[1], mu_ll[2], mu3_ll);
  }

  vector[K] home_advantage_tot = home_advantage_off + home_advantage_def;

  vector[K] cur_offense_away = offense[N_rounds];
  vector[K] cur_defense_away = defense[N_rounds];
  vector[K] cur_strength_away = cur_offense_away + cur_defense_away;

  vector[K] cur_offense_home = cur_offense_away + home_advantage_off;
  vector[K] cur_defense_home = cur_defense_away + home_advantage_def;
  vector[K] cur_strength_home = cur_offense_home + cur_defense_home;

  vector[K] cur_offense = (cur_offense_away + cur_offense_home) / 2;
  vector[K] cur_defense = (cur_defense_away + cur_defense_home) / 2;
  vector[K] cur_strength = cur_offense + cur_defense;

  array[N_pred, 2] int<lower=0> goals_pred;
  array[N_pred] int<lower = 0> goals1_pred;
  array[N_pred] int<lower = 0> goals2_pred;
  array[N_pred] int goal_diff_pred;
  array[N_pred] int<lower=0> total_goals_pred;

  for (n in 1:N_pred) {
    vector[2] off;
    vector[2] def;
    vector[2] mu;

    off[1] = offense[N_rounds, team1_pred[n]] + home_advantage_off[team1_pred[n]];
    def[1] = defense[N_rounds, team1_pred[n]] + home_advantage_def[team1_pred[n]];
    off[2] = offense[N_rounds, team2_pred[n]];
    def[2] = defense[N_rounds, team2_pred[n]];

    mu[1] = mean_log_goals + off[1] - def[2];
    mu[2] = mean_log_goals + off[2] - def[1];

    real strength_diff = abs(off[1] + def[1] - off[2] - def[2]);
    real logit_rho = alpha_mu3 + beta_mu3_strength_diff * strength_diff;
    real mu3 = log_inv_logit(logit_rho) + 0.5 * (mu[1] + mu[2]);

    goals_pred[n] = poisson_2d_log_rng(mu[1], mu[2], mu3);
    goals1_pred[n] = goals_pred[n, 1];
    goals2_pred[n] = goals_pred[n, 2];
    goal_diff_pred[n] = goals1_pred[n] - goals2_pred[n];
    total_goals_pred[n] = goals1_pred[n] + goals2_pred[n];
  }
}
