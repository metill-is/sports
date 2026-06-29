/**
 * Football (S, D) lean-Gaussian model.
 *
 * Reuses the production bivariate-Poisson mean model VERBATIM (log-link RW
 * offence/defence, per-team home advantage, scalar mean_log_goals) and swaps
 * the likelihood for a bivariate Gaussian on (S, D) = (total, signed diff)
 * with a MEAN-INDUCED, MEAN-SCALED covariance:
 *
 *   lambda_h = exp(mean_log_goals + off_h - def_a)
 *   lambda_a = exp(mean_log_goals + off_a - def_h)
 *   E[S] = lambda_h + lambda_a ;  E[D] = lambda_h - lambda_a
 *   Var(S) = Var(D) = phi * E[S]                 (shared phi, ~0.88)
 *   rho      = tanh(gamma_rho * E[D] / E[S])     (PD-safe, |rho| < 1)
 *   Cov(S,D) = rho * phi * E[S]
 *
 * phi and gamma_rho are the only parameters added vs BVP (which itself drops
 * alpha_mu3 / beta_mu3). Design + diagnostic:
 * docs/superpowers/specs/2026-06-29-sd-gaussian-backtest-design.md
 */

data {
  int<lower=0> K;
  int<lower=0> N;
  int<lower=0> N_rounds;
  array[N] int<lower=1, upper=K> team1;
  array[N] int<lower=1, upper=K> team2;
  array[N] int<lower=1, upper=N_rounds> round1;
  array[N] int<lower=1, upper=N_rounds> round2;
  matrix<lower=0>[K, N_rounds] time_between_matches;
  array[N] int<lower=0> goals1;
  array[N] int<lower=0> goals2;

  // Prediction data (declared for data-contract parity with BVP; unused here)
  int<lower=0> N_top_teams;
  array[N_top_teams] int<lower=0> top_teams;
  vector[N_top_teams] time_to_next_games;

  int<lower=0> N_pred;
  array[N_pred] int<lower=1, upper=K> team1_pred;
  array[N_pred] int<lower=1, upper=K> team2_pred;
  vector[N_pred] pred_timediff1;
  vector[N_pred] pred_timediff2;
}

transformed data {
  array[N] vector[2] obs_SD;
  for (n in 1:N) {
    obs_SD[n][1] = goals1[n] + goals2[n];
    obs_SD[n][2] = goals1[n] - goals2[n];
  }
  matrix[K, N_rounds] delta_t;
  for (k in 1:K)
    for (r in 1:N_rounds)
      delta_t[k, r] = sqrt(time_between_matches[k, r]);
}

parameters {
  sum_to_zero_vector[K] off0;
  array[N_rounds] sum_to_zero_vector[K] z_off;
  vector[K] z_sigma_off;
  real<lower=0> scale_sigma_off;
  real mean_sigma_off;

  sum_to_zero_vector[K] def0;
  array[N_rounds] sum_to_zero_vector[K] z_def;
  vector[K] z_sigma_def;
  real<lower=0> scale_sigma_def;
  real mean_sigma_def;

  real mean_log_goals;

  vector<lower=0>[K] home_advantage_off;
  vector<lower=0>[K] home_advantage_def;

  // (S, D) covariance: mean-scaled shrinkage phi and correlation slope gamma_rho
  real<lower=0> phi;
  real gamma_rho;
}

transformed parameters {
  array[N_rounds] vector[K] offense;
  array[N_rounds] vector[K] defense;
  vector<lower=0>[K] sigma_off = exp(mean_sigma_off + z_sigma_off * scale_sigma_off);
  vector<lower=0>[K] sigma_def = exp(mean_sigma_def + z_sigma_def * scale_sigma_def);

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
  mean_log_goals ~ normal(log(1.5), 1);

  phi ~ normal(0.88, 0.1);
  gamma_rho ~ normal(1.1, 0.4);

  for (n in 1:N) {
    real off_h = offense[round1[n], team1[n]] + home_advantage_off[team1[n]];
    real def_h = defense[round1[n], team1[n]] + home_advantage_def[team1[n]];
    real off_a = offense[round2[n], team2[n]];
    real def_a = defense[round2[n], team2[n]];

    real lambda_h = exp(mean_log_goals + off_h - def_a);
    real lambda_a = exp(mean_log_goals + off_a - def_h);
    real ES = lambda_h + lambda_a;
    real ED = lambda_h - lambda_a;

    real var_SD = phi * ES;
    real rho = tanh(gamma_rho * ED / ES);

    matrix[2, 2] Sigma;
    Sigma[1, 1] = var_SD;
    Sigma[2, 2] = var_SD;
    Sigma[1, 2] = rho * var_SD;
    Sigma[2, 1] = Sigma[1, 2];

    obs_SD[n] ~ multi_normal([ES, ED]', Sigma);
  }
}

generated quantities {
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

  vector[N] log_lik;
  for (n in 1:N) {
    real off_h = offense[round1[n], team1[n]] + home_advantage_off[team1[n]];
    real def_h = defense[round1[n], team1[n]] + home_advantage_def[team1[n]];
    real off_a = offense[round2[n], team2[n]];
    real def_a = defense[round2[n], team2[n]];
    real lambda_h = exp(mean_log_goals + off_h - def_a);
    real lambda_a = exp(mean_log_goals + off_a - def_h);
    real ES = lambda_h + lambda_a;
    real ED = lambda_h - lambda_a;
    real var_SD = phi * ES;
    real rho = tanh(gamma_rho * ED / ES);
    matrix[2, 2] Sigma;
    Sigma[1, 1] = var_SD;
    Sigma[2, 2] = var_SD;
    Sigma[1, 2] = rho * var_SD;
    Sigma[2, 1] = Sigma[1, 2];
    log_lik[n] = multi_normal_lpdf(obs_SD[n] | [ES, ED]', Sigma);
  }

  array[N_pred] int<lower=0> goals1_pred;
  array[N_pred] int<lower=0> goals2_pred;
  array[N_pred] int goal_diff_pred;
  array[N_pred] int<lower=0> total_goals_pred;

  for (n in 1:N_pred) {
    real off_h = offense[N_rounds, team1_pred[n]] + home_advantage_off[team1_pred[n]];
    real def_h = defense[N_rounds, team1_pred[n]] + home_advantage_def[team1_pred[n]];
    real off_a = offense[N_rounds, team2_pred[n]];
    real def_a = defense[N_rounds, team2_pred[n]];
    real lambda_h = exp(mean_log_goals + off_h - def_a);
    real lambda_a = exp(mean_log_goals + off_a - def_h);
    real ES = lambda_h + lambda_a;
    real ED = lambda_h - lambda_a;
    real var_SD = phi * ES;
    real rho = tanh(gamma_rho * ED / ES);
    matrix[2, 2] Sigma;
    Sigma[1, 1] = var_SD;
    Sigma[2, 2] = var_SD;
    Sigma[1, 2] = rho * var_SD;
    Sigma[2, 1] = Sigma[1, 2];

    vector[2] draw = multi_normal_rng([ES, ED]', Sigma);
    int S = to_int(round(fmax(0, draw[1])));
    int D = to_int(round(draw[2]));
    // Parity: S and D must share parity, since hg = (S + D) / 2 is an integer.
    if ((S + D) % 2 != 0) {
      if (D > 0) {
        D -= 1;
      } else if (D < 0) {
        D += 1;
      } else {
        S += 1;
      }
    }
    if (D > S) D = S;       // |D| cannot exceed S (no negative goals)
    if (D < -S) D = -S;
    goals1_pred[n] = (S + D) / 2;
    goals2_pred[n] = (S - D) / 2;
    goal_diff_pred[n] = goals1_pred[n] - goals2_pred[n];
    total_goals_pred[n] = goals1_pred[n] + goals2_pred[n];
  }
}
