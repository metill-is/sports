/**
 * Football (S, D) model — v6 = v3 family + match-dependent (sigma_S, sigma_D).
 *
 * Structural change vs v3:
 *   Scalar sigma_S, sigma_D replaced by log-linear heteroskedasticity
 *   on signed combined quality q(n) = s_h + s_a and mismatch g(n) = |s_h - s_a|,
 *   where s_k = off_k + def_k (including home advantage for the home team).
 *
 *   log sigma_S(n) = alpha_S + beta_S_q * q(n) + beta_S_g * g(n)
 *   log sigma_D(n) = alpha_D + beta_D_q * q(n) + beta_D_g * g(n)
 *
 * Efficiency: Sigma_SD = diag(sigma_S, sigma_D) * R * diag(sigma_S, sigma_D)
 *   with R = [[1, rho_SD], [rho_SD, 1]]. Since rho_SD is scalar,
 *   L_Omega = Cholesky(R) is computed once in transformed parameters.
 *   Per-match Cholesky factor L_n = diag_pre_multiply([sigma_S_n, sigma_D_n]', L_Omega).
 *   Likelihood uses multi_student_t_cholesky_lpdf — skips Stan's internal Cholesky.
 *
 * Design doc: Metill vault Knowledge/Sports Models/football-v6-matchdep-sigma-design-2026-04-21.md
 */

data {
  int<lower=0> K;
  int<lower=0> N;
  int<lower=0> N_rounds;
  int<lower=0> N_seasons;
  array[N] int<lower=1, upper=K> team1;
  array[N] int<lower=1, upper=K> team2;
  array[N] int<lower=1, upper=N_rounds> round1;
  array[N] int<lower=1, upper=N_rounds> round2;
  array[N] int<lower=1, upper=N_seasons> season;
  matrix<lower=0>[K, N_rounds] time_between_matches;
  array[N] int<lower=0> goals1;
  array[N] int<lower=0> goals2;

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
  real<lower=0> sigma_off;

  sum_to_zero_vector[K] def0;
  array[N_rounds] sum_to_zero_vector[K] z_def;
  real<lower=0> sigma_def;

  vector<lower=0>[K] home_advantage_off;
  vector<lower=0>[K] home_advantage_def;

  real mean_goals0;
  real<lower=-1, upper=1> rho_SD;
  real<lower=1> nu;

  real alpha_S;
  real alpha_D;
  real beta_S_q;
  real beta_S_g;
  real beta_D_q;
  real beta_D_g;
}

transformed parameters {
  array[N_rounds] vector[K] offense;
  array[N_rounds] vector[K] defense;

  offense[1] = off0;
  defense[1] = def0;
  for (i in 2:N_rounds) {
    offense[i] = offense[i-1] + sigma_off * (delta_t[, i] .* z_off[i]);
    defense[i] = defense[i-1] + sigma_def * (delta_t[, i] .* z_def[i]);
  }

  matrix[2, 2] L_Omega;
  L_Omega[1, 1] = 1;
  L_Omega[1, 2] = 0;
  L_Omega[2, 1] = rho_SD;
  L_Omega[2, 2] = sqrt(1 - square(rho_SD));
}

model {
  off0 ~ normal(0, 1);
  def0 ~ normal(0, 1);
  for (i in 1:N_rounds) {
    z_off[i] ~ std_normal();
    z_def[i] ~ std_normal();
  }

  sigma_off ~ exponential(100);
  sigma_def ~ exponential(100);

  home_advantage_off ~ normal(0.15, 0.3);
  home_advantage_def ~ normal(0.15, 0.3);

  mean_goals0 ~ normal(1.3, 0.5);
  rho_SD ~ normal(0, 0.5);
  nu ~ gamma(3, 0.15);

  alpha_S ~ normal(log(1.72), 0.3);
  alpha_D ~ normal(log(1.79), 0.3);
  beta_S_q ~ normal(0, 0.3);
  beta_S_g ~ normal(0, 0.3);
  beta_D_q ~ normal(0, 0.3);
  beta_D_g ~ normal(0, 0.3);

  for (n in 1:N) {
    real off_h = offense[round1[n], team1[n]] + home_advantage_off[team1[n]];
    real def_h = defense[round1[n], team1[n]] + home_advantage_def[team1[n]];
    real off_a = offense[round2[n], team2[n]];
    real def_a = defense[round2[n], team2[n]];

    vector[2] mu_n;
    mu_n[1] = 2 * mean_goals0 + (off_h + off_a) - (def_h + def_a);
    mu_n[2] = (off_h + def_h) - (off_a + def_a);

    real s_h = off_h + def_h;
    real s_a = off_a + def_a;
    real q_n = s_h + s_a;
    real g_n = abs(s_h - s_a);

    vector[2] sigma_vec;
    sigma_vec[1] = exp(alpha_S + beta_S_q * q_n + beta_S_g * g_n);
    sigma_vec[2] = exp(alpha_D + beta_D_q * q_n + beta_D_g * g_n);

    matrix[2, 2] L_n = diag_pre_multiply(sigma_vec, L_Omega);
    target += multi_student_t_cholesky_lpdf(obs_SD[n] | nu, mu_n, L_n);
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
  vector[N] mu_S_fit;
  vector[N] mu_D_fit;
  vector[N] sigma_S_fit;
  vector[N] sigma_D_fit;
  array[N] int<lower=0> total_goals_rep;
  vector[N] goal_diff_rep;

  for (n in 1:N) {
    real off_h = offense[round1[n], team1[n]] + home_advantage_off[team1[n]];
    real def_h = defense[round1[n], team1[n]] + home_advantage_def[team1[n]];
    real off_a = offense[round2[n], team2[n]];
    real def_a = defense[round2[n], team2[n]];

    vector[2] mu_n;
    mu_n[1] = 2 * mean_goals0 + (off_h + off_a) - (def_h + def_a);
    mu_n[2] = (off_h + def_h) - (off_a + def_a);

    real s_h = off_h + def_h;
    real s_a = off_a + def_a;
    real q_n = s_h + s_a;
    real g_n = abs(s_h - s_a);

    vector[2] sigma_vec;
    sigma_vec[1] = exp(alpha_S + beta_S_q * q_n + beta_S_g * g_n);
    sigma_vec[2] = exp(alpha_D + beta_D_q * q_n + beta_D_g * g_n);

    matrix[2, 2] L_n = diag_pre_multiply(sigma_vec, L_Omega);

    mu_S_fit[n] = mu_n[1];
    mu_D_fit[n] = mu_n[2];
    sigma_S_fit[n] = sigma_vec[1];
    sigma_D_fit[n] = sigma_vec[2];

    log_lik[n] = multi_student_t_cholesky_lpdf(obs_SD[n] | nu, mu_n, L_n);

    vector[2] draw = multi_student_t_cholesky_rng(nu, mu_n, L_n);
    total_goals_rep[n] = to_int(round(fmax(0, draw[1])));
    goal_diff_rep[n] = draw[2];
  }

  array[N_pred] int<lower=0> total_goals_pred;
  vector[N_pred] goal_diff_pred;

  for (n in 1:N_pred) {
    real off_h = offense[N_rounds, team1_pred[n]] + home_advantage_off[team1_pred[n]];
    real def_h = defense[N_rounds, team1_pred[n]] + home_advantage_def[team1_pred[n]];
    real off_a = offense[N_rounds, team2_pred[n]];
    real def_a = defense[N_rounds, team2_pred[n]];

    vector[2] mu_n;
    mu_n[1] = 2 * mean_goals0 + (off_h + off_a) - (def_h + def_a);
    mu_n[2] = (off_h + def_h) - (off_a + def_a);

    real s_h = off_h + def_h;
    real s_a = off_a + def_a;
    real q_n = s_h + s_a;
    real g_n = abs(s_h - s_a);

    vector[2] sigma_vec;
    sigma_vec[1] = exp(alpha_S + beta_S_q * q_n + beta_S_g * g_n);
    sigma_vec[2] = exp(alpha_D + beta_D_q * q_n + beta_D_g * g_n);

    matrix[2, 2] L_n = diag_pre_multiply(sigma_vec, L_Omega);
    vector[2] draw = multi_student_t_cholesky_rng(nu, mu_n, L_n);
    total_goals_pred[n] = to_int(round(fmax(0, draw[1])));
    goal_diff_pred[n] = draw[2];
  }
}
