/**
 * Bivariate Student-t Football Model — Direct (S, D) Covariance
 *
 * Models (total_goals, goal_difference) with the covariance matrix
 * parameterised directly in (S, D) space rather than derived from
 * a (Y_h, Y_a) correlation.
 *
 * Key difference from 2d_student_t_totalgoals-v-goaldiff.stan:
 * - sigma_S, sigma_D, rho_SD are modelled directly (3 params)
 * - No per-team sigma_team (was barely varying, CV=4.6%)
 * - No match-dependent correlation (was ρ ≈ 0 for typical matches)
 * - sigma_S and sigma_D are free to differ (Var(S) ≠ Var(D))
 *
 * The mean structure is unchanged:
 *   E[S] = 2μ + (off_h + off_a) - (def_h + def_a) + home_off
 *   E[D] = (off_h + def_h) - (off_a + def_a) + home_off + home_def
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

  // Prediction data
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
  vector[N] obs_total;
  vector[N] obs_diff;
  for (n in 1:N) {
    obs_total[n] = goals1[n] + goals2[n];
    obs_diff[n] = goals1[n] - goals2[n];
  }

  matrix[K, N_rounds] delta_t;
  for (k in 1:K)
    for (r in 1:N_rounds)
      delta_t[k, r] = sqrt(time_between_matches[k, r]);
}

parameters {
  // Offensive strengths
  sum_to_zero_vector[K] off0;
  array[N_rounds] sum_to_zero_vector[K] z_off;
  vector[K] z_sigma_off;
  real<lower=0> scale_sigma_off;
  real mean_sigma_off;

  // Defensive strengths
  sum_to_zero_vector[K] def0;
  array[N_rounds] sum_to_zero_vector[K] z_def;
  vector[K] z_sigma_def;
  real<lower=0> scale_sigma_def;
  real mean_sigma_def;

  // Mean goals per team per match
  real mean_goals0;
  real delta_mean_goals;
  real<lower=0> sigma_mean_goals;
  array[N_seasons - 1] real z_mean_goals;

  // Home advantage
  vector<lower=0>[K] home_advantage_off;
  vector<lower=0>[K] home_advantage_def;

  // Direct (S, D) covariance parameters
  real<lower=0> sigma_S;              // std dev of total goals
  real<lower=0> sigma_D;              // std dev of goal difference
  real<lower=-1, upper=1> rho_SD;     // correlation between S and D

  // Student-t degrees of freedom
  real<lower=1> nu;
}

transformed parameters {
  array[N_rounds] vector[K] offense;
  array[N_rounds] vector[K] defense;
  vector<lower=0>[K] sigma_off = exp(mean_sigma_off + z_sigma_off * scale_sigma_off);
  vector<lower=0>[K] sigma_def = exp(mean_sigma_def + z_sigma_def * scale_sigma_def);

  offense[1] = off0;
  defense[1] = def0;
  for (i in 2:N_rounds) {
    offense[i] = offense[i-1] + delta_t[,i] .* sigma_off .* z_off[i];
    defense[i] = defense[i-1] + delta_t[,i] .* sigma_def .* z_def[i];
  }

  vector[N_seasons] mean_goals;
  mean_goals[1] = mean_goals0;
  for (i in 2:N_seasons)
    mean_goals[i] = mean_goals[i-1] + delta_mean_goals + sigma_mean_goals * z_mean_goals[i-1];

  // Precompute the constant covariance matrix (same for all matches)
  matrix[2,2] Sigma;
  Sigma[1,1] = square(sigma_S);
  Sigma[2,2] = square(sigma_D);
  Sigma[1,2] = rho_SD * sigma_S * sigma_D;
  Sigma[2,1] = Sigma[1,2];
}

model {
  // --- Priors ---

  off0 ~ normal(0, 1);
  def0 ~ normal(0, 1);
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

  home_advantage_off ~ normal(0.15, 0.3);
  home_advantage_def ~ normal(0.15, 0.3);

  mean_goals0 ~ normal(1.3, 0.5);
  delta_mean_goals ~ normal(0, 0.1);
  sigma_mean_goals ~ exponential(2);
  z_mean_goals ~ std_normal();

  // Direct covariance priors
  sigma_S ~ normal(1.5, 0.5);
  sigma_D ~ normal(1.2, 0.5);
  rho_SD ~ normal(0, 0.5);

  nu ~ gamma(3, 0.15);

  // --- Likelihood ---
  for (n in 1:N) {
    real off_h = offense[round1[n], team1[n]] + home_advantage_off[team1[n]];
    real def_h = defense[round1[n], team1[n]] + home_advantage_def[team1[n]];
    real off_a = offense[round2[n], team2[n]];
    real def_a = defense[round2[n], team2[n]];

    real mu = mean_goals[season[n]];

    vector[2] mu_sd;
    mu_sd[1] = 2 * mu + (off_h + off_a) - (def_h + def_a);  // E[S]
    mu_sd[2] = (off_h + def_h) - (off_a + def_a);            // E[D]

    [obs_total[n], obs_diff[n]]' ~ multi_student_t(nu, mu_sd, Sigma);
  }
}

generated quantities {
  // Total home advantage
  vector[K] home_advantage_tot = home_advantage_off + home_advantage_def;

  // Current team strengths
  vector[K] cur_offense_away = offense[N_rounds];
  vector[K] cur_defense_away = defense[N_rounds];
  vector[K] cur_strength_away = cur_offense_away + cur_defense_away;

  vector[K] cur_offense_home = cur_offense_away + home_advantage_off;
  vector[K] cur_defense_home = cur_defense_away + home_advantage_def;
  vector[K] cur_strength_home = cur_offense_home + cur_defense_home;

  vector[K] cur_offense = (cur_offense_away + cur_offense_home) / 2;
  vector[K] cur_defense = (cur_defense_away + cur_defense_home) / 2;
  vector[K] cur_strength = cur_offense + cur_defense;

  // In-sample replicated data (for posterior predictive checks)
  // Model lives in (S, D) space — output those directly, no decomposition
  array[N] int<lower=0> total_goals_rep;
  vector[N] goal_diff_rep;

  for (n in 1:N) {
    real off_h = offense[round1[n], team1[n]] + home_advantage_off[team1[n]];
    real def_h = defense[round1[n], team1[n]] + home_advantage_def[team1[n]];
    real off_a = offense[round2[n], team2[n]];
    real def_a = defense[round2[n], team2[n]];

    vector[2] mu_sd;
    mu_sd[1] = 2 * mean_goals[season[n]] + (off_h + off_a) - (def_h + def_a);
    mu_sd[2] = (off_h + def_h) - (off_a + def_a);

    vector[2] sd_draw = multi_student_t_rng(nu, mu_sd, Sigma);

    total_goals_rep[n] = to_int(round(fmax(0, sd_draw[1])));
    goal_diff_rep[n] = sd_draw[2];  // keep continuous
  }

  // Out-of-sample predictions
  array[N_pred] int<lower=0> total_goals_pred;
  vector[N_pred] goal_diff_pred;

  for (n in 1:N_pred) {
    real off_h = offense[N_rounds, team1_pred[n]] + home_advantage_off[team1_pred[n]];
    real def_h = defense[N_rounds, team1_pred[n]] + home_advantage_def[team1_pred[n]];
    real off_a = offense[N_rounds, team2_pred[n]];
    real def_a = defense[N_rounds, team2_pred[n]];

    vector[2] mu_sd;
    mu_sd[1] = 2 * mean_goals[N_seasons] + (off_h + off_a) - (def_h + def_a);
    mu_sd[2] = (off_h + def_h) - (off_a + def_a);

    vector[2] sd_draw = multi_student_t_rng(nu, mu_sd, Sigma);

    total_goals_pred[n] = to_int(round(fmax(0, sd_draw[1])));
    goal_diff_pred[n] = sd_draw[2];  // keep continuous
  }
}
