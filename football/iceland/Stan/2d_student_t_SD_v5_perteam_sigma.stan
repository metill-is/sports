/**
 * Football (Y_h, Y_a) model — v5 = v3 family + per-team observation sigma.
 *
 * Structural change vs v3: the Student-t likelihood moves from (S, D)
 * space with a constant 2x2 Sigma, to (Y_h, Y_a) space with a
 * per-match Sigma derived from per-team variance scales:
 *
 *     sigma_team[k]  (K params, fixed-scale prior, NO hierarchy)
 *     rho_Y          (scalar within-match correlation of Y_h, Y_a)
 *
 *     Sigma_YHYA[n] = [[s_h^2,           rho_Y * s_h * s_a],
 *                      [rho_Y * s_h * s_a, s_a^2           ]]
 *
 * where s_h = sigma_team[team1[n]], s_a = sigma_team[team2[n]].
 *
 * The linear transform S = Y_h + Y_a, D = Y_h - Y_a induces a
 * per-match Sigma_SD:
 *
 *     Var(S)  = s_h^2 + s_a^2 + 2 rho_Y s_h s_a
 *     Var(D)  = s_h^2 + s_a^2 - 2 rho_Y s_h s_a
 *     Cov(S,D) = s_h^2 - s_a^2             (nonzero iff teams differ)
 *
 * Hypothesis: team-identity-dependent Sigma captures the match-dependent
 * covariance structure that v3's constant-Sigma Student-t cannot — the
 * same class of flexibility BVP gets from its strength_diff-dependent
 * mu3 term.
 *
 * Funnel risk mitigation: sigma_team is flat-prior (no hyperparameter
 * above it), which avoids the March 2026 direct_SD scale_sigma_def
 * inverse funnel. K ~ 68 extra parameters, low risk.
 *
 * Parameterisation note: we keep the mean structure identical to v3
 * (same off, def, home_advantage_{off,def}, mean_goals0) so the only
 * change is the likelihood family + covariance.
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
  array[N] vector[2] obs_YHYA;
  for (n in 1:N) {
    obs_YHYA[n][1] = goals1[n];
    obs_YHYA[n][2] = goals2[n];
  }

  matrix[K, N_rounds] delta_t;
  for (k in 1:K)
    for (r in 1:N_rounds)
      delta_t[k, r] = sqrt(time_between_matches[k, r]);
}

parameters {
  // Team-strength random walk (identical to v3)
  sum_to_zero_vector[K] off0;
  array[N_rounds] sum_to_zero_vector[K] z_off;
  real<lower=0> sigma_off;

  sum_to_zero_vector[K] def0;
  array[N_rounds] sum_to_zero_vector[K] z_def;
  real<lower=0> sigma_def;

  vector<lower=0>[K] home_advantage_off;
  vector<lower=0>[K] home_advantage_def;

  real mean_goals0;

  // NEW: per-team observation scale + scalar correlation on (Y_h, Y_a)
  vector<lower=0>[K] sigma_team;
  real<lower=-1, upper=1> rho_Y;

  real<lower=1> nu;
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

  // Per-team sigma: fixed-scale prior centred near Poisson-implied
  // SD (~1.25 given mean goals ~1.9). No hierarchy -> no funnel.
  sigma_team ~ normal(1.25, 0.5);
  rho_Y ~ normal(0, 0.5);

  nu ~ gamma(3, 0.15);

  for (n in 1:N) {
    real off_h = offense[round1[n], team1[n]] + home_advantage_off[team1[n]];
    real def_h = defense[round1[n], team1[n]] + home_advantage_def[team1[n]];
    real off_a = offense[round2[n], team2[n]];
    real def_a = defense[round2[n], team2[n]];

    vector[2] mu_n;
    mu_n[1] = mean_goals0 + off_h - def_a;
    mu_n[2] = mean_goals0 + off_a - def_h;

    real s_h = sigma_team[team1[n]];
    real s_a = sigma_team[team2[n]];
    matrix[2, 2] Sigma_n;
    Sigma_n[1, 1] = square(s_h);
    Sigma_n[2, 2] = square(s_a);
    Sigma_n[1, 2] = rho_Y * s_h * s_a;
    Sigma_n[2, 1] = Sigma_n[1, 2];

    target += multi_student_t_lpdf(obs_YHYA[n] | nu, mu_n, Sigma_n);
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

  // Per-match derived (S,D) covariance entries for downstream analytical
  // 1x2 calibration in R. Cheap to emit, saves re-derivation overhead.
  vector[N] mu_D_fit;
  vector[N] sigma_D_fit;

  vector[N] log_lik;
  array[N] int<lower=0> total_goals_rep;
  vector[N] goal_diff_rep;

  for (n in 1:N) {
    real off_h = offense[round1[n], team1[n]] + home_advantage_off[team1[n]];
    real def_h = defense[round1[n], team1[n]] + home_advantage_def[team1[n]];
    real off_a = offense[round2[n], team2[n]];
    real def_a = defense[round2[n], team2[n]];

    vector[2] mu_n;
    mu_n[1] = mean_goals0 + off_h - def_a;
    mu_n[2] = mean_goals0 + off_a - def_h;

    real s_h = sigma_team[team1[n]];
    real s_a = sigma_team[team2[n]];
    matrix[2, 2] Sigma_n;
    Sigma_n[1, 1] = square(s_h);
    Sigma_n[2, 2] = square(s_a);
    Sigma_n[1, 2] = rho_Y * s_h * s_a;
    Sigma_n[2, 1] = Sigma_n[1, 2];

    mu_D_fit[n] = mu_n[1] - mu_n[2];
    sigma_D_fit[n] = sqrt(square(s_h) + square(s_a)
                          - 2 * rho_Y * s_h * s_a);

    log_lik[n] = multi_student_t_lpdf(obs_YHYA[n] | nu, mu_n, Sigma_n);

    vector[2] yhya_draw = multi_student_t_rng(nu, mu_n, Sigma_n);
    total_goals_rep[n] = to_int(round(fmax(0, yhya_draw[1])))
                       + to_int(round(fmax(0, yhya_draw[2])));
    goal_diff_rep[n] = yhya_draw[1] - yhya_draw[2];
  }

  array[N_pred] int<lower=0> total_goals_pred;
  vector[N_pred] goal_diff_pred;

  for (n in 1:N_pred) {
    real off_h = offense[N_rounds, team1_pred[n]] + home_advantage_off[team1_pred[n]];
    real def_h = defense[N_rounds, team1_pred[n]] + home_advantage_def[team1_pred[n]];
    real off_a = offense[N_rounds, team2_pred[n]];
    real def_a = defense[N_rounds, team2_pred[n]];

    vector[2] mu_n;
    mu_n[1] = mean_goals0 + off_h - def_a;
    mu_n[2] = mean_goals0 + off_a - def_h;

    real s_h = sigma_team[team1_pred[n]];
    real s_a = sigma_team[team2_pred[n]];
    matrix[2, 2] Sigma_n;
    Sigma_n[1, 1] = square(s_h);
    Sigma_n[2, 2] = square(s_a);
    Sigma_n[1, 2] = rho_Y * s_h * s_a;
    Sigma_n[2, 1] = Sigma_n[1, 2];

    vector[2] yhya_draw = multi_student_t_rng(nu, mu_n, Sigma_n);
    total_goals_pred[n] = to_int(round(fmax(0, yhya_draw[1])))
                        + to_int(round(fmax(0, yhya_draw[2])));
    goal_diff_pred[n] = yhya_draw[1] - yhya_draw[2];
  }
}
