/**
 * Bivariate Student-t basketball model — scalar sigma variant.
 *
 * Drops the per-team sigma_team parameterisation in favour of a single
 * scalar `sigma`. Audit of the 2026-04-18 fit (K=30 teams) showed posterior
 * CV of sigma_team[k] was 1.0% — the per-team structure was statistically
 * unidentifiable. Collapsing avoids 30+ wasted parameters and lets the
 * per-match covariance matrix share its diagonal across all matches.
 *
 * rho is still match-dependent via the 4-parameter logit model, so Sigma
 * still varies per match and the likelihood remains in a per-match loop.
 * A separate variant could additionally collapse rho to scalar and
 * vectorise the likelihood entirely.
 *
 * Companion to: Knowledge/Sports Models/audit-stan-models-2026-04-19.md §10.1
 */
data {
  int<lower=0> K;                     // Number of teams
  int<lower=0> N;                     // Number of games
  int<lower=0> N_rounds;              // Number of rounds
  int<lower=0> N_seasons;             // Number of seasons
  array[N] int<lower=1, upper=K> team1;
  array[N] int<lower=1, upper=K> team2;
  array[N] int<lower=1, upper = N_rounds> round1;
  array[N] int<lower=1, upper = N_rounds> round2;
  array[N] int<lower=1, upper = N_seasons> season;
  matrix<lower = 0>[K, N_rounds] time_between_matches;
  array[N] int<lower=0> goals1;
  array[N] int<lower=0> goals2;
  array[N] int<lower=1> division;

  // Prediction data
  int<lower = 0> N_top_teams;
  array[N_top_teams] int<lower=0> top_teams;
  vector[N_top_teams] time_to_next_games;

  int<lower=0> N_pred;
  array[N_pred] int<lower=1, upper=K> team1_pred;
  array[N_pred] int<lower=1, upper=K> team2_pred;
  vector[N_pred] pred_timediff1;
  vector[N_pred] pred_timediff2;
  array[N_pred] int<lower=1> pred_division;
}

transformed data {
  matrix[K, N_rounds] delta_t;
  for (k in 1:K)
    for (n in 1:N_rounds)
      delta_t[k, n] = sqrt(time_between_matches[k, n]);
}

parameters {
  // Offensive strengths
  sum_to_zero_vector[K] off0;
  array[N_rounds] sum_to_zero_vector[K] z_off;
  vector[K] z_sigma_off;
  real<lower = 0> scale_sigma_off;
  real mean_sigma_off;

  // Defensive strengths
  sum_to_zero_vector[K] def0;
  array[N_rounds] sum_to_zero_vector[K] z_def;
  vector[K] z_sigma_def;
  real<lower = 0> scale_sigma_def;
  real mean_sigma_def;

  // Seasonal mean goals
  real mean_goals0;
  real delta_mean_goals;
  real<lower = 0> sigma_mean_goals;
  array[N_seasons - 1] real z_mean_goals;

  // Home advantage
  vector<lower = 0>[K] home_advantage_off;
  vector<lower = 0>[K] home_advantage_def;

  // --- Single scalar scoring variability ---
  // Replaces the former vector[K] sigma_team. Prior centred near observed
  // per-team score SD (Iceland basketball: ~13 points per team per game,
  // conditional on opposition already accounted for via offense/defense).
  real<lower = 0> sigma;

  // Degrees of freedom
  real<lower = 1> nu;

  // Correlation between home and away goals (match-dependent)
  real alpha_rho;
  real beta_rho;
  real beta2_rho;
  real beta3_rho;
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

  vector[N_seasons] mean_goals;
  mean_goals[1] = mean_goals0;
  for (i in 2:N_seasons)
    mean_goals[i] = mean_goals[i - 1] + delta_mean_goals + sigma_mean_goals * z_mean_goals[i - 1];
}

model {
  off0 ~ normal(0, 10);
  def0 ~ normal(0, 10);
  for (i in 1:N_rounds) {
    z_off[i] ~ std_normal();
    z_def[i] ~ std_normal();
  }

  z_sigma_off ~ std_normal();
  scale_sigma_off ~ exponential(2);
  mean_sigma_off ~ normal(-1.5, 2);

  z_sigma_def ~ std_normal();
  scale_sigma_def ~ exponential(2);
  mean_sigma_def ~ normal(-2, 2);

  home_advantage_off ~ normal(0, 10);
  home_advantage_def ~ normal(0, 10);

  // Scalar sigma: weakly informative centred near observed score SD.
  sigma ~ normal(10, 5);

  // Re-centred mean_goals0 prior (see base file comment).
  mean_goals0 ~ normal(80, 15);
  delta_mean_goals ~ normal(0, 10);
  sigma_mean_goals ~ exponential(2);
  z_mean_goals ~ std_normal();

  alpha_rho ~ normal(0, 3);
  beta_rho ~ normal(0, 0.1);
  beta2_rho ~ normal(0, 0.1);
  beta3_rho ~ normal(0, 0.1);
  nu ~ gamma(3, 0.15);

  for (n in 1:N) {
    vector[2] off;
    vector[2] def;
    vector[2] mu;
    matrix[2, 2] Sigma;

    off[1] = offense[round1[n], team1[n]] + home_advantage_off[team1[n]];
    def[1] = defense[round1[n], team1[n]] + home_advantage_def[team1[n]];
    off[2] = offense[round2[n], team2[n]];
    def[2] = defense[round2[n], team2[n]];

    mu[1] = mean_goals[season[n]] + off[1] - def[2];
    mu[2] = mean_goals[season[n]] + off[2] - def[1];

    real strength_diff = abs(off[1] + def[1] - off[2] - def[2]);
    real total_strength = abs(off[1] + def[1] + off[2] + def[2]);
    real logit_rho = alpha_rho + beta_rho * strength_diff
                   + beta2_rho * total_strength
                   + beta3_rho * total_strength * strength_diff;
    real rho = 2 * inv_logit(logit_rho) - 1;

    // Scalar sigma — same for both teams in every match.
    Sigma[1, 1] = square(sigma);
    Sigma[2, 2] = square(sigma);
    Sigma[1, 2] = rho * square(sigma);
    Sigma[2, 1] = Sigma[1, 2];

    [goals1[n], goals2[n]]' ~ multi_student_t(nu, mu, Sigma);
  }
}

generated quantities {
  // Per-match log-likelihood for loo::loo / PSIS-LOO. Mirrors the model block
  // with scalar sigma. Added 2026-04-20 for the scalarsigma vs tier1 elpd
  // comparison (audit follow-up).
  vector[N] log_lik;
  for (n in 1:N) {
    vector[2] off_ll;
    vector[2] def_ll;
    vector[2] mu_ll;
    matrix[2, 2] Sigma_ll;
    off_ll[1] = offense[round1[n], team1[n]] + home_advantage_off[team1[n]];
    def_ll[1] = defense[round1[n], team1[n]] + home_advantage_def[team1[n]];
    off_ll[2] = offense[round2[n], team2[n]];
    def_ll[2] = defense[round2[n], team2[n]];
    mu_ll[1] = mean_goals[season[n]] + off_ll[1] - def_ll[2];
    mu_ll[2] = mean_goals[season[n]] + off_ll[2] - def_ll[1];
    real sdiff_ll = abs(off_ll[1] + def_ll[1] - off_ll[2] - def_ll[2]);
    real stot_ll  = abs(off_ll[1] + def_ll[1] + off_ll[2] + def_ll[2]);
    real lrho_ll  = alpha_rho + beta_rho * sdiff_ll + beta2_rho * stot_ll + beta3_rho * stot_ll * sdiff_ll;
    real rho_ll   = 2 * inv_logit(lrho_ll) - 1;
    Sigma_ll[1,1] = square(sigma);
    Sigma_ll[2,2] = square(sigma);
    Sigma_ll[1,2] = rho_ll * square(sigma);
    Sigma_ll[2,1] = Sigma_ll[1,2];
    log_lik[n] = multi_student_t_lpdf([goals1[n], goals2[n]]' | nu, mu_ll, Sigma_ll);
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

  vector[N_pred] goals1_pred;
  vector[N_pred] goals2_pred;
  vector[N_pred] goal_diff_pred;
  vector[N_pred] total_goals_pred;

  for (n in 1:N_pred) {
    vector[2] off;
    vector[2] def;
    vector[2] mu;
    matrix[2, 2] Sigma;

    off[1] = offense[N_rounds, team1_pred[n]] + home_advantage_off[team1_pred[n]];
    def[1] = defense[N_rounds, team1_pred[n]] + home_advantage_def[team1_pred[n]];
    off[2] = offense[N_rounds, team2_pred[n]];
    def[2] = defense[N_rounds, team2_pred[n]];

    mu[1] = mean_goals[N_seasons] + off[1] - def[2];
    mu[2] = mean_goals[N_seasons] + off[2] - def[1];

    real strength_diff = abs(off[1] + def[1] - off[2] - def[2]);
    real total_strength = abs(off[1] + def[1] + off[2] + def[2]);
    real logit_rho = alpha_rho + beta_rho * strength_diff
                   + beta2_rho * total_strength
                   + beta3_rho * total_strength * strength_diff;
    real rho = 2 * inv_logit(logit_rho) - 1;

    Sigma[1, 1] = square(sigma);
    Sigma[2, 2] = square(sigma);
    Sigma[1, 2] = rho * square(sigma);
    Sigma[2, 1] = Sigma[1, 2];

    vector[2] y = multi_student_t_rng(nu, mu, Sigma);
    goals1_pred[n] = y[1];
    goals2_pred[n] = y[2];
    goal_diff_pred[n] = goals1_pred[n] - goals2_pred[n];
    total_goals_pred[n] = goals1_pred[n] + goals2_pred[n];
  }
}
