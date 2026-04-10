/**
 * Bivariate Student-t Football Model
 *
 * Models (total_goals, goal_difference) as bivariate Student-t, replacing
 * the diagonal-inflated bivariate Poisson. Team strengths evolve via random
 * walks on a linear scale — the same off/def parameters combine additively
 * to give E[total_goals] and E[goal_diff]:
 *
 *   E[Y_h] = mu + off_h - def_a + home_h
 *   E[Y_a] = mu + off_a - def_h
 *
 *   E[S] = E[Y_h] + E[Y_a] = 2*mu + (off_h + off_a) - (def_h + def_a) + home_h
 *   E[D] = E[Y_h] - E[Y_a] = (off_h + def_h) - (off_a + def_a) + home_h
 *
 * Key features:
 * - Linear-scale team strengths: off_k, def_k directly in "goals" units
 * - E[D] depends on total strength s_k = off_k + def_k (quality)
 * - E[S] depends on net attacking surplus across both teams (pace/style)
 * - Match-dependent covariance:
 *     * Team-specific sigma_k controls how variable that team's games are
 *     * Var(S) = sigma_{k1}^2 + sigma_{k2}^2 + 2*rho*sigma_{k1}*sigma_{k2}
 *     * Var(D) = sigma_{k1}^2 + sigma_{k2}^2 - 2*rho*sigma_{k1}*sigma_{k2}
 *     * Cov(S,D) = sigma_{k1}^2 - sigma_{k2}^2
 *     * rho depends on strength_diff and total_strength (as in handball model)
 * - Student-t tails capture blowouts and surprise results
 * - Discretization in generated quantities for downstream betting pipeline
 *
 * The covariance structure is derived from the (Y_h, Y_a) parameterization:
 *   If Y_h ~ (sigma_{k1}^2) and Y_a ~ (sigma_{k2}^2) with Corr(Y_h, Y_a) = rho,
 *   then S = Y_h + Y_a and D = Y_h - Y_a have the covariance shown above.
 *   This means:
 *     - Var(S) > Var(D) when rho > 0 (positive goal correlation → totals more variable)
 *     - Cov(S,D) > 0 when home team is more variable (home team drives both totals and diff)
 *     - For balanced sigma_{k1} ≈ sigma_{k2}, Cov(S,D) ≈ 0 (totals and diff nearly independent)
 */

data {
  int<lower=0> K;                     // Number of teams
  int<lower=0> N;                     // Number of games
  int<lower=0> N_rounds;              // Number of rounds
  int<lower=0> N_seasons;             // Number of seasons
  array[N] int<lower=1, upper=K> team1;    // Home team ID
  array[N] int<lower=1, upper=K> team2;    // Away team ID
  array[N] int<lower=1, upper=N_rounds> round1;
  array[N] int<lower=1, upper=N_rounds> round2;
  array[N] int<lower=1, upper=N_seasons> season;
  matrix<lower=0>[K, N_rounds] time_between_matches;
  array[N] int<lower=0> goals1;       // Home goals
  array[N] int<lower=0> goals2;       // Away goals
  array[N] int<lower=1> division;     // Division ID (for shared data prep)

  // Prediction data
  int<lower=0> N_top_teams;
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
  // Precompute observed (S, D) for likelihood
  vector[N] obs_total;
  vector[N] obs_diff;
  for (n in 1:N) {
    obs_total[n] = goals1[n] + goals2[n];
    obs_diff[n] = goals1[n] - goals2[n];
  }

  // Precompute sqrt(delta_t) for random walk
  matrix[K, N_rounds] delta_t;
  for (k in 1:K)
    for (r in 1:N_rounds)
      delta_t[k, r] = sqrt(time_between_matches[k, r]);
}

parameters {
  // Offensive strengths (linear scale, "goals" units)
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

  // Mean goals per team per match (time-varying across seasons)
  real mean_goals0;
  real delta_mean_goals;
  real<lower=0> sigma_mean_goals;
  array[N_seasons - 1] real z_mean_goals;

  // Home advantage (separate off/def components)
  vector<lower=0>[K] home_advantage_off;
  vector<lower=0>[K] home_advantage_def;

  // Team-specific scoring variability
  // sigma_team[k] controls how much variance team k contributes to any match
  vector[K] z_sigma_team;
  real<lower=0> scale_sigma_team;
  real mean_sigma_team;

  // Student-t degrees of freedom
  real<lower=1> nu;

  // Correlation between Y_h and Y_a (mapped to [-1,1])
  // Depends on match characteristics via logit link
  real alpha_rho;
  real beta_rho;         // effect of strength difference
  real beta2_rho;        // effect of total strength
  real beta3_rho;        // interaction
}

transformed parameters {
  // Time-evolving team strengths
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

  // Team-specific match variability
  vector<lower=0>[K] sigma_team = exp(mean_sigma_team + scale_sigma_team * z_sigma_team);

  // Time-varying mean goals (random walk across seasons)
  vector[N_seasons] mean_goals;
  mean_goals[1] = mean_goals0;
  for (i in 2:N_seasons)
    mean_goals[i] = mean_goals[i-1] + delta_mean_goals + sigma_mean_goals * z_mean_goals[i-1];
}

model {
  // --- Priors ---

  // Team strengths
  off0 ~ normal(0, 1);
  def0 ~ normal(0, 1);
  for (i in 1:N_rounds) {
    z_off[i] ~ std_normal();
    z_def[i] ~ std_normal();
  }

  // Random walk volatility
  z_sigma_off ~ std_normal();
  scale_sigma_off ~ exponential(2);
  mean_sigma_off ~ normal(-4, 2);

  z_sigma_def ~ std_normal();
  scale_sigma_def ~ exponential(2);
  mean_sigma_def ~ normal(-5, 2);

  // Home advantage
  home_advantage_off ~ normal(0.15, 0.3);
  home_advantage_def ~ normal(0.15, 0.3);

  // Mean goals per team (~1.3 goals → ~2.6 per match)
  mean_goals0 ~ normal(1.3, 0.5);
  delta_mean_goals ~ normal(0, 0.1);
  sigma_mean_goals ~ exponential(2);
  z_mean_goals ~ std_normal();

  // Team-specific variability
  // Football: sigma_team ~ 1.0-1.5 goals typically
  z_sigma_team ~ std_normal();
  scale_sigma_team ~ exponential(2);
  mean_sigma_team ~ normal(0, 1);

  // Student-t degrees of freedom
  nu ~ gamma(3, 0.15);

  // Correlation parameters
  alpha_rho ~ normal(0, 1);
  beta_rho ~ normal(0, 0.3);
  beta2_rho ~ normal(0, 0.3);
  beta3_rho ~ normal(0, 0.3);

  // --- Likelihood ---
  //
  // We model (Y_h, Y_a) with team-specific sigma and match-dependent rho,
  // then transform to (S, D) = (Y_h + Y_a, Y_h - Y_a) for the likelihood.
  //
  // Var(Y_h) = sigma_{k1}^2,  Var(Y_a) = sigma_{k2}^2,  Corr(Y_h,Y_a) = rho
  //
  // Under the linear transform [S; D] = [[1,1];[1,-1]] [Y_h; Y_a]:
  //   Sigma_SD = A * Sigma_YY * A'
  //   Var(S)   = s1^2 + s2^2 + 2*rho*s1*s2
  //   Var(D)   = s1^2 + s2^2 - 2*rho*s1*s2
  //   Cov(S,D) = s1^2 - s2^2
  //
  // This is elegant: Cov(S,D) depends only on the variance imbalance between
  // the two teams, not on rho. When teams are equally variable, S and D are
  // uncorrelated regardless of the goal correlation structure.

  for (n in 1:N) {
    // Team strengths at match time
    real off_h = offense[round1[n], team1[n]] + home_advantage_off[team1[n]];
    real def_h = defense[round1[n], team1[n]] + home_advantage_def[team1[n]];
    real off_a = offense[round2[n], team2[n]];
    real def_a = defense[round2[n], team2[n]];

    // Expected individual goals (linear scale)
    // mu_h = mean_goals + off_h - def_a
    // mu_a = mean_goals + off_a - def_h
    real mu = mean_goals[season[n]];

    // Expected total goals and goal difference
    vector[2] mu_sd;
    mu_sd[1] = 2 * mu + (off_h + off_a) - (def_h + def_a);  // E[S]
    mu_sd[2] = (off_h + def_h) - (off_a + def_a);            // E[D]

    // Match-dependent correlation
    real strength_diff = abs((off_h + def_h) - (off_a + def_a));
    real total_strength = abs((off_h + def_h) + (off_a + def_a));
    real logit_rho = alpha_rho
      + beta_rho * strength_diff
      + beta2_rho * total_strength
      + beta3_rho * strength_diff * total_strength;
    real rho = 2 * inv_logit(logit_rho) - 1;

    // Team-specific sigmas
    real s1 = sigma_team[team1[n]];
    real s2 = sigma_team[team2[n]];

    // Build Sigma_SD via the linear transformation
    matrix[2,2] Sigma;
    Sigma[1,1] = square(s1) + square(s2) + 2 * rho * s1 * s2;  // Var(S)
    Sigma[2,2] = square(s1) + square(s2) - 2 * rho * s1 * s2;  // Var(D)
    Sigma[1,2] = square(s1) - square(s2);                       // Cov(S,D)
    Sigma[2,1] = Sigma[1,2];

    [obs_total[n], obs_diff[n]]' ~ multi_student_t(nu, mu_sd, Sigma);
  }
}

generated quantities {
  // Total home advantage
  vector[K] home_advantage_tot = home_advantage_off + home_advantage_def;

  // Current team strengths (away = neutral)
  vector[K] cur_offense_away = offense[N_rounds];
  vector[K] cur_defense_away = defense[N_rounds];
  vector[K] cur_strength_away = cur_offense_away + cur_defense_away;

  // Current team strengths at home
  vector[K] cur_offense_home = cur_offense_away + home_advantage_off;
  vector[K] cur_defense_home = cur_defense_away + home_advantage_def;
  vector[K] cur_strength_home = cur_offense_home + cur_defense_home;

  // Average (home + away) / 2
  vector[K] cur_offense = (cur_offense_away + cur_offense_home) / 2;
  vector[K] cur_defense = (cur_defense_away + cur_defense_home) / 2;
  vector[K] cur_strength = cur_offense + cur_defense;

  // Posterior predictive draws — discretized to integer goal counts
  // for compatibility with downstream betting pipeline
  array[N_pred] int<lower=0> goals1_pred;
  array[N_pred] int<lower=0> goals2_pred;
  array[N_pred] int goal_diff_pred;
  array[N_pred] int<lower=0> total_goals_pred;

  for (n in 1:N_pred) {
    real off_h = offense[N_rounds, team1_pred[n]] + home_advantage_off[team1_pred[n]];
    real def_h = defense[N_rounds, team1_pred[n]] + home_advantage_def[team1_pred[n]];
    real off_a = offense[N_rounds, team2_pred[n]];
    real def_a = defense[N_rounds, team2_pred[n]];

    vector[2] mu_sd;
    mu_sd[1] = 2 * mean_goals[N_seasons] + (off_h + off_a) - (def_h + def_a);
    mu_sd[2] = (off_h + def_h) - (off_a + def_a);

    real strength_diff = abs((off_h + def_h) - (off_a + def_a));
    real total_strength = abs((off_h + def_h) + (off_a + def_a));
    real logit_rho = alpha_rho
      + beta_rho * strength_diff
      + beta2_rho * total_strength
      + beta3_rho * strength_diff * total_strength;
    real rho = 2 * inv_logit(logit_rho) - 1;

    real s1 = sigma_team[team1_pred[n]];
    real s2 = sigma_team[team2_pred[n]];

    matrix[2,2] Sigma;
    Sigma[1,1] = square(s1) + square(s2) + 2 * rho * s1 * s2;
    Sigma[2,2] = square(s1) + square(s2) - 2 * rho * s1 * s2;
    Sigma[1,2] = square(s1) - square(s2);
    Sigma[2,1] = Sigma[1,2];

    // Draw continuous (S, D) from bivariate Student-t
    vector[2] draw = multi_student_t_rng(nu, mu_sd, Sigma);

    // Discretize: round to nearest integers, enforce constraints
    int S = to_int(round(fmax(0, draw[1])));
    int D = to_int(round(draw[2]));

    // Enforce parity: S and D must both be even or both be odd
    // (since S + D = 2*Y_h and S - D = 2*Y_a must both be even)
    if ((S + D) % 2 != 0) {
      // Nudge D toward zero (conservative choice)
      if (D > 0) D -= 1;
      else if (D < 0) D += 1;
      else S += 1;
    }

    // |D| cannot exceed S (can't score negative goals)
    if (D > S) D = S;
    if (D < -S) D = -S;

    goals1_pred[n] = (S + D) / 2;
    goals2_pred[n] = (S - D) / 2;
    total_goals_pred[n] = S;
    goal_diff_pred[n] = D;
  }
}
