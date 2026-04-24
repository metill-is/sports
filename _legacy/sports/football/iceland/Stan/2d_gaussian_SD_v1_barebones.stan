/**
 * Football (S, D) model — BAREBONES v1.
 *
 * Absolute minimum parameters that still capture:
 *   - Team discrimination (random-walk offense + defense per team)
 *   - Time variation (seasons change team ratings)
 *   - Home advantage (small but real)
 *   - (S, D) covariance (sigma_S, sigma_D; rho_SD free)
 *
 * What we've REMOVED vs direct_SD.stan:
 *   - Per-team sigma_off, sigma_def hierarchy
 *     -> SCALAR sigma_off, sigma_def. Eliminates the 2 * (K+2) params
 *        in that hierarchy and the scale_sigma_def funnel that caused
 *        max Rhat 1.63 / min ESS 8 in Phase 2.
 *   - Per-team home_advantage_off, home_advantage_def
 *     -> SCALAR home_advantage_off, home_advantage_def. From 2K to 2
 *        params. Iceland home-adv variance was small in the free fit.
 *   - Season-varying mean_goals RW
 *     -> SCALAR mean_goals0. Already in lean variant.
 *   - Student-t tail freedom (nu)
 *     -> Gaussian likelihood. Verified numerically that nu = 12 has
 *        ~nearly Gaussian density at the scales we see; t-tails aren't
 *        earning their complexity on N=3172 Iceland male matches.
 *
 * Vectorised multi_normal likelihood with single Sigma caches the
 * inverse / log-det once across all N matches.
 *
 * Parameter count drops by ~270 (from ~19k full Student-t to ~18.7k
 * here). The "interesting" non-innovation params drop from 20+ to 7:
 *   mean_goals0, sigma_off, sigma_def, home_advantage_off,
 *   home_advantage_def, sigma_S, sigma_D, rho_SD.
 *
 * The N_rounds x K random-walk innovation tensor (z_off, z_def) is
 * still the dominant parameter count — and it's structurally needed
 * for team-strength time variation. Cutting it would regress to a
 * static Maher 1982 model which we believe is worse.
 *
 * Next-up variants (work up from here):
 *   v2: free nu (Gaussian -> Student-t)
 *   v3: per-team home advantage
 *   v4: per-team sigma_off, sigma_def (+ hierarchical hyperprior)
 */

data {
  int<lower=0> K;
  int<lower=0> N;
  int<lower=0> N_rounds;
  int<lower=0> N_seasons;                      // kept for prep compatibility; unused
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
  // Team strengths — random walk with SCALAR innovation variance
  sum_to_zero_vector[K] off0;
  array[N_rounds] sum_to_zero_vector[K] z_off;
  real<lower=0> sigma_off;

  sum_to_zero_vector[K] def0;
  array[N_rounds] sum_to_zero_vector[K] z_def;
  real<lower=0> sigma_def;

  // Home advantage — SCALAR
  real<lower=0> home_advantage_off;
  real<lower=0> home_advantage_def;

  // Intercept — SCALAR
  real mean_goals0;

  // Direct (S, D) covariance
  real<lower=0> sigma_S;
  real<lower=0> sigma_D;
  real<lower=-1, upper=1> rho_SD;
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

  matrix[2, 2] Sigma;
  Sigma[1, 1] = square(sigma_S);
  Sigma[2, 2] = square(sigma_D);
  Sigma[1, 2] = rho_SD * sigma_S * sigma_D;
  Sigma[2, 1] = Sigma[1, 2];
}

model {
  // --- Priors ---
  off0 ~ normal(0, 1);
  def0 ~ normal(0, 1);
  for (i in 1:N_rounds) {
    z_off[i] ~ std_normal();
    z_def[i] ~ std_normal();
  }

  // Scalar innovation sigmas. March per-team posterior had
  // exp(mean_sigma_off) ~ 0.011, exp(mean_sigma_def) ~ 0.013
  // -> exponential(100) has mean 0.01, matches data scale.
  sigma_off ~ exponential(100);
  sigma_def ~ exponential(100);

  home_advantage_off ~ normal(0.15, 0.3);
  home_advantage_def ~ normal(0.15, 0.3);

  mean_goals0 ~ normal(1.3, 0.5);

  sigma_S ~ normal(1.5, 0.5);
  sigma_D ~ normal(1.8, 0.5);      // re-centred at March posterior
  rho_SD ~ normal(0, 0.5);

  // --- Likelihood (Gaussian, vectorised) ---
  {
    array[N] vector[2] mu_SD;
    for (n in 1:N) {
      real off_h = offense[round1[n], team1[n]] + home_advantage_off;
      real def_h = defense[round1[n], team1[n]] + home_advantage_def;
      real off_a = offense[round2[n], team2[n]];
      real def_a = defense[round2[n], team2[n]];

      mu_SD[n][1] = 2 * mean_goals0 + (off_h + off_a) - (def_h + def_a);
      mu_SD[n][2] = (off_h + def_h) - (off_a + def_a);
    }
    obs_SD ~ multi_normal(mu_SD, Sigma);
  }
}

generated quantities {
  real home_advantage_tot = home_advantage_off + home_advantage_def;

  vector[K] cur_offense_away = offense[N_rounds];
  vector[K] cur_defense_away = defense[N_rounds];
  vector[K] cur_strength_away = cur_offense_away + cur_defense_away;

  vector[K] cur_offense_home = cur_offense_away + home_advantage_off;
  vector[K] cur_defense_home = cur_defense_away + home_advantage_def;
  vector[K] cur_strength_home = cur_offense_home + cur_defense_home;

  vector[K] cur_offense = (cur_offense_away + cur_offense_home) / 2;
  vector[K] cur_defense = (cur_defense_away + cur_defense_home) / 2;
  vector[K] cur_strength = cur_offense + cur_defense;

  vector[N] log_lik;      // log density, cross-class comparison caveat
  array[N] int<lower=0> total_goals_rep;
  vector[N] goal_diff_rep;

  for (n in 1:N) {
    real off_h = offense[round1[n], team1[n]] + home_advantage_off;
    real def_h = defense[round1[n], team1[n]] + home_advantage_def;
    real off_a = offense[round2[n], team2[n]];
    real def_a = defense[round2[n], team2[n]];

    vector[2] mu_sd;
    mu_sd[1] = 2 * mean_goals0 + (off_h + off_a) - (def_h + def_a);
    mu_sd[2] = (off_h + def_h) - (off_a + def_a);

    log_lik[n] = multi_normal_lpdf([obs_SD[n][1], obs_SD[n][2]]' | mu_sd, Sigma);

    vector[2] sd_draw = multi_normal_rng(mu_sd, Sigma);
    total_goals_rep[n] = to_int(round(fmax(0, sd_draw[1])));
    goal_diff_rep[n] = sd_draw[2];
  }

  array[N_pred] int<lower=0> total_goals_pred;
  vector[N_pred] goal_diff_pred;

  for (n in 1:N_pred) {
    real off_h = offense[N_rounds, team1_pred[n]] + home_advantage_off;
    real def_h = defense[N_rounds, team1_pred[n]] + home_advantage_def;
    real off_a = offense[N_rounds, team2_pred[n]];
    real def_a = defense[N_rounds, team2_pred[n]];

    vector[2] mu_sd;
    mu_sd[1] = 2 * mean_goals0 + (off_h + off_a) - (def_h + def_a);
    mu_sd[2] = (off_h + def_h) - (off_a + def_a);

    vector[2] sd_draw = multi_normal_rng(mu_sd, Sigma);
    total_goals_pred[n] = to_int(round(fmax(0, sd_draw[1])));
    goal_diff_pred[n] = sd_draw[2];
  }
}
