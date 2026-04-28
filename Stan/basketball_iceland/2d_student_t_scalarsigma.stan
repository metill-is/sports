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
  // ===== Offensive strengths (per-team random walk across rounds) =====

  // off0[k]: team k's offensive ability at round 1, in points-per-game relative to the
  // league mean (sum_to_zero_vector centres at 0). Sign +: above-average scorer.
  // |off0[k]| ~ 0-10 expected; normal(0, 10) is weakly informative and admits 2-sigma
  // outliers around +/- 20 without active shrinkage.
  sum_to_zero_vector[K] off0;

  // z_off[i, k]: standardised round-i innovation for the offensive random walk.
  // Dimensionless std_normal; the actual change per round is delta_t .* sigma_off .* z_off
  // (see transformed parameters). Sum-to-zero preserves the league-mean anchor.
  array[N_rounds] sum_to_zero_vector[K] z_off;

  // Non-centred lognormal hierarchy on per-team random-walk SD:
  //   sigma_off[k] = exp(mean_sigma_off + z_sigma_off[k] * scale_sigma_off).
  // mean_sigma_off ~ normal(-1.5, 2) on the log scale puts typical RW SD ~ exp(-1.5) ~ 0.22
  // points per sqrt-week (small round-to-round drift); scale_sigma_off ~ exponential(2)
  // (mean 0.5) governs between-team variation; z_sigma_off ~ std_normal are per-team draws.
  vector[K] z_sigma_off;
  real<lower = 0> scale_sigma_off;
  real mean_sigma_off;

  // ===== Defensive strengths (per-team random walk) =====

  // def0[k]: team k's defensive ability at round 1. Sign convention +: better defence
  // (reduces opponent's expected score, since defence enters as -def[opponent] in mu).
  // Same sum-to-zero, normal(0, 10) prior as offence -- both enter mu symmetrically.
  sum_to_zero_vector[K] def0;
  array[N_rounds] sum_to_zero_vector[K] z_def;

  // Defensive analogue of the offensive volatility hierarchy. mean_sigma_def ~ normal(-2, 2)
  // is shifted lower than offence (-1.5): empirically, defensive ability drifts more slowly
  // than offensive ability at the team-week level.
  vector[K] z_sigma_def;
  real<lower = 0> scale_sigma_def;
  real mean_sigma_def;

  // ===== Seasonal scoring environment (random walk + linear drift across seasons) =====

  // mean_goals0: season-1 league-wide expected points per team per game. normal(80, 15)
  // centres on the Icelandic top-division empirical mean (~80 pts/team/game); 2-sigma
  // admits rule-change regimes spanning 50-110.
  real mean_goals0;
  // delta_mean_goals: year-over-year drift in scoring. + means rising scoring (e.g. rule
  // changes that favour offence). normal(0, 10) admits ~+/-20 pts shift over two seasons.
  real delta_mean_goals;
  // sigma_mean_goals: residual SD around the seasonal linear trend. Strict +,
  // exponential(2) (mean 0.5) is a soft regulariser -- only large on sudden discontinuities.
  real<lower = 0> sigma_mean_goals;
  // z_mean_goals: standardised seasonal innovations; std_normal (actual shock per season
  // is sigma_mean_goals * z_mean_goals[s]).
  array[N_seasons - 1] real z_mean_goals;

  // ===== Home advantage (per team, strict positive) =====

  // home_advantage_off[k]: extra points team k scores at home above its road baseline.
  // Constrained >= 0 by substantive prior (no Icelandic team is *worse* at home).
  // Typical 3-5 pts; half-normal(0, 10) is wide enough for 15-pt outliers without aggressive
  // shrinkage.
  vector<lower = 0>[K] home_advantage_off;
  // home_advantage_def[k]: extra points team k *prevents* at home (>= 0, same
  // half-normal(0, 10)). Per-team total home edge in the model
  // = home_advantage_off + home_advantage_def.
  vector<lower = 0>[K] home_advantage_def;

  // --- Single scalar scoring variability ---
  // Replaces the former vector[K] sigma_team. Prior centred near observed
  // per-team score SD (Iceland basketball: ~13 points per team per game,
  // conditional on opposition already accounted for via offense/defense).
  // Strict +; normal(10, 5) is weakly informative -- allows posterior shrinkage if the
  // data demands but doesn't pin sigma to the prior mean.
  real<lower = 0> sigma;

  // ===== Heavy-tail degrees of freedom =====
  // nu: Student-t degrees of freedom. Constrained >= 1 to keep the variance defined.
  // gamma(3, 0.15) has mean 20 -- admits occasional blowouts (low nu) without committing
  // to either Gaussian (nu -> infinity) or extreme heavy tails (nu near 1).
  real<lower = 1> nu;

  // ===== Match-dependent score correlation =====
  // rho = 2 * inv_logit(alpha_rho + beta_rho * |dStrength| + beta2_rho * |totStrength|
  //                     + beta3_rho * (interaction)) - 1, in (-1, 1).
  // Story: lopsided games (large |dStrength|) and slow games (low |totStrength|) can
  // either break or reinforce the home/away score correlation; the 4-parameter logit
  // gives the data four levers without forcing a sign.

  // alpha_rho: baseline logit(rho). normal(0, 3) is wide on the logit scale -- the
  // posterior can centre rho anywhere in (-1, +1) before strength corrections kick in.
  real alpha_rho;
  // beta_rho: strength-gap modulation. normal(0, 0.1) is tight because |dStrength| can
  // span ~0-20 points; a unit-scale prior would let the gap dominate the rho range.
  real beta_rho;
  // beta2_rho: pace-of-game (|totStrength|) modulation. Same tight normal(0, 0.1) -- a
  // modest second-order effect is expected, not a primary driver.
  real beta2_rho;
  // beta3_rho: interaction (gap x pace). Tight normal(0, 0.1) -- third-order; smallest a
  // priori belief among the four ρ coefficients.
  real beta3_rho;
}

transformed parameters {
  // offense[i, k]: deterministic random-walk trajectory of team k's offensive ability
  // through round i, in points-per-game (same units as off0). Sum-to-zero across teams
  // at every i because each innovation is sum-to-zero.
  array[N_rounds] vector[K] offense;
  // sigma_off[k]: per-team random-walk SD on the natural (positive) scale, derived from
  // the lognormal hierarchy. Typical magnitude ~exp(-1.5) ~ 0.22 pts per sqrt-week given
  // the mean_sigma_off prior.
  vector<lower = 0>[K] sigma_off = exp(mean_sigma_off + z_sigma_off * scale_sigma_off);

  // defense[i, k]: defensive analogue of offense[i, k] -- same units, same sum-to-zero.
  array[N_rounds] vector[K] defense;
  // sigma_def[k]: per-team defensive RW SD (mean_sigma_def -> exp() ~ 0.14 pts/sqrt-week).
  vector<lower = 0>[K] sigma_def = exp(mean_sigma_def + z_sigma_def * scale_sigma_def);

  offense[1] = off0;
  defense[1] = def0;

  for (i in 2:N_rounds) {
    offense[i] = offense[i - 1] + delta_t[, i] .* sigma_off .* z_off[i];
    defense[i] = defense[i - 1] + delta_t[, i] .* sigma_def .* z_def[i];
  }

  // mean_goals[s]: league-wide expected points per team per game in season s, in points
  // (same units as mean_goals0). Built from a linear trend (delta_mean_goals) plus
  // standardised seasonal noise (sigma_mean_goals * z_mean_goals).
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
