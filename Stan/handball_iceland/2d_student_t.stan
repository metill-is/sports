/**
 * Input Data
 * 
 * The model requires match results and timing information:
 * - Match outcomes (goals scored by each team)
 * - Team identifiers
 * - Round/match timing information
 * - Season boundary indicators
 * - Prediction-related data
 */
data {
  int<lower=0> K;                     // Number of teams
  int<lower=0> N;                     // Number of games
  int<lower=0> N_rounds;              // Number of rounds
  int<lower=0> N_seasons;              // Number of seasons
  array[N] int<lower=1, upper=K> team1;    // Team 1 ID for each game (home)
  array[N] int<lower=1, upper=K> team2;    // Team 2 ID for each game (away)
  array[N] int<lower=1, upper = N_rounds> round1;  // Round ID for each game
  array[N] int<lower=1, upper = N_rounds> round2;  // Round ID for each game
  array[N] int<lower=1, upper = N_seasons> season;  // Season ID for each game
  matrix<lower = 0>[K, N_rounds] time_between_matches;  // Time difference between matches for each team and each round
  array[N] int<lower=0> goals1;       // Goals scored by   team 1 (home)
  array[N] int<lower=0> goals2;       // Goals scored by team 2 (away)
  array[N] int<lower=1> division;  // Division ID for each game
  
  // Prediction data
  int<lower = 0> N_top_teams;
  array[N_top_teams] int<lower=0> top_teams;
  vector[N_top_teams] time_to_next_games;

  int<lower=0> N_pred;                // Number of games to predict
  array[N_pred] int<lower=1, upper=K> team1_pred;  // Team 1 ID for each prediction game
  array[N_pred] int<lower=1, upper=K> team2_pred;  // Team 2 ID for each prediction game
  vector[N_pred] pred_timediff1;
  vector[N_pred] pred_timediff2;
  array[N_pred] int<lower=1> pred_division;  // Division ID for each prediction game
}

transformed data {
  array[2, N] int goals1_2;
  for (n in 1:N) {
    goals1_2[1, n] = goals1[n];
    goals1_2[2, n] = goals2[n];
  }
  matrix[K, N_rounds] delta_t;
  for (k in 1:K) {
    for (n in 1:N_rounds) {
      delta_t[k, n] = sqrt(time_between_matches[k, n]);
    }
  }

  vector[N_top_teams] delta_t_top = sqrt(time_to_next_games);
  vector[N_pred] pred_delta_t1 = sqrt(pred_timediff1);
  vector[N_pred] pred_delta_t2 = sqrt(pred_timediff2);

  matrix[K, N_rounds] rest_days;
  for (k in 1:K) {
    for (n in 1:N_rounds) {
      rest_days[k, n] = time_between_matches[k, n] <= 7 ? time_between_matches[k, n] : 7;
    }
  }

  vector[N_top_teams] rest_days_top;
  for (n in 1:N_top_teams) {
    rest_days_top[n] = time_to_next_games[n] <= 7 ? time_to_next_games[n] : 7;
  }
  vector[N_pred] pred_rest_days1;
  vector[N_pred] pred_rest_days2;
  for (n in 1:N_pred) {
    pred_rest_days1[n] = pred_timediff1[n] <= 7 ? pred_timediff1[n] : 7;
    pred_rest_days2[n] = pred_timediff2[n] <= 7 ? pred_timediff2[n] : 7;
  }
}

/**
 * Parameters
 * 
 * Hierarchical structure for team-specific parameters:
 * - Each team parameter has a population mean and scale
 * - Team-specific values are drawn from these populations
 * - This sharing of information helps with teams having fewer matches
 */
parameters {
  // ===== Offensive strengths (per-team random walk across rounds) =====

  // off0[k]: team k's offensive ability at round 1, in goals-per-game relative to the
  // league mean (sum_to_zero_vector centres at 0). Sign +: above-average scorer.
  // |off0[k]| ~ 0-5 typical in Icelandic top-division handball; normal(0, 10) is weakly
  // informative and admits 2-sigma outliers around +/- 20 goals without active shrinkage.
  sum_to_zero_vector[K] off0;

  // z_off[i, k]: standardised round-i innovation for the offensive RW. Dimensionless
  // std_normal; actual change per round = delta_t .* sigma_off .* z_off (see transformed
  // parameters). Sum-to-zero preserves the league-mean anchor.
  array[N_rounds] sum_to_zero_vector[K] z_off;

  // Non-centred lognormal hierarchy on per-team RW SD:
  //   sigma_off[k] = exp(mean_sigma_off + z_sigma_off[k] * scale_sigma_off).
  // mean_sigma_off ~ normal(-1.5, 2) on the log scale puts typical RW SD ~ exp(-1.5) ~ 0.22
  // goals per sqrt-week (small round-to-round drift); scale_sigma_off ~ exponential(2)
  // (mean 0.5) governs between-team variation; z_sigma_off ~ std_normal are per-team draws.
  vector[K] z_sigma_off;
  real<lower = 0> scale_sigma_off;
  real mean_sigma_off;

  // ===== Defensive strengths (per-team random walk) =====

  // def0[k]: team k's defensive ability at round 1. Sign convention +: better defence
  // (reduces opponent expected score, since defence enters as -def[opponent] in mu).
  // Same sum-to-zero, normal(0, 10) prior as offence -- both enter mu symmetrically.
  sum_to_zero_vector[K] def0;
  array[N_rounds] sum_to_zero_vector[K] z_def;

  // Defensive analogue of the offensive volatility hierarchy. mean_sigma_def ~ normal(-2, 2)
  // shifted lower than offence (-1.5): empirically, defensive ability drifts more slowly
  // than offensive ability at the team-week level.
  vector[K] z_sigma_def;
  real<lower = 0> scale_sigma_def;
  real mean_sigma_def;

  // ===== Seasonal scoring environment (random walk + linear drift across seasons) =====

  // mean_goals0: season-1 league-wide expected goals per team per game. normal(30, 10)
  // centres on the Icelandic handball top-division empirical mean (~28-30 goals/team/game)
  // with 2-sigma admitting rule-change regimes spanning ~10-50.
  real mean_goals0;
  // delta_mean_goals: year-over-year drift; + means rising scoring (rule changes, pace
  // evolution). normal(0, 10) admits ~+/- 20-goal shift over two seasons before the data
  // must override.
  real delta_mean_goals;
  // sigma_mean_goals: residual SD around the seasonal linear trend. Strict +,
  // exponential(2) (mean 0.5) is a soft regulariser -- only large on sudden discontinuities.
  real<lower = 0> sigma_mean_goals;
  // z_mean_goals: standardised seasonal innovations; std_normal.
  array[N_seasons - 1] real z_mean_goals;


  // ===== Home advantage (per team, strict positive) =====

  // home_advantage_off[k]: extra goals scored at home (>= 0 by substantive prior; no team
  // is *worse* at home). Typical 1-3 goals; half-normal(0, 10) is wide enough for ~5-goal
  // outliers without aggressive shrinkage.
  vector<lower = 0>[K] home_advantage_off;
  // home_advantage_def[k]: extra goals prevented at home (>= 0, same half-normal(0, 10)).
  // Per-team total home edge = home_advantage_off + home_advantage_def.
  vector<lower = 0>[K] home_advantage_def;

  // ===== Per-team scoring variability hierarchy =====
  // sigma_team[k] = exp(mean_sigma_team + z_sigma_team[k] * scale_sigma_team).
  // mean_sigma_team ~ normal(2, 2) puts typical team-level score SD ~ exp(2) ~ 7.4 goals,
  // close to the observed Icelandic handball per-team score SD of ~5-7 goals. Unlike
  // basketball's collapsed scalar sigma, handball retains the per-team structure because
  // dispersion across top-division handball teams varies meaningfully.
  vector[K] z_sigma_team;
  real<lower = 0> scale_sigma_team;
  real mean_sigma_team;

  // ===== Heavy-tail degrees of freedom =====
  // nu: Student-t degrees of freedom. Constrained >= 1 to keep variance defined.
  // gamma(3, 0.15) has mean 20 -- admits occasional blowouts (low nu) without forcing
  // Gaussian (nu -> infinity) or extreme heavy tails (nu near 1).
  real<lower = 1> nu;

  // ===== Score correlation =====
  // rho: correlation between home and away goals within a match, constant across all
  // matches (unlike basketball's match-dependent rho). uniform(-1, 1) is non-informative
  // -- the data alone drives the posterior. Substantively expected near zero or slightly
  // positive (high-pace games lift both scores).
  real<lower=-1, upper=1> rho;
}

/**
 * Transformed Parameters
 * 
 * Implements the core time series evolution:
 * 1. Transforms parameters to appropriate scales
 * 2. Evolves team strengths through time using random walks
 */
transformed parameters {
  // offense[i, k]: deterministic random-walk trajectory of team k's offensive ability
  // through round i, in goals-per-game (same units as off0). Sum-to-zero across teams
  // at every i because each innovation is sum-to-zero.
  array[N_rounds] vector[K] offense;
  // sigma_off[k]: per-team RW SD on the natural (positive) scale, derived from the
  // lognormal hierarchy. Typical magnitude ~exp(-1.5) ~ 0.22 goals per sqrt-week.
  vector<lower = 0>[K] sigma_off = exp(mean_sigma_off + z_sigma_off * scale_sigma_off);

  // defense[i, k]: defensive analogue of offense[i, k] -- same units, same sum-to-zero.
  array[N_rounds] vector[K] defense;
  // sigma_def[k]: per-team defensive RW SD (mean_sigma_def -> exp() ~ 0.14 goals/sqrt-week).
  vector<lower = 0>[K] sigma_def = exp(mean_sigma_def + z_sigma_def * scale_sigma_def);

  // Initialize first round
  offense[1, ] = off0;
  defense[1, ] = def0;

  // Remaining rounds follow random walk process
  for (i in 2:N_rounds) {
    offense[i, ] = offense[i - 1, ] + delta_t[ , i] .* sigma_off .* z_off[i, ];
    defense[i, ] = defense[i - 1, ] + delta_t[ , i] .* sigma_def .* z_def[i, ];
  }

  // sigma_team[k]: per-team scoring SD on the natural (positive) scale, derived from the
  // lognormal hierarchy on mean_sigma_team. Typical magnitude ~exp(2) ~ 7.4 goals -- this
  // is the per-team game-level scoring noise after offence/defence are accounted for.
  vector<lower=0>[K] sigma_team = exp(mean_sigma_team + scale_sigma_team * z_sigma_team);

  // mean_goals[s]: league-wide expected goals per team per game in season s, in goals
  // (same units as mean_goals0). Built from a linear trend (delta_mean_goals) plus
  // standardised seasonal noise (sigma_mean_goals * z_mean_goals).
  vector[N_seasons] mean_goals;
  mean_goals[1] = mean_goals0;
  for (i in 2:N_seasons) {
    mean_goals[i] = mean_goals[i - 1] + delta_mean_goals + sigma_mean_goals * z_mean_goals[i - 1];
  }

}

/**
 * Model
 * 
 * Specifies:
 * 1. Prior distributions
 * 2. Hierarchical structure
 * 3. Time series evolution
 * 4. Likelihood of observed scores
 */
model {
  // Priors for offensive parameters
  off0 ~ normal(0, 10);
  for (i in 1:N_rounds) {
    z_off[i] ~ std_normal();
  }
  
  // Priors for defensive parameters
  def0 ~ normal(0, 10);  
  for (i in 1:N_rounds) {
    z_def[i] ~ std_normal();
  }
  
  // Priors for volatility parameters
  z_sigma_off ~ std_normal();
  scale_sigma_off ~ exponential(2);
  mean_sigma_off ~ normal(-1.5, 2);

  z_sigma_def ~ std_normal();
  scale_sigma_def ~ exponential(2);
  mean_sigma_def ~ normal(-2, 2);
  
  // Priors for home advantage
  home_advantage_off ~ normal(0, 10);
  home_advantage_def ~ normal(0, 10);

  // Prior for team-specific variation in goals scored
  z_sigma_team ~ std_normal();
  scale_sigma_team ~ exponential(2);
  mean_sigma_team ~ normal(2, 2);

  // Prior for mean goals
  mean_goals0 ~ normal(30, 10);
  delta_mean_goals ~ normal(0, 10);
  sigma_mean_goals ~ exponential(2);
  z_mean_goals ~ std_normal();


  // Priors for scale, shape and correlation
  rho ~ uniform(-1, 1);
  nu ~ gamma(3, 0.15);

  for (n in 1:N) {
    vector[2] off;
    vector[2] def;
    vector[2] mu;
    matrix[2,2] Sigma;
    // Home team
    off[1] = offense[round1[n], team1[n]];
    def[1] = defense[round1[n], team1[n]];

    off[1] = off[1] + home_advantage_off[team1[n]];
    def[1] = def[1] + home_advantage_def[team1[n]];

    // Away team
    off[2] = offense[round2[n], team2[n]];
    def[2] = defense[round2[n], team2[n]];    

    // Expected goals
    mu[1] = mean_goals[season[n]] + off[1] - def[2];
    mu[2] = mean_goals[season[n]] + off[2] - def[1];
    

    // Create game-specific covariance matrix using team sigmas
    Sigma[1,1] = square(sigma_team[team1[n]]);
    Sigma[2,2] = square(sigma_team[team2[n]]);
    Sigma[1,2] = rho * sigma_team[team1[n]] * sigma_team[team2[n]];
    Sigma[2,1] = Sigma[1,2];
    
    [goals1[n], goals2[n]]' ~ multi_student_t(nu, mu, Sigma);
    
  }
}

/**
 * Generated Quantities
 * 
 * Computes:
 * 1. Current team strengths
 * 2. Predictions for future matches
 * 3. Various team strength summaries
 * 4. Home/away performance metrics
 */
generated quantities {

  // Per-match log-likelihood for loo::loo / PSIS-LOO. Added 2026-04-20 for
  // the scalarsigma vs tier1 elpd comparison (audit follow-up).
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
    Sigma_ll[1,1] = square(sigma_team[team1[n]]);
    Sigma_ll[2,2] = square(sigma_team[team2[n]]);
    Sigma_ll[1,2] = rho * sigma_team[team1[n]] * sigma_team[team2[n]];
    Sigma_ll[2,1] = Sigma_ll[1,2];
    log_lik[n] = multi_student_t_lpdf([goals1[n], goals2[n]]' | nu, mu_ll, Sigma_ll);
  }

  // Total home advantage
  vector[K] home_advantage_tot = home_advantage_off + home_advantage_def;

  // Current team strengths
  vector[K] cur_offense_away = offense[N_rounds, ];
  vector[K] cur_defense_away = defense[N_rounds, ];
  vector[K] cur_strength_away = cur_offense_away + cur_defense_away;

  // Current team strengths on home field
  vector[K] cur_offense_home = cur_offense_away + home_advantage_off; 
  vector[K] cur_defense_home = cur_defense_away + home_advantage_def;
  vector[K] cur_strength_home = cur_offense_home + cur_defense_home;

  vector[K] cur_offense = (cur_offense_away + cur_offense_home) / 2;
  vector[K] cur_defense = (cur_defense_away + cur_defense_home) / 2;
  vector[K] cur_strength = cur_offense + cur_defense;

  vector[N_pred] goals1_pred;
  vector[N_pred] goals2_pred;
  vector[N_pred] goal_diff_pred;       // Predicted goal difference
  vector[N_pred] total_goals_pred;     // Predicted total goals

  // Generate predictions for future games
  for (n in 1:N_pred) {
    vector[2] off;
    vector[2] def;
    vector[2] mu;
    matrix[2,2] Sigma;
    // Home team
    off[1] = offense[N_rounds, team1_pred[n]];
    def[1] = defense[N_rounds, team1_pred[n]];

    off[1] = off[1] + home_advantage_off[team1_pred[n]];
    def[1] = def[1] + home_advantage_def[team1_pred[n]];

    // Away team
    off[2] = offense[N_rounds, team2_pred[n]];
    def[2] = defense[N_rounds, team2_pred[n]];

    mu[1] = mean_goals[N_seasons] + off[1] - def[2];  
    mu[2] = mean_goals[N_seasons] + off[2] - def[1];
    
    // Create game-specific covariance matrix using team sigmas
    Sigma[1,1] = square(sigma_team[team1_pred[n]]);
    Sigma[2,2] = square(sigma_team[team2_pred[n]]);
    Sigma[1,2] = rho * sigma_team[team1_pred[n]] * sigma_team[team2_pred[n]];
    Sigma[2,1] = Sigma[1,2];

    vector[2] y = multi_student_t_rng(nu, mu, Sigma);
    goals1_pred[n] = y[1];
    goals2_pred[n] = y[2];
    goal_diff_pred[n] = goals1_pred[n] - goals2_pred[n];
    total_goals_pred[n] = goals1_pred[n] + goals2_pred[n];
  }
}

