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
  array[N] int<lower=0, upper=1> team1_home;  // Indicator whether team 1 is home
  array[N] int<lower=1, upper = N_rounds> round1;  // Round ID for each game
  array[N] int<lower=1, upper = N_rounds> round2;  // Round ID for each game
  array[N] int<lower=1, upper = N_seasons> season;  // Season ID for each game
  matrix<lower = 0>[K, N_rounds] time_between_matches;  // Time difference between matches for each team and each round
  array[N] int<lower=0> goals1;       // Goals scored by   team 1 (home)
  array[N] int<lower=0> goals2;       // Goals scored by team 2 (away)
  array[N] int<lower=1> division;  // Division ID for each game
  array[N] int<lower=0> casual;  // Indicator whether game is casual
  
  // Prediction data
  int<lower = 0> N_top_teams;
  array[N_top_teams] int<lower=0> top_teams;
  vector[N_top_teams] time_to_next_games;

  int<lower=0> N_pred;                // Number of games to predict
  array[N_pred] int<lower=1, upper=K> team1_pred;  // Team 1 ID for each prediction game
  array[N_pred] int<lower=1, upper=K> team2_pred;  // Team 2 ID for each prediction game
  array[N_pred] int<lower=0, upper=1> team1_home_pred;  // Indicator whether team 1 is home for each prediction game
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
  // Offensive parameters with hierarchical structure
  sum_to_zero_vector[K] off0;              // Initial offensive strengths
  array[N_rounds] sum_to_zero_vector[K] z_off;  // Innovations for offense
  
  vector[K] z_sigma_off;                   // Scale of random walk
  real<lower = 0> scale_sigma_off;
  real mean_sigma_off;

  // Defensive parameters
  sum_to_zero_vector[K] def0;              // Initial defensive strengths
  array[N_rounds] sum_to_zero_vector[K] z_def;  // Innovations for defense
  
  vector[K] z_sigma_def;                   // Scale of random walk
  real<lower = 0> scale_sigma_def;
  real mean_sigma_def;

  // Mean of log goals
  real mean_goals0;
  real delta_mean_goals;
  real<lower = 0> sigma_mean_goals;
  array[N_seasons - 1] real z_mean_goals;


  // Home advantage parameters
  vector<lower = 0>[K] home_advantage_off;
  vector<lower = 0>[K] home_advantage_def;  

  // Do teams play more relaxed for friendly international games?
  vector<lower = 0>[K] off_casual;
  vector<lower = 0>[K] def_casual;

  // Team-specific sigma parameters
  vector[K] z_sigma_team;        // Team-specific scoring variability
  real<lower = 0> scale_sigma_team;
  real mean_sigma_team;

  // Degrees of freedom for t-distribution
  real<lower = 1> nu;                      // Degrees of freedom for t-distribution
  
  // Correlation between home and away goals
  real<lower=-1, upper=1> rho;            // Correlation between home and away goals
}

/**
 * Transformed Parameters
 * 
 * Implements the core time series evolution:
 * 1. Transforms parameters to appropriate scales
 * 2. Evolves team strengths through time using random walks
 */
transformed parameters {
  // Offensive parameters over time
  array[N_rounds] vector[K] offense;        // Offensive strengths for each round
  vector<lower = 0>[K] sigma_off = exp(mean_sigma_off + z_sigma_off * scale_sigma_off);

  // Defensive parameters over time
  array[N_rounds] vector[K] defense;        // Defensive strengths for each round
  vector<lower = 0>[K] sigma_def = exp(mean_sigma_def + z_sigma_def * scale_sigma_def);

  // Initialize first round
  offense[1, ] = off0;
  defense[1, ] = def0;

  // Remaining rounds follow random walk process
  for (i in 2:N_rounds) {
    offense[i, ] = offense[i - 1, ] + delta_t[ , i] .* sigma_off .* z_off[i, ];
    defense[i, ] = defense[i - 1, ] + delta_t[ , i] .* sigma_def .* z_def[i, ];
  }

  vector<lower=0>[K] sigma_team = exp(mean_sigma_team + scale_sigma_team * z_sigma_team);

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
  home_advantage_off ~ normal(0, 20);
  home_advantage_def ~ normal(0, 20);

  // Prior for team-specific variation in goals scored
  z_sigma_team ~ std_normal();
  scale_sigma_team ~ exponential(2);
  mean_sigma_team ~ normal(2, 2);

  // Prior for friendly parameter
  off_casual ~ normal(0, 20);
  def_casual ~ normal(0, 20);

  // Prior for mean goals
  mean_goals0 ~ normal(80, 10);
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

    if (team1_home[n] == 1) {
      off[1] = off[1] + home_advantage_off[team1[n]];
      def[1] = def[1] + home_advantage_def[team1[n]];
    }

    // Away team
    off[2] = offense[round2[n], team2[n]];
    def[2] = defense[round2[n], team2[n]];
    
    if (casual[n] == 1) {
      off[1] = off[1] - off_casual[team1[n]];
      def[1] = def[1] - def_casual[team1[n]];
      off[2] = off[2] - off_casual[team2[n]];
      def[2] = def[2] - def_casual[team2[n]];
    }
    

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

    if (team1_home_pred[n] == 1) {
      off[1] = off[1] + home_advantage_off[team1_pred[n]];
      def[1] = def[1] + home_advantage_def[team1_pred[n]];
    }
 
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

