  int<lower=1> K;                            // teams
  int<lower=1> NS;                           // seasons (consecutive, training and prediction rows together)
  int<lower=1> TS;                           // team-seasons that occur in the rows
  int<lower=2> W;                            // weeks in the within-season grid
  int<lower=1> ND;                           // divisions
  int<lower=1> N;
  array[N] int<lower=1, upper=K> home;
  array[N] int<lower=1, upper=K> away;
  array[N] int<lower=1, upper=NS> season;
  array[N] int<lower=1, upper=ND> div;
  array[N] int<lower=1, upper=W> wk;
  array[N] int<lower=1, upper=TS> ts_home;
  array[N] int<lower=1, upper=TS> ts_away;
  vector[N] y;
  int<lower=0> N_pred;
  array[N_pred] int<lower=1, upper=K> home_pred;
  array[N_pred] int<lower=1, upper=K> away_pred;
  array[N_pred] int<lower=1, upper=NS> season_pred;
  array[N_pred] int<lower=1, upper=ND> div_pred;
  array[N_pred] int<lower=1, upper=W> wk_pred;
  array[N_pred] int<lower=1, upper=TS> ts_home_pred;
  array[N_pred] int<lower=1, upper=TS> ts_away_pred;
  // Exponential rates on standard deviations: rate = -log(0.01) / U means P(sd > U) = 1 %,
  // which is R-INLA's pc.prec(U, 0.01) prior on the corresponding precision.
  real<lower=0> rate_sigma;
  real<lower=0> rate_b;
  real<lower=0> rate_u;
  real<lower=0> rate_w;
  real rho_mu;                               // prior mean of log((1 + rho) / (1 - rho))
  real<lower=0> rho_sd;
  // Within-season movement of a team's strength: 0 = none (constant within a season),
  // 1 = weekly random walk that sums to zero over the season, 2 = stationary weekly AR(1)
  // around the season level with the weekly correlation fixed at rho_week.
  int<lower=0, upper=2> within_mode;
  real<lower=0, upper=1> rho_week;
