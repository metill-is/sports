// National-team football model, second generation: Stan reference of the model
// that R/nt-model.R fits with R-INLA.
//
// Goals are independent Poisson. For a match between home side h and away side
// a in time step t:
//
//   log rate_h = mu + home_for     * venue + w * (theta[t, h] - theta[t, a]) + delta[h] + delta[a]
//   log rate_a = mu + home_against * venue - w * (theta[t, h] - theta[t, a]) + delta[h] + delta[a]
//
// theta is one overall strength per team: a static level plus a calendar
// random walk centred over time (the same prior as INLA's rw1 with a
// sum-to-zero constraint plus an iid level). delta is a small pooled static
// attack/defence split (attack = theta + delta, defence = theta - delta).
// The two venue terms are pooled and sign free, and apply only when the listed
// home side plays in its own country. w is 1, or kappa_friendly in a friendly
// when damp_friendly = 1 (here kappa is a parameter; the INLA fit profiles it).

functions {
  // Strength paths, T x K: level + centred random walk per team.
  matrix strength_paths(vector alpha, matrix z_raw, real sigma_u) {
    int T = rows(z_raw) + 1;
    int K = cols(z_raw);
    matrix[T, K] theta;
    for (k in 1:K) {
      vector[T] walk = append_row(0, cumulative_sum(z_raw[, k])) * sigma_u;
      theta[, k] = alpha[k] + walk - mean(walk);
    }
    return theta;
  }
}

data {
  int<lower=1> K;                              // teams
  int<lower=2> T;                              // time steps; T is the forecast origin
  int<lower=1> N;                              // training matches
  array[N] int<lower=1, upper=K> home;         // [N] home team index
  array[N] int<lower=1, upper=K> away;         // [N] away team index
  array[N] int<lower=1, upper=T> period;       // [N] time step of the match
  vector<lower=0, upper=1>[N] venue;           // [N] 1: home side plays in its own country
  vector<lower=0, upper=1>[N] friendly;        // [N] 1: friendly match
  array[N] int<lower=0> home_goals;            // [N]
  array[N] int<lower=0> away_goals;            // [N]
  int<lower=0, upper=1> damp_friendly;         // 1: estimate the friendly multiplier

  int<lower=0> N_pred;                         // fixtures to predict at the forecast origin
  array[N_pred] int<lower=1, upper=K> home_pred;   // [N_pred]
  array[N_pred] int<lower=1, upper=K> away_pred;   // [N_pred]
  vector<lower=0, upper=1>[N_pred] venue_pred;     // [N_pred]

  // PC-prior scales: P(sd > U) = 0.01, i.e. sd ~ exponential(-log(0.01) / U)
  real<lower=0> u_sigma_alpha;                 // 2 in the INLA fit
  real<lower=0> u_sigma_u;                     // 0.1 per time step in the INLA fit
  real<lower=0> u_sigma_delta;                 // 0.3 in the INLA fit
}

transformed data {
  array[N] int home_idx;                       // column-major index into theta
  array[N] int away_idx;
  for (n in 1:N) {
    home_idx[n] = (home[n] - 1) * T + period[n];
    away_idx[n] = (away[n] - 1) * T + period[n];
  }
}

parameters {
  real mu;
  real home_for;
  real home_against;
  real<lower=0> sigma_alpha;
  real<lower=0> sigma_u;                       // innovation sd per time step
  real<lower=0> sigma_delta;
  vector[K] alpha_raw;
  vector[K] delta_raw;
  matrix[T - 1, K] z_raw;
  array[damp_friendly] real<lower=0> kappa_friendly;
}

model {
  matrix[T, K] theta = strength_paths(sigma_alpha * alpha_raw, z_raw, sigma_u);
  vector[K] delta = sigma_delta * delta_raw;
  vector[N] gap = to_vector(theta)[home_idx] - to_vector(theta)[away_idx];
  vector[N] open = mu + delta[home] + delta[away];
  if (damp_friendly) {
    gap = gap .* (1 - friendly * (1 - kappa_friendly[1]));
    kappa_friendly ~ normal(1, 0.3);
  }

  mu ~ normal(0, 2);
  home_for ~ normal(0, 2);
  home_against ~ normal(0, 2);
  sigma_alpha ~ exponential(-log(0.01) / u_sigma_alpha);
  sigma_u ~ exponential(-log(0.01) / u_sigma_u);
  sigma_delta ~ exponential(-log(0.01) / u_sigma_delta);
  alpha_raw ~ std_normal();
  delta_raw ~ std_normal();
  to_vector(z_raw) ~ std_normal();

  home_goals ~ poisson_log(open + gap + home_for * venue);
  away_goals ~ poisson_log(open - gap + home_against * venue);
}

generated quantities {
  vector[K] theta_now;                         // strength at the forecast origin
  vector[K] delta = sigma_delta * delta_raw;
  array[N_pred] int home_goals_pred;
  array[N_pred] int away_goals_pred;
  {
    matrix[T, K] theta = strength_paths(sigma_alpha * alpha_raw, z_raw, sigma_u);
    theta_now = theta[T]';
  }
  for (n in 1:N_pred) {
    real gap = theta_now[home_pred[n]] - theta_now[away_pred[n]];
    real open = mu + delta[home_pred[n]] + delta[away_pred[n]];
    home_goals_pred[n] = poisson_log_rng(open + gap + home_for * venue_pred[n]);
    away_goals_pred[n] = poisson_log_rng(open - gap + home_against * venue_pred[n]);
  }
}
