// v2 total model. total = home score + away score ~ normal(eta, sigma), with
//   eta = alpha[division] + s[division, season] + phi[home] + phi[away],
//   phi[team, date] = b[team] + u[team, season] + w[team-season, week].
// alpha is the division's scoring level, s a random walk over seasons per division
// (sum to zero), phi a team's contribution to the pace of a match.
data {
#include _data_v2.stan
  real<lower=0> rate_season;
  real level_mean;
  real<lower=0> level_sd;
}
transformed data {
#include _tdata_v2.stan
}
parameters {
  vector[ND] alpha;
  array[ND] sum_to_zero_vector[NS] s_season;
  real<lower=0> sigma;                       // sd of a match total around its mean
  real<lower=0> sigma_b;
  real<lower=0> sigma_u;
  real theta_rho;
  array[within_mode > 0] real<lower=0> sigma_w;   // within-season sd: per week (mode 1) or marginal (mode 2)
  real<lower=0> sigma_season;                // sd of the season-to-season step in a division's scoring level
  vector[K] b_z;
  matrix[K, NS] u_z;
  matrix[within_mode > 0 ? TS : 0, within_mode == 1 ? W - 1 : W] w_z;
}
transformed parameters {
#include _states_v2.stan
  matrix[ND, NS] level;                      // division scoring level by season
  vector[N] eta;
  for (d in 1:ND) level[d] = alpha[d] + s_season[d]';
  {
    vector[K * NS] uv = to_vector(u);
    vector[TS * W] wv = to_vector(w);
    vector[ND * NS] lv = to_vector(level);
    for (n in 1:N)
      eta[n] = lv[(season[n] - 1) * ND + div[n]] + b[home[n]] + b[away[n]] + uv[iu_home[n]] + uv[iu_away[n]]
               + wv[iw_home[n]] + wv[iw_away[n]];
  }
}
model {
  alpha ~ normal(level_mean, level_sd);
  for (d in 1:ND) (s_season[d][2:NS] - s_season[d][1:(NS - 1)]) ~ normal(0, sigma_season);
  sigma ~ exponential(rate_sigma);
  sigma_b ~ exponential(rate_b);
  sigma_u ~ exponential(rate_u);
  sigma_w ~ exponential(rate_w);
  sigma_season ~ exponential(rate_season);
  theta_rho ~ normal(rho_mu, rho_sd);
  b_z ~ std_normal();
  to_vector(u_z) ~ std_normal();
  to_vector(w_z) ~ std_normal();
  y ~ normal(eta, sigma);
}
generated quantities {
  vector[N] log_lik;
  vector[N_pred] mu_pred;
  for (n in 1:N) log_lik[n] = normal_lpdf(y[n] | eta[n], sigma);
  {
    vector[K * NS] uv = to_vector(u);
    vector[TS * W] wv = to_vector(w);
    vector[ND * NS] lv = to_vector(level);
    for (n in 1:N_pred)
      mu_pred[n] = lv[(season_pred[n] - 1) * ND + div_pred[n]] + b[home_pred[n]] + b[away_pred[n]]
                   + uv[iu_home_pred[n]] + uv[iu_away_pred[n]] + wv[iw_home_pred[n]] + wv[iw_away_pred[n]];
  }
}
