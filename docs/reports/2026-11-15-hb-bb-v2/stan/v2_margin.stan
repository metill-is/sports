// v2 margin model. margin = home score - away score ~ normal(eta, sigma), with
//   eta = hfa + ha[home] + theta[home] - theta[away],
//   theta[team, date] = b[team] + u[team, season] + w[team-season, week].
// Every piece is Gaussian, so R-INLA fits the same model exactly (fit_v2_inla()).
data {
#include _data_v2.stan
  real<lower=0> rate_ha;
  real<lower=0> hfa_sd;
}
transformed data {
#include _tdata_v2.stan
}
parameters {
  real hfa;                                  // league home advantage, in score units
  real<lower=0> sigma;                       // sd of a match margin around its mean
  real<lower=0> sigma_b;                     // sd across clubs of the long-run level
  real<lower=0> sigma_u;                     // sd of a season level around the club level
  real theta_rho;                            // log((1 + rho) / (1 - rho)); rho = season-to-season memory
  array[within_mode > 0] real<lower=0> sigma_w;   // within-season sd: per week (mode 1) or marginal (mode 2)
  real<lower=0> sigma_ha;                    // sd of team deviations from the league home advantage
  vector[K] b_z;
  matrix[K, NS] u_z;
  matrix[within_mode > 0 ? TS : 0, within_mode == 1 ? W - 1 : W] w_z;
  vector[K] ha_z;
}
transformed parameters {
#include _states_v2.stan
  vector[K] ha = sigma_ha * ha_z;
  vector[N] eta;
  {
    vector[K * NS] uv = to_vector(u);
    vector[TS * W] wv = to_vector(w);
    eta = hfa + ha[home] + b[home] - b[away] + uv[iu_home] - uv[iu_away] + wv[iw_home] - wv[iw_away];
  }
}
model {
  hfa ~ normal(0, hfa_sd);
  sigma ~ exponential(rate_sigma);
  sigma_b ~ exponential(rate_b);
  sigma_u ~ exponential(rate_u);
  sigma_w ~ exponential(rate_w);
  sigma_ha ~ exponential(rate_ha);
  theta_rho ~ normal(rho_mu, rho_sd);
  b_z ~ std_normal();
  to_vector(u_z) ~ std_normal();
  to_vector(w_z) ~ std_normal();
  ha_z ~ std_normal();
  y ~ normal(eta, sigma);
}
generated quantities {
  vector[N] log_lik;
  vector[N_pred] mu_pred;
  for (n in 1:N) log_lik[n] = normal_lpdf(y[n] | eta[n], sigma);
  {
    vector[K * NS] uv = to_vector(u);
    vector[TS * W] wv = to_vector(w);
    mu_pred = hfa + ha[home_pred] + b[home_pred] - b[away_pred] + uv[iu_home_pred] - uv[iu_away_pred]
              + wv[iw_home_pred] - wv[iw_away_pred];
  }
}
