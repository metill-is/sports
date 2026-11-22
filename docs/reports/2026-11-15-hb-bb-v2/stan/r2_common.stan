// R2 "common": R1 with the per-team volatility hierarchies collapsed to one random-walk sd
// for attack and one for defence, a single score sd (handball loses per-team sigma_team)
// and a single score correlation (basketball loses the 4-parameter rho). Student-t kept.
// Home advantage is still production's: per team, attack and defence, each >= 0,
// half-normal(0, 10), no pooling. The likelihood is the same density in closed form.
functions {
#include _functions.stan
}
data {
#include _data_slots.stan
  vector[2] prior_score_sd;                  // mean, sd of the half-normal prior on the score sd
}
parameters {
  sum_to_zero_vector[K] off0;
  sum_to_zero_vector[K] def0;
  vector[S] z_off;
  vector[S] z_def;
  real mean_sigma_off;                       // log random-walk sd per sqrt(day), attack
  real mean_sigma_def;
  real mean_goals0;
  real delta_mean_goals;
  real<lower=0> sigma_mean_goals;
  array[NS - 1] real z_mean_goals;
  vector<lower=0>[K] home_advantage_off;
  vector<lower=0>[K] home_advantage_def;
  real<lower=0> sigma;
  real<lower=1> nu;
  real<lower=-1, upper=1> rho;
}
transformed parameters {
  real<lower=0> sigma_off = exp(mean_sigma_off);
  real<lower=0> sigma_def = exp(mean_sigma_def);
  vector[S] att;
  vector[S] def;
  vector[NS] mean_goals;
  vector[N] mu1;
  vector[N] mu2;
  {
    vector[S] ca = cumulative_sum(sigma_off * slot_sdt .* z_off);
    vector[S] cd = cumulative_sum(sigma_def * slot_sdt .* z_def);
    att = off0[slot_team] + ca - ca[first_slot[slot_team]];
    def = def0[slot_team] + cd - cd[first_slot[slot_team]];
  }
  mean_goals[1] = mean_goals0;
  for (i in 2:NS)
    mean_goals[i] = mean_goals[i - 1] + delta_mean_goals + sigma_mean_goals * z_mean_goals[i - 1];
  mu1 = mean_goals[season] + att[home_slot] + home_advantage_off[home_team] - def[away_slot];
  mu2 = mean_goals[season] + att[away_slot] - def[home_slot] - home_advantage_def[home_team];
}
model {
  off0 ~ normal(0, 10);
  def0 ~ normal(0, 10);
  z_off ~ std_normal();
  z_def ~ std_normal();
  mean_sigma_off ~ normal(-1.5, 2);
  mean_sigma_def ~ normal(-2, 2);
  home_advantage_off ~ normal(0, 10);
  home_advantage_def ~ normal(0, 10);
  sigma ~ normal(prior_score_sd[1], prior_score_sd[2]);
  mean_goals0 ~ normal(prior_level[1], prior_level[2]);
  delta_mean_goals ~ normal(0, 10);
  sigma_mean_goals ~ exponential(2);
  z_mean_goals ~ std_normal();
  rho ~ uniform(-1, 1);
  nu ~ gamma(3, 0.15);
  target += sum(biv_lpdf_pointwise(y_home - mu1, y_away - mu2, rep_vector(sigma, N), rep_vector(sigma, N), rho, nu));
}
generated quantities {
  vector[K] att_last = att[last_slot];
  vector[K] def_last = def[last_slot];
  vector[N] log_lik = biv_lpdf_pointwise(y_home - mu1, y_away - mu2, rep_vector(sigma, N), rep_vector(sigma, N), rho, nu);
  vector[N_hold] loc_m;
  vector[N_hold] sc_m = rep_vector(sigma * sqrt(2 - 2 * rho), N_hold);
  vector[N_hold] loc_t;
  vector[N_hold] sc_t = rep_vector(sigma * sqrt(2 + 2 * rho), N_hold);
  real nu_out = nu;
  for (h in 1:N_hold) {
    int i = hold_home[h];
    int j = hold_away[h];
    real p1 = mean_goals[NS] + att_last[i] + home_advantage_off[i] - def_last[j];
    real p2 = mean_goals[NS] + att_last[j] - def_last[i] - home_advantage_def[i];
    loc_m[h] = p1 - p2;
    loc_t[h] = p1 + p2;
  }
}
