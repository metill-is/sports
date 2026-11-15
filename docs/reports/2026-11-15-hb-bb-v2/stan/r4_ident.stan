// R4 "identified" (use_t = 1) and R5 "Gaussian" (use_t = 0): R3 with the intercept ridge
// removed. The season level m[s] is the away side's baseline score; hfa is the league
// home-minus-away baseline difference; team home deviations are zero-mean and shrunken.
// Two baselines for two row types: nothing is left for the priors to separate.
functions {
#include _functions.stan
}
data {
#include _data_slots.stan
  vector[2] prior_score_sd;
  real<lower=0> prior_ha_mean_sd;
  real<lower=0> prior_ha_dev_sd;
  int<lower=0, upper=1> use_t;
}
parameters {
  sum_to_zero_vector[K] off0;
  sum_to_zero_vector[K] def0;
  vector[S] z_off;
  vector[S] z_def;
  real mean_sigma_off;
  real mean_sigma_def;
  real mean_goals0;
  real delta_mean_goals;
  real<lower=0> sigma_mean_goals;
  array[NS - 1] real z_mean_goals;
  real hfa;
  real<lower=0> ha_off_sd;
  real<lower=0> ha_def_sd;
  vector[K] ha_off_z;
  vector[K] ha_def_z;
  real<lower=0> sigma;
  array[use_t] real<lower=1> nu;
  real<lower=-1, upper=1> rho;
}
transformed parameters {
  real<lower=0> sigma_off = exp(mean_sigma_off);
  real<lower=0> sigma_def = exp(mean_sigma_def);
  vector[K] home_advantage_off = ha_off_sd * ha_off_z;
  vector[K] home_advantage_def = ha_def_sd * ha_def_z;
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
  mu1 = mean_goals[season] + hfa + att[home_slot] + home_advantage_off[home_team] - def[away_slot];
  mu2 = mean_goals[season] + att[away_slot] - def[home_slot] - home_advantage_def[home_team];
}
model {
  real nu_use = use_t ? nu[1] : -1;
  off0 ~ normal(0, 10);
  def0 ~ normal(0, 10);
  z_off ~ std_normal();
  z_def ~ std_normal();
  mean_sigma_off ~ normal(-1.5, 2);
  mean_sigma_def ~ normal(-2, 2);
  hfa ~ normal(0, prior_ha_mean_sd);
  ha_off_sd ~ normal(0, prior_ha_dev_sd);
  ha_def_sd ~ normal(0, prior_ha_dev_sd);
  ha_off_z ~ std_normal();
  ha_def_z ~ std_normal();
  sigma ~ normal(prior_score_sd[1], prior_score_sd[2]);
  mean_goals0 ~ normal(prior_level[1], prior_level[2]);
  delta_mean_goals ~ normal(0, 10);
  sigma_mean_goals ~ exponential(2);
  z_mean_goals ~ std_normal();
  rho ~ uniform(-1, 1);
  nu ~ gamma(3, 0.15);
  target += sum(biv_lpdf_pointwise(y_home - mu1, y_away - mu2, rep_vector(sigma, N), rep_vector(sigma, N), rho, nu_use));
}
generated quantities {
  vector[K] att_last = att[last_slot];
  vector[K] def_last = def[last_slot];
  real nu_out = use_t ? nu[1] : positive_infinity();
  vector[N] log_lik = biv_lpdf_pointwise(y_home - mu1, y_away - mu2, rep_vector(sigma, N), rep_vector(sigma, N), rho, use_t ? nu[1] : -1);
  vector[N_hold] loc_m;
  vector[N_hold] sc_m = rep_vector(sigma * sqrt(2 - 2 * rho), N_hold);
  vector[N_hold] loc_t;
  vector[N_hold] sc_t = rep_vector(sigma * sqrt(2 + 2 * rho), N_hold);
  for (h in 1:N_hold) {
    int i = hold_home[h];
    int j = hold_away[h];
    real p1 = mean_goals[NS] + hfa + att_last[i] + home_advantage_off[i] - def_last[j];
    real p2 = mean_goals[NS] + att_last[j] - def_last[i] - home_advantage_def[i];
    loc_m[h] = p1 - p2;
    loc_t[h] = p1 + p2;
  }
}
