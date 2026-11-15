// R1 "ragged", handball: the production model (2d_student_t.stan) with one change. Team
// states exist only at real appearances (one innovation per match a team played), and the
// sum-to-zero constraint applies to the initial levels only. Likelihood, priors,
// per-team volatility hierarchies and unpooled home advantage are production's.
data {
#include _data_slots.stan
}
parameters {
  sum_to_zero_vector[K] off0;
  sum_to_zero_vector[K] def0;
  vector[S] z_off;
  vector[S] z_def;
  vector[K] z_sigma_off;
  real<lower=0> scale_sigma_off;
  real mean_sigma_off;
  vector[K] z_sigma_def;
  real<lower=0> scale_sigma_def;
  real mean_sigma_def;
  real mean_goals0;
  real delta_mean_goals;
  real<lower=0> sigma_mean_goals;
  array[NS - 1] real z_mean_goals;
  vector<lower=0>[K] home_advantage_off;
  vector<lower=0>[K] home_advantage_def;
  vector[K] z_sigma_team;
  real<lower=0> scale_sigma_team;
  real mean_sigma_team;
  real<lower=1> nu;
  real<lower=-1, upper=1> rho;
}
transformed parameters {
  vector<lower=0>[K] sigma_off = exp(mean_sigma_off + z_sigma_off * scale_sigma_off);
  vector<lower=0>[K] sigma_def = exp(mean_sigma_def + z_sigma_def * scale_sigma_def);
  vector<lower=0>[K] sigma_team = exp(mean_sigma_team + scale_sigma_team * z_sigma_team);
  vector[S] att;
  vector[S] def;
  vector[NS] mean_goals;
  {
    vector[S] ca = cumulative_sum(sigma_off[slot_team] .* slot_sdt .* z_off);
    vector[S] cd = cumulative_sum(sigma_def[slot_team] .* slot_sdt .* z_def);
    att = off0[slot_team] + ca - ca[first_slot[slot_team]];
    def = def0[slot_team] + cd - cd[first_slot[slot_team]];
  }
  mean_goals[1] = mean_goals0;
  for (i in 2:NS)
    mean_goals[i] = mean_goals[i - 1] + delta_mean_goals + sigma_mean_goals * z_mean_goals[i - 1];
}
model {
  off0 ~ normal(0, 10);
  def0 ~ normal(0, 10);
  z_off ~ std_normal();
  z_def ~ std_normal();
  z_sigma_off ~ std_normal();
  scale_sigma_off ~ exponential(2);
  mean_sigma_off ~ normal(-1.5, 2);
  z_sigma_def ~ std_normal();
  scale_sigma_def ~ exponential(2);
  mean_sigma_def ~ normal(-2, 2);
  home_advantage_off ~ normal(0, 10);
  home_advantage_def ~ normal(0, 10);
  z_sigma_team ~ std_normal();
  scale_sigma_team ~ exponential(2);
  mean_sigma_team ~ normal(2, 2);
  mean_goals0 ~ normal(prior_level[1], prior_level[2]);
  delta_mean_goals ~ normal(0, 10);
  sigma_mean_goals ~ exponential(2);
  z_mean_goals ~ std_normal();
  rho ~ uniform(-1, 1);
  nu ~ gamma(3, 0.15);
  for (n in 1:N) {
    vector[2] mu;
    matrix[2, 2] Sigma;
    mu[1] = mean_goals[season[n]] + att[home_slot[n]] + home_advantage_off[home_team[n]] - def[away_slot[n]];
    mu[2] = mean_goals[season[n]] + att[away_slot[n]] - def[home_slot[n]] - home_advantage_def[home_team[n]];
    Sigma[1, 1] = square(sigma_team[home_team[n]]);
    Sigma[2, 2] = square(sigma_team[away_team[n]]);
    Sigma[1, 2] = rho * sigma_team[home_team[n]] * sigma_team[away_team[n]];
    Sigma[2, 1] = Sigma[1, 2];
    [y_home[n], y_away[n]]' ~ multi_student_t(nu, mu, Sigma);
  }
}
generated quantities {
  vector[K] att_last = att[last_slot];
  vector[K] def_last = def[last_slot];
  vector[N] log_lik;
  vector[N_hold] loc_m;
  vector[N_hold] sc_m;
  vector[N_hold] loc_t;
  vector[N_hold] sc_t;
  real nu_out = nu;
  for (n in 1:N) {
    vector[2] mu;
    matrix[2, 2] Sigma;
    mu[1] = mean_goals[season[n]] + att[home_slot[n]] + home_advantage_off[home_team[n]] - def[away_slot[n]];
    mu[2] = mean_goals[season[n]] + att[away_slot[n]] - def[home_slot[n]] - home_advantage_def[home_team[n]];
    Sigma[1, 1] = square(sigma_team[home_team[n]]);
    Sigma[2, 2] = square(sigma_team[away_team[n]]);
    Sigma[1, 2] = rho * sigma_team[home_team[n]] * sigma_team[away_team[n]];
    Sigma[2, 1] = Sigma[1, 2];
    log_lik[n] = multi_student_t_lpdf([y_home[n], y_away[n]]' | nu, mu, Sigma);
  }
  for (h in 1:N_hold) {
    int i = hold_home[h];
    int j = hold_away[h];
    real mu1 = mean_goals[NS] + att_last[i] + home_advantage_off[i] - def_last[j];
    real mu2 = mean_goals[NS] + att_last[j] - def_last[i] - home_advantage_def[i];
    real v = square(sigma_team[i]) + square(sigma_team[j]);
    real c = 2 * rho * sigma_team[i] * sigma_team[j];
    loc_m[h] = mu1 - mu2;
    loc_t[h] = mu1 + mu2;
    sc_m[h] = sqrt(v - c);
    sc_t[h] = sqrt(v + c);
  }
}
