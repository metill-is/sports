// R1 "ragged", basketball: the production model (2d_student_t_scalarsigma.stan) with one
// change. Team states exist only at real appearances, and the sum-to-zero constraint
// applies to the initial levels only. Likelihood (match-dependent rho), priors, per-team
// volatility hierarchies and unpooled home advantage are production's.
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
  real<lower=0> sigma;
  real<lower=1> nu;
  real alpha_rho;
  real beta_rho;
  real beta2_rho;
  real beta3_rho;
}
transformed parameters {
  vector<lower=0>[K] sigma_off = exp(mean_sigma_off + z_sigma_off * scale_sigma_off);
  vector<lower=0>[K] sigma_def = exp(mean_sigma_def + z_sigma_def * scale_sigma_def);
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
  sigma ~ normal(10, 5);
  mean_goals0 ~ normal(prior_level[1], prior_level[2]);
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
    vector[2] dfn;
    vector[2] mu;
    matrix[2, 2] Sigma;
    off[1] = att[home_slot[n]] + home_advantage_off[home_team[n]];
    dfn[1] = def[home_slot[n]] + home_advantage_def[home_team[n]];
    off[2] = att[away_slot[n]];
    dfn[2] = def[away_slot[n]];
    mu[1] = mean_goals[season[n]] + off[1] - dfn[2];
    mu[2] = mean_goals[season[n]] + off[2] - dfn[1];
    real strength_diff = abs(off[1] + dfn[1] - off[2] - dfn[2]);
    real total_strength = abs(off[1] + dfn[1] + off[2] + dfn[2]);
    real rho = 2 * inv_logit(alpha_rho + beta_rho * strength_diff + beta2_rho * total_strength
                             + beta3_rho * total_strength * strength_diff) - 1;
    Sigma[1, 1] = square(sigma);
    Sigma[2, 2] = square(sigma);
    Sigma[1, 2] = rho * square(sigma);
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
    vector[2] off;
    vector[2] dfn;
    vector[2] mu;
    matrix[2, 2] Sigma;
    off[1] = att[home_slot[n]] + home_advantage_off[home_team[n]];
    dfn[1] = def[home_slot[n]] + home_advantage_def[home_team[n]];
    off[2] = att[away_slot[n]];
    dfn[2] = def[away_slot[n]];
    mu[1] = mean_goals[season[n]] + off[1] - dfn[2];
    mu[2] = mean_goals[season[n]] + off[2] - dfn[1];
    real strength_diff = abs(off[1] + dfn[1] - off[2] - dfn[2]);
    real total_strength = abs(off[1] + dfn[1] + off[2] + dfn[2]);
    real rho = 2 * inv_logit(alpha_rho + beta_rho * strength_diff + beta2_rho * total_strength
                             + beta3_rho * total_strength * strength_diff) - 1;
    Sigma[1, 1] = square(sigma);
    Sigma[2, 2] = square(sigma);
    Sigma[1, 2] = rho * square(sigma);
    Sigma[2, 1] = Sigma[1, 2];
    log_lik[n] = multi_student_t_lpdf([y_home[n], y_away[n]]' | nu, mu, Sigma);
  }
  for (h in 1:N_hold) {
    int i = hold_home[h];
    int j = hold_away[h];
    real off1 = att_last[i] + home_advantage_off[i];
    real def1 = def_last[i] + home_advantage_def[i];
    real mu1 = mean_goals[NS] + off1 - def_last[j];
    real mu2 = mean_goals[NS] + att_last[j] - def1;
    real strength_diff = abs(off1 + def1 - att_last[j] - def_last[j]);
    real total_strength = abs(off1 + def1 + att_last[j] + def_last[j]);
    real rho = 2 * inv_logit(alpha_rho + beta_rho * strength_diff + beta2_rho * total_strength
                             + beta3_rho * total_strength * strength_diff) - 1;
    loc_m[h] = mu1 - mu2;
    loc_t[h] = mu1 + mu2;
    sc_m[h] = sigma * sqrt(2 - 2 * rho);
    sc_t[h] = sigma * sqrt(2 + 2 * rho);
  }
}
