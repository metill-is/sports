// Home-advantage isolation, variant A (the pilot's M1b, kept as the reference rung):
// per-team random-walk volatility, team home effects unpooled and forced >= 0.
data {
  int<lower=1> K;
  int<lower=1> N;
  int<lower=1> S;
  array[S] int<lower=1, upper=K> slot_team;
  vector<lower=0>[S] slot_sdt;
  array[K] int<lower=1, upper=S> first_slot;
  array[K] int<lower=1, upper=S> last_slot;
  array[N] int<lower=1, upper=S> home_slot;
  array[N] int<lower=1, upper=S> away_slot;
  array[N] int<lower=1, upper=K> home_team;
  array[N] int<lower=0> goals_home;
  array[N] int<lower=0> goals_away;
}
parameters {
  real mean_log_goals;
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
  vector<lower=0>[K] home_advantage_off;
  vector<lower=0>[K] home_advantage_def;
}
transformed parameters {
  vector<lower=0>[K] sigma_off = exp(mean_sigma_off + z_sigma_off * scale_sigma_off);
  vector<lower=0>[K] sigma_def = exp(mean_sigma_def + z_sigma_def * scale_sigma_def);
  vector[S] att;
  vector[S] def;
  {
    vector[S] ca = cumulative_sum(sigma_off[slot_team] .* slot_sdt .* z_off);
    vector[S] cd = cumulative_sum(sigma_def[slot_team] .* slot_sdt .* z_def);
    att = off0[slot_team] + ca - ca[first_slot[slot_team]];
    def = def0[slot_team] + cd - cd[first_slot[slot_team]];
  }
}
model {
  off0 ~ std_normal();
  def0 ~ std_normal();
  z_off ~ std_normal();
  z_def ~ std_normal();
  z_sigma_off ~ std_normal();
  scale_sigma_off ~ exponential(2);
  mean_sigma_off ~ normal(-4, 2);
  z_sigma_def ~ std_normal();
  scale_sigma_def ~ exponential(2);
  mean_sigma_def ~ normal(-5, 2);
  home_advantage_off ~ std_normal();
  home_advantage_def ~ std_normal();
  mean_log_goals ~ normal(log(1.5), 1);
  goals_home ~ poisson_log(mean_log_goals + att[home_slot] + home_advantage_off[home_team] - def[away_slot]);
  goals_away ~ poisson_log(mean_log_goals + att[away_slot] - def[home_slot] - home_advantage_def[home_team]);
}
generated quantities {
  vector[K] att_last = att[last_slot];
  vector[K] def_last = def[last_slot];
  vector[N] log_lik;
  {
    vector[N] m1 = mean_log_goals + att[home_slot] + home_advantage_off[home_team] - def[away_slot];
    vector[N] m2 = mean_log_goals + att[away_slot] - def[home_slot] - home_advantage_def[home_team];
    for (n in 1:N)
      log_lik[n] = poisson_log_lpmf(goals_home[n] | m1[n]) + poisson_log_lpmf(goals_away[n] | m2[n]);
  }
}
