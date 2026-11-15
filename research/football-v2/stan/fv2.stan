// Football v2 reference model. The same specification as research/football-v2/spec.R
// hands to INLA: weekly calendar states per team (one slot per team-week with a match
// or a fixture), two intercepts, pooled sign-free team home-advantage deviations,
// one innovation sd per component, optional season-break jump and division offsets.
// Slots without a match (fixtures to predict) carry no likelihood term, so their
// states are the forward-propagated prior.
data {
  int<lower=1> K;
  int<lower=1> S;
  int<lower=0> N;
  array[S] int<lower=1, upper=K> slot_team;      // slots sorted by team, then week
  array[K] int<lower=1, upper=S> first_slot;
  vector<lower=0>[S] step_in;                    // sqrt(weeks) for in-season steps, else 0
  vector<lower=0>[S] step_brk;                   // sqrt(breaks) for season-break steps, else 0
  array[N] int<lower=1, upper=S> home_slot;
  array[N] int<lower=1, upper=S> away_slot;
  array[N] int<lower=1, upper=K> home_team;
  array[N] int<lower=0> goals_home;
  array[N] int<lower=0> goals_away;
  int<lower=0, upper=1> use_break;
  int<lower=0> n_cell;                           // division-by-season cells (0 = no offsets)
  array[n_cell > 0 ? N : 0] int<lower=1, upper=n_cell> cell;
  real c_prior_mean;
  real<lower=0> c_prior_sd;
  real<lower=0> rate_tau;
  real<lower=0> rate_sigma;
  real<lower=0> rate_kappa;
  real<lower=0> rate_omega;
  real<lower=0> rate_delta;
}
transformed data {
  int has_cells = n_cell > 0;
}
parameters {
  real c_home;
  real c_away;
  sum_to_zero_vector[K] lvl_att;                 // entry levels
  sum_to_zero_vector[K] lvl_def;
  real<lower=0> tau_att;
  real<lower=0> tau_def;
  real<lower=0> sigma_att;                       // per sqrt(week)
  real<lower=0> sigma_def;
  array[use_break] real<lower=0> kappa_att;      // per season break
  array[use_break] real<lower=0> kappa_def;
  vector[S] z_att;
  vector[S] z_def;
  real<lower=0> omega_att;
  real<lower=0> omega_def;
  vector[K] ha_att_z;
  vector[K] ha_def_z;
  array[has_cells] real<lower=0> delta_goal;
  array[has_cells] real<lower=0> delta_ha;
  vector[n_cell] g_goal_z;
  vector[n_cell] g_ha_z;
}
transformed parameters {
  vector[K] ha_att = omega_att * ha_att_z;
  vector[K] ha_def = omega_def * ha_def_z;
  vector[n_cell] g_goal;
  vector[n_cell] g_ha;
  vector[S] att;
  vector[S] def;
  {
    vector[S] sa = sigma_att * step_in;
    vector[S] sd = sigma_def * step_in;
    if (use_break) {
      sa += kappa_att[1] * step_brk;
      sd += kappa_def[1] * step_brk;
    }
    vector[S] ca = cumulative_sum(sa .* z_att);
    vector[S] cd = cumulative_sum(sd .* z_def);
    att = lvl_att[slot_team] + ca - ca[first_slot[slot_team]];
    def = lvl_def[slot_team] + cd - cd[first_slot[slot_team]];
  }
  if (has_cells) {
    g_goal = delta_goal[1] * g_goal_z;
    g_ha = delta_ha[1] * g_ha_z;
  }
}
model {
  c_home ~ normal(c_prior_mean, c_prior_sd);
  c_away ~ normal(c_prior_mean, c_prior_sd);
  // iid N(0, tau^2) conditioned on a zero sum lives in K - 1 dimensions:
  // the vector statement counts K log(tau) terms, so one is handed back.
  lvl_att ~ normal(0, tau_att);
  lvl_def ~ normal(0, tau_def);
  target += log(tau_att) + log(tau_def);
  tau_att ~ exponential(rate_tau);
  tau_def ~ exponential(rate_tau);
  sigma_att ~ exponential(rate_sigma);
  sigma_def ~ exponential(rate_sigma);
  kappa_att ~ exponential(rate_kappa);
  kappa_def ~ exponential(rate_kappa);
  z_att ~ std_normal();
  z_def ~ std_normal();
  omega_att ~ exponential(rate_omega);
  omega_def ~ exponential(rate_omega);
  ha_att_z ~ std_normal();
  ha_def_z ~ std_normal();
  delta_goal ~ exponential(rate_delta);
  delta_ha ~ exponential(rate_delta);
  g_goal_z ~ std_normal();
  g_ha_z ~ std_normal();
  {
    vector[N] eta_home = c_home + att[home_slot] + ha_att[home_team] - def[away_slot];
    vector[N] eta_away = c_away + att[away_slot] - def[home_slot] - ha_def[home_team];
    if (has_cells) {
      eta_home += g_goal[cell] + 0.5 * g_ha[cell];
      eta_away += g_goal[cell] - 0.5 * g_ha[cell];
    }
    goals_home ~ poisson_log(eta_home);
    goals_away ~ poisson_log(eta_away);
  }
}
generated quantities {
  vector[N] log_lik;
  {
    vector[N] eta_home = c_home + att[home_slot] + ha_att[home_team] - def[away_slot];
    vector[N] eta_away = c_away + att[away_slot] - def[home_slot] - ha_def[home_team];
    if (has_cells) {
      eta_home += g_goal[cell] + 0.5 * g_ha[cell];
      eta_away += g_goal[cell] - 0.5 * g_ha[cell];
    }
    for (n in 1:N) {
      log_lik[n] = poisson_log_lpmf(goals_home[n] | eta_home[n])
                   + poisson_log_lpmf(goals_away[n] | eta_away[n]);
    }
  }
}
