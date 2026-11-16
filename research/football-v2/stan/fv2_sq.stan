// Football v2 reference model, strength-plus-style form. Same likelihood as fv2.stan
// (attack = strength + style, defence = strength - style) with a different prior: one
// weekly random walk per team for overall strength, and an optional fixed style value
// per team. Mirrors what R/model-v2-spec.R hands to INLA for
// param = "sq", style = "static" or "none".
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
  int<lower=0, upper=1> use_style;               // 1: a fixed style value per team
  real c_prior_mean;
  real<lower=0> c_prior_sd;
  real<lower=0> rate_tau;
  real<lower=0> rate_sigma;
  real<lower=0> rate_kappa;
  real<lower=0> rate_omega;
}
parameters {
  real c_home;
  real c_away;
  sum_to_zero_vector[K] lvl_str;                 // entry levels
  real<lower=0> tau_str;
  real<lower=0> sigma_str;                       // per sqrt(week)
  array[use_break] real<lower=0> kappa_str;      // per season break
  vector[S] z_str;
  array[use_style] sum_to_zero_vector[K] sty_z;
  array[use_style] real<lower=0> tau_sty;
  real<lower=0> omega_att;
  real<lower=0> omega_def;
  vector[K] ha_att_z;
  vector[K] ha_def_z;
}
transformed parameters {
  vector[K] ha_att = omega_att * ha_att_z;
  vector[K] ha_def = omega_def * ha_def_z;
  vector[S] att;
  vector[S] def;
  {
    vector[S] step = sigma_str * step_in;
    if (use_break) {
      step += kappa_str[1] * step_brk;
    }
    vector[S] cs = cumulative_sum(step .* z_str);
    vector[S] str = lvl_str[slot_team] + cs - cs[first_slot[slot_team]];
    att = str;
    def = str;
    if (use_style) {
      vector[K] sty = tau_sty[1] * sty_z[1];
      att += sty[slot_team];
      def -= sty[slot_team];
    }
  }
}
model {
  c_home ~ normal(c_prior_mean, c_prior_sd);
  c_away ~ normal(c_prior_mean, c_prior_sd);
  // iid N(0, tau^2) conditioned on a zero sum lives in K - 1 dimensions:
  // the vector statement counts K log(tau) terms, so one is handed back.
  lvl_str ~ normal(0, tau_str);
  target += log(tau_str);
  tau_str ~ exponential(rate_tau);
  sigma_str ~ exponential(rate_sigma);
  kappa_str ~ exponential(rate_kappa);
  z_str ~ std_normal();
  // Non-centred, so the zero-sum standard normal carries no tau term.
  for (i in 1:use_style) {
    sty_z[i] ~ std_normal();
  }
  tau_sty ~ exponential(rate_tau);
  omega_att ~ exponential(rate_omega);
  omega_def ~ exponential(rate_omega);
  ha_att_z ~ std_normal();
  ha_def_z ~ std_normal();
  goals_home ~ poisson_log(c_home + att[home_slot] + ha_att[home_team] - def[away_slot]);
  goals_away ~ poisson_log(c_away + att[away_slot] - def[home_slot] - ha_def[home_team]);
}
generated quantities {
  vector[N] log_lik;
  {
    vector[N] eta_home = c_home + att[home_slot] + ha_att[home_team] - def[away_slot];
    vector[N] eta_away = c_away + att[away_slot] - def[home_slot] - ha_def[home_team];
    for (n in 1:N) {
      log_lik[n] = poisson_log_lpmf(goals_home[n] | eta_home[n])
                   + poisson_log_lpmf(goals_away[n] | eta_away[n]);
    }
  }
}
