// Football v2 reference model with reversion at the season break. Same specification as
// research/football-v2/spec.R hands to INLA for clock = "break":
//   inside a season   x[j] = x[j-1] + sigma * sqrt(weeks) * z
//   across seasons    teams that stay in their division revert toward the mean
//                     end-of-season state of that division's staying teams,
//                     x = m + rho * (x_end - m) + kappa * z; teams that change
//                     division take a plain jump, x = x_end + kappa * z.
// rho is fixed (as in INLA, where it sits inside the precision matrix) or estimated here.
// n_field = 2: attack and defence walks. n_field = 1: one strength walk, with an optional
// fixed style value per team (attack = strength + style, defence = strength - style).
functions {
  // One state field from its innovations. The R twin is fv2_walk() in spec.R.
  vector walk(vector lvl, vector z, vector z_brk, real sigma, real kappa, real rho,
              vector step_in, array[] int seg_of_slot, array[] int seg_first,
              array[] int seg_last, array[] int seg_team, array[] int seg_prev,
              vector seg_stay, array[] int seg_group, vector seg_brk_sd,
              array[] int seg_order, array[] int wave_start, array[] int wave_end,
              int n_group) {
    int S = num_elements(z);
    int n_seg = num_elements(z_brk);
    vector[S] ca = cumulative_sum(sigma * step_in .* z);
    vector[n_seg] drift = ca[seg_last] - ca[seg_first];
    vector[n_seg] start;
    vector[n_seg] finish;
    for (w in 1:num_elements(wave_start)) {
      vector[n_group] gsum = rep_vector(0, n_group);
      vector[n_group] gcnt = rep_vector(0, n_group);
      for (i in wave_start[w]:wave_end[w]) {
        int s = seg_order[i];
        if (seg_group[s] > 0) {
          gsum[seg_group[s]] += finish[seg_prev[s]];
          gcnt[seg_group[s]] += 1;
        }
      }
      for (i in wave_start[w]:wave_end[w]) {
        int s = seg_order[i];
        if (seg_prev[s] == 0) {
          start[s] = lvl[seg_team[s]];
        } else {
          real prev = finish[seg_prev[s]];
          real jump = kappa * seg_brk_sd[s] * z_brk[s];
          if (seg_stay[s] > 0) {
            real centre = gsum[seg_group[s]] / gcnt[seg_group[s]];
            start[s] = centre + rho * (prev - centre) + jump;
          } else {
            start[s] = prev + jump;
          }
        }
        finish[s] = start[s] + drift[s];
      }
    }
    return start[seg_of_slot] + ca - ca[seg_first[seg_of_slot]];
  }
}
data {
  int<lower=1> K;
  int<lower=1> S;
  int<lower=0> N;
  int<lower=1, upper=2> n_field;
  int<lower=0, upper=1> use_style;               // n_field = 1 only
  int<lower=0, upper=1> est_rho;
  real<lower=0, upper=1> rho_fixed;
  vector<lower=0>[S] step_in;                    // sqrt(weeks) inside a season, 0 at a segment start
  int<lower=1> n_seg;                            // (team, season) segments
  array[S] int<lower=1, upper=n_seg> seg_of_slot;
  array[n_seg] int<lower=1, upper=S> seg_first;
  array[n_seg] int<lower=1, upper=S> seg_last;
  array[n_seg] int<lower=1, upper=K> seg_team;
  array[n_seg] int<lower=0, upper=n_seg> seg_prev;   // 0: the team's entry segment
  vector<lower=0, upper=1>[n_seg] seg_stay;      // 1: stayed in its division (reverts)
  int<lower=0> n_group;
  array[n_seg] int<lower=0, upper=n_group> seg_group;
  vector<lower=0>[n_seg] seg_brk_sd;             // sqrt(seasons crossed), 0 for an entry segment
  int<lower=1> n_wave;
  array[n_seg] int<lower=1, upper=n_seg> seg_order;  // segments season by season
  array[n_wave] int<lower=1, upper=n_seg> wave_start;
  array[n_wave] int<lower=1, upper=n_seg> wave_end;
  array[N] int<lower=1, upper=S> home_slot;
  array[N] int<lower=1, upper=S> away_slot;
  array[N] int<lower=1, upper=K> home_team;
  array[N] int<lower=0> goals_home;
  array[N] int<lower=0> goals_away;
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
  array[n_field] sum_to_zero_vector[K] lvl;      // entry levels
  vector<lower=0>[n_field] tau;
  vector<lower=0>[n_field] sigma;                // per sqrt(week)
  vector<lower=0>[n_field] kappa;                // per season break
  array[est_rho] real<lower=0, upper=1> rho_par;
  array[n_field] vector[S] z;
  array[n_field] vector[n_seg] z_brk;
  array[use_style] sum_to_zero_vector[K] sty_z;
  array[use_style] real<lower=0> tau_sty;
  real<lower=0> omega_att;
  real<lower=0> omega_def;
  vector[K] ha_att_z;
  vector[K] ha_def_z;
}
transformed parameters {
  real rho = est_rho ? rho_par[1] : rho_fixed;
  vector[K] ha_att = omega_att * ha_att_z;
  vector[K] ha_def = omega_def * ha_def_z;
  vector[S] att;
  vector[S] def;
  {
    array[n_field] vector[S] x;
    for (f in 1:n_field) {
      x[f] = walk(lvl[f], z[f], z_brk[f], sigma[f], kappa[f], rho, step_in, seg_of_slot,
                  seg_first, seg_last, seg_team, seg_prev, seg_stay, seg_group, seg_brk_sd,
                  seg_order, wave_start, wave_end, n_group);
    }
    att = x[1];
    def = x[n_field];
    if (use_style) {
      vector[K] sty = tau_sty[1] * sty_z[1];
      att += sty[seg_team[seg_of_slot]];
      def -= sty[seg_team[seg_of_slot]];
    }
  }
}
model {
  c_home ~ normal(c_prior_mean, c_prior_sd);
  c_away ~ normal(c_prior_mean, c_prior_sd);
  for (f in 1:n_field) {
    // iid N(0, tau^2) conditioned on a zero sum lives in K - 1 dimensions:
    // the vector statement counts K log(tau) terms, so one is handed back.
    lvl[f] ~ normal(0, tau[f]);
    target += log(tau[f]);
    z[f] ~ std_normal();
    z_brk[f] ~ std_normal();
  }
  tau ~ exponential(rate_tau);
  sigma ~ exponential(rate_sigma);
  kappa ~ exponential(rate_kappa);
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
