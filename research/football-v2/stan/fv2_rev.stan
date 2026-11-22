// Football v2 reference model with reversion at the season break. Same specification as
// R/model-v2-spec.R hands to INLA for clock = "break":
//   inside a season   x[j] = x[j-1] + sigma * sqrt(weeks) * z
//   across seasons    teams that stay in their division revert toward the mean
//                     end-of-season state of that division's staying teams,
//                     x = m + rho * (x_end - m) + kappa * z; teams that change
//                     division take a plain jump, x = x_end + kappa * z.
//                     Who stays, and so whose end state is in m, is data
//                     (member_seg), defined once in fv2_reversion_groups().
// rho is fixed (as in INLA, where it sits inside the precision matrix) or estimated here.
// n_field = 2: attack and defence walks. n_field = 1: one strength walk, with an optional
// fixed style value per team (attack = strength + style, defence = strength - style).
// use_level = 1 adds two league-wide weekly walks, centred on zero over the weeks: a scoring
// level added to both sides, and a home advantage added as +1/2 at home and -1/2 away.
functions {
  // One state field from its innovations. The R twin is fv2_walk() in R/model-v2-spec.R.
  vector walk(vector lvl, vector z, vector z_brk, real sigma, real kappa, real rho,
              vector step_in, array[] int seg_of_slot, array[] int seg_first,
              array[] int seg_last, array[] int seg_team, array[] int seg_prev,
              vector seg_stay, array[] int seg_group, vector seg_brk_sd,
              array[] int seg_order, array[] int wave_start, array[] int wave_end,
              array[] int member_seg, array[] int group_first, array[] int group_last) {
    int S = num_elements(z);
    int n_seg = num_elements(z_brk);
    vector[S] ca = cumulative_sum(sigma * step_in .* z);
    vector[n_seg] drift = ca[seg_last] - ca[seg_first];
    vector[n_seg] start;
    vector[n_seg] finish;
    for (w in 1:num_elements(wave_start)) {
      for (i in wave_start[w]:wave_end[w]) {
        int s = seg_order[i];
        if (seg_prev[s] == 0) {
          start[s] = lvl[seg_team[s]];
        } else {
          real prev = finish[seg_prev[s]];
          real jump = kappa * seg_brk_sd[s] * z_brk[s];
          if (seg_stay[s] > 0) {
            // The members are segments of the season being left, so their
            // finish is known: waves run season by season.
            int g = seg_group[s];
            real centre = mean(finish[member_seg[group_first[g]:group_last[g]]]);
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
  int<lower=0> n_group;                          // reversion groups (division by season left)
  array[n_seg] int<lower=0, upper=n_group> seg_group;
  int<lower=0> n_member;
  array[n_member] int<lower=1, upper=n_seg> member_seg;   // each group's members, group after group
  array[n_group] int<lower=1, upper=n_member> group_first;
  array[n_group] int<lower=1, upper=n_member> group_last;
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
  int<lower=0, upper=1> use_level;
  int<lower=1> n_week;                           // weeks with a match or a fixture
  array[N] int<lower=1, upper=n_week> match_week;
  vector<lower=0>[n_week] week_step;             // sqrt(weeks since the previous one), 0 first
  real c_prior_mean;
  real<lower=0> c_prior_sd;
  real<lower=0> rate_tau;
  real<lower=0> rate_sigma;
  real<lower=0> rate_kappa;
  real<lower=0> rate_omega;
  real<lower=0> rate_level;
}
transformed data {
  int n_lvl = use_level ? n_week : 0;
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
  array[use_level] real<lower=0> sigma_lvl_goal; // per sqrt(week)
  array[use_level] real<lower=0> sigma_lvl_ha;
  vector[n_lvl] z_lvl_goal;
  vector[n_lvl] z_lvl_ha;
}
transformed parameters {
  real rho = est_rho ? rho_par[1] : rho_fixed;
  vector[K] ha_att = omega_att * ha_att_z;
  vector[K] ha_def = omega_def * ha_def_z;
  vector[n_lvl] lvl_goal;
  vector[n_lvl] lvl_ha;
  vector[S] att;
  vector[S] def;
  if (use_level) {
    vector[n_week] raw_goal = cumulative_sum(sigma_lvl_goal[1] * week_step .* z_lvl_goal);
    vector[n_week] raw_ha = cumulative_sum(sigma_lvl_ha[1] * week_step .* z_lvl_ha);
    lvl_goal = raw_goal - mean(raw_goal);
    lvl_ha = raw_ha - mean(raw_ha);
  }
  {
    array[n_field] vector[S] x;
    for (f in 1:n_field) {
      x[f] = walk(lvl[f], z[f], z_brk[f], sigma[f], kappa[f], rho, step_in, seg_of_slot,
                  seg_first, seg_last, seg_team, seg_prev, seg_stay, seg_group, seg_brk_sd,
                  seg_order, wave_start, wave_end, member_seg, group_first, group_last);
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
  sigma_lvl_goal ~ exponential(rate_level);
  sigma_lvl_ha ~ exponential(rate_level);
  z_lvl_goal ~ std_normal();
  z_lvl_ha ~ std_normal();
  {
    vector[N] eta_home = c_home + att[home_slot] + ha_att[home_team] - def[away_slot];
    vector[N] eta_away = c_away + att[away_slot] - def[home_slot] - ha_def[home_team];
    if (use_level) {
      eta_home += lvl_goal[match_week] + 0.5 * lvl_ha[match_week];
      eta_away += lvl_goal[match_week] - 0.5 * lvl_ha[match_week];
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
    if (use_level) {
      eta_home += lvl_goal[match_week] + 0.5 * lvl_ha[match_week];
      eta_away += lvl_goal[match_week] - 0.5 * lvl_ha[match_week];
    }
    for (n in 1:N) {
      log_lik[n] = poisson_log_lpmf(goals_home[n] | eta_home[n])
                   + poisson_log_lpmf(goals_away[n] | eta_away[n]);
    }
  }
}
