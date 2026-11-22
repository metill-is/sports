  int<lower=1> K;                            // teams
  int<lower=1> N;                            // training matches
  int<lower=1> S;                            // appearance slots (2N): one per team per match played
  int<lower=1> NS;                           // seasons
  array[S] int<lower=1, upper=K> slot_team;
  vector<lower=0>[S] slot_sdt;               // sqrt(days since the team's previous match, capped at 100); 0 at a team's first slot
  array[K] int<lower=1, upper=S> first_slot;
  array[K] int<lower=1, upper=S> last_slot;
  array[N] int<lower=1, upper=S> home_slot;
  array[N] int<lower=1, upper=S> away_slot;
  array[N] int<lower=1, upper=K> home_team;
  array[N] int<lower=1, upper=K> away_team;
  array[N] int<lower=1, upper=NS> season;
  vector[N] y_home;
  vector[N] y_away;
  int<lower=0> N_hold;                       // held-out matches, all in season NS
  array[N_hold] int<lower=1, upper=K> hold_home;
  array[N_hold] int<lower=1, upper=K> hold_away;
  vector[2] prior_level;                     // mean, sd of the season-1 scoring level (per team per match)
