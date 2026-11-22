  // Column-major positions of (team, season) in u and (team-season, week) in w.
  array[N] int iu_home;
  array[N] int iu_away;
  array[N] int iw_home;
  array[N] int iw_away;
  array[N_pred] int iu_home_pred;
  array[N_pred] int iu_away_pred;
  array[N_pred] int iw_home_pred;
  array[N_pred] int iw_away_pred;
  for (n in 1:N) {
    iu_home[n] = (season[n] - 1) * K + home[n];
    iu_away[n] = (season[n] - 1) * K + away[n];
    iw_home[n] = (wk[n] - 1) * TS + ts_home[n];
    iw_away[n] = (wk[n] - 1) * TS + ts_away[n];
  }
  for (n in 1:N_pred) {
    iu_home_pred[n] = (season_pred[n] - 1) * K + home_pred[n];
    iu_away_pred[n] = (season_pred[n] - 1) * K + away_pred[n];
    iw_home_pred[n] = (wk_pred[n] - 1) * TS + ts_home_pred[n];
    iw_away_pred[n] = (wk_pred[n] - 1) * TS + ts_away_pred[n];
  }
