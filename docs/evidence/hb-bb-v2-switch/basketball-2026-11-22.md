# basketball: v2 switch-path replay, fit date 2026-11-22

Run 2026-11-23 17:38 UTC at commit 810ffd697 with `Rscript docs/evidence/hb-bb-v2-switch/replay.R basketball 2026-11-22`.
R-INLA 25.6.7. Temporary root, copied from origin/main at da015e5bb (extracted read-only with git archive): results 14, schedules 5, extracts 98, archive 60, publish 40 files.

## Result

- data/ unchanged by the run (git status before and after identical): **TRUE**.
- `validate_publish_dir()` on the sport's published tree: ok = **TRUE**, 40 files validated, 0 failed, 0 unmatched.
- Published cells: karla-1d, karla-bd, kvenna-1d, kvenna-bd.

## Fits

| Sex | Gate | Checks (run / skipped) | Mode status (margin, total) | Matches in likelihood / n_obs | Current season | Fit + extract (s) | Publish (s) | Diagnostics row: n_obs, engine, version, passed |
|---|---|---|---|---|---|---|---|---|
| male | passed | 4 / 1 | 0, 0 | 1909 / 1909 | 2027 | 133 | 2 | 1909, inla, 2.0, TRUE |
| female | passed | 4 / 1 | 0, 0 | 1383 / 1387 | 2027 | 120 | 2 | 1387, inla, 2.0, TRUE |

The hyperparameter-jump check is skipped: the temporary root has no earlier INLA fit of the cell, and the run log says so.

### male: hyperparameters (posterior mean, 90 % interval)

| Model | Parameter | Mean | 5 % | 95 % |
|---|---|---|---|---|
| margin | sd: the Gaussian observations | 13.049 | 12.648 | 13.453 |
| margin | sd: u_s | 6.286 | 3.973 | 9.436 |
| margin | rho: u_s | 0.645 | 0.238 | 0.912 |
| margin | sd: b | 7.376 | 4.735 | 10.759 |
| margin | sd: w_t | 4.162 | 2.558 | 6.347 |
| margin | sd: ha | 1.176 | 0.394 | 2.289 |
| total | sd: the Gaussian observations | 16.135 | 15.646 | 16.585 |
| total | sd: u_s | 4.319 | 3.814 | 5.039 |
| total | rho: u_s | 0.671 | 0.600 | 0.755 |
| total | sd: b | 2.825 | 2.396 | 3.199 |
| total | sd: w_t | 5.203 | 4.492 | 6.000 |
| total | sd: s_s | 2.991 | 2.532 | 3.660 |

Regulation-time coverage of season 2027's training rows: 100.0 %. Overtime points per period: mean 25.5, sd 10.1, from overtime games (105 overtime games).

Level-score shift (logit):

| Group | Shift | Level scores | Rows |
|---|---|---|---|
| all | 0.902 | 105 | 1909 |
| 1D | 0.581 | 34 | 899 |
| BD | 1.104 | 71 | 1010 |

### female: hyperparameters (posterior mean, 90 % interval)

| Model | Parameter | Mean | 5 % | 95 % |
|---|---|---|---|---|
| margin | sd: the Gaussian observations | 13.262 | 12.761 | 13.775 |
| margin | sd: u_s | 14.218 | 9.890 | 20.272 |
| margin | rho: u_s | 0.794 | 0.626 | 0.919 |
| margin | sd: b | 8.819 | 3.945 | 15.196 |
| margin | sd: w_t | 7.293 | 5.755 | 9.149 |
| margin | sd: ha | 2.321 | 1.354 | 3.609 |
| total | sd: the Gaussian observations | 15.854 | 15.304 | 16.372 |
| total | sd: u_s | 2.802 | 2.333 | 3.289 |
| total | rho: u_s | 0.655 | 0.561 | 0.739 |
| total | sd: b | 2.997 | 2.614 | 3.437 |
| total | sd: w_t | 5.260 | 4.448 | 5.974 |
| total | sd: s_s | 4.112 | 3.448 | 5.147 |

Regulation-time coverage of season 2027's training rows: 100.0 %. Overtime points per period: mean 23.2, sd 11.1, from overtime games (33 overtime games).

Level-score shift (logit):

| Group | Shift | Level scores | Rows |
|---|---|---|---|
| all | 0.254 | 33 | 1383 |

## Forecasts against production's stored forecasts of the same fit date

23 fixtures in common (production predicted from the morning's data; the replay reads today's results, so a fixture played on the fit date is training data here and drops out). Per sex: mean absolute difference in the home-win probability and in the expected margin, and the largest.

| Sex | Fixtures | Mean abs diff p(home) | Max | Mean abs diff expected margin | Max | Mean expected total v2 / Stan |
|---|---|---|---|---|---|---|
| female | 23 | 0.061 | 0.246 | 3.29 | 12.50 | 156.2 / 156.1 |

Draw probability: v2 mean 0.000, Stan mean 0.000 (handball's published draw is the level-score band; basketball's is 0).

