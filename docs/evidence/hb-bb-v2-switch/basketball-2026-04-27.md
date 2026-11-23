# basketball: v2 switch-path replay, fit date 2026-04-27

Run 2026-11-23 11:18 UTC at commit 294dc2562 with `Rscript docs/evidence/hb-bb-v2-switch/replay.R basketball 2026-04-27`.
R-INLA 25.6.7. Temporary root, copied from data/: results 14, schedules 5, extracts 0, archive 2, publish 40 files.

## Result

- data/ unchanged by the run (git status before and after identical): **TRUE**.
- `validate_publish_dir()` on the sport's published tree: ok = **TRUE**, 40 files validated, 0 failed, 0 unmatched.
- Published cells: karla-1d, karla-bd, kvenna-1d, kvenna-bd.

## Fits

| Sex | Gate | Checks (run / skipped) | Mode status (margin, total) | Matches in likelihood / n_obs | Current season | Fit + extract (s) | Publish (s) | Diagnostics row: n_obs, engine, version, passed |
|---|---|---|---|---|---|---|---|---|
| male | passed | 4 / 1 | 0, 0 | 1804 / 1804 | 2026 | 88 | 2 | 1804, inla, 2.0, TRUE |
| female | passed | 4 / 1 | 0, 0 | 1306 / 1310 | 2026 | 108 | 2 | 1310, inla, 2.0, TRUE |

The hyperparameter-jump check is skipped: the temporary root has no earlier INLA fit of the cell, and the run log says so.

### male: hyperparameters (posterior mean, 90 % interval)

| Model | Parameter | Mean | 5 % | 95 % |
|---|---|---|---|---|
| margin | sd: the Gaussian observations | 13.066 | 12.652 | 13.489 |
| margin | sd: u_s | 4.836 | 2.834 | 7.474 |
| margin | rho: u_s | 0.523 | -0.009 | 0.873 |
| margin | sd: b | 7.121 | 4.726 | 9.819 |
| margin | sd: w_t | 4.525 | 2.796 | 6.887 |
| margin | sd: ha | 1.370 | 0.577 | 2.585 |
| total | sd: the Gaussian observations | 16.378 | 15.933 | 16.895 |
| total | sd: u_s | 5.617 | 4.818 | 6.432 |
| total | rho: u_s | 0.599 | 0.464 | 0.713 |
| total | sd: b | 2.468 | 1.946 | 3.009 |
| total | sd: w_t | 3.431 | 2.793 | 4.096 |
| total | sd: s_s | 3.542 | 2.916 | 4.178 |

Regulation-time coverage of season 2026's training rows: 100.0 %. Overtime points per period: mean 25.6, sd 10.2, from overtime games (102 overtime games).

Level-score shift (logit):

| Group | Shift | Level scores | Rows |
|---|---|---|---|
| all | 0.931 | 102 | 1804 |
| 1D | 0.611 | 33 | 846 |
| BD | 1.130 | 69 | 958 |

### female: hyperparameters (posterior mean, 90 % interval)

| Model | Parameter | Mean | 5 % | 95 % |
|---|---|---|---|---|
| margin | sd: the Gaussian observations | 13.208 | 12.705 | 13.747 |
| margin | sd: u_s | 12.968 | 8.068 | 20.003 |
| margin | rho: u_s | 0.787 | 0.581 | 0.926 |
| margin | sd: b | 13.013 | 7.289 | 20.306 |
| margin | sd: w_t | 6.954 | 5.433 | 8.686 |
| margin | sd: ha | 2.380 | 1.390 | 3.593 |
| total | sd: the Gaussian observations | 15.909 | 15.343 | 16.449 |
| total | sd: u_s | 4.281 | 3.603 | 4.923 |
| total | rho: u_s | 0.764 | 0.673 | 0.836 |
| total | sd: b | 0.398 | 0.337 | 0.458 |
| total | sd: w_t | 5.196 | 4.492 | 5.977 |
| total | sd: s_s | 3.842 | 3.024 | 4.537 |

Regulation-time coverage of season 2026's training rows: 100.0 %. Overtime points per period: mean 23.3, sd 11.3, from overtime games (32 overtime games).

Level-score shift (logit):

| Group | Shift | Level scores | Rows |
|---|---|---|---|
| all | 0.276 | 32 | 1306 |

## Forecasts against production's stored forecasts of the same fit date

No fixture in common with a production extract of this fit date.

