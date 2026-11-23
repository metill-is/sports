# Handball and basketball v2: scorecard, 2026-11-17 to 2026-11-22

Run 2026-11-23 17:56 UTC at commit 94bf7d619 with `Rscript scripts/hbbb_v2_scorecard.R --from 2026-11-17 --to 2026-11-22 --draws 4000`; 8 v2 fits, 18 minutes (cached fits are not refitted).

A match is scored where production has a stored forecast of it (its newest fit dated before the match). v2 forecasts it from the same information: production's fit dated D saw the results to D - 1, and v2 is fitted on them, with the switch path's frozen specification. Scored as the report scored it; positive differences favour v2. Standard errors are clustered by team-season (every two matches sharing a team-season, on either side); the iid ones are in `comparison.csv`.

## Gate

| Sport | Scored matches | Mean expected minus observed home margin (se) | Passed | Why not |
|---|---|---|---|---|
| basketball | 17 | +2.84 (1.92) | FALSE | 17 scored matches, fewer than 150; mean expected home margin 1.90 against -0.94 observed: off by +2.84 (se 1.92), more than one standard error |
| handball | 20 | +0.53 (1.17) | FALSE | 20 scored matches, fewer than 150 |

## Against production's stored forecasts

| Group | Matches | Margin log score: v2 − production | Total log score | Result log likelihood |
|---|---|---|---|---|
| basketball female | 6 | -0.0179 ± 0.0835 | +0.0003 ± 0.0359 | -0.1090 ± 0.0680 |
| basketball male | 11 | -0.0059 ± 0.0421 | +0.0025 ± 0.0395 | -0.0106 ± 0.0313 |
| handball female | 9 | -0.0691 ± 0.0641 | -0.0111 ± 0.0308 | -0.0648 ± 0.0887 |
| handball male | 11 | +0.1409 ± 0.1347 | +0.0016 ± 0.0819 | +0.0359 ± 0.0482 |
| basketball, both sexes | 17 | -0.0101 ± 0.0411 | +0.0017 ± 0.0217 | -0.0453 ± 0.0337 |
| handball, both sexes | 20 | +0.0464 ± 0.0756 | -0.0041 ± 0.0490 | -0.0094 ± 0.0379 |

| Group | Mean expected margin: production / v2 / observed | Margin RMSE: production / v2 | Total RMSE: production / v2 |
|---|---|---|---|
| basketball female | 5.99 / 2.66 / 1.00 | 14.72 / 15.57 | 17.57 / 17.08 |
| basketball male | 1.01 / 1.48 / -2.00 | 8.89 / 9.50 | 18.11 / 18.72 |
| handball female | -1.69 / -2.92 / -2.22 | 3.79 / 4.45 | 6.61 / 6.53 |
| handball male | 1.34 / 0.35 / -1.18 | 6.50 / 6.14 | 6.67 / 6.74 |
| basketball, both sexes | 2.77 / 1.90 / -0.94 | 11.30 / 12.00 | 17.92 / 18.16 |
| handball, both sexes | -0.02 / -1.12 / -1.65 | 5.45 / 5.45 | 6.64 / 6.65 |

Production's forecast was 3 days old at the median; v2 had the same information.

## Against the Lengjan line

Proportionally de-vigged 1X2, the last complete scrape before kick-off. Handball is full time; basketball is regulation time, with v2's level-score shift and production's ±0.5 band.

| Cell | Priced matches | Period | 1X2 log likelihood: market / production / v2 | v2 − market |
|---|---|---|---|---|
| handball male | 5 | 2026-11-18 to 2026-11-22 | -0.5854 / -0.6034 / -0.5386 | +0.0468 ± 0.0383 |
| handball female | 2 | 2026-11-22 to 2026-11-22 | -0.2765 / -0.4430 / -0.4031 | -0.1266 ± 0.0334 |
| basketball male | 6 | 2026-11-19 to 2026-11-20 | -0.6833 / -0.4969 / -0.5381 | +0.1452 ± 0.0806 |

## Fits

8 v2 fits: 8 passed the production gate, 0 did not, 0 failed outright. Forecasts from a fit that did not pass are not scored (production would not have published them); `fits.csv` lists each fit's breaches.

0 played matches in the window had no stored production forecast and are not scored.

Data: origin/main at da015e5bb (extracted read-only with git archive).

