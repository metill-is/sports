# Weekly Stan reference fit for handball and basketball v2: measurements

This folder holds the measurements behind these settings in `R/model-v2-hbbb-reference.R`:

- the limits in `HBBB_REFERENCE_LIMITS`, on the mean difference over the compared fixtures and on any single fixture's, one set within a season and one across the season break;
- the rule for the season break (`hbbb_v2_across_break()`): a run is across the break while any fixture team has no match yet in the fixture's season;
- the NUTS bar;
- the NUTS settings (`adapt_delta` 0.99, and 2,000 sampling iterations per chain across the season break).

How they were measured:

- **Where:** on the Mac (M1 Pro, shared, inside the machine limiter), on 2026-11-24.
- **Software:** R-INLA 25.6.7 and CmdStan 2.40.0 (CI pins 2.38.0).
- **Data:** `data/` as main held it that day.
- **Writes:** none to `data/health/engine_reference/` (`--no-write`).

Each date folder is one run of:

```bash
Rscript scripts/engine_reference.R --sport handball,basketball --end-date <date> --no-write --out <folder>
```

`2026-11-24` ran without `--sport`, as CI runs it. Its football fixture files are left out; their summaries are kept.

Each run writes three files per cell:

- `summary_<sport>_<sex>.csv`: the row `engine_reference` would store, plus the cell's seconds. These runs predate the single-fixture limits and the Rhat scope column, so the rows have neither; their `passed`, `threshold` and `line_threshold` are what the code of that run stored.
- `diagnostics_<sport>_<sex>.csv`, per half:
  - NUTS divergences;
  - Rhat by group: `rhat_pred`, which the reference is held to; `rhat_hyper`; and `rhat_states`, the club and season levels;
  - the smallest bulk ESS and the NUTS seconds;
  - the INLA mode status and gate.
- `fixtures_<sport>_<sex>.csv`, per fixture: both engines' result, spread-line and total-line probabilities, and their expected margin and total.

| Folder | What it is |
|---|---|
| `2026-11-24-adapt095/` | The first run, before the NUTS changes: `adapt_delta` 0.95, and Rhat held over every hyperparameter and state. Three of four cells failed the NUTS bar, while the engines agreed to between 0.002 and 0.007. |
| `diagnostics/` | `rhat_diag.R` and its output, and `detection.R` and its output. See the list below. |
| `2026-11-24/` | Today's data, at the final NUTS settings. Women's handball is a mid-season pairing cell, because it had no fixture until 2026-12-10. |
| `2026-10-09/` | A round or two into each season. Women's basketball is across the break by the final rule: one team of twenty (KV) had no match yet of the 2027 season, in three of the 22 fixtures. |
| `2026-03-16/` | Late regular season. |
| `2026-07-01/` | Off-season: every pairing of each division at the next season's first scheduled fixture, across the season break. Sampled with 1,000 iterations per chain. |
| `2026-07-01-break2000/` | Men's basketball on that date at the final settings, which sample 2,000 iterations per chain across the break. |

The files in `diagnostics/`:

- `hb_f_1000_095.log`: women's handball. The high Rhat is all on one team's club and season levels and their sds (1.022 to 1.026); the locations are at 1.003.
- `bb_f_1000_095.log` and `bb_f_1000_099.log`: women's basketball has 41 margin divergences at 0.95 and none at 0.99.
- `bb_m_0701_2000_099.log`: men's basketball at 2026-07-01 with 2,000 sampling iterations. Forecast Rhat is 1.003 (against 1.023 at 1,000), and margin hyperparameter Rhat is 1.044 (against 1.14).
- `detection.R` and `detection.csv`: how large an error the limits catch (below).

## Which limits were set after seeing which runs

None of these limits was fixed before the data. In order:

1. **Before any run.** The in-season mean limit on the result, 0.01, is football's (`FV2_REFERENCE_MAX_MEAN_ABS_DIFF`, decision 5 of the football cutover plan). The lines started at 0.01 too. The NUTS bar was the health layer's, held over every parameter.
2. **`2026-11-24-adapt095`, the first run.** Three of four cells failed the NUTS bar while the engines agreed. After it, the bar was narrowed to the forecasts and `adapt_delta` raised to 0.99.
3. **`2026-11-24`, `2026-03-16` and `2026-07-01`,** all with 0.01 for everything.
   - After men's basketball measured 0.0116 on the total line on 2026-11-24, the in-season line limit went from 0.01 to 0.025.
   - The season-break limits (0.02 and 0.05) were set to take in the 2026-07-01 differences: women's basketball 0.0250 on the total line, women's handball 0.0065 on the result.
   - The 2,000 iterations across the break were set after men's basketball failed on Rhat there (1.023).
4. **`2026-10-09`, run after those limits were set.** It is the one held-out in-season date: its three in-season cells came in at most 0.0039 on the result and 0.0085 on a line. The break has no held-out date. Its only held-out cell-run is women's basketball on this date, a light case: one team without a match, in three of 22 fixtures.
5. **`2026-07-01-break2000`** reruns the date that motivated the longer sampling. It confirms the fix there, and it is not held out.
6. **2026-11-25, after the review of metill-is/sports#127.** These were set after seeing every run above, so nothing is held out for them:
   - the single-fixture limits;
   - the per-team rule for the break, which moved women's basketball 2026-10-09 across the break.

## Every cell-run, judged by the final rule

The final rule, per regime (within a season / across the break):

- **Mean** absolute differences over the compared fixtures: at most 0.01 / 0.02 on the result probabilities, and 0.025 / 0.05 on each of the spread and total lines.
- **One fixture:** at most 0.04 / 0.06 on any one result probability, and 0.075 / 0.14 on either line.
- **NUTS:** forecast Rhat (`rhat_pred`, worst half) at most 1.02, and divergences at most 0.5 % per half.
- **INLA:** both mode statuses 0, and the gate passed.

| Date | Cell | Fixtures | Regime | Mean: result | Mean: spread | Mean: total | One fixture: result | One fixture: line | Rhat | Time | Final rule |
|---|---|---|---|---|---|---|---|---|---|---|---|
| 2026-03-16 | handball men | 10 upcoming | in season | 0.0014 | 0.0023 | 0.0026 | 0.0068 | 0.0070 | 1.003 | 160 s | passes |
| 2026-03-16 | handball women | 18 upcoming | in season | 0.0019 | 0.0044 | 0.0038 | 0.0080 | 0.0142 | 1.002 | 149 s | passes |
| 2026-03-16 | basketball men | 24 upcoming | in season | 0.0017 | 0.0031 | 0.0058 | 0.0060 | 0.0191 | 1.005 | 484 s | passes |
| 2026-03-16 | basketball women | 19 upcoming | in season | 0.0021 | 0.0042 | 0.0036 | 0.0090 | 0.0093 | 1.003 | 465 s | passes |
| 2026-10-09 | handball men | 25 upcoming | in season | 0.0030 | 0.0055 | 0.0042 | 0.0162 | 0.0163 | 1.004 | 205 s | passes |
| 2026-10-09 | handball women | 17 upcoming | in season | 0.0039 | 0.0075 | 0.0039 | 0.0159 | 0.0215 | 1.006 | 148 s | passes |
| 2026-10-09 | basketball men | 31 upcoming | in season | 0.0021 | 0.0036 | 0.0085 | 0.0148 | 0.0256 | 1.005 | 699 s | passes |
| 2026-10-09 | basketball women | 22 upcoming | across the break | 0.0037 | 0.0079 | 0.0047 | 0.0166 | 0.0210 | 1.004 | 533 s | passes, at 1,000 iterations |
| 2026-11-24 | handball men | 26 upcoming | in season | 0.0011 | 0.0022 | 0.0027 | 0.0048 | 0.0083 | 1.003 | 166 s | passes |
| 2026-11-24 | handball women | 146 mid-season pairings | in season | 0.0031 | 0.0055 | 0.0063 | 0.0203 | 0.0214 | 1.004 | 166 s | passes |
| 2026-11-24 | basketball men | 18 upcoming | in season | 0.0022 | 0.0041 | 0.0116 | 0.0117 | 0.0370 | 1.007 | 663 s | passes |
| 2026-11-24 | basketball women | 22 upcoming | in season | 0.0017 | 0.0040 | 0.0040 | 0.0066 | 0.0128 | 1.003 | 529 s | passes |
| 2026-07-01 | handball men | 264 off-season pairings | across the break | 0.0019 | 0.0033 | 0.0073 | 0.0096 | 0.0283 | 1.005 | 151 s | passes |
| 2026-07-01 | handball women | 112 off-season pairings | across the break | 0.0065 | 0.0106 | 0.0083 | 0.0302 | 0.0302 | 1.004 | 148 s | passes |
| 2026-07-01 | basketball men | 264 off-season pairings | across the break | 0.0037 | 0.0059 | 0.0184 | 0.0175 | 0.0517 | 1.023 | 695 s | fails: Rhat, at 1,000 iterations |
| 2026-07-01 | basketball women | 180 off-season pairings | across the break | 0.0039 | 0.0066 | 0.0250 | 0.0205 | 0.0704 | 1.005 | 589 s | passes |
| 2026-07-01 | basketball men, 2,000 iterations | 264 off-season pairings | across the break | 0.0032 | 0.0054 | 0.0189 | 0.0153 | 0.0527 | 1.006 | 996 s | passes |

Women's basketball on 2026-10-09 ran under the earlier rule, which put a whole cell in season once any match of the new season was played. So its NUTS sampled 1,000 iterations, and its row stored the in-season limits. The final rule puts it across the break, where NUTS samples 2,000. It was not rerun.

The largest differences, per regime:

| Regime | Cell-runs | Mean: result | Mean: line | One fixture: result | One fixture: line |
|---|---|---|---|---|---|
| Within a season | 11, at three dates | 0.0039 | 0.0116 | 0.0203 | 0.0370 |
| Across the break | 6: 2026-07-01, its rerun, and women's basketball 2026-10-09 | 0.0065 | 0.0250 | 0.0302 | 0.0704 |
| **Limit**, within / across | | 0.01 / 0.02 | 0.025 / 0.05 | 0.04 / 0.06 | 0.075 / 0.14 |

The break needs its own limits for a structural reason, not because of noise:

- A team's strength is a club level plus a season level, and the data identify only their sum.
- Across the break, the next season's level is the AR(1)'s reversion of the last, so the way the sum is split reaches the forecast.
- The engines split it differently. Off-season basketball totals were 0.4 to 0.8 points apart on average, with women's NUTS converged (Rhat 1.005).
- The split matters for each team that has not played the new season, so the regime is decided per team. One unplayed team puts the run across the break, however many others have played.

## How the single-fixture limits were set

A mean over 10 to 264 fixtures hides an error confined to a few of them, such as one team's indices or one division's map. The single-fixture limits are about twice the largest single-fixture difference measured in each regime. That is the same room the mean limits leave: two to three times their largest measured (in season 2.6 times on the result and 2.2 times on a line, across the break 3.1 and 2.0 times).

The largest single difference grows with the number of fixtures compared. The in-season limits rest on runs of 10 to 146 fixtures, the break limits on 22 to 264.

## What the limits catch

`diagnostics/detection.R` estimates how large an error in one engine's margin breaches the limits, from INLA's own probabilities in the evidence fixtures. Each fixture's effective predictive sd comes from its P(home) and P(away). The script shifts one engine's margin by δ in two ways:

- every fixture by δ;
- one team's fixtures by δ, up at home and down away, done for each team in turn.

It then finds the smallest δ that breaches, in steps of 0.05, with the mean limits alone and with the single-fixture limits added. The engines' measured gap is left out, so a real error breaches sooner. Totals are not covered.

Handball is in goals and basketball in points. Ranges are over the cell-runs of each group. For the one-team rows, the first figure is the median team and the second the team least easily caught.

| Smallest margin error caught | Handball, in season | Basketball, in season | Handball, across the break | Basketball, across the break |
|---|---|---|---|---|
| Every fixture (the mean limits catch it) | 0.25–0.3 | 0.7–0.9 | 0.5–0.65 | 1.4–2.1 |
| One team, with mean limits only | 1.1–3.0; 3.1–9.55 | 7.4–9.5; 15.55–23.05 | 4.85–6.5; 7.6–8.95 | 18.3–23.6; 27.3–38.5 |
| One team, with the single-fixture limits | 0.55–0.65; 0.75–1.15 | 1.45–1.7; 2.6–2.9 | 0.8–1.1; 0.85–1.15 | 2.45–3.6; 2.5–7.4 |

The widest case, 7.4 points, is women's basketball on 2026-10-09. Its predictive sd there was 19.9 points.

## The Rhat of what the bar does not hold

- **The bar holds the forecasts only:** each fixture's predictive location and the observation sd (`hbbb_v2_rhat_groups()`). On every passing fit they were at most 1.007.
- **The other groups converge more slowly.** On those same passing fits, hyperparameter Rhat reached 1.076 and state Rhat 1.046. Both were on men's basketball's margin on 2026-10-09, where the smallest bulk ESS was 46.
- **On men's basketball's margin on 2026-07-01 at 1,000 iterations,** a fit that fails, hyperparameter Rhat was 1.138.

A handball or basketball row's `nuts_max_rhat` is the forecasts' Rhat. A football row's covers the hyperparameters and states. The `nuts_rhat_scope` column says which, and the health check names it in its value.
