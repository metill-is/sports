# Weekly Stan reference fit for handball and basketball v2: measurements

This folder holds the measurements behind three settings in `R/model-v2-hbbb-reference.R`:

- the limits in `HBBB_REFERENCE_LIMITS`;
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

- `summary_<sport>_<sex>.csv`: the row `engine_reference` would store, plus the cell's seconds.
- `diagnostics_<sport>_<sex>.csv`, per half:
  - NUTS divergences;
  - Rhat by group: `rhat_pred`, which the reference is held to; `rhat_hyper`; and `rhat_states`, the club and season levels;
  - the smallest bulk ESS and the NUTS seconds;
  - the INLA mode status and gate.
- `fixtures_<sport>_<sex>.csv`, per fixture: both engines' result, spread-line and total-line probabilities, and their expected margin and total.

| Folder | What it is |
|---|---|
| `2026-11-24-adapt095/` | The first run, before the NUTS changes: `adapt_delta` 0.95, and Rhat held over every hyperparameter and state. Three of four cells failed the NUTS bar, while the engines agreed to between 0.002 and 0.007. |
| `diagnostics/` | `rhat_diag.R` (the script) and its output. See the list below. |
| `2026-11-24/` | Today's data, at the final settings. Women's handball is a mid-season pairing cell, because it had no fixture until 2026-12-10. |
| `2026-10-09/` | A round or two into each season, at the final settings. |
| `2026-03-16/` | Late regular season. |
| `2026-07-01/` | Off-season: every pairing of each division at the next season's first scheduled fixture, across the season break. Sampled with 1,000 iterations per chain. |
| `2026-07-01-break2000/` | Men's basketball on that date at the final settings, which sample 2,000 iterations per chain across the break. |

The files in `diagnostics/`:

- `hb_f_1000_095.log`: women's handball. The high Rhat is all on one team's club and season levels and their sds (1.022 to 1.026); the locations are at 1.003.
- `bb_f_1000_095.log` and `bb_f_1000_099.log`: women's basketball has 41 margin divergences at 0.95 and none at 0.99.
- `bb_m_0701_2000_099.log`: men's basketball at 2026-07-01 with 2,000 sampling iterations. Forecast Rhat is 1.003 (against 1.023 at 1,000), and margin hyperparameter Rhat is 1.044 (against 1.14).

## Every cell-run, judged by the final rule

The `passed`, `threshold` and `line_threshold` in each summary are what the code of that run stored. The runs for 2026-03-16, 2026-07-01 and 2026-11-24 came before the two regimes, and stored 0.01 for everything. The table below judges every run by the final rule:

- Mean absolute differences must be at most 0.01 on the result and 0.025 on each line within a season. Across the break the limits are 0.02 and 0.05.
- Forecast Rhat (`rhat_pred`, worst half) must be at most 1.02.
- Divergences must be at most 0.5 % per half.
- Both INLA mode statuses must be 0, and the gate must pass.

| Date | Cell | Fixtures | Result | Spread line | Total line | Rhat | Time | Final rule |
|---|---|---|---|---|---|---|---|---|
| 2026-03-16 | handball men | 10 upcoming | 0.0014 | 0.0023 | 0.0026 | 1.003 | 160 s | passes |
| 2026-03-16 | handball women | 18 upcoming | 0.0019 | 0.0044 | 0.0038 | 1.002 | 149 s | passes |
| 2026-03-16 | basketball men | 24 upcoming | 0.0017 | 0.0031 | 0.0058 | 1.005 | 484 s | passes |
| 2026-03-16 | basketball women | 19 upcoming | 0.0021 | 0.0042 | 0.0036 | 1.003 | 465 s | passes |
| 2026-10-09 | handball men | 25 upcoming | 0.0030 | 0.0055 | 0.0042 | 1.004 | 205 s | passes |
| 2026-10-09 | handball women | 17 upcoming | 0.0039 | 0.0075 | 0.0039 | 1.006 | 148 s | passes |
| 2026-10-09 | basketball men | 31 upcoming | 0.0021 | 0.0036 | 0.0085 | 1.005 | 699 s | passes |
| 2026-10-09 | basketball women | 22 upcoming | 0.0037 | 0.0079 | 0.0047 | 1.004 | 533 s | passes |
| 2026-11-24 | handball men | 26 upcoming | 0.0011 | 0.0022 | 0.0027 | 1.003 | 166 s | passes |
| 2026-11-24 | handball women | 146 mid-season pairings | 0.0031 | 0.0055 | 0.0063 | 1.004 | 166 s | passes |
| 2026-11-24 | basketball men | 18 upcoming | 0.0022 | 0.0041 | 0.0116 | 1.007 | 663 s | passes |
| 2026-11-24 | basketball women | 22 upcoming | 0.0017 | 0.0040 | 0.0040 | 1.003 | 529 s | passes |
| 2026-07-01 | handball men | 264 off-season pairings | 0.0019 | 0.0033 | 0.0073 | 1.005 | 151 s | passes |
| 2026-07-01 | handball women | 112 off-season pairings | 0.0065 | 0.0106 | 0.0083 | 1.004 | 148 s | passes |
| 2026-07-01 | basketball men | 264 off-season pairings | 0.0037 | 0.0059 | 0.0184 | 1.023 | 695 s | fails: Rhat, at 1,000 iterations |
| 2026-07-01 | basketball women | 180 off-season pairings | 0.0039 | 0.0066 | 0.0250 | 1.005 | 589 s | passes |
| 2026-07-01 | basketball men, 2,000 iterations | 264 off-season pairings | 0.0032 | 0.0054 | 0.0189 | 1.006 | 996 s | passes |

Within a season (twelve cell-runs at three dates), the largest gaps were 0.0039 on the result and 0.0116 on a line. Across the break (2026-07-01) they were 0.0065 and 0.0250.

The break needs its own limits for a structural reason, not because of noise:

- A team's strength is a club level plus a season level, and the data identify only their sum.
- Across the break, the next season's level is the AR(1)'s reversion of the last, so the way the sum is split reaches the forecast.
- The engines split it differently. Off-season basketball totals were 0.4 to 0.8 points apart on average, with women's NUTS converged (Rhat 1.005).
