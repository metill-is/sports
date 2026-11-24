# Weekly Stan reference fit for handball and basketball v2: measurements

What the limits in `HBBB_REFERENCE_LIMITS`, the NUTS bar and `adapt_delta` 0.99
(`R/model-v2-hbbb-reference.R`) were set from. Measured on the Mac (M1 Pro,
shared, inside the machine limiter) on 2026-11-24 with R-INLA 25.6.7 and
CmdStan 2.40.0 (CI pins 2.38.0), on `data/` as main held it that day. Nothing
was written to `data/health/engine_reference/` (`--no-write`).

Each date folder is one run of

```bash
Rscript scripts/engine_reference.R --sport handball,basketball --end-date <date> --no-write --out <folder>
```

(`2026-11-24` without `--sport`, as CI runs it; its football fixture files are
left out, their summaries kept). Per cell:

- `summary_<sport>_<sex>.csv`: the row `engine_reference` would store, plus
  the cell's seconds;
- `diagnostics_<sport>_<sex>.csv`: per half, NUTS divergences, Rhat by group
  (`rhat_pred`, which the reference is held to; `rhat_hyper`; `rhat_states`,
  the club and season levels), the smallest bulk ESS, NUTS seconds, and the
  INLA mode status and gate;
- `fixtures_<sport>_<sex>.csv`: per fixture, both engines' result, spread-line
  and total-line probabilities and their expected margin and total.

| Folder | What it is |
|---|---|
| `2026-11-24-adapt095/` | The first run, before the two changes below: `adapt_delta` 0.95 and Rhat held over every hyperparameter and state. Three of four cells failed the NUTS bar while the engines agreed to 0.002 to 0.007. |
| `diagnostics/` | `rhat_diag.R` (the script) and its output. `hb_f_1000_095.log`: handball women's high Rhat is all on one team's club and season levels and their sds (1.022 to 1.026) with the locations at 1.003. `bb_f_1000_095.log` / `bb_f_1000_099.log`: basketball women's 41 margin divergences at 0.95, none at 0.99. |
| `2026-11-24/` | The final settings, today's data (one mid-season pairing cell: handball women had no fixture until 2026-12-10). |
| `2026-03-16/` | Late regular season. |
| `2026-07-01/` | Off-season: every pairing of each division at the next season's first scheduled fixture, across the season break. |
