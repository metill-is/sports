# International handball: the nations model and the championship forecasts

Women's EHF EURO 2026 (3–20 December 2026) and men's IHF World Championship 2027
(13–31 January 2027). Work of 24 November 2026; owner's go-ahead the same day.

| File | What it is |
| --- | --- |
| `tournament-facts.md` | Dates, venues, groups, schedules, format and tie-break rules of both championships, each with its EHF / IHF source and page |
| `report.md` | The model, the backtest against the legacy model, the forecast and what is not done |
| `backtest.R`, `legacy.R`, `summarise.R` | The backtest: v2 and the legacy Stan program fitted before each of eight recent championships, scored on their matches; the aggregate tables |
| `backtest_scores.csv`, `backtest_paired.csv`, `backtest_calibration.csv` | The backtest's aggregate tables (written by `summarise.R`) |

Code: `R/ih-data.R` (match table, seasons, venues), `R/ih-model.R` (the nations
model on the handball v2 engine), `R/ih-simulate.R` (championship simulator under
the EHF / IHF rules), `R/ih-publish.R` (the five files metill-platform renders),
`scripts/ih/run_tournament.R` (fit, simulate, publish). Config:
`config/intl-handball/` (hosts; the venues of the matches hosts played at
co-hosted championships, `championship_venues.csv`; the venues of the qualification
tournaments played at one venue, `qualifier_venues.csv`; team names; each
championship's groups and schedule). Tests: `tests/testthat/test-ih-*.R`.

The match table is not in this repository: the scripts read it from the folder
named by `IH_DATA_DIR`, and the backtest writes its per-match output to the folder
named by `IH_RESULTS_DIR`.

Rebuild the forecast (local only, no workflow runs it):

```bash
IH_DATA_DIR=<folder> Rscript scripts/ih/run_tournament.R --tournament euro2026w
```

Rerun the backtest and its tables:

```bash
IH_DATA_DIR=<folder> IH_RESULTS_DIR=<folder> ml run -- Rscript docs/reports/2026-11-24-intl-handball/backtest.R --models v2,base,legacy
IH_RESULTS_DIR=<folder> Rscript docs/reports/2026-11-24-intl-handball/summarise.R
```
