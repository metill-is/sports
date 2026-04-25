---
name: bet
description: Use when generating fresh recommendations or viewing the current ones. Runs the decide layer (Kelly + portfolio + calibration) and shows the latest recommendations.
argument-hint: "[--league LEAGUE] [--all] [--dry-run] [show]"
context: fork
---

# /bet — Generate or view betting recommendations

Two modes: **show** the current recommendations, or **run** the decide layer
to generate fresh ones. Bet *placement* lives in `/place-bets`; this skill
stops short of opening a browser.

## Mode 1: Show recommendations (default, or `show`)

Display the latest recommendations from `data/decisions/recommendations/`.
The Parquet store is partitioned by `sport`, `country`, and `run_date` —
the most recent `run_date` per league is what the placer would consume.

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e '
suppressPackageStartupMessages(devtools::load_all())
sports::rebuild_duckdb()
con <- DBI::dbConnect(duckdb::duckdb(), "sports.duckdb", read_only = TRUE)
print(DBI::dbGetQuery(con, "
  SELECT sport, country, sex, match_date, home_team, away_team,
         market, outcome, line, p, odds, ev, kelly, bet_amount
  FROM recommendations
  WHERE run_date = (SELECT MAX(run_date) FROM recommendations)
  ORDER BY ev DESC
"))
'
```

Equivalently, the placer's preview script shows the same set, deduped against
the ledger (i.e. what would actually be placed):

```bash
Rscript scripts/preview_bets.R
```

Present the table to the user. Then suggest:

- "Want me to refresh recommendations? (`/bet --all` or `/sports-update`)"
- "Want me to actually place these? (`/place-bets`)"

If `recommendations/` is empty or stale, run the decide layer first
(Mode 2 below).

## Mode 2: Run the decide layer (`--all`, `--league`, `--dry-run`)

Generate fresh recommendations. The decide layer reads the latest
`beliefs/latest/` snapshot for each (sport, country, sex), prepares odds,
runs joint Kelly + portfolio optimisation + calibration, and writes
candidates + recommendations Parquet.

### Step 1: Dry-run preview

```bash
cd /Users/brynjolfurjonsson/sports && Rscript run.R --all --step decide --dry-run
```

Show the planned targets. Confirm before proceeding (unless the user
already passed `--dry-run`).

### Step 2: Execute

Decide is fast (~seconds) and inline-safe — no detached launch needed.

```bash
Rscript run.R --all --step decide
```

Or for a single league / sex:

```bash
Rscript run.R --league football_iceland --step decide
Rscript run.R --league football_iceland --sex male --step decide
```

### Step 3: Show results

After completion, drop into Mode 1 to display the new recommendations.

## When to use which step

| User intent                                    | Step                                 |
| ---------------------------------------------- | ------------------------------------ |
| "Recompute recommendations against fresh odds" | `--step decide` (assumes fits exist) |
| "Recompute everything end-to-end"              | `--step all` (use `/sports-update`)  |
| "Just refresh the model fits"                  | `--step fit` (use `/sports-update`)  |
| "Place bets I see in the table"                | `/place-bets`                        |

If the latest fit is older than `betting.max_age_hours` (config/leagues.yml),
decide will warn — refit first via `/sports-update --step fit`.

## Error handling

- **"No recommendations rows for run_date X"** → the decide layer hasn't run
  for any league yet, or all runs are filtered out by EV threshold. Run
  Mode 2.
- **"No posterior found"** → fit is missing for that league × sex. Run
  `/sports-update --league {key} --step fit`.
- **"Stale posterior warning"** → fit is older than `max_age_hours`. Refit
  first.
- **"No odds rows for date range"** → `--step odds` hasn't run recently.
  Run `Rscript run.R --all --step odds`.

## Reference

- Decide entrypoint: `R/decide-pipeline.R::decide_league()`
- Sub-stages: `R/decide-{odds,kelly,portfolio,calibration}.R`
- Recommendations Parquet: `data/decisions/recommendations/sport=*/country=*/run_date=*/`
- Candidates Parquet (with stage column for debugging): `data/decisions/candidates/`
- Per-league betting config: `config/leagues.yml::{league}.betting`
- Global Kelly cap + budget: `config/bankroll.yml`
- Placement (separate skill): `/place-bets`
