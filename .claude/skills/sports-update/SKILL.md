---
name: sports-update
description: Use when refreshing models or running the full pipeline. Runs ingest + odds + fit + decide + publish for specified leagues.
argument-hint: "[ingest|odds|fit|decide|publish|all] [--league LEAGUE] [--sex male|female] [--force]"
context: fork
effort: high
---

# /sports-update — Full pipeline update

Wraps the five `scripts/0N_*.R` entry points for the active Icelandic
leagues. Each script has its own freshness guard: `02_scrape_odds.R`
skips when there are no upcoming games; `03_fit.R` skips when no new
games have been played since the last fit. Pass `--force` to bypass.

## Step 1: Parse arguments

Pick the default selector based on the user's phrasing:

| User phrasing                                                 | Default action                              |
| ------------------------------------------------------------- | ------------------------------------------- |
| "run the pipeline", "update the models", "refresh", "full update" | Run all five scripts in order              |
| "just refresh fits" / "rescrape odds" / "republish"           | Run a single script (mapping below)         |
| explicit `--league`, `--sex`                                  | Honour the user                             |

Selectors:

- `--league KEY` — one league (`football_iceland`, `basketball_iceland`, `handball_iceland`)
- `--sex SEX` — restrict to `male`, `female`, or `all`
- `--force` — bypass the freshness guard in `03_fit.R` and `02_scrape_odds.R`

Single-step mapping (when the user asks for one phase only):

| Phrasing                          | Script invocation                              |
| --------------------------------- | ---------------------------------------------- |
| "ingest", "scrape results"        | `scripts/01_ingest_results.R`                  |
| "odds", "scrape odds"             | `scripts/02_scrape_odds.R`                     |
| "fit", "refit", "refresh models"  | `scripts/03_fit.R`                             |
| "decide", "recompute recs"        | `scripts/04_decide.R`                          |
| "publish", "regenerate JSONs"     | `scripts/05_publish.R`                         |
| "settle", "resolve bets", "settle ledger" | `scripts/06_settle.R`                  |

## Step 2: Plan and warn

For full runs, warn that model fitting takes significant time:

- **1 league × 1 sex**: ~5–15 min (1000 MCMC iters)
- **All active × both sexes**: ~1–2 h (basketball fits dominate)

The freshness guards mean that re-running with no new completed games
short-circuits in <1 second per (league × sex) pair. So scheduled re-runs
are cheap; fresh fits only happen when there's new data.

Approximate inference (Pathfinder, ADVI) is **disabled** on the current
models — they crash. Always use the default `sample` method.

## Step 3: Execute (detached launch for long runs)

**Always use a detached launch for long fits.** This skill runs with
`context: fork` — a plain `run_in_background: true` Bash call inside the
fork creates a process that dies when the fork terminates, which has
silently killed pipeline runs partway through chromote-backed scrapes.
`nohup … & disown` detaches the process so it survives.

For a full refresh (chained):

```bash
cd /Users/brynjolfurjonsson/sports && \
  LOG=/tmp/sports-update-$(date +%Y%m%d-%H%M%S).log && \
  nohup bash -c '
    Rscript scripts/00_active_competitions.R && \
    Rscript scripts/01_ingest_results.R && \
    Rscript scripts/02_scrape_odds.R && \
    Rscript scripts/03_fit.R && \
    Rscript scripts/06_settle.R && \
    Rscript scripts/04_decide.R && \
    Rscript scripts/05_publish.R
  ' > "$LOG" 2>&1 & \
  disown && \
  echo "Launched PID $! (log: $LOG)"
```

For a single-step run (replace `03_fit.R` with the relevant script):

```bash
cd /Users/brynjolfurjonsson/sports && \
  LOG=/tmp/sports-update-$(date +%Y%m%d-%H%M%S).log && \
  nohup Rscript scripts/03_fit.R > "$LOG" 2>&1 & \
  disown && \
  echo "Launched PID $! (log: $LOG)"
```

Capture the PID and log path. Report both to the user so they can tail or
kill if needed.

**Monitoring.** Poll the log, not `ps`. The fork's sandbox can hide
processes from user-level `ps` even while running. A simple `until` loop
on `grep -qE "complete|Error" "$LOG"` with a generous timeout (1–3 h
depending on selector + step) is sufficient.

For short steps (`02_scrape_odds.R`, `04_decide.R`, `05_publish.R`)
running inline (without `nohup`) is fine — they complete in seconds.

## Step 4: Verify outputs

After completion, check the relevant Parquet stores:

| Step      | Verifying read                                                                            |
| --------- | ----------------------------------------------------------------------------------------- |
| `ingest`  | `data/facts/results/sport=*/country=*/` partitions touched recently                       |
| `odds`    | `data/facts/odds/sport=*/country=*/scraped_date=*/` has a fresh `scraped_date`            |
| `fit`     | `data/beliefs/latest/sport=*/country=*/sex=*/beliefs.parquet` mtime is recent             |
| `decide`  | `data/decisions/recommendations/sport=*/country=*/run_date=*/` has a fresh `run_date`     |
| `publish` | `data/publish/{football,basketball,handball}/iceland/{karla,kvenna}/*.json` mtime is recent |

Or, more uniformly, via DuckDB:

```bash
Rscript -e 'sports::rebuild_duckdb(); con <- DBI::dbConnect(duckdb::duckdb(), "sports.duckdb", read_only = TRUE); print(DBI::dbGetQuery(con, "SELECT sport, country, MAX(scraped_at) FROM odds GROUP BY 1,2"))'
```

## Step 5: Offer to commit

If the pipeline modified `data/`, offer to commit. Tracked Parquet stores
should be committed alongside model/code changes; ad-hoc rebuilds without
data changes are fine to leave uncommitted.

```bash
cd /Users/brynjolfurjonsson/sports && git add data/ config/ && git status
```

Only commit if the user confirms. Use a descriptive message such as
`"Refresh fits + publish ({date})"` or `"Scrape odds ({date})"`.

## Common workflows

### Pre-betting refresh

```bash
# Update everything, then preview pending bets. Settle runs before decide
# so current_pool reflects realised PnL from any newly-resolved bets.
Rscript scripts/00_active_competitions.R
Rscript scripts/01_ingest_results.R
Rscript scripts/02_scrape_odds.R
Rscript scripts/03_fit.R
Rscript scripts/06_settle.R
Rscript scripts/04_decide.R
Rscript scripts/05_publish.R
Rscript scripts/preview_bets.R
```

### Quick model test (smoke check)

To smoke-test a single fit manually with fewer iterations, call the
model layer directly:

```bash
Rscript -e 'devtools::load_all(); fit_one(load_leagues()$basketball_iceland, "male", iter_sampling = 200, write_archive = FALSE)'
```

### Just refresh decide + publish (after a manual fit)

```bash
Rscript scripts/04_decide.R
Rscript scripts/05_publish.R
```

## Reference

- Pipeline entry points: `scripts/0N_*.R` (one per layer)
- Freshness predicates: `R/pipeline-freshness.R`
- League registry: `config/leagues.yml`
- Per-step R modules: `R/{ingest,model,decide,publish}-*.R`
- Stan models: `Stan/{league_key}/{file}.stan`
- Publishing scaffolds: `R/publish-{football,basketball,handball}-iceland.R`
- Bet placement (separate, local-only): `/place-bets` skill
