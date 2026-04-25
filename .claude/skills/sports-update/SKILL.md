---
name: sports-update
description: Use when refreshing models or running the full pipeline. Runs ingest + odds + fit + decide + publish for specified leagues.
argument-hint: "[--league LEAGUE] [--all] [--step STEPS] [--sex male|female]"
context: fork
effort: high
---

# /sports-update — Full pipeline update

Wraps `Rscript run.R` (the thin CLI over `targets::tar_make()`) for the
three active Icelandic leagues. The DAG handles staleness automatically:
fresh targets are skipped, only out-of-date ones rerun.

## Step 1: Parse arguments

Pick the default selector based on the user's phrasing:

| User phrasing                                                 | Default selector                         |
| ------------------------------------------------------------- | ---------------------------------------- |
| "run the pipeline", "update the models", "refresh", "full update" | `--all --step all`                       |
| explicit `--league`, `--sex`, `--step`                        | honour the user                          |

`--all --step all` = the full pipeline for every active league. Targets that
are still fresh are skipped automatically.

Selectors:

- `--league KEY` — one league (`football_iceland`, `basketball_iceland`, `handball_iceland`)
- `--sex SEX` — restrict to `male`, `female`, or `all`
- `--all` — every active league (currently the three Icelandic ones)

Step overrides (`--step` accepts one value at a time):

- `--step data` — federation results + schedules only (KSÍ / KKÍ / HSÍ scrapers)
- `--step odds` — Lengjan odds scrape only
- `--step fit` — Stan fits only (long; see timing below)
- `--step decide` — Kelly + portfolio + calibration only
- `--step publish` — JSON snapshots only
- `--step all` — everything in DAG order (default)

Other flags: `--dry-run` (print planned targets), `--help`.

## Step 2: Dry-run preview

Always preview first. The DAG will print the targets that would actually run
(skipping any already up to date):

```bash
cd /Users/brynjolfurjonsson/sports && Rscript run.R --all --step all --dry-run
```

Show the plan to the user. For `--step fit`, warn that model fitting takes
significant time:

- **1 league × 1 sex**: ~5–15 min (1000 MCMC iters)
- **All active × both sexes**: ~1–2 h (basketball fits dominate)

Approximate inference (Pathfinder, ADVI) is **disabled** on the current
models — they crash. Always use the default `sample` method.

## Step 3: Execute (detached launch)

**Always use a detached launch for long fits.** This skill runs with
`context: fork` — a plain `run_in_background: true` Bash call inside the fork
creates a process that dies when the fork terminates, which has silently
killed pipeline runs partway through chromote-backed scrapes. `nohup … &
disown` detaches the process so it survives.

```bash
cd /Users/brynjolfurjonsson/sports && \
  LOG=/tmp/sports-update-$(date +%Y%m%d-%H%M%S).log && \
  nohup Rscript run.R {SELECTOR} > "$LOG" 2>&1 & \
  disown && \
  echo "Launched PID $! (log: $LOG)"
```

Capture the PID and log path. Report both to the user so they can tail or
kill if needed.

**Monitoring.** Poll the log, not `ps`. The fork's sandbox can hide processes
from user-level `ps` even while running. A simple `until` loop on
`grep -qE "Pipeline complete|Error" "$LOG"` with a generous timeout (1–3 h
depending on selector + step) is sufficient.

For short steps (`--step odds`, `--step decide`, `--step publish`) running
inline (without `nohup`) is fine — they complete in seconds to a few minutes.

## Step 4: Verify outputs

After completion, check the relevant Parquet stores or DuckDB views:

| Step      | Verifying read                                                                            |
| --------- | ----------------------------------------------------------------------------------------- |
| `data`    | `data/facts/results/sport=*/country=*/` partitions touched recently                       |
| `odds`    | `data/facts/odds/` partitions touched recently                                            |
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
# Update everything, then preview pending bets
Rscript run.R --all --step all
Rscript scripts/preview_bets.R
```

### Quick model test (smoke check)

The targets DAG doesn't expose an `--iter` flag. To smoke-test a single fit
manually, call the model layer directly:

```bash
Rscript -e 'devtools::load_all(); fit_league("basketball_iceland", "male", iter_sampling = 200, write_archive = FALSE)'
```

### Just refresh decide + publish (after a manual fit)

```bash
Rscript run.R --all --step decide
Rscript run.R --all --step publish
```

## Reference

- Pipeline entry: `run.R` (thin CLI)
- Targets DAG: `_targets.R`
- League registry: `config/leagues.yml`
- Per-step R modules: `R/{ingest,model,decide,publish}-*.R`
- Stan models: `Stan/{league_key}/{file}.stan`
- Publishing scaffolds: `R/publish-{football,basketball,handball}-iceland.R`
- Bet placement (separate, local-only): `/place-bets` skill
