---
name: sports-update
description: Use when refreshing models or running the full pipeline. Runs data + fit + results + bet for specified leagues.
argument-hint: "[--sport SPORT] [--league LEAGUE] [--all] [--active] [--iter N] [--step STEPS]"
context: fork
effort: high
---

# /sports-update — Full pipeline update

You are running the Sports unified pipeline. Follow these steps exactly.

## Step 1: Parse arguments

Pick the default selector based on the user's phrasing:

| User phrasing                                                 | Default selector                                      |
| ------------------------------------------------------------- | ----------------------------------------------------- |
| "run the pipeline", "update the models", "refresh"            | `--due` (skips leagues whose fit is still fresh)      |
| "full update", "run everything active", "pre-betting refresh" | `--active` (re-fits every league with upcoming games) |
| explicit `--all`, `--sport`, `--league`, `--country`          | honour the user                                       |

Default steps when none given: `--step data,fit,results,bet --sync`. Always include `--sync` to pull fresh data from `livesport-data` and `lengjan-odds` repos before running. Note: `--due` auto-injects these steps, so when using `--due` you can omit `--step` entirely.

Why this matters: a full `--active` run across the three Icelandic leagues is ~2h (basketball fits dominate at ~24min each × 2 sexes). `--due` skips fits fresher than 48h, often trimming the run by half or more.

Selectors (pick one):

- `--sport handball|basketball|football`
- `--league football_england|handball_denmark|...`
- `--country iceland|england|...`
- `--all` — all 18 leagues
- `--active` — only leagues with upcoming games
- `--stale` — leagues whose fit/results are out of date
- `--due` — auto-run leagues that are due for refresh

Step overrides:

- `--step data` — download fresh data only
- `--step data,fit` — data + model fitting (no results/bets)
- `--step fit --iter 200` — quick test fit (200 iterations instead of 1000)
- `--step data,fit,results,bet` — full pipeline (default)

Other flags: `--sex male|female`, `--sync` (default on), `--dry-run`, `--no-plots` (skip figure rendering), `--method sample` (approximate inference is disabled; keep `sample`)

## Step 2: Dry-run preview

Always preview first (include `--sync` so repos are pulled even in dry-run):

```bash
cd /Users/brynjolfurjonsson/sports/Sports && Rscript run.R {SELECTOR} {STEPS} --sync --dry-run
```

Show the plan to the user. For `--step fit`, warn that model fitting takes significant time:

- **1 league, 1 sex**: ~5-15 minutes (1000 iterations)
- **All active**: could be 1+ hours
- **Quick test** (`--iter 200`): ~2-3 minutes per league

## Step 3: Execute pipeline (detached launch)

**Always use a detached launch.** This skill runs with `context: fork` — a plain `run_in_background: true` Bash call inside the fork creates a process that dies when the fork terminates, which has silently killed pipeline runs partway through the handball scrape. `nohup … & disown` detaches the process from the skill's session so it survives.

```bash
cd /Users/brynjolfurjonsson/sports/Sports && \
  LOG=/tmp/sports-update-$(date +%Y%m%d-%H%M%S).log && \
  nohup Rscript run.R {SELECTOR} {STEPS} --sync {FLAGS} > "$LOG" 2>&1 & \
  disown && \
  echo "Launched PID $! (log: $LOG)"
```

Capture the PID and log path from stdout. Report both to the user so they can tail or kill if needed.

**Monitoring.** Poll the log rather than the process — the phased progress bar writes `Pipeline complete: N/M steps succeeded` on success or a non-zero exit + stack trace on failure. A simple `until` loop on `grep -qE "Pipeline complete|Error" "$LOG"` with a generous timeout (1–3 h depending on selector + step) is sufficient.

**Do not poll `ps` for the PID as the sole liveness check** — the fork's sandbox can hide the process from user-level `ps` even while it's running. Trust the log.

## Step 4: Verify outputs

After completion, check key outputs exist:

For `--step data`:

- Data CSVs updated in `Sports/{sport}/{country}/data/`

For `--step fit`:

- `fit.rds` or equivalent model output in results directory
- Check file timestamps are recent

For `--step results`:

- PNG figures in results directory
- `posterior_goals.csv` for betting

For `--step bet`:

- Betting output printed
- `{sport}/{country}/history/recommendations.csv` updated with any new recommendations
- **Do NOT expect `bets_log.csv` to be written** — the Sports pipeline never writes the ledger. That file is updated only when `lengjan-bets` is run separately (via `/place-bets` or `Rscript lengjan-bets/run.R`).

## Step 5: Offer to commit

If the pipeline modified files in git-tracked league directories, offer to commit:

```bash
cd /Users/brynjolfurjonsson/sports/Sports/{sport}/{country} && git add -A && git status
```

Only commit if the user confirms. Use a descriptive message like:
`"Update {sport}/{country} model ({date})"`

## Common workflows

### Pre-betting refresh

```bash
# Update data + refit models for leagues you'll bet on
Rscript run.R --active --step data,fit
# Then run bets
Rscript run.R --active --step bet
```

### Quick model test

```bash
# Test with fewer iterations to verify code works
Rscript run.R --league basketball_iceland --step fit --iter 200
```

### Full refresh (weekend task)

```bash
# Update everything — takes 1+ hours
Rscript run.R --all --step data,fit,results
```

## Reference

- Pipeline entry point: `Sports/run.R`
- League registry: `Sports/config/leagues.yml`
- Step dispatchers: `Sports/R/pipeline/step_{data,fit,bet}.R`
- Config loader: `Sports/R/pipeline/config.R`
- Schedule scanner: `Sports/R/schedule/scan.R`
- Stan models: `Sports/Stan/` (canonical copies)
