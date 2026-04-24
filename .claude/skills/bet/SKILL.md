---
name: bet
description: Use when placing bets or viewing recommendations. Runs the Sports betting pipeline for active or specified leagues.
argument-hint: "[--sport SPORT] [--league LEAGUE] [--all] [--active] [--dry-run] [show] [log 1,3,5] [log all]"
context: fork
---

# /bet — Sports betting workflow

Three modes depending on arguments:

## Mode 1: Show recommendations (default, or `show`)

Display current recommendations from the last pipeline run.

```bash
cd /Users/brynjolfurjonsson/sports/Sports && Rscript R/bets/log_placed.R --show
```

Present the table to the user. The table is sorted by expected profit (E[PnL]).

After showing, remind the user:

- "Tell me which bets you've placed (e.g., `/bet log 1,3,5` or `/bet log all`)"
- If recommendations.csv is missing or empty, suggest running the pipeline first: `/sports-update --active` or `/bet --active`

## Mode 2: Run pipeline (`--active`, `--sport`, `--league`, `--all`, etc.)

Run the betting pipeline to generate fresh recommendations. Parse the selector from arguments.

### Step 1: Dry-run preview

```bash
cd /Users/brynjolfurjonsson/sports/Sports && Rscript run.R {SELECTOR} --step bet --sync --dry-run
```

Show output and confirm before proceeding (unless user passed `--dry-run` only).

### Step 2: Execute

```bash
cd /Users/brynjolfurjonsson/sports/Sports && Rscript run.R {SELECTOR} --step bet --sync
```

### Step 3: Show results

After the pipeline completes, display the recommendations table:

```bash
cd /Users/brynjolfurjonsson/sports/Sports && Rscript R/bets/log_placed.R --show
```

Present the sorted table. Then tell the user:

- "Review these recommendations. After placing bets on Lengjan, tell me which ones — e.g., `/bet log 1,3,5` or `/bet log all`"

## Mode 3: Log placed bets (`log <indices>`)

When the user says `log 1,3,5` or `log all`, log those specific bets from recommendations.csv.

```bash
cd /Users/brynjolfurjonsson/sports/Sports && Rscript R/bets/log_placed.R {INDICES}
```

Where `{INDICES}` is `all` or comma-separated row numbers (e.g., `1,3,5`).

After logging, summarise: how many bets logged, total stake, leagues affected.

The user may also describe placed bets conversationally (e.g., "I placed the Germany and Spain ones" or "all except #8 and #12"). Map their description to row numbers and confirm before logging.

## Error handling

- **"No recommendations.csv found"**: Pipeline needs running → suggest `/sports-update --active` or `/bet --active`
- **"No posterior found"**: Model needs refreshing → suggest `/sports-update --league {key} --step data,fit,results`
- **"No odds data"**: Odds need scraping → suggest running lengjan-odds pipeline
- **Stale posterior warning**: Suggest `--step fit` first

## Reference

- Recommendations file: `Sports/recommendations.csv` (overwritten each pipeline run)
- Log script: `Sports/R/bets/log_placed.R`
- Pipeline entry: `Sports/run.R`
- Per-league config: `Sports/{sport}/{country}/config/bets.yml`
- History logs: `Sports/{sport}/{country}/history/bets_log.csv`
- Parquet store: `Sports/store/bets/`
