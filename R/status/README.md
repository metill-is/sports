# Sports Pipeline Status API

JSON endpoints for the Raycast betting pipeline monitor extension.

## Scripts

### `pipeline_status.R` — Full pipeline status

Returns a single JSON object with all monitoring signals. Designed to be polled
every ~15 minutes by a Raycast menu bar extension.

```bash
cd Sports/
Rscript R/status/pipeline_status.R              # Full status (~0.9s)
Rscript R/status/pipeline_status.R --quick       # Skip settleable check (~0.8s)
```

**Output** (stdout): JSON. All warnings/messages go to stderr.

**JSON structure:**

```jsonc
{
  "timestamp": "2026-03-07T11:35:04",

  "recommendations": {
    "total": 45,                              // Total pending recommendations
    "today": 2,                               // Recommendations for today's matches
    "tomorrow": 5,                            // Recommendations for tomorrow
    "total_stake": 27568,                     // Sum of all bet_amount values
    "by_date": [                              // Grouped by match date
      { "date": "2026-03-07", "count": 13, "stake": 6543 }
    ]
  },

  "unsettled": {
    "total": 51,                              // Total unsettled bets across all leagues
    "by_league": [                            // Per-league breakdown
      { "league": "football_england", "count": 15 }
    ],
    "settleable": {                           // Bets that CAN be settled (results exist)
      "count": 5,                             // 0 = no settleable bets
      "wins": 3,
      "losses": 2,
      "total_pnl": 1240,                      // Projected PnL if settled
      "bets": [                               // Individual settleable bets
        {
          "date_match": "2026-03-06",
          "sport": "handball", "country": "france", "sex": "male",
          "market": "totals", "outcome": "over",
          "home": "Montpellier", "away": "PSG",
          "odds": 1.89, "bet_amount": 457,
          "info": "63.5",                     // Handicap line or totals limit
          "home_score": 32, "away_score": 35,
          "win": true, "pnl": 407             // Preview — not yet written to ledger
        }
      ]
    }
  },

  "freshness": {
    "livesport_data": {                       // Match results scraper
      "last_commit": "2026-03-07T06:00:00+0000",
      "age_hours": 5.5                        // null if repo not found
    },
    "lengjan_odds": {                         // Betting odds scraper
      "last_commit": "2026-03-07T08:00:00+0000",
      "age_hours": 3.5
    },
    "models": [                               // Per-league model freshness
      {
        "league": "football_england",
        "fit_time": "2026-03-06T14:00:00",    // null if fit.rds missing
        "age_hours": 21.5
      }
    ],
    "data_sync_stale": false                  // true if livesport-data has newer commits
                                              //   than Sports data files (run data sync)
  },

  "bankroll": {
    "initial_pool": 23610,                    // From config/bankroll.yml
    "current_pool": 24150,                    // initial + settled_pnl - outstanding
    "outstanding": 8420,                      // Unsettled bet amounts
    "settled_pnl": 960,                       // Total settled PnL since epoch
    "week_pnl": 540                           // Settled PnL from last 7 days
  }
}
```

**Key signals for the Raycast extension:**

| Signal | JSON path | Trigger notification when... |
|--------|-----------|------------------------------|
| Settleable bets | `unsettled.settleable.count` | Increases from 0 → N |
| Today's bets | `recommendations.today` | > 0 (morning alert) |
| CI failure | *(check via `gh api` directly)* | `conclusion != "success"` |
| Stale model | `freshness.models[].age_hours` | > 48 and league has unsettled bets |
| Data sync needed | `freshness.data_sync_stale` | `true` |
| Large PnL event | `unsettled.settleable.total_pnl` | abs > 1000 after settlement |


### `settle_now.R` — Run settlement

Settles all unsettled bets that have results available. Writes to `bets_log.csv`
and Parquet store.

```bash
Rscript R/status/settle_now.R                           # Settle all leagues
Rscript R/status/settle_now.R --dry-run                  # Preview only (no writes)
Rscript R/status/settle_now.R --league football_england  # One league only
```

**Output** (stdout): JSON.

```jsonc
{
  "timestamp": "2026-03-07T12:00:00",
  "dry_run": false,
  "settled": [
    {
      "date_match": "2026-03-06",
      "sport": "handball", "country": "france", "sex": "male",
      "market": "totals", "outcome": "over",
      "home": "Montpellier", "away": "PSG",
      "odds": 1.89, "bet_amount": 457, "info": "63.5",
      "home_score": 32, "away_score": 35,
      "win": true, "pnl": 407
    }
  ],
  "summary": {
    "count": 5,
    "wins": 3,
    "losses": 2,
    "total_pnl": 1240
  },
  "still_pending": 46
}
```

## Integration with Raycast Extension

### Recommended polling strategy

```
Every 15 minutes:
  1. Call: Rscript R/status/pipeline_status.R
  2. Parse JSON, update menu bar icon/badge
  3. Compare settleable.count against cached previous value
  4. If increased: send macOS notification

On user action "Settle All":
  1. Call: Rscript R/status/settle_now.R
  2. Parse JSON, show settlement results in HUD
  3. Refresh status to update menu bar
```

### CI status (not in R script — call directly from extension)

The R scripts handle local state only. For CI workflow status, call the GitHub
API directly from the Raycast extension:

```bash
# Latest run status for each repo
gh api repos/metill-is/livesport-data/actions/runs?per_page=1 \
  --jq '{status: .workflow_runs[0].status, conclusion: .workflow_runs[0].conclusion}'

gh api repos/metill-is/lengjan-odds/actions/runs?per_page=1 \
  --jq '{status: .workflow_runs[0].status, conclusion: .workflow_runs[0].conclusion}'
```

### Triggering workflows

```bash
gh workflow run scrape.yml --repo metill-is/livesport-data
gh workflow run scrape.yml --repo metill-is/lengjan-odds
```

### Triggering full pipeline

```bash
cd Sports/ && Rscript run.R --active --step data,fit,results,bet
```

### Required environment

- R (>= 4.0) with: dplyr, readr, jsonlite, yaml, here
- Scripts must be run from `Sports/` directory (uses `here::here()`)
- Git repos at `../livesport-data/` and `../lengjan-odds/` (optional — freshness will be null)
- `gh` CLI for CI status checks (optional — called by extension, not R scripts)

## File locations

| File | Role | Updated by |
|------|------|------------|
| `recommendations.csv` | Pending bet recommendations | `run.R --step bet` |
| `*/*/history/bets_log.csv` | Bet ledger (one per league) | `lengjan-bets` (placement), `settle_now.R` (settlement) |
| `config/bankroll.yml` | Bankroll config | Manual |
| `config/leagues.yml` | League registry | Manual |
| `*/results/*/fit.rds` | Stan model fits | `run.R --step fit` |
| `store/` | Hive-partitioned Parquet mirror | `settle_now.R`, `step_fit.R`, `lengjan-bets` |
