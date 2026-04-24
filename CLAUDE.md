# Sports — CLAUDE.md

Bayesian sports prediction and automated betting for Icelandic football, basketball, and handball. **Consolidated monorepo** (pre-migration was four separate repos: `Sports/`, `lengjan-odds/`, `livesport-data/`, `lengjan-bets/` — all preserved under `_legacy/`).

> **Current focus (2026-04-24):** 3 active Icelandic leagues only (`basketball_iceland`, `handball_iceland`, `football_iceland`). Non-Icelandic leagues paused and will be reactivated incrementally from later plans. All user-facing content is in Icelandic.

## Status

Mid-migration. End-state design: [`docs/superpowers/specs/2026-04-24-sports-pipeline-redesign-design.md`](docs/superpowers/specs/2026-04-24-sports-pipeline-redesign-design.md). Implementation plans: [`docs/superpowers/plans/`](docs/superpowers/plans/).

| Plan | Scope | Status |
|---|---|---|
| **1: Foundation + Storage + ETL** | Monorepo init, storage layer, ETL for odds + ledger | ✅ Complete (Task 10 results/schedules deferred — legacy match-data CSVs were gitignored and lost; regenerating via scrapers moved to Plan 4) |
| 2: Model layer | `prepare_data()` + `fit()` + posteriors for 3 leagues | Pending |
| 3: Decide + Placer + Publish | Kelly, portfolio, bet placement, website JSON | Pending |
| 4: Ingest + Orchestration + CI | Scrapers (federations + Lengjan), `{targets}` DAG, CI workflows, metill-platform pull, cutover. **Also backfills the deferred Task 10 ETL.** | Pending |

## Directory structure

```
sports/
├── config/
│   ├── leagues.yml                 # Single source of truth for league metadata
│   └── leagues.schema.json         # JSON Schema validator
├── R/                              # Package source
│   ├── config.R                    # load_leagues() + filter_leagues()
│   ├── storage-schemas.R           # Arrow schemas for 8 tables
│   ├── storage.R                   # write_table() + read_table() primitives
│   └── duckdb-views.R              # rebuild_duckdb() — regenerable SQL view layer
├── Stan/                           # (Plan 2 populates)
├── data/                           # Parquet stores (git-tracked, hive-partitioned)
│   ├── facts/
│   │   ├── odds/                   # Lengjan odds snapshots (long-form, scraped_at, line, market)
│   │   ├── results/                # (Plan 4 backfill)
│   │   └── schedules/              # (Plan 4 backfill)
│   ├── beliefs/                    # (Plan 2 populates: latest/ + archive/)
│   └── decisions/
│       ├── ledger/                 # Placed-bet history (13 legacy ledgers ETL'd, total 1,870 rows, PnL = 89,369 ISK)
│       ├── candidates/             # (Plan 3 populates)
│       └── recommendations/        # (Plan 3 populates)
├── scripts/etl/                    # One-time legacy-CSV → Parquet migrations
│   ├── 03_etl_odds.R               # Lengjan odds → facts/odds/
│   └── 04_etl_ledger.R             # Legacy bets_log.csv → decisions/ledger/
├── tests/testthat/                 # 64 passing assertions across test-config, test-storage*, test-duckdb-views, test-etl-validation
├── sports.duckdb                   # Gitignored; rebuildable via rebuild_duckdb()
├── docs/superpowers/               # Specs + plans
└── _legacy/                        # Subtree-merged histories of the 4 predecessor repos
    ├── sports/                     # ex metill-is/sports
    ├── lengjan-odds/               # ex metill-is/lengjan-odds
    ├── livesport-data/             # ex metill-is/livesport-data
    └── lengjan-bets/               # ex metill-is/lengjan-bets (placer subsystem)
```

## Local-only subsystem

`R/placer/` (added in Plan 3) places bets against Lengjan via browser automation using credentials in `.Renviron` (template: `.Renviron.example`). It is **never** executed on CI — no workflow invokes it and no GitHub Actions secret named `LENGJAN_*` is configured. Enforced by `.claude/rules/sports-betting.md` conventions.

## Quick reference

```bash
# Development
Rscript -e 'devtools::load_all()'
Rscript -e 'devtools::test()'

# Rebuild sports.duckdb after fresh Parquet writes
Rscript -e 'sports::rebuild_duckdb()'

# Query any table via DuckDB
Rscript -e '
con <- DBI::dbConnect(duckdb::duckdb(), "sports.duckdb", read_only = TRUE)
print(DBI::dbGetQuery(con, "SELECT sport, country, COUNT(*) AS n, SUM(pnl) AS pnl FROM ledger WHERE settled GROUP BY 1,2 ORDER BY 1"))
'

# ETL from legacy (one-time; scripts idempotent per table partition)
Rscript scripts/etl/03_etl_odds.R
Rscript scripts/etl/04_etl_ledger.R
```

## Conventions

### R package structure

- `DESCRIPTION` + `NAMESPACE` treat the monorepo as an R package — `devtools::load_all()` / `devtools::test()` are the daily drivers.
- Exports: `load_leagues`, `filter_leagues`, `schemas`, `write_table`, `read_table`, `rebuild_duckdb`. Internal helpers are `#' @noRd`.
- `testthat` edition 3, tests in `tests/testthat/`.
- See `.claude/rules/r-package-conventions.md` and `.claude/rules/r-conventions.md`.

### Column-naming convention (spec §3.3)

Internal schemas use English throughout. Canonical column names: `home_team` / `away_team` / `match_date` / `fit_date` / `scraped_at` / `placed_at` / `p` (probability) / `odds` / `odds_placed` / `kelly` / `bet_amount` / `line`. Icelandic only appears at the publish boundary (Plan 3).

### Data formats

- **Parquet** for every store in `data/` (hive-partitioned, schema-validated by `write_table()`).
- **DuckDB** (`sports.duckdb`, gitignored) gives SQL query access via views over the Parquet paths.
- **CSV** only for ingest from legacy sources during ETL.

### Stan models

`.claude/rules/stan-conventions.md` — `cmdstanr`, non-centred parameterisations, `generated quantities` for posterior-predictive checks.

### Betting conventions

`.claude/rules/sports-betting.md` — P1–P4 placement rules, bets.yml schema (Plan 3 adds).

## Skills

The four model-invocable skills under `.claude/skills/` (`/bet`, `/sports-update`, `/add-league`, `/place-bets`) remain from the pre-migration workspace and continue to work against the legacy paths. They will be revised/superseded by Plan 4 when the `{targets}` DAG lands.

**Do not add `disable-model-invocation: true` to these skills.** They are intentionally model-invocable.

## Obsidian Output

Vault: `Metill` (MCP) / `~/Obsidian/Metill/` (direct path). Prefer MCP `write_note`.
Handoff: `Sports/Sports Handoff.md`.

### Relevant Knowledge topics

| Topic folder                       | Content                                            |
| ---------------------------------- | -------------------------------------------------- |
| `Knowledge/Betting Optimisation/`  | Kelly criterion, calibration, placement rules, PnL |
| `Knowledge/Sports Models/`         | Bayesian model theory, Stan implementation, goals  |
| `Knowledge/Lengjan Pipeline/`      | Odds scraping, schedule-aware filtering            |
| `Knowledge/Livesport Data/`        | Match data scraping, CI pipeline                   |

Each topic has a `_MOC.md` entry point — read it first, then selectively load sub-documents.

## Things 3

Route actionable tasks to the **Metill.is** area (ID: `4WyyavEFjCPunRi9iD5tKe`), project **Sports**.
