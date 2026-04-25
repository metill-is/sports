# Sports — CLAUDE.md

Bayesian sports prediction and automated betting for Icelandic football, basketball, and handball. **Consolidated monorepo** (pre-migration was four separate repos: `Sports/`, `lengjan-odds/`, `livesport-data/`, `lengjan-bets/` — all preserved under `_legacy/`).

> **Current focus (2026-04-24):** 3 active Icelandic leagues only (`basketball_iceland`, `handball_iceland`, `football_iceland`). Non-Icelandic leagues paused and will be reactivated incrementally from later plans. All user-facing content is in Icelandic.

## Status

Mid-migration. End-state design: [`docs/superpowers/specs/2026-04-24-sports-pipeline-redesign-design.md`](docs/superpowers/specs/2026-04-24-sports-pipeline-redesign-design.md). Implementation plans: [`docs/superpowers/plans/`](docs/superpowers/plans/).

| Plan | Scope | Status |
|---|---|---|
| **1: Foundation + Storage + ETL** | Monorepo init, storage layer, ETL for odds + ledger | ✅ Complete |
| **2: Ingest (federation scrapers) + historical backfill** | `R/ingest/` dispatcher + 3 federation scrapers (KSÍ / KKÍ / HSÍ) with `upsert_table` safe-merge semantics, historical match-data backfill | ✅ Complete — 9,914 rows in `data/facts/results/` |
| **3: Model layer** | `R/model-{prepare,fit,posteriors,league}.R` + 3 Stan models under `Stan/{league_key}/`, `beliefs/{latest,archive}/` snapshot + accretive tables, golden-output sanity gate vs `_legacy/*/results/*/fit.rds` backup | ✅ Complete |
| 4: Decide + Placer + Publish | Kelly, portfolio, bet placement, website JSON | Pending |
| 5: Orchestration + CI + cutover | `{targets}` DAG, CI workflows (scrape + fit + publish), metill-platform pull, cutover, archive `_legacy/` remotes | Pending |

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
│   ├── duckdb-views.R              # rebuild_duckdb() — regenerable SQL view layer
│   ├── ingest.R                    # Source-registry dispatcher: ingest_league()
│   ├── ingest-kki-basketball.R     # Baskethotel XLSX (2021-2026)
│   ├── ingest-hsi-handball.R       # HSÍ (chromote; 2021-2025 male OD historical)
│   ├── ingest-ksi-football.R       # KSÍ (paginated server-rendered; 2021-2025 men's)
│   ├── model-prepare.R             # prepare_data(league, sex) → list(stan_data, pred_d, teams)
│   ├── model-fit.R                 # fit_model() — pure cmdstanr wrapper
│   ├── model-posteriors.R          # extract_posteriors() → beliefs_latest tibble
│   └── model-league.R              # fit_league() — end-to-end orchestrator
├── Stan/
│   ├── basketball_iceland/2d_student_t_scalarsigma.stan
│   ├── handball_iceland/2d_student_t.stan
│   └── football_iceland/bivariate_poisson_no_inflation.stan
├── data/                           # Parquet stores (git-tracked, hive-partitioned)
│   ├── facts/
│   │   ├── odds/                   # Lengjan odds snapshots (1,433 rows)
│   │   ├── results/                # Match history (9,914 rows across 3 sports, up to 6 seasons)
│   │   └── schedules/              # Upcoming fixtures via ingest
│   ├── beliefs/
│   │   ├── latest/sport=X/country=Y/sex=Z/beliefs.parquet               # Snapshot per fit
│   │   └── archive/sport=X/country=Y/sex=Z/fit_date=YYYY-MM-DD/…         # Accretive per fit_date
│   └── decisions/
│       ├── ledger/                 # Placed-bet history (1,870 rows, PnL = 89,369 ISK)
│       ├── candidates/             # (Plan 4 populates)
│       └── recommendations/        # (Plan 4 populates)
├── scripts/
│   ├── etl/                        # One-time legacy-CSV → Parquet migrations
│   │   ├── 03_etl_odds.R
│   │   └── 04_etl_ledger.R
│   ├── backfill_ingest.R           # Single-command re-run of all 3 scrapers
│   └── fit_all.R                   # Fit all active (league × sex) combos → beliefs/
├── tests/testthat/                 # 230+ passing assertions across config, storage*, duckdb-views, etl-validation, ingest-*, model-*
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

### Model layer

- `fit_league(league_key, sex)` is the only public entry point. Loads config, calls `prepare_data()` → `fit_model()` → `extract_posteriors()`, writes `beliefs/latest/` (overwrite) and `beliefs/archive/` (accretive per `fit_date`).
- `prepare_data()` is pure — reads Parquet facts, returns `list(stan_data, pred_d, teams)`. No file I/O beyond `read_table()`.
- `fit_model()` is a pure cmdstanr wrapper — takes stan_data + stan_path, returns the fit. Callers save to disk.
- `extract_posteriors()` materialises posterior draws as the canonical `beliefs_latest` tibble (per-draw-per-match).
- Stan models live in `Stan/{league_key}/{file}.stan`. `leagues.yml`'s `stan_model` field uses this relative path.
- Backfill with `Rscript scripts/fit_all.R`. Run detached (wall-clock several hours with 1000 MCMC iters):
  ```
  nohup Rscript scripts/fit_all.R > /tmp/fit_all.log 2>&1 & disown
  ```

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
