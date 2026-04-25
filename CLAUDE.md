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
| **4: Decide + Publish** | Joint Kelly + portfolio + calibration → recommendations Parquet; football iceland 7-JSON publisher + basketball/handball 2-JSON scaffolds | ✅ Complete |
| **5: Placer (local-only)** | `R/placer-*.R` ports `_legacy/lengjan-bets/`, dual-writes ledger CSV+Parquet during cutover, CI safety gate enforces local-only | ✅ Complete |
| **6: Orchestration + CI + cutover** | `_targets.R` DAG + `run.R` CLI, 4 GitHub workflows (ci-tests, scrape-odds, scrape-results, fit-and-publish), metill-platform `pull-sports-data.yml`, placer dual-write dropped | ✅ Complete |

## Directory structure

```
sports/
├── _targets.R                      # {targets} DAG definition (5 layers: ingest -> fit -> decide -> publish)
├── run.R                           # Thin CLI: --league/--sex/--step over tar_make()
├── config/
│   ├── leagues.yml                 # Single source of truth for league metadata
│   ├── leagues.schema.json         # JSON Schema validator
│   ├── bankroll.yml                # Global Kelly cap + daily budget
│   └── active_competitions.json    # GENERATED at each scrape run (Plan 6)
├── R/                              # Package source
│   ├── config.R                    # load_leagues() + filter_leagues()
│   ├── storage-schemas.R           # Arrow schemas for 8 tables
│   ├── storage.R                   # write_table() + read_table() primitives
│   ├── duckdb-views.R              # rebuild_duckdb() — regenerable SQL view layer
│   ├── ingest.R                    # Source-registry dispatcher: ingest_league()
│   ├── ingest-kki-basketball.R     # Baskethotel XLSX (2021-2026)
│   ├── ingest-hsi-handball.R       # HSÍ (chromote; 2021-2025 male OD historical)
│   ├── ingest-ksi-football.R       # KSÍ (paginated server-rendered; 2021-2025 men's)
│   ├── ingest-lengjan-odds.R       # Lengjan odds scraper (port of _legacy/lengjan-odds/)
│   ├── schedule-active.R           # generate_active_competitions() -> config/active_competitions.json
│   ├── model-prepare.R             # prepare_data(league, sex) → list(stan_data, pred_d, teams)
│   ├── model-fit.R                 # fit_model() — pure cmdstanr wrapper
│   ├── model-posteriors.R          # extract_posteriors() → beliefs_latest tibble
│   ├── model-league.R              # fit_league() — end-to-end orchestrator
│   ├── decide-odds.R               # prepare_odds(league, sex)
│   ├── decide-kelly.R              # kelly_joint(beliefs, bets)
│   ├── decide-portfolio.R          # portfolio_optimise(packages)
│   ├── decide-calibration.R        # compute_calibration(league, sex)
│   ├── decide-pipeline.R           # decide_league() orchestrator
│   ├── publish-football-iceland.R  # 7 JSONs per sex
│   ├── publish-basketball-iceland.R # meta + next_games scaffold
│   ├── publish-handball-iceland.R  # meta + next_games scaffold
│   ├── publish-pipeline.R          # publish_one() dispatcher (sport-aware)
│   ├── placer-validate.R           # validate_team_names_config + validate_recommendations_schema
│   ├── placer-load.R               # load_recommendations + dedup_against_ledger
│   ├── placer-ledger.R             # append_to_ledger (Parquet canonical; CSV dual-write opt-in)
│   ├── placer-login.R              # chromote_login (auth + 2FA)
│   ├── placer-navigate.R           # extract_matches + find_match_in_extracted
│   ├── placer-place.R              # place_bet + P3/P4 state machine (~810 lines)
│   ├── placer-pipeline.R           # place_bets() top-level orchestrator
│   └── placer-preview.R            # preview_pending() — dry-run, no browser
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
│   │   ├── archive/sport=X/country=Y/sex=Z/fit_date=YYYY-MM-DD/…         # Accretive per fit_date
│   │   └── fits/                   # GITIGNORED — Stan fit RDS, regenerable
│   ├── decisions/
│   │   ├── candidates/sport=X/country=Y/run_date=YYYY-MM-DD/  # All stages
│   │   ├── recommendations/sport=X/country=Y/run_date=YYYY-MM-DD/  # Post-filter
│   │   └── ledger/                 # Placed-bet history (1,870 rows, PnL = 89,369 ISK) — canonical
│   └── publish/
│       ├── football/iceland/{karla,kvenna}/*.json
│       ├── basketball/iceland/{karla,kvenna}/*.json
│       └── handball/iceland/{karla,kvenna}/*.json
├── .github/workflows/              # CI (Plan 6) — placer is NEVER referenced here
│   ├── ci-tests.yml                # devtools::test() on push and PR
│   ├── scrape-odds.yml             # Lengjan odds 3x/day cron
│   ├── scrape-results.yml          # Federation results + schedules 1x/day
│   └── fit-and-publish.yml         # Daily Stan fit + decide + publish
├── scripts/
│   ├── etl/                        # One-time legacy-CSV → Parquet migrations
│   │   ├── 03_etl_odds.R
│   │   └── 04_etl_ledger.R
│   ├── backfill_ingest.R           # DEPRECATED — use Rscript run.R --step ingest
│   ├── decide_all.R                # DEPRECATED — use Rscript run.R --step decide
│   ├── fit_all.R                   # DEPRECATED — use Rscript run.R --step fit
│   ├── place_bets.R                # CLI: dry-run by default, --live opts in (local-only)
│   ├── preview_bets.R              # CLI: pending-bet preview, no browser (local-only)
│   └── publish_all.R               # DEPRECATED — use Rscript run.R --step publish
├── tests/testthat/                 # 511+ passing assertions across config, storage*, duckdb-views, etl-validation, ingest-*, model-*, decide-*, publish-*, placer-*
├── sports.duckdb                   # Gitignored; rebuildable via rebuild_duckdb()
├── docs/superpowers/               # Specs + plans
└── _legacy/                        # Subtree-merged histories of the 4 predecessor repos
    ├── sports/                     # ex metill-is/sports (archived post-cutover)
    ├── lengjan-odds/               # ex metill-is/lengjan-odds (archived post-cutover)
    ├── livesport-data/             # ex metill-is/livesport-data (archived post-cutover)
    └── lengjan-bets/               # ex metill-is/lengjan-bets (archived post-cutover)
```

## Local-only subsystem

`R/placer-*.R` (Plan 5) places bets against Lengjan via Chromote browser automation using `LENGJAN_USER` / `LENGJAN_PASS` from `.Renviron` (template at `.Renviron.example`). It is **never** executed on CI — no workflow invokes it and no GitHub Actions secret named `LENGJAN_*` is configured.

**Enforcement:** `tests/testthat/test-placer-ci-isolation.R` greps every `.github/workflows/*.yml` and fails the build if any line references `R/placer-`, `place_bets`, `preview_bets`, `placer_pipeline`, or `LENGJAN_*`. The test skips when there are no workflow files yet (Plan 6 territory).

**Ledger storage:** As of Plan 6 cutover, Parquet at `data/decisions/ledger/` is the canonical ledger store. `append_to_ledger()`'s `dual_write_csv` argument defaults to `FALSE`; set `TRUE` only for opt-in regression-testing against the legacy CSV at `_legacy/sports/{sport}/{country}/history/bets_log.csv`.

**P1–P4 placement rules** (only-writer, actual-odds, kelly-recompute, EV reject) are preserved verbatim from `_legacy/lengjan-bets/`. See `.claude/rules/sports-betting.md` for the full statement.

**Known gap (deferred):** The DOM regex that parses live odds out of Lengjan's odds buttons lives inline in `R/placer-place.R::click_market_button` / `click_table_button`. A pure `parse_actual_odds_from_dom()` seam was specified in Plan 5 Task 6 but not extracted in the port — a Lengjan UI deploy that changes the odds-element structure would silently produce wrong odds rather than a unit-test failure. ~15-line refactor when next touching `placer-place.R`.

## Quick reference

```bash
# Development
Rscript -e 'devtools::load_all()'
Rscript -e 'devtools::test()'

# Run the pipeline (daily driver -- uses {targets})
Rscript run.R --help
Rscript run.R --all --step odds                       # 3 leagues, scrape odds
Rscript run.R --league football_iceland --sex male --step fit
Rscript run.R --all --step fit                        # backfill all
Rscript run.R --all --step decide                     # decide layer
Rscript run.R --all --step publish                    # publish JSONs

# Targets directly (advanced)
Rscript -e 'targets::tar_make()'
Rscript -e 'targets::tar_make(names = c("fit_handball_iceland_male"))'
Rscript -e 'targets::tar_visnetwork()'                # DAG visualisation

# Local placer (NEVER on CI)
Rscript scripts/place_bets.R --dry-run
Rscript scripts/place_bets.R --live
Rscript scripts/preview_bets.R                        # no browser

# Rebuild sports.duckdb after fresh Parquet writes
Rscript -e 'sports::rebuild_duckdb()'

# Query any table via DuckDB
Rscript -e '
con <- DBI::dbConnect(duckdb::duckdb(), "sports.duckdb", read_only = TRUE)
print(DBI::dbGetQuery(con, "SELECT sport, country, COUNT(*) AS n, SUM(pnl) AS pnl FROM ledger WHERE settled GROUP BY 1,2 ORDER BY 1"))
'
```

## Deprecated runners

The `scripts/{fit,decide,publish}_all.R` and `scripts/backfill_ingest.R` scripts
are kept as ad-hoc escape hatches but are **deprecated** in favour of
`Rscript run.R`. They bypass the {targets} DAG and won't pick up cached
results -- use them only for one-shot reruns where freshness matters more
than caching.

## metill-platform integration

The `metill-is/metill-platform` repo runs `pull-sports-data.yml` hourly:
clones `metill-is/sports`, rsyncs `data/publish/` into `data/ithrottir/`,
commits if changed. A push to metill-platform triggers Fly.io auto-deploy.

Sports-side workflow:
1. fit-and-publish.yml writes data/publish/{...}/*.json
2. The push to main triggers metill-platform's pull-sports-data.yml
   within the next hour
3. metill-platform's commit deploys to fly.metill.is

To force a refresh without waiting for cron:

```bash
gh workflow run pull-sports-data.yml --repo metill-is/metill-platform
```

## Legacy archival

`_legacy/{sports,lengjan-odds,livesport-data,lengjan-bets}/` is kept on disk
for `git log --follow` history access. The corresponding GitHub repos are
archived (read-only) post-cutover:

```bash
gh repo archive metill-is/sports          # the OLD sports repo, not this one
gh repo archive metill-is/lengjan-odds
gh repo archive metill-is/livesport-data
gh repo archive metill-is/lengjan-bets
```

This is one-time post-cutover admin; the new monorepo is `metill-is/sports`
(replacing the archived predecessor).

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
- Daily driver: `Rscript run.R --all --step fit` (uses `{targets}` cache).
  `scripts/fit_all.R` is deprecated — keep for one-shot reruns. Run detached
  for long backfills (wall-clock several hours with 1000 MCMC iters):
  ```
  nohup Rscript run.R --all --step fit > /tmp/fit_all.log 2>&1 & disown
  ```

### Decide layer

- `decide_league(league_key, sex)` is the public entry — chains
  prepare_odds + kelly_joint + portfolio_optimise + compute_calibration,
  writes candidates (with stage column) + recommendations Parquet.
- Joint Kelly is the only mode (per 2026-03-06 memory note).
- Per-sex `kelly_frac` supported via object form in `betting:` config
  (football_iceland uses male=0.15, female=0.075).
- Daily driver: `Rscript run.R --all --step decide` (uses `{targets}` cache).
  `scripts/decide_all.R` is deprecated — keep for one-shot reruns. Wall-clock ~seconds.

### Publish layer

- `publish_<sport>_iceland(fit, league, sex)` produces JSON snapshots in
  `data/publish/<sport>/iceland/{karla,kvenna}/`. Football has full
  7-JSON port (meta, next_games, standings, team_strengths, final_positions,
  points_distribution, home_advantage); basketball + handball are
  scaffolds (meta + next_games only).
- Daily driver: `Rscript run.R --all --step publish` (the `{targets}` DAG wires
  fresh-fit-on-demand via `R/publish-pipeline.R::publish_one()`).
  `scripts/publish_all.R` is deprecated — keep for one-shot reruns; it still
  reads the legacy backup fit at `SPORTS_BACKUP_ROOT` when available.
- The `SPORTS_BACKUP_ROOT` env var overrides the default backup path.

### Betting conventions

`.claude/rules/sports-betting.md` — P1–P4 placement rules, bets.yml schema (Plan 3 adds).

### Placer (local-only)

- `Rscript scripts/place_bets.R` is the public entrypoint. Default is dry-run; `--live` actually places. Always reads `LENGJAN_USER` / `LENGJAN_PASS` from `.Renviron`.
- `Rscript scripts/preview_bets.R` shows pending bets without opening a browser.
- `R/placer-*.R` is **never** wired into CI; the `test-placer-ci-isolation.R` test enforces this.
- The placer is the only writer to `data/decisions/ledger/` (Parquet, canonical). Plan 6 cutover dropped the CSV dual-write default; opt-in via `dual_write_csv = TRUE` if a legacy CSV regression check is needed.
- P1: idempotent (dedup against ledger). P2: ledger records actual Lengjan odds. P3: Kelly stake recomputed if odds drift. P4: bets no longer +EV are rejected.

## Skills

The four model-invocable skills under `.claude/skills/` (`/bet`, `/sports-update`, `/add-league`, `/place-bets`) remain from the pre-migration workspace. They were authored against the legacy four-repo layout and will be revised to use `Rscript run.R --step ...` in a follow-up pass; for now they continue to work via the deprecated `scripts/*_all.R` runners.

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
