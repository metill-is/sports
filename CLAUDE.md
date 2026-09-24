# Sports — CLAUDE.md

Bayesian sports prediction and automated betting for Icelandic football, basketball, and handball. **Consolidated monorepo** (pre-migration was four separate repos: `Sports/`, `lengjan-odds/`, `livesport-data/`, `lengjan-bets/` — all preserved under `_legacy/`).

> **Scope:** the three Icelandic leagues (`basketball_iceland`, `handball_iceland`,
> `football_iceland`) plus the World Cup pipeline (`world-cup.yml`, `R/wc-*.R`),
> dormant since the 2026 tournament (dispatch-only since 2026-09-02).
> Other non-Icelandic leagues are paused. All user-facing content is in Icelandic.
> Authoritative list: `config/leagues.yml` + `scripts/00_active_competitions.R`.

## Source registry and design

File-level annotations (e.g. which scraper covers which federation, which Stan
model is used per league) live in `R/ingest.R::ingest_league()` (the source
registry) and `config/leagues.yml::*.stan_model`. Read those for the
authoritative mapping rather than mirroring them here. End-state design:
[`docs/superpowers/specs/2026-04-24-sports-pipeline-redesign-design.md`](docs/superpowers/specs/2026-04-24-sports-pipeline-redesign-design.md);
implementation plans: [`docs/superpowers/plans/`](docs/superpowers/plans/).

## Local-only subsystem

`R/placer-*.R` (Plan 5) places bets against Lengjan via Chromote browser automation using `LENGJAN_USER` / `LENGJAN_PASS` from `.Renviron` (template at `.Renviron.example`). It is **never** executed on CI — no workflow invokes it and no GitHub Actions secret named `LENGJAN_*` is configured.

**Enforcement:** `tests/testthat/test-placer-ci-isolation.R` greps every `.github/workflows/*.yml` and fails the build if any line references `R/placer-`, `place_bets`, `preview_bets`, `placer_pipeline`, or `LENGJAN_*`.

**P1–P4 placement rules** (only-writer, actual-odds, kelly-recompute, EV reject) are preserved verbatim from `_legacy/lengjan-bets/`. See `.claude/rules/sports-betting.md` for the full statement, the ledger storage note and the DOM odds parser.

- **Unattended placement (opt-in):** `scripts/auto_place.R` via the launchd
  agent `is.metill.sports.autoplace` (installed by `tools/install-autoplace.sh`).
  Kill switch: `touch data/AUTO_PLACE_DISABLED`. Health: the `placement_health`
  check in `/pipeline-doctor`. Run log: `~/Library/Logs/sports-autoplace.log`.
  **Background-git warning:** this job syncs `~/sports` (stash → pull --rebase → pop) on its own schedule and rescue-commits only the ledger, so commit other tracked generated data promptly (2026-06-11 backfill-clobber incident). The ledger's commit layers are in `.claude/rules/git-hygiene.md`.

## Quick reference

```bash
# Development
Rscript -e 'devtools::load_all()'
# UTF-8 is load-bearing: the shell exports LC_CTYPE=C, and under US-ASCII the
# HSI fixture parse silently drops its Icelandic rows -- 3 test-ingest-hsi.R
# assertions then fail (132 vs 108) on an UNCHANGED fixture. CI runners are
# UTF-8, so a C-locale failure never reproduces there.
LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 Rscript -e 'devtools::test()'

# Run the pipeline (daily driver -- one entry script per layer)
Rscript scripts/00_active_competitions.R              # write active_competitions.json
Rscript scripts/01_ingest_results.R                   # all active leagues
Rscript scripts/01_ingest_results.R --league football_iceland
Rscript scripts/02_scrape_odds.R                      # skips when no upcoming games
Rscript scripts/03_fit.R                              # skips when results haven't moved or no upcoming games
Rscript scripts/03_fit.R --league football_iceland --sex male --force
Rscript scripts/04_decide.R
Rscript scripts/05_publish.R
Rscript scripts/06_settle.R                           # resolve win/pnl for settled bets
Rscript scripts/07_healthcheck.R                      # read-only health snapshot -> data/health/status.json (or /pipeline-doctor)
Rscript scripts/0N_discover.R                         # discover Lengjan leagues we model but don't yet scrape

# Historical replay (re-fit + re-publish for any past date; football iceland only)
Rscript scripts/0Nr_replay.R --league football_iceland --sex male --as-of 2026-05-15
Rscript scripts/0Nr_replay.R --league football_iceland --sex male --season 2026 --per-round
Rscript scripts/0Nr_replay.R --league football_iceland --sex male --as-of 2026-05-15 --no-fit
Rscript scripts/0Nr_replay.R --league football_iceland --sex male --as-of 2026-05-15 \
        --publish-to data/publish_replay/2026-05-15/   # safe what-if (isolated tree)

# Local placer (NEVER on CI). Default is dry-run; --live opts in to placement.
Rscript scripts/place_bets.R                          # dry-run (default)
Rscript scripts/place_bets.R --live --no-confirm      # actually place: only after the slip is confirmed in chat (--live alone stops at a y/n prompt Rscript can't answer)
Rscript scripts/preview_bets.R                        # no browser

# Rebuild sports.duckdb after fresh Parquet writes
Rscript -e 'sports::rebuild_duckdb()'

# Query any table via DuckDB
Rscript -e '
con <- DBI::dbConnect(duckdb::duckdb(), "sports.duckdb", read_only = TRUE)
print(DBI::dbGetQuery(con, "SELECT sport, country, COUNT(*) AS n, SUM(pnl) AS pnl FROM ledger WHERE settled GROUP BY 1,2 ORDER BY 1"))
'
```

### World Cup

Tournament-time manual overlay refresh (martj42 score lag), knockout
conditioning and ingest date-correction: **`/wc-refresh` skill**.

## metill-platform integration

`metill-is/metill-platform` polls this repo 7×/day (`pull-sports-data.yml`) and deploys fly.metill.is when the publish JSONs change semantically, so propagation takes up to a few hours; cadence and the force-refresh command are in [`docs/runbooks/metill-platform-desync.md`](docs/runbooks/metill-platform-desync.md).

## Conventions

### R package structure

- `DESCRIPTION` + `NAMESPACE` treat the monorepo as an R package — `devtools::load_all()` / `devtools::test()` are the daily drivers.
- Exports: public entry points carry `#' @export` (NAMESPACE is generated by `devtools::document()`); internal helpers are `#' @noRd`.
- `testthat` edition 3, tests in `tests/testthat/`.
- See `~/.claude/rules/r-package-conventions.md` (user-global) and `.claude/rules/r-conventions.md` (project-local).

### Column-naming convention (spec §3.3)

Internal schemas use English throughout. Canonical column names: `home_team` / `away_team` / `match_date` / `fit_date` / `scraped_at` / `placed_at` / `p` (probability) / `odds` / `odds_placed` / `kelly` / `bet_amount` / `line`. Icelandic only appears at the publish boundary (Plan 3).

### Data formats

- **Parquet** for every store in `data/` (hive-partitioned, schema-validated by `write_table()`).
- **DuckDB** (`sports.duckdb`, gitignored) gives SQL query access via views over the Parquet paths.
- **CSV** only for ingest from legacy sources during ETL.

### Stan models

[`.claude/rules/stan-conventions.md`](.claude/rules/stan-conventions.md) — `cmdstanr`, non-centred parameterisations, `generated quantities` for posterior-predictive checks.

### Model + Decide layers

[`.claude/rules/model-decide.md`](.claude/rules/model-decide.md) — `fit_league()` + `decide_league()` public entries, stake formula (Browne shrinkage × calibration × Kelly ceiling × current pool), freshness predicates, joint Kelly. Loads when working on `R/{model,decide}-*.R`, `Stan/**`, `config/{leagues,bankroll}.yml`, or `scripts/03*_fit.R` / `scripts/04_decide.R`.

### Publish layer

[`.claude/rules/publish-layer.md`](.claude/rules/publish-layer.md) — football extracts tree (since 2026-05-05), basketball/handball extracts tree (since 2026-09-04), schema features (xg_for/xpts, 9-cell team-strengths grid, preseason baseline), metill-platform consumption. Loads when working on `R/{publish,extract}-*.R`, `scripts/05_publish.R` or the publish schemas; dated design notes are in [`docs/publish-layer-design.md`](docs/publish-layer-design.md).

### Betting conventions

[`.claude/rules/sports-betting.md`](.claude/rules/sports-betting.md) — P1–P4 placement rules, K1–K6 Kelly invariants, L1–L4 ledger immutability. Loads when working on `R/{decide,placer}-*.R`, `config/{leagues,bankroll}.yml`, `scripts/{place,preview}_bets.R`.

### CI / GitHub Actions

[`.claude/rules/ci-conventions.md`](.claude/rules/ci-conventions.md) — `PKG_SYSREQS: "false"` workaround (chromote / Launchpad PPA fragility), V8 from-source rebuild (libnode ABI mismatch), `workflow_run` glob trap (workflow `name:` fields must be glob-safe), workflow inventory. Loads when editing `.github/workflows/**`.

### Settle + health layers

[`.claude/rules/settle-health.md`](.claude/rules/settle-health.md) — settle join keys,
reschedule fallback, `pipeline_health()` composition, write-boundary guards.
Loads on `R/{settle,health,storage-validate,decide-kelly}.R`,
`scripts/0{6,7}_*.R`, `.github/workflows/healthcheck.yml`.
Run `06_settle.R` **before** `04_decide.R` so `current_pool` reflects realised PnL.

### Backtest harness

[`.claude/rules/backtest.md`](.claude/rules/backtest.md) — replays historical
decisions against results (PnL/ROI/calibration). Read-only, never on CI.
Football-only by default. Loads on `R/backtest-*.R`, `scripts/0N{b,r}_*.R`,
`docs/reports/2026-backtest.qmd`.

## Git hygiene

Six CI workflows commit to `main` automatically throughout the day (four on cron, plus `fit` and `decide-publish` chained by `workflow_run`; `republish` and `world-cup` are dispatch-only), so local working trees drift quickly. The cron-collision sync pattern (stash → pull --rebase → pop), stash discipline and the ledger's always-commit layers are in [`.claude/rules/git-hygiene.md`](./.claude/rules/git-hygiene.md); branch protection and PR vs direct push are in [`docs/runbooks/git-main-branch.md`](docs/runbooks/git-main-branch.md). Operational helpers: `/sync-main` (mid-session re-alignment) and `/wrap-up-session` (end-of-session consolidation checklist).

## Skills

The pipeline skills under `.claude/skills/` (`/bet`, `/sports-update`, `/add-league`, `/wire-league`, `/place-bets`) call `scripts/0N_*.R` directly. The git-hygiene skills (`/sync-main`, `/wrap-up-session`) handle cron-collision sync and end-of-session consolidation. `/pipeline-doctor` runs the read-only health snapshot, and `/wc-refresh` (the World Cup overlay refresh) is user-invocable only (`disable-model-invocation: true`). Drift back to legacy invocations is guarded by `tests/testthat/test-skill-conventions.R`, which fails the build if any skill references `lengjan-bets/`, `lengjan-odds/`, `Sports/{sport}/{country}/`, the `--sync` flag, or the legacy `Rscript run.R --step` pattern.

**Do not add `disable-model-invocation: true` to the five pipeline skills.** They are intentionally model-invocable.

## Obsidian Output

Vault: `Metill` (MCP) / `~/Obsidian/Metill/` (direct path). Prefer MCP `write_note`.
Handoff: `Sports/Sports Handoff.md`.

### Relevant Knowledge topics

| Topic folder                              | Content                                            |
| ----------------------------------------- | -------------------------------------------------- |
| `Sports/Knowledge/Betting Optimisation/`  | Kelly criterion, calibration, placement rules, PnL |
| `Sports/Knowledge/Sports Models/`         | Bayesian model theory, Stan implementation, goals  |
| `Sports/Knowledge/Lengjan Pipeline/`      | Odds scraping, schedule-aware filtering            |
| `Sports/Knowledge/Livesport Data/`        | Match data scraping, CI pipeline                   |
| `Sports/Knowledge/Publish Pipeline/`      | Extraction layer + JSON data contract with metill-platform |

Each topic has a `_MOC.md` entry point — read it first, then selectively load sub-documents.

## Things 3

Route actionable tasks to the **Metill.is** area (ID: `4WyyavEFjCPunRi9iD5tKe`), project **Sports**.
