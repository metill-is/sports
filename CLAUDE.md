# Sports — CLAUDE.md

Bayesian sports prediction and automated betting for Icelandic football, basketball, and handball. **Consolidated monorepo** (pre-migration was four separate repos: `Sports/`, `lengjan-odds/`, `livesport-data/`, `lengjan-bets/` — all preserved under `_legacy/`).

> **Current focus (2026-04-24):** 3 active Icelandic leagues only (`basketball_iceland`, `handball_iceland`, `football_iceland`). Non-Icelandic leagues paused and will be reactivated incrementally from later plans. All user-facing content is in Icelandic.

## Status

Migration complete (Plan 7, 2026-04-30). End-state design: [`docs/superpowers/specs/2026-04-24-sports-pipeline-redesign-design.md`](docs/superpowers/specs/2026-04-24-sports-pipeline-redesign-design.md). Implementation plans: [`docs/superpowers/plans/`](docs/superpowers/plans/).

| Plan | Scope | Status |
|---|---|---|
| **1: Foundation + Storage + ETL** | Monorepo init, storage layer, ETL for odds + ledger | ✅ Complete |
| **2: Ingest (federation scrapers) + historical backfill** | `R/ingest.R` + `R/ingest-*.R` (flat layout) — 3 federation scrapers (KSÍ / KKÍ / HSÍ) with `upsert_table` safe-merge semantics, historical match-data backfill | ✅ Complete — 9,914+ rows in `data/facts/results/` |
| **3: Model layer** | `R/model-{prepare,fit,posteriors,league}.R` + 3 Stan models under `Stan/{league_key}/`, `beliefs/{latest,archive}/` snapshot + accretive tables, golden-output sanity gate vs `_legacy/*/results/*/fit.rds` backup | ✅ Complete |
| **4: Decide + Publish** | Joint Kelly + portfolio + calibration → recommendations Parquet; football iceland 10/11-JSON publisher (per cell in `publish_divisions`: male BD/LD1/LD2/LD3/CUP, female BD/LD1/LD2/CUP) + basketball/handball 8-JSON publishers (seasonally paused) | ✅ Complete |
| **5: Placer (local-only)** | `R/placer-*.R` ports `_legacy/lengjan-bets/`, dual-writes ledger CSV+Parquet during cutover, CI safety gate enforces local-only | ✅ Complete |
| **6: Orchestration + CI + cutover** | `_targets.R` DAG + `run.R` CLI (later replaced by Plan 7), metill-platform `pull-sports-data.yml`, placer dual-write dropped | ✅ Complete (superseded by Plan 7 for CI topology) |
| **7: Drop {targets}** | `_targets.R` + `run.R` replaced by 6 entry scripts (`scripts/0N_*.R`) with explicit freshness predicates; CI split into 6 workflows — 5 auto-running (ci-tests, scrape-results, scrape-odds, fit, decide-publish via `workflow_run`) + 1 manual-dispatch (republish); cross-workflow git race resolved | ✅ Complete |

## Directory structure

```
sports/
├── scripts/0N_*.R       # Pipeline entry points (00–06); see Quick reference
├── config/              # leagues.yml + bankroll.yml + JSON Schema validators
├── R/                   # R package source — see .claude/rules/ for per-area details
│   ├── ingest-*.R       # Federation + Lengjan scrapers (KSÍ/KKÍ/HSÍ)
│   ├── model-*.R        # → .claude/rules/model-decide.md
│   ├── decide-*.R       # → .claude/rules/model-decide.md
│   ├── publish-*.R      # → .claude/rules/publish-layer.md
│   ├── extract-*.R      # Football per-fit extracts → publish-layer.md
│   ├── simulate-cup-bracket.R  # Mjólkurbikar forward simulator
│   ├── placer-*.R       # Local-only Lengjan placement → sports-betting.md
│   ├── settle.R         # Settle layer (win/pnl) → sports-betting.md
│   ├── commit-ledger.R  # Auto-commit ledger after placer/settle → git-hygiene.md
│   ├── pipeline-freshness.R  # needs_refit / has_upcoming_games predicates
│   ├── storage*.R       # Arrow schemas + write_table/read_table primitives
│   ├── config.R         # leagues.yml + bankroll.yml loaders
│   └── duckdb-views.R   # rebuild_duckdb() — SQL views over Parquet
├── Stan/{league_key}/   # Per-league Stan models
├── data/                # Parquet stores (hive-partitioned, git-tracked)
│   ├── facts/           # results, odds, schedules
│   ├── beliefs/         # latest, archive, extracts, diagnostics (+ gitignored fits/)
│   ├── decisions/       # candidates, recommendations, ledger (canonical Parquet)
│   ├── health/          # status.json — pipeline_health() snapshot (read-only)
│   └── publish/         # *.json per (sport, country, sex[, division])
├── tests/testthat/      # 1120+ assertions; devtools::test() to run
├── .github/workflows/   # cron + workflow_run + republish/world-cup manual-dispatch → .claude/rules/ci-conventions.md
├── docs/                # superpowers/ (specs + plans), audits/, runbooks/
└── _legacy/             # 4 archived predecessor repos for git log --follow
```

File-level annotations (e.g. which scraper covers which federation, which Stan
model is used per league) live in `R/ingest.R::ingest_league()` (the source
registry) and `config/leagues.yml::*.stan_model`. Read those for the
authoritative mapping rather than mirroring them here.

## Local-only subsystem

`R/placer-*.R` (Plan 5) places bets against Lengjan via Chromote browser automation using `LENGJAN_USER` / `LENGJAN_PASS` from `.Renviron` (template at `.Renviron.example`). It is **never** executed on CI — no workflow invokes it and no GitHub Actions secret named `LENGJAN_*` is configured.

**Enforcement:** `tests/testthat/test-placer-ci-isolation.R` greps every `.github/workflows/*.yml` and fails the build if any line references `R/placer-`, `place_bets`, `preview_bets`, `placer_pipeline`, or `LENGJAN_*`.

**Ledger storage:** As of Plan 6 cutover, Parquet at `data/decisions/ledger/` is the canonical ledger store. `append_to_ledger()`'s `dual_write_csv` argument defaults to `FALSE`; set `TRUE` only for opt-in regression-testing against the legacy CSV at `_legacy/sports/{sport}/{country}/history/bets_log.csv`.

**P1–P4 placement rules** (only-writer, actual-odds, kelly-recompute, EV reject) are preserved verbatim from `_legacy/lengjan-bets/`. See `.claude/rules/sports-betting.md` for the full statement.

**DOM odds parser:** A pure `parse_actual_odds_from_dom(html)` (exported from `R/placer-place.R`) mirrors the JS regex chain inline in `click_market_button` / `click_table_button`. Both click helpers re-parse the chosen button's `outerHTML` and verify the JS-reported odds against the R-side parse — a Lengjan UI deploy that changes the odds-element structure now surfaces as either a `parse_actual_odds_from_dom: could not parse` error or an `Odds parser disagreement` error, rather than silent wrong odds. The parser is fixture-tested in `tests/testthat/test-placer-place.R`.

- **Unattended placement (opt-in):** `scripts/auto_place.R` via the launchd
  agent `is.metill.sports.autoplace` (installed by `tools/install-autoplace.sh`).
  Kill switch: `touch data/AUTO_PLACE_DISABLED`. Health: the `placement_health`
  check in `/pipeline-doctor`. Design + plan under `docs/superpowers/`.
  Like the other ledger-writing wrappers it calls `commit_ledger_changes()`
  after each run, and `sync_recs()` rescue-commits any ledger rows a crashed
  run left uncommitted before its stash → pull → pop sync (2026-06-10
  incident). Run log: `~/Library/Logs/sports-autoplace.log`.
  **Background-git warning:** this launchd job runs `git` (stash → pull --rebase
  → pop) on `~/sports` on its own schedule. `sync_recs()` only rescue-commits the
  *ledger* — any *other* uncommitted git-tracked generated data (e.g.
  `data/publish/**/final_positions_history.json`, a backfill in progress) can be
  clobbered by that background sync or a concurrent branch op. Commit generated
  tracked data promptly during interactive sessions (2026-06-11 backfill-clobber
  incident).

## Quick reference

```bash
# Development
Rscript -e 'devtools::load_all()'
Rscript -e 'devtools::test()'

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

# Historical replay (re-fit + re-publish for any past date; football iceland only)
Rscript scripts/0Nr_replay.R --league football_iceland --sex male --as-of 2026-05-15
Rscript scripts/0Nr_replay.R --league football_iceland --sex male --season 2026 --per-round
Rscript scripts/0Nr_replay.R --league football_iceland --sex male --as-of 2026-05-15 --no-fit
Rscript scripts/0Nr_replay.R --league football_iceland --sex male --as-of 2026-05-15 \
        --publish-to data/publish_replay/2026-05-15/   # safe what-if (isolated tree)

# Local placer (NEVER on CI). Default is dry-run; --live opts in to placement.
Rscript scripts/place_bets.R                          # dry-run (default)
Rscript scripts/place_bets.R --live                   # actually place
Rscript scripts/preview_bets.R                        # no browser

# Rebuild sports.duckdb after fresh Parquet writes
Rscript -e 'sports::rebuild_duckdb()'

# Query any table via DuckDB
Rscript -e '
con <- DBI::dbConnect(duckdb::duckdb(), "sports.duckdb", read_only = TRUE)
print(DBI::dbGetQuery(con, "SELECT sport, country, COUNT(*) AS n, SUM(pnl) AS pnl FROM ledger WHERE settled GROUP BY 1,2 ORDER BY 1"))
'
```

## metill-platform integration

The `metill-is/metill-platform` repo runs `pull-sports-data.yml` hourly:
clones `metill-is/sports`, rsyncs `data/publish/` into `data/ithrottir/`,
commits if changed. A push to metill-platform triggers Fly.io auto-deploy.

Sports-side workflow:
1. decide-publish.yml writes data/publish/{...}/*.json
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

[`.claude/rules/publish-layer.md`](.claude/rules/publish-layer.md) — football extracts tree (since 2026-05-05), basketball/handball legacy fit-RDS path, schema features (xg_for/xpts, 9-cell team-strengths grid, preseason baseline), metill-platform consumption. Loads when working on `R/{publish,extract}-*.R`, `data/{publish,beliefs/extracts}/**`, or `scripts/05_publish.R`.

### Betting conventions

[`.claude/rules/sports-betting.md`](.claude/rules/sports-betting.md) — P1–P4 placement rules, K1–K6 Kelly invariants, L1–L4 ledger immutability. Loads when working on `R/{decide,placer}-*.R`, `config/{leagues,bankroll}.yml`, `scripts/{place,preview}_bets.R`.

### CI / GitHub Actions

[`.claude/rules/ci-conventions.md`](.claude/rules/ci-conventions.md) — `PKG_SYSREQS: "false"` workaround (chromote / Launchpad PPA fragility), V8 from-source rebuild (libnode ABI mismatch), `workflow_run` glob trap (workflow `name:` fields must be glob-safe), workflow inventory. Loads when editing `.github/workflows/**`.

### Placer (local-only)

- `Rscript scripts/place_bets.R` is the public entrypoint. Default is dry-run; `--live` actually places. Always reads `LENGJAN_USER` / `LENGJAN_PASS` from `.Renviron`.
- `Rscript scripts/preview_bets.R` shows pending bets without opening a browser.
- `R/placer-*.R` is **never** wired into CI; the `test-placer-ci-isolation.R` test enforces this.
- The placer is the only writer to `data/decisions/ledger/` (Parquet, canonical). Plan 6 cutover dropped the CSV dual-write default; opt-in via `dual_write_csv = TRUE` if a legacy CSV regression check is needed.
- P1: idempotent (dedup against ledger). P2: ledger records actual Lengjan odds. P3: Kelly stake recomputed if odds drift. P4: bets no longer +EV are rejected.

### Settle layer

`R/settle.R` exports `compute_settlement(bets, results, match_date_window_days)` and `settle_ledger(root, match_date_window_days)`. Joins ledger rows where `settled = FALSE` against `data/facts/results/` on `(sport, country, sex, match_date, home_team, away_team)`, computes `win` + `pnl` per market with strict-inequality boundaries (matching `decide-kelly.R::build_return_matrix` so calibration stays self-consistent with the EV used at placement). Already-settled rows are immutable (L4). Daily driver: `Rscript scripts/06_settle.R`. Run before `04_decide.R` so `current_pool = initial_pool + Σ(settled.pnl)` reflects realised PnL.

**Reschedule fallback (default `match_date_window_days = 3`)** — bets the strict join leaves unsettled get a second-pass lookup against results within the window, keyed on `(sport, country, sex, home_team, away_team)`. The fallback only fires when the window contains exactly one candidate result for the bet; ambiguous pairings (e.g. cup tie + league leg within three days) stay unsettled. This handles ledger orphans created when Lengjan reschedules a fixture after placement — the placer freezes `match_date` at the original kick-off (L3/L4 spirit) and the federation results scraper writes the played match at the new date. The ledger row's `match_date` is never mutated; only `settled` / `win` / `pnl` flip, preserving L4.

**Local-only by design** — both placer and settle write to `data/decisions/ledger/`, and `arrow::write_parquet` is read-then-write (not atomic), so adding a CI-host writer would race concurrent local placer runs and risk Parquet corruption. Promoting to a workflow would require atomic upsert semantics or a coordination mechanism.

### Health & monitoring (2026-05-30)

`R/health.R::pipeline_health(root, now)` is a **read-only** snapshot composing fit/odds freshness, persisted Stan-diagnostic drift (`data/beliefs/diagnostics/`, written per fit by `fit_league`), orphaned-bet, **placement-capture-rate** (recommendations for now-played matches that never reached the ledger — the forensic review found ~45% capture), and bankroll/drawdown checks into a `{check, scope, status, value, threshold}` tibble (`OK` < `WARN` < `FAIL`, plus `PAUSED` for off-season cells via `has_upcoming_games`). Thresholds are documented named constants in the function. `scripts/07_healthcheck.R` writes `data/health/status.json` + prints a summary; `healthcheck.yml` runs it twice daily and fails the run on `overall == FAIL` so GitHub's failure email fires (the alert channel — enable "Actions: failure" notifications in GitHub). The `/pipeline-doctor` skill + a SessionStart banner surface it interactively. All read-only on the ledger (CI-safe against the local placer); `tests/testthat/test-healthcheck-ci-isolation.R` enforces it. Triage playbooks: `docs/runbooks/`.

Two write-boundary guards complement the snapshot: `validate_values()` (`R/storage-validate.R`, wired into `write_table`) rejects impossible scores / `odds <= 1` / out-of-range `p`; `validate_bet_inputs()` (`R/decide-kelly.R`) quarantines a non-finite `p` or `odds <= 1` into a loud `dropped_invalid_input` candidate stage before stake sizing. The Stan gate (`check_stan_diagnostics`) also now covers treedepth / E-BFMI / tail-ESS and returns its metrics for persistence.

### Backtest harness (2026-05-30)

`R/backtest-*.R` + `scripts/0Nb_backtest.R` + `docs/reports/2026-backtest.qmd`
replay historical decisions against results to analyse strategy performance
(PnL/ROI/calibration, by market/sex). Read-only, never on CI; reuses
`compute_settlement()`. **Defaults to football only** (CLI + report); the engine
stays general — widen with `--league all` when basketball/handball resume. See
`.claude/rules/backtest.md`.

## Git hygiene

Five CI workflows commit to `main` throughout the day, so local working trees drift quickly. The cron-collision sync pattern (stash → pull --rebase → pop), stash discipline, the `git -C <abs-path>` rule for the Bash tool's persistent cwd, and the PR-vs-direct-push decision tree are documented in [`.claude/rules/git-hygiene.md`](./.claude/rules/git-hygiene.md). Operational helpers: `/sync-main` (mid-session re-alignment) and `/wrap-up-session` (end-of-session consolidation checklist).

## Skills

The pipeline skills under `.claude/skills/` (`/bet`, `/sports-update`, `/add-league`, `/place-bets`) call `scripts/0N_*.R` directly. The git-hygiene skills (`/sync-main`, `/wrap-up-session`) handle cron-collision sync and end-of-session consolidation. Drift back to legacy invocations is guarded by `tests/testthat/test-skill-conventions.R`, which fails the build if any skill references `lengjan-bets/`, `lengjan-odds/`, `Sports/{sport}/{country}/`, the `--sync` flag, or the legacy `Rscript run.R --step` pattern.

**Do not add `disable-model-invocation: true` to the four pipeline skills.** They are intentionally model-invocable.

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
