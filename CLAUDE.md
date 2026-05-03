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
| **7: Drop {targets}** | `_targets.R` + `run.R` replaced by 5 entry scripts (`scripts/0N_*.R`) with explicit freshness predicates; CI split into 5 workflows (results, odds, fit, decide-publish chained via `workflow_run`); cross-workflow git race resolved | ✅ Complete |

## Directory structure

```
sports/
├── scripts/                         # Pipeline entry points (one per layer)
│   ├── _lib.R                       # Shared CLI parser + target resolver
│   ├── 00_active_competitions.R
│   ├── 01_ingest_results.R
│   ├── 02_scrape_odds.R             # Skips when no upcoming games (rule)
│   ├── 03_fit.R                     # Skips when results haven't moved (rule)
│   ├── 04_decide.R
│   └── 05_publish.R
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
│   ├── model-league.R              # fit_one() — end-to-end orchestrator
│   ├── pipeline-freshness.R        # needs_refit() + has_upcoming_games() (Plan 7)
│   ├── decide-odds.R               # prepare_odds(league, sex)
│   ├── decide-kelly.R              # kelly_joint(beliefs, bets)
│   ├── decide-portfolio.R          # portfolio_optimise(packages)
│   ├── decide-calibration.R        # compute_calibration(league, sex)
│   ├── decide-pipeline.R           # decide_league() orchestrator
│   ├── publish-football-iceland.R  # 11 JSONs per sex (7 snapshot + 4 history)
│   ├── publish-basketball-iceland.R # 8 JSONs per sex (7 snapshot + final_positions_history; not surfaced on platform until autumn 2026)
│   ├── publish-handball-iceland.R  # 8 JSONs per sex (7 snapshot + final_positions_history; final_positions/points_distribution zero-row at end-of-season — resolves autumn 2026)
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
├── .github/workflows/              # CI (Plan 7) — placer is NEVER referenced here
│   ├── ci-tests.yml                # devtools::test() on push and PR
│   ├── scrape-results.yml          # Federation results + schedules 1x/day
│   ├── scrape-odds.yml             # Lengjan odds 3x/day cron
│   ├── fit.yml                     # Stan fit, chained on workflow_run from scrape-results
│   └── decide-publish.yml          # Decide + publish, chained from fit AND scrape-odds
│                                   # (so JSONs refresh on every odds scrape, not just daily)
├── scripts/etl/                    # One-time legacy-CSV → Parquet migrations
│   ├── 03_etl_odds.R
│   └── 04_etl_ledger.R
├── scripts/place_bets.R            # CLI: dry-run by default, --live opts in (local-only)
├── scripts/preview_bets.R          # CLI: pending-bet preview, no browser (local-only)
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

**DOM odds parser:** A pure `parse_actual_odds_from_dom(html)` (exported from `R/placer-place.R`) mirrors the JS regex chain inline in `click_market_button` / `click_table_button`. Both click helpers re-parse the chosen button's `outerHTML` and verify the JS-reported odds against the R-side parse — a Lengjan UI deploy that changes the odds-element structure now surfaces as either a `parse_actual_odds_from_dom: could not parse` error or an `Odds parser disagreement` error, rather than silent wrong odds. The parser is fixture-tested in `tests/testthat/test-placer-place.R`.

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
Rscript scripts/03_fit.R                              # skips when results haven't moved
Rscript scripts/03_fit.R --league football_iceland --sex male --force
Rscript scripts/04_decide.R
Rscript scripts/05_publish.R

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

`.claude/rules/stan-conventions.md` — `cmdstanr`, non-centred parameterisations, `generated quantities` for posterior-predictive checks.

### Model layer

- `fit_league(league_key, sex)` is the only public entry point. Loads config, calls `prepare_data()` → `fit_model()` → `extract_posteriors()`, writes `beliefs/latest/` (overwrite) and `beliefs/archive/` (accretive per `fit_date`).
- `prepare_data()` is pure — reads Parquet facts, returns `list(stan_data, pred_d, teams)`. No file I/O beyond `read_table()`.
- `fit_model()` is a pure cmdstanr wrapper — takes stan_data + stan_path, returns the fit. Callers save to disk.
- `extract_posteriors()` materialises posterior draws as the canonical `beliefs_latest` tibble (per-draw-per-match).
- Stan models live in `Stan/{league_key}/{file}.stan`. `leagues.yml`'s `stan_model` field uses this relative path.
- Daily driver: `Rscript scripts/03_fit.R`. The `needs_refit()` predicate
  in `R/pipeline-freshness.R` short-circuits (league × sex) pairs whose
  last `fit_date` is at least the latest completed `match_date`. Use
  `--force` to refit unconditionally. Run detached for long backfills
  (wall-clock several hours with 1000 MCMC iters):
  ```
  nohup Rscript scripts/03_fit.R --force > /tmp/fit.log 2>&1 & disown
  ```

### Decide layer

- `decide_league(league_key, sex)` is the public entry — chains
  prepare_odds + kelly_joint + portfolio_optimise + compute_calibration,
  writes candidates (with stage column) + recommendations Parquet.
- Joint Kelly is the only mode (per 2026-03-06 memory note).
- **Stake formula (post Plan 7a, 2026-04-29):**
  `bet_amount = round(kelly_raw × portfolio_lambda × min(kelly_frac × calibration, kelly_ceiling) × current_pool)`.
  - `kelly_raw` is the unconstrained joint Kelly fraction from
    `kelly_joint()`; bounded above by `max_match_stake` (per-league override
    or `bankroll$max_match_stake_default`, default 1.0 = unconstrained).
  - `kelly_frac` is the §7.2 multiplicative shrinkage (Browne γ); per-league
    in `leagues.yml::*.betting.kelly_frac` (scalar or `{male, female}`).
  - `calibration` is the Beta-Binomial multiplier from
    `compute_calibration()`, clamped to `[0.5, 1.5]`.
  - `kelly_ceiling` is the K5 hard cap (default 0.25 in `bankroll.yml`).
- Per-sex `kelly_frac` supported via object form in `betting:` config
  (basketball/football use per-sex; handball uses scalar). Browne-grounded
  defaults: male 0.20, female 0.10–0.15.
- Daily driver: `Rscript scripts/04_decide.R`. Wall-clock ~seconds.
  Runs on every odds scrape via `decide-publish.yml`'s `workflow_run`
  trigger so recommendations stay fresh as odds drift.

### Publish layer

- `publish_<sport>_iceland(fit, league, sex)` produces JSON snapshots in
  `data/publish/<sport>/iceland/{karla,kvenna}/`. Football emits 11 JSONs
  per sex: 7 snapshots (meta, next_games, standings, team_strengths,
  final_positions, points_distribution, home_advantage) plus 4 history
  files (standings_history, team_strengths_history, round_predictions_history,
  final_positions_history). Basketball + handball emit the same 7 snapshots
  plus `final_positions_history.json` (8 each).
- **Schema features (as of 2026-05-03):**
  - `standings.json` rows ship cumulative `xg_for`/`xg_against`/`xpts` over
    archived rounds, plus `n_predicted_matches`/`n_played_matches` for
    partial-coverage disclosure. Lookahead-free: each round uses the
    latest fit strictly before its first kickoff.
  - `team_strengths.json` ships a 9-cell grid per team:
    `component ∈ {offence, defence, total}` × `location ∈ {home, away, avg}`.
    `avg` is the per-draw mean so uncertainty intervals reflect the joint
    posterior. Same grid in `team_strengths_history.json`.
  - `final_positions_history.json` accretes per-round projections so the
    frontend can offer a round filter; deduplicated on `(as_of, team, placement)`.
  - `meta.json` includes `sport` for all three publishers.
- **metill-platform consumption** (as of 2026-05-03): only football
  surfaces on the platform. Of the 11 football JSONs, 6 are rendered today
  — `meta`, `next_games`, `standings`, `team_strengths`,
  `final_positions`, `team_strengths_history`. Three (`final_positions_history`,
  `standings_history`, `home_advantage`, `points_distribution`) are
  available for frontend rendering but not yet wired up.
  `round_predictions_history` is publisher-internal (read by
  `R/publish-football-iceland.R` itself to populate `xg_for/xg_against/xpts`
  in `standings`). Basketball/handball publish output is generated and
  rsynced but not rendered until autumn 2026. See
  `~/.claude/projects/-Users-brynjolfurjonsson-sports/memory/project_publish_consumers.md`
  and Obsidian: `Sports/Knowledge/Publish Pipeline/data-contract.md`
  (compiled-truth catalogue with per-JSON schemas).
- Daily driver: `Rscript scripts/05_publish.R`. Wires fresh-fit-on-demand
  via `R/publish-pipeline.R::publish_one()`.

### Betting conventions

`.claude/rules/sports-betting.md` — P1–P4 placement rules, bets.yml schema (Plan 3 adds).

### CI / GitHub Actions

Two coordinated changes, both applied to all five workflows under
`.github/workflows/`:

**1. `PKG_SYSREQS: "false"` on `setup-r-dependencies@v2`** disables pak's
automatic `add-apt-repository` install of system requirements.

- Rationale: `chromote`'s `SystemRequirements` field maps to
  `ppa:xtradeb/apps` on Ubuntu. Registering that PPA queries
  `api.launchpad.net` for metadata, and Launchpad outages periodically
  time out (`TimeoutError: [Errno 110]`) for several hours at a time —
  silently breaking every R-package install on every workflow. Disabling
  pak's sysreqs handler removes the dependency on Launchpad entirely.
- Substitute coverage: the Ubuntu 24.04 GitHub-hosted runner image
  pre-installs Google Chrome and Chromium (so chromote works at
  runtime), and the standard `ubuntu-latest` libraries (libcurl,
  libssl, libxml2, libyaml) cover every other declared
  `SystemRequirements` in the project's package set. The
  `browser-actions/setup-chrome@v1` step in the scrape workflows
  further pins Chrome and exports `CHROMOTE_CHROME`.

**2. Reinstall `V8` from source with `DOWNLOAD_STATIC_LIBV8=1`** after
`setup-r-dependencies`:

```yaml
- name: Reinstall V8 from source with bundled static libv8
  env:
    DOWNLOAD_STATIC_LIBV8: "1"
  run: |
    Rscript -e 'install.packages("V8", type = "source", repos = c(CRAN = "https://cloud.r-project.org"))'
```

- Rationale: `jsonvalidate` (used in `R/config.R::validate_leagues` to
  check `leagues.yml` against `leagues.schema.json`) depends on `V8`.
  Posit Package Manager's pre-built `V8` binary for Ubuntu 24.04 noble
  was linked against `libnode.so.109` (Node 18.x ABI), but noble ships
  Node 20.x, which provides `libnode.so.115`. With sysreqs disabled, no
  libnode is installed at all, so `dyn.load("V8.so")` fails immediately
  on every entry script that calls `load_leagues()` (every entry
  script does).
- Fix: build `V8` from source on the runner with
  `DOWNLOAD_STATIC_LIBV8=1`. V8's configure script downloads a static
  libv8 binary from CRAN's V8 release page and links against it. The
  resulting V8 `.so` has zero system library dependencies. This step
  takes ~30 s but is robust against any libnode/libv8 ABI changes in
  the runner image.
- The order matters: this step runs **after** `setup-r-dependencies`
  (which has installed the broken PPM binary) and overwrites V8 in
  `R_LIBS_USER`.

**Detection if a future package adds an unmet sysreq.** With
`PKG_SYSREQS=false`, pak still _prints_ system requirements but skips
the apt install. If a new R package declares a sysreq the runner
image doesn't pre-install, the failure is a runtime
`dyn.load: cannot open shared object file` (loud, immediate). The
remedy is either a one-line `apt-get install` step before
`setup-r-dependencies`, or — if the issue is an ABI mismatch like the
V8 one — a from-source rebuild step like the V8 one above.

**Workflow name gotcha (`workflow_run` glob trap).** The
`on.workflow_run.workflows` array uses glob patterns, not literal
strings. Special meta-characters (`+`, `*`, `?`, `[`, `]`, `!`) in a
workflow's `name:` field will silently break any `workflow_run`
trigger that references it ([github/docs#12572](https://github.com/github/docs/issues/12572)).
This bit us once: the upstream workflow was named
`"Scrape Federation Results + Schedules"`, so the downstream
`fit.yml`'s `workflows: ["Scrape Federation Results + Schedules"]`
never matched, and the chain silently produced zero fit runs from
2026-04-30 cutover until 2026-05-01 when the bug was found. Fix:
renamed to `"Scrape Federation Results and Schedules"` (no
meta-characters). Keep all workflow `name:` fields free of glob
meta-characters.

### Placer (local-only)

- `Rscript scripts/place_bets.R` is the public entrypoint. Default is dry-run; `--live` actually places. Always reads `LENGJAN_USER` / `LENGJAN_PASS` from `.Renviron`.
- `Rscript scripts/preview_bets.R` shows pending bets without opening a browser.
- `R/placer-*.R` is **never** wired into CI; the `test-placer-ci-isolation.R` test enforces this.
- The placer is the only writer to `data/decisions/ledger/` (Parquet, canonical). Plan 6 cutover dropped the CSV dual-write default; opt-in via `dual_write_csv = TRUE` if a legacy CSV regression check is needed.
- P1: idempotent (dedup against ledger). P2: ledger records actual Lengjan odds. P3: Kelly stake recomputed if odds drift. P4: bets no longer +EV are rejected.

## Skills

The four model-invocable skills under `.claude/skills/` (`/bet`, `/sports-update`, `/add-league`, `/place-bets`) call `scripts/0N_*.R` directly. Drift back to legacy invocations is guarded by `tests/testthat/test-skill-conventions.R`, which fails the build if any skill references `lengjan-bets/`, `lengjan-odds/`, `Sports/{sport}/{country}/`, the `--sync` flag, or the legacy `Rscript run.R --step` pattern.

**Do not add `disable-model-invocation: true` to these skills.** They are intentionally model-invocable.

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

Each topic has a `_MOC.md` entry point — read it first, then selectively load sub-documents.

## Things 3

Route actionable tasks to the **Metill.is** area (ID: `4WyyavEFjCPunRi9iD5tKe`), project **Sports**.
