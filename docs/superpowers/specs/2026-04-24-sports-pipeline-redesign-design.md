# Sports pipeline redesign — design

Author: Brynjólfur (with Claude Code)
Date: 2026-04-24
Status: Draft for review

## 1. Purpose

Consolidate the current four-repo Bayesian sports prediction system into a single monorepo with a formal storage layer, unified pipeline path, and an isolated local-only bet placer. Objectives:

- **Simple** — one repo, one config, one pipeline type, one storage layer
- **Robust** — schemas validated at every seam, no cross-repo path coupling, no silent schema drift
- **Efficient** — Parquet-backed storage with SQL query access via DuckDB, automated end-to-end pipeline via `{targets}` DAG on CI
- **Research-friendly** — archived odds + archived posteriors enable lookahead-free walk-forward backtesting; promoting a new Stan model is a one-line config change

This design replaces the current topology: `Sports/` + `lengjan-odds/` + `livesport-data/` + `lengjan-bets/` (four independent git repos, CSV-on-git as a message bus, three parallel pipeline types inside `Sports/`, redundant league metadata across three YAML files, manual website publish).

## 2. Scope

### Phase 1 (this design, execute in one session)

Migrate **three active Icelandic leagues** into the new monorepo end-to-end:

- `football_iceland` (male + female)
- `basketball_iceland` (male + female)
- `handball_iceland` (male + female)

Data sources in scope:

- KSÍ scraper (football federation)
- KKÍ / basketball federation
- HSÍ (handball federation)
- Lengjan odds scraper (retains schedule-aware filtering, 3×/day)
- Lengjan bet placer (local-only)

Website publish in scope:

- `football_iceland` (current coverage — 9 JSONs per sex)
- `basketball_iceland` + `handball_iceland` scaffolding (JSON producers wired up, templates deferred unless trivial)

### Phase 2+ (deferred)

Paused non-Icelandic leagues stay in `leagues.yml` with `active: false`. Re-activation is a phased exercise:

- Revive `livesport-data` as a **data source plugin** under `R/ingest/livesport/` when any non-Icelandic league is needed
- Revive `handball/other/` 12-country pipeline without symlinks (storage layer makes them unnecessary)
- Revive `football/{england, italy, spain, norway}` one at a time as appetite returns

No new features or models are in scope. Current Stan models move over as-is. Schema/column-name unification happens during migration (see §4.3).

## 3. Architecture — end state

### 3.1 Repo topology

```
sports/                                    # Public monorepo, R throughout
├── config/
│   ├── leagues.yml                        # ONE source of truth
│   ├── bankroll.yml                       # Global Kelly cap, daily budget
│   └── schemas/*.json                     # JSONSchema validators (generated from R)
├── R/
│   ├── ingest/                            # Was: lengjan-odds/, federation scrapers
│   │   ├── lengjan_odds.R
│   │   ├── ksi_football.R
│   │   ├── kki_basketball.R
│   │   └── hsi_handball.R
│   ├── model/                             # Was: Sports/R/shared + Sports/R/config + three pipeline types
│   │   ├── prepare_data.R                 # prepare_data(league, sex) → list(stan_data, metadata)
│   │   ├── fit.R                          # fit(league, sex, stan_model) → fit.rds
│   │   └── posteriors.R                   # extract(fit) → posterior table
│   ├── decide/                            # Was: Sports/R/bets/
│   │   ├── calibration.R
│   │   ├── kelly.R                        # Joint Kelly per match
│   │   ├── portfolio.R                    # Cross-league daily budget
│   │   └── recommendations.R              # Produce candidate/recommended tables
│   ├── placer/                            # Was: lengjan-bets/ — LOCAL ONLY
│   │   ├── login.R
│   │   ├── navigate.R
│   │   ├── place_bet.R
│   │   ├── ledger.R
│   │   └── README.md                      # "This subsystem runs on your laptop only"
│   ├── publish/                           # Was: football/iceland/R/export_website_data.R
│   │   └── website_json.R
│   ├── storage/                           # NEW — source of truth for persistence
│   │   ├── schemas.R                      # Arrow schemas for all tables
│   │   ├── store.R                        # Read/write primitives (Parquet + DuckDB)
│   │   └── duckdb_views.R                 # DuckDB view rebuild from Parquet
│   └── research/                          # NEW
│       ├── walkforward.R                  # Rolling-window backtest
│       └── compare_models.R               # ELPD/LOO over variants
├── Stan/
│   ├── football_iceland/*.stan
│   ├── basketball_iceland/*.stan
│   ├── handball_iceland/*.stan
│   └── research/*.stan                    # Experimental variants
├── data/                                  # Git-tracked; auto-committed by CI
│   ├── facts/
│   │   ├── results/sport={X}/country={Y}/sex={Z}/season={YYYY}/results.parquet
│   │   ├── schedules/sport={X}/country={Y}/sex={Z}/season={YYYY}/schedule.parquet
│   │   └── odds/sport={X}/country={Y}/scraped_date={YYYY-MM-DD}/odds.parquet
│   ├── beliefs/
│   │   ├── latest/sport={X}/country={Y}/sex={Z}/posterior.parquet   # overwritten
│   │   └── archive/sport={X}/country={Y}/sex={Z}/fit_date={YYYY-MM-DD}/posterior.parquet
│   ├── decisions/
│   │   ├── candidates/sport={X}/country={Y}/run_date={YYYY-MM-DD}/candidates.parquet
│   │   ├── recommendations/sport={X}/country={Y}/run_date={YYYY-MM-DD}/recommendations.parquet
│   │   └── ledger/sport={X}/country={Y}/ledger.parquet              # append-only, placer writes
│   └── publish/
│       ├── football/iceland/karla/*.json
│       ├── football/iceland/kvenna/*.json
│       ├── basketball/iceland/karla/*.json
│       └── handball/iceland/karla/*.json
├── sports.duckdb                          # Regenerated view layer over Parquet (gitignored)
├── run.R                                  # Thin CLI → tar_make(...)
├── _targets.R                             # DAG definition
├── DESCRIPTION                            # Optional; treat monorepo as a package
├── .Renviron.example                      # Placer credential template
└── .github/workflows/
    ├── scrape-odds.yml                    # cron: 3×/day → commit data/facts/odds/
    ├── scrape-results.yml                 # cron: 1×/day → commit data/facts/{results,schedules}/
    ├── fit-and-publish.yml                # post-scrape → commit beliefs/, decisions/candidates,recommendations/, publish/
    └── ci-tests.yml                       # on push: schema tests + placer-isolation test

metill-platform/                           # Separate repo, unchanged structure
├── app/...                                # FastAPI + templates + Observable Plot
├── data/ithrottir/                        # Pulled from sports/data/publish/ by scheduled workflow
└── .github/workflows/
    └── pull-sports-data.yml               # cron: hourly → git clone sports, copy JSON, commit, push (triggers Fly deploy)
```

### 3.2 Storage layer

**All four stores are Parquet on disk**, hive-partitioned, committed to the monorepo on `main`. A derived `sports.duckdb` file gives SQL query access (rebuildable from Parquet; gitignored to avoid binary diff noise).

| Store                       | Schema key columns                                                                                                                            | Partition                  | Written by      | Read by                     |
| --------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------- | --------------- | --------------------------- |
| `facts/results`             | sport, country, sex, season, match_date, home_team, away_team, home_score, away_score                                                         | sport/country/sex/season   | scrapers        | model                       |
| `facts/schedules`           | sport, country, sex, season, match_date, home_team, away_team, round                                                                          | sport/country/sex/season   | scrapers        | model, decide               |
| `facts/odds`                | sport, country, scraped_at, match_date, home_team, away_team, market, outcome, line, odds                                                     | sport/country/scraped_date | lengjan scraper | decide, research            |
| `beliefs/latest`            | sport, country, sex, fit_date, home_team, away_team, draw_id, home_goals, away_goals                                                          | sport/country/sex          | model           | decide, publish             |
| `beliefs/archive`           | same + fit_date                                                                                                                               | sport/country/sex/fit_date | model           | research                    |
| `decisions/candidates`      | run_id, sport, country, sex, match_date, market, outcome, p, odds, ev, kelly_raw, stage                                                       | sport/country/run_date     | decide          | research                    |
| `decisions/recommendations` | run_id, sport, country, sex, match_date, home_team, away_team, market, outcome, line, p, odds, ev, kelly, bet_amount                          | sport/country/run_date     | decide          | placer, website             |
| `decisions/ledger`          | placed_at, match_date, sport, country, sex, home_team, away_team, market, outcome, line, odds_placed, p, kelly, bet_amount, settled, win, pnl | sport/country              | placer          | decide (bankroll), research |

**Schema definition and validation.** Arrow schemas declared in `R/storage/schemas.R` as R objects. A small helper generates JSONSchema equivalents for the website publish layer. Every `store.R::write_*()` primitive validates against its schema before writing; write fails loud.

**Ledger writes.** The placer remains the **only** writer to `decisions/ledger/`. Enforced in code by: (a) only `R/placer/` imports `ledger_write()`, (b) CI test greps for `ledger_write` outside `R/placer/` and fails the build. The ledger file is rewritten in full on each placement/settlement (small enough that this is trivial — ~thousands of rows).

**DuckDB.** `sports.duckdb` is regenerated on demand by `R/storage/duckdb_views.R`, which declares views over Parquet paths. Used for research and ad-hoc queries:

```r
duckdb::dbGetQuery(con, "
  SELECT sport, country, sex, run_date, SUM(bet_amount) AS stake, SUM(pnl) AS pnl
  FROM ledger
  WHERE settled AND placed_at > '2026-01-01'
  GROUP BY 1, 2, 3, 4
")
```

### 3.3 Column-naming convention

Internal schemas use **English throughout**. Icelandic appears only at the publish boundary (website JSON keys may be Icelandic if templates prefer).

Canonical names:

- Teams: `home_team`, `away_team` (not `heima`/`gestir`, not `home`/`away`)
- Dates: `match_date`, `fit_date`, `scraped_at`, `run_date`, `placed_at`, `settled_at`
- Market: `market` ∈ {`moneyline`, `spread`, `total`}
- Outcome: `outcome` ∈ {`home`, `draw`, `away`, `over`, `under`}
- Line: `line` (signed for spread, positive for total, NA for moneyline)
- Probabilities: `p` (decimal, 0–1) — one name, used throughout. `p_home`, `p_draw`, `p_away`, `p_over`, `p_under` when disambiguating pivoted columns
- Odds: `odds` (decimal), `odds_placed` (in ledger, actual Lengjan odds per P2)
- Kelly: `kelly` (fraction of bankroll), `kelly_raw` (pre-scaling), `bet_amount` (ISK)
- Identifiers: `run_id` (ISO timestamp grouping a pipeline run), `draw_id` (MCMC draw index)

The `sex=all` settlement partition hack is eliminated: settlement flips the `settled`/`win`/`pnl` columns on the existing per-sex rows rather than writing a new partition.

### 3.4 Compute layers

Five layers, each a directory under `R/`, each a set of pure-ish functions with explicit inputs/outputs typed to the schemas:

**Ingest.** One script per data source, each writing to a fact store partition. Schedule-aware filtering still applies — `config/active_competitions.json` is generated by a targets step that reads current schedules, consumed by the scrape steps.

**Model.** The central unification. One `prepare_data(league, sex) → list(stan_data, metadata)` contract. Per-league implementations live in `R/model/prepare/{league_key}.R` and are dispatched from `config/leagues.yml`'s `data_source` field. The `withr::with_dir()` path-hack pattern is deleted. Fit is a single `fit(league, sex, stan_model, iter, method)` that writes `beliefs/latest/` and `beliefs/archive/`.

**Decide.** Takes (posterior, odds, bankroll, policy) → (candidates, recommendations). Joint Kelly per match, then cross-league portfolio optimisation on daily budget. Calibration multiplier is computed inside the decide layer, not in orchestration. Outputs both `candidates` (pre-filter, for research) and `recommendations` (post-filter, for placer).

**Placer.** Reads `decisions/recommendations` for today, dedups against `decisions/ledger`, authenticates to Lengjan via ChromoteSession, navigates and places, appends to ledger. P1–P4 rules as today. Pre-flight schema validation aborts before login on any config mismatch.

**Publish.** Reads `beliefs/latest/` + `decisions/recommendations/` + `facts/schedules/` + `facts/results/` and writes JSON to `data/publish/`. JSONSchema-validated. Website pulls these files via scheduled workflow.

**Research.** Walk-forward backtester reads `facts/odds` (with `scraped_at` preserving what was known at decision time) + `beliefs/archive` (with `fit_date`). Reconstructs "what would we have done on date D under policy P with model M?" and scores against subsequent `facts/results`. No production code touched.

### 3.5 Orchestration

`{targets}` throughout. `_targets.R` defines a DAG across all five layers:

```
scrape_odds ──┐
              ├──> active_competitions ──> prepare_data ──> fit ──> posteriors ──┐
scrape_results ──┘                                                                ├──> decide ──> recommendations
                                                                                  │
                                                                         bankroll ┘
                                                                                       │
                                                                                       └──> publish
```

Per-league targets are generated dynamically from `config/leagues.yml` (same pattern the scrapers use today). `run.R` is a thin CLI wrapper mapping `--league X --step fit` to `tar_make(names = c("fit_X"))`.

CI wires this up as three workflows:

- `scrape-odds.yml` — cron 3×/day; runs odds scrape targets; commits `[data] scrape odds` to `main`
- `scrape-results.yml` — cron 1×/day; runs results/schedule scrape targets; commits `[data] scrape results`
- `fit-and-publish.yml` — triggered on push to `main` affecting `data/facts/**`, or cron; runs model + decide + publish targets; commits `[data] fit/decide/publish`

The placer has no CI workflow. It runs via `Rscript R/placer/run.R` on the laptop, reading `.Renviron` for credentials. After placement it `git add data/decisions/ledger && git commit && git push`.

### 3.6 Config — one source of truth

`config/leagues.yml` holds every fact about a league, including scraper URLs and team-name mappings:

```yaml
football_iceland:
  sport: football
  country: iceland
  sexes: [male, female]
  active: true
  data_source:
    results: ksi_football
    schedule: ksi_football
    odds: lengjan_odds
  lengjan:
    competitions:
      - { id: "14", name: "Besta deild karla", sex: male }
      - { id: "15", name: "Besta deild kvenna", sex: female }
    team_names:
      # One block per league, inline
      "KR": "KR Reykjavík"
      "ÍA": "Íþróttabandalag Akraness"
      ...
  stan_model: football_iceland/bivariate_poisson_no_inflation.stan
  betting:
    kelly_fraction: 0.10
    markets: [moneyline, spread, total]
    min_bet: 200
```

The three separate YAML files (`lengjan-odds/config/competitions.yml`, `livesport-data/config/competitions.yml`, `Sports/config/leagues.yml`) + the sprawl of `team_names_*.csv` files merge into this one file, validated against a JSONSchema on load.

## 4. Migration plan — phase 1

### 4.1 Preparation (before any restructure)

1. Create `/Users/brynjolfurjonsson/sports/.git` via `git init`; first commit is this spec + a placeholder `README.md`
2. Set up remote: `gh repo create metill-is/sports --public` (or private, user's call at execution time)
3. Commit this spec to establish a design anchor before structural churn

### 4.2 Subtree merge (preserve four repos' histories)

```bash
# From sports/ monorepo root, after first commit:
git remote add sports-code     https://github.com/metill-is/sports
git remote add lengjan-odds    https://github.com/metill-is/lengjan-odds
git remote add livesport-data  https://github.com/metill-is/livesport-data
git remote add lengjan-bets    https://github.com/metill-is/lengjan-bets

git fetch --all

git subtree add --prefix=_legacy/sports     sports-code    main
git subtree add --prefix=_legacy/lengjan-odds   lengjan-odds   main
git subtree add --prefix=_legacy/livesport-data livesport-data main
git subtree add --prefix=_legacy/lengjan-bets   lengjan-bets   main
```

All four histories are preserved under `_legacy/`. Migration means moving files from `_legacy/` to the new structure, not rewriting. Git history for any file is one `git log --follow` away.

### 4.3 Migration steps (in order)

1. **Storage layer first.** Implement `R/storage/schemas.R` and `R/storage/store.R` with Arrow schema definitions for all eight tables. Write unit tests that round-trip a valid example and reject an invalid one.
2. **One-time ETL.** A throwaway script reads the existing CSV ledger(s) and predictions and writes them as Parquet under `data/`. Validate: row counts match, PnL totals match to the cent.
3. **Model layer for `basketball_iceland` first** (simplest — single data source, single model, existing `shared` pipeline). Implement `R/model/prepare/basketball_iceland.R` → `prepare_data()`. Implement `fit.R` and `posteriors.R`. Compare posteriors against current production fit for one date — must match.
4. **Model layer for `handball_iceland`.** Same pattern. Same match test.
5. **Model layer for `football_iceland`.** Same pattern. Same match test.
6. **Decide layer.** Port Kelly + portfolio + calibration. Produce `recommendations.parquet` for one date. Compare against current `recommendations.csv` — must match (or if it differs, explain why the new number is more correct).
7. **Placer.** Port from `_legacy/lengjan-bets/` largely as-is. Swap config reads from `lengjan-odds/config/*` to `config/leagues.yml`. Read `decisions/recommendations/` from Parquet. Write ledger to Parquet directly (small file, rewrite on append/settle is instantaneous). During cutover, keep the legacy CSV ledgers intact and write both for the first few bets as a belt-and-suspenders; drop CSV writes once a week of production runs agree.
8. **Publish.** Port `export_website_data.R`. Validate JSON against JSONSchema. Visual parity check against current website.
9. **CI workflows.** `scrape-odds.yml`, `scrape-results.yml` (federations scrapers; livesport stays deferred), `fit-and-publish.yml`, `ci-tests.yml`.
10. **metill-platform side.** Add `pull-sports-data.yml` hourly workflow that clones `sports`, copies `data/publish/**` into `data/ithrottir/`, commits, pushes. Fly.io auto-deploys on push.
11. **Cut over.** Run one full cycle end-to-end. Place one small bet manually via new placer. Confirm ledger entry. Archive old repos (set read-only; keep on GitHub for history).

### 4.4 Validation gates

Each of steps 3–8 has a "must match current production" gate. If they don't match, pause and investigate — don't hand-wave. This is the failure mode a rewrite is most vulnerable to.

## 5. Out of scope

- Any new Stan model or likelihood
- Any change to Kelly policy or portfolio logic (port 1:1; improvements are separate spec)
- Reviving `livesport-data` (deferred; when revived, goes under `R/ingest/livesport/`)
- Reviving any paused non-Icelandic league (deferred; phase 2 item)
- Migrating the handball/other 12-country structure (deferred; simplification happens when revived)
- Website redesign (metill-platform structure unchanged)
- Auth/secrets management changes (placer credentials stay in `.Renviron`)

## 6. Risks and open questions

**Risks:**

- **Schema-migration correctness.** The ETL step must not lose data. Mitigation: row-count and PnL-total checks, plus a "replay one historical day and match outputs" test.
- **Git repo size.** Parquet data commits will grow the repo. Estimated ~50 MB/year for odds + ~5 MB/year for other tables. Manageable; if it becomes a problem, consider Git LFS for `data/facts/odds/` specifically.
- **CI wall-clock time.** Fit step on CI for three Icelandic leagues × two sexes is ~30 min. Runs at most once per scrape cycle; acceptable.
- **Placer isolation slip.** A future contributor wires CI to run the placer. Mitigation: CI test that greps workflow files for `placer`, plus no GitHub Actions secret named `LENGJAN_*`.

**Open questions (resolve during execution, not pre-spec):**

- Do we need a `DESCRIPTION` file and treat the monorepo as an R package? Probably yes for `devtools::load_all()` ergonomics.
- What's the exact schema for the `candidates` table? (Today's `recommendations_log.csv` is close but messy.)
- Should `sports.duckdb` be rebuildable on demand or maintained incrementally? On demand is simpler; incremental is faster for research. Start with on-demand.

## 7. References

- Current state CLAUDE.md files: `Sports/CLAUDE.md`, `lengjan-odds/CLAUDE.md`, `livesport-data/CLAUDE.md`, `lengjan-bets/CLAUDE.md`, `metill-platform/CLAUDE.md`
- Rules: `Sports/.claude/rules/sports-pipeline.md`, `sports-betting.md`, `sports-per-sport.md`
- Knowledge topics: `Metill/Knowledge/Betting Optimisation/_MOC.md`, `Sports Models/_MOC.md`, `Lengjan Pipeline/_MOC.md`, `Livesport Data/_MOC.md`
