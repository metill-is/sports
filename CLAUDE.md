# Sports — CLAUDE.md

Bayesian sports prediction models across basketball, handball, and football. Shared code in `R/`, per-league data and configs in `{sport}/{country}/`.

## Directory structure

```
Sports/
├── run.R                          # Unified CLI entry point
├── config/leagues.yml             # 18-league registry
├── Stan/                          # Canonical Stan models
├── R/
│   ├── pipeline/                  # Unified dispatchers (config, step_data, step_fit, step_bet)
│   ├── config/                    # Per-sport config objects (get_config())
│   ├── shared/                    # Shared pipeline (prep_data, model_fitting, get_model_results, extract_posterior)
│   ├── backtest/                  # Model comparison backtesting (backtest_football.R, plot_backtest.R)
│   ├── bets/                      # Betting modules (kelly, portfolio, markets, odds, calibration, output, history)
│   ├── storage/                   # Centralised Parquet store (store.R, migrate_history.R)
│   ├── schedule/                  # Schedule scanner (scan.R)
│   ├── status/                    # JSON status endpoints for Raycast monitor (pipeline_status.R, settle_now.R)
│   └── lengjan/                   # Legacy Lengjan scraping (superseded by lengjan-odds/)
├── basketball/{iceland,international}/
├── football/{iceland,england,italy,spain,norway}/
└── handball/{iceland,international,other,denmark,france,germany,norway,spain,sweden,...}/
```

## Commands

### Unified pipeline (`run.R`)

```bash
# From Sports/ directory:
Rscript run.R --all --dry-run                        # Preview what would run
Rscript run.R --league football_england --step bet    # Bet on one league
Rscript run.R --sport handball --step bet             # Bet on all handball leagues
Rscript run.R --country iceland --step data,fit,results,bet  # Full pipeline for Iceland
Rscript run.R --active --step data,fit,results,bet           # Only leagues with upcoming games
Rscript run.R --all --step fit --iter 200             # Quick test fit (200 iterations)
Rscript run.R --league basketball_iceland --step results  # Re-generate posteriors from .rds
Rscript run.R --stale --dry-run                          # Preview stale leagues
Rscript run.R --stale --step data,fit,results,bet         # Full pipeline on stale leagues only
Rscript run.R --due --dry-run                             # Preview leagues due for refresh
Rscript run.R --due                                       # Auto-runs data,fit,results,bet on due leagues
Rscript run.R --league basketball_iceland --method pathfinder --step fit,results  # Fast approximate fit

# Method comparison (MCMC vs Pathfinder vs Variational)
Rscript R/backtest/compare_methods.R --league basketball_iceland --sex male
```

**Selectors** (pick one): `--sport`, `--country`, `--league`, `--all`, `--active`, `--stale`, `--due`
**Steps**: `--step data,fit,results,bet,settle` (default: all five)
**Modifiers**: `--stale` (filter to leagues with upcoming odds + stale/missing fit), `--due` (unbetted matches today/tomorrow + fit >48h old; auto-selects data,fit,results,bet)
**Overrides**: `--sex male|female`, `--iter <n>`, `--method sample|pathfinder|variational`, `--no-plots`, `--sync` (auto for data/settle), `--dry-run`

> **Note:** The pipeline generates `recommendations.csv` but never writes to `bets_log.csv`.
> Bet placement and ledger writes are the exclusive responsibility of `lengjan-bets/`.

### Pre-filter recommendations log

`history/recommendations_log.csv` is an append-only log of every candidate bet the model produced, at every pipeline stage — _not_ just the ones that survive every gate. Written by `R/bets/recommendations_log.R`, called from `run.R` during the bet phase.

Each row carries:

- `run_id` — one pipeline run's ISO timestamp (groups all stages of that run)
- `stage` — one of `candidate`, `post_portfolio`, `kept`, `dropped_min_bet`, `dropped_stale`, `dropped_dedup`
- All standard recommendation fields (`sport`, `country`, `sex`, `date`, `heima`, `gestir`, `market`, `outcome`, `o`, `p`, `ev`, `kelly`, `bet_amount`, `limit`, `booker`)

**Why it exists:** `recommendations.csv` is lossy for back-testing — it only holds survivors, so you can never evaluate the cost of a filter rule after the fact. The log keeps everything so back-tests can replay the candidate set against alternative policies (lower EV threshold, different min_bet, tweaked Kelly floor). Pair it with `predictions_archive/` to make lookahead-free counterfactuals possible.

**Step execution order**: Steps run in phases — all data first, then all fit, then all results, then all bet, then all settle. No per-league interleaving.

**Step semantics**:

- `fit` = Stan inference (saves `.rds`), no results generation. Method: `sample` (default MCMC), `pathfinder`, or `variational`. Note: approximate methods (pathfinder/variational) crash on the current Student's t and bivariate Poisson models due to positive-definite covariance constraints — use `sample` for production
- `results` = generate posterior CSVs from existing `.rds` (plots removed — will be rebuilt for Ghost)
- If `bet` is requested without `results`, `results` is auto-injected (bet needs posterior CSVs)

## Architecture

### Pipeline flow

```
config/leagues.yml → R/pipeline/config.R → filter by CLI args
    ↓
step_data.R → sync/download data (4 sources: baskethotel, hsi, livesport_football, livesport_handball)
             → Livesport sources: sync from ../livesport-data/ (git pull), fallback to Chromote
    ↓
step_fit.R → fit Stan model (3 pipelines: shared, football, handball_other)
    ↓
step_bet.R → calibration → load odds → Kelly criterion → bet packages
    ↓
run.R → cross-match portfolio optimisation → format + filter → recommendations.csv
```

### Three pipeline types

| Pipeline         | Used by                              | Fit handler                                                                                                  |
| ---------------- | ------------------------------------ | ------------------------------------------------------------------------------------------------------------ |
| `shared`         | basketball/iceland, handball/iceland | `R/shared/model_fitting.R` (centralised)                                                                     |
| `football`       | football/\*                          | `R/shared/prep_data_football.R` + `R/shared/get_model_results_football.R` (league labels from `leagues.yml`) |
| `handball_other` | handball/{dk,fr,de,no,es,se,…}       | `R/shared/model_fitting.R` via `withr::with_dir()` + `handball/other/R/utils/prep_data.R`                    |

### Stan models

| Sport              | Model                               | File                                                 |
| ------------------ | ----------------------------------- | ---------------------------------------------------- |
| Basketball Iceland | 2D Student's t (scalar sigma)       | `2d_student_t_scalarsigma.stan`                      |
| Handball Iceland   | 2D Student's t (per-team sigma)     | `2d_student_t.stan`                                  |
| Football Iceland   | No-inflation bivariate Poisson      | `bivariate_poisson_no_inflation.stan`                |
| Football (others)  | Diagonal-inflated bivariate Poisson | `bivariate_poisson_inflated_diagonal_corrmodel.stan` |

Both 2D Student's t variants use time-varying team strengths (random walk), separate offensive/defensive parameters, and home advantage effects. The scalar-sigma variant replaces per-team observation-noise scale with a single scalar; see `Knowledge/Sports Models/next-actions.md` in the Metill Obsidian vault for the audit evidence.

**Handball Iceland** stays on per-team sigma as of 2026-04-20 evening — a full diagnostic + adapt_delta=0.95 refit showed neither variant passes the strict-no-worse gate. The handball ~1% divergence rate at default adapt_delta is an inverse funnel at the right tail of `scale_sigma_def` (Σ stiffens, leapfrog misses); cleared by adapt_delta=0.95 but at the cost of flipping the ESS ranking (tier1 wins min ESS at the cleaner geometry).

**Football Iceland** swapped to `bivariate_poisson_no_inflation.stan` on 2026-04-20 evening after the loo comparison favoured no-inflation by 4.3 SE elpd. Known seed-dependence caveat: production refit at default `adapt_delta=0.8` came in at 4.78% divergences + min ESS_tail 120, vs 1.45% + 573 on the fixed-seed audit fit; both pass on identifiable parameters (posterior means match within MC SE), but the sampler is at the edge of production-hygiene gates on `scale_sigma_def` — same inverse-funnel geometry handball's 2026-04-20 diagnostic identified. `adapt_delta=0.95` plumbing is the queued fix (see next-actions.md). England audit (~2-3h, paused league) also still pending.

All active Stan files emit `vector[N] log_lik` in `generated quantities` for `loo::loo` PSIS-LOO comparisons (basketball + handball Iceland: morning 2026-04-20; football Iceland inflated + no-inflation: evening 2026-04-20).

### Data flow

```
data/{sex}/data.csv + schedule.csv
    → results/{sex}/fit.rds
    → results/{sex}/posterior_goals.csv
    → Website via raw.githubusercontent.com URLs
    → store/ (Hive-partitioned Parquet, dual-write)
```

### Parquet store (`store/`)

Hive-partitioned Parquet store for cross-league queries. Dual-write layer — CSV remains source of truth, Parquet mirrors it. Failures never break the pipeline.

```
store/
├── predictions/sport={X}/country={Y}/sex={Z}/predictions.parquet
├── predictions_archive/sport={X}/country={Y}/sex={Z}/fit_date={YYYY-MM-DD}/predictions.parquet
└── bets/sport={X}/country={Y}/sex={Z}/bets.parquet
```

`predictions/` holds only the latest posterior per bucket (overwritten every run).
`predictions_archive/` is a zstd-compressed, date-partitioned snapshot tier — every fit is preserved, so counterfactual back-tests can reconstruct "what did the model believe at time T?" without lookahead bias. Start collecting now; retroactive reconstruction is impossible once `fit.rds` is overwritten. Read with `read_predictions_archive(sports_dir, fit_date_from = ..., fit_date_to = ...)` from `R/storage/store.R`.

Query across leagues:

```r
source("R/storage/store.R")
read_bets(here::here()) |> dplyr::filter(sex != "all")  # All bets (exclude settlement duplicates)
read_predictions(here::here(), sport = "football")       # All football predictions
```

Backfill from existing CSV history:

```bash
Rscript R/storage/migrate_history.R
```

## Cross-references

| Topic                               | Where                               | When loaded                           |
| ----------------------------------- | ----------------------------------- | ------------------------------------- |
| Per-sport data sources & quirks     | `.claude/rules/sports-per-sport.md` | Any `Sports/**` file                  |
| Pipeline architecture & leagues.yml | `.claude/rules/sports-pipeline.md`  | `R/pipeline/**`, `run.R`, `config/**` |
| Betting conventions & bets.yml      | `.claude/rules/sports-betting.md`   | `R/bets/**`, `config/bets.yml`        |
| R code style                        | `.claude/rules/r-conventions.md`    | Any `*.R` file                        |
| Stan conventions                    | `.claude/rules/stan-conventions.md` | Any `*.stan` file                     |

## Skills

| Skill            | Purpose                                              |
| ---------------- | ---------------------------------------------------- |
| `/bet`           | Run betting pipeline for active or specified leagues |
| `/sports-update` | Full pipeline: data + fit + results + bet            |
| `/add-league`    | Add a new league to the pipeline                     |

## Key conventions

- Sex parameter: `"male"` or `"female"` throughout
- `Sys.setlocale("LC_ALL", "is_IS.UTF-8")` in every script
- `.here` files in every league directory (critical for `here::here()` resolution)
- Lengjan odds scraping now in `../lengjan-odds/` (standalone project)
- Livesport match data scraping now in `../livesport-data/` (daily CI, `git pull` to sync)
- Single mono-repo: `metill-is/sports` (private) — tracks code + config only
- `.gitignore` uses deny-all approach: `*` then `!**/*.R`, `!**/*.stan`, `!**/config/*.yml`, etc.
- Data (`**/data/`), results (`**/results/`), model fits (`*.rds`), and images (`*.png`) are **not tracked**
- Previous per-league repos preserved on GitHub for history (football-england, football-italy, etc.)
