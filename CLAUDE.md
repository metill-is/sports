# Sports — CLAUDE.md

Bayesian sports prediction models across basketball, handball, and football. Shared code in `R/`, per-league data and configs in `{sport}/{country}/`.

## Directory structure

```
Sports/
├── run.R                          # Unified CLI entry point
├── config/leagues.yml             # 17-league registry
├── Stan/                          # Canonical Stan models
├── R/
│   ├── pipeline/                  # Unified dispatchers (config, step_data, step_fit, step_bet)
│   ├── config/                    # Per-sport config objects (get_config())
│   ├── shared/                    # Shared pipeline (prep_data, model_fitting, get_model_results)
│   ├── bets/                      # Betting modules (kelly, markets, odds, output, history)
│   ├── storage/                   # Centralised Parquet store (store.R, migrate_history.R)
│   ├── schedule/                  # Schedule scanner (scan.R)
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
```

**Selectors** (pick one): `--sport`, `--country`, `--league`, `--all`, `--active`, `--stale`
**Steps**: `--step data,fit,results,bet,settle` (default: all five)
**Modifiers**: `--stale` (filter to leagues with upcoming odds + stale/missing fit)
**Overrides**: `--sex male|female`, `--iter <n>`, `--no-plots`, `--sync`, `--dry-run`

**Step execution order**: Steps run in phases — all data first, then all fit, then all results, then all bet, then all settle. No per-league interleaving.

**Step semantics**:
- `fit` = Stan sampling only (saves `.rds`), no results generation
- `results` = generate posterior CSVs + plots from existing `.rds`
- If `bet` is requested without `results`, `results` is auto-injected (bet needs posterior CSVs)

### Kelly fraction tuning

```bash
Rscript R/bets/update_kelly.R              # Compute optimal per-sex kelly_frac and update bets.yml
Rscript R/bets/update_kelly.R --dry-run    # Preview proposed values without writing
```

### Cross-league summary

```bash
Rscript R/summary/pnl.R              # Full PnL breakdown by sport/country
Rscript R/summary/pnl.R --settled    # Only settled bets
```

### Legacy scripts (still functional, Iceland only)

```bash
Rscript -e 'source("update.R")'        # Full: data + fit + results
Rscript -e 'source("update_data.R")'   # Data + schedules only
Rscript -e 'source("check_schedules.R")' # Scan schedules → active_competitions.json
```

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
step_bet.R → load odds → Kelly criterion → output bets (per-league config/bets.yml)
```

### Three pipeline types

| Pipeline | Used by | Fit handler |
|---|---|---|
| `shared` | basketball/iceland, handball/iceland | `R/shared/model_fitting.R` (centralised) |
| `football` | football/* | `R/shared/model_fitting.R` via `withr::with_dir()` + per-league `prep_data.R` |
| `handball_other` | handball/{dk,fr,de,no,es,se,…} | `R/shared/model_fitting.R` via `withr::with_dir()` + `handball/other/R/utils/prep_data.R` |

### Stan models

| Sport | Model | File |
|---|---|---|
| Basketball, Handball | 2D Student's t | `2d_student_t.stan` |
| Football | Diagonal-inflated bivariate Poisson | `bivariate_poisson_inflated_diagonal_corrmodel.stan` |

Both use time-varying team strengths (random walk), separate offensive/defensive parameters, and home advantage effects.

### Data flow

```
data/{sex}/data.csv + schedule.csv
    → results/{sex}/fit.rds
    → results/{sex}/posterior_goals.csv + figures/*.png
    → Website via raw.githubusercontent.com URLs
    → store/ (Hive-partitioned Parquet, dual-write)
```

### Parquet store (`store/`)

Hive-partitioned Parquet store for cross-league queries. Dual-write layer — CSV remains source of truth, Parquet mirrors it. Failures never break the pipeline.

```
store/
├── predictions/sport={X}/country={Y}/sex={Z}/predictions.parquet
└── bets/sport={X}/country={Y}/sex={Z}/bets.parquet
```

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

| Topic | Where | When loaded |
|---|---|---|
| Per-sport data sources & quirks | `.claude/rules/sports-per-sport.md` | Any `Sports/**` file |
| Pipeline architecture & leagues.yml | `.claude/rules/sports-pipeline.md` | `R/pipeline/**`, `run.R`, `config/**` |
| Betting conventions & bets.yml | `.claude/rules/sports-betting.md` | `R/bets/**`, `config/bets.yml` |
| R code style | `.claude/rules/r-conventions.md` | Any `*.R` file |
| Stan conventions | `.claude/rules/stan-conventions.md` | Any `*.stan` file |

## Skills

| Skill | Purpose |
|---|---|
| `/bet` | Run betting pipeline for active or specified leagues |
| `/sports-update` | Full pipeline: data + fit + results + bet |
| `/add-league` | Add a new league to the pipeline |

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
