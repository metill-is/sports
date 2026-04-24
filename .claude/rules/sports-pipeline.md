---
paths:
  - "Sports/R/pipeline/**"
  - "Sports/run.R"
  - "Sports/config/leagues.yml"
  - "Sports/config/bankroll.yml"
  - "Sports/check_schedules.R"
---

# Unified Pipeline Conventions

## `config/leagues.yml` schema

Each league entry supports these fields (defaults merged from `defaults:` block):

| Field             | Required          | Values                                                                          | Purpose                              |
| ----------------- | ----------------- | ------------------------------------------------------------------------------- | ------------------------------------ |
| `sport`           | Yes               | `basketball`, `handball`, `football`                                            | Sport type                           |
| `country`         | Yes               | Country name (lowercase)                                                        | Country identifier                   |
| `dir`             | Yes               | e.g. `handball/denmark`                                                         | Relative path from `Sports/`         |
| `sex`             | Yes               | `[male]`, `[female]`, `[male, female]`                                          | Which sexes to process               |
| `stan_model`      | Yes               | `2d_student_t.stan` or `bivariate_poisson_inflated_diagonal_corrmodel.stan`     | Stan model file                      |
| `pipeline`        | Yes               | `shared`, `football`, `handball_other`                                          | Which fit/results code to call       |
| `data_source`     | Yes               | `baskethotel`, `hsi`, `iceland_ksi`, `livesport_football`, `livesport_handball` | Data download handler                |
| `has_bets`        | Yes               | `true`/`false`                                                                  | Whether league has `config/bets.yml` |
| `config_module`   | Shared only       | e.g. `R/config/basketball_iceland.R`                                            | Config file for shared pipeline      |
| `rproj`           | Optional          | `.Rproj` filename                                                               | For `withr::with_dir()` context      |
| `iter_warmup`     | Default: 1000     | Integer                                                                         | MCMC warmup iterations               |
| `iter_sampling`   | Default: 1000     | Integer                                                                         | MCMC sampling iterations             |
| `chains`          | Default: 4        | Integer                                                                         | Number of MCMC chains                |
| `parallel_chains` | Default: 4        | Integer                                                                         | Parallel chain count                 |
| `method`          | Default: `sample` | `sample`, `pathfinder`, `variational`                                           | Inference method                     |

League keys use format `{sport}_{country}` (e.g. `handball_denmark`).

## Three pipeline types

| Pipeline         | Sports                               | Fit handler                                                                                     | How it works                                                                        |
| ---------------- | ------------------------------------ | ----------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------- |
| `shared`         | basketball/iceland, handball/iceland | `R/shared/model_fitting.R`                                                                      | Loads config via `config_module`, calls shared prep + fit + results                 |
| `football`       | football/\*                          | `R/shared/prep_data_football.R` + `R/shared/model_fitting.R` (league labels from `leagues.yml`) | Centralised football handler; `withr::with_dir()` into league dir only for data I/O |
| `handball_other` | handball/{dk,fr,de,no,es,se,…}       | `handball/other/R/utils/model_fitting.R`                                                        | `withr::with_dir()` into `handball/other/`, passes country param                    |

## Step dispatchers (`R/pipeline/`)

### `config.R`

- `load_leagues()` — reads `config/leagues.yml`, merges defaults into each entry
- `filter_leagues(leagues, args)` — filters by `--sport`, `--country`, `--league`, `--all`, `--active`
- `schedule_to_league_keys()` — maps schedule registry keys to leagues.yml keys for `--active`

### `step_data.R` — 4 handlers

| `data_source`        | Handler                                                                                                                                                                                                                                                       | What it does                                |
| -------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------- |
| `baskethotel`        | Downloads Excel from Baskethotel API                                                                                                                                                                                                                          | `withr::with_dir()` into basketball/iceland |
| `hsi`                | Runs HSÍ div1 + div2 + playoffs (both sexes) + cup (male only) scrapers, then process_data. Playoff/cup scrapers wrapped in `tryCatch` — they `stop()` off-season when the tournament page returns fewer than 2 tables, and that must not fail the data step. | `withr::with_dir()` into handball/iceland   |
| `livesport_football` | Syncs from `../livesport-data/` repo, runs `process_data.R`                                                                                                                                                                                                   | Fallback: direct Chromote scraping          |
| `livesport_handball` | Syncs from `../livesport-data/` repo, runs `process_data.R`                                                                                                                                                                                                   | Fallback: direct Chromote scraping          |

**Livesport data flow**: `livesport-data/` CI scrapes daily → `git pull` locally → `step_data` copies CSVs to `data/male/{league}/{year}/results.csv` + `data/male/{league}/schedule.csv` → runs `process_data.R` to combine.

### `step_fit.R` — 3 handlers

All three use `source()` + `new.env()` to load fitting modules (not `box::use()` — box resolves
relative paths from the calling file's location, not the runtime cwd set by `withr::with_dir()`).

- `shared`: `source(file.path(sports_dir, "R/shared/model_fitting.R"))` → `fit_model(config, sex, end_date)`
- `football`: `withr::with_dir()` into league dir → `source(here("R/common/model_fitting.R"))` → `fit_football_model(sex)`
- `handball_other`: `withr::with_dir()` into `handball/other/` → `source(here("R/utils/model_fitting.R"))` → `fit_model(country, sex)`

Uses `quiet_here()` helper to suppress noisy `here::i_am()` output.

### `step_bet.R`

- Reads per-league `config/bets.yml` via `yaml::read_yaml()`
- Calls `R/bets/run.R::run_betting_pipeline(cfg)`
- Skips leagues with `has_bets: false`
- See `sports-betting.md` rule for bets.yml schema

## Schedule integration (`--active` flag)

1. `R/schedule/scan.R` maintains a registry of sport→schedule file mappings (21 entries at last count — grep `^\s*[a-z_]+_[a-z_]+\s*=\s*list\(` to verify)
2. Each entry has `date_col` and optional `lengjan_key` for odds scraping
3. `check_schedules.R` scans schedules → prints summary → writes `active_competitions.json` to `lengjan-odds/config/`
4. `run.R --active` calls `schedule_to_league_keys()` to filter to only leagues with upcoming games

## `run.R` CLI flags

```
Selectors (pick one): --sport, --country, --league, --all, --active, --stale, --due
Steps:    --step data,fit,results,bet,settle  (default: all five)
Modifiers: --stale (leagues with upcoming odds + stale/missing fit),
           --due   (unbetted matches today/tomorrow + fit >48h old; auto-selects data,fit,results,bet)
Overrides: --sex male|female, --iter <n>, --dry-run, --no-plots, --sync (auto for data/settle),
           --method sample|pathfinder|variational (pathfinder/variational crash on current models — use sample)
```

## Key gotchas

- **`.here` files**: Every league directory needs a `.here` marker — without it, `here::here()` resolves to `Sports/` (the nearest ancestor with `.here`)
- **`withr::with_dir()` return values**: Use return values, not `<<-`. Inside `with_dir()` at global level, `<<-` can't find global variables
- **UTF-8 in YAML**: R's `yaml::write_yaml()` escapes non-ASCII. Write YAML with Icelandic text via Python or `writeLines()`. Set `Sys.setlocale("LC_ALL", "is_IS.UTF-8")` before reading
- **Detached launch for long runs**: A forked skill's `run_in_background: true` Bash call can silently die mid-scrape (observed 2026-04-24: CCD fork killed the pipeline at step 3/21 during HSÍ handball scrape, no error surfaced, log frozen). Launch long runs with `nohup Rscript run.R … > "$LOG" 2>&1 & disown` so the R process is detached from the session. Poll the log for `Pipeline complete` or `Error` — user-level `ps` may not see a sandboxed child.
- **Cross-league fit parallelism**: `--fit-parallel N` (default 1 = serial) dispatches N fits concurrently via `future::multisession`. Each fit still runs 4 chains internally, so N=2 on a 10-core machine uses 8 cores. Gated behind the flag; default path is bit-identical to the pre-flag serial loop. Implementation: `R/shared/fit_parallel.R`. Only the fit phase is parallelised — data/results/bet stay sequential.
- **Timing cache as planning input**: `config/timing_cache.json` records each step's prior duration. Before kicking off a large run, `jq 'to_entries | map(select(.key | startswith("fit_"))) | map(.value) | add / 60'` gives a realistic total-minutes estimate for fitting.
