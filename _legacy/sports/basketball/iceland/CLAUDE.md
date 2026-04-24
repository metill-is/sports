# Basketball Iceland

Bayesian basketball prediction model for Icelandic leagues (male + female). Uses a multivariate Student's t model with time-varying team strengths.

## Quick Reference

```bash
cd Sports/basketball/iceland

# Full update: download data + fit model + generate results
Rscript -e 'source("R/prep_data_kk.R"); source("R/prep_data_kvk.R"); source("R/update_model.R")'

# Model only (assumes data already downloaded)
Rscript R/update_model.R

# Betting recommendations (requires fresh posterior + odds in GSheets)
Rscript R/run_bets.R
```

## Update Workflow

### Step 1: Download Data

```r
source("R/prep_data_kk.R")    # Male: downloads from Baskethotel API → data/male/
source("R/prep_data_kvk.R")   # Female: downloads from Baskethotel API → data/female/
```

Downloads Excel files from `widgets.baskethotel.com` for divisions 1 and 2, processes into `data/{sex}/data.csv` and `data/{sex}/schedule.csv`.

**Season IDs** (hardcoded, must update each season):

| Sex    | Division | season_id | File                |
| ------ | -------- | --------- | ------------------- |
| Male   | Div 1    | 130403    | `R/prep_data_kk.R`  |
| Male   | Div 2    | 130402    | `R/prep_data_kk.R`  |
| Female | Div 1    | 130422    | `R/prep_data_kvk.R` |
| Female | Div 2    | 130421    | `R/prep_data_kvk.R` |

To find new season IDs: go to baskethotel.com, navigate to the league, inspect the widget export URL.

### Step 2: Fit Model

```r
source("R/update_model.R")
```

Runs for both male and female:

1. Prepares Stan data via `R/common/prep_data.R`
2. Compiles and samples `Stan/2d_student_t_scalarsigma.stan` (4 chains, 1000 warmup + 1000 sampling). The tier1 `Stan/2d_student_t.stan` is kept for reference and loo comparisons.
3. Saves `results/{sex}/fit.rds` (~500+ MB)

**Time**: ~5-10 minutes per sex on modern hardware.

### Step 3: Generate Results

Also handled by `update_model.R` — calls `generate_model_results()` after fitting:

- Writes `posterior_goals.csv` (used by betting pipeline)
- Generates 7 PNG figures in `results/{sex}/{date}/figures/`

### Step 4: Betting (Optional)

```bash
Rscript R/run_bets.R
```

Uses the shared pipeline at `Sports/R/bets/`. Config at `config/bets.yml`. Reads odds from Google Sheets, applies Kelly criterion.

## Architecture

```
basketball/iceland/
├── Stan/2d_student_t_scalarsigma.stan   # Production model (scalar observation sigma)
├── Stan/2d_student_t.stan               # Tier1 variant — per-team sigma; kept for loo comparisons
├── R/
│   ├── update_model.R              # Entry point: fit + results for both sexes
│   ├── prep_data_kk.R              # Download male data from Baskethotel
│   ├── prep_data_kvk.R             # Download female data from Baskethotel
│   ├── run_bets.R                  # Betting wrapper → Sports/R/bets/
│   └── common/
│       ├── prep_data.R             # Data → Stan format
│       ├── model_fitting.R         # Compile + sample Stan model
│       └── get_model_results.R     # Extract posteriors, generate plots
├── config/
│   └── bets.yml                    # Betting pipeline config
├── data/{male,female}/
│   ├── div1/, div2/                # Raw Excel downloads from Baskethotel
│   ├── data.csv                    # Combined historical results
│   └── schedule.csv                # Upcoming fixtures
├── results/{male,female}/
│   ├── fit.rds                     # Fitted Stan model
│   ├── posterior_goals.csv         # Posterior draws for betting
│   └── figures/*.png               # 7 visualisation PNGs
└── history/
    └── bets_log.csv                # Betting history (appended each run)
```

## Stan Model

`Stan/2d_student_t_scalarsigma.stan` (production, swapped 2026-04-20) — multivariate Student's t with:

- **Time-varying** offensive/defensive strengths (random walk scaled by sqrt(rest days))
- **Hierarchical** volatility (team-specific sigma for strength _evolution_ — `sigma_off`, `sigma_def`)
- **Scalar observation noise** (`sigma`) — the per-team `sigma_team` of the tier1 variant was found unidentifiable (posterior CV 1.0 %) and collapsed to one scalar. See Metill vault `Knowledge/Sports Models/audit-stan-models-2026-04-19.md` §10.1.
- **Heavy tails** (Student's t with estimated degrees of freedom)
- **Correlated** home/away scores (match-dependent rho via 4-parameter logit)
- **Home advantage** (separate offensive/defensive components per team)
- **Season-level** mean goals trend
- **Per-match `log_lik`** in generated quantities for loo comparisons

Non-centred parameterisation for efficient sampling.

`Stan/2d_student_t.stan` (tier1 baseline) is kept for A/B comparisons and documents the pre-audit per-team-sigma parameterisation.

## Data Schema

**`data/{sex}/data.csv`**:
| Column | Type | Description |
|--------|------|-------------|
| season | int | Season year |
| date | date | Match date |
| home | chr | Home team |
| away | chr | Away team |
| home_goals | dbl | Home score |
| away_goals | dbl | Away score |
| division | int | 1 or 2 |

**`posterior_goals.csv`** (output):
| Column | Type | Description |
|--------|------|-------------|
| iteration | int | MCMC draw (1-4000) |
| game_nr | int | Match index |
| division | int | League division |
| date | date | Match date |
| home | chr | Home team |
| away | chr | Away team |
| home_goals | dbl | Simulated home score |
| away_goals | dbl | Simulated away score |

## Betting Config

See `config/bets.yml`:

- **No ties** (`has_ties: false`, `tie_threshold: 0.5`)
- **kelly_frac**: 0.04 (4% of optimal Kelly)
- **Pool**: 995 EUR
- **Odds source**: Google Sheets (manual entry, exclude Lengjan booker)
- **Markets**: 1x2, handicap (Asian only — no European 3-way), totals

## Known Issues

1. **Function naming**: `prepare_football_data()` and related functions in `R/common/` are named after football (their origin) but work correctly for basketball. Do not rename — the shared pipeline depends on these names.

2. **Season IDs**: Baskethotel API season IDs are hardcoded and change every season. Check `prep_data_kk.R` and `prep_data_kvk.R` at the start of each season.

3. **Excel repair**: `repair_excel_file_mac()` in `R/common/excel_utils.R` is commented out. If downloads come corrupted, uncomment the calls in prep_data scripts.

4. **No error handling**: Stan compilation and sampling have no try/catch. If the model fails, the script crashes.

## Dependencies

**R packages**: tidyverse, cmdstanr, posterior, readxl, here, box, metill, gt, gtExtras, ggtext, scales, googlesheets4, clipr, yaml

**System**: CmdStan, R >= 4.4
