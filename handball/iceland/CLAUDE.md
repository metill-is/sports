# Handball Iceland

Bayesian handball prediction model for Icelandic leagues (male + female). Uses a multivariate Student's t model with time-varying team strengths. Same Stan model as basketball.

## Quick Reference

```bash
cd Sports/handball/iceland

# Full update: scrape data + fit model + generate results (both sexes)
Rscript R/update_results.R

# Model only for one sex (assumes data already scraped)
Rscript R/update_model.R          # Default: female only — edit script for male

# Betting recommendations (requires fresh posterior + odds in GSheets)
Rscript R/run_bets.R
```

## Update Workflow

### Step 1: Download Data

Data is scraped from the HSI website (Icelandic Handball Federation). The full update script `R/update_results.R` handles this automatically, but you can also run the scrapers individually:

```r
# Male
source("R/utils/male/download_newest_data_div1.R")
source("R/utils/male/download_newest_data_div2.R")
source("R/utils/male/download_newest_data_cup.R")   # Optional
source("R/utils/male/process_data.R")

# Female
source("R/utils/female/download_newest_data_div1.R")
source("R/utils/female/download_newest_data_div2.R")
source("R/utils/female/process_data.R")
```

**Output**: `data/{sex}/data.csv` (combined results) and `data/{sex}/schedule.csv` (upcoming fixtures).

**HSI URLs** (hardcoded, must update each season):

| Sex | Division | URL | File |
|-----|----------|-----|------|
| Male | Olís deild | `hsi.is/olis-deild-karla-2025-26` | `R/utils/male/download_newest_data_div1.R` |
| Male | Grill 66 deild | `hsi.is/grill-66-deild-karla-2025-26` | `R/utils/male/download_newest_data_div2.R` |
| Female | Olís deild | `hsi.is/olis-deild-kvenna-1` | `R/utils/female/download_newest_data_div1.R` |
| Female | Grill 66 deild | `hsi.is/grill-66-deild-kvenna-1` | `R/utils/female/download_newest_data_div2.R` |

The scrapers use `rvest::read_html_live()` which requires Chrome/Chromium for JavaScript rendering.

### Step 2: Fit Model

```r
source("R/update_results.R")   # Does data + model for both sexes
# OR
source("R/update_model.R")     # Model only (default: female)
```

Runs:
1. Prepares Stan data via `R/utils/prep_data.R`
2. Compiles and samples `Stan/2d_student_t.stan` (4 chains, 1000 warmup + 1000 sampling)
3. Saves `results/{sex}/{date}/fit.rds` (~500 MB)

**Time**: ~5-15 minutes per sex.

**Note**: `update_model.R` is hardcoded to female. To run male, either use `update_results.R` (runs both) or edit the sex variable in `update_model.R`.

### Step 3: Generate Results

Also handled by the update scripts — calls `generate_model_results()` after fitting:
- Writes `posterior_goals.csv` (used by betting pipeline)
- Generates 6 PNG figures in `results/{sex}/{date}/figures/`

### Step 4: Betting (Optional)

```bash
Rscript R/run_bets.R
```

Uses the shared pipeline at `Sports/R/bets/`. Config at `config/bets.yml`. Reads odds from Google Sheets (Lengjan booker only), applies Kelly criterion.

## Architecture

```
handball/iceland/
├── Stan/2d_student_t.stan          # Bayesian model (shared across sports)
├── R/
│   ├── update_results.R            # Full update: scrape + fit + results (both sexes)
│   ├── update_model.R              # Fit + results only (single sex, default female)
│   ├── run_bets.R                  # Betting wrapper → Sports/R/bets/
│   ├── get_odds_lengjan.R          # Legacy: scrape Lengjan odds
│   └── utils/
│       ├── prep_data.R             # Data → Stan format
│       ├── model_fitting.R         # Compile + sample Stan model
│       ├── get_model_results.R     # Extract posteriors, generate plots
│       ├── lengjan_info.R          # Lengjan competition IDs (sport=6, IS)
│       ├── get_1x2_lengjan.R       # Fetch Lengjan 1x2 odds
│       ├── male/
│       │   ├── download_newest_data_div1.R
│       │   ├── download_newest_data_div2.R
│       │   ├── download_newest_data_cup.R
│       │   ├── download_historical_data_div1.R
│       │   ├── download_historical_data_div2.R
│       │   └── process_data.R      # Combine div1+div2+cup → data.csv
│       └── female/
│           ├── download_newest_data_div1.R
│           ├── download_newest_data_div2.R
│           ├── download_historical_data_div1.R
│           ├── download_historical_data_div2.R
│           └── process_data.R
├── config/
│   └── bets.yml                    # Betting pipeline config
├── data/{male,female}/
│   ├── current_div1.csv, current_div2.csv   # Latest scraped results
│   ├── historical_div1.csv, historical_div2.csv
│   ├── data.csv                    # Combined (output of process_data.R)
│   └── schedule.csv                # Upcoming fixtures
├── odds/iceland/{male,female}/
│   ├── league_1.csv                # Lengjan 1x2 odds
│   └── name_table.csv              # Lengjan → model team name mapping
├── results/{male,female}/{date}/
│   ├── fit.rds                     # Fitted Stan model
│   ├── posterior_goals.csv         # Posterior draws for betting
│   └── figures/*.png               # 6 visualisation PNGs
└── history/
    └── bets_log.csv                # Betting history
```

## Betting Config

See `config/bets.yml`:
- **Has ties** (`has_ties: true`, `tie_threshold: 0.5` — continuous scores)
- **kelly_frac**: 0.20 (20% of optimal Kelly)
- **Pool**: 5862 kr (ISK)
- **Min bet**: 200 kr
- **Odds source**: Google Sheets (Lengjan booker only via `booker_include`)
- **Markets**: 1x2, handicap (European 3-way for whole-goal + Asian 2-way for fractional), totals

## Lengjan Integration

Lengjan competition IDs for Icelandic handball (defined in `R/utils/lengjan_info.R`):
- Sport ID: 6 (Handball)
- Country: IS
- Male Div 1: competition 1269
- Female Div 1: competition 1270

Note: Iceland handball is often off-season on Lengjan. When available, odds can be scraped via the shared `Sports/R/lengjan/` scraper or entered manually in GSheets.

## Known Issues

1. **Function naming**: `prepare_football_data()` and `fit_football_model()` in `R/utils/` are named after football (their origin) but work correctly for handball. Do not rename.

2. **HSI website fragility**: The web scrapers depend on HSI's HTML structure. Female data downloads sometimes fail due to website instability. If scraping fails, check the HSI URL manually — it may have changed for the new season.

3. **Season URLs**: HSI URLs contain the season year (e.g., `2025-26`) and must be updated at the start of each season (typically August/September). Check all download scripts in `R/utils/{male,female}/`.

4. **Cup tournament IDs**: Male cup data uses HSI tournament IDs that change yearly. Update the `mot_nr` list in `R/utils/male/download_newest_data_cup.R`.

5. **update_model.R defaults to female**: The simple entry point only runs female. Use `update_results.R` for both sexes, or edit `update_model.R`.

6. **No error handling**: Neither Stan sampling nor web scraping has try/catch. Network failures or model issues will crash the script.

7. **Legacy betting scripts** have been removed. Use `run_bets.R` + `config/bets.yml`.

## Dependencies

**R packages**: tidyverse, cmdstanr, posterior, rvest, here, box, metill, gt, gtExtras, ggtext, scales, googlesheets4, nloptr, clipr, glue, yaml, janitor, withr

**System**: CmdStan, R >= 4.4, Chrome/Chromium (for `read_html_live()`)
