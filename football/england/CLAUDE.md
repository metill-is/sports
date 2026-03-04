# football_england — CLAUDE.md

Bayesian predictions and betting automation for English football (4 divisions). Uses a diagonal-inflated bivariate Poisson model with correlation to predict match outcomes, then compares predictions against Lengjan bookmaker odds via Kelly criterion optimisation.

## Directory structure

```
football_england/
├── football_england.Rproj
├── config/
│   └── bets.yml                     # Bankroll parameters + dedup config
├── R/
│   ├── common/
│   │   ├── download_data.R          # Historical data downloader
│   │   ├── lengjan_info.R           # Lengjan competition IDs per league
│   │   ├── leagues.R                # League definitions
│   │   ├── model_fitting.R          # Stan model compilation & MCMC
│   │   ├── prep_data.R              # Data preparation for Stan
│   │   ├── process_data.R           # Historical data aggregation
│   │   ├── scrape_1x2.R             # Thin wrapper → shared R/lengjan/ modules
│   │   └── get_model_results.R      # Posterior extraction & figures
│   ├── bets/                        # Unified betting pipeline (box::use modules)
│   │   ├── kelly.R                  # get_kelly() + format_bet_text()
│   │   ├── market_1x2.R             # 1x2 market: posterior summary → Kelly
│   │   ├── market_handicap.R        # Handicap: parse_handicap + many-to-many join
│   │   ├── market_totals.R          # Over/under: many-to-many join with limit lines
│   │   └── output.R                 # Display pivot, clipboard, GSheets dedup
│   ├── run_bets.R                   # Entry point: all 3 markets (replaces check_odds_lengjan.R)
│   ├── get_odds.R                   # Main odds scraper (all 3 market types)
│   ├── check_odds.R                 # Kelly criterion vs Google Sheets odds (legacy)
│   ├── check_odds_lengjan.R         # Kelly criterion vs scraped Lengjan odds (legacy, 1x2 only)
│   ├── update_model.R               # Fit model & generate results
│   ├── lengjan -> ../../R/lengjan/  # Symlink to shared scraping modules
│   └── ...
├── Stan/
│   └── bivariate_poisson_inflated_diagonal_corrmodel.stan  # Current model
├── data/
│   ├── england/team_names.csv       # Lengjan name → model name mapping
│   ├── male/{league}/{year}/results.csv  # Historical match results
│   ├── odds.csv                     # Scraped 1x2 odds (output)
│   ├── odds_handicap.csv            # Scraped handicap odds (output)
│   └── odds_totals.csv              # Scraped totals odds (output)
└── results/
    └── male/
        ├── fit.rds                  # Fitted model (~2GB)
        ├── posterior_goals.csv      # MCMC draws for predictions
        └── figures/                 # PNG plots
```

## Commands

```bash
# From football_england/ directory:

# Unified betting pipeline — all 3 markets (1x2, handicap, totals)
Rscript R/run_bets.R              # use existing odds CSVs
Rscript R/run_bets.R --scrape     # scrape fresh odds first, then run

# Scrape all Lengjan odds (1x2 + handicap + totals, all 4 leagues)
Rscript R/get_odds.R

# Fit model and generate predictions
Rscript -e 'source("R/update_model.R")'

# Legacy: Kelly vs scraped Lengjan odds (1x2 only)
Rscript -e 'source("R/check_odds_lengjan.R")'

# Legacy: Kelly vs Google Sheets odds (includes EpicBet/CoolBet)
Rscript -e 'source("R/check_odds.R")'
```

## Leagues

| League | Lengjan competition ID | Division |
|---|---|---|
| Premier League | 296 | 1 |
| Championship | 291 | 2 |
| League 1 | 294 | 3 |
| League 2 | 295 | 4 |

Configured in `R/common/lengjan_info.R`.

## Odds scraping pipeline

`R/get_odds.R` orchestrates a two-stage scrape per league:

1. **Stage 1** (`scrape_competition`) — Loads competition list page, extracts:
   - 1x2 odds (home/draw/away) from `aria-label` on odds buttons
   - Match detail URLs for Stage 2
   - Team names and dates

2. **Stage 2** (`scrape_match_detail`) — For each match, loads detail page:
   - Appends `&marketTab=allMarkets` to URL
   - Clicks expand buttons via `aria-controls="row-OU_FT"` (totals) and `"row-HC_FT"` (handicap)
   - Parses `<table>` elements: line values from `<th>`, odds from `.h7cub57 p`

3. **Post-processing** — Standardises team names via `data/england/team_names.csv` (inner join drops unknown teams)

Output CSVs:
- `data/odds.csv` — columns: `date, country, league, home, away, o_home, o_draw, o_away`
- `data/odds_handicap.csv` — columns: `date, country, league, home, away, change, o_home, o_draw, o_away`
- `data/odds_totals.csv` — columns: `date, country, league, home, away, limit, o_over, o_under`

## Shared scraping modules

The `R/lengjan/` symlink points to `Sports/R/lengjan/`, which contains the shared Lengjan scraping code used across sports. If CSS selectors break after a Lengjan deploy, update `Sports/R/lengjan/scrape.R` — all sports benefit from the fix.

## Team name mapping

`data/england/team_names.csv` maps Lengjan's team names (`in` column) to the project's standardised names (`out` column). When a new team appears on Lengjan (e.g., promoted clubs), add a row to this file. The `inner_join` in `get_odds.R` silently drops unrecognised teams.

## Betting workflow

**Recommended (unified pipeline):**

1. Ensure model is fitted (`source("R/update_model.R")`)
2. Run `Rscript R/run_bets.R --scrape` (scrapes odds + finds value bets across all 3 markets)
3. Output: console table per market + clipboard rows for Google Sheets bet tracker

**How `run_bets.R` works:**

1. Reads bankroll config from `config/bets.yml` (kelly_frac, pool size, min bet)
2. Optionally scrapes fresh odds via `R/get_odds.R` (`--scrape` flag)
3. Loads posterior draws from `results/male/posterior_goals.csv`
4. Runs 3 market modules: 1x2 (`R/bets/market_1x2.R`), handicap (`R/bets/market_handicap.R`), totals (`R/bets/market_totals.R`)
5. Each module: joins posterior with odds → two-pass Kelly optimisation → format + filter
6. Optionally deduplicates against existing bets in Google Sheets (if `use_gsheets: true` in config)
7. Prints results + copies clipboard-ready rows

**Handicap line types:** The handicap module detects whole-goal vs fractional lines automatically:
- **Whole-goal** (±1, ±2, ...): European 3-way handicap — draw-after-handicap is a bettable outcome with its own odds, treated like 1x2
- **Fractional** (±0.5, ±1.5, ...): Asian handicap — no draw possible, purely 2-way
- All current Lengjan handicap lines are whole-goal (European 3-way)

**Bankroll config** (`config/bets.yml`): edit `cur_pool` after each bankroll change. Set `use_gsheets: true` to enable deduplication against the Bets_Lengjan sheet.

**Legacy scripts** (`check_odds_lengjan.R`, `check_odds.R`): still functional, kept for reference. Use `run_bets.R` instead.

### Module architecture (`R/bets/`)

All modules use `box::use()` imports:

| Module | Exports | Key logic |
|---|---|---|
| `kelly.R` | `get_kelly()`, `format_bet_text()` | Constrained Kelly optimisation via nloptr; EV + bet amount formatting |
| `market_1x2.R` | `run_1x2(post, cfg)` | Posterior summary → odds join → two-pass Kelly |
| `market_handicap.R` | `run_handicap(post, cfg)` | Splits by line type: whole-goal → European 3-way (home/draw/away), fractional → Asian 2-way. `parse_handicap("0-1" → -1)` → many-to-many join → Kelly |
| `market_totals.R` | `run_totals(post, cfg)` | Many-to-many join with limit lines → Kelly |
| `output.R` | `print_market()`, `make_clipboard_rows()`, `write_to_clipboard()`, `load_existing_bets()`, `dedup()` | Display pivot, clipboard formatting, optional GSheets dedup |

## Stan model

`bivariate_poisson_inflated_diagonal_corrmodel.stan`:
- Diagonal-inflated bivariate Poisson likelihood (accounts for draw overrepresentation)
- Time-varying offensive/defensive parameters per team via random walk
- Home advantage effects
- Covers seasons 2021+ across all 4 divisions + FA Cup + EFL Cup
- 4 chains, 1000 warmup, 1000 sampling iterations
