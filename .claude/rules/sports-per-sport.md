---
paths:
  - "Sports/**"
---

# Per-sport Details

## basketball/iceland/

- **Data source**: Excel files in `data/{sex}/div1/` and `data/{sex}/div2/` (2021-2026)
- **Prep scripts**: `R/prep_data_kk.R` (male), `R/prep_data_kvk.R` (female)
- **Local modules**: `R/common/` (mirrors shared code)
- **Divisions**: Bónusdeild (BD), 1. Deild (1D)
- **No ties**: Win/loss only
- **13 team colors** defined in config

## handball/iceland/

- **Data source**: Web scrapers downloading from handball federation website
- **Download scripts**: `R/utils/{male,female}/download_newest_data_div{1,2}.R`
- **Processing**: `R/utils/{male,female}/process_data.R` combines divisions
- **Local modules**: `R/utils/`
- **Divisions**: Olís deild (OD), Grill 66 deild (G66), plus cup for male
- **Ties allowed**: 2/1/0 point system
- **Betting odds**: `odds/` directory with Lengjan integration
- **Known issue**: Female data download sometimes fails due to federation website

## handball/other/ (shared pipeline)

- **Shared code only**: `R/`, `Stan/`, `.Rproj`, `.here` — loops over all 12 countries
- **Data source**: `livesport-data/` repo (daily CI scrape) — synced by `step_data.R`, falls back to direct Chromote
- **Data dirs are symlinks**: `data/{country}` → `../../{country}/data`, same for `results/` and `odds/`
- **Metadata stays in other/**: `data/last_updated.json`, `results/betting_quantiles.csv`
- **Countries**: austria, czech-republic, denmark, finland, france, germany, hungary, norway, poland, portugal, spain, sweden
- **Odds available**: denmark, france, germany, poland, spain, sweden (6 of 12)
- **R code unchanged**: all `here("data", country, ...)` paths resolve through symlinks

## handball/{country}/ (per-country data + betting)

12 country directories under `handball/` hold actual data (no code except betting, no git repos):
- `{country}/data/{sex}/` — results.csv, schedule.csv, div1/
- `{country}/results/{sex}/` — model outputs (fit.rds, posterior_goals.csv, figures/)
- `{country}/odds/` — only for 6 countries with Lengjan coverage

**6 countries with betting pipelines** (denmark, france, germany, norway, spain, sweden):
- `.here` — project root marker for `here::here()` (critical — without this, `here()` resolves to `Sports/`)
- `config/bets.yml` — betting config (lengjan-odds source, kelly_frac=0.10, no handicap, ties with threshold 0.5)
- `R/run_bets.R` — thin wrapper → `Sports/R/bets/run[run_betting_pipeline]`
- `history/bets_log.csv` — betting history (created on first pipeline run)
- Team name mappings live in `lengjan-odds/config/team_names_handball_{country}.csv`

**6 countries without betting** (austria, czech-republic, finland, hungary, poland, portugal):
- Data and model outputs only — no Lengjan odds coverage

## football/iceland/

- **Integrated into the mono-repo** since 2026-03-04 (previously a separate `bgautijonsson/football_iceland` repo)
- **Registered in `leagues.yml`** as `football_iceland` (pipeline: `football`, data_source: `iceland_ksi`, active: true)
- **Likelihood**: No-inflation bivariate Poisson (active since 2026-04-20 evening after loo comparison); per-league `Stan/` also contains older inflated and 2D Student's t variants
- **Data source**: Web scrapers for KSI (Icelandic FA) data, covering divisions 1-5 (male), 1-3 (female), plus cups; `data_source: iceland_ksi` handler
- **Website integration**: Hand-authored `.qmd` files with local figure paths (differs from basketball/handball pattern)
- **Seasonally active**: Icelandic football season runs May-October

## football/italy/

- **Separate project** with own `.Rproj` (not integrated into shared config system)
- **Likelihood**: Diagonal-inflated bivariate Poisson
- **2 divisions**: Serie A, Serie B
- **Data source**: `livesport-data/` repo (daily CI), fallback to direct Chromote via `R/get_historical_data.R`
- **Betting pipeline**: Full Lengjan integration via shared `R/bets/` pipeline
  - `config/bets.yml` — lengjan-odds source, kelly_frac=0.10, all 3 markets (1x2, handicap, totals)
  - `R/run_bets.R` — thin wrapper → `Sports/R/bets/run[run_betting_pipeline]`
  - `history/bets_log.csv` — betting history
- **Team name mapping**: `lengjan-odds/config/team_names_football_italy.csv`
- **Posterior layout**: Direct — `results/male/posterior_goals.csv` (no date subdirs)

## football/england/

- **Separate project** with own `.Rproj` (not integrated into shared config system)
- **Likelihood**: Diagonal-inflated bivariate Poisson with correlation
- **4 divisions**: Premier League, Championship, League 1, League 2 (plus FA Cup, EFL Cup for historical data)
- **Data source**: `livesport-data/` repo (daily CI), fallback to direct Chromote. Per-league CSVs in `data/male/{league}/{year}/results.csv` (2019–present)
- **Betting pipeline**: Full Lengjan integration via shared `R/lengjan/` modules (symlinked)
- **Odds output**: `data/odds.csv` (1x2), `data/odds_handicap.csv`, `data/odds_totals.csv`
- **Betting pipeline**: Full Lengjan integration via `R/run_bets.R` → shared `R/bets/` pipeline
- **Team name mapping**: `data/england/team_names.csv` standardises Lengjan names to model names
- See `football/england/CLAUDE.md` for full details

## Shared Lengjan scraping (`R/lengjan/`)

Shared modules for scraping odds from Lengjan (games.lotto.is). Used by `handball/iceland/` (via `scrape_all.R`) and `football/england/` (via symlink).

- **`scrape.R`** — Two-stage scraper:
  - `scrape_competition()` — Loads competition list page, extracts 1x2 odds + match URLs
  - `scrape_match_detail()` — Loads match detail page, expands sections via `aria-controls`, extracts handicap + totals from rendered tables
- **`parse.R`** — Icelandic date parsing (e.g., "3. mar19:45" → `Date`)
- **`config.R`** — Competition registry mapping sport names to Lengjan URL parameters
- **CSS selectors** are hashed class names from Lengjan's CSS Modules build — may break on site deploys. Update the `selectors` list in `scrape.R` if scraping stops working.
- **Rate limiting** built in: randomised delays between pages (2–5s) and clicks (1.5–3s)
