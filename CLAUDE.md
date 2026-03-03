# lengjan-odds — CLAUDE.md

Standalone scraper for betting odds from Lengjan (games.lotto.is). Uses {targets} for pipeline management and GitHub Actions for scheduled runs.

## Architecture

```
_targets.R          # Pipeline: config → scrape → accumulate
R/
  parse.R           # Icelandic date parsing
  scrape.R          # Two-stage browser scraper (1x2 + handicap/totals)
  pipeline.R        # Pipeline functions (load_competitions, scrape_all, accumulate_odds)
config/
  competitions.yml  # Sport/league IDs for Lengjan URLs
  team_names.csv    # Lengjan name → standardised name mapping
data/
  football_england/ # Accumulated odds CSVs (committed to git)
```

## Pipeline

```
config (competitions.yml) → scrape_all (always runs) → accumulate_odds (skips if unchanged)
```

- `scrape_all` always runs (`tar_cue(mode = "always")`) because the code doesn't change, only the data
- `accumulate_odds` reads existing CSVs, appends new rows with `scraped_at` timestamp, deduplicates on match identity + odds values, writes back
- If scraped odds are identical to previous run, {targets} skips the accumulation step entirely

## Commands

```bash
Rscript -e 'targets::tar_make()'        # Run full pipeline
Rscript -e 'targets::tar_read(odds)'    # Read latest scrape result
Rscript -e 'targets::tar_visnetwork()'  # Visualise pipeline DAG
```

## CSS Selector Fragility

Lengjan uses hashed CSS class names (CSS Modules). These break when Lengjan deploys.

Update `selectors` list in `R/scrape.R` by inspecting the DOM:
- `lj1n6v0` = match container, `lj1n6v1` = match link
- `lj1n6v9` = teams, `lj1n6vd` = odds list, `uazl1c1` = odds button
- `zh0raz0` = market section, `h7cub57` = odds value

The `aria-controls` IDs (`row-OU_FT`, `row-HC_FT`) are more stable.

## Data Format

CSVs in `data/football_england/`:
- `odds_1x2.csv`: date, league, home, away, o_home, o_draw, o_away, scraped_at
- `odds_handicap.csv`: date, league, home, away, change, o_home, o_draw, o_away, scraped_at
- `odds_totals.csv`: date, league, home, away, limit, o_over, o_under, scraped_at

## Consuming from other projects

```r
odds <- readr::read_csv(
  "https://raw.githubusercontent.com/metill-is/lengjan-odds/main/data/football_england/odds_1x2.csv"
)
```

## Dependencies

R packages: dplyr, glue, here, lubridate, readr, rvest, stringr, targets, tibble, yaml

Browser: Chrome/Chromium (managed by rvest::read_html_live() internally)
