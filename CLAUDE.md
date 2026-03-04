# lengjan-odds — CLAUDE.md

Standalone scraper for betting odds from Lengjan (games.lotto.is). Uses {targets} for pipeline management and GitHub Actions for scheduled runs.

## Architecture

```
_targets.R          # Pipeline: config → per-sport scrape → accumulate
R/
  parse.R           # Icelandic date parsing
  scrape.R          # Two-stage browser scraper (1x2 + handicap/totals)
  pipeline.R        # Pipeline functions (load_competitions, scrape_sport, accumulate_sport_odds)
config/
  competitions.yml  # Sport/league IDs for Lengjan URLs
  team_names_*.csv  # Lengjan name → standardised name mappings (per sport, optional)
data/
  football_england/ # Accumulated odds CSVs (committed to git)
  football_italy/
  football_spain/
```

## Pipeline

```
config (competitions.yml)
  ├── odds_football_england (always runs) → data_football_england (skips if unchanged)
  ├── odds_football_italy   (always runs) → data_football_italy
  └── odds_football_spain   (always runs) → data_football_spain
```

- Per-sport targets created dynamically via `tar_target_raw()` + `substitute()` from `competitions.yml` keys
- Each sport scrapes independently — failures in one don't block others
- `scrape_sport()` always runs (`tar_cue(mode = "always")`) because odds change, not code
- `accumulate_sport_odds()` reads existing CSVs, appends new rows with `scraped_at` timestamp, deduplicates on match identity + odds values, writes back
- Team name standardisation is optional: sports with `team_names` config use left_join mapping, others keep Lengjan names as-is

## Commands

```bash
Rscript -e 'targets::tar_make()'        # Run full pipeline (all sports)
Rscript -e 'targets::tar_visnetwork()'  # Visualise pipeline DAG
```

## Adding a new sport

1. Add entry to `config/competitions.yml`:
   ```yaml
   sport_key:
     sport: <id>          # 1=Football, 2=Basketball, 6=Handball
     country: "<code>"
     team_names: "team_names_sport_key.csv"  # optional
     leagues:
       League Name:
         competition: "<id>"
   ```
2. Optionally create `config/team_names_<sport_key>.csv` with `out, in` columns
3. Create `data/<sport_key>/` directory
4. Run pipeline — new targets are created automatically

## CSS Selector Fragility

Lengjan uses hashed CSS class names (CSS Modules). These break when Lengjan deploys.

Update `selectors` list in `R/scrape.R` by inspecting the DOM:
- `lj1n6v0` = match container, `lj1n6v1` = match link
- `lj1n6v9` = teams, `lj1n6vd` = odds list, `uazl1c1` = odds button
- `zh0raz0` = market section, `h7cub57` = odds value

The `aria-controls` IDs (`row-OU_FT`, `row-HC_FT`) are more stable.

## Data Format

CSVs in `data/<sport_key>/`:
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
