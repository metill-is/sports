# livesport-data — CLAUDE.md

Daily scraper for match results and schedules from livesport.com. Runs on GitHub Actions CI, commits data to git. Consumed by the Sports betting pipeline via local clone.

## Architecture

```
R/check_schedules.R      → config/active_competitions.json (pre-filter)
config/competitions.yml  → R/pipeline.R → per-competition targets
                           R/scrape.R   → headless Chrome via rvest::read_html_live()
                                        → data/{sport}/{country}/{sex}/{league}/
```

## Two-tier scraping

Leagues are split into **daily** (top divisions with betting value) and **historical** (lower divisions for promoted-team context). Controlled by `LIVESPORT_MODE` env var.

| Mode | Env var | Scrapes | Use case |
|------|---------|---------|----------|
| Daily (default) | `LIVESPORT_MODE=daily` or unset | `leagues` / `divisions` only | CI cron, in-season |
| Full | `LIVESPORT_MODE=full` | + `historical_leagues` / `historical_divisions` | Pre-season refresh |

**Rationale**: Lower divisions (e.g., English League One/Two, handball div2) provide historical context for promoted teams but don't need daily updates. Scrape them once pre-season, then daily CI only tracks top divisions.

### What's in each tier

**Football daily**: Premier League + Championship (ENG), Serie A + B (ITA), LaLiga + LaLiga2 (ESP)
**Football historical**: League One, League Two, FA Cup, EFL Cup (ENG only)
**Handball daily**: div1 for all 12 countries (both sexes where available)
**Handball historical**: div2 for 7 countries (DK, FR, DE, NO, PL, ES, SE)

**Removed entirely**: Norway football (not in Sports pipeline), Italy Serie C + cups (no crossover with top 2), Spain lower tiers + cups (broken pipeline, no crossover)

## Data layout

```
data/
  soccer/{country}/male/{league}/results.csv     # current season
  soccer/{country}/male/{league}/schedule.csv
  handball/{country}/{sex}/{division}/results.csv
  handball/{country}/{sex}/{division}/schedule.csv
```

## Coverage

**Football** (3 countries): england, italy, spain
**Handball** (12 countries): austria, czech-republic, denmark, finland, france, germany, hungary, norway, poland, portugal, spain, sweden

**Not covered** (use federation APIs): basketball/iceland, handball/iceland, football/iceland

## Commands

```bash
Rscript -e 'targets::tar_make()'                              # Daily mode (default)
LIVESPORT_MODE=full Rscript -e 'targets::tar_make()'          # Full mode (preseason)
Rscript -e 'targets::tar_visnetwork()'                        # Visualise DAG
```

## Rate limiting

Generous delays to prevent Chromote timeouts:
- 10-13s after page load (JS rendering)
- 5-7s between pagination clicks
- 5-8s between pages
- Daily mode: ~40 pages, ~15 min runtime
- Full mode: ~66 pages, ~25 min runtime

## CSS selectors

Two selector sets (configured per competition):

| Element | Football | Handball |
|---|---|---|
| Home team | `.event__homeParticipant strong/span` | `.event__participant--home` |
| Away team | `.event__awayParticipant strong/span` | `.event__participant--away` |
| Scores | `.event__score--home/away` | same |
| Date | `.event__time` | same |
| Pagination | `.wclButtonLink` or `.event__more` | `.event__more` |

## Consuming from Sports pipeline

The Sports pipeline reads from `../livesport-data/data/` automatically when the directory exists. To update:

```bash
cd ~/Metill/livesport-data && git pull
cd ~/Metill/Sports && Rscript run.R --all --step data
```

Falls back to direct Chromote scraping if livesport-data is not cloned.

## Adding a competition

1. Add entry to `config/competitions.yml` (football: flat `leagues` list; handball: nested `divisions`)
2. For lower divisions, use `historical_leagues` / `historical_divisions` instead
3. Pipeline auto-creates targets and data directories
4. Update Sports `config/leagues.yml` if the league needs model fitting

## Schedule-aware filtering

`R/check_schedules.R` scans existing `schedule.csv` files and writes `config/active_competitions.json`. A competition is **active** if it has scheduled matches within the next 14 days (configurable via `--lookahead N`).

CI runs this before `tar_make()`. Cold-start safe: if no schedule files exist, the JSON is not written and all competitions are scraped.

```bash
Rscript R/check_schedules.R              # Default 14-day lookahead
Rscript R/check_schedules.R --lookahead 7 # Custom lookahead
```

## Dependencies

R packages: dplyr, glue, here, jsonlite, readr, rvest, targets, yaml
Browser: Chrome/Chromium (auto-discovered by rvest, set `CHROMOTE_CHROME` on CI)
