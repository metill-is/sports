# livesport-data — CLAUDE.md

Daily scraper for match results and schedules from livesport.com. Runs on GitHub Actions CI, commits data to git. Consumed by the Sports betting pipeline via local clone.

## Architecture

```
config/competitions.yml  → R/pipeline.R → per-competition targets
                           R/scrape.R   → headless Chrome via rvest::read_html_live()
                                        → data/{sport}/{country}/{sex}/{league}/
```

## Data layout

```
data/
  soccer/{country}/male/{league}/results.csv     # current season
  soccer/{country}/male/{league}/schedule.csv
  handball/{country}/{sex}/{division}/results.csv
  handball/{country}/{sex}/{division}/schedule.csv
```

## Coverage

**Football** (3 countries, ~23 leagues): england, italy, spain, norway
**Handball** (12 countries, ~34 divisions): austria, czech-republic, denmark, finland, france, germany, hungary, norway, poland, portugal, spain, sweden

**Not covered** (use federation APIs): basketball/iceland, handball/iceland, football/iceland

## Commands

```bash
Rscript -e 'targets::tar_make()'        # Run full pipeline
Rscript -e 'targets::tar_visnetwork()'  # Visualise DAG
```

## Rate limiting

Generous delays to prevent Chromote timeouts:
- 10-13s after page load (JS rendering)
- 5-7s between pagination clicks
- 5-8s between pages
- ~136 pages total, ~35 minutes runtime

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
2. Pipeline auto-creates targets and data directories
3. Update Sports `config/leagues.yml` if the league needs model fitting

## Schedule-aware filtering

Optional: place `config/active_competitions.json` (same format as lengjan-odds) to skip off-season competitions.

## Dependencies

R packages: dplyr, glue, here, jsonlite, readr, rvest, targets, yaml
Browser: Chrome/Chromium (auto-discovered by rvest, set `CHROMOTE_CHROME` on CI)
