# Sports Workspace — CLAUDE.md

Bayesian sports prediction and automated betting system. 18 leagues configured (3 active Icelandic, 15 paused). All user-facing content is in Icelandic.

> **Current focus (2026-04-10):** Non-Icelandic leagues paused (`active: false` in `leagues.yml`). Active leagues: `basketball_iceland`, `handball_iceland`, `football_iceland`. CI scraping paused for non-Icelandic leagues in both `lengjan-odds` and `livesport-data`.

## Sub-projects

| Directory         | Purpose                                                     | Git repo                   | Key tech             |
| ----------------- | ----------------------------------------------------------- | -------------------------- | -------------------- |
| `Sports/`         | Bayesian sports predictions — `{sport}/{country}/` layout   | `metill-is/sports`         | Stan, cmdstanr, box  |
| `lengjan-odds/`   | Lengjan odds scraper ({targets} pipeline, 3× daily CI)      | `metill-is/lengjan-odds`   | R, targets, chromote |
| `livesport-data/` | Livesport match data scraper ({targets} pipeline, daily CI) | `metill-is/livesport-data` | R, targets, chromote |
| `lengjan-bets/`   | Lengjan bet placement (reads recommendations, places bets)  | `metill-is/lengjan-bets`   | R, chromote          |

> **Note:** The Raycast extension at `~/Raycast/` consumes Sports pipeline JSON endpoints (`R/status/`). See `~/Raycast/CLAUDE.md` for its architecture.

Each sub-project has its own `CLAUDE.md` with detailed architecture, commands, and conventions. **Always read the relevant sub-project's CLAUDE.md before making changes.**

## Cross-project data flow

```
livesport-data/ ──daily CI scrape──→ git commit → Sports/ (git pull + step_data sync)
lengjan-odds/ ──3× daily CI scrape──→ git commit → Sports/ (raw GitHub URLs or git pull)
# Icelandic league data scraped live from federation sites (HSÍ/KKÍ/KSÍ) during data step — NOT from livesport-data
Sports/ ──recommendations.csv──→ lengjan-bets/ ──bets_log.csv──→ Sports/
Sports/R/status/ ──JSON endpoints──→ ~/Raycast/ (menu bar monitor, 15-min polling)
~/Raycast/ ──gh workflow run──→ livesport-data/ + lengjan-odds/ (CI triggers)
Sports/football/iceland/R/export_website_data.R ──JSON──→ ~/metill-platform/data/ithrottir/ (via metill-platform/scripts/export_ithrottir.py)
```

> **Key rule:** Sports pipeline writes `recommendations.csv` but NEVER writes to `bets_log.csv`. Bet placement and ledger writes are the exclusive responsibility of `lengjan-bets/`.

## Cross-project conventions

### R code style

See `.claude/rules/r-conventions.md` for full details. Key points: `box::use()`, `here::here()`, base pipe `|>`, `theme_metill()`, Icelandic locale + variable names.

### Data formats

- CSV for raw input data
- Parquet (`arrow`) for model outputs and processed data

### Stan models

See `.claude/rules/stan-conventions.md` for full details. Key points: `cmdstanr` (not rstan), non-centred parameterisations, `generated quantities` for posterior predictive checks.

### Dependencies

- No `renv` — packages installed manually
- `metill` package installed from GitHub: `remotes::install_github("bgautijonsson/metill")`
- CmdStan must be installed for any Stan model work

### Package propagation

When modifying `~/metill-package/`, changes propagate to other projects only after:

1. Push to GitHub
2. `remotes::install_github("bgautijonsson/metill")` in each project
3. (Or `devtools::load_all()` for local testing)

## Quick-reference commands

```bash
# Sports (unified pipeline)
cd Sports && Rscript run.R --all --dry-run                     # Preview all leagues
cd Sports && Rscript run.R --active --step data,fit,results,bet # Active leagues only
cd Sports && Rscript run.R --sport handball --step bet          # Bet all handball
cd Sports && Rscript run.R --league football_england --step bet # Bet one league
cd Sports && Rscript run.R --all --step fit --iter 200          # Quick test fit
cd Sports && Rscript run.R --stale --dry-run                    # Preview stale leagues
cd Sports && Rscript run.R --due                                # Auto-run due leagues
```

### Launching long-running pipelines

Full `--active` runs take ~2h (basketball fits dominate at ~24 min each × 2 sexes). Two rules:

1. **Prefer `--due` over `--active` for casual refreshes.** `--due` skips leagues whose fit is <48h old and whose next match is not today/tomorrow — often half the wall-clock or less. Reserve `--active` for "full refresh" asks.
2. **Detach the process so it survives the session.** A plain `run_in_background: true` Bash call inside a forked skill context can silently die mid-scrape (observed: CCD fork killed the pipeline at step 3/21 during the HSÍ scrape, no error surfaced). Use:

   ```bash
   cd Sports && \
     LOG=/tmp/sports-update-$(date +%Y%m%d-%H%M%S).log && \
     nohup Rscript run.R --active --step data,fit,results,bet --sync > "$LOG" 2>&1 & \
     disown && echo "PID $! → $LOG"
   ```

   Poll the log for `Pipeline complete` or `Error`. Do not trust user-level `ps` to confirm liveness — a sandboxed fork's child can be hidden from it.

## Skills

| Skill            | Purpose                                                                       |
| ---------------- | ----------------------------------------------------------------------------- |
| `/bet`           | Run betting pipeline for active or specified leagues                          |
| `/sports-update` | Full pipeline: data + fit + results + bet                                     |
| `/add-league`    | Add a new league to the pipeline                                              |
| `/place-bets`    | Preview pending bets and place them on Lengjan (two-step: preview then place) |

All four skills are **model-invocable via natural language** (`disable-model-invocation` is intentionally unset). Asking "run the pipeline for handball", "place bets", "add a league for X", etc. should trigger the relevant skill through its description — no slash prefix required. Do not add `disable-model-invocation: true` during audits.

## Rules (`.claude/rules/`)

| Rule                  | Loaded when                                  | Content                                   |
| --------------------- | -------------------------------------------- | ----------------------------------------- |
| `r-conventions.md`    | Any `*.R` file                               | R code style                              |
| `stan-conventions.md` | Any `*.stan` file                            | Stan conventions                          |
| `sports-per-sport.md` | Any `Sports/**` file                         | Per-sport data sources and quirks         |
| `sports-pipeline.md`  | `Sports/R/pipeline/**`, `run.R`, `config/**` | Pipeline architecture, leagues.yml schema |
| `sports-betting.md`   | `Sports/R/bets/**`, `**/bets.yml`            | Betting conventions, bets.yml schema      |

## Environment requirements

- macOS, Homebrew, zsh
- R (≥ 4.0) with tidyverse, cmdstanr, arrow, box, here, gt, gtExtras, ggiraph, plotly
- CmdStan (for Bayesian models)
- Git + GitHub CLI (`gh`)

## Obsidian Output

Vault: `Metill` (MCP) / `~/Obsidian/Metill/` (direct path). Prefer MCP `write_note` over the `Write` tool. This is a consolidated vault — ESB and Althingi content also lives here.
Handoff: `Sports/Sports Handoff.md`

### Navigation

1. Read `index.md` at vault root — unified content catalogue
2. Read the project-specific handoff (see `Handoff:` above) — current state
3. Read the relevant `_MOC.md` for the Knowledge/ topic

### Relevant Knowledge topics

| Topic folder                      | Content                                            |
| --------------------------------- | -------------------------------------------------- |
| `Knowledge/Betting Optimisation/` | Kelly criterion, calibration, placement rules, PnL |
| `Knowledge/Sports Models/`        | Bayesian model theory, Stan implementation, goals  |
| `Knowledge/Lengjan Pipeline/`     | Odds scraping, schedule-aware filtering            |
| `Knowledge/Livesport Data/`       | Match data scraping, CI pipeline                   |

Each topic has a `_MOC.md` entry point. **Read it first**, then selectively load sub-documents.

### Wiki operations

After completing work, update `Handoff.md`, append to `log.md`, and update `index.md` if structure changed. Reusable findings should be filed to Knowledge/ topics, not just Sessions/. See `Vault Guide.md` for full Ingest/Query/Lint workflows.

### Conventions

- Naming: kebab-case for files, Title Case for topic folders
- Use wikilinks within this vault. Do NOT wikilink across vaults.
- Session logs: `YYYY-MM-DD — Topic.md` in `Sessions/`
- New knowledge: create in `Knowledge/{Topic}/`

## Things 3

Route actionable tasks to the **Metill.is** area (ID: `4WyyavEFjCPunRi9iD5tKe`).

| Sub-project    | Things project name |
| -------------- | ------------------- |
| Sports         | Sports              |
| lengjan-odds   | Sports              |
| livesport-data | Sports              |
| lengjan-bets   | Sports              |
