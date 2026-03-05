# lengjan-bets — Automated Bet Placement on Lengjan

## What this does

Reads pending bet recommendations from the Sports pipeline (`bets_log.csv`)
and places them on Lengjan (games.lotto.is) using headless browser automation
via `chromote::ChromoteSession`.

## Architecture

```
Sports/*/history/bets_log.csv   (pipeline output)
         |
    R/pipeline.R                (reads pending bets, resolves match IDs)
         |
    R/login.R                   (authenticates to Lengjan via ChromoteSession)
         |
    R/navigate.R                (finds matches on competition pages, extracts match IDs)
         |
    R/place_bet.R               (navigates to odds, enters stake, clicks Kaupa)
```

## Usage

```bash
# Dry run — navigates and selects but doesn't click "Kaupa"
Rscript run.R

# Live with per-bet confirmation prompts
Rscript run.R --live

# Live, no prompts (fully automatic)
Rscript run.R --live --no-confirm

# Specific league
Rscript run.R --live --league football_england
```

## Dependencies

- `chromote` (browser automation via Chrome DevTools Protocol)
- `dplyr`, `readr`, `purrr` (data wrangling)
- `cli` (logging)
- `yaml` (config parsing)
- `jsonlite` (JS result parsing)
- `here`, `box` (project structure)

## Credentials

Set in `.Renviron` (gitignored):
```
LENGJAN_USER=your_username
LENGJAN_PASS=your_password
```

## Key files

| File | Purpose |
|------|---------|
| `run.R` | CLI entry point |
| `R/pipeline.R` | Main pipeline: load bets, resolve IDs, orchestrate placement |
| `R/login.R` | Authenticate to Lengjan (click "Minar sidur", fill form, click "Innskra") |
| `R/navigate.R` | Navigate to competitions, extract match listings + IDs via JS |
| `R/place_bet.R` | Place individual bets (outcome, handicap, totals) via CDP clicks |

## Selector strategy

Uses **JavaScript-based DOM traversal** with text content matching rather than
hashed CSS classes. Key stable anchors:

- Section labels: "Urslit"/"Urslit leiksins", "Yfir eda undir", "Forgjof" (startsWith matching)
- Odds: `aria-label` with "studull: N.NN" (football) or inner `<p>` text (handball)
- Line labels: `<th>` text content in table rows ("59.5", "2.5", "1-0")
- Buy button: button containing "Kaupa"
- Stake input: visible `<input>` with numeric value

CDP `Input.dispatchMouseEvent` used for clicks (trusted events for React).

## Safety features

- **Dry run by default** — must pass `--live` to actually place bets
- **Per-bet confirmation** — interactive mode prompts before each bet
- **Odds verification** — compares pipeline odds with Lengjan odds, aborts if they differ by more than 5%
- **Rate limiting** — randomised delays between page loads (2-4s) and actions (0.5-1.5s)
- **Match-level isolation** — one bet failing doesn't block others
- **Browser always visible** — `headless = FALSE` so user can monitor

## Current status (2026-03-05)

**Working**: Login, match extraction, team name mapping, market detection, odds verification.

**Blocker**: Bet slip doesn't appear after clicking odds buttons. JS-dispatched click events
are untrusted; CDP `Input.dispatchMouseEvent` (`cdp_click()`) is scaffolded but untested.

See `~/Obsidian/Metill/Sports/lengjan-bet-placement.md` for detailed implementation notes.

## Team name mapping

Uses `lengjan-odds/config/team_names_*.csv` files:
- `out` column = pipeline/standardised name
- `in` column = Lengjan display name
- Lookup direction: Lengjan name -> pipeline name (for matching extracted matches to bets)

## Sport-specific differences

| Feature | Football | Handball |
|---------|----------|----------|
| Outcome section | "Urslit" | "Urslit leiksins" |
| Odds aria-label | "1, studull: 2.28" | (none) |
| Button text | "12.28" (prefix + odds) | "11.69" (prefix + odds) |
| Totals lines | 0.5, 1.5, 2.5, 3.5 | 55.5, 59.5, 60.5 |

## Relationship to other projects

- **lengjan-odds/** — reads odds FROM Lengjan (scraper). This project writes bets TO Lengjan.
- **Sports/** — generates bet recommendations in `bets_log.csv`. This project consumes them.
- Uses `competitions.yml` and `team_names_*.csv` from `lengjan-odds/config/`.
