---
name: place-bets
description: Use when placing bets on Lengjan. Previews pending bets, then places after confirmation.
argument-hint: "[--league LEAGUE] [--today] [--date YYYY-MM-DD]"
---

# /place-bets — Preview and place bets on Lengjan

Two-step workflow: preview pending bets, discuss with the user, then place. Both
scripts read from `data/decisions/recommendations/` and dedup against
`data/decisions/ledger/` automatically — recommendations already in the ledger
are never re-presented.

The placer is **local-only** — `LENGJAN_USER` / `LENGJAN_PASS` come from
`.Renviron`. Never run on CI (enforced by `test-placer-ci-isolation.R`).

## Step 1: Preview

Show what a live run would place. No browser is opened.

```bash
cd /Users/brynjolfurjonsson/sports && Rscript scripts/preview_bets.R
```

Filter to a single league, today, or a specific date:

```bash
Rscript scripts/preview_bets.R --league football_iceland
Rscript scripts/preview_bets.R --today
Rscript scripts/preview_bets.R --date 2026-04-26
```

Present the output to the user. Then ask:

- "Want me to place all of these? I can narrow by league or date, but the placer can't skip single bets."
- If there are bets on multiple dates, ask whether to place all dates or just today/tomorrow.

**Listen for adjustments:**

- "Skip #3 and #7" — `place_bets.R` has no `--exclude` flag; tell the user
  the placer cannot skip single bets (the per-bet `n` prompt never waits under
  `Rscript`; see Step 2), so narrow with `--league`/`--today`/`--date` instead.
- "Only the handball ones" — use `--league handball_iceland`.
- "Only today" — use `--today`.
- Any other changes the user mentions.

## Step 2: Place bets

Once the user confirms, run the placer. Default is dry-run; **`--live` is the
only flag that actually places money**.

Under `Rscript`, `--live` on its own places nothing: `readline()` returns "" without
waiting (non-interactive R), so the upfront `Place these bets? (y/n)` prompt reads an
empty answer and the run stops with "Cancelled by user." before login. Once the user
has confirmed the bet slip in chat, run it with `--live --no-confirm`, as in the
recipes below; the global ask rule on `place_bets.R --live` stops the command for
their approval before anything is placed.

**Default (all pending):**

```bash
Rscript scripts/place_bets.R --live --no-confirm
```

**Filtered to a league:**

```bash
Rscript scripts/place_bets.R --live --no-confirm --league football_iceland
```

**Filtered to today's matches:**

```bash
Rscript scripts/place_bets.R --live --no-confirm --today
```

**Filtered to a specific match date:**

```bash
Rscript scripts/place_bets.R --live --no-confirm --date 2026-04-26
```

**Show the browser** (adds `--show-browser`; useful when diagnosing login or DOM
issues — the placer is otherwise headless):

```bash
Rscript scripts/place_bets.R --live --no-confirm --show-browser
```

After placement completes, summarise what was placed (status `placed`) and what
was skipped or rejected (status `rejected_lower_odds`, `no_match_id`, etc.).

## P1–P4 placement invariants

The placer enforces the four placement rules from
[`.claude/rules/sports-betting.md`](../../rules/sports-betting.md):

- **P1 — idempotent:** dedup against `data/decisions/ledger/` before any
  Chromote session opens. No bet placed twice.
- **P2 — actual odds:** the ledger records the odds Lengjan actually offered at
  click time, not the odds in the recommendation file.
- **P3 — Kelly recompute:** if Lengjan's live odds drift, stake is recomputed
  from the recommended Kelly fraction against the actual odds.
- **P4 — +EV reject:** if recomputed EV ≤ 0, the bet is skipped and logged with
  status `rejected_lower_odds`.

## Error handling

- **"No leagues with team_names config"** → a league in `recommendations/` has
  no `lengjan$team_names` map in `config/leagues.yml`. Add it, then re-run.
- **"missing team_names for: <team>"** → recommendation has a team not keyed in
  `config/leagues.yml` under `lengjan.team_names.<sex>` (per-sex map; see
  `config/leagues.schema.json`).
- **"All recommendations have already been placed"** → ledger dedup ate
  everything; nothing to do.
- **Browser fails to launch / login fails** → check Chrome is installed and
  `LENGJAN_USER` / `LENGJAN_PASS` are set in `.Renviron` (template:
  `.Renviron.example`).

## Reference

- Preview script: `scripts/preview_bets.R`
- Placement script: `scripts/place_bets.R` → `place_bets()` → `R/placer-pipeline.R`
- Recommendations source: `data/decisions/recommendations/sport=*/country=*/run_date=*/`
- Ledger (canonical, Parquet): `data/decisions/ledger/`
- Placement rules: `.claude/rules/sports-betting.md`
