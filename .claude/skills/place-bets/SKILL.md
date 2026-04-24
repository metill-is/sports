---
name: place-bets
description: Use when placing bets on Lengjan. Previews pending bets, then places after confirmation.
argument-hint: "[--league LEAGUE]"
context: fork
---

# /place-bets — Preview and place bets on Lengjan

Two-step workflow: preview pending bets, discuss with the user, then place.

## Step 1: Preview

Run the preview script to show what a live run would place:

```bash
cd /Users/brynjolfurjonsson/sports/lengjan-bets && Rscript preview.R
```

If the user passed `--league`, forward it:

```bash
cd /Users/brynjolfurjonsson/sports/lengjan-bets && Rscript preview.R --league {LEAGUE}
```

Present the output to the user. Then ask:

- "Want me to place all of these, or do you want to skip/adjust any?"
- If there are bets on multiple dates, ask whether to place all dates or just today/tomorrow.

**Listen for adjustments:**

- "Skip #3 and #7" → build `--exclude` list
- "Only the handball ones" → build `--league` filter
- "Only today" → note the date filter
- "Change the stake on #2 to 500" → note manual override
- Any other changes the user mentions

## Step 2: Place bets

Once the user confirms, run the placement pipeline. Always start with `--live` (which enables per-bet confirmation in the browser).

**Default (all pending):**

```bash
cd /Users/brynjolfurjonsson/sports/lengjan-bets && Rscript run.R --live
```

**Filtered to a league:**

```bash
cd /Users/brynjolfurjonsson/sports/lengjan-bets && Rscript run.R --live --league {LEAGUE}
```

**Filtered to today's matches only:**

```bash
cd /Users/brynjolfurjonsson/sports/lengjan-bets && Rscript run.R --live --today
```

**Filtered to a specific match date:**

```bash
cd /Users/brynjolfurjonsson/sports/lengjan-bets && Rscript run.R --live --date YYYY-MM-DD
```

**Show the browser during placement** (adds `--show-browser`; useful when diagnosing login/DOM issues).

If the user wants no per-bet prompts:

```bash
cd /Users/brynjolfurjonsson/sports/lengjan-bets && Rscript run.R --live --no-confirm
```

**Handling exclusions:** If the user wants to skip specific bets, the current `run.R` doesn't support `--exclude`. In that case:

1. Tell the user which bets will appear in the interactive prompts
2. Remind them to answer "n" to the ones they want to skip, or "q" to stop
3. Alternatively, if they want only specific leagues, use `--league` to filter

After placement completes, summarise what was placed and what was skipped.

## Step 3: Log any manually placed bets (if needed)

If the user placed some bets manually on Lengjan (not through the automation), they can log them:

```bash
cd /Users/brynjolfurjonsson/sports/Sports && Rscript R/bets/log_placed.R {INDICES}
```

Where `{INDICES}` are the row numbers from the preview (e.g., `1,3,5` or `all`).

## Error handling

- **"No recommendations.csv found"** → Pipeline needs running: suggest `/bet --active` or `/sports-update`
- **"All recommendations have already been placed"** → Nothing to do
- **Browser fails to launch** → Check that Chrome/Chromium is installed and `LENGJAN_USER`/`LENGJAN_PASS` are set in `.Renviron`
- **Login fails** → Credentials may have changed; check `.Renviron`

## Reference

- Preview script: `lengjan-bets/preview.R`
- Placement pipeline: `lengjan-bets/run.R` → `R/pipeline.R`
- Recommendations: `Sports/recommendations.csv`
- Ledger: `Sports/{sport}/{country}/history/bets_log.csv`
