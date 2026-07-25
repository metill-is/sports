---
name: wc-refresh
description: Use during a World Cup tournament when the published forecast is missing scores martj42 hasn't logged yet — the manual overlay refresh that pulls, ingests, re-fits, re-forecasts and pushes. Also covers how played knockouts condition the forecast and how knockout fixture dates are corrected at ingest.
argument-hint: "[list-missing|refresh|no-push]"
allowed-tools: Bash, Read, Edit, Glob, Grep
---

# World Cup — manual refresh (martj42 lag)

martj42 backfills match scores ~1 day late, so during the tournament the daily
`world-cup.yml` cron is structurally a day behind. To publish a forecast that
includes scores martj42 hasn't logged yet:

1. `scripts/wc/refresh_now.sh --list-missing` — prints the played-but-unscored
   WC fixtures as CSV rows (team names already match martj42).
2. Paste them into `data/wc/manual_results.csv` and fill `home_score,away_score`
   (and `pen_winner` — the winning team's exact name — for a **knockout** match
   level on score, i.e. decided on penalties; leave blank otherwise).
3. `scripts/wc/refresh_now.sh` — pulls, ingests (overlay merged onto martj42's
   `NA` rows), re-fits (~46 min), re-forecasts, shows the champion table +
   `data/wc/forecast.html`, then on `y` commits + pushes and triggers the
   metill-platform pull (`gh workflow run pull-sports-data.yml -R
   metill-is/metill-platform`).

The overlay is committed and self-draining: once martj42 publishes the real
score, ingest warns and martj42 wins — delete that row. The manual path does not
touch `data/wc/martj42_pointer.txt`, so the next cron run is unaffected. Flags:
`--yes` (skip confirm), `--no-push` (preview only), `--no-pull` (offline).

## Played knockouts condition the forecast automatically

(Phase 2, shipped 2026-06-29.) `wc_knockout_results()` reads played cross-group
fixtures and `.wc_knockout_pins()` (`R/wc-knockout.R`) builds the
`{match_no → winner}` pins that `simulate_world_cup()` passes to
`wc_forward_bracket()` — so a decided match's winner advances w.p. 1 / loser 0
(placement collapses) and `bracket.json` gains a `played[]` field the platform
renders as a settled fixture. Penalty winners come from `data/wc/shootouts.csv`,
maintained by `wc_ingest_shootouts()` from martj42's `shootouts.csv` + the
overlay's `pen_winner` (martj42 canonical).

**No re-fit is needed for new knockout results** — pins act on the simulate
step, so re-running `scripts/wc/forecast.R` alone re-publishes the conditioned
forecast.

## Knockout fixture dates are corrected at ingest

2026-07-06 incident: martj42 dated all four remaining R16 ties on the round's
first day, so the published forecast — and the platform matchday reel, keyed on
exact `match_date` — carried 4 matches on 6 July instead of 2.

`wc_correct_knockout_dates()` (`R/wc-schedule.R`) re-dates unplayed knockout
rows to the stadium-local date of their official slot, venue-matched against the
vendored `data/wc/structure/wc2026_schedule.csv`; unmappable rows keep martj42's
date with a warning. `--list-missing` applies the same correction, so overlay
rows always key on corrected dates.
