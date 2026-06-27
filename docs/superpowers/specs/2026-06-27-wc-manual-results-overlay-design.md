# Design — Manual WC results overlay + local refresh

**Date:** 2026-06-27
**Repo:** `metill-is/sports` (`~/sports`) — all code changes live here.
**Status:** Approved design, pre-implementation.

## Problem

The HM 2026 (World Cup) forecast is gated entirely on one upstream source:
`scripts/wc/ingest.R` downloads
`https://raw.githubusercontent.com/martj42/international_results/master/results.csv`
on every run. That dataset is volunteer-maintained and commits roughly once each
morning (~07:00–09:00 UTC), so during the tournament it backfills match **scores
with a ~1-day lag** — the CSV already carries the WC *fixtures* as `NA`-score
rows, but the actual scores appear the next morning.

Consequence: even a forced re-run of the pipeline re-ingests the *same* stale
CSV. There is no seam today where the operator can say "I know last night's
scores, run with those." The forecast is structurally ~a day behind the ball.

Observed live on 2026-06-27: pipeline pointer `85d5335…` already equals
martj42's master tip, dated `2026-06-26 08:26 UTC` — i.e. the model is as fresh
as martj42 allows, and martj42 has not pushed today's results yet.

## Goal

Give the operator a **manual, on-demand** path to inject the match scores
martj42 has not logged yet, re-fit, re-forecast, and publish to the live site —
without waiting for martj42. Primary execution is **local** (run on the
operator's machine, eyeball the forecast, then push).

### Non-goals (YAGNI)

- No interactive score-entry UI/TUI (Approach C — rejected).
- No direct parquet patching of the facts store (Approach B — rejected;
  reimplements pipeline logic, corrupts easily, no audit trail).
- No new CI workflow. The existing `world-cup.yml` cron is unchanged; it simply
  honours the committed overlay for free (see §7).
- Not advancing `martj42_pointer.txt` from the manual path (see §7).

## Why this seam

`wc_ingest_internationals()` (`R/wc-ingest.R:76-77`) decides "played vs
scheduled" purely on whether `home_score`/`away_score` are `NA`. Patching a
martj42 `NA`-score row with a known score **before** that split makes the match
flow naturally into `results` and get fit — with **zero** downstream changes.
Inject at the single source-of-truth seam (the martj42 CSV, before any
filtering) and the existing population filter, Hive partitioning, fit, simulate,
and publish all "just work".

## Data flow

```
martj42 CSV ──download──▶ [overlay patch: NA → known scores] ──▶ wc_ingest_internationals
   split (played / scheduled) ──▶ facts parquet (results now includes injected matches)
   ──▶ prepare_data ──▶ fit_model (always re-fits) ──▶ sim_inputs.rds
   ──▶ simulate_world_cup + wc_head_to_head ──▶ 8 JSON in data/publish/world_cup/karla/
   ──▶ commit + push to metill-is/sports ──▶ gh workflow run pull-sports-data.yml
   ──▶ rsync to platform data/ithrottir/world_cup/karla/ ──▶ Fly deploy ──▶ /hm2026
```

## Components

| # | Artefact | Kind | Purpose |
|---|---|---|---|
| 1 | `data/wc/manual_results.csv` | new, **committed** | Operator-typed scores martj42 lacks. |
| 2 | `wc_apply_manual_results(raw, overlay)` | new pure fn, `R/wc-ingest.R` | Patch `NA`-score rows by key. |
| 3 | `wc_list_unscored_fixtures(raw, as_of)` | new pure fn, `R/wc-ingest.R` | The rows that need filling. |
| 4 | `wc_ingest_internationals(..., manual_overlay_path =)` | +1 optional param | Apply overlay after `read_csv`. |
| 5 | `scripts/wc/list_missing.R` | new thin script | Print paste-ready overlay rows. |
| 6 | `scripts/wc/refresh_now.sh` | new orchestrator | End-to-end local run + publish. |

`scripts/wc/ingest.R` is **unchanged**: param #4 defaults to the overlay path,
so both the local run and the CI cron pick up the overlay automatically.

### 1. `data/wc/manual_results.csv`

- Lives next to the already-tracked `data/wc/martj42_pointer.txt` (only
  `data/wc/raw/` is gitignored; `data/wc/` itself is committable).
- Columns: `date,home_team,away_team,home_score,away_score`.
- `#`-commented usage preamble (readr `comment = "#"`), e.g.:
  ```
  # Manual WC results overlay — scores martj42 hasn't published yet.
  # Team names MUST match martj42 spelling exactly (run scripts/wc/list_missing.R).
  # Rows are auto-ignored once martj42 publishes the real score; prune on the warning.
  date,home_team,away_team,home_score,away_score
  ```
- Empty (header only) in steady state.

### 2. `wc_apply_manual_results(raw, overlay)` — correctness core

Pure function, no I/O. `raw` = martj42 data frame post-`read_csv` (full schema);
`overlay` = the manual rows. For each overlay row, match exactly one `raw` row
on `(date, home_team, away_team)`:

- **0 matches** → `cli::cli_abort()` naming the offending row (a name/date typo
  must fail loudly, never silently no-op).
- **>1 matches** → abort (ambiguous; shouldn't happen given martj42's natural
  key, but guard it).
- **matched `raw` row still `NA`** → fill with the overlay scores.
- **matched `raw` row already scored (martj42 caught up)** → martj42 wins
  (canonical). Equal to overlay → silent. Differs → `cli::cli_warn()` "martj42
  now reports X–Y, remove this row from manual_results.csv" (the overlay
  **self-drains**).

Returns the patched `raw` plus a count of rows filled (for the ingest log).

### 3. `wc_list_unscored_fixtures(raw, as_of = Sys.Date())`

Pure function operating on the **raw martj42 schema** (same `raw` that
`wc_apply_manual_results` receives — read straight from `read_csv`, *before* the
`wc-ingest.R:58-66` transmute renames `tournament → division` / `date →
match_date`). Returns WC fixtures (`tournament == "FIFA World Cup"`,
`format(date,"%Y") == "2026"`) with `date <= as_of` and `NA` scores — i.e.
matches that should have been played but martj42 hasn't scored. Output shaped as
overlay rows (`date,home_team,away_team` + empty score columns) so the operator
pastes and fills.

### 4. `wc_ingest_internationals()` change

Add `manual_overlay_path = here::here("data", "wc", "manual_results.csv")`.
After `raw <- readr::read_csv(...)`: if the path exists and the file has data
rows, read it (strict col types: date, character, character, integer, integer,
`comment = "#"`) and `raw <- wc_apply_manual_results(raw, overlay)`. Default
path ⇒ no call-site change in `ingest.R` and CI honours it for free.

### 5. `scripts/wc/list_missing.R`

Download martj42 (reuse the `ingest.R` download + header guard), read it, call
`wc_list_unscored_fixtures()`, print as CSV lines to stdout. No writes.

### 6. `scripts/wc/refresh_now.sh` — orchestrator

`set -euo pipefail`. Flags:
- `--list-missing` — run `list_missing.R`, print rows to fill, exit.
- `--yes` — skip the preview/confirm gate.
- `--no-push` — run + preview, but don't commit/push/trigger.
- `--no-pull` — skip the pre-run `pull --rebase` (offline).

Default run:
1. `git -C "$SPORTS" pull --rebase origin main` (cron-collision safety).
2. `Rscript scripts/wc/ingest.R` (overlay applied inside).
3. `Rscript scripts/wc/fit.R`.
4. `Rscript scripts/wc/forecast.R`.
   — All three always run, in order. Never `forecast.R` alone: it reuses the
   stale `sim_inputs.rds`.
5. Print the champion table (forecast.R already does) + the `forecast.html`
   path; **pause** ("Publish? [y/N]") unless `--yes`.
6. On confirm and not `--no-push`:
   `git add data/publish/world_cup data/facts/results/sport=football/country=world
   data/facts/schedules/sport=football/country=world data/wc/manual_results.csv
   data/wc/accountability`;
   commit `data(wc): manual refresh <UTC ts> — martj42 lag`;
   `git pull --rebase origin main`; `git push`.
7. `gh workflow run pull-sports-data.yml -R metill-is/metill-platform`.
8. Print a deploy-watch hint
   (`gh run list -R metill-is/metill-platform --workflow=pull-sports-data.yml`).

(Confirmed during implementation: `data/wc/accountability` **is** staged.
`forecast.R` → `publish_world_cup()` → `wc_snapshot_predictions()`
(`R/wc-accountability.R`) rewrites the git-tracked
`data/wc/accountability/prediction_log.json` on every run, so the wrapper stages
it to mirror `world-cup.yml:217` and to avoid leaving it dirty post-publish. The
implementation plan briefly regressed this; the final review restored it.)

## 7. Interaction with the CI cron (no double-state)

- The manual path **deliberately does not advance `data/wc/martj42_pointer.txt`**.
  So the next `world-cup.yml` run still fires normally when martj42's SHA
  changes, re-ingests martj42's now-real scores (which win over the overlay),
  and the overlay drains via the §2 warning.
- Because the overlay merge lives inside `wc_ingest_internationals()` (default
  path), the CI cron also applies the committed overlay — but the cron is still
  SHA-gated on martj42, so committing an overlay alone never auto-triggers CI.
  Consistent, no surprise runs.

## Error handling

| Failure | Behaviour |
|---|---|
| martj42 download fails (404/redirect/schema change) | Existing `ingest.R` header guard aborts. |
| Overlay row matches no martj42 fixture (typo) | `wc_apply_manual_results` aborts, names the row. |
| martj42 caught up, score differs | Warn + prune instruction; martj42 wins (drain). |
| Empty/missing overlay | No-op; pipeline identical to today. |
| Malformed overlay CSV | Strict col-type read aborts. |
| Git push race (cron landed under you) | `pull --rebase` before push; `set -o pipefail`. |

## Testing

testthat unit tests (`tests/testthat/`):

- `wc_apply_manual_results`: fills an `NA` row by exact key; aborts on a
  non-matching (typo) row; leaves a martj42-scored row untouched and warns on a
  differing overlay (drain); no-op on empty overlay; correct fill count.
- `wc_list_unscored_fixtures`: returns only past-dated `NA`-score WC fixtures
  (excludes future fixtures and already-scored matches).

`devtools::test()` and `devtools::check()` green. The shell wrapper is glue —
covered by a `--list-missing` smoke run, not unit tests.

## Docs (same commit)

- `~/sports/CLAUDE.md`: WC manual-refresh runbook (list-missing → edit CSV →
  `refresh_now.sh` → confirm → auto-publish).
- `~/metill-platform/.claude/rules/hm2026.md`: update the "manual fallback" line
  to point at `refresh_now.sh` + the overlay.
- `manual_results.csv` `#` header is self-documenting.

## Decisions (reversible)

- Wrapper **previews-and-confirms by default** (`--yes` to skip) — matches the
  operator's eyeball-`forecast.html` loop.
- Overlay **only fills genuine `NA`s**; martj42 always wins once it has a real
  score.
- Spec + implementation isolated on branch `wc-manual-overlay` in `~/sports`.
