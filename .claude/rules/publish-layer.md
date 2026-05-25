---
paths:
  - "R/publish-*.R"
  - "R/extract-*.R"
  - "R/publish-pipeline.R"
  - "data/publish/**"
  - "data/beliefs/extracts/**"
  - "scripts/05_publish.R"
---

# Publish Layer

> The compiled-truth catalogue with per-JSON schemas lives in the
> Metill Obsidian vault at
> `Sports/Knowledge/Publish Pipeline/data-contract.md`.
> This file is the project-side quick reference.

## Football iceland (extracts tree, since 2026-05-05)

`publish_football_iceland(extracted, league, sex)` reads from the
6 per-fit Parquets at
`data/beliefs/extracts/sport=football/country=iceland/sex=Z/fit_date=*/`
(emitted by `extract_football_iceland()`) instead of the in-memory fit
RDS. Each parquet carries a `division` column (`"BD"` or `"LD1"`); the
reader filters by that column to materialise per-cell tibbles. The
fit RDS is fully ephemeral — gitignored, deletable after a fit
completes — and the legacy beliefs_archive (`part-0.parquet`) write
was dropped for football iceland in `R/model-league.R::fit_league()`.
Use `read_extracted_football(league, sex, fit_date = NULL)` to load
all per-sex divisions into the publisher's `extracted` argument; the
return shape is keyed by code from
`config/leagues.yml::football_iceland.publish_divisions[[sex]]` plus a
trailing `fit_date` slot. For example, with the 2026-05-24 config,
male returns
`list(BD = ..., LD1 = ..., LD2 = ..., LD3 = ..., CUP = ..., fit_date = D)`
and female returns
`list(BD = ..., LD1 = ..., LD2 = ..., CUP = ..., fit_date = D)`. Each
per-division slot is a list of the 6 per-cell tibbles.

### Why a separate tree

Putting per-cell summaries under `data/beliefs/archive/` (the
canonical per-draw-per-match Parquet table) caused two failures:
(1) arrow's hive-partition auto-detection saw mixed depths when
football used `division=…/` subdirs;
(2) `arrow::open_dataset()` couldn't unify the per-draw schema with
the pre-aggregated sidecar schemas (e.g. `home_goals: double` vs
`int32`).

Moving sidecars to `data/beliefs/extracts/` keeps the
`beliefs_archive` dataset uniform (one schema, depth 4) while still
partitioning the sidecars by `(sport, country, sex, fit_date)`. See
[memory: project_extracts_tree](../../.claude/projects/-Users-brynjolfurjonsson-sports/memory/project_extracts_tree.md).

### Retention policy: extracts are permanent

`data/beliefs/extracts/.../fit_date=*/` partitions are **accretive and
load-bearing for replay**. Never bulk-prune them; every kept partition
is one historical date at which the publisher can be re-run against
the same posteriors that shipped originally (modulo Stan RNG; see
`scripts/0Nr_replay.R` for the seed-pinned reproducibility path).

Storage cost: a typical football iceland fit writes ~335 KB per
division × (5 + 4) cells × 2 sexes ≈ 6 MB per fit-day, or ~50 MB per
active month. Two years of full coverage fits in ~1.2 GB —
comfortably within git's expected envelope for this repo.

If a future cleanup ever does become necessary, the recoverable thing
is to drop partitions older than a documented horizon AND verify no
downstream consumer (replay CLI, calibration backtest, post-hoc
xPts time-series) is pointed at them. Even then, prefer compaction
(e.g. monthly roll-ups) over deletion.

### Per-division output

The publisher loops over the per-sex Icelandic football publish set
defined in `config/leagues.yml::football_iceland.publish_divisions`.
As of 2026-05-24 that's 5 cells for male (`BD`, `LD1`, `LD2`, `LD3`,
`CUP`) and 4 cells for female (`BD`, `LD1`, `LD2`, `CUP`) — no
women's 3. or 4. deild exists in Iceland; men's 4. deild is held out
by `training_filter` because amateur cup blowouts produced funnel
posteriors. Each cell writes one full set of JSONs into
`data/publish/football/iceland/{sex_folder}-{slug}/` where the slug
is from `publish_divisions[*].slug`, matching the metill-platform
consumer's URL segments (`/besta/` → `bd`, `/lengja/` → `ld`,
`/2deild/` → `2deild`, `/3deild/` → `3deild`, `/bikar/` → `bikar`).

Total publish output (post 2026-05-24): 9 directories on a typical
nightly fit — `karla-{bd,ld,2deild,3deild,bikar}` (5) plus
`kvenna-{bd,ld,2deild,bikar}` (4). League cells emit 11 JSONs each;
cup cells emit 12 (the extra one is `tournament_placements.json`).
So per fit: `11×7 + 12×2 = 101` JSONs.

To add a new cell: append a `{code, slug, label_is, is_cup}` entry to
the per-sex `publish_divisions` list in `leagues.yml`, mirror it as a
`DIVISIONS[<slug>]` entry on the consumer
(`metill-platform/app/routes/ithrottir.py`), add the corresponding
`(URL, meta.json)` row to the sitemap in `app/routes/pages.py`. The
slug **must** be URL-safe (matches the schema's
`^[A-Za-z0-9][A-Za-z0-9_-]*$` pattern). No R code change required for
the producer side as long as the division's data already feeds the
fit (via `training_filter.divisions` + an ingest source for matches).

The per-cell extracted slice is pre-filtered to the division's teams +
matches by the reader's `division` filter, so the publisher's loop
body is mostly a render of `ext <- extracted[[target_div]]` rather
than a filter-then-render.

CUP cells skip the league-table outputs (`standings.json`,
`standings_history.json`, `final_positions.json`,
`final_positions_history.json`, `points_distribution.json`) — those
five JSONs ship as empty placeholders for endpoint stability.
`meta.json` carries `is_cup: true` + `division: "CUP"` so frontends
branch on the cup marker rather than inspecting standings rows. The
extract layer's `.extract_division_parquets_pfi()` short-circuits the
league-table simulation when `target_div == "CUP"`; the publisher's
standings block is gated on `!is_cup`.

CUP cells additionally ship `tournament_placements.json` — P(team
reaches at least round X) for X ∈ {R16, QF, SF, Final, Champion},
cumulative form. Produced by the R-side cup bracket simulator
`R/simulate-cup-bracket.R::simulate_cup_bracket()` reading per-draw
strength parameters extracted from the fit. The simulator forward-walks
R16 → R8 → SF → Final per posterior draw with per-match outcomes drawn
from the bivariate-Poisson model at training-cutoff strengths. Ties at
90' are handled by rejection sampling (keep drawing at the same lambdas
until a non-tied draw emerges) — mathematically equivalent to
P(winner | someone wins) under the model; avoids parametric ET/shootout
chain. Pairings for unscheduled rounds are drawn uniformly at random per
posterior draw (KSÍ does not pre-publish the bracket; pairings drawn
round-by-round — see `Sports/Mjólkurbikar Bracket Simulator Design.md`).

`bracket_state` uses a generalised `cup_teams + rounds` schema keyed by
round name, supporting every cup-lifecycle entry point uniformly via one
4-iteration walker loop: pre-R16, mid-R16 (some matches played, some
upcoming), post-R16 / pre-R8 draw (R16 winners derived from results, R8
random pairings), post-R8 draw, post-R8 played, etc.
`.build_bracket_state_pfi(pred_d, results, current_season)` unions
upcoming schedule + played results, identifies the 16 R16 teams via a
sliding-window heuristic (8 cup matches in ≤ 4 days with 16 distinct
teams), and ranks subsequent cup matches chronologically to populate
R8/SF/Final.

Output also carries a `summary` array with the P(Champion) leaderboard
for direct frontend rendering. Two shared parquets are also written per
fit — `sim_inputs_team.parquet` (per-draw raw team strengths) and
`sim_inputs_scalar.parquet` (per-draw scalar parameters) — so the
simulator can be re-run with alternative tiebreak / pairing options
without refitting.

Schema-only iterations on the publisher run via the `republish.yml`
`workflow_dispatch` Action, which calls `scripts/05_publish.R` without
re-fitting.

## Basketball + handball (legacy fit-based path; seasonally paused)

`publish_<sport>_iceland(fit, league, sex)` reads from the fit RDS at
`data/beliefs/fits/sport=X/country=Y/sex=Z/fit.rds` and writes to
`data/publish/{sport}/iceland/{karla,kvenna}/` (no division split —
only the top division is modelled). Migration to the extraction layer
+ a per-division split is deferred to the autumn 2026 cutover.

**Currently paused.** Icelandic basketball + handball regular seasons
finished in late April 2026; the playoff brackets aren't modelled.
CI's last basketball publish was 2026-04-29 (`f230c47`); last handball
publish was 2026-04-30 (`a4741b0`). Fits may still occur if completed
matches arrive (e.g. straggler results), but `decide-publish.yml`
no-ops on these sports until the autumn 2026 season opener — see
[memory: project_basketball_handball_seasonal_pause](../../.claude/projects/-Users-brynjolfurjonsson-sports/memory/project_basketball_handball_seasonal_pause.md).

## File counts

- Football: 10 JSONs per league cell (BD/LD1/LD2/LD3); 11 JSONs per
  CUP cell (the 10 league JSONs — 5 empty placeholders for cup + 5
  cup-applicable — plus `tournament_placements.json`). Per
  `config/leagues.yml::football_iceland.publish_divisions` as of
  2026-05-24, that's 9 cells:
  `karla-{bd,ld,2deild,3deild,bikar}` (5) plus
  `kvenna-{bd,ld,2deild,bikar}` (4) — i.e. `10×7 + 11×2 = 92` JSONs
  per fit. (`round_predictions_history.json` moved out of
  `data/publish/` in F7 — see consumption note below.)
- Per-fit extracts: 9 parquets — the 7 per-cell file types
  (`predicted_matches`, `team_strengths_quantiles`,
  `round_strengths_quantiles`, `home_advantage_quantiles`,
  `final_positions`, `points_distribution`, `tournament_placements`)
  plus 2 shared bracket-simulator inputs (`sim_inputs_team`,
  `sim_inputs_scalar`).
- Basketball + handball: same 7 snapshots plus
  `final_positions_history.json` per sex (8 × 2 = 16 each).

## Schema features (as of 2026-05-03)

- `standings.json` rows ship cumulative `xg_for`/`xg_against`/`xpts`
  over archived rounds, plus `n_predicted_matches`/`n_played_matches`
  for partial-coverage disclosure. Lookahead-free: each round uses
  the latest fit strictly before its first kickoff.
- `team_strengths.json` ships a 9-cell grid per team:
  `component ∈ {offence, defence, total}` ×
  `location ∈ {home, away, avg}`. `avg` is the per-draw mean so
  uncertainty intervals reflect the joint posterior. Same grid in
  `team_strengths_history.json`. Each record optionally carries a
  `preseason: {median, lower, upper}` object — sourced from the
  latest archived fit strictly before the cell's first played
  kickoff in the current season — used by the platform to render a
  baseline (red) sub-row in the forest plot. Field is omitted when
  no qualifying earlier fit exists.
- `final_positions_history.json` accretes per-round projections so
  the frontend can offer a round filter; deduplicated on
  `(as_of, team, placement)`.
- `meta.json` includes `sport` for all three publishers.

## metill-platform consumption (as of 2026-05-25)

Only football surfaces on the platform. Of the 10 football JSONs in
`data/publish/`, 6 are rendered today — `meta`, `next_games`,
`standings`, `team_strengths`, `final_positions`,
`team_strengths_history`. Four (`final_positions_history`,
`standings_history`, `home_advantage`, `points_distribution`) are
available for frontend rendering but not yet wired up. Basketball +
handball are seasonally paused (regular seasons finished late April
2026, playoffs not modelled); publish for those sports resumes autumn
2026.

`round_predictions_history.json` is publisher-internal — it
accumulates `(round, team)` xG/xPts predictions across fits and is
re-read each publish to dedup on `(round, team)` keeping the latest
`generated_at`. As of 2026-05-25 (F7) it lives at
`data/beliefs/round_predictions_history/football/iceland/{sex}-{slug}/`
rather than `data/publish/.../`, so the metill-platform rsync no
longer mirrors a file no consumer reads.

See [memory: project_publish_consumers](../../.claude/projects/-Users-brynjolfurjonsson-sports/memory/project_publish_consumers.md).

## Daily driver

`Rscript scripts/05_publish.R`. Wires fresh-fit-on-demand via
`R/publish-pipeline.R::publish_one()`.
