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
both divisions into the publisher's `extracted` argument; the return
shape is
`list(BD = list(<6 tibbles>), LD1 = list(<6 tibbles>), fit_date = D)`.

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

### Per-division output

The publisher loops over the three Icelandic football divisions
(`BD` = Besta deild, `LD1` = Lengjudeild, `CUP` = Mjólkurbikar) and
writes one full set of JSONs per `(sex, division)` cell into
`data/publish/football/iceland/{sex_folder}-{div_suffix}/`. The dir
suffix follows the platform URL slug (`/lengja/` → `ld`, not `ld1`;
`/bikar/` → `bikar`). Total publish output is 6 directories ×
11 JSONs = 66 files per fit. The per-cell extracted slice is
pre-filtered to the division's teams + matches by the reader's
`division` filter, so the publisher's loop body is now mostly a
render of `ext <- extracted[[target_div]]` rather than a
filter-then-render. The legacy fit-based wrapper
`.publish_football_iceland_from_fit_pfi(..., target_div = X)` is
BD/LD1-only and survives as a regression backstop, deletable after
a few production cycles.

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
R16 → R8 → SF → Final per posterior draw, drawing uniform-random pairings
for unscheduled rounds (KSÍ does not pre-publish the bracket; pairings
are drawn round-by-round — see `Sports/Mjólkurbikar Bracket Simulator
Design.md`). v1 limitation: only runs when 8 R16 fixtures are entirely
upcoming. Output also carries a `summary` array with the P(Champion)
leaderboard for direct frontend rendering. Two shared parquets are also
written per fit — `sim_inputs_team.parquet` (per-draw raw team
strengths) and `sim_inputs_scalar.parquet` (per-draw scalar parameters)
— so the simulator can be re-run with alternative tiebreak / pairing
options without refitting.

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

- Football: 11 JSONs per BD/LD1 cell; 12 JSONs per CUP cell (the 11
  league JSONs — 5 empty placeholders + 6 cup-applicable — plus
  `tournament_placements.json`). Across 6 cells (`karla-{bd,ld,bikar}`,
  `kvenna-{bd,ld,bikar}`) that's 11×4 + 12×2 = 68 JSONs per fit.
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

## metill-platform consumption (as of 2026-05-09)

Only football surfaces on the platform. Of the 11 football JSONs,
6 are rendered today — `meta`, `next_games`, `standings`,
`team_strengths`, `final_positions`, `team_strengths_history`. Three
(`final_positions_history`, `standings_history`, `home_advantage`,
`points_distribution`) are available for frontend rendering but not
yet wired up. `round_predictions_history` is publisher-internal
(read by `R/publish-football-iceland.R` itself to populate
`xg_for/xg_against/xpts` in `standings`). Basketball + handball are
seasonally paused (regular seasons finished late April 2026,
playoffs not modelled); publish for those sports resumes autumn 2026.

See [memory: project_publish_consumers](../../.claude/projects/-Users-brynjolfurjonsson-sports/memory/project_publish_consumers.md).

## Daily driver

`Rscript scripts/05_publish.R`. Wires fresh-fit-on-demand via
`R/publish-pipeline.R::publish_one()`.
