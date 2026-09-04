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

### Division accessors are sport-neutral (since 2026-09-04)

`publish_divisions` is no longer football's alone. Every Icelandic
league declares its own per-sex publish cells:

| League | male | female |
|---|---|---|
| `football_iceland` | BD, LD1, LD2, LD3, CUP | BD, LD1, LD2, CUP |
| `basketball_iceland` | BD, 1D | BD, 1D |
| `handball_iceland` | OD, G66 | OD, G66 |

They are read through nine accessors in `R/publish-divisions.R`, all
`.iceland_division_*(key, sex)` where `key` is a `leagues.yml`
top-level league key: `codes`, `slugs`, `labels`, `split`, `badges`,
`is_cup`, `qualify`, `relegation`, `expected_meetings`. The
football-only `.football_iceland_division_*` helpers they replace are
**deleted, with no compatibility aliases** — two live names for one
symbol is the drift this removed. Adding a publish cell is a config
edit plus a metill-platform `DIVISIONS` entry, never an R edit.

Four optional keys on a `publish_divisions` entry, all absent-safe:

| Key | Contract |
|---|---|
| `code_badge` | Short ASCII badge emitted as `next_games.json::division_code`, which the publish schemas pattern as `^[A-Z][A-Z0-9_]*$`. Basketball's code `1D` fails that on its own (leading digit), which is why the key exists. Absent falls back to `code`. Every entry carrying a `split` also derives `<code>_UPPER_PO`/`_LOWER_PO` → `<badge>U`/`<badge>L`. |
| `expected_meetings` | Times each pair meets in the **regular** season. An assertion and a fallback, **never the source** — `n_rounds` is derived from schedule + results (spec §12). Omit where the format is genuinely irregular (basketball female 1D). |
| `qualify` | `{slots, label_is}`. Absent = `meta.qualify: null` and **no** `p_qualify`. It is the generic replacement for football's `p_top_six`, which does not transfer: Bónusdeild karla is 12 teams with 8 qualifying, and Bónusdeild kvenna carries all 10 through. |
| `relegation_slots` | Teams relegated from this division. Replaces the hardcoded bottom-two rule (`placement >= n_teams - 1L`), which is wrong for a bottom-tier division where nothing is relegated. |

Only football BD (both sexes) configures `qualify` today — `{slots: 6,
label_is: "Efri hluti"}`, which is `split$upper`, so `p_qualify`
reproduces the existing `placement <= 6L` rule exactly. Basketball and
handball configure **no** `qualify` and **no** `relegation_slots`: four
cells with four different post-season structures, and no regulation was
resolved for the relegation counts. An unresolved number is omitted
rather than guessed — absent publishes honest nulls, a wrong number
silently mislabels a headline probability.

`expected_meetings` values are measured from `data/facts/results`, not
assumed. Icelandic women's handball plays a **triple** round robin
(8 teams, 84 matches, 3 meetings per pair), so the `2*(n_teams - 1)`
formula is wrong there. The assertion in
`tests/testthat/test-iceland-division-helpers.R` re-derives every value
from the parquet; when a federation changes format it is *supposed* to
go red, and the fix is to re-measure and rewrite the constant, **not**
to loosen the test.

Only `football_iceland` carries a `training_filter` key
(`config/leagues.yml` — its sole occurrence). The 2DT extractor's
round-strength trajectory indexes on rounds derived from the unfiltered
results, so it asserts `is.null(league$training_filter)`; adding a
`training_filter` to basketball or handball will abort that extractor
until the trajectory's round indexing is reworked.

Note that this config layer publishes nothing on its own — it is inert
until the extract, read and publish layers consume it.

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

## Split-season semantics (since 2026-07-10)

Besta deild karla/kvenna have a split season (efri/neðri hluti):
after the regular phase (22 rounds male / 18 female) the table splits
into an upper/lower group (6/6 male, 6/4 female), each playing a
single round-robin with **full carry-over** and a **group-locked**
final table. Declared per cell in
`config/leagues.yml::publish_divisions[*].split` and simulated by
`simulate_league_season()` (two-phase: per-draw split assignment +
KSÍ-template fixtures while the regular phase runs; known groups +
scheduled/template-completed fixtures once it's over — assembled by
`.league_split_state_pfi()`).

Consequences for the JSONs:
- `final_positions.json` / `final_positions_history.json` placements
  are **full-season**: `placement = 1` = Íslandsmeistari; relegation
  places are the bottom of the lower block. The `summary` `p_top_six`
  stays (redundant-but-harmless once split membership is explicit).
- `points_distribution.json` support includes split-phase games
  (e.g. a BD karla leader can reach base + (remaining + 5) × 3).
- `meta.json` carries an optional `split: {upper, lower}` object on
  split cells (schema-validated both sides) — the platform renders
  group boundaries + labels from it (coordinates with the platform's
  site-label fix).
- Format facts + verification evidence:
  `docs/superpowers/specs/2026-07-10-split-season-simulator-design.md`.
- `standings.json`, `next_games.json` and the xG/xPts round aggregation
  are split-aware (since 2026-07-10, follow-up to the simulator): every
  per-season filter reads the cell's division *family*
  (`.split_family_divisions_pfi()` — BD + BD_UPPER_PO + BD_LOWER_PO for
  a split cell), so split-phase matches tabulate into standings, ship in
  `next_games.json` (`division_code` `BDU`/`BDL`), keep `meta.round`
  counting, and keep cumulative xG/xPts accruing. Once split-phase
  matches are observed (played, or upcoming in the prediction window)
  the standings `rank` is group-locked — membership via
  `.split_group_membership_pfi()`, shared with the season simulator —
  so the platform can draw the group boundary from `rank` +
  `meta.split` with no schema change.

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

## Schema validation (since 2026-05-26)

Every JSON the publishers emit is validated against
`config/publish-schemas/<sport>/<file>.schema.json` (sport-namespaced) or
`config/publish-schemas/<file>.schema.json` (sport-agnostic fallback) via
`R/validate-publish.R::validate_publish_dir()`. `publish_one()` calls it
at the end of each successful publish; on failure the daily-driver and
`republish.yml` workflow both abort via `cli::cli_abort()`, leaving the
previous JSONs on disk (writes are idempotent — no truncate-before-write).

Today only football schemas exist (`config/publish-schemas/football/`).
Basketball + handball legacy JSONs land in `unmatched` (informational,
not errors) until F6 migrates them onto the football shape at the autumn
2026 cutover. The unmatched path also lets future producer-side artefacts
ship before their schema is written — the contract is opt-in per filename.

Cross-repo: `metill-platform/scripts/validate_publish.py` mirrors the R
validator using `fastjsonschema`. It runs inside `pull-sports-data.yml`
between rsync and commit; exit-non-zero stops the workflow before the
deploy-chain dispatch (F3) fires. Schemas ship from sports to platform
via the same rsync — single source of truth at
`config/publish-schemas/`, lands at `data/ithrottir-schemas/` on the
platform side. See `config/publish-schemas/README.md` for the full
contract.

## Daily driver

`Rscript scripts/05_publish.R`. Wires fresh-fit-on-demand via
`R/publish-pipeline.R::publish_one()`.
