---
paths:
  - "R/publish-*.R"
  - "R/extract-*.R"
  - "scripts/05_publish.R"
  - "config/publish-schemas/**"
  - "tools/gen-publish-schemas.R"
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
[memory: project_extracts_tree](/Users/brynjolfurjonsson/.claude/projects/-Users-brynjolfurjonsson-sports/memory/project_extracts_tree.md).

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

Dated design notes, moved verbatim to [`docs/publish-layer-design.md`](../../docs/publish-layer-design.md) on 2026-09-23 (read it before changing division config, 2DT or split-season code): sport-neutral division accessors and their `leagues.yml` keys, partition-level `fit_meta.parquet`, per-sport home-advantage units, the extract-time regular-season boundary, meta.json v2 + the D3 relabel, 2DT season and format, split-season semantics. Two invariants from those notes stay here:

- `fit_meta.parquet` is partition-level: one row per `fit_date=` partition and the only partition file with no `division` column, so it never enters an extractor's division-keyed write loop and the reader must not split it.
- The regular-season boundary is applied at extract time too: `.publish_n_rounds()` / `.regular_season_results()` in `R/publish-format.R` is the one boundary function, and both the extractor and the publisher call it.

## Basketball + handball (extracts tree, since 2026-09-04)

There is no per-sport 2DT publisher any more. `publish_basketball_iceland()`
and `publish_handball_iceland()` are DELETED, and both sports go through
`publish_one()` → `publish_iceland_league()` on the same extracts path
football uses, emitting the same ten JSONs per cell into
`data/publish/{sport}/iceland/{sex}-{slug}/`. That was B4: the old publishers
read `data/beliefs/fits/.../fit.rds`, a path `.gitignore` excludes and CI never
produces, so they warned and returned `invisible(NULL)` — exit 0, nothing
published, no health row — for months.

The 32 un-suffixed JSONs those publishers left at
`data/publish/{sport}/iceland/{karla,kvenna}/` were deleted on 2026-09-04 as
the schema-arming precondition; `tests/testthat/test-publish-legacy-cells.R`
stops that shape coming back.

**Playoffs aren't modelled.** The basketball + handball models cover the regular season only.

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
- Per-fit football extracts: 10 parquets — the 7 per-cell file types
  (`predicted_matches`, `team_strengths_quantiles`,
  `round_strengths_quantiles`, `home_advantage_quantiles`,
  `final_positions`, `points_distribution`, `tournament_placements`)
  plus 2 shared bracket-simulator inputs (`sim_inputs_team`,
  `sim_inputs_scalar`) and `fit_meta`.
- Per-fit basketball / handball extracts (since 2026-09-04): 7 parquets —
  the 6 division-keyed file types football has minus
  `tournament_placements` (neither sport models a knockout cup), plus
  `fit_meta`. Each division-keyed file carries a `division` payload column
  spanning `publish_divisions[[sex]]` — basketball `{BD, 1D}`, handball
  `{OD, G66}` — exactly as football's do.
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

See [memory: project_publish_consumers](/Users/brynjolfurjonsson/.claude/projects/-Users-brynjolfurjonsson-sports/memory/project_publish_consumers.md).

## Schema validation (since 2026-05-26; generated + fail-closed 2026-09-04)

Every JSON the publishers emit is validated against
`config/publish-schemas/<sport>/<file>.schema.json` via
`R/validate-publish.R::validate_publish_dir()`. `publish_one()` calls it at the
end of each successful publish, and on failure aborts via `cli::cli_abort()`,
leaving the previous JSONs on disk (writes are idempotent — no
truncate-before-write). `config/publish-schemas/README.md` is the full
contract; three things belong here because getting them wrong is silent.

**The per-sport schemas are GENERATED.** `config/publish-schemas/_base/` holds
the shared contract as `<name>.json`; `_delta/<sport>/<name>.json` is an RFC-7386
patch; `Rscript tools/gen-publish-schemas.R` renders `<sport>/<name>.schema.json`.
Never hand-edit a file under `<sport>/` — the next render reverts it and
`test-publish-schema-generation.R` goes red. Two traps the generator encodes: a
delta touching `required` replaces the array WHOLESALE (forgetting an entry
silently relaxes that sport's contract), and the whole tree is enforced pure
ASCII because `jsonlite::toJSON()` renders a UTF-8 em-dash as the literal
7-character string `<U+2014>` even when `Encoding()` says "UTF-8".
`_base` files are named `<name>.json` rather than `<name>.schema.json`
precisely so `_base` cannot resolve as if it were a sport.

**Validation is scoped to the publishing sport's OWN subtree, with the sport
named explicitly.** Validating the whole tree meant arming ANY sport armed it
inside EVERY other sport's publish call, so one sport's bad JSON aborted
another's publish. The obvious fix — narrowing `dir` to the sport subtree —
fails OPEN: `validate_publish_dir()` derives the sport from the first path
segment relative to `dir`, which for a subtree is `"iceland"`, so no schema
resolves, every file lands in `unmatched` and it returns `ok = TRUE,
n_files = 0` with nothing checked. The explicit `sport` argument IS the fix.
A reviewer seeing only the path change should reject it.

**The missing-schema default is fail-CLOSED.** A sport with no
`config/publish-schemas/<sport>/` directory used to publish with an
informational "skipping validation" note and exit 0 — the same
unchecked-but-green shape as B4. It now aborts. All three sports that reach
`publish_one()` are armed (football since 2026-05-26, basketball and handball
since 2026-09-04). `publish_world_cup()` (`R/wc-publish.R`) never calls
`publish_one()` or `.validate_or_abort()` — verified by grep and pinned by a
test — so `world_cup`, which has no schema directory by design, is untouched.
The escape hatch for a synthetic-data test whose payload the schema would
reject by design is `validate = FALSE`, never loosening the default. A sport
that published NOTHING stays a warning rather than an abort.

Cross-repo: `metill-platform/scripts/validate_publish.py` mirrors the R
validator using `fastjsonschema`. It runs inside `pull-sports-data.yml` between
rsync and commit; exit-non-zero stops the workflow before the deploy-chain
dispatch fires and production stays on the last-known-good payload. Schemas
ship via the same rsync from ONE clone at ONE SHA, so schema and JSON can never
skew — which also means **arming a sport is immediate on the platform side**.
Delete any non-conforming JSON for that sport BEFORE the arming commit, never
after, or the platform validator fails closed and freezes the site.

## The stored quantile grid

Extract parquets carry `PUBLISH_QUANTILE_GRID` (R/publish-quantile-grid.R) --
**23 quantiles, not all 99**. The grid is every 5th percentile plus `2, 3, 97,
98` for the 95% band's interpolated tails; `1` and `99` are excluded as the
noisiest tails of a 4000-draw posterior that nothing publishes.

Why it is not 99: the only consumer, `.intervals_from_quantiles_pfi()`,
filters to nine quantiles on its first line and discards the rest. Storing all
99 meant ~90% of the largest artefact in the repo was computed, written,
committed to git and shallow-cloned by nine CI workflows in order to be thrown
away -- football's `round_strengths_quantiles.parquet` alone was 8.0 MB of a
22 MB partition. It does not compress either: `value` held 1,001,475 distinct
doubles across 1,001,484 rows, so parquet's dictionary and RLE encodings have
nothing to work with. Trimming is ~30% off every partition, and the computed
intervals are byte-identical (proved by running both through
`.intervals_from_quantiles_pfi` on a real partition).

Why it is not the nine that are used: storing exactly what today's publisher
wants bakes a presentation choice into stored data, and changing a coverage
band would then need a REFIT rather than a republish. The extra ~1 MB per
partition buys any 5%-granular band without refitting.

**Adding a coverage band.** Extend `needed` in
`.intervals_from_quantiles_pfi()` AND `PUBLISH_QUANTILE_GRID` together. If you
forget the grid, `test-publish-quantile-grid.R` fails at test time, and at
runtime `.assert_quantiles_available()` aborts rather than letting
`pivot_wider()` silently produce NA bands. A quantile that was never written
cannot be recovered by republishing -- it needs a new fit.

Partitions written before this change carry all 99 and still read correctly
(the grid is a subset), and they age out via `prune_extracts()`.

## Daily driver

`Rscript scripts/05_publish.R`. Wires fresh-fit-on-demand via
`R/publish-pipeline.R::publish_one()`.

## Stage, validate, swap (2026-09-05)

`publish_one()` publishes into a staging copy of the (sport, sex) cells,
validates the staging tree, and only then swaps those cells into
`data/publish/`. A cell that fails the contract leaves the previous output
byte-identical (or, on a first publish, leaves no cell at all). WHY: the
workflows commit with `if: always()` and stage the whole publish tree, so a
rejected cell used to reach `main` anyway -- the first real handball publish
put four cells with `as_of = "-Inf"` there. The staging copy is seeded with
the cell's current files because the history JSONs accrete by reading the
existing file, and `round_predictions_history_root` is passed explicitly
(the publisher derives it from `dirname(output_root)` when NULL, which under
staging would be the temp tree). Two more first-publish rules from the same
day: a division with no played match stamps `as_of` with the snapshot date,
and a division's team set is the season's teams (results union scheduled
fixtures), so `team_strengths`, `home_advantage`, `points_distribution` and
`final_positions` always name the same teams.

