---
paths:
  - "R/publish-*.R"
  - "R/extract-*.R"
  - "R/publish-pipeline.R"
  - "data/publish/**"
  - "data/beliefs/extracts/**"
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

Five optional keys on a `publish_divisions` entry, all absent-safe:

| Key | Contract |
|---|---|
| `code_badge` | Short ASCII badge emitted as `next_games.json::division_code`, which the publish schemas pattern as `^[A-Z][A-Z0-9_]*$`. Basketball's code `1D` fails that on its own (leading digit), which is why the key exists. Absent falls back to `code`. Every entry carrying a `split` also derives `<code>_UPPER_PO`/`_LOWER_PO` → `<badge>U`/`<badge>L`. |
| `expected_meetings` | Times each pair meets in the **regular** season. An assertion and a fallback, **never the source** — `n_rounds` is derived from schedule + results (spec §12). Omit where the format is genuinely irregular (basketball female 1D). |
| `regular_season_rounds` | The last regular round, **stated outright**. Unlike `expected_meetings` this IS a source: it sets both `n_rounds` and the `cut`, ahead of the meetings derivation and ahead of the schedule. One cell carries it — basketball female 1D, see below. |
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

Basketball female 1D is the cell where no meetings-per-pair constant works.
Measured on `data/facts/results` season 2026 (2026-09-05): rounds 1-18 are 89
matches over 10 teams (44 pairs twice, 1 pair once), and rounds 19-24 are an
embedded 4-team promotion playoff — Þór Ak. v Fjölnir, Hamar/Þór v Selfoss,
then Hamar/Þór v Fjölnir — which brings in an **eleventh** team, Hamar/Þór,
who plays no regular round at all. So `expected_meetings * (n_teams - 1)` is
unusable in both directions. Left to the schedule derivation the cell published
`n_rounds` 24 and `meta.round` **6** — the floor over appearances, i.e.
Hamar/Þór's six playoff games — for a season that had finished, with the
playoff tabled as regular season. With `regular_season_rounds: 18` the cut
drops 98 rows to 89 and `meta.round` reads 17 (89 matches over 10 teams is 17.8
appearances each, and the round is the floor).

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

**Seasonally paused.** Icelandic basketball + handball regular seasons
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

### `fit_meta.parquet` is partition-level (since 2026-09-04)

One row per `fit_date=` partition on all three sports: `n_draws` (integer),
`fit_date` (Date), `stan_model` (character, from `leagues.yml`),
`model_units` (character — `points` basketball, `goals` handball,
`log_rate` football). It is the ONLY file in a partition with no
`division` column, so it must never enter an extractor's division-keyed
write loop and the reader must not split it — a split filters it to zero
rows on every cell. `model_units` comes from the SPORT, not from config:
the 2DT models are additive in raw points/goals while football's
bivariate Poisson is on the log scale, and reading that off the wrong
sport is the B5 bug wearing a metadata label.

### Home-advantage units are per-sport, and both halves are unit-tested

Football's home advantage is a LOG-rate: `.extract_home_advantage_draws_pfi()`
(`R/publish-iceland-league.R`, beside its sibling `.extract_team_draws_pfi()`)
publishes `exp(x)` for offence and defence and `exp(x / 2)` for the total. The
2DT sports' `home_advantage_*` are raw points/goals:
`.extract_home_advantage_draws_2dt()` (`R/extract-iceland-2dt-shared.R`)
publishes the parameter itself. That asymmetry IS B5, and each direction has
its own test — `test-extract-football-home-advantage-units.R` and
`test-extract-2dt-home-advantage-units.R`.

**The golden manifest does not cover this.** It was cited as football's
regression net until 2026-09-05; it is not. `build_football_extracts_fixture()`
synthesises `home_advantage_quantiles` closed-form and hardcodes
`model_units`, so the golden test holds no fit and calls no extractor —
rebinding `extract_football_iceland()` to a function that `stop()`s leaves all
21 of its assertions green. Its 92 hashes pin `publish_iceland_league()` only.
Anything in the EXTRACT layer needs a test that actually runs it, which is why
football's pull is a named internal rather than a closure.

The reader surfaces it WHOLE, as `read_extracted_iceland()$fit_meta`, next to
`sim_inputs` and `cup_bracket` — never inside a per-division slot. Running it
through the split filtered it to zero rows on every cell, which is why every
basketball and handball cell published `n_draws: 0` until 2026-09-04.
`.read_partition_extract()` aborts if a partition-level file ever grows a
`division` column, so the next such file cannot repeat it.

`meta.json::n_draws` resolves in this order: football's per-fit `sim_inputs`
scalar table, then its scoreline-count sum, then `fit_meta$n_draws`. fit_meta is
authoritative and last on purpose — on a real football partition all three
agree, so ordering it first would move no production number but would move the
pinned fixture, whose synthetic counts round to 48 against a fit_meta of 50.

It stays in `sport_publish_profile()$optional_extracts` for every sport.
`required_extracts` drives the reader's partition-completeness check, so
promoting it would mark every football partition written before the
contract existed incomplete — i.e. retire the replay history the extracts
tree exists for. `round_strengths_quantiles` IS required for all three
sports, because no basketball or handball partition predates it.

### The regular-season boundary is applied at extract time too

Basketball embeds its úrslitakeppni in the league division (KKÍ packages
it as extra rounds inside the same `season_id`), so the 2DT extractor
cuts before computing base points, the placement simulation and the round
trajectory. There is exactly ONE boundary function in the repo —
`.publish_n_rounds()` / `.regular_season_results()` in
`R/publish-format.R` — and both the extractor and the publisher call it,
because the two cuts must be the same cut or standings and
`final_positions` disagree about which matches counted. Handball needs no
cut: its playoff is a separate division (`PO`), already excluded by the
division filter. Measured 2026-09-04 on season 2026: basketball male BD
162 → 132 rows, male 1D 159 → 132, female BD 137 → 90, female 1D 98 → 98
(unset `expected_meetings`, so the schedule derivation was the source); all
four handball cells unchanged. Re-measured 2026-09-05, female 1D now cuts
98 → 89 off the `regular_season_rounds: 18` it gained.

`predicted_matches.parquet` is built from the UNCUT fixture set — a next
game is a next game — while the league-table simulation caps upcoming
fixtures at the boundary.

**Only a CONFIGURED boundary deletes played rows.** `.regular_season_cut(rows,
format)` cuts at `.publish_n_rounds()$cut`, which is `n_rounds` when
`source == "config"` — a stated `regular_season_rounds`, or the
`expected_meetings` derivation — and `NA` otherwise. A schedule-derived `n_rounds` is
computed FROM the played and scheduled rows, so cutting those same rows by it
is circular: it can never identify a post-season row, and it CAN delete
regular-season rows wherever `round` is stamped on a different axis from
appearance counting. Measured 2026-09-04: the schedule branch is the identity
on real data in every cell (all nine football cells, basketball female 1D
98 → 98), while the ungated filter deleted one played match from six football
cells and one basketball cell of the synthetic fixture. The FORWARD half
(`.regular_season_game_nrs_2dt()`) is deliberately NOT gated — capping how many
fixtures remain is a question about season LENGTH, which both sources answer.

## meta.json v2 + the D3 relabel (since 2026-09-04)

Every published cell of all three sports is self-describing, so no consumer
does league arithmetic. `metill-platform` used to compute
`total_rounds = 2 * (n_teams - 1)` (`ithrottir.py:406`) and
`max_points = round_num * 3` (`og.py:696`); both are facts the producer can see
in the data and the consumer cannot.

`.build_publish_meta()` (`R/publish-format.R`) copies the v1 ten-key block
VERBATIM — key order is part of the payload identity, because
`publish_json_digest()` hashes the parsed list — and appends, in this order:

| key | contract |
|---|---|
| `n_rounds` | integer or null. Cups are null. |
| `n_rounds_source` | `config` / `schedule` / `none` / `not_applicable`. |
| `units` | `{strength, home_advantage, diff_bin_width}` from the profile. |
| `points` | `{win, draw, loss}`; basketball's `draw` is **null**, not 0. |
| `season_scope` | `full_season` (football) / `regular_season` (bb+hb). |
| `postseason` | null (football) or `{name_is: "Úrslitakeppni", modelled: false}`. |
| `qualify` | null, or `{slots, label_is}` from `.iceland_division_qualify()`. |
| `relegation` | `{slots}`, null where unconfigured. |

The builder ABORTS when `round > n_rounds`: a published cell that would render
a negative "Umferðir eftir" is refused at the producer rather than clamped at
the consumer.

`final_positions.json` carries a top-level `basis` (`final_table` /
`regular_season_table`) and its summary is built by
`.build_placement_summary()`:

- `p_top_of_table` = P(placement == 1) — every sport, under a name that cannot
  be misread as *Íslandsmeistari*.
- `p_winner` — **only** when `basis == "final_table"`. For basketball and
  handball the league table decides the *deildarmeistari*; the Íslandsmeistari
  comes out of an úrslitakeppni this model does not simulate (spec §15, D3).
- `p_qualify` — only where the division configures `qualify`; football Besta
  deild alone today, where it equals `p_top_six` exactly.
- `p_top_six` — football only, the literal `placement <= 6L`, a DEPRECATED
  alias kept because metill-platform reads it. It is not derived from
  `qualify`, so the five football cells with no configured cut keep it.
- `p_relegation` — `relegation_slots` unset keeps football's published
  `placement >= n_teams - 1` expression verbatim; `0` publishes zeros,
  present-and-zero rather than a missing key.

`points_distribution.json`'s summary carries the placement columns it has
always carried (football `p_top_six`/`p_winner`/`p_relegation`, bb/hb
`p_top_of_table`/`p_relegation`) and gains no new key: it is one of the eight
artefacts the golden manifest asserts byte-identical across the v2 change.

**No `p_top_six`, `p_playoff` or qualification probability is emitted for
basketball or handball.** Measured on season 2026, the four basketball cells
qualify 8 of 12, 8 of 12, **10 of 10** and 4 of 11 teams for the post-season —
four cells, four structures, and the women's Bónusdeild takes every team
through. No per-division integer expresses that, and shipping one is the
"top-six number wearing a playoff label" failure D3 exists to prevent.

The venue lookup in `next_games.json` is football's alone
(`.publish_venues_pfi()`, `R/publish-next-games.R`): Valur, KA, Fram, ÍBV,
Stjarnan and Breiðablik field handball and basketball teams under the same club
name, so joining the static male-top-flight ground table on another sport would
publish an outdoor football ground for an indoor fixture.

**Schema state (2026-09-04).** The v2 keys are typed in
`config/publish-schemas/_base/meta.json` and rendered into all three sports.
They are REQUIRED for basketball and handball, whose cells are new and emit the
full contract from their first publish, and OPTIONAL for football, whose live
tree was published before v2 landed. `.validate_or_abort()` validates the
publishing sport's whole subtree, not just the cell it wrote, so requiring a v2
key of football today would make its next publish abort on its own
not-yet-republished siblings — male failing on the four female cells and vice
versa. Tightening football's `required` is a follow-up for whenever its tree is
next fully republished.

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
