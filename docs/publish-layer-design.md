# Publish layer: design notes

Moved verbatim from `.claude/rules/publish-layer.md` on 2026-09-23; the rule keeps the invariants.
Each section keeps its original date.

### Division accessors are sport-neutral (since 2026-09-04)

`publish_divisions` is no longer football's alone. Every Icelandic
league declares its own per-sex publish cells:

| League | male | female |
|---|---|---|
| `football_iceland` | BD, LD1, LD2, LD3, CUP | BD, LD1, LD2, CUP |
| `basketball_iceland` | BD, 1D | BD, 1D |
| `handball_iceland` | OD, G66 | OD, G66 |

They are read through eleven accessors in `R/publish-divisions.R`, all
`.iceland_division_*(key, sex)` where `key` is a `leagues.yml`
top-level league key: `codes`, `slugs`, `labels`, `split`, `badges`,
`is_cup`, `qualify`, `relegation`, `regular_season_rounds`,
`expected_meetings`, `preseason_hold`. The
football-only `.football_iceland_division_*` helpers they replace are
**deleted, with no compatibility aliases** — two live names for one
symbol is the drift this removed. Adding a publish cell is a config
edit plus a metill-platform `DIVISIONS` entry, never an R edit.

Six optional keys on a `publish_divisions` entry, all absent-safe:

| Key | Contract |
|---|---|
| `code_badge` | Short ASCII badge emitted as `next_games.json::division_code`, which the publish schemas pattern as `^[A-Z][A-Z0-9_]*$`. Basketball's code `1D` fails that on its own (leading digit), which is why the key exists. Absent falls back to `code`. Every entry carrying a `split` also derives `<code>_UPPER_PO`/`_LOWER_PO` → `<badge>U`/`<badge>L`. |
| `expected_meetings` | Times each pair meets in the **regular** season. Football: an assertion and a fallback, **never the source** — `n_rounds` is derived from schedule + results (spec §12). Basketball/handball: one of three meetings sources, ranked by `.division_format_2dt()` (see *2DT season and format* below) — the season's own fixture list overrides it only on a roster-size change. Omit where the format is genuinely irregular (basketball female 1D). |
| `regular_season_rounds` | The last regular round, **stated outright**. Unlike `expected_meetings` this IS a source: it sets both `n_rounds` and the `cut`, ahead of the meetings derivation and ahead of the schedule. One cell carries it — basketball female 1D, see below. |
| `qualify` | `{slots, label_is}`. Absent = `meta.qualify: null` and **no** `p_qualify`. It is the generic replacement for football's `p_top_six`, which does not transfer: Bónusdeild karla is 12 teams with 8 qualifying, and Bónusdeild kvenna carries all 10 through. |
| `preseason_hold` | Basketball/handball only. A season (integer) the division is pinned to: `.current_season_2dt()` ignores its schedule and resolves to the latest season at or before the hold with played results, even once the next season starts. A hold on a season the division has no results for is inert. Basketball female 1D sets `2026` (its 2027 aliases and format are unresolved), so the cell keeps publishing 2026 and metill-platform keeps it out of view. Remove the key to release it. |
| `relegation_slots` | Teams relegated from this division. Replaces the hardcoded bottom-two rule (`placement >= n_teams - 1L`), which is wrong for a bottom-tier division where nothing is relegated. Absent = `meta.relegation: null` and **no** `p_relegation` — the same absent-means-omitted rule `qualify` follows. Football's nine cells are the one exception, see below. |

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
(`config/leagues.yml` — its sole occurrence). Both Iceland extractors
keep two result sets: `model_results` (`model_training_results()`, which
also drops forfeits) for the strength trajectory, and every played result
(`.played_results()`) for tables, seasons and remaining fixtures — a
forfeit win is two points in the table and a pairing that must not be
simulated again. The 2DT extractor still asserts
`is.null(league$training_filter)`: no 2DT test covers a division whose
filtered-out teams still play in it, so adding a filter to basketball or
handball aborts that extractor until one does.

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
(`.remaining_fixtures_2dt()`, which replaced `.regular_season_game_nrs_2dt()` on
2026-09-16) is deliberately NOT gated — capping how many fixtures remain is a
question about season LENGTH, which both sources answer: with meetings unknown
it caps each side's schedule at `n_rounds` (`max_games`).

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
| `n_rounds_meetings_source` | Basketball/handball only, directly after `n_rounds_source`: where the meetings count behind `n_rounds` came from — `schedule` / `config` / `prior_results` / `none` / `not_applicable`. Absent on football, whose key order the golden manifest hashes. |
| `units` | `{strength, home_advantage, diff_bin_width}` from the profile. |
| `points` | `{win, draw, loss}`; basketball's `draw` is **null**, not 0. |
| `season_scope` | `full_season` (football) / `regular_season` (bb+hb). |
| `postseason` | null (football) or `{name_is: "Úrslitakeppni", modelled: false}`. |
| `qualify` | null, or `{slots, label_is}` from `.iceland_division_qualify()`. |
| `relegation` | `{slots}`, null where unconfigured. |

The builder ABORTS when `round > n_rounds`: a published cell that would render
a negative "Umferðir eftir" is refused at the producer rather than clamped at
the consumer.

## 2DT season and format (since 2026-09-16)

Basketball and handball (`profile$season_rule == "schedule_aware"`) resolve
the season, the format and the remaining fixtures differently from football,
which keeps `max(results$season)` (D5). Spec:
`docs/superpowers/specs/2026-09-16-2dt-full-season-projection-design.md`.

- **Season.** `.current_season_2dt()` (`R/season-structure-2dt.R`) is the
  latest season with a played result on or before `end_date`, or with a
  fixture after it, so a published next season is current before its first
  match. `preseason_hold` pins a division (above).
- **The extract decides the published season.** A new season's schedule lands
  weeks before a refit can use it, while the publisher runs several times a
  day. So the 2DT extractor stamps `final_positions.parquet` and
  `points_distribution.parquet` with a `season` column (the season it
  simulated), `read_extracted_iceland()` lifts it into a per-division
  `simulated_season` integer and drops the column (no published record gains
  a key), and `.publish_season_2dt()` labels the whole cell with it — meta,
  round, format, standings, history, team list. When the resolver has moved
  past the extract it only logs that a refit is due. An extract without the
  stamp (written before 2026-09-16) publishes under the old results-only rule.
  Without this, last season's table went out as the new season at round 0 and
  wrote a permanent round-0 heatmap step.
- **Format.** `.division_format_2dt()` ranks the meetings-per-pairing sources:
  the season's own fixture list, when it is a complete, self-consistent list
  AND either agrees with `expected_meetings` (or none is set) or the roster
  size changed since the last completed season (F17: women's Olísdeild 8 → 10,
  triple → double round robin); then `expected_meetings`; then the last
  completed season's results. A stated `regular_season_rounds` makes the
  meetings `not_applicable`. The source is published as
  `meta.n_rounds_meetings_source`.
- **Pre-season history.** With no played match in the season,
  `final_positions_history.json` rows carry the extract's fit date as `as_of`,
  so republishing one fit replaces its round-0 step instead of adding one.
- **Team list.** The season's results ∪ its schedule rows, so
  `home_advantage.json` covers every team before the first fixture enters
  Stan's 14-day window.

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
- `p_relegation` — emitted **only** where the division configures
  `relegation_slots`, exactly as `p_qualify` is emitted only where `qualify` is
  (ID-B15). `0` publishes zeros, present-and-zero rather than a missing key.
  All eight bb/hb cells leave it unset — no KKÍ or HSÍ regulation was
  resolved — so **none of them emits `p_relegation`**; for a bottom-tier
  division (basketball 1. deild, handball Grill 66) nothing is relegated at
  all, so football's hardcoded rule published a "Fallhætta" headline that was
  false, not merely uncertain. Football's nine cells keep that expression
  verbatim under `emit_legacy_relegation`, a DEPRECATED alias on the same
  footing as `emit_top_six_alias`, because they are live and metill-platform
  reads the key; retire it by configuring `relegation_slots` on them, which
  reproduces the same numbers from a stated fact. Consequently `p_relegation`
  is `required` in football's `final_positions` / `points_distribution`
  schemas and merely optional in basketball's and handball's.

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
