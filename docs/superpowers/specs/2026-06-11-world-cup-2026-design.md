# World Cup 2026 forecast — design

**Status:** shipped (initial), 2026-06-11. **Scope:** add a Bayesian forecast of
the 2026 FIFA Men's World Cup (group + knockout) to the Metill sports site,
reusing the existing football model. Decision context: PR thread / session
2026-06-11.

## Goal

Forecast the full tournament — group-stage qualification probabilities feeding
knockout-stage progression through to the final — and present it on metill.is,
inspired by the Icelandic football pages but visually distinct (a global
tournament, not a domestic league table). Refreshes as results arrive; a
headline feature is **model-vs-actual over/under-performance** as the tournament
progresses.

## Key decisions

- **Reuse the model unchanged.** The country-agnostic
  `Stan/football_iceland/bivariate_poisson_no_inflation.stan` is fit on
  international results — *no new model*. The random-walk-over-rounds structure
  is a feature here: recent friendlies anchor round-one strength, and each
  team's strength updates as WC matches accumulate. (User instruction; see
  memory `feedback-reuse-model-new-domains`.) **Validated:** fit on 2,059
  internationals (193 teams, N_rounds 77) gave 9/3000 = 0.3% divergences, well
  under the 1% gate — the sparse-data funnel risk did not materialise.
- **Training data:** martj42 bulk international-results CSV (goals-only, full
  history) → `data/wc/raw/`. Window 2022+; keep only matches involving a 2026
  WC team with both teams above an activity floor (the Iceland `training_filter`
  philosophy — align training and prediction populations). The CSV already
  carries the 72 WC group fixtures, so the fixture list is free.
- **Integration, not a bypass.** International results land in the canonical
  facts store under `sport=football / country=world / sex=male`, so the standard
  `prepare_data()` → `fit_model()` flow runs unchanged. `division` carries the
  competition type (the football model ignores it).
- **Dedicated WC simulator** rather than generalising the Iceland cup simulator:
  the WC is a 32-team *fixed-tree* bracket seeded from group results, not a
  16-team randomly-redrawn cup. Reuses the match-level bivariate-Poisson
  primitives + the `tournament_placements.json` output contract; leaves the
  Iceland cup code (and its tests) untouched.
- **Analysis-only, no betting** for v1 (Approach A from the feasibility brief).

## Architecture

| Stage | Component |
|---|---|
| Ingest | `R/wc-ingest.R::wc_ingest_internationals()` → facts store |
| Structure | `R/wc-structure.R::wc_structure()` — groups, hosts, R32 slot map, FIFA Annex-C thirds table (`data/wc/structure/third_allocation.csv`, 495 rows), non-sequential R16→Final tree |
| Fit | `scripts/wc/fit.R` → `prepare_data` + `fit_model` (model unchanged) → `data/wc/fit/` |
| Simulate | `R/wc-simulate.R::simulate_world_cup()` — per draw: group round-robins → tiebreak rank → 8 best thirds → Annex-C allocation → fixed knockout walk; aggregates group-finish + cumulative round-reached probabilities |
| Publish | `R/wc-publish.R::publish_world_cup()` → `data/publish/football/world/karla/{meta,groups,tournament_placements,team_strengths}.json` (Icelandic names at the boundary) |
| Render | `R/wc-render-html.R::wc_render_html()` + `inst/wc/forecast.template.html` → self-contained `data/wc/forecast.html` |
| Driver | `scripts/wc/forecast.R` (post-fit pipeline) |

Tests: `tests/testthat/test-wc-structure.R`, `test-wc-simulate.R` (group
ranking, thirds matching, Annex-C lookup, coverage/sum invariants, cumulative
monotonicity, dominant-team favouritism).

## Documented v1 simplifications (refinements, not blockers)

- **Neutral-venue handling.** The model applies nominal home advantage in
  training; WC predictions use the *neutral* `cur_offense/cur_defense`, so this
  is a second-order dilution. Hosts (MEX/CAN/USA) get home advantage in their
  group matches; knockout is neutral for all. A proper per-match neutral
  indicator is a v2 model refinement (would require touching the Stan model —
  hold for explicit approval).
- **Group tiebreak** is points → GD → GF (the 2026 head-to-head-first rule is a
  refinement; rarely changes the qualifier *set*, mostly seeding).
- **AET/penalty scores** in training are stored as final scores without a
  knockout flag (martj42 limitation); for a knockout-heavy tournament this
  slightly biases the goals distribution — a v2 data-cleaning item.

## v2 additions (2026-06-11, same session)

Publish moved to its own namespace **`data/publish/world_cup/karla/`** (sport
segment `world_cup`) so the WC JSONs never collide with the Iceland football
schemas — the validator keys schemas on `<sport>/<filename>`, so files under
`football/world/` were wrongly validated against Iceland's strict
`football/{meta,team_strengths}.schema.json`. The new namespace is unmatched
(informational) until WC schemas are written under
`config/publish-schemas/world_cup/`.

Four reader-facing views, all driven by richer per-draw simulator output:

1. **Upcoming-match predictions** (`predictions.json`) — per fixture P(home /
   draw / away) + expected goals (host advantage applied in the group stage).
2. **Group overviews + projected final tables** (enriched `groups.json`) —
   per team projected final points/GF/GA, a finishing-position distribution bar,
   P(advance), plus **xG / xPts / over-under** accumulated over played matches
   (empty until matches play; the fit at launch *is* the pre-tournament
   baseline, so xG-vs-actual is a clean "performance vs expectation" signal —
   freezing a baseline as the model updates is the live-loop refinement).
3. **Interactive what-if bracket** (`bracket.json`) — the headline feature.
   *Forward bracket model* (user-chosen over per-draw conditioning). The
   simulator exports a 48×48 head-to-head win matrix `W[a][b] = P(a beats b)` in
   one neutral knockout match, plus the R32 slot occupancy (who fills each of the
   32 entries). Knockout shortcut: under the trivariate-reduction bivariate
   Poisson the shared component cancels in the goal margin, so P(a beats b) is a
   **Skellam** probability of the two independent rates — exact-marginal, and
   independent of the correlation parameter. The page lays out the classic
   32→Final tree (left/right halves converging on the centre) and propagates the
   occupancy forward through `W`; **pinning a winner** forces that match's outcome
   and re-propagates downstream, also reporting the joint probability of the
   pinned scenario. Per-match renormalisation handles the bracket-independence
   leak (a strong team can reach a deep match from either half, so the excluded
   "team plays itself" mass is redistributed). **Light + smooth**: 24 KB (vs ~0.8
   MB for per-draw), no draw-count collapse on deep pins, and the same forward
   model drives the leaderboard + heatmap + bracket so all three agree.
   Approximation cost measured at <1pp on champion odds vs the exact sim.

Simulator (`simulate_world_cup`) returns `group_probs` (+ projected
points/GF/GA), `placement_probs` (from the forward model), `predictions`,
`performance`, and `bracket_model` (W + R32 occupancy + structure).
`wc_forward_bracket()` is the propagator, used in R for the static numbers and
mirrored in the page JS for the live what-if. Tests cover predictions
sum-to-one, projected-points bounds, win-matrix complementarity, occupancy
sums-to-one, forward-bracket monotonicity/sum-to-one, and pin forcing.

## Follow-ups

1. metill-platform consumer route (`app/routes/hm2026.py` + Jinja template
   reusing this page's design + the interactive bracket JS, reading
   `data/ithrottir/world_cup/karla/`) — the production path; data flows via the
   hourly `pull-sports-data.yml` rsync.
2. Live-update loop: re-ingest martj42 as matches play → refit → republish; and
   freeze a pre-tournament strength baseline for the over/under panel.
3. WC publish schemas under `config/publish-schemas/world_cup/` for enforcement.
4. Polish: cap/segment the 72-card upcoming list (currently next 12); 2026
   head-to-head-first group tiebreak; per-match neutral indicator in the model
   (needs approval — it touches the Stan model).
5. Optional: betting integration (Approach C) only if a clear edge appears.
