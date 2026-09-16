# 2DT sports: a real full-season projection, including pre-season

**Status:** design approved 2026-09-16. Amends **D3** of
`docs/superpowers/specs/2026-09-02-basketball-handball-metill-parity-design.md`, which promised a
regular-season projection but wired it to Stan's 14-day prediction window.

**Deadline:** the first 2027 basketball game is 2026-09-29 (women's BD), men's on 2026-09-30.
Merged code plus a completed refit is needed by about **2026-09-26**.

## Scope and fixed decisions

These were decided with the owner before this spec was written and are requirements, not options.

- **D1 — learn from football.** Football already fixed this exact bug with
  `simulate_league_season()`. Generalise that simulator; do not write a parallel 2DT copy.
- **D2 — football stays byte-identical** through the refactor.
- **D3 — strengths are frozen** at the last fitted round, as football does. Projecting random-walk
  drift forward is a follow-up that would improve both sports.
- **D4 — football's exact-tie handling is left as it is** (see follow-ups).
- **D5 — season detection changes only for the 2DT sports.** Football keeps its own path.
- **D6 — `needs_refit()` learns about fixtures entering the horizon.** A one-off forced dispatch would
  leave the same gap for every future season rollover.
- **D7 — cells in scope for the deadline:** basketball men's BD and 1D, basketball women's BD, and
  all four handball cells. **Basketball women's 1D is held** (two teams without history, drifting
  reserve-side names). metill-platform#58's `min_season` gate cannot hold it on its own: once §5
  lands, that division would resolve to 2027 and the gate would lift. The hold therefore lives in
  sports (§5.1), keeping the division on 2026 so the platform gate keeps it hidden.
- **D8 — teams without history** enter with a below-average prior (§7).

## Verified findings

Every row was checked against `origin/main` (sports `d6d5c813d` / `369b31d35`) or live data on
2026-09-16, not inferred.

| id | finding | evidence |
|----|---------|----------|
| F1 | 2DT final positions cover only ~14 days, not the season | `fit_league()` horizon defaults to 14 (`R/model-league.R:103`); `prepare_data()` keeps fixtures to `end_date + 14` (`R/model-prepare.R:148-157`). Handball karla-od `points_distribution` support is `{0..8}` at round 2 of 22. |
| F2 | Football had exactly this bug and fixed it | Header of `R/simulate-league-season.R`: the old logic "only integrated over the matches in the model's 14-day prediction window (~2 rounds) and therefore reported 'position after the next ~2 rounds' mislabelled as the final table." |
| F3 | Every Stan model freezes strength when predicting | football, handball and basketball generated quantities all read `offense[N_rounds]`; the prediction time gap is never used. A season-wide Stan window would give the same frozen distribution as an R simulation. |
| F4 | The simulator's scoring model is hard-coded | `.simulate_match_goals_slss()` is called directly (`R/simulate-league-season.R:209, 259`); 3/1/0 points are inline; the ranking uses a packed key `pts*1e6 + gd*1e3 + gf` (`:244, 290, 292`). |
| F5 | The packed ranking key is wrong for continuous scores | A basketball season's goals-for is ~2000, so gf swamps goal difference. Football's small integer scores are unaffected. |
| F6 | Football's simulator has 48 expectations | `tests/testthat/test-simulate-league-season.R` — the regression net for D2. |
| F7 | A team without strength draws empties the whole table | `R/simulate-league-season.R:170-179`; the football caller publishes an empty `final_positions` (`R/extract-football-iceland.R:277-310`). |
| F8 | The season is chosen from played results only | `current_season <- max(results$season)` at `R/extract-iceland-2dt-shared.R:529-533` and `R/publish-iceland-league.R:856`. With no 2027 results, both give 2026. |
| F9 | A per-team cap drops returning teams' new-season fixtures | `.regular_season_game_nrs_2dt` (`R/extract-iceland-2dt-shared.R:411-448`) counts from last season's games. |
| F10 | Team lists come from results only; unknown teams' fixtures are dropped | `R/model-prepare.R:129-131, 189-193`. |
| F11 | 2DT home advantage is in raw points/goals | Never exponentiated or halved (`R/extract-iceland-2dt-shared.R:261-284`); a past bug published values up to 420. |
| F12 | Both 2DT models carry a per-season scoring trend | `delta_mean_goals`, `sigma_mean_goals` in both `.stan` files; football's model has no season structure at all. |
| F13 | The zero-standings publish branch wipes `standings_history.json` | `R/publish-iceland-league.R:1320-1338`. Its comment says it is for cups, but it fires for any cell with no standings rows — including a league's first pre-season publish. |
| F14 | An `N_pred` mismatch silently empties predictions | `R/publish-iceland-2dt-helpers.R:81-98`. |
| F15 | Pre-season, basketball is never refit | `needs_refit()` is FALSE without new games (`R/pipeline-freshness.R:22-60`); `fit_skip_reason()` skips (`:152-161`) even once fixtures enter the 14-day window (`:98-118`). |
| F16 | Several 2027 handball schedules are incomplete | men's OD 128 of 132 fixtures, men's G66 131 of 132, women's G66 83 of 84 (triple round-robin). |
| F17 | **Women's handball OD changed format between seasons** | 2026: 8 teams, triple round-robin (config `expected_meetings: 3`; `.division_rr_multiplicity_pfi` derives 3 from 2026 results). 2027 schedule: **10 teams, 90 fixtures, 18 each — a double round-robin.** Live `kvenna-od` reads round 2 **of 27**, with `n_rounds_source: "config"`; the season is 18 rounds. |
| F18 | Stan comments give the wrong time unit | "per sqrt-week" (basketball `.stan`:68; handball :99-100), but gaps are **days** (`R/model-prepare.R:217-219`). |
| F19 | Live defects today | basketball publishes season 2026 as current; `kvenna-bd` has an empty table (`n_teams: 0`). |

## 1. Problem and end state

Today the 2DT "final positions" and "points distribution" are realised points plus roughly two rounds
of Stan predictions (F1), and before a season starts the pipeline cannot see the new season at all
(F8, F9, F10, F15). The result on metill.is is a mislabelled table in-season and last season's final
table pre-season.

**End state.** For every in-scope 2DT cell, `final_positions` and `points_distribution` describe the
whole regular season: realised results plus a simulation of every remaining regular-season fixture.
Pre-season that means season 2027, round 0, every scheduled team present, and a full projected
table. `next_games` keeps its 14-day window. `final_positions.basis` stays `"regular_season_table"`
(parity spec D3) — only its depth changes.

## 2. Architecture: one simulator, two sports

`simulate_league_season()` becomes sport-agnostic by taking three injected behaviours:

| argument | football (default, unchanged) | 2DT |
|---|---|---|
| `match_fn` | `.simulate_match_goals_slss` (bivariate Poisson) | `.simulate_match_scores_2dt` (§4) |
| `points_fn` | 3/1/0 on exact equality | `.points_2dt(has_ties, tie_threshold)` |
| `tie_break` | `"first"` (team order), as today | `"jitter"` (seeded), as the 2DT ranker already does |

The 2DT extractor stops passing Stan's `posterior_goals` to `.compute_final_positions_2dt` /
`.compute_points_distribution_2dt` and passes the simulator's per-draw tables instead. Stan's 14-day
`pred_d` continues to feed `next_games` and nothing else.

`split_format` stays `NULL` for 2DT: these cells publish the regular-season table only.

## 3. The simulator refactor (football byte-identical)

- `match_fn(off_h, def_h, off_a, def_a, scalars, n)` returns home and away scores for one fixture
  across `n` draws. Home advantage is applied by the caller, as today.
- `points_fn(g_h, g_a)` returns home and away points.
- **Ranking.** Replace the packed key with a lexicographic order over points, then goal difference,
  then goals for. With `tie_break = "first"` the result is identical to the packed key for every
  football table (integer scores within the key's documented range); `"jitter"` adds a seeded random
  final key.
- `gd`/`gf` accumulate as double so continuous scores are not truncated.
- The required-column check on `sim_inputs_scalar` moves into the match function, which knows what it
  needs; football's current columns remain required on the football path.

**Guard:** the 48 existing expectations must pass unchanged, and a golden check asserts football's
published `final_positions.json` / `points_distribution.json` are byte-identical before and after.

## 4. The 2DT match generator and its inputs

**Inputs.** `.extract_sim_inputs_2dt(fit, sport)` returns the team tibble the simulator expects
(`cur_offense`/`cur_defense` from `cur_*_away`, plus raw-points `home_advantage_off/def`) and the
likelihood scalars each model needs:

- both: `nu`, `mean_goals[N_seasons]`, `delta_mean_goals`, `sigma_mean_goals`
- handball: scalar `rho`, per-team `sigma_team[k]` (a team-level column)
- basketball: scalar `sigma`, and `alpha_rho`, `beta_rho`, `beta2_rho`, `beta3_rho`

**Generator.** `.simulate_match_scores_2dt()` reproduces each model's generated quantities
(basketball `.stan`:302-322, handball :363-380) across all draws at once:

- **Home advantage** is added to **both** the home side's offence and its defence, in **raw points
  (F11)**: `off_h = off + home_advantage_off`, `def_h = def + home_advantage_def`. The away side is
  unadjusted.
- **Means:** `mu_h = mean_goals + off_h − def_a`, `mu_a = mean_goals + off_a − def_h`.
- **Scales and correlation.** Basketball: `s_h = s_a = sigma`, and a per-fixture
  `rho = 2·inv_logit(alpha_rho + beta_rho·d + beta2_rho·t + beta3_rho·t·d) − 1`, where
  `d = |off_h + def_h − off_a − def_a|` and `t = |off_h + def_h + off_a + def_a|`. Handball:
  `s_h = sigma_team[home]`, `s_a = sigma_team[away]`, scalar `rho`.
- **Draw:** `z1, z2 ~ N(0,1)`; `e_h = s_h·z1`; `e_a = s_a·(rho·z1 + sqrt(1 − rho²)·z2)`; then
  `home = mu_h + e_h·w`, `away = mu_a + e_a·w` with **one shared** `w = sqrt(nu / chisq(nu))` per
  draw. Independent chi-square draws would produce two univariate t's with the wrong joint tails,
  not the model's bivariate t.

**Pre-season scoring level (F12).** When the current season has no fitted level, step it forward
using the model's own trend: `mean_goals[N_seasons] + delta_mean_goals + sigma_mean_goals·z`, one
`z` per draw.

**Strengths are frozen (D3)** at `offense[N_rounds]` / `defense[N_rounds]`, as football does.

## 5. Season detection (2DT only)

`.current_season_2dt(results, schedules, end_date, division)`: the latest season that has either
results or scheduled fixtures after `end_date`. It replaces both `max(results$season)` sites for the
2DT path only (D5); football's publisher branch is untouched. One function owns the rule so the
extract and publish layers cannot disagree.

### 5.1 Holding a division

A `publish_divisions` entry may set `preseason_hold: true`. `.current_season_2dt()` then ignores that
division's future schedule and keeps resolving to its last results season. Basketball women's 1D sets
it (D7): the division keeps publishing 2026, so metill-platform#58 keeps it hidden. Removing the flag
releases the division once its team aliases are resolved.

## 6. Remaining fixtures, derived structurally

Follow football: remaining fixtures = every ordered pairing not yet played × its multiplicity,
rather than whatever falls inside Stan's window. This removes the dependence on the 14-day window
and on the per-team cap (F9), and fills the undated fixtures (F16).

**Multiplicity comes from the new season first (F17).** In order:

1. the most common meetings-per-pairing count in the current season's schedule;
2. `.division_rr_multiplicity_pfi()` over prior seasons' results (football's helper);
3. config `expected_meetings`.

The source used is stamped next to `n_rounds_source` in `meta.json`, and `n_rounds` is recomputed
from the same multiplicity, so `kvenna-od` reads 18 rather than 27. Basketball women's 1D keeps its
stated `regular_season_rounds`, since it is held.

The division team list comes from the current season's schedule, not from results.

## 7. Teams without history (D8)

A scheduled team with no strength draws gets prior draws, per draw:

- **centre:** within each draw, rank the division's rated teams by total strength
  (`cur_offense + cur_defense`; a higher defence means fewer goals conceded), take the bottom two,
  and use the mean of *their* offence and the mean of *their* defence. Choosing the same two teams
  for both keeps offence and defence coherent;
- **spread:** about 1.5 × the division's between-team standard deviation;
- **step sizes and team sigma:** drawn from the fitted hyperpriors
  (`exp(mean_sigma_* + scale_sigma_*·z)`; handball `sigma_team` likewise).

The centre and the width are named constants so they can be tuned. This removes the failure in F7,
where one team without history empties the whole table. No Stan or data-prep change is needed for
the table; such a team still has no `next_games` rows until it has been fitted.

## 8. The refit trigger (D6)

`needs_refit()` additionally returns TRUE when the fixtures now inside the horizon include any that
the last fit's prediction set did not cover. "The last fit's prediction set" is the
`(home_team, away_team, match_date)` rows of the newest `predicted_matches.parquet` for that
`(sport, sex)` under the extracts tree; if no extract exists, the cell counts as uncovered. `fit_skip_reason()` then lets the daily run fit
basketball as soon as its first fixture enters the window, and every future season rollover
handles itself.

## 9. Landmines that must land first

1. **Protect `standings_history.json` (F13).** Truncate it on the zero-standings path only when
   `is_cup`. This must merge before §5, or the first pre-season publish erases each league's history.
2. **Pass `prep` from `fit_league()` into the extractor (F14),** so an extract run on a different day
   cannot silently empty `predicted_matches`.

## 10. The published contract

No new files or fields beyond `meta.json` recording the multiplicity source (§6). What changes is
content: `final_positions`, `points_distribution` and `final_positions_history` now span the full
regular season; `meta.round` is 0 pre-season; `meta.n_rounds` reflects the current season's format.
The schema keeps `basis: "regular_season_table"` for 2DT.

## 11. Testing

- **Football (D2):** the 48 simulator expectations unchanged, plus a golden byte-comparison of
  football's published league surfaces.
- **2DT equivalence:** cut the structural fixture list to the same 14-day window the Stan draws cover
  and assert the simulator reproduces today's `final_positions` within Monte Carlo error. This pins
  the copied likelihood (§4), including basketball's per-fixture `rho`.
- **Zero games played:** a synthetic league with a schedule and no current-season results publishes
  season 2027, round 0, every scheduled team, and a complete table.
- **Multiplicity (F17):** a league whose prior season was a triple round-robin and whose new schedule
  is a double round-robin resolves to 2, and `n_rounds` follows.
- **Incomplete schedule (F16):** a missing pairing is simulated anyway.
- **New team (§7):** a scheduled team with no draws appears in the table, below the division median.
- **Held division (§5.1):** a division with `preseason_hold: true` and a future schedule still
  resolves to its last results season.
- **Shared scaling (§4):** the simulated home/away scores are correlated at the model's `rho`, and
  their joint tails match a bivariate t (a regression test for independent chi-square draws).
- **History guard (§9.1):** a zero-standings league publish keeps `standings_history.json`; a cup
  publish still truncates it.
- **Refit trigger (§8):** a fixture entering the horizon that the last fit did not predict makes
  `needs_refit()` TRUE.

## 12. Rollout

1. Merge in workstream order: WS1 (§9), WS2 (§8), WS3 (simulator refactor), WS4 (2DT generator),
   WS5 (§5–§7), WS6.
2. The daily run fits basketball via the new trigger (or a forced `--league` dispatch if the deadline
   is tight), then `decide-publish` publishes and metill-platform pulls.
3. metill-platform#58's `min_season` gate lifts on its own for the in-scope basketball cells.
4. Verify on the live site before 2026-09-29: season 2027, round 0, full team lists, sane
   probabilities, and `kvenna-od` reading 18 rounds.

**Visible change:** handball's "Líkur á sæti" becomes a real season projection, so its numbers will
move noticeably.

## Workstreams

### WS1 — History guard and `prep` plumbing `[sports]`
§9. **Verification:** the history-guard tests in §11 pass; a forced extract on a later day still
produces `predicted_matches` rows.

### WS2 — Refit trigger `[sports]`
§8. **Verification:** the refit-trigger test in §11; a dry run of `scripts/03_fit.R` against today's
data plans a basketball fit rather than skipping it.

### WS3 — Generalise the simulator `[sports]`
§2–§3. **Verification:** 48 existing expectations plus the football golden check, both unchanged.

### WS4 — 2DT generator and inputs `[sports]`
§4. **Verification:** the equivalence test in §11, per sport.

### WS5 — Season detection, structural fixtures, multiplicity, new teams `[sports]`
§5–§7. **Verification:** the zero-games, multiplicity, incomplete-schedule and new-team tests.

### WS6 — Comment fixes `[sports]`
F18: "per sqrt-week" becomes "per sqrt-day" in both `.stan` files. Comments only: the model is
unchanged, though editing the file triggers one recompile.

### WS7 — Refit, publish, verify `[sports]` `[metill-platform]`
§12.

## Out of scope and follow-ups

- **Forward drift / between-season regression (D3).** Frozen strength makes a six-month pre-season
  forecast overconfident, because it ignores summer roster turnover. Football has the same weakness;
  fix both together using the fitted `sigma_off`/`sigma_def`.
- **Football's exact-tie bias (D4).** Team-order tie-breaking biased 2DT title races before the
  jitter fix; football still breaks exact ties that way.
- **Basketball women's 1D.** Needs an alias map for reserve-side renames (`Stjarnan U/u/b`,
  `Keflavík U/b`, `Njardvik b`, and whether `Þór Þ.` is Hamar/Þór) confirmed with KKÍ, and its
  stated `regular_season_rounds: 18` updated for the 12-team 2027 format (22 rounds).
- **Tie-threshold calibration** — `Sports/Knowledge/Sports Models/tie-threshold-scoring.md` in the
  Metill vault.
