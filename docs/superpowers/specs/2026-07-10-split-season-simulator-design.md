# Split-season simulator — design

**Date:** 2026-07-10
**Status:** approved (user-specified requirements; format facts verified per the
pre-condition in the task brief)
**Scope:** `R/simulate-league-season.R`, `R/extract-football-iceland.R`,
`R/backfill-final-positions.R`, `config/leagues.yml` (+ schema),
`config/publish-schemas/football/meta.schema.json`

## Problem

`simulate_league_season()` forward-simulates only the remaining fixtures of the
**regular** phase. For Besta deild karla/kvenna the season continues past the
regular phase into split groups (efri/neðri hluti) that decide the title and
relegation, so the published `final_positions` `placement = 1` is currently
*P(1st at the split)*, not *P(Íslandsmeistari)*, and `points_distribution`
support truncates at the split (Víkingur capped at 40 + 8×3 = 64 while the real
season runs to late October).

## Verified format facts (2026-07-10)

The vault note `Sports/Domain/Icelandic Football/Besta deild karla — Format`
flagged the split mechanics `⚠︎ óstaðfest`. All flagged facts are now verified
— primary evidence is our own ingested KSÍ data (`data/facts/results`,
divisions `BD_UPPER_PO`/`BD_LOWER_PO`, men 2022–2025, women 2023–2024) plus
KSÍ's published split tables (men's 2024 neðri hluti; women's 2025 tables):

| Fact | Men (BD karla) | Women (BD kvenna) |
|---|---|---|
| Teams / regular rounds | 12 / 22 (double RR) | 10 / 18 (double RR) |
| Split | top 6 / bottom 6 | top 6 / bottom 4 |
| Split-phase games | single RR: 5 each (rounds 23–27) | upper single RR: 5 each (rounds 19–23); lower single RR: 3 each |
| Carry-over | **full** — points, GD, GF continue (KSÍ 2024 neðri table: 27 games, 37 pts leader) | **full** (KSÍ 2025: efri leader 23 games / 56 pts) |
| Group locking | **locked** — neðri winner ranked 7th even with more points than efri last (2024: 37 > 34; 2025: 39 > 33) | same |
| Relegation | bottom 2 of neðri (places 11–12) | bottom 2 of neðri (places 9–10) |
| Split assignment tiebreak | points → GD → GF (2025: Fram/ÍBV both 29 pts split by GD) | same |
| 2026 confirmation | KSÍ schedule placeholders: split rounds 23–27, ending 24–25 Oct | split rounds 19–23; 10 teams confirmed in 2026 results |

**Home/away in the split phase is a fixed KSÍ template**, unanimous across all
6 observed (season × sex × group) datasets with 6-team groups and both observed
4-team groups. By group rank (1 = best regular-season finish in the group):

- 6-team group: 1 hosts {2,5,6}; 2 hosts {3,4,5}; 3 hosts {1,4,5};
  4 hosts {1,6}; 5 hosts {4,6}; 6 hosts {2,3} → home counts {3,3,3,2,2,2}.
- 4-team group: 1 hosts {2,3}; 2 hosts {3,4}; 3 hosts {4}; 4 hosts {1}
  → home counts {2,2,1,1}.

Pipeline facts confirmed alongside: the KSÍ ingest registry already scrapes the
split competitions (2026 IDs registered for both sexes); `training_filter`
already includes `BD_UPPER_PO`/`BD_LOWER_PO`, so the fit trains on split
matches; Lengjan odds comps for the split exist in config. Only the season
simulator (and sibling publish surfaces, see Out of scope) ignore them.

## Approaches considered

1. **Config-declared format + two-phase engine** *(chosen)* — split sizes in
   `leagues.yml`, engine simulates the split per posterior draw.
2. Infer format from the previous completed season's playoff divisions —
   rejected: our store is missing the women's 2025 playoff matches (KSÍ
   discovery-endpoint gap), so inference would wrongly conclude "flat" for
   women 2026; config is explicit and vault-verified.
3. Post-hoc adjustment of the published marginals — rejected: champion
   probability needs the joint distribution over split membership and
   carried-over points, which only exists per draw inside the engine.

## Design

### Config (`config/leagues.yml`)

`publish_divisions` entries gain an optional `split` object (absence = flat
league, current behaviour — this is the "parameterise by league format" knob):

```yaml
publish_divisions:
  male:
    - { code: BD, slug: bd, label_is: "Besta deild", is_cup: false,
        split: { upper: 6, lower: 6 } }
  female:
    - { code: BD, slug: bd, label_is: "Besta deild", is_cup: false,
        split: { upper: 6, lower: 4 } }
```

`config/leagues.schema.json` allows the optional object (`upper`/`lower`
integers ≥ 2). Split-phase games are structural (single round-robin within the
group) and carry-over is full — both hardcoded as the verified rule, not
parameterised (YAGNI until a league differs). Playoff result divisions are
derived as `paste0(code, "_UPPER_PO")` / `"_LOWER_PO"`, matching
`KSI_DIVISION_LABELS`.

### Engine (`simulate_league_season()`)

Two new optional arguments; `NULL` for both reproduces today's behaviour
bit-for-bit:

- `split_format = list(upper = <int>, lower = <int>)` — the league splits.
- `split_groups = tibble(team, group)` (`group ∈ "upper"/"lower"`) — membership
  when it is already decided (regular phase complete).

**Phase 1 (regular phase ongoing, `split_groups = NULL`):** simulate remaining
regular fixtures as today → per-draw packed-key ranking (pts → GD → GF, ties
deterministic) → per draw, placements 1..upper are efri, the rest neðri →
generate split fixtures from the home template applied to the draw's split
ranks → simulate those (same frozen-strength bivariate-Poisson) accumulating
into the carried pts/GD/GF → final ranking (below).

Vectorisation: iterate over template **rank pairs** (≤ 30), not draws. For each
pair, an inverse-permutation matrix maps (draw, split rank) → team index, so
the per-fixture goal simulation stays `nd`-vectorised exactly like the regular
phase (`OFF[cbind(seq_len(nd), h_idx)]`).

**Phase 2 (`split_groups` provided):** the caller has already folded played
split matches into `base_standings` (full carry-over makes this a plain sum)
and passes only the remaining split fixtures. The engine simulates them and
applies the group-locked ranking. No fixture generation inside the engine.

**Group-locked final ranking:** the existing packed key gains a group term —
`is_upper * 1e12 + pts * 1e6 + gd * 1e3 + gf` — so every efri team ranks above
every neðri team regardless of points (verified group locking), with the same
`ties.method = "first"` determinism. `placement = 1` is therefore
*Íslandsmeistari*; relegation places are the bottom of the neðri block.

`points_distribution` needs no semantic change: the pts matrix now simply
includes split-phase games (support for a current efri contender extends to
`base + (remaining_regular + 5) × 3`).

### Extract wiring (`.league_base_and_remaining_pfi` and callers)

A shared split-state derivation keeps the daily extract and the per-round
backfill in lockstep (same reason the base/remaining helper is shared):

For `target_div` with a configured split:

1. Regular-phase base + remaining fixtures exactly as today (structural double
   RR; playoff divisions never enter the multiplicity inference).
2. **Phase detection:** remaining regular fixtures `> 0` → phase 1 (pass
   `split_format`, no groups). Otherwise phase 2:
   - Membership from the realised regular table (pts → GD → GF), **overridden
     by observation**: any team appearing in a played or scheduled
     `*_UPPER_PO`/`*_LOWER_PO` row (with valid team names — KSÍ pre-publishes
     placeholder rows like `"23. Umferð"`/`"."` which are filtered by team
     membership) takes its observed group. Observation wins because KSÍ's
     deeper tiebreaks beyond GF could in principle diverge from ours.
   - `base_standings` = regular + played split matches (full carry-over).
   - Remaining split fixtures = scheduled valid unplayed split fixtures, plus
     **template completion** for group pairs neither played nor scheduled
     (template orientation keyed by regular-season final ranks) — mirrors the
     structural double-RR philosophy: trust real data where present, complete
     structurally.
3. Flat divisions (`LD1`–`LD3`, no `split` config): unchanged path.

`build_round_final_positions()` (backfill/replay) gains the same wiring via the
shared helper, so a per-round replay of a past season reproduces phase-1 then
phase-2 semantics round by round. Both callers need `sex` to look up the split
config; the extract already runs per sex, the backfill's callers pass it.

### Publish

`final_positions.json` / `points_distribution.json` /
`final_positions_history.json` schemas are unchanged — only the *semantics* of
`placement` change (full-season, champion-at-1). `p_top_six` stays (now
derivable from split membership once the split is known — redundant but
harmless, and the platform still reads it). `meta.json` gains an optional
`split: {upper, lower}` object on split cells so the platform's site-label fix
can render group boundaries and correct labels from data rather than
hardcoding; `meta.schema.json` updated (additive, optional).

### Testing

- Engine (test-simulate-league-season.R): flat-regression (NULL args ≡ current
  output on a seeded run); template unit tests (exact pairs + home-count
  multisets for 4/6); phase-1 partition (all placements 1..u reachable only
  via efri membership; points support includes split games); phase-2 group
  locking (the 2024 "KA finishes 7th on 37 > 34 pts" scenario); women's 6/4
  asymmetry; seed reproducibility; unsupported group size errors.
- Extract (test-extract-football-iceland.R): phase detection; observed-group
  override; placeholder-row filtering; template completion of missing
  scheduled pairs; carry-over base; backfill parity.
- Config: schema accepts the split object; split config loader.

## Out of scope (flagged as follow-ups)

- **Post-split BD-cell publish surfaces**: `standings.json` (freezes at round
  22, no group-aware rank), `next_games.json` + xG/xPts round aggregation
  (division-filtered to `BD`, so split-phase games vanish from the cell).
  Same root cause ("BD cell = division BD"), separate change — must land
  before the split (~6 Sep 2026). Spawned as a background-task chip.
  *(Shipped 2026-07-10: `docs/superpowers/plans/2026-07-10-split-cell-publish-surfaces.md`.)*
- **Women's 2025 playoff results missing** from `data/facts/results` (KSÍ
  discovery gap in `KSI_IDS`): the 2025 split verifiably happened (KSÍ tables);
  ~21 matches absent from training data. Separate ingest fix.
- **Re-backfilling 2026 `final_positions_history`** so pre-fix rounds carry
  champion semantics — needs per-round re-fits (`scripts/0Nr_replay.R
  --per-round`); operational decision for the user.
- metill-platform label changes (existing site-label chip; enabled by
  `meta.split`).
