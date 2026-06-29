# WC Knockout Conditioning (HM 2026 Phase 2) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Condition the World Cup forecast on played knockout results so the live
forecast stops showing eliminated teams with a chance — decided matches give the
winner P=1.0 downstream / loser 0.0, and the interactive bracket renders played
matches as settled facts (locked ✓ + score, loser greyed).

**Architecture:** A single `{match_no → winner_idx}` pins map, built by walking
`structure$bracket` from R32→Final (R32 occupants from the certain post-group
`occ`; downstream occupants from winners resolved so far) and looking up played
knockout results. The map drives two surfaces: (1) it is passed to
`wc_forward_bracket(..., pins=)` so `tournament_placements.json` collapses
(fixes heatmap + champion board); (2) the same played-match records are
published as a new `played[]` field in `bracket.json`, which the metill-platform
bracket client seeds into its `propagate()` (collapsing R16+ slots to confirmed
and rendering the winner row as a locked result). Penalty-decided knockouts
(level on score) get their winner from a WC-scoped `data/wc/shootouts.csv`
(martj42 `shootouts.csv` + a manual `pen_winner` overlay), never from the shared
`results` Arrow schema.

**Tech Stack:** R package (`devtools`/`testthat` 3), Arrow/Parquet facts store,
`jsonlite` publish layer; metill-platform = FastAPI + Jinja2 inline vanilla JS,
verified via `Claude_Preview` (no JS unit harness) + `pytest`.

## Global Constraints

- **Spelling:** British/international in code comments.
- **R style:** base pipe `|>`, `\()` only for single-line lambdas else
  `function(){}`, run `air format .` after generating R code.
- **Penalties live WC-scoped.** Winner source is `data/wc/shootouts.csv`
  (committed) — do **NOT** add a `shootout_winner`/penalty column to the shared
  `results` Arrow schema (`R/storage-schemas.R`): it would touch every league's
  partitions. `winner` in shootouts.csv is a **team name** (martj42 convention).
- **Inert when nothing is played.** With no played knockout results (today's
  state: group stage done, R32 unplayed) the pins map is empty and `played[]` is
  empty, so every published file is byte-identical to before. This is the primary
  regression guard — `scripts/wc/forecast.R` on current committed data must
  produce no diff in `tournament_placements.json`/`bracket.json` beyond
  `generated_at`.
- **0-based indices at the publish boundary.** `bracket.json` teams/occ are
  0-based; `played[].winner`/`.loser` must be 0-based to match.
- **No re-fit needed.** Pins act on the simulate step, which reads the existing
  `data/wc/fit/`. Verification runs `forecast.R` (≈2 min), never `fit.R` (~46 min).
- **Copy-voice (Part B, Icelandic, gender-neutral):** `úr leik` (out), `vann`
  (won), `vítaspyrnukeppni`/`(vít.)` for a shootout. En-dash score `2–1`. Run new
  user-facing strings through Miðeind grammar check before shipping Part B.
- **Platform: inline CSS/JS only**, no `editorial.css?v=N` bump (page-local CSS
  ships with the HTML). Do not rename any `data-chart` id. Verify both themes.
- **Branch:** `~/sports` work on `wc-knockout-conditioning` (created);
  metill-platform work on a fresh `hm2026-phase2-played-bracket` branch.
- **Regression gates:** `Rscript -e "devtools::test(filter='^wc')"` green after
  every Part-A task; `uv run --extra dev pytest tests/ -k hm2026` green after
  Part-B.

---

# PART A — `~/sports` upstream

## File Structure (Part A)

- **Create `R/wc-knockout.R`** — the knockout-conditioning unit:
  `wc_knockout_results()` (played cross-group WC fixtures + scores),
  `wc_shootout_winners()` (read `data/wc/shootouts.csv` → pair_key→winner map),
  `.wc_knockout_winner_of()` (decisive-score-or-shootout winner name),
  `.wc_knockout_pins()` (the bracket walk → `list(pins, played)`).
- **Create `data/wc/shootouts.csv`** — committed, header-only initially
  (`date,home_team,away_team,winner`).
- **Modify `R/wc-ingest.R`** — extend `wc_apply_manual_results()` to read an
  optional `pen_winner`; add `wc_ingest_shootouts()` (download martj42
  shootouts.csv, filter to 2026 WC knockout pairs, merge manual `pen_winner`,
  write `data/wc/shootouts.csv`).
- **Modify `R/wc-simulate.R`** — `simulate_world_cup(..., knockout_results=NULL,
  shootout_winners=NULL)`: build pins after occ; pass to `wc_forward_bracket`;
  attach `played` to `bracket_model`.
- **Modify `R/wc-publish.R`** — bracket.json gains `played[]`.
- **Modify `scripts/wc/ingest.R`** — call `wc_ingest_shootouts()`.
- **Modify `scripts/wc/forecast.R`** — read knockout results + shootout winners,
  pass into `simulate_world_cup()`.
- **Modify `data/wc/manual_results.csv`** — add `pen_winner` column + header doc.
- **Tests:** `tests/testthat/test-wc-knockout.R` (new),
  `tests/testthat/helper-wc.R` (a `make_r32_results` constructor + a known-bracket
  occ helper), additions to `test-wc-simulate.R`, `test-wc-publish.R`,
  `test-wc-ingest-overlay.R`.

## Interfaces (Part A)

- `wc_knockout_results(structure, root) → tibble(match_date, home_team,
  away_team, home_score:int, away_score:int)` — WC2026, division "FIFA World Cup",
  cross-group (both teams in `structure$group_of`, different groups), played
  (non-NA scores). Empty tibble during the group stage.
- `wc_shootout_winners(root) → named chr | NULL` — names are `.wc_pair_key(home,
  away)`, values winner team names; `NULL` when the file is absent/empty.
- `.wc_knockout_winner_of(res_row, shootout_winners) → chr(1)` — winner team name,
  or `NA_character_` (decisive: higher score; level: shootout map lookup; else NA).
- `.wc_knockout_pins(bracket, teams, occ_a, occ_b, knockout_results,
  shootout_winners=NULL) → list(pins = named list `match_no`(chr)→team idx,
  played = list of `list(match_no, winner, loser, winner_score, loser_score,
  shootout)` with **1-based** team idx)`.
- `simulate_world_cup(..., knockout_results=NULL, shootout_winners=NULL)` — adds
  `bracket_model$played` (the `.wc_knockout_pins()` `played` list, 1-based).
- `wc_ingest_shootouts(structure, shootouts_csv, manual_overlay_path, root) →
  (invisible) n rows written` — writes `data/wc/shootouts.csv`.

---

### Task A1: `wc_knockout_results()` — played cross-group fixtures

**Files:** Create `R/wc-knockout.R`; Test `tests/testthat/test-wc-knockout.R`;
add `make_r32_results()` to `tests/testthat/helper-wc.R`.

**Interfaces:** Produces `wc_knockout_results()` (signature above). Consumes
`read_table`, `structure$group_of`, `.wc_pair_key` (R/wc-schedule.R).

- [ ] **Step 1: helper** — add to `helper-wc.R` a `make_r32_results(structure,
  winners)` that returns a played-results tibble for the 16 R32 matchups implied
  by a given R32-occupant assignment (reuse `make_r32_schedule`'s cross-group
  pairing; attach scores so `winners[i]` wins match i). Keep it minimal.
- [ ] **Step 2: failing test** — `wc_knockout_results` filters to played
  cross-group WC2026 rows. Write `results` parquet via `write_table` into a
  `withr::local_tempdir()` root containing: a within-group played WC row, a
  cross-group played WC row, a cross-group **unplayed** WC row (NA score), and a
  non-WC division row. Assert only the one cross-group played row returns, with
  integer scores.
- [ ] **Step 3: run, verify FAIL** (`devtools::test_active_file('R/wc-knockout.R')`).
- [ ] **Step 4: implement** `wc_knockout_results()` (cross-group = both in
  `group_of`, `group_of[home] != group_of[away]`; played = non-NA both scores).
- [ ] **Step 5: run, verify PASS.** `air format .`
- [ ] **Step 6: commit** `feat(wc): read played knockout results from the facts store`.

---

### Task A2: `wc_shootout_winners()` + `.wc_knockout_winner_of()`

**Files:** Modify `R/wc-knockout.R`; create `data/wc/shootouts.csv` (header only);
Test `test-wc-knockout.R`.

**Interfaces:** Produces `wc_shootout_winners()`, `.wc_knockout_winner_of()`.

- [ ] **Step 1: create** `data/wc/shootouts.csv` with the single header line
  `date,home_team,away_team,winner` (committed empty store).
- [ ] **Step 2: failing tests** — (a) `wc_shootout_winners` reads a 1-row
  shootouts.csv into a `pair_key→winner` map and returns `NULL` for empty/missing;
  (b) `.wc_knockout_winner_of` returns the higher-score team for a decisive row,
  the shootout-map winner for a level row, and `NA` for a level row with no map.
- [ ] **Step 3: run, verify FAIL.**
- [ ] **Step 4: implement** both. `wc_shootout_winners` via
  `utils::read.csv(colClasses="character")` → `setNames(winner, pair_key)`.
- [ ] **Step 5: run, verify PASS.** `air format .`
- [ ] **Step 6: commit** `feat(wc): shootout-winner store + winner resolver`.

---

### Task A3: `.wc_knockout_pins()` — the bracket walk

**Files:** Modify `R/wc-knockout.R`; Test `test-wc-knockout.R`.

**Interfaces:** Produces `.wc_knockout_pins()` (signature above). Consumes
`structure$bracket`, `.wc_pair_key`, `wc_knockout_winner_of`.

Algorithm (walk `bracket` rows in tibble order; `r32_i` increments through
`round=="R32"` rows — same order `occ_a`/`occ_b` rows use in
`wc_forward_bracket`):
```
certain(v): i <- which(v >= 0.9995); if length(i)==1 i else NA   # post-group occ is one-hot
winner_of <- NA per match_no
for each bracket row (m, round):
  R32   → tA=certain(occ_a[r32_i]); tB=certain(occ_b[r32_i])
  else  → tA=winner_of[feeder_a #]; tB=winner_of[feeder_b #]
  if is.na(tA)||is.na(tB) → next                  # match not yet reachable
  result row where pair_key(teams[tA],teams[tB]) matches a knockout_results row
  if none → next
  w_name <- .wc_knockout_winner_of(row, shootout_winners); if NA → next   # undecided draw
  wi <- tidx[w_name]; li <- the other of {tA,tB}
  pins[[as.character(m)]] <- wi; winner_of[m] <- wi
  played += {match_no=m, winner=wi, loser=li,
             winner_score, loser_score, shootout = (home_score==away_score)}
```
`winner_score`/`loser_score` are the winner's/loser's goals (orient by which of
home/away is the winner). For a shootout (`home_score==away_score`) both equal.

- [ ] **Step 1: failing tests** using a fixed, fully-certain occ built from a
  known R32 assignment (helper `make_certain_occ(structure, r32_a_names,
  r32_b_names)` added to `helper-wc.R`):
  - (a) one decisive R32 result → `pins` has that `match_no`→winner idx; `played`
    has one record with correct winner/loser/scores/`shootout=FALSE`.
  - (b) a level R32 result + a shootout-winners map → pinned to the shootout
    winner, `shootout=TRUE`; same level result with `NULL` map → no pin, no record.
  - (c) **chaining:** both R32 feeders of an R16 match decided → the R16 match's
    teams resolve and, given an R16 result between them, the R16 match pins too
    (proves downstream resolution off `winner_of`).
  - (d) an R16 result whose feeders are NOT both decided → not pinned (self-gating).
- [ ] **Step 2: run, verify FAIL.**
- [ ] **Step 3: implement** `.wc_knockout_pins()`.
- [ ] **Step 4: run, verify PASS.** `air format .`
- [ ] **Step 5: commit** `feat(wc): build knockout-result pins by walking the bracket`.

---

### Task A4: wire pins into `simulate_world_cup()`

**Files:** Modify `R/wc-simulate.R` (signature + body near line 542 + the
`bracket_model` list near line 623); Test add to `test-wc-simulate.R`.

**Interfaces:** Consumes `.wc_knockout_pins()`. Produces
`bracket_model$played` and a pins-conditioned `placement_probs`.

- [ ] **Step 1: failing test** — `simulate_world_cup` with a synthetic decided R32
  result: build `fx` (group fully played via a deterministic group-result helper
  so occ is one-hot), pass `knockout_results` for one R32 matchup, and assert (i)
  the winner's `placement_probs` `P(reach R16) == 1` and the loser's `== 0`; (ii)
  `out$bracket_model$played` has one record with the right winner index; (iii)
  champion prob sums to 1 still. NB this needs a played group stage — add a
  `make_group_results_scored(structure)` helper (deterministic scores) so the R32
  occupants are certain. (If wiring a fully-played group is heavy, gate the test
  on certainty by asserting `certain` occ first.)
- [ ] **Step 2: run, verify FAIL.**
- [ ] **Step 3: implement** — add params `knockout_results=NULL,
  shootout_winners=NULL`; after `occ_a/occ_b` built, `kp <-
  .wc_knockout_pins(structure$bracket, teams, occ_a, occ_b, knockout_results,
  shootout_winners)`; `fwd <- wc_forward_bracket(W, occ_a, occ_b,
  structure$bracket, teams, pins = kp$pins)`; add `played = kp$played` to
  `bracket_model`. When `knockout_results` is NULL, `kp$pins` is `list()` and
  behaviour is unchanged.
- [ ] **Step 4: run, verify PASS** + the existing wc-simulate tests stay green.
  `air format .`
- [ ] **Step 5: commit** `feat(wc): condition the forecast on played knockout results`.

---

### Task A5: publish `played[]` in `bracket.json`

**Files:** Modify `R/wc-publish.R` (the `bracket_payload`, ~line 275); Test add
to `test-wc-publish.R`.

**Interfaces:** Consumes `sim_out$bracket_model$played` (1-based). Produces
`bracket.json` `played[]` (0-based winner/loser).

- [ ] **Step 1: failing test** — `publish_world_cup` with a `sim_out` whose
  `bracket_model$played` has one record → read back `bracket.json`, assert
  `played[[1]]$winner` is the 0-based index, `match_no`/`winner_score`/
  `loser_score`/`shootout` present; and that with empty `played` the key is `[]`.
- [ ] **Step 2: run, verify FAIL.**
- [ ] **Step 3: implement** — add to `bracket_payload`:
  `played = lapply(bm$played, function(p) list(match_no=p$match_no,
  winner=p$winner-1L, loser=p$loser-1L, winner_score=p$winner_score,
  loser_score=p$loser_score, shootout=p$shootout))` (empty list when none).
- [ ] **Step 4: run, verify PASS.** `air format .`
- [ ] **Step 5: commit** `feat(wc): publish played knockout matches in bracket.json`.

---

### Task A6: shootouts ingest + manual `pen_winner` overlay

**Files:** Modify `R/wc-ingest.R` (`wc_apply_manual_results` reads optional
`pen_winner`; add `wc_ingest_shootouts()`), `scripts/wc/ingest.R`,
`data/wc/manual_results.csv` (add `pen_winner` col + doc); Test add to
`test-wc-ingest-overlay.R`.

**Interfaces:** Produces `wc_ingest_shootouts(structure, shootouts_csv,
manual_overlay_path, root)` writing `data/wc/shootouts.csv`.

- [ ] **Step 1: failing test** — `wc_ingest_shootouts` given a martj42-shape
  shootouts data frame (with both WC-knockout and irrelevant historical rows) +
  a manual overlay carrying a `pen_winner` for a level fixture writes a
  `shootouts.csv` containing **only** 2026 WC knockout pairs, martj42 ∪ manual,
  deduped (martj42 canonical on conflict). Read it back via
  `wc_shootout_winners()`.
- [ ] **Step 2: run, verify FAIL.**
- [ ] **Step 3: implement** — `wc_ingest_shootouts()`: filter martj42 shootouts to
  rows whose `(home,away)` is a WC knockout pair (cross-group, both WC teams) and
  `date` in the WC window; union with manual `pen_winner` rows (date/home/away
  from the overlay, winner = `pen_winner`); martj42 wins on key conflict; write
  `date,home_team,away_team,winner`. Extend `wc_apply_manual_results` to tolerate
  an absent `pen_winner` column (existing overlays without it must still work).
  Add the `pen_winner` column to `data/wc/manual_results.csv` (empty values) and a
  header-comment line documenting it. Wire `wc_ingest_shootouts()` into
  `scripts/wc/ingest.R` (download `shootouts.csv` next to `results.csv`).
- [ ] **Step 4: run, verify PASS** (+ `test-wc-ingest-overlay.R` legacy tests green).
  `air format .`
- [ ] **Step 5: commit** `feat(wc): ingest penalty-shootout winners (martj42 + manual overlay)`.

---

### Task A7: forecast pipeline wiring + end-to-end inert check

**Files:** Modify `scripts/wc/forecast.R`.

- [ ] **Step 1:** after `fx <- wc_group_fixtures(s)` add `kres <-
  wc_knockout_results(s)` + `sw <- wc_shootout_winners()`; pass `knockout_results
  = kres, shootout_winners = sw` into `simulate_world_cup(...)`. Log
  `cat(sprintf("played knockout results: %d\n", nrow(kres)))`.
- [ ] **Step 2: run** `Rscript scripts/wc/forecast.R` on current committed data.
  Expected: `played knockout results: 0`; it completes; `git diff --stat
  data/publish/world_cup/karla/` shows only `generated_at`-level churn in
  `bracket.json`/`tournament_placements.json` (the new `played: []` key is the
  only structural addition to bracket.json). **Restore** the publish dir
  (`git checkout -- data/publish/world_cup/karla/`) — this is a verification run,
  not a publish.
- [ ] **Step 3: full wc test suite** `Rscript -e "devtools::test(filter='^wc')"` —
  all green.
- [ ] **Step 4: commit** `feat(wc): pass played knockout results into the forecast pipeline`.

---

### Task A8: synthetic end-to-end pin demonstration (verification, not committed data)

**Files:** none (scratch verification).

- [ ] **Step 1:** in an R session, load_all, build `s`, read the real `si`/`fx`,
  construct ONE synthetic `knockout_results` row for the actual R32 matchup of
  match 73 (read the certain occupants from `out$bracket_model$occ_a/occ_b` of a
  baseline run), re-run `simulate_world_cup` with it, and confirm
  `placement_probs` `P(reach R16)==1` for the winner / `0` for the loser and
  `bracket_model$played` is populated. Capture the numbers in the session note.
  (No data committed — proves the live path with real strengths.)

---

# PART B — metill-platform (consume `played[]`)

## File Structure (Part B)

- **Modify `app/templates/ithrottir_hm2026.html`** — bracket IIFE
  (`propagate`, `fillSlot`, `fillWinner`, new `renderPlayedWinner`) + inline CSS
  (`.hm-win--result`, `.hm-cand--out`).
- **Modify `.claude/rules/hm2026.md`** — document the `played[]` contract +
  Phase 2 done.
- **Test:** none server-side (client JS); verify via `Claude_Preview` against a
  bracket.json carrying a synthetic `played` entry; keep `pytest -k hm2026` green.

## Interfaces (Part B)

- Consumes `DATA.bracket.played[] = {match_no, winner(0-based), loser(0-based),
  winner_score, loser_score, shootout}`.
- Bracket IIFE scope already exposes: `el`, `teams`/`teamsEn`, `wcFlag`, `code3`,
  `confirmedNode`, `topNonzero`, `pct`, `propagate`, `fillSlot`, `fillWinner`.

---

### Task B1: seed played results in `propagate()` + `playedByMatch` index

**Files:** Modify the bracket IIFE (`app/templates/ithrottir_hm2026.html` ~1387,
~1400).

- [ ] **Step 1:** after the `occByMatch` build, add `const PLAYED = B.played||[];
  const playedByMatch={}; PLAYED.forEach(p=>playedByMatch[p.match_no]=p);`.
- [ ] **Step 2:** in `propagate`, when finalising `winner[m.match_no]`, give the
  played result top precedence over the reader `pin`:
  ```js
  const res = playedByMatch[m.match_no];
  if(res){ const d=new Float64Array(nt); d[res.winner]=1; winner[m.match_no]=d; }
  else if(pin!=null){ scenarioP*=(wd[pin]||0); const d=new Float64Array(nt); d[pin]=1; winner[m.match_no]=d; }
  else winner[m.match_no]=wd;
  ```
  (Played matches don't touch `scenarioP` — they're facts, not assumptions.)
- [ ] **Step 3: verify via `Claude_Preview`** (after B3 CSS/render also in place,
  so do the visual check in B3) — for now, confirm no console error and the page
  still renders with `played:[]` (current live data).

---

### Task B2: render the locked winner row + greyed loser slot

**Files:** Modify the bracket IIFE `fillWinner`, `fillSlot`, `confirmedNode`;
inline CSS in `{% block head %}`.

- [ ] **Step 1: CSS** — after the `.hm-cand--confirmed` block add:
  `.hm-win--result` (non-interactive: `cursor:default`; a ✓ in `var(--positive)`
  before the team; no amber-button affordance) and `.hm-cand--out` (the eliminated
  R32 arrival: `text-decoration:line-through; color:var(--grot);` and its `::after`
  ✓ suppressed).
- [ ] **Step 2:** extend `confirmedNode(t, ariaLabel, opts)` to take `opts.out`
  → add `hm-cand--out` and a "úr leik" aria suffix (keep the existing 2-arg calls
  working via a default `opts={}`).
- [ ] **Step 3:** `fillSlot` — for an R32 slot whose confirmed team is
  `playedByMatch[m.match_no]?.loser`, render `confirmedNode(t,label,{out:true})`
  with an "úr leik" label instead of the "áfram úr riðli" ✓. (Winner slot keeps
  the ✓ confirmed node.) The existing single-team predicate already routes here.
- [ ] **Step 4:** `fillWinner` — at the top, `const res=playedByMatch[m.match_no];
  if(res){ renderPlayedWinner(container,m,res); return; }`. `renderPlayedWinner`:
  `container.className="hm-win hm-win--result"`, label "Sigraði"/"Meistari",
  winner flag+code3, score `${res.winner_score}–${res.loser_score}` (+ ` (vít.)`
  when `res.shootout`), `role="img"` + factual aria-label, no click/keydown.
- [ ] **Step 5: regression gate** `uv run --extra dev pytest tests/ -k hm2026 -v`
  → PASS (no server-side change).
- [ ] **Step 6: commit** `feat(hm2026): render played knockout matches as settled facts in the bracket`.

---

### Task B3: verify with a synthetic `played` entry (both themes)

**Files:** none (verification); temporary local edit to a copy of bracket.json.

- [ ] **Step 1:** `preview_start` against this checkout. Temporarily inject a
  synthetic `played` entry for match 73 (e.g. patch the served
  `bracket.json` or `preview_eval` to splice `DATA.bracket.played` then re-run the
  bracket IIFE) — winner = the slot-A team, a `2–1` score.
- [ ] **Step 2:** `preview_snapshot` the `#bracket`: match-73 card shows the
  winner slot ✓ + the loser slot line-through "úr leik" + the winner row locked
  `✓ 2–1` (non-interactive); the **R16 slot fed by match 73** now shows the winner
  as a single confirmed ✓ arrival (no bar/%), proving the R16+ auto-collapse.
- [ ] **Step 3:** repeat for a `shootout:true, 1–1` entry → winner row reads
  `✓ 1–1 (vít.)`.
- [ ] **Step 4: both themes** (`data-theme` flip) — ✓ (`--positive`), greyed loser
  (`--grot`) stay legible.
- [ ] **Step 5:** revert the synthetic injection. Run new Icelandic strings
  (`úr leik`, `Sigraði`, `(vít.)`, the aria-labels) through Miðeind grammar check.

---

### Task B4: document + finish

**Files:** Modify `.claude/rules/hm2026.md`.

- [ ] **Step 1:** in the "Confirmed slots" paragraph + the `bracket.json` row of
  the data-contract table, document the `played[]` field and that Phase 2 is done
  (R16+ now collapse from real results; winner row = locked ✓ + score; loser
  greyed). Note the penalty source (`data/wc/shootouts.csv`).
- [ ] **Step 2: commit** `docs(hm2026): document Phase 2 played-knockout conditioning`.
- [ ] **Step 3:** open PRs (one per repo) after `git pull --rebase origin main` +
  re-reading `git log @{u}..HEAD` (cron-collision discipline). Upstream PR first
  (it owns the data contract); platform PR references it. Do **not** publish a real
  forecast until a real knockout result exists — the daily `world-cup.yml` cron
  picks the new code up automatically.

---

## Self-Review

**Spec coverage** (the user's confirmed scope = full + platform ✓ polish; penalties = shootouts.csv + manual column):
- Pins → placement collapse (heatmap/board) → Tasks A3–A4. ✓
- Penalty winner source (martj42 shootouts.csv + manual `pen_winner`) → A2, A6. ✓
- Interactive bracket reflects reality (R16+ auto-collapse) → `played[]` (A5) + propagate seeding (B1). ✓
- Locked winner row ✓ + score, loser greyed → B2. ✓
- Inert when nothing played (regression guard) → Global Constraints + A7 Step 2. ✓
- Verify R16 prob == 1.0 / loser 0.0 → A4 Step 1 (test) + A8 (live synthetic). ✓
- No shared-schema change; WC-scoped penalties → Global Constraints + A2/A6. ✓
- Platform inline-only, both themes, no data-chart rename → Part B constraints + B3. ✓

**Placeholder scan:** the pin-walk, winner-resolver, propagate seeding, and
render functions are shown as algorithm/code; tests name concrete assertions. The
two heaviest "build a played group stage" helpers (A1/A4) are described by their
contract rather than full code — acceptable for an inline TDD executor who writes
the helper while making the test pass; if handing to a fresh subagent, expand
`make_group_results_scored`/`make_certain_occ` first.

**Type consistency:** `.wc_knockout_pins` returns 1-based idx in `pins`/`played`;
`simulate_world_cup` carries 1-based `played` into `bracket_model`; `publish`
converts to 0-based for `bracket.json`; Part B consumes 0-based. `winner_score`/
`loser_score` named consistently A3→A5→B2. `pair_key` from `.wc_pair_key`
throughout.
