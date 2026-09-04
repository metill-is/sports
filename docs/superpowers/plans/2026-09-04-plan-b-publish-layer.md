# Plan B — the publish layer (WS7–WS12)

Implements sections 9–13 and 15 of
[the design](../specs/2026-09-02-basketball-handball-metill-parity-design.md).
Plan A (WS1–WS6) is merged into `feat/bb-hb-metill-parity`; suite is FAIL 0 / SKIP 45.

**This plan contains the fix for B4** — the reason basketball and handball have
never published from CI. Everything else here exists to make that fix correct.

## How to use this document

Read **Integration decisions** first. They were produced by an adversarial
crosscheck of six independently-drafted workstreams and they **override the task
text below them**. The crosscheck found eleven collisions, three of which are the
Plan A failure class — the same change drafted twice, each with its own "expect
RED" step, so whichever ran second would find its RED already green and leave a
test that can never fail beside an unvalidated change.

Then read **Execution order**. It is not advisory: three of its edges break
football's nine live cells or silently corrupt published data if violated.

## Global constraints

- Strict TDD. Write the failing test, RUN it, confirm the failure is the one
  predicted. **If a predicted RED comes back green, STOP** — that is the signal
  another workstream already made the change, and continuing leaves a dead test.
- **Never `git push`.** Plan A is unpushed; Plan B commits only.
- Football is LIVE and mid-season with nine publishing cells. Its 92 golden
  sha256 hashes (`tests/testthat/fixtures/golden/football-publish-hashes.csv`)
  are the net. They stay byte-identical except where a task says otherwise and
  justifies it.
- Tests must not hit the network and must not compile Stan. Use Plan A's
  `stub_fit()` and the committed fixture trees.
- Non-ASCII/Icelandic: python heredocs or `cat <<'EOF'`, never Write/Edit.
- No `skip()`/`skip_if()`/`Sys.getenv` in bb/hb coverage —
  `tests/testthat/test-fixture-skip-hygiene.R` fails the build on them.

---

## Integration decisions (override the drafts)

### From the adversarial crosscheck

**SC-1 — WS9 task 2 and WS10 task 2** `[critical — the Plan A double-RED class]`

*Problem.* Both workstreams create the per-sport publish profile's $points and $units fields, each with its own "write the failing test, expect could-not-find/NULL" step. Whichever runs second finds its RED already green and leaves a test that can never fail. Worse, the shapes are incompatible: WS9 specifies `points = c(win=, draw=, loss=)` as a NAMED INTEGER VECTOR and asserts `sport_publish_profile("basketball")$points == c(win=2L, draw=0L, loss=0L)`; WS10 specifies `$points = list(win, draw, loss)` with basketball `draw = NULL`. `c(win=2L, draw=NULL, loss=0L)` silently drops the draw element, so WS10's own basketball case is unrepresentable in WS9's shape. WS9 also carries $units$diff_bin_width AND a separate $diff_bins list; WS10 repoints the extractors' hardcoded bucket_width at $units$diff_bin_width.

*Decision.* WS9 task 2 is the SOLE author of sport_publish_profile(). It ships the complete field set in one commit — required_extracts, optional_extracts, empty_extracts, predicted_matches_shape, value_link, surfaces, has_ties, tie_threshold AND points, units, season_scope, postseason, placement_basis. The canonical shape of $points is a NAMED LIST: list(win = <int>, draw = <int or NULL>, loss = <int>), with basketball draw = NULL (not 0L) so `meta.points.draw` serialises as JSON null and no consumer infers a draw is possible. $units is list(strength = <chr>, home_advantage = <chr>, diff_bin_width = <int>); WS9's separate $diff_bins field is DELETED — diff_bin_width is the single source. WS10 task 2 is reduced to exactly one thing, with no profile edits and no RED against sport_publish_profile: repoint R/extract-basketball-iceland.R:61 and R/extract-handball-iceland.R:46 at sport_publish_profile(<sport>)$units$diff_bin_width and prove by regenerating tests/testthat/fixtures/extracts/ that no committed parquet moves. WS10's assertions about the profile's values move into WS9 task 2's test file.

**SC-2 — WS9 task 4 and WS10 task 7** `[critical — the Plan A double-RED class]`

*Problem.* Both draft the bb/hb next_games field rename to football's names plus goal_diff_distribution/division_code/venue. WS9 builds `.next_games_rows_pfi(predicted, profile, ...)` in a new R/publish-next-games.R branching on profile$predicted_matches_shape; WS10 does a select-and-rename inline in publish_iceland_league. They also disagree on where division_code comes from: WS9 passes `division_badges = .football_iceland_division_code_labels()`, WS10 uses the configured code_badge. WS10 already anticipated the collision in its own steps ("If the run comes back GREEN on the first try that is a FAILED RED"), which is the tell.

*Decision.* WS9 task 4 owns the IMPLEMENTATION (`.next_games_rows_pfi()` in R/publish-next-games.R, both input shapes behind one output contract). WS10 task 7 keeps ONLY its cross-cell contract test (tests/testthat/test-publish-next-games-contract.R) and drops its implementation step entirely; that test runs after WS9 task 4 and is expected to be green on arrival — it is a contract lock, not a TDD cycle, and its task text must say so instead of manufacturing a RED. The `division_badges` argument to `.next_games_rows_pfi()` is supplied by the caller as `.iceland_division_badges(league_key, sex)` (SC-3), never by the retired static map.

**SC-3 — WS7 task 4 and WS9 task 4** `[critical — breaks the build if run in the drafted order]`

*Problem.* WS9 task 4 instructs passing `.football_iceland_division_code_labels()` into the new `.next_games_rows_pfi()`, deferring the config swap to WS9 task 5. WS7 task 4 DELETES that function with no compatibility alias and rewrites all 43 call sites. If WS7 lands first (it must — WS9 tasks 3 and 5 depend on it), WS9 task 4 is written against a symbol that no longer exists.

*Decision.* WS9 task 4 uses `.iceland_division_badges("football_iceland", sex)` from the start — it never mentions `.football_iceland_division_code_labels()`. WS7 (all six tasks) lands before any WS9 task that touches R/publish-football-iceland.R, and WS9 task 5's literal-replacement list drops item (4) (the badge swap), which WS7 task 4 has already performed.

**SC-4 — WS7 (Produces list) and WS8 task 4** `[high — the Plan A double-RED class]`

*Problem.* `.iceland_division_expected_meetings(key, sex)` is drafted twice. WS7's Produces list names it as one of its nine accessors in R/publish-divisions.R; WS8 task 4 instructs adding it to R/extract-football-iceland.R and says explicitly "Coordinate with WS7 before committing: this helper must have exactly one definition" and "WS7 MUST NOT also define this — one definition, WS8 owns it". Two definitions in two files is a load-order collision; two RED tests means one is born green.

*Decision.* WS7 owns `.iceland_division_expected_meetings(key, sex)`, defined in R/publish-divisions.R alongside the other eight accessors, returning a named integer vector with NA_integer_ where the key is unset. WS8 task 4 DELETES its own definition step and its football-all-NA test, and consumes WS7's. WS8 keeps sole ownership of `.division_regular_rounds()`, which is a different function.

**SC-5 — WS7 tasks 1+5 and WS10 task 4** `[critical — a schema mismatch here aborts load_leagues() repo-wide]`

*Problem.* The two workstreams model post-season qualification differently in the SAME schema object, which is additionalProperties:false. WS7 declares a nested `qualify: {slots, label_is}` object and adds it to football BD. WS10 declares flat sibling keys `qualify_slots`, `relegation_slots` and `qualify_label_is`, and its task 4 step 1 instructs adding them to config/leagues.yml if absent. Whichever lands second writes a key the schema does not declare, and validate_leagues() aborts with "leagues.yml failed schema validation" — taking every script in the repo down, football included.

*Decision.* WS7's shape wins and is the only one written: the optional keys on publishDivisionList.items are exactly `code_badge` (string, ^[A-Z][A-Z0-9_]{1,3}$), `expected_meetings` (integer >= 1), `relegation_slots` (integer >= 0) and `qualify` (object, additionalProperties:false, required [slots, label_is]). There is no `qualify_slots` and no `qualify_label_is` anywhere. WS10 reads qualification through `.iceland_division_qualify(key, sex)[[code]]`, which returns NULL or list(slots, label_is); `.build_placement_summary()` takes `qualify = <that list or NULL>` instead of a bare `qualify_slots` integer, and emits meta.qualify: null and no p_qualify when it is NULL. WS10 task 4 makes ZERO edits to config/leagues.yml or config/leagues.schema.json.

**SC-6 — WS10 (tasks 5 and 7) and WS7** `[high — consumes a signature nobody produces]`

*Problem.* WS10 consumes `.iceland_division_config(key, sex)` returning the raw publish_divisions entry per division code, and marks it "NEW, may be absent — this workstream adds it if so". WS7's Produces list contains nine accessors and no such function. If WS10 adds it, the repo gains a tenth accessor with a different return shape (raw entries vs typed vectors) that duplicates all nine.

*Decision.* `.iceland_division_config()` is NOT created. WS10 reads each attribute through WS7's typed accessor for it — `.iceland_division_qualify()`, `.iceland_division_relegation()`, `.iceland_division_expected_meetings()`, `.iceland_division_badges()`, `.iceland_division_is_cup()` — indexing by division code. WS10's `.build_publish_meta(base, profile, format, division_cfg)` keeps its `division_cfg` parameter but the publisher assembles it locally as `list(qualify = .iceland_division_qualify(key, sex)[[div]], relegation_slots = .iceland_division_relegation(key, sex)[[div]], expected_meetings = .iceland_division_expected_meetings(key, sex)[[div]])`.

**SC-7 — WS9 task 1 and WS11 task 1** `[high — argument-order collision on a live exported function]`

*Problem.* Both add a new parameter to publish_one() and both specify it as the LAST formal. WS9 adds `end_date = Sys.Date()` as the seventh; WS11 adds `schema_dir = here::here("config","publish-schemas")` as "the seventh formal". Whichever lands second either displaces the other or produces an order the first one's tests were written against positionally.

*Decision.* The final signature is fixed as `publish_one(static, betting, key, sex, root = here::here("data"), validate = TRUE, end_date = Sys.Date(), schema_dir = here::here("config", "publish-schemas"))` — end_date seventh, schema_dir eighth. WS9 task 1 adds end_date only; WS11 task 1 appends schema_dir after it and must be sequenced after WS9 task 1. Every call site passes both by NAME, never positionally. WS12's `run_publish_targets()` forwards both through `...` or explicit named arguments so scripts/05_publish.R keeps production defaults.

**SC-8 — WS10 task 8 and WS11 tasks 3+4** `[high — one workstream hand-edits files the other makes generated output]`

*Problem.* WS10 task 8 edits config/publish-schemas/football/{meta,final_positions,points_distribution}.schema.json by hand to add the v2 keys. WS11 task 3 makes exactly those files GENERATED from config/publish-schemas/_base/ + _delta/football/, with a byte-equality test asserting `Rscript tools/gen-publish-schemas.R` reproduces them. If WS10 task 8 lands after WS11 task 3, the next render silently reverts WS10's edits and WS11's byte-equality test goes red. WS11 task 4 then re-adds the same keys to _base — the same change drafted twice. They also disagree on the n_rounds_source enum: WS10 writes [config, schedule, none, not_applicable]; WS11 task 4 consumes it as "{schedule, config}", which would reject two values WS10 emits.

*Decision.* WS11 task 3 (the generator plus _base/_delta/football rendering today's contract with no semantic change) lands BEFORE WS10 task 8. WS10 task 8 is then rewritten to edit `config/publish-schemas/_base/` and `config/publish-schemas/_delta/football/` and to run `Rscript tools/gen-publish-schemas.R`; it never touches a file under config/publish-schemas/football/ directly. WS11 task 4 is DELETED as a separate task — its content is now WS10 task 8. The n_rounds_source enum is exactly ["config", "schedule", "none", "not_applicable"] in _base, and any narrowing lives in a per-sport _delta.

**SC-9 — WS9 task 7 and Plan A's tests/testthat/test-fixture-skip-hygiene.R** `[critical — deletes files a live test hard-requires; nobody drafted the fix]`

*Problem.* WS9 task 7 does `git rm tests/testthat/test-publish-basketball.R tests/testthat/test-publish-handball.R`. Both filenames are in the `guarded` vector of the skip-hygiene test, whose `expect_setequal(setdiff(guarded, <WS3 file>), setdiff(present, <WS3 file>))` requires every listed file except WS3's to EXIST. Deleting them fails the build, and WS9's own step 6 ("full suite -> FAIL 0") would be impossible.

*Decision.* WS9 task 7 replaces the two deleted filenames in test-fixture-skip-hygiene.R's `guarded` vector with the file that supersedes them — "test-publish-b4-acceptance.R" — in the SAME commit as the deletion, and adds a comment recording that the bb/hb publishers were unified into publish_iceland_league so their per-sport test files no longer exist. Three other workstreams also append to this vector (WS7 task 3 adds test-iceland-division-helpers.R, WS12 tasks 2-3 add test-health-publish-freshness.R and test-health-season-resolution.R); those are additive and merge cleanly, but each must re-read the file before editing rather than pasting a whole-vector replacement.

**SC-10 — WS10 (risk, handed off) and WS11 tasks 3/5/7 (never picked it up)** `[critical — arming basketball aborts every 1D cell]`

*Problem.* config/publish-schemas/football/meta.schema.json constrains `division` to ^[A-Z][A-Z0-9_]*$, which rejects basketball's "1D" (leading digit). WS10 recorded this as a hazard for WS11 to fix in _base. WS11's plan copies football's schemas into _base verbatim (task 3) and its basketball _delta (task 5) narrows the sport enum and adds home_advantage bounds but never relaxes the division pattern. So the fix exists in no workstream's task list — it is handed off and dropped. Same class: final_positions.schema.json requires p_top_six/p_winner, which no bb/hb payload emits.

*Decision.* WS11 task 3 relaxes `_base/meta.schema.json`'s division pattern to ^[A-Z0-9][A-Z0-9_]*$ in the same commit that creates _base, and records in the schema description that basketball's 1D is why. WS11 task 5's basketball and handball deltas must additionally REMOVE p_top_six and p_winner from final_positions' `required` array (restating the full array — RFC 7386 replaces arrays wholesale). WS11 task 5 gains an explicit acceptance step: publish all 8 bb/hb fixture cells and assert `validate_publish_dir(..., sport = <sport>)` returns ok = TRUE with zero unmatched BEFORE task 7's arming git mv — a red here is a schema bug, never a reason to loosen the arming order.

**SC-11 — WS8 task 4/6 and WS10 task 1** `[high — two functions computing the same quantity, with different return shapes and different precedence]`

*Problem.* WS8 produces `.division_regular_rounds(results, sport, sex, season, division, expected_meetings)` -> list(cut, source ∈ {config,none}, n_teams) and applies it as a row filter inside the extractor. WS10 produces `.publish_n_rounds(results, schedules, season, division_codes, end_date, expected_meetings, is_cup)` -> list(n_rounds, source ∈ {config,schedule,none,not_applicable}, n_rounds_config, n_rounds_schedule, n_teams) plus `.regular_season_results(results, n_rounds)` and applies the same cut inside the publisher. WS8 task 6 explicitly punts ("WS10 calls .division_regular_rounds() itself; WS8 exports nothing new for it"), but WS10 never mentions `.division_regular_rounds` and rolls its own. Two implementations of the regular-season boundary can drift, and the extractor's cut and the publisher's cut must be identical or standings and final_positions disagree.

*Decision.* There is ONE boundary function and it lives in R/publish-format.R, owned by WS10: `.publish_n_rounds()` (returning n_rounds, source, n_rounds_config, n_rounds_schedule, n_teams) plus `.regular_season_results(results, n_rounds)`. WS8 task 4 does NOT create R/division-rounds.R and does NOT define `.division_regular_rounds()`; WS8 task 6 calls `.publish_n_rounds()` and `.regular_season_results()` for its extractor-side cut. This makes WS10 task 1 a hard prerequisite of WS8 task 6 — reorder so WS10 task 1 (which is pure, fixture-only and depends on nothing) lands before WS8 task 6. Precedence is FIXED as: configured expected_meetings WINS; the schedule derivation is the fallback; both values are always returned so WS12's check_publish_format_agreement can WARN on disagreement. The design's §12 precedence (schedule first) and its "meta.n_rounds == 44 for Bónusdeild karla" verification line are BOTH wrong and are overridden — bb male BD is a 2-meeting, 22-round regular season, and both WS7 and WS10 independently measured this.

**SC-12 — WS9 task 7 and WS12 task 7** `[medium — same edit to scripts/05_publish.R, plus a live-fire ordering trap with WS11 task 8]`

*Problem.* WS9 task 7 says "keep the surrounding tryCatch and its extract_partition_exists re-raise VERBATIM — WS12 owns changing the quiet-skip semantics", while WS12 task 7 rewrites the scripts/05_publish.R loop into run_publish_targets(). WS11 task 8 (fail-closed default) hard-depends on WS12 task 7 existing. WS12 task 7 already has a grep-first seam check, but nothing states the global order, and WS11 task 8 shipping first turns any single bb/hb schema breach into a football outage: scripts/05_publish.R:34 calls publish_one() bare in a loop with no tryCatch.

*Decision.* WS12 task 7 (run_publish_targets + the thin scripts/05_publish.R) is a hard prerequisite of WS11 task 8, and WS12 task 6 (run_fit_targets) is likewise a prerequisite of the first live 2DT fit. WS12 task 7's grep-first seam check stands but its outcome is now predetermined: WS9 task 7 leaves scripts/05_publish.R's loop untouched, so WS12 task 7 always implements. On WS12's own INT-2 the ruling is ANY-failed, not all-failed: `run_fit_targets()` and `run_publish_targets()` each return a `failed` tibble, and both scripts `quit(save = "no", status = 1L)` when it has any row. The task brief's "only if ALL targets failed" is overridden — it contradicts spec §13 and makes WS12's own verification (c) unsatisfiable, and it is the exact warn-and-exit-0 shape B4 hid in.

### From measurement taken after the drafters were briefed

**ID-B14 — `n_rounds` is derived, and `2*(n_teams-1)` is not the derivation.**
Measured 2026-09-04 across all eight cells. `2*(n-1)` is correct for basketball
(male BD/1D 22, female BD 18) but WRONG for Icelandic women's handball, which
plays a TRIPLE round robin: 8 teams, 84 matches, 21 rounds, `meetings = 3`,
against a formula value of 14. The correct derivation is
`n_rounds = n_fixtures / (n_teams / 2)` over the season's regular-season
fixtures. The clean source is the SCHEDULE: a live dry run of the 2027 KKI
season ids returned exactly 132 / 90 / 132 / 132 fixtures with NO urslitakeppni
rounds, because KKI publishes playoff fixtures later inside the same season_id.
The "last round with >= floor(n/2) matches" heuristic that an earlier note
floated is REJECTED: per-round counts fluctuate with postponements, so the
qualifying set is non-contiguous in all four basketball cells (female 1D reads
3,5,5,5,4,5,6,5,6,4,6,5,4,5,4,5,7,5,2,2,2,1,1,1).

**ID-B15 — there is no `qualify` entry for basketball or handball.**
Measured teams reaching the post-season, season 2026: basketball male BD 8 of
12, male 1D 8 of 12, female BD **10 of 10**, female 1D 4 of 11; handball PO
(a separate division) male 8, female 6. Four cells, four structures, and
women's Bonusdeild takes EVERY team through, so "qualification" is not a cut
there at all. No single per-division integer expresses this. WS7 already
reached the same conclusion independently and configures `qualify` for football
BD only; that stands. Publish the placement grid and a regular-season-scoped
title probability, and leave post-season qualification to a plan that models
the bracket. Shipping a plausible-looking number here is exactly the
"top-six number wearing a playoff label" failure D3 warns about.

**ID-B16 — the publish directory move is free, and that is verified, not assumed.**
`grep` over `metill-platform/app/` returns NO reference to any basketball or
handball path: the consumer's routes are football-only and its `sport_tabs`
carries Handbolti/Korfubolti as `href: None, disabled: True` stubs
(app/routes/ithrottir.py:473-477). So moving
`data/publish/{sport}/iceland/{karla,kvenna}/` to `{karla,kvenna}-{slug}/`
breaks nothing downstream today. It still gets a deliberate test (see the
crosscheck's missing-work list), because it stops being free the moment Plan C
lands routes.

---

## Execution order

EXECUTE IN THIS ORDER. Three edges below break football's nine live cells or corrupt published data if violated; they are marked BREAKS/CORRUPTS.

PHASE 0 — independent, land first, no dependencies:
  0a. WS11 task 1 is deferred (see SC-7); instead land WS11 task 2 FIRST in its schema_dir-less form is NOT possible — so land WS9 task 1 (end_date), then WS11 task 1 (schema_dir), then WS11 task 2 (subtree validation with an explicit `sport`). BREAKS FOOTBALL IF SKIPPED: `.validate_or_abort()` (R/publish-pipeline.R:145-160, verified) validates the WHOLE publish tree, so the moment config/publish-schemas/basketball/ exists, basketball's non-conforming JSON aborts FOOTBALL's publish call — and scripts/05_publish.R:34 calls publish_one() bare, so the run dies before the commit step. WS11 task 2's fix must be in place before ANY schema directory is armed. Note WS11's own trap: a bare `validate_publish_dir(file.path(output_root, sport))` fails OPEN (the derived sport becomes "iceland", every file lands in `unmatched`, ok = TRUE, n_files = 0) — the explicit `sport` argument IS the fix.
  0b. WS11 task 3 (generator + _base + _delta/football, with the division pattern relaxed per SC-10). Pure refactor, football semantics asserted unchanged via before/after validate_publish_dir on the live tree.
  0c. WS7 tasks 1-6 in order (schema keys -> football code_badge/qualify -> the nine .iceland_division_* accessors -> the 43-site cutover -> bb/hb publish_divisions -> invariants + rules doc). WS7 task 4 is one atomic commit; a half-cut-over tree does not load.
  0d. WS12 tasks 1-4 (health helpers + the three checks). Independent of everything except WS9's profile for `.publish_surfaces()`, so WS12 tasks 1-2 must wait for WS9 task 2 — see phase 2.

PHASE 1 — the profile and the reader (WS9):
  1a. WS9 task 2 — sport_publish_profile(), sole author, full field set including points/units/season_scope/postseason/placement_basis (SC-1).
  1b. WS9 task 3 — read_extracted_iceland + the extract_partition_exists rename. Requires WS7 (task 3 for the accessors).
  1c. WS9 task 4 — .next_games_rows_pfi, using .iceland_division_badges from the start (SC-3).

PHASE 2 — the format helpers, before the extractor uses them:
  2a. WS10 task 1 — .publish_n_rounds / .publish_round / .regular_season_results + the committed playoff-overhang fixture. This MOVES EARLIER than drafted because WS8 task 6 now consumes it (SC-11).
  2b. WS10 task 2 (reduced to the bucket_width repoint only).
  2c. WS12 tasks 1-4 may now run in parallel (they need WS9 task 2's $surfaces and extract_partition_exists).

PHASE 3 — the extractor (WS8), strictly in its own task order 1,2,3,5,4→deleted,6,7,8,9:
  WS8 task 4 is deleted except for the decision header, which moves into R/publish-format.R (SC-11); WS8 task 6 calls WS10's helpers. CORRUPTS DATA IF TASK 6 IS SKIPPED OR RUN AFTER TASK 9: without the regular-season cut, basketball's embedded úrslitakeppni feeds .compute_base_points_2dt() and the published league table is simulated on post-season points — a silently wrong table, not a visible error. WS8 task 9 (fixture regeneration) MUST be last within WS8; regenerating before tasks 5-8 bakes the old 5-parquet single-division shape into the committed fixture and every later assertion measures the wrong tree.

PHASE 4 — the publisher rename and the B4 fix (WS9 tasks 5,6,7,8):
  WS9 task 5 is the NAMED STOP POINT: if the 1618-line publisher is not green with all 92 golden hashes byte-identical, stop before task 7. That partial landing is safe — football publishes under a new name, bb/hb are exactly as broken as today, nothing is half-migrated.
  WS9 task 7 must land BEFORE WS11 task 7 (arming) — see phase 6. WS9 task 7 also updates the skip-hygiene guarded vector (SC-9).

PHASE 5 — meta v2 and the payload contract (WS10 tasks 3,4,5,6,7,8):
  WS10 task 6 (golden regeneration) must run immediately after task 5 and must assert `setdiff(basename(changed), c("meta.json","final_positions.json"))` is empty BEFORE regenerating. CORRUPTS THE REGRESSION NET IF RUN LOOSELY: the golden manifest is football's only byte-level net; regenerating it to absorb an unexplained diff converts it into a rubber stamp. Verified on disk: 93 lines = header + 92 payloads over 9 cells (7 cells x 10 + 2 cup cells x 11 — football cells are NOT uniformly 10 artefacts; the two bikar cells carry bracket.json and tournament_placements.json, so any assertion phrased as "the ten JSON basenames football emits" must be phrased as profile$surfaces instead).
  WS10 task 8 now edits _base/_delta and re-renders (SC-8).

PHASE 6 — arming, in exactly this order (every edge here BREAKS FOOTBALL if reversed):
  6a. WS12 task 7 (run_publish_targets + thin 05_publish.R) — prerequisite of 6d.
  6b. WS11 task 5 (draft bb/hb schemas under _draft, validated against fixture-published cells). Inert in both validators: verified that R's .resolve_schema_path (R/validate-publish.R) and the platform's resolve_schema_path (metill-platform/scripts/validate_publish.py:46-68) both try only <root>/<sport>/<name>.schema.json then <root>/<name>.schema.json, and "_draft" matches neither.
  6c. WS11 task 6 (git rm the 32 stale un-suffixed bb/hb JSONs). BREAKS FOOTBALL AND THE PLATFORM IF RUN AFTER 6d: those cells' meta.json carries no `division` and no `is_cup` (verified on disk), both schema-required, so arming with them present aborts football's publish R-side AND makes validate_publish.py exit non-zero platform-side, freezing fly.metill.is on the last-known-good payload. Safe on the platform's rsync guard: the tree has 134 JSONs today and the workflow refuses below 50, so 134 -> 102 clears it (verified in metill-platform/.github/workflows/pull-sports-data.yml:84-97).
  6d. WS11 task 7 (the arming git mv) — after 6b, 6c AND WS9 task 7. This turns the platform's Python validator fail-closed on the next pull-sports-data run (7x/day at 25 7-12,19 UTC), which is why 6c precedes it.
  6e. WS11 task 8 (fail-closed default) — after 6a and 6d. Verified safe for world_cup: publish_world_cup() (R/wc-publish.R) never calls publish_one() or .validate_or_abort(), so the inversion cannot touch it; re-grep before landing rather than trusting this line.
  6f. WS11 task 9 (cross-repo proof) and WS12 tasks 5-8 (compose into pipeline_health, fit isolation, final pass).

ONE MORE ORDERING HAZARD, not in any workstream's list: WS12 task 6 (run_fit_targets) should land BEFORE the first real 2DT fit is attempted, not after. Verified scripts/03_fit.R:32-54 calls fit_one(static, row$sex) with no tryCatch, and basketball (config/leagues.yml:15) and handball (:88) both precede football (:135), so one 2DT diagnostics-gate abort kills football's fits in the same run. The first live 2DT fits in five months are the highest-abort-risk event of the season and they run first in config order.

---

## Known gaps the crosscheck found and no workstream drafted

- The skip-hygiene guarded-vector update that must accompany WS9 task 7's deletion of test-publish-basketball.R and test-publish-handball.R (SC-9). No workstream drafts it; the build goes red without it.
- Relaxing config/publish-schemas/_base/meta.schema.json's `division` pattern from ^[A-Z][A-Z0-9_]*$ to ^[A-Z0-9][A-Z0-9_]*$ so basketball's 1D validates, and dropping p_top_six/p_winner from final_positions' `required` in the bb/hb deltas (SC-10). Handed from WS10 to WS11 and dropped by both.
- A test that the bb/hb publish cell directories actually moved from data/publish/{sport}/iceland/{karla,kvenna}/ to {karla,kvenna}-{slug}/. WS11 task 6 deletes the old ones and WS9 task 7 asserts the new eight exist, but nothing asserts the transition itself is complete — i.e. that no code path still writes the un-suffixed shape. Add to WS9 task 7: after publishing all 8 cells into a tempdir, assert every directory under publish/{basketball,handball}/iceland/ matches ^(karla|kvenna)-[a-z0-9]+$ (WS11 task 6 has this assertion but only against the real repo tree, not against fresh publisher output).
- Nothing threads `end_date` from WS12's run_publish_targets() into publish_one(). Production wants the Sys.Date() default, so this is correct by accident, but WS12's run_publish_targets signature must name the parameter explicitly rather than dropping it, or a future replay caller silently loses it.
- No workstream verifies that the platform's rsync guard survives WS11 task 6's deletion. I checked: 134 JSONs today, guard refuses below 50, 134 - 32 = 102. Record the measured numbers in WS11 task 6's commit body so a future larger deletion is checked against the real threshold rather than assumed safe.
- WS8's `stopifnot(is.null(league$training_filter))` guard (task 5) protects the trajectory's round indexing, but no workstream adds the equivalent statement to .claude/rules/ or to config/leagues.schema.json. Verified config/leagues.yml:300 is training_filter's sole occurrence (football only), so the guard is correct today — but a future session adding one to basketball gets an abort with no documentation of why. Add one line to .claude/rules/publish-layer.md in WS7 task 6's doc pass.
- Nobody drafts the removal of the p_top_six deprecated alias, only its introduction. WS10 task 4's roxygen names 'the follow-up commit whose only job is that removal, after metill-platform reads p_qualify' — that commit needs to exist as a tracked follow-up, not as prose in a docstring.
- No workstream states what happens to data/publish/{basketball,handball} on the metill-platform side after WS11 task 6's --delete rsync. I verified the platform has no basketball or handball routes today (grep over app/routes/*.py returns nothing), so the deletion is free — but that fact should be recorded in the commit body, because it stops being free the moment WS13 wires the routes.

## Draft claims the crosscheck checked and found FALSE

- WS10 task 6: 'tests/testthat/fixtures/golden/football-publish-hashes.csv (93 rows = 9 cells x 10 artefacts + header)'. FALSE arithmetic and a false uniformity claim. Verified: the file is 93 lines = 1 header + 92 payloads, and the cells are not uniform — 7 cells hash 10 artefacts each and the two bikar cells hash 11 each (on disk the bikar cells carry 12 files, including bracket.json). WS10's derived assertion 'exactly 18 changed lines (9 meta.json + 9 final_positions.json)' therefore needs verifying rather than asserting, and WS9's B4 acceptance wording 'the ten JSON basenames football emits' must become 'the basenames in sport_publish_profile(sport)$surfaces'.
- WS8 task 4 and WS7 both claim sole ownership of .iceland_division_expected_meetings(); WS8 states 'WS7 MUST NOT also define this'. Both cannot be true — see SC-4. WS8's related claim that the helper belongs in R/extract-football-iceland.R is also wrong once WS7 task 3 creates R/publish-divisions.R as the home for all such accessors.
- WS9 task 4 claims it can pass `.football_iceland_division_code_labels()` into the new helper and swap it to config later in task 5. FALSE once WS7 lands: WS7 task 4 deletes that function with no alias and rewrites all 43 call sites (I verified the count is exactly 43 across the 8 files WS7 names, matching its claim). WS9 task 4 would be written against a deleted symbol.
- WS11 task 4 consumes 'n_rounds_source in {schedule, config}'. FALSE relative to WS10, which produces four values — config, schedule, none, not_applicable. A two-value enum in _base would reject the cup ('not_applicable') and the unconfigured-and-unschedulable ('none') cases that WS10 deliberately emits.
- WS11's plan (tasks 3, 5, 7) omits relaxing meta.schema.json's `division` pattern. I verified config/publish-schemas/football/meta.schema.json:15 is `^[A-Z][A-Z0-9_]*$`, which rejects basketball's configured code '1D'. WS10 flagged this as a hazard FOR WS11; WS11 never picked it up, so as drafted this is dropped work, not a divergence of opinion.
- WS9 task 7's step 'git rm tests/testthat/test-publish-basketball.R tests/testthat/test-publish-handball.R' followed by 'full suite -> FAIL 0' is self-contradictory. Verified tests/testthat/test-fixture-skip-hygiene.R:6-22 lists both filenames in `guarded` and asserts their existence via expect_setequal, so the suite cannot be FAIL 0 after the deletion without the vector edit WS9 never drafts.

---

# WS7 — publish_divisions config, schema keys, and generalised division helpers (spec §9)

**Goal.** Make the per-sex publish-division registry sport-neutral, so basketball {BD, 1D} and handball {OD, G66} are describable in config exactly as football's five cells are, and so one set of `.iceland_division_*(key, sex)` accessors serves all three sports. Config-and-accessors only: WS7 changes ZERO published JSON bytes (the football golden manifest at tests/testthat/fixtures/golden/football-publish-hashes.csv must stay green with its committed hashes untouched), and WS8/WS9/WS10 are the consumers that turn this config into output.

**Consumes.**

- load_leagues(path, schema_path, validate) — R/config.R:8; validate_leagues() — R/config.R:49, which stop()s with the literal prefix "leagues.yml failed schema validation:" (R/config.R:62)
- config/leagues.schema.json :: definitions.publishDivisionList — currently at config/leagues.schema.json:127-152, items additionalProperties:false, required [code, slug, label_is, is_cup], existing optional key `split` only (N2)
- config/leagues.schema.json :: properties.<league>.properties.publish_divisions — config/leagues.schema.json:68-75, $ref to publishDivisionList per sex
- Plan A WS1 invariant: basketball_iceland.betting.enabled == FALSE and handball_iceland.betting.enabled == FALSE, and lengjan.competitions is [] for both (config/leagues.yml:25 and :100). WS7 edits the same two league blocks — do NOT resurrect the commented-out competition ids.
- config/publish-schemas/football/next_games.schema.json :: properties.matches.items.properties.division_code.pattern == "^[A-Z][A-Z0-9_]*$" (verified this session) — the sole hard constraint that forces basketball 1D to carry a code_badge, since "1D" starts with a digit and fails the pattern
- tests/testthat/fixtures/golden/football-publish-hashes.csv (Plan A WS2) — the regression net; its hashes must NOT be regenerated by WS7
- data/facts/results (git-tracked Parquet) — read by the qualify-slots sanity test for current-season team counts per (sport, sex, division)
- tests/testthat/test-fixture-skip-hygiene.R :: the `guarded` filename vector (tests/testthat/test-fixture-skip-hygiene.R:6-14) — WS7's new test file is appended to it

**Produces.**

- R/publish-divisions.R (new file, @noRd throughout, roxygen `#' @include config.R`) exporting nothing; DESCRIPTION Collate regenerated by devtools::document()
- .iceland_division_codes(key, sex) -> character() in YAML order. `key` is a config/leagues.yml top-level league key ("football_iceland" | "basketball_iceland" | "handball_iceland"); `sex` in c("male","female"). stop()s on an unknown sex or an absent publish_divisions[[sex]] block.
- .iceland_division_slugs(key, sex) -> named character(), names = code, values = slug
- .iceland_division_labels(key, sex) -> named character(), names = code, values = label_is
- .iceland_division_split(key, sex) -> named list(), names = code, each element NULL or list(upper = <int>, lower = <int>)
- .iceland_division_badges(key, sex) -> named character(), names = code, values = code_badge (falling back to `code` when code_badge is absent), PLUS derived entries `<code>_UPPER_PO` -> paste0(badge, "U") and `<code>_LOWER_PO` -> paste0(badge, "L") for every entry that carries a `split` object. This reproduces the retired static map on every code reachable through publish_football_iceland()'s `division %in% family_divs` filter (R/publish-football-iceland.R:1012, recode at :1043).
- .iceland_division_is_cup(key, sex) -> named logical(), names = code
- .iceland_division_qualify(key, sex) -> named list(), names = code, each element NULL (no `qualify` key configured -> consumer publishes meta.qualify: null and emits no p_qualify) or list(slots = <int>, label_is = <chr>)
- .iceland_division_relegation(key, sex) -> named integer(), names = code, NA_integer_ where `relegation_slots` is unset
- .iceland_division_expected_meetings(key, sex) -> named integer(), names = code, NA_integer_ where `expected_meetings` is unset (basketball female 1D is deliberately unset)
- config/leagues.schema.json :: definitions.publishDivisionList.items.properties gains FOUR optional keys, none added to `required`: code_badge (string, pattern ^[A-Z][A-Z0-9_]{1,3}$), expected_meetings (integer, minimum 1), relegation_slots (integer, minimum 0), qualify (object, additionalProperties:false, required [slots, label_is], slots integer minimum 1, label_is string minLength 1)
- config/leagues.yml :: basketball_iceland.publish_divisions.{male,female} = [BD/bd/"Bónusdeild"/BON, 1D/1d/"1. deild"/B1D]; handball_iceland.publish_divisions.{male,female} = [OD/od/"Olísdeild"/OD, G66/g66/"Grill 66-deild"/G66]. Every entry carries is_cup:false. No cup division is ingested for either sport (verified: distinct `division` values for season 2026 are basketball {BD,1D}, handball {OD,G66,PO}), so there is no cup cell and PO is NOT a publish division.
- config/leagues.yml :: football_iceland.publish_divisions entries gain code_badge with values byte-identical to the retired static map: BD=BD, LD1=LD, LD2=D2, LD3=D3, CUP=MB. Football BD (both sexes) also gains qualify: { slots: 6, label_is: "Efri hluti" } — 6 is split$upper, already verified 2026-07-10, and makes p_qualify numerically identical to the existing p_top_six (R/publish-football-iceland.R:1422 uses placement <= 6L), which is exactly the alias relationship spec §11 describes.
- DELETED with no alias: .football_iceland_division_codes/slugs/labels/split (R/extract-football-iceland.R:40-141) and .football_iceland_division_code_labels (R/extract-football-iceland.R:143-152). 43 references across 8 live-tree files are rewritten in the same commit.

### Task 1: Schema: four optional keys on publishDivisionList.items

**Files:**

- Modify: /Users/brynjolfurjonsson/sports/config/leagues.schema.json (definitions.publishDivisionList.items.properties, currently lines 130-150)
- Test (create): /Users/brynjolfurjonsson/sports/tests/testthat/test-publish-divisions-schema.R

- [ ] Step 1 — write the failing test FIRST. Create tests/testthat/test-publish-divisions-schema.R with three test_that blocks, all driven through load_leagues(path = tmp) on a withr::local_tempfile() written with yaml::as.yaml(). Block A: a minimal football_iceland league whose publish_divisions.male entry carries code, slug, label_is, is_cup PLUS code_badge = "BD", expected_meetings = 2L, relegation_slots = 2L and qualify = list(slots = 6L, label_is = "Efri hluti") -> expect_no_error(load_leagues(path = tmp)). Block B: the same entry with an undeclared key (nonsense = 1L) -> expect_error(load_leagues(path = tmp), "leagues.yml failed schema validation"), proving additionalProperties:false is still armed after the edit. Block C: the same entry with code_badge = "1D" -> expect_error(load_leagues(path = tmp), "leagues.yml failed schema validation"), proving the badge pattern rejects a leading digit. Do NOT touch config/leagues.yml in this task. NOTE for the minimal league bodies: `betting` is additionalProperties:false with required [kelly_frac, ev_threshold, markets, scoring, min_bet] — copy the shape from tests/testthat/test-config-betting-schema.R rather than inventing one.

- [ ] Step 2 — RUN it and state the RED. `Rscript -e 'devtools::load_all(); testthat::test_local(filter = "publish-divisions-schema")'` from /Users/brynjolfurjonsson/sports. EXPECTED RED: Block A FAILS with an error whose message begins `leagues.yml failed schema validation:` and whose body contains `must NOT have additional properties` at instancePath `/football_iceland/publish_divisions/male/0` (ajv, via validate_leagues at R/config.R:52-62). Blocks B and C PASS for the wrong reason — every unknown key is currently rejected, so Block C cannot yet distinguish a bad pattern from an undeclared key. Record both facts; Block C only becomes meaningful after Step 3.

- [ ] Step 3 — implement. In config/leagues.schema.json, inside definitions.publishDivisionList.items.properties (alongside the existing code/slug/label_is/is_cup/split), add exactly four properties and add NONE of them to `required`: (a) "code_badge": { "type": "string", "pattern": "^[A-Z][A-Z0-9_]{1,3}$", "description": "Short ASCII filter key emitted as next_games.json::division_code. MUST satisfy config/publish-schemas/football/next_games.schema.json's ^[A-Z][A-Z0-9_]*$ — basketball's 1D fails that pattern on its own, which is why this key exists. Optional; absent falls back to `code`." }; (b) "expected_meetings": { "type": "integer", "minimum": 1, "description": "Times each pair meets in the REGULAR season. An assertion, not a source: n_rounds is derived from schedule+results and this value is only the fallback and the WARN comparator (spec §12). Omit where the format is genuinely irregular." }; (c) "relegation_slots": { "type": "integer", "minimum": 0, "description": "Teams relegated from this division. Replaces the hardcoded bottom-two rule (placement >= n_teams - 1L) at R/publish-football-iceland.R:1424, R/publish-basketball-iceland.R:261 and R/publish-handball-iceland.R:256, which is wrong for a bottom-tier division where nothing is relegated." }; (d) "qualify": { "type": "object", "additionalProperties": false, "required": ["slots", "label_is"], "properties": { "slots": { "type": "integer", "minimum": 1 }, "label_is": { "type": "string", "minLength": 1 } }, "description": "Post-season qualification cutoff, published as meta.qualify and used for summary[].p_qualify. Mirrors meta.qualify's own {slots, label_is} shape so no label lives in R. Absent = meta.qualify: null and no p_qualify emitted." }. Write the file with python3 json round-trip or a cat <<'EOF' heredoc, NOT the Edit tool — the descriptions are ASCII here but neighbouring keys in the file are not.

- [ ] Step 4 — re-run the same command. EXPECTED GREEN: all three blocks pass. Then run `Rscript -e 'devtools::load_all(); print(names(load_leagues()))'` and confirm the real config/leagues.yml still validates (the schema edit is purely additive; if this aborts, the JSON is malformed).

- [ ] Step 5 — commit. `git -C /Users/brynjolfurjonsson/sports add config/leagues.schema.json tests/testthat/test-publish-divisions-schema.R && git -C /Users/brynjolfurjonsson/sports commit`. Message: `feat(config): declare code_badge/expected_meetings/relegation_slots/qualify on publishDivisionList` with a body noting N2 (items is additionalProperties:false, so bb/hb config is un-writable without this) and that none of the four is required. End with the Co-Authored-By trailer. Do NOT push.

**Verification.** load_leagues() accepts a publish_divisions entry carrying all four new keys, still rejects an undeclared key, and rejects a code_badge starting with a digit — each proven by running the test, not by reading the schema. The unedited config/leagues.yml still loads.

### Task 2: Football config: move the static badge map into publish_divisions as code_badge, and add BD's qualify

**Files:**

- Modify: /Users/brynjolfurjonsson/sports/config/leagues.yml (football_iceland.publish_divisions, lines 310-330)
- Modify: /Users/brynjolfurjonsson/sports/tests/testthat/test-publish-divisions-config.R (the `optional` vector at :14)
- Test (append): /Users/brynjolfurjonsson/sports/tests/testthat/test-publish-divisions-config.R

- [ ] Step 1 — write the failing test FIRST. Append to tests/testthat/test-publish-divisions-config.R a block `test_that("football publish_divisions carries the legacy badge map as code_badge", ...)`: read `legacy <- .football_iceland_division_code_labels()` (still live at R/extract-football-iceland.R:143) and, for each sex and each entry in load_leagues()[["football_iceland"]][["publish_divisions"]][[sex]], expect_identical(entry$code_badge, unname(legacy[[entry$code]])). Add a second block asserting `bd$qualify` equals list(slots = 6L, label_is = "Efri hluti") for BOTH sexes, with an inline comment that 6 is split$upper (config/leagues.yml:317 and :327, verified 2026-07-10) so p_qualify reproduces the existing p_top_six rule `placement <= 6L` at R/publish-football-iceland.R:1422 exactly.

- [ ] Step 2 — RUN and state the RED. `Rscript -e 'devtools::load_all(); testthat::test_local(filter = "publish-divisions-config")'`. EXPECTED RED: the first new block fails with `expect_identical(entry$code_badge, ...) : NULL not identical to "BD"` (entry$code_badge is NULL because the key is absent from YAML); the second fails with `NULL not identical to list(slots = 6L, label_is = "Efri hluti")`. Both failures name the sex/entry index. All PRE-EXISTING blocks in this file still pass at this point.

- [ ] Step 3 — implement the YAML. Edit config/leagues.yml's football_iceland.publish_divisions with a python3 heredoc or `cat <<'EOF'` splice on ASCII anchors — the block contains "Mjólkurbikar", "Lengjudeild", "Bikar kvenna", and the Edit/Write tools mishandle these. Add `code_badge:` to all nine entries with EXACTLY the retired map's values (BD=BD, LD1=LD, LD2=D2, LD3=D3, CUP=MB) and add `qualify: { slots: 6, label_is: "Efri hluti" }` to the two BD entries only. Add a YAML comment above the male list recording: code_badge values are lifted verbatim from the retired .football_iceland_division_code_labels() and are mirrored by metill-platform's DIVISIONS dict — changing one requires changing both. Leave every existing key byte-unchanged.

- [ ] Step 4 — fix the pre-existing unknown-fields test that this deliberately breaks. tests/testthat/test-publish-divisions-config.R:14 reads `optional <- "split"`; the config edit makes it fail with `sex=male entry 1 has unknown fields`. Widen it to `optional <- c("split", "code_badge", "expected_meetings", "relegation_slots", "qualify")`, keeping the required vector at c("code","slug","label_is","is_cup"). Confirm you SAW that failure before widening — it is the proof the test is live.

- [ ] Step 5 — re-run. `Rscript -e 'devtools::load_all(); testthat::test_local(filter = "publish-divisions")'` -> GREEN. Then run the golden gate: `Rscript -e 'devtools::load_all(); testthat::test_local(filter = "publish-football-golden")'` and confirm 0 failures and that tests/testthat/fixtures/golden/football-publish-hashes.csv is UNMODIFIED (`git -C /Users/brynjolfurjonsson/sports status --short tests/testthat/fixtures/golden/` prints nothing). A config-only key addition must not move a single published byte; if a hash changed, stop — something reads publish_divisions more broadly than assumed.

- [ ] Step 6 — commit config/leagues.yml + the test file. Message: `feat(config): carry football division badges and BD qualify in publish_divisions`, body explaining that the static R map is being retired in favour of config so basketball BD (Bónusdeild) and football BD (Besta deild) stop sharing one filter key, and that qualify.slots = split$upper reproduces p_top_six exactly. Co-Authored-By trailer. No push.

**Verification.** Every football publish_divisions entry carries a code_badge byte-identical to the map it replaces, both BD entries carry qualify {6, "Efri hluti"}, and the football golden hashes are unchanged and un-regenerated — proving a config-only change published nothing new.

### Task 3: R/publish-divisions.R: the nine sport-neutral accessors

**Files:**

- Create: /Users/brynjolfurjonsson/sports/R/publish-divisions.R
- Modify: /Users/brynjolfurjonsson/sports/DESCRIPTION (Collate — regenerated, not hand-edited)
- Test (create): /Users/brynjolfurjonsson/sports/tests/testthat/test-iceland-division-helpers.R

- [ ] Step 1 — write the failing test FIRST. Create tests/testthat/test-iceland-division-helpers.R. Block A (spec verification (c), the football-unchanged proof): for both sexes, expect_identical(.iceland_division_codes("football_iceland", sex), .football_iceland_division_codes(sex)) and the same for slugs, labels and split against their .football_iceland_* counterparts — the old functions are still live, so this is a genuine byte-for-byte equivalence assertion, not a restatement of literals. Block B: `.iceland_division_badges("football_iceland", sex)` agrees with `.football_iceland_division_code_labels()` on every name it defines (expect_identical over the intersection) AND defines BD_UPPER_PO = "BDU" and BD_LOWER_PO = "BDL" for both sexes; document in a comment that LD1_PO and LD4 are deliberately dropped because they are unreachable — publish_football_iceland() filters `division %in% family_divs` (R/publish-football-iceland.R:1012) before the recode at :1043, and family_divs is target_div plus its split-family codes only (.split_family_divisions_pfi, R/extract-football-iceland.R:126-135), so no payload can carry them. Block C: `.iceland_division_is_cup("football_iceland", "male")[["CUP"]]` is TRUE and `[["BD"]]` is FALSE. Block D: `.iceland_division_qualify("football_iceland", "male")$BD` equals list(slots = 6L, label_is = "Efri hluti") and `$LD1` is NULL. Block E: `.iceland_division_relegation` and `.iceland_division_expected_meetings` return named integer vectors of the right length, all NA_integer_ for football (neither key is configured there yet). Block F: every accessor stop()s on sex = "nonsense" and on key = "world_cup" (a league key with no publish_divisions block).

- [ ] Step 2 — RUN and state the RED. `Rscript -e 'devtools::load_all(); testthat::test_local(filter = "iceland-division-helpers")'`. EXPECTED RED: every block errors with `could not find function ".iceland_division_codes"` (and the analogous message per accessor). This is the whole-file RED; no partial pass is acceptable.

- [ ] Step 3 — implement R/publish-divisions.R. Head the file with `#' @include config.R` + `NULL`. Write ONE private worker, `.iceland_division_entries(key, sex, caller)`, that does the stopifnot(sex %in% c("male","female")), reads load_leagues()[[key]][["publish_divisions"]][[sex]], and stop()s with a message naming `caller`, `key` and `sex` when the block is NULL or length 0 — this is the one place the boilerplate currently duplicated five times at R/extract-football-iceland.R:40-141 lives. Then nine thin wrappers over it: codes (vapply code), slugs / labels / badges (setNames over code), split / qualify (lapply returning NULL or an integer/character-coerced list, matching the existing .football_iceland_division_split shape at R/extract-football-iceland.R:100-124), is_cup (vapply logical), relegation and expected_meetings (vapply integer, NA_integer_ when the key is absent). In `.iceland_division_badges`, resolve each entry's badge as `if (is.null(d$code_badge)) d$code else d$code_badge`, then for every entry carrying a non-NULL `split` append two derived names, paste0(code, "_UPPER_PO") -> paste0(badge, "U") and paste0(code, "_LOWER_PO") -> paste0(badge, "L"). Mark every function `#' @keywords internal` + `#' @noRd`. Add a file-level comment stating the sole hard constraint on badges: config/publish-schemas/football/next_games.schema.json's ^[A-Z][A-Z0-9_]*$, which "1D" violates.

- [ ] Step 4 — regenerate docs/Collate. `Rscript -e 'devtools::document()'` from the repo root, then `git -C /Users/brynjolfurjonsson/sports diff DESCRIPTION` and confirm the only change is `'publish-divisions.R'` appearing in Collate. Do not hand-edit Collate.

- [ ] Step 5 — re-run the filtered test -> GREEN, then run the full suite `Rscript -e 'devtools::load_all(); devtools::test()'` and confirm the FAIL count is 0 and SKIP is 45 (the Plan A baseline, observed 2026-09-04). The old .football_iceland_* helpers are still present and still called at this point, so nothing else may move.

- [ ] Step 6 — append "test-iceland-division-helpers.R" to the `guarded` vector in tests/testthat/test-fixture-skip-hygiene.R:6-14 so the new coverage can never acquire a skip gate, and re-run that file. Commit R/publish-divisions.R, DESCRIPTION, the two test files and any NAMESPACE churn. Message: `feat(publish): add sport-neutral .iceland_division_* accessors`. Co-Authored-By trailer. No push.

**Verification.** For both sexes, the four new football accessors return output identical to the four functions they will replace — asserted against the live originals in the same process, not against copied literals. The derived badge map reproduces BDU/BDL. Full suite stays at FAIL 0 / SKIP 45.

### Task 4: Cut over all 43 call sites and delete the football-only helpers

**Files:**

- Modify: /Users/brynjolfurjonsson/sports/R/extract-football-iceland.R (14 refs; delete the five helper definitions at :40-152, keep .split_family_divisions_pfi at :126-135, update the `#' @include` line at :1)
- Modify: /Users/brynjolfurjonsson/sports/R/publish-football-iceland.R (4 refs: :742, :744, :797, :1009)
- Modify: /Users/brynjolfurjonsson/sports/R/backfill-final-positions.R (1 ref, a roxygen mention at :24)
- Modify: /Users/brynjolfurjonsson/sports/scripts/backfill_final_positions_history.R (2 refs: :40, :108)
- Modify: /Users/brynjolfurjonsson/sports/tests/testthat/helper-extract-fixtures.R (1 ref, :28)
- Modify: /Users/brynjolfurjonsson/sports/tests/testthat/test-publish-football-golden.R (1 ref, :57)
- Modify: /Users/brynjolfurjonsson/sports/tests/testthat/test-extract-football-iceland.R (1 ref, :74)
- Modify: /Users/brynjolfurjonsson/sports/tests/testthat/test-publish-divisions-config.R (19 refs)

- [ ] Step 1 — write the failing test FIRST (spec verification (d)). Append to tests/testthat/test-iceland-division-helpers.R a block `test_that("no .football_iceland_division_ references survive in the live tree", ...)`: shell out with system2("grep", c("-rl", "--include=*.R", "--include=*.Rd", "--include=*.qmd", "\\.football_iceland_division_", "R", "scripts", "tests/testthat", "man"), stdout = TRUE) rooted at rprojroot::find_package_root_file(), and expect_length(hits, 0L) with the hit list in `info`. Search ONLY R/, scripts/, tests/testthat/ and man/ — .claude/worktrees/ holds three stale checkouts (angry-jackson-c4c357, crazy-rhodes-a4ad59, sharp-jepsen-e3a666) and docs/ + _legacy/ hold historical prose; all are out of scope and a tree-wide grep would make this test permanently red.

- [ ] Step 2 — RUN and state the RED. `Rscript -e 'devtools::load_all(); testthat::test_local(filter = "iceland-division-helpers")'`. EXPECTED RED: `expect_length(hits, 0L)` fails with `hits has length 8, not length 0`, listing R/backfill-final-positions.R, R/extract-football-iceland.R, R/publish-football-iceland.R, scripts/backfill_final_positions_history.R, tests/testthat/helper-extract-fixtures.R, tests/testthat/test-publish-divisions-config.R, tests/testthat/test-publish-football-golden.R, tests/testthat/test-extract-football-iceland.R. Confirm the count with `grep -rn '\.football_iceland_division_' R/ scripts/ tests/testthat/ | grep -v worktrees | wc -l` -> 43 (measured 2026-09-04).

- [ ] Step 3 — rewrite the R call sites. R/extract-football-iceland.R: delete the five helper definitions (.football_iceland_division_codes/slugs/labels/split at :40-141 and .football_iceland_division_code_labels at :143-152) but KEEP .split_family_divisions_pfi at :126-135 and .summarise_quantile_band_pfi at :11-25; add publish-divisions.R to the `#' @include` line at :1; rewrite :1510, :1515, :1678, :1801, :1806 to `.iceland_division_codes("football_iceland", sex)` / `.iceland_division_split("football_iceland", sex)`. R/publish-football-iceland.R: :742 -> .iceland_division_slugs("football_iceland", sex); :744 -> .iceland_division_labels(...); :797 -> .iceland_division_split("football_iceland", sex)[[target_div]]; :1009 -> `division_labels <- .iceland_division_badges("football_iceland", sex)` — note this call site MOVES INSIDE the per-division loop's scope requirements only if `sex` is not already in scope there; verify `sex` is a live binding at :1009 before editing (it is the publisher's own argument). Leave the `!!!division_labels` recode at :1043 and its `.default = .data$division` untouched. R/backfill-final-positions.R:24 is roxygen prose — update the name and re-run devtools::document().

- [ ] Step 4 — rewrite the script and test call sites. scripts/backfill_final_positions_history.R:40 -> setdiff(.iceland_division_codes("football_iceland", sex), "CUP"); :108 -> .iceland_division_split("football_iceland", sex). tests/testthat/helper-extract-fixtures.R:28, test-publish-football-golden.R:57, test-extract-football-iceland.R:74 -> the new names. In tests/testthat/test-publish-divisions-config.R rewrite all 19 refs; the block at :146 that iterates `names(.football_iceland_division_code_labels())` becomes an iteration over `.iceland_division_badges("football_iceland", sex)` per sex, keeping the expect_match against the schema pattern read from config/publish-schemas/football/next_games.schema.json — that pattern read is the point of the block and must survive.

- [ ] Step 5 — document and run. `Rscript -e 'devtools::document()'` then `Rscript -e 'devtools::load_all(); devtools::test()'`. EXPECTED: FAIL 0, SKIP 45, and the grep block from Step 1 now GREEN. Then run the golden gate specifically: `Rscript -e 'devtools::load_all(); testthat::test_local(filter = "publish-football-golden")'` -> 0 failures, and `git -C /Users/brynjolfurjonsson/sports status --short tests/testthat/fixtures/golden/` prints nothing. This is the whole-task gate: a pure rename that moves a published byte is not a rename.

- [ ] Step 6 — commit everything in ONE commit (a half-cut-over tree does not load). Message: `refactor(publish): rename .football_iceland_division_* to .iceland_division_*(key, sex)`, body noting: no compatibility aliases (spec §9 — two live names for one symbol is the drift this removes), 43 references across 8 files, golden hashes unchanged. Co-Authored-By trailer. No push.

**Verification.** grep over R/, scripts/, tests/testthat/ and man/ returns zero .football_iceland_division_ hits; devtools::test() is FAIL 0 / SKIP 45; the football golden test passes against its committed, unregenerated hashes — so football's nine published cells are provably byte-unchanged by the rename.

### Task 5: basketball_iceland and handball_iceland publish_divisions blocks

**Files:**

- Modify: /Users/brynjolfurjonsson/sports/config/leagues.yml (basketball_iceland block, lines 15-86; handball_iceland block, lines 87-132)
- Test (append): /Users/brynjolfurjonsson/sports/tests/testthat/test-iceland-division-helpers.R

- [ ] Step 1 — resolve the qualification numbers BEFORE writing config, and record provenance. `qualify.slots` is the only value in this task not measured from data: it drives the headline p_qualify and meta.qualify.label_is that render on the page, so a plausible-but-wrong number is a silently mislabelled published probability. Read the 2026-27 competition regulations — KKÍ reglugerðir (kki.is) for Bónusdeild and 1. deild, both sexes; HSÍ mótareglugerð (hsi.is) for Olísdeild and Grill 66-deild, both sexes — and record, as a YAML comment directly above each entry, the source URL and the retrieval date. The spec's unverified starting hypotheses, to be CONFIRMED OR REFUTED not copied: bb BD 8 qualify / 2 relegation both sexes; bb 1D 8 qualify / 0 relegation; hb male OD 8 qualify / 2 relegation; hb female OD 4 qualify / 1 relegation; hb G66 0 qualify / 0 relegation both sexes. FAIL-SAFE, and it is not a fallback of last resort but the correct action: if a regulation cannot be resolved, OMIT the `qualify` key for that division entirely. An absent key publishes meta.qualify: null and emits no p_qualify — honest and inert. Likewise omit `relegation_slots` rather than guessing. Only `expected_meetings` may be written from measurement.

- [ ] Step 2 — write the failing test FIRST. Append four blocks to tests/testthat/test-iceland-division-helpers.R. Block A: for key in c("basketball_iceland","handball_iceland") and both sexes, expect_identical(.iceland_division_codes(key, sex), c("BD","1D")) resp. c("OD","G66"); slugs c(BD="bd", `1D`="1d") resp. c(OD="od", G66="g66"); labels "Bónusdeild"/"1. deild" resp. "Olísdeild"/"Grill 66-deild"; badges c(BD="BON", `1D`="B1D") resp. c(OD="OD", G66="G66"); all is_cup FALSE. Write the Icelandic strings with a python3 heredoc or `cat <<'EOF'`, never Write/Edit. Block B (badge/schema agreement, all three leagues): read the pattern from config/publish-schemas/football/next_games.schema.json (properties.matches.items.properties.division_code.pattern) and expect_match every value of .iceland_division_badges(key, sex) for all three keys x both sexes — this is the block that would catch a 1D shipped without a badge. Block C (expected_meetings against data): read data/facts/results via read_table(root = here::here("data")), take the max season present, and for each configured (key, sex, code) with rows, cut to round <= expected_meetings * (n_teams - 1), count distinct unordered pairs, and expect that every pair inside the cut appears exactly expected_meetings times; skip the assertion — with an `if (nrow(x) > 0L && !is.na(em))` guard, never skip() — where expected_meetings is NA. Count the assertions made in a counter and expect_gt(counter, 0L) so the loop cannot silently assert nothing. Block D (qualify sanity, spec verification (b)): for every configured division with a `qualify` object, expect_true(q$slots >= 1L && q$slots < n_teams) against the same current-season team counts, again with a non-zero-assertions guard.

- [ ] Step 3 — RUN and state the RED. `Rscript -e 'devtools::load_all(); testthat::test_local(filter = "iceland-division-helpers")'`. EXPECTED RED: Blocks A, B and D error out of .iceland_division_entries() with the caller-named stop from Task 3 — `.iceland_division_codes: no publish_divisions["male"] entry for basketball_iceland in config/leagues.yml` — because neither league has a publish_divisions block. Block C fails the same way. This is the RED that proves the accessors reach config rather than a default.

- [ ] Step 4 — implement the YAML with a python3 heredoc ("Bónusdeild", "Olísdeild", "Grill 66-deild" are non-ASCII). Insert a publish_divisions block into basketball_iceland (after `stan_model:` at config/leagues.yml:58, before `betting:` at :59) and into handball_iceland (after `stan_model:` at :113, before `betting:` at :114). Do NOT touch `lengjan.competitions: []` or `betting.enabled: false` in either block — Plan A WS1 owns those and resurrecting the commented-out competition ids would arm placement against D2. Values, all measured from data/facts/results season 2026 this session: basketball male BD {expected_meetings 2 — 12 teams, cut at round <= 22, 132 matches, all 66 pairs exactly 2x}; basketball male 1D {expected_meetings 2 — 12 teams, 132 matches in-cut, all 66 pairs 2x}; basketball female BD {expected_meetings 2 — 10 teams, cut <= 18, 90 matches, all 45 pairs 2x}; basketball female 1D {expected_meetings DELIBERATELY OMITTED — 11 teams, in-cut pair table is 1x1, 2x44, 4x1, i.e. genuinely irregular with byes, so there is no correct constant and the schedule-derived n_rounds must be the only source}; handball male OD and G66 {expected_meetings 2 — 12 teams, 132 matches, 66 pairs, no post-season rows in the division}; handball female OD and G66 {expected_meetings 3 — 8 teams, 84 matches, 28 pairs, exactly 3.0 meetings per pair: a TRIPLE round robin, and the cell that breaks the platform's 2*(n_teams-1) assumption}. Add a comment above the handball female list stating that triple-RR fact explicitly. Add code_badge BON/B1D for basketball and OD/G66 for handball, with a comment explaining that "1D" is not usable as its own badge (leading digit fails ^[A-Z][A-Z0-9_]*$) and that BON keeps basketball's Bónusdeild off football's BD filter key.

- [ ] Step 5 — re-run. `Rscript -e 'devtools::load_all(); print(names(load_leagues()))'` FIRST (spec verification (a) — leagues.schema.json is additionalProperties:false at the league level too, so a mistyped key here takes every script down; run it, do not assume). Then the filtered test -> GREEN, then the full suite -> FAIL 0 / SKIP 45, then the golden gate -> 0 failures with the hash CSV unmodified. Note that Block C is a live-data assertion by design: when a division's format changes next season it is SUPPOSED to go red, and the fix is to re-measure and re-write expected_meetings, not to loosen the test.

- [ ] Step 6 — commit config/leagues.yml + the test file. Message: `feat(config): publish_divisions for basketball {BD,1D} and handball {OD,G66}`, body recording the measured expected_meetings per cell, the deliberate omission for basketball female 1D, and the regulation source URL + retrieval date behind every qualify/relegation number (or, where a regulation was unresolvable, that the key was omitted rather than guessed). Co-Authored-By trailer. No push.

**Verification.** load_leagues() validates the real config with both new blocks. The nine accessors return the eight expected publish cells with the right slugs, Icelandic labels and badges. expected_meetings is not asserted against itself — each value is re-derived from data/facts/results by counting pair meetings inside its own round cut, and the assertion counter proves the loop actually ran. Every configured qualify.slots is strictly below its division's current team count. Football's golden hashes are still untouched.

### Task 6: Close the seams: three-league config invariants, rules doc, and the no-cup / no-PO statement

**Files:**

- Modify: /Users/brynjolfurjonsson/sports/tests/testthat/test-publish-divisions-config.R (generalise the football-only invariant blocks)
- Modify: /Users/brynjolfurjonsson/sports/.claude/rules/publish-layer.md
- Test (append): /Users/brynjolfurjonsson/sports/tests/testthat/test-iceland-division-helpers.R

- [ ] Step 1 — write the failing tests FIRST. (a) Generalise the slug-collision block at tests/testthat/test-publish-divisions-config.R:96-104 to loop `for (key in c("football_iceland","basketball_iceland","handball_iceland"))` x both sexes, asserting length(slugs) == length(unique(slugs)) AND length(badges_configured) == length(unique(badges_configured)) within each (key, sex). (b) Generalise the required/optional-fields block at :11-28 the same way. (c) Leave the training_filter block at :30-48 football-scoped and say so in a comment — only football_iceland has a `training_filter` key (verified: config/leagues.yml:300 is its sole occurrence), so looping it over all three would fail on a NULL. (d) Append to test-iceland-division-helpers.R a block asserting the publish surface matches what is actually ingested: read data/facts/results for the max season, and for basketball and handball expect_setequal(.iceland_division_codes(key, sex), setdiff(unique(division_values_for_that_cell), "PO")) — i.e. every ingested division except handball's separate PO playoff division is published, and nothing is published that is not ingested. Comment that no CUP division exists for either sport (distinct 2026 divisions are basketball {BD,1D}, handball {OD,G66,PO}), which is why every new entry is is_cup:false and why there is no cup cell to build.

- [ ] Step 2 — RUN and state the RED. `Rscript -e 'devtools::load_all(); testthat::test_local(filter = "publish-divisions|iceland-division")'`. EXPECTED RED for block (d) only if the setdiff is written wrong; the generalised blocks (a) and (b) should go GREEN immediately against Task 5's config — that is fine and is itself the signal that the config is consistent. If (a) or (b) goes RED, the config carries a duplicate slug or badge within a (key, sex) and Task 5 must be corrected before proceeding. Record which blocks were red and why; do not paper over a red here.

- [ ] Step 3 — implement whatever the RED demands (a config correction, or the setdiff/comment in block (d)), then re-run to GREEN.

- [ ] Step 4 — update .claude/rules/publish-layer.md in the same commit, per the repo's docs-with-code convention. Replace any statement that the division helpers are football-specific with: the accessors are `.iceland_division_*(key, sex)` in R/publish-divisions.R, sport-neutral, config-driven from config/leagues.yml::<key>.publish_divisions; adding a publish cell is a config edit plus a metill-platform DIVISIONS entry, never an R edit. Record the four new optional schema keys and their contracts in one short table: code_badge (why it exists — basketball 1D fails next_games' ^[A-Z][A-Z0-9_]*$), expected_meetings (an ASSERTION and a fallback, never the source — n_rounds is derived from schedule+results, spec §12), qualify {slots,label_is} (absent = meta.qualify null and no p_qualify — the generic replacement for p_top_six, which does not transfer: Bónusdeild karla is 12 teams with 8 qualifying), relegation_slots (replaces the hardcoded placement >= n_teams - 1L bottom-two rule). State plainly that WS7 publishes nothing — the config is inert until WS8/WS9/WS10 read it.

- [ ] Step 5 — final gates. `Rscript -e 'devtools::load_all(); devtools::test()'` -> FAIL 0, SKIP 45. `git -C /Users/brynjolfurjonsson/sports status --short tests/testthat/fixtures/golden/` -> empty. `grep -rn '\.football_iceland_division_' R/ scripts/ tests/testthat/ man/ | grep -v worktrees | wc -l` -> 0. `Rscript -e 'devtools::load_all(); for (k in c("football_iceland","basketball_iceland","handball_iceland")) for (s in c("male","female")) cat(k, s, paste(.iceland_division_slugs(k, s), collapse = " "), "\n")` — read the eight bb/hb cells off stdout and confirm they are karla-bd, karla-1d, kvenna-bd, kvenna-1d, karla-od, karla-g66, kvenna-od, kvenna-g66.

- [ ] Step 6 — commit tests + rules doc. Message: `test(config): three-league publish_divisions invariants; docs(rules): sport-neutral division helpers`. Co-Authored-By trailer. No push. Then re-read `git -C /Users/brynjolfurjonsson/sports log --oneline -6` and confirm all six WS7 commits are present and the branch is still feat/bb-hb-metill-parity.

**Verification.** Slug and badge uniqueness, the required/optional field set, and the publish-vs-ingest division agreement all hold for all three leagues, asserted in one parameterised loop rather than three copies. The rules doc no longer describes the helpers as football-only and states the four new keys' contracts. Printing the eight bb/hb slugs shows the exact directory suffixes WS9 will create, and the football golden manifest is still the committed one.

**Risks.**

- SEAM (highest): I chose `qualify: { slots, label_is }` as a NESTED object, not the spec's flat `qualify_slots` integer. Reason: spec §11 requires meta.qualify = { slots, label_is } and a bare integer leaves label_is homeless — it would have to be hardcoded in R, which is exactly the copy-in-code §9 removes, and the labels genuinely differ per cell ("Efri hluti" for football BD vs "Úrslitakeppni" for the 2DT cells). It also mirrors the existing nested `split` precedent. CONSEQUENCE: `.iceland_division_qualify(key, sex)` returns a named LIST of NULL-or-list(slots,label_is), NOT an integer vector. Any sibling workstream (WS10) drafted against `qualify_slots` must be reconciled against this signature before implementation starts.
- SEAM: I add `.iceland_division_is_cup(key, sex)` and `.iceland_division_expected_meetings(key, sex)` beyond the spec's named set. is_cup exists because R/publish-football-iceland.R:797 derives is_cup from the literal `identical(target_div, "CUP")`, which is a football literal WS9's §10(f) does not list among the three it replaces; expected_meetings exists because WS10 and WS12 both need to read it. Both are behaviour-preserving for football (CUP is the only is_cup:true entry) and optional for a consumer to adopt.
- SEAM: I add `qualify: { slots: 6, label_is: "Efri hluti" }` to football BD (both sexes) but to no other football cell. Consequence for WS10: p_qualify will exist for football BD and be numerically identical to the existing p_top_six (both are placement <= 6), but football LD1/LD2/LD3/CUP will have no qualify config and therefore no p_qualify — while p_top_six is currently emitted for every football cell. WS10 must decide whether p_top_six's alias relationship is BD-only. WS7 deliberately does not decide it, because emission is WS10's and any emission change moves the golden hashes.
- SPEC CONTRADICTION, load-bearing for WS10: spec §12 and WS10 verification (a) call Bónusdeild karla "a four-meeting league" and assert meta.n_rounds == 44. That is wrong and contradicts the spec's own verified boundary table. Measured this session from data/facts/results season 2026: basketball male BD is 12 teams, 162 rows; cutting at round <= 22 gives 132 matches with all 66 pairs meeting exactly 2x, then rounds 23-35 decay 4,4,4,3,2,2,2,2,2,2,1,1,1 — a bracket. So expected_meetings is 2 and the regular season is 22 rounds; played:35 is regular season plus úrslitakeppni. WS7 writes 2. WS10's 44 must be corrected to 22 or its n_rounds derivation will disagree with config and fire the WARN it is supposed to prevent.
- The badge map loses two keys. `.football_iceland_division_code_labels()` defines LD1_PO="LDP" and LD4="D4"; the config-derived map defines neither (LD1 carries no `split`, and LD4 is in no publish_divisions list). Traced as unreachable: publish_football_iceland() filters `division %in% family_divs` (R/publish-football-iceland.R:1012) before the recode at :1043, and family_divs comes from .split_family_divisions_pfi (R/extract-football-iceland.R:126-135), which returns the target division plus only its own _UPPER_PO/_LOWER_PO. If that trace is wrong, the recode's `.default = .data$division` would emit a raw "LD1_PO", which violates next_games' ^[A-Z][A-Z0-9_]*$ and aborts football's publish on the next cron run. The football golden test in Task 4 Step 5 is the empirical check on the trace and must be run, not reasoned about.
- `qualify.slots` and `relegation_slots` are the only values in this workstream not measured from data. A wrong-but-plausible slots count silently mislabels the headline probability on the page and no test can catch it — Task 5 Step 1's assertion (1 <= slots < n_teams) catches transcription errors only. The mitigation is the human regulation read plus the fail-safe of omitting the key rather than guessing; do not substitute a confident-sounding number for a resolved citation.
- Task 5 Block C asserts expected_meetings against live git-tracked data/facts/results, so it will legitimately go red when a federation changes a competition format between seasons. That is the intent (spec §2), but the next session must be told the fix is to re-measure and rewrite the constant, not to relax the assertion — record this in the rules-doc update in Task 6.
- The three stale worktrees under .claude/worktrees/ (angry-jackson-c4c357, crazy-rhodes-a4ad59, sharp-jepsen-e3a666) each hold their own copies of extract-football-iceland.R and publish-football-iceland.R with the old helper names. The Task 4 grep MUST exclude them or it can never go green; equally, do not "fix" them — they are separate checkouts.
- Five cron workflows commit to main all day and the launchd autoplace agent runs git stash/pull/pop on this worktree on its own schedule. config/leagues.yml is edited in Tasks 2 and 5 and is NOT a cron-written path, but commit each task promptly rather than leaving the edited YAML in the working tree across a sync. Use `git -C /Users/brynjolfurjonsson/sports` for every git call — the Bash tool's cwd persists.

---

# WS8 — 2DT extractor: division loop, regular-season boundary, round-strength trajectory, fit_meta

**Goal.** Turn `.extract_2dt_iceland_pfi()` from a single-hardcoded-division extractor into the producer of the complete bb/hb extracts partition that WS9's reader and WS10's meta will consume: a loop over the configured publish divisions with the cross-division fit pulls hoisted exactly as football's `.extract_division_parquets_pfi()` does, a `division` payload column on every parquet, an honest D3 regular-season boundary (basketball's úrslitakeppni rounds live inside the same division and must not reach the league table), the previously-thought-impossible `round_strengths_quantiles.parquet` (N4: `Stan/basketball_iceland/2d_student_t_scalarsigma.stan:157,164` declares `array[N_rounds] vector[K] offense`/`defense`), and `fit_meta.parquet` written on BOTH the football and 2DT trees at the one moment the two partition shapes are free to converge. `data/beliefs/extracts/` currently contains only `sport=football`, so every shape decision below lands on first write, not as a migration.

**Consumes.**

- WS7 (BLOCKING): `.iceland_division_codes(key, sex)` -> character vector of publish division codes, read from `config/leagues.yml::<key>.publish_divisions[[sex]][].code`. WS8 cannot start until this exists and `load_leagues()` accepts the `publish_divisions` blocks for basketball_iceland and handball_iceland. Today `config/leagues.yml` has `publish_divisions` only under football_iceland (line 310) and the helper is still named `.football_iceland_division_codes(sex)` (R/extract-football-iceland.R:40).
- WS7: the `publish_divisions` entries themselves, each carrying schema-required `code`, `slug`, `label_is`, `is_cup` plus the WS8-relevant optional `expected_meetings` (2 for bb BD/1D both sexes and hb male OD/G66; 3 for hb female OD/G66; DELIBERATELY ABSENT for basketball female 1D, which is genuinely irregular — 11 teams, byes, one pair met once and one four times).
- WS7: `config/leagues.schema.json` `definitions.publishDivisionList.items.properties` extended with `expected_meetings` (not required). Until that edit lands `load_leagues()` rejects the key — `betting`/`publishDivisionList` are both `additionalProperties: false` (N2).
- WS2: `tests/testthat/helper-stub-fit.R` :: `stub_fit(draws_list)` (named list Stan var -> draws matrix, `lp__` required, NOT class CmdStanMCMC), `stub_2dt_draws(teams, n_pred, n_draws, seed, constants)`, `local_stub_2dt(league, sex, end_date, root, n_draws, constants)`.
- WS2: `tests/testthat/helper-fixture-facts.R` :: `fixture_facts_root(env)`, `fixture_division_teams(sport, sex, division)`, `FIXTURE_END_DATE = 2100-01-15`, `FIXTURE_FIT_DATE = 2100-01-01`, `FIXTURE_N_DRAWS = 50L`, `FIXTURE_DIVISIONS` (bb male/female = c(BD = 4L, `1D` = 6L); hb male/female = c(OD = 4L, G66 = 6L)) — a SINGLE round robin per (sport, sex, division, season) across seasons 2099 + 2100.
- WS2: `tests/testthat/fixtures/golden/football-publish-hashes.csv` + `tests/testthat/test-publish-football-golden.R` — the football regression net. Tasks 1 and 8 touch football code paths and must leave these hashes byte-identical.
- WS3 (already merged): `.compute_home_advantage_quantiles_2dt()` no longer has an `exp()`, a `/2` or a `transform` argument; `tests/testthat/test-extract-2dt-home-advantage-units.R` pins 1.5 / 2.5 / 4.0 / 8.0 / 12.07.

**Produces.**

- `.compute_team_strength_trajectory(fit, results, teams, current_top_teams, current_season, top_div)` in NEW `R/extract-strength-trajectory.R` — the sport-agnostic per-round trajectory, moved verbatim out of `R/publish-football-iceland.R:87-233` and renamed (the `_pfi` suffix was a lie once the 2DT extractor calls it). Returns a long tibble (round, .draw, team, component, location, value) where `round` is the division matchweek, NOT the model's global per-team round index. NO variable-name parameterisation is needed: football (`Stan/football_iceland/bivariate_poisson_no_inflation.stan:159,162,188,195`) and 2DT (`Stan/basketball_iceland/2d_student_t_scalarsigma.stan:112,116,157,164`) declare the identical names `offense`, `defense`, `home_advantage_off`, `home_advantage_def` with identical `array[N_rounds] vector[K]` shape, and neither publisher transforms the values (R/publish-football-iceland.R:1334-1345 feeds `round_strengths_quantiles` straight into `.intervals_from_quantiles_pfi`).
- `.division_regular_rounds(results, sport, sex, season, division, expected_meetings = NULL)` in NEW `R/division-rounds.R` -> `list(cut = integer(1), source = c("config","none"), n_teams = integer(1))`. `cut` is the last regular-season round; rows with `round > cut` are post-season. WS10 MUST derive `meta.n_rounds` from rows already cut by this helper — deriving it from raw `played + remaining` publishes 35 for Bónusdeild karla (the -13 af 22 bug in another costume).
- `.iceland_division_expected_meetings(key, sex)` -> named integer vector (division code -> meetings, `NA_integer_` where the config omits it). Lives beside WS7's other `.iceland_division_*` helpers in the same file. WS7 MUST NOT also define this — one definition, WS8 owns it.
- `.extract_team_strength_draws_2dt(fit, teams)` -> long tibble (team, component, location, .draw, value) over the six `cur_*` variables; `.extract_home_advantage_draws_2dt(fit, teams)` -> long tibble (team, component, .draw, value) over the three `home_advantage_*` variables. Both hoisted out of the per-division loop, mirroring football's `team_strengths_draws` / `home_advantage_draws` (R/extract-football-iceland.R:1628-1652).
- CHANGED SIGNATURES: `.compute_team_strengths_quantiles_2dt(team_strengths_draws, current_top_teams)` and `.compute_home_advantage_quantiles_2dt(home_advantage_draws, current_top_teams)` — both now take draws, not `fit` + `teams`. `tests/testthat/test-extract-2dt-home-advantage-units.R` call sites updated to the composition.
- `.compute_predicted_matches_2dt(fit, pred_d, ..., posterior_goals = NULL)` — new optional argument so the caller can hoist the single `.compute_posterior_goals_2dt()` call (it is currently made twice per extract: once inside this function, once at R/extract-iceland-2dt-shared.R:397).
- THE 2DT EXTRACTS PARTITION CONTRACT (first write, no back-compat constraint). `data/beliefs/extracts/sport={basketball,handball}/country=iceland/sex=<sex>/fit_date=<YYYY-MM-DD>/` holds exactly 7 parquets: `predicted_matches`, `team_strengths_quantiles`, `round_strengths_quantiles`, `home_advantage_quantiles`, `final_positions`, `points_distribution` — each carrying a `division` payload column whose value set equals `.iceland_division_codes(key, sex)` — plus `fit_meta`, which is PARTITION-LEVEL and carries NO `division` column.
- `fit_meta.parquet` columns (identical on both trees): `n_draws` (integer), `fit_date` (Date), `stan_model` (character, from `league$stan_model`), `model_units` (character: `"points"` basketball, `"goals"` handball, `"log_rate"` football). One row. WS9's reader must treat it as partition-level and must not split it by division — the only file in the partition that is not division-keyed.
- `round_strengths_quantiles` column contract (both trees, unchanged from football's): `round` (integer, division matchweek 1..N), `team`, `component` in {offence, defence, total}, `location` in {home, away, avg}, `quantile` 1..99, `value`, `division`.

### Task 1: Move the per-round trajectory helper to a neutral file, with football provably unchanged

**Files:**

- CREATE R/extract-strength-trajectory.R
- MODIFY R/publish-football-iceland.R (delete lines 76-233: the comment block + `.compute_team_strength_trajectory_pfi`)
- MODIFY R/extract-football-iceland.R:249 (the single call site)
- CREATE tests/testthat/test-strength-trajectory.R
- MODIFY NAMESPACE / man/ only if roxygen regenerates them (the function is internal, `@noRd`)

- [ ] Read R/publish-football-iceland.R:76-233 in full. It is one function plus its comment header; `.compute_team_strength_trajectory_pfi` is called from exactly one place — `grep -rn 'compute_team_strength_trajectory' R/ tests/` returns R/publish-football-iceland.R:87 (definition) and R/extract-football-iceland.R:249 (call). Confirm that count before moving anything.

- [ ] Write tests/testthat/test-strength-trajectory.R. Test 1: `expect_true(is.function(.compute_team_strength_trajectory))`. Test 2 (behavioural, sport-agnostic): build a 3-team, 4-round hand-made case — `teams <- tibble::tibble(team = c('A','B','C'), team_nr = 1:3)`; `results` = 6 played rows with `match_date` ordered, `division = 'BD'`, `season = 2100L`, non-NA scores; `fit <- stub_fit(list(offense = <n_draws x (4*3) matrix, colnames 'offense[r,k]'>, defense = <same>, home_advantage_off = <n_draws x 3>, home_advantage_def = <n_draws x 3>, lp__ = ...))` with every `offense[r,k]` pinned to a distinct known constant. Assert the returned tibble has columns (round, .draw, team, component, location, value), that `sort(unique(out$round))` equals each team's matchweek sequence, that `nrow(out) == n_team_rounds * 9 * n_draws`, and that for one (round, team) the `offence`/`home` value equals `offense[global_round, k] + home_advantage_off[k]` exactly (`expect_equal(..., tolerance = 0)`).

- [ ] RUN it: `Rscript -e 'devtools::load_all(quiet = TRUE); devtools::test(filter = "strength-trajectory")'` from /Users/brynjolfurjonsson/sports. EXPECTED RED: both tests error with `could not find function ".compute_team_strength_trajectory"` (the old name still carries the `_pfi` suffix). Do not proceed until you have seen that exact string.

- [ ] Create R/extract-strength-trajectory.R. Paste R/publish-football-iceland.R:76-233 VERBATIM, changing exactly three things: the function name loses `_pfi`; the roxygen/comment header gains a line saying the helper is shared by football and both 2DT sports because all three Stan models declare the same `offense`/`defense`/`home_advantage_off`/`home_advantage_def` names and shapes (cite Stan/football_iceland/bivariate_poisson_no_inflation.stan:188,195 and Stan/basketball_iceland/2d_student_t_scalarsigma.stan:157,164); the `warning(sprintf('publish_football_iceland: fit covers %d rounds x %d teams; ...'))` message becomes `'.compute_team_strength_trajectory: fit covers %d rounds x %d teams; ...'`. `grep -rn 'fit covers %d' tests/` first to confirm no test asserts that string — it returns nothing today.

- [ ] Delete the moved block from R/publish-football-iceland.R and update the call at R/extract-football-iceland.R:249 to the new name. Add `#' @include` only if the file needs collation ordering — it does not; these are plain function calls resolved at runtime.

- [ ] Re-run the new test file: GREEN.

- [ ] Run the football regression net: `Rscript -e 'devtools::load_all(quiet = TRUE); devtools::test(filter = "publish-football-golden")'`. It must be GREEN with zero hash changes — this is the proof the move is behaviour-preserving. Do NOT regenerate tests/testthat/fixtures/golden/football-publish-hashes.csv.

- [ ] Run the full suite (`Rscript -e 'devtools::load_all(quiet = TRUE); devtools::test()'`) and confirm FAIL 0 against Plan A's baseline. Commit: `refactor(extract): move the per-round strength trajectory to a sport-neutral file`.

**Verification.** `.compute_team_strength_trajectory()` returns the exact `offense[r,k] + home_advantage_off[k]` value for a pinned stub across an all-three-sports-shared code path, and all 10 football JSONs x 9 cells hash identically to the committed golden manifest — so the helper is provably usable by the 2DT extractor without moving football a byte.

### Task 2: stub_2dt_draws() gains the offense/defense round arrays, internally consistent with cur_*

**Files:**

- MODIFY tests/testthat/helper-stub-fit.R (`stub_2dt_draws`, `local_stub_2dt`)
- MODIFY tests/testthat/test-stub-fit.R (the variable-surface assertion at :20)

- [ ] Read tests/testthat/helper-stub-fit.R:81-141 and tests/testthat/test-stub-fit.R. Note that `stub_2dt_draws()` today emits ONLY team-indexed `cur_*` / `home_advantage_*` / `goals*_pred` / `lp__` blocks — there is no `offense`/`defense`, so any trajectory call against it dies inside `stub_fit`.

- [ ] Add failing assertions to tests/testthat/test-stub-fit.R: (a) `d <- stub_2dt_draws(c('A','B','C'), n_pred = 4L, n_draws = 20L, n_rounds = 5L)`; `expect_true(all(c('offense','defense') %in% names(d)))`; (b) `expect_equal(ncol(d$offense), 5L * 3L)` and `expect_equal(colnames(d$offense)[1:2], c('offense[1,1]','offense[2,1]'))` — match whatever index order you actually generate, but PIN it; (c) the consistency property the whole workstream leans on: `expect_equal(unname(d$cur_offense_away[, k]), unname(d$offense[, paste0('offense[', 5L, ',', k, ']')]), tolerance = 0)` for every k, and likewise `cur_defense_away` vs `defense[N_rounds, k]`, `cur_offense_home == cur_offense_away + home_advantage_off`, `cur_strength_home == cur_offense_home + cur_defense_home` — mirroring `Stan/basketball_iceland/2d_student_t_scalarsigma.stan:279-289`.

- [ ] RUN: `Rscript -e 'devtools::load_all(quiet = TRUE); devtools::test(filter = "stub-fit")'`. EXPECTED RED: assertion (a) fails with `all(c("offense", "defense") %in% names(d)) is not TRUE`, and (c) fails with `subscript out of bounds` / `Column 'offense[5,1]' not found` because the stub never generated them.

- [ ] Implement in helper-stub-fit.R: add `n_rounds = 10L` to `stub_2dt_draws()`'s signature. Generate `offense` and `defense` as `n_draws x (n_rounds * k)` matrices with colnames `sprintf('%s[%d,%d]', prefix, r, kk)` in the same flattening order cmdstanr uses for `array[N_rounds] vector[K]` (round-major within team is what `posterior::as_draws_rvars()` reassembles into dims (draws, N_rounds, K) — assert the reassembled dims in the test rather than trusting the ordering by eye). Make the walk deterministic and monotone-ish so a trajectory plot is meaningful: `offense[r,k] = off[k] + 0.05 * (r - n_rounds)` plus the existing per-draw noise. Then DERIVE the cur_* blocks from the last round instead of generating them independently: `cur_offense_away <- offense[, r == n_rounds]`, `cur_defense_away <- defense[, r == n_rounds]`, `cur_offense_home <- cur_offense_away + home_advantage_off`, `cur_defense_home <- cur_defense_away + home_advantage_def`, `cur_strength_away <- cur_offense_away + cur_defense_away`, `cur_strength_home <- cur_offense_home + cur_defense_home`. Keep the `constants` pinning seam working for every block, including the two new ones.

- [ ] Update `local_stub_2dt()` to pass `n_rounds = prep$stan_data$N_rounds` — the stub must be sized from the same `prepare_data()` call the code under test uses, exactly as `n_pred` already is. Add `stopifnot(prep$stan_data$N_rounds >= 1L)`.

- [ ] Re-run test-stub-fit: GREEN. Then `grep -rn 'stub_2dt_draws' tests/ tools/` and re-run every hit (`tests/testthat/test-stub-fit.R`, `tools/make-extract-fixtures.R:154`) — the cur_* draws now have different VALUES, so any test asserting an exact 2DT number (not just structure) must be found now, not at fixture-regeneration time. `devtools::test(filter = 'extract-basketball|extract-handball|publish-basketball|publish-handball')` must stay GREEN (those files assert structure, not values — re-read them if one fails).

- [ ] Commit: `test(fixtures): stub_2dt_draws emits offense/defense round arrays consistent with cur_*`.

**Verification.** `stub_2dt_draws()` now round-trips through `posterior::as_draws_rvars()` to dims (n_draws, N_rounds, K) for offense/defense, and its `cur_strength_home` equals `offense[N_rounds,k] + defense[N_rounds,k] + home_advantage_off[k] + home_advantage_def[k]` exactly — which is what makes Task 7's trajectory/cur_strength identity check a real assertion rather than a coincidence.

### Task 3: Hoist the fit$draws() pulls out of the two 2DT quantile helpers

**Files:**

- MODIFY R/extract-iceland-2dt-shared.R (`.compute_team_strengths_quantiles_2dt` :163-186, `.compute_home_advantage_quantiles_2dt` :209-233)
- MODIFY tests/testthat/test-extract-2dt-home-advantage-units.R (three call sites at :56, :74, :83)
- CREATE tests/testthat/test-extract-2dt-draw-pulls.R

- [ ] Read R/extract-iceland-2dt-shared.R:163-233 and R/extract-football-iceland.R:1628-1652 side by side. Football pulls all six `cur_*` into `team_strengths_draws` and all three `home_advantage_*` into `home_advantage_draws` ONCE, above the division loop, then each division only `semi_join`s and quantiles. The 2DT helpers pull inside — which under a division loop becomes 9 `fit$draws()` calls per division against a 300-600 MB fit.

- [ ] Write tests/testthat/test-extract-2dt-draw-pulls.R: assert `.extract_team_strength_draws_2dt(fit, teams)` returns one row per (.draw, team, component, location) over components {offence, defence, total} x locations {home, away} (6 blocks, no `avg` — `avg` is a per-draw mean computed downstream), and `.extract_home_advantage_draws_2dt(fit, teams)` returns (team, component, .draw, value) over {offence, defence, total}. Use `stub_fit(stub_2dt_draws(...))` and a 3-team `teams` tibble; pin one value exactly via `constants`.

- [ ] RUN: `Rscript -e 'devtools::load_all(quiet = TRUE); devtools::test(filter = "extract-2dt-draw-pulls")'`. EXPECTED RED: `could not find function ".extract_team_strength_draws_2dt"`.

- [ ] Implement both pull functions in R/extract-iceland-2dt-shared.R, immediately above the quantile helpers. `.extract_team_strength_draws_2dt` = `dplyr::bind_rows()` of the six existing `.extract_team_draws_2dt(fit, var, teams, component, location)` calls currently inlined at :165-174. `.extract_home_advantage_draws_2dt` = the `extract_one()` closure currently inlined at :211-223, lifted to the top level and applied to the three variables. Move NOTHING else — in particular do not reintroduce an `exp()` or a `transform` argument (WS3/B5).

- [ ] Change the two quantile helpers to take draws: `.compute_team_strengths_quantiles_2dt(team_strengths_draws, current_top_teams)` (keeps the per-draw `avg` computation at :175-180 and the `semi_join` + `.summarise_quantile_band_2dt`), `.compute_home_advantage_quantiles_2dt(home_advantage_draws, current_top_teams)` (keeps the `semi_join` + band). Preserve the B5 comment block at :188-208 verbatim and append one line naming the new seam (`the pull now lives in .extract_home_advantage_draws_2dt(); the units guarantee is the composition of the two`).

- [ ] Update the three call sites in tests/testthat/test-extract-2dt-home-advantage-units.R to the composition, e.g. `.compute_home_advantage_quantiles_2dt(.extract_home_advantage_draws_2dt(ha_stub(off = 1.5, def = 2.5), ha_teams()), ha_teams()['team'])`. The asserted values (1.5, 2.5, 4.0, 8.0, 12.07) MUST NOT change — if any moves, the hoist changed behaviour and is wrong.

- [ ] Update the callers inside `.extract_2dt_iceland_pfi()` (:390-395) so `devtools::load_all()` still runs; the full loop lands in Task 5.

- [ ] RUN `devtools::test(filter = 'extract-2dt|extract-basketball|extract-handball')`: GREEN. Commit: `refactor(extract): hoist the 2DT posterior pulls out of the quantile helpers`.

**Verification.** The B5 units assertions still read exactly 1.5 / 2.5 / 4.0 / 8.0 / 12.07 through the new two-function composition, and `fit$draws()` is called at most 9 times per extract regardless of how many divisions the loop covers.

### Task 4: The regular-season boundary: `.division_regular_rounds()`, and why it is a round cut and not a KKÍ stage filter

**Files:**

- CREATE R/division-rounds.R
- CREATE tests/testthat/test-division-rounds.R
- MODIFY R/extract-football-iceland.R (add `.iceland_division_expected_meetings(key, sex)` beside WS7's other `.iceland_division_*` helpers)
- MODIFY docs/superpowers/plans/ — no; record the decision as a comment header in R/division-rounds.R

- [ ] Record the DECISION in the file header of R/division-rounds.R (write it with `cat <<'EOF'` or a python heredoc, not Write/Edit — it contains 'úrslitakeppni' and 'Deildarkeppni'). The decision, and the four reasons, verbatim in substance: **the D3 regular-season boundary is a ROUND CUT, not the KKÍ stage dimension.** (1) `stage` is not in the data and cannot be put there here: Plan A's integration decision ID-3 deferred it because adding `stage` to `schemas()$results` is a schema migration and `validate_against_schema()` (R/storage.R:67-81) hard-fails on a missing schema column, breaking ~14 writers plus the exact-set assertion at tests/testthat/test-ingest-kki.R:20-27. (2) It would only ever cover basketball. Handball's post-season is a separate division (`PO`; verified in data/facts/results season 2026: hb male PO 20 rows, hb female PO 16 rows) already excluded by the division filter, so a round rule must exist regardless — stage would be a second, partly-overlapping mechanism. (3) The round cut is locally checkable and stage is not: the pair-meeting test is an assertion this repo can run, a vendor stage label is not. Verified against real data on 2026-09-04 — bb male BD cut 22 -> 132 rows / 66 pairs x 2; bb male 1D cut 22 -> 132 / 66 x 2; bb female BD cut 18 -> 90 / 45 x 2; bb female 1D cut 20 -> 93 rows with meetings 1, 2 and 4 (irregular, which is why config omits `expected_meetings` there). (4) `round` already means the right thing: `derive_league_round()` (R/derive-round.R:37-74) sets it to each team's cumulative appearance index within (sport, country, sex, season, division), taking the max of the two sides. FOLLOW-UP recorded in the same header: when the KKÍ stage ids from spec finding N7 (league 190: 300475 Deildarkeppni / 306658 Úrslitakeppni; 191: 300472/306497; 189: 300530/306645 + A riðill 305952, B riðill 305951; 231: 300529/306557) are eventually ingested, `stage == 'Deildarkeppni'` becomes the primary source and this round cut becomes its fallback and cross-check.

- [ ] Write tests/testthat/test-division-rounds.R against hand-built tibbles (no fixture, no network). Case A (configured, clean): 12 teams, double round robin, rows carrying `round` 1..22 plus 30 post-season rows at `round` 23..35 -> `expect_equal(out$cut, 22L)`, `expect_equal(out$source, 'config')`, `expect_equal(out$n_teams, 12L)`. Case B (configured, triple RR — women's handball): 8 teams, `expected_meetings = 3L`, rounds 1..21 -> `cut == 21L`. Case C (unconfigured — bb female 1D): `expected_meetings = NULL` -> `cut == max(results$round)`, `source == 'none'`, and `expect_warning(..., 'no expected_meetings')`. Case D (the assertion fires): 12 teams, `expected_meetings = 2L`, but the rows contain a THIRD meeting for one pair inside rounds 1..22 -> `expect_error(..., 'more than 2 meetings')`. Case E (in-progress season must NOT abort): 12 teams, `expected_meetings = 2L`, only rounds 1..9 played so every pair has met at most once -> `cut == 22L`, no error, no warning.

- [ ] RUN: `Rscript -e 'devtools::load_all(quiet = TRUE); devtools::test(filter = "division-rounds")'`. EXPECTED RED: every test errors with `could not find function ".division_regular_rounds"`.

- [ ] Implement `.division_regular_rounds(results, sport, sex, season, division, expected_meetings = NULL)`. Filter to the (sport, country=iceland, sex, season, division) rows with non-NA `round`. `n_teams <- length(unique(c(home_team, away_team)))`. If `expected_meetings` is a non-NA integer: `cut <- as.integer(expected_meetings * (n_teams - 1L))`, `source <- 'config'`. Else `cut <- max(round, na.rm = TRUE)`, `source <- 'none'`, and `cli::cli_warn()` naming the cell and saying the projection therefore includes every played row. The assertion is ONE-SIDED by design — inside rounds 1..cut, no pair may meet MORE than `expected_meetings` times; fewer is a legitimately in-progress season (Case E). Abort text must contain the literal substring `more than {expected_meetings} meetings` plus the cell identity and the offending pair, via `cli::cli_abort()`. Document the one-sidedness in a comment: a too-low cut under-counts silently, a too-high cut (post-season rows leaking into the league table) is the dangerous direction and is what this catches.

- [ ] Add `.iceland_division_expected_meetings(key, sex)` next to WS7's `.iceland_division_codes()` in R/extract-football-iceland.R: returns a named integer vector over the division codes, `NA_integer_` where `publish_divisions[[sex]][[i]]$expected_meetings` is absent. Add a test asserting football male returns all-NA (football configures none) and basketball female returns `c(BD = 2L, `1D` = NA_integer_)`. Coordinate with WS7 before committing: this helper must have exactly one definition.

- [ ] Re-run: GREEN. `devtools::test()` full suite: FAIL 0. Commit: `feat(extract): derive the regular-season round boundary per division`.

**Verification.** Against real 2026 data the helper reproduces the spec's boundary table exactly — 22 for both basketball male divisions, 18 for basketball female BD, 21 for both women's handball divisions — aborts when post-season rows would inflate a pair beyond its configured meeting count, and does NOT abort on a half-played season.

### Task 5: Loop `.extract_2dt_iceland_pfi()` over the configured divisions and stamp `division` on every parquet

**Files:**

- MODIFY R/extract-iceland-2dt-shared.R:312-439 (`.extract_2dt_iceland_pfi`)
- MODIFY R/extract-basketball-iceland.R (drop `top_div = "BD"`, pass the league key)
- MODIFY R/extract-handball-iceland.R (drop `top_div = "OD"`, pass the league key)
- MODIFY R/publish-iceland-2dt-helpers.R (`.compute_predicted_matches_2dt` gains `posterior_goals = NULL`)
- CREATE tests/testthat/test-extract-2dt-divisions.R
- MODIFY tests/testthat/test-extract-basketball-iceland.R + test-extract-handball-iceland.R (roxygen-driven expectations only if they assert a division set)

- [ ] Write tests/testthat/test-extract-2dt-divisions.R using `fixture_facts_root()` + `local_stub_2dt()` (the pattern already in tests/testthat/test-extract-basketball-iceland.R:5-17). Run `extract_basketball_iceland()` for sex 'male' and assert, for EACH of the five current parquets: the file has a `division` column and `expect_setequal(unique(df$division), .iceland_division_codes('basketball_iceland', 'male'))` (= c('BD','1D')). Then the spec's named hazard: read final_positions.parquet, split by division, and assert `expect_length(intersect(fp_BD$team, fp_1D$team), 0L)` AND that each division's team set equals `fixture_division_teams('basketball', 'male', div)` — `.compute_final_positions_2dt()` filters internally, so a bad loop simulates the 1D table on BD's base points. Add the same for points_distribution, and assert `sum(probability)` per (division, team) is 1 within 1e-8. Repeat one cell for handball (OD/G66).

- [ ] RUN: `Rscript -e 'devtools::load_all(quiet = TRUE); devtools::test(filter = "extract-2dt-divisions")'`. EXPECTED RED: the first assertion fails with `unique(fp$division)` being NULL — `final_positions.parquet` has no `division` column at all today (R/extract-iceland-2dt-shared.R:404-408 writes the bare three-column tibble), and only `BD` is ever computed.

- [ ] Restructure `.extract_2dt_iceland_pfi()`. Replace the `top_div = "BD"` scalar parameter with `key` (the leagues.yml key) and resolve `divisions <- .iceland_division_codes(key, sex)` inside; keep every other parameter. HOISTED above the loop, in this order: `prep` (unchanged), `teams`, `pred_d`, the `results` read (:349-361) — and add `stopifnot(is.null(league$training_filter))` with a comment explaining WHY (basketball_iceland and handball_iceland configure no `training_filter`, verified in config/leagues.yml, so this extractor's `results` set is identical to the one `prepare_data()` modelled; the moment a training_filter is added, the per-team appearance index this extractor derives desynchronises from the model's `round1`/`round2` and Task 7's trajectory silently mis-indexes) — `current_season`, `posterior_goals <- .compute_posterior_goals_2dt(fit, pred_d)` (ONE call, replacing today's two: :397 and the one inside `.compute_predicted_matches_2dt`), `team_strengths_draws`, `home_advantage_draws`, and `predicted_matches` (cross-division; it already carries `division` from pred_d, so FILTER it to `division %in% divisions` and do NOT mutate a second division column onto it).

- [ ] Add `posterior_goals = NULL` to `.compute_predicted_matches_2dt()`; when supplied it skips its internal `.compute_posterior_goals_2dt()` call. Default NULL keeps the existing publisher call sites working until WS9 deletes them.

- [ ] Inside the loop, per division: `top_results <- results[results$season == current_season & results$division == div, ]` (this is where Task 6's cut lands), `current_top_teams`, `base_points <- .compute_base_points_2dt(top_results, ...)`, `.compute_team_strengths_quantiles_2dt(team_strengths_draws, current_top_teams)`, `.compute_home_advantage_quantiles_2dt(home_advantage_draws, current_top_teams)`, `.compute_final_positions_2dt(posterior_goals, div, base_points, ...)`, `.compute_points_distribution_2dt(posterior_goals, div, base_points, ...)`. Then `lapply(parts, function(df) dplyr::mutate(df, division = div))` for the four division-scoped tibbles ONLY — exactly football's shape at R/extract-football-iceland.R:1690. Bind across divisions with `dplyr::bind_rows()` and write one parquet per file type, exactly as football does at :1700-1707.

- [ ] Update the two per-sport wrappers to pass `key = 'basketball_iceland'` / `'handball_iceland'` and delete their `top_div` arguments. Update their roxygen in the SAME commit: the 'The 4 football-specific extracts ... the per-round strength projection is football-specific' paragraph at R/extract-basketball-iceland.R:22-25 is now false and must go (spec finding N4), and the file list becomes per-division.

- [ ] Re-run test-extract-2dt-divisions: GREEN. Then re-run `devtools::test(filter = 'extract-basketball|extract-handball|publish-basketball|publish-handball|fixture-harness')` — the existing extract tests assert `%in%` on the five filenames and a BD-only team set at test-extract-basketball-iceland.R:80-83; that last one must now be scoped to the BD slice, so update it and say so in the commit message.

- [ ] Commit: `feat(extract): loop the 2DT extractor over the configured publish divisions`.

**Verification.** For every 2DT cell the extractor writes one parquet per file type covering BOTH configured divisions, each row carrying its own `division`, and the two divisions' `final_positions` team sets are disjoint and equal to their own division's team list — proving no second-tier table was simulated on top-tier base points.

### Task 6: Apply the regular-season cut so basketball's úrslitakeppni cannot reach the league table

**Files:**

- MODIFY R/extract-iceland-2dt-shared.R (inside the per-division block from Task 5)
- MODIFY tests/testthat/test-extract-2dt-divisions.R (add the post-season cases)

- [ ] Add two failing test blocks to tests/testthat/test-extract-2dt-divisions.R. Block A (played post-season rows): materialise `fixture_facts_root()`, then append synthetic post-season rows to the temp root with `write_table()` — for basketball male BD (4 fixture teams, single RR, max round 3, `expected_meetings = 2` so cut = 2*(4-1) = 6) append a 3-match mini-bracket at `round` 7, 8, 9 in season 2100 with lopsided scores that would REVERSE the table if counted (give the fixture's weakest team, `BB M BD 04`, three wins). Run the extractor and assert the winner of `final_positions` at placement 1 is still `BB M BD 01` and that `max(points)` in points_distribution is unchanged from a run without the injected rows. Block B (upcoming post-season fixtures): append schedule rows that push one team's appearance count past the cut, and assert those fixtures still appear in `predicted_matches.parquet` (a next game is a next game) but contribute NO points to `final_positions` / `points_distribution`.

- [ ] RUN: `Rscript -e 'devtools::load_all(quiet = TRUE); devtools::test(filter = "extract-2dt-divisions")'`. EXPECTED RED: Block A fails because the injected round-7..9 wins flow through `.compute_base_points_2dt()` into `base_points` and `BB M BD 04` takes placement 1 — the assertion reports the wrong team at placement 1. Block B fails because `.compute_iter_team_points_2dt()` (R/publish-iceland-2dt-helpers.R:239-273) filters posterior_goals on `division` only and adds every upcoming fixture's simulated points.

- [ ] Implement the played-rows cut: inside the per-division block, call `.division_regular_rounds(results, sport = league$sport, sex = sex, season = current_season, division = div, expected_meetings = .iceland_division_expected_meetings(key, sex)[[div]])` and filter `top_results` to `is.na(round) | round <= cut` BEFORE computing `current_top_teams` and `base_points`. Keep the `cut` and `source` in scope — WS10 needs both for `meta.n_rounds` / `n_rounds_source`, and Task 8's `fit_meta` does not carry them (they are per-division, fit_meta is per-partition), so surface them as a `division_rounds` tibble (division, cut, source, n_teams) written as a column set onto `final_positions`... NO — do not widen final_positions. Instead return them via the loop and stamp `n_rounds` + `n_rounds_source` as two extra columns on `points_distribution`? Also no. Decide it cleanly and state it in the commit: the cut is applied here and the NUMBER is re-derived by WS10 from the same helper. WS10 calls `.division_regular_rounds()` itself; WS8 exports nothing new for it beyond that function.

- [ ] Implement the upcoming-fixture cap: build a per-team appearance ledger from the cut `top_results` (`played_n` per team), then walk `pred_d` rows for this division in `match_date` order, keeping a fixture only while BOTH teams' running count is `<= cut`. Filter `posterior_goals` to the surviving `game_nr`s before passing it to `.compute_final_positions_2dt()` / `.compute_points_distribution_2dt()`. `predicted_matches` is built from the UNCUT set — say so in a comment. Guard the `source == 'none'` case: with no configured `expected_meetings` the cut is `max(round)`, so the cap must not silently drop every upcoming fixture — when `source == 'none'`, skip the cap entirely and let the warning from Task 4 carry the caveat.

- [ ] Re-run: both blocks GREEN. Re-run the whole 2DT + fixture-harness set. Full `devtools::test()`: FAIL 0.

- [ ] Commit: `fix(extract): exclude post-season rounds from the 2DT regular-season projection (D3)`.

**Verification.** Injecting a synthetic úrslitakeppni that would hand placement 1 to the weakest team leaves `final_positions` unchanged, and an upcoming fixture beyond the regular-season boundary is published in `predicted_matches` while contributing zero points to the simulated table — the two ways basketball's embedded post-season could corrupt a D3 regular-season projection.

### Task 7: Write round_strengths_quantiles.parquet for both 2DT sports

**Files:**

- MODIFY R/extract-iceland-2dt-shared.R (per-division block + the write list)
- MODIFY tests/testthat/test-extract-2dt-divisions.R or CREATE tests/testthat/test-extract-2dt-round-strengths.R
- MODIFY R/extract-basketball-iceland.R + R/extract-handball-iceland.R roxygen (six files, not five)

- [ ] Write tests/testthat/test-extract-2dt-round-strengths.R. Assertions: (a) `round_strengths_quantiles.parquet` exists in the partition and carries columns (round, team, component, location, quantile, value, division); (b) `expect_setequal(unique(df$component), c('offence','defence','total'))`, `expect_setequal(unique(df$location), c('home','away','avg'))`, `expect_setequal(unique(df$quantile), seq_len(99L))`; (c) `expect_false(anyNA(df$value))` and `expect_equal(sort(unique(df$round)), seq_len(max(df$round)))` per division — rounds run 1..N with no gaps; (d) one row group per (round, team, component, location, quantile) — `expect_equal(nrow(dplyr::count(df, round, team, component, location, quantile) |> dplyr::filter(n > 1L)), 0L)`; (e) THE IDENTITY: for a team whose appearance count equals the fit's `N_rounds`, the median `value` at its final matchweek for (component='total', location='avg') equals the median of `cur_strength` — computed from the stub as `(cur_strength_home + cur_strength_away)/2` — to 1e-6. Task 2 made the stub internally consistent precisely so this is exact rather than approximate; pick the team by `which.max(appearances)` from the fixture facts and assert first that its appearance count equals `prep$stan_data$N_rounds`, so the test fails loudly if the fixture ever changes shape instead of silently comparing the wrong quantities.

- [ ] RUN: `Rscript -e 'devtools::load_all(quiet = TRUE); devtools::test(filter = "extract-2dt-round-strengths")'`. EXPECTED RED: assertion (a) fails — `file.exists(.../round_strengths_quantiles.parquet) is not TRUE` — because R/extract-iceland-2dt-shared.R:415-436 writes exactly five parquets.

- [ ] Implement inside the per-division block, copying the SHAPE of R/extract-football-iceland.R:243-270 (not the code — call the Task 1 helper): `trajectory_long <- .compute_team_strength_trajectory(fit = fit, results = results, teams = teams, current_top_teams = current_top_teams, current_season = current_season, top_div = div)`. Pass the CUT results from Task 6, not the raw ones, so the published trajectory is the regular season (a basketball team's matchweek 23-35 are bracket games). Then build the per-draw `avg` exactly as football does (summarise mean over location by (.draw, round, team, component), `location = 'avg'`), bind, and `.summarise_quantile_band_2dt(c('round','team','component','location'))`. Emit the football-shaped empty tibble when `nrow(trajectory_long) == 0L`.

- [ ] Note in a comment why this needs no variable-name parameterisation, citing both Stan files (football :188,195 and basketball :157,164 both declare `array[N_rounds] vector[K] offense` / `defense`, and `home_advantage_off`/`_def` are `vector<lower=0>[K]` in both) — the earlier analysis that called a 2DT round trajectory impossible was wrong, and this comment is what stops it being re-derived.

- [ ] Add the tibble to the per-division `parts` list so Task 5's `mutate(division = div)` + bind + write picks it up with no extra write call.

- [ ] Update the roxygen in R/extract-basketball-iceland.R and R/extract-handball-iceland.R: the file list becomes six (seven after Task 8), and the sentence claiming `round_strengths_quantiles` is football-specific is deleted.

- [ ] Re-run: GREEN. `devtools::test(filter = 'publish-football-golden')` must still be GREEN — the 2DT path shares the helper but writes to a different tree. Full suite FAIL 0.

- [ ] Commit: `feat(extract): publish per-round strength trajectories for basketball and handball`.

**Verification.** Both 2DT sports write a gapless 1..N round trajectory over the 9-cell component x location grid with no NA values, and a team's final-matchweek `total`/`avg` median reproduces its `cur_strength` posterior median to 1e-6 — proving the trajectory is the same quantity the existing `team_strengths` surface reports, at the same index, and not a differently-indexed neighbour.

### Task 8: fit_meta.parquet on BOTH the football and the 2DT trees

**Files:**

- MODIFY R/extract-iceland-2dt-shared.R (write block)
- MODIFY R/extract-football-iceland.R (write block, near :1700-1717)
- CREATE tests/testthat/test-extract-fit-meta.R
- MODIFY tests/testthat/helper-extract-fixtures.R (`build_football_extracts_fixture` writes it too)

- [ ] Write tests/testthat/test-extract-fit-meta.R. Assert for one basketball cell, one handball cell and one football cell that `fit_meta.parquet` exists, has exactly one row, has columns `n_draws` (integer), `fit_date` (Date), `stan_model` (character), `model_units` (character), that `n_draws` equals `posterior::ndraws(fit$draws('lp__'))`, that `fit_date` equals the partition's `fit_date=` segment, that `stan_model` equals `league$stan_model` (`basketball_iceland/2d_student_t_scalarsigma.stan`, `handball_iceland/2d_student_t.stan`, `football_iceland/bivariate_poisson_no_inflation.stan` — verified at config/leagues.yml:58, :114, :292), and that `model_units` is `points` / `goals` / `log_rate` respectively. Add the seam assertion WS9 depends on: `expect_false('division' %in% names(fit_meta))` with a comment saying fit_meta is partition-level and the reader must not split it by division.

- [ ] RUN: `Rscript -e 'devtools::load_all(quiet = TRUE); devtools::test(filter = "extract-fit-meta")'`. EXPECTED RED: `file.exists(.../fit_meta.parquet) is not TRUE` for all three sports.

- [ ] Implement in the 2DT extractor: build the one-row tibble after the loop and `arrow::write_parquet()` it into the partition. `n_draws <- posterior::ndraws(fit$draws('lp__'))` — this is exactly the value each 2DT publisher recomputes today, which is one of the reasons a publisher still has to hold a fit. `model_units` comes from the sport (`switch(sport, basketball = 'points', handball = 'goals')`), NOT from config.

- [ ] Implement the same in `extract_football_iceland()` with `model_units = 'log_rate'` (football's `home_advantage_*` and `offense`/`defense` are log-rates — Stan/football_iceland/bivariate_poisson_no_inflation.stan:155,185). Write it as a 7th file alongside `sim_inputs_team` / `sim_inputs_scalar`; it must NOT enter the `file_types` loop, which is division-bound.

- [ ] Add fit_meta to `build_football_extracts_fixture()` in tests/testthat/helper-extract-fixtures.R so the golden football publish still finds a complete partition once WS9 starts requiring it. Keep the values deterministic (`n_draws = FIXTURE_N_DRAWS`, `fit_date = FIXTURE_FIT_DATE`).

- [ ] RUN `devtools::test(filter = 'publish-football-golden')`: MUST be GREEN with the committed hashes untouched. `read_extracted_football()` uses a fixed `file_types` vector (R/extract-football-iceland.R:1810-1815) so an extra file in the partition changes nothing published — verify by reading that block before you claim it.

- [ ] Full suite FAIL 0. Commit: `feat(extract): write fit_meta.parquet on both the football and 2DT extracts trees`.

**Verification.** All three sports write an identical-schema one-row `fit_meta.parquet` whose `n_draws` matches `posterior::ndraws(fit$draws('lp__'))` and whose `stan_model` matches config, football's 90 published JSON hashes are unchanged, and the file provably carries no `division` column so WS9's division split cannot swallow it.

### Task 9: Regenerate the committed 2DT extracts fixture and re-pin the harness contract

**Files:**

- MODIFY tools/make-extract-fixtures.R (`.write_2dt_extract_fixtures`, if the stub call needs `n_rounds`)
- REGENERATE tests/testthat/fixtures/extracts/** (20 files today -> 28)
- MODIFY tests/testthat/test-fixture-harness.R (:52-95, the 5-parquet contract and the 250 KB budget)

- [ ] Read tools/make-extract-fixtures.R:137-166 and tests/testthat/test-fixture-harness.R:52-95. The generator runs the REAL extractors against `stub_fit(stub_2dt_draws(...))` — so the committed fixture's schema is the extractor's own output, which is exactly why it must be regenerated after Tasks 5-8 and not hand-edited. Note today's committed tree: 20 files, 5 per (sport, sex) partition, BD/OD only, `du -sk` = 288 KB with the test's own sum-of-file-sizes measure sitting just under the 250 KB budget (team_strengths_quantiles is 36,931 bytes x 4).

- [ ] Rewrite the harness contract test FIRST, as the RED. Replace the `%in%` five-file check at test-fixture-harness.R:56-77 with `expect_setequal(list.files(part), c('predicted_matches.parquet','team_strengths_quantiles.parquet','round_strengths_quantiles.parquet','home_advantage_quantiles.parquet','final_positions.parquet','points_distribution.parquet','fit_meta.parquet'))` and add, for every division-keyed file, `expect_setequal(unique(df$division), .iceland_division_codes(key, sex))` plus `expect_false('division' %in% names(fit_meta))`.

- [ ] RUN: `Rscript -e 'devtools::load_all(quiet = TRUE); devtools::test(filter = "fixture-harness")'`. EXPECTED RED: `expect_setequal` reports the two new filenames missing from `list.files(part)`, and `unique(df$division)` is NULL for the committed (pre-Task-5) parquets.

- [ ] Update `.write_2dt_extract_fixtures()` if `stub_2dt_draws()` now needs `n_rounds` passed explicitly (it calls the stub directly at tools/make-extract-fixtures.R:154 rather than via `local_stub_2dt`) — pass `n_rounds = prep$stan_data$N_rounds`. Then regenerate: `Rscript tools/make-extract-fixtures.R` from the repo root (NOT from a worktree subdirectory; the script derives the package root from its own `--file=` path but `here::here()` elsewhere does not).

- [ ] MEASURE the regenerated tree and act on the number, do not guess it: `find tests/testthat/fixtures/extracts -type f -exec ls -l {} \; | awk '{s += $5} END {print s}'`. Two divisions (4 + 6 teams instead of 4) and a 99-quantile round trajectory over ~42 (team, matchweek) pairs per partition will exceed the 250 KB budget by a wide margin. Raise the constant at tests/testthat/test-fixture-harness.R:94 to the next round number above the measured size and put the MEASURED byte count and the reason in a comment on the same line — 'two divisions x the 99-quantile round trajectory; measured N bytes on <date>'. A budget raised without a recorded measurement is not a budget.

- [ ] Re-run test-fixture-harness: GREEN. Then run every consumer of the fixture tree: `grep -rln 'fixture_extracts_root\|fixtures", "extracts"' tests/testthat/` (today: helper-extract-fixtures.R and test-fixture-harness.R) and `devtools::test(filter = 'publish-basketball|publish-handball')`.

- [ ] Full suite: `Rscript -e 'devtools::load_all(quiet = TRUE); devtools::test()'` — FAIL 0, and record the SKIP count against Plan A's baseline of 45 (it must not have grown; tests/testthat/test-fixture-skip-hygiene.R already fails the build on any `skip(`/`skip_if(`/`Sys.getenv` gate in new bb/hb coverage).

- [ ] `git -C /Users/brynjolfurjonsson/sports status` and confirm only the intended fixture parquets moved. Commit the regenerated fixtures WITH the code: `test(fixtures): regenerate the 2DT extracts tree at the 7-parquet, two-division shape`. Do NOT push.

**Verification.** The committed extracts fixture is byte-for-byte the current extractor's own output at the new contract — 7 parquets per partition, both divisions present in every division-keyed file, fit_meta division-free — the harness asserts that set exactly rather than by `%in%`, the size budget carries a measured number, and the full suite is FAIL 0 with SKIP no higher than Plan A's 45.

**Risks.**

- BLOCKING DEPENDENCY ON WS7. Nothing in WS8 can run until `.iceland_division_codes(key, sex)` exists AND `load_leagues()` accepts `publish_divisions` (plus the optional `expected_meetings`) for both 2DT leagues. `config/leagues.schema.json` makes `definitions.publishDivisionList.items` `additionalProperties: false` with `required: [code, slug, label_is, is_cup]`, so an `expected_meetings` key without the matching schema edit takes `load_leagues()` — and therefore every script — down at load time. Do not stub the helper locally in WS8; that would create the second definition the spec's WS7 exists to prevent.
- WS10 SEAM — the one that re-creates the -13 af 22 bug. Spec section 12 derives `meta.n_rounds` as max over teams of `played + remaining_scheduled`. For basketball that counts the úrslitakeppni: Bónusdeild karla 2026 has `played` up to 35 against a 22-round regular season. WS10 MUST call `.division_regular_rounds()` and cut before deriving, or publish `n_rounds = 35` and a `season_scope: regular_season` payload that contradicts its own number. WS8 produces the helper; WS10 must consume it rather than re-deriving.
- WS9 SEAM — `fit_meta.parquet` is the only file in the partition without a `division` column. `read_extracted_football()`'s per-division split (R/extract-football-iceland.R:1793+) splits every file it reads on the payload `division`; the generalised `read_extracted_iceland()` must treat fit_meta as partition-level (a `profile$partition_extracts` slot, not `required_extracts`) or it will filter it to zero rows for every division.
- BASKETBALL FEMALE 1D DEGRADES, VISIBLY. It is 11 teams with byes and pair meetings of 1, 2 and 4, so config deliberately omits `expected_meetings` and Task 4 falls back to `source = 'none'` — no cut, no upcoming cap, and a warning. That cell therefore publishes a table that includes its post-season rows. This is the honest outcome (inventing a cut that demonstrably fails the pair test would be exactly the 'assertion as source' the spec forbids), but WS10/WS12 should surface it: it is the natural first consumer of `n_rounds_source`, and the permanent fix is capturing the KKÍ stage ids from finding N7.
- TRAJECTORY INDEX ALIGNMENT IS A STANDING ASSUMPTION. `.compute_team_strength_trajectory()` derives each team's global round as a `row_number()` over the played `results` it is handed, and indexes the fit's `offense[r, k]` with it. That is only correct while the extractor's `results` set matches the one `prepare_data()` modelled. It does today because neither 2DT league configures a `training_filter` (verified in config/leagues.yml — only football_iceland has one, at :300). Task 5 adds `stopifnot(is.null(league$training_filter))` for that reason; if a future session adds a training filter to basketball or handball, that guard fires rather than the trajectory silently mis-indexing. Football already lives with the looser version of this (its helper warns and drops out-of-range entries).
- STUB VALUE CHURN. Task 2 changes what `stub_2dt_draws()` emits for every `cur_*` variable (they are now derived from the offense/defense walk instead of generated independently), so every committed 2DT fixture parquet changes value. Structure-only assertions survive; any test asserting an exact 2DT number does not. Grep and re-run before committing Task 2 rather than discovering it during Task 9's regeneration. Football is untouched — `build_football_extracts_fixture()` is a pure function of the facts fixture and never calls the 2DT stub.
- FIXTURE SIZE. The committed extracts tree grows from 20 files / ~230 KB to 28 files carrying two divisions and a 99-quantile round trajectory. The budget assertion at tests/testthat/test-fixture-harness.R:94 will fail; raising it is correct but must carry the measured byte count and the reason, or the next session cannot tell a deliberate raise from a slipped one.
- The 250 KB budget interacts with the fixture's shape, not just its size: `FIXTURE_DIVISIONS` keeps BD/OD at 4 teams *because* of that budget (helper-fixture-facts.R:14-16). Do not 'fix' a budget overrun by shrinking the fixture — the two-division shape is the thing under test.

---

# WS9 — one reader, one publisher, one dispatch (design §10 d/f/g; the B4 fix)

**Goal.** Make data/beliefs/extracts/ the sole publish input for all three sports, so basketball and handball publish on CI for the first time. Today publish_one() (R/publish-pipeline.R:57-133) takes the extracts branch only for key=="football_iceland" (:60-103); every other league falls through to data/beliefs/fits/.../fit.rds (:104-115), which .gitignore:48 excludes and decide-publish.yml never produces — it warns "No fit at {fit_path} -- skipping" and returns invisible(NULL) with exit 0. WS9 deletes that fallback and the dispatch list, generalises read_extracted_football (R/extract-football-iceland.R:1793) into read_extracted_iceland() by parameterisation, renames publish_football_iceland (R/publish-football-iceland.R:697) to publish_iceland_league() gated on a per-sport profile, and deletes publish_basketball_iceland (R/publish-basketball-iceland.R:38-371) and publish_handball_iceland (R/publish-handball-iceland.R:37-366) outright.

ARCHITECTURE VERDICT — full unification, YES; with two named deviations from the spec's literal text, both justified, neither a retreat from unification.

ID-1: publish_iceland_league KEEPS the division loop inside; its signature is publish_iceland_league(extracted, league, sex, profile, end_date, root, output_root, extracts_root, archive_root, round_predictions_history_root), NOT the spec §10(f) per-division `division` argument. Reason, measured: prepare_data() and read_table("results") are hoisted above the loop at R/publish-football-iceland.R:780-792, and the loop at :794 runs up to 5 divisions for football male. A per-division signature re-runs prepare_data() once per division (up to 5x per cell, 2.5x average across the 9 football cells) for zero behavioural gain, and extracting the ~920-line loop body into a separate function is pure code motion — the single largest textual edit in the workstream with no payoff and no way to review it except by re-reading 920 lines. Unification is one publisher for three sports; it is not decomposition of that publisher. If a later session wants the per-division function, the honest shape is .publish_iceland_division(ext, ctx, division, profile) taking a hoisted `ctx` — out of scope here.

ID-2: next_games needs TWO input-shape adapters behind ONE output contract. This is a real per-sport difference in the extract parquets, not drift: football's predicted_matches.parquet is a scoreline COUNT table (home_team, away_team, match_date, home_goals, away_goals, count) aggregated inline at R/publish-football-iceland.R:1011-1072, while the 2DT predicted_matches.parquet is already per-match summarised — verified by reading tests/testthat/fixtures/extracts/sport=basketball/.../predicted_matches.parquet: columns game_nr, match_date, home_team, away_team, division, mean_home_goals, mean_away_goals, mean_goal_diff, p_home_win, p_draw, p_away_win, goal_diff_distribution (list col), produced by .compute_predicted_matches_2dt (R/extract-iceland-2dt-shared.R:79-160). Task 4 puts both behind .next_games_rows_pfi(), branching on profile$predicted_matches_shape, emitting football's identical column set from either. The 2DT branch is a filter + rename + join — deleting R/publish-basketball-iceland.R:130-166 is a net deletion that supplies goal_diff_distribution, which the platform's fixture strip needs and bb/hb have never emitted.

SEQUENCING AND THE STOP POINT. Football is live and cron-published several times a day; tests/testthat/fixtures/golden/football-publish-hashes.csv (93 lines = 92 JSONs over 9 cells) is the only net. Every one of tasks 1-6 ends with `devtools::test(filter="publish-football-golden")` byte-identical, and each is a separate commit, so any step can be reverted alone. Task 1 first EXTENDS the net to cover publish_one's dispatch (today the golden test calls publish_football_iceland directly at tests/testthat/test-publish-football-golden.R:21-32, so the dispatch rewrite in task 7 would be unnetted). The NAMED STOP is after task 5: if the literal-generalisation of the 1618-line publisher is not green by then, stop before task 7. That partial landing is safe — football has a working publisher under a new name, bb/hb are exactly as broken as today, and nothing is half-migrated. Do not start task 7 with tasks 1-6 red.

**Consumes.**

- WS7: .iceland_division_codes(key, sex), .iceland_division_slugs(key, sex), .iceland_division_labels(key, sex), .iceland_division_split(key, sex), .iceland_division_badges(key, sex), and an is_cup lookup per (key, sex, code). WS9 replaces the five .football_iceland_division_* call sites at R/publish-football-iceland.R:742, :744, :797, :1009 and R/extract-football-iceland.R:1801,1806, plus the literal `is_cup <- identical(target_div, "CUP")` at R/publish-football-iceland.R:795. HARD DEPENDENCY: tasks 3 and 5 cannot land before WS7.
- WS7: config/leagues.yml publish_divisions blocks for basketball_iceland {BD/bd, 1D/1d} and handball_iceland {OD/od, G66/g66}, both sexes, every entry carrying is_cup (N2: definitions.publishDivisionList.items is additionalProperties:false, required [code, slug, label_is, is_cup]). Without these, .iceland_division_codes("basketball_iceland", sex) aborts and task 7 cannot run.
- WS8: a `division` payload column on all five 2DT parquets. Today only predicted_matches.parquet carries it (verified against the committed fixture: divisions BD,1D). read_extracted_iceland reuses football's existing per-division split, which returns profile$empty_extracts for any parquet lacking the column — so tasks 1-7 run WITHOUT WS8, but final_positions/points_distribution/team_strengths/home_advantage ship empty records until WS8 lands. Task 7's assertions are scoped to what the current fixture can prove; the non-empty-table assertions belong to WS8.
- WS8: round_strengths_quantiles.parquet and fit_meta.parquet. Both MUST be in profile$optional_extracts, never required — see risk R2.
- Plan A / WS2 harness: fixture_facts_root(env), fixture_extracts_root(sports, env), build_football_extracts_fixture(facts_root, extracts_root, sex, fit_date), publish_json_digest(path) (tests/testthat/helper-extract-fixtures.R:118), FIXTURE_END_DATE = 2100-01-15, FIXTURE_FIT_DATE = 2100-01-01, FIXTURE_N_DRAWS = 50L, FIXTURE_DIVISIONS (tests/testthat/helper-fixture-facts.R:9-23), stub_fit(), local_stub_2dt(league, sex, end_date, root, n_draws, constants) (tests/testthat/helper-stub-fit.R:127).
- Plan A: tests/testthat/fixtures/golden/football-publish-hashes.csv — 92 JSONs over 9 football cells. The gate on tasks 1-6.
- WS10 (downstream): consumes profile$points and profile$units for meta.json v2, and profile$surfaces for basis/qualify gating. WS9 carries those fields but reads only $points (task 6) and $surfaces (task 6). Do not emit meta.units/n_rounds/season_scope/basis/p_qualify in WS9.
- WS11 (downstream, ORDERING CONSTRAINT): .validate_or_abort (R/publish-pipeline.R:145-158) currently early-returns for any sport without config/publish-schemas/<sport>/, so bb/hb publish unvalidated through task 7. WS11's subtree fix may land any time; WS11's ARMING (git mv of _draft/{basketball,handball}) must land AFTER task 7, or the stale June bb/hb JSON aborts football's publish.
- WS12 (downstream): owns turning publish_one's quiet 'no partition yet' skip into a FAIL for an in-season cell, and the per-cell tryCatch in scripts/05_publish.R. WS9 preserves the existing tryCatch and its extract_partition_exists re-raise verbatim — do not change the skip semantics here.
- WS14 (downstream): owns rewriting the publish_one roxygen (R/publish-pipeline.R:20-31 still says the bb/hb migration is 'deferred to the autumn 2026 cutover') and .claude/rules/publish-layer.md. WS9 task 8 only ships the machine-checkable grep guard.

**Produces.**

- R/publish-profile.R :: sport_publish_profile(sport) -> list(required_extracts=chr, optional_extracts=chr, empty_extracts=named list of 0-row tibbles, predicted_matches_shape="scoreline_counts"|"match_summary", value_link=named chr by component, points=c(win=,draw=,loss=) integer, has_ties=lgl, tie_threshold=num, diff_bins=list(width=,low=,high=), units=list(strength=,home_advantage=,diff_bin_width=), surfaces=chr). Exported. Aborts for an unknown sport.
- R/extract-iceland-read.R :: read_extracted_iceland(league, sex, fit_date = NULL, extracts_root = here::here("data","beliefs","extracts"), target_divs = NULL, profile = sport_publish_profile(league$sport)) -> named list keyed by division code, each a list of profile$required_extracts + profile$optional_extracts tibbles, plus $fit_date (Date), $sim_inputs (list|NULL), $cup_bracket (list|NULL). Exported. REPLACES read_extracted_football, which is deleted, not aliased.
- R/publish-pipeline.R :: extract_partition_exists(extracts_root, sport, country, sex) -> logical. Renamed from football_extract_partition_exists; body unchanged.
- R/publish-iceland-league.R :: publish_iceland_league(extracted, league, sex, profile = sport_publish_profile(league$sport), end_date = Sys.Date(), root = here::here("data"), output_root = here::here("data","publish"), extracts_root = here::here("data","beliefs","extracts"), archive_root = here::here("data","beliefs","archive"), round_predictions_history_root = NULL) -> invisible(NULL). Renamed from publish_football_iceland. Loops divisions internally (ID-1).
- R/publish-pipeline.R :: publish_one(static, betting, key, sex, root = here::here("data"), validate = TRUE, end_date = Sys.Date()) -> invisible(NULL). NEW trailing end_date parameter; single extracts path for every sport; no fit_path fallback, no dispatch list.
- R/publish-next-games.R :: .next_games_rows_pfi(predicted, profile, pred_d, family_divs, division_badges, end_date, horizon_days = 14L, venues = NULL) -> tibble with exactly c(date, venue, division, division_code, home, away, mean_home_goals, mean_away_goals, mean_goal_diff, p_home_win, p_draw, p_away_win, goal_diff_distribution) for BOTH input shapes.
- Publish path shape for bb/hb becomes data/publish/{basketball,handball}/iceland/{karla,kvenna}-{bd,1d,od,g66}/ with football's 10 JSON basenames per cell: meta, next_games, standings, standings_history, team_strengths, team_strengths_history, final_positions, final_positions_history, points_distribution, home_advantage.
- tests/testthat/test-publish-b4-acceptance.R :: the RDS-absent proof — 8 bb/hb cells published from a fixture root with no data/beliefs/fits/ present.
- tests/testthat/test-publish-refactor-hygiene.R :: zero-reference grep guard for publish_basketball_iceland, publish_handball_iceland, read_extracted_football, football_extract_partition_exists, .football_iceland_division_.
- DELETED: R/publish-basketball-iceland.R (371 lines), R/publish-handball-iceland.R (366 lines), .compute_standings_rows_2dt (R/publish-iceland-2dt-helpers.R:149-225), read_extracted_football, tests/testthat/test-publish-basketball.R, tests/testthat/test-publish-handball.R.

### Task 1: Thread end_date through publish_one, and extend the golden net to cover the dispatch path

**Files:**

- modify: /Users/brynjolfurjonsson/sports/R/publish-pipeline.R (signature at :52-55; the publish_football_iceland call at :93-101; roxygen at :43-49)
- test: /Users/brynjolfurjonsson/sports/tests/testthat/test-publish-football-golden.R (append a third test_that block)

- [ ] Read tests/testthat/test-publish-football-golden.R:6-47. Note it calls read_extracted_football() and publish_football_iceland() DIRECTLY — publish_one()'s dispatch is currently outside the golden net, which is exactly what task 7 rewrites.

- [ ] Append a test_that("publish_one reproduces the golden football manifest") block that: (a) root <- fixture_facts_root(); (b) for each sex, build_football_extracts_fixture(root, file.path(root, "beliefs", "extracts"), sex); (c) league <- load_leagues()[["football_iceland"]]; static <- league[c("sport","country","sexes","active","stan_model","data_source")] (copy the slice verbatim from scripts/05_publish.R:27-31); (d) publish_one(static, league$betting, "football_iceland", sex, root = root, validate = FALSE, end_date = FIXTURE_END_DATE); (e) hash every .json under file.path(root, "publish") with publish_json_digest() and expect_setequal + hash-equality against fixtures/golden/football-publish-hashes.csv, mirroring lines 33-46 of the existing test.

- [ ] Note in a code comment why the temp root is safe: publish_one derives output_root = file.path(root, "publish") and publish_football_iceland derives round_predictions_history_root = file.path(dirname(output_root), "beliefs", "round_predictions_history") (R/publish-football-iceland.R:713-718), so with a tempdir root BOTH land inside the tempdir and no test writes into the repo's real data/beliefs/ tree.

- [ ] RUN: Rscript -e 'devtools::test(pkg = "/Users/brynjolfurjonsson/sports", filter = "publish-football-golden")'. EXPECTED RED, exactly: Error in publish_one(static, league$betting, "football_iceland", sex, : unused argument (end_date = FIXTURE_END_DATE). publish_one's signature (R/publish-pipeline.R:52-55) is (static, betting, key, sex, root, validate) — it has no end_date, so production publish always runs prepare_data at Sys.Date(), which filters every 2100-dated fixture row out.

- [ ] Add `end_date = Sys.Date()` as the LAST parameter of publish_one(), an @param roxygen line, and pass `end_date = end_date` into the publish_football_iceland(...) call at R/publish-pipeline.R:93-101. Change nothing else.

- [ ] RUN the same filter again. EXPECTED GREEN. If instead the hash comparison fails, the printed `changed payloads:` list names the cells — that means end_date is not reaching the publisher; do not regenerate the golden file to make it pass (see risk R3).

- [ ] RUN the full suite: Rscript -e 'devtools::test(pkg = "/Users/brynjolfurjonsson/sports")'. Expect FAIL 0 against the Plan A baseline of FAIL 0 / SKIP 45 (baseline observed at the start of this planning session, 2026-09-04).

- [ ] Rscript -e 'devtools::document("/Users/brynjolfurjonsson/sports")'; commit: `feat(publish): thread end_date through publish_one and net its dispatch path` with the Co-Authored-By trailer.

**Verification.** The golden manifest is reproduced through publish_one(), not only through publish_football_iceland(). Every later dispatch edit (tasks 3, 5, 7) is now caught by a byte-level check, and publish_one gained the parameter without which no far-future fixture can drive it.

### Task 2: sport_publish_profile(sport): the per-sport data that replaces four parallel booleans

**Files:**

- create: /Users/brynjolfurjonsson/sports/R/publish-profile.R
- test: /Users/brynjolfurjonsson/sports/tests/testthat/test-publish-profile.R
- modify: NAMESPACE (via devtools::document)

- [ ] Read R/extract-football-iceland.R:1808-1815 (football's required file_types), :1826-1907 (its empty_tibbles literal, including tournament_placements), and R/extract-iceland-2dt-shared.R:417-435 (the exactly five parquets the 2DT extractor writes). These three blocks are the source of truth for required/optional/empty; transcribe, do not invent.

- [ ] Write tests/testthat/test-publish-profile.R asserting: (a) sport_publish_profile("football")$points == c(win=3L, draw=1L, loss=0L); handball c(win=2L, draw=1L, loss=0L); basketball c(win=2L, draw=0L, loss=0L). (b) $predicted_matches_shape is "scoreline_counts" for football and "match_summary" for basketball and handball. (c) football's $required_extracts is setequal to the six names in R/extract-football-iceland.R:1808-1815; the 2DT $required_extracts is setequal to c("predicted_matches","team_strengths_quantiles","home_advantage_quantiles","final_positions","points_distribution"). (d) FOR ALL THREE SPORTS: "fit_meta" %in% $optional_extracts AND !("fit_meta" %in% $required_extracts); likewise "round_strengths_quantiles" is optional for the two 2DT sports and required for football; "tournament_placements" is optional for football and absent for the 2DT sports. (e) names($empty_extracts) is setequal to union($required_extracts, $optional_extracts) and every element is a 0-row tibble (vapply nrow == 0L). (f) football's $surfaces contains "round_predictions_history", "xg", "cup_bracket", "split", "preseason_strengths"; neither 2DT profile contains any of those five; all three contain "standings", "standings_history", "team_strengths", "team_strengths_history", "final_positions", "final_positions_history", "points_distribution", "home_advantage", "next_games", "meta". (g) $has_ties is FALSE/TRUE/TRUE for basketball/handball/football and $tie_threshold is 0 for basketball (matching config/leagues.yml betting.scoring). (h) sport_publish_profile("cricket") aborts.

- [ ] RUN: Rscript -e 'devtools::test(pkg = "/Users/brynjolfurjonsson/sports", filter = "publish-profile")'. EXPECTED RED, exactly: Error in sport_publish_profile("football") : could not find function "sport_publish_profile".

- [ ] Implement R/publish-profile.R: a file-scope .PUBLISH_PROFILES list keyed by sport, and an exported sport_publish_profile(sport) that looks it up and cli::cli_abort()s on an unknown sport naming the three known ones. Also populate $value_link (per component: "identity" for the 2DT sports, "exp" for football with its total half-split declared here), $diff_bins (list(width=1L, low=-50L, high=50L) for 2DT, matching .extract_2dt_iceland_pfi's defaults at R/extract-iceland-2dt-shared.R:319-322) and $units (strength/home_advantage/diff_bin_width). NOTHING reads value_link, diff_bins or units yet — they exist so WS8 and WS10 consume this one registry instead of re-deriving. Say so in the roxygen.

- [ ] RUN the filter again -> green. RUN the full suite -> FAIL 0.

- [ ] devtools::document(); commit: `feat(publish): add sport_publish_profile, the per-sport publish registry`.

**Verification.** A single data structure answers every 'does this sport do X' question, and assertion (d) is the load-bearing one: WS8's new fit_meta.parquet can never mark an existing on-disk football partition incomplete, because a required-file list drives .partition_is_complete().

### Task 3: One reader: read_extracted_iceland, and the extract_partition_exists rename

**Files:**

- create: /Users/brynjolfurjonsson/sports/R/extract-iceland-read.R
- modify: /Users/brynjolfurjonsson/sports/R/extract-football-iceland.R (delete read_extracted_football, :1793-end-of-function)
- modify: /Users/brynjolfurjonsson/sports/R/publish-pipeline.R (:1 @include chain, :10 rename, :62 call, :72 call)
- modify: /Users/brynjolfurjonsson/sports/R/replay.R:103
- modify: /Users/brynjolfurjonsson/sports/tests/testthat/helper-extracted-publish.R:18
- modify: /Users/brynjolfurjonsson/sports/tests/testthat/test-publish-football-golden.R:21
- modify: /Users/brynjolfurjonsson/sports/tests/testthat/test-extract-football-iceland.R:249,321,332,337,346,353
- modify: /Users/brynjolfurjonsson/sports/tests/testthat/test-fail-loud.R:10-17
- test: /Users/brynjolfurjonsson/sports/tests/testthat/test-extract-iceland-read.R (new)

- [ ] Write tests/testthat/test-extract-iceland-read.R with: (a) league <- load_leagues()[["basketball_iceland"]]; root <- fixture_extracts_root("basketball"); out <- read_extracted_iceland(league, sex = "male", fit_date = FIXTURE_FIT_DATE, extracts_root = root); expect_setequal(setdiff(names(out), c("fit_date","sim_inputs","cup_bracket")), .iceland_division_codes("basketball_iceland", "male")). (b) every per-division element has names() covering sport_publish_profile("basketball")$required_extracts. (c) out$fit_date == FIXTURE_FIT_DATE. (d) out$BD$predicted_matches has >0 rows and its team set is disjoint from out$`1D`$predicted_matches' — the committed fixture's predicted_matches.parquet carries division BD and 1D, so this proves the per-division split works on a 2DT tree. (e) a partition with one REQUIRED parquet removed (copy to a tempdir, file.remove predicted_matches.parquet) aborts with a message matching "is incomplete". (f) a partition missing ONLY round_strengths_quantiles.parquet and fit_meta.parquet reads fine and returns 0-row tibbles for both — this is the current committed fixture, so it is the default case, and it is what lets tasks 5-7 run before WS8 lands. (g) football still reads: read_extracted_iceland(load_leagues()[["football_iceland"]], "male", fit_date = FIXTURE_FIT_DATE, extracts_root = <built by build_football_extracts_fixture>) returns the five male football division codes.

- [ ] RUN: Rscript -e 'devtools::test(pkg = "/Users/brynjolfurjonsson/sports", filter = "extract-iceland-read")'. EXPECTED RED, exactly: Error in read_extracted_iceland(league, sex = "male", ...) : could not find function "read_extracted_iceland".

- [ ] Create R/extract-iceland-read.R by MOVING the read_extracted_football body verbatim out of R/extract-football-iceland.R (from :1793 to the closing brace after the `out$fit_date <- fit_date_out; out` block). Apply exactly four edits and no others: (1) rename, add the `profile = sport_publish_profile(league$sport)` parameter, and replace `stopifnot(league$sport == "football", league$country == "iceland")` with `stopifnot(league$country == "iceland")` plus a check that the derived league key exists; (2) derive `league_key <- paste0(league$sport, "_", league$country)` and VERIFY it against names(load_leagues()) with a stopifnot — do not assume the naming convention holds, assert it, and replace the two .football_iceland_division_codes(sex) calls at the old :1801 and :1806 with .iceland_division_codes(league_key, sex); (3) replace the literal `file_types <- c(...)` with `file_types <- profile$required_extracts` and `per_division_file_types <- c(file_types, profile$optional_extracts)` (the old code hardcoded the tournament_placements append); (4) replace the whole `empty_tibbles <- list(...)` literal with `empty_tibbles <- profile$empty_extracts`. Leave the descending fit_date scan, .partition_is_complete, the incomplete-partition abort, read_one_division, the sim_inputs block and the cup_bracket block untouched — those are already sport-agnostic and are copied ZERO times. sim_inputs and cup_bracket need no surface gating: their file.exists() checks already degrade to NULL for a 2DT partition.

- [ ] Delete read_extracted_football from R/extract-football-iceland.R. Rename football_extract_partition_exists -> extract_partition_exists at R/publish-pipeline.R:10 (body unchanged — it already takes sport as an argument) and update its two call sites (:72) and its test (tests/testthat/test-fail-loud.R:10,16,17).

- [ ] Update every remaining caller: R/publish-pipeline.R:62, R/replay.R:103, tests/testthat/helper-extracted-publish.R:18, tests/testthat/test-publish-football-golden.R:21, tests/testthat/test-extract-football-iceland.R:249,321,332,337,346,353. Also fix the comment reference at scripts/patch_cup_completed.R:82.

- [ ] Add R/extract-iceland-read.R and R/publish-profile.R to the `#' @include` chain at R/publish-pipeline.R:1 so the Collate order resolves; then Rscript -e 'devtools::document("/Users/brynjolfurjonsson/sports")' and Rscript -e 'devtools::load_all("/Users/brynjolfurjonsson/sports")' to confirm the package still loads.

- [ ] RUN filter="extract-iceland-read" -> green. RUN filter="publish-football-golden" -> MUST be byte-identical, both the direct and the publish_one test from task 1. RUN filter="extract-football-iceland" and filter="fail-loud" -> green. RUN the full suite -> FAIL 0.

- [ ] Commit: `refactor(extract): generalise read_extracted_football into read_extracted_iceland`.

**Verification.** One reader serves all three sports; the newest-complete-partition scan and the incomplete-partition abort exist in exactly one place. Football's 92 golden hashes are unchanged, proving the parameterisation is behaviour-preserving on the live path.

### Task 4: One next_games contract, two input shapes

**Files:**

- create: /Users/brynjolfurjonsson/sports/R/publish-next-games.R
- modify: /Users/brynjolfurjonsson/sports/R/publish-football-iceland.R (replace the inline block at :1006-1073 with a call)
- test: /Users/brynjolfurjonsson/sports/tests/testthat/test-publish-next-games.R (new)

- [ ] Read R/publish-football-iceland.R:991-1073 (the venue tribble, the division_labels recode and the summarise) and R/extract-iceland-2dt-shared.R:79-160 (.compute_predicted_matches_2dt). Confirm for yourself that the two predicted_matches shapes differ as described in ID-2 before writing any code.

- [ ] Write tests/testthat/test-publish-next-games.R with three blocks: (a) SCORELINE_COUNTS — hand-build a small tibble(home_team, away_team, match_date, home_goals, away_goals, count) with a known weighted mean, call .next_games_rows_pfi(..., profile = sport_publish_profile("football")), assert names(out) is EXACTLY c("date","venue","division","division_code","home","away","mean_home_goals","mean_away_goals","mean_goal_diff","p_home_win","p_draw","p_away_win","goal_diff_distribution") in that order, and assert mean_goal_diff equals the hand-computed count-weighted value to 1e-9. (b) MATCH_SUMMARY — read tests/testthat/fixtures/extracts/sport=basketball/country=iceland/sex=male/fit_date=2100-01-01/predicted_matches.parquet, call with profile = sport_publish_profile("basketball"), assert the SAME column set and order, assert all(out$p_draw == 0) (basketball has_ties is FALSE), and assert out$goal_diff_distribution[[1]] is a 2-column tibble(diff, p) with sum(p) within 1e-9 of 1. (c) EMPTY — both shapes with a 0-row input return a 0-row tibble with the identical column set and column classes (compare vapply(out, class, character(1)) across the two shapes). Use match dates from the fixture (2100-01-xx) — no near-date literals.

- [ ] RUN: Rscript -e 'devtools::test(pkg = "/Users/brynjolfurjonsson/sports", filter = "publish-next-games")'. EXPECTED RED, exactly: Error in .next_games_rows_pfi(...) : could not find function ".next_games_rows_pfi".

- [ ] Implement R/publish-next-games.R :: .next_games_rows_pfi(predicted, profile, pred_d, family_divs, division_badges, end_date, horizon_days = 14L, venues = NULL). For profile$predicted_matches_shape == "scoreline_counts", move R/publish-football-iceland.R:1011-1072 in VERBATIM (including the .data-pronoun comment at :1029-1031 — it documents a real dplyr trap). For "match_summary", the body is: filter on match_date within [end_date, end_date + horizon_days] and division %in% family_divs, arrange(match_date, game_nr), left_join venues, recode division -> division_code via division_badges, mutate date, select the contract columns. The 2DT parquet already carries every numeric column, so this branch computes nothing.

- [ ] In R/publish-football-iceland.R replace :1006-1073 with a single call, passing venues = male_top_division_venues (leave the tribble at :991-1004 where it is) and division_badges = the existing .football_iceland_division_code_labels() result — the badge helper is swapped to config in task 5, not here.

- [ ] RUN filter="publish-next-games" -> green. RUN filter="publish-football-golden" -> MUST be byte-identical. If a hash moves, the likely cause is column ORDER or the arrange() key; diff the produced next_games.json against the golden payload rather than regenerating the manifest. RUN the full suite -> FAIL 0.

- [ ] Commit: `refactor(publish): one next_games contract behind two extract shapes`.

**Verification.** Both extract shapes produce byte-identical output structure, proven on real fixture data for each; football's live next_games.json is unchanged. This is the piece that lets task 7 delete R/publish-basketball-iceland.R:130-166 (the bespoke summarise emitting mean_home/p_tie and no goal_diff_distribution) instead of porting it.

### Task 5: publish_football_iceland -> publish_iceland_league: rename and de-literalise. NAMED STOP POINT.

**Files:**

- rename: /Users/brynjolfurjonsson/sports/R/publish-football-iceland.R -> R/publish-iceland-league.R (git mv)
- modify: that file — :697 signature, :721 sport stopifnot, :742,:744,:797 division helpers, :778 output path segment, :795 is_cup literal, :1009 badge helper, :1375 round_predictions path segment
- modify: /Users/brynjolfurjonsson/sports/R/publish-pipeline.R (:1 @include, :93 call)
- modify: /Users/brynjolfurjonsson/sports/tests/testthat/test-publish-football-golden.R, test-publish-football.R, test-publish-football-split.R, test-publish-football-round-predictions.R, test-publish-cup-bracket.R, helper-extracted-publish.R
- test: /Users/brynjolfurjonsson/sports/tests/testthat/test-publish-iceland-league.R (new)

- [ ] CONFIRM WS7 has landed: Rscript -e 'devtools::load_all("/Users/brynjolfurjonsson/sports"); print(.iceland_division_codes("basketball_iceland", "male")); print(.iceland_division_slugs("football_iceland", "male"))'. If either errors, stop — this task cannot proceed.

- [ ] Write tests/testthat/test-publish-iceland-league.R: (a) publish_iceland_league(extracted = list(), league = list(sport = "cricket", country = "iceland"), sex = "male") errors; (b) country != "iceland" errors; (c) sex = "other" errors; (d) a football publish through publish_iceland_league() into a tempdir reproduces the golden manifest for the two BD cells (reuse the task-1 pattern, restricted to karla-bd and kvenna-bd hashes to keep the test cheap).

- [ ] RUN: Rscript -e 'devtools::test(pkg = "/Users/brynjolfurjonsson/sports", filter = "publish-iceland-league")'. EXPECTED RED, exactly: Error in publish_iceland_league(...) : could not find function "publish_iceland_league".

- [ ] git mv R/publish-football-iceland.R R/publish-iceland-league.R. Rename the function at :697 and add the `profile = sport_publish_profile(league$sport)` parameter after `sex`. Keep the division loop inside (ID-1) — do NOT add a `division` parameter and do NOT extract the loop body.

- [ ] Replace, one at a time, re-running the golden filter after each: (1) :721 `stopifnot(league$sport == "football", league$country == "iceland")` -> `stopifnot(league$sport %in% c("football","basketball","handball"), league$country == "iceland")` and derive `league_key <- paste0(league$sport, "_", league$country)`; (2) :742 / :744 / :797 -> .iceland_division_slugs / _labels / _split(league_key, sex); (3) :795 `is_cup <- identical(target_div, "CUP")` -> the config-driven is_cup lookup from WS7 (the literal is a football-only assumption and there is no cup division for bb/hb); (4) :1009 .football_iceland_division_code_labels() -> .iceland_division_badges(league_key, sex); (5) the `"football"` path segment in the out_dir file.path at :776-781 and in the round_predictions dir at :1374-1377 -> league$sport.

- [ ] Update every caller and test that names publish_football_iceland: R/publish-pipeline.R:93, tests/testthat/helper-extracted-publish.R, test-publish-football-golden.R, test-publish-football.R, test-publish-football-split.R, test-publish-football-round-predictions.R, test-publish-cup-bracket.R. Fix the @include chain at R/publish-pipeline.R:1 (it still names publish-football-iceland.R).

- [ ] devtools::document(); devtools::load_all(). RUN filter="publish-iceland-league" -> green. RUN filter="publish-football-golden" -> ALL 92 hashes byte-identical, through BOTH the direct call and publish_one. RUN filter="publish-football" and filter="publish-cup" -> green. RUN the full suite -> FAIL 0.

- [ ] Commit: `refactor(publish): publish_football_iceland becomes publish_iceland_league`.

- [ ] **STOP AND RE-VERIFY HERE.** Before starting task 6, confirm all four: (i) the full suite is FAIL 0; (ii) all 92 golden hashes match; (iii) git status shows no uncommitted working-tree changes under R/ or tests/; (iv) `git log --oneline -6` shows tasks 1-5 as five separate, individually revertable commits. If any of the four fails, fix it here. A partial landing at this point is SAFE: football publishes under the new name, bb/hb are exactly as broken as before, nothing is half-migrated.

**Verification.** The 1618-line publisher carries no football literal in its dispatch-relevant paths, and the golden manifest proves football's 92 payloads are bit-for-bit unchanged across the rename plus five literal replacements. This is the last step that can be abandoned without leaving the repo in an intermediate state.

### Task 6: Gate the football-only surfaces on profile$surfaces and drive points off profile$points

**Files:**

- modify: /Users/brynjolfurjonsson/sports/R/publish-iceland-league.R (the xG/round_predictions blocks around :845-912 and :1370-1410; the standings points at :1160; the preseason block at :1275-1325; the cup/tournament_placements blocks at :1560-1610)
- test: /Users/brynjolfurjonsson/sports/tests/testthat/test-publish-surfaces.R (new)

- [ ] Write tests/testthat/test-publish-surfaces.R that publishes football into a tempdir with a MUTATED profile and asserts the gating: (a) profile with "round_predictions_history" removed from $surfaces -> expect_false(file.exists(<history_root>/football/iceland/karla-bd/round_predictions_history.json)); (b) "xg" removed -> every row in standings.json has xg_for/xg_against/xpts as JSON null; (c) "preseason_strengths" removed -> no record in team_strengths.json carries a `preseason` key; (d) "cup_bracket" removed -> no bracket.json and no tournament_placements.json under the CUP cell; (e) "split" removed -> no `split` key in meta.json for a split cell. Plus a pure unit block: a small points helper returns 3/1/0, 2/1/0 and 2/0/0 wins/draws tallies for football/handball/basketball profiles.

- [ ] RUN: Rscript -e 'devtools::test(pkg = "/Users/brynjolfurjonsson/sports", filter = "publish-surfaces")'. EXPECTED RED on block (a) first: `file.exists(path) is not FALSE` — R/publish-iceland-league.R writes round_predictions_history.json unconditionally (the `else if (!file.exists(round_predictions_path))` branch at :1399-1410 creates an empty one even with no predictions).

- [ ] Wrap each football-only block in `if ("<surface>" %in% profile$surfaces)`: "xg" around the team_expected / round_predictions computation and the xg_for/xg_against/xpts/xg_trend standings columns (keep the columns present but NA_real_ when gated off — config/publish-schemas/football/standings.schema.json already types them ["number","null"], which is why the same schema shape serves bb/hb); "round_predictions_history" around :1370-1410; "preseason_strengths" around the preseason join; "cup_bracket" around tournament_placements.json and bracket.json; "split" around the meta$split block and the split-family machinery.

- [ ] Replace the hardcoded tabulation at R/publish-iceland-league.R:1160 `points = 3L * .data$wins + .data$draws` with `profile$points[["win"]] * .data$wins + profile$points[["draw"]] * .data$draws` (and add the loss term only if profile$points[["loss"]] is non-zero — it is 0 for all three, so keep the expression identical in value for football).

- [ ] RUN filter="publish-surfaces" -> green. RUN filter="publish-football-golden" -> MUST STILL BE BYTE-IDENTICAL: football's profile contains every surface and points 3/1/0, so every gate is TRUE and the arithmetic is unchanged. Any hash movement here means a gate is inverted or the points expression changed type (integer vs double) — fix the code, do not regenerate the manifest. RUN the full suite -> FAIL 0.

- [ ] Commit: `feat(publish): gate sport-specific publish surfaces on the profile`.

**Verification.** Every football-only surface is switched by one predicate with a per-sport data answer, and the golden manifest proves football's output is unaffected because all its gates evaluate TRUE. The publisher can now run for a sport that has no xG, no cup and no split.

### Task 7: THE B4 FIX: route bb/hb through the extracts path and delete both 2DT publishers

**Files:**

- modify: /Users/brynjolfurjonsson/sports/R/publish-pipeline.R (:1 @include; :57-133 — delete the football-only guard at :60, the fit_path block at :104-115 and the dispatch list at :117-127)
- delete: /Users/brynjolfurjonsson/sports/R/publish-basketball-iceland.R, /Users/brynjolfurjonsson/sports/R/publish-handball-iceland.R
- modify: /Users/brynjolfurjonsson/sports/R/publish-iceland-2dt-helpers.R (delete .compute_standings_rows_2dt, :149-225)
- delete: /Users/brynjolfurjonsson/sports/tests/testthat/test-publish-basketball.R, tests/testthat/test-publish-handball.R
- test: /Users/brynjolfurjonsson/sports/tests/testthat/test-publish-b4-acceptance.R (new)

- [ ] Write tests/testthat/test-publish-b4-acceptance.R USING A HEREDOC (cat <<'EOF' or a python script) — it contains the Icelandic labels Bónusdeild, 1. deild, Olísdeild and Grill 66-deild, and the Write/Edit tools mangle non-ASCII in this repo. The test: root <- fixture_facts_root(); file.copy the committed tests/testthat/fixtures/extracts/sport=basketball and sport=handball trees into file.path(root, "beliefs", "extracts"); expect_false(dir.exists(file.path(root, "beliefs", "fits"))) — the RDS-ABSENT CONDITION, asserted explicitly, because the RDS exists on the dev machine and its presence would mask the failure; then for key in c("basketball_iceland","handball_iceland") and sex in c("male","female") call publish_one(static, betting, key, sex, root = root, validate = FALSE, end_date = FIXTURE_END_DATE).

- [ ] Assertions in that test: (a) expect_setequal of the produced cell directories against the exact eight: publish/basketball/iceland/{karla,kvenna}-{bd,1d} and publish/handball/iceland/{karla,kvenna}-{od,g66}; (b) each cell contains exactly the ten JSON basenames football emits (meta, next_games, standings, standings_history, team_strengths, team_strengths_history, final_positions, final_positions_history, points_distribution, home_advantage); (c) meta.json carries `division` equal to the cell's code and `is_cup` FALSE, and `league` equal to the configured Icelandic label; (d) next_games.json$matches is non-empty for at least karla-bd and karla-od, and every match object has all of mean_home_goals, mean_away_goals, mean_goal_diff, p_home_win, p_draw, p_away_win, division_code, goal_diff_distribution and NONE of mean_home, mean_away, mean_diff, p_home, p_away, p_tie. Do NOT assert non-empty standings/final_positions here — those parquets carry no `division` column until WS8 lands, so they legitimately read as empty; WS8 owns that assertion. No skip(), no Sys.getenv gate (tests/testthat/test-fixture-skip-hygiene.R fails the build on either).

- [ ] RUN: Rscript -e 'devtools::test(pkg = "/Users/brynjolfurjonsson/sports", filter = "publish-b4-acceptance")'. EXPECTED RED: assertion (a) fails with Actual: character(0) (no publish/basketball or publish/handball directory is created at all), and the console carries four warnings of the form `! No fit at <root>/beliefs/fits/sport=basketball/country=iceland/sex=male/fit.rds -- skipping publish_basketball_iceland_male` from R/publish-pipeline.R:111-113. That message IS B4.

- [ ] Rewrite publish_one(): delete the `if (identical(key, "football_iceland"))` guard at :60 so the extracts branch is the ONLY path; inside it call read_extracted_iceland(league = league, sex = sex, extracts_root = extracts_root) and then publish_iceland_league(extracted = extracted, league = league, sex = sex, profile = sport_publish_profile(league$sport), end_date = end_date, root = root, output_root = output_root, extracts_root = extracts_root, archive_root = archive_root). Keep the surrounding tryCatch and its extract_partition_exists() re-raise VERBATIM — WS12 owns changing the quiet-skip semantics, not this task. Delete the fit_path block (:104-115) and the dispatch list (:117-127) entirely; the gitignored fit RDS stops being a publish input, which is the whole of B4.

- [ ] git rm R/publish-basketball-iceland.R R/publish-handball-iceland.R tests/testthat/test-publish-basketball.R tests/testthat/test-publish-handball.R. Remove both from the @include chain at R/publish-pipeline.R:1. Delete .compute_standings_rows_2dt (R/publish-iceland-2dt-helpers.R:149-225) — the unified publisher's inline tabulation driven by profile$points replaces it. Before deleting anything else from that helpers file, grep for each symbol: leave .compute_round_num_2dt alone (WS10 owns replacing it), and leave .compute_posterior_goals_2dt / .extract_team_draws_2dt / .compute_base_points_2dt / .compute_iter_team_points_2dt alone (the extractor still calls them).

- [ ] devtools::document(); devtools::load_all(). RUN filter="publish-b4-acceptance" -> GREEN, eight cells, ten files each. RUN filter="publish-football-golden" -> ALL 92 HASHES BYTE-IDENTICAL through both the direct call and publish_one. RUN the full suite -> FAIL 0. Also run: Rscript -e 'devtools::load_all("/Users/brynjolfurjonsson/sports"); print(names(formals(publish_one)))' and confirm there is no fit-related argument left.

- [ ] Commit: `fix(publish): make extracts the sole publish input for all three sports (B4)`. In the commit body, state that basketball and handball have never published on CI and that data/publish/{basketball,handball}/ JSON on disk is stale from 2026-06-23.

**Verification.** With data/beliefs/fits/ provably absent — the exact CI condition — all eight bb/hb cells publish ten JSONs each, with football's field names and goal_diff_distribution present and the old mean_home/p_tie names gone. Football's 92 golden payloads are unchanged. 737 lines of parallel publisher (the thing that produced B5) no longer exist.

### Task 8: Zero-reference guard

**Files:**

- test: /Users/brynjolfurjonsson/sports/tests/testthat/test-publish-refactor-hygiene.R (new)

- [ ] Write tests/testthat/test-publish-refactor-hygiene.R modelled on tests/testthat/test-skill-conventions.R: walk R/, scripts/, tools/, man/ and tests/testthat/ (excluding this file itself, by basename), and assert zero hits for each of publish_basketball_iceland, publish_handball_iceland, read_extracted_football, football_extract_partition_exists, [.]football_iceland_division_, and .compute_standings_rows_2dt. Add a second block asserting R/publish-pipeline.R contains no match for 'fit\\.rds' and no match for 'beliefs", "fits"'. Use testthat::test_path() to resolve paths, NOT here::here() — the worktree gotcha in the project memory (here::here resolves to the main checkout).

- [ ] Run devtools::document() FIRST so man/*.Rd is regenerated, then RUN: Rscript -e 'devtools::test(pkg = "/Users/brynjolfurjonsson/sports", filter = "publish-refactor-hygiene")'. EXPECTED RED: the expectation prints the surviving file:line hits. The likely survivors are stale man/*.Rd, the comment at scripts/patch_cup_completed.R:82, and roxygen prose in R/publish-pipeline.R:20-31 (which still says the bb/hb migration is 'deferred to the autumn 2026 cutover').

- [ ] Fix each survivor. For R/publish-pipeline.R:20-31, replace the paragraph with a one-line accurate statement — the full docstring and .claude/rules/publish-layer.md rewrite belongs to WS14; do not do it here beyond making the grep pass.

- [ ] RUN the filter again -> green. RUN the full suite -> FAIL 0. Commit: `test(publish): guard against resurrected 2DT publisher references`.

**Verification.** A future session cannot reintroduce a parallel 2DT publisher or a fit-RDS publish input without the build failing. The grep runs over generated man/ too, so a stale roxygen alias is caught.

**Risks.**

- R1 — Collate/@include breakage. R/publish-pipeline.R:1 is `#' @include publish-football-iceland.R publish-basketball-iceland.R publish-handball-iceland.R validate-publish.R`. Tasks 3, 5 and 7 each add or delete files named there. Symptom: devtools::load_all() fails with `object 'publish_basketball_iceland' not found` or a Collate error in DESCRIPTION. Mitigation: run devtools::document() AND devtools::load_all() in every task that adds or removes an R/ file, before running any test.
- R2 — fit_meta.parquet as a required extract would break production football publish. read_extracted_iceland's .partition_is_complete() (moved verbatim from R/extract-football-iceland.R:1828-1830) requires every file in profile$required_extracts. WS8 adds fit_meta.parquet to BOTH trees, but every football partition already on disk lacks it — marking it required makes every existing partition incomplete and the reader aborts with 'No fit_date partition ... contains a complete extracted set'. Task 2 assertion (d) is the guard. Same argument for round_strengths_quantiles on the 2DT trees.
- R3 — the golden manifest is the only football net, and regenerating it silently voids it. Tasks 1-6 must each leave all 92 hashes unchanged. If a hash moves, diff the produced JSON against the golden payload and fix the CODE. Only regenerate (Rscript tools/make-extract-fixtures.R --golden) for a deliberate contract change, and then only with the justification in the commit body — an unjustified regeneration converts the net into a rubber stamp.
- R4 — the reader now needs the league KEY but its signature takes `league`. Deriving league_key <- paste0(league$sport, "_", league$country) happens to be correct for all three current leagues, but it is a convention, not a guarantee. Task 3 asserts it against names(load_leagues()) rather than trusting it; a future non-Iceland league would trip the assertion loudly instead of silently reading the wrong division list.
- R5 — tests that write into the real repo tree. publish_iceland_league derives round_predictions_history_root from dirname(output_root) (R/publish-football-iceland.R:713-718); a test that passes the package default output_root writes into the repo's data/beliefs/round_predictions_history/, which the launchd autoplace agent's background git stash/pull can then clobber or commit. Every new test in this workstream must pass a tempdir root and assert nothing lands outside it.
- R6 — ORDERING against WS11. .validate_or_abort (R/publish-pipeline.R:145-158) gates on dir.exists(schema_dir/<sport>) but then validates the WHOLE publish tree, so arming basketball schemas before task 7 makes the stale 2026-06-23 bb/hb JSON (no division, no is_cup) abort FOOTBALL's publish, and scripts/05_publish.R:34 calls publish_one bare with no tryCatch so the run dies before the commit step. Required order: WS11's subtree fix (or nothing) -> WS9 task 7 -> WS11's arming git mv.
- R7 — deliberate contract break on p_tie. The old bb/hb next_games emitted p_tie = P(exact tie) from continuous Student-t draws (R/publish-basketball-iceland.R:147). The unified contract emits p_draw, which is 0 for basketball by construction (has_ties FALSE) and threshold-based for handball. Any consumer reading p_tie breaks. Nothing on metill-platform reads the bb/hb paths today (its routes are football-only), so the break is free now and will not be later — WS13 must read p_draw.
- R8 — ID-1 leaves publish_iceland_league at ~1600 lines with the loop inside. That is a real, accepted cost: the file stays large and the loop body stays long. The alternative (spec §10f's per-division signature) costs up to 5 prepare_data() calls per cell and a 920-line code motion with no behavioural gain. Record the deferral so a later session does the decomposition deliberately, with the hoisted-ctx shape, rather than rediscovering it.
- R9 — bb/hb ship EMPTY standings/final_positions/points_distribution/team_strengths until WS8 adds the `division` payload column to the other four 2DT parquets. read_extracted_iceland returns profile$empty_extracts for any parquet lacking the column (the behaviour football already has). This is visible-but-harmless while nothing on the platform consumes the paths, but it means task 7's green does NOT prove the tables are right — only that the pipeline runs end to end without a fit RDS. Do not report parity until WS8 lands.
- R10 — scripts/03_fit.R:34-54 calls fit_one() with no tryCatch, and basketball and handball precede football in config order, so a 2DT diagnostics-gate breach still kills football's fits in the same run. WS9 does not touch this; WS12 owns it. Mentioned because a failing 2DT fit will present as 'the publisher broke' during this workstream when it is actually an upstream fit abort — check the fit run log before debugging the publisher.

---

# WS10 — meta.json v2 + next_games contract + D3 regular-season relabel

**Goal.** Make every published cell self-describing so no consumer does league arithmetic: meta.json gains units / points / n_rounds + n_rounds_source / a regular-season-correct round / season_scope / postseason / qualify / relegation; final_positions gains basis + p_qualify + p_top_of_table (p_winner only when basis=="final_table", p_top_six only as football's named alias); bb/hb next_games carries football's field names plus goal_diff_distribution, division_code and venue. The concrete blockers closed: "Umferðir eftir" can no longer render -13 (bb male BD standings.played 35 against meta.round 22), women's handball publishes its TRIPLE round robin (n_rounds 21, not 2*(8-1)=14), and no bb/hb payload can carry a top-six number under a playoff label or the word Íslandsmeistari. Football's other 8 artefacts stay byte-identical; exactly meta.json and final_positions.json change, and their golden hashes are regenerated once, deliberately, in a commit that says why.

**Consumes.**

- WS7: config/leagues.yml publish_divisions entries for basketball_iceland + handball_iceland carrying code, slug, label_is, is_cup and the new expected_meetings / qualify_slots / relegation_slots / code_badge keys, with config/leagues.schema.json::definitions.publishDivisionList.items.properties extended to permit them (the object is additionalProperties:false, so an unregistered key aborts load_leagues())
- WS7: .iceland_division_codes(key, sex) / .iceland_division_slugs(key, sex) / .iceland_division_labels(key, sex) / .iceland_division_split(key, sex), replacing .football_iceland_division_*(sex) at R/extract-football-iceland.R:40-100
- WS7 (NEW, may be absent — this workstream adds it if so): .iceland_division_config(key, sex) returning the raw publish_divisions entry list per division code, which is how qualify_slots / relegation_slots / expected_meetings / code_badge reach the publisher
- WS8: the 2DT extractor emitting all six parquets per (sex, fit_date) partition with a `division` payload column; predicted_matches.parquet already carries football's field names (mean_home_goals, mean_away_goals, mean_goal_diff, p_home_win, p_draw, p_away_win, goal_diff_distribution, division) — verified in the committed fixture tests/testthat/fixtures/extracts/sport=basketball/country=iceland/sex=male/fit_date=2100-01-01/predicted_matches.parquet
- WS9: read_extracted_iceland(league, sex, fit_date, extracts_root, target_divs, profile); sport_publish_profile(sport); publish_iceland_league(extracted, league, sex, division, profile, ...) — WS10 adds fields to the profile and calls the format helpers from inside publish_iceland_league
- WS2 harness (already merged): fixture_facts_root(), fixture_extracts_root(sports), build_football_extracts_fixture(facts_root, extracts_root, sex), publish_json_digest(path), FIXTURE_END_DATE = 2100-01-15, FIXTURE_FIT_DATE = 2100-01-01, tests/testthat/fixtures/golden/football-publish-hashes.csv (93 rows = 9 cells x 10 artefacts + header)
- Existing package code: write_json_consistent(x, path, ...) at R/storage.R:429 (defaults na="null"); .points_2dt() at R/publish-iceland-2dt-helpers.R:124; .compute_standings_rows_2dt() at :149; .compute_round_num_2dt() at :328 (deleted by this workstream); football's inline round computation at R/publish-football-iceland.R:933-941; football's meta list at :965-985; football's final_positions summary at R/publish-football-iceland.R:1417-1440; the 2DT equivalents at R/publish-basketball-iceland.R:113-126 and :258-275

**Produces.**

- R/publish-format.R (NEW) :: .publish_n_rounds(results, schedules, season, division_codes, end_date, expected_meetings = NULL, is_cup = FALSE) -> list(n_rounds = integer(1)|NA_integer_, source = one of "config"/"schedule"/"none"/"not_applicable", n_rounds_config = integer(1)|NA, n_rounds_schedule = integer(1)|NA, n_teams = integer(1))
- R/publish-format.R :: .publish_round(results, season, division_codes, n_rounds) -> integer(1), clamped to [0, n_rounds] when n_rounds is finite
- R/publish-format.R :: .regular_season_results(results, n_rounds) -> tibble; NA-safe (keeps rows where is.na(round)); identity when n_rounds is NA
- R/publish-format.R :: .build_publish_meta(base, profile, format, division_cfg) -> named list written verbatim by write_json_consistent(); key order = sport, sex, league, division, is_cup, season, generated_at, fit_date, round, n_draws, split (when present), n_rounds, n_rounds_source, units, points, season_scope, postseason, qualify, relegation
- R/publish-format.R :: .build_placement_summary(final_positions, n_teams, basis, qualify_slots, relegation_slots, emit_top_six_alias = FALSE) -> tibble(team, p_qualify, p_top_of_table, p_winner?, p_top_six?, p_relegation)
- sport_publish_profile(sport) gains: $points = list(win, draw, loss) (football 3/1/0, handball 2/1/0, basketball 2/NULL/0); $units = list(strength, home_advantage, diff_bin_width) (basketball points/points/5L, handball goals/goals/2L, football <verified string>/goal_multiplier/1L); $season_scope ("full_season" football, "regular_season" bb+hb); $postseason (NULL football, list(name_is = "Úrslitakeppni", modelled = FALSE) bb+hb); $placement_basis ("final_table" football, "regular_season_table" bb+hb)
- meta.json v2 keys, for ALL THREE sports: n_rounds (integer or null), n_rounds_source, units{strength,home_advantage,diff_bin_width}, points{win,draw,loss}, season_scope, postseason (object or null), qualify{slots,label_is}, relegation{slots}
- final_positions.json + points_distribution.json gain a required top-level `basis` string; final_positions.json summary gains p_qualify and p_top_of_table for every sport, keeps p_winner only when basis == "final_table", keeps p_top_six only for football (deprecated alias, removal commit named in the docstring)
- bb/hb next_games.json contract, identical in key set to football's: date, venue (null), division, division_code, home, away, mean_home_goals, mean_away_goals, mean_goal_diff, p_home_win, p_draw, p_away_win, goal_diff_distribution[{diff,p}]
- tests/testthat/fixtures/facts/playoff-overhang.parquet (NEW, committed) — the real basketball_iceland season-2026 BD male / BD female / 1D female rows, the three cells whose post-season is embedded in the league division; generated by tools/make-extract-fixtures.R::make_playoff_overhang_fixture()
- tests/testthat/test-publish-format.R, tests/testthat/test-publish-meta-contract.R, tests/testthat/test-publish-next-games-contract.R (NEW)
- config/publish-schemas/football/meta.schema.json + final_positions.schema.json extended with the v2 keys (WS11 folds these into its _base/_delta generator)
- Regenerated tests/testthat/fixtures/golden/football-publish-hashes.csv — exactly the meta.json and final_positions.json rows change

### Task 1: Format helpers: n_rounds, round and the regular-season row filter (pure, RED first)

**Files:**

- create R/publish-format.R
- create tests/testthat/test-publish-format.R
- create tests/testthat/fixtures/facts/playoff-overhang.parquet
- modify tools/make-extract-fixtures.R (add make_playoff_overhang_fixture())

- [ ] Generate the committed real-data fixture FIRST, because the RED test consumes it. Add make_playoff_overhang_fixture(dest = NULL) to tools/make-extract-fixtures.R: read data/facts/results with arrow::open_dataset(), filter country == "iceland", sport == "basketball", season == 2026L, division %in% c("BD", "1D"), collect(), and arrow::write_parquet() the 556 rows to tests/testthat/fixtures/facts/playoff-overhang.parquet. Do NOT anonymise team names. Wire it into make_extract_fixtures() alongside the existing generators, and run it: Rscript -e 'source("tools/make-extract-fixtures.R"); make_playoff_overhang_fixture()'. Confirm the file is under 30 KB and that nrow == 556 (162 + 159 + 137 + 98).

- [ ] Write tests/testthat/test-publish-format.R with NO skip()/skip_if()/Sys.getenv anywhere (the hygiene test at tests/testthat/test-fixture-skip-hygiene.R strips comments and greps). Three blocks. Block A drives the committed real-data fixture: for basketball male BD read the fixture parquet, call .publish_n_rounds(results, schedules = results[0, ], season = 2026L, division_codes = "BD", end_date = as.Date("2026-06-01"), expected_meetings = 2L) and expect $n_rounds == 22L, $source == "config", $n_teams == 12L; expect nrow(.regular_season_results(bd_male, 22L)) == 132L (30 post-season matches dropped); expect .publish_round(bd_male, 2026L, "BD", 22L) == 22L. Repeat for male 1D (n_rounds 22, regular 132 of 159, round 22), female BD (expected_meetings 2, n_teams 10, n_rounds 18, regular 90 of 137, round 18), and female 1D with expected_meetings = NULL (source "schedule", n_rounds 24, round 6 -- the deliberately irregular 11-team cell).

- [ ] Block B drives the synthetic facts fixture via fixture_facts_root(): assert the women's-handball triple round robin is honoured -- handball female OD in the fixture has 4 teams, so expected_meetings = 3L must give n_rounds 9 (NOT 2*(4-1) = 6) and round 3; handball male OD with expected_meetings = 2L gives 6 and 3. Assert football male BD (12 teams, 66 played rows, 3 forward schedule rows in the fixture, expected_meetings = NULL) resolves source == "schedule", n_rounds == 12L, round == 11L -- the value football's meta.json publishes today, which is what keeps the golden diff to two files. Assert explicitly that n_rounds != 92L, i.e. the derivation counts scheduled APPEARANCES and never reads schedules$round (the fixture stamps 90/91/92 to catch exactly that mistake).

- [ ] Block C covers the edges: is_cup = TRUE returns n_rounds NA_integer_ with source "not_applicable" and .publish_round() then falls back to min appearances over all rows (football CUP must still report round 1, matching data/publish/football/iceland/karla-bikar/meta.json today); zero results rows returns n_teams 0L, n_rounds NA, source "none", round 0L; .regular_season_results() KEEPS rows with is.na(round) (cup rows are NA by design -- see R/derive-round.R's cup carve-out) and is the identity function when n_rounds is NA.

- [ ] RUN IT: Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-publish-format.R")'. EXPECTED RED: every block errors with `Error in .publish_n_rounds(...): could not find function ".publish_n_rounds"` -- three erroring test_that blocks, zero passes. If instead a test passes, stop: a stale definition exists somewhere and must be found before implementing.

- [ ] Implement R/publish-format.R. .publish_n_rounds(): slice results to season + division_codes; slice schedules to season + division_codes + match_date > end_date; n_teams = number of distinct teams across both slices; when is_cup return the not_applicable shape immediately; n_rounds_config = as.integer(expected_meetings * (n_teams - 1)) when expected_meetings is non-NULL else NA_integer_; n_rounds_schedule = max over teams of (appearances in .regular_season_results(played, n_rounds_config) + appearances in the schedule slice), NA when both slices are empty; choose config when finite, else schedule when finite, else NA with source "none". Return all five components -- WS12's check_publish_format_agreement calls this same function and compares $n_rounds_config with $n_rounds_schedule. .publish_round(): min over the cell's teams of appearances in .regular_season_results(rows, n_rounds), 0L when no rows, clamped into [0, n_rounds] when n_rounds is finite. .regular_season_results(): identity when is.na(n_rounds), else keep rows where is.na(round) | round <= n_rounds. Roxygen every function @noRd with a one-line reason and cite the measured counts (132/162, 90/137, 21 = 3*(8-1)) so the next reader does not re-derive them.

- [ ] RUN AGAIN: same command; expect 0 failures. Then run the whole suite (Rscript -e 'devtools::test()') and confirm FAIL 0 -- .regular_season_results() is new code with no callers yet, so nothing else may move.

- [ ] Commit: `feat(publish): derive n_rounds, round and the regular-season boundary upstream`. Body states the config-beats-schedule inversion and why (bb male BD played 35 vs a 22-round regular season), and the triple-round-robin case. No git push.

**Verification.** The three basketball cells whose post-season is embedded in the league division are cut to their exact regular season from committed real data (132 of 162, 132 of 159, 90 of 137 matches), women's handball resolves 3*(n-1) rather than 2*(n-1), and football male BD still resolves round 11 on the fixture -- the number its meta.json publishes today.

### Task 2: Profile constants: points, units and one source of truth for the goal-diff bin width

**Files:**

- modify R/publish-pipeline.R (or wherever WS9 placed sport_publish_profile())
- modify R/extract-basketball-iceland.R:61
- modify R/extract-handball-iceland.R:46
- create tests/testthat/test-publish-profile-units.R

- [ ] Before writing anything, VERIFY football's strength scale rather than guessing: read the team_strengths_quantiles block in R/extract-football-iceland.R (the home_advantage block at :276-283 documents itself as exp(home_advantage_*) with `total` halved, and the roxygen at :1460-1462 confirms it). Decide $units$strength for football from what the extractor actually stores -- "log_goals" if the quantile band is on the raw log-rate, "goals" if it is exponentiated -- and put the file:line evidence in the commit message. basketball is "points" and handball is "goals" per .compute_home_advantage_quantiles_2dt()'s post-B5 comment at R/extract-iceland-2dt-shared.R:188-207.

- [ ] Write tests/testthat/test-publish-profile-units.R. Assert sport_publish_profile("basketball")$points is list(win = 2L, draw = NULL, loss = 0L) and handball 2/1/0 and football 3/1/0 -- cross-check against .points_2dt() at R/publish-iceland-2dt-helpers.R:132-142, which awards 2 for a win, 1 for a tie when has_ties, 0 otherwise, and against config/leagues.yml betting.scoring.has_ties (basketball false at :83, handball true at :130, football true at :354). Assert $units$diff_bin_width is 5L basketball, 2L handball, 1L football. Assert $season_scope, $postseason and $placement_basis take the values in Produces. Then assert the single-source-of-truth property: the committed extracts fixture's goal-diff bins are multiples of the profile width -- read tests/testthat/fixtures/extracts/sport=basketball/.../predicted_matches.parquet, unnest goal_diff_distribution, and expect all(diff %% 5 == 0); the same for handball with 2.

- [ ] RUN IT. EXPECTED RED: `Error ... $ operator is invalid for atomic vectors` or `Error: sport_publish_profile("basketball")$points is NULL` depending on WS9's current profile shape -- the point is that $points / $units / $season_scope / $postseason / $placement_basis do not exist yet. Record the exact message.

- [ ] Add the five fields to sport_publish_profile(). Then repoint the two hardcoded bin widths -- R/extract-basketball-iceland.R:61 `bucket_width = 5L` and R/extract-handball-iceland.R:46 `bucket_width = 2L` -- at sport_publish_profile(<sport>)$units$diff_bin_width, so meta.units cannot drift from the bins the extractor actually wrote.

- [ ] RUN AGAIN: expect green. Then prove the repoint is value-preserving: regenerate the committed 2DT extracts fixture (Rscript -e 'source("tools/make-extract-fixtures.R"); make_extract_fixtures()') and confirm `git status --short tests/testthat/fixtures/extracts/` reports NO modified parquets. If any parquet moves, the widths changed and the repoint is wrong.

- [ ] Commit: `feat(publish): put points, units and the goal-diff bin width in the sport profile`.

**Verification.** meta.units.diff_bin_width is provably the number the extractor binned with (5 stig / 2 mörk), the fixture parquets are byte-unchanged by the repoint, and the points scheme is asserted against the two places it is actually implemented (.points_2dt and betting.scoring.has_ties) rather than restated by hand.

### Task 3: meta.json v2 assembly (pure builder, RED first)

**Files:**

- modify R/publish-format.R
- modify tests/testthat/test-publish-format.R

- [ ] Extend tests/testthat/test-publish-format.R with a .build_publish_meta() block. Build `base` as the exact 10-key list publish_football_iceland writes today (R/publish-football-iceland.R:965-976: sport, sex, league, division, is_cup, season, generated_at, fit_date, round, n_draws) plus a `split` element, call .build_publish_meta(base, profile = sport_publish_profile("football"), format = <a .publish_n_rounds() result>, division_cfg = list(qualify_slots = 6L, relegation_slots = 2L, qualify_label_is = "Í toppbaráttu")), and assert names() is EXACTLY sport, sex, league, division, is_cup, season, generated_at, fit_date, round, n_draws, split, n_rounds, n_rounds_source, units, points, season_scope, postseason, qualify, relegation -- in that order. Key order is asserted because publish_json_digest() hashes jsonlite::toJSON() of the parsed list, so order is part of the payload identity.

- [ ] Assert the same call with is_cup = TRUE / a not_applicable format writes n_rounds = NULL (serialised as null by write_json_consistent's na = "null" default, R/storage.R:429-432) and n_rounds_source = "not_applicable", and that `split` is absent when base carries none rather than present-and-null.

- [ ] Assert the sport-specific shapes: football gets season_scope "full_season" and postseason NULL; basketball and handball get "regular_season" and list(name_is = "Úrslitakeppni", modelled = FALSE). Write the Icelandic literals as \uXXXX escapes -- do not paste raw non-ASCII through the Edit tool; if you need the literal characters, write the file with a python heredoc or cat <<'EOF'.

- [ ] Assert the invariant that the whole workstream exists to guarantee: for every combination in the block, isTRUE(is.na(meta$n_rounds)) || meta$n_rounds >= meta$round.

- [ ] RUN IT. EXPECTED RED: `could not find function ".build_publish_meta"`.

- [ ] Implement .build_publish_meta(base, profile, format, division_cfg) in R/publish-format.R: copy `base` verbatim (never re-order or rename an existing key), then append n_rounds, n_rounds_source, units = profile$units, points = profile$points, season_scope = profile$season_scope, postseason = profile$postseason, qualify = list(slots = division_cfg$qualify_slots, label_is = division_cfg$qualify_label_is), relegation = list(slots = division_cfg$relegation_slots %||% NA_integer_). Guard with stopifnot() that n_rounds >= round whenever both are finite, and abort with a cli message naming the cell if not -- a silent negative Umferðir-eftir is the exact defect this workstream removes, so the producer refuses to write it.

- [ ] RUN AGAIN green, then devtools::test() FAIL 0 (the builder still has no callers). Commit: `feat(publish): assemble meta.json v2`.

**Verification.** The builder refuses to emit a payload whose n_rounds is below its round, the key order is pinned so a future re-order shows up as a golden-hash change rather than silently, and the cup shape (null n_rounds) is covered before any publisher calls it.

### Task 4: final_positions: basis, p_qualify, p_top_of_table, and retiring p_top_six for bb/hb

**Files:**

- modify R/publish-format.R
- modify tests/testthat/test-publish-format.R
- modify config/leagues.yml (only if WS7 left football without qualify_slots/relegation_slots)

- [ ] First check the config seam: Rscript -e 'devtools::load_all(); str(.iceland_division_config("football_iceland", "male"))'. Every non-cup football division must carry qualify_slots: 6 and relegation_slots: 2, or p_qualify stops equalling football's p_top_six and p_relegation stops equalling the literal placement >= n_teams - 1 at R/publish-football-iceland.R:1422-1424. If they are absent, add them to config/leagues.yml::football_iceland.publish_divisions (both sexes, BD/LD1/LD2/LD3, CUP excluded) together with qualify_label_is: "Í toppbaráttu", and run load_leagues() to prove the schema accepts them -- config/leagues.schema.json's publishDivisionList.items is additionalProperties:false, so an unregistered key takes every script down at load. Use a python heredoc for the Icelandic string.

- [ ] Add a .build_placement_summary() block to tests/testthat/test-publish-format.R. Build a synthetic final_positions tibble (6 teams x 6 placements, probabilities summing to 1 per team). For basis = "final_table", qualify_slots = 6L, relegation_slots = 2L, emit_top_six_alias = TRUE (football): assert columns team, p_qualify, p_top_of_table, p_winner, p_top_six, p_relegation; assert p_top_six == p_qualify to 1e-12 (the alias identity); assert p_top_of_table == p_winner to 1e-12; assert p_relegation equals sum(probability[placement >= n_teams - 1]) -- byte-for-byte the current football expression when relegation_slots is 2. For basis = "regular_season_table", qualify_slots = 8L, relegation_slots = 2L, emit_top_six_alias = FALSE (basketball BD): assert p_winner and p_top_six are ABSENT from names(), p_qualify == sum(probability[placement <= 8]), and p_top_of_table == sum(probability[placement == 1]). For relegation_slots = 0L (handball G66): assert p_relegation is present and identically 0 for every team -- present-and-zero, never a missing key, so no consumer needs a null branch.

- [ ] RUN IT. EXPECTED RED: `could not find function ".build_placement_summary"`.

- [ ] Implement .build_placement_summary(final_positions, n_teams, basis, qualify_slots, relegation_slots, emit_top_six_alias = FALSE) in R/publish-format.R, summarising per team from the `probability` column exactly as football does at R/publish-football-iceland.R:1420-1427 (sum over placements, not mean over draws -- the 2DT publisher's mean-over-iterations form at R/publish-basketball-iceland.R:258-263 dies with publish_basketball_iceland in WS9). p_relegation: when relegation_slots is NA use the legacy placement >= n_teams - 1 expression verbatim; when 0 emit zeros; otherwise placement > n_teams - relegation_slots. Return an empty 0-row tibble with the right columns when final_positions has no rows (football CUP).

- [ ] RUN AGAIN green. Commit: `feat(publish): generic p_qualify replaces the top-six label`. The roxygen for the p_top_six alias must name its removal: "deprecated alias, football only; removed in the follow-up commit whose only job is that removal, after metill-platform reads p_qualify".

**Verification.** The alias identity p_top_six == p_qualify is asserted rather than assumed, so football's published number is provably unchanged; the bb/hb branch cannot emit a top-six number or a champion probability at all; and a zero-relegation division still publishes a well-formed key.

### Task 5: Wire meta v2 + the regular-season boundary into publish_iceland_league

**Files:**

- modify R/publish-iceland-league.R (WS9's unified publisher; whichever file WS9 put publish_iceland_league() in)
- modify R/publish-iceland-2dt-helpers.R (delete .compute_round_num_2dt at :328)
- create tests/testthat/test-publish-meta-contract.R

- [ ] Write tests/testthat/test-publish-meta-contract.R. No skip gates. It publishes every cell from fixtures into a tempdir with validate = FALSE and then asserts on the JSON. bb/hb: extracts <- fixture_extracts_root(); facts <- fixture_facts_root(); for each of the 8 cells call read_extracted_iceland() + publish_iceland_league(..., end_date = FIXTURE_END_DATE, root = facts). football: build_football_extracts_fixture() for both sexes, then the 9 cells.

- [ ] Assert, for all 17 cells: meta.json parses; n_rounds is null or >= round; round <= max(standings.played); every standings row satisfies played == wins + draws + losses and points == wins*meta.points.win + draws*(meta.points.draw %||% 0) + losses*meta.points.loss -- this is the assertion that catches a mislabelled scoring scheme in any sport, and it must hold for football's 3/1/0 too. Assert meta.units, meta.season_scope, meta.postseason, meta.qualify.slots and meta.relegation.slots are present with the profile's values.

- [ ] Assert the bb/hb-only D3 properties: meta.season_scope == "regular_season"; meta.postseason$modelled is FALSE; final_positions.json$basis == "regular_season_table"; "p_winner" and "p_top_six" appear in no bb/hb payload (recurse the parsed JSON, checking KEY names, not string values); and grepl("Íslandsmeistar", raw_text, fixed = TRUE) is FALSE for every JSON file under the basketball/ and handball/ trees.

- [ ] Assert the concrete blocker on real data, using the committed playoff-overhang fixture rather than a fit: write it into a temp facts root, call the same .publish_n_rounds() / .regular_season_results() pair the publisher now uses for basketball male BD 2026 with expected_meetings = 2L, and assert max(played) over teams is 22 (not 35) and n_rounds - round == 0 (not -13).

- [ ] RUN IT. EXPECTED RED: the meta assertions error with `Error: meta$n_rounds is NULL` / `subscript out of bounds` for every cell (publish_iceland_league still writes the 10-key v1 meta), and the bb standings assertion fails because played counts post-season rows. Record the exact first failure line.

- [ ] Wire it. In publish_iceland_league(): resolve division_cfg from .iceland_division_config(key, sex)[[division]]; call format <- .publish_n_rounds(results, schedules, current_season, family_divs, end_date, expected_meetings = division_cfg$expected_meetings, is_cup = is_cup); replace the inline round computation at R/publish-football-iceland.R:933-941 with .publish_round(results, current_season, family_divs, format$n_rounds); filter the per-division results slice that feeds standings, base points and the as_of stamp through .regular_season_results(..., format$n_rounds); build meta with .build_publish_meta(); build the final_positions summary with .build_placement_summary(..., basis = profile$placement_basis, emit_top_six_alias = identical(league$sport, "football")) and add the top-level `basis` key to final_positions.json AND points_distribution.json. Read schedules once per (league, sex) with read_table("schedules", root = root, filter = list(sport, country, sex)) alongside the existing results read.

- [ ] Delete .compute_round_num_2dt() (R/publish-iceland-2dt-helpers.R:328-338) and grep the tree for surviving references: `grep -rn 'compute_round_num_2dt' R/ tests/ scripts/` must return nothing.

- [ ] RUN AGAIN: test-publish-meta-contract.R green. Then devtools::test() -- expect test-publish-football-golden.R to FAIL and NOTHING ELSE. Do not fix it here; task 6 owns it. Commit: `feat(publish): meta.json v2 and the regular-season boundary in the unified publisher`, noting in the body that the football golden is red until the next commit.

**Verification.** Every published cell across all three sports satisfies played == W+D+L and points == the scheme meta itself declares; bb male BD's regular-season standings top out at 22 played against n_rounds 22, so Umferðir eftir is 0 rather than -13; and the word Íslandsmeistari cannot reach a bb/hb payload.

### Task 6: Football regression: prove exactly two artefacts moved, then regenerate the golden manifest

**Files:**

- modify tests/testthat/fixtures/golden/football-publish-hashes.csv
- modify tests/testthat/test-publish-football-golden.R

- [ ] Run the golden test alone and CAPTURE the changed list: Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-publish-football-golden.R")'. The test's failure info line is `changed payloads: <comma-separated relative paths>` (tests/testthat/test-publish-football-golden.R:44-47). EXPECTED RED: that list is non-empty and every entry ends in /meta.json or /final_positions.json.

- [ ] Before regenerating anything, assert the containment mechanically. Add a second test_that block to test-publish-football-golden.R that publishes the same pinned fixture, hashes the produced files, and expects setdiff(basename(changed), c("meta.json", "final_positions.json")) to be empty -- so a future edit that perturbs standings.json or next_games.json is caught by a named assertion instead of being absorbed into a regeneration. Run it and confirm it PASSES against the current red state. If it does not -- if standings.json, next_games.json, team_strengths.json, home_advantage.json, points_distribution.json, standings_history.json, final_positions_history.json, team_strengths_history.json or tournament_placements.json moved -- STOP and fix the task-5 wiring; that is a real football regression, most likely .regular_season_results() dropping rows it should keep (check for NA rounds and for a football n_rounds below max(round)).

- [ ] Diff one cell by eye before trusting the hashes: publish football male BD from the fixture into a tempdir and diff its meta.json against data/publish/football/iceland/karla-bd/meta.json. Confirm the first 10 keys and their values are unchanged (round must still be 11 on the fixture / 21 on real data) and that only the v2 keys are new.

- [ ] Regenerate: Rscript -e 'source("tools/make-extract-fixtures.R"); make_football_golden_hashes()'. Confirm the message reports the same payload count as before (93 rows minus the header = 92 files) and that `git diff --stat` on the CSV shows exactly 18 changed lines (9 meta.json + 9 final_positions.json) -- if a cell's final_positions.json did not move, say which and why in the commit body (football CUP publishes an empty final_positions with no summary rows, so its `basis` addition still changes the hash; verify rather than assume).

- [ ] RUN: devtools::test(). Expect FAIL 0 and a SKIP count no higher than the 45 recorded at the end of Plan A.

- [ ] Commit: `test(publish): regenerate the football golden hashes for meta.json v2`. The body must state the justification explicitly -- which two artefacts changed, that the other eight are asserted byte-identical by the new containment block, and that the change is purely additive keys (list them) with no existing value altered. This is the deliberate regeneration the plan allows; a commit that regenerates without that paragraph should be rejected in review.

**Verification.** The football regression net is not merely re-baselined -- a named assertion now pins the blast radius to meta.json and final_positions.json permanently, and the eight untouched artefacts remain hash-locked to the pre-WS10 bytes.

### Task 7: next_games: one contract for all three sports

**Files:**

- modify R/publish-iceland-league.R
- create tests/testthat/test-publish-next-games-contract.R

- [ ] Write tests/testthat/test-publish-next-games-contract.R. Publish all 17 cells from fixtures as in task 5, then for every next_games.json assert the per-match key set is EXACTLY date, venue, division, division_code, home, away, mean_home_goals, mean_away_goals, mean_goal_diff, p_home_win, p_draw, p_away_win, goal_diff_distribution -- expect_setequal on names(), so both a missing key and a surviving legacy key fail. Assert the legacy 2DT names mean_home, mean_away, mean_diff, p_home, p_away, p_tie appear nowhere in any payload. Assert goal_diff_distribution is a non-empty array of {diff, p} whose p sums to 1 within 1e-6 and whose diff values are all multiples of meta.units.diff_bin_width (5 for basketball, 2 for handball, 1 for football) -- this is the cross-file consistency check that makes the units claim falsifiable. Assert venue is present for every match and null for every bb/hb match. Assert division_code equals the configured code_badge for that division.

- [ ] The fixture's upcoming matches are dated 2100-01-16..2100-01-20 against FIXTURE_END_DATE 2100-01-15, so the 14-day window filter admits them and the time-bomb rule is satisfied with no Sys.Date() arithmetic. Do not add any date literal near today.

- [ ] RUN IT. EXPECTED RED: expect_setequal on the bb/hb cells reports `Missing: division_code, venue` (WS9's publisher passes ext$predicted_matches straight through, and the committed fixture predicted_matches.parquet carries game_nr, match_date, home_team, away_team, division, mean_* , p_*, goal_diff_distribution but no venue or badge). If the run comes back GREEN on the first try that is a FAILED RED, not a pass: WS9 already satisfied the contract. In that case say so in the commit message with the evidence line, keep the test as the contract lock, and skip the implementation step -- do not invent a change to manufacture a red.

- [ ] Implement in publish_iceland_league(): select and rename from ext$predicted_matches into the exact contract, adding date = format(match_date, "%Y-%m-%d"), division_code from the division's configured code_badge, and venue from the football venue lookup for football / NA_character_ for bb/hb. Keep football's existing column order (R/publish-football-iceland.R:1048-1054) so football's next_games.json hash does not move.

- [ ] RUN AGAIN green, then devtools::test() and confirm the golden test is still green -- next_games.json is one of the eight artefacts task 6 pinned, so any movement here is a bug.

- [ ] Commit: `feat(publish): one next_games contract across football, basketball and handball`.

**Verification.** The platform's next-games-grid.js field names and its goal-diff fixture strip are satisfied by the producer for all three sports, with the bin width cross-checked against meta rather than trusted; and football's next_games.json is proven unmoved by the golden test.

### Task 8: Publish schemas for the v2 keys, and the 1D pattern hazard handed to WS11

**Files:**

- modify config/publish-schemas/football/meta.schema.json
- modify config/publish-schemas/football/final_positions.schema.json
- modify config/publish-schemas/football/points_distribution.schema.json
- modify docs/superpowers/specs/2026-09-02-basketball-handball-metill-parity-design.md (a short WS11 note) or the WS11 plan file, whichever exists

- [ ] Write the schema assertions first, in tests/testthat/test-publish-meta-contract.R: publish the 9 football cells from fixtures and run them through the same validator publish_one() uses (validate_publish_dir(output_root, sport = "football")), expecting zero errors. RUN IT. EXPECTED RED: the run passes today because meta.schema.json has no additionalProperties:false -- so instead make the RED real by first adding the v2 keys to the schema's `required` array and running the validator against the PRE-task-5 published tree at data/publish/football/iceland/ (which still lacks them); expect an error naming n_rounds as a missing required property. Record it, then validate the freshly published tree and expect clean.

- [ ] Edit config/publish-schemas/football/meta.schema.json: add n_rounds ({"type": ["integer", "null"], "minimum": 1}), n_rounds_source ({"enum": ["config", "schedule", "none", "not_applicable"]}), units (object: strength string, home_advantage string, diff_bin_width integer minimum 1), points (object: win integer, draw ["integer","null"], loss integer), season_scope ({"enum": ["full_season", "regular_season"]}), postseason ({"type": ["object", "null"]} with name_is + modelled), qualify (object: slots integer, label_is string), relegation (object: slots ["integer","null"]); add all of them to `required` except postseason, which is null for football. Update the schema's description line, which currently says "Schema v1 -- additive changes only without bumping", to v2 with the date.

- [ ] Edit final_positions.schema.json: add top-level `basis` ({"enum": ["final_table", "regular_season_table"]}) to properties and to `required`; add p_qualify and p_top_of_table to the summary item's properties and `required`; move p_winner and p_top_six to optional (they are football-only from here on) and note in the description that p_top_six is a deprecated alias. Add the same top-level `basis` to points_distribution.schema.json.

- [ ] Record the WS11 hazard where WS11's drafter will read it: config/publish-schemas/football/meta.schema.json constrains division to "^[A-Z][A-Z0-9_]*$", which REJECTS basketball's 1D division code (leading digit). WS11's _base must relax it to ^[A-Z0-9][A-Z0-9_]*$ before arming basketball validation, or every 1D cell aborts publish_one(). Also note that final_positions.schema.json's summary currently requires p_top_six and p_winner, which no bb/hb payload emits -- the _delta for basketball and handball must drop both from `required`.

- [ ] RUN: devtools::test() FAIL 0, and separately Rscript -e 'devtools::load_all(); scripts_out <- validate_publish_dir(here::here("data","publish"), sport = "football"); print(scripts_out)' against the REAL published tree to see what the next cron publish will face -- the tree on disk is still v1, so errors are expected and prove the schema is armed; state in the commit that the first post-merge publish run rewrites it.

- [ ] Commit: `feat(publish): schema v2 for meta, final_positions and points_distribution`.

**Verification.** The v2 keys are enforced rather than merely emitted, the deprecated football-only aliases are marked optional so the bb/hb delta is a subtraction and not a fork, and the 1D pattern rejection is written down before WS11 arms basketball validation inside football's publish call.

**Risks.**

- THE DESIGN'S OWN n_rounds RULE IS WRONG FOR BASKETBALL AND THIS PLAN DELIBERATELY DEVIATES. Spec §12 says "derived as max over teams of played + remaining_scheduled ... fall back to expected_meetings * (n_teams - 1)". Measured on data/facts/results season 2026: bb male BD max played = 35 against a 22-round regular season, bb female BD 34 against 18. Publishing 35 would contradict final_positions.basis == "regular_season_table" and leave the ticker at 35/35. This plan inverts the precedence: expected_meetings (config) WINS when configured, and the schedule derivation is the fallback for cells with no configured meetings. Verified this recovers the exact regular season for 7 of 8 bb/hb cells (round <= 2*(n-1) keeps 132/162, 132/159, 90/137 matches; reg_min == reg_max == n_rounds_config in all seven). Both values are still returned by .publish_n_rounds() so WS12's check_publish_format_agreement can WARN on disagreement exactly as specified. The parent must reconcile this with WS12's draft.
- THE DESIGN'S WS10 VERIFICATION LINE "assert meta.n_rounds == 44 (4 meetings x 11 opponents)" IS WRONG. It contradicts §"the boundary table" in the same document ("meta.round = 22 for basketball male BD is correct -- it equals the regular-season length") and the measured data (12 teams, 162 matches, 2 meetings). The correct number is 22. Do not hard-code 44 anywhere. WS13's "the ticker reads 35/44" inherits the same error; after this workstream bb male BD reads 22/22 because standings are tabulated on regular-season rows only.
- Football's meta.json and final_positions.json CANNOT stay byte-identical -- the v2 keys are additive by construction. The golden manifest must be regenerated. Task 6 makes this safe by asserting the changed set contains only those two basenames; if any of the other eight artefacts changes hash, the wiring has a bug and the regeneration must not proceed.
- config/publish-schemas/football/meta.schema.json:15 constrains division to ^[A-Z][A-Z0-9_]*$. Basketball's 1D division code does NOT match (leading digit). WS11 must relax the pattern in _base (e.g. ^[A-Z0-9][A-Z0-9_]*$) before arming basketball validation, or every 1D cell aborts. Recorded here because WS10 is what first writes `division` into a bb meta.json.
- p_top_of_table is not named in the design. It exists because §15 drops p_winner for bb/hb while the same section's stateline ("{lið} á mestar líkur á að vinna deildarkeppnina") and the "Efst spáð" fact both need P(placement == 1). Emitting it under a name that cannot be misread as Íslandsmeistari is cheaper than making the platform re-derive it from `records`. WS13's drafter must be told to read p_top_of_table; for football it equals p_winner exactly (asserted).
- qualify_slots must be 6 for every non-cup FOOTBALL division or p_qualify stops equalling p_top_six and the alias becomes a lie. If WS7 did not configure football, task 4 adds it. Likewise relegation_slots: when it is absent the code must keep the literal placement >= n_teams - 1 expression, or football's p_relegation changes and the golden diff grows past the two expected files.
- bb/hb schedules on disk are stale post-season leftovers (season 2026, 46 rows, all April-May 2026) and there are no forward fixtures yet, so the schedule branch of .publish_n_rounds() is UNEXERCISED by real bb/hb data today. Its coverage comes from the fixture tree (which has 3 forward schedule rows per cell, round stamped 90/91/92 on purpose) and from bb female 1D. Never derive n_rounds from max(schedules$round) -- the fixture would give 92.
- This workstream lands AFTER WS7, WS8 and WS9 on the same branch. Every wiring task names publish_iceland_league() / sport_publish_profile(); if WS9's signature has drifted, adapt the call sites but do not fork the helpers -- there must be exactly one .publish_n_rounds() and one .build_publish_meta() in the tree.

---

# WS11 — Schema generation, subtree validation, and arming (spec §11, with §14 correction 5)

**Goal.** Close both fail-open ends of the publish contract without ever letting one sport's schemas abort another's publish. Three things, in this order: (1) fix the verified arming hazard so `.validate_or_abort()` validates only its own sport's subtree, and give it a `schema_dir` parameter so the `_draft` workflow can be exercised through the publisher; (2) replace hand-written per-sport schemas with a single generated source (`config/publish-schemas/_base/` + `_delta/<sport>/` rendered by `tools/gen-publish-schemas.R`), so football's strictness can never be silently relaxed by promoting a file to a shared root; (3) author basketball/handball schemas under `config/publish-schemas/_draft/` — which resolves in NEITHER validator — and arm them with a single `git mv` only after the stale June JSON is gone and conforming JSON exists, then invert the missing-schema default from an informational skip to an abort. The same files rsync to metill-platform as `data/ithrottir-schemas/`, so arming turns the Python validator fail-closed in the same motion; the sequencing below is what stops that from freezing the site.

**Consumes.**

- WS9: sport_publish_profile(sport)$surfaces — the delta-directory-equals-surfaces test (task 5 step 6) is the seam that keeps the two from drifting.
- WS9: publish_one()'s single extracts path (no fit_path fallback), so task 5 can publish 8 bb/hb cells from the committed fixtures with no Stan and no fit RDS.
- WS10: the exact meta.json v2 key set (units{strength,home_advantage,diff_bin_width}, points{win,draw,loss}, n_rounds, n_rounds_source in {schedule,config}, corrected round, season_scope in {full_season,regular_season}, postseason, qualify{slots,label_is}) and the required `basis` in {final_table, regular_season_table} plus summary[].p_qualify on final_positions/points_distribution. Task 4 MUST NOT land before WS10 — a schema requiring a key the publisher does not emit turns the next football publish into an abort.
- WS10: the {sex}-{slug} publish path shape for bb/hb — task 6's deletion of the old un-suffixed cells assumes the new cells exist (or will) at data/publish/<sport>/iceland/{karla,kvenna}-{slug}/.
- WS12: the per-cell tryCatch around publish_one() in scripts/05_publish.R. Task 8 MUST NOT land before it — scripts/05_publish.R:34 calls publish_one() bare in a loop, so a fail-closed default without the guard converts one bad cell into a football outage.
- WS2 (Plan A, merged): fixture_facts_root(env) at tests/testthat/helper-fixture-facts.R:37 (returns a temp root with facts/ written via write_table); the committed 2DT extracts tree at tests/testthat/fixtures/extracts/sport={basketball,handball}/country=iceland/sex={male,female}/fit_date=2100-01-01/; publish_json_digest(path) at tests/testthat/helper-extract-fixtures.R:118; the football golden file at tests/testthat/fixtures/golden/football-publish-hashes.csv (92 hashes).
- Plan A (merged): tools/make-extract-fixtures.R:46-53 — the .fixture_gen_pkg_root() self-execution guard pattern that tools/gen-publish-schemas.R copies.
- metill-platform (read-only, NO code change in this workstream): scripts/validate_publish.py::resolve_schema_path (:46-68) and .github/workflows/pull-sports-data.yml:79-116 (one sparse clone at one SHA, rsyncs data/publish -> data/ithrottir and config/publish-schemas -> data/ithrottir-schemas, both with --delete, 7x/day at 25 7-12,19 UTC).

**Produces.**

- validate_publish_dir(dir, schema_dir = here::here("config", "publish-schemas"), sport = NULL) -> list(ok, n_files, n_passed, n_failed, errors, unmatched)  # exported, R/validate-publish.R. NEW 3rd arg: sport = NULL keeps today's first-path-segment derivation (used by the existing whole-tree callers at tests/testthat/test-publish-schemas.R:11 and test-publish-football-split.R:134); a non-NULL sport is used for EVERY file's schema lookup, which is what makes `dir` safe to point at a sport subtree.
- .validate_or_abort(output_root, sport, key, sex, schema_dir = here::here("config", "publish-schemas")) -> invisible(NULL)  # @noRd, R/publish-pipeline.R:145. NEW 5th arg. Validates file.path(output_root, sport) only. ABORTS (from task 8) when config/publish-schemas/<sport>/ is absent or the schema root is absent.
- publish_one(static, betting, key, sex, root = here::here("data"), validate = TRUE, schema_dir = here::here("config", "publish-schemas")) -> invisible(NULL)  # exported. NEW 7th arg, defaulted and last, so scripts/05_publish.R:34's positional 4-arg call is unchanged. `validate = FALSE` remains the escape hatch for synthetic-data tests.
- gen_publish_schemas(source_dir, out_dir, sports = NULL) -> character()  # top-level in tools/gen-publish-schemas.R, side-effect-free at source time (sys.source()-able, per tools/make-extract-fixtures.R's guard pattern). Renders _base + _delta/<sport> into <out_dir>/<sport>/<name>.schema.json.
- .merge_patch(base, patch) -> list  # in tools/gen-publish-schemas.R. RFC 7386 JSON Merge Patch: recursive object merge, `null` deletes a key, arrays and scalars replace WHOLESALE (so a delta touching `required` must restate the full array).
- config/publish-schemas/_base/<name>.schema.json  # 11 shared shapes: meta, next_games, standings, standings_history, team_strengths, team_strengths_history, final_positions, final_positions_history, home_advantage, points_distribution, tournament_placements.
- config/publish-schemas/_delta/<sport>/<name>.json  # RFC-7386 patch. Its EXISTENCE is the manifest: a sport emits exactly the surfaces it has delta files for. football = 11, basketball = 10, handball = 10 (no tournament_placements — no cup division is ingested for either sport). An empty {} is legal.
- config/publish-schemas/{football,basketball,handball}/<name>.schema.json  # generated + committed; the ONLY files either validator resolves.
- INVARIANT (tested): config/publish-schemas/ holds no root-level *.schema.json. A file there becomes the sport-agnostic fallback for EVERY sport including world_cup, on both the R and the Python side.
- INVARIANT (tested + enforced by the generator): every file under config/publish-schemas/ is pure ASCII. jsonlite::toJSON() renders a UTF-8 em-dash as the literal 7-char string `<U+2014>` even when Encoding() is "UTF-8" — VERIFIED — which would silently corrupt any description the generator round-trips.
- INVARIANT (tested): data/publish/{basketball,handball}/iceland/ contains only directories matching ^(karla|kvenna)-[a-z0-9]+$.
- tests/testthat/{test-publish-schema-arming.R, test-publish-schema-generation.R, test-publish-schema-2dt.R, test-publish-legacy-cells.R}  # no skip()/skip_if()/Sys.getenv anywhere.

### WS10 handoff, measured 2026-09-04 — the exact v2 shapes `_base` must carry

WS10 tasks 3-7 have landed. Task 8 was NOT executed as drafted: per SC-8 it
must not hand-edit `config/publish-schemas/football/`, and WS11 task 3 (the
generator plus `_base`/`_delta`) has not landed yet, so there is nothing to
edit. This section is task 8's deliverable — the specification WS11 task 3
folds into `_base`, and WS11 task 5 subtracts from in the bb/hb deltas.

**Nothing is broken while this is pending.** Verified on freshly published
football output from the pinned fixture: `validate_publish_dir(<out>/football,
sport = "football")` returns `ok = TRUE, n_files = 92, n_errors = 0,
unmatched = 0`. No schema in `config/publish-schemas/football/` sets
`additionalProperties: false`, so the v2 keys are accepted as unknown-but-
permitted and the arming order is unaffected.

**Two corrections to WS11's own Consumes bullet, which is wrong as written.**

1. `n_rounds_source` is NOT `{schedule, config}`. It has FOUR values —
   `["config", "schedule", "none", "not_applicable"]` — and all four are
   emitted today (football's two bikar cells publish `not_applicable`). A
   two-value enum in `_base` rejects them.
2. `p_qualify` is NOT on `points_distribution`, and it is not on every
   `final_positions` either. It is emitted only where the division configures
   a `qualify` cut, which today is football Besta deild (both sexes) alone. It
   must be OPTIONAL in `_base`.

**`_base/meta.schema.json`** — add to `properties`, and to `required` except
where noted:

| key | shape |
|---|---|
| `n_rounds` | `{"type": ["integer", "null"], "minimum": 1}` — null on a cup |
| `n_rounds_source` | `{"enum": ["config", "schedule", "none", "not_applicable"]}` |
| `units` | object, required `strength`, `home_advantage` (strings), `diff_bin_width` (integer >= 1) |
| `points` | object, required `win` (integer), `draw` (`["integer","null"]` — basketball is null), `loss` (integer) |
| `season_scope` | `{"enum": ["full_season", "regular_season"]}` |
| `postseason` | `{"type": ["object", "null"]}` with `name_is` (string) + `modelled` (boolean). NOT in `required` — it is null for football |
| `qualify` | `{"type": ["object", "null"]}` with `slots` (integer) + `label_is` (string). IN `required`, value nullable |
| `relegation` | object, required `slots` (`["integer","null"]`; null everywhere today — no division configures `relegation_slots`) |

The `division` pattern is already relaxed to `^[A-Z0-9][A-Z0-9_]*$` for
basketball's `1D`; that stands (SC-10).

**`_base/final_positions.schema.json`**:

- add top-level `basis` `{"enum": ["final_table", "regular_season_table"]}`,
  and to `required`.
- `summary[]` items: add `p_top_of_table` (number 0-1) to `properties` AND to
  `required` — every sport emits it. Add `p_qualify` (number 0-1) to
  `properties` only, NOT to `required`.
- MOVE `p_winner` and `p_top_six` OUT of `required` into optional. They are
  football-only from here: `p_winner` is emitted only when
  `basis == "final_table"`, and `p_top_six` is a deprecated football alias.
  WS11 task 5's bb/hb deltas must then restate the whole `required` array
  (RFC 7386 replaces arrays wholesale) as
  `["team", "p_top_of_table", "p_relegation"]`.

**`_base/points_distribution.schema.json`** — a THIRD subtraction WS11 has not
budgeted for. Its `summary[]` `required` array currently lists `p_top_six`,
`p_winner` and `p_relegation`. Basketball and handball emit
`p_top_of_table` and `p_relegation` there instead, so `p_top_six` and
`p_winner` must move to optional in `_base` and the bb/hb deltas must restate
`required` as `["team", "mean_points", "median_points", "lower_80",
"upper_80", "base_points", "p_top_of_table", "p_relegation"]`.

`points_distribution.json` does NOT gain a top-level `basis`, contrary to
WS10's own Produces list. It is one of the eight artefacts the golden
regeneration asserts byte-identical, and adding a key there would have put a
third basename in the changed set. If the platform needs the basis alongside
the points distribution it reads it from `meta.season_scope`.

**Follow-up that needs to exist as a tracked commit, not as prose:** removing
the `p_top_six` alias (and its `emit_top_six_alias` argument in
`.build_placement_summary()`) once metill-platform reads `p_qualify`. That
commit collapses the points_distribution split above at the same time.

### Task 1: `.validate_or_abort()` gains a `schema_dir` parameter; `publish_one()` threads it

**Files:**

- MODIFY /Users/brynjolfurjonsson/sports/R/publish-pipeline.R (`.validate_or_abort` def at :145, hardcoded `schema_dir <- here::here("config", "publish-schemas")` at :146; call sites at :103 and :135; `publish_one` signature at :57-60; roxygen `@param validate` block at :43-47)
- MODIFY /Users/brynjolfurjonsson/sports/man/publish_one.Rd (regenerated by `devtools::document()`)
- CREATE /Users/brynjolfurjonsson/sports/tests/testthat/test-publish-schema-arming.R

- [ ] Context & interfaces. CONSUMES: `validate_publish_dir(dir, schema_dir)` (R/validate-publish.R:32); `.resolve_schema_path(schema_dir, sport, base)` (R/validate-publish.R:89). PRODUCES: `.validate_or_abort(output_root, sport, key, sex, schema_dir = here::here("config", "publish-schemas"))` @noRd, and `publish_one(static, betting, key, sex, root = here::here("data"), validate = TRUE, schema_dir = here::here("config", "publish-schemas"))` exported. Both new parameters are LAST and defaulted, so `scripts/05_publish.R:34`'s 4-argument positional call is unchanged. This task lands first because every later task needs a temp `schema_dir` to test against without writing into the real `config/publish-schemas/`.

- [ ] Step 1: write the failing test. Create `tests/testthat/test-publish-schema-arming.R` with one block: `test_that(".validate_or_abort accepts an explicit schema_dir", ...)`. Build a temp `out <- withr::local_tempdir()` holding ONE conforming football cell at `out/football/iceland/karla-bd/meta.json` (construct the object inline in the test — do NOT read it from `data/publish/`, task 6 deletes part of that tree; use the 10 keys from `config/publish-schemas/football/meta.schema.json` `required`: sport="football", sex="male", league="Besta deild", division="BD", is_cup=FALSE, season=2026L, generated_at="2100-01-01T12:00:00+0000", fit_date="2100-01-01", round=8L, n_draws=4000L, written with `jsonlite::write_json(..., auto_unbox = TRUE, null = "null")`). Build `sch <- withr::local_tempdir()` and `file.copy(here::here("config", "publish-schemas", "football"), sch, recursive = TRUE)`. Assert `expect_no_error(.validate_or_abort(out, sport = "football", key = "football_iceland", sex = "male", schema_dir = sch))`.

- [ ] Step 2: RUN it — `Rscript -e 'devtools::load_all(); testthat::test_local(filter = "publish-schema-arming")'` from /Users/brynjolfurjonsson/sports. EXPECTED RED, exact: `Error in .validate_or_abort(out, sport = "football", key = "football_iceland",  : unused argument (schema_dir = sch)`. If you see any other error, stop and re-read R/publish-pipeline.R:145 before continuing.

- [ ] Step 3: implement. Add `schema_dir = here::here("config", "publish-schemas")` as the fifth formal of `.validate_or_abort` and DELETE the hardcoded assignment at :146. Add `schema_dir = here::here("config", "publish-schemas")` as the seventh formal of `publish_one` and pass `schema_dir = schema_dir` at both call sites (:103, :135). Document the new `publish_one` parameter in the roxygen block (`@param schema_dir Directory holding `<sport>/*.schema.json`. Overridable so tests can validate against `config/publish-schemas/_draft/`.`) and run `Rscript -e 'devtools::document()'`.

- [ ] Step 4: RUN again — the new block passes. Then run the whole suite: `Rscript -e 'devtools::test()'`. Confirm FAIL 0 and SKIP 45 (Plan A's measured baseline as of 2026-09-04 — if SKIP differs, note it, do not chase it).

- [ ] Step 5: commit. `git -C /Users/brynjolfurjonsson/sports add R/publish-pipeline.R man/publish_one.Rd tests/testthat/test-publish-schema-arming.R && git -C /Users/brynjolfurjonsson/sports commit` with message `feat(publish): thread schema_dir through publish_one into .validate_or_abort` and the Co-Authored-By trailer. Do NOT push.

**Verification.** `publish_one()` and `.validate_or_abort()` can both be pointed at an arbitrary schema directory, so every later task can arm a sport in a temp tree without touching `config/publish-schemas/`. `scripts/05_publish.R` still runs unchanged (its 4-arg call resolves the default).

### Task 2: Fix the arming hazard: validate the sport's OWN subtree, with an explicit `sport` (closing the silent fail-open the naive subtree fix creates)

**Files:**

- MODIFY /Users/brynjolfurjonsson/sports/R/validate-publish.R (`validate_publish_dir` signature at :32-33, `sport <- rel_parts[1]` at :48, `.resolve_schema_path` call at :50, roxygen at :22-31)
- MODIFY /Users/brynjolfurjonsson/sports/R/publish-pipeline.R (`.validate_or_abort` body — the `validate_publish_dir(output_root, ...)` call at :160)
- MODIFY /Users/brynjolfurjonsson/sports/man/validate_publish_dir.Rd (regenerated)
- MODIFY /Users/brynjolfurjonsson/sports/tests/testthat/test-publish-schema-arming.R (append)

- [ ] Context & the trap you must not fall into. The spec says `validate_publish_dir(file.path(output_root, sport), ...)`. Doing ONLY that silently fails OPEN: `validate_publish_dir` derives the sport from the first path segment relative to `dir` (`rel_parts[1]`, R/validate-publish.R:48), so with `dir = data/publish/football` the derived sport becomes `"iceland"`, `.resolve_schema_path` finds nothing, every file lands in `unmatched`, and the function returns `ok = TRUE, n_files = 0`. Football would stop being validated at all. So the subtree fix REQUIRES an explicit sport. PRODUCES: `validate_publish_dir(dir, schema_dir = here::here("config", "publish-schemas"), sport = NULL)` — exported; `sport = NULL` preserves today's first-path-segment derivation (used by the whole-tree call in `tests/testthat/test-publish-schemas.R:1` and `test-publish-football-split.R:134`), a non-NULL `sport` is used for EVERY file's lookup so `dir` may be a sport subtree.

- [ ] Step 1: write both failing tests, appended to `tests/testthat/test-publish-schema-arming.R`. (a) `test_that("validate_publish_dir resolves schemas from a sport subtree when given sport=", ...)`: temp tree with `<tmp>/football/iceland/karla-bd/meta.json` MISSING the required `sport` key; call `validate_publish_dir(file.path(tmp, "football"), schema_dir = here::here("config", "publish-schemas"), sport = "football")`; assert `expect_false(res$ok)`, `expect_equal(res$n_files, 1L)`, `expect_length(res$unmatched, 0L)`. (b) `test_that(".validate_or_abort validates only its own sport's subtree", ...)`: temp `out` with a CONFORMING football cell (as in task 1) and a NON-conforming basketball cell at `out/basketball/iceland/karla/meta.json` carrying exactly the stale June shape — `list(sport = "basketball", sex = "male", league = "Bónusdeild", season = 2026L, generated_at = "2100-01-01T12:00:00+0000", fit_date = "2100-01-01", round = 22L, n_draws = 4000L)`, i.e. no `division`, no `is_cup` (verified as the on-disk shape of `data/publish/basketball/iceland/karla/meta.json`; construct it inline, never read that file). Build a temp `sch` containing BOTH `football/` and `basketball/` (copy the real `config/publish-schemas/football/` twice, renaming the second) so basketball is armed. Assert `expect_no_error(.validate_or_abort(out, sport = "football", key = "football_iceland", sex = "male", schema_dir = sch))` AND `expect_error(.validate_or_abort(out, sport = "basketball", key = "basketball_iceland", sex = "male", schema_dir = sch), "do not match")`.

- [ ] Step 2: RUN — `Rscript -e 'devtools::load_all(); testthat::test_local(filter = "publish-schema-arming")'`. TWO EXPECTED REDs, exact: (a) `Error in validate_publish_dir(file.path(tmp, "football"), schema_dir = ..., : unused argument (sport = "football")`. (b) The football assertion fails as `Expected `.validate_or_abort(...)` to run without any errors` with the raised message beginning `publish_one(football_iceland/male) produced JSONs that do not match config/publish-schemas/football/*.schema.json.` — THIS is the arming hazard rendered as a test failure: basketball's bad JSON aborting football's publish call.

- [ ] Step 3: implement. In `R/validate-publish.R`, add `sport = NULL` as the third formal; inside the loop replace `sport <- rel_parts[1]` with `file_sport <- if (is.null(sport)) rel_parts[1] else sport` and pass `file_sport` to `.resolve_schema_path`. Keep `rel_parts` (still used for nothing else — delete it if it becomes unused, but confirm with a grep of the function body first). Update the roxygen `@param` block and add a `@param sport` explaining the fail-open trap in one sentence. In `R/publish-pipeline.R:160` replace the call with `validate_publish_dir(sport_dir, schema_dir = schema_dir, sport = sport)` (`sport_dir` is already computed at :147). Run `Rscript -e 'devtools::document()'`.

- [ ] Step 4: RUN again — both blocks pass. Then run `Rscript -e 'devtools::test()'` and confirm FAIL 0; in particular `test-publish-schemas.R` and `test-publish-football-split.R` must be unaffected (they pass no `sport`, so the NULL branch preserves their behaviour).

- [ ] Step 5: prove football's live validation is unchanged, not just its tests. Run `Rscript -e 'devtools::load_all(); r <- validate_publish_dir(here::here("data", "publish"), sport = NULL); cat(r$ok, r$n_files, r$n_passed, r$n_failed, length(r$unmatched), "\n")'` and record the five numbers in the commit message body.

- [ ] Step 6: commit — `fix(publish): validate only the publishing sport's subtree, with an explicit sport`. Include in the body: "A bare `validate_publish_dir(file.path(output_root, sport))` fails OPEN — the derived sport becomes 'iceland' and every file is unmatched. The explicit `sport` argument is what makes the subtree fix a fix." Do NOT push.

**Verification.** Arming one sport can no longer abort another sport's publish: with basketball armed against non-conforming JSON, `.validate_or_abort(sport = "football")` completes silently and `.validate_or_abort(sport = "basketball")` aborts. Football's whole-tree validation numbers on the live `data/publish/` tree are unchanged from before the edit.

### Task 3: `tools/gen-publish-schemas.R` + `_base/` + `_delta/football/`, rendering today's football schemas with no contract change

**Files:**

- CREATE /Users/brynjolfurjonsson/sports/tools/gen-publish-schemas.R
- CREATE /Users/brynjolfurjonsson/sports/config/publish-schemas/_base/{meta,next_games,standings,standings_history,team_strengths,team_strengths_history,final_positions,final_positions_history,home_advantage,points_distribution,tournament_placements}.schema.json (11 files)
- CREATE /Users/brynjolfurjonsson/sports/config/publish-schemas/_delta/football/{same 11 basenames}.json
- MODIFY /Users/brynjolfurjonsson/sports/config/publish-schemas/football/*.schema.json (regenerated; 3 non-ASCII characters removed — see step 2)
- MODIFY /Users/brynjolfurjonsson/sports/config/publish-schemas/README.md
- CREATE /Users/brynjolfurjonsson/sports/tests/testthat/test-publish-schema-generation.R

- [ ] Context & interfaces. PRODUCES: `gen_publish_schemas(source_dir, out_dir, sports = NULL) -> character()` (the paths written), defined at TOP LEVEL in `tools/gen-publish-schemas.R` with NO side effects at source time, so a test can `sys.source()` it — copy the self-execution guard pattern from `tools/make-extract-fixtures.R:46-53` (`.fixture_gen_pkg_root()` resolves the package root from `--file=` and returns NULL when sourced, which disables the render). Also PRODUCES `.merge_patch(base, patch)` (RFC 7386 JSON Merge Patch: recursive object merge; a `null` value in the patch DELETES the key; arrays and scalars replace wholesale). CONVENTION, and it is the whole point: `_delta/<sport>/<name>.json` EXISTING is what declares that `<sport>` emits `<name>` — there is no separate manifest file to drift. An empty `{}` delta is legal and means "identical to base". CONSUMES nothing from other workstreams; this task is independent of WS7-WS10 and may land at any time.

- [ ] Step 1: write the failing test. Create `tests/testthat/test-publish-schema-generation.R` with `test_that("the generator reproduces the committed per-sport schemas byte-for-byte", ...)`: `env <- new.env(); sys.source(testthat::test_path("..", "..", "tools", "gen-publish-schemas.R"), envir = env)`; `tmp <- withr::local_tempdir()`; `env$gen_publish_schemas(source_dir = testthat::test_path("..", "..", "config", "publish-schemas"), out_dir = tmp)`; then for every file under `config/publish-schemas/football/` assert `expect_equal(readBin(<generated>, "raw", n), readBin(<committed>, "raw", n))` — compare raw bytes, not parsed objects, or the test proves nothing about the committed tree being regenerable.

- [ ] Step 2: RUN — `Rscript -e 'devtools::load_all(); testthat::test_local(filter = "publish-schema-generation")'`. EXPECTED RED, exact: `Error in file(filename, "r", encoding = encoding) : cannot open the connection` with the warning `cannot open file '/Users/brynjolfurjonsson/sports/tools/gen-publish-schemas.R': No such file or directory`.

- [ ] Step 3: normalise the sources to ASCII BEFORE writing the generator — this is a hard prerequisite, not a style choice. VERIFIED: `jsonlite::toJSON()` renders a UTF-8 em-dash as the literal seven-character string `<U+2014>` even when `Encoding()` is already `"UTF-8"`, which would silently corrupt every description the generator round-trips. There are exactly THREE non-ASCII occurrences in the whole schema tree (`LC_ALL=C grep -c '[^ -~\t]'`): `config/publish-schemas/football/meta.schema.json:5` (`Schema v1 — additive` -> `Schema v1 -- additive`), `:24` (`(efri/neðri hluti)` -> `(efri/nedri hluti)`), and `config/publish-schemas/football/team_strengths.schema.json:5` (`(component × location)` -> `(component x location)`). Apply these with a python heredoc (`python3 - <<'EOF'` doing a byte-level `str.replace` on the file contents) — NOT with the Edit/Write tools, which mishandle these characters. Re-run the grep and assert zero hits across `config/publish-schemas/`.

- [ ] Step 4: implement the generator. Read with `jsonlite::read_json(path, simplifyVector = FALSE)` and write with `jsonlite::toJSON(x, auto_unbox = TRUE, pretty = 2, null = "null")` plus a trailing newline. VERIFIED round-trip property you are relying on: `read_json(simplifyVector = FALSE)` returns scalars as length-1 atomic vectors and arrays as lists, so `auto_unbox = TRUE` unboxes the scalars and leaves `"required": ["sport", ...]` (a length-10 list) as an array — confirmed against `football/meta.schema.json`. Add a hard guard: the generator ABORTS with a message naming the offending file if any source file or rendered output contains a byte outside `[\x09\x0a\x20-\x7e]`, citing the `<U+2014>` corruption. Key order is base order first, then delta-only keys appended, which is deterministic.

- [ ] Step 5: author `_base/` and `_delta/football/`. Fastest correct route: copy each of the 11 `football/<name>.schema.json` to `_base/<name>.schema.json` verbatim and write `_delta/football/<name>.json` as `{}`. Then move the genuinely football-specific bits out of `_base` into `_delta/football/`: at minimum the `properties.split` object in `meta.schema.json` (split-season format — football-only, see the schema's own description) and the `$id` (which names no sport today, so leave it in base). Resist inventing further deltas now; the bb/hb deltas in task 5 will tell you what actually differs. Every `_base` file that a sport does NOT emit simply has no `_delta/<sport>/` entry.

- [ ] Step 6: RUN the byte-equality test again. It will now FAIL on formatting (indentation/spacing of the hand-written committed files versus the generator's output). This is EXPECTED and the resolution is: the generator's rendering becomes canonical. Run `Rscript tools/gen-publish-schemas.R` to overwrite `config/publish-schemas/football/`, then re-run the test — it must now pass. Byte-equality with the pre-existing hand-formatted files is NOT the invariant; regenerability is.

- [ ] Step 7: prove the SEMANTICS did not change — this is the real gate. Before regenerating you should have captured the baseline; if not, `git stash` the schema regeneration, run `Rscript -e 'devtools::load_all(); saveRDS(validate_publish_dir(here::here("data", "publish")), "/private/tmp/claude-501/-Users-brynjolfurjonsson-sports/9cfbc60d-03cb-4b24-8f5a-2494c163c24f/scratchpad/vpd-before.rds")'`, unstash, and re-run into `vpd-after.rds`. Assert `identical(before$ok, after$ok)`, `identical(before$n_files, after$n_files)`, `identical(sort(before$errors), sort(after$errors))`, `identical(sort(before$unmatched), sort(after$unmatched))`. Any difference means a delta lost a constraint — find it before committing.

- [ ] Step 8: add two invariant tests to the same file. (a) `test_that("no sport-agnostic schema sits at the config/publish-schemas root", ...)`: `expect_length(list.files(here::here("config", "publish-schemas"), pattern = "\\.schema\\.json$"), 0L)`. A file there becomes the fallback for EVERY sport including `world_cup` on both the R and the Python side, arming validation nobody asked for. (b) `test_that("the generator's source directories resolve as no sport", ...)`: `expect_null(.resolve_schema_path(here::here("config", "publish-schemas"), "_base", "meta.json"))` and the same for `"_delta"` and `"_draft"`. (c) `test_that("config/publish-schemas is pure ASCII", ...)` walking every file and asserting no byte outside the printable+tab+newline range.

- [ ] Step 9: rewrite `config/publish-schemas/README.md`'s Layout and Updating sections for the generated model: edit `_base/` or `_delta/<sport>/`, run `Rscript tools/gen-publish-schemas.R`, commit both source and rendered output. State the RFC-7386 gotcha explicitly: a delta touching `required` REPLACES the array wholesale, so it must restate the full list. State the ASCII rule and why.

- [ ] Step 10: run `Rscript -e 'devtools::test()'` (FAIL 0), then commit — `feat(publish): generate per-sport schemas from _base + _delta`. Body records the five before/after `validate_publish_dir` numbers from step 7. Do NOT push.

**Verification.** `Rscript tools/gen-publish-schemas.R` re-renders `config/publish-schemas/football/` byte-identically to what is committed, and `validate_publish_dir()` over the live `data/publish/` tree returns the identical `ok`/`n_files`/`errors`/`unmatched` it returned before the refactor — so football's contract is provably unchanged while becoming impossible to diverge by hand.

### Task 4: Extend `_base` to the meta v2 / basis / p_qualify contract and re-render football (AFTER WS10)

**Files:**

- MODIFY /Users/brynjolfurjonsson/sports/config/publish-schemas/_base/meta.schema.json
- MODIFY /Users/brynjolfurjonsson/sports/config/publish-schemas/_base/{final_positions,points_distribution}.schema.json
- MODIFY /Users/brynjolfurjonsson/sports/config/publish-schemas/_delta/football/{meta,final_positions,points_distribution}.json
- MODIFY /Users/brynjolfurjonsson/sports/config/publish-schemas/football/*.schema.json (regenerated)
- MODIFY /Users/brynjolfurjonsson/sports/tests/testthat/test-publish-schema-generation.R (append)

- [ ] Context & ordering. THIS TASK MUST NOT LAND BEFORE WS10. It makes the new meta keys REQUIRED, and a schema that requires a key the publisher does not yet emit turns the next football publish into an abort. CONSUMES from WS10: the exact `meta.json` v2 key set (`units {strength, home_advantage, diff_bin_width}`, `points {win, draw, loss}`, `n_rounds`, `n_rounds_source` in `{"schedule", "config"}`, corrected `round`, `season_scope` in `{"full_season", "regular_season"}`, `postseason` null-or-`{name_is, modelled}`, `qualify` null-or-`{slots, label_is}`), and from `final_positions.json`/`points_distribution.json` the required top-level `basis` in `{"final_table", "regular_season_table"}` plus `summary[].p_qualify`. Read WS10's actual emitted payload before writing the schema — do not schema-write from this plan's prose.

- [ ] Step 1: write the failing test. Append `test_that("meta.schema.json requires the v2 contract keys", ...)`: build a temp cell whose `meta.json` carries the v1 ten keys but NOT `n_rounds`; `res <- validate_publish_dir(file.path(tmp, "football"), schema_dir = here::here("config", "publish-schemas"), sport = "football")`; `expect_false(res$ok)`. Add a sibling block asserting `basis` is required in `final_positions.json`.

- [ ] Step 2: RUN — `Rscript -e 'devtools::load_all(); testthat::test_local(filter = "publish-schema-generation")'`. EXPECTED RED, exact: `Failure: expect_false(res$ok) — res$ok is not FALSE` (it is TRUE, because `_base/meta.schema.json` does not yet require `n_rounds`).

- [ ] Step 3: implement in `_base`, not in `football/`. Add each new key to `_base/meta.schema.json` `properties` with its type/enum, and add the ones WS10 emits unconditionally to `required` (remember: a delta that later narrows `required` must restate it whole). Put in `_delta/football/meta.json` only what is genuinely football-specific: `properties.split` (already there from task 3) and `properties.season_scope.enum = ["full_season"]`. Add `basis` and `p_qualify` to `_base/{final_positions,points_distribution}.schema.json`; keep football's deprecated `p_top_six` alias as an OPTIONAL property in `_delta/football/final_positions.json` (§11 gives it a named removal commit — do not put it in `_base`, or bb/hb inherit a Besta-deild concept).

- [ ] Step 4: `Rscript tools/gen-publish-schemas.R`, then RUN the test again — green.

- [ ] Step 5: prove football still publishes. Run the football publish path against the committed fixtures exactly as WS9's football regression test does, then `Rscript -e 'devtools::test()'` and confirm the golden-file test (`tests/testthat/fixtures/golden/football-publish-hashes.csv`, 92 hashes) is still green. If a hash moved, that is WS10's payload change and must have been deliberately regenerated there — NOT here.

- [ ] Step 6: commit — `feat(publish): extend the shared schema base to the meta v2 contract`. Do NOT push.

**Verification.** A `meta.json` missing `n_rounds`, or a `final_positions.json` missing `basis`, is rejected by the generated football schema; the live football publish and its 92 golden hashes are unaffected.

### Task 5: Author the basketball + handball schemas under `_draft/` and validate fixture-published bb/hb JSON against them (AFTER WS9 + WS10)

**Files:**

- CREATE /Users/brynjolfurjonsson/sports/config/publish-schemas/_delta/basketball/{meta,next_games,standings,standings_history,team_strengths,team_strengths_history,final_positions,final_positions_history,home_advantage,points_distribution}.json (10 files — NO tournament_placements)
- CREATE /Users/brynjolfurjonsson/sports/config/publish-schemas/_delta/handball/{same 10 basenames}.json
- CREATE /Users/brynjolfurjonsson/sports/config/publish-schemas/_draft/{basketball,handball}/*.schema.json (generated, 20 files)
- MODIFY /Users/brynjolfurjonsson/sports/tools/gen-publish-schemas.R (a `--draft` mode routing basketball/handball output to `_draft/<sport>/`)
- CREATE /Users/brynjolfurjonsson/sports/tests/testthat/test-publish-schema-draft.R

- [ ] Context & interfaces. CONSUMES from WS2: `fixture_facts_root(env)` (tests/testthat/helper-fixture-facts.R:37 — returns a temp root with `facts/` written via `write_table`) and the committed 2DT extracts tree at `tests/testthat/fixtures/extracts/sport={basketball,handball}/country=iceland/sex={male,female}/fit_date=2100-01-01/`. CONSUMES from WS9: `publish_one()`'s single extracts path and `sport_publish_profile(sport)$surfaces`. CONSUMES from WS10: the emitted bb/hb payload. WHY `_draft` works, verified: `.resolve_schema_path()` (R/validate-publish.R:89) tries exactly `<schema_dir>/<sport>/<base>.schema.json` then `<schema_dir>/<base>.schema.json`, and the Python mirror `resolve_schema_path()` (metill-platform/scripts/validate_publish.py:46-68) does the same two lookups — a first path segment of `_draft` matches in neither, so schemas can be committed, reviewed and rsynced to the platform while remaining completely inert.

- [ ] Step 1: write the failing test. Create `tests/testthat/test-publish-schema-draft.R` with `test_that("the draft bb/hb schemas accept fixture-published cells", ...)`. Build the input: `root <- fixture_facts_root()`; copy the committed extracts partitions into `file.path(root, "beliefs", "extracts")` (i.e. `dir.create(..., recursive = TRUE)` then `file.copy(testthat::test_path("fixtures", "extracts", "sport=basketball"), <dest>, recursive = TRUE)` and the same for handball) — do NOT use `fixture_extracts_root()`, which returns a standalone root that `publish_one()` cannot see. Then for each of the 8 cells call `publish_one(static, betting, key, sex, root = root, validate = FALSE)` (validate FALSE: the real schemas are not armed yet and that is the point). Finally assert, per sport: `res <- validate_publish_dir(file.path(root, "publish", sport), schema_dir = here::here("config", "publish-schemas", "_draft"), sport = sport); expect_true(res$ok); expect_length(res$unmatched, 0L); expect_equal(res$n_files, 4L * length(sport_publish_profile(sport)$surfaces))`. NO `skip()` / `skip_if()` / `Sys.getenv` anywhere in this file.

- [ ] Step 2: RUN — `Rscript -e 'devtools::load_all(); testthat::test_local(filter = "publish-schema-draft")'`. EXPECTED RED, exact: `Error in validate_publish_dir(...) : dir.exists(schema_dir) is not TRUE` — the `stopifnot` at R/validate-publish.R:34, because `config/publish-schemas/_draft` does not exist.

- [ ] Step 3: implement the deltas. Write 10 `_delta/basketball/*.json` and 10 `_delta/handball/*.json` — start each as `{}` and add ONLY differences you can point at in the actual published fixture JSON. The ones you should expect to need, each justified: `meta.json` -> `properties.sport.enum` narrowed to the single sport, `properties.season_scope.enum = ["regular_season"]`, `properties.split` set to `null` (merge-patch delete — bb/hb have no split-season format), `postseason` required as an object rather than nullable; `home_advantage.json` -> per-sport `records.items.properties.median` `minimum`/`maximum` bounds, because 2DT home advantage is RAW points/goals (B5, fixed in Plan A) not a log scale, and a bound here is exactly what would have caught a 4-point home edge publishing as 54.6 — pick bounds from the fixture's actual values widened generously, and write the reasoning into the schema `description`; `final_positions.json` -> `basis` enum narrowed to `["regular_season_table"]`, `p_winner` NOT in `required` (§15: p_winner is emitted only when basis is final_table), `p_top_six` absent entirely. NOTE: `tournament_placements` gets no bb/hb delta — no cup division is ingested for either sport, so neither emits it, and the delta's ABSENCE is what records that.

- [ ] Step 4: add the `--draft` mode to the generator: `gen_publish_schemas(sports = c("basketball", "handball"), out_dir = file.path(source_dir, "_draft"))` writes `_draft/<sport>/<name>.schema.json`. Render, then RUN the test again. Iterate on the deltas until green — every failure it reports is a real disagreement between your schema and WS9/WS10's actual payload, so read the failure before loosening anything.

- [ ] Step 5: add a second block to the same file — `test_that("the _draft tree is inert in both validators", ...)`: `expect_null(.resolve_schema_path(here::here("config", "publish-schemas"), "_draft", "meta.json"))`, and `res <- validate_publish_dir(file.path(root, "publish", "basketball"), schema_dir = here::here("config", "publish-schemas"), sport = "basketball"); expect_equal(res$n_files, 0L); expect_gt(length(res$unmatched), 0L)` — i.e. with the REAL schema_dir, basketball is still entirely unvalidated.

- [ ] Step 6: add a third block tying the delta directories to the profile — `test_that("each sport's _delta file set equals its declared surfaces", ...)`: for each of the three sports, `expect_setequal(sub("\\.json$", "", list.files(here::here("config", "publish-schemas", "_delta", sport))), sport_publish_profile(sport)$surfaces)`. This is the seam that stops the delta directory (which IS the manifest) from drifting from WS9's profile.

- [ ] Step 7: prove the whole suite is unmoved: `Rscript -e 'devtools::test()'` — FAIL 0, and `test-fixture-skip-hygiene.R` green (your new file has no skip gates). Commit — `feat(publish): draft basketball + handball schemas under _draft (inert)`. Do NOT push.

**Verification.** Every JSON the 8 fixture-published bb/hb cells emit validates against the drafted schemas, while the real `config/publish-schemas/` still classifies all of them as `unmatched` — the schemas are complete, reviewable and provably not yet in force on either side of the rsync.

### Task 6: Delete the stale un-suffixed bb/hb publish directories (the arming precondition)

**Files:**

- DELETE /Users/brynjolfurjonsson/sports/data/publish/basketball/iceland/{karla,kvenna}/ (16 tracked JSONs)
- DELETE /Users/brynjolfurjonsson/sports/data/publish/handball/iceland/{karla,kvenna}/ (16 tracked JSONs)
- CREATE /Users/brynjolfurjonsson/sports/tests/testthat/test-publish-legacy-cells.R

- [ ] Context. VERIFIED on disk: `data/publish/{basketball,handball}/iceland/{karla,kvenna}/` are git-tracked, newest stamped 2026-06-23, and their `meta.json` carries neither `division` nor `is_cup` — both required by the generated schema. They are at the OLD path shape (no `{sex}-{slug}` division segment, §14 correction 2). Arming with these on disk would fail closed on BOTH sides: R-side `.validate_or_abort(sport = "basketball")` aborts, and platform-side `validate_publish.py` exits non-zero and freezes the site on the last-known-good payload. Nothing on metill-platform routes bb/hb today, so deleting them is free; the platform's `--delete` rsync will drop them from `data/ithrottir/` on the next pull. CONSUMES from WS9/WS10: the new `{sex}-{slug}` path shape.

- [ ] Step 1: write the failing test. Create `tests/testthat/test-publish-legacy-cells.R` with `test_that("no un-suffixed bb/hb publish cell survives", ...)`: for `sport` in c("basketball", "handball") and `d` in c("karla", "kvenna"), `expect_false(dir.exists(here::here("data", "publish", sport, "iceland", d)))`. Add a second assertion that every directory under `data/publish/{basketball,handball}/iceland/` matches `^(karla|kvenna)-[a-z0-9]+$`, so the old shape can never come back.

- [ ] Step 2: RUN — `Rscript -e 'devtools::load_all(); testthat::test_local(filter = "publish-legacy-cells")'`. EXPECTED RED, exact: four failures of the form `Failure: dir.exists(here::here("data", "publish", sport, "iceland", d)) is not FALSE`.

- [ ] Step 3: sync before touching tracked data — five cron workflows commit to `main` all day. `git -C /Users/brynjolfurjonsson/sports fetch origin && git -C /Users/brynjolfurjonsson/sports status --short`. If the working tree is dirty with generated data, follow `.claude/rules/git-hygiene.md`'s stash -> pull --rebase -> pop.

- [ ] Step 4: delete — `git -C /Users/brynjolfurjonsson/sports rm -r data/publish/basketball/iceland/karla data/publish/basketball/iceland/kvenna data/publish/handball/iceland/karla data/publish/handball/iceland/kvenna`. Confirm 32 files staged for deletion.

- [ ] Step 5: RUN the test again — green. Then `Rscript -e 'devtools::test()'` and check `test-publish-schemas.R`'s live-tree block specifically: it walks `data/publish/` whole and must still report `ok = TRUE` (basketball/handball now contribute zero files rather than unmatched ones).

- [ ] Step 6: commit — `chore(publish): drop the un-suffixed basketball/handball cells (June 2026, pre-division shape)`. Body must state that they were produced on a laptop, never on CI, and are superseded by the `{sex}-{slug}` cells. Do NOT push.

**Verification.** `data/publish/{basketball,handball}/iceland/` holds no un-suffixed directory, and a whole-tree `validate_publish_dir()` over `data/publish/` still returns `ok = TRUE` — so the next task can arm the schemas without any pre-existing JSON to trip over on either side of the rsync.

### Task 7: Arm: one `git mv` from `_draft/` into `config/publish-schemas/{basketball,handball}/`

**Files:**

- RENAME /Users/brynjolfurjonsson/sports/config/publish-schemas/_draft/basketball/ -> config/publish-schemas/basketball/ (10 files)
- RENAME /Users/brynjolfurjonsson/sports/config/publish-schemas/_draft/handball/ -> config/publish-schemas/handball/ (10 files)
- MODIFY /Users/brynjolfurjonsson/sports/tools/gen-publish-schemas.R (drop the `--draft` routing for these two sports; they now render in place)
- MODIFY /Users/brynjolfurjonsson/sports/tests/testthat/test-publish-schema-draft.R (repoint from `_draft` to the real schema_dir; rename the file to test-publish-schema-2dt.R)
- MODIFY /Users/brynjolfurjonsson/sports/config/publish-schemas/README.md

- [ ] Context. Arming is deliberately a single directory move so rollback is `git rm -r config/publish-schemas/<sport>` and touches no JSON. It must land AFTER task 6 (stale cells gone) and AFTER tasks 4-5 (schemas proven against real fixture output). Cross-repo consequence, and it is immediate: `pull-sports-data.yml` sparse-checks out `data/publish` and `config/publish-schemas` from ONE clone at ONE SHA (metill-platform/.github/workflows/pull-sports-data.yml:79-116) and rsyncs `config/publish-schemas/` -> `data/ithrottir-schemas/` with `--delete`, so this move turns the Python validator fail-closed for both sports on the very next pull. `_base/`, `_delta/` and `README.md` ride along harmlessly — verified inert, since neither resolver ever looks below a `<sport>/` segment it did not derive from the JSON's own path.

- [ ] Step 1: write the failing test. In `tests/testthat/test-publish-schema-draft.R` (which you will rename in step 3), add `test_that("basketball and handball schemas are armed", ...)`: `for (sport in c("basketball", "handball")) expect_false(is.null(.resolve_schema_path(here::here("config", "publish-schemas"), sport, "meta.json")))`.

- [ ] Step 2: RUN — `Rscript -e 'devtools::load_all(); testthat::test_local(filter = "publish-schema")'`. EXPECTED RED, exact: two failures `Failure: is.null(.resolve_schema_path(...)) is not FALSE` — the path is NULL because `config/publish-schemas/{basketball,handball}/` do not exist.

- [ ] Step 3: arm. `git -C /Users/brynjolfurjonsson/sports mv config/publish-schemas/_draft/basketball config/publish-schemas/basketball && git -C /Users/brynjolfurjonsson/sports mv config/publish-schemas/_draft/handball config/publish-schemas/handball && rmdir /Users/brynjolfurjonsson/sports/config/publish-schemas/_draft`. Simplify `tools/gen-publish-schemas.R` so all three sports render in place (keep the `out_dir` parameter — the generator test still renders into a temp dir). Repoint every `schema_dir = here::here("config", "publish-schemas", "_draft")` in the test file to the real directory, and DELETE the now-false "the _draft tree is inert" block, replacing it with the mirror assertion: with the real schema_dir, `res$n_files` is now non-zero and `res$unmatched` is empty. `git mv` the test file to `tests/testthat/test-publish-schema-2dt.R`.

- [ ] Step 4: RUN — green. Then the two arming-safety proofs, both required. (a) `Rscript -e 'devtools::load_all(); r <- validate_publish_dir(here::here("data", "publish")); cat(r$ok, r$n_files, r$n_failed, "\n"); print(r$errors)'` — `ok` must be TRUE and `errors` empty; this is the live tree with basketball and handball now armed. (b) `Rscript -e 'devtools::load_all(); gen <- new.env(); sys.source("tools/gen-publish-schemas.R", envir = gen)'` then re-render into a temp dir and byte-compare all three sport directories, so the generator test still covers 31 files rather than 11.

- [ ] Step 5: prove the negative case still isolates — re-run `test-publish-schema-arming.R` (task 2). Its basketball-aborts / football-survives pair is the standing guarantee that arming cannot take football down, and it must be green with the schemas now real.

- [ ] Step 6: update `config/publish-schemas/README.md`: remove the paragraph claiming only football is populated and that bb/hb 'migrate into the schema set as part of F6 at the autumn 2026 cutover' (that is this change), and document the `_draft` arming workflow and its `git rm -r` rollback.

- [ ] Step 7: `Rscript -e 'devtools::test()'` (FAIL 0), commit — `feat(publish): arm the basketball and handball schemas`. Body notes the cross-repo effect: the next `pull-sports-data.yml` run makes `validate_publish.py` fail-closed for both sports. Do NOT push.

**Verification.** `.resolve_schema_path()` resolves both new sports; a whole-tree `validate_publish_dir()` over the live `data/publish/` returns `ok = TRUE` with zero errors; `test-publish-schema-arming.R` still proves a broken basketball cell cannot abort football's publish; and the generator reproduces all 31 committed schema files byte-for-byte.

### Task 8: Invert the default: publishing a sport with no schema directory ABORTS

**Files:**

- MODIFY /Users/brynjolfurjonsson/sports/R/publish-pipeline.R (`.validate_or_abort` :148 and :151-158 — the two fail-open short-circuits)
- MODIFY /Users/brynjolfurjonsson/sports/tests/testthat/test-publish-schema-arming.R (append)
- MODIFY /Users/brynjolfurjonsson/sports/.claude/rules/publish-layer.md (the `Schema validation (since 2026-05-26)` section at :272-299, and the `paths:` frontmatter at :1-9)
- MODIFY /Users/brynjolfurjonsson/sports/R/publish-pipeline.R (the `publish_one()` roxygen at :20-31 and :35-38)

- [ ] Context & the one hard ordering constraint. CONSUMES from WS12: the per-cell `tryCatch` around `publish_one()` in `scripts/05_publish.R`. That guard MUST be in place before this task lands. `scripts/05_publish.R:34` calls `publish_one()` bare inside a loop; converting a skip into an abort without the guard turns any single bad cell into a football outage — the exact asymmetry §13 Guard 2 exists to prevent. Verify it first: `grep -n 'tryCatch' /Users/brynjolfurjonsson/sports/scripts/05_publish.R` must hit. If it does not, stop and land WS12's guard first. SAFETY NOTE, verified: `publish_world_cup()` (R/wc-publish.R:59) writes `data/publish/world_cup/karla/` on its own path and never calls `publish_one()` or `.validate_or_abort()`, so `world_cup` — which has no schema directory and is not getting one — is untouched by this inversion.

- [ ] Step 1: write the failing test. Append `test_that(".validate_or_abort aborts when a sport has no schema directory", ...)`: build a temp `out` with a conforming football cell AND a handball cell; build a temp `sch` containing ONLY `football/`; assert `expect_error(.validate_or_abort(out, sport = "handball", key = "handball_iceland", sex = "male", schema_dir = sch), "no schemas")`. Add a sibling assertion that a missing schema ROOT also aborts: `expect_error(.validate_or_abort(out, sport = "football", key = "football_iceland", sex = "male", schema_dir = file.path(tempdir(), "does-not-exist")))`.

- [ ] Step 2: RUN — `Rscript -e 'devtools::load_all(); testthat::test_local(filter = "publish-schema-arming")'`. EXPECTED RED, exact: `Failure: `.validate_or_abort(out, sport = "handball", ...)` did not throw an error.` — today it emits `cli_alert_info("publish_one(handball_iceland/male): no schemas at .../handball/ -- skipping validation")` and returns `invisible(NULL)` (R/publish-pipeline.R:151-158).

- [ ] Step 3: implement. Replace the `:151-158` block with a `cli::cli_abort()` naming the sport, the missing directory, and the two ways out (add the schemas, or pass `validate = FALSE` for a synthetic-data test). Split the `:148` compound condition: `!dir.exists(schema_dir)` becomes an abort (a missing schema root is a broken checkout, not a reason to publish unchecked); keep `!dir.exists(sport_dir)` as a quiet return but add a `cli_alert_warning` — the publisher just ran, so an absent sport directory is worth a line in the log.

- [ ] Step 4: RUN again — green. Then run the FULL suite: `Rscript -e 'devtools::test()'` and expect fallout in any test that publishes synthetic data through `publish_one()` with `validate` left at its default. Fix each by passing `validate = FALSE` (the documented escape hatch) — NOT by re-loosening the default. Record which test files you touched.

- [ ] Step 5: end-to-end proof — `Rscript scripts/05_publish.R` from the repo root. It must exit 0 with a `schema validation passed` line for every published cell and no `skipping validation` line anywhere. Then deliberately break one cell (temporarily rename a required key in one `data/publish/handball/iceland/karla-od/meta.json`), re-run, and confirm: handball's cell is reported as failed, football's cells still republish, and the script exits non-zero. Restore the key with `git checkout --` afterwards.

- [ ] Step 6: rewrite the docs in the SAME commit. (a) `R/publish-pipeline.R:20-31` — the `publish_one()` roxygen still says basketball and handball 'still read the fit RDS directly ... their migration to the extraction layer is deferred to the autumn 2026 cutover'; that is now false on both counts. (b) `:35-38` — state the fail-closed default and the `validate = FALSE` escape hatch. (c) `.claude/rules/publish-layer.md:281-284` — delete 'Today only football schemas exist ... until F6 migrates them onto the football shape at the autumn 2026 cutover'; replace with the generated `_base`/`_delta` model, the `_draft` arming pattern, the subtree-plus-explicit-`sport` rule and WHY the naive subtree call fails open, and the fail-closed default. (d) Add `- "config/publish-schemas/**"` and `- "tools/gen-publish-schemas.R"` to that rule's `paths:` frontmatter — verified absent today, which means the rule does not load when someone edits a schema. Run `Rscript -e 'devtools::document()'`.

- [ ] Step 7: `Rscript -e 'devtools::test()'` (FAIL 0), commit — `feat(publish): fail closed when a sport has no publish schemas`. Do NOT push.

**Verification.** Publishing a sport with no `config/publish-schemas/<sport>/` aborts with a message naming the sport; `scripts/05_publish.R` reports `schema validation passed` for every cell across all three sports and emits no `skipping validation` line; a deliberately broken handball cell fails that cell, republishes football, and exits non-zero; `publish_world_cup()` is unaffected.

### Task 9: Cross-repo: prove metill-platform's Python validator now reports zero `unmatched` for basketball and handball

**Files:**

- READ-ONLY /Users/brynjolfurjonsson/metill-platform/scripts/validate_publish.py
- READ-ONLY /Users/brynjolfurjonsson/metill-platform/.github/workflows/pull-sports-data.yml
- MODIFY /Users/brynjolfurjonsson/sports/config/publish-schemas/README.md (the `Consumer side (Python)` section)
- MODIFY /Users/brynjolfurjonsson/sports/tests/testthat/test-publish-schema-2dt.R (append the resolver-parity block)

- [ ] Context. This task changes NO metill-platform code — the whole point of §14 correction 5 is that the platform's schema tree is populated by the existing rsync from one clone at one SHA, so schema and JSON can never skew. What it does is EXECUTE the consumer-side validator against exactly what the rsync will deliver, before the next scheduled pull does it unattended. `pull-sports-data.yml` runs 7x/day (`25 7-12,19 * * *`), so an unverified arming has a few hours to become a frozen site.

- [ ] Step 1: the executable RED. Stage a mirror of what the rsync produces: `mkdir -p <scratch>/mirror && rsync -a --delete /Users/brynjolfurjonsson/sports/data/publish/ <scratch>/mirror/ithrottir/ && rsync -a --delete /Users/brynjolfurjonsson/sports/config/publish-schemas/ <scratch>/mirror/ithrottir-schemas/` (scratchpad root: /private/tmp/claude-501/-Users-brynjolfurjonsson-sports/9cfbc60d-03cb-4b24-8f5a-2494c163c24f/scratchpad). Then, FROM the metill-platform project root (`uv run` outside the root drops `--extra` behind a warning that is easy to skim past): `cd /Users/brynjolfurjonsson/metill-platform && uv run --extra data python scripts/validate_publish.py --tree <scratch>/mirror/ithrottir --schemas <scratch>/mirror/ithrottir-schemas`. Do this ONCE BEFORE task 7's arming commit if you can (checkout the pre-arming SHA into the mirror), or reconstruct it by moving `basketball/` and `handball/` back out of the mirror's schema dir. EXPECTED RED: the run exits 0 but its `unmatched` list names every basketball and handball JSON — i.e. both sports pass by not being checked, which is the fail-open this whole workstream exists to close.

- [ ] Step 2: run it against the armed mirror (schemas in place). EXPECTED GREEN: exit 0, and zero entries in `unmatched` whose path starts with `basketball/` or `handball/`. Record `n_passed` and the full `unmatched` list in the commit body — the only remaining unmatched entries should be under `world_cup/`, which has no schema by design.

- [ ] Step 3: confirm the source directories ride the rsync harmlessly. `ls <scratch>/mirror/ithrottir-schemas/` will show `_base/`, `_delta/`, `README.md` alongside the three sport directories. Assert with a one-liner that none of them can resolve: no `<scratch>/mirror/ithrottir-schemas/*.schema.json` at the root (the sport-agnostic fallback the Python `resolve_schema_path` checks second), and no `data/publish/_base` or `data/publish/_delta` segment could ever exist to make them a first path segment. If the extra ~22 files in the platform repo are judged noise, the fix is an `--exclude '_*'` on the platform's schema rsync — record that as a follow-up in the README, do NOT edit metill-platform in this workstream.

- [ ] Step 4: write the resolver-parity test (R side, no Python dependency in the suite). Append to `tests/testthat/test-publish-schema-2dt.R`: `test_that("the R and Python schema resolvers agree on every live case", ...)` — a table-driven block over `(sport, basename) -> expected` covering the sport-namespaced hit for all three sports, the `_base`/`_delta` non-hits, and the root-fallback non-hit, asserting `.resolve_schema_path()` matches. Add a note in the test body recording the one latent divergence found by reading both: R uses the anchored `sub("\\.json$", ".schema.json", base)` (R/validate-publish.R:91) while Python uses the unanchored `name.replace(".json", ".schema.json")` (validate_publish.py:60) — identical for every basename in use, and the test asserts every published basename matches `^[a-z_]+\.json$` so it stays that way.

- [ ] Step 5: rewrite `config/publish-schemas/README.md`'s `Consumer side (Python)` and `How drift surfaces` sections: both ends now fail closed for all three sports; the schema tree is generated, not hand-mirrored; the local verification command from step 1 is recorded verbatim so the next person can re-run it in one paste.

- [ ] Step 6: `Rscript -e 'devtools::test()'` (FAIL 0), commit — `docs(publish): record the cross-repo validator proof for the 2DT schemas`. Do NOT push.

**Verification.** `uv run --extra data python scripts/validate_publish.py` run from /Users/brynjolfurjonsson/metill-platform against a staged mirror of the rsync output reports every basketball and handball JSON as VALIDATED rather than `unmatched`, exits 0, and leaves only `world_cup/` unmatched — so the next scheduled `pull-sports-data.yml` will not freeze the site.

**Risks.**

- ORDERING IS THE WHOLE RISK. Tasks 1-3 are independent and may land any time. Task 4 requires WS10. Task 5 requires WS9 + WS10. Tasks 6-7 require task 5. Task 8 requires task 7 AND WS12's publish tryCatch. Task 9 requires task 7. Landing task 7 before task 6 arms basketball validation against the stale June JSON (meta.json verified to lack `division` and `is_cup`, both required) and fails the platform pull closed — freezing fly.metill.is on the last-known-good payload within hours.
- The naive subtree fix fails OPEN, silently. `validate_publish_dir(file.path(output_root, sport))` alone makes the derived sport `"iceland"`, so every file lands in `unmatched` and the function returns ok=TRUE, n_files=0 — football would stop being validated with no error anywhere. Task 2's explicit `sport` argument is what makes it a fix; a reviewer seeing only the path change should reject it.
- At branch-merge time data/publish may contain NO new-shape bb/hb cells at all: producing them needs a real 2DT fit, and per the spec's residual risks nobody has ever run one green inside fit.yml's budget. Arming would then be proven only against fixture-published JSON (task 5), never against production output. That is the best proof available pre-fit and it should be stated as such — the first real 2DT publish is still the moment the contract is tested for real, and WS12's check_publish_freshness is what will say so.
- RFC-7386 replaces arrays wholesale, so a `_delta` that touches `required` and forgets to restate the full list silently RELAXES that sport's contract. The generator cannot detect this; only the per-sport validation tests can. Review every delta that mentions `required`.
- jsonlite's `<U+2014>` corruption (verified) means a single non-ASCII character introduced into a `_base` description silently rewrites it in all three rendered sports at once. The generator's ASCII abort is the guard; do not weaken it to a warning.
- The rsync copies `_base/`, `_delta/` and `README.md` into metill-platform's data/ithrottir-schemas/ as ~22 files of noise. Verified inert (neither resolver looks below a `<sport>/` segment derived from the JSON's own path), but if anyone ever places a `*.schema.json` at the config/publish-schemas ROOT it becomes the fallback for every sport including world_cup on both sides. Task 3's root-emptiness test is the only thing standing between that and an unasked-for world_cup contract.
- Task 8's inverted default is a live-fire change to the daily cron: after it, any sport publishing without schemas kills its cell. Only three sports go through publish_one() and all three will be armed; publish_world_cup() (R/wc-publish.R:59) is verified NOT to call publish_one() or .validate_or_abort() and is therefore unaffected. Re-verify that with a grep before landing task 8 rather than trusting this line.
- Existing tests that publish synthetic data through publish_one() with `validate` at its default will start aborting at task 8. The correct fix is `validate = FALSE` per call site (the documented escape hatch); loosening the default to make them pass would undo the workstream.
- data/publish is git-tracked and five cron workflows plus the launchd autoplace agent touch this repo on their own schedules. Task 6 deletes 32 tracked files — fetch and check the working tree immediately before, and commit immediately after, per .claude/rules/git-hygiene.md.

---

# WS12 — Health checks (publish freshness, season resolution, format agreement) + per-target fit/publish loop isolation

**Goal.** Make the "warn and exit 0" failure class impossible to stay invisible. Three new pipeline_health() checks turn a silent publish skip, an unresolvable federation season and a changed competition format into rows a human (and the GitHub failure email) can see; two per-target tryCatch guards stop one marginal 2DT fit or publish taking football's down with it in the same run. Verified today (2026-09-04) against the real repo: pipeline_health() composes eight checks at R/health.R:528-551 and NOT ONE reads data/publish/; scripts/03_fit.R:53 calls fit_one(static, row$sex) bare; scripts/05_publish.R:34 calls publish_one(...) bare; hsi_unresolved_seasons(2027) returns 3 rows (male cup, male playoffs, female playoffs) while kki_unresolved_seasons(2027) returns 0; has_upcoming_games() is FALSE for all four bb/hb cells and TRUE for both football cells.

**Consumes.**

- sport_publish_profile(sport) -> list with $surfaces (character vector of publish-artefact basenames) — FROM WS9 (spec §10(e)). Hard dependency of `.publish_surfaces()`. WS12 must land AFTER WS9.
- extract_partition_exists(extracts_root, sport, country, sex) -> logical(1) — FROM WS9, the rename of football_extract_partition_exists (R/publish-pipeline.R:9-19; its body already takes sport as an argument).
- config/leagues.yml::<key>.publish_divisions[[sex]] entries carrying code/slug/label_is/is_cup, and the optional expected_meetings / qualify_slots — FROM WS7. Read DIRECTLY off the leagues object passed into the check, NOT via `.iceland_division_*()` (see Integration decision INT-1).
- meta.json keys n_rounds, n_rounds_source, qualify$slots — FROM WS10 (spec §12).
- hsi_unresolved_seasons(season, sexes) -> tibble(sex, division, season) — R/ingest-hsi-handball.R:141 (Plan A, shipped).
- kki_unresolved_seasons(season, sexes, path) -> tibble(sex, division, season) — R/ingest-kki-basketball.R:253 (Plan A, shipped).
- hsi_current_season(today) / kki_current_season(today) — R/ingest-hsi-handball.R:523, R/ingest-kki-basketball.R:216. Both return 2027 today.
- has_upcoming_games(static, sex, root, days = 14L) — R/pipeline-freshness.R:98. NOTE: it reads Sys.Date() internally and ignores any injected `now`; every upcoming fixture in a test MUST be Sys.Date() + N.
- health_row(check, scope, status, value, threshold) / health_empty() — R/health.R:31,39.
- fit_one(static, sex) -> integer(1) — R/model-league.R:335. publish_one(static, betting, key, sex, root, validate) — R/publish-pipeline.R:57.
- tests/testthat/test-health.R's existing fixture idiom: withr::local_tempdir() + write_table(...) + a synthetic `leagues` list (see .fb_league at tests/testthat/test-health.R:144-146).

**Produces.**

- health_thresholds() gains publish_max_age_hours = 36 (numeric) — R/health.R:9-28, the existing named-constant block. Later workstreams must not remove it.
- `.parse_publish_stamp(x) -> POSIXct(1)` — @noRd, R/health-publish.R. Tolerates BOTH stamp shapes on disk: '%Y-%m-%dT%H:%M:%S%z' (what publish writes, R/publish-football-iceland.R:929 and R/publish-basketball-iceland.R:102) and '%Y-%m-%dT%H:%M:%SZ' (what write_health_status writes, R/health.R:588). Returns NA on an unparseable value; never errors.
- `.publish_cell_dir(root, sport, sex, slug) -> character(1)` — @noRd, R/health-publish.R. file.path(root, 'publish', sport, 'iceland', paste0(sex_folder, '-', slug)) where sex_folder = if (sex == 'male') 'karla' else 'kvenna' (matches R/publish-football-iceland.R:775,813).
- `.expected_publish_artefacts(sport, is_cup, surfaces_for) -> character()` — @noRd, R/health-publish.R. Basenames WITHOUT the .json extension. Cup-only surfaces ('bracket', 'tournament_placements') are included iff isTRUE(is_cup).
- `check_publish_freshness(leagues, root, now, th, surfaces_for = .publish_surfaces, extract_exists_fn = extract_partition_exists) -> health tibble(check, scope, status, value, threshold)` — @noRd, R/health-publish.R. check == 'publish_freshness'; scope == '<league_key> <sex> <division_code>'.
- `.publish_surfaces(sport) -> character()` — @noRd, R/health-publish.R. Thin wrapper: sport_publish_profile(sport)$surfaces. NO fallback — a sport with no profile aborts, deliberately.
- `check_publish_format_agreement(leagues, root) -> health tibble` — @noRd, R/health-publish.R. check == 'publish_format'; WARN only, never FAIL.
- `check_season_resolution(leagues, root, now, resolvers = .federation_resolvers()) -> health tibble` — @noRd, R/health-season.R. check == 'season_resolution'; scope == '<federation> <sex> <division>' for a gap row, '<federation>' for the OK row.
- `.federation_resolvers() -> named list` — @noRd, R/health-season.R. list(hsi = list(current = hsi_current_season, unresolved = hsi_unresolved_seasons, league_divisions = c('div1','div2')), kki = list(current = kki_current_season, unresolved = kki_unresolved_seasons, league_divisions = c('div1','div2'))). Keyed by the data_source$results prefix.
- `pipeline_health(root, now, leagues)` gains three rows-producing calls inside its existing safe() wrapper (R/health.R:537-550). Signature UNCHANGED.
- `run_fit_targets(targets, leagues, force, league_named, root = here::here('data'), fit_fn = fit_one, skip_fn = fit_skip_reason) -> list(fitted = integer(1), skipped = integer(1), failed = tibble(key, sex, message))` — exported, R/model-league.R (next to fit_one at :335).
- `run_publish_targets(targets, leagues, root = here::here('data'), publish_fn = publish_one) -> list(published = integer(1), failed = tibble(key, sex, message))` — exported, R/publish-pipeline.R (next to publish_one at :57).
- scripts/03_fit.R and scripts/05_publish.R become thin callers that quit(status = 1L) when failed has any row.
- .github/workflows/fit.yml 'Commit if beliefs changed' step gains `if: always()`.

### Task 0: Integration decisions for WS12 (read before Task 1 — these override any contradicting instruction)

**Files:**

- /Users/brynjolfurjonsson/sports/docs/superpowers/plans/<plan-b>.md (record these in the plan's 'Integration decisions' section; no code)

- [ ] INT-1. The checks iterate the `leagues` OBJECT PASSED IN, never `.iceland_division_*()` / `load_leagues()`. Rationale: `.football_iceland_division_codes(sex)` and its three siblings (R/extract-football-iceland.R:40-100) each call `load_leagues()` internally with no injection seam, so using them would make every health test read the real config/leagues.yml and stop being hermetic. `pipeline_health(leagues = ...)` already threads a leagues object (R/health.R:528-535) and test-health.R:144 already exploits it. Consequence: WS12 does NOT depend on WS7's `.iceland_division_*` rename, only on the config keys.

- [ ] INT-2. Exit non-zero when ANY target failed, not only when ALL failed. The task brief for this workstream says 'exiting non-zero only if ALL targets failed'; that contradicts (a) spec §13 Guard 1 ('quit(status = 1L) at the end if the failure list is non-empty') and (b) this workstream's own verification (c), which injects ONE basketball failure, requires football's fits to complete, AND requires the run to exit non-zero — which is impossible under an all-failed rule. An all-failed rule would also recreate exactly the warn-and-exit-0 hole B4 lived in. Implement ANY-failed -> exit 1. If the orchestrator wants the narrower rule, it must say so explicitly and accept that partial fit failures go green.

- [ ] INT-3. Artefact-set comparison is asymmetric: a MISSING expected artefact is FAIL (the platform 404s on it); an UNEXPECTED extra file is WARN (it is stale output from an older shape — e.g. the June-dated final_positions_history.json still sitting in data/publish/basketball/iceland/karla/). Spec §13 says 'does not hold exactly the artefact set'; this splits that one sentence into two severities rather than weakening it. State the reasoning in the roxygen block.

- [ ] INT-4. HSÍ cup/playoffs are WARN, not FAIL, in check_season_resolution. Measured today: hsi_unresolved_seasons(2027L) returns exactly 3 rows — male cup, male playoffs, female playoffs — because HSÍ does not create the úrslitakeppni or the 2026-27 bikar tournaments until later in the season (R/ingest-hsi-handball.R:122-134 documents this as correct deferral, and tests/testthat/test-ingest-hsi.R:264-274 pins it). FAILing on them would make the check permanently red on day one, which is alarm fatigue, which is the failure mode this workstream exists to prevent. FAIL is scoped to the league divisions the pipeline actually needs now: c('div1','div2') for both federations.

- [ ] INT-5. Football contributes NO check_season_resolution rows. Its data_source$results is 'ksi_football' (config/leagues.yml:141), for which there is no unresolved-seasons resolver and no season registry to go stale. Emitting a fake OK row for it would imply a guarantee that does not exist.

- [ ] INT-6. check_publish_format_agreement skips is_cup cells entirely. A cup has no league table, so expected_meetings * (n_teams - 1) is meaningless there. Verified: data/publish/football/iceland/karla-bikar/ carries 12 artefacts including bracket.json and tournament_placements.json, unlike the 10 in every league cell.

- [ ] INT-7. The alert channel is a GitHub workflow-failure email. Write that in the roxygen of check_publish_freshness and in .claude/rules/settle-health.md in those words: it is signal, not a pager. healthcheck.yml runs twice daily and fails the run on overall == FAIL; there is no push notification, no escalation, and no on-call. A FAIL introduced by these checks will be noticed within ~12 hours if the maintainer reads mail, and not at all if they do not.

**Verification.** The plan document's Integration decisions section contains INT-1..INT-7 verbatim, and the orchestrator has explicitly accepted or overridden INT-2 before Task 6 is executed.

### Task 1: Threshold, timestamp parser and expected-artefact helper (pure, hermetic)

**Files:**

- Create: /Users/brynjolfurjonsson/sports/R/health-publish.R
- Modify: /Users/brynjolfurjonsson/sports/R/health.R (the health_thresholds() block, lines 9-28)
- Modify: /Users/brynjolfurjonsson/sports/DESCRIPTION (Collate field — regenerated, not hand-edited)
- Test: /Users/brynjolfurjonsson/sports/tests/testthat/test-health-publish-freshness.R (new)

- [ ] Step 1 — measure the ground truth first, do not assume it. Run: `Rscript -e 'suppressMessages(devtools::load_all(".", quiet=TRUE)); str(sport_publish_profile("football")$surfaces); str(sport_publish_profile("basketball")$surfaces)'`. Record whether the elements carry a .json extension. Every later step in this task assumes BASENAMES WITHOUT the extension; if WS9 shipped them with '.json', adapt `.expected_publish_artefacts()` (strip with tools::file_path_sans_ext) and say so in a comment rather than silently diverging. Also run `for d in data/publish/football/iceland/*/; do echo "$(basename $d): $(ls $d | wc -l)"; done` — the expected answer today is 10 files for every cell except karla-bikar and kvenna-bikar, which have 12 (bracket.json + tournament_placements.json).

- [ ] Step 2 — write the failing test. Create tests/testthat/test-health-publish-freshness.R with a heredoc (`cat > tests/testthat/test-health-publish-freshness.R <<'EOF'` … `EOF`), NOT the Write/Edit tools — the file will later carry Icelandic division labels. First three test_that blocks: (a) `expect_true('publish_max_age_hours' %in% names(health_thresholds()))`; (b) `expect_equal(format(.parse_publish_stamp('2026-09-02T22:27:08+0000'), '%Y-%m-%d %H:%M:%S', tz='UTC'), '2026-09-02 22:27:08')` and the same for the 'Z' shape '2026-09-02T22:27:08Z', plus `expect_true(is.na(.parse_publish_stamp('not a stamp')))`; (c) `expect_setequal(.expected_publish_artefacts('football', is_cup = FALSE, surfaces_for = function(s) c('meta','standings','bracket','tournament_placements')), c('meta','standings'))` and with is_cup = TRUE the same call returns all four.

- [ ] Step 3 — RUN it and state the RED. `Rscript -e 'devtools::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-health-publish-freshness.R")'`. EXPECTED RED, exactly: block (a) fails with `"publish_max_age_hours" %in% names(health_thresholds()) is not TRUE`; blocks (b) and (c) fail with `Error in .parse_publish_stamp(...): could not find function ".parse_publish_stamp"` and `could not find function ".expected_publish_artefacts"`. If (a) passes, someone already added the threshold — stop and reconcile before continuing.

- [ ] Step 4 — implement. In R/health.R add `publish_max_age_hours = 36` to health_thresholds() with a comment recording the derivation: decide-publish commits ~4x/day (git log --oneline -3 -- data/publish/ shows 2026-09-02 at 12:37Z, 18:00Z, 22:27Z), so 36h is a full day of missed runs plus slack; it is a judgement call, not a measurement. Create R/health-publish.R starting with `#' @include health.R publish-pipeline.R` + `NULL`, and implement `.parse_publish_stamp()` (try '%Y-%m-%dT%H:%M:%S%z' then '%Y-%m-%dT%H:%M:%SZ' with tz='UTC', return NA_POSIXct on both failing), `.publish_cell_dir()` and `.expected_publish_artefacts()` per the Produces signatures.

- [ ] Step 5 — regenerate collation and re-run. `Rscript -e 'devtools::document()'` (DESCRIPTION carries a Collate field at line 47, so a new R/ file MUST be documented in, never hand-added), then re-run the test file. EXPECTED GREEN: 3 passed, 0 failed. Then run the full suite once — `Rscript -e 'devtools::test()'` — and confirm FAIL 0 against Plan A's baseline (FAIL 0 / SKIP 45, observed at the close of Plan A).

- [ ] Step 6 — commit. `git -C /Users/brynjolfurjonsson/sports add R/health.R R/health-publish.R DESCRIPTION NAMESPACE tests/testthat/test-health-publish-freshness.R && git -C /Users/brynjolfurjonsson/sports commit -m 'feat(health): publish-freshness threshold, stamp parser and artefact-set helper' ...`. Do NOT push.

**Verification.** devtools::test() is FAIL 0. .parse_publish_stamp() round-trips both stamp shapes actually present on disk (the publish writers use %z at R/publish-football-iceland.R:929 and R/publish-basketball-iceland.R:102; write_health_status uses a literal Z at R/health.R:588) — a parser that handled only one would silently NA every meta.json and turn the whole check into a false FAIL.

### Task 2: check_publish_freshness — and the in-season-with-no-partition state FAILs

**Files:**

- Modify: /Users/brynjolfurjonsson/sports/R/health-publish.R
- Test: /Users/brynjolfurjonsson/sports/tests/testthat/test-health-publish-freshness.R (append)
- Modify: /Users/brynjolfurjonsson/sports/tests/testthat/test-fixture-skip-hygiene.R (add the new file to `guarded`)

- [ ] Step 1 — write the failing tests. Append to tests/testthat/test-health-publish-freshness.R (heredoc, not Write/Edit). Shared fixture helper, defined at the top of the appended block:
```r
.bb_league <- function() list(basketball_iceland = list(
  sport = 'basketball', country = 'iceland', sexes = list('male'), active = TRUE,
  publish_divisions = list(male = list(
    list(code = 'BD', slug = 'bd', label_is = 'Bónusdeild', is_cup = FALSE)))))
.seed_upcoming <- function(root, sport = 'basketball') write_table(tibble::tibble(
  sport = sport, country = 'iceland', sex = 'male', season = 2027L,
  match_date = Sys.Date() + 3L, home_team = 'A', away_team = 'B',
  division = 'BD', round = 1L, kickoff_time = NA_character_), 'schedules', root = root)
.seed_cell <- function(root, stamp, files = c('meta','standings')) { d <- file.path(root, 'publish', 'basketball', 'iceland', 'karla-bd'); dir.create(d, recursive = TRUE, showWarnings = FALSE); for (f in files) jsonlite::write_json(list(generated_at = stamp), file.path(d, paste0(f, '.json')), auto_unbox = TRUE); d }
.surf <- function(sport) c('meta','standings')
```
Note the time-bomb rule: `Sys.Date() + 3L`, because has_upcoming_games() reads Sys.Date() internally (R/pipeline-freshness.R:115) and ignores any injected now.
Seven test_that blocks:
(1) fresh cell -> OK. Seed schedules + a cell stamped `format(now - 3600, '%Y-%m-%dT%H:%M:%S%z')`; assert status == 'OK'.
(2) missing meta.json -> FAIL naming the cell. Seed schedules, create the directory with standings.json only; assert status == 'FAIL' and `expect_match(res$scope, 'basketball_iceland male BD')`.
(3) THE CORE CASE — nothing on disk at all AND no extract partition -> FAIL, not PAUSED. Seed schedules only (no publish dir), pass `extract_exists_fn = function(...) FALSE`; assert `expect_equal(res$status, 'FAIL')`, `expect_false(res$status == 'PAUSED')` and `expect_match(res$value, 'no extract partition')`. Add a comment: this is the state the pipeline was in from cutover to 2026-09; a check that calls it PAUSED re-hides B4.
(4) stale generated_at -> FAIL. Stamp at `now - 50*3600` against publish_max_age_hours = 36; assert FAIL and `expect_match(res$value, 'h old')`.
(5) no upcoming games -> PAUSED. Do NOT seed schedules; assert status == 'PAUSED' and `expect_match(res$value, 'no upcoming games')`.
(6) missing artefact -> FAIL. Fresh stamp but files = 'meta' only against surfaces c('meta','standings'); assert FAIL and `expect_match(res$value, 'standings')`.
(7) unexpected extra artefact -> WARN (INT-3). Fresh stamp, files = c('meta','standings','final_positions_history'); assert status == 'WARN' and `expect_match(res$value, 'final_positions_history')`.
Every call is `check_publish_freshness(.bb_league(), root, now, health_thresholds(), surfaces_for = .surf, extract_exists_fn = function(...) TRUE)` except (3).

- [ ] Step 2 — RUN and state the RED. `Rscript -e 'devtools::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/test-health-publish-freshness.R")'`. EXPECTED RED: all seven new blocks error with `Error in check_publish_freshness(...): could not find function "check_publish_freshness"`. The three blocks from Task 1 stay green — if they do not, Task 1 regressed.

- [ ] Step 3 — implement in R/health-publish.R. Loop `for (key in names(leagues))`; skip unless `isTRUE(lg$active)`; for each sex in `.cell_sexes(lg)` (R/health.R:47) and each entry of `lg$publish_divisions[[sex]]` (INT-1 — read it off the object, do not call load_leagues()). Per cell, in this order: (a) `has_upcoming_games(list(sport = lg$sport, country = lg$country), sex, root = root)` FALSE -> PAUSED row, next; (b) `dir <- .publish_cell_dir(root, lg$sport, sex, d$slug)`; if the directory or meta.json is absent -> FAIL, and when `extract_exists_fn(file.path(root,'beliefs','extracts'), lg$sport, lg$country, sex)` is also FALSE make the value read 'no publish output and no extract partition (in-season)' so the message names the real cause; (c) parse generated_at, NA or age > th$publish_max_age_hours -> FAIL with the age in hours; (d) compare `tools::file_path_sans_ext(list.files(dir, pattern = '[.]json$'))` against `.expected_publish_artefacts(lg$sport, isTRUE(d$is_cup), surfaces_for)` — missing -> FAIL, extras only -> WARN (INT-3); (e) otherwise OK. Wrap every filesystem read in tryCatch so a corrupt meta.json becomes a FAIL row, never an abort — pipeline_health()'s safe() wrapper (R/health.R:533-536) would otherwise collapse the entire check into one check_error row and lose the per-cell scope. Roxygen must carry INT-3's reasoning and INT-7's honest-limit sentence.

- [ ] Step 4 — register the file with the hygiene guard. Add 'test-health-publish-freshness.R' to the `guarded` vector in tests/testthat/test-fixture-skip-hygiene.R:6-14. The block's expect_setequal (lines 19-22) requires every listed file except WS3's to exist, so this lands in the same commit as the file itself. Confirm the new tests contain no `skip(`, `skip_if(`, `Sys.getenv`.

- [ ] Step 5 — re-run. `testthat::test_file(...)` -> 10 passed, 0 failed; then `testthat::test_file("tests/testthat/test-fixture-skip-hygiene.R")` -> green; then `devtools::document()` and `devtools::test()` -> FAIL 0.

- [ ] Step 6 — commit: 'feat(health): check_publish_freshness FAILs an in-season cell with no publish output'.

**Verification.** Test (3) is the workstream's reason to exist and must be read as a behavioural claim, not a coverage tick: with an in-season active cell, no publish directory and no extract partition, the returned status is exactly 'FAIL'. Re-run it once with the implementation deliberately returning 'PAUSED' for that branch and confirm the test goes red — a test that passes under both answers proves nothing.

### Task 3: check_season_resolution — the alarm for the state the pipeline was in

**Files:**

- Create: /Users/brynjolfurjonsson/sports/R/health-season.R
- Test: /Users/brynjolfurjonsson/sports/tests/testthat/test-health-season-resolution.R (new)
- Modify: /Users/brynjolfurjonsson/sports/tests/testthat/test-fixture-skip-hygiene.R (add to `guarded`)
- Modify: /Users/brynjolfurjonsson/sports/DESCRIPTION (Collate, regenerated)

- [ ] Step 1 — write the failing tests (heredoc). Build the leagues object from data_source, because that is the dispatch key: `list(handball_iceland = list(sport='handball', country='iceland', sexes=list('male','female'), active=TRUE, data_source=list(results='hsi_handball')))`. Inject resolvers rather than touching the real registries:
```r
.res <- function(unresolved_rows) list(hsi = list(
  current = function(today) 2027L,
  unresolved = function(season, ...) unresolved_rows,
  league_divisions = c('div1','div2')))
```
Five test_that blocks:
(1) no gaps -> a single OK row. unresolved_rows is the zero-row tibble(sex=character(), division=character(), season=integer()); assert nrow(res) == 1L and res$status == 'OK'.
(2) a div1 gap -> FAIL naming sex, division and season. Assert status == 'FAIL' and `expect_match(res$scope, 'hsi male div1')` and `expect_match(res$value, '2027')`.
(3) INT-4 — a cup/playoffs-only gap -> WARN, not FAIL. unresolved_rows = the three real rows (male cup, male playoffs, female playoffs); assert `expect_true(all(res$status %in% c('OK','WARN')))` and `expect_false(any(res$status == 'FAIL'))`. Comment it with the measurement: hsi_unresolved_seasons(2027L) returns exactly these three rows today, so a FAIL here would be permanently red from day one.
(4) an inactive league contributes no rows. Set active = FALSE; assert nrow(res) == 0L.
(5) INT-5 — football contributes no rows. leagues = list(football_iceland = list(sport='football', country='iceland', sexes=list('male'), active=TRUE, data_source=list(results='ksi_football'))); assert nrow(res) == 0L.

- [ ] Step 2 — RUN and state the RED: every block errors with `Error in check_season_resolution(...): could not find function "check_season_resolution"`.

- [ ] Step 3 — implement R/health-season.R (`#' @include health.R ingest-hsi-handball.R ingest-kki-basketball.R` + NULL). `.federation_resolvers()` returns the two-entry list per the Produces signature. `check_season_resolution()` maps each active league's `lg$data_source$results` to a federation by prefix — 'hsi_' -> hsi, 'kki_' -> kki, anything else -> no rows (INT-5) — de-duplicates by federation (both sexes of one league share a registry), calls `current(as.Date(now))` then `unresolved(season)`, splits the returned rows on `division %in% league_divisions`, and emits: one FAIL row per league-division gap; one WARN row per deferred cup/playoffs gap; one OK row per federation when there are no gaps at all. Roxygen: cite R/ingest-hsi-handball.R:122-134 for why cup/playoffs deferral is correct, and state that this check is what distinguishes 'the season is genuinely over' from 'the scraper went blind in October' (spec §13, residual risk at spec line 769).

- [ ] Step 4 — add 'test-health-season-resolution.R' to `guarded` in tests/testthat/test-fixture-skip-hygiene.R.

- [ ] Step 5 — `devtools::document()`; `testthat::test_file(...)` -> 5 passed 0 failed; `devtools::test()` -> FAIL 0.

- [ ] Step 6 — the real-registry smoke check, run by hand and recorded in the commit message: `Rscript -e 'suppressMessages(devtools::load_all(".", quiet=TRUE)); print(check_season_resolution(load_leagues(), here::here("data"), Sys.time()))'`. EXPECTED as of 2026-09-04: kki OK, hsi WARN naming male cup / male playoffs / female playoffs, and NO FAIL row. A FAIL here today means either the registry regressed or INT-4 was mis-implemented — investigate before committing, do not relax the check.

- [ ] Step 7 — commit: 'feat(health): check_season_resolution FAILs an unresolvable league season, WARNs deferred cups'.

**Verification.** Spec verification (b), executed for real: temporarily delete the '2027' key from HSI_TOURNAMENT_IDS$male$div1 (R/ingest-hsi-handball.R:29ff), reload, and confirm check_season_resolution against the REAL registry returns a FAIL row scoped 'hsi male div1'. Restore the key and confirm the FAIL disappears. Both halves — a check that cannot go green again is as useless as one that cannot go red.

### Task 4: check_publish_format_agreement — WARN on a changed competition format

**Files:**

- Modify: /Users/brynjolfurjonsson/sports/R/health-publish.R
- Test: /Users/brynjolfurjonsson/sports/tests/testthat/test-health-format-agreement.R (new)

- [ ] Step 1 — confirm the inputs exist before writing a line of test. `Rscript -e 'cat(readLines("data/publish/football/iceland/karla-bd/meta.json"), sep="\n")'` and check for `n_rounds`, `n_rounds_source` and `qualify`. Today's on-disk meta has NONE of them — it reads `{"sport":"football","sex":"male","league":"Besta deild","division":"BD","is_cup":false,"season":2026,"generated_at":...,"fit_date":...,"round":21,"n_draws":4000,"split":{...}}`. If they are still absent, WS10 has not landed and this task MUST wait; do not invent the keys.

- [ ] Step 2 — write the failing tests (heredoc). Fixture: a cell directory with meta.json carrying `n_rounds`, `n_rounds_source` and `qualify = list(slots = 8)`, plus standings.json carrying a `teams` array of length n (n_teams is read as the standings row count — the cheapest honest source). Leagues object carries publish_divisions entries with `expected_meetings` and `qualify_slots`. Five blocks: (1) derived n_rounds == expected_meetings * (n_teams - 1) -> OK; (2) disagreement with n_rounds_source == 'schedule' -> WARN whose value names BOTH numbers (`expect_match(res$value, '22')` and `expect_match(res$value, '44')`); (3) disagreement with n_rounds_source == 'config' -> OK (config is the fallback; it cannot disagree with itself); (4) qualify_slots >= n_teams -> WARN; (5) INT-6 — an is_cup cell produces no row.

- [ ] Step 3 — RUN and state the RED: `could not find function "check_publish_format_agreement"` for all five.

- [ ] Step 4 — implement in R/health-publish.R. Same cell loop as check_publish_freshness but with no freshness or upcoming-games logic: skip is_cup cells (INT-6), skip a cell whose meta.json is absent or unreadable (check_publish_freshness owns that failure — two checks reporting one fault is noise), skip a cell whose config entry lacks expected_meetings or qualify_slots (both are optional in WS7's schema extension). Never escalate past WARN: a competition format change is a thing to look at, not an outage. Roxygen must record the concrete motivating number — Icelandic women's handball plays a TRIPLE round robin (8 teams, 21 rounds, 84 matches = 3 x 28), so `2 * (n_teams - 1)` is wrong for four of the seven measured 2DT cells, which is why n_rounds is published upstream at all.

- [ ] Step 5 — `devtools::test()` -> FAIL 0. Commit: 'feat(health): WARN when a published cell disagrees with its configured format'.

**Verification.** Block (2) asserts the WARN message names both the derived and the configured number. A WARN that says only 'n_rounds disagrees' costs a diagnostic round trip every time it fires; the whole value of this check is that the two numbers appear side by side in the healthcheck output.

### Task 5: Compose the three checks into pipeline_health, and update the docs that describe it

**Files:**

- Modify: /Users/brynjolfurjonsson/sports/R/health.R (pipeline_health at :528-551 and its roxygen at :509-527)
- Modify: /Users/brynjolfurjonsson/sports/.claude/rules/settle-health.md (the 'Health & monitoring (2026-05-30)' block at lines 43-61)
- Modify: /Users/brynjolfurjonsson/sports/.claude/skills/pipeline-doctor/SKILL.md (the triage table)
- Create: /Users/brynjolfurjonsson/sports/docs/runbooks/stale-publish.md
- Test: /Users/brynjolfurjonsson/sports/tests/testthat/test-health.R (append)

- [ ] Step 1 — write the failing test. Append to tests/testthat/test-health.R: a block seeding a tempdir with an in-season basketball cell that has no publish output, calling `pipeline_health(root = root, now = Sys.time(), leagues = <the bb league object with publish_divisions>)`, and asserting `expect_true(any(out$check == 'publish_freshness'))`, `expect_true(any(out$check == 'season_resolution'))`, `expect_true(any(out$check == 'publish_format'))` and `expect_equal(overall_health_status(out), 'FAIL')`. Also assert the existing shape invariant still holds: `expect_true(all(out$status %in% c('OK','WARN','FAIL','PAUSED')))` — the same assertion test-health.R:44 already makes.

- [ ] Step 2 — RUN and state the RED: `any(out$check == "publish_freshness") is not TRUE` (and the same for the other two check names). The row-count and shape assertions pass — only the membership ones fail. This is the proof that the composition, not the check, is what is missing.

- [ ] Step 3 — implement. Add three `safe(...)` entries to the dplyr::bind_rows() call at R/health.R:537-550, after check_discovery so the new rows sort last in the printed table: `safe(check_publish_freshness(leagues, root, now, th))`, `safe(check_season_resolution(leagues, root, now))`, `safe(check_publish_format_agreement(leagues, root))`. Update the roxygen at :510-513 — it currently enumerates the composition and says nothing reads data/publish/, which becomes false in this commit.

- [ ] Step 4 — docs, same commit (CLAUDE.md: doc changes land with the code they describe). (a) .claude/rules/settle-health.md: extend the composition sentence to eleven checks and add one paragraph naming the three, INCLUDING INT-7's honest-limit sentence in those words — the alert channel is a GitHub workflow-failure email; that is signal, not a pager. (b) pipeline-doctor SKILL.md triage table: three rows — `publish_freshness` FAIL -> 'publish skipped or stale; extracts missing' -> docs/runbooks/stale-publish.md; `season_resolution` FAIL -> 'federation season id unresolvable' -> docs/runbooks/season-restart.md (it already exists); `publish_format` WARN -> 'competition format changed' -> docs/runbooks/stale-publish.md. (c) Write docs/runbooks/stale-publish.md in the house symptom -> diagnose -> fix -> verify shape used by docs/runbooks/metill-platform-desync.md, and make the FIRST diagnostic step the one that would have caught B4: `ls data/beliefs/extracts/` — if there is no `sport=<s>` partition for the failing cell, the fit ran but the extractor did not, and the publisher has nothing to read. Second step: confirm you are on a branch that is in sync with main, because a feature branch behind main shows every football cell as stale for branch reasons, not pipeline reasons (measured 2026-09-04: local publish JSONs stamped 2026-09-02T22:27Z, i.e. ~35h old on a branch, while main had moved on).

- [ ] Step 5 — run the REAL healthcheck and read the output before believing it: `Rscript scripts/07_healthcheck.R`. Football's four upcoming-games cells will report against on-disk publish output; bb/hb will report PAUSED until schedules repopulate (has_upcoming_games() is FALSE for all four bb/hb cells today). If football reports FAIL on age, check `git -C /Users/brynjolfurjonsson/sports log --oneline -1 origin/main -- data/publish/` FIRST — a stale branch is not a stale pipeline, and tuning publish_max_age_hours to silence a branch artefact would disarm the check.

- [ ] Step 6 — `devtools::test()` -> FAIL 0. Commit: 'feat(health): compose publish + season checks into pipeline_health'.

**Verification.** pipeline_health() on a tempdir with an in-season basketball cell and no publish output returns overall == 'FAIL'. Before this commit the same input returns 'OK' — that delta is the whole workstream in one assertion, and it should be captured in the commit message as the before/after pair.

### Task 6: Fit-loop isolation: run_fit_targets() + a thin 03_fit.R + fit.yml if: always()

**Files:**

- Modify: /Users/brynjolfurjonsson/sports/R/model-league.R (add run_fit_targets() next to fit_one at :335-338)
- Modify: /Users/brynjolfurjonsson/sports/scripts/03_fit.R (replace the loop at :32-54)
- Modify: /Users/brynjolfurjonsson/sports/.github/workflows/fit.yml (the 'Commit if beliefs changed' step at :78)
- Test: /Users/brynjolfurjonsson/sports/tests/testthat/test-pipeline-run-isolation.R (new)

- [ ] Step 1 — write the failing tests (heredoc). The policy lives in R/ precisely so it is testable without spawning Rscript; this mirrors the reasoning already written at the top of tests/testthat/test-script-ledger-commit.R:9-14. Blocks:
(1) THE CORE CASE — config order is basketball -> handball -> football (config/leagues.yml:15, :86, :120ish), so a basketball abort must not reach football. targets = data.frame(key = c('basketball_iceland','football_iceland'), sex = 'male'); `fit_fn = function(static, sex) if (static$sport == 'basketball') stop('divergent transitions after warmup') else 1L`; `skip_fn = function(...) NULL`. Assert `res$fitted == 1L`, `nrow(res$failed) == 1L`, `res$failed$key == 'basketball_iceland'` and `expect_match(res$failed$message, 'divergent')`.
(2) all-green: no failures, fitted counts every target.
(3) skip_fn returning a reason increments skipped and never calls fit_fn (assert with a counter in the closure).
(4) INT-2: `nrow(res$failed) > 0L` for a partial failure — the value scripts/03_fit.R turns into exit 1.
(5) static guard on the script, in the style of test-script-ledger-commit.R: read scripts/03_fit.R and assert it contains 'run_fit_targets' and 'quit(save = "no", status = 1L)', and that it does NOT contain a bare `fit_one(static, row$sex)` call.
(6) static guard on the workflow: read .github/workflows/fit.yml, find the 'Commit if beliefs changed' step, assert an `if: always()` line precedes its `run:`. Do this by locating the step index and scanning forward, not by a whole-file grep for 'always()' — a whole-file grep would pass on an `if: always()` attached to any other step, which is the exact bug the assertion exists to catch.

- [ ] Step 2 — RUN and state the RED: blocks 1-4 fail with `Error in run_fit_targets(...): could not find function "run_fit_targets"`; block 5 fails with the script-content assertion (`... is not TRUE` on the 'run_fit_targets' grep); block 6 fails on the missing `if: always()`.

- [ ] Step 3 — implement run_fit_targets() in R/model-league.R. Signature per Produces. Body: the loop currently at scripts/03_fit.R:32-54 moved verbatim — the same `static <- league_def[c('sport','country','sexes','active','stan_model','data_source')]` slice, the same `skip_fn(static, row$sex, force, league_named)` call, the same cli messages — with `fit_fn(static, row$sex)` wrapped in `tryCatch(..., error = function(e) {...})` that appends `tibble::tibble(key = row$key, sex = row$sex, message = conditionMessage(e))` to a failures list, emits `cli::cli_alert_danger()` naming the cell, and continues. Return list(fitted, skipped, failed). Roxygen must state WHY: fit_model() aborts on a diagnostics-gate breach and fit_skip_reason()'s own docstring (R/pipeline-freshness.R:134-137) records real off-season basketball R-hat/ESS breaches, so the first live 2DT fits in five months are the highest abort-risk event of the season — and they run BEFORE football.

- [ ] Step 4 — rewrite scripts/03_fit.R's loop as: `res <- run_fit_targets(targets, leagues, force = opts$force, league_named = league_named)`, then a success line, then per INT-2 `if (nrow(res$failed) > 0L) { cli::cli_alert_danger('...'); print(as.data.frame(res$failed)); quit(save = 'no', status = 1L) }`. Keep the existing `nrow(targets) == 0L` early exit at :22-25 untouched.

- [ ] Step 5 — edit fit.yml: add `if: always()` to the 'Commit if beliefs changed' step (line 78) so successful fits still commit while the run goes red. Leave the existing `git add data/beliefs/latest/ data/beliefs/archive/ data/beliefs/extracts/` line at :80 exactly as it is — N1 established that the extracts tree, which is the publish input, is ALREADY committed by this step, and this workstream requires zero other workflow changes.

- [ ] Step 6 — `devtools::document()`; `testthat::test_file('tests/testthat/test-pipeline-run-isolation.R')` -> 6 passed 0 failed; `devtools::test()` -> FAIL 0. Then the end-to-end proof, which must be RUN, not reasoned about: `Rscript scripts/03_fit.R --league football_iceland --sex male` on the real repo, confirming it still fits and exits 0 (`echo "exit=$?"` on its own line — never gate it behind a pipe into grep, since a pipeline returns the filter's status).

- [ ] Step 7 — commit: 'feat(fit): isolate per-target fit failures so one 2DT abort cannot kill football'.

**Verification.** Spec verification (c), all three properties and not just the last: with fit_fn injected to abort on basketball, (i) football's target still fits (res$fitted == 1L), (ii) res$failed names basketball, (iii) the script exits non-zero on that failed frame. Plus the workflow half: with the fit step failing, fit.yml's commit step still runs — assert it structurally (block 6), because the only real proof is a CI run and this branch is never pushed.

### Task 7: Publish-loop isolation: run_publish_targets() + a thin 05_publish.R

**Files:**

- Modify: /Users/brynjolfurjonsson/sports/R/publish-pipeline.R (add run_publish_targets() next to publish_one at :57)
- Modify: /Users/brynjolfurjonsson/sports/scripts/05_publish.R (replace the loop at :25-35)
- Test: /Users/brynjolfurjonsson/sports/tests/testthat/test-pipeline-run-isolation.R (append)

- [ ] Step 0 — SEAM CHECK before writing anything. WS11's verification (b) also claims a per-cell tryCatch around publish_one ('proving the subtree fix and the per-cell tryCatch together'). Run `grep -n 'tryCatch\|run_publish_targets' /Users/brynjolfurjonsson/sports/scripts/05_publish.R`. If WS11 already wrapped the loop, do NOT reimplement it: keep this task's tests (they are the behavioural proof WS11's verification leans on) and delete the implementation steps. If it is still the bare `publish_one(static, betting, row$key, row$sex)` call at :34, proceed.

- [ ] Step 1 — write the failing tests (heredoc), appended to test-pipeline-run-isolation.R. Blocks: (1) targets = basketball then football; `publish_fn = function(static, betting, key, sex, ...) if (key == 'basketball_iceland') stop('schema validation failed: standings.json missing required key') else invisible(NULL)`; assert `res$published == 1L`, `nrow(res$failed) == 1L`, `res$failed$key == 'basketball_iceland'`. (2) all-green path. (3) static guard: scripts/05_publish.R contains 'run_publish_targets' and 'quit(save = "no", status = 1L)' and no bare `publish_one(static, betting, row$key, row$sex)`.

- [ ] Step 2 — RUN and state the RED: `Error in run_publish_targets(...): could not find function "run_publish_targets"`, plus the script-content assertion failing.

- [ ] Step 3 — implement run_publish_targets() in R/publish-pipeline.R, mirroring run_fit_targets(): same target loop, the same `static <- league_def[c('sport','country','sexes','active','stan_model','data_source')]` slice and `betting <- league_def$betting` that scripts/05_publish.R:27-31 uses today, publish_fn wrapped in tryCatch, failures recorded and the loop continued. Roxygen must record the asymmetry the spec calls out: WS11 inverts .validate_or_abort()'s default to fail-closed, and without this guard that change converts today's harmless bb/hb skip into a FOOTBALL outage, because scripts/05_publish.R:34 calls publish_one() bare and basketball precedes football in config order.

- [ ] Step 4 — rewrite scripts/05_publish.R's loop to call run_publish_targets() and quit(status = 1L) on any failure (INT-2, same rule as the fit script — a partially failed publish that exits 0 is the exact hole this workstream closes).

- [ ] Step 5 — `devtools::document()`; `testthat::test_file(...)` -> 9 passed 0 failed; `devtools::test()` -> FAIL 0. Then the real proof, run on the repo: `Rscript scripts/05_publish.R --league football_iceland` and confirm exit 0 and that the football JSONs still validate. Cross-check against Plan A's golden net: `testthat::test_file('tests/testthat/test-publish-football-golden.R')` must be green — the sha256 hashes in tests/testthat/fixtures/golden/football-publish-hashes.csv are the regression net for all 10 JSONs x 9 football cells, and nothing in WS12 has any business moving them.

- [ ] Step 6 — commit: 'feat(publish): isolate per-cell publish failures so a bb/hb abort cannot stop football republishing'.

**Verification.** Spec verification (d): with publish_fn injected to abort on basketball, football's cell still publishes and the failure frame names basketball. Plus the golden-file test stays byte-green — if any football hash moved, WS12 changed football output, which it must not; stop and find out why rather than regenerating the golden file.

### Task 8: Whole-workstream verification pass and the honest-limit note

**Files:**

- Modify: /Users/brynjolfurjonsson/sports/.claude/rules/settle-health.md
- Modify: /Users/brynjolfurjonsson/sports/docs/runbooks/README.md (index the new runbook)
- Read-only: /Users/brynjolfurjonsson/sports/data/health/status.json

- [ ] Step 1 — full suite, stamped. `Rscript -e 'devtools::test()' 2>&1 | tail -20`. Record the FAIL / WARN / SKIP counts AND the date-time observed, per the state-claim rule: a bare '222 passed' rots within hours. Baseline to beat: Plan A closed at FAIL 0 / SKIP 45.

- [ ] Step 2 — the real healthcheck, end to end. `Rscript scripts/07_healthcheck.R` from the repo root, then `python3 -m json.tool data/health/status.json | head -60`. Confirm the three new check names appear in `checks`, that `overall` is what the data actually justifies, and that no check errored into a `check_error` row (R/health.R:534) — a check_error means the new code aborted inside safe() and the per-cell scope was lost.

- [ ] Step 3 — the false-positive audit, which is the part most likely to be skipped and most likely to matter. For every FAIL and WARN row the run produces, decide in writing whether it is real. Known-benign states as of 2026-09-04: hsi WARN for male cup / male playoffs / female playoffs (INT-4, deferred by the federation); bb/hb publish_freshness PAUSED (has_upcoming_games() is FALSE for all four cells); football publish_freshness possibly stale-FAIL purely because this branch is behind main (local publish JSONs stamped 2026-09-02T22:27Z). Anything else is a real finding — fix the pipeline, not the threshold.

- [ ] Step 4 — write the honest-limit note into .claude/rules/settle-health.md alongside the composition paragraph, in these words: the alert channel is a GitHub workflow-failure email, which is signal, not a pager; healthcheck.yml runs twice daily and fails the run on overall == FAIL; there is no push notification and no escalation, so a FAIL is noticed within roughly twelve hours if the maintainer reads mail and not at all if they do not. Add the corollary that follows from it: because the channel is low-bandwidth, a check that is permanently WARN is worse than no check, which is why INT-4 scopes FAIL to the league divisions and leaves federation-deferred cups at WARN.

- [ ] Step 5 — add the stale-publish runbook row to docs/runbooks/README.md so /pipeline-doctor's triage table and the index agree.

- [ ] Step 6 — final git hygiene. `git -C /Users/brynjolfurjonsson/sports fetch origin && git -C /Users/brynjolfurjonsson/sports status` and `git -C /Users/brynjolfurjonsson/sports log --oneline origin/main..HEAD`. Commit the doc changes: 'docs(health): runbook + rules for the publish and season checks'. DO NOT PUSH — Plan A and Plan B both stay local on feat/bb-hb-metill-parity.

**Verification.** Three artefacts exist and agree with each other: data/health/status.json contains publish_freshness, season_resolution and publish_format rows; .claude/rules/settle-health.md describes eleven composed checks and states the email-not-pager limit; docs/runbooks/stale-publish.md's first diagnostic step is `ls data/beliefs/extracts/`. Every FAIL/WARN in the real run has a written adjudication as real or benign — an unadjudicated WARN on day one becomes an ignored WARN by day thirty.

**Risks.**

- INT-2 CONTRADICTS THE TASK BRIEF and needs an explicit ruling before Task 6 ships. The brief says 'exiting non-zero only if ALL targets failed'; spec §13 says exit non-zero 'if the failure list is non-empty', and this workstream's own verification (c) requires a run where football succeeds, basketball fails, and the exit is non-zero — which the all-failed rule makes impossible. I have specified ANY-failed. If the orchestrator wants the brief's rule, it must say so and accept that a partial fit failure exits 0, which is the warn-and-exit-0 shape B4 lived in for months.
- HARD ORDERING DEPENDENCY: WS12 cannot land before WS9. `.publish_surfaces()` consumes sport_publish_profile(sport)$surfaces and check_publish_freshness consumes extract_partition_exists(); both are WS9 deliverables. The unit tests inject stubs and stay hermetic, but Task 1 Step 1 and Task 5 Step 5 call the real functions. Task 4 additionally cannot run before WS10 — verified today that data/publish/football/iceland/karla-bd/meta.json carries no n_rounds, no n_rounds_source and no qualify key.
- The check goes red on real data the moment bb/hb schedules repopulate and publish has not yet been fixed — which is correct behaviour and the whole point, but it means WS12 must not merge ahead of WS9/WS10, or main goes red for a reason the reader will mistake for a WS12 bug and 'fix' by loosening the check.
- publish_max_age_hours = 36 is a judgement call, not a measurement. Derived from decide-publish committing roughly four times a day (git log --oneline -3 -- data/publish/ shows 12:37Z, 18:00Z and 22:27Z on 2026-09-02). Too tight and every quiet weekend goes red; too loose and a two-day publish outage looks healthy. It should be revisited after the first month of bb/hb publishing, and the revisit should cite observed inter-commit gaps rather than intuition.
- has_upcoming_games() reads Sys.Date() internally (R/pipeline-freshness.R:115) and ignores the `now` threaded through every health check. So check_publish_freshness is NOT fully deterministic under an injected clock: PAUSED-vs-evaluated depends on the real date. Every fixture must use Sys.Date() + N, and a test asserting the PAUSED branch must seed no upcoming schedule rather than back-dating `now`.
- Task 7 overlaps WS11's verification (b), which also claims the per-cell publish tryCatch. Task 7 Step 0 is a grep-first seam check for exactly this. If both workstreams implement it independently the second one hits a merge conflict in scripts/05_publish.R; if both assume the other did it, WS11's fail-closed validation inversion ships with no guard and the first bb/hb schema breach takes football's republish down.
- An extra-artefact WARN (INT-3) will fire immediately against the stale June-dated data/publish/basketball/iceland/{karla,kvenna}/ directories, which carry 8 files under the OLD un-suffixed path shape. Those directories are scheduled for `git rm` per spec §14 correction 2. Until that lands the WARN is real but uninteresting — adjudicate it in Task 8 Step 3 rather than suppressing it in the check.
- check_season_resolution reads the live registries and the federation-seasons cache through hsi_/kki_unresolved_seasons(). Neither hits the network in this path (both are pure registry+cache lookups), so the check stays CI-safe — but if a later workstream adds live discovery to those resolvers, this check would start making HTTP calls from the healthcheck workflow. Roxygen should state the no-network requirement so that change is a deliberate one.

---
