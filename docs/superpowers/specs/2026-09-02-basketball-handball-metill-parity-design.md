# Basketball + handball: automated publishing to metill.is

**Status:** design approved 2026-09-02. Supersedes the Phase 2 half of
`docs/superpowers/designs/2026-05-26-f6-basketball-handball-extraction.md`.

## Scope and fixed decisions

Four decisions were taken before design and are requirements, not options:

- **D1 — full multi-division parity.** Eight publish cells: basketball `{BD, 1D}` x `{karla, kvenna}`,
  handball `{OD, G66}` x `{karla, kvenna}`, at football's layout
  `data/publish/<sport>/iceland/<sex_folder>-<slug>/`.
- **D2 — publish only.** No bets are placed on either sport this season. Enforced by config *and* a
  runtime flag, not by the absence of odds (see WS1).
- **D3 — regular-season projection.** The league table decides the *deildarmeistari*; the
  *Islandsmeistari* comes out of an unmodelled urslitakeppni. Encoded in the payload, not only in copy.
- **D4 — all four cells** (both sexes, both sports).

Schedule pressure was explicitly withdrawn by the user on 2026-09-02: *"No stress on having enough
time. The season won't slip. Do this calmly and slowly making sure it's fully implemented and great."*
The architecture below was re-selected under that brief; see 'Architecture selection'.

## Verified findings

Every claim below was confirmed by reading source, not inferred.

| id | finding | evidence |
|----|---------|----------|
| B1 | Ingest deadlocks on its own output | `R/ingest.R:136-140` gates on `config/active_competitions.json`, which `R/schedule-active.R:36-56` derives solely from schedule rows that ingest writes. Both sports read `false`. `--league` does not bypass; `opts$force` is parsed and never read. |
| B2 | HSI changed URL *scheme*, not just year | `HSI_URLS` pins `olis-deild-karla-2025-26` (`R/ingest-hsi-handball.R:21`); `hsi_current_season()` returns **2027** today. `https://www.hsi.is/olis-deild-karla-2026-27` returns **HTTP 404**; HSI now serves `/tournament/<id>`. |
| B3 | KKI season ids stop at 2026 | `R/ingest-kki-basketball.R:31-51`, all four `(sex, div)` branches. Fails safe (under-fetches). |
| B4 | **Publish has never run on CI** | `publish_one()` takes the extracts branch only for `football_iceland` (`R/publish-pipeline.R:105-118`); all else reads `data/beliefs/fits/.../fit.rds`, excluded by `.gitignore:48` and never produced by `decide-publish.yml`. Warn, `invisible(NULL)`, **exit 0**. Every bb/hb JSON on disk was made on a laptop. |
| B5 | 2DT home advantage is exponentiated | `R/extract-iceland-2dt-shared.R:194-216` applies `exp(x/2)`. Stan declares `vector<lower=0>[K] home_advantage_off` in **raw points** (`:112`, prior `normal(0,10)`) and `home_advantage_tot = off + def` (`:277`). A 4-point edge would publish as `exp(4)=54.6`. Never fired: `data/beliefs/extracts/` holds only `sport=football`. |
| N1 | CI already commits the extracts tree | `.github/workflows/fit.yml:80` runs `git add data/beliefs/latest/ data/beliefs/archive/ data/beliefs/extracts/`; `extracts/` is absent from `.gitignore`. **Zero workflow changes needed.** |
| N2 | Schema forbids the obvious config edits | `config/leagues.schema.json`: `betting` is `additionalProperties:false`, required `[kelly_frac, ev_threshold, markets, scoring, min_bet]`. `definitions.publishDivisionList.items` is `additionalProperties:false`, required `[code, slug, label_is, is_cup]`. |
| N3 | Both registries are fixable, not merely rotatable | HSI ids are discoverable from hsi.is nav; `https://www.hsi.is/tournament/7643` is titled **"Grill 66 deild kvenna"**, recovering the id the code calls unrecoverable. KKI exposes ids in its own URLs: `kki.is/motamal/.../Leikir?league_id=190&season_id=130403` -- and 130403 *is* the registered male div1 2026 id. |
| N4 | 2DT models **do** have per-round trajectories | `Stan/basketball_iceland/2d_student_t_scalarsigma.stan:157,164` declare `array[N_rounds] vector[K] offense`/`defense` as a random walk (`:168-173`). Earlier analysis wrongly called `round_strengths` impossible for 2DT. **bb/hb reach 10 artefacts, not 8.** |
| N5 | Autoplace is armed | `is.metill.sports.autoplace` is loaded in `launchctl`; no `data/AUTO_PLACE_DISABLED` file exists. Both leagues are `active: true` with live Lengjan comps (bb 1519/1528, hb 1269). Repopulating schedules would stake real money on handball -- contradicting D2. |
| N6 | **All KKI ids resolved live** | Read from kki.is in a browser 2026-09-02. `league_id` is stable, `season_id` rotates. Bonus karla 190 / 1.d karla 191 / Bonus kvenna 189 / 1.d kvenna 231; 2026-27 season_ids 132568 / 132571 / 132567 / 132570. **Every 2025-26 value matches the repo's existing `2026` key** (130403/130402/130422/130421), validating both the mapping and the closing-year convention. B3 needs no API probing. |
| N7 | **KKI declares a stage dimension the repo never captures** | Every league page carries a `stig` filter: `Deildarkeppni` (regular season) vs `Urslitakeppni` (playoffs) -- 190: 300475/306658, 191: 300472/306497, 189: 300530/306645 **plus `A ridill` 305952 and `B ridill` 305951**, 231: 300529/306557. The `leikdagur` filter for Bonus karla lists 1..22 then `4 lida`, `8 lida`, `Urslit`. |

### The regular-season boundary (derived from data, 2026 season)

Cutting each cell at `meetings x (n_teams - 1)` and counting how many times each *pair* meets
inside the cut is decisive, where a per-round match-count heuristic is not:

| cell | teams | total rows | regular rounds | regular matches | pair meetings inside cut |
|---|---|---|---|---|---|
| basketball male BD | 12 | 162 | 22 | 132 | all 66 pairs exactly 2x |
| basketball female BD | 10 | 137 | 18 | 90 | all 45 pairs exactly 2x |
| basketball male 1D | 12 | 159 | 22 | 132 | all 66 pairs exactly 2x |
| basketball female 1D | 11 | 98 | ~20 | 93 | **irregular**: 1x1, 2x44, 4x1 |
| handball male OD | 12 | 132 | 22 | 132 | double RR, no post-season rows |
| handball female OD | 8 | 84 | 21 | 84 | 84/28 pairs = **3 meetings** |
| handball male G66 | 12 | 132 | 22 | 132 | double RR, no post-season rows |
| handball female G66 | 8 | 84 | 21 | 84 | 84/28 pairs = **3 meetings** |

So `n_rounds = meetings x (n_teams - 1)`, with `meetings = 2` everywhere **except women's handball,
which plays a triple round-robin** (8 teams, 21 rounds). Consequences, all load-bearing:

- **`total_rounds = 2*(n_teams-1)` (`app/routes/ithrottir.py:406`) is wrong for the two women's
  handball cells** and cannot be fixed downstream -- only the producer sees the schedule. `n_rounds`
  is published upstream in `meta.json` (WS10).
- **A per-round match-count heuristic is not a safe derivation.** Counting "the last round with a full
  slate" gives 22 for basketball female BD, where the pair-meeting test gives the correct 18 -- rounds
  19-22 hold post-season matches that happen to look like full rounds. `n_rounds` is derived from the
  **schedule**, and the pair-meeting test is the acceptance check.
- **Basketball embeds its post-season in the league division; handball does not.** KKI packages the
  urslitakeppni as extra rounds inside the same `season_id` (`R/ingest-kki-basketball.R:23-24`), so
  basketball `BD`/`1D` carry post-season rows (male BD rounds 23-35 decay 4,4,4,3,2,2,2,2,2,2,1,1,1 --
  the classic bracket shape). Handball's `PO` is already a separate division, so `OD`/`G66` are clean.
  Excluding rounds `> n_rounds` from the regular-season surfaces is what makes D3 honest rather than
  cosmetic.
- **Basketball female 1D is genuinely irregular** (11 teams, so byes; one pair meets once and one meets
  four times). It is the cell that proves `expected_meetings` must be an assertion that can fail, never
  a source.
- **N7 supersedes the inference.** KKI *declares* the boundary that the pair-meeting test *infers*, and
  the two agree. It also resolves the women's Bonusdeild anomaly: the 137 rows are an 18-round double
  round-robin **plus A/B group phases plus playoffs**, not a 2.44-meeting format. Capturing `stage` at
  ingest makes D3 correct by construction rather than correct-on-2026-data; the round-derivation rule
  becomes the fallback for any cell where stage is unavailable. **WS5 must establish whether the
  Baskethotel XLSX export exposes stage, and record the decision either way.**
- `meta.round = 22` for basketball male BD is **correct** -- it equals the regular-season length. The
  defect is that `standings.played` counts post-season games (up to 35), so "Umferdir eftir" renders
  **-13**.

## Architecture selection

Three architectures were proposed and each judged by three adversarial lenses (CI-completeness,
schedule risk, two-repo coupling). Under the original deadline brief: risk-first 6.7, symmetry-first
5.3, contract-first 4.7. Under the revised brief the ranking inverts, because every objection to
symmetry-first was a schedule objection.

**Chosen:** Symmetry-first as the spine — one extracts-tree reader, one publisher, one division loop for all three sports, with the two parallel 2DT publishers deleted rather than patched — grafted with contract-first's schema discipline (a generated schema set, a `_draft/` arming path, declared units) and risk-first's ingest-gate deletion, and with both federation season registries replaced by derived season resolution (stable key + live discovery + a season-stamp assert) instead of hand-rotated integers.

**Why:**

Under the original brief symmetry-first was penalised almost entirely for schedule reasons: it deletes 737 lines of live publisher and rewrites a 1618-line football publisher, gated on a golden-file test that did not exist yet, in the week handball opened. Every one of those objections is a "this is slow and risky to do in three days" objection, and the brief has withdrawn that constraint. What remains is the only proposal that ends with one code path per artefact instead of two-and-a-bit, and the judges' own verification confirmed its cornerstone: `data/beliefs/extracts/` contains only `sport=football` while `.github/workflows/fit.yml:80` already runs `git add data/beliefs/extracts/` and `extracts/` is absent from `.gitignore` (N1, re-verified here). So the 2DT partition shape is unconstrained by back-compat and the CI-visible publish input already exists — the automation blocker B4 is a routing problem, not a plumbing one.

Contract-first loses as the spine because its own critical path never fixes B4: `publish_one()` (R/publish-pipeline.R:105-118) still resolves `data/beliefs/fits/.../fit.rds`, which `.gitignore:48` excludes and `decide-publish.yml` never produces, and its plan leaves `publish_basketball_iceland(fit, ...)` taking a cmdstanr fit. A perfect schema over a publish path that cannot run on CI certifies a laptop snapshot as green. But its schema work is the best in the set and is grafted wholesale.

Risk-first loses as the spine because it is explicitly a minimum path — its value was in sequencing and in two cheap alarms, and its own structure (a copied `read_extracted_2dt`, a 2DT-only `fit_meta.parquet`) reintroduces the copy-paste mechanism that produced B5. Its ingest-gate deletion and its `.assert_season_stamp` tripwire are grafted; its registry-rotation answer is superseded by N3.

Three findings from this session change the shape of the answer and none of the three proposals had them:

1. **The 2DT models DO have a per-round strength trajectory.** `Stan/basketball_iceland/2d_student_t_scalarsigma.stan:153-177` declares `array[N_rounds] vector[K] offense` and `defense` as transformed parameters (a random walk, exactly like football's), and `prepare_data()` already builds `N_rounds`/`round1`/`round2` (R/model-prepare.R:273). Both proposals asserted `round_strengths_quantiles` was impossible for 2DT and used that to omit `team_strengths_history.json` permanently. It is not impossible. bb/hb reach 10 artefacts, not 8.

2. **B5 is worse than described, and the fix is bigger than removing `exp()`.** The Stan generated quantities block (lines 277-289) defines `home_advantage_tot = home_advantage_off + home_advantage_def`, with both terms `vector<lower=0>[K]` in raw points. `.compute_home_advantage_quantiles_2dt()` (R/extract-iceland-2dt-shared.R:194-216) applies `exp(x/2)` to that. Football's `/2` is a per-side allocation of a *log* multiplier; on an additive sum it is not a halving of anything meaningful. Both the `exp()` and the `/2` must go, not just the `exp()`.

3. **The registries are fixable, not merely rotatable (N3), and the same reasoning applies to both federations.** The right move is to swap the volatile key for a stable one — HSÍ: every page becomes `/tournament/<id>` keyed by season, ids discovered live from hsi.is; KKÍ: `league_id` is stable across seasons and `season_id` is not, so store `league_id` and resolve `season_id` from kki.is. A hand-rotated registry that silently goes stale each July is the defect; the fix is a resolver with the registry demoted to a verified cache, plus a guard that aborts when the page a resolver returns disagrees with the season that was asked for.

Finally, the revised brief's "still be right in three years" test settles the two contested sub-decisions: DIVERGENCES.yml loses to a schema generator (the judge correctly costed a semantic JSON-Schema differ as more work than the generator it substitutes for), and the platform-side `_normalise_next_games()` shim loses to an upstream field rename (the shim existed only to decouple merge order under deadline pressure; a permanently bilingual contract is the thing we are trying to stop building).

---

## 1. Problem statement and end state

Basketball and handball have been modelled since the monorepo migration but have never published from CI. Every JSON under `data/publish/{basketball,handball}/iceland/{karla,kvenna}/` on disk today was generated on a laptop; the newest is stamped `2026-06-23`. The cause is a single routing fact: `publish_one()` (R/publish-pipeline.R:105-118) takes an extracts-tree branch only for `key == "football_iceland"`, and every other sport falls through to `data/beliefs/fits/sport=<s>/country=<c>/sex=<x>/fit.rds`, which `.gitignore:48` excludes and which `decide-publish.yml` (a plain `actions/checkout@v5` with no fit step) can never produce. The miss is silent: a warning, `return(invisible(NULL))`, exit 0, no health row.

The end state of this spec:

- Eight publish cells — basketball {BD, 1D} x {karla, kvenna}, handball {OD, G66} x {karla, kvenna} — produced end-to-end by `scrape-results.yml -> fit.yml -> decide-publish.yml` with no laptop in the loop, at football's directory layout `data/publish/<sport>/iceland/<sex_folder>-<slug>/`.
- Each cell carries football's full artefact set where the surface is meaningful: `meta`, `next_games`, `standings`, `standings_history`, `team_strengths`, `team_strengths_history`, `home_advantage`, `final_positions`, `final_positions_history`, `points_distribution`. Ten files, not eight (see §9 on why `team_strengths_history` is now reachable).
- One reader, one publisher and one division loop shared by all three sports. `publish_basketball_iceland()` and `publish_handball_iceland()` are deleted, not shimmed.
- Both federation ingests resolve the current season from the federation rather than from a hand-edited integer, and abort rather than write a partition whose contents disagree with the season requested.
- Both ends of the two-repo contract fail closed: schemas generated from one source, validated in `sports` before commit and in `metill-platform` before deploy.
- No bets are placed on either sport (D2), enforced by config and by a runtime flag, not by the absence of odds.

What this spec does not do: it does not simulate the úrslitakeppni for either sport, it does not add a cup cell (no CUP division has ever been ingested for basketball or handball), and it does not invent an xG analogue for a model that has no goals process.

## 2. The governing principle: no state that silently goes stale

Three of the four blockers, and both of the platform's blockers, are the same defect wearing different clothes: a value that is correct when written and becomes silently wrong later, with nothing that notices.

- `HSI_URLS` (R/ingest-hsi-handball.R:19-31) pins `.../olis-deild-karla-2025-26`. `hsi_current_season()` (:518-522) returns `yr + 1L` when the month is >= 7, so today it returns 2027. The URL and the season stamp come from two independent sources and drift apart every July. Unblocking ingest without fixing this scrapes 2025-26 pages into a `season=2027` hive partition inside a git-tracked directory that five cron jobs commit to daily.
- `KKI_SEASON_IDS` (R/ingest-kki-basketball.R:31-51) stops at key `2026`. `fetch_kki(seasons = NULL)` iterates registry keys only, so this fails safe — it under-fetches rather than mis-writes — but it still means basketball ingests nothing from the moment the season turns until someone hand-probes four integers.
- `config/active_competitions.json` is generated from `data/facts/schedules` rows inside today+14 (R/schedule-active.R:36-56) and then gates the ingest that writes those rows (R/ingest.R:136-140). A closed loop with no external input: once a season ends, nothing can restart it. `--league` does not bypass (scripts/01_ingest_results.R:19-24 subsets the leagues and still passes `active_path`), `opts$force` is parsed and never read, and hand-editing the JSON is futile because `scrape-results.yml` runs step 00 immediately before step 01.
- On the platform side, `total_rounds = 2 * (n_teams - 1)` and `max_points = round_num * 3` are the same defect in arithmetic form: a league format and a points scheme hardcoded at a consumer that cannot see them change.

The rule this spec applies throughout: **a fact that varies between seasons, sports or divisions is either derived from the data at read time, or declared in a contract that a guard validates. It is never a literal that a human is expected to rotate.** Where a literal is unavoidable (a discovered tournament id), it is stored as a *cache with provenance* — value, discovered-at, source title — and every use is checked against an independent property of what came back.

## 3. Betting interlock (D2) — lands before anything touches ingest

This must be first, ahead of every data change, and the ordering is a safety constraint rather than a scheduling preference. Both leagues are `active: true` with live Lengjan competition ids (`basketball_iceland.lengjan.competitions` = 1519/1528, `handball_iceland` = 1269). The moment §7 repopulates `data/facts/schedules`, `ingest_one_lengjan()` starts scraping odds, `04_decide.R` starts producing recommendations, and the local launchd agent `is.metill.sports.autoplace` — which places *all* recommendations by design — starts staking real money on handball, which MEMORY's 2026-06-13 methodology verdict already records as not Lengjan-bankable.

N2 rules out the obvious implementation. `config/leagues.schema.json` gives `betting` `"additionalProperties": false` with `required: [kelly_frac, ev_threshold, markets, scoring, min_bet]`, and the league object itself is `additionalProperties: false`. A `betting.enabled` key is rejected at `load_leagues()`, taking every pipeline script down with it. So the schema is edited first, in the same commit:

```json
"enabled": {
  "type": "boolean",
  "description": "Absent = true. When false, no odds are scraped and no candidates are produced for this league. Publishing is unaffected."
}
```

added to `betting.properties` (not to `required`, so the football entry is untouched).

Then in `config/leagues.yml`, both `basketball_iceland.betting` and `handball_iceland.betting` gain `enabled: false`, and their `lengjan.competitions` lists are emptied with the ids preserved verbatim in a comment (belt and braces: zero competitions in means zero odds rows out, independently of the flag, and survives a code path that forgets to consult it).

New helper `betting_enabled(league) -> logical(1)` in `R/decide-kelly.R` or `R/config.R`, returning `!isFALSE(league$betting$enabled)`. Consulted by `ingest_one_lengjan()` (log and return 0L) and by `decide_league()` (log and return zero candidate rows).

Note for the record: `check_odds_freshness` and `check_capture_rate` in `pipeline_health()` must learn about the flag too, or they will start reporting a betting-disabled league as unhealthy the moment its fixtures appear. Both take `leagues` already; the fix is a one-line filter plus a `PAUSED`-shaped row saying "betting disabled by config".

## 4. Test and fixture harness — built before the code it protects

The current coverage for these two sports is zero and has always been zero. All 12 tests in `tests/testthat/test-publish-{basketball,handball}.R` open with a machine-local absolute path (`/Users/brynjolfurjonsson/sports-backup-20260424-163153/...`, overridable by `SPORTS_BACKUP_ROOT`) and `testthat::skip()` when it is missing — which is always, in CI. The six extract tests gate on `data/beliefs/fits/sport=basketball/.../fit.rds`, a 300-600 MB gitignored artefact. This spec deletes ~740 lines of live publisher, so the harness comes first and the fixtures are the deliverable, not a by-product.

**Fixture A — a synthetic extracts tree.** `tools/make-extract-fixtures.R` generates, and commits under `tests/testthat/fixtures/extracts/`, a partition per sport:

```
sport={football,basketball,handball}/country=iceland/sex={male,female}/fit_date=2100-01-01/
  predicted_matches.parquet  team_strengths_quantiles.parquet
  round_strengths_quantiles.parquet  home_advantage_quantiles.parquet
  final_positions.parquet  points_distribution.parquet  fit_meta.parquet
```

Two divisions, 10 teams each, 50 draws, all upcoming dates `2100-01-xx` (the repo's time-bomb rule: never a near-future literal). Target size < 250 KB total. Publisher tests become `read_extracted_iceland() -> publish_iceland_league() -> validate_publish_dir()` round-trips with no cmdstan, no chromote and no fit.

**Fixture B — a draws stub.** `tests/testthat/helper-stub-fit.R` defines `stub_fit(draws_list)`, an object exposing `$draws(var)` backed by small committed `posterior::draws_array` objects covering the 2DT variable set (`offense`, `defense`, `home_advantage_off/def/tot`, `cur_*`, `mean_goals`, `sigma`, `nu`, `lp__`). This is what un-gates the *extractor* tests, which is the layer where B5 lived and where the fit RDS gate has never let a test run.

**Behavioural assertions — every one of these must fail on today's code.** Compilation is not evidence in a repo whose characteristic failure is warn-and-exit-0.

1. **Units (B5, RED today).** A stub fit whose `home_advantage_tot` draws are all exactly 4.0 must publish a `home_advantage.json` median of 4.0. Today it publishes `exp(4/2)` = 7.389. Assert equality to 1e-9, not a bound.
2. **Division fan-out.** `publish_iceland_league()` over a configured two-division cell writes exactly two output directories, with the second division's `standings.rows` disjoint from the first's teams.
3. **No silent skip.** `publish_one()` for an in-season cell with no extract partition raises a condition. Today it warns and returns `invisible(NULL)`; the test asserts `expect_error()`.
4. **Football golden file.** Publish football BD/LD1/LD2/LD3/CUP x both sexes from a pinned `fit_date` fixture before and after the refactor; assert byte-identical JSON modulo `generated_at`. This is the safety net for deleting the two 2DT publishers and rewriting the football one.
5. **Schema generator idempotence.** `tools/gen-publish-schemas.R` re-rendered into a temp dir must byte-match the committed `config/publish-schemas/` tree (§11).
6. **Season-stamp guard.** A parsed page fixture whose match dates are all 2025 must abort when requested as season 2027 (§5).
7. **Skip hygiene.** A convention test greps `tests/testthat/test-{publish,extract}-{basketball,handball}*.R` for `skip(`, `skip_if(`, `skip_if_not(` and `Sys.getenv` and fails if any survives. The point is that this coverage can never quietly stop running again.

The 12 machine-local tests are deleted rather than repaired: they exercise the `publish_*(fit, ...)` signature that §10 removes. Deleting them is honest; leaving 12 permanent skips is not.

## 5. Season rollover: derived resolution for HSÍ

**Replace both HSÍ registries with one, keyed by season, all URLs of the same shape.** `HSI_URLS` (current-season league slugs) and `HSI_HISTORICAL_IDS` (tournament ids) become a single `HSI_TOURNAMENT_IDS[[sex]][[div]][["<season>"]] <- <id>`, and `hsi_url(sex, div, season)` is a lookup returning `https://www.hsi.is/tournament/<id>` or `NULL`. `hsi_current_season()` survives but is demoted: it names the season we are *asking for*, and never selects a URL. The current-vs-historical branch in `fetch_results_hsi` (:598-616) and the `HSI_URLS` read in `fetch_schedule_hsi` are deleted.

Seed values: the existing `HSI_HISTORICAL_IDS` 2021-2025 entries, plus the six live 2027 ids read off hsi.is navigation (Olís karla 9142, Grill66 karla 9140, Olís kvenna 9141, Grill66 kvenna umspil 9143, bikar karla 8437, bikar kvenna 8436), plus the recovered female div2 2025 candidate 7643 (§6). Every seeded value carries provenance in `config/federation-seasons.json`, not in an R comment.

**Discovery, so the registry stops being the thing that goes stale.** `hsi_discover_tournaments(sexes, divisions, season)` drives chromote (via the existing `fetch_hsi_html()`) over hsi.is's tournament index, reads each linked `/tournament/<id>` together with its title, and maps title to `(sex, div)` through a small pattern table (`^Olís deild karla`, `^Grill 66 deild kvenna`, ...). It returns a tibble of `(sex, div, season, id, title, discovered_at, source = "live")`. `refresh_federation_seasons()` merges that into `config/federation-seasons.json` — a git-tracked cache with provenance — and `hsi_url()` resolves registry first, cache second, `NULL` third. `NULL` means "do not fetch", which is the fail-safe direction.

**The guard that makes any of this trustworthy.** New `.assert_season_stamp(rows, season, tol = 0.05)`, called inside `hsi_fetch_and_parse()` (:540-560) and inside the KKÍ parse path (§6). More than 5% of parsed `match_date` calendar years falling outside `{season - 1, season}` is a hard `cli::cli_abort`, not a warning. This is what converts a mis-mapped id, a stale slug or a federation URL-scheme change from "writes a fake partition into a cron-committed directory" into "fails the run loudly before the first row is written". It is the single most important line in this section, and it must be tested against a fixture, not just added.

**Ordering constraint (non-negotiable).** This section merges, and is verified against a scratch storage root, strictly before §7 removes the ingest gate. With the gate lifted and the URLs stale, one cron run writes an entire 2025-26 season into `season=2027` under `data/facts/results/` — a git-tracked hive partition in a repo where five workflows commit daily, making cleanup a partition delete racing automation. Ship §5, §6 and §7 in one PR so no CI run ever observes the intermediate state.

## 6. Season rollover: derived resolution for KKÍ, and the female G66 2025 hole

**Swap the volatile key for the stable one.** N3's observation is the design: kki.is exposes `https://kki.is/motamal/leikir-og-urslit/motayfirlit/Leikir?league_id=190&season_id=130403`, and 130403 is exactly the repo's registered male div1 2026 `season_id`. `league_id` identifies the competition and is stable across seasons; `season_id` is what rotates. So the registry stores what does not change:

```r
KKI_LEAGUE_IDS <- list(
  male   = list(div1 = 190L, div2 = 191L),   # Bónus deild karla, 1. deild karla
  female = list(div1 = 189L, div2 = 231L)    # Bónus deild kvenna, 1. deild kvenna
)
```

All four are **resolved** (N6) and each was cross-validated by confirming its 2025-26 `season_id` equals the value the repo already holds under key `2026`. `kki_league_id()` still aborts on an unresolved id rather than silently fetching nothing, and a test asserts every `(sex, div)` in `publish_divisions` resolves to a non-NA id -- that test now passes on real values. The discovery pass remains, because next July the *season* ids rotate again; the point of N6 is that discovery is cheap and proven, not that the registry is final. `KKI_SEASON_IDS` is retained verbatim as a verified cache for 2021-2026 — it is real, hand-verified data and throwing it away would be vandalism — but it stops being the only source.

`kki_discover_season_ids(sex, div)` loads the league page under chromote (the season selector is JS-rendered, so a plain `httr` fetch returns an empty shell — same constraint the HSÍ scraper already lives with) and reads the selector's `(label, season_id)` options. `refresh_federation_seasons()` merges the result into `config/federation-seasons.json` alongside HSÍ's.

`fetch_kki(seasons = NULL)` changes meaning from "iterate every registry key" to "the current season only", resolved as registry -> cache -> discovery -> `NULL`. Explicit `seasons =` still backfills. Without this change, removing the ingest gate (§7) turns 48 XLSX downloads a day (2 sexes x 2 divs x 6 seasons x results+schedule) into a permanent cost. `download_baskethotel_xlsx()`'s existing `PK` magic-byte check already catches the plausible-but-dead id case documented at R/ingest-kki-basketball.R:14-27 (`190366` returning a 6230-byte header-only file); `.assert_season_stamp()` now catches the plausible-and-live-but-wrong-season case, which nothing catches today.

**Female Grill 66 2025 backfill.** `HSI_HISTORICAL_IDS` documents the hole explicitly: the legacy source recorded 7644 for female div2 2025, which is a copy-paste of the male div2 id, and the docstring concludes the genuine id is "not recoverable". N3 shows it is: `https://www.hsi.is/tournament/7643` is titled "Grill 66 deild kvenna" and sits exactly between the verified female div1 2025 (7642) and male div2 2025 (7644). **Adjacency is a hypothesis, not a confirmation.** The procedure is: register 7643 as a *candidate*, fetch it under chromote, assert (a) the page title matches the female Grill 66 pattern, (b) `.assert_season_stamp()` passes for season 2025 — fixtures in Sept 2024 to May 2025 — and (c) the team set intersects the known 2024 and 2026 female G66 squads. Only then is it committed with `source: "inferred-verified"` in the provenance cache. If any check fails, the season stays absent and the gap is reported by `check_season_resolution` (§13) rather than papered over. Closing it backfills the handball female G66 2025 season, which is missing from `data/facts/results` entirely and is the reason that cell's history is thin.

**The PO-only-2026 anomaly, explained.** `HSI_HISTORICAL_IDS` has no `playoffs` key for either sex — playoff ids exist only in `HSI_URLS` (male 8427, female 8430), which is the *current-season* table. So the playoffs were only ever scraped for whatever season was current when the code last ran, which is why `data/facts/results` holds PO rows for 2026 only (20 male, 16 female). Nothing is broken and nothing is missing upstream; the historical playoff ids were simply never registered. Under the season-keyed registry, `playoffs` becomes an ordinary `(sex, div, season)` triple and prior seasons can be backfilled by discovery on the same footing as everything else. PO stays *in* the training data (it is real handball between the same teams) and stays *out* of `publish_divisions` — there is no league table for a knockout, and D3 says the úrslitakeppni is not modelled as a bracket.

## 7. The ingest activation gate — a generic fix, because football hits it in November

`ingest_one_league()` (R/ingest.R:136-140) calls `.is_league_active(active_path, key)` and returns `0L` when `config/active_competitions.json` marks the league inactive. That JSON is written by `generate_active_competitions()` from `data/facts/schedules` rows inside `[today, today + lookahead_days]` — rows that only ingest itself can write. The three fail-open paths (`schedule_root` missing, the Parquet read erroring, the league key absent from the JSON) all require the data to be *gone*, not merely stale, so a league whose season has ended can never restart itself. Both 2DT sports are `"false"` today. Football's last fixture is 2026-10-25 and it hits the identical wall in November.

**Fix.** Delete the `.is_league_active()` call from `ingest_one_league()`. Keep the `active_path` argument in the signature so `scripts/01_ingest_results.R:39` is untouched, and keep `.is_league_active()` itself, still called by `ingest_one_lengjan()` (R/ingest.R:170-178) — the odds scrape is where the gate is *correct*, because odds genuinely do not exist outside a fixture window and no closed loop is involved.

**Replace the cost control without reintroducing the loop.** The gate's stated justification is "saves a chromote launch". Preserve that intent with a control whose input is *fetch* state rather than fetched data: a new `data/health/ingest_log.json` records `(league, sex, last_attempt_at, last_rows)`, and `ingest_one_league()` skips a (league, sex) whose last attempt was within `offseason_min_interval_hours` (default 24) *and* whose last three attempts all returned zero rows. A league that is genuinely dormant is polled once a day; a league whose fixtures appear resumes on the next poll with no human action. Because the input is "when did we last try", not "what did we find", it cannot deadlock. `--force` and `--league` both bypass it, and `opts$force` gets wired into `01_ingest_results.R` (it is currently parsed by `parse_pipeline_args()` and never read) so the flag stops being a lie.

**Explicitly rejected:** plumbing `opts$force` as *the* fix. A flag someone must remember to pass every September is not automation, and it would leave football's November wall standing.

## 8. The B5 units bug: both the exp() and the halving are wrong

`.compute_home_advantage_quantiles_2dt()` (R/extract-iceland-2dt-shared.R:194-216) computes `value = exp(transform(value))` with `transform = function(x) x / 2` for the `total` component, under a comment at :192 reading "Mirrors football's home_advantage_quantiles.parquet — exp-transformed log-multiplier". Note the shared `extract_one()` applies `exp()` to *all three* components, not only `total`.

The Stan source settles it. `Stan/basketball_iceland/2d_student_t_scalarsigma.stan:112-116` declares `vector<lower=0>[K] home_advantage_off` and `home_advantage_def`, both entering the mean linearly in raw points (`mu_ll[1] = mean_goals[...] + off_ll[1] - def_ll[2]`, :264), and :277 defines `home_advantage_tot = home_advantage_off + home_advantage_def`. The legacy helper 100 lines away in a different file, `R/publish-iceland-2dt-helpers.R:293-296`, says exactly this in prose. Football's `exp()` is correct *for football*, where the parameter is a log-rate multiplier, and football's `/2` is a per-side allocation of that log multiplier — neither operation has a meaning on an additive sum of two non-negative point quantities.

**Fix.** Remove both. `value = .data$value`, no transform, for all three components. A 4-point home edge publishes as 4, not `exp(2)` = 7.389, and not `exp(4)` = 54.6.

**Why it never fired, and why that is the dangerous part.** `data/beliefs/extracts/` contains only `sport=football` — the F6 extractors shipped 2026-05-26, a month after the last basketball/handball fit — and the extract call site in `R/model-league.R:288-304` is a warn-only `tryCatch`, so the extractor failing outright is invisible in the fit log and a wrong number would be equally invisible. The moment §10 makes the extracts tree the publish source, this becomes the first number on every `home_advantage.json`.

**Two structural follow-ons, so it cannot recur silently:**

- The warn-only `tryCatch` at R/model-league.R:288-304 is promoted to an abort for any sport whose extracts are the sole publish input — which, after §10, is all three.
- `meta.json` gains a required `units` object (§12): `{ strength: "log_rate" | "points", home_advantage: "multiplier" | "points", diff_bin_width: <int> }`. This is the contract-level statement of the asymmetry that the copied comment asserted and got wrong. A publisher that does not say what units it is in fails schema validation, and a consumer branches on units rather than on sport name.

## 9. Multi-division config and the generalised division helpers (D1)

**Config.** N2 constrains the shape: `config/leagues.schema.json`'s `definitions.publishDivisionList.items` is `additionalProperties: false` with `required: [code, slug, label_is, is_cup]`. Every entry must carry `is_cup`, and any new key needs a schema edit in the same commit. The added blocks:

```yaml
basketball_iceland:
  publish_divisions:
    male:
      - { code: BD, slug: bd, label_is: "Bónusdeild", is_cup: false,
          expected_meetings: 2, qualify_slots: 8, relegation_slots: 2 }
      - { code: 1D, slug: 1d, label_is: "1. deild", is_cup: false,
          expected_meetings: 2, qualify_slots: 8, relegation_slots: 0 }
    female:
      - { code: BD, slug: bd, label_is: "Bónusdeild", is_cup: false,
          expected_meetings: 2, qualify_slots: 8, relegation_slots: 2 }
      # 11 teams in 2026 and genuinely irregular (byes; one pair met once, one
      # four times). expected_meetings is DELIBERATELY omitted here: there is no
      # correct constant, so the schedule-derived n_rounds is the only source and
      # the assertion must not fire. See the boundary table above.
      - { code: 1D, slug: 1d, label_is: "1. deild", is_cup: false,
          qualify_slots: 8, relegation_slots: 0 }
handball_iceland:
  publish_divisions:
    male:
      - { code: OD,  slug: od,  label_is: "Olísdeild", is_cup: false,
          expected_meetings: 2, qualify_slots: 8, relegation_slots: 2 }
      - { code: G66, slug: g66, label_is: "Grill 66-deild", is_cup: false,
          expected_meetings: 2, qualify_slots: 0, relegation_slots: 0 }
    female:
      # TRIPLE round-robin: 8 teams, 21 rounds, 84 matches, 3 meetings per pair.
      # This is the cell that breaks the platform's 2*(n-1) assumption.
      - { code: OD,  slug: od,  label_is: "Olísdeild", is_cup: false,
          expected_meetings: 3, qualify_slots: 4, relegation_slots: 1 }
      - { code: G66, slug: g66, label_is: "Grill 66-deild", is_cup: false,
          expected_meetings: 3, qualify_slots: 0, relegation_slots: 0 }
```

Three new optional keys are added to `publishDivisionList.items.properties` (all `"type": "integer", "minimum": 0`): `expected_meetings`, `qualify_slots`, `relegation_slots`. **`expected_meetings` is an assertion, not a source.** `n_rounds` is derived from data (§12); the config value is compared against the derived value and a mismatch emits a `WARN` health row naming both numbers. This is the §2 principle applied to a competition format that changes between seasons.

**`qualify_slots` and `relegation_slots` must be verified against the 2026-27 KKÍ and HSÍ regulations before the config is committed.** They are the only numbers in this spec taken from a plausible-sounding summary rather than from source, and a wrong `qualify_slots` silently mislabels the headline probability on the page. A test asserts `0 <= qualify_slots < n_teams`, which catches transcription errors but not a wrong-but-plausible value; the mitigation is a human reading the federation's competition rules once, and the spec should not pretend otherwise.

**Helpers.** The four `.football_iceland_division_{codes,slugs,labels,split}(sex)` functions (R/extract-football-iceland.R:40-120) each read `load_leagues()[["football_iceland"]][["publish_divisions"]][[sex]]` and are otherwise fully generic. They gain a leading `key` argument and are renamed `.iceland_division_*(key, sex)`, joined by `.iceland_division_qualify(key, sex)` and `.iceland_division_relegation(key, sex)`. The eight external call sites (R/publish-football-iceland.R:742/744/797/1009, scripts/backfill_final_positions_history.R:40/108, plus tests) are updated in the same commit. **No compatibility aliases.** "One release" is undefined in a repo with no release cadence where five cron workflows commit daily; two live names for one symbol is the drift this spec exists to remove.

**The division-badge collision.** `.football_iceland_division_code_labels()` (R/extract-football-iceland.R:143) is a static map mirroring the platform's `DIVISIONS` dict, and football `BD` (Besta deild) collides with basketball `BD` (Bónusdeild) on the bare code. The map moves into config as a per-division `code_badge` (pattern `^[A-Z][A-Z0-9_]{1,3}$`), so the client-side filter key is namespaced by sport in practice. This is cheap now and a silently mislabelled filter chip later.

## 10. The extracts-tree refactor: one extractor shape, one reader, one publisher

This is the workstream that fixes B4, and N1 makes it cheaper than it looks: `.github/workflows/fit.yml:80` already runs `git add data/beliefs/latest/ data/beliefs/archive/ data/beliefs/extracts/`, `extracts/` is absent from `.gitignore`, and `fit_league()` (R/model-league.R:281-286) already dispatches `extract_basketball_iceland()` / `extract_handball_iceland()` in the same `write_archive` branch as football. The producer half is wired. Nothing has ever read it. **Zero workflow changes are required.**

Because the bb/hb subtree is empty on disk, the partition shape has no back-compat constraint. This is the one moment it is free to define correctly, so all of the following land on the first write rather than as a later migration.

**(a) Extractor.** `.extract_2dt_iceland_pfi()` (R/extract-iceland-2dt-shared.R:302-424) replaces its `top_div = "BD"` scalar with `divisions = .iceland_division_codes(key, sex)` and loops. Cross-division inputs (`posterior_goals`, `teams`, the raw `fit$draws()` pulls, `prep`) stay hoisted exactly as football's `.extract_division_parquets_pfi()` does; the per-division quantities (`top_results`, `current_top_teams`, `.compute_final_positions_2dt`, `.compute_points_distribution_2dt`, `.compute_team_strengths_quantiles_2dt`, `.compute_home_advantage_quantiles_2dt`) move inside. All parquets gain a `division` payload column; `predicted_matches` already carries one. **Hazard to test explicitly:** `.compute_final_positions_2dt(posterior_goals, top_div, base_points, ...)` filters internally, so the loop must not simulate a second-tier table on top-tier base points.

**(b) `round_strengths_quantiles.parquet` for 2DT — newly established as feasible.** Both earlier proposals asserted the 2DT models have no per-round trajectory and used that to drop `team_strengths_history.json` permanently. They are wrong: `Stan/basketball_iceland/2d_student_t_scalarsigma.stan:153-177` declares `array[N_rounds] vector[K] offense` and `defense` as transformed parameters (a random walk with `delta_t`-scaled innovations), and `prepare_data()` already builds `N_rounds`, `round1`, `round2` (R/model-prepare.R:273-276). So `.compute_team_strength_trajectory_pfi()` — which maps the fit's global round index onto division-specific matchweeks — generalises from football to 2DT by parameterising the variable names it pulls. bb/hb reach the full 10-artefact set. Caveat to verify against a real fit: 2DT rounds are per-team appearance indices, so a division whose teams entered at different global rounds needs the same index mapping football already performs; the generalisation is of that helper, not a reimplementation.

**(c) `fit_meta.parquet` on BOTH trees.** A 6th (football: 7th) file carrying `n_draws`, `fit_date`, `stan_model`, `model_units`. Today `n_draws` is recomputed inside each publisher as `posterior::ndraws(fit$draws("lp__"))`, which is exactly the kind of thing that forces a publisher to hold a fit. Writing it on the 2DT tree only — as one proposal suggested — would make the two trees diverge in shape at the single moment they are free to converge, and would permanently justify two readers. It goes on both.

**(d) ONE reader.** `read_extracted_football()` (R/extract-football-iceland.R:1793-1900) becomes `read_extracted_iceland(league, sex, fit_date = NULL, extracts_root, target_divs = NULL, profile = sport_publish_profile(league$sport))`, moved to a new `R/extract-iceland-read.R`. Its body needs three edits: the `stopifnot(league$sport == "football")` becomes a profile lookup; the hardcoded `file_types` vector becomes `profile$required_extracts` / `profile$optional_extracts`; the `empty_tibbles` list becomes `profile$empty_extracts`. Everything else — the descending `fit_date=*` scan for the newest *complete* partition, the incomplete-partition abort, the per-division split on the payload `division` column, the graceful empty-tibble degradation for a configured-but-absent division — is already sport-agnostic and is copied zero times. `read_extracted_football` is deleted, not aliased. `football_extract_partition_exists()` (R/publish-pipeline.R:9-19) is likewise renamed `extract_partition_exists()`; its body already takes `sport` as an argument.

**(e) The profile, kept honest.** `sport_publish_profile(sport)` carries only fields that encode a real per-sport difference: `required_extracts`, `optional_extracts`, `empty_extracts`, `value_link` (per component: `identity` for 2DT, `exp` for football, with football's `total` half-split declared here rather than inlined), `points` (`c(win = 3, draw = 1, loss = 0)` for football, `c(2, 1, 0)` handball, `c(2, 0, 0)` basketball), `has_ties`, `diff_bins` (width/low/high), `units`, and `surfaces` — a character vector of the publish artefacts this sport emits. The judges' criticism of four parallel booleans (`has_xg`, `has_round_strengths`, `cup_capable`, `split_capable`, all TRUE for football and FALSE for both others — one boolean in a costume, giving 16 nominal branches of which 2 are exercised) is accepted: they collapse into `surfaces` membership, and the branch condition becomes "does this sport emit this surface", which is one predicate with a per-sport data answer.

**(f) ONE publisher.** `publish_football_iceland()` (R/publish-football-iceland.R:697) becomes `publish_iceland_league(extracted, league, sex, division, profile, end_date, root, output_root, extracts_root, archive_root, round_predictions_history_root)`. Three football literals are replaced: the `stopifnot` on sport, the `"football"` path segment (becomes `league$sport`), and the `.football_iceland_division_*()` calls. The surfaces football alone emits (`round_predictions_history.json`, the xG join and `xg_trend`, the cup bracket / `tournament_placements.json`, the split-season machinery) are gated on `surfaces` membership. `xg_for` / `xg_against` / `xpts` ship `null` for 2DT — `config/publish-schemas/football/standings.schema.json` already types them `["number", "null"]`, which is evidence the schemas were written sport-general and merely filed under football. `.compute_standings_rows_2dt()` (R/publish-iceland-2dt-helpers.R:149-225) is brought up to football's row contract (adding `n_predicted_matches`, `n_played_matches`, `xg_against_trend`, `goals_trend`, `goals_against_trend`) and then deleted in favour of football's inline tabulation driven by `profile$points`. `publish_basketball_iceland()` and `publish_handball_iceland()` are deleted outright — 737 lines, replaced by a loop.

**(g) Dispatch.** `publish_one()` (R/publish-pipeline.R:57-133) loses the `identical(key, "football_iceland")` branch, the `fit_path` fallback and the `dispatch` list. Every sport takes the extracts path, inside the existing `tryCatch` whose handler already distinguishes "no partition yet" from "partition exists but will not read". The gitignored fit RDS stops being a publish input, which is the whole of B4. Per §13 the "no partition yet" branch stops being an unconditional quiet skip: it stays quiet only when the league has no upcoming games.

## 11. The JSON contract and schema generation

**Today both ends fail open.** `.validate_or_abort()` (R/publish-pipeline.R:145-158) short-circuits with an informational note for any sport lacking `config/publish-schemas/<sport>/` — and `config/publish-schemas/` contains only `README.md` and `football/`. The platform's mirror `scripts/validate_publish.py` returns "unmatched" rather than failing. Two skips do not make a check, and four new cells would ship completely ungated.

**The arming hazard, and its fix.** `.validate_or_abort()` gates on `dir.exists(file.path(schema_dir, sport))` but then calls `validate_publish_dir(output_root, ...)` over the **whole** publish tree. So merely creating `config/publish-schemas/basketball/` arms basketball validation *inside football's publish call*, and the June-dated basketball JSON (which has no `division` and no `is_cup`) would abort football's publish on the next cron run. `scripts/05_publish.R` calls `publish_one()` bare in a loop with no `tryCatch`, so the run dies and the commit step never fires.

Two changes, both required:

1. **Validate the sport's own subtree.** `validate_publish_dir(file.path(output_root, sport), ...)`. Arming one sport can no longer abort another. This is a correctness fix independent of everything else in this spec.
2. **Invert the default.** A sport that publishes with no schema directory becomes a `cli_abort`, not an informational skip, documented in `.claude/rules/publish-layer.md` in the same commit.

**Contract-first's `_draft/` arming pattern is grafted, and it is the best single idea in that proposal.** `.resolve_schema_path()` tries exactly `<schema_dir>/<sport>/<base>.schema.json` then `<schema_dir>/<base>.schema.json`, so a first path segment of `_draft` matches no sport and resolves in neither validator. Schemas are authored and reviewed under `config/publish-schemas/_draft/{basketball,handball}/`, with tests pointing `validate_publish_dir(..., schema_dir = here::here("config", "publish-schemas", "_draft"))` (the parameter already exists). Arming is a single `git mv` in the same commit as the conforming JSON. Rollback is `git rm -r config/publish-schemas/<sport>` — one move, touches no JSON. Note the Phase-1 caveat the judges caught: `.validate_or_abort()` hardcodes `schema_dir <- here::here("config", "publish-schemas")` and takes no argument, so it gains a `schema_dir` parameter defaulting to that path, or the draft workflow cannot be exercised through the publisher.

**Generation, not a divergence register.** Contract-first proposed `DIVERGENCES.yml` plus a test that diffs schema basenames across sport directories and fails on any unregistered difference. The judge's costing is accepted: comparing `required` arrays, type unions, enum sets, nested properties and `oneOf` branches is semantic JSON-Schema diffing, which is materially *more* work than the generator it substitutes for, and it only detects drift rather than preventing it. So: `config/publish-schemas/_base/<name>.schema.json` holds the shared shape, `config/publish-schemas/_delta/<sport>/<name>.json` holds per-sport overrides, and `tools/gen-publish-schemas.R` renders the committed per-sport files. A test re-renders into a temp dir and asserts a byte match. Drift becomes impossible rather than reportable, and football's strictness is never silently relaxed by promoting a file to a shared root.

**Contract changes (all three sports).**

- `meta.json` — bb/hb gain football's existing required `division` and `is_cup`. All sports gain: `units: { strength, home_advantage, diff_bin_width }` (§8); `n_rounds` and a corrected `round` (§12); `points: { win, draw, loss }`; `season_scope: "full_season" | "regular_season"`; `postseason: null | { name_is, modelled }`; `qualify: null | { slots, label_is }`.
- `next_games.json` — bb/hb adopt football's field names verbatim: `mean_home_goals`, `mean_away_goals`, `mean_goal_diff`, `p_home_win`, `p_draw`, `p_away_win`, `division` (Icelandic label), `division_code`, `venue` (null for bb/hb; already typed `["string","null"]`), and `goal_diff_distribution`. This is a **net deletion**: `R/publish-basketball-iceland.R:130-155` hand-rolls a `summarise()` producing `mean_home` / `p_tie` / etc., while `.compute_predicted_matches_2dt()` (R/extract-iceland-2dt-shared.R:79-160) already computes the football-named columns and left-joins the binned distribution from `.bin_goal_diff_distribution_2dt()`. Deleting the bespoke block and reading the extract supplies `goal_diff_distribution` — which the platform's fixture strip requires and bb/hb have never emitted — for free.
- `final_positions.json` / `points_distribution.json` — new required top-level `basis: "final_table" | "regular_season_table"`. `summary[]` gains `p_qualify` (see below) and emits `p_winner` only when `basis == "final_table"`.

**The `p_top_six` mislabelling, resolved rather than deferred.** `p_top_six` is a Besta-deild concept. Bónusdeild karla is 12 teams with 8 reaching the úrslitakeppni; Olísdeild's cutoff differs again. Shipping a top-six number under a playoff label would be a materially wrong published number. The fix is a generic `p_qualify`, computed against the division's configured `qualify_slots`, with `meta.qualify = { slots, label_is }` telling the consumer what it means. Football keeps emitting `p_top_six` as a deprecated alias for exactly one PR pair — the platform is updated to read `p_qualify` in the same series, and the alias is removed in a follow-up commit whose only job is that removal. A deprecation with a named removal commit is affordable now; an indefinite alias is not.

**Rejected: the platform-side `_normalise_next_games()` shim.** It existed to let the two repos merge in either order under deadline pressure. With the deadline withdrawn it buys nothing and costs a permanently bilingual contract that every future consumer — the renderer, `og.py`, any Quarto report — must reimplement.

## 12. n_rounds, round, and points: the fix is upstream

The platform design surfaced two blockers and both are the §2 defect in arithmetic form.

`app/routes/ithrottir.py:406` computes `total_rounds = 2 * (n_teams - 1)`. Bónusdeild karla is a four-meeting league: the on-disk standings show `played: 35` for 12 teams, so "Umferðir eftir" would render **-13 af 22**. `app/routes/og.py:696` hardcodes `max_points = round_num * 3`, wrong for both new sports (2 points per win) and doubly wrong because basketball's `meta.round` reads 22 against 35 played.

**Both are fixed upstream, in `meta.json`, and the platform does no arithmetic.** The reasoning: the league format and the points scheme are facts about the competition that the producer can see in the data and the consumer cannot; `og.py` already mirrors `ithrottir.py`'s division structure by comment and would become a second copy of the arithmetic; and a downstream clamp would hide a wrong number rather than surface it.

- **`meta.round`** — replaced by `min(played)` over the division's current-season teams, i.e. "every team has completed at least N matches". The current derivation via `.compute_round_num_2dt()` is a team-appearance index that desynchronises from `played` whenever a division's teams have uneven fixture counts. This is also more correct for football when rounds are uneven, so it changes all three sports.
- **`meta.n_rounds`** — total scheduled matches per team for the current (division, season), derived as `max` over teams of `played + remaining_scheduled` from the union of `data/facts/results` and `data/facts/schedules`. Where the federation has not yet published the full fixture list, fall back to `expected_meetings * (n_teams - 1)` from config, and stamp `n_rounds_source: "schedule" | "config"` in meta so a consumer and a health check can tell the difference. When both are available and disagree, publish the derived value and emit a `WARN` health row naming both numbers — a competition format change should be noticed, not absorbed.
- **`meta.points`** — `{ win, draw, loss }` from the profile. `og.py` computes `max_points = meta.round * meta.points.win`; nothing about the scoring scheme is hardcoded on the consumer side ever again.

A test asserts `n_rounds >= round` for every published cell and that `round` never exceeds `max(played)`.

## 13. Health checks and the warn-and-exit-0 failure mode

`pipeline_health()` (R/health.R:530-537) composes eight checks — `fit_freshness`, `odds_freshness`, `diagnostics_drift`, `orphaned_bets`, `capture_rate`, `placement_health`, `bankroll`, `discovery` — and **not one of them reads `data/publish/`**. That is precisely why "basketball and handball publish has never succeeded on CI" was invisible for months. Three checks and two guards are added.

**`check_publish_freshness(leagues, root, now, th)`.** For every `(league, sex, division)` in `publish_divisions` where the league is `active` and `has_upcoming_games()` is TRUE: read `data/publish/<sport>/iceland/<sex_folder>-<slug>/meta.json` and FAIL when it is absent, when `generated_at` exceeds `th$publish_max_age_hours`, or when the directory does not hold exactly the artefact set that sport's `surfaces` declares. Crucially — and correcting a specification error the judges caught in one of the proposals — the state "no extract partition exists at all for an in-season, active cell" must FAIL, not report PAUSED. That state *is* the current silent breakage; a check that reports PAUSED for it is a check that stays quiet in the one scenario it was built for. PAUSED is reserved for a cell whose league has no upcoming games.

**`check_season_resolution(leagues, root, now)`.** For every active league, FAIL when the current season for any configured `(sex, division)` has no resolvable federation id (registry, cache or discovery). This is the alarm for the class of failure §5 and §6 remove: it turns "handball quietly ingested nothing in October" into a red workflow on the day it starts.

**`check_publish_format_agreement(leagues, root)`.** WARN when a published cell's derived `n_rounds` disagrees with its configured `expected_meetings * (n_teams - 1)`, or when `qualify_slots >= n_teams`. Cheap, and it is the only thing standing between a changed competition format and a wrong headline number.

**Guard 1 — per-cell `tryCatch` around `fit_one()`.** `scripts/03_fit.R:34-54` calls `fit_one(static, row$sex)` bare, and `fit_model()` aborts on a diagnostics-gate breach. Config order is `basketball_iceland` (leagues.yml:15) -> `handball_iceland` (:79) -> `football_iceland` (:120), so the first live 2DT fits in five months — the highest abort-risk event of the season, and `fit_skip_reason()`'s own docstring records real off-season basketball R-hat/ESS breaches — can take football's fits down with them. Wrap the call, record the failing `(league, sex)`, continue the loop, `quit(status = 1L)` at the end if the failure list is non-empty, and change `fit.yml`'s commit step to `if: always()` so successful fits still commit while the run goes red.

**Guard 2 — the same for `publish_one()`.** `scripts/05_publish.R` calls `publish_one()` bare, and §11 makes `.validate_or_abort()` fail closed. Without a per-cell guard, a basketball standings row missing a newly-required key would abort the whole script *before football republishes*. The guard is the same shape: catch, record, continue, non-zero exit. Adding fail-closed validation without this guard converts today's harmless skip into a football outage — the judges caught exactly this asymmetry in one proposal, which added the fit guard and not the publish one.

The alert channel remains a GitHub workflow-failure email. That is signal, not a pager, and it should be stated as such rather than described as monitoring.

## 14. The metill-platform sport dimension

The platform design produced by the companion agent is adopted essentially as written — a `SPORTS` registry keyed by URL slug replacing the football-pinned `LEAGUES` + `DIVISIONS` module globals, `/ithrottir/{sport_slug}/iceland/{sex_slug}/{division_slug}/` with `fotbolti` as an ordinary registry key so every existing football URL is served by the same handler, one generalised `ithrottir_league.html` plus three ~20-line per-sport Aðferð partials, and three new opt-in JS options (`noDraw`, `highlightZero: false`, `scoreLabel`/`scorePrecision`). Two of its findings deserve emphasis because they would otherwise ship wrong: `standings-table.js:275` already computes `showExpectedCols = rows.some(r => r.xpts != null)`, so the xG columns vanish for bb/hb with no code change; and the goal-diff strip's centre bucket spans ±2 stig (basketball, 5-point bins) or ±1 mark (handball, 2-point bins) and is therefore **not** the draw probability, so the jökull highlight and the "Jökulbláa súlan stendur ávallt fyrir jafntefli" copy must go — that is a materially wrong published number, not a cosmetic issue.

Five corrections to that design under this spec:

1. **No `_normalise_next_games()` shim.** §11 renames the fields upstream. The platform reads one set of names.
2. **No `total_rounds` or `max_points` arithmetic.** Both read `meta.n_rounds`, `meta.round` and `meta.points.win` (§12). `_league_data_dir`'s legacy-directory transition shim stays, and is deleted in a named follow-up commit once the new `{sex}-{slug}` directories are confirmed on disk — after which the old un-suffixed `karla/` and `kvenna/` directories are `git rm`'d on the sports side so the `--delete` rsync removes them from `data/ithrottir/` rather than leaving four stale ghost cells.
3. **`p_qualify`, not `p_top_six`** (§11), with the heading text taken from `meta.qualify.label_is`.
4. **OG cache key gains the sport.** `og.py::_cache_key('ithrottir', sex, division, fingerprint)` omits it, so football `karla-bd` and basketball `karla-bd` would share a cache path and serve each other's card.
5. **Schemas are not hand-mirrored.** `data/ithrottir-schemas/` is populated by the same `tools/gen-publish-schemas.R` output and arrives via the existing rsync. `pull-sports-data.yml` sparse-checks out `data/publish` and `config/publish-schemas` from *one* clone at *one* SHA and rsyncs both, so schema and JSON can never skew — a genuinely nice property of the existing setup that the design should rely on explicitly rather than rediscover.

One unresolved design question the platform agent flagged and this spec does not settle: what a cell should render between deploy and its season opener, when `meta.json` exists but its `season` predates the current one. The `_sport_available` gate plus the existing `"{label} · Síðar"` disabled chip covers the not-yet-published case but not the published-stale case. Recommended default: render the empty-state banner "Tímabilið er ekki hafið — spár birtast þegar deildin byrjar." whenever `meta.season < current_season`, and leave the tab enabled. Flagging it as a decision rather than asserting it.

## 15. Icelandic copy and the D3 regular-season relabel

For basketball and handball the league table decides the *deildarmeistari*; the Íslandsmeistari comes out of an úrslitakeppni that this model does not simulate as a bracket (though PO results do feed the strength estimates, §6). D3 requires the published surfaces to say so.

**Encoded in the payload, not only in the template**, so `og.py` and any future consumer cannot reuse football's champion copy for a sport where it is false. `meta.season_scope` (`"regular_season"`), `meta.postseason` (`{ name_is: "Úrslitakeppni", modelled: false }`) and `final_positions.basis` (`"regular_season_table"`) are the levers; `p_winner` is emitted only when `basis == "final_table"`.

Strings (all grammar-checked with Miðeind before merge — the existing football stateline uses a single-V2-verb construction that was checked, and the replacement preserves it):

- Stateline with standings, replacing ithrottir.py:454-457: **"Eftir {n} af {N} umferðum á {lið} mestar líkur á að vinna deildarkeppnina."**
- Stateline with no standings yet, replacing :460: **"{lið} á mestar líkur á að vinna deildarkeppnina."**
- "Efst spáð" fact sub, replacing the bare "{p}% líkur" at :424: **"{p}% líkur á efsta sæti í deildarkeppni"**
- Heatmap panel title, replacing "Lokastaða (spáð)" in `finishing_heatmap.html:11`: **"Lokastaða deildarkeppni (spáð)"**
- Disclaimer, under the heatmap and repeated in the Aðferð partial: **"Spáin nær aðeins til deildarkeppninnar. Íslandsmeistari ræðst í úrslitakeppni sem líkanið spáir ekki fyrir um."**
- Qualification fact label, from `meta.qualify.label_is`: **"Líkur á úrslitakeppni"** (bb/hb) — never "Í toppbaráttu", which is the top-six framing.
- Fixture-card legend, basketball: **"Súlur: líkur á heimasigri · útisigri"** (jafntefli removed, not zeroed) and **"Stigamunur: 0 í miðju · lína = vænt gildi"**. Handball: **"Markamunur: 0 í miðju · lína = vænt gildi"**. Neither centre bucket is jafntefli.
- Fixture-card score footer, replacing the hardcoded `xG` at `next-games-grid.js:255`: **"Vænt mörk"** (handball, 1 decimal) / **"Vænt stig"** (basketball, 0 decimals). The third legend span "xG = vænt mörk" is deleted for both.
- Standings score-column group head: **"Mörk"** for handball, **"Stigaskor"** for basketball — because the league-points group in the same two-row thead is also "Stig" and two identical group heads are unreadable.
- Division titles: **Olísdeild**, **Grill 66-deild**, **Bónusdeild**, **1. deild**; the H1 composes as "{title} {sex}" exactly as football does.
- Off-season in-page empty state: **"Tímabilið er ekki hafið — spár birtast þegar deildin byrjar."**

The word Íslandsmeistari appears nowhere on a basketball or handball page.

## 16. Documentation and the rules that a future session will trust

Four documents currently assert things this spec makes false, and a stale rule is what the next session will believe.

- `R/publish-pipeline.R:20-31` — the `publish_one()` docstring states that basketball and handball "still read the fit RDS directly ... their migration to the extraction layer is deferred to the autumn 2026 cutover". This *is* that cutover; the docstring is rewritten in the same commit.
- `.claude/rules/publish-layer.md` — currently describes a football extracts tree and a basketball/handball legacy fit-RDS path. Rewritten to the single extracts path, the division loop, the schema-generation workflow, the fail-closed validation default, and the `surfaces` model.
- `metill-platform/.claude/rules/ithrottir.md` — its "Platform scope" section says football-only until autumn 2026 and describes the bb/hb data as parked. Rewritten, with the `paths:` frontmatter extended to `data/ithrottir/{basketball,handball}/**`.
- `CLAUDE.md` — the metill-platform integration section and the Quick reference stay accurate, but `.claude/rules/` gains a short note on the derived season-resolution contract and on `config/federation-seasons.json` being a provenance cache rather than hand-maintained config.

A memory topic file records: the extracts tree was already CI-committed and unread (N1); `betting` and `publishDivisionList` are `additionalProperties: false` (N2); the 2DT models expose a per-round `offense`/`defense` trajectory, so `team_strengths_history` is reachable for all three sports; and the season-registry pattern is now derived-with-cache for both federations.

---

## Workstreams

### WS1 - Betting interlock: schema key + config + runtime flag `[sports]`

Add `betting.enabled` (boolean, not required) to config/leagues.schema.json's betting.properties — N2: betting is additionalProperties:false, so the key is rejected at load_leagues() without this edit. Set `enabled: false` for basketball_iceland and handball_iceland, and empty their `lengjan.competitions` with the ids preserved in a comment. Add `betting_enabled(league)` consulted by ingest_one_lengjan() and decide_league(). Teach check_odds_freshness and check_capture_rate to report a betting-disabled league as PAUSED rather than unhealthy. Lands before any ingest change: the moment fixtures reappear, the launchd autoplace agent would otherwise stake real money on handball, which the 2026-06-13 methodology verdict records as not Lengjan-bankable.

**Verification.** load_leagues() succeeds (schema edit is real, not assumed). Run scripts/02_scrape_odds.R against a scratch root with a synthetic handball fixture inside the odds window and assert zero odds rows written and a log line naming the flag. Run scripts/04_decide.R against a scratch root seeded with handball odds rows injected directly, and assert zero candidate rows — this proves the decide-layer guard independently of the ingest guard, so a future re-add of the competition ids cannot re-enable placement.

### WS2 - Test and fixture harness (RED before any behaviour change) `[sports]`

tools/make-extract-fixtures.R writes a committed synthetic extracts tree under tests/testthat/fixtures/extracts/ for all three sports (2 divisions, 10 teams, 50 draws, fit_date=2100-01-01, <250KB). helper-stub-fit.R provides stub_fit(draws_list) exposing $draws(var) over small committed posterior::draws_array objects for the 2DT variable set, which un-gates the extractor tests without the 300-600MB fit RDS. Delete the 12 machine-local-path tests in test-publish-{basketball,handball}.R (they exercise the publish_*(fit, ...) signature workstream 9 removes). Add a convention test that greps these files for skip(/skip_if(/skip_if_not(/Sys.getenv and fails on any hit.

**Verification.** Run devtools::test() with SPORTS_BACKUP_ROOT unset and data/beliefs/fits/ moved aside; assert the basketball and handball publish/extract test files report zero skips and a non-zero pass count. Capture the pass count as the baseline with its timestamp. Then write the seven behavioural assertions from §4 and confirm assertions 1 (units), 3 (no silent skip) and 6 (season stamp) FAIL against current main — a RED proof re-run by hand, not taken from a report.

### WS3 - B5: remove the exp() AND the /2 from 2DT home advantage `[sports]`

In .compute_home_advantage_quantiles_2dt (R/extract-iceland-2dt-shared.R:194-216) drop both the exp() wrapper (applied via the shared extract_one to all three components) and the transform = x/2 on `total`. Stan/basketball_iceland/2d_student_t_scalarsigma.stan:112-116 declares home_advantage_off/def as vector<lower=0>[K] entering the mean linearly, and :277 defines home_advantage_tot = off + def — additive raw points, so neither operation has a meaning. Replace the copied football comment with a cross-reference to R/publish-iceland-2dt-helpers.R:293-296. Promote the warn-only tryCatch at R/model-league.R:288-304 to an abort, since extracts become the sole publish input.

**Verification.** The RED test from workstream 2 flips GREEN: a stub fit whose home_advantage_tot draws are all 4.0 publishes a median of exactly 4.0 (assert to 1e-9, not a bound — a bound would also pass on exp(4/2)=7.389 under a loose threshold). Separately, deliberately break the extractor (raise inside .compute_home_advantage_quantiles_2dt) and confirm scripts/03_fit.R now exits non-zero instead of logging a warning and continuing.

### WS4 - HSÍ derived season resolution, the season-stamp guard, and the female G66 2025 backfill `[sports]`

Collapse HSI_URLS and HSI_HISTORICAL_IDS into one HSI_TOURNAMENT_IDS[[sex]][[div]][["<season>"]] registry; every URL becomes /tournament/<id>. hsi_current_season() names the requested season and never selects a URL. Add hsi_discover_tournaments() (chromote, via the existing fetch_hsi_html) reading ids and titles off hsi.is, refresh_federation_seasons() writing config/federation-seasons.json with provenance, and .assert_season_stamp(rows, season, tol=0.05) aborting inside hsi_fetch_and_parse when >5% of parsed match_date years fall outside {season-1, season}. Seed 2027 ids (9142/9140/9141/9143/8437/8436) and the female div2 2025 candidate 7643, all as provenance-stamped cache entries.

**Verification.** Live chromote run against a scratch storage root, not a curl — the tournament pages are JS-rendered. (a) fetch_results_hsi(NULL, "male") writes only season=2027 partitions and the parsed dates lie in Sept 2026-May 2027. (b) Deliberately point the 2027 male div1 key at the 2025 id and confirm the run ABORTS on .assert_season_stamp rather than writing rows — this is the guard's RED proof and must be run, not reasoned about. (c) For 7643: assert the page title matches the female Grill 66 pattern, that .assert_season_stamp passes for season 2025, and that the parsed team set intersects the known 2024 and 2026 female G66 squads. Only then commit it. (d) After backfill, query data/facts/results and confirm handball female G66 season 2025 is non-empty for the first time.

### WS5 - KKÍ derived season resolution keyed on league_id `[sports]`

Add KKI_LEAGUE_IDS (male div1 = 190 per N3; the other three established by discovery) as the stable key, demote KKI_SEASON_IDS to a verified 2021-2026 cache, and add kki_discover_season_ids(sex, div) driving chromote over kki.is/motamal/leikir-og-urslit/motayfirlit/Leikir?league_id=<id> to read the JS-rendered season selector. Change fetch_kki(seasons = NULL) from 'iterate every registry key' to 'current season only, resolved registry -> cache -> discovery -> NULL', keeping explicit seasons= for backfill. Apply .assert_season_stamp to the parsed XLSX rows.

**Verification.** (a) kki_discover_season_ids("male", "div1") returns 130403 for 2026 — the value already in the registry — proving the resolver agrees with hand-verified truth before it is trusted for an unknown season. (b) The three unknown league_ids are confirmed by asserting each discovered page's title matches its division and that its 2026 season_id equals the registered one. (c) Fetch the resolved 2027 season and assert a non-header-only XLSX (the existing PK magic-byte check) and dates in Oct 2026-Apr 2027. (d) Instrument fetch_kki and assert the daily call count drops from 48 to 8.

### WS6 - Ingest activation gate: delete it for federation ingest, replace the cost control `[sports]`

Remove the .is_league_active() call from ingest_one_league() (R/ingest.R:136-140), keeping the active_path argument so scripts/01_ingest_results.R:39 is unchanged and keeping the gate in ingest_one_lengjan() where it is correct. Replace the saved-chromote-launch intent with a control whose input is fetch state, not fetched data: data/health/ingest_log.json records (league, sex, last_attempt_at, last_rows), and a (league, sex) is skipped only when its last attempt was within offseason_min_interval_hours (24) AND its last three attempts all returned zero rows. Wire opts$force into 01_ingest_results.R (parsed by parse_pipeline_args and currently never read). Merge in ONE PR with workstreams 4 and 5 so no CI run ever sees the gate lifted while a season resolver is stale. Football hits this identical wall after its last fixture on 2026-10-25.

**Verification.** (a) On a scratch root with config/active_competitions.json marking both sports false, run scripts/01_ingest_results.R and assert non-zero rows written for both — the deadlock is provably broken, not merely bypassed. (b) Simulate a dormant league (three consecutive zero-row attempts) and assert the fourth run within 24h skips, then that a run after 24h attempts again — proving the control cannot deadlock. (c) On the real repo, run 00 then 01 in sequence exactly as scrape-results.yml does, and confirm step 00 rewriting the JSON no longer suppresses step 01.

### WS7 - publish_divisions config, schema keys, and generalised division helpers `[sports]`

Add publish_divisions blocks for both sports — basketball {BD/bd, 1D/1d}, handball {OD/od, G66/g66}, each sex — every entry carrying the schema-required is_cup (N2). Extend definitions.publishDivisionList.items.properties with expected_meetings, qualify_slots, relegation_slots and code_badge, none of them required. Rename the four .football_iceland_division_{codes,slugs,labels,split}(sex) helpers to .iceland_division_*(key, sex), add .iceland_division_{qualify,relegation,badges}, and update all eight external call sites. No compatibility aliases. Move the static .football_iceland_division_code_labels() map into config as code_badge so football BD (Besta deild) and basketball BD (Bónusdeild) stop colliding on the client filter key. qualify_slots and relegation_slots must be read off the 2026-27 KKÍ and HSÍ competition regulations by a human before commit.

**Verification.** (a) load_leagues() succeeds against the edited schema for all three leagues — run it, do not assume, since additionalProperties:false rejects at load and would take every script down. (b) A test asserts 0 <= qualify_slots < n_teams for every configured division against the actual current-season team counts in data/facts/results. (c) A test asserts .iceland_division_codes("football_iceland", sex) returns byte-identical output to the pre-rename .football_iceland_division_codes(sex) for both sexes, so the football path is provably unchanged. (d) grep the tree for surviving .football_iceland_division_ references and assert zero.

### WS8 - 2DT extractor: division loop, round-strength trajectory, shared fit_meta `[sports]`

Replace .extract_2dt_iceland_pfi's top_div scalar with a divisions vector from .iceland_division_codes(key, sex) and loop, hoisting the cross-division inputs (posterior_goals, teams, raw draws, prep) and moving the per-division quantities inside; bind a `division` payload column onto all five parquets. Add round_strengths_quantiles.parquet by generalising .compute_team_strength_trajectory_pfi to pull the 2DT variable names — newly established as feasible: Stan/basketball_iceland/2d_student_t_scalarsigma.stan:153-177 declares array[N_rounds] vector[K] offense and defense as a transformed-parameter random walk, and prepare_data() already builds N_rounds/round1/round2. Write fit_meta.parquet (n_draws, fit_date, stan_model, model_units) on BOTH the football and 2DT trees, so the two partition shapes converge at the only moment they are free to.

**Verification.** (a) Run the extractor against a real 2DT fit and assert six parquets per partition, each carrying a `division` column with exactly the configured codes. (b) Assert the second division's final_positions team set is disjoint from the first's — the specific hazard is .compute_final_positions_2dt filtering internally and silently simulating a lower-tier table on top-tier base points. (c) Assert round_strengths_quantiles has one row group per (round, team, component, location) with round running 1..N_rounds and no NA values, and spot-check that a team's final-round `avg` strength equals its cur_strength posterior median to 1e-6 — this proves the trajectory is the same quantity the existing team_strengths surface reports, not a differently-indexed one. (d) Assert football's six parquets are byte-identical before and after fit_meta is added.

### WS9 - One reader, one publisher, one dispatch: delete the 2DT publishers `[sports]`

read_extracted_football -> read_extracted_iceland(league, sex, fit_date, extracts_root, target_divs, profile) in a new R/extract-iceland-read.R, with exactly three edits to its body (sport check, file_types -> profile$required_extracts/optional_extracts, empty_tibbles -> profile$empty_extracts); the newest-complete-partition scan and incomplete-partition abort are copied zero times. football_extract_partition_exists -> extract_partition_exists. Add sport_publish_profile(sport) carrying only real per-sport differences plus a `surfaces` vector (replacing four parallel booleans that all take one value per sport). publish_football_iceland -> publish_iceland_league(extracted, league, sex, division, profile, ...), gating the football-only surfaces on surfaces membership and driving the standings tabulation off profile$points. Delete publish_basketball_iceland and publish_handball_iceland (737 lines) and the .compute_standings_rows_2dt duplicate. Rewrite publish_one to a single extracts path: no fit_path fallback, no dispatch list. Adopt football's next_games field names by deleting the bespoke summarise at R/publish-basketball-iceland.R:130-155 in favour of .compute_predicted_matches_2dt, which supplies goal_diff_distribution for free.

**Verification.** (a) The football golden-file test: publish BD/LD1/LD2/LD3/CUP x both sexes from a pinned fit_date partition before and after, assert byte-identical JSON modulo generated_at. This is the gate on the whole workstream and must be green before the deletions land. (b) From a clean checkout with data/beliefs/fits/ deleted, run scripts/05_publish.R and assert all eight bb/hb cells are written — this is the actual B4 proof and it must be run in the RDS-absent condition, since the RDS is present on the dev machine and its presence would mask the failure. (c) Assert publish_one raises for an in-season cell with no extract partition (the RED test from workstream 2). (d) grep for surviving publish_basketball_iceland / publish_handball_iceland / read_extracted_football references and assert zero.

### WS10 - meta.json v2: units, n_rounds, round, points, qualify, basis, and the D3 relabel `[sports]`

meta.json gains units {strength, home_advantage, diff_bin_width}, points {win, draw, loss}, n_rounds + n_rounds_source, a corrected round, season_scope, postseason, is_cup, division and qualify {slots, label_is}. `round` becomes min(played) over the division's current-season teams for all three sports, replacing .compute_round_num_2dt's team-appearance index (basketball reads 22 against played:35). `n_rounds` is derived as max over teams of played + remaining_scheduled from results union schedules, falling back to expected_meetings * (n_teams - 1) with the source stamped. final_positions/points_distribution gain required `basis`, emit p_qualify against the configured qualify_slots, and emit p_winner only when basis == final_table; football keeps p_top_six as an alias with a named removal commit. All D3 Icelandic strings from §15, grammar-checked with Miðeind.

**Verification.** (a) Publish Bónusdeild karla from real data and assert meta.n_rounds == 44 (4 meetings x 11 opponents) and meta.round == min(played), then assert the platform's Umferðir-eftir arithmetic n_rounds - round is non-negative — the concrete number that renders as -13 af 22 today. (b) Assert n_rounds >= round and round <= max(played) for every published cell across all three sports. (c) Assert p_qualify sums correctly against qualify_slots (the mean of an indicator over draws) and that no bb/hb payload contains the key p_top_six. (d) grep every published bb/hb JSON and every platform template for the substring Íslandsmeistar and assert zero hits.

### WS11 - Schema generation, subtree validation, and arming `[sports]`

Fix .validate_or_abort to validate file.path(output_root, sport) rather than the whole publish tree — today creating config/publish-schemas/basketball/ arms basketball validation inside football's publish call and aborts it on the stale June JSON. Add a schema_dir parameter (currently hardcoded to here::here("config","publish-schemas")) so the draft workflow can be exercised through the publisher. Invert the default: a sport that publishes with no schema directory aborts. Build config/publish-schemas/_base/ + _delta/<sport>/ and tools/gen-publish-schemas.R rendering the committed per-sport files, rejecting the DIVERGENCES.yml register (semantic JSON-Schema diffing costs more than the generator it substitutes for and only detects drift). Author the bb/hb schemas under config/publish-schemas/_draft/, arm with a single git mv in the same commit as the conforming JSON.

**Verification.** (a) With the _draft schemas present, run scripts/05_publish.R and confirm zero validation lines mention basketball or handball — proving _draft resolves in neither resolver. (b) With the schemas armed but publishing deliberately broken (rename one required key), assert publish_one aborts for that sport AND that football's cells still republish, proving the subtree fix and the per-cell tryCatch together. (c) Re-render the generator into a temp dir and assert a byte match with the committed tree. (d) Delete config/publish-schemas/basketball/ and assert the next publish aborts rather than skipping — proving the inverted default. (e) Run metill-platform's scripts/validate_publish.py against the rsynced tree and assert zero files report 'unmatched'.

### WS12 - Health checks and the two abort guards `[sports]`

Add check_publish_freshness (per configured cell of an active league with upcoming games: FAIL on a missing meta.json, on generated_at older than th$publish_max_age_hours, on an artefact set that disagrees with the sport's declared surfaces, and — correcting a specification error in one of the source proposals — FAIL rather than PAUSED when no extract partition exists at all for an in-season cell, since that state IS the current silent breakage). Add check_season_resolution (FAIL when an active league's current season has no resolvable federation id) and check_publish_format_agreement (WARN on derived-vs-configured n_rounds disagreement, or qualify_slots >= n_teams). Wrap fit_one() in scripts/03_fit.R:34-54 and publish_one() in scripts/05_publish.R in per-cell tryCatch that records, continues and exits non-zero; set fit.yml's commit step to if: always().

**Verification.** (a) Delete one cell's meta.json and assert check_publish_freshness reports FAIL with that cell named; restore it and assert PASS. (b) Remove the 2027 key from HSI_TOURNAMENT_IDS and assert check_season_resolution FAILs — this is the alarm for the exact state the pipeline is in today. (c) Force a fit abort by injecting a diagnostics-gate breach on basketball male, then assert football's fits still complete, that data/beliefs/ is still committed, and that the run exits non-zero. All three properties, not just the last. (d) Same injection through publish_one, asserting football's JSON still republishes.

### WS13 - metill-platform sport dimension `[metill-platform]`

Replace LEAGUES + DIVISIONS with the SPORTS registry keyed by URL slug (fotbolti as an ordinary entry so football URLs are served by the same handler); thread sport_slug through _league_data_dir, _division_available_for_sex, _build_league_context and all three routes; rename ithrottir_fotbolti.html to ithrottir_league.html with three per-sport Aðferð partials; add noDraw, highlightZero:false, scoreLabel and scorePrecision as opt-in JS options; registry-derive sport_tabs with an availability gate so an unpublished sport degrades to the existing '· Síðar' chip and lights up on the next data pull with no deploy. Read n_rounds/round/points from meta rather than computing them. Add sport to og.py's _cache_key. Read p_qualify. Add the 8 short links and the sitemap loop. No _normalise_next_games shim.

**Verification.** (a) A regression list of the 9 existing football URLs asserted 200 with byte-identical rendered HTML modulo timestamps — football is an ordinary registry entry and must be provably unchanged. (b) A parametrised sweep over SPORTS x SEXES x divisions asserting 200 for every published cell and 404 for an unknown sport slug. (c) Render Bónusdeild karla and assert the Umferðir-eftir string is non-negative and the ticker reads 35/44 — the concrete blocker. (d) Render an OG card for football karla-bd and basketball karla-bd in sequence and assert the two PNGs differ, proving the cache-key collision is closed. (e) Render a basketball fixture card and assert no 'jafntefli' string and no jökull-highlighted centre bucket.

### WS14 - Documentation, rules and memory `[sports]`

Rewrite the publish_one docstring (R/publish-pipeline.R:20-31 still says the bb/hb migration is 'deferred to the autumn 2026 cutover'), .claude/rules/publish-layer.md (still describes the legacy fit-RDS path), and metill-platform/.claude/rules/ithrottir.md (still says football-only until autumn 2026, with football-only paths: frontmatter). Add a rules note on derived season resolution and on config/federation-seasons.json being a provenance cache. Record in the memory topic file: N1 (extracts already CI-committed and unread), N2 (both config objects are additionalProperties:false), the 2DT per-round trajectory finding that makes team_strengths_history reachable, and the derived-registry pattern for both federations.

**Verification.** grep .claude/rules/ and every roxygen block for the strings 'fit.rds', 'deferred', 'autumn 2026', 'football-only' and 'parked', and assert every surviving hit is deliberate. Confirm metill-platform/.claude/rules/ithrottir.md's paths: frontmatter actually matches data/ithrottir/basketball/** by loading it. Every shipped guard in the memory note cites its merge commit SHA, so the next session checks staleness with one git branch --contains.

---

## metill-platform: files to change

| file | change | risk |
|---|---|---|
| `app/routes/ithrottir.py` | The core change. Replace LEAGUES (43-52) + DIVISIONS (65-71) with the SPORTS registry + SEXES set. Thread sport_slug through _league_data_dir (110-117, plus the legacy-dir transition shim), _division_available_for_sex (541-552), _build_league_context (276-530) and all three routes (555, 567, 582). Add _normalise_next_games() mapping mean_home/mean_away/mean_diff/p_home/p_away/p_tie/division to the football key names when the football keys are absent, applied in both the upcoming filter (293-297) and the /data/next_games endpoint. Replace the hardcoded sport_tabs stubs (473-477) with the registry-derived list + _sport_available(). Fix total_rounds (406): prefer meta.n_rounds, else division meetings x (n_teams - 1), and clamp so 'Umferðir eftir' never goes negative. Gate the two Íslandsmeistari statelines (456, 460) and the Efst spáð fact sub (424) on sport.title_decides_champion. Add sport + sport_slug + playoff_note to the returned context dict. | Highest-risk file. 588 lines, every route and the whole context builder touched. Mitigation: football is an ordinary registry entry so its URLs and rendered output must be byte-identical — assert that with a golden-output test on /ithrottir/fotbolti/iceland/karla/besta/ before and after. |
| `app/templates/ithrottir_fotbolti.html` | Rename to ithrottir_league.html. Extract lines 100-160 (the Aðferð prose block) into three per-sport partials. Parameterise the og_image block (line 5) to emit the 3-part id for non-football sports. Interpolate sport_slug into the dataBase JS const (line 200-ish) and add the noDraw / highlightZero / scoreGroup / scoreSub / scoreLabel opts to the renderFixturesGrid and renderStandingsTable call sites. Bump every ?v=N cache-buster. | The ?v=N buster appears twice per module (modulepreload in {% block head %} and the import line in {% block scripts %}); a mismatch causes a silent double-fetch of the module. Bump both. |
| `app/templates/components/ithrottir/method_fotbolti.html` | NEW. The existing football Aðferð prose (bivariate Poisson, xSkoruð/xFengin/xStig, Δ/Form, markamismunadreifing) moved verbatim out of the page template, including the is_cup branch. | Low — pure move. Verify the {% if is_cup %} branch survives the extraction. |
| `app/templates/components/ithrottir/method_handbolti.html` | NEW. 2DT Student-t framing; no xG paragraph; the D3 playoff disclaimer; a note that the goal-diff strip's centre bucket spans ±1 mark and is not the draw probability. | New Icelandic copy — run through the grammar checker before shipping. |
| `app/templates/components/ithrottir/method_korfubolti.html` | NEW. As handball, plus: no draws in basketball, the centre bucket spans ±2 stig, and 'stig' means two different things in the standings (Stigaskor vs Stig). | Same. |
| `app/templates/components/ithrottir/finishing_heatmap.html` | Line 11 panel-title 'Lokastaða (spáð)' becomes 'Lokastaða deildarkeppni (spáð)' when not sport.title_decides_champion. Render sport.playoff_note as a panel-caption line under it. | Low. |
| `app/templates/components/ithrottir/fixture_card.html` | Legend (lines 17-21): drop the 'jafntefli' term from the bars legend when not sport.has_draws; drop the 'xG = vænt mörk' span entirely when not sport.has_xg; reword 'Markamismunur' to 'Stigamunur' for basketball and drop 'jafntefli í miðju' for both new sports. | Low. |
| `app/templates/components/ithrottir/standings_table.html` | Line 10 prose hardcodes 'spáðum markamismun' — parameterise on sport.score_noun. | Low. |
| `app/templates/components/ithrottir/jump_rail.html` | The Spá anchor label stays 'Spá'; no change needed. Included only to confirm it was checked. | None. |
| `app/static/js/next-games-grid.js` | renderCard: add opts.noDraw (two-way H/Ú probs + two-segment probBar, distinct from the existing advance_home knockout path); add opts.scoreLabel replacing the hardcoded 'xG' at line 255; add opts.scorePrecision (0 for basketball). renderGoalDiffStrip: add opts.highlightZero (default true) and derive barW from the minimum inter-bucket gap rather than 100/(span+1). | Shared with the HM 2026 page (hm2026 reuses renderFixturesGrid). All new behaviour must be opt-in with football/WC defaults preserved. tests/test_hm2026_routes.py and test_ithrottir_wc26.py guard that. |
| `app/static/js/standings-table.js` | Add opts.showDraws (drop the J th at line 331 and the draws td at line 359), opts.scoreGroup (the 'Mörk' group head, line 322) and opts.scoreSub (the 'Skoruð'/'Fengin' sub-heads, lines 340-342). Add showFravik, computed from the data, to hide the always-empty Frávik column when no row yields a residual series. No change needed for the xG/Δ columns — showExpectedCols already handles it. | Column-count arithmetic: morkSpan/stigSpan (311-312) plus the new showDraws must keep the two-row thead's colspans consistent, or the table shears. Snapshot-test both the football (draws + xG) and basketball (neither) headers. |
| `app/routes/og.py` | ITH_DIVISION_DIR (625) becomes per-sport, derived from ithrottir.SPORTS. _ithrottir_league_dir (631) takes sport. generate_ithrottir_card: replace max_points = round_num * 3 (696) with max(r['played'] for r in top) * sport['points_win'] — the *3 is wrong for both new sports AND meta.round is unreliable for basketball (reads 22 against 35 played). _card_png (953-957) splits page_id on '-' and treats a 2-part id as fotbolti so every crawled football OG URL keeps working. ITH_LEAGUES (898) gains the 8 new cells for prewarm. | OG cards are cached on disk keyed by a fingerprint that does not include the sport — _cache_key('ithrottir', sex, division, fp) would collide between football karla-bd and basketball karla-bd. Add sport to the cache key. |
| `app/routes/pages.py` | _SITEMAP_ENTRIES (29-61): replace the 9 hand-listed football rows with a loop over ithrottir.SPORTS x SEXES x divisions (skipping cells whose meta.json is absent), giving 17 rows. _LEAGUE_SHORTLINKS (180-191): add the 8 new slugs listed in url_scheme; do not touch the existing 9. | pages.py importing from ithrottir.py is a new coupling — check no import cycle (ithrottir.py does not import pages.py, so it is clean). A generated sitemap that silently drops a cell when meta.json is briefly missing mid-rsync is acceptable; a crash is not — guard the loop. |
| `app/templates/base.html` | The Íþróttir nav dropdown (196) currently has a single football item. Add Handbolti and Körfubolti items pointing at each sport's karla top division so the new sports are discoverable from every page. Mobile nav (252) can stay pointing at football. | Low, but the dropdown's aria-current logic keys on _path.startswith('/ithrottir') (172) and would mark all three active simultaneously. Narrow it per item. |
| `data/ithrottir-schemas/` | NEW basketball/ and handball/ namespaces mirroring football/. scripts/validate_publish.py currently returns 'unmatched' (informational, not an error) for any sport without a schema dir, so bb/hb JSON is ungated at BOTH ends. A home_advantage.schema.json with a sane median bound would have caught the sports-side B5 exp()-transform bug (a 4-point home edge publishing as 54.6). | Fails closed once added — a schema that is stricter than reality would block the pull entirely and freeze the site on the last-known-good payload. Derive the schemas from the actual on-disk JSONs, then loosen. |
| `tests/test_ithrottir_routes.py` | Add a parametrised sweep over SPORTS x SEXES x divisions asserting 200 for every published cell and 404 for unknown sport slugs. Add an explicit regression list of the 9 existing football URLs asserting 200 (they must not break). Extend the fact-label assertions to the D3 wording for bb/hb. | Tests currently assert football-specific fact labels ('Efst spáð', 'Fallhætta') that must survive unchanged for football. |
| `.claude/rules/ithrottir.md` | The rule's 'Platform scope' section explicitly says football-only until autumn 2026 and describes the bb/hb data as parked. Rewrite it to the multi-sport model, update the paths: frontmatter to cover data/ithrottir/{basketball,handball}/**, and correct the 'Source of truth' claim that bb/hb publish to single-division dirs. | Stale rule text is what a future session will trust — this is the highest-value doc change, not an afterthought. |

### URL scheme

**Football routes do not change.** The literal segment `fotbolti` becomes a registry key, so the generalised handler serves the identical paths.

New route patterns in `app/routes/ithrottir.py`:

| Pattern | Replaces |
|---|---|
| `GET /ithrottir/{sport_slug}/iceland/{sex_slug}/{division_slug}/` | `GET /ithrottir/fotbolti/iceland/{sex_slug}/{division_slug}/` (line 555) |
| `GET /ithrottir/{sport_slug}/iceland/{sex_slug}/{division_slug}/data/{dataset}` | line 567 |
| `GET /ithrottir/{sport_slug}/iceland/{sex_slug}/` → 301 to `…/{default_division}/` | line 582 |

Guard (extends the existing 404 tuple): `sport_slug not in SPORTS or sex_slug not in SEXES or division_slug not in SPORTS[sport_slug]["divisions"] or not _division_available_for_sex(...)` → 404. An unknown sport slug (e.g. `/ithrottir/sund/iceland/karla/besta/`) 404s cleanly.

No path-matching conflict: `/ithrottir/adferdafraedi` is one segment, `/ithrottir/` is zero, the new pattern is four or five. FastAPI resolves by segment count and the literal routes are registered first in the module anyway.

**Concrete new URLs (8 cells, D1/D4):**

```
/ithrottir/korfubolti/iceland/karla/bonus/     -> data/ithrottir/basketball/iceland/karla-bd/
/ithrottir/korfubolti/iceland/karla/1deild/    -> .../basketball/iceland/karla-1d/
/ithrottir/korfubolti/iceland/kvenna/bonus/    -> .../basketball/iceland/kvenna-bd/
/ithrottir/korfubolti/iceland/kvenna/1deild/   -> .../basketball/iceland/kvenna-1d/
/ithrottir/handbolti/iceland/karla/olis/       -> .../handball/iceland/karla-od/
/ithrottir/handbolti/iceland/karla/grill66/    -> .../handball/iceland/karla-g66/
/ithrottir/handbolti/iceland/kvenna/olis/      -> .../handball/iceland/kvenna-od/
/ithrottir/handbolti/iceland/kvenna/grill66/   -> .../handball/iceland/kvenna-g66/
```

plus `/ithrottir/korfubolti/iceland/{sex}/` and `/ithrottir/handbolti/iceland/{sex}/` 301-redirecting to the top division, mirroring football's existing legacy redirect.

The URL slug and the disk suffix are deliberately different (`bonus`→`bd`, `olis`→`od`, `grill66`→`g66`), following the existing football precedent where `besta`→`bd` and `lengja`→`ld`. `data/publish/basketball/iceland/karla-bd/` and `data/publish/football/iceland/karla-bd/` share a suffix but live under different sport folders, so there is no collision.

**Landing redirect** (`/ithrottir/` → `…/karla/besta/`, line 538) is unchanged — football stays the section's canonical entry point.

**Short links** in `app/routes/pages.py::_LEAGUE_SHORTLINKS` (line 180). The existing 9 are a stable public contract (football reel outros deep-link to them) — do not touch. Add 8, sport-disambiguated where the bare name would be ambiguous:

```python
("olisdeild-kk",     "/ithrottir/handbolti/iceland/karla/olis/"),
("olisdeild-kvk",    "/ithrottir/handbolti/iceland/kvenna/olis/"),
("grill66-kk",       "/ithrottir/handbolti/iceland/karla/grill66/"),
("grill66-kvk",      "/ithrottir/handbolti/iceland/kvenna/grill66/"),
("bonusdeild-kk",    "/ithrottir/korfubolti/iceland/karla/bonus/"),
("bonusdeild-kvk",   "/ithrottir/korfubolti/iceland/kvenna/bonus/"),
("korfu-1deild-kk",  "/ithrottir/korfubolti/iceland/karla/1deild/"),
("korfu-1deild-kvk", "/ithrottir/korfubolti/iceland/kvenna/1deild/"),
```

`korfu-1deild-*` rather than `1deild-*` because football already owns `2deild-*`/`3deild-*` and a bare `1deild-kk` would read as football's (non-existent) first tier.

**OG image URLs** stay backwards-compatible. The template emits `/og/ithrottir/{{ sex }}-{{ division }}.png` for football (2 parts, unchanged, every crawled URL stays alive) and `/og/ithrottir/{{ sport }}-{{ sex }}-{{ division }}.png` for the new sports (3 parts). `og.py::_card_png` splits on `-` and treats a 2-part id as `fotbolti`.

### Sport registry

Replaces `LEAGUES` (lines 43-52) and `DIVISIONS` (lines 65-71) in `/Users/brynjolfurjonsson/metill-platform/app/routes/ithrottir.py`. One module-level dict keyed by **URL sport slug**; football is an ordinary entry, which is what keeps its URLs unchanged.

```python
# Sport dimension. Key = URL slug (Icelandic); `dir` = on-disk sport folder
# (English, mirrors ~/sports/data/publish/). Football is a registry entry like
# any other, so /ithrottir/fotbolti/... is served by the generalised route and
# its URLs are unchanged.
#
# score_group / score_sub  : standings column-group head + its two sub-heads.
#   Basketball's league points are ALSO "stig", so the scored/conceded group is
#   "Stigaskor" to keep the two-row thead unambiguous.
# has_draws                : False -> the J column and the middle 1X2 column
#                            are removed (not rendered as zero).
# points_win/points_draw   : OG card max-points axis + methodology copy.
# meetings                 : times each pair meets; total_rounds fallback.
#                            PREFER meta.json::n_rounds when the sports side
#                            publishes it (see open_risks).
# has_xg                   : documentation only -- standings-table.js already
#                            auto-hides on xpts == null. Drives the fixture-card
#                            score label and the legend copy.
# title_decides_champion   : D3 lever. False -> every "Íslandsmeistari" string
#                            becomes a deildarkeppni (regular-season) string.
# playoff_note             : the D3 disclaimer, None for football.
SPORTS: dict[str, dict] = {
    "fotbolti": {
        "dir": "football",
        "label": "Knattspyrna",
        "score_group": "Mörk", "score_sub": ("Skoruð", "Fengin"),
        "score_label": "xG", "score_noun": "mörk",
        "has_draws": True, "has_xg": True,
        "points_win": 3, "points_draw": 1,
        "title_decides_champion": True,
        "playoff_note": None,
        "default_division": "besta",
        "divisions": {   # unchanged from today's DIVISIONS
            "besta":  {"dir": "bd",     "title": "Besta deild",  "code": "BD", "is_cup": False, "meetings": 2, "sexes": {"karla", "kvenna"}},
            "lengja": {"dir": "ld",     "title": "Lengjudeild",  "code": "LD", "is_cup": False, "meetings": 2, "sexes": {"karla", "kvenna"}},
            "2deild": {"dir": "2deild", "title": "2. deild",     "code": "D2", "is_cup": False, "meetings": 2, "sexes": {"karla", "kvenna"}},
            "3deild": {"dir": "3deild", "title": "3. deild",     "code": "D3", "is_cup": False, "meetings": 2, "sexes": {"karla"}},
            "bikar":  {"dir": "bikar",  "title": "Mjólkurbikar", "code": "MB", "is_cup": True,  "meetings": 1, "sexes": {"karla", "kvenna"}},
        },
    },
    "handbolti": {
        "dir": "handball",
        "label": "Handbolti",
        "score_group": "Mörk", "score_sub": ("Skoruð", "Fengin"),
        "score_label": "Vænt mörk", "score_noun": "mörk",
        "has_draws": True, "has_xg": False,
        "points_win": 2, "points_draw": 1,
        "title_decides_champion": False,
        "playoff_note": ("Spáin nær aðeins til deildarkeppninnar. "
                         "Íslandsmeistari ræðst í úrslitakeppni sem líkanið spáir ekki fyrir um."),
        "default_division": "olis",
        "divisions": {
            "olis":    {"dir": "od",  "title": "Olísdeild",      "code": "OD",  "is_cup": False, "meetings": 2, "sexes": {"karla", "kvenna"}},
            "grill66": {"dir": "g66", "title": "Grill 66-deild", "code": "G66", "is_cup": False, "meetings": 2, "sexes": {"karla", "kvenna"}},
        },
    },
    "korfubolti": {
        "dir": "basketball",
        "label": "Körfubolti",
        "score_group": "Stigaskor", "score_sub": ("Skoruð", "Fengin"),
        "score_label": "Vænt stig", "score_noun": "stig",
        "has_draws": False, "has_xg": False,
        "points_win": 2, "points_draw": 0,
        "title_decides_champion": False,
        "playoff_note": ("Spáin nær aðeins til deildarkeppninnar. "
                         "Íslandsmeistari ræðst í úrslitakeppni sem líkanið spáir ekki fyrir um."),
        "default_division": "bonus",
        "divisions": {
            "bonus":  {"dir": "bd", "title": "Bónusdeild", "code": "BD", "is_cup": False, "meetings": 4, "sexes": {"karla", "kvenna"}},
            "1deild": {"dir": "1d", "title": "1. deild",   "code": "1D", "is_cup": False, "meetings": 2, "sexes": {"karla", "kvenna"}},
        },
    },
}

SEXES = {"karla", "kvenna"}   # replaces LEAGUES; the per-sex `title` it carried
                              # was already overwritten at line 283 by
                              # f"{div['title']} {sex_slug}", so nothing is lost.
```

`_league_data_dir` (currently line 110-117) becomes:

```python
def _league_data_dir(sport_slug: str, sex_slug: str, division_slug: str) -> Path:
    sport = SPORTS[sport_slug]
    suffix = sport["divisions"][division_slug]["dir"]
    base = DATA_DIR / "ithrottir" / sport["dir"] / "iceland"
    d = base / f"{sex_slug}-{suffix}"
    # Transition shim: bb/hb publish to {sex}/ today and move to {sex}-{suffix}/
    # when the sports-side division loop lands. Delete once the new dirs exist.
    if not d.exists() and division_slug == sport["default_division"]:
        legacy = base / sex_slug
        if legacy.exists():
            return legacy
    return d
```

`CREST_SLUG` stays one shared dict — exact-string keying is what makes it safe across sports. Every handball karla club (Afturelding, FH, Fram, HK, Haukar, KA, Selfoss, Stjarnan, Valur, ÍBV, ÍR, Þór) is already in it and is the same multi-sport club. Missing: `Álftanes`, `Ármann` (add if artwork exists). **Do not add a fuzzy or prefix match** — basketball's `"Þór Þ."` is Þór Þorlákshöfn and must not resolve to `thor` (Þór Akureyri). It currently falls back to the text short-code, which is correct. Combined squads (`Hamar/Þór`, `KA/Þór`) stay absent, matching the existing Grindavík/Njarðvík precedent.

### Template strategy

**One generalised page template, three small per-sport prose partials.** Not per-sport page templates.

Rename `/Users/brynjolfurjonsson/metill-platform/app/templates/ithrottir_fotbolti.html` → `ithrottir_league.html` (nothing outside `ithrottir.py` references the filename). Rationale: the page IA is identical across all three sports — Leikir → Staðan → Spá → Aðferð, driven by `act_band.html` + `jump_rail.html` — and all five chart sections work on bb/hb data as-is. The only structural variation in the 263-line file is the existing `{% if is_cup %}` branch, which stays football-only. Three copies would triple the maintenance of the ~90-line `{% block scripts %}` module-import/render-orchestration block and the `?v=N` cache-buster list, and would drift within a month.

What DOES vary is the ~60-line Aðferð prose block (lines 100-160): football explains bivariate Poisson + xSkoruð/xFengin/xStig + Δ/Form; the 2DT sports need Student-t framing, no xG paragraph at all, and the D3 playoff disclaimer. Extract it to `components/ithrottir/method_{fotbolti,handbolti,korfubolti}.html` and include dynamically:

```jinja
{% include "components/ithrottir/method_" ~ sport_slug ~ ".html" %}
```

Net: 1 page template + 3 ~20-line partials, versus 3 × 263-line templates.

**How the four sport-varying behaviours are handled.**

*1 — Icelandic terminology (stig vs mörk).* Registry-driven, threaded through the route context as `sport` (the whole registry entry, minus `divisions`). Three consumption points:

- `standings-table.js` header (lines 322-345): the group head `Mörk` and the sub-heads `Skoruð`/`Fengin` become `opts.scoreGroup` / `opts.scoreSub`. Basketball passes `"Stigaskor"` because its league-points group is *also* `Stig` — two identical group heads in one thead is unreadable. Handball passes `"Mörk"` unchanged.
- `fixture_card.html` legend (line 18-20): `<span>xG = vænt mörk</span>` becomes `{{ sport.score_label }} = vænt {{ sport.score_noun }}` → "Vænt stig = vænt stig" for basketball, which is redundant; better, drop the third legend span entirely for `has_xg == False` sports and let the card footer's own label carry it.
- `next-games-grid.js::renderCard` line 254-256: the hardcoded `xG ${goals(m.mean_home_goals)} – ${goals(m.mean_away_goals)}` becomes `${opts.scoreLabel} ${goals(...)} – ${goals(...)}`, and `goals()` needs a per-sport precision (football 1 decimal, basketball 0 — "Vænt stig 88,3 – 81,7" should read "88 – 82").

*2 — No draws (basketball).* Two removals plus one semantic correction, all opt-in flags so football's code path is untouched:

- `standings-table.js`: `opts.showDraws` (default `true`). When false, drop `<th scope="col" rowspan="2">J</th>` (line 331) and `<td>${r.draws}</td>` (line 359). The data ships `draws: 0`, so rendering it would be *wrong*, not merely empty — D-requirement is that the column vanishes.
- `next-games-grid.js::renderCard`: add a third mode alongside the existing `isKnockout` two-way path. **Do not reuse `advance_home`** — it is semantically "advances via ET/penalties" and its probBar uses `1 - advance_home`. Add `opts.noDraw`, giving `probs = [{key:"H", p:m.p_home_win, align:"l"}, {key:"Ú", p:m.p_away_win, align:"r"}]` and a two-segment `probBar`. Note `p_home_win + p_away_win == 1` for basketball because the publisher rolls the continuous-draw mass into `p_tie` which is ~0.
- The goal-diff strip (`renderGoalDiffStrip`, lines 150-186). `.bin_goal_diff_distribution_2dt()` uses a **5-point bucket** for basketball and a **2-point bucket** for handball, so the bucket centred at 0 covers ±2 points (basketball) or ±1 goal (handball) — it is *not* the draw probability. Add `opts.highlightZero` (default `true`) and pass `false` for **both** new sports: the jokull highlight and the "Jökulbláa súlan stendur ávallt fyrir jafntefli" copy must go. Handball's honest draw probability is the `J` column, not the strip. Also change `barW` to derive from the minimum gap between consecutive `diff` values rather than `100/(span+1)` — with 5-point buckets over ±40 the current formula floors at 1.5 and renders thin needles with wide gaps.

*3 — Hidden xG columns.* **Already solved, no change needed.** `standings-table.js:275` computes `showExpectedCols = rows.some(r => r.xpts != null)`, and `morkSpan`/`stigSpan` (lines 311-312) collapse the spanner row accordingly. Verified against the on-disk basketball standings, where every row has `xg_for: null, xg_against: null, xpts: null`. Likewise the Frávik sparkline: `buildResidualSeries` (line 51) takes `Math.min(gf.length, ga.length, xgf.length, xga.length)` and bb/hb ship `xg_trend: []` with no `goals_trend`, so it returns `[]` and the sparkline renders empty. The only thing to add is hiding the **Frávik column header** when no row produces a series — currently it would render an always-empty column. One extra `showFravik` flag, computed from the data not the registry (so it lights up automatically if the sports side ever backfills 2DT xG).

*4 — D3 regular-season relabel.* One boolean, `sport.title_decides_champion`, gating five strings — enumerated in `icelandic_copy`. Server-side: the `stateline` (ithrottir.py:453-463) and the `Efst spáð` fact's `sub` (line 424). Template-side: the heatmap panel title in `finishing_heatmap.html:11`, the jump-rail/act-band label `Deildarspá`, and the new `playoff_note` disclaimer rendered under the heatmap panel and repeated in the Aðferð partial.

**Sport tabs.** `sport_tabs` (ithrottir.py:473-477) becomes registry-derived, replacing the two `disabled: True` stubs. Sex is preserved across a sport switch (a reader on kvenna football lands on kvenna handball) — safe because D4 puts a top division in every (sport, sex) cell:

```python
sport_tabs = [
    {"label": s["label"],
     "href": f"/ithrottir/{slug}/iceland/{sex_slug}/{s['default_division']}/",
     "active": slug == sport_slug,
     "disabled": not _sport_available(slug, sex_slug)}
    for slug, s in SPORTS.items()
]
```

where `_sport_available` returns True iff that cell's `meta.json` parses. `league_tabs.html` already renders a disabled chip as `{{ label }} · Síðar` with `aria-disabled` and `tabindex="-1"`, so an unpublished sport degrades into exactly the treatment that is on the page today — and lights up on the next data pull with **no deploy**. That is the right shape for the staggered season starts (handball this week, basketball ~1 Oct).

`division_tabs` and `sex_tabs` (lines 481-508) need `sport_slug` interpolated into their hrefs; `_sex_href`'s "fall back to the top division when the other sex doesn't publish this division" logic generalises unchanged.

### Icelandic copy

- Sport tab labels (sport_tabs): "Knattspyrna" (unchanged) · "Handbolti" · "Körfubolti"
- Handball division titles: "Olísdeild" (slug olis) · "Grill 66-deild" (slug grill66). Page H1 composes as "{title} {sex}" → "Olísdeild karla", "Grill 66-deild kvenna" — both grammatical with the genitive sex word, same as football.
- Basketball division titles: "Bónusdeild" (slug bonus) · "1. deild" (slug 1deild) → "Bónusdeild karla", "1. deild kvenna".
- Standings score-column group head — handball: "Mörk" (unchanged from football). Basketball: "Stigaskor", because the league-points group in the same thead is also "Stig" and two identical group heads are unreadable. Sub-heads stay "Skoruð" / "Fengin" for both.
- Standings points column: "Stig" for all three sports (unchanged).
- D3 stateline, with standings (replaces ithrottir.py:454-457 "…mestar líkur á Íslandsmeistaratitlinum"): "Eftir {n} af {N} umferðum á {lið} mestar líkur á að vinna deildarkeppnina." — keeps football's single-V2-verb construction (á … mestar líkur á), which was grammar-checked with Miðeind.
- D3 stateline, no standings yet (replaces line 460): "{lið} á mestar líkur á að vinna deildarkeppnina."
- D3 fact sub under "Efst spáð" (replaces the bare "{p}% líkur" at line 424): "{p}% líkur á efsta sæti í deildarkeppni"
- D3 heatmap panel title (replaces "Lokastaða (spáð)" in finishing_heatmap.html:11): "Lokastaða deildarkeppni (spáð)"
- D3 disclaimer (sport.playoff_note — rendered under the heatmap panel and repeated in the Aðferð partial): "Spáin nær aðeins til deildarkeppninnar. Íslandsmeistari ræðst í úrslitakeppni sem líkanið spáir ekki fyrir um."
- Fixture-card legend, basketball (replaces "Súlur: líkur á heima · jafntefli · gestum"): "Súlur: líkur á heimasigri · útisigri" — jafntefli removed, not zeroed.
- Fixture-card legend, basketball margin strip (replaces "Markamismunur: jafntefli í miðju · lína = vænt gildi"): "Stigamunur: 0 í miðju · lína = vænt gildi" — the centre bucket spans ±2 stig, so it must NOT be labelled jafntefli.
- Fixture-card legend, handball margin strip: "Markamunur: 0 í miðju · lína = vænt gildi" — the centre bucket spans ±1 mark, likewise not jafntefli.
- Fixture-card score footer label (replaces the hardcoded "xG" at next-games-grid.js:255) — handball: "Vænt mörk"; basketball: "Vænt stig" (rendered to 0 decimals, e.g. "Vænt stig 88 – 82").
- Fixture-card legend third span "xG = vænt mörk" — deleted entirely for both new sports (no xG published).
- Standings prose (standings_table.html:10, currently "…spáðum markamismun liða undanfarna fimm leiki") — basketball: "…spáðum stigamun liða undanfarna fimm leiki"; handball unchanged.
- Off-season sport-tab chip: the existing "{label} · Síðar" treatment in league_tabs.html is reused verbatim for a sport whose meta.json is absent.
- Off-season in-page empty state (new, for a published-but-not-yet-started cell): "Tímabilið er ekki hafið — spár birtast þegar deildin byrjar." Mirrors the existing standings_meta string "Birtist þegar deildin byrjar" (ithrottir.py:417).

---

## Residual risks

- The 2DT fit has never run green inside fit.yml's CI budget. Publish for these sports has never succeeded on CI, which means the fit -> extract -> commit path has never been exercised there either, and nobody has timed a 2DT fit on a runner against the 240-minute cap. `fit_skip_reason()`'s own docstring records real off-season basketball R-hat/ESS breaches as a reason paused leagues are skipped even under --force, and `fit_model()` aborts on a diagnostics-gate breach. Every downstream claim in this spec is conditional on an event nobody has measured, and its remediation (model or gate tuning) is unbounded. Time this first, on a runner, before trusting any of it.
- The six HSÍ 2027 tournament ids were read off site navigation and none has been opened. `.assert_season_stamp` is the tripwire, but it fires after a chromote round-trip, and a title-pattern mismatch in `hsi_discover_tournaments` would silently discover nothing rather than discover wrong.
- The female G66 2025 id 7643 is inferred from adjacency between two verified ids. The three-part verification (title, season stamp, squad intersection) makes a wrong id unlikely to be committed, but it cannot prove the id is the right one rather than a different competition whose fixtures happen to fall in the same window. If verification fails the hole stays open and the kvenna-g66 cell's history stays thin.
- `qualify_slots` and `relegation_slots` are the only numbers in this spec taken from a plausible summary rather than from source. A test can assert `0 <= qualify_slots < n_teams`, which catches transcription errors and not a wrong-but-plausible value. A human must read the 2026-27 KKÍ and HSÍ competition regulations once; the spec does not have a mechanism that substitutes for that.
- Generalising `.compute_team_strength_trajectory_pfi` to 2DT is asserted from the Stan source, not demonstrated against a fitted object. The 2DT round index is a per-team appearance index, so a division whose teams entered at different global rounds needs the same mapping football performs — that mapping is what makes it non-trivial, and it may not transfer cleanly.
- `data/beliefs/extracts/` becoming the sole publish input makes it the new single point of failure. A crashed extract now costs a publish, where before it cost only a sidecar. The complete-partition rule inherited from `read_extracted_football` is what protects against a half-written partition; if `required_extracts` is set too loosely for 2DT, a partial cell publishes.
- The extracts tree grows. Football's partitions are already 919 files and this adds two sports x two sexes x a per-fit partition, each now carrying a per-round trajectory parquet. Nothing in this spec prunes old `fit_date=` partitions, and the tree is git-tracked and committed by CI daily. Repository growth is a real cost that will need a retention policy, and it is out of scope here.
- The two-repo contract has no contract test. The schemas are generated from one source and rsynced from one clone at one SHA, which removes the skew window, but nothing asserts that the platform's renderer actually reads the fields the schema requires. The `xG` label hardcoded at `next-games-grid.js:255` is the current example of that gap; the next one will be found the same way.
- The alert channel for every new health check is a GitHub workflow-failure email. That is signal, not a pager. A FAIL on a Saturday is seen on Monday, and describing this as monitoring would overstate it.
- The launchd autoplace agent still runs `git` (stash -> pull --rebase -> pop) on ~/sports on its own schedule, and `sync_recs()` rescue-commits only the ledger. This spec generates a lot of tracked data (extracts partitions, publish JSON, `config/federation-seasons.json`) during interactive work, and the 2026-06-11 backfill-clobber incident is the precedent. Commit generated tracked data promptly.
- Removing the ingest gate means off-season federation fetches warn daily rather than being skipped, adding recurring noise to the scrape-results log. The three-strikes-plus-24h control bounds the cost but not the noise, and a genuinely-over season looks identical to a broken scraper in the log until `check_season_resolution` distinguishes them.
