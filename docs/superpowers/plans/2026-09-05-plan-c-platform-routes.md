# Plan C — the metill-platform sport dimension (the consumer side)

Implements section 14 (the sport dimension) and the consumer half of section 15
(Icelandic copy / the D3 relabel) of
[the design](../specs/2026-09-02-basketball-handball-metill-parity-design.md).

**Target repo is `/Users/brynjolfurjonsson/metill-platform`, not this one.** The
plan lives here so all three plans sit together; every path below is
metill-platform-relative unless it starts with `~/sports/`.

Plan A (WS1–WS6) and Plan B (WS7–WS12) are the producer side. This plan assumes
Plan B has landed on `feat/bb-hb-metill-parity` and that its output is what the
"Verified contract" section below records — because that section was produced by
*running* the Plan B publisher, not by reading its plan.

## How to use this document

1. Read **Verified contract**. It is the observed shape of the JSON, and it
   contradicts design §14 in seven places. Where they disagree, this section wins.
2. Read **Integration decisions**. They override the task text below them.
3. Read **Execution order**. Four of its edges break football's nine live,
   indexed URLs or publish a materially wrong number if violated.
4. Then work the tasks in order.

## Global constraints

- Strict TDD. Write the failing test, RUN it, confirm the failure is the one
  predicted. **If a predicted RED comes back green, STOP** — something already
  made the change and continuing leaves a test that can never fail.
- **Football is live, mid-season, and indexed.** Nine URLs
  (`/ithrottir/fotbolti/iceland/{karla,kvenna}/{besta,lengja,2deild,3deild,bikar}/`
  minus `kvenna/3deild`), two 301 redirects, nine short links and seven OG card
  ids are a public contract. Task 1 pins them before anything is refactored.
- **Never `git push`.** Every push to `main` triggers a Fly deploy
  (`.github/workflows/deploy.yml`). Work on a branch; commits only.
- Tests: `uv run --extra dev pytest tests/` and `uv run --extra dev ruff check .`
  from the repo root. `asyncio_mode = "auto"`; route tests use the
  `httpx.AsyncClient` + `ASGITransport` fixture already at
  `tests/test_ithrottir_routes.py:10-15`.
- Non-ASCII/Icelandic strings: write them with a python heredoc or
  `cat <<'EOF'`, never with the Write/Edit tools.
- The HTML formatter hook rewrites templates on save and **breaks Jinja `{% if %}`
  inside HTML attributes** (metill-platform/CLAUDE.md, "Conventions"). Use Alpine
  `:class` / a precomputed context value instead.
- `{% block %}` must live in `base.html`, never inside an `{% include %}`d
  partial (same section). The per-sport method partials are includes, so they
  carry no blocks.

---

## Verified contract

Everything here was observed on 2026-09-04 by publishing all eight bb/hb cells
from the committed fixtures with the Plan B publisher:

```
cd ~/sports && Rscript -e '
  invisible(Sys.setlocale("LC_ALL","en_US.UTF-8")); devtools::load_all(".", quiet=TRUE)
  suppressMessages(library(arrow))
  root <- "<scratch>/planc"
  write_table(read_parquet("tests/testthat/fixtures/facts/results.parquet"), "results", root=root)
  write_table(read_parquet("tests/testthat/fixtures/facts/schedules.parquet"), "schedules", root=root)
  dir.create(file.path(root,"beliefs")); file.copy("tests/testthat/fixtures/extracts", file.path(root,"beliefs"), recursive=TRUE)
  lg <- load_leagues()
  for (k in c("basketball_iceland","handball_iceland")) {
    st <- lg[[k]][c("sport","country","sexes","active","stan_model","data_source","publish_divisions")]
    for (sx in c("male","female")) publish_one(st, lg[[k]]$betting, k, sx, root=root, validate=FALSE, end_date=as.Date("2100-01-15"))
  }'
```

It wrote **10 JSONs into each of 8 directories**:
`publish/{basketball,handball}/iceland/{karla,kvenna}-{bd,1d|od,g66}/`. Football's
live tree on disk has 10 per league cell and **12** in each of the two `bikar`
cells (`~/sports/data/publish/football/iceland/karla-bikar/`).

### What the platform can rely on

| claim | observed |
|---|---|
| Directory shape | `{sex}-{slug}` for all three sports. `~/sports/data/publish/basketball` and `.../handball` **do not exist at all** on the branch right now — commit `0c14d03df` ("chore(publish): drop the un-suffixed basketball/handball cells") removed them. |
| `next_games` field names | Football's, for all sports: `mean_home_goals`, `mean_away_goals`, `mean_goal_diff`, `p_home_win`, `p_draw`, `p_away_win`, `goal_diff_distribution`, `division`, `division_code`, `venue`, `date`, `home`, `away`. The old `mean_home` / `p_home` / `p_tie` names are gone. |
| Basketball draws | `p_draw` is present and **exactly `0`** (not absent). Rendering it is wrong; the column must vanish. |
| `meta.json` v2 (bb/hb) | `n_rounds`, `n_rounds_source` ∈ {config, schedule, none, not_applicable}, `units{strength, home_advantage, diff_bin_width}`, `points{win, draw, loss}`, `season_scope`, `postseason{name_is, modelled}`, `qualify`, `relegation{slots}` — all present, all **required** by `~/sports/config/publish-schemas/_delta/{basketball,handball}/meta.json`. |
| `meta.units.diff_bin_width` | `5` for basketball, `2` for handball, `1` for football. |
| `meta.points` | basketball `{win: 2, draw: null, loss: 0}`; handball `{win: 2, draw: 1, loss: 0}`; football `{win: 3, draw: 1, loss: 0}`. |
| `meta.season_scope` / `meta.postseason` | bb/hb `"regular_season"` + `{name_is: "Úrslitakeppni", modelled: false}`; football `"full_season"` + `null` (`~/sports/R/publish-profile.R:168-171, 210-212`). |
| `final_positions.basis` | bb/hb `"regular_season_table"`; football `"final_table"`. |
| `final_positions.summary` keys | bb/hb: `team`, `p_top_of_table`, `p_relegation` — **no `p_winner`, no `p_top_six`, no `p_qualify`**. Football: `team`, `p_qualify` (BD only), `p_top_of_table`, `p_winner`, `p_top_six` (deprecated alias), `p_relegation`. |
| `points_distribution.summary` | same split: bb/hb carry `p_top_of_table` + `p_relegation`; football keeps `p_top_six` + `p_winner`. |
| `standings.rows` | identical key set across sports. bb/hb ship `xg_for: null`, `xg_against: null`, `xpts: null`, `xg_trend: []`, `xg_against_trend: []`, and **non-empty** `goals_trend` / `goals_against_trend`. |
| `team_strengths.records` | bb/hb have no `preseason` key; football does. Components `offence`/`defence`/`total`, locations `home`/`away`/`avg` for both. |
| `home_advantage` | handball medians land in **0.69–1.89 goals** on the fixture — raw units, i.e. the B5 `exp()` bug is fixed. |
| Schemas | `~/sports/config/publish-schemas/{basketball,handball}/` each hold 10 compiled `*.schema.json`. |

### Where design §14 / §15 is now STALE

**S1 — the registry's `code` values are wrong and would silently empty the
fixtures grid.** `_build_league_context` filters upcoming matches on
`m["division_code"] == div["code"]` (`app/routes/ithrottir.py:294-297`). The
publisher writes the **badge**, not the division code:
`next_games[].division_code` is `"BON"` for basketball BD, `"B1D"` for 1D,
`"OD"` and `"G66"` for handball — sourced from `publish_divisions[*].code_badge`
in `~/sports/config/leagues.yml:83-96, 166-178`. Design §14's registry sets
`"code": "BD"` and `"code": "1D"` for basketball. With those values every
basketball fixture is filtered out and the page renders "Engir leikir framundan"
with no error anywhere. `division` (the code) and `division_code` (the badge)
are *both* present in the payload and they differ.

**S2 — `meetings: 4` for basketball Bónusdeild is wrong.** `~/sports/config/leagues.yml:83-96`
configures `expected_meetings: 2` for both basketball divisions and both sexes,
and `3` for handball's two women's divisions. Moot in practice: **read
`meta.n_rounds`**, which the producer now publishes precisely so no consumer has
to guess (ID-C6).

**S3 — `p_qualify` does not exist for basketball or handball.** Design §14
correction 3 and §15's "Qualification fact label, from `meta.qualify.label_is`:
**Líkur á úrslitakeppni**" are unreachable. `meta.qualify` is `null` for all
eight bb/hb cells and `.build_placement_summary()` emits `p_qualify` only when a
cut is configured (`~/sports/R/publish-format.R:389, 403`). The producer refuses
it on purpose — the four basketball cells qualify 8/12, 8/12, 10/10 and 4/11
teams, so no per-division integer expresses it. Where football BD *does*
configure one, `label_is` is **"Efri hluti"**, not "Í toppbaráttu". The
"Í toppbaráttu" fact is therefore **dropped** for bb/hb, not relabelled (ID-C4).

**S4 — no `_normalise_next_games()` shim is needed.** Design §14 correction 1
already says this; recording it here because the *files-to-change* table
(design line ~514) still instructs adding one.

**S5 — the `_league_data_dir` legacy-directory transition shim is dead on
arrival.** Design §14 correction 2 keeps it "until the new dirs are confirmed".
The un-suffixed `{karla,kvenna}/` dirs are already gone upstream, so the next
`rsync -a --delete` removes `data/ithrottir/{basketball,handball}/iceland/{karla,kvenna}/`
from this repo. There is no window in which the shim resolves anything. Do not
write it.

**S6 — `data/ithrottir-schemas/{basketball,handball}/` must NOT be hand-authored.**
`.github/workflows/pull-sports-data.yml:100-113` sparse-checks out
`config/publish-schemas` from the upstream clone and rsyncs it over
`data/ithrottir-schemas/` with `--delete` *before* running the validator. The
bb/hb schemas arrive automatically and gate the pull fail-closed on the first
run after Plan B merges. Note the wrinkle: the commit step only stages
`data/ithrottir/` (line 138), so the schema refresh is never committed — the
in-repo copy stays football-only while validation always uses the fresh one.
Cosmetic, but it means "the repo has no bb/hb schemas" is not evidence they are
ungated.

**S7 — the OG cache-key collision is latent, not live.** `og.py:676` builds
`_cache_key('ithrottir', sex, division, fingerprint)` from the **URL division
slug**, and the three sports' slugs are disjoint (`besta|lengja|2deild|3deild|bikar`
vs `bonus|1deild` vs `olis|grill66`), so nothing collides today. The *real* bug
in that file is `_card_png`'s `page_id.split("-", 1)` (`og.py:954`): a 3-part id
like `korfubolti-karla-bonus` yields `sex="korfubolti"`, misses `ITH_SEX_GEN`
and silently falls back to the static card. Fix both — adding the sport to the
key is one line of cheap defence — but describe the collision honestly.

### Two more things the design did not record

**S8 — `renderCard` and `renderGoalDiffStrip` take no `opts` at all.**
`app/static/js/next-games-grid.js:196` is `function renderCard(m, teamRanks)` and
`:150` is `function renderGoalDiffStrip(dist)`; `renderFixturesGrid`'s `opts`
(`:263`) is never threaded down (`:295`). Every new per-sport option requires a
signature change on two functions shared with the HM 2026 page
(`app/templates/ithrottir_hm2026.html:2102, 2154`) and the cup page.

**S9 — `p_relegation` is emitted for bb/hb but `meta.relegation.slots` is `null`,**
so it falls back to the legacy `placement >= n_teams - 1` expression
(`~/sports/R/publish-format.R:383-387`). For basketball's bottom tier (1. deild)
"Fallhætta" is then a bottom-two probability with no relegation behind it. See
residual risk R5.

---

## Integration decisions (override the task text below)

**ID-C1 — the D3 relabel reads the PAYLOAD, not a registry boolean.**
Design §14 proposes `sport.title_decides_champion`. Use
`meta.season_scope` and `final_positions.basis` instead, exactly as the design's
own §15 says ("encoded in the payload, not only in the template"). Concretely,
one context value computed in `_build_league_context`:

```python
# Football's LIVE tree predates meta v2 (no season_scope key), so absence
# means full season. The bb/hb schemas REQUIRE the key, so it is never
# absent where it matters.
regular_season_only = (meta or {}).get("season_scope") == "regular_season"
playoff_note = ((meta or {}).get("postseason") or {}).get("name_is") if regular_season_only else None
```

The registry carries **no** `title_decides_champion` and **no** hardcoded
`playoff_note` string — a new sport gets the right copy from its payload with no
platform deploy. `sport.has_draws` and `sport.score_group` stay in the registry:
those are presentation vocabulary, not model claims.

**ID-C2 — the registry's `code` is the BADGE.** Per S1: `bonus → "BON"`,
`1deild → "B1D"`, `olis → "OD"`, `grill66 → "G66"`; football's five are
unchanged (`BD`, `LD`, `D2`, `D3`, `MB`). Task 4 asserts the registry badge
equals the `division_code` in the fixture payload for every cell — that
assertion is the whole defence against S1.

**ID-C3 — no `_normalise_next_games()`, no legacy-dir shim.** Per S4 and S5.

**ID-C4 — the fourth fact tile for a cell with no `qualify`.** Football keeps
"Í toppbaráttu" (from `p_top_six`, or `p_qualify` when present — see the
deprecation note in `~/sports/R/publish-format.R:370-372`). Where
`meta.qualify` is `null` **and** no summary row carries `p_qualify`/`p_top_six`,
the tile becomes **"Lið í deild"** with `final_positions.n_teams` as the value
and no sub-line. This is a fact the payload actually supports; every
playoff-shaped alternative is a claim the model does not make. Flagged for the
user's copy review — it is the one string in this plan invented rather than
inherited.

**ID-C5 — `total_rounds` comes from `meta.n_rounds`, clamped.** Never
`2*(n_teams-1)`. When `n_rounds` is absent (football's live pre-v2 tree, until
its first post-merge republish) fall back to `2*(n_teams-1)` exactly as today so
football's rendered output is byte-identical. `max(0, n_rounds - matches_played)`
so "Umferðir eftir" can never go negative.

**ID-C6 — one page template, three prose partials.** Adopted from §14 unchanged.
`ithrottir_fotbolti.html` → `ithrottir_league.html`; the Aðferð block (lines
~95-160, including its `{% if is_cup %}` branch) moves verbatim into
`components/ithrottir/method_fotbolti.html`; two new ~20-line partials for
handbolti and korfubolti; included as
`{% include "components/ithrottir/method_" ~ sport_slug ~ ".html" %}`.

**ID-C7 — one methodology page for now.** `/ithrottir/adferdafraedi`
(`app/routes/ithrottir.py:182-188`) is football prose. The per-sport Aðferð
partial carries the sport-specific paragraphs and links to it as *almenn
aðferðafræði*. A per-sport methodology page is out of scope; recorded as
residual risk R6.

**ID-C8 — nothing links to a bb/hb URL until a real cell exists on disk.**
`_sport_available(sport_slug, sex_slug)` (meta.json parses) gates the sport tab,
the nav dropdown item and the sitemap row. Tasks 12–14 are written so that a
missing directory is the *normal* state, not an error path.

**ID-C9 — the eight new schema files are not this repo's problem.** Per S6, do
not create `data/ithrottir-schemas/{basketball,handball}/`. Task 16 verifies the
validator reports zero `unmatched` after a real pull instead.

---

## Execution order

EXECUTE IN THIS ORDER. Edges marked BREAKS/WRONG-NUMBER are not advisory.

```
PHASE 0 — the net
  1.  Football regression lock.          BREAKS FOOTBALL IF SKIPPED: tasks 2-3
      rewrite every route decorator and the whole context builder; without a
      pinned baseline a broken indexed URL is invisible until Search Console.

PHASE 1 — the registry and the generalised handler (football-only behaviour)
  2.  SPORTS registry, football entry only; thread sport_slug through
      _league_data_dir / _division_available_for_sex / _build_league_context.
  3.  Generalise the three route decorators to {sport_slug}. BREAKS FOOTBALL IF
      DONE BEFORE 2: the handler would read a registry that does not exist.

PHASE 2 — payload safety, BEFORE any new cell is reachable
  5.  Summary-key safety (p_winner / p_top_six KeyError).
  6.  total_rounds from meta.n_rounds.
  7.  D3 copy from meta.season_scope / final_positions.basis.
      BREAKS FOOTBALL IF 5-7 ARE DEFERRED PAST 4 AND MERGED: the first bb/hb
      cell to land on disk 500s the page (KeyError 'p_winner' at
      app/routes/ithrottir.py:391) and, worse, a cell that does NOT 500 renders
      "mestar líkur á Íslandsmeistaratitlinum" for a title the model never
      simulated.

PHASE 3 — register the new sports and their cells
  4.  basketball + handball entries in SPORTS + the fixture-backed 200 sweep.
      Depends on 3. Sequenced after 5-7 in the commit graph even though it is
      numbered 4 — see the task header.

PHASE 4 — presentation
  8.  Template rename + method partials + sport_slug in dataBase + og_image id.
  9.  standings-table.js: showDraws / scoreGroup / scoreSub / showFravik.
  10. next-games-grid.js: thread opts; noDraw, highlightZero, scoreLabel,
      scorePrecision, barW from the minimum inter-bucket gap.
      WRONG NUMBER IF 10 IS SKIPPED: the goal-diff strip's centre bucket spans
      +/-2 stig (basketball, diff_bin_width 5) or +/-1 mark (handball, 2). It
      is NOT the draw probability. Shipping football's jokull highlight plus
      "jafntefli i midju" publishes a materially wrong figure.
  11. Partial copy: fixture_card, standings_table, finishing_heatmap.

PHASE 5 — discovery
  12. og.py. Must land in the SAME commit as, or before, task 8's og_image
      block change. BREAKS FOOTBALL OG IF _card_png's 2-part branch is not
      preserved: every crawled /og/ithrottir/{sex}-{div}.png URL is live.
  13. pages.py sitemap loop + 8 short links. Depends on 4 (a sitemap row for an
      unrouted URL is a 404 in the sitemap) and MUST guard a missing meta.json
      with try/except — the bb/hb directories do not exist yet at all.
  14. base.html nav dropdown, with per-item aria-current.

PHASE 6 — close out
  15. Rewrite .claude/rules/ithrottir.md (its "Platform scope" section is what
      the next session will believe).
  16. Whole-branch verification.
```

---

## WS-P1 — The registry and the generalised handler

### Task 1: Pin football's live surface before touching anything

**Files:**

- create `tests/test_ithrottir_football_regression.py`

**Interfaces:** none — pure assertion over the running app.

- [ ] Write the file. Parametrise over the nine live league URLs
      (`/ithrottir/fotbolti/iceland/{karla,kvenna}/{besta,lengja,2deild,3deild,bikar}/`
      minus `kvenna/3deild`) asserting `200`. Assert `/ithrottir/fotbolti/iceland/karla/`
      and `/ithrottir/` both `301`. Assert the nine short links in
      `app/routes/pages.py:182-192` each `301` to their exact current target.
      Assert `/ithrottir/fotbolti/iceland/kvenna/3deild/` is `404`. For
      `karla/besta` assert the four football fact labels still render
      (`Efst spáð`, `Í toppbaráttu`, `Fallhætta`, `Umferðir eftir`) and that the
      substring `Íslandsmeistaratitlinum` is present — football's stateline must
      survive the D3 work in task 7 untouched. Assert
      `/og/ithrottir/karla-besta.png` returns 200 with `content-type: image/png`.

- [ ] RUN IT. **EXPECTED: GREEN on arrival.** This is a contract lock, not a TDD
      cycle — say so in the module docstring. If any assertion is red *now*,
      stop and fix the assertion to match live behaviour; do not "fix" the app.

- [ ] Commit: `test(ithrottir): pin football's nine live URLs before the sport refactor`.

**Verification.** Every later task re-runs this file. A red line in it is a
broken indexed URL, and there is no other detector.

---

### Task 2: `SPORTS` registry — football only, no behaviour change

**Files:**

- modify `app/routes/ithrottir.py`
- modify `tests/test_ithrottir_routes.py`

**Interfaces:**

```python
SPORTS: dict[str, dict]   # key = URL slug; football is an ordinary entry
SEXES: set[str]           # {"karla", "kvenna"} — replaces LEAGUES
def _league_data_dir(sport_slug: str, sex_slug: str, division_slug: str) -> Path
def _division_available_for_sex(sport_slug: str, sex_slug: str, division_slug: str) -> bool
def _build_league_context(sport_slug: str, sex_slug: str, division_slug: str) -> dict
```

- [ ] Add to `tests/test_ithrottir_routes.py`: `from app.routes.ithrottir import SPORTS, SEXES`;
      assert `set(SPORTS) == {"fotbolti"}`; assert `SPORTS["fotbolti"]["dir"] == "football"`;
      assert `set(SPORTS["fotbolti"]["divisions"]) == {"besta","lengja","2deild","3deild","bikar"}`;
      assert `SPORTS["fotbolti"]["divisions"]["bikar"]["code"] == "MB"` and
      `["is_cup"] is True`; assert
      `_league_data_dir("fotbolti","karla","besta").name == "karla-bd"` and that
      its parent chain contains `football` and `iceland`.

- [ ] RUN IT. **EXPECTED RED:**
      `ImportError: cannot import name 'SPORTS' from 'app.routes.ithrottir'`.

- [ ] Implement. Replace `LEAGUES` (`:43-52`) and `DIVISIONS` (`:65-71`) with
      `SPORTS` + `SEXES`. Carry over each football division's `dir`, `title`,
      `code`, `is_cup`, `sexes` verbatim. Add per-sport keys used later:
      `dir`, `label`, `score_group`, `score_sub`, `score_label`, `score_noun`,
      `has_draws`, `has_xg`, `score_precision`, `default_division`. **Do not add
      `title_decides_champion` or `playoff_note`** (ID-C1). `SEXES` replaces
      `LEAGUES` losslessly: `league["title"]` was already overwritten at `:283`
      with `f"{div['title']} {sex_slug}"`.
      Thread `sport_slug` through `_league_data_dir` (`:110-117`),
      `_division_available_for_sex` (`:541-552`) and `_build_league_context`
      (`:276`); the three route handlers pass the literal `"fotbolti"` for now.
      Update the two existing tests that import `DIVISIONS`
      (`test_lower_divisions_in_divisions_dict`, `test_bikar_in_divisions`) and
      the two that call `_league_data_dir` positionally
      (`test_league_data_dir_resolves_per_division`, `test_lower_divisions_data_dir_resolves`,
      `test_bikar_data_dir_resolves`).

- [ ] RUN AGAIN green, plus `tests/test_ithrottir_football_regression.py` green.
      Commit: `refactor(ithrottir): SPORTS registry replaces the football-pinned LEAGUES/DIVISIONS`.

**Verification.** Football renders byte-identically (task 1 green) while every
football-specific name has left the module's data model.

---

### Task 3: Generalise the three route decorators

**Files:**

- modify `app/routes/ithrottir.py`
- modify `tests/test_ithrottir_routes.py`

**Interfaces:** `def _sport_available(sport_slug: str, sex_slug: str) -> bool`

- [ ] Add a test asserting the generalised path templates are registered:

```python
def test_generalised_sport_route_registered():
    from app.main import app
    paths = {getattr(r, "path", None) for r in app.routes}
    assert "/ithrottir/{sport_slug}/iceland/{sex_slug}/{division_slug}/" in paths
    assert "/ithrottir/fotbolti/iceland/{sex_slug}/{division_slug}/" not in paths
```

      plus `async def test_unknown_sport_404`: `/ithrottir/sund/iceland/karla/besta/` → 404,
      and `/ithrottir/korfubolti/iceland/karla/bonus/` → 404 (not yet registered).

- [ ] RUN IT. **EXPECTED RED:** `AssertionError: assert '/ithrottir/{sport_slug}/iceland/{sex_slug}/{division_slug}/' in {...}`
      — the first assertion. The `sund` and `korfubolti` cases are already green
      (they 404 today); say so in the test docstring so a future reader does not
      mistake them for the cycle's RED.

- [ ] Implement. Rewrite the three decorators at `:555`, `:567`, `:582` to
      `{sport_slug}`. Guard tuple becomes
      `sport_slug not in SPORTS or sex_slug not in SEXES or division_slug not in SPORTS[sport_slug]["divisions"] or not _division_available_for_sex(...)`.
      The `/ithrottir/{sport_slug}/iceland/{sex_slug}/` redirect targets
      `SPORTS[sport_slug]["default_division"]`. Add `_sport_available(sport, sex)`
      returning `_load_json(_league_data_dir(sport, sex, default_division) / "meta.json") is not None`.
      Make `sport_tabs` (`:473-477`) registry-derived, preserving `sex_slug`
      across a sport switch, `disabled=not _sport_available(...)`. Interpolate
      `sport_slug` into `_sex_href` and the `division_tabs` hrefs (`:481-508`).
      Leave `/adferdafraedi` (`:182`) and `/` (`:533`) alone — 2- and 1-segment
      paths cannot collide with a 5-segment template.

- [ ] RUN AGAIN green + task 1 green. Commit:
      `feat(ithrottir): serve every sport from one {sport_slug} handler`.

**Verification.** Football's URLs are produced by a generic handler and the
regression lock proves the rendered output is unchanged; an unknown sport slug
404s rather than 500ing on a registry miss.

---

## WS-P2 — Payload safety (lands before any new cell is reachable)

### Task 5: The context builder must survive a summary with no `p_winner`

**Files:**

- modify `app/routes/ithrottir.py`
- create `tests/fixtures/ithrottir/` (generated — see steps)
- create `tests/test_ithrottir_multisport.py`

**Interfaces:** none new; `_build_league_context` becomes total over both summary shapes.

- [ ] Generate the fixture tree once, with the exact command in **Verified
      contract** above, and copy the eight `{sex}-{slug}` directories to
      `tests/fixtures/ithrottir/{basketball,handball}/iceland/`. Trim each
      `final_positions_history.json` / `team_strengths_history.json` /
      `standings_history.json` to keep the tree small; leave the other seven
      byte-identical. Record the generating command in a
      `tests/fixtures/ithrottir/README.md` so the fixture is reproducible.

- [ ] Write `tests/test_ithrottir_multisport.py` with a `monkeypatch.setattr(ithrottir, "DATA_DIR", ...)`
      fixture pointing at a `tmp_path` copy of that tree (the idiom already at
      `tests/test_ithrottir_routes.py:322-338`). Add a test calling
      `_build_league_context("korfubolti", "karla", "bonus")` and asserting it
      returns a dict whose `facts` has four entries.

- [ ] RUN IT. **EXPECTED RED:** `KeyError: 'p_winner'` raised from
      `app/routes/ithrottir.py:391` (`max(summary, key=lambda r: r["p_winner"])`).
      (Registering `korfubolti` in `SPORTS` is task 4; until then, call the
      builder directly rather than through the router, and mark the test
      `pytest.mark.usefixtures` on the tmp data dir.)

- [ ] Implement. Replace the three summary reads:
      `favourite = max(summary, key=lambda r: r.get("p_top_of_table", r.get("p_winner", 0)), default=None)`;
      the "Efst spáð" sub uses the same accessor; `top6_contenders` becomes
      `qualify_rows = [r for r in summary if (r.get("p_qualify") or r.get("p_top_six") or 0) >= 0.50]`
      and the fact is emitted only when at least one summary row carries one of
      those keys — otherwise the ID-C4 "Lið í deild" tile with
      `final_positions.n_teams`. `p_relegation` is present in every sport's
      summary, so `relegation_risk` is unchanged.

- [ ] RUN AGAIN green + task 1 green (football's four fact labels unchanged).
      Commit: `fix(ithrottir): read p_top_of_table/p_qualify so a summary without p_winner cannot 500`.

**Verification.** The builder is total over both published summary shapes;
football's fact pack is byte-identical (regression lock) and a basketball cell
produces four well-formed tiles instead of a 500.

---

### Task 6: `total_rounds` from `meta.n_rounds`, clamped

**Files:**

- modify `app/routes/ithrottir.py`
- modify `tests/test_ithrottir_multisport.py`

- [ ] Add: for handball **kvenna** `olis` (a triple round robin — 8 teams, 84
      fixtures, 21 rounds; `~/sports/config/leagues.yml:170-178`), assert the
      "Umferðir eftir" fact's value string ends `f"af {meta['n_rounds']}"` and
      that `n_rounds` is *not* `2 * (n_teams - 1)`. Add a second case with
      `meta.n_rounds` deleted from the fixture asserting the football fallback
      `2*(n_teams-1)` still applies. Add a third with `matches_played > n_rounds`
      (write `played: 35` into a copy of the basketball standings — the real
      playoff-overhang state) asserting the rendered remainder is `0`, never
      negative.

- [ ] RUN IT. **EXPECTED RED:** the first case fails with
      `assert '14 af 14'.endswith('af 21')` — `2*(8-1) == 14` against the
      published `n_rounds: 21`.

- [ ] Implement at `:406`:
      `n_rounds = (meta or {}).get("n_rounds") or 2 * (n_teams - 1)`, then
      `remaining = max(0, n_rounds - matches_played)`. Use `n_rounds` in the
      fact, the stateline and the `ticker_value` (`:474`).

- [ ] RUN AGAIN green + task 1 green. Commit:
      `fix(ithrottir): round count comes from meta.n_rounds, clamped at zero`.

**Verification.** The one competition format that breaks `2*(n_teams-1)` renders
correctly, and the playoff-overhang state that would have shown "−13 af 22"
shows "0 af 22".

---

### Task 7: The D3 relabel, driven by the payload

**Files:**

- modify `app/routes/ithrottir.py`
- modify `app/templates/components/ithrottir/finishing_heatmap.html`
- modify `tests/test_ithrottir_multisport.py`

**Interfaces:** context gains `regular_season_only: bool` and `playoff_note: str | None`.

- [ ] Add a test asserting, for every bb/hb fixture cell, that the built
      context's `stateline` contains `"deildarkeppnina"` and does **not**
      contain `"Íslandsmeistar"`; that `playoff_note` is the full sentence
      *"Spáin nær aðeins til deildarkeppninnar. Íslandsmeistari ræðst í
      úrslitakeppni sem líkanið spáir ekki fyrir um."*; and that the "Efst
      spáð" sub reads `f"{p}% líkur á efsta sæti í deildarkeppni"`. Add the
      inverse for football `karla/besta`: `"Íslandsmeistaratitlinum"` present,
      `playoff_note is None`. Write the Icelandic literals with a python
      heredoc.

- [ ] RUN IT. **EXPECTED RED:**
      `AssertionError: assert 'deildarkeppnina' in '... á BAM BD 01 mestar líkur á Íslandsmeistaratitlinum.'`

- [ ] Implement. Compute `regular_season_only` and `playoff_note` per ID-C1.
      Replace the two statelines (`:453-463`) and the "Efst spáð" sub (`:424`)
      with the §15 strings when `regular_season_only`. Add both to the returned
      context. In `finishing_heatmap.html:8` render
      `Lokastaða deildarkeppni (spáð)` when `regular_season_only` else
      `Lokastaða (spáð)`, and render `playoff_note` as a `panel-caption` line
      beneath when set. Do not put the Jinja conditional inside an HTML
      attribute (the formatter hook breaks those).

- [ ] Cross-check the new strings with the Miðeind grammar checker before
      committing — the football stateline's single-V2-verb construction
      (*á … mestar líkur á*) was checked and the replacement preserves it.

- [ ] RUN AGAIN green + task 1 green. Commit:
      `feat(ithrottir): D3 regular-season copy driven by meta.season_scope`.

**Verification.** The word *Íslandsmeistari* cannot reach a basketball or
handball page, and it does so because the payload says
`season_scope: "regular_season"` — a fourth sport with the same property gets
the right copy with no code change.

---

## WS-P3 — Register the new sports

### Task 4: basketball + handball entries, and the badge assertion

*(Numbered 4 to match the design's reading order; sequenced after tasks 5–7 —
see Execution order.)*

**Files:**

- modify `app/routes/ithrottir.py`
- modify `tests/test_ithrottir_multisport.py`

- [ ] Add a parametrised sweep over the eight cells asserting `200` against the
      fixture tree, plus a `404` for `/ithrottir/korfubolti/iceland/karla/olis/`
      (a real sport with another sport's division slug). **Then the S1
      assertion**, which is the point of this task:

```python
@pytest.mark.parametrize("sport,sex,div", ALL_NEW_CELLS)
def test_registry_badge_matches_published_division_code(sport, sex, div, fixture_root):
    from app.routes.ithrottir import SPORTS, _league_data_dir
    payload = json.loads((_league_data_dir(sport, sex, div) / "next_games.json").read_text())
    codes = {m["division_code"] for m in payload["matches"]}
    assert codes == {SPORTS[sport]["divisions"][div]["code"]}
```

      and an end-to-end guard that the rendered page does **not** contain
      `Engir leikir framundan` for a cell whose fixture has upcoming matches.

- [ ] RUN IT. **EXPECTED RED:** `KeyError: 'korfubolti'` from `SPORTS[sport]`
      in the badge test (and 404s in the sweep).

- [ ] Implement. Add the two registry entries. Divisions and **badges**
      (ID-C2): `korfubolti` → `bonus {dir: "bd", code: "BON", title: "Bónusdeild"}`,
      `1deild {dir: "1d", code: "B1D", title: "1. deild"}`; `handbolti` →
      `olis {dir: "od", code: "OD", title: "Olísdeild"}`,
      `grill66 {dir: "g66", code: "G66", title: "Grill 66-deild"}`. All four
      `is_cup: False`, `sexes: {"karla","kvenna"}`. Sport keys:
      korfubolti `{label: "Körfubolti", dir: "basketball", score_group: "Stigaskor",
      score_sub: ("Skoruð","Fengin"), score_label: "Vænt stig", score_noun: "stig",
      has_draws: False, has_xg: False, score_precision: 0, default_division: "bonus"}`;
      handbolti `{label: "Handbolti", dir: "handball", score_group: "Mörk",
      score_sub: ("Skoruð","Fengin"), score_label: "Vænt mörk", score_noun: "mörk",
      has_draws: True, has_xg: False, score_precision: 1, default_division: "olis"}`.

- [ ] RUN AGAIN green + task 1 green. Commit:
      `feat(ithrottir): register basketball and handball with their published badges`.

**Verification.** Every new URL serves 200 off real published JSON, and the
badge assertion makes S1 — a silently empty fixtures grid — impossible to ship.

---

## WS-P4 — Presentation

### Task 8: One page template, three prose partials

**Files:**

- rename `app/templates/ithrottir_fotbolti.html` → `app/templates/ithrottir_league.html`
- create `app/templates/components/ithrottir/method_{fotbolti,handbolti,korfubolti}.html`
- modify `app/routes/ithrottir.py` (the `TemplateResponse` name at `:564`)
- modify `tests/test_ithrottir_multisport.py`

- [ ] Add a test asserting the basketball page contains `Student-t` (its method
      partial's distinguishing phrase) and does **not** contain
      `Tvívíð Poisson-dreifing`; the inverse for football; and that both contain
      the shared `dataBase` const with the right sport slug
      (`/ithrottir/korfubolti/iceland/karla/bonus/data`).

- [ ] RUN IT. **EXPECTED RED:**
      `jinja2.exceptions.TemplateNotFound: components/ithrottir/method_korfubolti.html`.

- [ ] Implement. `git mv` the template. Move lines ~95-160 verbatim into
      `method_fotbolti.html`, **including the `{% if is_cup %}` branch** — verify
      by diffing the rendered football cup page against task 1's baseline.
      Write the two new partials: 2DT Student-t framing, no xG paragraph, the
      `playoff_note` disclaimer repeated, and an explicit sentence that the
      goal-diff strip's centre bucket spans ±1 mark (handball) / ±2 stig
      (basketball) and is not the draw probability. Basketball's adds that
      *stig* means two things in the standings (Stigaskor vs Stig) and that
      there are no draws. Icelandic via heredoc; grammar-check before commit.
      Replace the Aðferð block with the dynamic include. Interpolate
      `sport_slug` into the `dataBase` const (`:197`) and the `og_image` block
      (`ithrottir_league.html:5`): 2-part id for football, 3-part `{{ sport_slug }}-{{ sex_slug }}-{{ division_slug }}`
      otherwise. **Bump every `?v=N` cache-buster in BOTH places** — the
      `modulepreload` in `{% block head %}` (`:11-15`) and the `import` line in
      `{% block scripts %}` (`:181-191`); a mismatch causes a silent double
      fetch.

- [ ] `grep -rn "ithrottir_fotbolti" app/ tests/ scripts/` must return nothing.

- [ ] RUN AGAIN green + task 1 green. Commit:
      `refactor(ithrottir): one league template, three per-sport method partials`.

**Verification.** One 263-line template serves three sports; football's rendered
page is unchanged including the cup branch; the method prose is per-sport and
states the bucket-width caveat where a reader will meet it.

---

### Task 9: `standings-table.js` — the draw column vanishes

**Files:**

- modify `app/static/js/standings-table.js`
- modify `app/templates/ithrottir_league.html` (call site + `?v=` bump)
- create `tests/test_ithrottir_standings_opts.py`

**Interfaces:**
`renderStandingsTable(el, data, opts)` gains
`opts.showDraws` (default `true`), `opts.scoreGroup` (default `"Mörk"`),
`opts.scoreSub` (default `["Skoruð","Fengin"]`). `showFravik` is derived from the
data, not from opts.

- [ ] JS has no test runner in this repo, so assert through the rendered HTML
      shell: a pytest test asserting the basketball page's `{% block scripts %}`
      passes `showDraws: false` and `scoreGroup: "Stigaskor"` to
      `renderStandingsTable` (call site `ithrottir_league.html:238`), the handball page passes `scoreGroup: "Mörk"` and
      no `showDraws` override, and the football page passes neither.
      Additionally add a `node --check`-style syntax guard if one already exists
      in CI; otherwise rely on `test_ithrottir_football_regression.py` catching a
      broken module via the page still rendering.

- [ ] RUN IT. **EXPECTED RED:** `assert 'showDraws: false' in r.text` fails —
      the call site at `ithrottir_league.html:238` passes no opts at all.

- [ ] Implement in `standings-table.js`. Read the three opts with the defaults
      above. When `showDraws` is false, drop `<th scope="col" rowspan="2">J</th>`
      (`:331`) and `<td>${r.draws}</td>` (`:359`). Replace the literal `Mörk`
      group head (`:322`) and the `Skoruð`/`Fengin` sub-heads (`:340-342`).
      Add `const showFravik = rows.some(r => buildResidualSeries(r).length > 0)`
      and hide the Frávik header + cells when false — verified necessary:
      bb/hb ship `xg_trend: []` so `buildResidualSeries` (`:51`) returns `[]`
      for every row and the column is always empty. **Do not touch
      `showExpectedCols` (`:275`)** — verified it already hides the xG/Δ columns
      from `xpts: null`, and `morkSpan`/`stigSpan` (`:311-312`) already collapse
      with it. Re-derive `morkSpan`/`stigSpan` only if `showFravik` changes the
      spanned column count (it does not — Frávik sits outside both groups;
      confirm before editing).

- [ ] RUN AGAIN green. Open both a football and a basketball page and count
      `<th>` against `<td>` per row — the two-row thead's colspans must stay
      consistent or the table shears. Commit:
      `feat(ithrottir): standings drops the draw column and relabels the score group per sport`.

**Verification.** Basketball's standings has no J column (rather than a column
of zeros), its scored/conceded group reads *Stigaskor* so it is not confusable
with the *Stig* points group in the same thead, and football's header is
unchanged.

---

### Task 10: `next-games-grid.js` — two-way cards and an honest goal-diff strip

**Files:**

- modify `app/static/js/next-games-grid.js`
- modify `app/templates/ithrottir_league.html` (call site + `?v=` bump)
- modify `tests/test_ithrottir_standings_opts.py`

**Interfaces:**
`renderCard(m, teamRanks, opts)` and `renderGoalDiffStrip(dist, opts)` — both
currently take no opts (S8). New opts, all defaulting to today's behaviour:
`noDraw` (false), `highlightZero` (true), `scoreLabel` ("xG"), `scorePrecision` (1).

- [ ] Add assertions that the basketball page's render call passes
      `noDraw: true`, `highlightZero: false`, `scoreLabel: "Vænt stig"`,
      `scorePrecision: 0`; the handball page passes `highlightZero: false` and
      `scoreLabel: "Vænt mörk"` but **not** `noDraw`; the football page and the
      HM 2026 page (`app/templates/ithrottir_hm2026.html:2154`) pass neither.

- [ ] RUN IT. **EXPECTED RED:** `assert 'highlightZero: false' in r.text`.

- [ ] Implement. Thread `opts` from `renderFixturesGrid` (`:263`) through the
      `renderCard` map (`:295`) and into `renderGoalDiffStrip` (`:254`).
      - `noDraw`: a third probs mode alongside the existing `isKnockout` branch
        (`:203-212`) — `[{key:"H", p:m.p_home_win, align:"l"}, {key:"Ú", p:m.p_away_win, align:"r"}]`
        and a two-segment `probBar`. **Do not reuse `advance_home`**: it means
        "advances via ET/penalties" and its bar is `1 - advance_home`.
      - `scoreLabel` / `scorePrecision`: replace the hardcoded
        `xG ${goals(...)}` at `:256`; `goals()` (`:83`) gains a precision
        argument, still comma-decimal.
      - `highlightZero: false`: no jökull fill on the `d.diff === 0` bar
        (`:174-176`) — all bars `var(--subtle)` at 0.45.
      - `barW`: replace `Math.max(1.5, 100 / (span + 1) - 0.5)` (`:166`) with a
        width derived from the **minimum gap between consecutive `diff` values**.
        Measured on the fixture: basketball's 5-point bins over ±10 give
        `barW ≈ 4.3` against a ≈23.9-unit stride — thin needles with wide gaps.

- [ ] RUN AGAIN green. Load the HM 2026 page and the football cup page and
      confirm their cards are visually unchanged (`tests/test_hm2026_routes.py`
      and `tests/test_ithrottir_wc26.py` must stay green).
      Commit: `feat(ithrottir): opt-in two-way cards, per-sport score label and an unhighlighted diff strip`.

**Verification.** Basketball cards show H/Ú only; neither new sport highlights a
centre bucket that spans ±2 stig / ±1 mark; football and the World Cup page take
the untouched default path.

---

### Task 11: Legend and prose copy in the partials

**Files:**

- modify `app/templates/components/ithrottir/fixture_card.html`
- modify `app/templates/components/ithrottir/standings_table.html`
- modify `tests/test_ithrottir_multisport.py`

- [ ] Assert on the rendered basketball page: `Súlur: líkur á heimasigri · útisigri`
      present, `jafntefli` absent from the fixture legend, `Stigamunur: 0 í miðju · lína = vænt gildi`
      present, `xG = vænt mörk` absent, and the standings prose reads
      `spáðum stigamun`. Handball: `Markamunur: 0 í miðju · lína = vænt gildi`,
      `jafntefli` retained in the bars legend, `xG = vænt mörk` absent. Football:
      all three original legend spans byte-identical.

- [ ] RUN IT. **EXPECTED RED:** `assert 'jafntefli' not in legend` — the current
      `fixture_card.html:18-20` hardcodes all three spans.

- [ ] Implement. Drive the bars legend on `sport.has_draws`, the margin-strip
      legend on `sport.score_noun` (Stigamunur / Markamunur) with the literal
      `0 í miðju` for both new sports, and drop the third `xG = vænt mörk` span
      entirely when `not sport.has_xg`. In `standings_table.html:10`
      parameterise `markamismun` on `sport.score_noun`. Icelandic via heredoc;
      grammar-check.

- [ ] RUN AGAIN green + task 1 green. Commit:
      `feat(ithrottir): per-sport fixture legend and standings prose`.

**Verification.** No surface on a basketball page says *jafntefli*, and neither
new sport labels the strip's centre bucket as a draw.

---

## WS-P5 — Discovery surfaces

### Task 12: `og.py` — per-sport cards, 3-part ids, football's URLs alive

**Files:**

- modify `app/routes/og.py`
- modify `tests/test_og_cards.py`

- [ ] Add: `/og/ithrottir/karla-besta.png` → 200 `image/png` (the live 2-part
      contract); `/og/ithrottir/korfubolti-karla-bonus.png` → 200; assert the
      basketball card's max-points axis uses `meta.points.win` by asserting the
      generated bytes differ from a card generated with a monkeypatched
      `points.win` of 3 (the current hardcoded `round_num * 3`); assert
      `_cache_key` output differs for `("ithrottir","fotbolti","karla","besta",fp)`
      vs `("ithrottir","korfubolti","karla","besta",fp)` with the same
      fingerprint.

- [ ] RUN IT. **EXPECTED RED:** the basketball URL returns the static
      "Greining og spár · Íþróttir" fallback, so its bytes equal the static
      card's — `assert card != static_card` fails. (Verified cause:
      `_card_png` at `og.py:954` does `page_id.split("-", 1)`, yielding
      `sex="korfubolti"`, which misses `ITH_SEX_GEN`.)

- [ ] Implement. `ITH_DIVISION_DIR` (`:625`) becomes per-sport, derived from
      `ithrottir.SPORTS` (import is safe — `ithrottir.py` does not import
      `og.py`; re-grep before relying on it). `_ithrottir_league_dir` (`:631`)
      takes `sport`. In `generate_ithrottir_card`, replace
      `max_points = max(round_num * 3, 1)` (`:693`) with
      `max(r["played"] for r in top) * meta["points"]["win"]`, falling back to
      `round_num * 3` when `meta.points` is absent (football's pre-v2 live tree)
      — `meta.round` is unreliable for basketball, which reads 22 against 35
      played in the playoff-overhang state. Add `sport` to the `_cache_key` call
      (`:676`) — latent today (S7) but one line. `_card_png`: split on `-` and
      treat a **2-part id as `fotbolti`** so every crawled football OG URL keeps
      working; a 3-part id names the sport. `ITH_LEAGUES` (`:898`) gains the
      eight new cells for prewarm, each wrapped in the existing per-item
      `try/except` so a missing directory logs a warning rather than failing
      startup.

- [ ] RUN AGAIN green + task 1 green (which asserts the football OG URL).
      Commit: `feat(og): per-sport íþróttir cards, football's 2-part ids unchanged`.

**Verification.** Both id shapes resolve, the points axis comes from the
published scheme rather than football's 3, and no cache path is shared across
sports.

---

### Task 13: `pages.py` — sitemap loop and eight short links

**Files:**

- modify `app/routes/pages.py`
- modify `tests/test_routes.py`

- [ ] Assert: every URL in the generated sitemap returns 200; the nine football
      rows are still present; a cell whose `meta.json` is missing produces **no**
      row and **no** exception (delete one from a tmp tree and re-generate); the
      eight new short links 301 to their documented targets; the nine existing
      short links are unchanged.

- [ ] RUN IT. **EXPECTED RED:** `AssertionError` — the basketball URLs are
      absent from the sitemap (`_SITEMAP_ENTRIES` at `pages.py:29-61` hand-lists
      nine football rows).

- [ ] Implement. Replace the nine hand-listed rows with a loop over
      `ithrottir.SPORTS × SEXES × divisions`, skipping any cell whose
      `meta.json` does not exist, and **wrap the `stat()` in try/except** — the
      bb/hb directories do not exist on this repo's disk at all right now
      (`data/ithrottir/{basketball,handball}/iceland/` currently holds the stale
      un-suffixed `karla/`+`kvenna/` dirs, which the next `rsync --delete`
      removes; the new ones appear only after a successful upstream 2DT
      fit+publish). A sitemap that silently drops a cell mid-rsync is
      acceptable; a 500 is not. Add the eight short links from design §14's
      `url_scheme` verbatim (`olisdeild-kk/kvk`, `grill66-kk/kvk`,
      `bonusdeild-kk/kvk`, `korfu-1deild-kk/kvk`); **do not touch the existing
      nine** — the football reel outros deep-link to them
      (`scripts/reels/competitions.py`).

- [ ] RUN AGAIN green + task 1 green. Commit:
      `feat(pages): derive the íþróttir sitemap from the sport registry, add eight short links`.

**Verification.** The sitemap advertises exactly the cells that exist, and it
cannot 500 on an absent directory — the state the platform is in today.

---

### Task 14: The nav dropdown

**Files:**

- modify `app/templates/base.html`
- modify `tests/test_ithrottir_multisport.py`

- [ ] Assert: the dropdown contains three sport items; on
      `/ithrottir/korfubolti/iceland/karla/bonus/` only the Körfubolti item
      carries `aria-current="page"`; on `/ithrottir/fotbolti/iceland/karla/besta/`
      only Fótbolti does.

- [ ] RUN IT. **EXPECTED RED:** `assert r.text.count('aria-current="page"') ...`
      — `base.html:172`/`:197` key the football item on
      `_path.startswith('/ithrottir')`, so all three would be current at once
      once the items exist (and today there is only one item, so the count is
      wrong either way).

- [ ] Implement. Add Handbolti and Körfubolti items pointing at each sport's
      karla top division. Narrow the per-item condition to
      `_path.startswith('/ithrottir/<slug>/')`; the *trigger* button keeps the
      broad `/ithrottir` test. Mobile nav (`base.html:252`, a single Íþróttir link to football) may stay as is.

- [ ] RUN AGAIN green + task 1 green. Commit:
      `feat(nav): handbolti and körfubolti in the Íþróttir dropdown`.

**Verification.** Each sport is reachable from every page and exactly one item
is marked current.

---

### Task 15: Rewrite the rule file

**Files:**

- modify `.claude/rules/ithrottir.md`

- [ ] Rewrite "Platform scope": it currently says **football-only until autumn
      2026**, calls the bb/hb data *parked*, and instructs a future session to
      adapt the chart modules to `mean_home`/`p_tie`/`division` — every one of
      those is now false (Verified contract). Replace with the multi-sport model,
      the badge-vs-code distinction (S1), the payload-driven D3 relabel (ID-C1),
      and the diff-bin-width caveat.
- [ ] Correct "Source of truth": bb/hb publish to `{sex}-{slug}` dirs, ten
      artefacts per cell, football's two bikar cells carry twelve.
- [ ] Record S6: `data/ithrottir-schemas/` is rsynced from
      `~/sports/config/publish-schemas/` at pull time and is **not** committed
      by the workflow, so the in-repo copy being football-only is not evidence
      that bb/hb are ungated.
- [ ] Extend the `paths:` frontmatter with
      `app/templates/ithrottir_league.html`, `data/ithrottir/basketball/**`,
      `data/ithrottir/handball/**`.
- [ ] Commit: `docs(ithrottir): rewrite the rule for the multi-sport model`.

**Verification.** The next session reads a rule that matches the code. This is
the highest-value doc change in the plan, not an afterthought.

---

### Task 16: Whole-branch verification

- [ ] `uv run --extra dev pytest tests/` — full suite green, including
      `test_hm2026_routes.py`, `test_ithrottir_wc26.py`, `test_og_cards.py`.
- [ ] `uv run --extra dev ruff check .` clean.
- [ ] `uv run uvicorn app.main:app --reload` and walk all 17 league URLs plus
      `/ithrottir/`, both `/iceland/{sex}/` redirects per sport, the 17 short
      links and the sitemap. Check the browser console is clean on one page per
      sport — a JS opts regression is silent server-side.
- [ ] Trace one basketball fixture card end-to-end: the payload's
      `mean_home_goals` → the card's *Vænt stig* at 0 decimals; the payload's
      `goal_diff_distribution` diffs (multiples of 5) → five evenly spaced,
      unhighlighted bars. This is the check that catches a bug living across
      task 10's JS and task 4's registry, which no per-task test can see.
- [ ] Force a real pull: `gh workflow run pull-sports-data.yml --repo metill-is/metill-platform`,
      then confirm the run's validator step reports **zero `unmatched`** for
      basketball and handball and that `data/ithrottir/{basketball,handball}/iceland/`
      now holds eight `{sex}-{slug}` directories and no `karla/`/`kvenna/`.
      **If the eight directories are absent, that is expected** until an upstream
      2DT fit+publish succeeds on CI — see R1. Record which state you observed.
- [ ] `git log @{u}..HEAD` before any push, then open a PR. Do not push to `main`.

---

## Residual risks

**R1 — the data does not exist yet, and its arrival is gated on an event nobody
has measured.** `~/sports/data/publish/{basketball,handball}` was deleted in
commit `0c14d03df` and the eight new cells appear only after a 2DT fit *and* a
publish succeed on a GitHub runner. Per the design's own first residual risk,
the 2DT fit has never run green inside `fit.yml`'s budget and nobody has timed it
against the 240-minute cap. So this plan can be complete, merged and deployed
while every new URL still renders an empty shell. `_sport_available` keeps the
tabs disabled and task 13 keeps the sitemap honest, but there is no platform-side
mitigation for the underlying event. Do not announce the URLs until a cell lands.

**R2 — the badge/code near-miss (S1) is a silent failure, and only one assertion
catches it.** `division_code` is `"BON"` while `division` is `"BD"`; a registry
that takes the wrong one renders "Engir leikir framundan" with a 200, a clean
console and no log line. Task 4's parametrised badge test is the only detector,
and it is fixture-based — if the upstream `code_badge` config changes without
the fixture being regenerated, the test passes against stale data. Regenerate
`tests/fixtures/ithrottir/` whenever `~/sports/config/leagues.yml`'s
`publish_divisions` changes.

**R3 — `next-games-grid.js` and `standings-table.js` are shared with HM 2026 and
the cup page, and this plan changes two function signatures** (`renderCard`,
`renderGoalDiffStrip`) that today take no opts at all. Every new behaviour is
opt-in with the current behaviour as the default, and
`tests/test_hm2026_routes.py` / `test_ithrottir_wc26.py` guard the other
consumers — but those are route tests, not render tests: they assert the page
serves, not that a card looks right. The visual check in task 16 is the real
guard and it is manual.

**R4 — the copy is new and mostly unreviewed.** §15's strings were drafted
before the producer existed; three of them (the qualification label, and both
"Líkur á úrslitakeppni" framings) are unreachable (S3), and ID-C4's
"Lið í deild" is invented by this plan. Every Icelandic string added here should
go through Miðeind's grammar checker *and* past the user before merge.

**R5 — "Fallhætta" may be a claim the payload cannot back.** `meta.relegation.slots`
is `null` for all eight bb/hb cells, so `p_relegation` falls back to the legacy
`placement >= n_teams - 1` rule (`~/sports/R/publish-format.R:383-387`). For
basketball's 1. deild — the bottom tier — that is a bottom-two probability with
no relegation beneath it. The tile is not *wrong* (it is a real finishing
probability) but its label overclaims. Either relabel it per-sport or configure
`relegation_slots` upstream; this plan does neither and the decision is deferred.

**R6 — one methodology page for three sports** (ID-C7). `/ithrottir/adferdafraedi`
describes the bivariate Poisson football model; a basketball reader following
"aðferðafræðisíðu" from the Aðferð partial lands on prose about a model that is
not the one behind the page they came from. The per-sport partial mitigates it;
it does not fix it.

**R7 — the fixture tree is synthetic.** Everything in *Verified contract* was
observed on `tests/testthat/fixtures/`, whose teams are `BAM BD 01` and whose
season is `2100`. Field names, types, presence/absence and units are trustworthy;
*magnitudes* and team-name edge cases are not. In particular the `CREST_SLUG`
near-miss is real on production data and untested here: `~/sports/data/facts/results`
carries **both** `Þór Þ.` and `Þór Þorlákshöfn` (basketball) **and** both
`Þór Ak.` and `Þór Akureyri`, while `CREST_SLUG` keys only the bare `"Þór"`
(`app/routes/ithrottir.py:80`). Exact-string keying is what keeps this safe —
`Þór Þ.` currently falls through to its text short-code, which is correct. **Do
not add a prefix or fuzzy match, and do not add `"Þór Þ."` pointing at `thor`.**
Handball's bare `"Þór"` *is* Þór Akureyri, so the existing key is right for that
sport by coincidence, not by design.
