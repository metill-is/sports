# Football Iceland — lower-division publish + consumer integration

> **For agentic workers:** REQUIRED SUB-SKILL: Use `superpowers:subagent-driven-development` (recommended) or `superpowers:executing-plans` to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Surface three additional Icelandic football cells on metill.is — `karla-2deild` (men's 2. deild, KSÍ tier 3), `karla-3deild` (men's 3. deild, tier 4), and `kvenna-2deild` (women's 2. deild, tier 3) — without touching the model, ingest, or fit layers.

**Architecture:** No new infrastructure. The Stan model already trains on these divisions (per `config/leagues.yml::training_filter`), KSÍ ingest already pulls them (per `R/ingest-ksi-football.R`), and the data sits in `data/facts/results/`. The change is **publish surface only**: extend the extract + publish loops over an extra 3 (sex, division) cells, then register their URL slugs on the consumer.

**Cell inventory (post-change):**

| Sex | Division | KSÍ tier | Matches in data | New? |
|---|---|---:|---:|:---:|
| male | BD | 1 | 708 | — |
| male | LD1 (Lengjudeildin) | 2 | 690 | — |
| male | **LD2 (2. deild)** | **3** | **684** | **NEW** |
| male | **LD3 (3. deild)** | **4** | **683** | **NEW** |
| male | CUP (Mjólkurbikar) | knockout | 455 | — |
| female | BD | 1 | 475 | — |
| female | LD1 (Lengjudeildin) | 2 | 465 | — |
| female | **LD2 (2. deild)** | **3** | **414** | **NEW** |
| female | CUP (Bikar kvenna) | knockout | 179 | — |

Total publish cells: 6 → 9. Total per-fit JSON output: 68 → 101 files.

**Out of scope (decided):**
- **Male LD4 (4. deild, tier 5):** 285 matches but explicitly excluded from `training_filter` (`config/leagues.yml:201-207`) — amateur 12-1 / 10-0 cup blowouts introduced funnel-shaped posteriors. Re-including would require a separate model-design pass.
- **Female LD3 / LD4:** no women's 3. or 4. division exists in the Icelandic pyramid.
- **Betting on lower divisions:** Lengjan doesn't price 3. deild or women's 2. deild. The plan ships publish output (standings, predictions, team strengths) but does not extend the decide/placer layers. Male 2. deild already has `lengjan.competitions` id 1581 configured (`config/leagues.yml:133`); betting on it remains opt-in and untouched here.

**Spec / design notes:** none separate from this file — the design is shallow enough to live inline.

**Tech stack:** R package monorepo (`devtools::test()`) for the producer side; FastAPI + Jinja2 + pytest for the consumer side. No new dependencies in either repo.

---

## Task 1: Producer — register publish divisions per sex

**Files:**
- Read: `config/leagues.yml` (lines 117-230, football_iceland block)
- Modify: `config/leagues.yml` — add `publish_divisions` block under `football_iceland`
- Modify: `R/extract-football-iceland.R` (line 33, plus the two callers at 634 and 862)
- Modify: `R/publish-football-iceland.R` (line 1566 — the main publish loop; line 660 is the legacy `_from_fit_pfi` wrapper and stays BD/LD1-only per `.claude/rules/publish-layer.md`)
- Modify: `config/leagues.schema.json` — add `publish_divisions` to the validator
- Read-only: `R/config.R::load_leagues()` to confirm it propagates the new field

- [ ] **Step 1.1: Add `publish_divisions` to `config/leagues.yml`**

Append the following under `football_iceland`, immediately after `training_filter` (line 210):

```yaml
  # Per-sex publish surface. Drives the extract + publish loops and is
  # the single source of truth that the metill-platform consumer's
  # DIVISIONS dict mirrors. To add a division, ensure it's in
  # training_filter.divisions AND has a (sex, division) partition in
  # data/facts/results/, then list it here. URL slugs match the
  # consumer's URL slug exactly (e.g. "ld" not "ld1", "2deild" matches
  # /fotbolti/iceland/{sex}/2deild/).
  publish_divisions:
    male:
      - { code: BD,  slug: bd,     label_is: "Besta deild",    is_cup: false }
      - { code: LD1, slug: ld,     label_is: "Lengjudeild",    is_cup: false }
      - { code: LD2, slug: 2deild, label_is: "2. deild",       is_cup: false }
      - { code: LD3, slug: 3deild, label_is: "3. deild",       is_cup: false }
      - { code: CUP, slug: bikar,  label_is: "Mjólkurbikar",   is_cup: true  }
    female:
      - { code: BD,  slug: bd,     label_is: "Besta deild",    is_cup: false }
      - { code: LD1, slug: ld,     label_is: "Lengjudeild",    is_cup: false }
      - { code: LD2, slug: 2deild, label_is: "2. deild",       is_cup: false }
      - { code: CUP, slug: bikar,  label_is: "Bikar kvenna",   is_cup: true  }
```

The `code` matches the canonical internal division name (joins to `data/facts/results/`'s `division` column). The `slug` becomes both the URL segment on the consumer and the dir suffix under `data/publish/football/iceland/{sex_folder}-{slug}/`. `label_is` is informational but feeds the consumer's tab labels via the JSON's `meta.json` — see Task 4.

- [ ] **Step 1.2: Extend `config/leagues.schema.json`**

Add a `publish_divisions` definition mirroring the YAML shape. Required keys per entry: `code` (enum or string), `slug` (URL-safe pattern), `label_is` (string), `is_cup` (boolean). Optional at the top level for non-football leagues (basketball/handball don't yet have division splits). Validate against `config/leagues.yml` after editing — `Rscript -e 'sports::load_leagues()'` should not error.

- [ ] **Step 1.3: Replace `.FOOTBALL_ICELAND_DIVISIONS_PFI` with config-driven lookups**

Currently `R/extract-football-iceland.R:33` reads:

```r
.FOOTBALL_ICELAND_DIVISIONS_PFI <- c("BD", "LD1", "CUP")
```

Replace this file-scope constant with a helper that reads from config:

```r
.football_iceland_divisions_pfi <- function(sex) {
  cfg <- load_leagues()[["football_iceland"]][["publish_divisions"]][[sex]]
  vapply(cfg, function(d) d$code, character(1))
}
```

Update the two callers (lines 634 and 862) so `target_divs` defaults are computed per-sex at the call site rather than from the file-scope constant. The function signatures already accept `sex`, so this is a 2-line edit per call site.

- [ ] **Step 1.4: Replace `division_dir_suffix` in the main publish loop**

`R/publish-football-iceland.R:1566` currently reads:

```r
division_dir_suffix <- c(BD = "bd", LD1 = "ld", CUP = "bikar")
```

Replace with a per-sex lookup driven by the same config:

```r
publish_cfg <- load_leagues()[["football_iceland"]][["publish_divisions"]][[sex]]
division_dir_suffix <- setNames(
  vapply(publish_cfg, function(d) d$slug,    character(1)),
  vapply(publish_cfg, function(d) d$code,    character(1))
)
```

The for-loop body at line 1567+ is already division-generic (it branches on `is_cup`, gates league-table outputs, etc.). No further changes to the body needed.

**Do NOT** modify `R/publish-football-iceland.R:660` (`c(BD = "bd", LD1 = "ld")`). That's inside `.publish_football_iceland_from_fit_pfi`, the legacy regression-backstop path that `.claude/rules/publish-layer.md` flags as "BD/LD1-only and survives as a regression backstop, deletable after a few production cycles." Leaving it pinned avoids re-engineering a path that's slated for removal.

- [ ] **Step 1.5: Verify the extract layer handles flat round-robin formats**

The extract layer was first written for BD which has UPPER/LOWER playoff splits, LD1 which is a flat round-robin, and CUP which is a knockout. LD2/LD3 are flat round-robins with no playoff phase — structurally identical to LD1 from the extract layer's perspective. Read `R/extract-football-iceland.R::.extract_division_parquets_pfi()` (~line 634-780) and confirm no branch hardcodes BD/LD1/CUP. The function should already key off `target_div` and the parquet's `division` column.

If a hardcoded branch is found, add a fallback path that treats unknown divisions as "flat round-robin, no playoff structure" — same code path as LD1.

- [ ] **Step 1.6: Manual smoke test**

```bash
Rscript -e '
suppressMessages(devtools::load_all())
# Force a fit (skip if recent enough)
fit <- fit_league("football_iceland", "male")
extract_football_iceland(fit, "football_iceland", "male")
publish_football_iceland(NULL, "football_iceland", "male")  # reads from extracts
'
```

Expected: `data/publish/football/iceland/karla-2deild/` and `data/publish/football/iceland/karla-3deild/` each contain 11 JSON files. Repeat for female — `data/publish/football/iceland/kvenna-2deild/` appears with 11 files.

Inspect `karla-2deild/meta.json` — confirm `division: "LD2"`, `is_cup: false`, `league: "2. deild"`, `n_draws: 4000` (or equivalent).

Inspect `karla-2deild/standings.json` — confirm `rows` is populated (not empty placeholder), team count matches the active 2026 LD2 season, and `xpts`/`xg_for` columns are present.

---

## Task 2: Producer — tests

**Files:**
- Modify: `tests/testthat/test-publish-football-iceland.R` (or create a new `test-publish-football-iceland-divisions.R`)
- Modify: `tests/testthat/test-config-validate.R` (if it exists; otherwise add validation to an existing config test)
- Read-only: `tests/testthat/test-extract-football-iceland.R`

- [ ] **Step 2.1: Schema-level test — `publish_divisions` round-trips**

Assert that `load_leagues()[["football_iceland"]][["publish_divisions"]]` has the expected shape:
- Top-level keys: `male`, `female`
- Each entry: list of records with `code`, `slug`, `label_is`, `is_cup`
- Male contains LD2 and LD3; female contains LD2; neither contains LD4
- Every `code` listed in `publish_divisions` is also in `training_filter.divisions` (or is the CUP code, which is excluded from training by design)

- [ ] **Step 2.2: Publish-loop test — new cells are emitted**

In a fresh tmpdir, run `publish_football_iceland()` for `male` and `female` against a fixture-backed extracts tree (or run end-to-end with the test corpus). Assert:
- `karla-2deild/meta.json`, `karla-3deild/meta.json`, `kvenna-2deild/meta.json` all exist
- Each new cell has 11 JSON files (not 12 — these are league-format, not cup)
- `is_cup` flag in each `meta.json` is `false`
- `division` field matches `LD2` or `LD3`

- [ ] **Step 2.3: Extract-loop test — `target_divs` defaults pick up new divisions**

In `tests/testthat/test-extract-football-iceland.R`, add a sub-test that calls `extract_football_iceland()` with default `target_divs` for `sex = "male"` and asserts the produced parquet has rows for `division %in% c("LD2", "LD3")`. Likewise for `female` with `division == "LD2"`.

- [ ] **Step 2.4: Run the full test suite**

```bash
Rscript -e 'devtools::test()'
```

Expected: all 1120+ assertions pass, plus the 4-8 new ones above. No regressions in BD/LD1/CUP cells.

---

## Task 3: Producer — verify CI is division-agnostic

**Files:**
- Read-only: `.github/workflows/fit.yml`, `.github/workflows/decide-publish.yml`

The Explore agent's survey confirmed that `scripts/03_fit.R` and `scripts/05_publish.R` already iterate over all (league, sex) pairs from `load_leagues()` with no division filter. CI changes should not be required.

- [ ] **Step 3.1: Confirm no division literals in CI configs**

```bash
grep -rE '(BD|LD1|LD2|LD3|LD4|CUP|bikar|2deild|3deild)' .github/workflows/
```

Expected output: empty. If any workflow has a hardcoded division literal, route it through config first.

- [ ] **Step 3.2: Wait for a real CI fit + publish run after merge**

After the PR merges, `fit.yml` runs nightly (or on push) and `decide-publish.yml` chains via `workflow_run`. Monitor:

```bash
gh run list --limit 10 --workflow=decide-publish.yml --json status,conclusion,createdAt
```

After the first successful run, inspect the committed `data/publish/football/iceland/` tree on origin/main to confirm the new dirs are committed by the workflow's git push.

---

## Task 4: Consumer — register new divisions in metill-platform

**Files (in `~/metill-platform/`):**
- Modify: `app/routes/ithrottir.py` — `DIVISIONS` dict (~lines 57-61), tab availability filtering
- Modify: `app/routes/pages.py` — sitemap entries (~lines 45-50)

The sports-side change has shipped — `data/ithrottir/football/iceland/karla-2deild/`, `karla-3deild/`, `kvenna-2deild/` will appear within an hour of merge, courtesy of `pull-sports-data.yml`'s hourly cron. The consumer changes below only handle routing + UI registration.

- [ ] **Step 4.1: Extend the `DIVISIONS` dict**

In `app/routes/ithrottir.py`, replace lines 57-61:

```python
DIVISIONS = {
    "besta":   {"dir": "bd",     "title": "Besta deild",   "code": "BD",  "is_cup": False, "sexes": {"karla", "kvenna"}},
    "lengja":  {"dir": "ld",     "title": "Lengjudeild",   "code": "LD",  "is_cup": False, "sexes": {"karla", "kvenna"}},
    "2deild":  {"dir": "2deild", "title": "2. deild",      "code": "LD2", "is_cup": False, "sexes": {"karla", "kvenna"}},
    "3deild":  {"dir": "3deild", "title": "3. deild",      "code": "LD3", "is_cup": False, "sexes": {"karla"}},
    "bikar":   {"dir": "bikar",  "title": "Mjólkurbikar",  "code": "MB",  "is_cup": True,  "sexes": {"karla", "kvenna"}},
}
```

The new `sexes` field is a set of sex-slugs for which this division is published. Female 3. deild does not exist in Iceland (no data → no JSON → 404). The set lets the consumer hide tabs that have no data per sex rather than rendering broken links.

For backward compat, divisions without an explicit `sexes` field should be treated as available to all sexes (defensive default in any code that reads the field).

- [ ] **Step 4.2: Filter the division tab list per sex**

Find the tab-rendering site (per the Explore agent's report, ~line 358-359 of `app/routes/ithrottir.py`). Replace whatever currently iterates `DIVISIONS.items()` with:

```python
divisions_for_sex = [
    (slug, meta) for slug, meta in DIVISIONS.items()
    if sex_slug in meta.get("sexes", {"karla", "kvenna"})
]
```

Use `divisions_for_sex` everywhere the previous code iterated `DIVISIONS`. The result: the `karla` (male) page shows 5 tabs (besta, lengja, 2deild, 3deild, bikar); the `kvenna` (female) page shows 4 tabs (besta, lengja, 2deild, bikar).

- [ ] **Step 4.3: Add a routing guard for unsupported (sex, division) pairs**

In the `league_page()` handler (~line 391), tighten the existing 404 check:

```python
if sex_slug not in LEAGUES or division_slug not in DIVISIONS:
    raise HTTPException(status_code=404)
if sex_slug not in DIVISIONS[division_slug].get("sexes", {"karla", "kvenna"}):
    raise HTTPException(status_code=404)
```

The same check belongs in `league_dataset()` (~line 412). Without this guard, hitting `/fotbolti/iceland/kvenna/3deild/` would 500 (data dir doesn't exist) rather than 404 cleanly.

- [ ] **Step 4.4: Extend the sitemap**

In `app/routes/pages.py`, after line 50, add:

```python
("/ithrottir/fotbolti/iceland/karla/2deild/",  _FB / "karla-2deild"  / "meta.json"),
("/ithrottir/fotbolti/iceland/karla/3deild/",  _FB / "karla-3deild"  / "meta.json"),
("/ithrottir/fotbolti/iceland/kvenna/2deild/", _FB / "kvenna-2deild" / "meta.json"),
```

(Order: keep grouped by sex, BD → LD1 → LD2 → LD3 → CUP within each sex.)

---

## Task 5: Consumer — tests

**Files (in `~/metill-platform/`):**
- Modify: `tests/test_ithrottir_routes.py`

- [ ] **Step 5.1: Happy-path tests for the new cells**

Add three tests mirroring the existing `test_karla_besta_route()` pattern:
- `test_karla_2deild_route()` — GET `/fotbolti/iceland/karla/2deild/` → 200
- `test_karla_3deild_route()` — GET `/fotbolti/iceland/karla/3deild/` → 200
- `test_kvenna_2deild_route()` — GET `/fotbolti/iceland/kvenna/2deild/` → 200

Each test fixtures a `data/ithrottir/football/iceland/{cell}/` directory with a minimal `meta.json` + `standings.json` and asserts the page renders.

- [ ] **Step 5.2: Unsupported-sex 404 test**

```python
def test_kvenna_3deild_returns_404():
    """Women's 3. deild does not exist — the tab is hidden and the URL 404s."""
    resp = client.get("/ithrottir/fotbolti/iceland/kvenna/3deild/")
    assert resp.status_code == 404
```

- [ ] **Step 5.3: Tab-rendering test — male shows 5 tabs, female shows 4**

Extend or add to the existing tab-navigation test (~line 165-194 of `test_ithrottir_routes.py`). Assert:
- The `karla` (male) page's tab list contains 5 divisions
- The `kvenna` (female) page's tab list contains 4 divisions and does NOT contain `3deild`

- [ ] **Step 5.4: Sitemap test (if one exists)**

If `tests/` has a sitemap-shape test, extend it to cover the 3 new URLs. If not, no action.

- [ ] **Step 5.5: Run the full test suite**

```bash
cd ~/metill-platform && uv run pytest
```

Expected: all existing tests pass + 4-5 new ones above.

---

## Task 6: End-to-end smoke + rollout

- [ ] **Step 6.1: Sports-side PR**

```
git checkout -b add-football-iceland-lower-divisions
# Tasks 1-3 changes
gh pr create --title "publish: surface 2. deild + 3. deild (men's) and 2. deild (women's) football iceland"
```

PR body should reference this plan file. Use `gh pr merge --rebase --auto --delete-branch` per `.claude/rules/git-hygiene.md`.

- [ ] **Step 6.2: Verify CI ships the new cells**

After merge, wait for the next `decide-publish.yml` run (or trigger via `republish.yml`'s `workflow_dispatch`). Confirm via:

```bash
gh run watch  # for the publish workflow
git pull --rebase  # after the workflow's auto-commit
ls data/publish/football/iceland/
```

Expected: `karla-2deild/`, `karla-3deild/`, `kvenna-2deild/` directories appear.

- [ ] **Step 6.3: Verify `pull-sports-data.yml` propagates to metill-platform**

`pull-sports-data.yml` runs hourly. Either wait or force:

```bash
gh workflow run pull-sports-data.yml --repo metill-is/metill-platform
gh run watch --repo metill-is/metill-platform
```

After it completes, the 3 new cells should appear under `data/ithrottir/football/iceland/` on metill-platform's main.

- [ ] **Step 6.4: Consumer-side PR**

```
cd ~/metill-platform
git checkout -b add-lower-divisions
# Tasks 4-5 changes
gh pr create --title "ithrottir: route 2. deild + 3. deild football iceland pages"
```

PR body references this plan + links to the sports-side PR.

- [ ] **Step 6.5: Visual QA after consumer PR merges**

Once metill-platform's Fly.io auto-deploy completes:
- Visit `https://metill.is/ithrottir/fotbolti/iceland/karla/2deild/` — confirm standings render with 12 teams (2026 LD2 has 12 sides), forest plot of strengths shows lower-tier teams, no console errors
- Visit `karla/3deild/` — confirm 12 teams, same layout
- Visit `kvenna/2deild/` — confirm women's teams, no console errors
- Visit `kvenna/3deild/` — confirm clean 404 page (not 500)
- Visit `karla/4deild/` — confirm clean 404 (unregistered slug)
- Tab strip on each new page should highlight the current tab and link to siblings

---

## Verification

After both PRs ship:

```r
# In sports/ repo
sports::rebuild_duckdb()
con <- DBI::dbConnect(duckdb::duckdb(), "sports.duckdb", read_only = TRUE)
DBI::dbGetQuery(con, "
  SELECT sex, division, COUNT(*) AS n_played, MAX(match_date) AS last_match
  FROM results
  WHERE sport = 'football' AND country = 'iceland' AND season = 2026
  GROUP BY 1, 2 ORDER BY 1, 2
")
```

Expected rows for `sex=male, division IN ('LD2', 'LD3')` and `sex=female, division='LD2'` with 2026-season match counts and a `last_match` date within the active season window. (LD2/LD3 typically May-Sept in Iceland.)

```bash
# In metill-platform/ repo, after consumer-side merge
for slug in 2deild 3deild; do
  curl -fsS "https://metill.is/ithrottir/fotbolti/iceland/karla/${slug}/data/standings" \
    | jq '.rows | length'
done
# Expected: a positive integer (team count) for each
```

---

## Open questions / future work

- **Lower-division calibration:** Once the new cells have shipped recommendations for a season, the Beta-Binomial calibration in `R/decide-calibration.R` will start tracking LD2 performance per cell. Worth a side note in the Obsidian Knowledge/Betting Optimisation log: lower divisions typically have wider uncertainty, so calibration may need a per-division split (K2 invariant currently splits only on `(league, sex, market)`). Defer until ≥30 settled bets per cell (K3 prior anchor).
- **Pre-season baseline (preseason field in `team_strengths.json`):** for newly-published divisions, the first nightly fit after release won't have an earlier same-season archived fit. The `preseason` field will be omitted (per `publish-layer.md`) until at least one prior fit accumulates. Frontend should already handle this gracefully (preseason is documented as optional), but worth eyeballing.
- **Male LD4 revisit:** if at some point Lengjan starts offering odds on 4. deild, or if a hierarchical kelly_frac pooling (Plan 7d in the Obsidian roadmap) absorbs the funnel-posterior shrinkage automatically, the training_filter exclusion of LD4 can be re-evaluated. Out of scope here.
- **Config-driven publish on basketball + handball:** the per-sex `publish_divisions` block introduced by Task 1.1 is the natural seam for the autumn 2026 basketball/handball cell split (currently only "karla"/"kvenna" — no division breakdown). When those sports' models extend to per-division output, the same pattern applies: add a `publish_divisions` block to the relevant league entries in `leagues.yml`, mirror in metill-platform's `DIVISIONS`. No new architecture required.
