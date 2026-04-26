# Per-sex `team_names` schema + post-Plan-6 small wins — design

Author: Brynjólfur (with Claude Code)
Date: 2026-04-26
Status: Approved for implementation

## 1. Purpose

Close the highest-leverage post-Plan-6 follow-up: redesign the `team_names`
config from a single sex-agnostic map to a per-sex map. The current shape
cannot represent Lengjan's `kv` suffix on women's teams (e.g. canonical
`Fram` → `Fram` for men, `Fram kv` for women), which silently blocks every
women's-league bet and also blocks every male-BD bet whose canonical name
happens to also exist on the women's side.

Bundle two mechanical post-migration sweeps in the same session: cron
verification (read-only) and a small CLAUDE.md drift fix.

## 2. Scope

### In scope

**Part 1 — small wins** (no design decisions, mechanical):

1. Verify the four CI workflows (`scrape-odds`, `scrape-results`,
   `fit-and-publish`, `pull-sports-data`) have run successfully at least
   once since Plan 6 cutover (2026-04-25). Spot-check resulting Parquet
   row counts and metill-platform JSON freshness. Report findings only —
   no code change.
2. Replace the stale "Skills" paragraph in `CLAUDE.md` (which still says
   the four `.claude/skills/*` "will be revised in a follow-up pass")
   with a single sentence noting the rewrite shipped in `f50b0bd` and is
   guarded by `tests/testthat/test-skill-conventions.R`.
3. Run `git worktree list` to confirm the audit's noted leftover worktree
   is gone (already removed per audit Resolution Round 2; sanity check
   only).

**Part 2 — per-sex `team_names` schema redesign**:

- New shape, hard cutover (no backwards-compat):
  ```yaml
  team_names:
    male:   { canonical: lengjan_display, ... }
    female: { canonical: lengjan_display, ... }
  ```
  Both sub-maps required; either may be `{}`.
- `config/leagues.yml` — rewrite all three leagues' `team_names` blocks.
  Concrete content in §4.
- `config/leagues.schema.json` — `team_names` requires `male` and `female`
  keys, each an object mapping string → string.
- `R/placer-validate.R::validate_team_names_config` — per-rec sex-keyed
  lookup; updated error messages; remove the now-superseded sex-aware hint
  paragraph added in `f0bcac0`.
- `R/placer-pipeline.R::pipeline_to_lengjan` (line 133) — change
  `tn <- league$lengjan$team_names` to use the rec's `sex`.
- `tests/testthat/test-placer-validate.R` — update existing fixtures to
  nested form, add new tests for per-sex behaviour.

### Out of scope

- Filling the `female` sub-map for `football_iceland` and `handball_iceland`
  (no historical Lengjan odds for women's football/handball; populate
  organically as the scraper picks up new fixtures).
- Stan parameter data-stories (in-flight by user; separate work).
- Lengjan SPA `loadEventFired` performance investigation.
- Phase 2+ items (non-Icelandic leagues, livesport revival, walk-forward
  backtester, betting-PnL harness port, Student-t v3 vs BVP).
- Manual one-time admin (`gh repo archive` of legacy GitHub repos).

## 3. Schema design

### 3.1 Shape choice — Option 1 (nested by sex)

Considered three options:

| | Shape | Decision |
| --- | --- | --- |
| 1 | `team_names: {male: {...}, female: {...}}` | **Chosen** |
| 2 | `team_names: {Fram: {male: ..., female: ...}}` per-team override | Rejected — mixed value types, asymmetric "what teams exist?" question |
| 3 | Flat `team_names_male` + `team_names_female` | Rejected — fragments schema |

Option 1 wins on:
- One value type throughout (sex → {canonical → Lengjan display})
- Symmetric "what teams exist for sex X?" — one sub-map lookup
- Schema validation is structural (each sex key maps to a string→string map)
- The lookup site reads naturally: `league$team_names[[rec$sex]]`

The empirical pattern (`Fram` vs `Fram kv` for women, applied uniformly by
Lengjan) means almost every value differs by sex anyway, so Option 2's
deduplication win is illusory.

### 3.2 Required-keys rule

Schema requires both `male` and `female` keys, even if empty (`{}`). This
makes the file self-documenting — readers see at a glance which sexes are
tracked. All three current leagues have `sexes: [male, female]`; future
sex-restricted leagues (none currently) can revisit this rule.

### 3.3 Empty sub-map semantics

An empty sub-map for sex X is a valid config state meaning "we haven't
mapped any X teams yet". The validator treats it as a fail-fast condition
when a rec for sex X arrives, with an error message pointing the reader at
how to populate it (e.g. `data/facts/odds`).

## 4. Concrete `config/leagues.yml` rewrites

### 4.1 `basketball_iceland`

```yaml
team_names:
  male: {}                         # No men's BD on Lengjan currently
  female:                          # Existing map, unchanged content
    Grindavík: Grindavík kv
    Valur: Valur kv
    Njarðvík: Njarðvík kv
    Haukar: Haukar kv
    Keflavík: Keflavík kv
    Tindastóll: UMF Tindastoll kv
    Ármann: Ármann kv
    Stjarnan: Stjarnan kv
```

### 4.2 `handball_iceland`

```yaml
team_names:
  male:                            # Existing map, unchanged content
    Þór: Þór Akureyri
    HK: Handknattleiksfélag Kópavogs
    ÍBV: ÍBV Vestmannaeyjar
    ÍR: ÍR Reykjavík
    FH: FH Hafnarfjörður
    KA: KA Akureyri
    ÍH: ÍH Keflavík
    HBH: HB Hafnarfjörður
  female: {}                       # No women's handball on Lengjan
```

### 4.3 `football_iceland`

```yaml
team_names:
  male:
    # Existing LD1 + men-only sex-disjoint entries (preserved verbatim):
    Þór: Þór Ak.
    Víkingur R.: Víkingur Rvk
    Þróttur R.: Þróttur Rvk
    Leiknir R.: Leiknir Rvk
    Ægir: Ægir
    ÍR: ÍR
    Grótta: Grótta
    Grindavík: Grindavík
    Njarðvík: Njarðvík
    Völsungur: Völsungur
    # NEW — BD teams that also appear in women's BD (previously
    # unrepresentable; verified against historical odds in
    # data/facts/odds/sport=football/country=iceland):
    KR: KR
    FH: FH
    Stjarnan: Stjarnan
    Valur: Valur
    Fram: Fram
    ÍBV: ÍBV
    Breiðablik: Breiðablik
    Keflavík: Keflavík
    ÍA: ÍA
    KA: KA
  female: {}                       # No historical women's football odds yet
```

The standalone "Note: team_names is currently shared across sexes within a
league…" comment block above the existing football map is removed — the
gap it documents is the gap this change closes.

## 5. Code changes

### 5.1 Validator — `R/placer-validate.R::validate_team_names_config`

Current: per-league lookup of `league$lengjan$team_names`, then check every
rec's home/away team is in the (sex-agnostic) map.

New: for each rec, look up `league$lengjan$team_names[[rec$sex]]` and check
home/away. Two distinct error conditions:

1. Sub-map for the rec's sex is empty `list()` → `team_names empty for
   {key} ({sex}); add canonical → Lengjan-display mappings under
   lengjan.team_names.{sex} (source: data/facts/odds or wait for next
   scrape).`
2. Sub-map non-empty but rec's home/away missing → `{key} ({sex}) is
   missing team_names for: {teams}.`

Drop the `f0bcac0` sex-aware hint paragraph (the
`project_team_names_schema` MEMORY pointer is now obsolete).

### 5.2 Pipeline — `R/placer-pipeline.R:133`

Inside the per-rec loop:

```r
# Before:
tn <- league$lengjan$team_names

# After:
tn <- league$lengjan$team_names[[rec$sex]]
```

Surrounding logic unchanged.

### 5.3 Schema — `config/leagues.schema.json`

Replace the current `team_names` definition:

```json
"team_names": {
  "type": "object",
  "additionalProperties": { "type": "string" }
}
```

With:

```json
"team_names": {
  "type": "object",
  "required": ["male", "female"],
  "additionalProperties": false,
  "properties": {
    "male":   { "type": "object", "additionalProperties": { "type": "string" } },
    "female": { "type": "object", "additionalProperties": { "type": "string" } }
  }
}
```

## 6. Tests

`tests/testthat/test-placer-validate.R` rewrites:

- Existing fixtures (`lengjan = list(team_names = list("KR" = "KR Reykjavik"))`)
  → nested (`lengjan = list(team_names = list(male = list("KR" = "KR Reykjavik"), female = list()))`).
- Existing 6 test cases retained: fixture format migrated to nested form,
  assertions kept where the contract is unchanged, error-message expectations
  updated where the validator wording changes (per §5.1).
- **New**: `validate_team_names_config errors when the rec's sex sub-map is empty`.
- **New**: `validate_team_names_config does not satisfy a male rec from the female sub-map (or vice versa)`.
- **New**: `validate_team_names_config errors when team_names lacks the rec's sex key entirely` (defence-in-depth against schema bypass).
- **New**: `validate_team_names_config errors with a clear message when rec$sex is NA or an unknown value` (lookup falls through to NULL; fail loud, don't silently accept).

## 7. Migration strategy

Hard cutover. Single commit changes:

1. `config/leagues.schema.json` (schema)
2. `config/leagues.yml` (3 league `team_names` blocks)
3. `R/placer-validate.R` (validator + error messages)
4. `R/placer-pipeline.R` (pipeline lookup)
5. `tests/testthat/test-placer-validate.R` (fixtures + new tests)

No backwards-compat shim. The 1,870 ledger rows from past placer runs are
unaffected (ledger does not store or reference `team_names`).

## 8. Verification gate

- `Rscript -e 'devtools::test()'` — all green (existing 528 PASS + new
  per-sex tests).
- Smoke: `Rscript scripts/preview_bets.R` against the current
  three male BD recs (Fram-ÍBV, KR-FH, Stjarnan-Valur). Pre-change
  the validator rejects all three (KR/FH/Stjarnan/Valur/Fram/ÍBV not in
  the male map); post-change it should pass `validate_team_names_config()`
  and proceed to the actual preview output.
- `Rscript -e 'jsonlite::fromJSON("config/leagues.schema.json")'` and a
  schema-validate pass over `config/leagues.yml` (existing test in
  `test-config.R`).

## 9. Risks

- **Lookup-key drift between rec and config** — if `rec$sex` ever takes a
  value other than `"male"` / `"female"` (e.g. `"all"`, `NA`), the lookup
  `team_names[[rec$sex]]` silently returns NULL. Mitigation: validator
  explicitly rejects any `rec$sex` not in `c("male", "female")` with a
  loud error before attempting the lookup (covered by the new test in §6).
- **Empty `female: {}` for football blocks women's BD bets** — this is the
  intended state. The Besta deild kvenna recommendations (when generated
  from the women's-side fit) will fail-fast with a clear message until the
  female sub-map is populated, which is fine because we're not betting it
  yet.
- **Schema test coverage** — existing `test-config.R` validates `leagues.yml`
  against the schema; the new schema is stricter, so any unrelated schema
  drift surfaces immediately.

## 10. Out-of-scope follow-ups (kept on the punch-list)

- Per-rec `sex` validation in `R/decide-pipeline.R` (separate PR; defence
  against `NA` sex in recs).
- Documenting the per-sex schema convention in
  `Knowledge/Lengjan Pipeline/_MOC.md` (Obsidian doc sweep, separate from
  code).
