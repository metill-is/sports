# Lengjan League Discovery & Frictionless Wiring — Design

- **Date:** 2026-06-22
- **Status:** Design approved; spec under review
- **Author:** Brynjólfur Gauti Jónsson (with Claude Code)
- **Scope owner:** `sports` monorepo
- **Related:** `.claude/skills/add-league/`, `.claude/rules/ci-conventions.md`,
  `.claude/rules/sports-betting.md`, `R/ingest-lengjan-odds.R`, `R/health.R`

## 1. Problem & motivation

The biggest available upgrade to coverage is **betting more of the leagues
Lengjan already prices**. Today the pipeline only ever requests odds for the
competition IDs hand-listed in `config/leagues.yml::*.lengjan.competitions`.
There is **no mechanism to discover** that Lengjan has started offering a
competition we are not yet scraping — every comp ID in config (746, 757, 4670,
…) was found manually, and gaps are caught late (the women's Lengjudeild odds
existed before anyone noticed and wired comp 4670 on 2026-06-21).

### 1.1 The double-check (empirically verified 2026-06-22)

The user's intuition — "we can't add configs unless Lengjan is actually showing
odds for those leagues" — is **essentially correct, but the mechanism is a
*discovery* gate, not a *config* gate**:

1. **Adding a config entry is not gated on live odds.** `ingest_lengjan_odds()`
   loops `lengjan.competitions`, builds
   `https://games.lotto.is/getraunaleikir/lengjan?sport=N&country=IS&competition=ID`,
   and parses whatever is shown. An empty competition page returns 0 rows with
   **no error** (`parse_competition_page()` returns an empty tibble). This is the
   normal off-season state. A competition entry, once added, is therefore
   **additive and permanent** — it goes quiet between rounds and self-resumes.

2. **The real gate is discovering the competition ID.** Those IDs are
   Lengjan-internal and there is no discovery code anywhere. They are
   discoverable from Lengjan's league dropdown ("Veldu deild"), whose options
   carry the ID as the `value` attribute. Verified live:

   ```
   <select> #3 (Veldu deild):
     <option value="746">Besta deild karla</option>
     <option value="1568">Besta deild kvenna</option>
   ```

   (746 and 1568 match `config/leagues.yml` exactly.) The sport dropdown
   (`value`: 1=Knattspyrna/football, 2=Körfubolti/basketball, 40=Formúla 1,
   101=MMA, 102=Hnefaleikar) and country dropdown (IS=Ísland, plus SE, NO, BR,
   AR, …, HM=World Cup) sit alongside it.

3. **The catch that proves the intuition:** at probe time the football+Iceland
   league dropdown listed **only Besta deild karla + kvenna** — not Lengjudeild,
   2. deild, the cup, or women's LD, *even though we have all those IDs in
   config*. So **the dropdown lists only competitions Lengjan is currently
   offering odds for.** You cannot *discover* a new ID until Lengjan lists it
   (which coincides with odds becoming available); once discovered, you keep it
   in config forever.

**Conclusion:** the fix is not "add more configs" — it is **a discovery step
that watches the dropdowns and surfaces, the moment Lengjan lists a competition
we model but do not scrape, the ID + name + a drafted team-name map** so wiring
is a quick review instead of a manual hunt.

## 2. Goals & non-goals

### Goals
- Detect, automatically and proactively, when Lengjan begins offering a
  competition that maps to a division we **already model** and is **not yet in
  config**.
- Emit a **reviewable, machine-actionable proposal** (drafted `leagues.yml`
  competition entry + best-guess `team_names` from a live scrape) that a future
  Claude Code session can finalise with minimal friction.
- Surface new findings through the **existing health-check spine** (a WARN row in
  `data/health/status.json`, the SessionStart banner, `/pipeline-doctor`).
- Run on a **CI schedule** with no credentials (the dropdown is public data),
  staying read-only and off the placer path.

### Non-goals (YAGNI)
- **No new models.** Competitions that do not map to an already-modelled
  division (foreign football, F1, MMA, …) are counted for awareness only, never
  proposed for wiring. Standing up new modelled leagues remains the `/add-league`
  path (Tier B), out of scope here.
- **No automatic config commits.** The system proposes; a human/Claude reviews
  and merges. (Explicitly rejected the "fully automatic" option — money is
  downstream.)
- **No hidden-API reverse-engineering** in v1 (see §11 future work).
- **No change to the odds scraper, decide, settle, publish, or placer logic** —
  they already handle any configured competition uniformly (division is
  recovered from `results`; the model already predicts modelled divisions).

## 3. Decisions log (from brainstorming)

| Decision | Choice | Rationale |
|---|---|---|
| Scope | Auto-discovery **+** frictionless wiring for already-modelled leagues | Highest leverage, bounded; odds are the only missing piece for these |
| Automation | **Propose a reviewable patch** Claude can act on | Safe + low-friction; rejects fully-automatic (mis-guessed names, surprise bets) |
| Trigger | **CI-scheduled + health WARN** | Reuses existing chromote/health/email infrastructure; hands-off until alerted |
| Alert severity | **WARN** (default) | A newly-listed league is not an outage. Email fires on FAIL only; WARN shows in status.json/banner/doctor. Escalation to FAIL is a one-line change if desired later. |
| Cadence | **Daily** | Competitions appear at most a few times per season; daily is ample |
| Wiring skill | **New `/wire-league` skill** | `/add-league` is fork/high-effort for whole new leagues (Stan model etc.); wiring an already-modelled comp is a smaller, distinct reaction flow |

## 4. Architecture & data flow

```
CI (daily)  .github/workflows/discover-leagues.yml
  └─ Rscript scripts/0N_discover.R
       for each ACTIVE modelled (sport, country) in leagues.yml:
          lengjan_list_competitions(sport, country, session)     [NEW scrape primitive]
             → tibble {sport, country, comp_id, lengjan_name}
          diff vs configured comp IDs                            → new comps
          classify_competition(name, sport, country)             → {sex, division, confidence}
          for each new + modelled comp:
             scrape one competition page → team renderings
             propose_team_names(...) fuzzy-map → canonical guesses + confidence
       write_discovery_proposal(...)
          → data/discovery/proposals.json  (machine-readable)
          → data/discovery/SUMMARY.md      (human summary)
       commit if changed (path: data/discovery/** only)

pipeline_health():
  check_discovery() reads proposals.json → WARN if un-actioned entries exist
     → data/health/status.json + SessionStart banner + /pipeline-doctor

React (human / Claude Code):
  /wire-league  reads proposals.json
     → drafts config (competition entry) + team_names additions
     → verifies via a one-comp scrape + decide_league (dry)
     → presents diff for review/merge
```

## 5. Components

Each unit has one purpose, a defined interface, and explicit dependencies.

### 5.1 `R/discover-lengjan.R` (new)

| Function | Signature → returns | Notes |
|---|---|---|
| `parse_competition_dropdown(html)` | parsed HTML → tibble `{comp_id, lengjan_name}` | **Pure**, fixture-tested. Reads the "Veldu deild" `<select>` `<option value>`/text pairs. Mirrors `parse_competition_page()` structure. |
| `lengjan_list_competitions(sport, country, session)` | → tibble `{sport, country, comp_id, lengjan_name}` | Fetches the parent page (`?sport=&country=`, no `competition=`) via the same navigate+settle pattern as `.lengjan_fetch`; calls the pure parser. |
| `classify_competition(lengjan_name, sport, country)` | → `{sex, division, confidence}` | Name → division/sex via match against `publish_divisions` `label_is` + patterns ("kvenna"/" kv"→female; "Lengjudeild"→LD1; "2. deild"→LD2; "3. deild"→LD3; "Mjólkurbikar"/"bikar"→CUP; "Besta deild"→BD). Unmatched → `confidence="low"`, `division=NA`. |
| `discover_new_competitions(leagues, session, root)` | → tibble of new comps with classification + `modelled` flag | Diffs live dropdown vs configured comp IDs per active league; joins classification; `modelled = division maps to a fitted cell`. |
| `propose_team_names(comp_id, sport, country, sex, session, known_teams)` | → tibble `{lengjan, canonical_guess, confidence}` | Scrapes one comp page for renderings; normalised fuzzy-match (strip accents, trailing " kv", "Rvk"/"R." abbreviations) against `known_teams` (from `results`/`beliefs_latest` for that division). |
| `write_discovery_proposal(findings, root)` | writes JSON + MD; returns path | Machine-readable `proposals.json` + `SUMMARY.md`. JSON written via the project's UTF-8-safe path (Icelandic names). |

**Dependencies:** `chromote`, `rvest`, `config.R` (`load_leagues`, `tn_renderings`), `storage.R` (`read_table` for known teams). No ledger, no placer, no login.

### 5.2 `scripts/0N_discover.R` (new)

Thin entry point per the `0N_*.R` convention: set locale → `devtools::load_all()`
→ `load_leagues()` → open a chromote session → `discover_new_competitions()` →
`write_discovery_proposal()` → close session. Soft-fails a Lengjan timeout
(warn, keep last proposal, exit 0) using the `.lengjan_fetch` retry contract.

### 5.3 `.github/workflows/discover-leagues.yml` (new)

- **Trigger:** `schedule` (daily, a fixed UTC time not colliding with existing
  workflows) + `workflow_dispatch`.
- **Steps:** checkout → R + chromote setup (reuse `scrape-odds.yml`'s system-deps
  pattern, incl. `PKG_SYSREQS: "false"` and the chromote/Chrome install) →
  `Rscript scripts/0N_discover.R` → commit `data/discovery/**` if changed.
- **Disjoint write path** (`data/discovery/**` only) → no cross-workflow git race
  (per `.claude/rules/ci-conventions.md`).
- **No `LENGJAN_*` secrets**; the workflow references no placer tokens, so
  `test-placer-ci-isolation.R` stays green. A new isolation test (5.6) also
  asserts discovery itself is placer-free.

### 5.4 `R/health.R::check_discovery()` (new, wired into `pipeline_health()`)

Reads `data/discovery/proposals.json`. Returns a `{check, scope, status, value,
threshold}` row:
- `OK` — no un-actioned proposals (file absent or empty `competitions`).
- `WARN` — N un-actioned proposed competitions that map to a modelled division
  (value = comma-joined `sport/sex/division (id)`).
- Never `FAIL` by default (a new league is not an outage). Documented named
  constant marks the escalation point if the user later wants an email.

"Un-actioned" = a proposed `comp_id` not yet present in
`config/leagues.yml::*.lengjan.competitions`. This makes the WARN **self-clear**
the moment the comp is wired — no manual state to reset.

### 5.5 `/wire-league` skill (new, `.claude/skills/wire-league/`)

Model-invocable reaction flow:
1. Read `data/discovery/proposals.json`.
2. For a chosen proposal: draft the `leagues.yml` `competitions` entry + the
   `team_names[[sex]]` additions (confirmed guesses inline; low-confidence ones
   flagged for verification, fail-safe).
3. Verify: run a one-comp scrape + `decide_league(..., write = FALSE)` to confirm
   the names join and candidates are produced.
4. Present the diff for review; the human merges.

Complements `/add-league` (which remains the Tier-B path for whole new leagues).
A `tests/testthat/test-skill-conventions.R`-style guard keeps it from drifting to
legacy invocations.

### 5.6 Tests (`tests/testthat/test-discover-lengjan.R`, new)

- `parse_competition_dropdown()` on a **saved real parent-page HTML fixture**
  (captured during implementation) → expected `{comp_id, lengjan_name}`.
- `classify_competition()` table: each known Icelandic comp name → correct
  `{sex, division}`; an unknown name → `confidence="low"`.
- `propose_team_names()` fuzzy-match on a fixture (e.g. "Víkingur Ól." → "Víkingur
  Ó.", "FH kv" → "FH"), including a deliberately-unmatchable rendering.
- `write_discovery_proposal()` round-trip (write → re-read → schema holds, UTF-8
  intact).
- `check_discovery()` with a seeded `proposals.json` (un-actioned → WARN;
  all-wired → OK).
- **CI-isolation:** assert `R/discover-lengjan.R` + `scripts/0N_discover.R` +
  `discover-leagues.yml` reference no ledger write, no placer symbol, no
  `LENGJAN_*`.

## 6. Data contract — `data/discovery/proposals.json`

```json
{
  "generated_at": "2026-06-22T06:00:00Z",
  "competitions": [
    {
      "sport": "football",
      "country": "iceland",
      "comp_id": "757",
      "lengjan_name": "Lengjudeildin",
      "inferred_sex": "male",
      "inferred_division": "LD1",
      "classify_confidence": "high",
      "modelled": true,
      "status": "new",
      "proposed_team_names": [
        { "lengjan": "Víkingur Ól.", "canonical_guess": "Víkingur Ó.", "confidence": "high" },
        { "lengjan": "Þróttur Vogum", "canonical_guess": "Þróttur V.", "confidence": "medium" }
      ]
    }
  ],
  "unmodelled_offered_count": 37
}
```

- `data/discovery/` is **git-tracked** (committed by the workflow so the health
  check and `/wire-league` can read the latest proposal). Small JSON + MD only.
- `status` ∈ `{new}` for v1 (a comp absent from config). `modelled=false`
  entries may be listed for visibility but are never proposed for wiring.
- `unmodelled_offered_count` is the Tier-B awareness signal (count only).

## 7. Classification & team-name matching

- **Division/sex classification** is a deterministic name match against
  `publish_divisions[*].label_is` plus a small pattern table; it is advisory
  (`classify_confidence`), never authoritative — a human confirms at wiring time.
- **Team-name matching** normalises both sides (NFC, strip diacritics for
  comparison only, drop trailing " kv", expand/normalise "Rvk"/"R."/"Ó."/"Ól."
  variants) and matches Lengjan renderings to the division's `known_teams`.
  Matches are emitted as **guesses with confidence**; the canonical column always
  uses the real canonical string (not the stripped form). A wrong guess is
  fail-safe: `normalise_lengjan_team_names()` + `decide-pipeline.R` warn-skip an
  unmatched match rather than mis-bet (existing invariant).

## 8. Safety invariants

1. **Read-only & CI-safe by construction.** Discovery never reads/writes the
   ledger, never logs in (the dropdown is public), never imports a placer symbol.
   Enforced by the new isolation test (5.6) alongside the existing
   `test-placer-ci-isolation.R`.
2. **Fail-safe wiring.** Team-name proposals are guesses; the decide layer
   warn-skips an unmatched rendering — never a mis-placed bet.
3. **Additive/permanent config.** A wired comp stays in config forever; dormant
   between rounds = 0 rows, no error.
4. **No new models.** Only comps mapping to a fitted division are proposed for
   wiring; others are counted, not detailed.
5. **Self-clearing alert.** The WARN keys off "comp_id not in config", so it
   disappears automatically once the comp is wired — no manual reset.

## 9. Edge cases & error handling

| Case | Behaviour |
|---|---|
| Lengjan fetch timeout | Reuse `.lengjan_fetch` retry/soft-fail; warn, keep the last `proposals.json`, exit 0 (no red-X). |
| Empty dropdown for a sport (off-season, e.g. handball not listed today) | No comps → no findings. Normal. |
| Comp name unclassifiable | `modelled=false`, listed under "needs human", not auto-proposed. |
| Playoff/variant already in config (e.g. "Besta Deildin Efri Hluti" 20443) | Not new (ID present) → ignored. |
| New playoff ID | Flagged as `new`; classification confidence depends on name match. |
| `<select>` / option DOM drift | Fixture-tested parser fails loudly (analogous to the odds-parser disagreement guard), surfacing as a build/run failure rather than silent empty output. |
| All proposed comps already wired | `check_discovery()` → OK; WARN self-clears. |

## 10. Testing strategy

- **Pure parsers on saved HTML fixtures** — capture one real parent-page HTML
  during implementation; never hit the network in tests.
- **Tables for classification + fuzzy-match** — deterministic, fast.
- **Round-trip for the proposal writer** — schema + UTF-8 integrity.
- **Seeded `proposals.json` for `check_discovery()`** — WARN vs OK.
- **Isolation test** — no ledger/placer/`LENGJAN_*` tokens in the new files.
- Follow `.claude/rules/r-conventions.md` (base pipe, `box::use` rules) and the
  testthat-3 conventions; run `devtools::test()` before claiming done.

## 11. Future work (out of scope)

- **Hidden-API discovery** — replace DOM-dropdown scraping with the backend
  endpoint that populates the selectors (no chromote). Optimisation, not v1.
- **Tier B (new modelled leagues)** — foreign football etc. via `/add-league`
  (ingest source + Stan model per league).
- **FAIL-severity escalation / email** — flip `check_discovery()` to FAIL if the
  user later wants an email the day a new modelled league appears.
- **Auto-PR** — have the workflow open a draft PR with the drafted `leagues.yml`
  diff instead of (or in addition to) the committed proposal artifact.

## 12. Success criteria

- A scheduled CI run lists Lengjan's currently-offered competitions for each
  modelled `(sport, country)`, diffs against config, and writes
  `data/discovery/proposals.json`.
- When Lengjan lists a modelled competition we do not yet scrape, the next
  health snapshot shows a `discovery` WARN naming it, and `proposals.json`
  carries a drafted competition entry + team-name guesses.
- `/wire-league` turns a proposal into a reviewed `leagues.yml` edit that passes
  a dry `decide_league`, and the WARN self-clears on merge.
- All new code is read-only on the money path, CI-safe, and covered by
  fixture-based tests.
