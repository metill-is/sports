# Plan A — Ingest and Safety Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make current-season basketball and handball data flow into `data/facts/` automatically, with no possibility of a bet being placed, and with the 2DT home-advantage units bug fixed before it can ever publish.

**Architecture:** Six workstreams. The betting interlock lands first because the autoplace agent is armed. A test-and-fixture harness lands second because later plans delete ~740 lines of live publisher and the existing coverage has never executed. Then the units bug, then both federation season resolvers (registry demoted to a verified cache, ids discovered live, a season-stamp guard that aborts on disagreement), then deletion of the self-locking ingest activation gate.

**Tech Stack:** R package (`devtools`, `testthat` ed. 3, `roxygen2`), `arrow`/Parquet, `chromote` + `rvest::read_html_live` for JS-rendered federation pages, `cmdstanr` fits (read-only here).

**Spec:** [`docs/superpowers/specs/2026-09-02-basketball-handball-metill-parity-design.md`](../specs/2026-09-02-basketball-handball-metill-parity-design.md)

## Global Constraints

- Branch: `feat/bb-hb-metill-parity`. **Commit only — do NOT `git push`.** Spec section 5 requires WS4+WS5+WS6 to ship in one PR.
- Five cron workflows commit to `main` all day, touching only `data/` paths. Sync with stash -> pull --rebase -> pop (see `.claude/rules/git-hygiene.md`); always `git -C /Users/brynjolfurjonsson/sports`.
- `devtools::load_all()` / `devtools::test()` are the drivers. Roxygen docs update in the SAME commit as the code.
- British/international spelling in prose. Icelandic only at the publish boundary.
- Any "upcoming match" test fixture MUST use `Sys.Date() + N` or `2100-01-01` — never a hardcoded near date; real-clock filters ignore an injected `now` and near dates rot into failing tests.
- Never write an unquoted `echo ===` in zsh: `EQUALS` expansion aborts the rest of the command **silently**, so the output you get is truncated rather than complete. Quote it.
- Run R from the repo root so `here::here()` resolves correctly.
- **D2 is absolute: no bet may be placed on basketball or handball.** If any step would arm placement, stop.

---

## Integration decisions

**These override the per-workstream drafts wherever they disagree.** Each was a real conflict found by an adversarial crosscheck of the drafts; each is resolved here once so the seams are consistent.

### ID-1. `stub_fit()` has exactly one contract, and WS2 owns it

```r
# tests/testthat/helper-stub-fit.R
stub_fit(draws_list)   # named list: Stan variable name -> draws_array / matrix
                       # returns list(draws = function(variables) ...)
                       # NO fake class. MUST include lp__.
```

WS3 consumes this exact form. **The stub must not claim class `CmdStanMCMC`.** Verified by grep across all six 2DT files: the only `fit$` usage anywhere on the extract/publish path is `fit$draws` (6 occurrences); every `CmdStanMCMC` mention is a roxygen `@param`, and the only `inherits()` call is on `fit_date`. A fake R6 class would be a lie that hides a real dispatch dependency if one is ever added. `lp__` is required because `publish_{basketball,handball}_iceland` call `posterior::ndraws(fit$draws("lp__"))`.

### ID-2. `.assert_season_stamp()` has one definition, one location, one message

```r
# R/ingest.R, after .is_league_active()
.assert_season_stamp(rows, season, source = "unknown", tol = 0.05)
```

WS4 defines it; WS5 **consumes it as-is** and passes `source =`, not `context =`. Do **not** create `R/ingest-season-guard.R`. Tests asserting the abort must match WS4's exact message text (`expect_error()` regexes are case-sensitive).

### ID-3. The KKÍ `stage` column is DEFERRED out of Plan A

**Do not execute WS5 Task 9.** Adding `stage` to `schemas()$results` is a schema migration, not an ingest fix: `validate_against_schema()` (`R/storage.R:67-81`) hard-fails on any missing schema column, so it breaks ~14 existing writers (`R/wc-ingest.R:365`, `scripts/0Nm_backfill_round.R:71`, and a dozen test fixtures) plus the exact-set assertion at `tests/testthat/test-ingest-kki.R:20-27` and WS2's own committed facts fixture.

The stage ids are recorded in the spec (finding N7) and the work moves to Plan B, where the publisher that consumes stage is built in the same change. Plan A's regular-season boundary therefore uses the **round-derivation** rule, which the data independently confirms.

### ID-4. Never mock `Sys.sleep` — use the injection seam that already exists

`testthat::local_mocked_bindings()` aborts with "Can't find binding" for base functions. This codebase already solves it by dependency injection: `poll_hsi_tables(..., sleep_fn = Sys.sleep)` (`R/ingest-hsi-handball.R:182-186`). Extend that seam to `fetch_results_hsi` / `fetch_schedule_hsi` rather than mocking. (`HSI_HISTORICAL_SLEEP_SECS = 3`, `:531`.)

### ID-5. The `config/leagues.yml` replacement ranges are 24–32 and 88–91

WS1 Task 3 says 24–31 and 88–90, which stop one line short of the existing `team_names:` keys at lines 32 and 91 — producing **two** `team_names:` keys under `lengjan:`. `yaml::yaml.load()` keeps the last, silently discarding the team-name maps that the same workstream's tests then assert on.

### ID-6. The machine-local publish tests are DELETED, not repaired

Spec section 4 is explicit. WS2 Tasks 6 and 7 rewrite `test-publish-{basketball,handball}.R` against `publish_*(fit, ...)` — the exact signature Plan B deletes — so the work would be created and destroyed inside one PR chain. **Delete the 8 skip-gated blocks; keep the argument-validation blocks that already run.** Coverage for the publish path comes from WS2 Task 9's football golden file and, in Plan B, from the extracts-based tests.

### ID-7. `digest` must be added to `DESCRIPTION` Suggests

Verified absent from both Imports and Suggests. WS2 Tasks 4, 5 and 9 call `digest::digest()`. Add it in the same commit as its first use — not as a conditional diagnostic step.

### ID-8. `--league` bypasses the off-season backoff, as spec section 7 requires

WS6 wires only `--force`. A user running `Rscript scripts/01_ingest_results.R --league handball_iceland` on a dormant cell must not get a silent skip: an explicit single-league request is itself an override.

### Corrections folded into the workstreams below

- WS3 Task 3's source guard forbids the literal strings `exp(` and `transform`, both of which appear in the comment WS3 Task 2 itself writes. Scope the guard to the function body, or assert on the parsed call rather than the file text.
- WS6 Task 2's backoff test asserts a reset at `t0+29h` that its own 24h interval prevents. Use `run(53)` or a `force = TRUE` run.
- WS1 Task 4's commit message claims `decide_write_empty()` writes empty partitions. It does not: `write_table()` returns `invisible(NULL)` for a zero-row frame (`R/storage.R:155-157`). The test passes for the wrong reason — fix the message, and assert the real behaviour.
- WS1 must declare `tests/testthat/test-placer-load.R` in its modified-files list: `drop_betting_disabled()` flips `:23-30` from `nrow(out) == 1L` to `0L`. Write that edit out; do not leave it conditional.
- WS2's skip-hygiene grep set must name WS3's actual file, `test-extract-2dt-home-advantage-units.R`. The drafts disagree on this filename, and `file.exists()` filtering would silently skip it — leaving B5's own coverage unguarded, the exact failure the test exists to prevent.

---

## Execution order

`WS1 -> WS2 -> WS3 -> WS4 -> WS5 -> WS6`. WS2 produces `stub_fit()` for WS3; WS4 produces `.assert_season_stamp()` for WS5; WS6 must not land before WS1 (it repopulates schedules, which is what arms placement).

---


# WS1 — Betting interlock: schema key + config + runtime flag (spec §3, decision D2)

**Produces (later workstreams rely on these):**

- `betting_enabled(league) -> logical(1)  # exported, R/config.R. TRUE unless league$betting$enabled is exactly FALSE. Absent betting block or absent key = TRUE.`
- `validate_betting_enabled(leagues, recs) -> invisible(TRUE)  # exported, R/placer-validate.R. stop()s naming every betting-disabled league key present in recs.`
- `drop_betting_disabled(recs, leagues = NULL) -> tibble  # @noRd, R/placer-load.R. Drops recs rows whose paste0(sport, '_', country) is a betting-disabled league key; logs the count and the flag name.`
- `ingest_one_lengjan(static, lengjan, key, active_path, betting = NULL) -> integer(1)  # exported, R/ingest.R. New 5th arg; NULL = enabled, so the 4-arg positional calls in tests/testthat/test-ingest-lengjan-odds.R keep working.`
- `check_capture_rate(root, now, th, leagues = list()) -> health tibble  # @noRd, R/health.R. New 4th arg, defaulted so existing call sites keep working.`
- `config/leagues.schema.json: betting.properties.enabled (boolean, NOT in required) — later workstreams adding betting keys must not remove it.`
- `config/leagues.yml invariant: basketball_iceland.betting.enabled == FALSE, handball_iceland.betting.enabled == FALSE, football_iceland.betting has no `enabled` key. WS7 (publish_divisions) edits the same two league blocks — do not resurrect lengjan.competitions there.`

## Workstream 1 — Betting interlock (spec §3, decision D2)

**Why this lands first.** `is.metill.sports.autoplace` is loaded in launchd and there is no `data/AUTO_PLACE_DISABLED` kill switch. Both leagues are `active: true` with live Lengjan ids (bb 1519/1528, hb 1269). The moment a later workstream repopulates `data/facts/schedules/`, `ingest_one_lengjan()` scrapes odds, `04_decide.R` writes recommendations, and `run_auto_place()` — which places *all* recommendations by design — stakes real money on handball. D2 is publish-only.

**Where the runtime checks go, and why there.**

| layer | choke point | why not elsewhere |
|---|---|---|
| decide | `decide_league()` (R/decide-pipeline.R:39) | Every decide caller funnels through it: `decide_one()` → `scripts/04_decide.R`, plus `R/backtest-walkforward.R:261,350,392` and the replay script. Guarding `decide_one()` instead would leave the backtest and any library caller armed; guarding `scripts/04_decide.R` would leave the whole R API armed. |
| placer | `load_recommendations()` (R/placer-load.R:19) | The single funnel for the placer: `place_bets()` calls it directly (R/placer-pipeline.R:71) and `preview_pending()` calls it (R/placer-preview.R:20), and `run_auto_place()` reaches it only via `preview_pending()` (R/auto-place.R:199), so a disabled league cannot be previewed, staked, or wake the unattended placer. It **filters** rather than aborts here on purpose: a hard `stop()` on a stray handball row would block *football* placement too, since `place_bets()` is all-league. |
| placer, last line | `validate_betting_enabled()` in `place_bets()` step 4 | Aborts loudly before `chromote_login()`, alongside the existing pre-flight validators, for rows that reached `recs` by any other route. An abort here cannot half-place a slip. |
| ingest | `ingest_one_lengjan()` (R/ingest.R:159) | Belt-and-braces: zero odds rows in means zero candidates out even if a future edit re-adds the competition ids. |

All four consult one predicate, `betting_enabled()`, so a league can never be disabled in one layer and armed in another.

---

### Task 1: Schema — accept `betting.enabled`

**Files:**
- Modify: `/Users/brynjolfurjonsson/sports/config/leagues.schema.json` (insert into `betting.properties`, currently lines 81-82)
- Test: `/Users/brynjolfurjonsson/sports/tests/testthat/test-config-betting-schema.R` (append)

**Interfaces:**
- Consumes: `load_leagues(path, schema_path, validate)` (R/config.R:8), `validate_leagues()` (R/config.R:49)
- Produces: `config/leagues.schema.json` → `properties.betting.properties.enabled` (boolean, *not* in `required`)

- [ ] **Step 1: Write the failing schema-acceptance test.** Append to `tests/testthat/test-config-betting-schema.R`:

```r
test_that("schema accepts an optional betting.enabled flag", {
  # `betting` is additionalProperties:false, so this key must be declared in
  # the schema before config/leagues.yml can carry it (spec N2).
  tmp <- withr::local_tempfile(fileext = ".yml")
  writeLines(yaml::as.yaml(list(
    handball_iceland = list(
      sport = "handball", country = "iceland", sexes = list("male"),
      active = TRUE,
      data_source = list(
        results = "hsi_handball", schedule = "hsi_handball",
        odds = "lengjan_odds"
      ),
      stan_model = "handball_iceland/2d_student_t.stan",
      betting = list(
        enabled = FALSE,
        kelly_frac = 0.05, ev_threshold = 0,
        markets = list(moneyline = TRUE, spread = TRUE, total = TRUE),
        scoring = list(has_ties = TRUE, tie_threshold = 0.5),
        min_bet = 200
      )
    )
  )), tmp)

  expect_no_error(load_leagues(path = tmp))
})

test_that("betting.enabled is optional, not required", {
  # football_iceland carries no `enabled` key and must keep validating.
  tmp <- withr::local_tempfile(fileext = ".yml")
  writeLines(yaml::as.yaml(list(
    football_iceland = list(
      sport = "football", country = "iceland", sexes = list("male"),
      active = TRUE,
      data_source = list(
        results = "ksi_football", schedule = "ksi_football",
        odds = "lengjan_odds"
      ),
      stan_model = "football_iceland/bivariate_poisson.stan",
      betting = list(
        kelly_frac = 0.10, ev_threshold = 0,
        markets = list(moneyline = TRUE),
        scoring = list(has_ties = TRUE, tie_threshold = 0.5),
        min_bet = 200
      )
    )
  )), tmp)

  expect_no_error(load_leagues(path = tmp))
})

test_that("betting.enabled rejects a non-boolean", {
  tmp <- withr::local_tempfile(fileext = ".yml")
  writeLines(yaml::as.yaml(list(
    handball_iceland = list(
      sport = "handball", country = "iceland", sexes = list("male"),
      active = TRUE,
      data_source = list(
        results = "hsi_handball", schedule = "hsi_handball",
        odds = "lengjan_odds"
      ),
      stan_model = "handball_iceland/2d_student_t.stan",
      betting = list(
        enabled = "false", # string, not boolean
        kelly_frac = 0.05, ev_threshold = 0,
        markets = list(moneyline = TRUE),
        scoring = list(has_ties = TRUE, tie_threshold = 0.5),
        min_bet = 200
      )
    )
  )), tmp)

  expect_error(load_leagues(path = tmp), "enabled|boolean")
})
```

- [ ] **Step 2: Run it — expect RED on the first and third tests.**

```bash
(cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::test(filter = "config-betting-schema")')
```

Exact expected failure for *"schema accepts an optional betting.enabled flag"*: `Expected `load_leagues(path = tmp)` to run without any errors, but it threw an error.` with message `leagues.yml failed schema validation:` / `  /handball_iceland/betting: must NOT have additional properties`. *"betting.enabled rejects a non-boolean"* fails the same way for the wrong reason (additionalProperties, not type) — after Step 3 it must fail on `must be boolean` instead. *"betting.enabled is optional, not required"* passes already; it is the regression lock for football.

- [ ] **Step 3: Add the key to the schema.** In `config/leagues.schema.json`, inside `properties.betting.properties`, immediately before `"kelly_frac"` (line 82):

```json
            "enabled": {
              "type": "boolean",
              "description": "Absent = true. When false, no odds are scraped for this league, decide_league() writes zero candidates, and the placer refuses its recommendations. Publishing is unaffected. Not in `required` so leagues that never disable betting stay untouched."
            },
```

- [ ] **Step 4: Re-run — expect GREEN, then confirm the live config still loads.**

```bash
(cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::test(filter = "config-betting-schema")')
(cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::load_all(quiet = TRUE); str(names(load_leagues()))')
```

Expect `[ FAIL 0 | ... ]` and `chr [1:3] "basketball_iceland" "handball_iceland" "football_iceland"`.

- [ ] **Step 5: Commit.**

```bash
git -C /Users/brynjolfurjonsson/sports add config/leagues.schema.json tests/testthat/test-config-betting-schema.R
git -C /Users/brynjolfurjonsson/sports commit -m "config(schema): allow an optional betting.enabled flag

The betting object is additionalProperties:false, so an enabled key in
leagues.yml is rejected at load_leagues() and takes every pipeline script
down with it. The schema must accept the key before the config that uses
it lands (D2: publish-only for basketball and handball this season).

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 2: The `betting_enabled()` predicate

**Files:**
- Modify: `/Users/brynjolfurjonsson/sports/R/config.R` (append after `filter_leagues()`, which ends at line 215)
- Create: `/Users/brynjolfurjonsson/sports/tests/testthat/test-betting-interlock.R`
- Modify: `/Users/brynjolfurjonsson/sports/NAMESPACE`, `/Users/brynjolfurjonsson/sports/man/betting_enabled.Rd` (generated)

**Interfaces:**
- Consumes: nothing
- Produces: `betting_enabled(league) -> logical(1)`

- [ ] **Step 1: Write the failing predicate test.** Create `tests/testthat/test-betting-interlock.R`:

```r
# WS1 -- betting interlock (spec section 3, decision D2).
# One predicate, four enforcement points: ingest, decide, the placer's
# recommendation loader, and the placer's pre-flight validator.

test_that("betting_enabled() defaults to TRUE when the key is absent", {
  expect_true(betting_enabled(list(betting = list(kelly_frac = 0.1))))
})

test_that("betting_enabled() defaults to TRUE when there is no betting block", {
  expect_true(betting_enabled(list(sport = "football", country = "iceland")))
})

test_that("betting_enabled() is FALSE only for an explicit FALSE", {
  expect_false(betting_enabled(list(betting = list(enabled = FALSE))))
  expect_true(betting_enabled(list(betting = list(enabled = TRUE))))
})

test_that("betting_enabled() treats a NULL betting slice as enabled", {
  # decide_one() forwards `betting` verbatim; a NULL slice must not silently
  # disarm a league that meant to bet.
  expect_true(betting_enabled(list(betting = NULL)))
})
```

- [ ] **Step 2: Run it — expect RED.**

```bash
(cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::test(filter = "betting-interlock")')
```

Exact expected failure, four times: `Error in `betting_enabled(...)`: could not find function "betting_enabled"`.

- [ ] **Step 3: Add the predicate.** Append to `R/config.R`:

```r
#' Is betting enabled for a league?
#'
#' Reads `betting$enabled` from a league definition. The key is optional in
#' `config/leagues.schema.json`, so an absent key means enabled and only an
#' explicit `enabled: false` disarms a league. This is the single predicate
#' consulted by the odds-ingest guard ([ingest_one_lengjan()]), the decide
#' guard ([decide_league()]), the placer's recommendation loader
#' ([load_recommendations()]) and the placer pre-flight
#' ([validate_betting_enabled()]) -- so a league can never be disabled in one
#' layer while another stays armed.
#'
#' @param league A league definition (an element of [load_leagues()]), or any
#'   list carrying a `betting` slice.
#' @return `TRUE` unless `betting$enabled` is exactly `FALSE`.
#' @export
betting_enabled <- function(league) {
  !isFALSE(league$betting$enabled)
}
```

- [ ] **Step 4: Document and run — expect GREEN.**

```bash
(cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::document()')
(cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::test(filter = "betting-interlock")')
```

Expect `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 4 ]` and `export(betting_enabled)` in NAMESPACE:

```bash
grep -n 'export(betting_enabled)' /Users/brynjolfurjonsson/sports/NAMESPACE
```

- [ ] **Step 5: Commit.**

```bash
git -C /Users/brynjolfurjonsson/sports add R/config.R NAMESPACE man/betting_enabled.Rd tests/testthat/test-betting-interlock.R
git -C /Users/brynjolfurjonsson/sports commit -m "feat(config): add betting_enabled() as the single interlock predicate

Four layers must agree on whether a league may bet (ingest, decide, the
placer's loader, the placer's pre-flight). Each reading betting\$enabled
itself invites the failure this guards against: one layer disabled while
another stays armed. Absent means enabled, so football is untouched.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 3: Disarm basketball and handball in `config/leagues.yml`

**Files:**
- Modify: `/Users/brynjolfurjonsson/sports/config/leagues.yml` (basketball lines 24-31 and 54; handball lines 88-90 and 104)
- Test: `/Users/brynjolfurjonsson/sports/tests/testthat/test-betting-interlock.R` (append)

**Interfaces:**
- Consumes: `betting_enabled()` (Task 2), `load_leagues()`
- Produces: config invariant — bb/hb `betting.enabled == FALSE` and empty `lengjan.competitions`; football has no `enabled` key

- [ ] **Step 1: Write the failing config assertion.** Append to `tests/testthat/test-betting-interlock.R`:

```r
test_that("shipped config disables betting for basketball and handball (D2)", {
  leagues <- load_leagues()
  expect_false(betting_enabled(leagues$basketball_iceland))
  expect_false(betting_enabled(leagues$handball_iceland))
})

test_that("shipped config leaves football betting armed (absent == enabled)", {
  leagues <- load_leagues()
  expect_null(leagues$football_iceland$betting$enabled)
  expect_true(betting_enabled(leagues$football_iceland))
})

test_that("disabled leagues carry no Lengjan competitions (belt and braces)", {
  # Zero competitions in means zero odds rows out, independently of the flag.
  leagues <- load_leagues()
  expect_length(leagues$basketball_iceland$lengjan$competitions, 0L)
  expect_length(leagues$handball_iceland$lengjan$competitions, 0L)
  expect_gt(length(leagues$football_iceland$lengjan$competitions), 0L)
})

test_that("disabled leagues keep their team_names maps for a later re-arm", {
  leagues <- load_leagues()
  expect_gt(length(leagues$handball_iceland$lengjan$team_names$male), 0L)
  expect_gt(length(leagues$basketball_iceland$lengjan$team_names$male), 0L)
})
```

- [ ] **Step 2: Run — expect RED.**

```bash
(cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::test(filter = "betting-interlock")')
```

Exact expected failures: `betting_enabled(leagues$basketball_iceland) is not FALSE` / `` `actual`: TRUE `` / `` `expected`: FALSE `` (twice), and `length(leagues$basketball_iceland$lengjan$competitions) not equal to 0. 1/1 mismatches` plus the same for handball (`1` vs `0`).

- [ ] **Step 3: Edit `config/leagues.yml` — basketball.** Replace lines 24-31 (the `lengjan:`/`competitions:` block for `basketball_iceland`) with:

```yaml
  lengjan:
    # 2026-09-02 (D2, docs/superpowers/specs/2026-09-02-basketball-handball-metill-parity-design.md):
    # PUBLISH ONLY this season -- no bets on basketball. Competition ids are
    # emptied as well as flagged so that zero odds rows are scraped even if a
    # code path forgets to consult betting.enabled. Restore verbatim when
    # re-arming:
    #   - { id: "1519", name: "Bónusdeild karla úrslitakeppni", sex: male }
    #   - { id: "1528", name: "Bónusdeild kvenna úrslitakeppni", sex: female }
    # (2026-04-28: Lengjan restructured kvenna efri/neðri (30774/30773) into a
    # single playoff umbrella per sex; the old IDs return empty placeholder
    # pages.)
    competitions: []
    team_names:
```

Then in the `basketball_iceland.betting:` block, insert as the first key under `betting:` (line 54):

```yaml
  betting:
    # 2026-09-02 (D2): publish only -- no bets on basketball this season.
    # Enforced at runtime by betting_enabled() in ingest, decide and the placer.
    enabled: false
```

- [ ] **Step 4: Edit `config/leagues.yml` — handball.** Replace lines 88-90:

```yaml
  lengjan:
    # 2026-09-02 (D2): PUBLISH ONLY this season -- no bets on handball. The
    # 2026-06-13 methodology verdict records handball as not Lengjan-bankable.
    # Restore verbatim when re-arming:
    #   - { id: "1269", name: "Olísdeild karla", sex: male }
    competitions: []
    team_names:
```

And insert as the first key under `handball_iceland.betting:` (line 104):

```yaml
  betting:
    # 2026-09-02 (D2): publish only -- no bets on handball this season.
    enabled: false
```

- [ ] **Step 5: Run — expect GREEN, and confirm nothing else in the suite depended on those competition ids.**

```bash
(cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::test(filter = "betting-interlock")')
(cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::test(filter = "config")')
(cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::load_all(quiet = TRUE); print(names(filter_leagues(load_leagues(), active_only = TRUE, has_lengjan = TRUE)))')
```

Expect `[ FAIL 0 ]` on both, and the last command prints `[1] "football_iceland"` — `scripts/02_scrape_odds.R` now never reaches the two disabled leagues at all. `tests/testthat/test-config.R:162` only asserts `length(active) > 0`, which football satisfies.

- [ ] **Step 6: Commit.**

```bash
git -C /Users/brynjolfurjonsson/sports add config/leagues.yml tests/testthat/test-betting-interlock.R
git -C /Users/brynjolfurjonsson/sports commit -m "config(leagues): disable betting on basketball + handball (D2)

The launchd autoplace agent is loaded with no kill-switch file, and both
leagues are active with live Lengjan ids. The moment a later workstream
repopulates data/facts/schedules the agent would stake real money on
handball, which the 2026-06-13 methodology verdict records as not
Lengjan-bankable. Flag plus emptied competitions: zero rows in as well as
a runtime refusal, so a single forgotten code path cannot re-arm it.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 4: Runtime guard in `decide_league()`

**Files:**
- Modify: `/Users/brynjolfurjonsson/sports/R/decide-pipeline.R` (insert after line 74, the close of the `empty_return()` closure, before `# 3. Read beliefs` at line 75)
- Test: `/Users/brynjolfurjonsson/sports/tests/testthat/test-betting-interlock.R` (append)

**Interfaces:**
- Consumes: `betting_enabled()` (Task 2), `decide_write_empty()` (R/decide-pipeline.R:351), `empty_candidates()`/`empty_recommendations()`
- Produces: `decide_league()` returns a zero-row tibble and writes only empty tables when `betting_enabled(league)` is FALSE

- [ ] **Step 1: Write the failing decide test.** Append to `tests/testthat/test-betting-interlock.R`:

```r
# Local fixtures: kept independent of test-decide-pipeline.R's helpers, which
# are not visible across test files.
interlock_decide_setup <- function(root, match_date = Sys.Date() + 1L) {
  set.seed(11)
  beliefs <- tibble::tibble(
    sport = "handball", country = "iceland", sex = "male",
    fit_date = Sys.Date(),
    match_date = match_date,
    home_team = "Valur", away_team = "FH",
    draw_id = 1:1000,
    home_goals = rpois(1000, lambda = 28),
    away_goals = rpois(1000, lambda = 26)
  )
  write_table(beliefs, "beliefs_latest", root = root)

  odds <- tibble::tibble(
    sport = "handball", country = "iceland",
    scraped_at = Sys.time(),
    match_date = match_date,
    home_team = "Valur", away_team = "FH",
    market = c("moneyline", "moneyline"),
    outcome = c("home", "away"),
    line = NA_real_,
    odds = c(1.80, 2.20)
  )
  write_table(odds, "odds", root = root)
}

interlock_league <- function(enabled = NULL) {
  betting <- list(
    kelly_frac = 0.10, ev_threshold = 0.0,
    markets = list(moneyline = TRUE, spread = TRUE, total = TRUE),
    scoring = list(has_ties = TRUE, tie_threshold = 0.5),
    min_bet = 200, max_age_hours = 999999L
  )
  if (!is.null(enabled)) betting$enabled <- enabled
  list(
    sport = "handball", country = "iceland", sexes = "male",
    active = TRUE, stan_model = "x.stan", betting = betting
  )
}

interlock_bankroll <- function() {
  list(
    initial_pool = 23610, current_pool = 23610,
    daily_budget_frac = 0.05, daily_budget_min_isk = 1000
  )
}

test_that("decide_league writes zero candidates for a betting-disabled league", {
  root <- withr::local_tempdir()
  interlock_decide_setup(root)

  out <- suppressMessages(decide_league(
    league = interlock_league(enabled = FALSE), sex = "male",
    root = root, bankroll = interlock_bankroll(),
    return_candidates = TRUE
  ))

  expect_equal(nrow(out), 0L)
  cands <- read_table("candidates",
    filter = list(sport = "handball", country = "iceland"), root = root
  )
  recs <- read_table("recommendations",
    filter = list(sport = "handball", country = "iceland"), root = root
  )
  expect_equal(nrow(cands), 0L)
  expect_equal(nrow(recs), 0L)
})

test_that("decide_league names the flag when it refuses a league", {
  root <- withr::local_tempdir()
  interlock_decide_setup(root)

  expect_message(
    decide_league(
      league = interlock_league(enabled = FALSE), sex = "male",
      root = root, bankroll = interlock_bankroll()
    ),
    "betting.enabled"
  )
})

test_that("decide_league still produces candidates when the flag is absent", {
  # Guards the football path: no `enabled` key must mean business as usual.
  root <- withr::local_tempdir()
  interlock_decide_setup(root)

  suppressMessages(decide_league(
    league = interlock_league(enabled = NULL), sex = "male",
    root = root, bankroll = interlock_bankroll()
  ))

  cands <- read_table("candidates",
    filter = list(sport = "handball", country = "iceland"), root = root
  )
  expect_gt(nrow(cands), 0L)
})
```

- [ ] **Step 2: Run — expect RED on the first two.**

```bash
(cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::test(filter = "betting-interlock")')
```

Exact expected failures: *"decide_league writes zero candidates…"* → `nrow(out) not equal to 0L.` / `1/1 mismatches` / `[1] 2 - 0 == 2` (both moneyline outcomes survive as candidates), then `nrow(cands) not equal to 0L`. *"decide_league names the flag…"* → `` `decide_league(...)` did not produce any messages. ``. The third test passes already and is the regression lock.

- [ ] **Step 3: Add the guard.** In `R/decide-pipeline.R`, insert immediately after the `empty_return()` closure closes (line 74) and before `# 3. Read beliefs`:

```r
  # 2b. Betting interlock ---------------------------------------------------
  # The decide-layer choke point. Every caller funnels through decide_league()
  # -- decide_one() from scripts/04_decide.R, the backtest harness
  # (R/backtest-walkforward.R) and the replay script -- so a league carrying
  # `betting.enabled: false` can never reach the recommendations table, and
  # therefore never reach the placer, which reads only that table. Guarding
  # decide_one() instead would leave every library caller armed.
  if (!betting_enabled(league)) {
    cli::cli_alert_warning(
      "decide_league: betting disabled by config (betting.enabled: false) for
       {league$sport}/{league$country}/{sex} — writing zero candidates."
    )
    if (write) decide_write_empty(league, sex, run_id, root)
    return(invisible(empty_return()))
  }

```

- [ ] **Step 4: Run — expect GREEN, and confirm the decide suite and backtest suite are unmoved.**

```bash
(cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::test(filter = "betting-interlock")')
(cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::test(filter = "decide")')
(cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::test(filter = "backtest")')
```

All three must report `[ FAIL 0 ]`.

- [ ] **Step 5: Commit.**

```bash
git -C /Users/brynjolfurjonsson/sports add R/decide-pipeline.R tests/testthat/test-betting-interlock.R
git -C /Users/brynjolfurjonsson/sports commit -m "feat(decide): refuse to produce candidates for a betting-disabled league

A config flag nobody reads is not a safety control. decide_league() is the
one funnel every decide caller passes through (decide_one, the backtest
harness, the replay script), so enforcing there means a disabled league
cannot reach the recommendations table -- and the placer reads nothing
else. Writes the empty tables so downstream freshness checks still see a
run rather than a gap.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 5: Placer refuses a disabled league

**Files:**
- Modify: `/Users/brynjolfurjonsson/sports/R/placer-load.R` (call inserted after line 53; helper appended after `dedup_against_ledger()`, which ends at line 104)
- Modify: `/Users/brynjolfurjonsson/sports/R/placer-validate.R` (append after `validate_recommendations_schema()`)
- Modify: `/Users/brynjolfurjonsson/sports/R/placer-pipeline.R` (insert after line 103)
- Test: `/Users/brynjolfurjonsson/sports/tests/testthat/test-betting-interlock.R` (append)
- Modify: `NAMESPACE`, `man/validate_betting_enabled.Rd` (generated)

**Interfaces:**
- Consumes: `betting_enabled()` (Task 2), the shipped config from Task 3, `load_recommendations()`, `validate_team_names_config()`
- Produces: `validate_betting_enabled(leagues, recs) -> invisible(TRUE)`; `drop_betting_disabled(recs, leagues = NULL)` (`@noRd`)

- [ ] **Step 1: Write the failing placer tests.** Append to `tests/testthat/test-betting-interlock.R`:

```r
interlock_recs_root <- function(env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  run_id <- as.POSIXct(format(Sys.Date()), tz = "UTC")
  # Sys.Date() + 3 rather than a fixed date: load_recommendations() drops
  # match_date < Sys.Date(), so a hardcoded near date rots into a dead test.
  recs <- tibble::tibble(
    run_id = rep(run_id, 2L),
    sport = c("football", "handball"),
    country = c("iceland", "iceland"),
    sex = c("male", "male"),
    match_date = rep(Sys.Date() + 3L, 2L),
    home_team = c("KR", "Valur"),
    away_team = c("FH", "FH"),
    market = c("moneyline", "moneyline"),
    outcome = c("home", "home"),
    line = c(NA_real_, NA_real_),
    p = c(0.55, 0.60),
    odds = c(2.05, 1.90),
    ev = c(0.13, 0.14),
    kelly = c(0.02, 0.03),
    bet_amount = c(400, 500)
  )
  write_table(recs, "recommendations", root = root)
  root
}

test_that("load_recommendations drops rows from betting-disabled leagues", {
  root <- interlock_recs_root()
  out <- suppressMessages(load_recommendations(root))
  expect_equal(nrow(out), 1L)
  expect_equal(out$sport, "football")
})

test_that("load_recommendations names the flag when it drops rows", {
  root <- interlock_recs_root()
  expect_message(load_recommendations(root), "betting.enabled")
})

test_that("preview_pending hides betting-disabled recommendations", {
  # run_auto_place() counts pending bets through preview_pending(), so this is
  # what keeps the unattended placer asleep.
  root <- interlock_recs_root()
  out <- suppressMessages(preview_pending(root = root))
  expect_equal(nrow(out), 1L)
  expect_equal(out$sport, "football")
})

test_that("validate_betting_enabled aborts on a disabled league's rec", {
  leagues <- load_leagues()
  recs <- tibble::tibble(
    sport = "handball", country = "iceland", sex = "male",
    home_team = "Valur", away_team = "FH"
  )
  expect_error(
    validate_betting_enabled(leagues, recs),
    "handball_iceland"
  )
})

test_that("validate_betting_enabled passes football and an empty frame", {
  leagues <- load_leagues()
  recs <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male",
    home_team = "KR", away_team = "FH"
  )
  expect_true(validate_betting_enabled(leagues, recs))
  expect_true(validate_betting_enabled(leagues, recs[0, , drop = FALSE]))
})
```

- [ ] **Step 2: Run — expect RED.**

```bash
(cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::test(filter = "betting-interlock")')
```

Exact expected failures: *"load_recommendations drops rows…"* → `nrow(out) not equal to 1L.` / `1/1 mismatches` / `[1] 2 - 1 == 1`; *"…names the flag"* → `` `load_recommendations(root)` did not produce any messages. ``; *"preview_pending hides…"* → `nrow(out) not equal to 1L` (`2 - 1 == 1`); both `validate_betting_enabled` tests → `could not find function "validate_betting_enabled"`.

- [ ] **Step 3: Add `drop_betting_disabled()` and wire it into `load_recommendations()`.** Append to `R/placer-load.R`:

```r
#' Drop recommendation rows belonging to betting-disabled leagues.
#'
#' Keyed on `paste0(sport, "_", country)`, the league key used throughout
#' `config/leagues.yml`. Filters rather than aborts, deliberately: `place_bets()`
#' is all-league, so a hard stop on one stray disabled row would also block
#' placement for the leagues that *are* armed. The drop is logged with its
#' count and the flag name, because a dropped row means a config-armed safety
#' catch fired, not a routine filter. An unreadable config leaves rows
#' untouched -- [decide_league()] is the first line of defence and
#' [validate_betting_enabled()] the last.
#'
#' @param recs Recommendations tibble.
#' @param leagues Loaded leagues config; `NULL` loads it.
#' @return `recs` minus rows from betting-disabled leagues.
#' @noRd
drop_betting_disabled <- function(recs, leagues = NULL) {
  if (nrow(recs) == 0L) {
    return(recs)
  }
  if (is.null(leagues)) {
    leagues <- tryCatch(load_leagues(), error = function(e) NULL)
  }
  if (is.null(leagues) || length(leagues) == 0L) {
    return(recs)
  }
  disabled <- names(leagues)[!vapply(leagues, betting_enabled, logical(1))]
  if (length(disabled) == 0L) {
    return(recs)
  }
  hit <- paste0(recs$sport, "_", recs$country) %in% disabled
  if (any(hit)) {
    cli::cli_alert_warning(paste0(
      "Dropping ", sum(hit), " recommendation row(s) from betting-disabled ",
      "league(s): ", paste(disabled, collapse = ", "),
      " (betting.enabled: false in config/leagues.yml)."
    ))
  }
  recs[!hit, , drop = FALSE]
}
```

Then in `load_recommendations()`, replace the closing `recs` at line 54 with:

```r
  # Betting interlock: the placer's choke point. place_bets() and
  # preview_pending() both enter here, and run_auto_place() counts pending
  # bets via preview_pending() -- so a disabled league can neither be
  # previewed, staked, nor wake the unattended placer.
  drop_betting_disabled(recs)
}
```

- [ ] **Step 4: Add the pre-flight validator.** Append to `R/placer-validate.R`:

```r
#' Abort placement when a recommendation belongs to a betting-disabled league.
#'
#' The last guard before Chrome launches, mirroring
#' [validate_team_names_config()]. [load_recommendations()] already drops these
#' rows, so reaching here means a caller supplied recommendations by some other
#' route -- exactly the case where a silent skip would be wrong, because real
#' money is committed a few lines later. Aborting here (rather than inside the
#' per-bet loop) means no slip can be half-placed.
#'
#' @param leagues Loaded `config/leagues.yml` (from [load_leagues()]).
#' @param recs Recommendations tibble bound for placement.
#' @return `invisible(TRUE)` when every row's league is betting-enabled.
#' @export
validate_betting_enabled <- function(leagues, recs) {
  if (!is.list(leagues)) {
    stop(
      "validate_betting_enabled: `leagues` must be a named list ",
      "from load_leagues()",
      call. = FALSE
    )
  }
  if (nrow(recs) == 0L) {
    return(invisible(TRUE))
  }
  keys <- unique(paste0(recs$sport, "_", recs$country))
  bad <- keys[vapply(keys, function(k) {
    !is.null(leagues[[k]]) && !betting_enabled(leagues[[k]])
  }, logical(1))]
  if (length(bad) > 0L) {
    stop(
      "validate_betting_enabled: betting is disabled by config ",
      "(betting.enabled: false) for: ", paste(bad, collapse = ", "),
      " — refusing to place. Flip the flag in config/leagues.yml to re-arm.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}
```

- [ ] **Step 5: Call it from `place_bets()`.** In `R/placer-pipeline.R`, after line 103 (`validate_team_names_config(leagues_cfg, recs)`):

```r
  validate_betting_enabled(leagues_cfg, recs)
```

- [ ] **Step 6: Document and run — expect GREEN.**

```bash
(cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::document()')
(cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::test(filter = "betting-interlock")')
(cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::test(filter = "placer")')
```

Both suites `[ FAIL 0 ]`. `tests/testthat/test-placer-load.R` uses only football and basketball fixtures — basketball is now disabled, so if *"load_recommendations honours league filter"* (test-placer-load.R:24) fails with `nrow(out) not equal to 1L` / `[1] 0 - 1 == -1`, that is the interlock working on a fixture: update that test to assert `expect_equal(nrow(out), 0L)` with a comment naming D2, rather than weakening `drop_betting_disabled()`.

- [ ] **Step 7: Commit.**

```bash
git -C /Users/brynjolfurjonsson/sports add R/placer-load.R R/placer-validate.R R/placer-pipeline.R NAMESPACE man/validate_betting_enabled.Rd tests/testthat/test-betting-interlock.R tests/testthat/test-placer-load.R
git -C /Users/brynjolfurjonsson/sports commit -m "feat(placer): refuse recommendations from betting-disabled leagues

load_recommendations() is the placer's only funnel -- place_bets() and
preview_pending() both enter through it, and run_auto_place() counts
pending bets via preview_pending() -- so filtering there keeps the
unattended agent asleep for a disabled league. It filters rather than
aborts because place_bets() is all-league and one stray handball row must
not block football. validate_betting_enabled() is the backstop: a hard
abort before chromote_login(), where nothing can be half-placed.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 6: Odds-ingest guard

**Files:**
- Modify: `/Users/brynjolfurjonsson/sports/R/ingest.R` (roxygen lines 148-157; signature line 159; guard as the first statement of the body)
- Modify: `/Users/brynjolfurjonsson/sports/scripts/02_scrape_odds.R` (line 66)
- Test: `/Users/brynjolfurjonsson/sports/tests/testthat/test-betting-interlock.R` (append)

**Interfaces:**
- Consumes: `betting_enabled()` (Task 2)
- Produces: `ingest_one_lengjan(static, lengjan, key, active_path, betting = NULL) -> integer(1)` — new trailing arg, so the 4-arg positional calls at `tests/testthat/test-ingest-lengjan-odds.R:180,193` keep working

- [ ] **Step 1: Write the failing ingest test.** Append to `tests/testthat/test-betting-interlock.R`:

```r
test_that("ingest_one_lengjan scrapes nothing for a betting-disabled league", {
  testthat::local_mocked_bindings(
    .is_league_active = function(active_path, key) TRUE,
    ingest_lengjan_odds = function(...) stop("scraper must not be reached")
  )
  res <- suppressMessages(ingest_one_lengjan(
    list(sport = "handball", country = "iceland"),
    list(competitions = list(list(id = "1269", name = "x", sex = "male"))),
    "handball_iceland", "active.json",
    betting = list(enabled = FALSE)
  ))
  expect_identical(res, 0L)
})

test_that("ingest_one_lengjan is unaffected when betting is absent", {
  testthat::local_mocked_bindings(
    .is_league_active = function(active_path, key) TRUE,
    ingest_lengjan_odds = function(...) 7L
  )
  res <- suppressMessages(ingest_one_lengjan(
    list(sport = "football", country = "iceland"), list(),
    "football_iceland", "active.json"
  ))
  expect_identical(res, 7L)
})
```

- [ ] **Step 2: Run — expect RED.**

```bash
(cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::test(filter = "betting-interlock")')
```

Exact expected failure for the first: `Error in `ingest_one_lengjan(...)`: unused argument (betting = list(enabled = FALSE))`. (The second passes already — it locks the existing 4-arg contract.)

- [ ] **Step 3: Add the parameter and the guard.** In `R/ingest.R`, replace the roxygen block and signature (lines 147-160) with:

```r
#' Run Lengjan odds ingest for a single league (pipeline wrapper).
#'
#' Takes the static + lengjan + betting slices separately so the caller doesn't
#' re-load the full leagues config per league.
#'
#' @param static Per-league static slice.
#' @param lengjan Per-league `lengjan` slice (competitions + team_names).
#' @param key League key.
#' @param active_path Path to `config/active_competitions.json`.
#' @param betting Per-league `betting` slice. `NULL` (the default) means
#'   enabled, matching an absent `betting.enabled` key in the schema.
#' @return Number of odds rows written (integer).
#' @export
ingest_one_lengjan <- function(static, lengjan, key, active_path,
                               betting = NULL) {
  # Betting interlock: belt and braces behind the emptied `competitions` list.
  # Zero odds rows in means zero candidates out even if a future edit restores
  # the competition ids without also flipping the flag back.
  if (!betting_enabled(list(betting = betting))) {
    cli::cli_alert_info(
      "{key}: skipped (betting.enabled: false in config/leagues.yml)"
    )
    return(0L)
  }
  if (!.is_league_active(active_path, key)) {
```

- [ ] **Step 4: Pass the slice from the script.** In `scripts/02_scrape_odds.R`, replace line 66:

```r
  total_rows <- total_rows + ingest_one_lengjan(
    static, lengjan, key, active_path,
    betting = league_def$betting
  )
```

- [ ] **Step 5: Run — expect GREEN.**

```bash
(cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::document()')
(cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::test(filter = "betting-interlock")')
(cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::test(filter = "ingest")')
(cd /Users/brynjolfurjonsson/sports && Rscript scripts/02_scrape_odds.R)
```

Both suites `[ FAIL 0 ]`; the script exits 0 (it only ever reaches football, since `filter_leagues(has_lengjan = TRUE)` already drops the two emptied leagues).

- [ ] **Step 6: Commit.**

```bash
git -C /Users/brynjolfurjonsson/sports add R/ingest.R scripts/02_scrape_odds.R man/ingest_one_lengjan.Rd tests/testthat/test-betting-interlock.R
git -C /Users/brynjolfurjonsson/sports commit -m "feat(ingest): skip Lengjan odds for a betting-disabled league

Third of the three interlock points, behind the emptied competitions list
and in front of the decide guard. It is redundant today -- has_lengjan
already drops both leagues -- and that is the point: the day someone
restores the competition ids without flipping the flag, no odds are
scraped and no candidates can follow.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 7: Health checks report PAUSED, not unhealthy

**Files:**
- Modify: `/Users/brynjolfurjonsson/sports/R/health.R` (guard inside the loop at line 209; `check_capture_rate()` signature line 321 and filter near line 341; `pipeline_health()` call site line 534)
- Test: `/Users/brynjolfurjonsson/sports/tests/testthat/test-betting-interlock.R` (append)

**Interfaces:**
- Consumes: `betting_enabled()` (Task 2), `health_row()`, `health_thresholds()`
- Produces: `check_capture_rate(root, now, th, leagues = list())` — new trailing arg with a default, so existing call sites are unchanged

> Note: the spec says both checks "take `leagues` already". Verified false for `check_capture_rate` — its signature is `check_capture_rate(root, now, th)` (R/health.R:321), invoked as `safe(check_capture_rate(root, now, th))` (R/health.R:534). Hence the extra argument below.

- [ ] **Step 1: Write the failing health tests.** Append to `tests/testthat/test-betting-interlock.R`:

```r
test_that("odds_freshness reports PAUSED for a betting-disabled league", {
  root <- withr::local_tempdir()
  leagues <- list(
    handball_iceland = list(
      sport = "handball", country = "iceland", sexes = "male",
      betting = list(enabled = FALSE)
    )
  )
  out <- check_odds_freshness(leagues, root, Sys.time(), health_thresholds())
  expect_equal(nrow(out), 1L)
  expect_equal(out$status, "PAUSED")
  expect_match(out$value, "betting disabled")
})

test_that("a PAUSED odds row does not escalate the overall status", {
  paused <- health_row("odds_freshness", "handball_iceland", "PAUSED",
    "betting disabled by config", "n/a"
  )
  expect_equal(overall_health_status(paused), "OK")
})

test_that("capture_rate ignores recs from betting-disabled leagues", {
  root <- withr::local_tempdir()
  run_id <- as.POSIXct(format(Sys.Date()), tz = "UTC")
  recs <- tibble::tibble(
    run_id = rep(run_id, 2L),
    sport = c("football", "handball"),
    country = c("iceland", "iceland"),
    sex = c("male", "male"),
    match_date = rep(Sys.Date() - 1L, 2L),
    home_team = c("KR", "Valur"),
    away_team = c("FH", "FH"),
    market = c("moneyline", "moneyline"),
    outcome = c("home", "home"),
    line = c(NA_real_, NA_real_),
    p = c(0.55, 0.60), odds = c(2.05, 1.90),
    ev = c(0.13, 0.14), kelly = c(0.02, 0.03),
    bet_amount = c(400, 500)
  )
  write_table(recs, "recommendations", root = root)

  out <- check_capture_rate(
    root, Sys.time(), health_thresholds(),
    leagues = load_leagues()
  )
  # Only the football rec is counted: "0% (0/1 placed)", not "(0/2 placed)".
  expect_match(out$value, "/1 placed", fixed = TRUE)
})
```

- [ ] **Step 2: Run — expect RED.**

```bash
(cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::test(filter = "betting-interlock")')
```

Exact expected failures: *"odds_freshness reports PAUSED…"* → `nrow(out) not equal to 1L.` / `1/1 mismatches` / `[1] 0 - 1 == -1` (the current function `next`s past a league with no schedule rows and returns zero rows); *"capture_rate ignores recs…"* → `Error in `check_capture_rate(...)`: unused argument (leagues = load_leagues())`. The `overall_health_status` test passes already — it is the lock that `PAUSED` never escalates.

- [ ] **Step 3: Add the odds_freshness guard.** In `R/health.R`, as the first statement inside the `for (key in names(leagues))` loop at line 209 (before `lg <- leagues[[key]]` is used for the schedule read):

```r
  for (key in names(leagues)) {
    lg <- leagues[[key]]
    # Betting interlock: a league that is not allowed to bet cannot have stale
    # odds. Placed before the schedule read so the row appears the moment
    # fixtures return, rather than the check silently going quiet.
    if (!betting_enabled(lg)) {
      rows[[key]] <- health_row(
        "odds_freshness", key, "PAUSED",
        "betting disabled by config (betting.enabled: false)", thr_lbl
      )
      next
    }
    static <- list(sport = lg$sport, country = lg$country)
```

- [ ] **Step 4: Teach capture_rate the flag.** In `R/health.R`, change the signature at line 321 to:

```r
check_capture_rate <- function(root, now, th, leagues = list()) {
```

and insert immediately after `rec_d <- dplyr::distinct(recent[, key, drop = FALSE])` (line 341):

```r
  # Betting interlock: historical recommendations for a league that has since
  # been disabled were never going to be placed, so counting them would drag
  # the capture rate toward FAIL for a purely intentional reason.
  if (length(leagues) > 0L) {
    disabled <- names(leagues)[!vapply(leagues, betting_enabled, logical(1))]
    if (length(disabled) > 0L) {
      lg_key <- paste0(rec_d$sport, "_", rec_d$country)
      rec_d <- rec_d[!(lg_key %in% disabled), , drop = FALSE]
    }
  }
```

Then update the `pipeline_health()` call site (line 534):

```r
    safe(check_capture_rate(root, now, th, leagues)),
```

- [ ] **Step 5: Run — expect GREEN, and take a live health snapshot.**

```bash
(cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::test(filter = "betting-interlock")')
(cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::test(filter = "health")')
(cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::load_all(quiet = TRUE); h <- pipeline_health(); print(h[h$check %in% c("odds_freshness", "capture_rate"), ]); print(overall_health_status(h))')
```

Expect `[ FAIL 0 ]` on both suites, `PAUSED` rows scoped `basketball_iceland` and `handball_iceland`, and an overall status no worse than before this workstream.

- [ ] **Step 6: Commit.**

```bash
git -C /Users/brynjolfurjonsson/sports add R/health.R tests/testthat/test-betting-interlock.R
git -C /Users/brynjolfurjonsson/sports commit -m "fix(health): report betting-disabled leagues as PAUSED, not unhealthy

Once later workstreams restore basketball and handball fixtures,
odds_freshness would FAIL on two leagues that are deliberately not
betting, and capture_rate would count their historical recommendations as
missed placements. Both would fire the alert email for an intended state
and desensitise the channel. PAUSED never escalates
overall_health_status(). check_capture_rate() gains a leagues argument --
contrary to the spec it did not have one.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 8: Whole-workstream verification

**Files:** none modified (verification only; `docs/` note optional)

**Interfaces:** consumes everything above.

- [ ] **Step 1: Full suite from the package root.**

```bash
(cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::test()')
```

Expect `[ FAIL 0 | WARN 0 ]`. Record the PASS count and the date — it is the baseline WS2 compares against.

- [ ] **Step 2: Prove the interlock end to end, decide → placer, on a scratch root.**

```bash
(cd /Users/brynjolfurjonsson/sports && Rscript -e '
devtools::load_all(quiet = TRUE)
root <- tempfile(); dir.create(root)
run_id <- as.POSIXct(format(Sys.Date()), tz = "UTC")
# Inject handball odds directly, bypassing the ingest guard entirely --
# this proves the decide guard stands on its own.
write_table(tibble::tibble(
  sport = "handball", country = "iceland", scraped_at = Sys.time(),
  match_date = Sys.Date() + 1L, home_team = "Valur", away_team = "FH",
  market = c("moneyline", "moneyline"), outcome = c("home", "away"),
  line = NA_real_, odds = c(1.8, 2.2)
), "odds", root = root)
set.seed(1)
write_table(tibble::tibble(
  sport = "handball", country = "iceland", sex = "male",
  fit_date = Sys.Date(), match_date = Sys.Date() + 1L,
  home_team = "Valur", away_team = "FH", draw_id = 1:1000,
  home_goals = rpois(1000, 28), away_goals = rpois(1000, 26)
), "beliefs_latest", root = root)
lg <- load_leagues()$handball_iceland
decide_league(league = lg, sex = "male", root = root,
              bankroll = load_bankroll(ledger_root = root))
cat("recommendations rows:", nrow(read_table("recommendations", root = root)), "\n")
cat("pending after preview:", nrow(preview_pending(root = root)), "\n")
')
```

Expect a `betting.enabled: false` warning, `recommendations rows: 0`, `pending after preview: 0`.

- [ ] **Step 3: Confirm the placer subsystem is still absent from CI.**

```bash
(cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::test(filter = "placer-ci-isolation")')
```

Expect `[ FAIL 0 ]` — `validate_betting_enabled` lives in `R/placer-validate.R`, and no workflow may reference it.

- [ ] **Step 4: Push the branch.**

```bash
git -C /Users/brynjolfurjonsson/sports fetch origin
git -C /Users/brynjolfurjonsson/sports rebase origin/main
git -C /Users/brynjolfurjonsson/sports push -u origin feat/bb-hb-metill-parity
```

(`fetch` first: five cron workflows commit to `main` all day, so the branch point moves under a long session.)


---


# WS2 — Test and fixture harness (spec §4): synthetic facts + extracts fixtures, a draws stub, un-gating the 4 dead test files, and the football golden-file safety net

**Produces (later workstreams rely on these):**

- `fixture_facts_root(env = parent.frame()) -> character  # temp data root containing facts/results + facts/schedules for all three sports`
- `FIXTURE_END_DATE  # as.Date("2100-01-15") — the pinned end_date every fixture-driven test passes`
- `FIXTURE_FIT_DATE  # as.Date("2100-01-01") — the pinned extracts partition stamp`
- `fixture_division_teams(sport, sex, division) -> character  # deterministic team names in the fixture`
- `stub_fit(draws) -> list(draws = function(variables = NULL, ...))  # object whose $draws(var) mimics CmdStanMCMC$draws()`
- `stub_2dt_draws(teams, n_pred, n_draws = 50L, seed = 2100L, constants = list()) -> posterior::draws_df`
- `local_stub_2dt(league, sex, end_date = FIXTURE_END_DATE, root, n_draws = 50L, constants = list()) -> list(fit =, prep =)  # sizes goals*_pred from prepare_data()'s own pred_d, so n_pred can never silently mismatch`
- `fixture_extracts_root(sports = c("basketball", "handball"), env = parent.frame()) -> character`
- `build_football_extracts_fixture(facts_root, extracts_root, sex, fit_date = FIXTURE_FIT_DATE) -> invisible(NULL)`
- `publish_json_digest(path) -> character  # sha256 of the JSON with every generated_at key stripped at any depth`
- `make_extract_fixtures(dest = NULL, quiet = FALSE) -> invisible(list(bytes = <integer>, files = <character>))`
- `make_football_golden_hashes(dest = NULL) -> invisible(character)`

## Workstream 2 — Test and fixture harness (spec §4)

Branch: `feat/bb-hb-metill-parity`. Every command below uses absolute paths (worktree cwd-drift rule).

**Verified baseline before this workstream (re-measure in Task 11):**

| file | blocks | skip gates | today |
|---|---|---|---|
| `test-publish-basketball.R` | 7 | lines 11, 50, 100, 160, 192 | SKIP 5 / PASS 4 |
| `test-publish-handball.R` | 5 | lines 11, 50, 100 | SKIP 3 / PASS 4 |
| `test-extract-basketball-iceland.R` | 3 | all 3 (`fit.rds`) | SKIP 3 / PASS 0 |
| `test-extract-handball-iceland.R` | 3 | all 3 (`fit.rds`) | SKIP 3 / PASS 0 |

**14 skips must be gone by Task 11.** Nothing in this workstream touches
`.compute_home_advantage_quantiles_2dt` or the `exp()` units bug — WS3 owns that
exclusively. WS2's only B5 obligation is including WS3's
`tests/testthat/test-extract-home-advantage-units.R` in the Task 10 grep set.

---

### Task 1: Worktree-safe fixture-generator scaffold

**Files:**
- Create `/Users/brynjolfurjonsson/sports/tools/make-extract-fixtures.R`
- Create `/Users/brynjolfurjonsson/sports/tests/testthat/test-fixture-harness.R`

**Interfaces:**
- Consumes: nothing.
- Produces: `make_extract_fixtures(dest = NULL, quiet = FALSE) -> invisible(list(bytes, files))`; the file-local `.fixture_gen_pkg_root()` guard.

- [ ] **Step 1: Write the failing guard test.** The generator must be `source()`-able from a test without loading the package — in a worktree `here::here()` resolves to the MAIN checkout, so a top-level `devtools::load_all(here::here())` would regenerate fixtures against the wrong package.

```r
# /Users/brynjolfurjonsson/sports/tests/testthat/test-fixture-harness.R
test_that("the fixture generator is source-able without loading a package", {
  gen <- testthat::test_path("..", "..", "tools", "make-extract-fixtures.R")
  expect_true(file.exists(gen))

  src <- readLines(gen, warn = FALSE)
  # No unguarded load_all / here::here() at column 0 (top level).
  top_level <- grep("^[^ \t#]", src, value = TRUE)
  expect_false(any(grepl("load_all", top_level, fixed = TRUE)))
  expect_false(any(grepl("here::here", top_level, fixed = TRUE)))

  env <- new.env(parent = globalenv())
  expect_silent(sys.source(gen, envir = env))
  expect_true(is.function(env$make_extract_fixtures))
})
```

- [ ] **Step 2: Run it, expect RED.**

```bash
Rscript -e 'devtools::load_all("/Users/brynjolfurjonsson/sports", quiet = TRUE); testthat::test_file("/Users/brynjolfurjonsson/sports/tests/testthat/test-fixture-harness.R")'
```

Exact expected failure: `Failure: file.exists(gen) is not TRUE` on the first
expectation (the `tools/make-extract-fixtures.R` path does not exist yet).

- [ ] **Step 3: Write the scaffold.** The script resolves its own location from `--file=` and only self-executes at top level (`sys.nframe() == 0L`), so `sys.source()`ing it from a test defines functions and runs nothing.

```r
# /Users/brynjolfurjonsson/sports/tools/make-extract-fixtures.R
#
# Regenerate every committed test fixture for the bb/hb metill-parity harness.
#
#   Rscript tools/make-extract-fixtures.R
#
# WHY the odd self-execution guard: in a git worktree `here::here()` resolves to
# the MAIN checkout, so a top-level `devtools::load_all(here::here())` would load
# a different package than the one being edited and silently regenerate fixtures
# against it. The package root is derived from THIS file's own path instead, and
# the load happens only when the file is run as a script.

FIXTURE_SEASONS <- c(2099L, 2100L)
FIXTURE_END_DATE <- as.Date("2100-01-15")
FIXTURE_FIT_DATE <- as.Date("2100-01-01")
FIXTURE_N_DRAWS <- 50L

# Division -> team count. Football BD carries the split-season group sizes from
# config/leagues.yml (male 6/6, female 6/4), so it needs 12 / 10 teams.
FIXTURE_DIVISIONS <- list(
  basketball = list(male = c(BD = 6L, `1D` = 6L), female = c(BD = 6L, `1D` = 6L)),
  handball   = list(male = c(OD = 6L, G66 = 6L), female = c(OD = 6L, G66 = 6L)),
  football   = list(
    male   = c(BD = 12L, LD1 = 6L, LD2 = 6L, LD3 = 6L, CUP = 4L),
    female = c(BD = 10L, LD1 = 6L, LD2 = 6L, CUP = 4L)
  )
)

#' Deterministic fixture team names for one (sport, sex, division) cell.
#' @noRd
fixture_division_teams <- function(sport, sex, division) {
  n <- FIXTURE_DIVISIONS[[sport]][[sex]][[division]]
  stopifnot(!is.null(n))
  sprintf(
    "%s%s %s %02d",
    toupper(substr(sport, 1L, 2L)), toupper(substr(sex, 1L, 1L)),
    division, seq_len(n)
  )
}

# Resolve the package root from this script's own --file= argument. Returns NULL
# when the file was source()d rather than run, which disables self-execution.
.fixture_gen_pkg_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  hit <- grep("^--file=", args, value = TRUE)
  if (length(hit) != 1L) return(NULL)
  script <- sub("^--file=", "", hit)
  if (basename(script) != "make-extract-fixtures.R") return(NULL)
  normalizePath(file.path(dirname(script), ".."), mustWork = FALSE)
}

#' Regenerate all committed fixtures. Filled in over Tasks 2-8.
#' @noRd
make_extract_fixtures <- function(dest = NULL, quiet = FALSE) {
  if (is.null(dest)) {
    root <- .fixture_gen_pkg_root()
    stopifnot(!is.null(root))
    dest <- file.path(root, "tests", "testthat", "fixtures")
  }
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  files <- character()
  bytes <- 0L
  if (!quiet) message("make_extract_fixtures: wrote ", length(files), " files")
  invisible(list(bytes = bytes, files = files))
}

if (sys.nframe() == 0L && !is.null(.fixture_gen_pkg_root())) {
  devtools::load_all(.fixture_gen_pkg_root(), quiet = TRUE)
  make_extract_fixtures()
}
```

- [ ] **Step 4: Run, expect PASS.**

```bash
Rscript -e 'devtools::load_all("/Users/brynjolfurjonsson/sports", quiet = TRUE); testthat::test_file("/Users/brynjolfurjonsson/sports/tests/testthat/test-fixture-harness.R")'
```

Expect `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 3 ]`.

- [ ] **Step 5: Commit.**

```bash
git -C /Users/brynjolfurjonsson/sports add tools/make-extract-fixtures.R tests/testthat/test-fixture-harness.R
git -C /Users/brynjolfurjonsson/sports commit -m "test(fixtures): scaffold a worktree-safe fixture generator

The bb/hb parity work deletes ~740 lines of live publisher, so the harness
has to exist first. The generator resolves the package root from its own
--file= path rather than here::here(), because here::here() resolves to the
MAIN checkout inside a worktree and would regenerate fixtures against the
wrong package.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 2: Synthetic facts tree (results + schedules) for all three sports

**Files:**
- Modify `/Users/brynjolfurjonsson/sports/tools/make-extract-fixtures.R` (add `.fixture_results()`, `.fixture_schedules()`, wire into `make_extract_fixtures()`)
- Create `/Users/brynjolfurjonsson/sports/tests/testthat/fixtures/facts/results.parquet`
- Create `/Users/brynjolfurjonsson/sports/tests/testthat/fixtures/facts/schedules.parquet`
- Create `/Users/brynjolfurjonsson/sports/tests/testthat/helper-fixture-facts.R`
- Modify `/Users/brynjolfurjonsson/sports/tests/testthat/test-fixture-harness.R` (append)

**Interfaces:**
- Consumes: `write_table()`, `read_table()`, `prepare_data()`, `load_leagues()`.
- Produces: `fixture_facts_root(env = parent.frame())`, `FIXTURE_END_DATE`, `FIXTURE_FIT_DATE`, `fixture_division_teams(sport, sex, division)`.

- [ ] **Step 1: Write the failing contract test.** Append to `test-fixture-harness.R`:

```r
test_that("the facts fixture drives prepare_data for all three sports", {
  root <- fixture_facts_root()
  leagues <- load_leagues()

  cells <- list(
    list(key = "basketball_iceland", sex = "male"),
    list(key = "basketball_iceland", sex = "female"),
    list(key = "handball_iceland", sex = "male"),
    list(key = "handball_iceland", sex = "female"),
    list(key = "football_iceland", sex = "male"),
    list(key = "football_iceland", sex = "female")
  )

  for (cell in cells) {
    prep <- suppressMessages(prepare_data(
      leagues[[cell$key]], cell$sex,
      end_date = FIXTURE_END_DATE, root = root
    ))
    info <- paste(cell$key, cell$sex)
    # Training data present, upcoming fixtures inside the DEFAULT 14-day horizon.
    expect_gt(nrow(prep$teams), 5L)
    expect_gt(prep$stan_data$N, 10L)
    expect_gt(nrow(prep$pred_d), 0L)
    expect_equal(prep$stan_data$N_pred, nrow(prep$pred_d), info = info)
    expect_true(all(prep$pred_d$match_date > FIXTURE_END_DATE), info = info)
    expect_true(all(prep$pred_d$match_date <= FIXTURE_END_DATE + 14L), info = info)
  }
})
```

- [ ] **Step 2: Run it, expect RED.**

```bash
Rscript -e 'devtools::load_all("/Users/brynjolfurjonsson/sports", quiet = TRUE); testthat::test_file("/Users/brynjolfurjonsson/sports/tests/testthat/test-fixture-harness.R")'
```

Exact expected failure: `Error in fixture_facts_root() : could not find function "fixture_facts_root"`.

- [ ] **Step 3: Add the fixture builders to the generator.** Insert above `make_extract_fixtures()`:

```r
# Single round-robin per (sport, sex, division, season). Home team indexed
# lower than away wins by a margin that also orders goal difference, so the
# realised table equals the team order -- a deterministic standings target.
.fixture_results_one <- function(sport, sex, division, season, start_date) {
  teams <- fixture_division_teams(sport, sex, division)
  grid <- utils::combn(seq_along(teams), 2L)
  n <- ncol(grid)
  hi <- grid[1L, ]
  ai <- grid[2L, ]
  base <- switch(sport, basketball = 80L, handball = 24L, football = 1L)
  tibble::tibble(
    sport      = sport,
    country    = "iceland",
    sex        = sex,
    season     = as.integer(season),
    match_date = start_date + rep(seq_len(ceiling(n / 3L)), each = 3L)[seq_len(n)],
    home_team  = teams[hi],
    away_team  = teams[ai],
    home_score = as.integer(base + length(teams) - hi + 1L),
    away_score = as.integer(base + length(teams) - ai),
    division   = division,
    round      = as.integer(rep(seq_len(ceiling(n / 3L)), each = 3L)[seq_len(n)])
  )
}

#' All committed synthetic results rows.
#' @noRd
.fixture_results <- function() {
  out <- list()
  for (sport in names(FIXTURE_DIVISIONS)) {
    for (sex in names(FIXTURE_DIVISIONS[[sport]])) {
      for (division in names(FIXTURE_DIVISIONS[[sport]][[sex]])) {
        # 2099 gives prepare_data a second season (season_first / N_seasons);
        # 2100 is the current season the publishers summarise.
        out[[length(out) + 1L]] <- .fixture_results_one(
          sport, sex, division, 2099L, as.Date("2099-11-02")
        )
        out[[length(out) + 1L]] <- .fixture_results_one(
          sport, sex, division, 2100L, as.Date("2100-01-02")
        )
      }
    }
  }
  dplyr::bind_rows(out)
}

# Three upcoming matches per (sport, sex, division), all inside
# [FIXTURE_END_DATE, FIXTURE_END_DATE + 14] so prepare_data()'s DEFAULT
# schedule_horizon_days = 14L picks them up -- the publishers call
# prepare_data() internally at the default and take no prep= argument.
#' @noRd
.fixture_schedules <- function() {
  out <- list()
  for (sport in names(FIXTURE_DIVISIONS)) {
    for (sex in names(FIXTURE_DIVISIONS[[sport]])) {
      for (division in names(FIXTURE_DIVISIONS[[sport]][[sex]])) {
        teams <- fixture_division_teams(sport, sex, division)
        out[[length(out) + 1L]] <- tibble::tibble(
          sport        = sport,
          country      = "iceland",
          sex          = sex,
          season       = 2100L,
          match_date   = as.Date(c("2100-01-16", "2100-01-18", "2100-01-20")),
          home_team    = teams[c(1L, 3L, 5L)],
          away_team    = teams[c(2L, 4L, 6L)],
          division     = division,
          round        = c(90L, 91L, 92L),
          kickoff_time = c("19:15", "19:15", "17:00")
        )
      }
    }
  }
  dplyr::bind_rows(out)
}
```

and replace the body of `make_extract_fixtures()`'s accumulation with:

```r
  facts_dir <- file.path(dest, "facts")
  dir.create(facts_dir, recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(.fixture_results(), file.path(facts_dir, "results.parquet"))
  arrow::write_parquet(.fixture_schedules(), file.path(facts_dir, "schedules.parquet"))
  files <- c(
    file.path(facts_dir, "results.parquet"),
    file.path(facts_dir, "schedules.parquet")
  )
  bytes <- sum(file.info(files)$size)
```

- [ ] **Step 4: Write the test helper.**

```r
# /Users/brynjolfurjonsson/sports/tests/testthat/helper-fixture-facts.R
#
# Committed synthetic facts tree, materialised into a temp hive-partitioned data
# root. Mirrors setup_mini_root() in test-model-prepare.R but covers all three
# sports and both sexes, at far-future dates so it can never rot (the repo's
# time-bomb rule: no near-date literals in fixtures).

FIXTURE_END_DATE <- as.Date("2100-01-15")
FIXTURE_FIT_DATE <- as.Date("2100-01-01")
FIXTURE_N_DRAWS <- 50L

FIXTURE_DIVISIONS <- list(
  basketball = list(male = c(BD = 6L, `1D` = 6L), female = c(BD = 6L, `1D` = 6L)),
  handball   = list(male = c(OD = 6L, G66 = 6L), female = c(OD = 6L, G66 = 6L)),
  football   = list(
    male   = c(BD = 12L, LD1 = 6L, LD2 = 6L, LD3 = 6L, CUP = 4L),
    female = c(BD = 10L, LD1 = 6L, LD2 = 6L, CUP = 4L)
  )
)

fixture_division_teams <- function(sport, sex, division) {
  n <- FIXTURE_DIVISIONS[[sport]][[sex]][[division]]
  stopifnot(!is.null(n))
  sprintf(
    "%s%s %s %02d",
    toupper(substr(sport, 1L, 2L)), toupper(substr(sex, 1L, 1L)),
    division, seq_len(n)
  )
}

#' Materialise the committed facts fixture into a temp data root.
#' Lifetime is tied to `env` (the calling test_that frame by default).
fixture_facts_root <- function(env = parent.frame()) {
  tmp <- withr::local_tempdir(.local_envir = env)
  results <- arrow::read_parquet(
    testthat::test_path("fixtures", "facts", "results.parquet")
  )
  schedules <- arrow::read_parquet(
    testthat::test_path("fixtures", "facts", "schedules.parquet")
  )
  write_table(results, "results", root = tmp)
  write_table(schedules, "schedules", root = tmp)
  tmp
}
```

- [ ] **Step 5: Generate the parquets and re-run.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript tools/make-extract-fixtures.R
Rscript -e 'devtools::load_all("/Users/brynjolfurjonsson/sports", quiet = TRUE); testthat::test_file("/Users/brynjolfurjonsson/sports/tests/testthat/test-fixture-harness.R")'
```

Expect `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 39 ]` (3 from Task 1 + 6 expectations x 6 cells).

- [ ] **Step 6: Commit.**

```bash
git -C /Users/brynjolfurjonsson/sports add tools/make-extract-fixtures.R tests/testthat/helper-fixture-facts.R tests/testthat/fixtures/facts tests/testthat/test-fixture-harness.R
git -C /Users/brynjolfurjonsson/sports commit -m "test(fixtures): synthetic facts tree for all three sports

prepare_data() is the entry point every publisher and extractor calls, and
the bb/hb tests have never had a data root they could point it at. Dates are
2099/2100 literals so the fixture cannot rot into a failing test the way a
near-future 'upcoming match' would.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 3: `stub_fit()` — the cmdstanr `$draws()` surface the 2DT code actually uses

**Files:**
- Create `/Users/brynjolfurjonsson/sports/tests/testthat/helper-stub-fit.R`
- Create `/Users/brynjolfurjonsson/sports/tests/testthat/test-stub-fit.R`

**Interfaces:**
- Consumes: `fixture_facts_root()`, `FIXTURE_END_DATE`, `prepare_data()`.
- Produces: `stub_fit(draws)`, `stub_2dt_draws(teams, n_pred, n_draws = 50L, seed = 2100L, constants = list())`, `local_stub_2dt(league, sex, end_date, root, n_draws, constants)`.

Verified consumer surface (grepped, not guessed):
`R/publish-iceland-2dt-helpers.R:17` (`.extract_team_draws_2dt`) reads
`cur_offense_home`, `cur_defense_home`, `cur_strength_home`, `cur_offense_away`,
`cur_defense_away`, `cur_strength_away`; `:51` (`.compute_posterior_goals_2dt`)
reads `c("goals1_pred", "goals2_pred")`; `:300` and
`R/extract-iceland-2dt-shared.R:197` read `home_advantage_off/def/tot`;
`R/publish-{basketball,handball}-iceland.R:114/:111` read `lp__` through
`posterior::ndraws()`.

- [ ] **Step 1: Write the failing test.**

```r
# /Users/brynjolfurjonsson/sports/tests/testthat/test-stub-fit.R
test_that("stub_2dt_draws covers the whole 2DT variable surface", {
  d <- stub_2dt_draws(teams = c("A", "B", "C"), n_pred = 4L, n_draws = 20L)
  fit <- stub_fit(d)

  expect_equal(posterior::ndraws(fit$draws("lp__")), 20L)

  team_vars <- c(
    "cur_offense_home", "cur_defense_home", "cur_strength_home",
    "cur_offense_away", "cur_defense_away", "cur_strength_away",
    "home_advantage_off", "home_advantage_def", "home_advantage_tot"
  )
  for (v in team_vars) {
    sub <- fit$draws(v)
    expect_equal(
      posterior::variables(sub), paste0(v, "[", 1:3, "]"),
      info = v
    )
  }

  joint <- fit$draws(c("goals1_pred", "goals2_pred"))
  expect_length(posterior::variables(joint), 8L)
  expect_s3_class(posterior::as_draws_df(joint), "draws_df")
})

test_that("stub_2dt_draws honours pinned constants", {
  d <- stub_2dt_draws(
    teams = c("A", "B"), n_pred = 2L, n_draws = 10L,
    constants = list(home_advantage_tot = 4)
  )
  vals <- as.vector(posterior::draws_of(
    posterior::as_draws_rvars(d)$home_advantage_tot
  ))
  expect_true(all(vals == 4))
})

test_that("stub_2dt_draws is deterministic for a fixed seed", {
  a <- stub_2dt_draws(c("A", "B"), n_pred = 2L, n_draws = 10L, seed = 7L)
  b <- stub_2dt_draws(c("A", "B"), n_pred = 2L, n_draws = 10L, seed = 7L)
  expect_equal(a, b)
})

test_that("local_stub_2dt sizes goals*_pred from prepare_data's own pred_d", {
  root <- fixture_facts_root()
  league <- load_leagues()[["basketball_iceland"]]
  st <- suppressMessages(local_stub_2dt(league, "male", root = root))

  # The publishers call prepare_data() internally at the DEFAULT
  # schedule_horizon_days = 14L and take no prep= argument, so a stub sized at
  # any other horizon would make .compute_posterior_goals_2dt warn and return
  # zero rows. Assert the match rather than trusting it.
  expect_equal(
    max(as.integer(sub(
      ".*\\[(\\d+)\\]$", "\\1",
      grep("^goals1_pred", posterior::variables(st$fit$draws("goals1_pred")), value = TRUE)
    ))),
    nrow(st$prep$pred_d)
  )
  expect_silent(pg <- .compute_posterior_goals_2dt(st$fit, st$prep$pred_d))
  expect_gt(nrow(pg), 0L)
})
```

- [ ] **Step 2: Run it, expect RED.**

```bash
Rscript -e 'devtools::load_all("/Users/brynjolfurjonsson/sports", quiet = TRUE); testthat::test_file("/Users/brynjolfurjonsson/sports/tests/testthat/test-stub-fit.R")'
```

Exact expected failure: `Error in stub_2dt_draws(...) : could not find function "stub_2dt_draws"` on all four blocks.

- [ ] **Step 3: Write the helper.**

```r
# /Users/brynjolfurjonsson/sports/tests/testthat/helper-stub-fit.R
#
# A cmdstanr-fit-like object for tests. The 2DT extractors and publishers touch
# exactly one method -- `fit$draws(var)` -- so a list carrying that closure is a
# complete substitute for a 300-600 MB gitignored fit.rds, which is why the
# extract tests have never once executed.

#' Minimal CmdStanMCMC substitute.
#'
#' @param draws A `posterior::draws_df` (or anything `posterior` accepts).
#' @return A list whose `$draws(variables)` subsets `draws` by variable name;
#'   a bare variable name selects all of its indexed elements.
stub_fit <- function(draws) {
  stopifnot(posterior::is_draws(draws))
  force(draws)
  list(
    draws = function(variables = NULL, ...) {
      if (is.null(variables)) {
        return(draws)
      }
      posterior::subset_draws(draws, variable = variables)
    }
  )
}

#' Build the full 2DT posterior surface as a draws_df.
#'
#' @param teams Character vector of team names (length K).
#' @param n_pred Number of prediction rows -- MUST equal `nrow(pred_d)` from the
#'   same `prepare_data()` call the code under test will make.
#' @param n_draws Posterior draws. 50 keeps fixtures small and quantiles stable.
#' @param seed Integer seed; the same seed always yields the same draws.
#' @param constants Named list of `variable = value`; every element of that
#'   variable is pinned to `value` (used by callers that need an exactly-known
#'   posterior, e.g. a units assertion).
stub_2dt_draws <- function(teams, n_pred, n_draws = 50L, seed = 2100L,
                           constants = list()) {
  stopifnot(is.character(teams), length(teams) >= 2L, n_pred >= 1L)
  k <- length(teams)
  set.seed(seed)

  block <- function(prefix, n, centre, spread) {
    m <- matrix(
      rep(centre, each = n_draws) + stats::rnorm(n_draws * n, 0, spread),
      nrow = n_draws, ncol = n
    )
    if (!is.null(constants[[prefix]])) {
      m[] <- constants[[prefix]]
    }
    colnames(m) <- paste0(prefix, "[", seq_len(n), "]")
    m
  }

  # Team-ordered strengths: team 1 strongest, so the simulated table matches the
  # facts fixture's deterministic results ordering.
  off <- seq(from = 1.5, to = -1.5, length.out = k)
  def <- seq(from = -1.2, to = 1.2, length.out = k)

  mats <- list(
    block("cur_offense_home", k, off + 0.3, 0.4),
    block("cur_defense_home", k, def - 0.2, 0.4),
    block("cur_strength_home", k, off - def + 0.5, 0.5),
    block("cur_offense_away", k, off, 0.4),
    block("cur_defense_away", k, def, 0.4),
    block("cur_strength_away", k, off - def, 0.5),
    block("home_advantage_off", k, rep(1.1, k), 0.2),
    block("home_advantage_def", k, rep(0.7, k), 0.2),
    block("home_advantage_tot", k, rep(1.8, k), 0.3),
    block("goals1_pred", n_pred, rep(24, n_pred), 4),
    block("goals2_pred", n_pred, rep(22, n_pred), 4),
    block("lp__", 1L, -1234, 5)
  )
  df <- as.data.frame(do.call(cbind, mats), check.names = FALSE)
  posterior::as_draws_df(df)
}

#' Prepare data once, then build a stub sized from that exact `pred_d`.
#'
#' @return `list(fit =, prep =)`.
local_stub_2dt <- function(league, sex, end_date = FIXTURE_END_DATE, root,
                           n_draws = FIXTURE_N_DRAWS, constants = list()) {
  prep <- prepare_data(league, sex, end_date = end_date, root = root)
  stopifnot(nrow(prep$pred_d) > 0L)
  list(
    fit = stub_fit(stub_2dt_draws(
      teams = prep$teams$team,
      n_pred = nrow(prep$pred_d),
      n_draws = n_draws,
      constants = constants
    )),
    prep = prep
  )
}
```

- [ ] **Step 4: Run, expect PASS.**

```bash
Rscript -e 'devtools::load_all("/Users/brynjolfurjonsson/sports", quiet = TRUE); testthat::test_file("/Users/brynjolfurjonsson/sports/tests/testthat/test-stub-fit.R")'
```

Expect `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 16 ]`.

- [ ] **Step 5: Commit.**

```bash
git -C /Users/brynjolfurjonsson/sports add tests/testthat/helper-stub-fit.R tests/testthat/test-stub-fit.R
git -C /Users/brynjolfurjonsson/sports commit -m "test(fixtures): stub_fit() replaces the 300MB fit.rds gate

The six extract tests have never executed because they gate on a gitignored
fit.rds. The 2DT code touches exactly one method -- fit\$draws(var) -- so a
closure over a 50-draw draws_df is a complete substitute. local_stub_2dt()
sizes goals*_pred from prepare_data()'s own pred_d so the N_pred mismatch
that makes .compute_posterior_goals_2dt warn and return zero rows cannot
happen silently.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 4: Repoint `test-extract-basketball-iceland.R` at the harness (delete 3 skip gates)

**Files:**
- Modify `/Users/brynjolfurjonsson/sports/tests/testthat/test-extract-basketball-iceland.R` (full rewrite of lines 1-132)

**Interfaces:**
- Consumes: `fixture_facts_root()`, `local_stub_2dt()`, `FIXTURE_FIT_DATE`, `FIXTURE_END_DATE`, `extract_basketball_iceland()`.
- Produces: nothing.

- [ ] **Step 1: Rewrite the file in full.** All three blocks keep their intent; the `skip_if_not(file.exists(... fit.rds))` gates and the `readRDS()` calls are gone. The 5-file assertion uses `%in%` so WS8 adding `fit_meta.parquet` / `round_strengths_quantiles.parquet` will not break it.

```r
# /Users/brynjolfurjonsson/sports/tests/testthat/test-extract-basketball-iceland.R
#
# Driven by the committed facts fixture + stub_fit(). These assertions never ran
# before: they gated on data/beliefs/fits/sport=basketball/.../fit.rds, a
# gitignored 300-600 MB artefact that CI never has.

extract_bb_fixture <- function(sex, env = parent.frame()) {
  root <- fixture_facts_root(env = env)
  league <- load_leagues()[["basketball_iceland"]]
  st <- suppressMessages(local_stub_2dt(league, sex, root = root))
  extracts_root <- file.path(withr::local_tempdir(.local_envir = env), "extracts")
  list(
    root = root, league = league, st = st, extracts_root = extracts_root,
    partition = file.path(
      extracts_root, "sport=basketball", "country=iceland",
      paste0("sex=", sex), paste0("fit_date=", format(FIXTURE_FIT_DATE, "%Y-%m-%d"))
    )
  )
}

test_that("extract_basketball_iceland writes the 5 expected parquets", {
  f <- extract_bb_fixture("male")

  extract_basketball_iceland(
    fit = f$st$fit,
    league = f$league,
    sex = "male",
    fit_date = FIXTURE_FIT_DATE,
    end_date = FIXTURE_END_DATE,
    root = f$root,
    extracts_root = f$extracts_root,
    prep = f$st$prep
  )

  expected_files <- c(
    "predicted_matches.parquet",
    "team_strengths_quantiles.parquet",
    "home_advantage_quantiles.parquet",
    "final_positions.parquet",
    "points_distribution.parquet"
  )
  written <- list.files(f$partition)
  for (fl in expected_files) {
    expect_true(fl %in% written, info = paste("missing", fl))
  }

  pm <- arrow::read_parquet(file.path(f$partition, "predicted_matches.parquet"))
  expect_gt(nrow(pm), 0L)
  expect_true(all(c(
    "game_nr", "match_date", "division", "home_team", "away_team",
    "mean_home_goals", "mean_away_goals", "mean_goal_diff",
    "p_home_win", "p_draw", "p_away_win", "goal_diff_distribution"
  ) %in% names(pm)))
  # Basketball has no ties.
  expect_true(all(pm$p_draw == 0))
})

test_that("extracted team_strengths_quantiles covers the 9-cell grid", {
  f <- extract_bb_fixture("male")

  extract_basketball_iceland(
    fit = f$st$fit,
    league = f$league,
    sex = "male",
    fit_date = FIXTURE_FIT_DATE,
    end_date = FIXTURE_END_DATE,
    root = f$root,
    extracts_root = f$extracts_root,
    prep = f$st$prep
  )

  ts <- arrow::read_parquet(
    file.path(f$partition, "team_strengths_quantiles.parquet")
  )
  expect_true(all(c("team", "component", "location", "quantile", "value") %in% names(ts)))
  expect_setequal(unique(ts$component), c("offence", "defence", "total"))
  expect_setequal(unique(ts$location), c("home", "away", "avg"))
  expect_setequal(unique(ts$quantile), seq_len(99L))
  # Only the current top division (BD) is summarised.
  expect_setequal(
    unique(ts$team),
    fixture_division_teams("basketball", "male", "BD")
  )
})

test_that("extract_basketball_iceland is idempotent (rerun overwrites cleanly)", {
  f <- extract_bb_fixture("female")

  args <- list(
    fit = f$st$fit, league = f$league, sex = "female",
    fit_date = FIXTURE_FIT_DATE,
    end_date = FIXTURE_END_DATE,
    root = f$root,
    extracts_root = f$extracts_root,
    prep = f$st$prep
  )
  do.call(extract_basketball_iceland, args)
  target <- file.path(f$partition, "team_strengths_quantiles.parquet")
  size_before <- file.info(target)$size
  digest_before <- digest::digest(file = target)
  do.call(extract_basketball_iceland, args)
  expect_equal(file.info(target)$size, size_before)
  expect_equal(digest::digest(file = target), digest_before)
})
```

- [ ] **Step 2: Run, expect PASS with SKIP 0.**

```bash
Rscript -e 'devtools::load_all("/Users/brynjolfurjonsson/sports", quiet = TRUE); testthat::test_file("/Users/brynjolfurjonsson/sports/tests/testthat/test-extract-basketball-iceland.R")'
```

Expect `SKIP 0` and `FAIL 0`. If `digest` is not an installed dependency the run
errors with `there is no package called 'digest'` — in that case add it:

```bash
grep -n "Suggests" -A 20 /Users/brynjolfurjonsson/sports/DESCRIPTION
```

- [ ] **Step 3: Commit.**

```bash
git -C /Users/brynjolfurjonsson/sports add tests/testthat/test-extract-basketball-iceland.R
git -C /Users/brynjolfurjonsson/sports commit -m "test(extract): run the basketball extract tests for the first time

All three blocks gated on a gitignored 300-600MB fit.rds, so this coverage
has never executed -- which is how the 2DT extractor reached production
unexercised. Driving them from stub_fit() + the facts fixture removes the
gate entirely rather than leaving three permanent skips.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 5: Repoint `test-extract-handball-iceland.R` at the harness (delete 3 skip gates)

**Files:**
- Modify `/Users/brynjolfurjonsson/sports/tests/testthat/test-extract-handball-iceland.R` (full rewrite of lines 1-130)

**Interfaces:**
- Consumes: same as Task 4, plus `extract_handball_iceland()`.
- Produces: nothing.

- [ ] **Step 1: Rewrite the file in full.** Handball's top division is `OD`, ties are on, buckets are 2-point over [-20, 20].

```r
# /Users/brynjolfurjonsson/sports/tests/testthat/test-extract-handball-iceland.R
#
# Driven by the committed facts fixture + stub_fit(). Previously gated on
# data/beliefs/fits/sport=handball/.../fit.rds and therefore never executed.

extract_hb_fixture <- function(sex, env = parent.frame()) {
  root <- fixture_facts_root(env = env)
  league <- load_leagues()[["handball_iceland"]]
  st <- suppressMessages(local_stub_2dt(league, sex, root = root))
  extracts_root <- file.path(withr::local_tempdir(.local_envir = env), "extracts")
  list(
    root = root, league = league, st = st, extracts_root = extracts_root,
    partition = file.path(
      extracts_root, "sport=handball", "country=iceland",
      paste0("sex=", sex), paste0("fit_date=", format(FIXTURE_FIT_DATE, "%Y-%m-%d"))
    )
  )
}

test_that("extract_handball_iceland writes the 5 expected parquets", {
  f <- extract_hb_fixture("male")

  extract_handball_iceland(
    fit = f$st$fit,
    league = f$league,
    sex = "male",
    fit_date = FIXTURE_FIT_DATE,
    end_date = FIXTURE_END_DATE,
    root = f$root,
    extracts_root = f$extracts_root,
    prep = f$st$prep
  )

  expected_files <- c(
    "predicted_matches.parquet",
    "team_strengths_quantiles.parquet",
    "home_advantage_quantiles.parquet",
    "final_positions.parquet",
    "points_distribution.parquet"
  )
  written <- list.files(f$partition)
  for (fl in expected_files) {
    expect_true(fl %in% written, info = paste("missing", fl))
  }

  pm <- arrow::read_parquet(file.path(f$partition, "predicted_matches.parquet"))
  expect_gt(nrow(pm), 0L)
  expect_true(all(c(
    "game_nr", "match_date", "division", "home_team", "away_team",
    "mean_home_goals", "mean_away_goals", "mean_goal_diff",
    "p_home_win", "p_draw", "p_away_win", "goal_diff_distribution"
  ) %in% names(pm)))
  # Handball has ties, so the three outcome probabilities must sum to 1.
  expect_equal(
    pm$p_home_win + pm$p_draw + pm$p_away_win,
    rep(1, nrow(pm)),
    tolerance = 1e-9
  )
})

test_that("handball extracted team_strengths_quantiles covers the 9-cell grid", {
  f <- extract_hb_fixture("male")

  extract_handball_iceland(
    fit = f$st$fit,
    league = f$league,
    sex = "male",
    fit_date = FIXTURE_FIT_DATE,
    end_date = FIXTURE_END_DATE,
    root = f$root,
    extracts_root = f$extracts_root,
    prep = f$st$prep
  )

  ts <- arrow::read_parquet(
    file.path(f$partition, "team_strengths_quantiles.parquet")
  )
  expect_true(all(c("team", "component", "location", "quantile", "value") %in% names(ts)))
  expect_setequal(unique(ts$component), c("offence", "defence", "total"))
  expect_setequal(unique(ts$location), c("home", "away", "avg"))
  expect_setequal(unique(ts$quantile), seq_len(99L))
  expect_setequal(
    unique(ts$team),
    fixture_division_teams("handball", "male", "OD")
  )
})

test_that("extract_handball_iceland is idempotent (rerun overwrites cleanly)", {
  f <- extract_hb_fixture("female")

  args <- list(
    fit = f$st$fit, league = f$league, sex = "female",
    fit_date = FIXTURE_FIT_DATE,
    end_date = FIXTURE_END_DATE,
    root = f$root,
    extracts_root = f$extracts_root,
    prep = f$st$prep
  )
  do.call(extract_handball_iceland, args)
  target <- file.path(f$partition, "team_strengths_quantiles.parquet")
  size_before <- file.info(target)$size
  digest_before <- digest::digest(file = target)
  do.call(extract_handball_iceland, args)
  expect_equal(file.info(target)$size, size_before)
  expect_equal(digest::digest(file = target), digest_before)
})
```

- [ ] **Step 2: Run, expect PASS with SKIP 0.**

```bash
Rscript -e 'devtools::load_all("/Users/brynjolfurjonsson/sports", quiet = TRUE); testthat::test_file("/Users/brynjolfurjonsson/sports/tests/testthat/test-extract-handball-iceland.R")'
```

- [ ] **Step 3: Commit.**

```bash
git -C /Users/brynjolfurjonsson/sports add tests/testthat/test-extract-handball-iceland.R
git -C /Users/brynjolfurjonsson/sports commit -m "test(extract): run the handball extract tests for the first time

Same gate as basketball. The tie-probability assertion is new: handball's
p_home_win/p_draw/p_away_win must sum to 1, which no test has ever checked
because the file could not run.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 6: Rewrite `test-publish-basketball.R` (delete 5 skip gates, keep the 2 live blocks)

**Files:**
- Modify `/Users/brynjolfurjonsson/sports/tests/testthat/test-publish-basketball.R` (full rewrite of lines 1-224)

**Interfaces:**
- Consumes: `fixture_facts_root()`, `local_stub_2dt()`, `publish_basketball_iceland()`.
- Produces: nothing.

`publish_basketball_iceland()` (R/publish-basketball-iceland.R:38) calls
`prepare_data()` **internally** at the default `schedule_horizon_days = 14L` and
accepts no `prep=`. `local_stub_2dt()` therefore sizes the stub from a
`prepare_data()` call with the *same* defaults, and each test re-asserts that
`p_home + p_away` is non-degenerate so a silent zero-row fallback cannot pass.

- [ ] **Step 1: Rewrite the file in full.**

```r
# /Users/brynjolfurjonsson/sports/tests/testthat/test-publish-basketball.R
#
# Fixture-driven. The previous version located a fit.rds under a machine-local
# absolute path (/Users/.../sports-backup-20260424-163153) and skipped when it
# was absent -- which was always, on CI.

publish_bb_fixture <- function(sex, env = parent.frame()) {
  root <- fixture_facts_root(env = env)
  league <- load_leagues()[["basketball_iceland"]]
  st <- suppressMessages(local_stub_2dt(league, sex, root = root))
  out <- withr::local_tempdir(.local_envir = env)
  suppressMessages(publish_basketball_iceland(
    st$fit, league,
    sex = sex,
    end_date = FIXTURE_END_DATE,
    root = root,
    output_root = out
  ))
  list(
    root = root, league = league, st = st, out = out,
    out_dir = file.path(
      out, "basketball", "iceland",
      if (sex == "male") "karla" else "kvenna"
    )
  )
}

test_that("publish_basketball_iceland produces 2 JSONs for male", {
  f <- publish_bb_fixture("male")

  for (fl in c("meta.json", "next_games.json")) {
    expect_true(file.exists(file.path(f$out_dir, fl)), info = paste("missing:", fl))
  }

  meta <- jsonlite::read_json(file.path(f$out_dir, "meta.json"))
  expect_equal(meta$sport, "basketball")
  expect_equal(meta$sex, "male")
  expect_type(meta$n_draws, "integer")
  expect_equal(meta$n_draws, FIXTURE_N_DRAWS)
  expect_equal(meta$season, 2100L)

  ng <- jsonlite::read_json(file.path(f$out_dir, "next_games.json"))
  expect_true("matches" %in% names(ng))
  expect_true("generated_at" %in% names(ng))
  # The n_pred trap: a stub sized off the wrong horizon makes
  # .compute_posterior_goals_2dt warn and return zero rows, which would show
  # up here as an empty matches array.
  expect_gt(length(ng$matches), 0L)
})

test_that("publish_basketball_iceland produces 2 JSONs for female", {
  f <- publish_bb_fixture("female")

  for (fl in c("meta.json", "next_games.json")) {
    expect_true(file.exists(file.path(f$out_dir, fl)), info = paste("missing:", fl))
  }

  meta <- jsonlite::read_json(file.path(f$out_dir, "meta.json"))
  expect_equal(meta$sport, "basketball")
  expect_equal(meta$sex, "female")
  expect_type(meta$n_draws, "integer")
  expect_equal(meta$season, 2100L)

  ng <- jsonlite::read_json(file.path(f$out_dir, "next_games.json"))
  expect_true("matches" %in% names(ng))
  expect_gt(length(ng$matches), 0L)
})

test_that("publish_basketball_iceland rejects wrong sport", {
  fake_fit <- structure(list(), class = "CmdStanMCMC")
  bad_league <- list(sport = "handball", country = "iceland")
  expect_error(
    publish_basketball_iceland(fake_fit, bad_league, sex = "male"),
    "basketball"
  )
})

test_that("publish_basketball_iceland rejects invalid sex", {
  fake_fit <- structure(list(), class = "CmdStanMCMC")
  league <- list(sport = "basketball", country = "iceland")
  expect_error(
    publish_basketball_iceland(fake_fit, league, sex = "other"),
    "male.*female|female.*male"
  )
})

test_that("publish_basketball_iceland emits the 7 publish surface files (male)", {
  f <- publish_bb_fixture("male")

  expected <- c(
    "meta.json", "next_games.json", "standings.json",
    "team_strengths.json", "final_positions.json",
    "points_distribution.json", "home_advantage.json"
  )
  for (fl in expected) {
    expect_true(file.exists(file.path(f$out_dir, fl)), info = paste("missing:", fl))
  }

  st <- jsonlite::read_json(file.path(f$out_dir, "standings.json"))
  expect_true(all(c("generated_at", "season", "as_of", "rows") %in% names(st)))
  expect_length(st$rows, length(fixture_division_teams("basketball", "male", "BD")))

  ts <- jsonlite::read_json(file.path(f$out_dir, "team_strengths.json"))
  expect_true(all(c("generated_at", "records") %in% names(ts)))
  expect_gt(length(ts$records), 0L)
  components <- unique(vapply(ts$records, \(r) r$component, character(1)))
  locations <- unique(vapply(ts$records, \(r) r$location, character(1)))
  expect_setequal(components, c("offence", "defence", "total"))
  expect_setequal(locations, c("home", "away", "avg"))

  fp <- jsonlite::read_json(file.path(f$out_dir, "final_positions.json"))
  expect_true(all(c("generated_at", "season", "n_teams", "records", "summary") %in% names(fp)))

  pd <- jsonlite::read_json(file.path(f$out_dir, "points_distribution.json"))
  expect_true(all(c("generated_at", "season", "records", "summary") %in% names(pd)))

  ha <- jsonlite::read_json(file.path(f$out_dir, "home_advantage.json"))
  expect_true(all(c("generated_at", "records") %in% names(ha)))
  expect_gt(length(ha$records), 0L)
})

test_that("publish_basketball_iceland: standings has draws=0 (no ties in basketball)", {
  f <- publish_bb_fixture("male")

  st <- jsonlite::read_json(file.path(f$out_dir, "standings.json"))
  expect_gt(length(st$rows), 0L)
  draws_vec <- vapply(st$rows, \(r) as.integer(r$draws %||% 0L), integer(1))
  expect_true(all(draws_vec == 0L))
})

test_that("publish_basketball_iceland: female next_games schema uses {p_home, p_away}", {
  f <- publish_bb_fixture("female")

  ng_path <- file.path(f$out_dir, "next_games.json")
  expect_true(file.exists(ng_path))
  ng <- jsonlite::read_json(ng_path)
  expect_true("matches" %in% names(ng))
  expect_gt(length(ng$matches), 0L)
  m1 <- ng$matches[[1]]
  expect_true(all(c("p_home", "p_away") %in% names(m1)))
})
```

- [ ] **Step 2: Run, expect SKIP 0.**

```bash
Rscript -e 'devtools::load_all("/Users/brynjolfurjonsson/sports", quiet = TRUE); testthat::test_file("/Users/brynjolfurjonsson/sports/tests/testthat/test-publish-basketball.R")'
```

Expect `SKIP 0` (was `SKIP 5`). Any failure here is an honest finding about the
publisher on synthetic data, not about the harness — record it before changing
the fixture.

- [ ] **Step 3: Commit.**

```bash
git -C /Users/brynjolfurjonsson/sports add tests/testthat/test-publish-basketball.R
git -C /Users/brynjolfurjonsson/sports commit -m "test(publish): un-gate the basketball publisher tests

Five of seven blocks skipped on a machine-local backup path that exists on
exactly one laptop. They now run against the committed facts fixture and a
stub fit, and assert next_games is non-empty so the N_pred mismatch that
empties every posterior-dependent JSON cannot pass silently.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 7: Rewrite `test-publish-handball.R` (delete 3 skip gates, keep the 2 live blocks)

**Files:**
- Modify `/Users/brynjolfurjonsson/sports/tests/testthat/test-publish-handball.R` (full rewrite of lines 1-131)

**Interfaces:**
- Consumes: `fixture_facts_root()`, `local_stub_2dt()`, `publish_handball_iceland()`.
- Produces: nothing.

- [ ] **Step 1: Rewrite the file in full.**

```r
# /Users/brynjolfurjonsson/sports/tests/testthat/test-publish-handball.R
#
# Fixture-driven. The previous version located a fit.rds under a machine-local
# absolute path (/Users/.../sports-backup-20260424-163153) and skipped when it
# was absent -- which was always, on CI.

publish_hb_fixture <- function(sex, env = parent.frame()) {
  root <- fixture_facts_root(env = env)
  league <- load_leagues()[["handball_iceland"]]
  st <- suppressMessages(local_stub_2dt(league, sex, root = root))
  out <- withr::local_tempdir(.local_envir = env)
  suppressMessages(publish_handball_iceland(
    st$fit, league,
    sex = sex,
    end_date = FIXTURE_END_DATE,
    root = root,
    output_root = out
  ))
  list(
    root = root, league = league, st = st, out = out,
    out_dir = file.path(
      out, "handball", "iceland",
      if (sex == "male") "karla" else "kvenna"
    )
  )
}

test_that("publish_handball_iceland produces 2 JSONs for male", {
  f <- publish_hb_fixture("male")

  for (fl in c("meta.json", "next_games.json")) {
    expect_true(file.exists(file.path(f$out_dir, fl)), info = paste("missing:", fl))
  }

  meta <- jsonlite::read_json(file.path(f$out_dir, "meta.json"))
  expect_equal(meta$sport, "handball")
  expect_equal(meta$sex, "male")
  expect_type(meta$n_draws, "integer")
  expect_equal(meta$n_draws, FIXTURE_N_DRAWS)
  expect_equal(meta$season, 2100L)

  ng <- jsonlite::read_json(file.path(f$out_dir, "next_games.json"))
  expect_true("matches" %in% names(ng))
  expect_true("generated_at" %in% names(ng))
  expect_gt(length(ng$matches), 0L)
})

test_that("publish_handball_iceland produces 2 JSONs for female", {
  f <- publish_hb_fixture("female")

  for (fl in c("meta.json", "next_games.json")) {
    expect_true(file.exists(file.path(f$out_dir, fl)), info = paste("missing:", fl))
  }

  meta <- jsonlite::read_json(file.path(f$out_dir, "meta.json"))
  expect_equal(meta$sport, "handball")
  expect_equal(meta$sex, "female")
  expect_type(meta$n_draws, "integer")
  expect_equal(meta$season, 2100L)

  ng <- jsonlite::read_json(file.path(f$out_dir, "next_games.json"))
  expect_true("matches" %in% names(ng))
  expect_gt(length(ng$matches), 0L)
})

test_that("publish_handball_iceland rejects wrong sport", {
  fake_fit <- structure(list(), class = "CmdStanMCMC")
  bad_league <- list(sport = "basketball", country = "iceland")
  expect_error(
    publish_handball_iceland(fake_fit, bad_league, sex = "male"),
    "handball"
  )
})

test_that("publish_handball_iceland rejects invalid sex", {
  fake_fit <- structure(list(), class = "CmdStanMCMC")
  league <- list(sport = "handball", country = "iceland")
  expect_error(
    publish_handball_iceland(fake_fit, league, sex = "other"),
    "male.*female|female.*male"
  )
})

test_that("publish_handball_iceland emits the 7 publish surface files (male)", {
  f <- publish_hb_fixture("male")

  expected <- c(
    "meta.json", "next_games.json", "standings.json",
    "team_strengths.json", "final_positions.json",
    "points_distribution.json", "home_advantage.json"
  )
  for (fl in expected) {
    expect_true(file.exists(file.path(f$out_dir, fl)), info = paste("missing:", fl))
  }

  st <- jsonlite::read_json(file.path(f$out_dir, "standings.json"))
  expect_true(all(c("generated_at", "season", "as_of", "rows") %in% names(st)))
  expect_length(st$rows, length(fixture_division_teams("handball", "male", "OD")))

  ts <- jsonlite::read_json(file.path(f$out_dir, "team_strengths.json"))
  expect_true(all(c("generated_at", "records") %in% names(ts)))
  expect_gt(length(ts$records), 0L)
  components <- unique(vapply(ts$records, \(r) r$component, character(1)))
  locations <- unique(vapply(ts$records, \(r) r$location, character(1)))
  expect_setequal(components, c("offence", "defence", "total"))
  expect_setequal(locations, c("home", "away", "avg"))

  fp <- jsonlite::read_json(file.path(f$out_dir, "final_positions.json"))
  expect_true(all(c("generated_at", "season", "n_teams", "records", "summary") %in% names(fp)))

  pd <- jsonlite::read_json(file.path(f$out_dir, "points_distribution.json"))
  expect_true(all(c("generated_at", "season", "records", "summary") %in% names(pd)))

  ha <- jsonlite::read_json(file.path(f$out_dir, "home_advantage.json"))
  expect_true(all(c("generated_at", "records") %in% names(ha)))
  expect_gt(length(ha$records), 0L)
})
```

- [ ] **Step 2: Run, expect SKIP 0.**

```bash
Rscript -e 'devtools::load_all("/Users/brynjolfurjonsson/sports", quiet = TRUE); testthat::test_file("/Users/brynjolfurjonsson/sports/tests/testthat/test-publish-handball.R")'
```

Expect `SKIP 0` (was `SKIP 3`).

- [ ] **Step 3: Commit.**

```bash
git -C /Users/brynjolfurjonsson/sports add tests/testthat/test-publish-handball.R
git -C /Users/brynjolfurjonsson/sports commit -m "test(publish): un-gate the handball publisher tests

Three of five blocks skipped on the same machine-local backup path. Now
fixture-driven, with a standings row-count assertion pinned to the fixture's
OD team list.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 8: Commit the 2DT extracts fixture tree (generated by the real extractor)

**Files:**
- Modify `/Users/brynjolfurjonsson/sports/tools/make-extract-fixtures.R` (add `.write_2dt_extract_fixtures()`)
- Create `/Users/brynjolfurjonsson/sports/tests/testthat/fixtures/extracts/sport=basketball/country=iceland/sex={male,female}/fit_date=2100-01-01/*.parquet`
- Create `/Users/brynjolfurjonsson/sports/tests/testthat/fixtures/extracts/sport=handball/country=iceland/sex={male,female}/fit_date=2100-01-01/*.parquet`
- Create `/Users/brynjolfurjonsson/sports/tests/testthat/helper-extract-fixtures.R`
- Modify `/Users/brynjolfurjonsson/sports/tests/testthat/test-fixture-harness.R` (append)

**Interfaces:**
- Consumes: `extract_basketball_iceland()`, `extract_handball_iceland()`, `local_stub_2dt()`, `fixture_facts_root()`.
- Produces: `fixture_extracts_root(sports = c("basketball", "handball"), env = parent.frame())`.

The committed tree holds **exactly the 5 parquets the 2DT extractor emits today**
(`R/extract-iceland-2dt-shared.R:302`, comment at `:400`). `fit_meta.parquet` and
`round_strengths_quantiles.parquet` are WS8's to add; the contract test uses
`%in%` so WS8's two extra files extend the tree without breaking this task.

- [ ] **Step 1: Append the failing contract test** to `test-fixture-harness.R`:

```r
test_that("the committed 2DT extracts fixture has the 5-parquet contract", {
  base <- testthat::test_path("fixtures", "extracts")
  stamp <- paste0("fit_date=", format(FIXTURE_FIT_DATE, "%Y-%m-%d"))

  five <- c(
    "predicted_matches.parquet", "team_strengths_quantiles.parquet",
    "home_advantage_quantiles.parquet", "final_positions.parquet",
    "points_distribution.parquet"
  )
  cols <- list(
    team_strengths_quantiles = c("team", "component", "location", "quantile", "value"),
    home_advantage_quantiles = c("team", "component", "quantile", "value"),
    final_positions = c("team", "placement", "probability"),
    points_distribution = c("team", "points", "probability")
  )

  for (sport in c("basketball", "handball")) {
    for (sex in c("male", "female")) {
      part <- file.path(
        base, paste0("sport=", sport), "country=iceland",
        paste0("sex=", sex), stamp
      )
      expect_true(dir.exists(part), info = part)
      # `%in%`, not setequal: WS8 adds fit_meta + round_strengths_quantiles.
      expect_true(all(five %in% list.files(part)), info = part)

      for (ft in names(cols)) {
        df <- arrow::read_parquet(file.path(part, paste0(ft, ".parquet")))
        expect_true(all(cols[[ft]] %in% names(df)), info = paste(part, ft))
        expect_gt(nrow(df), 0L)
      }
    }
  }
})

test_that("the committed extracts fixture stays inside the 250 KB budget", {
  files <- list.files(
    testthat::test_path("fixtures", "extracts"),
    recursive = TRUE, full.names = TRUE
  )
  expect_gt(length(files), 0L)
  expect_lt(sum(file.info(files)$size), 250L * 1024L)
})

test_that("fixture_extracts_root materialises a readable tree", {
  root <- fixture_extracts_root()
  part <- file.path(
    root, "sport=handball", "country=iceland", "sex=female",
    paste0("fit_date=", format(FIXTURE_FIT_DATE, "%Y-%m-%d"))
  )
  expect_true(dir.exists(part))
  fp <- arrow::read_parquet(file.path(part, "final_positions.parquet"))
  expect_equal(
    sum(fp$probability),
    length(unique(fp$team)),
    tolerance = 1e-8
  )
})
```

- [ ] **Step 2: Run, expect RED.**

```bash
Rscript -e 'devtools::load_all("/Users/brynjolfurjonsson/sports", quiet = TRUE); testthat::test_file("/Users/brynjolfurjonsson/sports/tests/testthat/test-fixture-harness.R")'
```

Exact expected failures: `Failure: dir.exists(part) is not TRUE` (fixtures/extracts absent),
`Failure: length(files) is not strictly more than 0`, and
`Error: could not find function "fixture_extracts_root"`.

- [ ] **Step 3: Add the generator step.** Insert into `make-extract-fixtures.R` above `make_extract_fixtures()`:

```r
# Generate the committed 2DT extracts tree by running the REAL extractors
# against a stub fit -- so the fixture's schema is the extractor's own output,
# not a hand-written guess that can drift from it.
#' @noRd
.write_2dt_extract_fixtures <- function(dest, facts_root) {
  extracts_root <- file.path(dest, "extracts")
  unlink(file.path(extracts_root, "sport=basketball"), recursive = TRUE)
  unlink(file.path(extracts_root, "sport=handball"), recursive = TRUE)

  cfg <- list(
    basketball = list(key = "basketball_iceland", fn = extract_basketball_iceland),
    handball   = list(key = "handball_iceland", fn = extract_handball_iceland)
  )
  leagues <- load_leagues()
  for (sport in names(cfg)) {
    for (sex in c("male", "female")) {
      league <- leagues[[cfg[[sport]]$key]]
      prep <- prepare_data(league, sex, end_date = FIXTURE_END_DATE, root = facts_root)
      fit <- stub_fit(stub_2dt_draws(
        prep$teams$team, nrow(prep$pred_d), n_draws = FIXTURE_N_DRAWS
      ))
      cfg[[sport]]$fn(
        fit = fit, league = league, sex = sex,
        fit_date = FIXTURE_FIT_DATE,
        end_date = FIXTURE_END_DATE,
        root = facts_root,
        extracts_root = extracts_root,
        prep = prep
      )
    }
  }
  list.files(extracts_root, recursive = TRUE, full.names = TRUE)
}
```

and extend `make_extract_fixtures()` after the facts writes:

```r
  facts_root <- file.path(tempdir(), paste0("fixture-facts-", Sys.getpid()))
  unlink(facts_root, recursive = TRUE)
  dir.create(facts_root, recursive = TRUE, showWarnings = FALSE)
  write_table(.fixture_results(), "results", root = facts_root)
  write_table(.fixture_schedules(), "schedules", root = facts_root)

  extract_files <- .write_2dt_extract_fixtures(dest, facts_root)
  files <- c(files, extract_files)
  bytes <- sum(file.info(files)$size)
  if (!quiet) {
    message(sprintf(
      "make_extract_fixtures: %d files, %s KB (extracts tree: %s KB / 250 KB budget)",
      length(files), format(round(bytes / 1024, 1)),
      format(round(sum(file.info(extract_files)$size) / 1024, 1))
    ))
  }
```

`stub_fit()` / `stub_2dt_draws()` live in the test helper, so the script must
source it before use — add near the top of `make_extract_fixtures()`:

```r
  helper <- file.path(dirname(dest), "helper-stub-fit.R")
  if (file.exists(helper)) sys.source(helper, envir = environment())
```

- [ ] **Step 4: Write the extracts helper.**

```r
# /Users/brynjolfurjonsson/sports/tests/testthat/helper-extract-fixtures.R
#
# Materialise the committed extracts fixture into a temp tree so tests that
# publish from it can write alongside without dirtying the repo.

#' Copy the committed 2DT extracts partitions into a temp extracts root.
fixture_extracts_root <- function(sports = c("basketball", "handball"),
                                  env = parent.frame()) {
  tmp <- file.path(withr::local_tempdir(.local_envir = env), "extracts")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  src <- testthat::test_path("fixtures", "extracts")
  for (sport in sports) {
    from <- file.path(src, paste0("sport=", sport))
    if (dir.exists(from)) {
      file.copy(from, tmp, recursive = TRUE)
    }
  }
  tmp
}
```

- [ ] **Step 5: Generate, then run.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript tools/make-extract-fixtures.R
Rscript -e 'devtools::load_all("/Users/brynjolfurjonsson/sports", quiet = TRUE); testthat::test_file("/Users/brynjolfurjonsson/sports/tests/testthat/test-fixture-harness.R")'
```

Read the generator's printed `extracts tree: N KB / 250 KB budget` line. The
size is driven by the 99-quantile bands over the top-division team count: if the
tree exceeds 250 KB, drop `BD`/`OD` from 6 teams to 4 in `FIXTURE_DIVISIONS`
(both in `tools/make-extract-fixtures.R` and `helper-fixture-facts.R`, which must
stay identical), re-run the generator and re-run Tasks 4-7's files.

- [ ] **Step 6: Commit.**

```bash
git -C /Users/brynjolfurjonsson/sports add tools/make-extract-fixtures.R tests/testthat/helper-extract-fixtures.R tests/testthat/fixtures/extracts tests/testthat/test-fixture-harness.R
git -C /Users/brynjolfurjonsson/sports commit -m "test(fixtures): commit the 2DT extracts tree, generated by the extractor itself

The fixture is the extractor's own output against a stub fit, so its schema
cannot drift from the code the way a hand-written table would. Exactly the 5
parquets shipped today -- fit_meta and round_strengths_quantiles are WS8's to
add, and the contract test uses %in% so adding them is non-breaking.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 9: Football golden-file assertion (spec §4 item 4) — the safety net for WS9

**Files:**
- Modify `/Users/brynjolfurjonsson/sports/tests/testthat/helper-extract-fixtures.R` (add `build_football_extracts_fixture()`, `publish_json_digest()`)
- Modify `/Users/brynjolfurjonsson/sports/tools/make-extract-fixtures.R` (add `make_football_golden_hashes()`)
- Create `/Users/brynjolfurjonsson/sports/tests/testthat/fixtures/golden/football-publish-hashes.csv`
- Create `/Users/brynjolfurjonsson/sports/tests/testthat/test-publish-football-golden.R`

**Interfaces:**
- Consumes: `read_extracted_football()`, `publish_football_iceland()`, `.football_iceland_division_codes(sex)`, `fixture_facts_root()`.
- Produces: `build_football_extracts_fixture(facts_root, extracts_root, sex, fit_date = FIXTURE_FIT_DATE)`, `publish_json_digest(path)`, `make_football_golden_hashes(dest = NULL)`.

No workstream currently owns this assertion, and WS9 deletes the two 2DT
publishers and rewrites the football one — so it must exist first.

**Why football's partition is built at test time rather than committed:**
football publishes BD(12)+LD1(6)+LD2(6)+LD3(6)+CUP(4) per sex; the 99-quantile
band over 34 teams x 9 strength cells is ~30k float64 rows for
`team_strengths_quantiles` alone (~240 KB raw) before `round_strengths_quantiles`,
which is an order of magnitude larger. That blows the 250 KB budget on its own,
so the football partition is a deterministic pure function of the committed facts
fixture instead, and only the golden hash manifest is committed.

- [ ] **Step 1: Write the failing golden test.**

```r
# /Users/brynjolfurjonsson/sports/tests/testthat/test-publish-football-golden.R
#
# Safety net for the WS9 refactor: publishing football from a PINNED fixture
# must produce byte-identical JSON (modulo generated_at) before and after the
# two 2DT publishers are deleted and the football publisher is rewritten.

test_that("football publish output is byte-identical to the golden manifest", {
  golden_path <- testthat::test_path("fixtures", "golden", "football-publish-hashes.csv")
  expect_true(
    file.exists(golden_path),
    info = "regenerate with: Rscript tools/make-extract-fixtures.R --golden"
  )
  golden <- utils::read.csv(golden_path, stringsAsFactors = FALSE)

  facts_root <- fixture_facts_root()
  extracts_root <- file.path(withr::local_tempdir(), "extracts")
  out <- withr::local_tempdir()
  league <- load_leagues()[["football_iceland"]]

  for (sex in c("male", "female")) {
    build_football_extracts_fixture(facts_root, extracts_root, sex)
    extracted <- read_extracted_football(
      league, sex = sex, fit_date = FIXTURE_FIT_DATE, extracts_root = extracts_root
    )
    suppressMessages(publish_football_iceland(
      extracted = extracted, league = league, sex = sex,
      end_date = FIXTURE_END_DATE,
      root = facts_root,
      output_root = out,
      extracts_root = extracts_root,
      archive_root = file.path(withr::local_tempdir(), "archive")
    ))
  }

  produced <- list.files(
    file.path(out, "football"), pattern = "\\.json$",
    recursive = TRUE, full.names = TRUE
  )
  rel <- sub(paste0("^", out, "/"), "", produced)
  expect_setequal(rel, golden$file)

  actual <- vapply(produced, publish_json_digest, character(1), USE.NAMES = FALSE)
  names(actual) <- rel
  changed <- names(actual)[actual[golden$file] != golden$sha256]
  expect_equal(
    length(changed), 0L,
    info = paste("changed payloads:", paste(changed, collapse = ", "))
  )
})

test_that("the golden manifest covers every configured football cell", {
  golden <- utils::read.csv(
    testthat::test_path("fixtures", "golden", "football-publish-hashes.csv"),
    stringsAsFactors = FALSE
  )
  for (sex in c("male", "female")) {
    sex_folder <- if (sex == "male") "karla" else "kvenna"
    for (slug in .football_iceland_division_slugs(sex)) {
      prefix <- file.path("football", "iceland", paste0(sex_folder, "-", slug))
      expect_true(
        any(startsWith(golden$file, prefix)),
        info = prefix
      )
    }
  }
})
```

- [ ] **Step 2: Run, expect RED.**

Exact expected failure: `Failure: file.exists(golden_path) is not TRUE` with the
regenerate hint, then `Error in build_football_extracts_fixture(...) : could not
find function "build_football_extracts_fixture"`.

- [ ] **Step 3: Add the football fixture builder + digest to `helper-extract-fixtures.R`.**

```r
# Expand a (team, centre) seed into the 99-quantile band shape
# read_extracted_football() expects. Deterministic: no RNG.
.fixture_quantile_band <- function(keys, centre, scale = 0.4) {
  z <- stats::qnorm(seq(0.01, 0.99, by = 0.01))
  tidyr::expand_grid(!!!keys, quantile = seq_len(99L)) |>
    dplyr::mutate(value = round(centre[.data$team] + scale * z[.data$quantile], 4L))
}

#' Materialise a football extracts partition from the committed facts fixture.
#'
#' Football's 99-quantile bands over ~34 teams do not fit the 250 KB committed
#' budget, so the partition is rebuilt deterministically instead of stored.
build_football_extracts_fixture <- function(facts_root, extracts_root, sex,
                                            fit_date = FIXTURE_FIT_DATE) {
  divs <- .football_iceland_division_codes(sex)
  part <- file.path(
    extracts_root, "sport=football", "country=iceland",
    paste0("sex=", sex), paste0("fit_date=", format(fit_date, "%Y-%m-%d"))
  )
  dir.create(part, recursive = TRUE, showWarnings = FALSE)

  schedules <- read_table(
    "schedules", root = facts_root,
    filter = list(sport = "football", country = "iceland", sex = sex)
  )

  per_div <- lapply(divs, function(div) {
    teams <- fixture_division_teams("football", sex, div)
    centre <- stats::setNames(seq(1.2, -1.2, length.out = length(teams)), teams)
    grid <- tidyr::expand_grid(
      team = teams,
      component = c("offence", "defence", "total"),
      location = c("home", "away", "avg")
    )
    ts <- grid |>
      tidyr::expand_grid(quantile = seq_len(99L)) |>
      dplyr::mutate(
        value = round(
          centre[.data$team] + 0.4 * stats::qnorm(.data$quantile / 100),
          4L
        ),
        division = div
      )
    rs <- ts |>
      tidyr::expand_grid(round = 1:2) |>
      dplyr::mutate(value = round(.data$value + 0.05 * .data$round, 4L)) |>
      dplyr::select("round", "team", "component", "location", "quantile", "value", "division")
    ha <- tidyr::expand_grid(
      team = teams, component = c("offence", "defence", "total"),
      quantile = seq_len(99L)
    ) |>
      dplyr::mutate(
        value = round(0.15 + 0.05 * stats::qnorm(.data$quantile / 100), 4L),
        division = div
      )
    fp <- tidyr::expand_grid(team = teams, placement = seq_along(teams)) |>
      dplyr::mutate(probability = 1 / length(teams), division = div)
    pd <- tidyr::expand_grid(team = teams, points = seq.int(10L, 14L)) |>
      dplyr::mutate(probability = 0.2, division = div)

    sched <- schedules[schedules$division == div, , drop = FALSE]
    pm <- tidyr::expand_grid(
      idx = seq_len(nrow(sched)), home_goals = 0:3, away_goals = 0:3
    ) |>
      dplyr::mutate(
        home_team = sched$home_team[.data$idx],
        away_team = sched$away_team[.data$idx],
        match_date = sched$match_date[.data$idx],
        home_goals = as.integer(.data$home_goals),
        away_goals = as.integer(.data$away_goals),
        count = as.integer(FIXTURE_N_DRAWS / 16L),
        division = div
      ) |>
      dplyr::select("home_team", "away_team", "match_date", "home_goals",
                    "away_goals", "count", "division")
    tp <- tibble::tibble(
      team = teams, round_name = "winner",
      probability = 1 / length(teams), division = div
    )
    list(
      predicted_matches = pm, team_strengths_quantiles = ts,
      round_strengths_quantiles = rs, home_advantage_quantiles = ha,
      final_positions = fp, points_distribution = pd,
      tournament_placements = tp
    )
  })

  for (ft in names(per_div[[1]])) {
    arrow::write_parquet(
      dplyr::bind_rows(lapply(per_div, function(d) d[[ft]])),
      file.path(part, paste0(ft, ".parquet"))
    )
  }
  invisible(NULL)
}

# Recursively drop every `generated_at` key, then hash the canonical
# serialisation -- "byte-identical modulo generated_at".
.strip_generated_at <- function(x) {
  if (!is.list(x)) return(x)
  x[names(x) == "generated_at"] <- NULL
  lapply(x, .strip_generated_at)
}

publish_json_digest <- function(path) {
  payload <- .strip_generated_at(
    jsonlite::read_json(path, simplifyVector = FALSE)
  )
  digest::digest(
    jsonlite::toJSON(payload, auto_unbox = TRUE, digits = NA, null = "null"),
    algo = "sha256"
  )
}
```

- [ ] **Step 4: Add the manifest generator to `tools/make-extract-fixtures.R`.**

```r
#' Regenerate tests/testthat/fixtures/golden/football-publish-hashes.csv.
#' @noRd
make_football_golden_hashes <- function(dest = NULL) {
  root <- .fixture_gen_pkg_root()
  if (is.null(dest)) {
    dest <- file.path(root, "tests", "testthat", "fixtures", "golden")
  }
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  helpers <- file.path(root, "tests", "testthat")
  for (h in c("helper-fixture-facts.R", "helper-stub-fit.R", "helper-extract-fixtures.R")) {
    sys.source(file.path(helpers, h), envir = environment())
  }

  facts_root <- file.path(tempdir(), paste0("golden-facts-", Sys.getpid()))
  unlink(facts_root, recursive = TRUE)
  write_table(.fixture_results(), "results", root = facts_root)
  write_table(.fixture_schedules(), "schedules", root = facts_root)

  extracts_root <- file.path(tempdir(), paste0("golden-extracts-", Sys.getpid()))
  out <- file.path(tempdir(), paste0("golden-out-", Sys.getpid()))
  unlink(c(extracts_root, out), recursive = TRUE)
  league <- load_leagues()[["football_iceland"]]
  for (sex in c("male", "female")) {
    build_football_extracts_fixture(facts_root, extracts_root, sex)
    extracted <- read_extracted_football(
      league, sex = sex, fit_date = FIXTURE_FIT_DATE, extracts_root = extracts_root
    )
    publish_football_iceland(
      extracted = extracted, league = league, sex = sex,
      end_date = FIXTURE_END_DATE, root = facts_root,
      output_root = out, extracts_root = extracts_root,
      archive_root = file.path(tempdir(), paste0("golden-archive-", Sys.getpid()))
    )
  }
  produced <- list.files(
    file.path(out, "football"), pattern = "\\.json$",
    recursive = TRUE, full.names = TRUE
  )
  manifest <- data.frame(
    file = sub(paste0("^", out, "/"), "", produced),
    sha256 = vapply(produced, publish_json_digest, character(1), USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )
  manifest <- manifest[order(manifest$file), , drop = FALSE]
  path <- file.path(dest, "football-publish-hashes.csv")
  utils::write.csv(manifest, path, row.names = FALSE)
  message("make_football_golden_hashes: ", nrow(manifest), " payloads -> ", path)
  invisible(path)
}
```

and extend the self-execution block:

```r
if (sys.nframe() == 0L && !is.null(.fixture_gen_pkg_root())) {
  devtools::load_all(.fixture_gen_pkg_root(), quiet = TRUE)
  args <- commandArgs(trailingOnly = TRUE)
  if ("--golden" %in% args) {
    make_football_golden_hashes()
  } else {
    make_extract_fixtures()
  }
}
```

- [ ] **Step 5: Generate the manifest and run, expect PASS.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript tools/make-extract-fixtures.R --golden
Rscript -e 'devtools::load_all("/Users/brynjolfurjonsson/sports", quiet = TRUE); testthat::test_file("/Users/brynjolfurjonsson/sports/tests/testthat/test-publish-football-golden.R")'
```

Expect `FAIL 0 | SKIP 0`. Run it twice in a row — a second green run proves the
publisher is deterministic under the fixture and that nothing but `generated_at`
moves between runs.

- [ ] **Step 6: Commit.**

```bash
git -C /Users/brynjolfurjonsson/sports add tools/make-extract-fixtures.R tests/testthat/helper-extract-fixtures.R tests/testthat/fixtures/golden tests/testthat/test-publish-football-golden.R
git -C /Users/brynjolfurjonsson/sports commit -m "test(publish): golden-file net for football before the WS9 refactor

WS9 deletes the two 2DT publishers and rewrites the football one. Nothing
today would notice if that silently changed a football payload, so pin every
JSON for BD/LD1/LD2/LD3/CUP x both sexes as a sha256 over the payload with
generated_at stripped. The football extracts partition is rebuilt
deterministically rather than committed: its 99-quantile bands over ~34 teams
are ~240KB on their own, past the whole fixture budget.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 10: Skip-hygiene convention test (spec §4 item 7)

**Files:**
- Create `/Users/brynjolfurjonsson/sports/tests/testthat/test-fixture-skip-hygiene.R`

**Interfaces:**
- Consumes: nothing (pure file grep).
- Produces: nothing.

The grep set includes WS3's `test-extract-home-advantage-units.R` so that file
can never ship with a gate either — WS2 asserts only its *hygiene*, never its
content.

- [ ] **Step 1: Write the test.**

```r
# /Users/brynjolfurjonsson/sports/tests/testthat/test-fixture-skip-hygiene.R
#
# The bb/hb coverage spent its whole life skipping: 8 publish blocks gated on a
# machine-local backup path, 6 extract blocks on a gitignored 300MB fit.rds.
# This test exists so it can never quietly stop running again.

test_that("the bb/hb publish + extract tests carry no skip gates", {
  guarded <- c(
    "test-publish-basketball.R",
    "test-publish-handball.R",
    "test-extract-basketball-iceland.R",
    "test-extract-handball-iceland.R",
    # WS3's units test (B5). Listed here so it lands ungated from day one;
    # this file asserts hygiene only, never that test's content.
    "test-extract-home-advantage-units.R"
  )
  banned <- c("skip(", "skip_if(", "skip_if_not(", "skip_if_not_installed(", "Sys.getenv")

  present <- guarded[file.exists(testthat::test_path(guarded))]
  # Every file except WS3's must exist by the end of WS2.
  expect_setequal(
    setdiff(guarded, "test-extract-home-advantage-units.R"),
    setdiff(present, "test-extract-home-advantage-units.R")
  )

  for (f in present) {
    src <- readLines(testthat::test_path(f), warn = FALSE)
    src <- src[!grepl("^\\s*#", src)]
    for (pat in banned) {
      hits <- grep(pat, src, fixed = TRUE, value = TRUE)
      expect_length(hits, 0L)
      if (length(hits) > 0L) {
        cat("\n", f, " -> ", pat, ":\n", paste(hits, collapse = "\n"), "\n", sep = "")
      }
    }
  }
})

test_that("no test file references the machine-local backup root", {
  files <- list.files(testthat::test_path("."), pattern = "^(test|helper)-.*\\.R$")
  for (f in files) {
    src <- readLines(testthat::test_path(f), warn = FALSE)
    expect_false(
      any(grepl("sports-backup-|SPORTS_BACKUP_ROOT", src)),
      info = f
    )
  }
})
```

- [ ] **Step 2: Run, expect PASS** (Tasks 4-7 already removed every gate).

```bash
Rscript -e 'devtools::load_all("/Users/brynjolfurjonsson/sports", quiet = TRUE); testthat::test_file("/Users/brynjolfurjonsson/sports/tests/testthat/test-fixture-skip-hygiene.R")'
```

If it fails on `sports-backup-`, a stale `backup_*_fit()` helper survived a
rewrite in Task 6 or 7 — delete it there, not here.

- [ ] **Step 3: Commit.**

```bash
git -C /Users/brynjolfurjonsson/sports add tests/testthat/test-fixture-skip-hygiene.R
git -C /Users/brynjolfurjonsson/sports commit -m "test(conventions): fail the build if bb/hb coverage starts skipping again

Fourteen assertions sat permanently skipped for months without anyone
noticing, which is exactly how the 2DT publisher reached production
unexercised. A static grep is the cheapest way to make that visible.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 11: Prove the harness — 14 skips gone, whole suite still green

**Files:**
- Modify `/Users/brynjolfurjonsson/sports/.claude/rules/backtest.md`? **No.** This task writes no files; it verifies and records.

**Interfaces:**
- Consumes: everything above.
- Produces: nothing.

- [ ] **Step 1: Measure the four repointed files.**

```bash
Rscript -e '
devtools::load_all("/Users/brynjolfurjonsson/sports", quiet = TRUE)
res <- testthat::test_dir(
  "/Users/brynjolfurjonsson/sports/tests/testthat",
  filter = "publish-basketball|publish-handball|extract-basketball-iceland|extract-handball-iceland",
  reporter = "summary", stop_on_failure = FALSE
)
df <- as.data.frame(res)
print(colSums(df[, c("failed", "skipped", "passed")]))
'
```

Expected: `skipped 0`, `failed 0`, `passed` well above the old baseline
(was `SKIP 5 + 3 + 3 + 3 = 14`, `PASS 4 + 4 + 0 + 0 = 8`). **14 skips removed.**
Record the exact new counts in the commit message below.

- [ ] **Step 2: Run the harness's own files.**

```bash
Rscript -e '
devtools::load_all("/Users/brynjolfurjonsson/sports", quiet = TRUE)
res <- testthat::test_dir(
  "/Users/brynjolfurjonsson/sports/tests/testthat",
  filter = "fixture-harness|stub-fit|fixture-skip-hygiene|publish-football-golden",
  reporter = "summary", stop_on_failure = FALSE
)
df <- as.data.frame(res); print(colSums(df[, c("failed", "skipped", "passed")]))
'
```

Expected `failed 0`, `skipped 0`.

- [ ] **Step 3: Full suite, to catch collateral damage** (new top-level helper files are sourced by *every* test file, so a name collision would surface here and nowhere else).

```bash
Rscript -e 'devtools::load_all("/Users/brynjolfurjonsson/sports", quiet = TRUE); devtools::test()' 2>&1 | tail -30
```

Expect no new failures versus `main`. If a pre-existing failure is present,
confirm it also fails on `main` before attributing it to this workstream:

```bash
git -C /Users/brynjolfurjonsson/sports stash list
git -C /Users/brynjolfurjonsson/sports log --oneline -3
```

- [ ] **Step 4: Regenerate from scratch to prove the generator is the source of truth.**

```bash
cd /Users/brynjolfurjonsson/sports
rm -rf tests/testthat/fixtures/facts tests/testthat/fixtures/extracts
Rscript tools/make-extract-fixtures.R
git -C /Users/brynjolfurjonsson/sports status --short tests/testthat/fixtures
```

`git status` must show **no** modifications: the committed fixtures are exactly
what the generator produces. A diff here means non-determinism (an unseeded RNG
or a `Sys.time()` leak) and must be fixed before the workstream closes.

- [ ] **Step 5: Commit the verification note (docs only) and push the branch.**

```bash
git -C /Users/brynjolfurjonsson/sports commit --allow-empty -m "test(fixtures): harness verified -- 14 permanent skips removed

Baseline before WS2: test-publish-basketball.R SKIP 5 / PASS 4,
test-publish-handball.R SKIP 3 / PASS 4, both extract files SKIP 3 / PASS 0.
After: SKIP 0 across all four, plus the fixture-harness, stub-fit,
skip-hygiene and football-golden files. Regenerating from
tools/make-extract-fixtures.R reproduces the committed fixtures byte for
byte, so the generator -- not the parquet blobs -- is the source of truth.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
git -C /Users/brynjolfurjonsson/sports fetch origin
git -C /Users/brynjolfurjonsson/sports rebase origin/main
git -C /Users/brynjolfurjonsson/sports push -u origin feat/bb-hb-metill-parity
```


---


# WS3 — B5: remove the exp() AND the /2 from 2DT home advantage

**Produces (later workstreams rely on these):**

- `.compute_home_advantage_quantiles_2dt(fit, teams, current_top_teams) -> tibble(team, component, quantile, value) — signature UNCHANGED, but `value` is now the RAW parameter (points for basketball, goals for handball), not exp(). `component` in {offence, defence, total}; `total` == offence + defence exactly. Downstream publishers (WS9/WS10) must render this without any inverse transform, and meta.json units.home_advantage must read "points" for these two sports.`
- `Internal closure .compute_home_advantage_quantiles_2dt$extract_one(var, component) — the `transform` argument is REMOVED. Any later task that wants a per-component transform must reintroduce it deliberately, not inherit it.`
- `fit_league(...) now ABORTS (cli::cli_abort, class "sports_extract_failed") when extract_{football,basketball,handball}_iceland() raises, instead of cli_alert_warning + continue. Any later task calling fit_league() with a fit stub that lacks $draws() MUST mock the sport's extractor in local_mocked_bindings().`

## Workstream 3 — B5: the 2DT home-advantage units bug

**Verified before drafting (do not re-derive):**

- `R/extract-iceland-2dt-shared.R:196-215` — `extract_one(var, component, transform = identity)` sets `value = exp(transform(.data$value))` for **all three** components, and `total` is called with `transform = function(x) x / 2`. The comment at `:192-193` claims it "Mirrors football's home_advantage_quantiles.parquet — exp-transformed log-multiplier".
- `Stan/basketball_iceland/2d_student_t_scalarsigma.stan:112` / `:116` — `vector<lower = 0>[K] home_advantage_off` / `home_advantage_def`, documented as "extra points team k scores at home above its road baseline"; prior `normal(0, 10)` at `:201-202`; `:277` `vector[K] home_advantage_tot = home_advantage_off + home_advantage_def;`.
- `Stan/handball_iceland/2d_student_t.stan:143` / `:146` / `:335` — identical parameterisation, in goals.
- `R/publish-iceland-2dt-helpers.R:293-296` already says it in prose: "unlike football's bivariate Poisson, basketball/handball `home_advantage_*` are in raw points/goals, NOT log-rates".
- `R/extract-football-iceland.R:1638-1657` — football's `extract_home_adv()` closure inside `extract_football_iceland()` also does `exp(transform(.data$value))` with `transform = x / 2` on `total`. For football this is **correct** (the parameter is a log-rate multiplier) and must not change.
- `R/model-league.R:288-304` — the extract call is wrapped in a warn-only `tryCatch`, so a broken extractor exits 0.
- Never fired in production: `data/beliefs/extracts/` holds only `sport=football`.

Branch: `feat/bb-hb-metill-parity` (already checked out).

---

### Task 1: RED — a stub fit with a 4.0-point home edge must publish 4.0

**Files:**
- Create `/Users/brynjolfurjonsson/sports/tests/testthat/test-extract-2dt-home-advantage-units.R`
- Test: the file itself; run with `Rscript -e 'devtools::test(filter = "extract-2dt-home-advantage-units")'`

**Interfaces:**
- **Consumes:** WS2's `tests/testthat/helper-stub-fit.R::stub_fit(draws_list)` — named list keyed by Stan variable name, each value a `posterior::draws_array` with variable names `<var>[1]`..`<var>[K]`; returns an object whose `$draws(var)` is a function. Also `.summarise_quantile_band_2dt()` and `.compute_home_advantage_quantiles_2dt()` from `R/extract-iceland-2dt-shared.R` (internal — reachable because `devtools::test()` runs under `load_all()`).
- **Produces:** the RED proof Task 2 flips GREEN. No new R interfaces.

Constraints held throughout: **no `skip(`, `skip_if(`, `skip_if_not(` or `Sys.getenv` anywhere in this file** (spec §4 assertion 7 — this coverage must never be able to stop running silently). No dates appear in this file at all, so the time-bomb-fixture rule is trivially satisfied.

- [ ] **Step 1: Pin the WS2 stub contract before depending on it.** Create the file with only the contract check, so a WS2 shape drift fails by name rather than deep inside `pivot_longer()`.

```r
# tests/testthat/test-extract-2dt-home-advantage-units.R
#
# B5 (design spec 2026-09-02 §8): basketball + handball `home_advantage_*`
# are RAW points / goals, NOT football's log-rate multiplier.
#   Stan/basketball_iceland/2d_student_t_scalarsigma.stan:112,116
#     vector<lower = 0>[K] home_advantage_off / _def, prior normal(0, 10),
#     entering the mean linearly (:264).
#   :277  home_advantage_tot = home_advantage_off + home_advantage_def
#   Stan/handball_iceland/2d_student_t.stan:143,146,335 — same, in goals.
# So the extractor must publish the parameter itself: no exp(), no halving.
# This file has no skip() by design — see spec §4 assertion 7.

test_that("WS2's stub_fit() exposes the $draws(var) contract this file needs", {
  arr <- posterior::as_draws_array(matrix(
    1.5,
    nrow = 4L, ncol = 2L,
    dimnames = list(NULL, c("home_advantage_off[1]", "home_advantage_off[2]"))
  ))
  fit <- stub_fit(list(home_advantage_off = arr))

  expect_true(is.function(fit$draws))
  got <- fit$draws("home_advantage_off")
  expect_setequal(
    posterior::variables(got),
    c("home_advantage_off[1]", "home_advantage_off[2]")
  )
  expect_equal(posterior::ndraws(got), 4L)
})
```

- [ ] **Step 2: Run the contract check — it must pass before any RED claim is meaningful.**

```bash
Rscript -e 'devtools::test(filter = "extract-2dt-home-advantage-units")'
```

Expect `[ FAIL 0 | PASS 3 ]`. If it fails with `could not find function "stub_fit"`, WS2 has not landed on this branch — stop and rebase onto it; do **not** hand-roll a local stub, that would let WS3 pass against a shape WS2 never ships.

- [ ] **Step 3: Add the constant-draws helper and the three RED assertions.** Append to the same file. `off = 1.5`, `def = 2.5`, so `tot = 4.0` — a plausible basketball home edge, and three values whose exponentials are all distinct and far from their raw counterparts.

```r
# Two teams, 50 draws, every draw exactly constant — so every one of the 99
# quantiles equals the constant and the assertion is on an exact number, not
# a Monte-Carlo band.
.ha_stub_fit_const <- function(off = 1.5, def = 2.5, n_draws = 50L,
                               teams = c("Alpha", "Bravo")) {
  k <- length(teams)
  const <- function(x, var) {
    posterior::as_draws_array(matrix(
      x,
      nrow = n_draws, ncol = k,
      dimnames = list(NULL, paste0(var, "[", seq_len(k), "]"))
    ))
  }
  stub_fit(list(
    home_advantage_off = const(off, "home_advantage_off"),
    home_advantage_def = const(def, "home_advantage_def"),
    home_advantage_tot = const(off + def, "home_advantage_tot")
  ))
}

.ha_component <- function(ha, component, team = "Alpha") {
  out <- ha[ha$component == component & ha$team == team, , drop = FALSE]
  out[order(out$quantile), , drop = FALSE]
}

test_that("2DT total home advantage publishes raw points, not exp(tot / 2)", {
  teams <- tibble::tibble(team = c("Alpha", "Bravo"))
  fit <- .ha_stub_fit_const(off = 1.5, def = 2.5)

  ha <- .compute_home_advantage_quantiles_2dt(fit, teams, teams)

  tot <- .ha_component(ha, "total")
  expect_equal(nrow(tot), 99L)
  expect_equal(tot$quantile, 1:99)
  # 4.0 points, published as 4.0. NOT exp(4 / 2) = 7.389056098930650,
  # and NOT exp(4) = 54.598150033144236.
  expect_equal(tot$value, rep(4.0, 99L), tolerance = 1e-9)
})

test_that("2DT offence and defence home advantage are raw, not exponentiated", {
  teams <- tibble::tibble(team = c("Alpha", "Bravo"))
  fit <- .ha_stub_fit_const(off = 1.5, def = 2.5)

  ha <- .compute_home_advantage_quantiles_2dt(fit, teams, teams)

  # exp() was applied by the shared extract_one() to ALL THREE components,
  # not only to `total` — so offence and defence need their own assertions.
  expect_equal(
    .ha_component(ha, "offence")$value, rep(1.5, 99L),
    tolerance = 1e-9
  )
  expect_equal(
    .ha_component(ha, "defence")$value, rep(2.5, 99L),
    tolerance = 1e-9
  )
})

test_that("2DT total home advantage equals offence + defence (Stan :277)", {
  teams <- tibble::tibble(team = c("Alpha", "Bravo"))
  fit <- .ha_stub_fit_const(off = 1.5, def = 2.5)

  ha <- .compute_home_advantage_quantiles_2dt(fit, teams, teams)

  # The additivity in Stan :277 survives any identity transform and is
  # destroyed by any exp(): exp(1.5) + exp(2.5) = 16.664 != exp(2) = 7.389.
  # This is the invariant that makes the bug impossible to reintroduce
  # component-by-component.
  expect_equal(
    .ha_component(ha, "total")$value,
    .ha_component(ha, "offence")$value + .ha_component(ha, "defence")$value,
    tolerance = 1e-9
  )
})
```

- [ ] **Step 4: Run it and record the EXACT RED failures.**

```bash
Rscript -e 'devtools::test(filter = "extract-2dt-home-advantage-units")'
```

Expected: `[ FAIL 4 | PASS 4 ]` (the contract test's 3 expectations pass, plus `expect_equal(tot$quantile, 1:99)`), with these four failures:

1. `tot$value (actual) not equal to rep(4, 99) (expected).` — waldo reports `99/99 mismatches (average diff: 3.39)`; every actual value is `7.389056098930650` = `exp(4 / 2)`.
2. `.ha_component(ha, "offence")$value not equal to rep(1.5, 99).` — `99/99 mismatches (average diff: 2.98)`; every actual value is `4.481689070338065` = `exp(1.5)`.
3. `.ha_component(ha, "defence")$value not equal to rep(2.5, 99).` — `99/99 mismatches (average diff: 9.68)`; every actual value is `12.182493960703473` = `exp(2.5)`.
4. Additivity: actual `total` is `7.389056098930650` against an expected `exp(1.5) + exp(2.5)` = `16.664183031041538` — `99/99 mismatches (average diff: 9.28)`.

If failure 1 instead reports `54.598150033144236`, the `/2` was already removed by someone else and only the `exp()` remains — re-read `R/extract-iceland-2dt-shared.R:196-215` before continuing.

- [ ] **Step 5: Commit the RED test on its own, so the proof is in history separate from the fix.**

```bash
git -C /Users/brynjolfurjonsson/sports add tests/testthat/test-extract-2dt-home-advantage-units.R
git -C /Users/brynjolfurjonsson/sports commit -m "test(extract): RED proof that 2DT home advantage is exponentiated (B5)

Stan declares home_advantage_off/def as vector<lower=0>[K] in raw points
(basketball) / goals (handball), entering the mean linearly, with
home_advantage_tot = off + def. The shared 2DT extractor applies
exp(x) to all three components and exp(x/2) to total, so a 4-point home
edge would publish as 7.389. It has never fired because
data/beliefs/extracts/ holds only sport=football -- and it becomes the
first number on every home_advantage.json the moment extracts become the
publish source. Assert to 1e-9: a loose bound would also pass on 7.389.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 2: GREEN — remove both the exp() and the /2 from the 2DT extractor

**Files:**
- Modify `/Users/brynjolfurjonsson/sports/R/extract-iceland-2dt-shared.R` lines 192-220 (the comment header at 192-193, the `extract_one()` signature at 196, `value =` at 205, and the `total` call at 213-215)
- Test: `tests/testthat/test-extract-2dt-home-advantage-units.R` (from Task 1)

**Interfaces:**
- **Consumes:** the four RED failures from Task 1 Step 4.
- **Produces:** `.compute_home_advantage_quantiles_2dt(fit, teams, current_top_teams)` — signature unchanged, `value` now the raw parameter. The internal `extract_one()` loses its `transform` argument.

- [ ] **Step 1: Replace the comment header (lines 192-193) with the units contract.** This is deliverable (d): the next copy-paste must hit an explanation, not a claim that it mirrors football.

```r
# Per-team home-advantage quantile bands.
#
# UNITS -- do NOT copy football's transform into this function. In the 2DT
# models `home_advantage_off` / `home_advantage_def` are declared
# `vector<lower = 0>[K]` in RAW points (basketball) / goals (handball) and
# enter the mean linearly:
#   Stan/basketball_iceland/2d_student_t_scalarsigma.stan:112,116 (prior
#   normal(0, 10) at :201-202; mu_ll assembled at :264), and :277 defines
#   home_advantage_tot = home_advantage_off + home_advantage_def.
#   Stan/handball_iceland/2d_student_t.stan:143,146,335 -- identical, goals.
# R/publish-iceland-2dt-helpers.R:293-296 states the same contract in prose.
# So the published value is the parameter itself: no exp(), no halving.
#
# Football is the asymmetric case and is CORRECT as it stands: there the
# parameter is a log-rate of a bivariate Poisson, so
# extract_football_iceland()'s extract_home_adv() (R/extract-football-
# iceland.R:1638-1657) exponentiates, and halves `total` as a per-side
# allocation of the log multiplier. Neither operation has a meaning on an
# additive sum of two non-negative point quantities -- applying them here
# published a 4-point home edge as exp(4/2) = 7.389 (spec 2026-09-02 §8, B5).
```

- [ ] **Step 2: Drop the `transform` argument and the `exp()` (lines 196, 205).** The `value` column already exists from `pivot_longer()`, so the identity is expressed by removing the mutate line, not by writing `value = .data$value`.

```r
.compute_home_advantage_quantiles_2dt <- function(fit, teams,
                                                  current_top_teams) {
  extract_one <- function(var, component) {
    fit$draws(var) |>
      posterior::as_draws_df() |>
      tibble::as_tibble() |>
      tidyr::pivot_longer(c(-".chain", -".draw", -".iteration")) |>
      dplyr::mutate(
        team_idx = as.integer(readr::parse_number(.data$name)),
        team = teams$team[.data$team_idx],
        component = component
      ) |>
      dplyr::select("team", "component", ".draw", "value")
  }
```

- [ ] **Step 3: Drop the `transform = function(x) x / 2` on `total` (lines 210-217).**

```r
  home_adv_draws <- dplyr::bind_rows(
    extract_one("home_advantage_off", "offence"),
    extract_one("home_advantage_def", "defence"),
    extract_one("home_advantage_tot", "total")
  ) |>
    dplyr::semi_join(current_top_teams, by = "team")

  .summarise_quantile_band_2dt(home_adv_draws, c("team", "component"))
}
```

- [ ] **Step 4: Confirm no other `exp(` survives in the 2DT extractor's home-advantage path.**

```bash
grep -n 'exp(' /Users/brynjolfurjonsson/sports/R/extract-iceland-2dt-shared.R
grep -n 'transform' /Users/brynjolfurjonsson/sports/R/extract-iceland-2dt-shared.R
```

Both must return **no output**. A hit on either means the edit landed in the wrong closure.

- [ ] **Step 5: Run the RED test — all four failures must flip.**

```bash
Rscript -e 'devtools::test(filter = "extract-2dt-home-advantage-units")'
```

Expect `[ FAIL 0 | PASS 8 ]`.

- [ ] **Step 6: Run the two 2DT extractor suites for collateral damage.** They gate on the 300-600 MB fit RDS and will mostly skip; the point is that nothing errors.

```bash
Rscript -e 'devtools::test(filter = "extract-basketball-iceland")'
Rscript -e 'devtools::test(filter = "extract-handball-iceland")'
```

Expect `FAIL 0` in both.

- [ ] **Step 7: Commit the fix.**

```bash
git -C /Users/brynjolfurjonsson/sports add R/extract-iceland-2dt-shared.R
git -C /Users/brynjolfurjonsson/sports commit -m "fix(extract): publish 2DT home advantage in raw points, not exp(x/2)

The shared 2DT extractor was copied from football, whose home_advantage_*
IS a bivariate-Poisson log-rate -- so exp() and the /2 per-side allocation
are right there. Basketball and handball declare the same-named parameters
as vector<lower=0>[K] in raw points/goals entering the mean linearly, with
home_advantage_tot = off + def, so both operations are meaningless on them
and a 4-point edge published as 7.389.

Removes exp() from all three components (it was applied by the shared
extract_one(), not only to total) and drops the transform argument
entirely, so reintroducing a per-component transform has to be a
deliberate act. The comment header now carries the Stan line references
and names football as the asymmetric case, since the copied comment
asserting the opposite is what let this survive.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 3: Guard that football's exp() and /2 are untouched, and say why in its roxygen

**Files:**
- Modify `/Users/brynjolfurjonsson/sports/tests/testthat/test-extract-2dt-home-advantage-units.R` (append one test)
- Modify `/Users/brynjolfurjonsson/sports/R/extract-football-iceland.R` roxygen lines 1460-1463
- Modify `/Users/brynjolfurjonsson/sports/man/extract_football_iceland.Rd` (regenerated; line 81 carries the same text)

**Interfaces:**
- **Consumes:** Task 2's edit to `R/extract-iceland-2dt-shared.R`.
- **Produces:** a source-level regression guard. Football's `extract_home_adv()` is a closure defined inside `extract_football_iceland()` (`R/extract-football-iceland.R:1638`), so it is not callable in isolation and a behavioural test on it would need the gitignored football fit RDS — the guard is therefore on the source text, in the same style as `tests/testthat/test-script-ledger-commit.R` and `test-placer-ci-isolation.R`.

- [ ] **Step 1: Write the failing-direction guard first — assert against the file as it stands *after* Task 2.** Append to `tests/testthat/test-extract-2dt-home-advantage-units.R`. Paths use `testthat::test_path()`, never `here::here()` — in a git worktree `here::here()` resolves to the main checkout and the guard would read the wrong file.

```r
test_that("football keeps exp() and the /2; the 2DT extractor keeps neither", {
  fb <- paste(
    readLines(
      testthat::test_path("..", "..", "R", "extract-football-iceland.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  dt <- paste(
    readLines(
      testthat::test_path("..", "..", "R", "extract-iceland-2dt-shared.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  # Football's home_advantage_* IS a bivariate-Poisson log-rate. exp() and
  # the per-side /2 are correct there and must survive this workstream.
  expect_true(
    grepl("exp(transform(.data$value))", fb, fixed = TRUE),
    info = "football's extract_home_adv() must keep its exp() transform"
  )
  expect_true(
    grepl('extract_home_adv("home_advantage_tot", "total"', fb, fixed = TRUE),
    info = "football's total component call site moved or was renamed"
  )
  expect_true(
    grepl("transform = function(x) x / 2", fb, fixed = TRUE),
    info = "football's per-side /2 allocation on `total` must survive"
  )

  # The 2DT extractor must carry neither (B5, spec 2026-09-02 §8).
  expect_false(
    grepl("exp(", dt, fixed = TRUE),
    info = "no exp() belongs anywhere in the 2DT extractor -- raw points/goals"
  )
  expect_false(
    grepl("transform", dt, fixed = TRUE),
    info = "extract_one() must not regain a transform argument"
  )
})
```

- [ ] **Step 2: Run it.**

```bash
Rscript -e 'devtools::test(filter = "extract-2dt-home-advantage-units")'
```

Expect `[ FAIL 0 | PASS 13 ]`. To confirm the guard actually bites rather than passing vacuously, temporarily re-add ` value = exp(.data$value),` to `extract_one()` in `R/extract-iceland-2dt-shared.R`, re-run, and confirm **two** new failures (the `expect_false(grepl("exp("...))` guard here, and the `total` equality in Task 1) — then `git -C /Users/brynjolfurjonsson/sports checkout -- R/extract-iceland-2dt-shared.R`.

- [ ] **Step 3: Amend football's roxygen so the asymmetry is documented at the source of the copy.** Replace `R/extract-football-iceland.R:1460-1463`:

```r
#' - `home_advantage_quantiles.parquet` — 99-quantile band per
#'   (division, team, component) for the multiplicative home-advantage parameter
#'   `exp(home_advantage_*)`. The `total` component is `exp(home_advantage_tot / 2)`
#'   matching the publisher's per-side allocation. **Football only** — the
#'   parameter here is a bivariate-Poisson log-rate, so exponentiating is the
#'   inverse link and halving `total` allocates the log multiplier per side.
#'   Do NOT copy either operation into the 2DT (basketball/handball) extractor:
#'   there `home_advantage_*` is `vector<lower = 0>[K]` in raw points/goals
#'   entering the mean linearly, and `home_advantage_tot = off + def`. See
#'   `.compute_home_advantage_quantiles_2dt()` in R/extract-iceland-2dt-shared.R.
```

- [ ] **Step 4: Regenerate the man page in the same edit (house rule: docs ship with the code).**

```bash
Rscript -e 'devtools::document()'
git -C /Users/brynjolfurjonsson/sports diff --stat man/extract_football_iceland.Rd
```

Expect `man/extract_football_iceland.Rd` to be the only changed `man/` file.

- [ ] **Step 5: Commit.**

```bash
git -C /Users/brynjolfurjonsson/sports add \
  tests/testthat/test-extract-2dt-home-advantage-units.R \
  R/extract-football-iceland.R \
  man/extract_football_iceland.Rd
git -C /Users/brynjolfurjonsson/sports commit -m "test(extract): lock the football/2DT home-advantage units asymmetry

B5 happened because a comment claimed the 2DT extractor mirrored
football's exp-transformed log-multiplier -- and nothing checked. Football's
extract_home_adv() is a closure inside extract_football_iceland() and needs
the gitignored fit RDS to exercise, so the guard is on the source text
(same style as test-script-ledger-commit.R): football must keep exp() and
the /2, the 2DT extractor must have neither. Football's roxygen now names
itself as the football-only case and points at the 2DT function, so the
next reader of the correct code is warned before copying it.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 4: Promote the warn-only extract tryCatch to an abort

**Files:**
- Modify `/Users/brynjolfurjonsson/sports/R/model-league.R` lines 288-304 (the `tryCatch` inside `fit_league()`)
- Modify `/Users/brynjolfurjonsson/sports/tests/testthat/test-model-league.R` lines 38-42
- Test: `tests/testthat/test-model-league.R`

**Interfaces:**
- **Consumes:** nothing from earlier tasks (independent of Tasks 1-3, but belongs to WS3 per spec §8's "two structural follow-ons").
- **Produces:** `fit_league()` aborts with condition class `"sports_extract_failed"` when the sport's extractor raises. Later workstreams that call `fit_league()` with a bare fit stub must mock the extractor.

**Verified precondition:** exactly ONE existing test breaks. `tests/testthat/test-model-league.R:1-69` ("fit_league writes beliefs_latest and beliefs_archive") uses `fake_fit <- structure(list(save_object = ...), class = "CmdStanMCMC")` with `write_archive` defaulted TRUE and a basketball league, so `extract_basketball_iceland()` reaches `fit$draws(...)` on an object with no `draws` element and raises — today swallowed by the warning. The test at `:71` passes `write_archive = FALSE` (block skipped); the two football tests at `:113` and `:170` already mock `extract_football_iceland`.

- [ ] **Step 1: Write the failing test for the abort.** Append to `tests/testthat/test-model-league.R`. Reuses the file's existing `mini_results.parquet` / `mini_schedules.parquet` fixtures and mocking idiom.

```r
test_that("fit_league aborts when the extractor fails (extracts are the publish source)", {
  root <- withr::local_tempdir()
  results <- arrow::read_parquet(
    testthat::test_path("fixtures", "model", "mini_results.parquet")
  )
  schedules <- arrow::read_parquet(
    testthat::test_path("fixtures", "model", "mini_schedules.parquet")
  )
  write_table(results, "results", root = root)
  write_table(schedules, "schedules", root = root)

  mini_league <- list(
    sport = "basketball", country = "iceland",
    sexes = c("male"), active = TRUE,
    data_source = list(
      results = "kki_basketball",
      schedule = "kki_basketball",
      odds = "lengjan_odds"
    ),
    stan_model = "basketball_iceland/2d_student_t_scalarsigma.stan"
  )

  fake_beliefs <- tibble::tibble(
    sport = "basketball", country = "iceland", sex = "male",
    fit_date = Sys.Date(),
    match_date = rep(Sys.Date() + c(3L, 7L), each = 5L),
    home_team = rep(c("Alpha", "Charlie"), each = 5L),
    away_team = rep(c("Bravo", "Delta"), each = 5L),
    draw_id = rep(1:5, times = 2L),
    home_goals = runif(10, 70, 100),
    away_goals = runif(10, 70, 100)
  )
  fake_fit <- structure(
    list(save_object = function(file) saveRDS(NULL, file)),
    class = "CmdStanMCMC"
  )

  testthat::local_mocked_bindings(
    fit_model = function(...) fake_fit,
    extract_posteriors = function(...) fake_beliefs,
    extract_basketball_iceland = function(...) {
      stop("synthetic extractor failure", call. = FALSE)
    },
    .package = "sports"
  )

  expect_error(
    fit_league(
      league   = mini_league, sex = "male",
      fit_date = Sys.Date(), root = root,
      stan_dir = here::here("Stan")
    ),
    class = "sports_extract_failed"
  )
})
```

- [ ] **Step 2: Run it and record the exact RED failure.**

```bash
Rscript -e 'devtools::test(filter = "model-league")'
```

Expected: `[ FAIL 1 | ... ]` on the new test —
`Error: fit_league(...) did not throw the expected condition. Expected an error with class <sports_extract_failed>` (testthat reports `expect_error()` finding no error at all, because the `tryCatch` at `R/model-league.R:288-304` converts it to `cli_alert_warning` and `fit_league()` returns the beliefs tibble normally). The pre-existing tests in this file still pass at this point.

- [ ] **Step 3: Promote the tryCatch to an abort.** Replace `R/model-league.R:288-304`:

```r
    if (!is.null(extract_fn)) {
      # WHY abort, not warn: since the bb/hb/football publishers read the
      # extracts tree as their sole input, a failed extraction is a silently
      # missing publish source -- and a *wrong* extraction (the B5 units bug,
      # spec 2026-09-02 §8) would have been equally invisible behind a
      # warning that nothing reads. Fail the fit run instead.
      tryCatch(
        extract_fn(
          fit, league,
          sex = sex,
          fit_date = fit_date,
          end_date = end_date,
          root = root,
          prep = prep
        ),
        error = function(e) {
          cli::cli_abort(
            c(
              "extract_{league$sport}_iceland() failed for sex {sex}.",
              x = conditionMessage(e),
              i = "The extracts tree is the publish source; a fit without it
                   cannot be published."
            ),
            class = "sports_extract_failed",
            parent = e
          )
        }
      )
    }
```

- [ ] **Step 4: Run — the new test passes and exactly one pre-existing test now fails.**

```bash
Rscript -e 'devtools::test(filter = "model-league")'
```

Expected: `[ FAIL 1 ]` on **"fit_league writes beliefs_latest and beliefs_archive"** (`tests/testthat/test-model-league.R:44`), with
`Error in fit_league(...): extract_basketball_iceland() failed for sex male.` / `x attempt to apply non-function` — because that test's `fake_fit` has no `$draws` element and the extractor is not mocked. This is the abort doing its job, not a regression.

- [ ] **Step 5: Mock the extractor in that one test.** In `tests/testthat/test-model-league.R:38-42`, add one line to the existing `local_mocked_bindings()` call:

```r
  testthat::local_mocked_bindings(
    fit_model = function(...) fake_fit,
    extract_posteriors = function(...) fake_beliefs,
    # fake_fit has no $draws(); the real extractor would abort now that
    # fit_league() no longer swallows extraction failures.
    extract_basketball_iceland = function(...) invisible(NULL),
    .package = "sports"
  )
```

Leave `:71` (uses `write_archive = FALSE`, so the extraction block never runs) and `:152` / `:207` (already mock `extract_football_iceland`) untouched.

- [ ] **Step 6: Run the full suite — this changes a shared code path, so a filtered run is not sufficient evidence.**

```bash
Rscript -e 'devtools::test(filter = "model-league")'
Rscript -e 'devtools::test()' 2>&1 | tail -30
```

Expect `FAIL 0` from both, and a non-zero pass count (baseline: this repo reports 1120+ assertions — record the exact `[ FAIL 0 | WARN n | SKIP n | PASS n ]` line with today's date, since it moves).

- [ ] **Step 7: Commit.**

```bash
git -C /Users/brynjolfurjonsson/sports add R/model-league.R tests/testthat/test-model-league.R
git -C /Users/brynjolfurjonsson/sports commit -m "fix(model): abort fit_league when the extractor fails

The extraction call was warn-only, so scripts/03_fit.R exited 0 with no
extracts written. That is the same blind spot that hid B5 for three
months: a wrong number and a missing artefact were equally invisible in
the fit log. Once the extracts tree is the sole publish input for all
three sports, a failed extraction is a failed fit.

The condition carries class sports_extract_failed and the original error
as parent. One existing test needed the basketball extractor mocked --
its fake_fit has no \$draws(), which the old warning swallowed.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

- [ ] **Step 8: Spec verification for WS3 — the deliberate-break proof, run by hand.** The spec requires confirming `scripts/03_fit.R` now exits non-zero. Do this without a real 20-minute MCMC fit by injecting a raise into the extractor and running the script against a scratch root:

```bash
cd /Users/brynjolfurjonsson/sports
cp R/extract-iceland-2dt-shared.R /private/tmp/claude-501/-Users-brynjolfurjonsson-sports/9cfbc60d-03cb-4b24-8f5a-2494c163c24f/scratchpad/extract-2dt.bak
# raise at the top of .compute_home_advantage_quantiles_2dt
perl -0pi -e 's/(\.compute_home_advantage_quantiles_2dt <- function\(fit, teams,\n\s+current_top_teams\) \{\n)/$1  stop("deliberate WS3 break", call. = FALSE)\n/' R/extract-iceland-2dt-shared.R
grep -n 'deliberate WS3 break' R/extract-iceland-2dt-shared.R   # must print one line
Rscript scripts/03_fit.R --league basketball_iceland --sex male --force; echo "exit=$?"
cp /private/tmp/claude-501/-Users-brynjolfurjonsson-sports/9cfbc60d-03cb-4b24-8f5a-2494c163c24f/scratchpad/extract-2dt.bak R/extract-iceland-2dt-shared.R
git -C /Users/brynjolfurjonsson/sports diff --stat R/extract-iceland-2dt-shared.R   # must be empty
```

Expect `exit=1` with `extract_basketball_iceland() failed for sex male` in the output. Before Task 4 the same run printed `! extract_basketball_iceland failed: deliberate WS3 break` and exited 0. Record both numbers; do not take them from a report. Nothing is committed by this step — the final `git diff --stat` proving an empty diff is the check that the injection was fully reverted.


---


# WS4 — HSÍ derived season resolution, the season-stamp guard, and the female G66 2025 backfill

**Produces (later workstreams rely on these):**

- ``.assert_season_stamp(rows, season, source = "unknown", tol = 0.05)` -> invisible(rows); aborts with class `sports_season_stamp_error` when >tol of `rows$match_date` calendar years fall outside `{season - 1, season}`. Zero rows and all-NA dates pass. Lives in R/ingest.R; WS5 calls it on parsed KKÍ XLSX rows.`
- ``HSI_TOURNAMENT_IDS` — `list[[sex]][[division]][["<season>"]] <- <integer id>`; sexes `male`/`female`, divisions `div1`/`div2`/`cup`(male)/`playoffs`. 26 entries at Task 3, 27 after Task 7.`
- ``hsi_tournament_id(sex, division, season)` -> integer(1) or NULL. Resolves registry first, `config/federation-seasons.json` cache second, NULL third.`
- ``hsi_url(sex, division, season)` -> "https://www.hsi.is/tournament/<id>" or NULL. NO `kind` argument, NO stop() on unknown sex/division — unknown resolves to NULL, which means do-not-fetch.`
- ``hsi_unresolved_seasons(season, sexes = c("male", "female"))` -> tibble(sex, division, season) of reachable (sex, division) pairs with no id for that season. Consumed by WS12's `check_season_resolution()`.`
- ``hsi_page_title(html)` -> character(1); the page `<title>` with the ` | HSÍ` suffix stripped.`
- ``parse_hsi_tournament_index(html)` -> tibble(id integer, title character); pure, fixture-tested.`
- ``hsi_discover_tournaments(index_url = "https://www.hsi.is/mot", season)` -> tibble(federation, sex, division, season, id, title, discovered_at, source, verified).`
- ``federation_seasons_path()` -> here::here("config", "federation-seasons.json").`
- ``read_federation_seasons(path = federation_seasons_path())` -> tibble(federation, sex, division, season, id, title, source, discovered_at, verified, note).`
- ``federation_season_id(federation, sex, division, season, path = federation_seasons_path())` -> integer(1) or NULL; only `verified` entries with a non-NA season resolve.`
- ``merge_federation_seasons(new_entries, existing)` -> tibble; keyed on (federation, sex, division, season), trust order inferred-verified > hand-verified > live-nav > live > inferred-candidate; aborts with class `sports_federation_id_conflict` when two `verified` entries disagree on `id` for the same key.`
- ``refresh_federation_seasons(entries, path = federation_seasons_path())` -> invisible(tibble); exported maintenance entry point that merges and rewrites the cache.`

## WS4 — HSÍ derived season resolution, season-stamp guard, female G66 2025 backfill

Branch: `feat/bb-hb-metill-parity`. **Commit only — do NOT `git push`.** Spec §5 requires WS4+WS5+WS6 to land in one PR, so nothing here goes to `origin` on its own.

Every `git` invocation uses `git -C /Users/brynjolfurjonsson/sports` (the Bash tool's cwd persists across calls; see `.claude/rules/git-hygiene.md`).

**Measured baseline (observed 2026-09-02, `devtools::load_all(".")` on `feat/bb-hb-metill-parity`).** These numbers are what the tests below assert against; re-measure if the fixture changes:

- `tests/testthat/fixtures/hsi_handball/male_div1_current.html` — `<title>` is `Olís deild karla 2025-26 | HSÍ`; `parse_hsi_results_page()` yields **108 rows**, **66 dated 2025** and **42 dated 2026**; `parse_hsi_schedule_page()` yields **0 rows** (the season is fully played).
- So this fixture is **season 2026**. Requested as 2026 → 0.0% of dates outside `{2025, 2026}` → passes. Requested as 2027 → 61.1% outside `{2026, 2027}` → aborts. Requested as 2025 → 38.9% outside `{2024, 2025}` → aborts.
- `data/facts/results` handball division `G66`: female seasons 2021 (64), 2022 (98), 2023 (62), 2024 (75), 2026 (84) — **2025 absent**. Male G66 has 2025 (60).
- Known female G66 squads: 2024 = Berserkir, FH, Fjölnir, Fram U, Grótta, HK, Haukar U, Selfoss, Valur U, Víkingur; 2026 = Afturelding, FH, Fjölnir, Fram 2, Grótta, HK, Valur 2, Víkingur. Intersection of the two = **FH, Fjölnir, Grótta, HK, Víkingur** (5 clubs). Task 7 derives these from disk rather than hardcoding, so no Icelandic literals enter R source.

---

### Task 1: `.assert_season_stamp()` — the guard, in the shared ingest file

**Files:**
- Modify `/Users/brynjolfurjonsson/sports/R/ingest.R` — insert after `.is_league_active()` (currently ends line 118), before the `ingest_one_league()` roxygen block.
- Test (create) `/Users/brynjolfurjonsson/sports/tests/testthat/test-season-stamp.R`

**Interfaces:**
- Consumes: nothing.
- Produces: `.assert_season_stamp(rows, season, source = "unknown", tol = 0.05)` -> `invisible(rows)`; aborts with condition class `sports_season_stamp_error`.

It goes in `R/ingest.R` rather than `R/ingest-hsi-handball.R` because WS5 calls the identical guard on parsed KKÍ XLSX rows, and `ingest.R` is collated before both federation files.

- [ ] **Step 1: Write the failing unit tests.**

```r
# tests/testthat/test-season-stamp.R
make_rows <- function(dates) {
  tibble::tibble(match_date = as.Date(dates))
}

test_that(".assert_season_stamp passes an Icelandic winter season's two years", {
  rows <- make_rows(c("2024-09-14", "2024-12-02", "2025-01-18", "2025-05-03"))
  expect_identical(.assert_season_stamp(rows, 2025L, source = "test"), rows)
})

test_that(".assert_season_stamp aborts with its own class when the years are wrong", {
  rows <- make_rows(c("2024-09-14", "2024-12-02", "2025-01-18", "2025-05-03"))
  expect_error(
    .assert_season_stamp(rows, 2027L, source = "hsi male/div2"),
    class = "sports_season_stamp_error"
  )
})

test_that(".assert_season_stamp tolerates a small out-of-span minority", {
  # 1 stray in 40 = 2.5%, under the 5% default.
  rows <- make_rows(c(rep("2024-10-01", 39), "2023-06-01"))
  expect_no_error(.assert_season_stamp(rows, 2025L, source = "test"))
  # 4 strays in 40 = 10%, over it.
  rows_bad <- make_rows(c(rep("2024-10-01", 36), rep("2023-06-01", 4)))
  expect_error(
    .assert_season_stamp(rows_bad, 2025L, source = "test"),
    class = "sports_season_stamp_error"
  )
})

test_that(".assert_season_stamp is a no-op on zero rows and on all-NA dates", {
  expect_no_error(.assert_season_stamp(make_rows(character()), 2027L))
  expect_no_error(.assert_season_stamp(make_rows(c(NA, NA)), 2027L))
  expect_no_error(.assert_season_stamp(NULL, 2027L))
})

test_that(".assert_season_stamp names the observed years in its message", {
  rows <- make_rows(c("2024-10-01", "2024-11-01", "2025-02-01"))
  expect_error(
    .assert_season_stamp(rows, 2027L, source = "hsi male/div1"),
    regexp = "hsi male/div1"
  )
})
```

- [ ] **Step 2: Run it, confirm the exact failure.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-season-stamp.R")'
```

Expected: every `test_that` block errors with `Error in .assert_season_stamp(...) : could not find function ".assert_season_stamp"` — 5 failed, 0 passed.

- [ ] **Step 3: Implement the guard.** Insert into `R/ingest.R` immediately after the closing brace of `.is_league_active()`:

```r
#' Abort when a fetched page's dates disagree with the season requested.
#'
#' A federation tournament id is a literal that is correct when written and
#' silently wrong later; nothing downstream notices, because a season-stamped
#' hive partition accepts any rows it is handed. This guard is what makes that
#' staleness loud. Icelandic winter seasons labelled `season` span calendar
#' years `season - 1` (autumn) and `season` (spring), so more than `tol` of the
#' parsed `match_date` years falling outside that pair means the page fetched is
#' not the season asked for -- a stale slug, a mis-mapped id, or a federation
#' URL-scheme change.
#'
#' Signals class `sports_season_stamp_error` so callers that otherwise degrade
#' fetch failures to warnings can re-raise it rather than swallow it.
#'
#' @param rows Tibble with a `match_date` Date column, or NULL. Zero rows and
#'   all-NA dates pass -- emptiness is a separate concern with its own checks.
#' @param season Integer season requested.
#' @param source Label naming the fetch, used in the abort message.
#' @param tol Maximum tolerated fraction of out-of-span calendar years.
#' @return `rows`, invisibly.
#' @keywords internal
#' @noRd
.assert_season_stamp <- function(rows, season, source = "unknown", tol = 0.05) {
  if (is.null(rows) || nrow(rows) == 0L) {
    return(invisible(rows))
  }
  years <- as.integer(format(rows$match_date, "%Y"))
  years <- years[!is.na(years)]
  if (length(years) == 0L) {
    return(invisible(rows))
  }

  season <- as.integer(season)
  allowed <- c(season - 1L, season)
  bad_frac <- mean(!(years %in% allowed))

  if (bad_frac > tol) {
    observed <- paste(sort(unique(years)), collapse = ", ")
    cli::cli_abort(
      c(
        "Season stamp mismatch for {source}: asked for season {season}.",
        "x" = paste0(
          "{round(100 * bad_frac, 1)}% of {length(years)} parsed dates fall ",
          "outside {allowed[1]}/{allowed[2]}."
        ),
        "i" = "Observed calendar years: {observed}.",
        "i" = "The tournament id registered for this (sex, division, season) is stale or wrong."
      ),
      class = "sports_season_stamp_error"
    )
  }

  invisible(rows)
}
```

- [ ] **Step 4: Run, expect pass.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-season-stamp.R")'
```

Expected: `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 9 ]`.

- [ ] **Step 5: Commit.**

```bash
git -C /Users/brynjolfurjonsson/sports add R/ingest.R tests/testthat/test-season-stamp.R
git -C /Users/brynjolfurjonsson/sports commit -m "feat(ingest): add .assert_season_stamp guard

A federation tournament id is a literal that is right when written and
silently wrong later. Nothing downstream notices, because a season-stamped
hive partition accepts whatever rows it is handed -- so a stale HSI slug
would write a whole 2025-26 season into season=2027 inside a directory five
cron jobs commit to daily. This turns that from a fake partition into a
loud abort before the first row is written. Shared with KKI (WS5).

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 2: the provenance cache — `config/federation-seasons.json` and its reader

**Files:**
- Create `/Users/brynjolfurjonsson/sports/R/federation-seasons.R`
- Create `/Users/brynjolfurjonsson/sports/tools/seed-federation-seasons.R`
- Create `/Users/brynjolfurjonsson/sports/config/federation-seasons.json` (generated by the seeder, never hand-written — the titles carry Icelandic characters)
- Test (create) `/Users/brynjolfurjonsson/sports/tests/testthat/test-federation-seasons.R`
- Modify `/Users/brynjolfurjonsson/sports/DESCRIPTION` (Collate, roxygen-generated)
- Modify `/Users/brynjolfurjonsson/sports/NAMESPACE` (roxygen-generated)

**Interfaces:**
- Consumes: nothing.
- Produces: `federation_seasons_path()`, `read_federation_seasons()`, `federation_season_id()`, `merge_federation_seasons()`, `refresh_federation_seasons()` (exported).

- [ ] **Step 1: Write the failing tests.**

```r
# tests/testthat/test-federation-seasons.R
entry <- function(sex, division, season, id, source = "live-nav",
                  verified = TRUE, title = NA_character_) {
  tibble::tibble(
    federation = "hsi", sex = sex, division = division,
    season = as.integer(season), id = as.integer(id),
    title = title, source = source,
    discovered_at = "2026-09-02", verified = verified,
    note = NA_character_
  )
}

test_that("read_federation_seasons round-trips what refresh writes", {
  path <- withr::local_tempfile(fileext = ".json")
  refresh_federation_seasons(entry("male", "div1", 2027L, 9142L), path = path)
  got <- read_federation_seasons(path)
  expect_equal(nrow(got), 1L)
  expect_identical(got$id, 9142L)
  expect_type(got$season, "integer")
  expect_type(got$verified, "logical")
})

test_that("read_federation_seasons returns a typed zero-row frame when absent", {
  got <- read_federation_seasons(file.path(tempdir(), "no-such-file.json"))
  expect_equal(nrow(got), 0L)
  expect_named(
    got,
    c("federation", "sex", "division", "season", "id", "title",
      "source", "discovered_at", "verified", "note")
  )
})

test_that("federation_season_id resolves only verified, season-attributed rows", {
  path <- withr::local_tempfile(fileext = ".json")
  refresh_federation_seasons(
    dplyr::bind_rows(
      entry("male", "div1", 2027L, 9142L),
      entry("male", "cup", NA_integer_, 8437L,
            source = "live-nav-unattributed", verified = FALSE)
    ),
    path = path
  )
  expect_identical(federation_season_id("hsi", "male", "div1", 2027L, path), 9142L)
  expect_null(federation_season_id("hsi", "male", "cup", 2027L, path))
  expect_null(federation_season_id("hsi", "female", "div1", 2027L, path))
  expect_null(federation_season_id("kki", "male", "div1", 2027L, path))
})

test_that("merge_federation_seasons upgrades an unverified row from a trusted source", {
  existing <- entry("female", "div2", 2025L, 7643L,
                    source = "inferred-candidate", verified = FALSE)
  incoming <- entry("female", "div2", 2025L, 7643L,
                    source = "inferred-verified", verified = TRUE)
  merged <- merge_federation_seasons(incoming, existing)
  expect_equal(nrow(merged), 1L)
  expect_true(merged$verified)
  expect_identical(merged$source, "inferred-verified")
})

test_that("merge_federation_seasons aborts when two verified rows disagree", {
  existing <- entry("male", "div1", 2027L, 9142L)
  incoming <- entry("male", "div1", 2027L, 9999L)
  expect_error(
    merge_federation_seasons(incoming, existing),
    class = "sports_federation_id_conflict"
  )
})

test_that("the committed cache seeds the four 2027 league ids", {
  cache <- read_federation_seasons()
  hsi27 <- cache[cache$federation == "hsi" &
                   !is.na(cache$season) & cache$season == 2027L, ]
  expect_setequal(
    hsi27$id,
    c(9142L, 9140L, 9141L, 9143L)
  )
  expect_true(all(hsi27$verified))
  expect_true(all(nzchar(hsi27$discovered_at)))
})

test_that("unattributed live-nav observations are recorded but unresolvable", {
  cache <- read_federation_seasons()
  unattr <- cache[is.na(cache$season), ]
  expect_setequal(unattr$id, c(8437L, 8436L))
  expect_false(any(unattr$verified))
})
```

- [ ] **Step 2: Run it, confirm the exact failure.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-federation-seasons.R")'
```

Expected: all 7 blocks error with `could not find function "refresh_federation_seasons"` / `"read_federation_seasons"` / `"merge_federation_seasons"` — 7 failed, 0 passed.

- [ ] **Step 3: Implement the cache module.**

```r
# R/federation-seasons.R
#' @include ingest.R
NULL

#' Column order and types of the federation-season provenance cache.
#' @keywords internal
#' @noRd
.federation_seasons_empty <- function() {
  tibble::tibble(
    federation = character(),
    sex = character(),
    division = character(),
    season = integer(),
    id = integer(),
    title = character(),
    source = character(),
    discovered_at = character(),
    verified = logical(),
    note = character()
  )
}

#' Trust ranking of provenance sources, highest first.
#'
#' `inferred-verified` outranks `hand-verified` because it is the only source
#' that has been checked against three independent properties of the fetched
#' page (title pattern, season stamp, roster intersection); a hand-verified id
#' was only ever eyeballed.
#' @keywords internal
#' @noRd
FEDERATION_SOURCE_TRUST <- c(
  "inferred-verified" = 5L,
  "hand-verified" = 4L,
  "live-nav" = 3L,
  "live" = 2L,
  "inferred-candidate" = 1L,
  "live-nav-unattributed" = 0L
)

#' Path to the git-tracked federation-season provenance cache.
#' @keywords internal
#' @noRd
federation_seasons_path <- function() {
  here::here("config", "federation-seasons.json")
}

#' Read the federation-season provenance cache.
#'
#' A missing file is not an error -- it means nothing has been discovered yet,
#' and every lookup falls through to NULL, which is the fail-safe direction.
#'
#' @param path Path to the cache JSON.
#' @return Tibble with the columns of [.federation_seasons_empty()].
#' @keywords internal
#' @noRd
read_federation_seasons <- function(path = federation_seasons_path()) {
  empty <- .federation_seasons_empty()
  if (!file.exists(path)) {
    return(empty)
  }
  payload <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  entries <- payload$entries
  if (is.null(entries) || length(entries) == 0L || nrow(entries) == 0L) {
    return(empty)
  }
  out <- tibble::as_tibble(entries)
  for (nm in names(empty)) {
    if (!nm %in% names(out)) out[[nm]] <- empty[[nm]][NA_integer_]
  }
  out$season <- as.integer(out$season)
  out$id <- as.integer(out$id)
  out$verified <- as.logical(out$verified)
  out[, names(empty), drop = FALSE]
}

#' Resolve one federation id from the provenance cache.
#'
#' Only `verified` entries carrying a season attribution resolve. An
#' observation whose season is unknown (`season = NA`) is deliberately
#' unresolvable: recording that an id exists is not the same as knowing which
#' season it belongs to.
#' @keywords internal
#' @noRd
federation_season_id <- function(federation, sex, division, season,
                                 path = federation_seasons_path()) {
  cache <- read_federation_seasons(path)
  if (nrow(cache) == 0L) {
    return(NULL)
  }
  hit <- cache[
    cache$federation == federation &
      cache$sex == sex &
      cache$division == division &
      !is.na(cache$season) & cache$season == as.integer(season) &
      !is.na(cache$verified) & cache$verified, ,
    drop = FALSE
  ]
  if (nrow(hit) == 0L) {
    return(NULL)
  }
  as.integer(hit$id[[1L]])
}

#' Merge discovered entries into an existing provenance cache.
#'
#' Keyed on (federation, sex, division, season) -- NA seasons key on the id
#' instead, since an unattributed observation is about the id, not a season.
#' Two `verified` rows disagreeing on `id` for the same key is a hard abort:
#' that is a federation renumbering or a bad discovery pass, and silently
#' picking one is exactly the class of quiet wrongness this workstream exists
#' to remove.
#' @keywords internal
#' @noRd
merge_federation_seasons <- function(new_entries,
                                     existing = .federation_seasons_empty()) {
  if (nrow(new_entries) == 0L) {
    return(existing)
  }
  combined <- dplyr::bind_rows(existing, new_entries)
  combined$.key <- ifelse(
    is.na(combined$season),
    paste(combined$federation, combined$sex, combined$division, "id", combined$id, sep = "/"),
    paste(combined$federation, combined$sex, combined$division, combined$season, sep = "/")
  )

  for (k in unique(combined$.key)) {
    grp <- combined[combined$.key == k, , drop = FALSE]
    ver <- grp[!is.na(grp$verified) & grp$verified, , drop = FALSE]
    if (nrow(ver) > 1L && length(unique(ver$id)) > 1L) {
      cli::cli_abort(
        c(
          "Conflicting verified federation ids for {k}.",
          "x" = "Ids seen: {paste(sort(unique(ver$id)), collapse = ', ')}.",
          "i" = "Resolve by hand before merging -- do not let discovery pick a winner."
        ),
        class = "sports_federation_id_conflict"
      )
    }
  }

  combined$.trust <- unname(FEDERATION_SOURCE_TRUST[combined$source])
  combined$.trust[is.na(combined$.trust)] <- -1L

  combined |>
    dplyr::arrange(.data$.key, dplyr::desc(.data$.trust)) |>
    dplyr::distinct(.data$.key, .keep_all = TRUE) |>
    dplyr::select(-".key", -".trust") |>
    dplyr::arrange(
      .data$federation, .data$sex, .data$division, .data$season, .data$id
    )
}

#' Merge entries into the provenance cache and rewrite it.
#'
#' The maintenance entry point: `hsi_discover_tournaments()` (and WS5's
#' `kki_discover_season_ids()`) produce rows, this persists them with their
#' provenance so the next session can tell a live-read id from an inferred one.
#'
#' @param entries Tibble shaped like [.federation_seasons_empty()].
#' @param path Path to the cache JSON.
#' @return The merged cache, invisibly.
#' @export
refresh_federation_seasons <- function(entries,
                                       path = federation_seasons_path()) {
  merged <- merge_federation_seasons(entries, read_federation_seasons(path))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(
    list(schema_version = 1L, entries = merged),
    path,
    pretty = TRUE,
    auto_unbox = TRUE,
    na = "null"
  )
  invisible(merged)
}
```

- [ ] **Step 4: Write the seeder and generate the cache.** The titles carry Icelandic characters, so the JSON is generated, never typed.

```r
# tools/seed-federation-seasons.R
#!/usr/bin/env Rscript
# Seed config/federation-seasons.json with the HSI ids observed on 2026-09-02.
# Idempotent: re-running merges the same rows back onto themselves.
#
#   Rscript tools/seed-federation-seasons.R

suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))

observed <- "2026-09-02"

seed <- tibble::tribble(
  ~sex,     ~division,  ~season,      ~id,     ~title_ascii,                    ~source,                 ~verified, ~note,
  "male",   "div1",     2027L,        9142L,   "Olis deild karla 2026-27",      "live-nav",              TRUE,      NA_character_,
  "male",   "div2",     2027L,        9140L,   "Grill 66 deild karla 2026-27",  "live-nav",              TRUE,      NA_character_,
  "female", "div1",     2027L,        9141L,   "Olis deild kvenna 2026-27",     "live-nav",              TRUE,      NA_character_,
  "female", "div2",     2027L,        9143L,   "Grill 66 deild kvenna umspil",  "live-nav",              TRUE,      "Nav title carries 'umspil'; confirm the 2026-27 female G66 format at first ingest.",
  "male",   "cup",      NA_integer_,  8437L,   "Bikar karla",                   "live-nav-unattributed", FALSE,     "Pre-change HSI_URLS attributed 8437 to 2025-26 (season 2026); it is still in the 2026-09-02 nav. Registered at 2026 in HSI_TOURNAMENT_IDS on the attributed evidence; season 2027 stays deferred until discovery attributes it.",
  "female", "cup",      NA_integer_,  8436L,   "Bikar kvenna",                  "live-nav-unattributed", FALSE,     "Observed but unreachable: hsi_divisions_for_sex('female') has no 'cup' division, so nothing would ever fetch it."
)

entries <- tibble::tibble(
  federation = "hsi",
  sex = seed$sex,
  division = seed$division,
  season = seed$season,
  id = seed$id,
  title = seed$title_ascii,
  source = seed$source,
  discovered_at = observed,
  verified = seed$verified,
  note = seed$note
)

merged <- refresh_federation_seasons(entries)
cli::cli_alert_success("Seeded {nrow(merged)} federation-season entries.")
```

Run it:

```bash
cd /Users/brynjolfurjonsson/sports && Rscript tools/seed-federation-seasons.R
```

Expected: `Seeded 6 federation-season entries.` and `config/federation-seasons.json` exists.

- [ ] **Step 5: Document and run the tests.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::document()' && Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-federation-seasons.R")'
```

Expected: `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 17 ]`. `DESCRIPTION` Collate gains `'federation-seasons.R'` and `NAMESPACE` gains `export(refresh_federation_seasons)`.

- [ ] **Step 6: Commit.**

```bash
git -C /Users/brynjolfurjonsson/sports add R/federation-seasons.R tools/seed-federation-seasons.R config/federation-seasons.json tests/testthat/test-federation-seasons.R DESCRIPTION NAMESPACE
git -C /Users/brynjolfurjonsson/sports commit -m "feat(config): provenance cache for federation season ids

Where a discovered tournament id is unavoidable, store it as a cache with
provenance -- value, source, discovered-at, verified -- rather than as an R
comment nobody re-reads. An R comment cannot say 'this was read off the live
nav on 2026-09-02 but its season is unattributed'; a row can, and a lookup
can then refuse to resolve it. Seeded via a script because the titles carry
Icelandic characters.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 3: collapse the two HSÍ registries into one season-keyed registry

**Files:**
- Modify `/Users/brynjolfurjonsson/sports/R/ingest-hsi-handball.R` — delete lines 4-30 (`HSI_URLS` + docstring), 48-102 (`HSI_HISTORICAL_IDS` + docstring), 104-121 (`hsi_historical_url`); rewrite 137-153 (`hsi_url`).
- Modify `/Users/brynjolfurjonsson/sports/tests/testthat/test-ingest-hsi.R` — replace lines 42-77.

**Interfaces:**
- Consumes: `federation_season_id()` (Task 2).
- Produces: `HSI_TOURNAMENT_IDS`, `hsi_tournament_id(sex, division, season)`, `hsi_url(sex, division, season)`.

Registry contents, and the arithmetic behind the assertions — 19 historical (male div1 5, male div2 5, female div1 5, female div2 4) + 4 × 2027 league + 1 male cup 2026 + 2 playoffs 2026 = **26 entries, 26 distinct ids**. Female div2 2025 is deliberately still absent; Task 7 adds it, taking both counts to 27.

- [ ] **Step 1: Replace the registry tests.** Delete `tests/testthat/test-ingest-hsi.R` lines 42-77 (the `HSI_HISTORICAL_IDS` and `hsi_historical_url` blocks) and append:

```r
test_that("HSI_TOURNAMENT_IDS is one season-keyed registry with distinct ids", {
  ids <- HSI_TOURNAMENT_IDS
  expect_setequal(names(ids), c("male", "female"))
  expect_setequal(names(ids$male), c("div1", "div2", "cup", "playoffs"))
  expect_setequal(names(ids$female), c("div1", "div2", "playoffs"))

  flat <- unlist(ids, use.names = FALSE)
  expect_type(flat, "integer")
  expect_true(all(flat > 0L))
  # 19 historical + 4 x 2027 league + male cup 2026 + 2 playoffs 2026.
  expect_equal(length(flat), 26L)
  # Every id distinct. This is what catches the legacy copy-paste that put the
  # male div2 2025 id (7644) under female div2 2025.
  expect_equal(length(unique(flat)), length(flat))
})

test_that("every registry (sex, division) is reachable from hsi_divisions_for_sex", {
  for (sex in names(HSI_TOURNAMENT_IDS)) {
    expect_setequal(
      names(HSI_TOURNAMENT_IDS[[sex]]),
      intersect(names(HSI_TOURNAMENT_IDS[[sex]]), hsi_divisions_for_sex(sex))
    )
  }
  # Concretely: a female "cup" key would never be fetched, so it must not exist.
  expect_false("cup" %in% names(HSI_TOURNAMENT_IDS$female))
})

test_that("hsi_url builds /tournament/<id> for every registered triple", {
  expect_equal(hsi_url("male", "div1", 2024L), "https://www.hsi.is/tournament/6983")
  expect_equal(hsi_url("male", "div1", 2027L), "https://www.hsi.is/tournament/9142")
  expect_equal(hsi_url("male", "div2", 2027L), "https://www.hsi.is/tournament/9140")
  expect_equal(hsi_url("female", "div1", 2027L), "https://www.hsi.is/tournament/9141")
  expect_equal(hsi_url("female", "div2", 2027L), "https://www.hsi.is/tournament/9143")
  expect_equal(hsi_url("male", "playoffs", 2026L), "https://www.hsi.is/tournament/8427")
  expect_equal(hsi_url("female", "playoffs", 2026L), "https://www.hsi.is/tournament/8430")
  expect_equal(hsi_url("male", "cup", 2026L), "https://www.hsi.is/tournament/8437")
})

test_that("hsi_url returns NULL rather than erroring on anything unregistered", {
  # NULL means do-not-fetch, which is the fail-safe direction.
  expect_null(hsi_url("male", "div1", 1999L))
  expect_null(hsi_url("other", "div1", 2024L))
  expect_null(hsi_url("male", "nonesuch", 2024L))
  # The legacy hole this workstream recovers -- still open until Task 7.
  expect_null(hsi_url("female", "div2", 2025L))
})

test_that("hsi_current_season names the requested season and nothing else", {
  # Icelandic winter seasons are labelled by their closing calendar year.
  expect_equal(hsi_current_season(as.Date("2026-09-02")), 2027L)
  expect_equal(hsi_current_season(as.Date("2027-03-01")), 2027L)
  expect_equal(hsi_current_season(as.Date("2026-06-30")), 2026L)
})

test_that("hsi_tournament_id falls back to the provenance cache", {
  path <- withr::local_tempfile(fileext = ".json")
  refresh_federation_seasons(
    tibble::tibble(
      federation = "hsi", sex = "male", division = "playoffs",
      season = 2027L, id = 9999L, title = NA_character_,
      source = "live-nav", discovered_at = "2026-09-02",
      verified = TRUE, note = NA_character_
    ),
    path = path
  )
  testthat::local_mocked_bindings(
    federation_seasons_path = function() path
  )
  expect_identical(hsi_tournament_id("male", "playoffs", 2027L), 9999L)
  # Registry still wins over cache for a key both hold.
  expect_identical(hsi_tournament_id("male", "div1", 2027L), 9142L)
})
```

- [ ] **Step 2: Run, confirm the exact failure.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-ingest-hsi.R")'
```

Expected: `object 'HSI_TOURNAMENT_IDS' not found` in two blocks, `could not find function "hsi_tournament_id"` in one, and in the `hsi_url` blocks `Error in hsi_url("male", "div1", 2024L): unused argument` is *not* what you get — the old signature accepts `season` positionally and returns the old slug, so those fail as `hsi_url("male","div1",2024L) not equal to "https://www.hsi.is/tournament/6983"` (actual: `"https://www.hsi.is/olis-deild-karla-2025-26"`), and `expect_null(hsi_url("other","div1",2024L))` fails as `Error: Unknown sex for HSI: other`. Roughly 6 failed blocks.

- [ ] **Step 3: Replace the two registries.** Delete `R/ingest-hsi-handball.R` lines 4-30, 48-102 and 104-121, and put this in their place (immediately after the `#' @include ingest.R` / `NULL` header):

```r
#' HSÍ tournament ids, keyed by (sex, division, season).
#'
#' Layout: `HSI_TOURNAMENT_IDS[[sex]][[division]][["<season>"]]` -> integer
#' `mot_nr` used in `https://www.hsi.is/tournament/{mot_nr}`. Seasons are
#' labelled by the closing calendar year (2025 = Sept 2024 - May 2025), the
#' same convention as [hsi_current_season()].
#'
#' This replaces the previous split between `HSI_URLS` (dated league slugs for
#' whichever season happened to be current when a human last edited the file)
#' and `HSI_HISTORICAL_IDS` (tournament ids for everything older). That split
#' was the defect: the slug and the season stamp came from two independent
#' sources and drifted apart every July, and `https://www.hsi.is/olis-deild-
#' karla-2026-27` is a 404 because HSÍ now serves `/tournament/<id>` only.
#' One shape, one key, one lookup.
#'
#' Provenance for every seeded value lives in `config/federation-seasons.json`,
#' not in this comment -- see [read_federation_seasons()]. `cup` and `playoffs`
#' have no 2027 entry: HSÍ has not created those tournaments yet, and
#' [hsi_unresolved_seasons()] reports the gap rather than a guessed id filling
#' it. Historical 2021-2025 values came from the legacy
#' `_legacy/sports/handball/iceland/R/utils/{male,female}/download_historical_
#' data_div{1,2}.R`.
#' @keywords internal
#' @noRd
HSI_TOURNAMENT_IDS <- list(
  male = list(
    div1 = list(
      "2021" = 5260L, "2022" = 5640L, "2023" = 6149L,
      "2024" = 6983L, "2025" = 7641L, "2027" = 9142L
    ),
    div2 = list(
      "2021" = 5262L, "2022" = 5643L, "2023" = 6143L,
      "2024" = 6981L, "2025" = 7644L, "2027" = 9140L
    ),
    cup = list(
      "2026" = 8437L
    ),
    playoffs = list(
      "2026" = 8427L
    )
  ),
  female = list(
    div1 = list(
      "2021" = 5261L, "2022" = 5641L, "2023" = 6146L,
      "2024" = 6982L, "2025" = 7642L, "2027" = 9141L
    ),
    div2 = list(
      "2021" = 5263L, "2022" = 5642L, "2023" = 6148L,
      "2024" = 6980L, "2027" = 9143L
    ),
    playoffs = list(
      "2026" = 8430L
    )
  )
)
```

Then replace `hsi_url()` (old lines 137-153) with:

```r
#' Resolve an HSÍ tournament id for a (sex, division, season) triple.
#'
#' Registry first, `config/federation-seasons.json` cache second, `NULL` third.
#' `NULL` means do not fetch, which is the fail-safe direction -- an
#' unregistered triple must never fall back to "some other season's page".
#' Unknown sexes and divisions resolve to `NULL` rather than aborting, so a
#' config typo skips one cell instead of taking the whole ingest down.
#' @keywords internal
#' @noRd
hsi_tournament_id <- function(sex, division, season) {
  key <- as.character(as.integer(season))
  from_registry <- HSI_TOURNAMENT_IDS[[sex]][[division]][[key]]
  if (!is.null(from_registry)) {
    return(as.integer(from_registry))
  }
  federation_season_id("hsi", sex, division, season)
}

#' Build the HSÍ tournament URL for a (sex, division, season) triple.
#'
#' @return Character URL, or `NULL` when the triple has no resolvable id.
#' @keywords internal
#' @noRd
hsi_url <- function(sex, division, season) {
  id <- hsi_tournament_id(sex, division, season)
  if (is.null(id) || is.na(id)) {
    return(NULL)
  }
  sprintf("https://www.hsi.is/tournament/%d", id)
}
```

Add `#' @include federation-seasons.R` to the file header so Collate orders the cache reader first:

```r
#' @include ingest.R
#' @include federation-seasons.R
NULL
```

- [ ] **Step 4: Run, expect pass.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::document()' && Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-ingest-hsi.R")'
```

Expected: `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 27 ]` — the two surviving original blocks (parser, source registration) plus the six new ones.

- [ ] **Step 5: Commit.**

```bash
git -C /Users/brynjolfurjonsson/sports add R/ingest-hsi-handball.R tests/testthat/test-ingest-hsi.R DESCRIPTION
git -C /Users/brynjolfurjonsson/sports commit -m "refactor(hsi): one season-keyed tournament registry

HSI_URLS pinned dated league slugs for whichever season a human last edited,
while hsi_current_season() derived the season independently -- two sources
that drift apart every July. They have now drifted: olis-deild-karla-2026-27
is a 404 and HSI serves /tournament/<id> only. Collapsing both registries
onto one (sex, division, season) key removes the drift by construction, and
an unregistered triple resolves to NULL (do not fetch) rather than silently
falling back to another season's page.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 4: wire the guard into the fetch path and delete the current-vs-historical branch

**Files:**
- Modify `/Users/brynjolfurjonsson/sports/R/ingest-hsi-handball.R` — `hsi_fetch_and_parse()` (pre-change lines 533-561), `fetch_results_hsi()` (563-622), `fetch_schedule_hsi()` (624-673).
- Modify `/Users/brynjolfurjonsson/sports/tests/testthat/test-ingest-hsi.R` (append).

**Interfaces:**
- Consumes: `.assert_season_stamp()` (Task 1), `hsi_url()` (Task 3).
- Produces: `hsi_fetch_and_parse(url, sex, div, division_label, season)` — unchanged signature, but a `sports_season_stamp_error` now propagates instead of degrading to a warning.

The re-raise is the whole point: `hsi_fetch_and_parse()` wraps everything in a `tryCatch(error = ...)` that warns and returns `NULL` so one bad tournament page cannot take down the league ingest. A season-stamp abort caught by that handler would become a warning, i.e. exactly the silence the guard exists to break. `tryCatch` dispatches to the first matching handler, so the specific class goes first.

- [ ] **Step 1: Write the failing tests.** Append to `tests/testthat/test-ingest-hsi.R`:

```r
test_that("the season-stamp guard fires on the fixture's real dates", {
  html <- rvest::read_html(fixture("male_div1_current.html"), encoding = "UTF-8")
  rows <- parse_hsi_results_page(
    html, "handball", "iceland", "male", "OD", 2026L
  )
  # The fixture is the 2025-26 season: 108 rows, 66 in 2025, 42 in 2026.
  expect_equal(nrow(rows), 108L)
  expect_setequal(unique(format(rows$match_date, "%Y")), c("2025", "2026"))
  # Requested as its own season -> passes.
  expect_no_error(.assert_season_stamp(rows, 2026L, source = "fixture"))
  # Requested as 2027 -> 61.1% of dates outside {2026, 2027} -> aborts.
  expect_error(
    .assert_season_stamp(rows, 2027L, source = "fixture"),
    class = "sports_season_stamp_error"
  )
})

test_that("hsi_fetch_and_parse re-raises a season-stamp abort, not a warning", {
  testthat::local_mocked_bindings(
    fetch_hsi_html = function(url, ...) {
      rvest::read_html(fixture("male_div1_current.html"), encoding = "UTF-8")
    }
  )
  # Fixture is season 2026; asked for as 2027 the guard must escape the
  # warn-and-return-NULL handler that wraps ordinary fetch failures.
  expect_error(
    hsi_fetch_and_parse(
      "https://www.hsi.is/tournament/9142", "male", "div1", "OD", 2027L
    ),
    class = "sports_season_stamp_error"
  )
})

test_that("hsi_fetch_and_parse still degrades an ordinary fetch failure to a warning", {
  testthat::local_mocked_bindings(
    fetch_hsi_html = function(url, ...) stop("chromote boot failed")
  )
  expect_warning(
    out <- hsi_fetch_and_parse(
      "https://www.hsi.is/tournament/9142", "male", "div1", "OD", 2026L
    ),
    "HSI results fetch failed"
  )
  expect_null(out)
})

test_that("fetch_results_hsi stamps rows with the season it asked for", {
  testthat::local_mocked_bindings(
    fetch_hsi_html = function(url, ...) {
      rvest::read_html(fixture("male_div1_current.html"), encoding = "UTF-8")
    },
    # The fixture is male div1 2026; register only that triple so the mocked
    # page is never served for a season it does not belong to.
    hsi_url = function(sex, division, season) {
      if (sex == "male" && division == "div1" && season == 2026L) {
        "https://www.hsi.is/tournament/8000"
      } else {
        NULL
      }
    },
    Sys.sleep = function(...) invisible(NULL)
  )
  out <- fetch_results_hsi(NULL, "male", seasons = 2026L)
  expect_equal(nrow(out), 108L)
  expect_true(all(out$season == 2026L))
  expect_true(all(out$division == "OD"))
})

test_that("fetch_results_hsi aborts when a registered id serves the wrong season", {
  testthat::local_mocked_bindings(
    fetch_hsi_html = function(url, ...) {
      rvest::read_html(fixture("male_div1_current.html"), encoding = "UTF-8")
    },
    hsi_url = function(sex, division, season) {
      if (sex == "male" && division == "div1") {
        "https://www.hsi.is/tournament/8000"
      } else {
        NULL
      }
    },
    Sys.sleep = function(...) invisible(NULL)
  )
  # Same page, asked for as 2027. This is the RED proof of the guard: a stale
  # registry entry must stop the run, not write a fake season=2027 partition.
  expect_error(
    fetch_results_hsi(NULL, "male", seasons = 2027L),
    class = "sports_season_stamp_error"
  )
})

test_that("fetch_schedule_hsi guards its rows against the requested season too", {
  synthetic <- tibble::tibble(
    sport = "handball", country = "iceland", sex = "male",
    season = 2027L,
    match_date = as.Date(c("2024-10-01", "2024-11-01", "2025-01-15")),
    home_team = c("A", "B", "C"), away_team = c("D", "E", "F"),
    division = "OD", round = NA_integer_
  )
  expect_error(
    .assert_season_stamp(synthetic, 2027L, source = "hsi male/div1 schedule"),
    class = "sports_season_stamp_error"
  )
  # A zero-row schedule (the fixture's case: the season is fully played) passes.
  expect_no_error(
    .assert_season_stamp(hsi_empty_schedule(), 2027L, source = "empty")
  )
})
```

- [ ] **Step 2: Run, confirm the exact failure.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-ingest-hsi.R")'
```

Expected 3 failures. `hsi_fetch_and_parse re-raises` fails as `expect_error(...) did not throw an error` (it returns rows, because no guard is called yet). `fetch_results_hsi stamps rows` fails with `Error: Unknown sex for HSI: male`-style breakage or a wrong row count, because `fetch_results_hsi` still reads `HSI_URLS`, which no longer exists — the concrete message is `object 'HSI_URLS' not found`. `fetch_results_hsi aborts` fails the same way.

- [ ] **Step 3: Wire the guard and delete the branch.** Replace the body of `hsi_fetch_and_parse()`:

```r
#' Fetch a single HSÍ tournament page and parse into results rows.
#'
#' The ordinary-failure handler degrades a fetch error to a warning so one bad
#' tournament page cannot take down a league's ingest. A season-stamp mismatch
#' is deliberately NOT an ordinary failure: it means the id we hold is wrong,
#' and warning about it would write the wrong rows anyway. `tryCatch` dispatches
#' to the first matching handler, so the specific class is listed first and
#' re-raised.
#' @keywords internal
#' @noRd
hsi_fetch_and_parse <- function(url, sex, div, division_label, season) {
  tryCatch(
    {
      html <- fetch_hsi_html(url)
      rows <- parse_hsi_results_page(
        html,
        sport = "handball",
        country = "iceland",
        sex = sex,
        division = division_label,
        season = season
      )
      .assert_season_stamp(
        rows, season,
        source = sprintf("hsi %s/%s results (%s)", sex, div, url)
      )
      rows
    },
    sports_season_stamp_error = function(e) stop(e),
    error = function(e) {
      cli::cli_warn(c(
        "HSI results fetch failed for {sex}/{div} season={season}",
        "i" = "{conditionMessage(e)}"
      ))
      NULL
    }
  )
}
```

Replace the body of `fetch_results_hsi()` (the current-vs-historical branch goes entirely):

```r
#' Source-module entrypoint: results for a (league, sex).
#'
#' Iterates the configured divisions for the requested sex and, for each
#' requested season, resolves a `/tournament/<id>` URL via [hsi_url()]. There is
#' no current-vs-historical branch any more: every season is the same shape of
#' lookup against the same registry, so "this season" stops being a special
#' case that a human has to re-point every July.
#'
#' An unresolvable (sex, division, season) is skipped with a warning naming it,
#' never silently -- see [hsi_unresolved_seasons()].
#'
#' @param league Unused (source-module signature parity).
#' @param sex "male" or "female".
#' @param seasons Optional integer vector. `NULL` means the current season only.
#' @keywords internal
#' @noRd
fetch_results_hsi <- function(league, sex, seasons = NULL) {
  requested <- if (is.null(seasons)) {
    hsi_current_season()
  } else {
    as.integer(seasons)
  }
  divisions <- hsi_divisions_for_sex(sex)
  frames <- list()

  for (div in divisions) {
    division_label <- HSI_DIVISION_LABELS[[div]]

    for (season in requested) {
      url <- hsi_url(sex, div, season)
      if (is.null(url)) {
        cli::cli_warn(
          "HSI: no tournament id for {sex}/{div} season={season} -- skipped."
        )
        next
      }

      parsed <- hsi_fetch_and_parse(url, sex, div, division_label, season)
      if (!is.null(parsed)) frames[[length(frames) + 1L]] <- parsed

      Sys.sleep(HSI_HISTORICAL_SLEEP_SECS)
    }
  }

  if (length(frames) == 0L) {
    return(hsi_empty_results())
  }
  dplyr::bind_rows(frames)
}
```

And `fetch_schedule_hsi()`:

```r
#' Source-module entrypoint: schedule (upcoming only) for a (league, sex).
#'
#' Same registry lookup as [fetch_results_hsi()], for the current season only,
#' with the same season-stamp guard: a schedule scraped off a stale page is as
#' wrong as results scraped off one, and schedules feed the fixture window that
#' drives odds and decide.
#' @keywords internal
#' @noRd
fetch_schedule_hsi <- function(league, sex) {
  current <- hsi_current_season()
  divisions <- hsi_divisions_for_sex(sex)
  frames <- list()

  for (div in divisions) {
    division_label <- HSI_DIVISION_LABELS[[div]]
    url <- hsi_url(sex, div, current)
    if (is.null(url)) {
      cli::cli_warn(
        "HSI: no tournament id for {sex}/{div} season={current} -- schedule skipped."
      )
      next
    }

    parsed <- tryCatch(
      {
        html <- fetch_hsi_html(url)
        rows <- parse_hsi_schedule_page(
          html,
          sport = "handball",
          country = "iceland",
          sex = sex,
          division = division_label,
          season = current
        )
        .assert_season_stamp(
          rows, current,
          source = sprintf("hsi %s/%s schedule (%s)", sex, div, url)
        )
        rows
      },
      sports_season_stamp_error = function(e) stop(e),
      error = function(e) {
        cli::cli_warn(c(
          "HSI schedule fetch failed for {sex}/{div}",
          "i" = "{conditionMessage(e)}"
        ))
        NULL
      }
    )
    if (is.null(parsed)) next
    frames[[length(frames) + 1L]] <- parsed
  }

  if (length(frames) == 0L) {
    return(hsi_empty_schedule())
  }
  combined <- dplyr::bind_rows(frames)
  combined[combined$match_date >= Sys.Date(), , drop = FALSE]
}
```

Rename the sleep constant's docstring, since it is no longer historical-only — change the roxygen title at (pre-change) line 524 to `#' Delay between consecutive HSÍ tournament-page fetches (seconds).` and leave the value at `3`.

- [ ] **Step 4: Run, expect pass.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::document()' && Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-ingest-hsi.R")'
```

Expected: `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 43 ]`.

- [ ] **Step 5: Commit.**

```bash
git -C /Users/brynjolfurjonsson/sports add R/ingest-hsi-handball.R tests/testthat/test-ingest-hsi.R
git -C /Users/brynjolfurjonsson/sports commit -m "feat(hsi): abort on a season-stamp mismatch instead of writing the rows

fetch_results_hsi had a current-vs-historical branch: the current season came
from a hand-edited slug, older seasons from a tournament id. One shape now,
one lookup. And because the ingest gate comes off in WS6, a stale id would
otherwise write a whole 2025-26 season into season=2027 under a git-tracked
hive partition five cron jobs commit to daily -- so the guard is re-raised
past the warn-and-return-NULL handler rather than caught by it.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 5: make the cup/playoff deferral explicit and reportable

**Files:**
- Modify `/Users/brynjolfurjonsson/sports/R/ingest-hsi-handball.R` (append `hsi_unresolved_seasons()` after `hsi_url()`)
- Modify `/Users/brynjolfurjonsson/sports/tests/testthat/test-ingest-hsi.R` (append)

**Interfaces:**
- Consumes: `hsi_tournament_id()`, `hsi_divisions_for_sex()`.
- Produces: `hsi_unresolved_seasons(season, sexes = c("male", "female"))` -> tibble(sex, division, season). WS12's `check_season_resolution()` consumes it.

Why this task exists: before the change, `fetch_schedule_hsi` and `fetch_results_hsi` reached playoffs (male 8427, female 8430) and the male cup (8437) *unconditionally* via `HSI_URLS`, because those live in the current-season table. Under a season-keyed registry with no 2027 playoff or cup id, those fetches simply stop. `data/facts/results` holds PO rows for 2026 only, so the loss would look identical to ordinary off-season emptiness. HSÍ has not created the 2026-27 úrslitakeppni yet (it is created in spring), so the honest move is to defer it and make the deferral assertable and loud — not to guess an id.

- [ ] **Step 1: Write the failing tests.** Append to `tests/testthat/test-ingest-hsi.R`:

```r
test_that("cup and playoffs are explicitly deferred for 2027, not silently dropped", {
  gaps <- hsi_unresolved_seasons(2027L)
  expect_s3_class(gaps, "tbl_df")
  expect_named(gaps, c("sex", "division", "season"))
  expect_setequal(
    paste(gaps$sex, gaps$division),
    c("male cup", "male playoffs", "female playoffs")
  )
  # The four league cells DO resolve for 2027 -- the deferral is scoped.
  expect_false(any(gaps$division %in% c("div1", "div2")))
})

test_that("hsi_unresolved_seasons is empty for a fully-registered season", {
  # 2026: male cup 8437 + both playoffs 8427/8430 are registered, but the four
  # league cells are not (they were only ever reachable as dated slugs).
  gaps26 <- hsi_unresolved_seasons(2026L)
  expect_setequal(
    paste(gaps26$sex, gaps26$division),
    c("male div1", "male div2", "female div1", "female div2")
  )
  # 2024: every league cell registered, cup and playoffs never were.
  gaps24 <- hsi_unresolved_seasons(2024L)
  expect_false(any(gaps24$division %in% c("div1", "div2")))
})

test_that("an unresolved division warns and the rest of the ingest continues", {
  testthat::local_mocked_bindings(
    fetch_hsi_html = function(url, ...) {
      rvest::read_html(fixture("male_div1_current.html"), encoding = "UTF-8")
    },
    hsi_url = function(sex, division, season) {
      if (division == "div1") "https://www.hsi.is/tournament/8000" else NULL
    },
    Sys.sleep = function(...) invisible(NULL)
  )
  expect_warning(
    out <- fetch_results_hsi(NULL, "male", seasons = 2026L),
    "no tournament id for male/div2"
  )
  # div1 still came back: an unresolved division skips itself, not the league.
  expect_equal(nrow(out), 108L)
  expect_true(all(out$division == "OD"))
})
```

- [ ] **Step 2: Run, confirm the exact failure.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-ingest-hsi.R")'
```

Expected: the first two blocks fail with `could not find function "hsi_unresolved_seasons"`; the third passes already (Task 4 added the warning).

- [ ] **Step 3: Implement.** Append to `R/ingest-hsi-handball.R` after `hsi_url()`:

```r
#' Reachable HSÍ (sex, division) pairs with no resolvable id for a season.
#'
#' Before the season-keyed registry, `playoffs` and `cup` were fetched
#' unconditionally from the current-season table, so they were scraped for
#' whatever season happened to be current -- which is why `data/facts/results`
#' holds PO rows for 2026 only. Under the registry they are ordinary
#' (sex, division, season) triples, and HSÍ does not create the úrslitakeppni
#' or the 2026-27 bikar tournaments until later in the season.
#'
#' Deferring them is correct; deferring them silently is not, because the
#' absence is indistinguishable from ordinary off-season emptiness. This
#' function names the gap so [fetch_results_hsi()] can warn and WS12's
#' `check_season_resolution()` can raise it.
#'
#' @param season Integer season to check.
#' @param sexes Sexes to check.
#' @return Tibble with columns `sex`, `division`, `season`.
#' @keywords internal
#' @noRd
hsi_unresolved_seasons <- function(season, sexes = c("male", "female")) {
  rows <- list()
  for (sex in sexes) {
    for (div in hsi_divisions_for_sex(sex)) {
      if (is.null(hsi_tournament_id(sex, div, season))) {
        rows[[length(rows) + 1L]] <- tibble::tibble(
          sex = sex, division = div, season = as.integer(season)
        )
      }
    }
  }
  if (length(rows) == 0L) {
    return(tibble::tibble(
      sex = character(), division = character(), season = integer()
    ))
  }
  dplyr::bind_rows(rows)
}
```

- [ ] **Step 4: Run, expect pass.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::document()' && Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-ingest-hsi.R")'
```

Expected: `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 52 ]`.

- [ ] **Step 5: Commit.**

```bash
git -C /Users/brynjolfurjonsson/sports add R/ingest-hsi-handball.R tests/testthat/test-ingest-hsi.R
git -C /Users/brynjolfurjonsson/sports commit -m "feat(hsi): report deferred cup/playoff seasons instead of dropping them

Playoffs and cup used to ride the current-season table, so they were scraped
for whatever season was current -- hence PO rows for 2026 only. Season-keying
them is right, but HSI has not created the 2026-27 urslitakeppni yet, and an
unresolved division that skips quietly is indistinguishable from an off-season
with no fixtures. hsi_unresolved_seasons() names the gap; fetch_results_hsi
warns per skipped cell and carries on with the rest.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 6: `hsi_discover_tournaments()` — demote the registry to a verified cache

**Files:**
- Modify `/Users/brynjolfurjonsson/sports/R/ingest-hsi-handball.R` (append `HSI_TITLE_PATTERNS`, `hsi_page_title()`, `parse_hsi_tournament_index()`, `hsi_discover_tournaments()`)
- Modify `/Users/brynjolfurjonsson/sports/tests/testthat/test-ingest-hsi.R` (append)
- Create `/Users/brynjolfurjonsson/sports/tests/testthat/fixtures/hsi_handball/tournament_index.html`

**Interfaces:**
- Consumes: `fetch_hsi_html()`, `refresh_federation_seasons()`.
- Produces: `hsi_page_title(html)`, `parse_hsi_tournament_index(html)`, `hsi_discover_tournaments(index_url, season)`.

The network is a thin wrapper; the parse and the title→(sex, division) mapping are pure and fixture-tested.

- [ ] **Step 1: Write the failing parser tests** (inline HTML, so this runs before any capture). Append to `tests/testthat/test-ingest-hsi.R`:

```r
test_that("hsi_page_title strips the HSI suffix", {
  html <- rvest::read_html(fixture("male_div1_current.html"), encoding = "UTF-8")
  expect_equal(hsi_page_title(html), "Olís deild karla 2025-26")
})

test_that("parse_hsi_tournament_index reads ids and titles off a nav listing", {
  doc <- rvest::read_html(paste0(
    "<html><body><nav>",
    "<a href='/tournament/9142'>Olís deild karla 2026-27</a>",
    "<a href='/tournament/9140'>Grill 66 deild karla 2026-27</a>",
    "<a href='/tournament/9141'>Olís deild kvenna 2026-27</a>",
    "<a href='/tournament/9143'>Grill 66 deild kvenna umspil</a>",
    "<a href='/tournament/9142'>Olís deild karla 2026-27</a>",
    "<a href='/frettir/eitthvad'>Fréttir</a>",
    "</nav></body></html>"
  ))
  idx <- parse_hsi_tournament_index(doc)
  expect_named(idx, c("id", "title"))
  expect_type(idx$id, "integer")
  # Non-tournament links dropped, duplicate link deduplicated.
  expect_equal(nrow(idx), 4L)
  expect_setequal(idx$id, c(9142L, 9140L, 9141L, 9143L))
})

test_that("HSI_TITLE_PATTERNS maps every live nav title to one (sex, division)", {
  titles <- c(
    "Olís deild karla 2026-27",
    "Grill 66 deild karla 2026-27",
    "Olís deild kvenna 2026-27",
    "Grill 66 deild kvenna umspil",
    "Fréttir og viðburdir"
  )
  mapped <- .hsi_match_title(titles)
  expect_equal(mapped$sex, c("male", "male", "female", "female", NA_character_))
  expect_equal(
    mapped$division,
    c("div1", "div2", "div1", "div2", NA_character_)
  )
})

test_that("hsi_discover_tournaments returns provenance-shaped rows", {
  doc <- rvest::read_html(paste0(
    "<html><body>",
    "<a href='/tournament/9142'>Olís deild karla 2026-27</a>",
    "<a href='/tournament/9141'>Olís deild kvenna 2026-27</a>",
    "<a href='/tournament/1'>Handbolti á Íslandi</a>",
    "</body></html>"
  ))
  testthat::local_mocked_bindings(fetch_hsi_html = function(url, ...) doc)
  got <- hsi_discover_tournaments(season = 2027L)
  expect_named(
    got,
    c("federation", "sex", "division", "season", "id", "title",
      "source", "discovered_at", "verified", "note")
  )
  # Unmappable titles are dropped, not guessed at.
  expect_equal(nrow(got), 2L)
  expect_true(all(got$federation == "hsi"))
  expect_true(all(got$source == "live"))
  expect_true(all(got$verified))
  expect_identical(
    got$id[got$sex == "male" & got$division == "div1"], 9142L
  )
})

test_that("a discovery pass merges into the cache without disturbing the registry", {
  path <- withr::local_tempfile(fileext = ".json")
  discovered <- tibble::tibble(
    federation = "hsi", sex = "male", division = "playoffs",
    season = 2027L, id = 9500L, title = "Úrslitakeppni karla 2026-27",
    source = "live", discovered_at = format(Sys.Date()),
    verified = TRUE, note = NA_character_
  )
  refresh_federation_seasons(discovered, path = path)
  testthat::local_mocked_bindings(federation_seasons_path = function() path)
  expect_equal(hsi_url("male", "playoffs", 2027L), "https://www.hsi.is/tournament/9500")
  expect_equal(nrow(hsi_unresolved_seasons(2027L)), 2L)
})
```

- [ ] **Step 2: Run, confirm the exact failure.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-ingest-hsi.R")'
```

Expected: `could not find function "hsi_page_title"`, `"parse_hsi_tournament_index"`, `".hsi_match_title"`, `"hsi_discover_tournaments"` — 4 failed blocks; the last block fails on `hsi_url("male","playoffs",2027L)` returning `NULL` (the cache fallback exists but nothing populates it in that shape yet — it will pass once the rest compiles).

- [ ] **Step 3: Implement.** Append to `R/ingest-hsi-handball.R`:

```r
#' Title patterns mapping an HSÍ tournament title to (sex, division).
#'
#' Ordered: the first match wins, so the more specific "Grill 66" patterns must
#' precede nothing here -- they are disjoint from "Olís" -- but ordering is kept
#' explicit so adding a pattern later cannot silently shadow one.
#' @keywords internal
#' @noRd
HSI_TITLE_PATTERNS <- tibble::tibble(
  pattern = c(
    "^Olís\\s*deild\\s+karla",
    "^Grill\\s*66\\s*deild\\s+karla",
    "^Olís\\s*deild\\s+kvenna",
    "^Grill\\s*66\\s*deild\\s+kvenna",
    "^Úrslitakeppni\\s+karla",
    "^Úrslitakeppni\\s+kvenna",
    "^(Coca[- ]?Cola\\s+)?[Bb]ikar\\s*(keppni)?\\s+karla"
  ),
  sex = c("male", "male", "female", "female", "male", "female", "male"),
  division = c("div1", "div2", "div1", "div2", "playoffs", "playoffs", "cup")
)

#' Map tournament titles to (sex, division); NA where no pattern matches.
#'
#' An unmappable title is dropped by the caller rather than guessed at -- the
#' whole point of discovery is that it is more trustworthy than a guess.
#' @keywords internal
#' @noRd
.hsi_match_title <- function(titles) {
  sex <- rep(NA_character_, length(titles))
  division <- rep(NA_character_, length(titles))
  for (i in seq_len(nrow(HSI_TITLE_PATTERNS))) {
    hit <- is.na(sex) &
      stringr::str_detect(titles, HSI_TITLE_PATTERNS$pattern[[i]])
    sex[hit] <- HSI_TITLE_PATTERNS$sex[[i]]
    division[hit] <- HSI_TITLE_PATTERNS$division[[i]]
  }
  tibble::tibble(title = titles, sex = sex, division = division)
}

#' The page title of a rendered HSÍ page, without the " | HSÍ" suffix.
#' @keywords internal
#' @noRd
hsi_page_title <- function(html) {
  raw <- rvest::html_element(html, "title") |> rvest::html_text2()
  stringr::str_trim(stringr::str_replace(raw, "\\s*\\|\\s*HSÍ\\s*$", ""))
}

#' Parse `/tournament/<id>` links and their titles out of a rendered page.
#'
#' Pure function -- network lives in [hsi_discover_tournaments()], so the title
#' mapping stays fixture-testable.
#'
#' @param html xml_document.
#' @return Tibble with `id` (integer) and `title` (character), deduplicated.
#' @keywords internal
#' @noRd
parse_hsi_tournament_index <- function(html) {
  links <- rvest::html_elements(html, "a[href*='/tournament/']")
  if (length(links) == 0L) {
    return(tibble::tibble(id = integer(), title = character()))
  }
  hrefs <- rvest::html_attr(links, "href")
  ids <- suppressWarnings(as.integer(
    stringr::str_match(hrefs, "/tournament/(\\d+)")[, 2L]
  ))
  titles <- stringr::str_trim(rvest::html_text2(links))

  out <- tibble::tibble(id = ids, title = titles)
  out <- out[!is.na(out$id) & nzchar(out$title), , drop = FALSE]
  dplyr::distinct(out, .data$id, .keep_all = TRUE)
}

#' Discover HSÍ tournament ids for a season off the live site.
#'
#' This is what stops the registry being the thing that goes stale: it reads
#' the ids HSÍ is actually serving today, rather than the ones a human typed
#' last September. Merge the result with [refresh_federation_seasons()]; the
#' registry then becomes a verified cache rather than the sole source.
#'
#' HSÍ's site is client-side rendered, so this goes through the existing
#' chromote-backed [fetch_hsi_html()] -- a plain `httr` GET returns a shell.
#'
#' @param index_url Tournament index / navigation page.
#' @param season Season to attribute the discovered ids to. The caller is
#'   asserting "this index is showing season N"; [.assert_season_stamp()] is
#'   what checks that assertion the first time each id is fetched.
#' @return Tibble shaped like the provenance cache.
#' @keywords internal
#' @noRd
hsi_discover_tournaments <- function(index_url = "https://www.hsi.is/mot",
                                     season = hsi_current_season()) {
  html <- fetch_hsi_html(index_url, min_tables = 0L, min_rows = 0L)
  idx <- parse_hsi_tournament_index(html)
  mapped <- .hsi_match_title(idx$title)

  out <- tibble::tibble(
    federation = "hsi",
    sex = mapped$sex,
    division = mapped$division,
    season = as.integer(season),
    id = idx$id,
    title = idx$title,
    source = "live",
    discovered_at = format(Sys.Date()),
    verified = TRUE,
    note = NA_character_
  )
  out[!is.na(out$sex) & !is.na(out$division), , drop = FALSE]
}
```

- [ ] **Step 4: Run, expect pass.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::document()' && Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-ingest-hsi.R")'
```

Expected: `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 71 ]`.

- [ ] **Step 5: Capture the live nav fixture and check the parser against it.** This is a live chromote run; it takes ~30-60 s.

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e '
devtools::load_all(".")
page <- rvest::read_html_live("https://www.hsi.is/mot")
Sys.sleep(10)
outer <- page$session$Runtime$evaluate("document.documentElement.outerHTML")$result$value
Encoding(outer) <- "UTF-8"
writeLines(outer, "tests/testthat/fixtures/hsi_handball/tournament_index.html", useBytes = TRUE)
page$session$close()
doc <- rvest::read_html("tests/testthat/fixtures/hsi_handball/tournament_index.html", encoding = "UTF-8")
idx <- parse_hsi_tournament_index(doc)
print(as.data.frame(idx))
print(as.data.frame(.hsi_match_title(idx$title)))
'
```

Read the printed table. The four league ids 9142 / 9140 / 9141 / 9143 must appear and map to (male, div1), (male, div2), (female, div1), (female, div2) respectively. Add a fixture test asserting exactly that:

```r
test_that("the captured hsi.is index maps the four 2027 league ids", {
  doc <- rvest::read_html(fixture("tournament_index.html"), encoding = "UTF-8")
  idx <- parse_hsi_tournament_index(doc)
  mapped <- .hsi_match_title(idx$title)
  resolved <- tibble::tibble(
    id = idx$id, sex = mapped$sex, division = mapped$division
  )
  resolved <- resolved[!is.na(resolved$sex), , drop = FALSE]
  expect_identical(resolved$id[resolved$sex == "male" & resolved$division == "div1"], 9142L)
  expect_identical(resolved$id[resolved$sex == "male" & resolved$division == "div2"], 9140L)
  expect_identical(resolved$id[resolved$sex == "female" & resolved$division == "div1"], 9141L)
  expect_identical(resolved$id[resolved$sex == "female" & resolved$division == "div2"], 9143L)
})
```

Re-run the file; expect `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 75 ]`.

- [ ] **Step 6: Commit.**

```bash
git -C /Users/brynjolfurjonsson/sports add R/ingest-hsi-handball.R tests/testthat/test-ingest-hsi.R tests/testthat/fixtures/hsi_handball/tournament_index.html
git -C /Users/brynjolfurjonsson/sports commit -m "feat(hsi): discover tournament ids off the live site

Seeding the registry from hsi.is once only moves the staleness date; the
registry has to stop being the sole source. hsi_discover_tournaments reads
what HSI is serving today and refresh_federation_seasons persists it with
provenance, so hsi_url resolves registry -> cache -> NULL and the spring
urslitakeppni ids arrive without a hand edit. Title mapping and index parse
are pure and fixture-tested; only the fetch touches the network.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 7: verify the recovered id 7643 and backfill female Grill 66 2025

**Files:**
- Create `/Users/brynjolfurjonsson/sports/scripts/0Nv_verify_hsi_7643.R`
- Modify `/Users/brynjolfurjonsson/sports/R/ingest-hsi-handball.R` (add `"2025" = 7643L` to `HSI_TOURNAMENT_IDS$female$div2`)
- Modify `/Users/brynjolfurjonsson/sports/tests/testthat/test-ingest-hsi.R` (bump 26 -> 27, flip the `expect_null` at female/div2/2025)
- Modify `/Users/brynjolfurjonsson/sports/config/federation-seasons.json` (via `refresh_federation_seasons()`)
- Create `/Users/brynjolfurjonsson/sports/data/facts/results/sport=handball/country=iceland/sex=female/season=2025/`

**Interfaces:**
- Consumes: `hsi_url()`, `hsi_page_title()`, `.assert_season_stamp()`, `refresh_federation_seasons()`, `ingest_league()`, `read_table()`.
- Produces: no new function; the registry gains one entry and `data/facts/results` gains one partition.

The pre-change docstring called this id "not recoverable from the legacy source". `https://www.hsi.is/tournament/7643` is titled "Grill 66 deild kvenna" and sits between the verified female div1 2025 (7642) and male div2 2025 (7644) — but adjacency is a hypothesis. Three independent checks confirm or reject it: title pattern, season stamp, roster intersection.

- [ ] **Step 1: Write the verification script.**

```r
# scripts/0Nv_verify_hsi_7643.R
#!/usr/bin/env Rscript
# Verify the recovered female Grill 66 2025 tournament id (7643) before it is
# registered. HSI_HISTORICAL_IDS recorded 7644 for this cell, which is a
# copy-paste of the male div2 id, and concluded the real id was unrecoverable.
# 7643 sits between the verified female div1 2025 (7642) and male div2 2025
# (7644) -- adjacency is a hypothesis, so it gets three independent checks:
#
#   (a) page title matches the female Grill 66 pattern
#   (b) .assert_season_stamp() passes for season 2025 (dates in the
#       Sept 2024 - May 2025 span)
#   (c) parsed teams intersect the known 2024 and 2026 female G66 squads
#
# Read-only: writes nothing but its own log. Registration is a separate step.
#
#   Rscript scripts/0Nv_verify_hsi_7643.R

options(width = 120)
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))

CANDIDATE <- 7643L
url <- sprintf("https://www.hsi.is/tournament/%d", CANDIDATE)

cli::cli_h1("Verifying HSI tournament {CANDIDATE} as female div2 season 2025")

html <- fetch_hsi_html(url)

# (a) Title.
title <- hsi_page_title(html)
cli::cli_alert_info("Page title: {title}")
mapped <- .hsi_match_title(title)
ok_title <- identical(mapped$sex[[1L]], "female") &&
  identical(mapped$division[[1L]], "div2")
if (ok_title) {
  cli::cli_alert_success("(a) Title maps to female/div2.")
} else {
  cli::cli_alert_danger("(a) Title maps to {mapped$sex} / {mapped$division} -- REJECT.")
}

# (b) Season stamp.
rows <- parse_hsi_results_page(
  html, sport = "handball", country = "iceland",
  sex = "female", division = "G66", season = 2025L
)
cli::cli_alert_info("Parsed {nrow(rows)} rows; date range {min(rows$match_date)} to {max(rows$match_date)}.")
print(table(format(rows$match_date, "%Y")))
ok_stamp <- tryCatch(
  {
    .assert_season_stamp(rows, 2025L, source = paste("candidate", CANDIDATE))
    TRUE
  },
  sports_season_stamp_error = function(e) {
    cli::cli_alert_danger("(b) {conditionMessage(e)}")
    FALSE
  }
)
if (ok_stamp) cli::cli_alert_success("(b) Season stamp passes for 2025.")

# (c) Roster intersection against what is already on disk.
known <- read_table(
  "results",
  filter = list(sport = "handball", country = "iceland", sex = "female")
)
known <- known[known$division == "G66" & known$season %in% c(2024L, 2026L), ]
known_teams <- sort(unique(c(known$home_team, known$away_team)))
candidate_teams <- sort(unique(c(rows$home_team, rows$away_team)))
shared <- intersect(known_teams, candidate_teams)
cli::cli_alert_info("Candidate teams ({length(candidate_teams)}): {paste(candidate_teams, collapse = ', ')}")
cli::cli_alert_info("Shared with known 2024/2026 G66 squads ({length(shared)}): {paste(shared, collapse = ', ')}")
ok_roster <- length(shared) >= 5L
if (ok_roster) {
  cli::cli_alert_success("(c) Roster intersection >= 5 clubs.")
} else {
  cli::cli_alert_danger("(c) Only {length(shared)} shared clubs -- REJECT.")
}

if (ok_title && ok_stamp && ok_roster) {
  cli::cli_alert_success(
    "ALL THREE PASS. Register {CANDIDATE} as female/div2/2025 with source 'inferred-verified'."
  )
} else {
  cli::cli_abort(
    "Verification FAILED -- leave female div2 2025 unregistered and let check_season_resolution report the gap."
  )
}
```

- [ ] **Step 2: Run it live.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript scripts/0Nv_verify_hsi_7643.R 2>&1 | tee /tmp/hsi-7643-verify.log
```

Expected on success: title `Grill 66 deild kvenna`, a 2024/2025 date table, and `ALL THREE PASS`. The roster threshold of 5 is the measured intersection of the 2024 and 2026 female G66 squads (FH, Fjölnir, Grótta, HK, Víkingur), so a genuine 2025 season cannot fall below it while an unrelated tournament will. If the script aborts, the id is not registered, `hsi_unresolved_seasons(2025L)` keeps reporting the gap, and the remaining steps of this task do not run.

- [ ] **Step 3: Register the verified id.** Add to `R/ingest-hsi-handball.R`, in `HSI_TOURNAMENT_IDS$female$div2`:

```r
    div2 = list(
      "2021" = 5263L, "2022" = 5642L, "2023" = 6148L,
      "2024" = 6980L, "2025" = 7643L, "2027" = 9143L
    ),
```

and record its provenance:

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e '
devtools::load_all(".")
refresh_federation_seasons(tibble::tibble(
  federation = "hsi", sex = "female", division = "div2",
  season = 2025L, id = 7643L, title = "Grill 66 deild kvenna",
  source = "inferred-verified", discovered_at = format(Sys.Date()),
  verified = TRUE,
  note = paste(
    "Legacy source recorded 7644 (a male div2 copy-paste) and called the real",
    "id unrecoverable. 7643 verified 2026-09-02 on three independent checks:",
    "title pattern, season stamp for 2025, roster intersection with the known",
    "2024 and 2026 female G66 squads. See scripts/0Nv_verify_hsi_7643.R."
  )
))
'
```

- [ ] **Step 4: Update the registry tests.** In `tests/testthat/test-ingest-hsi.R`:

```r
  # 19 historical + recovered female div2 2025 + 4 x 2027 league
  # + male cup 2026 + 2 playoffs 2026.
  expect_equal(length(flat), 27L)
```

and replace the `expect_null(hsi_url("female", "div2", 2025L))` line in the "returns NULL rather than erroring" block with a new block:

```r
test_that("the recovered female div2 2025 id is registered and distinct", {
  expect_equal(hsi_url("female", "div2", 2025L), "https://www.hsi.is/tournament/7643")
  # 7644 is the male div2 2025 id -- the copy-paste this recovers from.
  expect_equal(hsi_url("male", "div2", 2025L), "https://www.hsi.is/tournament/7644")
  cache <- read_federation_seasons()
  hit <- cache[cache$sex == "female" & cache$division == "div2" &
                 !is.na(cache$season) & cache$season == 2025L, ]
  expect_equal(nrow(hit), 1L)
  expect_identical(hit$source, "inferred-verified")
  expect_true(hit$verified)
})
```

Run:

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-ingest-hsi.R")'
```

Expected: `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 80 ]`.

- [ ] **Step 5: Dry-run the backfill against a scratch storage root.** Never write into `data/` until the shape is confirmed — five workflows commit there daily.

```bash
cd /Users/brynjolfurjonsson/sports && SCRATCH=$(mktemp -d) && echo "scratch=$SCRATCH" && Rscript -e "
devtools::load_all('.')
league <- load_leagues()[['handball_iceland']]
ingest_league(league, 'female', root = '$SCRATCH', seasons = 2025L)
got <- read_table('results', root = '$SCRATCH',
                  filter = list(sport = 'handball', country = 'iceland', sex = 'female'))
print(as.data.frame(dplyr::count(got, season, division)))
g66 <- got[got\$division == 'G66' & got\$season == 2025L, ]
cat('G66 2025 rows:', nrow(g66), '\n')
cat('date range:', format(min(g66\$match_date)), 'to', format(max(g66\$match_date)), '\n')
"
```

Expected: a `G66` row for `season = 2025` with roughly 60-100 rows (male G66 2025 has 60; female G66 in neighbouring seasons runs 62-98), dates inside Sept 2024 – May 2025, and an `OD` row for 2025 (div1 7642 re-fetched, idempotent under `upsert_table()`). Playoffs 2025 warns and is skipped — that is the Task 5 deferral, expected here.

- [ ] **Step 6: Sync, backfill for real, and commit the data immediately.** The autoplace launchd agent runs `git stash → pull --rebase → pop` on this repo on its own schedule and only rescue-commits the *ledger*, so a generated tracked partition left in the working tree can be clobbered (2026-06-11 incident).

```bash
git -C /Users/brynjolfurjonsson/sports fetch origin
git -C /Users/brynjolfurjonsson/sports status --short
cd /Users/brynjolfurjonsson/sports && Rscript -e "
devtools::load_all('.')
league <- load_leagues()[['handball_iceland']]
ingest_league(league, 'female', seasons = 2025L)
" 2>&1 | tee /tmp/hsi-g66-backfill.log
cd /Users/brynjolfurjonsson/sports && Rscript -e "
devtools::load_all('.')
got <- read_table('results', filter = list(sport = 'handball', country = 'iceland', sex = 'female'))
print(as.data.frame(dplyr::count(got[got\$division == 'G66', ], season)))
"
```

Expected: the printed count now shows female G66 seasons 2021, 2022, 2023, 2024, **2025**, 2026 — 2025 non-empty for the first time.

```bash
git -C /Users/brynjolfurjonsson/sports add R/ingest-hsi-handball.R tests/testthat/test-ingest-hsi.R config/federation-seasons.json scripts/0Nv_verify_hsi_7643.R 'data/facts/results/sport=handball/country=iceland/sex=female'
git -C /Users/brynjolfurjonsson/sports commit -m "data(handball): backfill female Grill 66 2025, recovering tournament id 7643

HSI_HISTORICAL_IDS recorded 7644 for this cell -- a copy-paste of the male
div2 id -- and its docstring concluded the real id was unrecoverable, leaving
female G66 2025 absent from data/facts/results and that cell's history thin.
7643 sits between the verified female div1 2025 (7642) and male div2 2025
(7644), but adjacency is a hypothesis, so it was confirmed on three
independent properties of the fetched page before registration: title
pattern, season stamp for 2025, and a 5-club roster intersection with the
known 2024 and 2026 squads. scripts/0Nv_verify_hsi_7643.R re-runs the proof.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 8: whole-workstream verification

**Files:** none modified — this task only runs things and records what it saw.

**Interfaces:** consumes everything above.

Per-task green does not prove the workstream is green: the season-stamp guard, the registry and the deferral are owned by different tasks, and a break that only shows up tracing one (sex, division, season) end-to-end is invisible to per-task checks.

- [ ] **Step 1: Full suite.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::load_all("."); devtools::test()' 2>&1 | tail -30
```

Expected: 0 failures. Record the total PASS count and the timestamp — that is the WS4 baseline WS5 and WS6 build on.

- [ ] **Step 2: Prove there are no stale references to the deleted registries.**

```bash
grep -rn 'HSI_URLS\|HSI_HISTORICAL_IDS\|hsi_historical_url' \
  /Users/brynjolfurjonsson/sports/R \
  /Users/brynjolfurjonsson/sports/scripts \
  /Users/brynjolfurjonsson/sports/tests \
  /Users/brynjolfurjonsson/sports/.claude \
  ; echo "exit=$?"
```

Expected: no output, `exit=1`. (`docs/superpowers/` still names them and should — it is the design record.)

- [ ] **Step 3: RED proof of the guard on the live path, against a scratch root.** Point the 2027 male div1 key at the 2025 id and confirm the run aborts rather than writing rows. This must be run, not reasoned about.

```bash
cd /Users/brynjolfurjonsson/sports && SCRATCH=$(mktemp -d) && Rscript -e "
devtools::load_all('.')
# Deliberately stale registry entry: 2027 -> the 2025 tournament.
sabotaged <- HSI_TOURNAMENT_IDS
sabotaged\$male\$div1[['2027']] <- 7641L
assignInNamespace('HSI_TOURNAMENT_IDS', sabotaged, ns = 'sports')
league <- load_leagues()[['handball_iceland']]
res <- tryCatch(
  ingest_league(league, 'male', root = '$SCRATCH', seasons = 2027L),
  sports_season_stamp_error = function(e) {
    cat('ABORTED AS DESIGNED:', conditionMessage(e), '\n'); 'aborted'
  }
)
stopifnot(identical(res, 'aborted'))
stopifnot(!dir.exists(file.path('$SCRATCH', 'facts', 'results')))
cat('No rows written. Guard proven.\n')
"
```

Expected: `ABORTED AS DESIGNED: Season stamp mismatch for hsi male/div1 results ...` followed by `No rows written. Guard proven.`

- [ ] **Step 4: Live current-season smoke test, scratch root.** ~5-10 min under chromote.

```bash
cd /Users/brynjolfurjonsson/sports && SCRATCH=$(mktemp -d) && echo "scratch=$SCRATCH" && nohup Rscript -e "
devtools::load_all('.')
league <- load_leagues()[['handball_iceland']]
for (sex in league\$sexes) ingest_league(league, sex, root = '$SCRATCH', seasons = NULL)
got <- read_table('results', root = '$SCRATCH')
print(as.data.frame(dplyr::count(got, sex, season, division)))
cat('date range:', format(min(got\$match_date)), 'to', format(max(got\$match_date)), '\n')
" > /tmp/hsi-2027-smoke.log 2>&1 & disown
```

Then read `/tmp/hsi-2027-smoke.log`. Expected: only `season = 2027` partitions; divisions `OD` and `G66` for both sexes; dates inside Sept 2026 – May 2027; one warning per skipped cell naming `male/cup`, `male/playoffs`, `female/playoffs` for season 2027.

- [ ] **Step 5: Confirm the tree is clean and nothing is unpushed by accident.**

```bash
git -C /Users/brynjolfurjonsson/sports fetch origin
git -C /Users/brynjolfurjonsson/sports status --short
git -C /Users/brynjolfurjonsson/sports log --oneline origin/main..feat/bb-hb-metill-parity
git -C /Users/brynjolfurjonsson/sports stash list
```

Expected: clean working tree, 7 WS4 commits ahead of `origin/main`, no stashes. **Do not push** — WS4, WS5 and WS6 go up as one PR.

---


# WS5 — KKÍ derived season resolution keyed on league_id (spec §6)

**Produces (later workstreams rely on these):**

- `KKI_LEAGUE_IDS  # list(male = list(div1 = 190L, div2 = 191L), female = list(div1 = 189L, div2 = 231L)) — stable competition ids`
- `kki_league_id(sex, div) -> integer(1)  # aborts on an unresolved NA_integer_ cell`
- `kki_season_id_cached(sex, div, season) -> integer(1) or NULL  # verified 2021-2026 lookup, NULL on miss`
- `kki_current_season(today = Sys.Date()) -> integer(1)  # closing-year convention, month >= 7 -> year + 1`
- `kki_league_url(league_id, season_id = NULL, stage_id = NULL) -> character(1)`
- `parse_kki_season_options(html) -> tibble(label, season_id, season)  # pure, reads the FIRST <select> by position`
- `parse_kki_stage_options(html) -> tibble(label, stage_id)  # pure, reads the SECOND <select> by position`
- `kki_discover_season_ids(sex, div, url_fn = kki_league_url, fetch_fn = fetch_rendered_html) -> tibble(sex, div, league_id, season, season_id, label, discovered_at, source)`
- `kki_discover_stage_ids(sex, div, season_id, fetch_fn = fetch_rendered_html) -> tibble(sex, div, season_id, stage_id, label)`
- `kki_resolve_season_id(sex, div, season, allow_discovery = TRUE) -> integer(1)  # cache -> discovery -> abort`
- `kki_write_federation_cache(discovered, path = here::here('config', 'federation-seasons.json')) -> invisible(path)  # owns the top-level 'kki' key only`
- `fetch_kki(league, sex, seasons = NULL, type = c('results_only','schedule_only'), stage = c('regular','all'))  # seasons = NULL now means CURRENT SEASON ONLY`
- `fetch_rendered_html(url, ready_fn = function(html) TRUE, max_attempts = 12L, wait_seconds = 5, sleep_fn = Sys.sleep, live_fn = rvest::read_html_live) -> xml_document`
- `*.open_live_page(url, live_fn = rvest::read_html_live) -> live page object  # actual name .open_live_page`
- `*.rendered_dom(page) -> xml_document  # actual name .rendered_dom; UTF-8-safe outerHTML extraction`
- `schemas()$results gains nullable column stage = arrow::string()`

> **ID-3 OVERRIDE: do NOT execute Task 9 (stage capture).** It is deferred to Plan B. Stop after Task 8.

## WS5 — KKÍ derived season resolution keyed on `league_id`

**Branch:** `feat/bb-hb-metill-parity` (already checked out). Commit after every task; **do not push** — spec §5 requires WS4 + WS5 + WS6 to land as one PR.

### Decided up front: the stage dimension IS reachable (probe run 2026-09-02)

The Baskethotel XLSX export exposes no stage column (`parse_baskethotel_xlsx` at `R/ingest-kki-basketball.R:123` reads only `id, vikudagur, dags., tími, heimalið, gestalið, leikvöllur, stig, áhorfendur`), **but it accepts a `stage` query parameter**. Live probe against `season_id=130403` (male div1 2026):

| URL suffix | raw rows | matches after cleaning | date range |
|---|---|---|---|
| *(none)* | 231 | **162** | 2025-10-02 … 2026-05-18 |
| `&stage=300475` (Deildarkeppni) | 179 | **132** | 2025-10-02 … 2026-03-26 |
| `&stage=306658` (Úrslitakeppni) | 51 | **30** | 2026-04-01 … 2026-05-18 |
| `&stage=999999` | — | — | non-XLSX; the existing `PK` magic-byte check aborts |
| `&stage_id=…`, `&competition_stage_id=…` | 231 | 162 | ignored — the parameter name is exactly `stage` |

132 + 30 = 162 exactly, and 132 is the spec's derived regular-season count for basketball male BD. So the stage split is **authoritative at source**, not derived. Task 9 captures it. Round-derivation stays only as the documented fallback for a season whose stage ids fail discovery.

---

### Task 1: `KKI_LEAGUE_IDS` and `kki_league_id()` that aborts on an unresolved cell

**Files:**
- Modify `/Users/brynjolfurjonsson/sports/R/ingest-kki-basketball.R` (insert after `KKI_DIVISION_LABELS`, line 57)
- Test `/Users/brynjolfurjonsson/sports/tests/testthat/test-ingest-kki.R` (append after line 56)

**Interfaces:**
- Consumes: `KKI_DIVISION_LABELS` (`:57`)
- Produces: `KKI_LEAGUE_IDS`, `kki_league_id(sex, div) -> integer(1)`

Seeded per spec §6 with only `male$div1 = 190L` known; the other three are `NA_integer_` — a typed, assertable absence. Task 5 resolves them.

- [ ] **Step 1: Write the failing test.**

```r
# append to tests/testthat/test-ingest-kki.R
test_that("KKI_LEAGUE_IDS covers the full (sex, div) grid as typed integers", {
  expect_setequal(names(KKI_LEAGUE_IDS), c("male", "female"))
  for (sex in names(KKI_LEAGUE_IDS)) {
    expect_setequal(names(KKI_LEAGUE_IDS[[sex]]), names(KKI_DIVISION_LABELS))
    for (div in names(KKI_LEAGUE_IDS[[sex]])) {
      expect_type(KKI_LEAGUE_IDS[[sex]][[div]], "integer")
      expect_length(KKI_LEAGUE_IDS[[sex]][[div]], 1L)
    }
  }
})

test_that("kki_league_id returns the known id and aborts on an unresolved cell", {
  expect_identical(kki_league_id("male", "div1"), 190L)
  expect_error(kki_league_id("male", "nosuchdiv"), "unknown KK. division")
  expect_error(kki_league_id("nosuchsex", "div1"), "unknown KK. sex")
})

test_that("kki_league_id aborts, rather than returning NA, on an unresolved id", {
  local_mocked_bindings(
    KKI_LEAGUE_IDS = list(male = list(div1 = NA_integer_))
  )
  expect_error(kki_league_id("male", "div1"), "not been resolved")
})
```

- [ ] **Step 2: Run it, expect the exact failure.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-ingest-kki.R")'
```

Expected: three failures, the first reading `Error in eval(...): object 'KKI_LEAGUE_IDS' not found`, the second and third `could not find function "kki_league_id"`.

- [ ] **Step 3: Implement.** Insert into `R/ingest-kki-basketball.R` immediately after `KKI_DIVISION_LABELS` (line 57).

```r
#' Stable KKÍ competition ids, keyed by (sex, division).
#'
#' `league_id` identifies the competition on kki.is and does **not** rotate
#' between seasons; `season_id` does. Registering the stable key is what stops
#' this module from going stale every August. Discoverable from kki.is's own
#' URLs, e.g.
#' `https://kki.is/motamal/leikir-og-urslit/motayfirlit/Leikir?league_id=190&season_id=130403`
#' — and 130403 is exactly this file's registered male div1 2026 season id.
#'
#' `NA_integer_` means "not yet resolved by discovery" — a typed absence that
#' [kki_league_id()] refuses to fetch on, never a placeholder that quietly
#' fetches nothing.
#' @keywords internal
#' @noRd
KKI_LEAGUE_IDS <- list(
  male = list(
    div1 = 190L,
    div2 = NA_integer_
  ),
  female = list(
    div1 = NA_integer_,
    div2 = NA_integer_
  )
)

#' Look up the stable KKÍ competition id for a (sex, division).
#'
#' @param sex "male" or "female".
#' @param div "div1" (Bónusdeild) or "div2" (1. deild).
#' @return Length-1 integer. Aborts when the cell is unknown or unresolved.
#' @keywords internal
#' @noRd
kki_league_id <- function(sex, div) {
  if (!sex %in% names(KKI_LEAGUE_IDS)) {
    cli::cli_abort("unknown KKÍ sex {.val {sex}}; expected one of {.val {names(KKI_LEAGUE_IDS)}}.")
  }
  sex_map <- KKI_LEAGUE_IDS[[sex]]
  if (!div %in% names(sex_map)) {
    cli::cli_abort("unknown KKÍ division {.val {div}}; expected one of {.val {names(sex_map)}}.")
  }
  id <- sex_map[[div]]
  if (is.na(id)) {
    cli::cli_abort(c(
      "KKÍ league_id for {.val {sex}}/{.val {div}} has not been resolved.",
      "i" = "Run {.run kki_discover_season_ids()} against the kki.is competition nav and register the id in {.var KKI_LEAGUE_IDS}.",
      "x" = "Refusing to fetch: an unresolved id would silently return no rows."
    ))
  }
  as.integer(id)
}
```

- [ ] **Step 4: Run, expect pass.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-ingest-kki.R")'
```

Expected: `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 12 ]` (the 3 pre-existing tests plus the new ones).

- [ ] **Step 5: Commit.**

```bash
git -C /Users/brynjolfurjonsson/sports add R/ingest-kki-basketball.R tests/testthat/test-ingest-kki.R
git -C /Users/brynjolfurjonsson/sports commit -m "feat(kki): key the registry on the stable league_id, not the rotating season_id

season_id rotates every August, so a registry keyed on it goes stale on a
fixed schedule and fails closed (fetch_kki iterates registry keys only, so
2027 is simply invisible). league_id identifies the competition and does not
rotate. Unresolved cells are NA_integer_ and kki_league_id() aborts on them,
so an unknown division can never degrade into a silent zero-row fetch.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 2: demote `KKI_SEASON_IDS` to a verified cache behind `kki_season_id_cached()`

**Files:**
- Modify `/Users/brynjolfurjonsson/sports/R/ingest-kki-basketball.R` (roxygen block `:9-30`, add accessor after `:52`)
- Test `/Users/brynjolfurjonsson/sports/tests/testthat/test-ingest-kki.R` (append)

**Interfaces:**
- Consumes: `KKI_SEASON_IDS` (`:31-52`), unchanged in value
- Produces: `kki_season_id_cached(sex, div, season) -> integer(1) | NULL`

The 24 ids are real hand-verified data (verified by XLSX download 2026-04-24). They are kept **verbatim**; only their role changes — from sole source to first-resolution cache.

- [ ] **Step 1: Write the failing test.**

```r
test_that("kki_season_id_cached serves verified hits and returns NULL on a miss", {
  expect_identical(kki_season_id_cached("male", "div1", 2026L), 130403L)
  expect_identical(kki_season_id_cached("female", "div2", 2021L), 118317L)
  expect_null(kki_season_id_cached("male", "div1", 2027L))
  expect_null(kki_season_id_cached("male", "div1", 2014L))
})

test_that("the verified cache still holds exactly the 2021-2026 hand-checked grid", {
  for (sex in names(KKI_SEASON_IDS)) {
    for (div in names(KKI_SEASON_IDS[[sex]])) {
      expect_identical(
        names(KKI_SEASON_IDS[[sex]][[div]]),
        as.character(2021:2026)
      )
    }
  }
  expect_length(unique(unlist(KKI_SEASON_IDS, use.names = FALSE)), 24L)
})
```

- [ ] **Step 2: Run it, expect the exact failure.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-ingest-kki.R")'
```

Expected: `Error ... could not find function "kki_season_id_cached"` (4 expectations in the first block); the second block passes already.

- [ ] **Step 3: Implement.** Replace the first two paragraphs of the roxygen block at `R/ingest-kki-basketball.R:9-13` with the demotion note, and add the accessor after the closing `)` on line 52.

```r
#' Verified season-id cache for KKÍ basketball (2021-2026).
#'
#' **Role:** first stop in [kki_resolve_season_id()]'s resolution order
#' (cache -> discovery -> abort). This is no longer the only source of season
#' ids — it is a hand-verified cache of the seasons that were checked by XLSX
#' download on 2026-04-24, retained because that verification is real evidence
#' and re-deriving it costs 24 network round-trips. Seasons past 2026 come from
#' [kki_discover_season_ids()].
#'
#' Layout: `KKI_SEASON_IDS[[sex]][[div]][[as.character(season)]]`.
#' Divisions: div1 = Bónusdeild (BD), div2 = 1. Deild (1D).
```

```r
#' Look up a verified season id from the 2021-2026 cache.
#'
#' @param sex,div Cell keys, as in [KKI_SEASON_IDS].
#' @param season Integer closing year (2026 = the 2025-26 season).
#' @return Length-1 integer on a hit, `NULL` on a miss. `NULL` is the signal
#'   for [kki_resolve_season_id()] to fall through to discovery.
#' @keywords internal
#' @noRd
kki_season_id_cached <- function(sex, div, season) {
  sex_map <- KKI_SEASON_IDS[[sex]]
  if (is.null(sex_map)) return(NULL)
  season_map <- sex_map[[div]]
  if (is.null(season_map)) return(NULL)
  hit <- season_map[[as.character(season)]]
  if (is.null(hit)) NULL else as.integer(hit)
}
```

- [ ] **Step 4: Run, expect pass.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-ingest-kki.R")'
```

Expected: `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 18 ]`.

- [ ] **Step 5: Commit.**

```bash
git -C /Users/brynjolfurjonsson/sports add R/ingest-kki-basketball.R tests/testthat/test-ingest-kki.R
git -C /Users/brynjolfurjonsson/sports commit -m "refactor(kki): demote KKI_SEASON_IDS from sole source to verified cache

The 24 ids were confirmed by XLSX download on 2026-04-24 and are real
evidence, so they stay verbatim. What changes is their role: they become the
first step of a resolution order rather than the whole of it, so a season
absent from the cache falls through to discovery instead of being invisible.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 3: one shared chromote path for HSÍ and KKÍ

**Files:**
- Create `/Users/brynjolfurjonsson/sports/R/ingest-live-html.R`
- Modify `/Users/brynjolfurjonsson/sports/R/ingest-hsi-handball.R:222-251` (`fetch_hsi_html` body only)
- Create `/Users/brynjolfurjonsson/sports/tests/testthat/test-ingest-live-html.R`
- Modify `/Users/brynjolfurjonsson/sports/DESCRIPTION` (Collate, regenerated)

**Interfaces:**
- Produces: `.open_live_page(url, live_fn)`, `.rendered_dom(page)`, `fetch_rendered_html(url, ready_fn, max_attempts, wait_seconds, sleep_fn, live_fn)`
- Consumed by WS4 transitively — `fetch_hsi_html()`'s **signature and polling semantics are unchanged**, so no WS4 call site moves. `poll_hsi_tables()` is left exactly as it is.

- [ ] **Step 1: Write the failing test** (`tests/testthat/test-ingest-live-html.R`). The `live_fn` stub means no browser and no network.

```r
fake_page <- function(html_strings) {
  i <- 0L
  list(
    session = list(
      Runtime = list(
        evaluate = function(expr) {
          i <<- min(i + 1L, length(html_strings))
          list(result = list(value = html_strings[[i]]))
        }
      ),
      close = function() invisible(NULL)
    )
  )
}

test_that("fetch_rendered_html polls until ready_fn is satisfied", {
  page <- fake_page(c(
    "<html><body><select></select></body></html>",
    "<html><body><select></select><select></select></body></html>"
  ))
  slept <- 0
  html <- fetch_rendered_html(
    "https://example.invalid/x",
    ready_fn = function(h) length(rvest::html_elements(h, "select")) >= 2L,
    max_attempts = 5L,
    wait_seconds = 0.01,
    sleep_fn = function(s) slept <<- slept + 1,
    live_fn = function(url) page
  )
  expect_s3_class(html, "xml_document")
  expect_length(rvest::html_elements(html, "select"), 2L)
  expect_equal(slept, 2)
})

test_that("fetch_rendered_html aborts, naming the URL, when never ready", {
  page <- fake_page("<html><body></body></html>")
  expect_error(
    fetch_rendered_html(
      "https://example.invalid/never",
      ready_fn = function(h) FALSE,
      max_attempts = 2L,
      wait_seconds = 0,
      sleep_fn = function(s) invisible(NULL),
      live_fn = function(url) page
    ),
    "example.invalid/never"
  )
})

test_that(".rendered_dom preserves Icelandic characters", {
  page <- fake_page("<html><body><option>Úrslitakeppni</option></body></html>")
  html <- .rendered_dom(page)
  expect_identical(rvest::html_text(rvest::html_element(html, "option")), "Úrslitakeppni")
})
```

- [ ] **Step 2: Run it, expect the exact failure.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-ingest-live-html.R")'
```

Expected: 3 errors, `could not find function "fetch_rendered_html"` (×2) and `could not find function ".rendered_dom"`.

- [ ] **Step 3: Implement** `R/ingest-live-html.R`.

```r
#' Shared headless-browser entry point for JS-rendered federation pages.
#'
#' Both hsi.is (client-side Drupal) and kki.is (JS-hydrated season selector)
#' return an empty shell to a plain `httr`/`rvest::read_html()` fetch. One
#' browser path serves both so a chromote fix lands once.
#' @keywords internal
#' @noRd
NULL

#' Open a live (chromote-backed) page.
#'
#' @param url Page URL.
#' @param live_fn Injection seam for tests; defaults to the real browser.
#' @return A live page object exposing `$session`.
#' @keywords internal
#' @noRd
.open_live_page <- function(url, live_fn = rvest::read_html_live) {
  live_fn(url)
}

#' Snapshot a live page's rendered DOM as a parsed document.
#'
#' Extracts `outerHTML` and re-parses it as raw UTF-8 bytes, so accented
#' Icelandic text in headers and `<option>` labels survives the round-trip
#' (rvest's string conversion drops non-ASCII without explicit byte handling).
#'
#' @param page A live page object from [.open_live_page()].
#' @return `xml_document`.
#' @keywords internal
#' @noRd
.rendered_dom <- function(page) {
  outer <- page$session$Runtime$evaluate("document.documentElement.outerHTML")$result$value
  Encoding(outer) <- "UTF-8"
  rvest::read_html(charToRaw(outer), encoding = "UTF-8")
}

#' Fetch a JS-rendered page, polling until it is ready.
#'
#' Hydration is asynchronous, so the first DOM snapshot is usually incomplete.
#' Polls up to `max_attempts` times with `wait_seconds` between snapshots and
#' returns the first document satisfying `ready_fn`. Aborts (naming the URL)
#' rather than returning a half-hydrated page — a silently-empty parse is the
#' failure mode this helper exists to prevent.
#'
#' @param url Page URL.
#' @param ready_fn Predicate on the parsed document; `TRUE` stops polling.
#' @param max_attempts,wait_seconds Poll budget (default 12 x 5s = 60s cap).
#' @param sleep_fn,live_fn Injection seams for tests.
#' @return `xml_document`.
#' @keywords internal
#' @noRd
fetch_rendered_html <- function(url,
                                ready_fn = function(html) TRUE,
                                max_attempts = 12L,
                                wait_seconds = 5,
                                sleep_fn = Sys.sleep,
                                live_fn = rvest::read_html_live) {
  page <- .open_live_page(url, live_fn = live_fn)
  on.exit(try(page$session$close(), silent = TRUE), add = TRUE)

  for (attempt in seq_len(max_attempts)) {
    sleep_fn(wait_seconds)
    html <- .rendered_dom(page)
    if (isTRUE(ready_fn(html))) {
      return(html)
    }
  }

  cli::cli_abort(c(
    "Page never finished hydrating after {max_attempts} poll{?s}.",
    "i" = "URL: {.url {url}}",
    "x" = "Refusing to parse a half-rendered DOM."
  ))
}
```

- [ ] **Step 4: Point `fetch_hsi_html()` at the shared helpers.** In `R/ingest-hsi-handball.R`, replace line 224 and lines 246-250.

```r
  # was: page <- rvest::read_html_live(url)
  page <- .open_live_page(url)
```

```r
  # was: the three-line outerHTML block at :246-250
  # Extract the rendered DOM as UTF-8 bytes, so Icelandic characters in the
  # column headers survive the read_html round-trip. Shared with the KKÍ
  # discovery path — see R/ingest-live-html.R.
  .rendered_dom(page)
```

- [ ] **Step 5: Regenerate Collate, run the full suite, expect pass.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::document(quiet = TRUE); devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-ingest-live-html.R"); testthat::test_file("tests/testthat/test-ingest-hsi.R")'
```

Expected: live-html `[ FAIL 0 | PASS 5 ]`; HSÍ `[ FAIL 0 | PASS <unchanged count> ]` — `fetch_hsi_html` is not directly covered, so the HSÍ file must show exactly the same totals as before the edit.

- [ ] **Step 6: Commit.**

```bash
git -C /Users/brynjolfurjonsson/sports add R/ingest-live-html.R R/ingest-hsi-handball.R DESCRIPTION tests/testthat/test-ingest-live-html.R
git -C /Users/brynjolfurjonsson/sports commit -m "refactor(ingest): one chromote path for every JS-rendered federation page

KKI discovery needs a headless browser for the same reason HSI does. Adding a
second read_html_live() call site would mean two places to fix the next time
chromote boot or UTF-8 handling breaks, so fetch_hsi_html now delegates page
open and DOM extraction to shared helpers. Its signature and polling
behaviour are unchanged, so no caller moves.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 4: parse the kki.is selectors and drive discovery

**Files:**
- Create `/Users/brynjolfurjonsson/sports/tests/testthat/fixtures/kki_basketball/motayfirlit_190_2027.html`
- Modify `/Users/brynjolfurjonsson/sports/R/ingest-kki-basketball.R` (append after `kki_season_id_cached`)
- Test `/Users/brynjolfurjonsson/sports/tests/testthat/test-ingest-kki.R`

**Interfaces:**
- Consumes: `fetch_rendered_html()` (Task 3), `kki_league_id()` (Task 1)
- Produces: `kki_league_url()`, `parse_kki_season_options()`, `parse_kki_stage_options()`, `kki_discover_season_ids()`

**Critical parser constraint (verified live):** the selects' `name` attributes are present on first paint (`6-500-season`) and **empty after hydration**. Selection is by **position**: first select = season, second = stage, third = leikdagur.

- [ ] **Step 1: Capture the fixture.** Written with a quoted heredoc — `Write`/`Edit` mangle multi-line Icelandic blocks.

```bash
cat > /Users/brynjolfurjonsson/sports/tests/testthat/fixtures/kki_basketball/motayfirlit_190_2027.html <<'EOF'
<html><head><title>Mótayfirlit</title></head><body>
<form>
  <select name=""><option value="132568" selected>2026-2027</option><option value="130403">2025-2026</option><option value="128582">2024-2025</option><option value="">Veldu tímabil</option></select>
  <select name=""><option value="">Öll stig</option><option value="300475">Deildarkeppni</option><option value="306658">Úrslitakeppni</option></select>
  <select name=""><option value="">Allir leikdagar</option><option value="1">1</option><option value="22">22</option><option value="4l">4 liða</option></select>
</form>
</body></html>
EOF
```

- [ ] **Step 2: Write the failing test.**

```r
test_that("parse_kki_season_options reads the FIRST select by position", {
  html <- rvest::read_html(fixture("motayfirlit_190_2027.html"), encoding = "UTF-8")
  seasons <- parse_kki_season_options(html)

  expect_named(seasons, c("label", "season_id", "season"))
  expect_identical(nrow(seasons), 3L)               # the empty-value prompt is dropped
  expect_identical(seasons$season_id[[1]], 132568L)
  expect_identical(seasons$label[[1]], "2026-2027")
  expect_identical(seasons$season, c(2027L, 2026L, 2025L))  # closing-year convention
  expect_identical(seasons$season_id[seasons$season == 2026L], 130403L)
})

test_that("parse_kki_stage_options reads the SECOND select by position", {
  html <- rvest::read_html(fixture("motayfirlit_190_2027.html"), encoding = "UTF-8")
  stages <- parse_kki_stage_options(html)

  expect_named(stages, c("label", "stage_id"))
  expect_identical(stages$stage_id, c("300475", "306658"))
  expect_identical(stages$label, c("Deildarkeppni", "Úrslitakeppni"))
})

test_that("parse_kki_season_options aborts on an unhydrated shell", {
  shell <- rvest::read_html("<html><body><div id='app'></div></body></html>")
  expect_error(parse_kki_season_options(shell), "no <select>")
})

test_that("kki_league_url builds the verified working URL shape", {
  expect_identical(
    kki_league_url(190L, season_id = 130403L),
    "https://kki.is/motamal/leikir-og-urslit/motayfirlit/Leikir?league_id=190&season_id=130403"
  )
  expect_identical(
    kki_league_url(190L),
    "https://kki.is/motamal/leikir-og-urslit/motayfirlit/Leikir?league_id=190"
  )
})

test_that("kki_discover_season_ids stamps provenance without touching the network", {
  html <- rvest::read_html(fixture("motayfirlit_190_2027.html"), encoding = "UTF-8")
  out <- kki_discover_season_ids(
    "male", "div1",
    fetch_fn = function(url, ...) html
  )

  expect_named(out, c(
    "sex", "div", "league_id", "season", "season_id",
    "label", "discovered_at", "source"
  ))
  expect_true(all(out$league_id == 190L))
  expect_true(all(out$source == "live"))
  expect_s3_class(out$discovered_at, "POSIXct")
  expect_identical(out$season_id[out$season == 2026L], 130403L)
})
```

- [ ] **Step 3: Run it, expect the exact failure.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-ingest-kki.R")'
```

Expected: `could not find function "parse_kki_season_options"` / `"parse_kki_stage_options"` / `"kki_league_url"` / `"kki_discover_season_ids"`.

- [ ] **Step 4: Implement.** Append to `R/ingest-kki-basketball.R`.

```r
#' Build a kki.is competition-overview URL.
#'
#' The page whose selectors carry the season and stage ids. Verified working
#' shape (2026-09-02):
#' `.../motayfirlit/Leikir?league_id=190&season_id=130403`.
#'
#' @param league_id Stable competition id (see [KKI_LEAGUE_IDS]).
#' @param season_id,stage_id Optional query parameters.
#' @return Length-1 character URL.
#' @keywords internal
#' @noRd
kki_league_url <- function(league_id, season_id = NULL, stage_id = NULL) {
  url <- sprintf(
    "https://kki.is/motamal/leikir-og-urslit/motayfirlit/Leikir?league_id=%d",
    as.integer(league_id)
  )
  if (!is.null(season_id)) url <- paste0(url, "&season_id=", as.integer(season_id))
  if (!is.null(stage_id)) url <- paste0(url, "&stage_id=", stage_id)
  url
}

#' Extract one positional `<select>`'s non-empty options.
#'
#' Selection is **positional**, never by `name`: kki.is renders `name` on first
#' paint (`6-500-season`) and blanks it during hydration, so a name-based
#' selector silently matches nothing on exactly the DOM we care about.
#'
#' @param html Rendered document.
#' @param index 1-based select position.
#' @param what Human label used in the abort message.
#' @return Tibble of `label` / `value`, options with an empty value dropped.
#' @keywords internal
#' @noRd
.kki_select_options <- function(html, index, what) {
  selects <- rvest::html_elements(html, "select")
  if (length(selects) < index) {
    cli::cli_abort(c(
      "kki.is page exposes no <select> at position {index} (found {length(selects)}); cannot read {what}.",
      "i" = "The page is probably an unhydrated shell — discovery must go through {.fn fetch_rendered_html}."
    ))
  }
  opts <- rvest::html_elements(selects[[index]], "option")
  out <- tibble::tibble(
    label = stringr::str_trim(rvest::html_text(opts)),
    value = stringr::str_trim(rvest::html_attr(opts, "value"))
  )
  out[!is.na(out$value) & nzchar(out$value), , drop = FALSE]
}

#' Parse the season selector (first select) into (label, season_id, season).
#'
#' Labels are spans like "2026-2027"; the canonical season is the **closing**
#' year, matching [KKI_SEASON_IDS]'s convention (2026 = the 2025-26 season).
#' Options whose label is not a `YYYY-YYYY` span are dropped.
#'
#' @param html Rendered document.
#' @return Tibble, newest first (kki.is's own order).
#' @keywords internal
#' @noRd
parse_kki_season_options <- function(html) {
  raw <- .kki_select_options(html, 1L, "the season selector")
  m <- stringr::str_match(raw$label, "^(\\d{4})\\s*[-–]\\s*(\\d{4})$")
  keep <- !is.na(m[, 1L])
  tibble::tibble(
    label = raw$label[keep],
    season_id = as.integer(raw$value[keep]),
    season = as.integer(m[keep, 3L])
  )
}

#' Parse the stage selector (second select) into (label, stage_id).
#'
#' KKÍ's authoritative regular-season / post-season split. The "Öll stig"
#' option carries an empty value and is dropped by [.kki_select_options()],
#' which is correct — "all stages" is the no-parameter fetch.
#'
#' @param html Rendered document.
#' @return Tibble of stage labels and ids (ids stay character: they are opaque).
#' @keywords internal
#' @noRd
parse_kki_stage_options <- function(html) {
  raw <- .kki_select_options(html, 2L, "the stage selector")
  tibble::tibble(label = raw$label, stage_id = raw$value)
}

#' Discover every season id kki.is currently offers for a (sex, division).
#'
#' Drives the shared chromote helper over the competition-overview page and
#' reads the hydrated season selector. Roughly 50 seasons back to 1988.
#'
#' @param sex,div Cell keys.
#' @param url_fn,fetch_fn Injection seams; defaults hit kki.is via chromote.
#' @return Tibble `(sex, div, league_id, season, season_id, label,
#'   discovered_at, source)`.
#' @keywords internal
#' @noRd
kki_discover_season_ids <- function(sex, div,
                                    url_fn = kki_league_url,
                                    fetch_fn = fetch_rendered_html) {
  league_id <- kki_league_id(sex, div)
  html <- fetch_fn(
    url_fn(league_id),
    ready_fn = function(h) length(rvest::html_elements(h, "select")) >= 2L
  )
  seasons <- parse_kki_season_options(html)
  tibble::tibble(
    sex = sex,
    div = div,
    league_id = league_id,
    season = seasons$season,
    season_id = seasons$season_id,
    label = seasons$label,
    discovered_at = Sys.time(),
    source = "live"
  )
}
```

- [ ] **Step 5: Run, expect pass.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-ingest-kki.R")'
```

Expected: `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 34 ]`.

- [ ] **Step 6: Commit.**

```bash
git -C /Users/brynjolfurjonsson/sports add R/ingest-kki-basketball.R tests/testthat/test-ingest-kki.R tests/testthat/fixtures/kki_basketball/motayfirlit_190_2027.html
git -C /Users/brynjolfurjonsson/sports commit -m "feat(kki): discover season ids from kki.is's own hydrated selectors

The season ids exist in kki.is URLs; nothing but a hand-maintained registry
was reading them. Selectors are matched by POSITION, not by name: kki.is
renders name='6-500-season' on first paint and blanks it during hydration, so
a name-based selector matches nothing on exactly the DOM we need.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 5: resolve the three unknown `league_id`s live, and prove all four

**Files:**
- Modify `/Users/brynjolfurjonsson/sports/R/ingest-kki-basketball.R` (`KKI_LEAGUE_IDS`; append `kki_write_federation_cache`)
- Create/Modify `/Users/brynjolfurjonsson/sports/config/federation-seasons.json`
- Test `/Users/brynjolfurjonsson/sports/tests/testthat/test-ingest-kki.R`

**Interfaces:**
- Consumes: `kki_discover_season_ids()` (Task 4), `kki_season_id_cached()` (Task 2), `write_json_consistent()` (`R/storage.R:429`)
- Produces: `kki_write_federation_cache(discovered, path)`; `KKI_LEAGUE_IDS` fully resolved

**Values to register** (read live from kki.is competition nav, 2026-09-02). Every 2025-26 id matches the repo's existing `'2026'` cache key, which validates both the mapping and the closing-year convention:

| competition | div | league_id | 2026-27 season_id | 2025-26 | cache says |
|---|---|---|---|---|---|
| Bónusdeild karla | male/div1 | 190 | 132568 | 130403 | 130403 ✓ |
| 1. deild karla | male/div2 | 191 | 132571 | 130402 | 130402 ✓ |
| Bónusdeild kvenna | female/div1 | 189 | 132567 | 130422 | 130422 ✓ |
| 1. deild kvenna | female/div2 | 231 | 132570 | 130421 | 130421 ✓ |

- [ ] **Step 1: Write the failing test.** This is correction (5)'s test — it must pass on **real** ids.

```r
test_that("every configured basketball (sex, div) resolves to a non-NA league_id", {
  for (sex in names(KKI_LEAGUE_IDS)) {
    for (div in names(KKI_LEAGUE_IDS[[sex]])) {
      expect_false(
        is.na(KKI_LEAGUE_IDS[[sex]][[div]]),
        label = paste0("KKI_LEAGUE_IDS$", sex, "$", div)
      )
      expect_gt(kki_league_id(sex, div), 0L)
    }
  }
  expect_identical(
    sort(unlist(KKI_LEAGUE_IDS, use.names = FALSE)),
    c(189L, 190L, 191L, 231L)
  )
})

test_that("the federation cache records a discovered id for every cell, agreeing with the verified 2026 cache", {
  cache_path <- testthat::test_path("..", "..", "config", "federation-seasons.json")
  skip_if_not(file.exists(cache_path), "federation-seasons.json not yet written")
  cache <- jsonlite::fromJSON(cache_path, simplifyVector = FALSE)

  expect_true("kki" %in% names(cache))
  for (sex in names(KKI_LEAGUE_IDS)) {
    for (div in names(KKI_LEAGUE_IDS[[sex]])) {
      cell <- cache$kki[[sex]][[div]]
      expect_false(is.null(cell), label = paste("cache cell", sex, div))
      expect_identical(as.integer(cell$league_id), kki_league_id(sex, div))
      expect_identical(
        as.integer(cell$seasons[["2026"]]$season_id),
        kki_season_id_cached(sex, div, 2026L)
      )
      expect_identical(as.integer(cell$seasons[["2027"]]$season_id) > 0L, TRUE)
    }
  }
})
```

- [ ] **Step 2: Run it, expect the exact failure.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-ingest-kki.R")'
```

Expected: 3 failures in the first block — `KKI_LEAGUE_IDS$male$div2 is not FALSE` (and the two female cells), plus the `sort(...)` identity failing with `NA` in the vector. The second block **skips** (`federation-seasons.json not yet written`).

- [ ] **Step 3: Register the resolved ids.** Replace `KKI_LEAGUE_IDS` in `R/ingest-kki-basketball.R`.

```r
KKI_LEAGUE_IDS <- list(
  male = list(
    div1 = 190L, # Bónusdeild karla
    div2 = 191L  # 1. deild karla
  ),
  female = list(
    div1 = 189L, # Bónusdeild kvenna
    div2 = 231L  # 1. deild kvenna
  )
)
```

Extend the roxygen block above it with the provenance note:

```r
#' Resolved 2026-09-02 by reading kki.is's competition nav under chromote.
#' Each id was confirmed by checking that its 2025-26 season id equals this
#' file's hand-verified `'2026'` cache key — 190/130403, 191/130402,
#' 189/130422, 231/130421 — so the mapping and the closing-year convention are
#' cross-validated, not assumed. Sibling ids on the same nav, registered here
#' only as documentation: 2. deild karla 232, 3. deild karla 31405, VÍS bikar
#' karla 205 / kvenna 208, Meistarakeppni karla 206 / kvenna 207.
```

- [ ] **Step 4: Implement the provenance writer.** Append to `R/ingest-kki-basketball.R`.

```r
#' Merge discovered KKÍ ids into the git-tracked provenance cache.
#'
#' Owns **only** the top-level `kki` key of
#' `config/federation-seasons.json`; WS4's HSÍ writer owns `hsi`. Disjoint
#' keys, so the two can run in either order without clobbering.
#'
#' @param discovered Tibble as returned by [kki_discover_season_ids()],
#'   optionally row-bound across cells.
#' @param path Cache path.
#' @return `path`, invisibly.
#' @keywords internal
#' @noRd
kki_write_federation_cache <- function(discovered,
                                       path = here::here("config", "federation-seasons.json")) {
  existing <- if (file.exists(path)) {
    jsonlite::fromJSON(path, simplifyVector = FALSE)
  } else {
    list()
  }

  kki <- list()
  for (sex in unique(discovered$sex)) {
    for (div in unique(discovered$div[discovered$sex == sex])) {
      rows <- discovered[discovered$sex == sex & discovered$div == div, , drop = FALSE]
      seasons <- list()
      for (i in seq_len(nrow(rows))) {
        seasons[[as.character(rows$season[[i]])]] <- list(
          season_id = rows$season_id[[i]],
          label = rows$label[[i]],
          discovered_at = format(rows$discovered_at[[i]], "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
          source = rows$source[[i]]
        )
      }
      kki[[sex]][[div]] <- list(
        league_id = rows$league_id[[1L]],
        seasons = seasons
      )
    }
  }

  existing$kki <- kki
  write_json_consistent(existing, path, auto_unbox = TRUE, pretty = TRUE)
  invisible(path)
}
```

- [ ] **Step 5: Run discovery live and write the cache.** This is the one network step in WS5; it needs a browser.

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e '
devtools::load_all(quiet = TRUE)
grid <- expand.grid(sex = c("male", "female"), div = c("div1", "div2"), stringsAsFactors = FALSE)
found <- dplyr::bind_rows(lapply(seq_len(nrow(grid)), function(i) {
  out <- kki_discover_season_ids(grid$sex[[i]], grid$div[[i]])
  Sys.sleep(3)
  out
}))
# Cross-check every discovered 2026 id against the hand-verified cache.
chk <- found[found$season == 2026L, ]
for (i in seq_len(nrow(chk))) {
  cached <- kki_season_id_cached(chk$sex[[i]], chk$div[[i]], 2026L)
  stopifnot(identical(chk$season_id[[i]], cached))
  cat(sprintf("OK %s/%s league_id=%d  2026=%d (cache agrees)  2027=%d\n",
      chk$sex[[i]], chk$div[[i]], chk$league_id[[i]], chk$season_id[[i]],
      found$season_id[found$sex == chk$sex[[i]] & found$div == chk$div[[i]] & found$season == 2027L]))
}
kki_write_federation_cache(found)
'
```

Expected: four `OK` lines — `male/div1 league_id=190 2026=130403 (cache agrees) 2027=132568`, `male/div2 191 / 130402 / 132571`, `female/div1 189 / 130422 / 132567`, `female/div2 231 / 130421 / 132570`. A `stopifnot` failure here means the mapping is wrong: **stop and re-read the nav**, do not register.

- [ ] **Step 6: Run the tests, expect pass.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-ingest-kki.R")'
```

Expected: `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 51 ]` — the skip is gone because the cache now exists.

- [ ] **Step 7: Commit.**

```bash
git -C /Users/brynjolfurjonsson/sports add R/ingest-kki-basketball.R config/federation-seasons.json tests/testthat/test-ingest-kki.R
git -C /Users/brynjolfurjonsson/sports commit -m "data(kki): resolve all four league_ids, cross-validated against the verified cache

Discovery is only trustworthy if it reproduces hand-verified truth first.
Each of 190/191/189/231 was accepted only because its 2025-26 season id
equals this repo's independently verified '2026' key (130403/130402/130422/
130421), so the mapping and the closing-year convention are both confirmed
rather than assumed. Provenance lands in config/federation-seasons.json, not
in an R comment, so staleness is inspectable.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 6: `kki_current_season()` and the cache → discovery → abort resolution order

**Files:**
- Modify `/Users/brynjolfurjonsson/sports/R/ingest-kki-basketball.R` (append)
- Test `/Users/brynjolfurjonsson/sports/tests/testthat/test-ingest-kki.R`

**Interfaces:**
- Consumes: `kki_season_id_cached()` (Task 2), `kki_discover_season_ids()` (Task 4)
- Produces: `kki_current_season(today = Sys.Date())`, `kki_resolve_season_id(sex, div, season, allow_discovery = TRUE)`

- [ ] **Step 1: Write the failing test.**

```r
test_that("kki_current_season uses the closing-year convention", {
  expect_identical(kki_current_season(as.Date("2026-09-02")), 2027L)
  expect_identical(kki_current_season(as.Date("2026-07-01")), 2027L)
  expect_identical(kki_current_season(as.Date("2026-06-30")), 2026L)
  expect_identical(kki_current_season(as.Date("2026-01-15")), 2026L)
})

test_that("kki_resolve_season_id serves the cache without discovering", {
  local_mocked_bindings(
    kki_discover_season_ids = function(...) stop("discovery must not run on a cache hit")
  )
  expect_identical(kki_resolve_season_id("male", "div1", 2026L), 130403L)
})

test_that("kki_resolve_season_id falls through to discovery on a cache miss", {
  local_mocked_bindings(
    kki_discover_season_ids = function(sex, div, ...) {
      tibble::tibble(
        sex = sex, div = div, league_id = 190L,
        season = c(2027L, 2026L), season_id = c(132568L, 130403L),
        label = c("2026-2027", "2025-2026"),
        discovered_at = Sys.time(), source = "live"
      )
    }
  )
  expect_identical(kki_resolve_season_id("male", "div1", 2027L), 132568L)
})

test_that("kki_resolve_season_id aborts with an actionable message when nothing resolves", {
  local_mocked_bindings(
    kki_discover_season_ids = function(sex, div, ...) {
      tibble::tibble(
        sex = character(), div = character(), league_id = integer(),
        season = integer(), season_id = integer(), label = character(),
        discovered_at = Sys.time()[0], source = character()
      )
    }
  )
  expect_error(kki_resolve_season_id("male", "div1", 2099L), "could not be resolved")
  expect_error(kki_resolve_season_id("male", "div1", 2099L), "kki_discover_season_ids")
})

test_that("kki_resolve_season_id can be told not to discover", {
  local_mocked_bindings(
    kki_discover_season_ids = function(...) stop("discovery was not supposed to run")
  )
  expect_error(
    kki_resolve_season_id("male", "div1", 2027L, allow_discovery = FALSE),
    "could not be resolved"
  )
})
```

- [ ] **Step 2: Run it, expect the exact failure.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-ingest-kki.R")'
```

Expected: `could not find function "kki_current_season"` and `could not find function "kki_resolve_season_id"` across the five new blocks.

- [ ] **Step 3: Implement.** Append to `R/ingest-kki-basketball.R`.

```r
#' The KKÍ season currently in progress.
#'
#' Same closing-year convention as [KKI_SEASON_IDS] and
#' `hsi_current_season()`: the basketball year spans Oct-May, so from July
#' onwards the current season is next calendar year's label.
#'
#' @param today Reference date (injectable for tests).
#' @return Length-1 integer season.
#' @keywords internal
#' @noRd
kki_current_season <- function(today = Sys.Date()) {
  yr <- as.integer(format(today, "%Y"))
  mo <- as.integer(format(today, "%m"))
  if (mo >= 7L) yr + 1L else yr
}

#' Resolve a (sex, div, season) to a Baskethotel season id.
#'
#' Resolution order, fail-loud at the end:
#' 1. **cache** — [kki_season_id_cached()], the 2021-2026 hand-verified grid;
#' 2. **discovery** — [kki_discover_season_ids()] against kki.is's live
#'    season selector;
#' 3. **abort** — with the command that would fix it.
#'
#' Aborting rather than returning `NULL` is deliberate: a `NULL` here would
#' reach `fetch_kki()` as a skipped division, and a silently skipped division
#' looks identical to an out-of-season one. Ingest is the only place that can
#' tell the difference, so it says so.
#'
#' @param sex,div Cell keys.
#' @param season Integer closing year.
#' @param allow_discovery Set `FALSE` to restrict resolution to the cache
#'   (used by offline tests and by any caller that must not open a browser).
#' @return Length-1 integer season id.
#' @keywords internal
#' @noRd
kki_resolve_season_id <- function(sex, div, season, allow_discovery = TRUE) {
  season <- as.integer(season)

  cached <- kki_season_id_cached(sex, div, season)
  if (!is.null(cached)) {
    return(cached)
  }

  if (isTRUE(allow_discovery)) {
    found <- tryCatch(
      kki_discover_season_ids(sex, div),
      error = function(e) {
        cli::cli_warn(c(
          "KKÍ discovery failed for {sex}/{div}: {conditionMessage(e)}",
          "i" = "Falling through to the resolution abort."
        ))
        NULL
      }
    )
    if (!is.null(found) && nrow(found) > 0L) {
      hit <- found$season_id[found$season == season]
      if (length(hit) >= 1L) {
        return(as.integer(hit[[1L]]))
      }
    }
  }

  cli::cli_abort(c(
    "KKÍ season {season} for {.val {sex}}/{.val {div}} could not be resolved.",
    "i" = "Cache miss ({.fn kki_season_id_cached} covers 2021-2026) and discovery returned no match.",
    "i" = "Reproduce with {.code kki_discover_season_ids(\"{sex}\", \"{div}\")} and register the id in the cache."
  ))
}
```

- [ ] **Step 4: Run, expect pass.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-ingest-kki.R")'
```

Expected: `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 61 ]`.

- [ ] **Step 5: Commit.**

```bash
git -C /Users/brynjolfurjonsson/sports add R/ingest-kki-basketball.R tests/testthat/test-ingest-kki.R
git -C /Users/brynjolfurjonsson/sports commit -m "feat(kki): resolve season ids cache -> discovery -> abort

The failure this replaces is silent: an unregistered season made fetch_kki
iterate zero keys and return zero rows, which is indistinguishable from an
off-season. Ending in an abort that names the fixing command means a season
rollover surfaces as a red run, not as a quiet gap in data/facts/results.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 7: the season-stamp guard on the KKÍ parse path

**Files:**
- Create `/Users/brynjolfurjonsson/sports/R/ingest-season-guard.R` — **only if WS4 has not already created `.assert_season_stamp`**
- Modify `/Users/brynjolfurjonsson/sports/R/ingest-kki-basketball.R` (`fetch_kki` body)
- Test `/Users/brynjolfurjonsson/sports/tests/testthat/test-ingest-kki.R`

**Interfaces:**
- Consumes (WS4): `.assert_season_stamp(rows, season, tol = 0.05, context = NULL) -> invisible(rows)`
- Produces: the guard is live on every KKÍ parse

- [ ] **Step 1: Check whether WS4 has landed the guard.** If this prints a path, **skip Step 3** and use WS4's definition as-is.

```bash
grep -rn "\.assert_season_stamp <- function" /Users/brynjolfurjonsson/sports/R/
```

- [ ] **Step 2: Write the failing test.**

```r
test_that(".assert_season_stamp passes rows inside the season span", {
  rows <- tibble::tibble(match_date = as.Date(c("2025-10-02", "2026-03-26", "2026-05-18")))
  expect_invisible(.assert_season_stamp(rows, 2026L))
})

test_that(".assert_season_stamp tolerates a few strays but aborts past tol", {
  ok <- tibble::tibble(match_date = c(as.Date("2025-10-02") + 0:38, as.Date("2019-01-01")))
  expect_invisible(.assert_season_stamp(ok, 2026L))   # 1/40 = 2.5% <= 5%

  bad <- tibble::tibble(match_date = c(as.Date("2025-10-02") + 0:17, as.Date(c("2019-01-01", "2019-02-01"))))
  expect_error(.assert_season_stamp(bad, 2026L), "season stamp")  # 2/20 = 10% > 5%
})

test_that(".assert_season_stamp is a no-op on an empty frame", {
  expect_invisible(.assert_season_stamp(tibble::tibble(match_date = as.Date(character())), 2026L))
})

test_that("fetch_kki aborts when Baskethotel returns another season's fixtures", {
  wrong_season <- tibble::tibble(
    sport = "basketball", country = "iceland", sex = "male", season = 2027L,
    match_date = as.Date("2021-11-01") + 0:19,
    home_team = "A", away_team = "B",
    home_score = 80L, away_score = 70L, division = "BD", round = NA_integer_
  )
  local_mocked_bindings(
    download_baskethotel_xlsx = function(...) "/dev/null",
    parse_baskethotel_xlsx = function(...) wrong_season,
    kki_resolve_season_id = function(...) 999999L
  )
  expect_error(
    fetch_kki(list(), "male", seasons = 2027L, type = "results_only"),
    "season stamp"
  )
})
```

- [ ] **Step 3: Implement the guard** — *only if Step 1 found nothing*. Create `R/ingest-season-guard.R`.

```r
#' Assert that parsed rows actually belong to the season they are stamped with.
#'
#' A federation id that is plausible-but-wrong (a stale slug, a mis-mapped
#' tournament, a URL scheme change) returns a perfectly valid page for the
#' *wrong* season. Nothing else in ingest can tell: the rows parse, validate
#' and write cleanly into a git-tracked hive partition that five workflows
#' commit to daily, and cleanup becomes a partition delete racing automation.
#' This converts that into a loud failure before the first row is written.
#'
#' A season spans two calendar years, so `{season - 1, season}` are both
#' legitimate. `tol` allows the odd rescheduled straggler.
#'
#' @param rows Data frame with a `match_date` Date column.
#' @param season Integer closing-year season the rows claim to be.
#' @param tol Maximum fraction of out-of-span rows before aborting.
#' @param context Optional string naming the fetch, for the error message.
#' @return `rows`, invisibly.
#' @keywords internal
#' @noRd
.assert_season_stamp <- function(rows, season, tol = 0.05, context = NULL) {
  if (nrow(rows) == 0L) {
    return(invisible(rows))
  }
  season <- as.integer(season)
  yrs <- as.integer(format(rows$match_date, "%Y"))
  ok <- !is.na(yrs) & yrs %in% c(season - 1L, season)
  frac_bad <- mean(!ok)

  if (frac_bad > tol) {
    observed <- sort(unique(yrs[!ok]))
    cli::cli_abort(c(
      "Season stamp check failed{if (is.null(context)) '' else paste0(' for ', context)}: {round(100 * frac_bad, 1)}% of {nrow(rows)} row{?s} fall outside {season - 1L}-{season}.",
      "x" = "Out-of-span calendar year{?s} observed: {observed}.",
      "i" = "The federation id almost certainly points at a different season. Refusing to write."
    ))
  }
  invisible(rows)
}
```

- [ ] **Step 4: Call it from `fetch_kki`.** In `R/ingest-kki-basketball.R`, insert immediately after the `parsed <- parse_baskethotel_xlsx(...)` call (currently `:219-226`), before the `frames[[...]] <- parsed` line.

```r
      .assert_season_stamp(
        parsed, season_int,
        context = paste0("KKÍ ", sex, "/", div, " season ", season_key)
      )
```

- [ ] **Step 5: Run, expect pass.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::document(quiet = TRUE); devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-ingest-kki.R")'
```

Expected: `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 68 ]`.

- [ ] **Step 6: Commit.**

```bash
git -C /Users/brynjolfurjonsson/sports add R/ingest-season-guard.R R/ingest-kki-basketball.R DESCRIPTION tests/testthat/test-ingest-kki.R
git -C /Users/brynjolfurjonsson/sports commit -m "feat(ingest): abort when parsed rows do not belong to the requested season

The PK magic-byte check already catches a dead season id. Nothing caught a
live-but-wrong one: the rows parse and validate, then land in a git-tracked
partition that five workflows commit to daily, so cleanup becomes a delete
racing cron. Failing before the first write is the only cheap moment.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 8: `fetch_kki(seasons = NULL)` means the current season only

**Files:**
- Modify `/Users/brynjolfurjonsson/sports/R/ingest-kki-basketball.R` (`fetch_kki`, `:178-235`; docstrings on `fetch_results_kki` `:256-261` and `fetch_schedule_kki` `:263-298`)
- Test `/Users/brynjolfurjonsson/sports/tests/testthat/test-ingest-kki.R`

**Interfaces:**
- Consumes: `kki_league_id()`, `kki_current_season()`, `kki_resolve_season_id()`, `.assert_season_stamp()`
- Produces: `fetch_kki(league, sex, seasons = NULL, type = c("results_only","schedule_only"))` — argument list unchanged, `seasons = NULL` semantics changed

This is load-bearing for WS6. Once the ingest gate is deleted, the old "iterate every registry key" meaning becomes 2 sexes × 2 divs × 6 seasons × 2 types = **48 XLSX downloads a day, forever**. New: **8**.

- [ ] **Step 1: Write the failing test.**

```r
test_that("fetch_kki with seasons = NULL fetches only the current season", {
  calls <- new.env(parent = emptyenv())
  calls$ids <- integer()
  local_mocked_bindings(
    kki_current_season = function(today = Sys.Date()) 2026L,
    download_baskethotel_xlsx = function(season_id, type = "results_only") {
      calls$ids <- c(calls$ids, as.integer(season_id))
      "stub.xlsx"
    },
    parse_baskethotel_xlsx = function(path, sport, country, sex, division, season) {
      tibble::tibble(
        sport = sport, country = country, sex = sex, season = as.integer(season),
        match_date = as.Date("2025-10-02") + 0:9,
        home_team = "A", away_team = "B",
        home_score = 80L, away_score = 70L, division = division, round = NA_integer_
      )
    }
  )

  out <- fetch_kki(list(), "male", seasons = NULL, type = "results_only")

  # One download per division, current season only -- 2 per (sex, type),
  # i.e. 8 per day across 2 sexes x 2 types, down from 48.
  expect_length(calls$ids, 2L)
  expect_setequal(calls$ids, c(130403L, 130402L))
  expect_setequal(unique(out$division), unname(KKI_DIVISION_LABELS))
  expect_true(all(out$season == 2026L))
})

test_that("fetch_kki still backfills when seasons is given explicitly", {
  calls <- new.env(parent = emptyenv())
  calls$ids <- integer()
  local_mocked_bindings(
    download_baskethotel_xlsx = function(season_id, type = "results_only") {
      calls$ids <- c(calls$ids, as.integer(season_id))
      "stub.xlsx"
    },
    parse_baskethotel_xlsx = function(path, sport, country, sex, division, season) {
      tibble::tibble(
        sport = sport, country = country, sex = sex, season = as.integer(season),
        match_date = as.Date(paste0(as.integer(season) - 1L, "-10-02")) + 0:9,
        home_team = "A", away_team = "B",
        home_score = 80L, away_score = 70L, division = division, round = NA_integer_
      )
    }
  )

  fetch_kki(list(), "female", seasons = c(2024L, 2025L), type = "results_only")
  expect_length(calls$ids, 4L)
  expect_setequal(calls$ids, c(127289L, 128585L, 127381L, 128590L))
})

test_that("fetch_kki returns the canonical empty frame when every download fails", {
  local_mocked_bindings(
    kki_current_season = function(today = Sys.Date()) 2026L,
    download_baskethotel_xlsx = function(...) stop("boom")
  )
  out <- suppressWarnings(fetch_kki(list(), "male", seasons = NULL, type = "results_only"))
  expect_identical(nrow(out), 0L)
  expect_named(out, names(parse_baskethotel_xlsx_empty()))
})
```

- [ ] **Step 2: Run it, expect the exact failure.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-ingest-kki.R")'
```

Expected: the first block fails with `expect_length(calls$ids, 2L)` → `calls$ids has length 12, not length 2` (today's `fetch_kki` iterates 2 divisions × 6 registry seasons). The second and third blocks pass already.

- [ ] **Step 3: Implement.** Replace the body of `fetch_kki` (`R/ingest-kki-basketball.R:187-235`).

```r
#' Download + parse the requested seasons for a sex, across both divisions.
#'
#' @param league League config entry (unused beyond presence — KKÍ-specific
#'   metadata is baked into this module).
#' @param sex "male" or "female".
#' @param seasons Integer vector of closing-year seasons to fetch. **`NULL`
#'   (the default) means the current season only** — not "every registry
#'   key". Ingest runs daily and every past season is immutable, so
#'   re-downloading history was pure cost: 2 sexes x 2 divisions x 6 seasons
#'   x 2 types = 48 downloads a day, versus 8 now. Pass `seasons` explicitly
#'   to backfill.
#' @param type "results_only" or "schedule_only".
#' @return Combined tibble across both divisions and every requested season.
#' @keywords internal
#' @noRd
fetch_kki <- function(league, sex, seasons = NULL,
                      type = c("results_only", "schedule_only")) {
  type <- match.arg(type)
  stopifnot(sex %in% names(KKI_LEAGUE_IDS))

  wanted <- if (is.null(seasons)) kki_current_season() else as.integer(seasons)
  frames <- list()

  for (div in names(KKI_LEAGUE_IDS[[sex]])) {
    division_label <- KKI_DIVISION_LABELS[[div]]

    for (season_int in wanted) {
      season_id <- kki_resolve_season_id(sex, div, season_int)

      path <- tryCatch(
        download_baskethotel_xlsx(season_id, type = type),
        error = function(e) {
          cli::cli_warn(c(
            "Baskethotel download failed for {sex}/{div}/{season_int} (season_id {season_id})",
            "i" = "{conditionMessage(e)}"
          ))
          NULL
        }
      )
      if (is.null(path)) next

      parsed <- parse_baskethotel_xlsx(
        path,
        sport = "basketball",
        country = "iceland",
        sex = sex,
        division = division_label,
        season = season_int
      )
      .assert_season_stamp(
        parsed, season_int,
        context = paste0("KKÍ ", sex, "/", div, " season ", season_int)
      )
      frames[[length(frames) + 1L]] <- parsed
    }
  }

  if (length(frames) == 0L) {
    return(parse_baskethotel_xlsx_empty())
  }
  dplyr::bind_rows(frames)
}
```

Then update the two entry-point docstrings so `seasons = NULL` is not documented as a lie:

```r
#' Source-module entrypoint: results for a (league, sex).
#'
#' `seasons = NULL` fetches the current season only — see [fetch_kki()].
#' `ingest_league()` passes `seasons` straight through, so a backfill is
#' `ingest_league(league, sex, seasons = 2021:2026)`.
#' @keywords internal
#' @noRd
```

- [ ] **Step 4: Run the whole suite, expect pass.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::load_all(quiet = TRUE); devtools::test()'
```

Expected: `[ FAIL 0 | WARN 0 ]` across the package. `test-ingest-integration.R` and `test-ingest-dispatcher.R` must stay green — they exercise `ingest_league()`, whose `seasons` pass-through is unchanged.

- [ ] **Step 5: Commit.**

```bash
git -C /Users/brynjolfurjonsson/sports add R/ingest-kki-basketball.R tests/testthat/test-ingest-kki.R
git -C /Users/brynjolfurjonsson/sports commit -m "perf(kki): seasons = NULL now means the current season, not all of history

WS6 deletes the ingest activation gate, which is the only thing currently
stopping a daily re-download of six immutable seasons. Without this change
that becomes 48 XLSX downloads a day forever; with it, 8. Explicit seasons=
still backfills, so nothing is lost -- only the default changes, from
'everything, every day' to 'the season we are actually in'.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 9: capture KKÍ's authoritative stage at ingest

**Files:**
- Modify `/Users/brynjolfurjonsson/sports/R/storage-schemas.R:13-24` (results schema)
- Modify `/Users/brynjolfurjonsson/sports/R/ingest.R:64-72` (NA backfill for non-KKÍ sources)
- Modify `/Users/brynjolfurjonsson/sports/R/ingest-kki-basketball.R` (`baskethotel_url`, `download_baskethotel_xlsx`, `kki_discover_stage_ids`, `fetch_kki`)
- Create `/Users/brynjolfurjonsson/sports/tests/testthat/fixtures/kki_basketball/sample_male_div1_2027_deildarkeppni.xlsx`
- Test `/Users/brynjolfurjonsson/sports/tests/testthat/test-ingest-kki.R`

**Interfaces:**
- Consumes: `parse_kki_stage_options()` (Task 4), `kki_resolve_season_id()` (Task 6), `write_table()`/`read_table()` schema evolution (`R/storage.R:384-412`)
- Produces: `schemas()$results$stage`; `baskethotel_url(season_id, type, stage_id = NULL)`; `kki_discover_stage_ids(sex, div, season_id, fetch_fn)`; `fetch_kki(..., stage = c("regular", "all"))`

**Why this is in scope.** D3 requires an honest regular-season projection. The probe at the top of this workstream shows KKÍ *declares* the boundary (`stage=300475` → exactly 132 matches for male BD, matching the spec's derived count; `stage=306658` → the remaining 30). Capturing it makes the rule correct by construction instead of correct-for-2026. **Decided fallback:** if stage discovery fails for a cell, `stage` is written `NA` and the downstream `n_rounds` derivation (`meetings × (n_teams − 1)`, pair-meeting test as the acceptance check) governs — that path stays as the documented backstop, not as the primary.

- [ ] **Step 1: Capture the stage fixture.**

```bash
cd /Users/brynjolfurjonsson/sports && curl -s -o tests/testthat/fixtures/kki_basketball/sample_male_div1_2027_deildarkeppni.xlsx \
  "https://widgets.baskethotel.com/widget-service/export/view/schedule_and_results?api=a0d07178160bf749eb6e5e761fc623fe42e2bb57&season_id=132568&lang=is&month=all&type=results_only&stage=300475" \
  && head -c 2 tests/testthat/fixtures/kki_basketball/sample_male_div1_2027_deildarkeppni.xlsx | xxd -p
```

Expected: `504b`. Anything else means the stage id rotated for 2026-27 — re-read it off the page with `kki_discover_stage_ids()` (Step 5) before continuing.

- [ ] **Step 2: Write the failing test.**

```r
test_that("the results schema carries a nullable stage column, last", {
  s <- schemas()$results
  expect_true("stage" %in% s$names)
  expect_identical(s$names[[length(s$names)]], "stage")
  expect_identical(s$GetFieldByName("stage")$type$ToString(), "string")
})

test_that("baskethotel_url appends the verified `stage` parameter, not `stage_id`", {
  url <- baskethotel_url(130403L, type = "results_only", stage_id = "300475")
  expect_match(url, "&stage=300475$")
  expect_false(grepl("stage_id=", url, fixed = TRUE))
  expect_false(grepl("stage=", baskethotel_url(130403L, type = "results_only"), fixed = TRUE))
})

test_that("kki_discover_stage_ids reads the second selector", {
  html <- rvest::read_html(fixture("motayfirlit_190_2027.html"), encoding = "UTF-8")
  out <- kki_discover_stage_ids("male", "div1", 132568L, fetch_fn = function(url, ...) html)

  expect_named(out, c("sex", "div", "season_id", "stage_id", "label"))
  expect_identical(out$stage_id, c("300475", "306658"))
  expect_true(all(out$season_id == 132568L))
})

test_that("parse_baskethotel_xlsx stamps the stage it was fetched under", {
  skip_if_not(
    file.exists(fixture("sample_male_div1_2027_deildarkeppni.xlsx")),
    "no 2027 stage fixture captured"
  )
  parsed <- parse_baskethotel_xlsx(
    fixture("sample_male_div1_2027_deildarkeppni.xlsx"),
    sport = "basketball", country = "iceland", sex = "male",
    division = "BD", season = 2027L, stage = "Deildarkeppni"
  )
  expect_true("stage" %in% names(parsed))
  expect_true(all(parsed$stage == "Deildarkeppni"))
})

test_that("parse_baskethotel_xlsx defaults stage to NA when none was requested", {
  parsed <- parse_baskethotel_xlsx(
    fixture("sample_male_div1_2026.xlsx"),
    sport = "basketball", country = "iceland", sex = "male",
    division = "BD", season = 2026L
  )
  expect_true(all(is.na(parsed$stage)))
  expect_type(parsed$stage, "character")
})

test_that("results rows lacking stage still validate and round-trip", {
  root <- withr::local_tempdir()
  rows <- tibble::tibble(
    sport = "handball", country = "iceland", sex = "male", season = 2027L,
    match_date = as.Date("2100-01-01"), home_team = "A", away_team = "B",
    home_score = 30L, away_score = 28L, division = "OD", round = 1L,
    stage = NA_character_
  )
  expect_silent(write_table(rows, "results", root = root))
  back <- read_table("results", root = root)
  expect_identical(nrow(back), 1L)
  expect_true(is.na(back$stage))
})
```

- [ ] **Step 3: Run it, expect the exact failure.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-ingest-kki.R")'
```

Expected: block 1 fails `"stage" %in% s$names is not TRUE`; block 2 fails `unused argument (stage_id = "300475")`; block 3 `could not find function "kki_discover_stage_ids"`; blocks 4-5 fail `unused argument (stage = ...)` / `"stage" %in% names(parsed) is not TRUE`; block 6 fails `Table 'results' missing column(s): stage`.

- [ ] **Step 4: Add the schema column.** In `R/storage-schemas.R`, after `round = arrow::int32()` in the `results` schema (line 23):

```r
      round       = arrow::int32(),
      # KKÍ declares a competition stage (Deildarkeppni / Úrslitakeppni /
      # A-B riðill) that is authoritative for the regular-season boundary,
      # reachable as a `stage` query parameter on the Baskethotel export.
      # NA for every source that posts no stage (KSÍ, HSÍ, World Cup) and for
      # partitions written before 2026-09. Additive and nullable — read_table()
      # unifies fragment schemas, so no migration. Added 2026-09-02.
      stage       = arrow::string()
```

- [ ] **Step 5: Implement the ingest side.** In `R/ingest-kki-basketball.R`:

```r
# baskethotel_url() -- add the stage parameter, verified name `stage`
# (`stage_id` and `competition_stage_id` are ignored by the endpoint).
baskethotel_url <- function(season_id, type = c("results_only", "schedule_only"),
                            stage_id = NULL) {
  type <- match.arg(type)
  url <- sprintf(
    paste0(
      "https://widgets.baskethotel.com/widget-service/export/view/",
      "schedule_and_results?api=%s&season_id=%s&lang=is&month=all&type=%s"
    ),
    BASKETHOTEL_API, season_id, type
  )
  if (!is.null(stage_id) && nzchar(stage_id)) {
    url <- paste0(url, "&stage=", stage_id)
  }
  url
}
```

```r
# download_baskethotel_xlsx() -- forward stage_id (signature line + url line):
download_baskethotel_xlsx <- function(season_id,
                                      type = c("results_only", "schedule_only"),
                                      stage_id = NULL) {
  type <- match.arg(type)
  url <- baskethotel_url(season_id, type, stage_id = stage_id)
```

```r
# parse_baskethotel_xlsx() -- new `stage` argument, stamped onto every row.
# Signature gains `stage = NA_character_`; the tibble gains, after `round`:
    round = NA_integer_,
    stage = as.character(stage)
# and the empty frame (parse_baskethotel_xlsx_empty) gains `stage = character()`.
```

```r
#' Discover KKÍ's declared competition stages for a (sex, div, season).
#'
#' The stage selector is KKÍ's own regular-season / post-season split, and it
#' is authoritative in a way no round-count heuristic is: for male div1 2026,
#' `stage=300475` (Deildarkeppni) returns exactly 132 matches and
#' `stage=306658` (Úrslitakeppni) the remaining 30, summing to the 162 the
#' unfiltered export returns. Stage ids rotate with the season, so they are
#' discovered, never registered.
#'
#' @param sex,div Cell keys.
#' @param season_id Baskethotel season id from [kki_resolve_season_id()].
#' @param fetch_fn Injection seam; defaults to the shared chromote helper.
#' @return Tibble `(sex, div, season_id, stage_id, label)`.
#' @keywords internal
#' @noRd
kki_discover_stage_ids <- function(sex, div, season_id,
                                   fetch_fn = fetch_rendered_html) {
  html <- fetch_fn(
    kki_league_url(kki_league_id(sex, div), season_id = season_id),
    ready_fn = function(h) length(rvest::html_elements(h, "select")) >= 2L
  )
  stages <- parse_kki_stage_options(html)
  tibble::tibble(
    sex = sex,
    div = div,
    season_id = as.integer(season_id),
    stage_id = stages$stage_id,
    label = stages$label
  )
}
```

In `fetch_kki`, add `stage = c("regular", "all")` to the signature (with `stage <- match.arg(stage)`) and, inside the division loop after `season_id` resolves:

```r
      stage_id <- NULL
      stage_label <- NA_character_
      if (identical(stage, "regular")) {
        stages <- tryCatch(
          kki_discover_stage_ids(sex, div, season_id),
          error = function(e) {
            cli::cli_warn(c(
              "KKÍ stage discovery failed for {sex}/{div}/{season_int}: {conditionMessage(e)}",
              "i" = "Falling back to the unfiltered export; the regular-season boundary reverts to round derivation."
            ))
            NULL
          }
        )
        hit <- if (is.null(stages)) NULL else stages[stages$label == "Deildarkeppni", , drop = FALSE]
        if (!is.null(hit) && nrow(hit) == 1L) {
          stage_id <- hit$stage_id[[1L]]
          stage_label <- hit$label[[1L]]
        }
      }
```

then pass `stage_id = stage_id` to `download_baskethotel_xlsx()` and `stage = stage_label` to `parse_baskethotel_xlsx()`.

- [ ] **Step 6: Backfill `stage` for every other source.** In `R/ingest.R`, extend the block at `:64-72`:

```r
  # Only KKÍ posts a competition stage. Backfill NA for every other source so
  # the results schema's stage column is satisfied without touching the KSÍ /
  # HSÍ / World Cup fetchers — same pattern as schedules.kickoff_time above.
  if (nrow(results) > 0 && !("stage" %in% names(results))) {
    results$stage <- NA_character_
  }
```

- [ ] **Step 7: Run the full suite, expect pass.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::document(quiet = TRUE); devtools::load_all(quiet = TRUE); devtools::test()'
```

Expected: `[ FAIL 0 | WARN 0 ]`. `test-ingest-ksi.R`, `test-ingest-hsi.R`, `test-wc-ingest-overlay.R` and `test-ingest-integration.R` must all stay green — they never set `stage`, and `ingest_league()` now fills it.

- [ ] **Step 8: Prove the boundary end to end against live KKÍ.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e '
devtools::load_all(quiet = TRUE)
reg <- fetch_kki(list(), "male", seasons = 2026L, type = "results_only", stage = "regular")
all <- fetch_kki(list(), "male", seasons = 2026L, type = "results_only", stage = "all")
bd_reg <- reg[reg$division == "BD", ]
bd_all <- all[all$division == "BD", ]
cat("BD regular:", nrow(bd_reg), " BD all:", nrow(bd_all), "\n")
cat("stage label:", unique(bd_reg$stage), "\n")
pairs <- table(apply(cbind(pmin(bd_reg$home_team, bd_reg$away_team),
                           pmax(bd_reg$home_team, bd_reg$away_team)), 1, paste, collapse = "|"))
cat("distinct pairs:", length(pairs), " meetings table:\n"); print(table(pairs))
'
```

Expected: `BD regular: 132  BD all: 162`, `stage label: Deildarkeppni`, `distinct pairs: 66`, and the meetings table showing all 66 pairs at exactly `2` — the spec's pair-meeting acceptance check, now satisfied by KKÍ's own declaration rather than by a derived cut.

- [ ] **Step 9: Commit.**

```bash
git -C /Users/brynjolfurjonsson/sports add R/storage-schemas.R R/ingest.R R/ingest-kki-basketball.R DESCRIPTION tests/testthat/test-ingest-kki.R tests/testthat/fixtures/kki_basketball/sample_male_div1_2027_deildarkeppni.xlsx
git -C /Users/brynjolfurjonsson/sports commit -m "feat(kki): capture KKI's declared stage, so D3 is correct by construction

D3 needs an honest regular-season projection, and KKI already declares the
boundary: the Baskethotel export accepts a stage parameter, and for male BD
2026 stage=300475 returns exactly the 132 regular matches (all 66 pairs
twice) with the post-season's 30 in stage=306658. Deriving the cut from round
counts happens to fit 2026 and provably misfits basketball female BD, whose
post-season rounds look like full rounds. Round derivation stays as the
documented fallback when stage discovery fails.

stage is additive and nullable; read_table already unifies fragment schemas,
so existing partitions null-fill and no other fetcher changes.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Closing check for WS5

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::load_all(quiet = TRUE); devtools::test()' \
  && git -C /Users/brynjolfurjonsson/sports log --oneline origin/main..HEAD \
  && git -C /Users/brynjolfurjonsson/sports status --short
```

Expected: full suite green, nine WS5 commits listed on `feat/bb-hb-metill-parity`, clean working tree. **Do not push** — the branch opens as one PR only after WS4 and WS6 have also landed on it (spec §5: no CI run may observe the ingest gate lifted while a season resolver is stale).

---


# WS6 — Delete the ingest activation gate, generically (spec §7)

**Produces (later workstreams rely on these):**

- `ingest_log_path(root = here::here("data")) -> fs_path  # data/health/ingest_log.json; @noRd`
- `read_ingest_log(root = here::here("data")) -> named list of records, empty named list when the file is absent; @export`
- `record_ingest_attempt(key, sex, n_rows, now = Sys.time(), root = here::here("data")) -> invisible(record); record = list(last_attempt_at, last_rows, zero_streak, last_nonzero_at); @export`
- `..ingest_backoff(entry, now, min_interval_hours = 24, zero_streak_threshold = 3L) -> logical(1); pure, fail-open (FALSE) on NULL/unparseable entry; @noRd  [real name: .ingest_backoff]`
- `ingest_one_league(static, key, active_path, root = here::here("data"), force = FALSE, offseason_min_interval_hours = 24, now = Sys.time()) -> integer(1) rows fetched; active_path retained but no longer consulted; @export`
- `ingest_one_lengjan(static, lengjan, key, active_path) -> integer(1)  # UNCHANGED, still gated by .is_league_active()`
- `".is_league_active(active_path, key) -> logical(1)  # UNCHANGED, now single-caller (ingest_one_lengjan)"`

## Workstream 6 — Delete the ingest activation gate, generically

**Why this is not a basketball/handball fix.** `ingest_one_league()` gates on
`config/active_competitions.json`, which `generate_active_competitions()` derives
solely from `data/facts/schedules` rows — rows only ingest can write. A league whose
fixtures have all been played can never write the rows that would mark it active
again. Both 2DT sports read `"false"` today; football's last fixture is 2026-10-25
and it hits the identical wall in November. Hand-editing the JSON does not help:
`scrape-results.yml` runs step 00 immediately before step 01 and overwrites it.

**What replaces the cost control.** The gate's own docstring justifies it as "saves a
chromote launch" — real (`R/ingest-hsi-handball.R:224` calls
`rvest::read_html_live()`), but recoverable from *fetch* state instead of *fetched*
data. `data/health/ingest_log.json` records, per `(league, sex)`,
`last_attempt_at` / `last_rows` / `zero_streak` / `last_nonzero_at`. A cell is
skipped only when its last attempt was inside `offseason_min_interval_hours` (24)
**and** its last three attempts all returned zero rows. Because the input is "when
did we last try", a dormant league resumes on the next poll with no human action.

**Residual risk the spec names, and its bound.** Off-season cells now *warn daily*
rather than skipping silently, so the log gets noisier. The backoff bounds that to
**one line per (league, sex) per 24 h** — six lines a day for the three Icelandic
leagues at full dormancy, against a workflow that runs once daily anyway. The
message *content* is what carries the diagnosis (Task 5's runbook):
a fetch that **errors** aborts the run and red-Xs CI (broken scraper);
a fetch that returns **0 rows for the first time after a non-empty run** warns
loudly, naming `last_nonzero_at` (season end *or* a silently-empty scraper — the one
genuinely ambiguous case, deliberately made loud); the **2nd+** consecutive zero
drops to `cli_alert_info` "treating as off-season". An errored fetch records no
attempt, so it never earns backoff — the fail-open is on the side of retrying.

Branch: `feat/bb-hb-metill-parity`. Verify before starting:

```bash
git -C /Users/brynjolfurjonsson/sports fetch origin
git -C /Users/brynjolfurjonsson/sports checkout feat/bb-hb-metill-parity
git -C /Users/brynjolfurjonsson/sports status --short
```

---

### Task 1: The fetch-state store (`R/ingest-log.R`)

**Files:**
- Create: `/Users/brynjolfurjonsson/sports/R/ingest-log.R`
- Create: `/Users/brynjolfurjonsson/sports/tests/testthat/test-ingest-log.R`
- Modify (generated): `/Users/brynjolfurjonsson/sports/NAMESPACE`, `/Users/brynjolfurjonsson/sports/DESCRIPTION` (Collate), `/Users/brynjolfurjonsson/sports/man/read_ingest_log.Rd`, `/Users/brynjolfurjonsson/sports/man/record_ingest_attempt.Rd`

**Interfaces:**
- Consumes: nothing from this workstream. Mirrors the existing
  `record_placement_status()` / `read_placement_status()` pattern at
  `R/auto-place.R:98-138` (same directory, same timestamp format, same
  `jsonlite::write_json(auto_unbox = TRUE, pretty = TRUE)` call shape).
- Produces: `ingest_log_path()`, `read_ingest_log()`, `record_ingest_attempt()`,
  `.ingest_backoff()` — all four consumed by Task 2.

- [ ] **Step 1: Write the failing test file.**

```r
# tests/testthat/test-ingest-log.R
#
# The ingest log is the replacement for the deleted activation gate (spec §7).
# Its input is *fetch state* -- when did we last try -- never fetched data, so
# unlike config/active_competitions.json it cannot deadlock a finished season.

test_that("read_ingest_log returns an empty named list when the file is absent", {
  root <- withr::local_tempdir()
  expect_identical(read_ingest_log(root), stats::setNames(list(), character()))
})

test_that("record_ingest_attempt stores rows and resets the zero streak", {
  root <- withr::local_tempdir()
  now <- as.POSIXct("2026-11-01 06:00:00", tz = "UTC")

  rec <- record_ingest_attempt("handball_iceland", "male", 0L, now = now, root = root)
  expect_identical(rec$zero_streak, 1L)
  expect_identical(rec$last_rows, 0L)
  expect_null(rec$last_nonzero_at)

  rec <- record_ingest_attempt("handball_iceland", "male", 0L,
    now = now + 86400, root = root
  )
  expect_identical(rec$zero_streak, 2L)

  rec <- record_ingest_attempt("handball_iceland", "male", 42L,
    now = now + 2 * 86400, root = root
  )
  expect_identical(rec$zero_streak, 0L)
  expect_identical(rec$last_rows, 42L)
  expect_identical(rec$last_nonzero_at, "2026-11-03T06:00:00Z")
})

test_that("record_ingest_attempt keys per (league, sex) and round-trips through JSON", {
  root <- withr::local_tempdir()
  now <- as.POSIXct("2026-11-01 06:00:00", tz = "UTC")

  record_ingest_attempt("handball_iceland", "male", 0L, now = now, root = root)
  record_ingest_attempt("handball_iceland", "female", 7L, now = now, root = root)

  log <- read_ingest_log(root)
  expect_setequal(names(log), c("handball_iceland/male", "handball_iceland/female"))
  expect_identical(as.integer(log[["handball_iceland/male"]]$zero_streak), 1L)
  expect_identical(as.integer(log[["handball_iceland/female"]]$last_rows), 7L)
  expect_true(fs::file_exists(ingest_log_path(root)))
})

test_that(".ingest_backoff fires only on 3+ zeroes inside the interval", {
  now <- as.POSIXct("2026-11-04 06:00:00", tz = "UTC")
  entry <- function(streak, hours_ago) {
    list(
      last_attempt_at = format(now - hours_ago * 3600,
        "%Y-%m-%dT%H:%M:%SZ",
        tz = "UTC"
      ),
      last_rows = 0L,
      zero_streak = streak
    )
  }

  expect_false(.ingest_backoff(entry(2L, 1), now))   # streak too short
  expect_true(.ingest_backoff(entry(3L, 1), now))    # dormant, tried an hour ago
  expect_false(.ingest_backoff(entry(9L, 25), now))  # dormant but 25h stale -> poll
  expect_true(.ingest_backoff(entry(9L, 25), now, min_interval_hours = 48))
})

test_that(".ingest_backoff fails open on missing or unparseable state", {
  now <- as.POSIXct("2026-11-04 06:00:00", tz = "UTC")
  expect_false(.ingest_backoff(NULL, now))
  expect_false(.ingest_backoff(list(zero_streak = 9L), now))
  expect_false(.ingest_backoff(
    list(last_attempt_at = "not-a-timestamp", zero_streak = 9L), now
  ))
})
```

- [ ] **Step 2: Run it and record the exact failure.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::test(filter = "ingest-log")'
```

Expected: every `test_that()` block errors with
`Error in read_ingest_log(root): could not find function "read_ingest_log"` (and
correspondingly for `record_ingest_attempt`, `.ingest_backoff`, `ingest_log_path`).
5 failed, 0 passed.

- [ ] **Step 3: Write `R/ingest-log.R`.**

```r
#' @include storage.R
NULL

#' Path to the ingest-attempt log.
#'
#' Lives beside `placement_status.json` in `data/health/`. Unlike that file it
#' is git-tracked: CI checks out fresh each run, so the backoff state only
#' survives if it is committed (see `.github/workflows/scrape-results.yml`).
#' @param root Data root.
#' @noRd
ingest_log_path <- function(root = here::here("data")) {
  fs::path(root, "health", "ingest_log.json")
}

#' Read the ingest-attempt log.
#'
#' Records one entry per `"<league_key>/<sex>"` with fields `last_attempt_at`
#' (UTC ISO-8601), `last_rows`, `zero_streak` and `last_nonzero_at`. A missing
#' file yields an empty list, which fails open: nothing is ever skipped on the
#' strength of absent state.
#'
#' @param root Data root.
#' @return A named list of records; empty named list when the file is absent.
#' @export
read_ingest_log <- function(root = here::here("data")) {
  path <- ingest_log_path(root)
  if (!fs::file_exists(path)) {
    return(stats::setNames(list(), character()))
  }
  out <- tryCatch(
    jsonlite::read_json(path, simplifyVector = TRUE),
    error = function(e) {
      cli::cli_alert_warning(
        "Unreadable ingest log at {path} ({conditionMessage(e)}); treating as empty."
      )
      NULL
    }
  )
  if (is.null(out) || !is.list(out)) stats::setNames(list(), character()) else out
}

#' Record one federation-fetch attempt.
#'
#' Called by [ingest_one_league()] after every `(league, sex)` fetch that did
#' not error. An errored fetch deliberately records nothing, so a broken
#' scraper can never earn a backoff.
#'
#' @param key League key, e.g. `"handball_iceland"`.
#' @param sex `"male"` or `"female"`.
#' @param n_rows Rows fetched (results + schedule).
#' @param now Attempt time.
#' @param root Data root.
#' @return The updated record, invisibly.
#' @export
record_ingest_attempt <- function(key, sex, n_rows,
                                  now = Sys.time(),
                                  root = here::here("data")) {
  log <- read_ingest_log(root)
  id <- paste(key, sex, sep = "/")
  prev <- log[[id]]
  prev_streak <- if (is.null(prev) || is.null(prev$zero_streak)) {
    0L
  } else {
    as.integer(prev$zero_streak)
  }
  prev_nonzero <- if (is.null(prev)) NULL else prev$last_nonzero_at
  stamp <- format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  n_rows <- as.integer(n_rows)

  log[[id]] <- list(
    last_attempt_at = stamp,
    last_rows = n_rows,
    zero_streak = if (n_rows > 0L) 0L else prev_streak + 1L,
    last_nonzero_at = if (n_rows > 0L) stamp else prev_nonzero
  )

  path <- ingest_log_path(root)
  fs::dir_create(fs::path_dir(path))
  jsonlite::write_json(log, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  invisible(log[[id]])
}

#' Should this (league, sex) be skipped this run?
#'
#' TRUE only when the cell has returned zero rows `zero_streak_threshold` times
#' running AND was polled within `min_interval_hours`. Every other branch --
#' no entry, no timestamp, an unparseable timestamp, a short streak -- returns
#' FALSE, so the failure mode is "fetch anyway", never "never fetch again".
#'
#' @param entry One record from [read_ingest_log()], or `NULL`.
#' @param now Current time.
#' @param min_interval_hours Minimum hours between polls of a dormant cell.
#' @param zero_streak_threshold Consecutive empty fetches before backing off.
#' @return `logical(1)`.
#' @noRd
.ingest_backoff <- function(entry, now,
                            min_interval_hours = 24,
                            zero_streak_threshold = 3L) {
  if (is.null(entry) || is.null(entry$last_attempt_at)) {
    return(FALSE)
  }
  streak <- if (is.null(entry$zero_streak)) 0L else as.integer(entry$zero_streak)
  if (is.na(streak) || streak < zero_streak_threshold) {
    return(FALSE)
  }
  last <- as.POSIXct(entry$last_attempt_at,
    format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"
  )
  if (is.na(last)) {
    return(FALSE)
  }
  as.numeric(difftime(now, last, units = "hours")) < min_interval_hours
}
```

- [ ] **Step 4: Document and re-run.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::document()' \
  && Rscript -e 'devtools::test(filter = "ingest-log")'
```

Expected: `NAMESPACE` gains `export(read_ingest_log)` and
`export(record_ingest_attempt)`; `DESCRIPTION`'s `Collate:` gains
`'ingest-log.R'`; `man/read_ingest_log.Rd` and `man/record_ingest_attempt.Rd`
are created. Test output: `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 17 ]`.

- [ ] **Step 5: Commit.**

```bash
git -C /Users/brynjolfurjonsson/sports add R/ingest-log.R tests/testthat/test-ingest-log.R \
  NAMESPACE DESCRIPTION man/read_ingest_log.Rd man/record_ingest_attempt.Rd
git -C /Users/brynjolfurjonsson/sports commit -m "feat(ingest): fetch-state log to replace the activation gate

config/active_competitions.json is derived from schedule rows that only
ingest writes, so a finished season cannot restart itself. This store keys
on when we last *tried* rather than on what we found, which is what makes
the replacement cost control loop-free.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 2: Delete the gate from `ingest_one_league()`, add the backoff

**Files:**
- Modify: `/Users/brynjolfurjonsson/sports/R/ingest.R` lines 118–146 (roxygen block
  + body of `ingest_one_league`). **Do not touch** lines 105–115
  (`.is_league_active`) or 149–182 (`ingest_one_lengjan`).
- Create: `/Users/brynjolfurjonsson/sports/tests/testthat/test-ingest-activation-gate.R`
- Modify (generated): `/Users/brynjolfurjonsson/sports/man/ingest_one_league.Rd`

**Interfaces:**
- Consumes: `read_ingest_log()`, `record_ingest_attempt()`, `.ingest_backoff()` (Task 1);
  `ingest_league(league, sex, root, seasons)` (R/ingest.R:54);
  `register_ingest_source()` / `unregister_ingest_source()` for stubs.
- Produces: `ingest_one_league(static, key, active_path, root = here::here("data"), force = FALSE, offseason_min_interval_hours = 24, now = Sys.time())`. The
  first three arguments keep their positions, so `scripts/01_ingest_results.R:40`
  and `docs/superpowers/plans/*` call sites stay valid; Task 3 adds `force =`.

- [ ] **Step 1: Write the three behavioural tests.**

Note the fixture dates: `Sys.Date() + 30L` per the time-bomb rule — a hardcoded
near date would rot into a failing test.

```r
# tests/testthat/test-ingest-activation-gate.R
#
# Spec §7. The activation gate was a deadlock: config/active_competitions.json
# is written from schedule rows that only ingest can write, so a league whose
# fixtures have all been played could never mark itself active again. Federation
# ingest must therefore run regardless of that JSON; the odds scrape must keep
# honouring it, because odds genuinely do not exist outside a fixture window and
# no closed loop is involved there.

stub_source <- function(n_results = 1L) {
  list(
    fetch_results = function(league, sex, seasons = NULL) {
      if (n_results == 0L) {
        return(tibble::tibble())
      }
      tibble::tibble(
        sport = league$sport, country = league$country, sex = sex,
        season = 2026L,
        match_date = Sys.Date() - 10L,
        home_team = "A", away_team = "B",
        home_score = 10L, away_score = 8L,
        division = "D1", round = 1L
      )
    },
    fetch_schedule = function(league, sex) {
      if (n_results == 0L) {
        return(tibble::tibble())
      }
      tibble::tibble(
        sport = league$sport, country = league$country, sex = sex,
        season = 2026L,
        match_date = Sys.Date() + 30L,
        home_team = "C", away_team = "D",
        division = "D1", round = 2L
      )
    }
  )
}

write_inactive_json <- function(dir, key) {
  path <- file.path(dir, "active_competitions.json")
  jsonlite::write_json(
    list(
      generated_at = "2026-11-01T06:00:00Z",
      lookahead_days = 14L,
      degraded = FALSE,
      active = stats::setNames(list(FALSE), key)
    ),
    path,
    auto_unbox = TRUE, pretty = TRUE
  )
  path
}

test_that("a league marked inactive still ingests results and schedules", {
  register_ingest_source("gate_stub", stub_source())
  on.exit(unregister_ingest_source("gate_stub"), add = TRUE)

  tmp <- withr::local_tempdir()
  active_path <- write_inactive_json(tmp, "handball_iceland")
  static <- list(
    sport = "handball", country = "iceland", sexes = "male",
    data_source = list(results = "gate_stub", schedule = "gate_stub")
  )

  n <- suppressMessages(
    ingest_one_league(static, "handball_iceland", active_path, root = tmp)
  )

  expect_identical(n, 2L)
  expect_equal(nrow(read_table("results", root = tmp, filter = list(sport = "handball"))), 1L)
  expect_equal(nrow(read_table("schedules", root = tmp, filter = list(sport = "handball"))), 1L)
})

test_that("the same inactive league still skips the Lengjan odds scrape", {
  called <- FALSE
  testthat::local_mocked_bindings(
    ingest_lengjan_odds = function(...) {
      called <<- TRUE
      5L
    }
  )
  tmp <- withr::local_tempdir()
  active_path <- write_inactive_json(tmp, "handball_iceland")

  res <- suppressMessages(ingest_one_lengjan(
    list(sport = "handball", country = "iceland"), list(),
    "handball_iceland", active_path
  ))

  expect_identical(res, 0L)
  expect_false(called)
})

test_that("backoff engages after three empty fetches and resets on a non-empty one", {
  register_ingest_source("empty_gate_stub", stub_source(n_results = 0L))
  on.exit(unregister_ingest_source("empty_gate_stub"), add = TRUE)

  tmp <- withr::local_tempdir()
  active_path <- write_inactive_json(tmp, "handball_iceland")
  static <- list(
    sport = "handball", country = "iceland", sexes = "male",
    data_source = list(results = "empty_gate_stub", schedule = "empty_gate_stub")
  )
  t0 <- as.POSIXct("2026-11-01 06:00:00", tz = "UTC")
  run <- function(hours_in) {
    suppressMessages(ingest_one_league(static, "handball_iceland", active_path,
      root = tmp, now = t0 + hours_in * 3600
    ))
  }

  run(0)
  run(1)
  run(2)
  expect_identical(read_ingest_log(tmp)[["handball_iceland/male"]]$zero_streak, 3L)

  # 4th attempt an hour later: backed off, so last_attempt_at does NOT move.
  run(3)
  expect_identical(
    read_ingest_log(tmp)[["handball_iceland/male"]]$last_attempt_at,
    "2026-11-01T08:00:00Z"
  )

  # 25h after the last real attempt the cell is polled again (no deadlock).
  run(27)
  expect_identical(
    read_ingest_log(tmp)[["handball_iceland/male"]]$last_attempt_at,
    "2026-11-02T09:00:00Z"
  )

  # --force bypasses the backoff outright.
  run_forced <- suppressMessages(ingest_one_league(static, "handball_iceland",
    active_path,
    root = tmp, force = TRUE, now = t0 + 28 * 3600
  ))
  expect_identical(run_forced, 0L)
  expect_identical(
    read_ingest_log(tmp)[["handball_iceland/male"]]$last_attempt_at,
    "2026-11-02T10:00:00Z"
  )

  # Fixtures reappear -> the streak resets with no human action.
  unregister_ingest_source("empty_gate_stub")
  register_ingest_source("empty_gate_stub", stub_source(n_results = 1L))
  expect_identical(run(29), 2L)
  expect_identical(read_ingest_log(tmp)[["handball_iceland/male"]]$zero_streak, 0L)
})
```

- [ ] **Step 2: Run it and record the exact failure.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::test(filter = "ingest-activation-gate")'
```

Expected, before any change: test 1 fails with
`Failure: n (2L) not identical to 2L. Actual: 0L` — because
`ingest_one_league()` short-circuits on the inactive JSON and returns `0L`
(the `read_table()` assertions then error with
`Error: Not a valid table name/no such directory`, since nothing was written).
Test 2 **passes** already (that gate is correct and stays). Test 3 fails with
`Error in ingest_one_league(...): unused argument (now = ...)`.

- [ ] **Step 3: Rewrite `ingest_one_league()` in `R/ingest.R`.**

Replace lines 118–146 (the roxygen block plus the function) with:

```r
#' Run federation ingest for a single league across all configured sexes.
#'
#' Called by `scripts/01_ingest_results.R` for each active league.
#'
#' **No activation gate.** This wrapper used to short-circuit on
#' `config/active_competitions.json`, but that file is derived by
#' [generate_active_competitions()] from `data/facts/schedules` rows -- rows
#' that only this function can write. A league whose fixtures have all been
#' played could therefore never mark itself active again (spec §7: both 2DT
#' sports read `false`; football hits the same wall after 2026-10-25). The
#' gate remains in [ingest_one_lengjan()], where it is correct: odds genuinely
#' do not exist outside a fixture window, and no closed loop is involved.
#'
#' The gate's stated purpose -- saving a chromote launch (HSÍ fetches via
#' `rvest::read_html_live()`) -- is served instead by a backoff keyed on
#' *fetch* state: a `(league, sex)` is skipped only when its last three
#' attempts all returned zero rows AND the last was within
#' `offseason_min_interval_hours`. A dormant cell is polled once a day; a cell
#' whose fixtures reappear resumes on the next poll with no human action. A
#' fetch that *errors* records no attempt, so a broken scraper never earns a
#' backoff -- it keeps aborting the run, which is the intended loud signal.
#'
#' Takes the per-league "static" slice (sport, country, sexes, active,
#' stan_model, data_source) rather than the full leagues config.
#'
#' @param static Per-league static slice (sport, country, sexes, active,
#'   stan_model, data_source).
#' @param key League key (e.g. `"football_iceland"`).
#' @param active_path Path to `config/active_competitions.json`. Retained for
#'   call-site compatibility (`scripts/01_ingest_results.R`) and no longer
#'   consulted here -- see the note above.
#' @param root Data root.
#' @param force Bypass the off-season backoff (wired to `--force`).
#' @param offseason_min_interval_hours Minimum hours between polls of a cell
#'   whose last three fetches were all empty.
#' @param now Current time; injectable for tests.
#' @return Integer count of rows fetched (results + schedule combined),
#'   summed across the league's sexes. Not equal to rows newly written --
#'   `upsert_table()` deduplicates on disk. Use only as a "did anything
#'   happen" indicator.
#' @export
ingest_one_league <- function(static, key, active_path,
                              root = here::here("data"),
                              force = FALSE,
                              offseason_min_interval_hours = 24,
                              now = Sys.time()) {
  log <- read_ingest_log(root)
  total <- 0L

  for (sex in static$sexes) {
    entry <- log[[paste(key, sex, sep = "/")]]
    if (!isTRUE(force) &&
      .ingest_backoff(entry, now, offseason_min_interval_hours)) {
      cli::cli_alert_info(
        "{key}/{sex}: dormant ({entry$zero_streak} empty fetches in a row, last tried {entry$last_attempt_at}); next poll after {offseason_min_interval_hours}h. --force overrides."
      )
      next
    }

    n <- ingest_league(static, sex, root = root, seasons = NULL)
    rec <- record_ingest_attempt(key, sex, n, now = now, root = root)
    total <- total + n

    if (n == 0L) {
      if (identical(rec$zero_streak, 1L)) {
        since <- if (is.null(rec$last_nonzero_at)) {
          "never non-empty"
        } else {
          paste("last non-empty", rec$last_nonzero_at)
        }
        cli::cli_alert_warning(
          "{key}/{sex}: 0 rows, first empty fetch ({since}). Season end or a broken scraper -- watch the next run."
        )
      } else {
        cli::cli_alert_info(
          "{key}/{sex}: 0 rows ({rec$zero_streak} in a row) -- treating as off-season."
        )
      }
    }
  }

  total
}
```

- [ ] **Step 4: Re-run both test files plus the neighbours that touch these symbols.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::document()' \
  && Rscript -e 'devtools::test(filter = "ingest")'
```

Expected: all of `test-ingest-activation-gate`, `test-ingest-log`,
`test-ingest-dispatcher`, `test-ingest-lengjan-odds`, `test-ingest-hsi`,
`test-ingest-kki`, `test-ingest-ksi` green — `[ FAIL 0 | WARN 0 ]`.
`test-ingest-lengjan-odds.R:172` and `:189` still mock `.is_league_active`, which
must still exist; if that file errors with `could not find function ".is_league_active"`
the wrong function was deleted. `man/ingest_one_league.Rd` is regenerated with the
four new arguments.

- [ ] **Step 5: Confirm nothing else in the package still consults the gate for federation ingest.**

```bash
grep -rn "is_league_active" /Users/brynjolfurjonsson/sports/R /Users/brynjolfurjonsson/sports/scripts
```

Expected exactly three hits, all in `R/ingest.R`: the definition (~line 113) and
the single remaining call inside `ingest_one_lengjan()` (~line 160), plus its
roxygen mention. Zero hits in `scripts/`.

- [ ] **Step 6: Commit.**

```bash
git -C /Users/brynjolfurjonsson/sports add R/ingest.R man/ingest_one_league.Rd \
  tests/testthat/test-ingest-activation-gate.R
git -C /Users/brynjolfurjonsson/sports commit -m "fix(ingest): delete the self-locking activation gate from ingest_one_league

The gate read config/active_competitions.json, which is derived solely from
schedule rows that only ingest writes -- so a league whose fixtures were all
played could never restart itself. Both 2DT sports are stuck there now and
football hits the same wall in November, hence a generic fix rather than a
per-sport one. The odds scrape keeps the gate: odds really do not exist
outside a fixture window and nothing there is circular.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 3: Stop `--force` being a lie

**Files:**
- Modify: `/Users/brynjolfurjonsson/sports/scripts/01_ingest_results.R` (usage header
  lines 8–10, call site line 40)
- Create: `/Users/brynjolfurjonsson/sports/tests/testthat/test-script-ingest-force.R`

**Interfaces:**
- Consumes: `parse_pipeline_args()` → `list(league, sex, force)` (`scripts/_lib.R:8`);
  `ingest_one_league(..., force = )` (Task 2).
- Produces: nothing downstream; this is the operator-facing escape hatch the
  runbook in Task 5 points at.

- [ ] **Step 1: Write the static guard test.**

Scripts are not loadable by `devtools::load_all()`, so this mirrors the existing
static-check pattern in `tests/testthat/test-script-ledger-commit.R`.

```r
# tests/testthat/test-script-ingest-force.R
#
# opts$force was parsed by parse_pipeline_args() and never read -- passing
# --force to 01_ingest_results.R silently did nothing. Now that the off-season
# backoff is the only thing standing between a dormant league and a fetch,
# the flag is the operator's override and must actually reach it.

test_that("01_ingest_results.R forwards --force to ingest_one_league", {
  path <- here::here("scripts", "01_ingest_results.R")
  expect_true(file.exists(path))
  src <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(src, "parse_pipeline_args\\(\\)", fixed = FALSE)
  expect_match(src, "force\\s*=\\s*isTRUE\\(opts\\$force\\)", fixed = FALSE)
  expect_match(src, "--force", fixed = TRUE)
})

test_that("01_ingest_results.R no longer implies the active JSON gates ingest", {
  src <- paste(
    readLines(here::here("scripts", "01_ingest_results.R"), warn = FALSE),
    collapse = "\n"
  )
  # The file must still *require* the JSON (02_scrape_odds.R consumes it), but
  # must not describe itself as scraping only "active" leagues via that file.
  expect_match(src, "active_competitions.json", fixed = TRUE)
  expect_no_match(src, "skipped (no active fixtures)", fixed = TRUE)
})
```

- [ ] **Step 2: Run it and record the exact failure.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::test(filter = "script-ingest-force")'
```

Expected: first test fails with
`Failure: src does not match "force\s*=\s*isTRUE\(opts\$force\)". Actual value: "...ingest_one_league(static, key, active_path)..."`.
Second test passes already.

- [ ] **Step 3: Edit the script.**

Replace the usage header (lines 8–10) with:

```r
# Usage:
#   Rscript scripts/01_ingest_results.R                    # all active leagues
#   Rscript scripts/01_ingest_results.R --league football_iceland
#   Rscript scripts/01_ingest_results.R --force            # ignore off-season backoff
#
# A league whose last three fetches were all empty is polled at most once per
# 24h (data/health/ingest_log.json). --force bypasses that.
```

and replace line 40 with:

```r
  ingest_one_league(static, key, active_path, force = isTRUE(opts$force))
```

- [ ] **Step 4: Re-run and smoke-test the script end to end.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::test(filter = "script-ingest-force")'
```

Expected: `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 5 ]`.

```bash
cd /Users/brynjolfurjonsson/sports && Rscript scripts/01_ingest_results.R --league handball_iceland --force
```

Expected: the run reaches the HSÍ scraper for `handball_iceland/male` and
`/female` — i.e. a real fetch attempt, **not** `handball_iceland: skipped (no
active fixtures)`. Whether the fetch itself succeeds is WS2's problem (HSÍ moved
to `/tournament/<id>`); the point verified here is that the gate no longer fires.
Afterwards `data/health/ingest_log.json` exists with two entries:

```bash
cat /Users/brynjolfurjonsson/sports/data/health/ingest_log.json
```

- [ ] **Step 5: Commit.**

```bash
git -C /Users/brynjolfurjonsson/sports add scripts/01_ingest_results.R \
  tests/testthat/test-script-ingest-force.R
git -C /Users/brynjolfurjonsson/sports commit -m "fix(scripts): wire --force through 01_ingest_results.R

The flag was parsed and discarded. With the backoff now the only thing that
can skip a fetch, --force is the operator override the runbook points at, so
it has to reach ingest_one_league(). Guarded statically because the script is
not loadable by devtools.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 4: Persist the backoff state across CI's fresh checkout

**Files:**
- Modify: `/Users/brynjolfurjonsson/sports/.github/workflows/scrape-results.yml` line 63
- Create: `/Users/brynjolfurjonsson/sports/tests/testthat/test-ci-ingest-log-persisted.R`

**Interfaces:**
- Consumes: `ingest_log_path()` (Task 1) — the workflow stages the literal path.
- Produces: nothing in R.

**Why.** `actions/checkout@v5` gives every run a clean tree. If
`data/health/ingest_log.json` is not committed, the log is always absent on CI,
`.ingest_backoff()` always fails open, and the replacement cost control is inert
exactly where the chromote launch is paid for. Cost of tracking it: one small JSON
in the daily `scrape-results` commit (`cron: '0 6 * * *'`), which already commits
`config/active_competitions.json` on the same cadence. Note this file is
**tracked**, unlike `data/health/placement_status.json`, which `.gitignore:81`
excludes because it is machine-local to the launchd placer.

- [ ] **Step 1: Write the guard test.**

```r
# tests/testthat/test-ci-ingest-log-persisted.R
#
# The off-season backoff replaced the activation gate (spec §7). Its state
# lives in data/health/ingest_log.json, and CI checks out fresh every run --
# so if the workflow does not commit the file, the backoff is permanently
# inert on CI and every dormant league pays a chromote launch daily. This
# guards the pairing.

test_that("scrape-results.yml stages the ingest log", {
  f <- here::here(".github", "workflows", "scrape-results.yml")
  expect_true(file.exists(f))
  src <- paste(readLines(f, warn = FALSE), collapse = "\n")

  expect_match(src, "scripts/01_ingest_results.R", fixed = TRUE)
  expect_match(src, "data/health/ingest_log.json", fixed = TRUE)
})

test_that(".gitignore does not exclude the ingest log", {
  ignores <- readLines(here::here(".gitignore"), warn = FALSE)
  expect_false(any(grepl("ingest_log", ignores, fixed = TRUE)))
  # placement_status.json stays ignored -- it is machine-local to the launchd
  # placer, whereas the ingest log must survive a CI checkout.
  expect_true(any(grepl("data/health/placement_status.json", ignores, fixed = TRUE)))
})
```

- [ ] **Step 2: Run it and record the exact failure.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::test(filter = "ci-ingest-log-persisted")'
```

Expected: first test fails with
`Failure: src does not match "data/health/ingest_log.json"` (the workflow's
`git add` currently lists only results, schedules and the active JSON). Second
test passes.

- [ ] **Step 3: Edit the workflow.**

Replace line 63 of `.github/workflows/scrape-results.yml`:

```yaml
          git add data/facts/results/ data/facts/schedules/ config/active_competitions.json data/health/ingest_log.json
```

and add a comment directly above the `git add` so the pairing is legible on CI:

```yaml
          # ingest_log.json carries the off-season backoff state (spec §7).
          # CI checks out fresh, so it must be committed or the backoff is
          # inert here and every dormant league pays a chromote launch daily.
```

- [ ] **Step 4: Re-run, plus the CI-convention neighbours.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::test(filter = "ci-|placer-ci|healthcheck-ci")'
```

Expected: `[ FAIL 0 | WARN 0 ]`. `test-placer-ci-isolation.R` must stay green —
nothing added here references `R/placer-`, `place_bets` or `LENGJAN_*`.

- [ ] **Step 5: Commit the workflow and the first real log file together.**

```bash
git -C /Users/brynjolfurjonsson/sports add .github/workflows/scrape-results.yml \
  tests/testthat/test-ci-ingest-log-persisted.R data/health/ingest_log.json
git -C /Users/brynjolfurjonsson/sports commit -m "ci(scrape-results): commit the ingest log so the backoff survives checkout

CI gets a clean tree each run; an uncommitted ingest_log.json means the
backoff never fires on CI, which is precisely where the chromote launch the
old gate saved is paid for. One small JSON on an already-daily commit.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 5: Document the new log-reading, and run the whole suite

**Files:**
- Create: `/Users/brynjolfurjonsson/sports/docs/runbooks/dormant-league.md`
- Modify: `/Users/brynjolfurjonsson/sports/CLAUDE.md` line 26

**Interfaces:**
- Consumes: everything above. Produces no code.

**Why a runbook.** The residual risk the spec names is log noise: off-season cells
now warn daily instead of skipping silently. The bound is the backoff (≤1 line per
cell per 24 h); the *disambiguation* is the message text, and that only helps if it
is written down next to `stale-odds.md` and `failed-fit.md`, which is where the
on-call reflex already goes.

- [ ] **Step 1: Write the runbook.**

```bash
cat > /Users/brynjolfurjonsson/sports/docs/runbooks/dormant-league.md <<'EOF'
# Runbook: a league is fetching zero rows

`scripts/01_ingest_results.R` no longer consults
`config/active_competitions.json` (spec §7 — that file is derived from
schedule rows only ingest can write, so a finished season could never restart
itself). Every configured league is now fetched every run, and the only thing
that skips a fetch is the off-season backoff in
`data/health/ingest_log.json`.

## Reading the log line

| What you see in the run log | What it means | Action |
|---|---|---|
| The run **errors** and the workflow goes red | Broken scraper: the federation site moved, changed markup, or timed out past its retries. `record_ingest_attempt()` was never reached, so no backoff was earned and the next run retries in full. | Fix the scraper. |
| `0 rows, first empty fetch (last non-empty <ts>)` — a **warning** | Ambiguous by construction, and deliberately loud: either the season just ended, or a scraper started returning an empty frame without erroring. | Compare `<ts>` with the league's fixture list. If the last fixture has been played, ignore. If fixtures remain, treat as a broken scraper. |
| `0 rows (N in a row) -- treating as off-season` — info | Settled dormancy. | None. |
| `dormant (N empty fetches in a row, last tried <ts>); next poll after 24h` | The backoff is doing its job: no chromote launch this run. | None. |

A cell that starts producing rows again resets `zero_streak` to `0` on the very
next poll — no human action, no config edit, no flag.

## Log volume

A fully dormant three-league repo emits at most six lines per day (one per
`(league, sex)` per 24 h), because a backed-off cell logs once and then stays
quiet until the interval lapses. If that becomes noisy, raise
`offseason_min_interval_hours` at the `ingest_one_league()` call site rather
than reintroducing a data-derived gate.

## Forcing a poll

```bash
Rscript scripts/01_ingest_results.R --league handball_iceland --force
```

`--force` bypasses the backoff for that run only; it does not clear the streak
(a fetch that returns rows does).

## Inspecting the state

```bash
cat data/health/ingest_log.json
```

```r
sports::read_ingest_log()
```

The file is git-tracked and committed by `scrape-results.yml` — CI checks out
fresh, so uncommitted state would make the backoff inert there. Deleting it is
safe: every read fails open to "fetch anyway".
EOF
```

- [ ] **Step 2: Update the directory-structure line in CLAUDE.md.**

Replace line 26:

```
│   ├── health/          # status.json (pipeline_health snapshot) + ingest_log.json (off-season backoff state)
```

- [ ] **Step 3: Run the full suite.**

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e 'devtools::test()' 2>&1 | tail -20
```

Expected: `[ FAIL 0 | WARN 0 | SKIP n | PASS >1120 ]`. Watch specifically for
`test-ingest-lengjan-odds.R` (mocks `.is_league_active`, which must still exist),
`test-schedule-active.R` (`generate_active_competitions()` is untouched — the JSON
is still produced and still consumed by `02_scrape_odds.R`), and
`test-skill-conventions.R` / `test-placer-ci-isolation.R`.

- [ ] **Step 4: Confirm the working tree holds nothing but intended changes.**

```bash
git -C /Users/brynjolfurjonsson/sports status --short
git -C /Users/brynjolfurjonsson/sports diff --stat origin/main...HEAD
```

Expected: no unexpected `data/facts/**` churn from the Step-4 smoke run in Task 3
(if the HSÍ fetch did write rows, commit them separately as `data(facts): ...` —
do not fold federation data into a code commit).

- [ ] **Step 5: Commit the docs.**

```bash
git -C /Users/brynjolfurjonsson/sports add docs/runbooks/dormant-league.md CLAUDE.md
git -C /Users/brynjolfurjonsson/sports commit -m "docs(runbook): how to tell a finished season from a broken scraper

Removing the gate trades a silent skip for a daily warning. The backoff bounds
the volume; this runbook is what makes the remaining lines diagnostic rather
than noise.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Merge note

Spec §445 requires this workstream to land in **one PR with workstreams 4 and 5**,
so no CI run ever sees the gate lifted while a season resolver is still stale, and
§136 requires **WS1 (`betting.enabled: false`)** to be already merged — the moment
schedules repopulate, `ingest_one_lengjan()` starts scraping odds and the armed
`is.metill.sports.autoplace` agent would stake real money on handball. Do not
merge this workstream ahead of WS1.

---
