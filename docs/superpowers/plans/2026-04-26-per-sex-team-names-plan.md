# Per-sex `team_names` schema + post-Plan-6 small wins — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Bundle three post-Plan-6 mechanical follow-ups (cron verification, CLAUDE.md drift fix, worktree hygiene check) and the per-sex `team_names` schema redesign so women's leagues become bettable and current male BD recs unblock.

**Architecture:** Hard cutover. Single source of truth (`config/leagues.yml`) gets a nested `team_names: {male: {...}, female: {...}}` shape; schema, validator, pipeline lookup, and tests all change in one commit.

**Tech Stack:** R package monorepo with `devtools`, `testthat` edition 3, `arrow` for Parquet I/O, `cli` for messages, `chromote` for browser automation, `cmdstanr` for Stan models. Config validated via `jsonvalidate` against JSON Schema draft-07.

**Spec:** [`docs/superpowers/specs/2026-04-26-per-sex-team-names-design.md`](../specs/2026-04-26-per-sex-team-names-design.md)

---

## Task 1: Small wins (cron verify + CLAUDE.md drift + worktree check)

**Files:**
- Modify: `CLAUDE.md` (Skills section, ~line 273)
- Read-only: GitHub Actions runs, `git worktree list`

- [ ] **Step 1.1: Verify the four CI workflows have run successfully**

Run:
```bash
gh run list --limit 30 --json workflowName,status,conclusion,createdAt,headBranch \
  | jq '.[] | select(.headBranch == "main") | "\(.createdAt) \(.workflowName) \(.status)/\(.conclusion)"' \
  | head -30
```

Expected: at least one successful (`completed/success`) run for each of `CI Tests`, `Scrape Odds`, `Scrape Results`, and `Fit and Publish` since 2026-04-25 (Plan 6 cutover). Print the results to the user.

If any workflow has zero successful runs since Plan 6, flag it loudly. Do not attempt to fix in this task — just report.

- [ ] **Step 1.2: Spot-check fresh data**

Run:
```bash
Rscript -e '
suppressMessages({ library(arrow); library(dplyr) })
o <- open_dataset("data/facts/odds") |> collect()
cat("Odds rows:", nrow(o), "max scraped_at:", as.character(max(o$scraped_at)), "\n\n")
r <- open_dataset("data/facts/results") |> collect()
cat("Results rows:", nrow(r), "\n\n")
s <- open_dataset("data/facts/schedules") |> collect()
cat("Schedule rows:", nrow(s), "\n\n")
b <- open_dataset("data/beliefs/latest") |> collect()
cat("Beliefs rows:", nrow(b), "\n")
'
```

Expected: `max scraped_at` is within the last 24h (proves `scrape-odds` cron is firing). Beliefs and results rowcounts are non-zero. Print findings to user — no code change.

- [ ] **Step 1.3: Edit CLAUDE.md "Skills" section**

Find the "Skills" section in `CLAUDE.md` (at or near line 273) which currently reads:

```markdown
## Skills

The four model-invocable skills under `.claude/skills/` (`/bet`, `/sports-update`, `/add-league`, `/place-bets`) remain from the pre-migration workspace. They were authored against the legacy four-repo layout and will be revised to use `Rscript run.R --step ...` in a follow-up pass; for now they continue to work via the deprecated `scripts/*_all.R` runners.

**Do not add `disable-model-invocation: true` to these skills.** They are intentionally model-invocable.
```

Replace with:

```markdown
## Skills

The four model-invocable skills under `.claude/skills/` (`/bet`, `/sports-update`, `/add-league`, `/place-bets`) were rewritten in `f50b0bd` (post-Plan-6) to invoke the `{targets}` DAG via `Rscript run.R --step ...`. Drift is guarded by `tests/testthat/test-skill-conventions.R`, which fails the build if any skill references the legacy four-repo layout (`lengjan-bets/`, `lengjan-odds/`, `Sports/{sport}/{country}/`, or the `--sync` flag).

**Do not add `disable-model-invocation: true` to these skills.** They are intentionally model-invocable.
```

- [ ] **Step 1.4: Verify no leftover worktrees**

Run: `git worktree list`

Expected: only the main checkout. If extras appear, report to user — do not delete (destructive op needs explicit ask).

- [ ] **Step 1.5: Commit Task 1**

```bash
git add CLAUDE.md
git commit -m "$(cat <<'EOF'
docs: CLAUDE.md — Skills section reflects post-Plan-6 rewrite

The four .claude/skills/* entries were rewritten in f50b0bd to use
Rscript run.R --step ... and are now guarded by
test-skill-conventions.R against drift back to the legacy layout.
Updates the stale "will be revised in a follow-up pass" paragraph.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 2: Per-sex `team_names` schema (TDD, single commit)

**Files:**
- Modify: `tests/testthat/test-placer-validate.R` (test fixtures + new tests)
- Modify: `config/leagues.schema.json` (`team_names` definition)
- Modify: `config/leagues.yml` (3 leagues' `team_names` blocks)
- Modify: `R/placer-validate.R` (`validate_team_names_config`)
- Modify: `R/placer-pipeline.R` (per-sex pipeline_to_lengjan map, lines ~132-138 and ~149-156)

### RED: write failing tests first

- [ ] **Step 2.1: Replace `tests/testthat/test-placer-validate.R` with the new test set**

Overwrite the entire file with:

```r
# Existing 6 cases migrated to nested team_names shape, plus 4 new cases
# for per-sex behaviour.

test_that("validate_team_names_config passes when all recs have a team_names entry", {
  leagues <- list(
    football_iceland = list(
      sport = "football", country = "iceland",
      lengjan = list(team_names = list(
        male = list("KR" = "KR Reykjavik"),
        female = list()
      ))
    )
  )
  recs <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male",
    home_team = "KR", away_team = "KR"
  )
  expect_invisible(validate_team_names_config(leagues, recs))
})

test_that("validate_team_names_config errors when a league lacks team_names", {
  leagues <- list(
    football_iceland = list(
      sport = "football", country = "iceland",
      lengjan = list() # no team_names key
    )
  )
  recs <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male",
    home_team = "KR", away_team = "FH"
  )
  expect_error(
    validate_team_names_config(leagues, recs),
    "team_names"
  )
})

test_that("validate_team_names_config errors when a recommended team is unmapped", {
  leagues <- list(
    football_iceland = list(
      sport = "football", country = "iceland",
      lengjan = list(team_names = list(
        male = list("KR" = "KR Reykjavik"),
        female = list()
      ))
    )
  )
  recs <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male",
    home_team = "KR", away_team = "Mystery FC"
  )
  expect_error(
    validate_team_names_config(leagues, recs),
    "Mystery FC"
  )
})

test_that("validate_recommendations_schema accepts the canonical column set", {
  recs <- tibble::tibble(
    run_id = Sys.time(), sport = "football", country = "iceland", sex = "male",
    match_date = as.Date("2026-04-25"),
    home_team = "KR", away_team = "FH",
    market = "moneyline", outcome = "home", line = NA_real_,
    p = 0.55, odds = 1.85, ev = 0.02, kelly = 0.05, bet_amount = 200
  )
  expect_invisible(validate_recommendations_schema(recs))
})

test_that("validate_recommendations_schema errors on a missing column", {
  recs <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male"
  )
  expect_error(
    validate_recommendations_schema(recs),
    "missing|column"
  )
})

test_that("validate_team_names_config errors when league key is absent from leagues.yml", {
  leagues <- list()
  recs <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male",
    home_team = "KR", away_team = "FH"
  )
  expect_error(
    validate_team_names_config(leagues, recs),
    "football_iceland"
  )
})

test_that("validate_team_names_config errors loudly on non-list leagues input", {
  recs <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male",
    home_team = "KR", away_team = "FH"
  )
  expect_error(
    validate_team_names_config("not a list", recs),
    "named list"
  )
})

test_that("validate_team_names_config errors loudly on recs missing core columns", {
  leagues <- list(
    football_iceland = list(
      sport = "football", country = "iceland",
      lengjan = list(team_names = list(
        male = list("KR" = "KR"), female = list()
      ))
    )
  )
  bad_recs <- tibble::tibble(sport = "football", country = "iceland")
  expect_error(
    validate_team_names_config(leagues, bad_recs),
    "missing column"
  )
})

# ---------- New per-sex behaviour ----------

test_that("validate_team_names_config errors when the rec's sex sub-map is empty", {
  leagues <- list(
    football_iceland = list(
      sport = "football", country = "iceland",
      lengjan = list(team_names = list(
        male = list("KR" = "KR"),
        female = list()
      ))
    )
  )
  recs <- tibble::tibble(
    sport = "football", country = "iceland", sex = "female",
    home_team = "Fram", away_team = "Stjarnan"
  )
  expect_error(
    validate_team_names_config(leagues, recs),
    "empty.*sub-map|female|data/facts/odds"
  )
})

test_that("validate_team_names_config does not satisfy a male rec from the female sub-map", {
  leagues <- list(
    football_iceland = list(
      sport = "football", country = "iceland",
      lengjan = list(team_names = list(
        male = list(),
        female = list("Fram" = "Fram kv")
      ))
    )
  )
  recs <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male",
    home_team = "Fram", away_team = "KR"
  )
  expect_error(
    validate_team_names_config(leagues, recs),
    "male|empty"
  )
})

test_that("validate_team_names_config errors when team_names lacks the rec's sex key entirely", {
  leagues <- list(
    football_iceland = list(
      sport = "football", country = "iceland",
      lengjan = list(team_names = list(
        male = list("KR" = "KR")
        # no female key at all
      ))
    )
  )
  recs <- tibble::tibble(
    sport = "football", country = "iceland", sex = "female",
    home_team = "Fram", away_team = "Stjarnan"
  )
  expect_error(
    validate_team_names_config(leagues, recs),
    "female|sub-map"
  )
})

test_that("validate_team_names_config errors with a clear message when rec$sex is unknown", {
  leagues <- list(
    football_iceland = list(
      sport = "football", country = "iceland",
      lengjan = list(team_names = list(
        male = list("KR" = "KR"),
        female = list()
      ))
    )
  )
  recs <- tibble::tibble(
    sport = "football", country = "iceland", sex = "all",
    home_team = "KR", away_team = "FH"
  )
  expect_error(
    validate_team_names_config(leagues, recs),
    "invalid sex|sex value"
  )
})
```

- [ ] **Step 2.2: Run the test file — expect failures**

Run:
```bash
Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-placer-validate.R")'
```

Expected: at least the 4 NEW tests fail (because `validate_team_names_config` still uses the flat schema). The existing tests may still pass because the wrapper `tn <- league$lengjan$team_names` returns a named list either way; it's the "find KR in `tn`" logic that fails. Either way, do not commit yet.

### GREEN: schema, config, validator, pipeline (in dependency order)

- [ ] **Step 2.3: Update `config/leagues.schema.json`**

Replace the existing `team_names` block:

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

- [ ] **Step 2.4: Rewrite `config/leagues.yml` `team_names` blocks**

For `basketball_iceland.lengjan.team_names` (lines 25-33), replace:

```yaml
    team_names:
      Grindavík: Grindavík kv
      Valur: Valur kv
      Njarðvík: Njarðvík kv
      Haukar: Haukar kv
      Keflavík: Keflavík kv
      Tindastóll: UMF Tindastoll kv
      Ármann: Ármann kv
      Stjarnan: Stjarnan kv
```

With:

```yaml
    team_names:
      male: {}                         # No men's BD on Lengjan currently
      female:
        Grindavík: Grindavík kv
        Valur: Valur kv
        Njarðvík: Njarðvík kv
        Haukar: Haukar kv
        Keflavík: Keflavík kv
        Tindastóll: UMF Tindastoll kv
        Ármann: Ármann kv
        Stjarnan: Stjarnan kv
```

For `handball_iceland.lengjan.team_names` (lines 60-68), replace:

```yaml
    team_names:
      Þór: Þór Akureyri
      HK: Handknattleiksfélag Kópavogs
      ÍBV: ÍBV Vestmannaeyjar
      ÍR: ÍR Reykjavík
      FH: FH Hafnarfjörður
      KA: KA Akureyri
      ÍH: ÍH Keflavík
      HBH: HB Hafnarfjörður
```

With:

```yaml
    team_names:
      male:
        Þór: Þór Akureyri
        HK: Handknattleiksfélag Kópavogs
        ÍBV: ÍBV Vestmannaeyjar
        ÍR: ÍR Reykjavík
        FH: FH Hafnarfjörður
        KA: KA Akureyri
        ÍH: ÍH Keflavík
        HBH: HB Hafnarfjörður
      female: {}                       # No women's handball on Lengjan currently
```

For `football_iceland.lengjan.team_names` (lines 99-116), replace the **entire block including the multi-line comment** (the comment documents the gap that this change closes — drop it):

```yaml
    # Note: team_names is currently shared across sexes within a league.
    # Teams that appear in both male and female schedules (e.g. Fram, FH,
    # ÍBV, Valur, Stjarnan, Víkingur R., Breiðablik) cannot be mapped here
    # because Lengjan suffixes the women's display ("Fram kv", etc.) while
    # the men's display omits the suffix — a single key cannot represent
    # both. Mappings below are for teams that appear in the men's LD1 only,
    # i.e. men-only across the 2026 women's schedule.
    team_names:
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
```

With:

```yaml
    team_names:
      male:
        # Existing LD1 + men-only sex-disjoint entries:
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
        # BD teams that also appear in women's BD (verified against
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

- [ ] **Step 2.5: Verify the YAML loads cleanly under the new schema**

Run:
```bash
Rscript -e '
suppressMessages(devtools::load_all(quiet = TRUE))
leagues <- load_leagues()
str(leagues$football_iceland$lengjan$team_names, max.level = 2)
str(leagues$basketball_iceland$lengjan$team_names, max.level = 2)
str(leagues$handball_iceland$lengjan$team_names, max.level = 2)
'
```

Expected: each prints a `List of 2` with `$ male:` and `$ female:` sub-lists.

If `load_leagues()` runs schema validation (check `R/config.R`), it should pass. If it fails, fix the YAML/JSON Schema to match.

- [ ] **Step 2.6: Update `R/placer-validate.R::validate_team_names_config`**

Replace the entire function (lines 17-86) with:

```r
validate_team_names_config <- function(leagues, recs) {
  if (!is.list(leagues)) {
    stop(
      "validate_team_names_config: `leagues` must be a named list ",
      "from load_leagues()",
      call. = FALSE
    )
  }
  needed_cols <- c("sport", "country", "sex", "home_team", "away_team")
  missing_input <- setdiff(needed_cols, names(recs))
  if (length(missing_input) > 0L) {
    stop(
      "validate_team_names_config: `recs` missing column(s): ",
      paste(missing_input, collapse = ", "),
      call. = FALSE
    )
  }

  valid_sexes <- c("male", "female")
  bad_sex <- setdiff(unique(recs$sex), valid_sexes)
  if (length(bad_sex) > 0L) {
    stop(
      "validate_team_names_config: rec(s) have invalid sex value(s): ",
      paste(bad_sex, collapse = ", "),
      ". Expected one of: ", paste(valid_sexes, collapse = ", "),
      call. = FALSE
    )
  }

  groups <- unique(recs[, c("sport", "country", "sex"), drop = FALSE])
  for (i in seq_len(nrow(groups))) {
    sp <- groups$sport[i]
    co <- groups$country[i]
    sx <- groups$sex[i]
    key <- paste0(sp, "_", co)

    if (!key %in% names(leagues)) {
      stop(
        "validate_team_names_config: no leagues.yml entry for ", key,
        call. = FALSE
      )
    }

    league <- leagues[[key]]
    tn_all <- league$lengjan$team_names

    if (is.null(tn_all) || length(tn_all) == 0L) {
      stop(
        "validate_team_names_config: ", key,
        " has no lengjan$team_names. Add ",
        "team_names: {male: {...}, female: {...}} to config/leagues.yml.",
        call. = FALSE
      )
    }

    if (!sx %in% names(tn_all)) {
      stop(
        "validate_team_names_config: ", key,
        " is missing the `", sx, "` sub-map under lengjan.team_names. ",
        "Both `male` and `female` keys are required ",
        "(use `{}` for an empty sub-map).",
        call. = FALSE
      )
    }

    tn <- tn_all[[sx]]

    rows <- recs[
      recs$sport == sp & recs$country == co & recs$sex == sx, ,
      drop = FALSE
    ]
    teams <- unique(c(rows$home_team, rows$away_team))

    if (length(tn) == 0L) {
      stop(
        "validate_team_names_config: ", key, " (", sx, ") ",
        "has an empty team_names sub-map. Add canonical -> Lengjan-display ",
        "mappings under lengjan.team_names.", sx,
        " (source: data/facts/odds/sport=", sp, "/country=", co,
        " or wait for the next scrape). Missing teams: ",
        paste(teams, collapse = ", "),
        call. = FALSE
      )
    }

    missing_teams <- setdiff(teams, names(tn))

    if (length(missing_teams) > 0L) {
      stop(
        "validate_team_names_config: ", key, " (", sx, ") ",
        "is missing team_names for: ",
        paste(missing_teams, collapse = ", "),
        call. = FALSE
      )
    }
  }

  invisible(TRUE)
}
```

Also update the roxygen `@param recs` line to mention `sex` is required:

Replace:
```r
#' @param recs Recommendations tibble.  Must contain \code{sport},
#'   \code{country}, \code{home_team}, and \code{away_team} columns.
```

With:
```r
#' @param recs Recommendations tibble.  Must contain \code{sport},
#'   \code{country}, \code{sex}, \code{home_team}, and \code{away_team}
#'   columns.  \code{sex} must be one of \code{"male"} or \code{"female"};
#'   the team_names lookup is sex-keyed.
```

- [ ] **Step 2.7: Update `R/placer-pipeline.R` lines 132-156 for per-sex pipeline_to_lengjan**

Replace the existing block (lines 132-138):

```r
    # Build pipeline -> Lengjan name map (direction: canonical -> Lengjan display)
    tn <- league$lengjan$team_names # list: pipeline_name = "Lengjan display"
    pipeline_to_lengjan <- if (!is.null(tn) && length(tn) > 0L) {
      stats::setNames(as.character(unlist(tn)), names(tn))
    } else {
      character(0)
    }
```

With:

```r
    # Build per-sex pipeline -> Lengjan name maps.
    # team_names has shape list(male = list(...), female = list(...)); each
    # sub-map is canonical-pipeline-name -> Lengjan-display-name.
    tn_all <- league$lengjan$team_names
    pipeline_to_lengjan_by_sex <- lapply(tn_all, function(sex_map) {
      if (length(sex_map) == 0L) return(character(0))
      stats::setNames(as.character(unlist(sex_map)), names(sex_map))
    })
```

The `resolve_match_ids_new` call at lines 141-146 currently passes `pipeline_to_lengjan` but the function does not use it (verified — `pipeline_to_lengjan` does not appear in the function body lines 280-313). Pass an empty map to keep the signature stable:

Replace lines 141-146:
```r
    match_ids <- resolve_match_ids_new(
      session = session,
      league = league,
      sport_id = sport_id,
      pipeline_to_lengjan = pipeline_to_lengjan
    )
```

With:
```r
    match_ids <- resolve_match_ids_new(
      session = session,
      league = league,
      sport_id = sport_id,
      pipeline_to_lengjan = character(0)  # kept for API stability; unused inside
    )
```

In the per-bet loop (lines 149-156), replace:

```r
    # Place each bet
    for (i in seq_len(nrow(league_recs))) {
      bet <- league_recs[i, ]

      home_l <- pipeline_to_lengjan[[bet$home_team]]
      if (is.null(home_l) || is.na(home_l)) home_l <- bet$home_team
      away_l <- pipeline_to_lengjan[[bet$away_team]]
      if (is.null(away_l) || is.na(away_l)) away_l <- bet$away_team
```

With:

```r
    # Place each bet
    for (i in seq_len(nrow(league_recs))) {
      bet <- league_recs[i, ]
      pipeline_to_lengjan <- pipeline_to_lengjan_by_sex[[bet$sex]] %||%
        character(0)

      home_l <- pipeline_to_lengjan[[bet$home_team]]
      if (is.null(home_l) || is.na(home_l)) home_l <- bet$home_team
      away_l <- pipeline_to_lengjan[[bet$away_team]]
      if (is.null(away_l) || is.na(away_l)) away_l <- bet$away_team
```

(`%||%` is already defined at the bottom of `R/placer-pipeline.R`.)

- [ ] **Step 2.8: Re-run the validator tests — expect all green**

Run:
```bash
Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test-placer-validate.R")'
```

Expected: all 12 tests pass (8 existing + 4 new), 0 failures, 0 warnings (other than pre-existing locale chatter).

If any tests fail, debug. Likely cause: error-message regex mismatches. Fix the regex in the test or the wording in the validator until all pass.

- [ ] **Step 2.9: Run the full test suite**

Run:
```bash
Rscript -e 'devtools::test()' 2>&1 | tail -20
```

Expected: `[ FAIL 0 | WARN ≤10 | SKIP 10 | PASS ≥532 ]` (528 prior + 4 new). Pre-existing locale warnings are fine.

If FAIL > 0, identify whether the failures are caused by:
(a) Other tests touching `team_names` indirectly (via `load_leagues()` or schema validation) — in which case fix the test fixture;
(b) Genuine regression — in which case fix the code.

- [ ] **Step 2.10: Smoke test — `preview_bets.R` against current male BD recs**

Run:
```bash
Rscript scripts/preview_bets.R 2>&1 | head -40
```

Expected: the 3 male BD recs (Fram-ÍBV, KR-FH, Stjarnan-Valur) pass `validate_team_names_config()` and show up in the preview output. Pre-change all three would fail with `is missing team_names for: KR, FH, Stjarnan, Valur, Fram, ÍBV`.

If the preview reports `no_match_id` errors at this stage, that's expected (we haven't hit Lengjan to verify match resolution — the smoke is about the validator, not the placer). The success criterion is: validator does not abort.

- [ ] **Step 2.11: Document changes — `devtools::document()`**

Run:
```bash
Rscript -e 'devtools::document()'
```

Expected: regenerates `man/*.Rd` for the updated `validate_team_names_config` roxygen block. Stage any updated `.Rd` files.

- [ ] **Step 2.12: Commit Task 2**

```bash
git add config/leagues.schema.json config/leagues.yml \
  R/placer-validate.R R/placer-pipeline.R \
  tests/testthat/test-placer-validate.R \
  man/validate_team_names_config.Rd
git commit -m "$(cat <<'EOF'
feat: per-sex team_names schema — unblock women's leagues + male BD

Previously team_names was a single sex-agnostic map (canonical ->
Lengjan display). This could not represent Lengjan's `kv` suffix on
women's teams (Fram vs Fram kv), so any team appearing in both sexes
silently broke the lookup.

New shape:
  team_names:
    male:   { canonical: lengjan_display, ... }
    female: { canonical: lengjan_display, ... }

Both keys required at the schema level; either may be {}.

Touches:
- config/leagues.schema.json (nested team_names definition)
- config/leagues.yml (3 leagues' blocks; football_iceland.male
  gains 10 new BD entries: KR, FH, Stjarnan, Valur, Fram, ÍBV,
  Breiðablik, Keflavík, ÍA, KA — sourced from data/facts/odds)
- R/placer-validate.R (per-sex sex-keyed lookup; explicit reject
  of unknown sex values; sex column now required on recs)
- R/placer-pipeline.R (per-sex pipeline_to_lengjan map; passes
  empty map to resolve_match_ids_new which does not use it)
- tests/testthat/test-placer-validate.R (8 existing tests
  migrated to nested fixtures; 4 new tests for per-sex behaviour)

Closes the post-Plan-6 P2 finding noted in
Audits/2026-04-25 Monorepo Audit.md and the team_names schema
TODO from MEMORY/project_team_names_schema.md.

Spec: docs/superpowers/specs/2026-04-26-per-sex-team-names-design.md

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Self-review checklist (executor: do this after both tasks complete)

- [ ] All 12 validator tests green (`testthat::test_file("tests/testthat/test-placer-validate.R")`)
- [ ] Full suite green (`devtools::test()` shows `FAIL 0`)
- [ ] `preview_bets.R` no longer aborts at `validate_team_names_config()` for male BD recs
- [ ] `git status` clean (no stray files)
- [ ] Two commits visible in `git log --oneline -5` (CLAUDE.md docs commit, then per-sex feat commit)
- [ ] No edits to `_legacy/` (this work is monorepo-only)

## Out-of-scope, deliberately not done

- Filling `football_iceland.female` and `handball_iceland.female` sub-maps (will accrete from scraper)
- Dropping the dead `pipeline_to_lengjan` parameter from `resolve_match_ids_new` (separate cleanup; out of scope)
- Stan parameter data-stories (in-flight by user; separate work)
- Lengjan SPA `loadEventFired` performance investigation
- Per-rec `sex` validation in `R/decide-pipeline.R` (separate PR; defence in depth)
- Updating `Knowledge/Lengjan Pipeline/_MOC.md` to document the per-sex schema (Obsidian doc sweep, separate from code)
