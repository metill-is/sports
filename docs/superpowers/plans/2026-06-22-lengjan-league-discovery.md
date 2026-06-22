# Lengjan League Discovery — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a read-only, CI-scheduled watcher that reads Lengjan's competition dropdowns, diffs them against `config/leagues.yml`, and emits a reviewable proposal (drafted competition entry + best-guess `team_names`) the moment Lengjan lists a league we already model but do not yet scrape — surfaced via a health-check WARN.

**Architecture:** A new pure-where-possible engine (`R/discover-lengjan.R`) parses the "Veldu deild" `<select>` on Lengjan's parent page (option `value` = competition ID), classifies each competition's `(sex, division)` by name, flags whether we model it, drafts `team_names` by fuzzy-matching the live team renderings to our canonical names, and writes `data/discovery/proposals.json` (+ `SUMMARY.md`). A thin entry script (`scripts/0N_discover.R`) and a scheduled workflow (`discover-leagues.yml`) run it daily and commit only `data/discovery/**`. A new `check_discovery()` health row WARNs on un-actioned proposals, keyed off the live config so it self-clears on merge. A `/wire-league` skill turns a proposal into a reviewed `leagues.yml` edit.

**Tech Stack:** R package (`devtools`/`testthat` edition 3), `chromote` + `rvest` (same as the odds scraper), `jsonlite` via `write_json_consistent()`, GitHub Actions.

## Global Constraints

- **R package conventions:** base pipe `|>`; explicit namespacing in package code (`rvest::html_elements`); roxygen `@export`/`@noRd` on every function; `testthat` edition 3; run `devtools::document()` after any export change and `devtools::test()` before claiming a task done.
- **Non-ASCII:** Icelandic characters in R **source** use `\uXXXX` escapes (R CMD check). Any test/source file containing Icelandic string literals is written **via Python** (the Write tool corrupts non-ASCII).
- **Read-only on the money path:** discovery never reads or writes the ledger, never logs in, never imports a placer symbol, and references no `LENGJAN_*` env var. Enforced by a new isolation test.
- **CI conventions (`.claude/rules/ci-conventions.md`):** every R workflow sets `PKG_SYSREQS: "false"` on `setup-r-dependencies@v2` and rebuilds V8 from source with `DOWNLOAD_STATIC_LIBV8: "1"`; workflow `name:` fields contain no glob meta-characters (`+ * ? [ ] !`); the new workflow writes only the disjoint path `data/discovery/**` (no cross-workflow git race).
- **Health row schema:** `health_row(check, scope, status, value, threshold)`; statuses ordered `OK` < `WARN` < `FAIL` (plus `PAUSED`). The discovery check returns `WARN` at most (a new league is not an outage; email fires on `FAIL` only).
- **JSON:** always via `write_json_consistent(x, path, pretty = TRUE, auto_unbox = TRUE)` (UTF-8 safe). Never hand-write JSON.
- **Lengjan URL:** `https://games.lotto.is/getraunaleikir/lengjan?sport=%d&country=%s` for the parent listing, `&competition=%s` appended for one competition. Sport IDs via `.lengjan_sport_id()` (football=1, basketball=2, handball=6); country via `.lengjan_country_code()` (iceland="IS"). Both live in `R/ingest-lengjan-odds.R` and are usable package-internally.
- **Reused primitives (already in `R/ingest-lengjan-odds.R`):** `.lengjan_fetch(session, url, ...)` (navigate + settle + retry/soft-fail), `parse_competition_page(html, sport, country)` (one row per moneyline outcome with Lengjan-display `home_team`/`away_team`), `%||%`.

---

## File structure

| File | New/Modify | Responsibility |
|---|---|---|
| `R/discover-lengjan.R` | Create | Discovery engine: dropdown parse, classification, team-name matching, orchestration, proposal writer |
| `tests/testthat/fixtures/lengjan-parent-page.html` | Create | Saved real parent-page HTML for the dropdown-parser test |
| `tests/testthat/test-discover-lengjan.R` | Create | Unit tests for every engine function |
| `tests/testthat/test-discover-ci-isolation.R` | Create | Asserts the engine + entry script touch no ledger/placer/creds |
| `scripts/0N_discover.R` | Create | Entry point: open session → discover → write proposal |
| `.github/workflows/discover-leagues.yml` | Create | Daily scheduled run; commits `data/discovery/**` |
| `R/health.R` | Modify | Add `check_discovery()`; wire into `pipeline_health()` |
| `tests/testthat/test-healthcheck.R` (or new `test-discover-health.R`) | Create/Modify | Test `check_discovery()` WARN/OK |
| `.claude/skills/wire-league/SKILL.md` | Create | Reaction flow: proposal → reviewed `leagues.yml` edit |
| `CLAUDE.md`, `.claude/rules/ci-conventions.md` | Modify | Document the new workflow + script |

> **Note on `unmodelled_offered_count`:** v1 counts only competitions Lengjan offers *within the (sport, country) pairs we already model* that we do not model (e.g. men's 4. deild). Enumerating foreign-country / other-sport Tier-B competitions is future work (spec §11).

---

### Task 1: Dropdown parser (`parse_competition_dropdown`)

**Files:**
- Create: `R/discover-lengjan.R`
- Create (fixture): `tests/testthat/fixtures/lengjan-parent-page.html`
- Test: `tests/testthat/test-discover-lengjan.R`

**Interfaces:**
- Produces: `parse_competition_dropdown(html) -> tibble(comp_id: chr, lengjan_name: chr)`. Pure; reads the `<select>` whose placeholder option text is `"Veldu deild"` and returns its non-placeholder `<option>` `value`/text pairs.

- [ ] **Step 1: Capture the fixture**

Save a real parent page to the fixture path (run once, locally). This is captured data, not test logic, so a short ad-hoc script is fine:

```r
# scratch (not committed): capture the dropdown fixture
local({
  s <- chromote::ChromoteSession$new(); on.exit(s$close())
  try(s$default_timeout <- 30, silent = TRUE)
  s$Page$navigate("https://games.lotto.is/getraunaleikir/lengjan?sport=1&country=IS")
  Sys.sleep(6)
  html <- s$Runtime$evaluate("document.documentElement.outerHTML")$result$value
  writeLines(html, "tests/testthat/fixtures/lengjan-parent-page.html", useBytes = TRUE)
})
```

Verify the fixture contains a `<select>` with `<option value="746">Besta deild karla</option>` (or whatever competitions are live at capture time) and a placeholder `<option>Veldu deild</option>`.

- [ ] **Step 2: Write the failing test**

The fixture has Icelandic option text, so this test file is written **via Python**. Create `tests/testthat/test-discover-lengjan.R` with:

```r
test_that("parse_competition_dropdown extracts comp_id + name from the league select", {
  html <- rvest::read_html(testthat::test_path("fixtures", "lengjan-parent-page.html"))
  comps <- parse_competition_dropdown(html)

  expect_s3_class(comps, "tbl_df")
  expect_named(comps, c("comp_id", "lengjan_name"))
  expect_true(nrow(comps) >= 1L)
  expect_true(all(nzchar(comps$comp_id)))          # placeholder ("" value) dropped
  expect_false(any(comps$lengjan_name == "Veldu deild"))
  expect_true("746" %in% comps$comp_id)            # Besta deild karla, present at capture
})

test_that("parse_competition_dropdown returns empty tibble when no league select present", {
  html <- rvest::read_html("<html><body><p>no selects</p></body></html>")
  comps <- parse_competition_dropdown(html)
  expect_named(comps, c("comp_id", "lengjan_name"))
  expect_equal(nrow(comps), 0L)
})
```

- [ ] **Step 3: Run the test, verify it fails**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-discover-lengjan.R")'`
Expected: FAIL — `could not find function "parse_competition_dropdown"`.

- [ ] **Step 4: Write the implementation**

Create `R/discover-lengjan.R` with the file header and the parser. No non-ASCII string literals here except the ASCII `"Veldu deild"`, so this file may be written with the normal editor:

```r
# R/discover-lengjan.R
#' @include ingest-lengjan-odds.R storage.R config.R
NULL

#' Parse the Lengjan "Veldu deild" competition dropdown.
#'
#' The parent listing page (no `competition=` query param) renders three
#' `<select>`s: sport, country, and league ("Veldu deild"). The league select's
#' `<option value>` is the competition ID and the text is Lengjan's display name.
#' We identify the league select by its placeholder option text rather than
#' position, so a layout reorder does not silently pick the wrong dropdown.
#'
#' @param html Parsed HTML (from `rvest::read_html()`).
#' @return Tibble `{comp_id, lengjan_name}`; empty-with-columns if absent.
#' @export
parse_competition_dropdown <- function(html) {
  empty <- tibble::tibble(comp_id = character(0), lengjan_name = character(0))
  selects <- rvest::html_elements(html, "select")
  if (length(selects) == 0L) {
    return(empty)
  }
  for (sel in selects) {
    opts <- rvest::html_elements(sel, "option")
    if (length(opts) == 0L) next
    txt <- rvest::html_text2(opts)
    Encoding(txt) <- "UTF-8"
    if (!identical(trimws(txt[[1L]]), "Veldu deild")) next
    vals <- rvest::html_attr(opts, "value")
    keep <- !is.na(vals) & nzchar(vals)
    return(tibble::tibble(
      comp_id = as.character(vals[keep]),
      lengjan_name = trimws(txt[keep])
    ))
  }
  empty
}
```

- [ ] **Step 5: Run the test, verify it passes**

Run: `Rscript -e 'devtools::document(); devtools::load_all(); testthat::test_file("tests/testthat/test-discover-lengjan.R")'`
Expected: PASS (both tests).

- [ ] **Step 6: Commit**

```bash
git add R/discover-lengjan.R tests/testthat/test-discover-lengjan.R \
        tests/testthat/fixtures/lengjan-parent-page.html NAMESPACE
git commit -m "feat(discover): parse Lengjan competition dropdown"
```

---

### Task 2: Competition classifier (`classify_competition`)

**Files:**
- Modify: `R/discover-lengjan.R`
- Test: `tests/testthat/test-discover-lengjan.R`

**Interfaces:**
- Produces: `classify_competition(lengjan_name, sport, country) -> tibble(sex: chr, division: chr, confidence: chr)`. Pure. `sex` ∈ {male, female}; `division` ∈ {BD, LD1, LD2, LD3, LD4, CUP} or `NA`; `confidence` ∈ {high, low}.

- [ ] **Step 1: Write the failing test** (append to `test-discover-lengjan.R`, via Python — Icelandic literals)

```r
test_that("classify_competition maps Icelandic names to (sex, division)", {
  expect_equal(classify_competition("Besta deild karla", "football", "iceland")[c("sex","division")],
               tibble::tibble(sex = "male", division = "BD"))
  expect_equal(classify_competition("Besta deild kvenna", "football", "iceland")[c("sex","division")],
               tibble::tibble(sex = "female", division = "BD"))
  expect_equal(classify_competition("Lengjudeildin", "football", "iceland")$division, "LD1")
  expect_equal(classify_competition("Lengjudeild kv.", "football", "iceland")$sex, "female")
  expect_equal(classify_competition("2. deild karla", "football", "iceland")$division, "LD2")
  expect_equal(classify_competition("3. deild karla", "football", "iceland")$division, "LD3")
  expect_equal(classify_competition("Mjólkurbikar kvenna", "football", "iceland")[c("sex","division")],
               tibble::tibble(sex = "female", division = "CUP"))
  expect_equal(classify_competition("Mjólkurbikar karla", "football", "iceland")$division, "CUP")
})

test_that("classify_competition flags unknown names low-confidence with NA division", {
  r <- classify_competition("Some New Playoff", "football", "iceland")
  expect_true(is.na(r$division))
  expect_equal(r$confidence, "low")
})
```

- [ ] **Step 2: Run, verify it fails**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-discover-lengjan.R")'`
Expected: FAIL — `could not find function "classify_competition"`.

- [ ] **Step 3: Implement** (append to `R/discover-lengjan.R`; non-ASCII patterns use `\uXXXX`)

```r
#' Classify a Lengjan competition name into (sex, division).
#'
#' Deterministic, advisory name match. Sex from a "kvenna"/"kv" marker;
#' division from the league-name pattern. A name that matches no division
#' pattern is `division = NA`, `confidence = "low"` -- surfaced for a human,
#' never auto-wired. Patterns are ASCII except the basketball "Bonusdeild" name,
#' written with a `ó` escape (R-source non-ASCII rule); the cup matches the
#' ASCII substring "bikar", so it needs no escape.
#'
#' @param lengjan_name Competition display name from the dropdown.
#' @param sport,country Pass-through context (reserved for sport-specific rules).
#' @return Tibble `{sex, division, confidence}` (one row).
#' @export
classify_competition <- function(lengjan_name, sport, country) {
  nm <- lengjan_name
  Encoding(nm) <- "UTF-8"
  female <- grepl("kvenna", nm, ignore.case = TRUE) ||
    grepl("(^| )kv\\.?( |$)", nm, ignore.case = TRUE)
  sex <- if (female) "female" else "male"

  division <- if (grepl("bikar", nm, ignore.case = TRUE)) {
    "CUP"
  } else if (grepl("3\\. *deild", nm, ignore.case = TRUE)) {
    "LD3"
  } else if (grepl("4\\. *deild", nm, ignore.case = TRUE)) {
    "LD4"
  } else if (grepl("2\\. *deild", nm, ignore.case = TRUE)) {
    "LD2"
  } else if (grepl("lengjudeild", nm, ignore.case = TRUE)) {
    "LD1"
  } else if (grepl("besta *deild|b\u00f3nusdeild", nm, ignore.case = TRUE)) {
    "BD"
  } else {
    NA_character_
  }
  tibble::tibble(
    sex = sex,
    division = division,
    confidence = if (is.na(division)) "low" else "high"
  )
}
```

- [ ] **Step 4: Run, verify it passes**

Run: `Rscript -e 'devtools::document(); devtools::load_all(); testthat::test_file("tests/testthat/test-discover-lengjan.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/discover-lengjan.R tests/testthat/test-discover-lengjan.R NAMESPACE
git commit -m "feat(discover): classify competition name to (sex, division)"
```

---

### Task 3: Team-name matcher (`match_team_names`)

**Files:**
- Modify: `R/discover-lengjan.R`
- Test: `tests/testthat/test-discover-lengjan.R`

**Interfaces:**
- Produces: `match_team_names(renderings, known_teams) -> tibble(lengjan: chr, canonical_guess: chr, confidence: chr)`. Pure. Exact normalised match → "high"; Levenshtein distance ≤ 2 → "medium"; else `canonical_guess = NA`, "low".
- Internal helper `.norm_team(x)` (normalisation used by the matcher).

- [ ] **Step 1: Write the failing test** (append, via Python — Icelandic literals)

```r
test_that("match_team_names matches Lengjan renderings to canonical names", {
  known <- c("FH", "Víkingur R.", "Valur", "Þróttur R.")
  out <- match_team_names(
    c("FH kv", "Víkingur Rvk kv", "Þróttur Rvk kv", "Qwerty United"),
    known
  )
  expect_named(out, c("lengjan", "canonical_guess", "confidence"))
  expect_equal(out$canonical_guess[out$lengjan == "FH kv"], "FH")
  expect_equal(out$canonical_guess[out$lengjan == "Víkingur Rvk kv"], "Víkingur R.")
  expect_equal(out$confidence[out$lengjan == "FH kv"], "high")
  expect_true(is.na(out$canonical_guess[out$lengjan == "Qwerty United"]))
  expect_equal(out$confidence[out$lengjan == "Qwerty United"], "low")
})

test_that("match_team_names handles an empty rendering set", {
  out <- match_team_names(character(0), c("FH"))
  expect_equal(nrow(out), 0L)
  expect_named(out, c("lengjan", "canonical_guess", "confidence"))
})
```

- [ ] **Step 2: Run, verify it fails**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-discover-lengjan.R")'`
Expected: FAIL — `could not find function "match_team_names"`.

- [ ] **Step 3: Implement** (append to `R/discover-lengjan.R`)

```r
#' Normalise a team name for fuzzy comparison (comparison key only).
#'
#' Lowercases, strips a trailing women's marker (" kv"/" kv."), transliterates
#' diacritics to ASCII, removes dots, collapses whitespace, and folds the common
#' "Rvk" -> "r" and (post-transliteration) "ol" -> "o" abbreviations so
#' "Víkingur Rvk kv" and "Víkingur R." collide. The original canonical
#' string is always what gets emitted -- this key is never shown.
#' @noRd
.norm_team <- function(x) {
  x <- tolower(trimws(x))
  x <- sub("\\s*kv\\.?$", "", x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- gsub("\\.", "", x)
  x <- gsub("\\brvk\\b", "r", x)
  x <- gsub("\\bol\\b", "o", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

#' Fuzzy-match Lengjan team renderings to our canonical team names.
#'
#' Exact normalised match -> "high"; nearest within Levenshtein distance 2 ->
#' "medium"; otherwise `canonical_guess = NA`, "low". Low/medium guesses are
#' fail-safe: a wrong `team_names` entry makes `decide_league` warn-skip the
#' match, never mis-bet (existing normaliser invariant).
#'
#' @param renderings Character vector of Lengjan display names.
#' @param known_teams Character vector of canonical (federation) team names.
#' @return Tibble `{lengjan, canonical_guess, confidence}`.
#' @export
match_team_names <- function(renderings, known_teams) {
  empty <- tibble::tibble(
    lengjan = character(0), canonical_guess = character(0), confidence = character(0)
  )
  if (length(renderings) == 0L) {
    return(empty)
  }
  kn <- unique(known_teams)
  if (length(kn) == 0L) {
    return(tibble::tibble(
      lengjan = renderings, canonical_guess = NA_character_, confidence = "low"
    ))
  }
  kn_norm <- vapply(kn, .norm_team, character(1))
  rows <- lapply(renderings, function(r) {
    rn <- .norm_team(r)
    hit <- which(kn_norm == rn)
    if (length(hit) >= 1L) {
      return(tibble::tibble(lengjan = r, canonical_guess = kn[[hit[[1L]]]], confidence = "high"))
    }
    d <- utils::adist(rn, kn_norm)[1L, ]
    j <- which.min(d)
    if (length(j) == 1L && is.finite(d[[j]]) && d[[j]] <= 2L) {
      tibble::tibble(lengjan = r, canonical_guess = kn[[j]], confidence = "medium")
    } else {
      tibble::tibble(lengjan = r, canonical_guess = NA_character_, confidence = "low")
    }
  })
  dplyr::bind_rows(rows)
}
```

- [ ] **Step 4: Run, verify it passes**

Run: `Rscript -e 'devtools::document(); devtools::load_all(); testthat::test_file("tests/testthat/test-discover-lengjan.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/discover-lengjan.R tests/testthat/test-discover-lengjan.R NAMESPACE
git commit -m "feat(discover): fuzzy-match Lengjan renderings to canonical names"
```

---

### Task 4: Live lister + discovery orchestrator

**Files:**
- Modify: `R/discover-lengjan.R`
- Test: `tests/testthat/test-discover-lengjan.R`

**Interfaces:**
- Consumes: `parse_competition_dropdown`, `classify_competition`, `match_team_names` (Tasks 1-3); `.lengjan_fetch`, `parse_competition_page`, `.lengjan_sport_id`, `.lengjan_country_code` (from `R/ingest-lengjan-odds.R`); `filter_leagues`, `read_table`, `%||%`.
- Produces:
  - `lengjan_list_competitions(sport, country, session) -> tibble(sport, country, comp_id, lengjan_name)`
  - `propose_team_names(comp_id, sport, country, sex, session, known_teams) -> tibble(lengjan, canonical_guess, confidence)`
  - `discover_new_competitions(leagues, session = NULL, root = here::here("data"), list_fn = NULL, team_names_fn = NULL) -> list(competitions = <list of finding lists>, unmodelled_offered_count = int)`. Each finding is a list: `{sport, country, comp_id, lengjan_name, inferred_sex, inferred_division, classify_confidence, modelled = TRUE, status = "new", proposed_team_names = <tibble>}`. `list_fn(sport, country)` and `team_names_fn(comp_id, sport, country, sex, division)` are injectable for tests.
  - Internal helpers `.configured_comp_ids(leagues, sport, country)`, `.modelled_division_codes(league, sex)`, `.known_teams_for(sport, country, sex, division, root)`.

- [ ] **Step 1: Write the failing test** (append, via Python — Icelandic literals). Uses injected `list_fn`/`team_names_fn` so no network/Stan is touched.

```r
test_that("discover_new_competitions flags a new modelled comp and skips configured + unmodelled ones", {
  leagues <- list(
    football_iceland = list(
      sport = "football", country = "iceland", active = TRUE,
      lengjan = list(competitions = list(list(id = "746", name = "Besta deild karla", sex = "male"))),
      publish_divisions = list(
        male = list(
          list(code = "BD"), list(code = "LD1"), list(code = "LD2"),
          list(code = "LD3"), list(code = "CUP")
        )
      )
    )
  )
  fake_list <- function(sport, country) tibble::tibble(
    sport = sport, country = country,
    comp_id = c("746", "757", "9999"),
    lengjan_name = c("Besta deild karla", "Lengjudeildin", "4. deild karla")
  )
  fake_tn <- function(comp_id, sport, country, sex, division) tibble::tibble(
    lengjan = "Some Team", canonical_guess = "Some Team", confidence = "high"
  )

  res <- discover_new_competitions(leagues, list_fn = fake_list, team_names_fn = fake_tn)

  expect_length(res$competitions, 1L)                       # 757 only
  f <- res$competitions[[1L]]
  expect_equal(f$comp_id, "757")
  expect_equal(f$inferred_division, "LD1")
  expect_true(f$modelled)
  expect_equal(res$unmodelled_offered_count, 1L)            # 4. deild (LD4) not modelled
  expect_s3_class(f$proposed_team_names, "tbl_df")
})
```

- [ ] **Step 2: Run, verify it fails**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-discover-lengjan.R")'`
Expected: FAIL — `could not find function "discover_new_competitions"`.

- [ ] **Step 3: Implement** (append to `R/discover-lengjan.R`; no non-ASCII literals)

```r
#' List every competition Lengjan currently offers for a (sport, country).
#' @param sport,country Canonical names ("football", "iceland").
#' @param session A live `chromote::ChromoteSession`.
#' @return Tibble `{sport, country, comp_id, lengjan_name}` (possibly empty).
#' @export
lengjan_list_competitions <- function(sport, country, session) {
  url <- sprintf(
    "https://games.lotto.is/getraunaleikir/lengjan?sport=%d&country=%s",
    .lengjan_sport_id(sport), .lengjan_country_code(country)
  )
  html <- .lengjan_fetch(session, url)
  comps <- parse_competition_dropdown(html)
  if (nrow(comps) == 0L) {
    return(tibble::tibble(
      sport = character(0), country = character(0),
      comp_id = character(0), lengjan_name = character(0)
    ))
  }
  comps$sport <- sport
  comps$country <- country
  comps[, c("sport", "country", "comp_id", "lengjan_name"), drop = FALSE]
}

#' Draft team_names for a competition by scraping its page once.
#' @param comp_id Lengjan competition ID.
#' @param sport,country,sex Context.
#' @param session A live `chromote::ChromoteSession`.
#' @param known_teams Canonical team names to match against.
#' @return Tibble `{lengjan, canonical_guess, confidence}`.
#' @export
propose_team_names <- function(comp_id, sport, country, sex, session, known_teams) {
  url <- sprintf(
    "https://games.lotto.is/getraunaleikir/lengjan?sport=%d&country=%s&competition=%s",
    .lengjan_sport_id(sport), .lengjan_country_code(country), comp_id
  )
  html <- .lengjan_fetch(session, url)
  rows <- parse_competition_page(html, sport = sport, country = country)
  renderings <- unique(c(rows$home_team, rows$away_team))
  renderings <- renderings[!is.na(renderings) & nzchar(renderings)]
  match_team_names(renderings, known_teams)
}

#' @noRd
.configured_comp_ids <- function(leagues, sport, country) {
  ids <- character(0)
  for (lg in leagues) {
    if (identical(lg$sport, sport) && identical(lg$country, country)) {
      for (cmp in lg$lengjan$competitions %||% list()) {
        ids <- c(ids, as.character(cmp$id))
      }
    }
  }
  unique(ids)
}

#' @noRd
.modelled_division_codes <- function(league, sex) {
  pd <- league$publish_divisions[[sex]]
  if (is.null(pd) || length(pd) == 0L) {
    return(NULL) # NULL = no division gating (single-division sport)
  }
  vapply(pd, function(d) d$code, character(1))
}

#' @noRd
.known_teams_for <- function(sport, country, sex, division, root) {
  res <- tryCatch(
    read_table("results", root = root,
      filter = list(sport = sport, country = country, sex = sex)
    ),
    error = function(e) tibble::tibble()
  )
  if (nrow(res) == 0L) {
    return(character(0))
  }
  if (!is.na(division) && "division" %in% names(res)) {
    res <- res[!is.na(res$division) & res$division == division, , drop = FALSE]
  }
  unique(c(res$home_team, res$away_team))
}

#' Discover competitions Lengjan now offers that we model but do not yet scrape.
#'
#' For each active modelled `(sport, country)`, lists the live competitions,
#' diffs against configured comp IDs, classifies each new one, keeps those whose
#' inferred division we model, and drafts `team_names`. Pure inputs are
#' injectable (`list_fn`, `team_names_fn`) so the orchestration is unit-tested
#' without network or Stan. Competitions for a modelled `(sport, country)` whose
#' division we do NOT model are counted in `unmodelled_offered_count`.
#'
#' @param leagues Full leagues list (`load_leagues()`).
#' @param session A live `chromote::ChromoteSession` (required unless both
#'   `list_fn` and `team_names_fn` are supplied).
#' @param root Data root for `read_table("results")`.
#' @param list_fn,team_names_fn Injectable closures for testing.
#' @return `list(competitions = <list>, unmodelled_offered_count = int)`.
#' @export
discover_new_competitions <- function(leagues, session = NULL,
                                      root = here::here("data"),
                                      list_fn = NULL, team_names_fn = NULL) {
  if (is.null(list_fn)) {
    stopifnot(!is.null(session))
    list_fn <- function(sport, country) lengjan_list_competitions(sport, country, session)
  }
  if (is.null(team_names_fn)) {
    stopifnot(!is.null(session))
    team_names_fn <- function(comp_id, sport, country, sex, division) {
      kt <- .known_teams_for(sport, country, sex, division, root)
      propose_team_names(comp_id, sport, country, sex, session, kt)
    }
  }

  active <- filter_leagues(leagues, active_only = TRUE, has_lengjan = TRUE)
  pairs <- list()
  for (key in names(active)) {
    lg <- active[[key]]
    pk <- paste(lg$sport, lg$country, sep = "\r")
    if (is.null(pairs[[pk]])) {
      pairs[[pk]] <- list(sport = lg$sport, country = lg$country, league = lg)
    }
  }

  findings <- list()
  unmodelled <- 0L
  for (p in pairs) {
    live <- tryCatch(list_fn(p$sport, p$country), error = function(e) NULL)
    if (is.null(live) || nrow(live) == 0L) next
    have <- .configured_comp_ids(leagues, p$sport, p$country)
    new <- live[!(as.character(live$comp_id) %in% have), , drop = FALSE]
    if (nrow(new) == 0L) next
    for (i in seq_len(nrow(new))) {
      cls <- classify_competition(new$lengjan_name[[i]], p$sport, p$country)
      codes <- .modelled_division_codes(p$league, cls$sex)
      modelled <- is.null(codes) || (!is.na(cls$division) && cls$division %in% codes)
      if (!modelled) {
        unmodelled <- unmodelled + 1L
        next
      }
      tn <- tryCatch(
        team_names_fn(new$comp_id[[i]], p$sport, p$country, cls$sex, cls$division),
        error = function(e) {
          tibble::tibble(
            lengjan = character(0), canonical_guess = character(0),
            confidence = character(0)
          )
        }
      )
      findings[[length(findings) + 1L]] <- list(
        sport = p$sport, country = p$country,
        comp_id = as.character(new$comp_id[[i]]),
        lengjan_name = new$lengjan_name[[i]],
        inferred_sex = cls$sex,
        inferred_division = cls$division,
        classify_confidence = cls$confidence,
        modelled = TRUE, status = "new",
        proposed_team_names = tn
      )
    }
  }
  list(competitions = findings, unmodelled_offered_count = unmodelled)
}
```

- [ ] **Step 4: Run, verify it passes**

Run: `Rscript -e 'devtools::document(); devtools::load_all(); testthat::test_file("tests/testthat/test-discover-lengjan.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/discover-lengjan.R tests/testthat/test-discover-lengjan.R NAMESPACE
git commit -m "feat(discover): live lister + new-competition orchestrator"
```

---

### Task 5: Proposal writer (`write_discovery_proposal` + `SUMMARY.md`)

**Files:**
- Modify: `R/discover-lengjan.R`
- Test: `tests/testthat/test-discover-lengjan.R`

**Interfaces:**
- Consumes: the `discover_new_competitions()` return shape; `write_json_consistent` (from `R/storage.R`).
- Produces: `write_discovery_proposal(findings, root = here::here("data"), now = Sys.time()) -> invisible(path)`. Writes `data/discovery/proposals.json` and `data/discovery/SUMMARY.md`. Internal `.write_discovery_summary(payload, path)`.

- [ ] **Step 1: Write the failing test** (append, via Python — Icelandic literals)

```r
test_that("write_discovery_proposal round-trips JSON with UTF-8 intact", {
  tmp <- withr::local_tempdir()
  findings <- list(
    competitions = list(list(
      sport = "football", country = "iceland", comp_id = "757",
      lengjan_name = "Lengjudeildin", inferred_sex = "male",
      inferred_division = "LD1", classify_confidence = "high",
      modelled = TRUE, status = "new",
      proposed_team_names = tibble::tibble(
        lengjan = c("Víkingur Ól.", "Zzz"),
        canonical_guess = c("Víkingur Ó.", NA_character_),
        confidence = c("high", "low")
      )
    )),
    unmodelled_offered_count = 2L
  )
  path <- write_discovery_proposal(findings, root = tmp)
  expect_true(file.exists(path))
  expect_true(file.exists(file.path(tmp, "discovery", "SUMMARY.md")))

  back <- jsonlite::read_json(path)
  expect_equal(length(back$competitions), 1L)
  expect_equal(back$competitions[[1]]$comp_id, "757")
  expect_equal(back$competitions[[1]]$proposed_team_names[[1]]$lengjan, "Víkingur Ól.")
  expect_equal(back$unmodelled_offered_count, 2L)
})

test_that("write_discovery_proposal writes an empty competitions array cleanly", {
  tmp <- withr::local_tempdir()
  path <- write_discovery_proposal(
    list(competitions = list(), unmodelled_offered_count = 0L), root = tmp
  )
  back <- jsonlite::read_json(path)
  expect_equal(length(back$competitions), 0L)
})
```

- [ ] **Step 2: Run, verify it fails**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-discover-lengjan.R")'`
Expected: FAIL — `could not find function "write_discovery_proposal"`.

- [ ] **Step 3: Implement** (append to `R/discover-lengjan.R`; no non-ASCII literals)

```r
#' @noRd
.tn_to_list <- function(tn) {
  if (is.null(tn) || nrow(tn) == 0L) {
    return(list())
  }
  lapply(seq_len(nrow(tn)), function(j) {
    list(
      lengjan = tn$lengjan[[j]],
      canonical_guess = if (is.na(tn$canonical_guess[[j]])) NULL else tn$canonical_guess[[j]],
      confidence = tn$confidence[[j]]
    )
  })
}

#' @noRd
.write_discovery_summary <- function(payload, path) {
  lines <- c(
    "# Lengjan discovery — proposed competitions",
    "",
    paste0("Generated: ", payload$generated_at),
    paste0("Unmodelled competitions offered (in our sports/countries): ",
      payload$unmodelled_offered_count),
    ""
  )
  if (length(payload$competitions) == 0L) {
    lines <- c(lines, "_No new modelled competitions to wire._")
  } else {
    for (c in payload$competitions) {
      lines <- c(lines,
        sprintf("## %s / %s — %s (id=%s)", c$sport, c$inferred_sex,
          c$inferred_division, c$comp_id),
        sprintf("- Lengjan name: %s", c$lengjan_name),
        sprintf("- Classify confidence: %s", c$classify_confidence),
        "- Proposed team_names:"
      )
      if (length(c$proposed_team_names) == 0L) {
        lines <- c(lines, "  - (none scraped yet)")
      } else {
        for (t in c$proposed_team_names) {
          cg <- t$canonical_guess %||% "??? (verify)"
          lines <- c(lines, sprintf("  - %s -> %s (%s)", t$lengjan, cg, t$confidence))
        }
      }
      lines <- c(lines, "")
    }
  }
  writeLines(enc2utf8(lines), path, useBytes = TRUE)
  invisible(path)
}

#' Write the discovery proposal to JSON + a human-readable summary.
#' @param findings Output of [discover_new_competitions()].
#' @param root Data root; writes under `root/discovery/`.
#' @param now Timestamp for the payload.
#' @return invisible(path to proposals.json).
#' @export
write_discovery_proposal <- function(findings, root = here::here("data"),
                                     now = Sys.time()) {
  comps <- lapply(findings$competitions, function(f) {
    f$proposed_team_names <- .tn_to_list(f$proposed_team_names)
    f
  })
  payload <- list(
    generated_at = format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    competitions = comps,
    unmodelled_offered_count = findings$unmodelled_offered_count
  )
  dir <- file.path(root, "discovery")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, "proposals.json")
  write_json_consistent(payload, path, pretty = TRUE, auto_unbox = TRUE)
  .write_discovery_summary(payload, file.path(dir, "SUMMARY.md"))
  invisible(path)
}
```

> **Note:** `write_json_consistent(..., auto_unbox = TRUE)` unboxes length-1 vectors to JSON scalars; the `competitions` list and each `proposed_team_names` list stay arrays. An empty `list()` serialises to `[]`, which `jsonlite::read_json` reads back as a length-0 list — matching the test.

- [ ] **Step 4: Run, verify it passes**

Run: `Rscript -e 'devtools::document(); devtools::load_all(); testthat::test_file("tests/testthat/test-discover-lengjan.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/discover-lengjan.R tests/testthat/test-discover-lengjan.R NAMESPACE
git commit -m "feat(discover): write proposals.json + SUMMARY.md"
```

---

### Task 6: Entry script (`scripts/0N_discover.R`)

**Files:**
- Create: `scripts/0N_discover.R`

**Interfaces:**
- Consumes: `load_leagues`, `discover_new_competitions`, `write_discovery_proposal`.

- [ ] **Step 1: Write the script** (no non-ASCII literals)

```r
#!/usr/bin/env Rscript
# scripts/0N_discover.R --
# Discover Lengjan competitions we model but do not yet scrape.
#
# Reads each active modelled (sport, country)'s "Veldu deild" dropdown, diffs
# against config/leagues.yml, and writes data/discovery/proposals.json +
# SUMMARY.md. Read-only on Lengjan (public dropdown, no login) and on our data
# (results only) -- never touches the ledger or the placer. CI-safe.
#
# Usage:
#   Rscript scripts/0N_discover.R

invisible(Sys.setlocale("LC_ALL", "en_US.UTF-8"))
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))

leagues <- load_leagues()

session <- chromote::ChromoteSession$new()
try(session$default_timeout <- 30, silent = TRUE)
on.exit(session$close(), add = TRUE)

findings <- tryCatch(
  discover_new_competitions(leagues, session = session),
  error = function(e) {
    cli::cli_alert_warning("Discovery failed: {conditionMessage(e)}")
    NULL
  }
)

if (is.null(findings)) {
  cli::cli_alert_warning("No discovery output this run; keeping last proposal.")
  quit(save = "no", status = 0L)
}

path <- write_discovery_proposal(findings)
n <- length(findings$competitions)
cli::cli_alert_success(
  "Discovery wrote {n} proposed competition(s) to {path} ",
  "({findings$unmodelled_offered_count} unmodelled offered)."
)
```

- [ ] **Step 2: Smoke-test it runs end-to-end (live, local only)**

Run: `Rscript scripts/0N_discover.R`
Expected: prints "Discovery wrote N proposed competition(s)…" and creates `data/discovery/proposals.json` + `SUMMARY.md`. (At quiet times N may be 0 — a clean run with an empty `competitions` array is correct.)

- [ ] **Step 3: Commit**

```bash
git add scripts/0N_discover.R
git commit -m "feat(discover): entry script scripts/0N_discover.R"
```

---

### Task 7: Scheduled workflow (`discover-leagues.yml`)

**Files:**
- Create: `.github/workflows/discover-leagues.yml`

**Interfaces:** none (CI orchestration). Mirrors `scrape-odds.yml`; writes only `data/discovery/**`.

- [ ] **Step 1: Write the workflow**

```yaml
name: Discover Lengjan Leagues

on:
  schedule:
    # Once daily UTC, off the odds-scrape slots (8,11,14,20) and the
    # healthcheck slots. Competitions appear at most a few times per season.
    - cron: '40 6 * * *'
  workflow_dispatch: {}

permissions:
  contents: write

concurrency:
  group: discover-leagues
  cancel-in-progress: false

jobs:
  discover:
    runs-on: ubuntu-latest
    timeout-minutes: 30

    env:
      GITHUB_PAT: ${{ secrets.GITHUB_TOKEN }}

    steps:
      - uses: actions/checkout@v5

      - uses: r-lib/actions/setup-r@v2
        with:
          r-version: 'release'
          use-public-rspm: true

      - uses: browser-actions/setup-chrome@v2
        id: setup-chrome
        with:
          chrome-version: stable

      - uses: r-lib/actions/setup-r-dependencies@v2
        env:
          PKG_SYSREQS: "false"
        with:
          dependencies: '"hard"'
          extra-packages: |
            any::devtools
            any::chromote

      - name: Reinstall V8 from source with bundled static libv8
        env:
          DOWNLOAD_STATIC_LIBV8: "1"
        run: |
          Rscript -e 'install.packages("V8", type = "source", repos = c(CRAN = "https://cloud.r-project.org"))'

      - name: Discover Lengjan leagues
        env:
          CHROMOTE_CHROME: ${{ steps.setup-chrome.outputs.chrome-path }}
        run: Rscript scripts/0N_discover.R

      - name: Commit if discovery changed
        run: |
          git config user.name  "github-actions[bot]"
          git config user.email "github-actions[bot]@users.noreply.github.com"
          git add data/discovery/
          if git diff --cached --quiet; then
            echo "No discovery change"
            exit 0
          fi
          git commit -m "data(discovery): refresh $(date -u +%Y-%m-%dT%H:%MZ)"
          git pull --rebase origin main
          git push
```

- [ ] **Step 2: Verify the placer-isolation test still passes**

The workflow references no placer/credential token, so the existing guard stays green.
Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-placer-ci-isolation.R")'`
Expected: PASS.

- [ ] **Step 3: Verify the workflow name has no glob meta-characters**

`"Discover Lengjan Leagues"` contains none of `+ * ? [ ] !` — safe for any future `workflow_run` reference (per `.claude/rules/ci-conventions.md`).

- [ ] **Step 4: Commit**

```bash
git add .github/workflows/discover-leagues.yml
git commit -m "ci(discover): daily Discover Lengjan Leagues workflow"
```

---

### Task 8: Health check (`check_discovery`)

**Files:**
- Modify: `R/health.R`
- Test: `tests/testthat/test-discover-health.R` (Create)

**Interfaces:**
- Consumes: `.configured_comp_ids` (Task 4), `load_leagues`, `health_row`, `%||%`, `jsonlite::read_json`.
- Produces: `check_discovery(root, th) -> health_row(...)`; added to the `pipeline_health()` `bind_rows`.

- [ ] **Step 1: Write the failing test** (Create `tests/testthat/test-discover-health.R`, via Python — Icelandic literals)

```r
test_that("check_discovery WARNs on an un-actioned modelled proposal", {
  tmp <- withr::local_tempdir()
  findings <- list(
    competitions = list(list(
      sport = "football", country = "iceland", comp_id = "757",
      lengjan_name = "Lengjudeildin", inferred_sex = "male",
      inferred_division = "LD1", classify_confidence = "high",
      modelled = TRUE, status = "new",
      proposed_team_names = tibble::tibble(
        lengjan = "Víkingur Ól.", canonical_guess = "Víkingur Ó.", confidence = "high"
      )
    )),
    unmodelled_offered_count = 0L
  )
  write_discovery_proposal(findings, root = tmp)

  # comp 757 is NOT in the live config for this test's purposes only if the real
  # config lacks it; the check reads load_leagues(). To make the test
  # config-independent, assert the WARN path via a comp_id that cannot be wired.
  findings$competitions[[1]]$comp_id <- "DISCOVERY_TEST_UNWIRED"
  write_discovery_proposal(findings, root = tmp)

  row <- check_discovery(tmp, health_thresholds())
  expect_equal(row$check, "discovery")
  expect_equal(row$status, "WARN")
  expect_match(row$value, "DISCOVERY_TEST_UNWIRED")
})

test_that("check_discovery is OK when no proposals file exists", {
  tmp <- withr::local_tempdir()
  row <- check_discovery(tmp, health_thresholds())
  expect_equal(row$status, "OK")
})

test_that("check_discovery is OK when all proposed comps are already configured", {
  tmp <- withr::local_tempdir()
  cfg_id <- load_leagues()[["football_iceland"]]$lengjan$competitions[[1]]$id
  findings <- list(
    competitions = list(list(
      sport = "football", country = "iceland", comp_id = as.character(cfg_id),
      lengjan_name = "Besta deild karla", inferred_sex = "male",
      inferred_division = "BD", classify_confidence = "high",
      modelled = TRUE, status = "new",
      proposed_team_names = tibble::tibble(
        lengjan = character(0), canonical_guess = character(0), confidence = character(0)
      )
    )),
    unmodelled_offered_count = 0L
  )
  write_discovery_proposal(findings, root = tmp)
  row <- check_discovery(tmp, health_thresholds())
  expect_equal(row$status, "OK")
})
```

- [ ] **Step 2: Run, verify it fails**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-discover-health.R")'`
Expected: FAIL — `could not find function "check_discovery"`.

- [ ] **Step 3: Implement `check_discovery` in `R/health.R`** (insert before `pipeline_health`; no non-ASCII literals)

```r
#' Discovery: Lengjan now lists a modelled competition we do not yet scrape.
#'
#' Reads `data/discovery/proposals.json` and WARNs on any proposed competition
#' that is `modelled` and whose `comp_id` is not yet in
#' `config/leagues.yml::*.lengjan.competitions`. Keying "un-actioned" off the
#' LIVE config (not the proposal's `status`) makes the WARN self-clear the
#' instant the comp is wired, even before the next discovery run refreshes the
#' file. Never escalates past WARN -- a newly-listed league is not an outage.
#' @noRd
check_discovery <- function(root, th) {
  thr_lbl <- "0 un-actioned modelled competitions"
  path <- file.path(root, "discovery", "proposals.json")
  if (!file.exists(path)) {
    return(health_row("discovery", "lengjan", "OK", "no proposals file", thr_lbl))
  }
  prop <- tryCatch(jsonlite::read_json(path), error = function(e) NULL)
  comps <- prop$competitions %||% list()
  if (length(comps) == 0L) {
    return(health_row("discovery", "lengjan", "OK", 0, thr_lbl))
  }
  leagues <- tryCatch(load_leagues(), error = function(e) list())
  unactioned <- Filter(function(c) {
    isTRUE(c$modelled) &&
      !(as.character(c$comp_id) %in%
        .configured_comp_ids(leagues, c$sport, c$country))
  }, comps)
  n <- length(unactioned)
  if (n == 0L) {
    return(health_row("discovery", "lengjan", "OK", 0, thr_lbl))
  }
  labels <- vapply(unactioned, function(c) {
    sprintf("%s/%s %s (id=%s)", c$sport, c$inferred_sex %||% "?",
      c$inferred_division %||% "?", c$comp_id)
  }, character(1))
  health_row("discovery", "lengjan", "WARN",
    paste0(n, ": ", paste(labels, collapse = "; ")), thr_lbl)
}
```

- [ ] **Step 4: Wire into `pipeline_health()`**

In `R/health.R`, add the discovery check to the `dplyr::bind_rows(...)` inside `pipeline_health()` (after `check_bankroll`):

```r
  dplyr::bind_rows(
    safe(check_fit_freshness(leagues, root, now, th)),
    safe(check_odds_freshness(leagues, root, now, th)),
    safe(check_diagnostics_drift(root, th)),
    safe(check_orphaned_bets(root, now, th)),
    safe(check_capture_rate(root, now, th)),
    safe(check_placement_health(root, now, th)),
    safe(check_bankroll(root, th)),
    safe(check_discovery(root, th))
  )
```

- [ ] **Step 5: Run the test, verify it passes**

Run: `Rscript -e 'devtools::document(); devtools::load_all(); testthat::test_file("tests/testthat/test-discover-health.R")'`
Expected: PASS (all three).

- [ ] **Step 6: Run the full health test file to confirm no regression**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-healthcheck.R")'`
Expected: PASS (pre-existing health tests unaffected).

- [ ] **Step 7: Commit**

```bash
git add R/health.R tests/testthat/test-discover-health.R NAMESPACE
git commit -m "feat(health): discovery WARN row, self-clearing off live config"
```

---

### Task 9: Read-only isolation test

**Files:**
- Create: `tests/testthat/test-discover-ci-isolation.R`

**Interfaces:** none.

- [ ] **Step 1: Write the test** (no non-ASCII literals — normal editor is fine)

```r
# tests/testthat/test-discover-ci-isolation.R
#
# Discovery is read-only on the money path: it must never read/write the ledger,
# call the placer, or reference Lengjan credentials. (The dropdown is public, so
# discovery needs no login.) This guards that invariant at the source level.

test_that("discovery engine + entry script reference no ledger/placer/credentials", {
  files <- c(
    here::here("R", "discover-lengjan.R"),
    here::here("scripts", "0N_discover.R")
  )
  files <- files[file.exists(files)]
  expect_true(length(files) > 0L)

  forbidden <- c(
    "append_to_ledger", "settle_ledger", "data/decisions/ledger",
    "place_bets", "preview_bets", "placer_", "R/placer-",
    "run_auto_place", "LENGJAN_USER", "LENGJAN_PASS",
    "placer_login", "login_lengjan"
  )
  failures <- character(0)
  for (f in files) {
    contents <- readLines(f, warn = FALSE)
    for (token in forbidden) {
      hit <- grep(token, contents, fixed = TRUE)
      if (length(hit) > 0L) {
        failures <- c(failures, sprintf("%s:%d references %s",
          basename(f), hit[1L], shQuote(token)))
      }
    }
  }
  if (length(failures) > 0L) {
    fail(paste("Discovery must stay read-only:",
      paste("  -", failures, collapse = "\n"), sep = "\n"))
  }
  expect_true(TRUE)
})
```

- [ ] **Step 2: Run, verify it passes**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-discover-ci-isolation.R")'`
Expected: PASS.

- [ ] **Step 3: Commit**

```bash
git add tests/testthat/test-discover-ci-isolation.R
git commit -m "test(discover): enforce read-only money-path isolation"
```

---

### Task 10: `/wire-league` skill

**Files:**
- Create: `.claude/skills/wire-league/SKILL.md`

**Interfaces:** none (documentation/process).

- [ ] **Step 1: Write the skill** (Create `.claude/skills/wire-league/SKILL.md`)

````markdown
---
name: wire-league
description: Use when Lengjan has started listing a league we already model and a discovery proposal needs turning into a config edit. Reads data/discovery/proposals.json, drafts the leagues.yml competition entry + team_names, verifies via a dry decide, and presents the diff for review.
argument-hint: "[comp_id]"
context: fork
effort: high
---

# /wire-league — Turn a discovery proposal into a config edit

The `discover-leagues.yml` workflow writes `data/discovery/proposals.json` when
Lengjan starts offering a competition we model but do not yet scrape, and the
`discovery` health row WARNs about it. This skill turns one proposal into a
reviewed `config/leagues.yml` edit. It does NOT place bets and does NOT add a new
modelled league (that is `/add-league`).

## Step 1: Read the proposal

```bash
cat data/discovery/proposals.json
```

Pick the target competition (by `comp_id` if one was passed as an argument).
Note its `sport`, `country`, `inferred_sex`, `inferred_division`, `lengjan_name`,
and `proposed_team_names`.

## Step 2: Draft the `leagues.yml` edit

Under the matching league's `lengjan:` block:

1. Append a `competitions` entry:
   `- { id: "<comp_id>", name: "<lengjan_name>", sex: <inferred_sex> }`
2. Add the `team_names[[inferred_sex]]` entries from `proposed_team_names`:
   - `confidence: high` → add as-is.
   - `confidence: medium`/`low` or `canonical_guess: null` → add only if you can
     confirm the canonical team from the division's `results`; otherwise leave a
     comment that it is a pattern-guess to verify on first odds. A wrong guess is
     fail-safe (`decide_league` warn-skips an unmatched name), but do not invent.

Icelandic strings: edit `config/leagues.yml` directly (it is YAML/UTF-8, not R
source), mirroring the existing women's-Lengjudeild block (comp 4670) as the
template.

## Step 3: Verify the wiring

```bash
# Config still parses:
Rscript -e 'devtools::load_all(); stopifnot("<comp_id>" %in% vapply(load_leagues()[["<league_key>"]]$lengjan$competitions, function(c) c$id, character(1)))'

# Team-name map is still invertible:
Rscript -e 'devtools::load_all(); validate_team_names_config(load_leagues()[["<league_key>"]])'

# Dry decide produces candidates once odds exist (no ledger writes):
Rscript scripts/02_scrape_odds.R --league <league_key>
Rscript -e 'devtools::load_all(); print(decide_league("<league_key>", sex = "<inferred_sex>", write = FALSE))'
```

If `decide_league` warns "no beliefs for <team>", a team-name guess is wrong —
fix the mapping and re-run. No bet is ever placed by this skill.

## Step 4: Present for review

Show the `git diff config/leagues.yml` and the dry-decide output. The human
reviews and commits. The `discovery` health WARN self-clears on the next health
snapshot once the comp_id is in config.
````

- [ ] **Step 2: Verify the skill conventions test still passes**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-skill-conventions.R")'`
Expected: PASS (the new skill references no legacy paths/flags).

- [ ] **Step 3: Commit**

```bash
git add .claude/skills/wire-league/SKILL.md
git commit -m "feat(skill): /wire-league turns a discovery proposal into a config edit"
```

---

### Task 11: Documentation + full-suite gate

**Files:**
- Modify: `CLAUDE.md`, `.claude/rules/ci-conventions.md`

**Interfaces:** none.

- [ ] **Step 1: Add the workflow to the CI inventory**

In `.claude/rules/ci-conventions.md`, add a row to the "Workflow inventory" table:

```
| `discover-leagues.yml` | cron 1×/day + dispatch | Read-only Lengjan competition-dropdown discovery → `data/discovery/proposals.json`; commits if changed. Surfaces via the `discovery` health WARN. References no placer token. |
```

- [ ] **Step 2: Note the script + skill in CLAUDE.md**

In `CLAUDE.md`, add `scripts/0N_discover.R` to the Quick reference block:

```
Rscript scripts/0N_discover.R                         # discover Lengjan leagues we model but don't yet scrape
```

and add `/wire-league` to the Skills section list alongside `/add-league`.

- [ ] **Step 3: Run the full test suite**

Run: `Rscript -e 'devtools::document(); devtools::test()'`
Expected: 0 failures. (`devtools::document()` first so NAMESPACE reflects every new `@export`.)

- [ ] **Step 4: Commit**

```bash
git add CLAUDE.md .claude/rules/ci-conventions.md NAMESPACE
git commit -m "docs(discover): document workflow, entry script, and /wire-league"
```

---

## Self-review

**Spec coverage** (each spec section → task):
- §4 data flow → Tasks 1–8 (engine), 6 (script), 7 (CI), 8 (health).
- §5.1 engine functions → Tasks 1–5 (every function named in §5.1 is implemented).
- §5.2 entry script → Task 6. §5.3 workflow → Task 7. §5.4 `check_discovery` → Task 8. §5.5 `/wire-league` → Task 10. §5.6 tests → Tasks 1–5, 8, 9.
- §6 data contract → Task 5 (writer) matches the JSON shape; `unmodelled_offered_count` narrowed to in-scope per the note (consistent with §11).
- §7 classification + matching → Tasks 2–3. §8 safety invariants → Task 9 (isolation), fail-safe noted in Tasks 3/10. §9 edge cases → covered: timeout soft-fail (Task 6), empty dropdown (Task 1/4), unclassifiable (Task 2), already-configured (Task 8). §10 testing → Tasks 1–5, 8, 9. §12 success criteria → Tasks 6–8 end-to-end.

**Placeholder scan:** none — every code step shows complete code.

**Type consistency:** `discover_new_competitions()` returns `list(competitions, unmodelled_offered_count)`; `write_discovery_proposal()` consumes exactly that; `check_discovery()` reads the JSON shape `write_discovery_proposal()` emits (`competitions[].{comp_id, sport, country, modelled, inferred_sex, inferred_division}`). `.configured_comp_ids()` is defined once (Task 4) and reused in Task 8. `health_row()`/`health_thresholds()` signatures match `R/health.R`.

## Execution Handoff

Two execution options:

1. **Subagent-Driven (recommended)** — dispatch a fresh subagent per task, review between tasks, fast iteration.
2. **Inline Execution** — execute tasks in this session via executing-plans, batch with checkpoints.

Which approach?
