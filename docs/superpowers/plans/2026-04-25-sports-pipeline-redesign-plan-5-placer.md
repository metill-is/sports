# Sports Pipeline Redesign — Plan 5: Placer (local-only Lengjan automation)

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Port `_legacy/lengjan-bets/` (~1,840 lines across 7 R files) into `R/placer-*.R` so the consolidated monorepo can place real-money bets on Lengjan via Chromote. Read recommendations from `data/decisions/recommendations/` Parquet, dedup against `data/decisions/ledger/`, navigate the Lengjan UI, enforce the P1–P4 placement rules, and append placed bets to the ledger.

**Architecture:** Eight flat R files mirror the legacy structure: `placer-{validate,load,ledger,login,navigate,place,pipeline,preview}.R`. The placer is **local-only** — never executed on CI, no GitHub Actions secret named `LENGJAN_*`, and Plan 5 adds an explicit CI test that fails the build if a workflow file references `R/placer-*.R`. Browser automation stays on Chromote (per spec §4.3 step 7: "port largely as-is"). The `place_bet.R` state machine (722 legacy lines) is the largest single port — Plan 5 keeps it functionally identical but extracts testable seams (DOM-parse functions, P3/P4 checks, kelly recalc) so the testable bits get coverage without spinning up a browser.

The placer remains the **only writer to the ledger**. Plan 1's ETL'd ledger Parquet (`data/decisions/ledger/`, 1,870 rows / 89,369 ISK PnL) is now the canonical store. During cutover, Plan 5 dual-writes to the legacy per-league `bets_log.csv` paths AND the unified Parquet, so a regression in either format is recoverable. Plan 6's `{targets}` DAG drops the CSV dual-write once a week of production runs match.

**Tech Stack:** R (≥ 4.0), `{chromote}` (browser automation), `{httr2}` (auxiliary HTTP), `{jsonlite}` (DOM result parsing), Plan 1–4 stack (`{arrow}`, `{dplyr}`, `{cli}`, `{here}`, `{readr}`), `{testthat}` ed 3. No new runtime deps — chromote was already in legacy.

**Scope (Plan 5):**

- 8 `R/placer-*.R` files porting `_legacy/lengjan-bets/R/*.R` + `preview.R`
- `scripts/place_bets.R` + `scripts/preview_bets.R` — CLI entrypoints (replace legacy `run.R` + `preview.R`)
- TDD on the testable seams: validate, load, dedup, ledger append, kelly recalc, DOM-parse helpers
- Skip-friendly smoke tests for Chromote-bound functions (gated on `LENGJAN_USER` env var)
- CI safety gate: a test that grep's `.github/workflows/*.yml` and fails if any line mentions `R/placer-` or `placer_pipeline` or `LENGJAN_*`
- CLAUDE.md update — Plan 5 → ✅ Complete; "Local-only subsystem" expanded with placer details; new R/placer-*.R + scripts/ entries in directory tree
- A `.Renviron.example` template at repo root (already present from Plan 1; extend with placer-specific entries if needed)

**Out of scope (still):**

- Orchestration via `{targets}`, CI workflows for fit/decide/publish, metill-platform integration — Plan 6
- Placer running on CI — explicitly forbidden by spec §3.5; the CI safety gate enforces this
- New placement policies (P1–P4 are preserved verbatim)
- CLV tracker / ROI report (`_legacy/sports/R/bets/{clv_tracker,roi_report}.R`) — Plan 6+ research tooling
- Reviving paused non-Icelandic leagues
- Migrating the legacy CSV ledger writes away — dual-write stays during Plan 5; Plan 6 drops CSV after a week of agreement

---

## File structure created by this plan

```
sports/
├── R/
│   ├── placer-validate.R           # validate_team_names_config(), validate_recommendations()
│   ├── placer-load.R               # load_recommendations() + dedup_against_ledger()
│   ├── placer-ledger.R             # append_to_ledger() + CSV dual-write
│   ├── placer-login.R              # chromote_login()
│   ├── placer-navigate.R           # navigate_to_match() + extract_match_id()
│   ├── placer-place.R              # place_one_bet() state machine + P3/P4
│   ├── placer-pipeline.R           # place_bets() orchestrator
│   └── placer-preview.R            # preview_pending() — dry-run, no browser
├── scripts/
│   ├── place_bets.R                # CLI: --live, --dry-run, --league, --today, --no-confirm
│   └── preview_bets.R              # CLI: --league, --today (no browser)
├── tests/testthat/
│   ├── fixtures/placer/
│   │   ├── recs_sample.parquet     # Tiny recommendations fixture
│   │   ├── ledger_sample.parquet   # Tiny ledger fixture for dedup testing
│   │   └── lengjan_match_html.txt  # Captured HTML snippet of a Lengjan match page
│   ├── test-placer-validate.R
│   ├── test-placer-load.R
│   ├── test-placer-ledger.R
│   ├── test-placer-place.R         # P3/P4 logic + DOM parsing only; chromote bits skipped
│   ├── test-placer-preview.R
│   └── test-placer-ci-isolation.R  # Asserts no CI workflow references the placer
```

---

## Task 1: `R/placer-validate.R` — pre-flight validation (TDD)

**Files:**

- Create: `R/placer-validate.R`
- Create: `tests/testthat/test-placer-validate.R`

**Purpose:** Port `_legacy/lengjan-bets/R/validate.R` (95 lines). Pre-flight checks that fail fast before chromote starts: every recommendation's (sport, country, sex) combo has matching `lengjan.team_names` in `config/leagues.yml` and the recommendations tibble has the schema columns the placer expects.

The legacy `validate.R` checked for per-league `team_names_*.csv` files at `lengjan-odds/config/`; the new design pulls team-name maps from `config/leagues.yml`'s `lengjan.team_names` block (already established in Plans 1–4). The validation logic is the same — fail loudly when team names are missing.

**Signatures:**

```r
validate_team_names_config(leagues, recs) -> invisible(TRUE) | stop()
validate_recommendations_schema(recs) -> invisible(TRUE) | stop()
```

- `leagues` — output of `load_leagues()`
- `recs` — recommendations tibble from `load_recommendations()`

### Step 1: Write failing tests

```r
# tests/testthat/test-placer-validate.R

test_that("validate_team_names_config passes when all recs have a team_names entry", {
  leagues <- list(
    football_iceland = list(
      sport = "football", country = "iceland",
      lengjan = list(team_names = list("KR" = "KR Reykjavík"))
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
      lengjan = list()   # no team_names key
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
      lengjan = list(team_names = list("KR" = "KR Reykjavík"))
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
    # missing home_team, away_team, etc.
  )
  expect_error(
    validate_recommendations_schema(recs),
    "missing|column"
  )
})
```

### Step 2: Verify failure

```bash
cd /Users/brynjolfurjonsson/sports
Rscript -e 'devtools::test(filter = "placer-validate")'
```

Expected: 5 errors (functions not defined).

### Step 3: Implement `R/placer-validate.R`

```r
#' @include config.R
NULL

#' Validate that every (sport, country) appearing in `recs` has a
#' `lengjan$team_names` entry in `leagues`, and that every team name
#' appearing in `recs` is keyed in that map.
#'
#' Fails fast with `stop()` so the caller doesn't waste a Chromote session
#' on a misconfigured league.
#'
#' @param leagues Named list from `load_leagues()`.
#' @param recs Recommendations tibble. Must have `sport`, `country`,
#'   `home_team`, `away_team` columns.
#' @return Invisibly TRUE on success.
#' @export
validate_team_names_config <- function(leagues, recs) {
  stopifnot(is.list(leagues))
  stopifnot(all(c("sport", "country", "home_team", "away_team") %in% names(recs)))

  # Group recommendations by (sport, country); for each, find the matching
  # league config and verify team_names coverage.
  groups <- unique(recs[, c("sport", "country"), drop = FALSE])
  for (i in seq_len(nrow(groups))) {
    sp <- groups$sport[i]; co <- groups$country[i]
    key <- paste0(sp, "_", co)

    if (!key %in% names(leagues)) {
      stop("validate_team_names_config: no leagues.yml entry for ", key,
           call. = FALSE)
    }
    league <- leagues[[key]]
    tn <- league$lengjan$team_names
    if (is.null(tn) || length(tn) == 0L) {
      stop("validate_team_names_config: ", key,
           " has no lengjan$team_names. Add it to config/leagues.yml.",
           call. = FALSE)
    }

    # All teams in this (sport, country) must be keyed in team_names
    rows <- recs[recs$sport == sp & recs$country == co, , drop = FALSE]
    teams <- unique(c(rows$home_team, rows$away_team))
    missing <- setdiff(teams, names(tn))
    if (length(missing) > 0L) {
      stop("validate_team_names_config: ", key,
           " is missing team_names for: ",
           paste(missing, collapse = ", "),
           call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Validate that `recs` has the columns the placer pipeline expects.
#'
#' @param recs Tibble.
#' @return Invisibly TRUE on success.
#' @export
validate_recommendations_schema <- function(recs) {
  required <- c("sport", "country", "sex", "match_date",
                "home_team", "away_team",
                "market", "outcome", "line",
                "p", "odds", "ev", "kelly", "bet_amount")
  missing <- setdiff(required, names(recs))
  if (length(missing) > 0L) {
    stop("validate_recommendations_schema: missing columns: ",
         paste(missing, collapse = ", "),
         call. = FALSE)
  }
  invisible(TRUE)
}
```

### Step 4: Roxygen + verify + commit

```bash
Rscript -e 'roxygen2::roxygenise(); roxygen2::update_collate(".")'
Rscript -e 'devtools::test(filter = "placer-validate")'
```

Expected: 5 passes.

```bash
git add R/placer-validate.R NAMESPACE DESCRIPTION tests/testthat/test-placer-validate.R
git commit -m "feat: R/placer-validate.R — pre-flight team-name + schema checks

Ports _legacy/lengjan-bets/R/validate.R into the new R/ layout.
validate_team_names_config(leagues, recs) checks that every (sport,
country) seen in recommendations has a lengjan\$team_names map in
leagues.yml AND that every team name in recommendations is keyed
there. Fails fast before chromote starts.

validate_recommendations_schema(recs) checks the canonical column
set (the one decide_league() writes).

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>"
```

---

## Task 2: `R/placer-load.R` — load recommendations + dedup (TDD)

**Files:**

- Create: `R/placer-load.R`
- Create: `tests/testthat/fixtures/placer/recs_sample.parquet`
- Create: `tests/testthat/fixtures/placer/ledger_sample.parquet`
- Create: `tests/testthat/test-placer-load.R`

**Purpose:** Replace `_legacy/lengjan-bets/R/pipeline.R::load_recommendations()` (which read `Sports/recommendations.csv`) with `read_table("recommendations", root)`. Replace `dedup_against_ledger()` (which globbed all per-league `bets_log.csv` paths) with `read_table("ledger", root)`.

**Signatures:**

```r
load_recommendations(root, leagues = NULL, today_only = FALSE,
                     target_date = NULL, run_date = NULL) -> tibble
dedup_against_ledger(recs, root) -> tibble
```

- `load_recommendations` reads the latest `run_date` partition (or `target_date` if given) and returns rows for `match_date >= today` (or matching `target_date`).
- `dedup_against_ledger` anti-joins on `(sport, country, sex, match_date, home_team, away_team, market, outcome, line)` against settled-or-pending ledger rows.

### Step 1: Create fixtures

Run interactively:

```r
library(arrow); library(tibble)
recs <- tibble::tibble(
  run_id = as.POSIXct("2026-04-25 09:00:00", tz = "UTC"),
  sport = c("football", "football", "basketball"),
  country = "iceland",
  sex = c("male", "male", "female"),
  match_date = as.Date(c("2026-04-26", "2026-04-26", "2026-04-27")),
  home_team = c("KR", "Fram", "Valur"),
  away_team = c("FH", "Stjarnan", "Haukar"),
  market = "moneyline",
  outcome = c("home", "away", "home"),
  line = NA_real_,
  p = c(0.55, 0.40, 0.65),
  odds = c(1.85, 2.50, 1.65),
  ev = c(0.02, 0.00, 0.07),
  kelly = c(0.05, 0.00, 0.08),
  bet_amount = c(1180, 0, 1890)
)
arrow::write_parquet(recs, "tests/testthat/fixtures/placer/recs_sample.parquet")

ledger <- tibble::tibble(
  placed_at = as.POSIXct("2026-04-25 12:00:00", tz = "UTC"),
  match_date = as.Date("2026-04-26"),
  sport = "football", country = "iceland", sex = "male",
  home_team = "KR", away_team = "FH",
  market = "moneyline", outcome = "home", line = NA_real_,
  odds_placed = 1.84, p = 0.55, kelly = 0.05, bet_amount = 1180,
  settled = FALSE, win = NA, pnl = NA_real_
)
arrow::write_parquet(ledger, "tests/testthat/fixtures/placer/ledger_sample.parquet")
```

### Step 2: Failing tests

```r
# tests/testthat/test-placer-load.R

setup_placer_root <- function() {
  tmp <- withr::local_tempdir(.local_envir = parent.frame())
  recs <- arrow::read_parquet(testthat::test_path("fixtures", "placer",
                                                   "recs_sample.parquet"))
  led <- arrow::read_parquet(testthat::test_path("fixtures", "placer",
                                                  "ledger_sample.parquet"))
  write_table(recs, "recommendations", root = tmp)
  write_table(led, "ledger", root = tmp)
  tmp
}

test_that("load_recommendations returns rows for matching target_date", {
  root <- setup_placer_root()
  out <- load_recommendations(root, target_date = as.Date("2026-04-26"))
  expect_equal(nrow(out), 2L)
  expect_true(all(out$match_date == as.Date("2026-04-26")))
})

test_that("load_recommendations honours league filter", {
  root <- setup_placer_root()
  out <- load_recommendations(root,
                              leagues = "basketball_iceland",
                              target_date = as.Date("2026-04-27"))
  expect_equal(nrow(out), 1L)
  expect_equal(out$sport, "basketball")
})

test_that("dedup_against_ledger drops already-placed bets", {
  root <- setup_placer_root()
  recs <- load_recommendations(root, target_date = as.Date("2026-04-26"))
  out <- dedup_against_ledger(recs, root)
  # KR vs FH moneyline/home is in the ledger; should be removed.
  expect_equal(nrow(out), 1L)
  expect_equal(out$home_team, "Fram")
})

test_that("dedup_against_ledger is a no-op when ledger is empty", {
  tmp <- withr::local_tempdir()
  recs <- arrow::read_parquet(testthat::test_path("fixtures", "placer",
                                                   "recs_sample.parquet"))
  write_table(recs, "recommendations", root = tmp)
  loaded <- load_recommendations(tmp, target_date = as.Date("2026-04-26"))
  out <- dedup_against_ledger(loaded, tmp)
  expect_equal(nrow(out), nrow(loaded))
})

test_that("load_recommendations returns 0 rows + correct cols when nothing matches", {
  root <- setup_placer_root()
  out <- load_recommendations(root, target_date = as.Date("2050-01-01"))
  expect_equal(nrow(out), 0L)
  expect_true(all(c("match_date", "home_team", "market") %in% names(out)))
})
```

### Step 3: Verify failure + implement

```r
# R/placer-load.R

#' @include storage.R
NULL

#' Load recommendations for placement.
#'
#' Reads `data/decisions/recommendations/` for the most recent `run_date`
#' partition (or `target_date` when supplied), filters to upcoming matches,
#' and optionally restricts to the league keys requested.
#'
#' @param root Data root.
#' @param leagues Optional character vector of league keys (e.g.
#'   `"football_iceland"`). NULL = all.
#' @param today_only Keep only matches today.
#' @param target_date Specific match_date to keep. Mutually exclusive with
#'   `today_only` and overrides the future-only filter.
#' @param run_date Specific run_date partition to read. NULL = most recent.
#' @return Tibble matching `schemas()$recommendations` (post-filter).
#' @export
load_recommendations <- function(root,
                                 leagues = NULL,
                                 today_only = FALSE,
                                 target_date = NULL,
                                 run_date = NULL) {
  recs <- tryCatch(
    read_table("recommendations", root = root),
    error = function(e) tibble::tibble()
  )
  if (nrow(recs) == 0L) return(empty_recommendations_for_placement())

  if (!is.null(run_date)) {
    recs <- recs[as.character(recs$run_date) == as.character(run_date), , drop = FALSE]
  }

  if (!is.null(target_date)) {
    recs <- recs[recs$match_date == as.Date(target_date), , drop = FALSE]
  } else if (today_only) {
    recs <- recs[recs$match_date == Sys.Date(), , drop = FALSE]
  } else {
    recs <- recs[recs$match_date >= Sys.Date(), , drop = FALSE]
  }

  if (!is.null(leagues)) {
    keep <- paste0(recs$sport, "_", recs$country) %in% leagues
    recs <- recs[keep, , drop = FALSE]
  }

  recs
}

#' Anti-join recommendations against the placed-bet ledger.
#'
#' Removes any recommendation already represented in `data/decisions/ledger/`
#' (matched on sport, country, sex, match_date, home_team, away_team, market,
#' outcome, line). The placer is the only writer to the ledger, so this is
#' the canonical "haven't placed it yet" filter.
#'
#' @param recs Tibble from `load_recommendations()`.
#' @param root Data root.
#' @return Tibble of recommendations not yet placed.
#' @export
dedup_against_ledger <- function(recs, root) {
  if (nrow(recs) == 0L) return(recs)

  led <- tryCatch(
    read_table("ledger", root = root),
    error = function(e) tibble::tibble()
  )
  if (nrow(led) == 0L) return(recs)

  key_cols <- c("sport", "country", "sex", "match_date",
                "home_team", "away_team", "market", "outcome", "line")
  if (!all(key_cols %in% names(led))) return(recs)

  rec_key <- do.call(paste, c(recs[, key_cols], sep = "||"))
  led_key <- do.call(paste, c(led[, key_cols], sep = "||"))
  recs[!rec_key %in% led_key, , drop = FALSE]
}

#' @keywords internal
#' @noRd
empty_recommendations_for_placement <- function() {
  tibble::tibble(
    run_id      = as.POSIXct(character(), tz = "UTC"),
    sport       = character(), country = character(), sex = character(),
    match_date  = as.Date(character()),
    home_team   = character(), away_team = character(),
    market      = character(), outcome = character(),
    line        = numeric(), p = numeric(), odds = numeric(),
    ev          = numeric(), kelly = numeric(), bet_amount = numeric()
  )
}
```

### Step 4: Verify + commit

```bash
Rscript -e 'roxygen2::roxygenise(); roxygen2::update_collate(".")'
Rscript -e 'devtools::test(filter = "placer-load")'
```

Expected: 5 passes.

```bash
git add R/placer-load.R NAMESPACE DESCRIPTION \
        tests/testthat/fixtures/placer/ \
        tests/testthat/test-placer-load.R
git commit -m "feat: R/placer-load.R — Parquet recs + ledger dedup

Replaces _legacy/lengjan-bets/R/pipeline.R::load_recommendations
(read Sports/recommendations.csv) and dedup_against_ledger (glob
all per-league bets_log.csv) with single read_table() calls
against the unified Parquet recommendations + ledger tables.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>"
```

---

## Task 3: `R/placer-ledger.R` — Parquet append + CSV dual-write (TDD)

**Files:**

- Create: `R/placer-ledger.R`
- Create: `tests/testthat/test-placer-ledger.R`

**Purpose:** Append a placed-bet row to `data/decisions/ledger/` Parquet (using `upsert_table` to preserve existing rows) AND, during cutover, dual-write to the legacy per-league `_legacy/sports/{sport}/{country}/history/bets_log.csv` so a regression in either format is recoverable. Plan 6 drops the CSV dual-write.

**Signature:**

```r
append_to_ledger(bet_row, root, dual_write_csv = TRUE,
                 legacy_root = "_legacy/sports") -> invisible(NULL)
```

`bet_row` must match `schemas()$ledger`. Required cols: placed_at, match_date, sport, country, sex, home_team, away_team, market, outcome, line, odds_placed, p, kelly, bet_amount, settled, win, pnl.

### Step 1: Failing tests

```r
# tests/testthat/test-placer-ledger.R

mk_ledger_row <- function(home = "KR", odds_placed = 1.85, bet_amount = 1180) {
  tibble::tibble(
    placed_at = Sys.time(),
    match_date = as.Date("2026-04-26"),
    sport = "football", country = "iceland", sex = "male",
    home_team = home, away_team = "FH",
    market = "moneyline", outcome = "home", line = NA_real_,
    odds_placed = odds_placed, p = 0.55, kelly = 0.05,
    bet_amount = bet_amount,
    settled = FALSE, win = NA, pnl = NA_real_
  )
}

test_that("append_to_ledger writes a new bet to Parquet", {
  tmp <- withr::local_tempdir()
  bet <- mk_ledger_row()
  append_to_ledger(bet, root = tmp, dual_write_csv = FALSE)

  back <- read_table("ledger", root = tmp)
  expect_equal(nrow(back), 1L)
  expect_equal(back$home_team, "KR")
  expect_equal(back$bet_amount, 1180)
})

test_that("append_to_ledger preserves existing rows on append", {
  tmp <- withr::local_tempdir()
  append_to_ledger(mk_ledger_row(home = "KR"),  root = tmp, dual_write_csv = FALSE)
  append_to_ledger(mk_ledger_row(home = "Fram"), root = tmp, dual_write_csv = FALSE)

  back <- read_table("ledger", root = tmp)
  expect_equal(nrow(back), 2L)
  expect_setequal(back$home_team, c("KR", "Fram"))
})

test_that("append_to_ledger CSV dual-write writes the legacy path", {
  tmp <- withr::local_tempdir()
  legacy <- withr::local_tempdir()
  bet <- mk_ledger_row()
  append_to_ledger(bet, root = tmp, dual_write_csv = TRUE,
                   legacy_root = legacy)

  csv_path <- file.path(legacy, "football", "iceland", "history", "bets_log.csv")
  expect_true(file.exists(csv_path))
  back <- readr::read_csv(csv_path, show_col_types = FALSE)
  expect_equal(nrow(back), 1L)
})

test_that("append_to_ledger CSV dual-write APPENDS to an existing file", {
  tmp <- withr::local_tempdir()
  legacy <- withr::local_tempdir()
  csv_dir <- file.path(legacy, "football", "iceland", "history")
  dir.create(csv_dir, recursive = TRUE)
  csv_path <- file.path(csv_dir, "bets_log.csv")

  pre_existing <- mk_ledger_row(home = "Stjarnan")
  readr::write_csv(pre_existing, csv_path)

  append_to_ledger(mk_ledger_row(home = "KR"),
                   root = tmp, dual_write_csv = TRUE,
                   legacy_root = legacy)

  back <- readr::read_csv(csv_path, show_col_types = FALSE)
  expect_equal(nrow(back), 2L)
  expect_setequal(back$home_team, c("Stjarnan", "KR"))
})
```

### Step 2: Implement

```r
# R/placer-ledger.R

#' @include storage.R
NULL

#' Append a placed-bet row to the ledger.
#'
#' Writes to `data/decisions/ledger/` Parquet via `upsert_table` (preserves
#' all existing rows) and, when `dual_write_csv = TRUE`, also appends to
#' `<legacy_root>/<sport>/<country>/history/bets_log.csv` for cutover safety.
#'
#' This is the only writer to the ledger.
#'
#' @param bet_row Single-row tibble matching `schemas()$ledger`.
#' @param root Data root for the Parquet ledger.
#' @param dual_write_csv Default TRUE during Plan 5 cutover. Plan 6 drops.
#' @param legacy_root Root path for legacy CSV writes (default `_legacy/sports`).
#' @return invisible(NULL)
#' @export
append_to_ledger <- function(bet_row, root,
                             dual_write_csv = TRUE,
                             legacy_root = file.path(root, "..", "_legacy",
                                                     "sports")) {
  stopifnot(nrow(bet_row) == 1L)
  required <- c("placed_at", "match_date", "sport", "country", "sex",
                "home_team", "away_team", "market", "outcome", "line",
                "odds_placed", "p", "kelly", "bet_amount",
                "settled", "win", "pnl")
  missing <- setdiff(required, names(bet_row))
  if (length(missing) > 0L) {
    stop("append_to_ledger: bet_row missing columns: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  upsert_table(bet_row, "ledger", root = root)

  if (isTRUE(dual_write_csv)) {
    csv_dir <- file.path(legacy_root,
                         bet_row$sport[[1]], bet_row$country[[1]],
                         "history")
    dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)
    csv_path <- file.path(csv_dir, "bets_log.csv")

    if (file.exists(csv_path)) {
      existing <- readr::read_csv(csv_path, show_col_types = FALSE)
      combined <- dplyr::bind_rows(existing, bet_row)
    } else {
      combined <- bet_row
    }
    readr::write_csv(combined, csv_path)
  }

  invisible(NULL)
}
```

### Step 3: Verify + commit

```bash
Rscript -e 'devtools::test(filter = "placer-ledger")'
```

Expected: 4 passes.

```bash
git add R/placer-ledger.R NAMESPACE DESCRIPTION tests/testthat/test-placer-ledger.R
git commit -m "feat: R/placer-ledger.R — Parquet ledger append + CSV dual-write

The placer is the only writer to the ledger. append_to_ledger
upserts the row into data/decisions/ledger/ Parquet and, during
cutover, also appends to _legacy/sports/{sport}/{country}/history/
bets_log.csv. Plan 6 drops the CSV write once a week of agreement.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>"
```

---

## Task 4: `R/placer-login.R` — Chromote auth flow (smoke-test)

**Files:**

- Create: `R/placer-login.R`
- Create: `tests/testthat/test-placer-login.R`

**Purpose:** Port `_legacy/lengjan-bets/R/login.R` (137 lines) verbatim modulo `box::use` removal. Establishes a Chromote session, navigates to Lengjan login, fills credentials from `LENGJAN_USER` / `LENGJAN_PASS` env vars, handles the 2FA prompt if needed, and returns the live ChromoteSession ready for navigation.

**Signature:**

```r
chromote_login(headless = TRUE, timeout_s = 30) -> ChromoteSession | stop()
```

The function is gated on env vars: missing `LENGJAN_USER` / `LENGJAN_PASS` produces a clean error before opening Chrome. Tests skip on missing env.

### Step 1-5: Port verbatim, drop box::use, namespace-prefix chromote calls

Direct port. Skim `_legacy/.../R/login.R` and copy with these adaptations:
- Replace `box::use(chromote[ChromoteSession], ...)` with `chromote::ChromoteSession$new(...)`.
- Replace any `box::use(R/...)` cross-references.
- Use `cli::cli_*` for status messages (already done in legacy).

The 2FA flow uses an interactive prompt — keep it as `readline()` or convert to `cli::cli_input_text()` if `{cli}` ≥ 3.5 is available. The legacy uses a custom prompt; preserve.

### Tests (skip-friendly)

```r
test_that("chromote_login errors clearly when env vars are unset", {
  withr::local_envvar(LENGJAN_USER = "", LENGJAN_PASS = "")
  expect_error(
    chromote_login(),
    "LENGJAN_USER|LENGJAN_PASS|env"
  )
})

test_that("chromote_login skips when chromote is unavailable", {
  skip_if_not_installed("chromote")
  skip_if(Sys.getenv("LENGJAN_USER") == "", "LENGJAN_USER unset; skip live login")
  # Optional live smoke if user has chrome + credentials — leave as skip
  # by default; uncomment locally.
  testthat::skip("live login is opt-in")
})
```

### Commit

```bash
git add R/placer-login.R NAMESPACE DESCRIPTION tests/testthat/test-placer-login.R
git commit -m "feat: R/placer-login.R — chromote auth flow (port from _legacy)

Verbatim port of _legacy/lengjan-bets/R/login.R modulo box::use
removal. Errors fast on missing LENGJAN_USER / LENGJAN_PASS env
vars before opening Chrome. Live login is opt-in via local
.Renviron + manual test invocation; default test suite skips.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>"
```

---

## Task 5: `R/placer-navigate.R` — find match + extract Lengjan ID

**Files:**

- Create: `R/placer-navigate.R`
- Create: `tests/testthat/fixtures/placer/lengjan_match_html.txt`
- Create: `tests/testthat/test-placer-navigate.R`

**Purpose:** Port `_legacy/lengjan-bets/R/navigate.R` (177 lines). Two responsibilities:

1. Open a competition page in the live Chromote session, find the row matching `(home_team, away_team, match_date)`, extract the Lengjan match ID from the row's `data-match-id` (or whatever attribute the live HTML uses).
2. Navigate to the match-detail page so subsequent place_one_bet calls have the right DOM.

The DOM-parse logic (extract match ID from a row) is testable from a captured HTML fixture; the navigation itself requires a live session.

### Step 1: Capture fixture

```bash
# In a one-off browser-controlled session, save the relevant inner-HTML to a file.
# Or: copy a competition-page HTML block from a recent legacy run trace.
```

The fixture is a snippet — about 40-100 lines of HTML — that includes the row attributes the legacy code extracts. Look at `_legacy/.../R/navigate.R` for the exact selectors used.

### Step 2-5: Port + extract testable seam

Split:
- `extract_match_id_from_html(html, home_lengjan_name, away_lengjan_name, match_date)` — pure parser, fixture-tested.
- `navigate_to_match(session, league, home_team, away_team, match_date)` — chromote-bound, smoke-test or skip.

### Commit

`feat: R/placer-navigate.R — match navigation + ID extraction`

---

## Task 6: `R/placer-place.R` — bet placement state machine + P3/P4 (TDD seams)

**Files:**

- Create: `R/placer-place.R`
- Create: `tests/testthat/test-placer-place.R`

**Purpose:** Port `_legacy/lengjan-bets/R/place_bet.R` (722 lines — the biggest file). The legacy is a state machine: navigate to odds button → click → read live odds from DOM → P4 (still +EV?) → P3 (recompute kelly if odds drifted >1%) → enter stake → click "Kaupa" → confirm placement.

**Strategy:** Preserve the state-machine flow verbatim. Extract three testable functions:

1. `recompute_kelly_at_actual_odds(p, odds_actual, kelly_frac, current_pool, min_bet)` — returns the Kelly fraction + ISK amount given the actual odds from DOM. Pure function, testable.
2. `passes_p4_check(p, odds_actual, ev_threshold)` — returns TRUE iff bet is still +EV. Pure.
3. `parse_actual_odds_from_dom(html)` — extracts the odds number from the cell. Pure parser, fixture-testable.

The Chromote-bound public function is:

```r
place_one_bet(session, bet, league, bankroll, dry_run = TRUE,
              ev_threshold = 0.0, p3_drift_pct = 0.01) ->
  list(status = c("placed", "rejected_p3", "rejected_p4", "dry_run", "error"),
       odds_actual = numeric(),
       bet_amount_actual = numeric(),
       message = character())
```

`status` enumerates terminal states; the orchestrator (Task 7) uses it to decide whether to call `append_to_ledger`.

### Step 1: Failing tests for the testable seams

```r
test_that("recompute_kelly_at_actual_odds preserves when odds unchanged", {
  out <- recompute_kelly_at_actual_odds(
    p = 0.55, odds_actual = 1.85, kelly_frac = 0.10,
    current_pool = 23610, min_bet = 200
  )
  expect_gt(out$kelly, 0)
  expect_gte(out$bet_amount, 200)
})

test_that("recompute_kelly_at_actual_odds returns 0 stake when ev <= 0", {
  out <- recompute_kelly_at_actual_odds(
    p = 0.50, odds_actual = 1.50, kelly_frac = 0.10,
    current_pool = 23610, min_bet = 200
  )
  expect_equal(out$bet_amount, 0)
})

test_that("passes_p4_check accepts +EV", {
  expect_true(passes_p4_check(p = 0.55, odds_actual = 1.85, ev_threshold = 0))
})

test_that("passes_p4_check rejects -EV", {
  expect_false(passes_p4_check(p = 0.50, odds_actual = 1.80, ev_threshold = 0))
})

test_that("parse_actual_odds_from_dom extracts numeric from a captured cell", {
  html <- '<button class="odds-btn"><span class="odds">1.85</span></button>'
  expect_equal(parse_actual_odds_from_dom(html), 1.85)
})
```

### Step 2-7: Implement the testable seams + port the chromote state machine verbatim

Most lines are the chromote state machine — keep functionally identical to legacy. The pure-function extractions above are the only material refactor.

### Commit

`feat: R/placer-place.R — bet placement state machine + P3/P4`

---

## Task 7: `R/placer-pipeline.R` — orchestrator

**Files:**

- Create: `R/placer-pipeline.R`
- Create: `tests/testthat/test-placer-pipeline.R`

**Purpose:** Port `_legacy/lengjan-bets/R/pipeline.R::run_bets()` (479 legacy lines, ~half of which becomes Tasks 2/3/6 above). The remainder is the actual orchestrator: validate → load → dedup → login → for each bet { navigate → place → log if placed } → summarise.

**Signature:**

```r
place_bets(leagues = NULL, dry_run = TRUE, interactive = TRUE,
           today_only = FALSE, target_date = NULL,
           headless = TRUE, root = here::here("data"),
           dual_write_csv = TRUE) -> tibble
```

Returns a per-bet results tibble with `status` from `place_one_bet`.

### Tests (mock Chromote)

```r
test_that("place_bets returns 0-row tibble when no recommendations match", {
  tmp <- withr::local_tempdir()
  out <- place_bets(target_date = as.Date("2050-01-01"), root = tmp)
  expect_equal(nrow(out), 0L)
})

test_that("place_bets dry-run path skips chromote entirely", {
  # With dry_run = TRUE and no bets after dedup, there's no need to login.
  # Mock chromote_login to assert it is NOT called.
  testthat::local_mocked_bindings(
    chromote_login = function(...) stop("Should not have logged in"),
    .package = "sports"
  )
  tmp <- withr::local_tempdir()
  out <- place_bets(target_date = as.Date("2050-01-01"),
                    root = tmp, dry_run = TRUE)
  expect_equal(nrow(out), 0L)
})
```

### Commit

`feat: R/placer-pipeline.R — place_bets() orchestrator`

---

## Task 8: `R/placer-preview.R` — preview without browser

**Files:**

- Create: `R/placer-preview.R`
- Create: `tests/testthat/test-placer-preview.R`

**Purpose:** Port `_legacy/lengjan-bets/preview.R` (172 lines). Loads recommendations, dedups, prints a per-bet preview table — never opens chromote. Useful for "what would I bet today?" without committing real money.

**Signature:**

```r
preview_pending(leagues = NULL, today_only = FALSE,
                target_date = NULL, root = here::here("data")) -> tibble
```

Prints a summary via cli; returns the same tibble for programmatic use. The printing is tested by capturing stdout.

### Commit

`feat: R/placer-preview.R — preview_pending() (no browser)`

---

## Task 9: CLI entrypoints `scripts/{place_bets,preview_bets}.R`

**Files:**

- Create: `scripts/place_bets.R`
- Create: `scripts/preview_bets.R`

**Purpose:** CLI replacements for `_legacy/lengjan-bets/run.R` + `preview.R`. Same CLI flags:

```bash
Rscript scripts/preview_bets.R                           # dry preview, all leagues
Rscript scripts/preview_bets.R --league football_iceland
Rscript scripts/preview_bets.R --today

Rscript scripts/place_bets.R                  # dry-run, all leagues
Rscript scripts/place_bets.R --live           # places, with per-bet confirmation
Rscript scripts/place_bets.R --live --no-confirm
Rscript scripts/place_bets.R --live --today
Rscript scripts/place_bets.R --live --league football_iceland
Rscript scripts/place_bets.R --live --show-browser
```

### Scripts

```r
#!/usr/bin/env Rscript
# scripts/place_bets.R — CLI entrypoint for the placer.

suppressPackageStartupMessages(devtools::load_all(here::here()))

args <- commandArgs(trailingOnly = TRUE)
get_flag <- function(name) any(args == paste0("--", name))
get_arg <- function(name) {
  i <- which(args == paste0("--", name))
  if (length(i) == 0L) NULL else args[[i + 1L]]
}

dry_run     <- !get_flag("live")
interactive <- !get_flag("no-confirm")
today_only  <- get_flag("today")
headless    <- !get_flag("show-browser")
leagues     <- get_arg("league")
target_date <- if (!is.null(get_arg("date"))) as.Date(get_arg("date")) else NULL

results <- place_bets(
  leagues = leagues,
  dry_run = dry_run,
  interactive = interactive,
  today_only = today_only,
  target_date = target_date,
  headless = headless
)

if (nrow(results) > 0L && "status" %in% names(results)) {
  cat("\n=== Results ===\n")
  print(results[, c("home_team", "away_team", "market", "outcome",
                    "odds", "bet_amount", "status")])
}
```

`scripts/preview_bets.R` is even shorter — just calls `preview_pending()`.

### Commit

`feat: scripts/{place_bets,preview_bets}.R — CLI entrypoints`

---

## Task 10: CI safety gate (test that no workflow runs the placer)

**Files:**

- Create: `tests/testthat/test-placer-ci-isolation.R`

**Purpose:** Spec §6 lists "Placer isolation slip" as a real risk: a future contributor could wire the placer into CI. Mitigate via a test that grep's `.github/workflows/*.yml` and fails the build if any line references `R/placer-`, `placer_pipeline`, `place_bets`, or `LENGJAN_*`.

```r
test_that("no GitHub Actions workflow references the placer", {
  workflow_dir <- here::here(".github", "workflows")
  if (!dir.exists(workflow_dir)) {
    testthat::skip("no .github/workflows yet")
  }
  yml_files <- list.files(workflow_dir, pattern = "\\.ya?ml$", full.names = TRUE)
  if (length(yml_files) == 0L) {
    testthat::skip("no workflow files yet")
  }

  forbidden <- c("R/placer-", "placer_pipeline", "place_bets",
                 "LENGJAN_USER", "LENGJAN_PASS")
  for (f in yml_files) {
    contents <- readLines(f, warn = FALSE)
    for (token in forbidden) {
      hit <- grep(token, contents, fixed = TRUE)
      if (length(hit) > 0L) {
        fail(paste0("CI workflow ", basename(f),
                    " references placer token \"", token,
                    "\" at line ", hit[1L],
                    ". Placer must remain local-only."))
      }
    }
  }
  expect_true(TRUE)
})
```

### Commit

`test: CI safety gate — no workflow references the placer`

---

## Task 11: CLAUDE.md update + Plan 5 → ✅

**Files:**

- Modify: `CLAUDE.md`

**Purpose:**

1. Status table — Plan 5 row → "✅ Complete — `R/placer-*.R` ports `_legacy/lengjan-bets/`, dual-writes ledger CSV+Parquet during cutover".
2. Directory tree — add the 8 R/placer-*.R files + 2 scripts/.
3. "Local-only subsystem" section — expand with concrete details (LENGJAN_USER env, CI safety gate test, dual-write status).
4. Test count assertion comment — bump to "400+".
5. Add a "Placer" section under Conventions:

```markdown
### Placer (local-only)

- `Rscript scripts/place_bets.R` is the public entrypoint. Default is dry-run;
  `--live` actually places. Always reads `LENGJAN_USER` / `LENGJAN_PASS` from
  `.Renviron` (template at `.Renviron.example`).
- The placer is **never** executed on CI. The `test-placer-ci-isolation.R`
  test fails the build if any `.github/workflows/*.yml` references
  `R/placer-`, `placer_pipeline`, `place_bets`, or `LENGJAN_*`.
- During Plan 5 cutover, every placement dual-writes both the unified
  Parquet ledger (`data/decisions/ledger/`) and the legacy per-league
  CSV (`_legacy/sports/{sport}/{country}/history/bets_log.csv`). Plan 6
  drops the CSV writes after a week of agreement.
- P1–P4 placement rules (only-writer, actual-odds, kelly-recompute, EV
  reject) are preserved verbatim from `_legacy/lengjan-bets/`.
```

### Commit

`docs: CLAUDE.md — Plan 5 (placer) complete`

---

## Final validation

- [ ] **Step 1: Full test suite**

```bash
cd /Users/brynjolfurjonsson/sports
Rscript -e 'devtools::test()'
```

Expected: 0 fail, 8+ skips (chromote-bound tests skip), pass count up by ~25-35 from Plan 4's 387 → ~415-420.

- [ ] **Step 2: Manual dry-run smoke test (local only)**

```bash
Rscript scripts/preview_bets.R --today
Rscript scripts/place_bets.R --today           # dry-run, no chromote
```

Both should print results without error. The `place_bets.R` dry-run will spin up Chromote if there are pending bets — that's expected; just verify it navigates and logs without committing.

- [ ] **Step 3: Push (with explicit user authorisation)**

```bash
git push origin main
```

---

## What this plan achieves

- Local-only Lengjan automation lives in the unified package.
- Reads recommendations from Parquet, dedups against the unified ledger.
- Real-money path (`--live`) preserved verbatim from production.
- Cutover safety: dual-write to CSV during Plan 5; Plan 6 drops the CSV.
- CI-isolation gate prevents accidental future wiring.

## Risks & mitigations

- **Real-money regression.** The state machine in `place_one_bet` is the load-bearing path. Mitigation: port verbatim, preserve all P1–P4 checks, manual --live smoke-test on a single bet before declaring Plan 5 done.
- **Lengjan UI deploy breaks selectors.** The legacy code's CSS selectors are hashed-class-name based. Mitigation: `extract_match_id_from_html` + `parse_actual_odds_from_dom` are tested against captured fixtures, so a selector regression surfaces as a unit-test failure.
- **CI accidentally runs the placer.** Mitigation: `test-placer-ci-isolation.R` greps workflow files for forbidden tokens.
- **Dual-write divergence.** During cutover, the Parquet ledger and CSV ledger could diverge if one write succeeds and the other fails. Mitigation: `append_to_ledger` writes Parquet first (the canonical source); CSV is best-effort. After Plan 6, only Parquet is written.
- **Chromote 2FA prompt blocks unattended runs.** The legacy login flow already handles this with `readline()`. Mitigation: keep that prompt; document in CLAUDE.md that `--no-confirm` doesn't bypass 2FA.
