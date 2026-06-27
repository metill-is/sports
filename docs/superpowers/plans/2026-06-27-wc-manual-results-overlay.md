# WC Manual Results Overlay + Local Refresh — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Let the operator inject match scores martj42 hasn't published yet, re-fit, re-forecast, and publish the WC forecast on demand — locally, with one command.

**Architecture:** A committed, human-editable overlay CSV (`data/wc/manual_results.csv`) is merged onto martj42's `NA`-score rows *before* the played/scheduled split inside `wc_ingest_internationals()`, so injected matches flow into the facts store and get fit with zero downstream changes. A pure helper does the merge (fail-loud on typos, self-drain once martj42 catches up); a second pure helper lists which fixtures need filling; a shell wrapper orchestrates ingest→fit→forecast→preview→publish→trigger-platform-pull.

**Tech Stack:** R package (devtools/testthat 3 / roxygen2 / readr / dplyr / arrow / cli), Bash, `gh` CLI. Spec: `docs/superpowers/specs/2026-06-27-wc-manual-results-overlay-design.md`.

## Global Constraints

- All code changes live in `~/sports` (`metill-is/sports`) on branch `wc-manual-overlay`. The one metill-platform doc tweak (Task 6b) is a separate commit in `~/metill-platform`.
- testthat edition 3 (`Config/testthat/edition: 3`); run tests with `devtools::test()`.
- New exported functions require `devtools::document()` to refresh `NAMESPACE` + `man/`.
- The martj42 raw schema (read straight from `read_csv`, before the `wc-ingest.R` transmute) has columns `date, home_team, away_team, home_score, away_score, tournament, city, country, neutral`. The merge/list helpers operate on THIS schema — `tournament`/`date`, not `division`/`match_date`.
- British/international spelling in comments.
- Commit-message style: `feat(wc):` / `test(wc):` / `docs(wc):` (mirrors the repo's `data(wc):`).
- The manual path **must not** advance `data/wc/martj42_pointer.txt`, and the wrapper's git-add set is exactly: `data/publish/world_cup`, `data/facts/results/sport=football/country=world`, `data/facts/schedules/sport=football/country=world`, `data/wc/manual_results.csv` (NOT `data/wc/accountability` — `forecast.R`/`publish_world_cup()` do not write it).

---

### Task 1: `wc_apply_manual_results()` — the merge core

**Files:**
- Modify: `R/wc-ingest.R` (add exported function above `wc_ingest_internationals`)
- Create: `tests/testthat/test-wc-manual-results.R`
- Regenerate: `NAMESPACE`, `man/wc_apply_manual_results.Rd` (via `devtools::document()`)

**Interfaces:**
- Consumes: nothing (pure function).
- Produces: `wc_apply_manual_results(raw, overlay) -> data.frame`. `raw` = martj42-schema tibble; `overlay` = tibble with `date` (Date), `home_team` (chr), `away_team` (chr), `home_score` (int), `away_score` (int). Returns `raw` with matched `NA`-score rows filled. Aborts on a 0-match or >1-match overlay row; warns and keeps martj42 when the matched row is already scored and differs.

- [ ] **Step 1: Write the failing tests**

Create `tests/testthat/test-wc-manual-results.R`:

```r
mk_raw <- function() {
  tibble::tibble(
    date = as.Date(c("2026-06-26", "2026-06-26", "2026-06-25")),
    home_team = c("Algeria", "Jordan", "Spain"),
    away_team = c("Austria", "Argentina", "Brazil"),
    home_score = c(NA_integer_, NA_integer_, 1L),
    away_score = c(NA_integer_, NA_integer_, 2L),
    tournament = "FIFA World Cup",
    city = "x", country = "US", neutral = TRUE
  )
}

test_that("fills an NA-score row matched by key", {
  overlay <- tibble::tibble(
    date = as.Date("2026-06-26"), home_team = "Algeria",
    away_team = "Austria", home_score = 3L, away_score = 0L
  )
  out <- wc_apply_manual_results(mk_raw(), overlay)
  row <- out[out$home_team == "Algeria", ]
  expect_equal(row$home_score, 3L)
  expect_equal(row$away_score, 0L)
})

test_that("aborts on an overlay row that matches no fixture (typo)", {
  overlay <- tibble::tibble(
    date = as.Date("2026-06-26"), home_team = "Algerie", # typo
    away_team = "Austria", home_score = 3L, away_score = 0L
  )
  expect_error(wc_apply_manual_results(mk_raw(), overlay), "matches no martj42 fixture")
})

test_that("martj42 wins once it has a score; a differing overlay warns (drain)", {
  overlay <- tibble::tibble(
    date = as.Date("2026-06-25"), home_team = "Spain",
    away_team = "Brazil", home_score = 5L, away_score = 5L # differs from 1-2
  )
  expect_warning(out <- wc_apply_manual_results(mk_raw(), overlay), "already reports")
  row <- out[out$home_team == "Spain", ]
  expect_equal(row$home_score, 1L) # martj42 unchanged
  expect_equal(row$away_score, 2L)
})

test_that("no warning when overlay equals an already-scored martj42 row", {
  overlay <- tibble::tibble(
    date = as.Date("2026-06-25"), home_team = "Spain",
    away_team = "Brazil", home_score = 1L, away_score = 2L # same as martj42
  )
  expect_no_warning(out <- wc_apply_manual_results(mk_raw(), overlay))
  expect_equal(out$home_score[out$home_team == "Spain"], 1L)
})

test_that("empty overlay is a no-op", {
  empty <- tibble::tibble(
    date = as.Date(character()), home_team = character(),
    away_team = character(), home_score = integer(), away_score = integer()
  )
  expect_identical(wc_apply_manual_results(mk_raw(), empty), mk_raw())
})

test_that("an ambiguous (duplicate) key aborts", {
  raw <- dplyr::bind_rows(mk_raw(), mk_raw()[1, ]) # duplicate Algeria-Austria
  overlay <- tibble::tibble(
    date = as.Date("2026-06-26"), home_team = "Algeria",
    away_team = "Austria", home_score = 3L, away_score = 0L
  )
  expect_error(wc_apply_manual_results(raw, overlay), "ambiguous")
})
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `Rscript -e 'devtools::test(filter = "wc-manual-results")'`
Expected: FAIL — `could not find function "wc_apply_manual_results"`.

- [ ] **Step 3: Implement the function**

In `R/wc-ingest.R`, add above `wc_ingest_internationals` (after the `NULL` at the top):

```r
#' Patch martj42 results with operator-supplied scores.
#'
#' Fills the `home_score`/`away_score` of `raw` rows whose scores are still `NA`
#' from an overlay keyed on `(date, home_team, away_team)`, for use when martj42
#' lags behind played matches. martj42 stays canonical: once it carries a real
#' score the overlay row is ignored (and, if it disagrees, a warning prompts the
#' operator to prune it, so the overlay self-drains). An overlay row that matches
#' no fixture aborts loudly — a silent no-op would hide a team-name typo.
#'
#' @param raw martj42-schema data frame (`date, home_team, away_team,
#'   home_score, away_score, ...`), as read by [wc_ingest_internationals()].
#' @param overlay Data frame with `date` (Date), `home_team`, `away_team`,
#'   `home_score`, `away_score`. May be empty (no-op).
#' @return `raw` with matched `NA`-score rows filled.
#' @export
wc_apply_manual_results <- function(raw, overlay) {
  if (is.null(overlay) || nrow(overlay) == 0L) {
    return(raw)
  }
  sep <- ""
  raw_key <- paste(raw$date, raw$home_team, raw$away_team, sep = sep)
  n_filled <- 0L
  for (i in seq_len(nrow(overlay))) {
    o <- overlay[i, , drop = FALSE]
    hits <- which(raw_key == paste(o$date, o$home_team, o$away_team, sep = sep))
    if (length(hits) == 0L) {
      cli::cli_abort(c(
        "Manual overlay row matches no martj42 fixture.",
        "x" = "{o$date} {o$home_team} vs {o$away_team}",
        "i" = "Check the team-name spelling (run scripts/wc/list_missing.R)."
      ))
    }
    if (length(hits) > 1L) {
      cli::cli_abort(
        "Overlay row {o$date} {o$home_team} vs {o$away_team} is ambiguous \\
        ({length(hits)} martj42 rows match)."
      )
    }
    j <- hits[[1L]]
    eh <- raw$home_score[[j]]
    ea <- raw$away_score[[j]]
    if (!is.na(eh) && !is.na(ea)) {
      if (!identical(as.integer(eh), as.integer(o$home_score)) ||
        !identical(as.integer(ea), as.integer(o$away_score))) {
        cli::cli_warn(c(
          "martj42 already reports a different score; keeping martj42.",
          "i" = "{o$date} {o$home_team}: {eh}-{ea} (martj42) vs \\
            {o$home_score}-{o$away_score} (overlay). Remove it from manual_results.csv."
        ))
      }
      next
    }
    raw$home_score[[j]] <- as.integer(o$home_score)
    raw$away_score[[j]] <- as.integer(o$away_score)
    n_filled <- n_filled + 1L
  }
  if (n_filled > 0L) {
    cli::cli_alert_success(
      "Applied {n_filled} manual result{?s} martj42 hasn't published yet."
    )
  }
  raw
}
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `Rscript -e 'devtools::test(filter = "wc-manual-results")'`
Expected: PASS — 6 tests, 0 failures.

- [ ] **Step 5: Regenerate docs/NAMESPACE**

Run: `Rscript -e 'devtools::document()'`
Expected: writes `man/wc_apply_manual_results.Rd`, adds `export(wc_apply_manual_results)` to `NAMESPACE`.

- [ ] **Step 6: Commit**

```bash
git add R/wc-ingest.R tests/testthat/test-wc-manual-results.R NAMESPACE man/wc_apply_manual_results.Rd
git commit -m "feat(wc): wc_apply_manual_results — overlay martj42 NA scores"
```

---

### Task 2: `wc_list_unscored_fixtures()` — the scaffold helper

**Files:**
- Modify: `R/wc-ingest.R` (add exported function near Task 1's)
- Create: `tests/testthat/test-wc-list-unscored.R`
- Regenerate: `NAMESPACE`, `man/wc_list_unscored_fixtures.Rd`

**Interfaces:**
- Consumes: nothing (pure function).
- Produces: `wc_list_unscored_fixtures(raw, as_of = Sys.Date()) -> data.frame` with columns `date, home_team, away_team, home_score, away_score` (scores all `NA`), containing only WC-finals fixtures with `date <= as_of` and an `NA` score.

- [ ] **Step 1: Write the failing test**

Create `tests/testthat/test-wc-list-unscored.R`:

```r
test_that("lists only past-dated, unscored WC-finals fixtures", {
  raw <- tibble::tibble(
    date = as.Date(c("2026-06-25", "2026-06-28", "2026-06-25", "2026-06-25")),
    home_team = c("Spain", "Panama", "Brazil", "Iceland"),
    away_team = c("Brazil", "England", "Croatia", "Norway"),
    home_score = c(NA_integer_, NA_integer_, 2L, NA_integer_),
    away_score = c(NA_integer_, NA_integer_, 1L, NA_integer_),
    tournament = c("FIFA World Cup", "FIFA World Cup", "FIFA World Cup", "UEFA Euro"),
    city = "x", country = "US", neutral = TRUE
  )
  out <- wc_list_unscored_fixtures(raw, as_of = as.Date("2026-06-26"))
  # Spain-Brazil: past + NA + WC  -> kept
  # Panama-England: future (06-28) -> dropped
  # Brazil-Croatia: already scored -> dropped
  # Iceland-Norway: not WC finals  -> dropped
  expect_equal(nrow(out), 1L)
  expect_equal(out$home_team, "Spain")
  expect_true(all(is.na(out$home_score)))
  expect_setequal(
    names(out), c("date", "home_team", "away_team", "home_score", "away_score")
  )
})
```

- [ ] **Step 2: Run to verify it fails**

Run: `Rscript -e 'devtools::test(filter = "wc-list-unscored")'`
Expected: FAIL — `could not find function "wc_list_unscored_fixtures"`.

- [ ] **Step 3: Implement the function**

In `R/wc-ingest.R`, add next to `wc_apply_manual_results`:

```r
#' List WC fixtures that should be played but martj42 hasn't scored.
#'
#' Operator scaffold for `data/wc/manual_results.csv`: the WC-finals fixtures
#' with a kickoff on/before `as_of` and a still-`NA` score. Operates on the raw
#' martj42 schema (`tournament`/`date`), the same `raw` [wc_apply_manual_results()]
#' receives.
#'
#' @param raw martj42-schema data frame.
#' @param as_of Latest kickoff date to include. Default today.
#' @return Data frame `date, home_team, away_team, home_score, away_score` with
#'   `NA` scores, ready to paste into the overlay.
#' @importFrom rlang .data
#' @export
wc_list_unscored_fixtures <- function(raw, as_of = Sys.Date()) {
  raw |>
    dplyr::filter(
      .data$tournament == "FIFA World Cup",
      format(.data$date, "%Y") == "2026",
      .data$date <= as_of,
      is.na(.data$home_score) | is.na(.data$away_score)
    ) |>
    dplyr::transmute(
      date = .data$date,
      home_team = .data$home_team,
      away_team = .data$away_team,
      home_score = NA_integer_,
      away_score = NA_integer_
    )
}
```

- [ ] **Step 4: Run to verify it passes**

Run: `Rscript -e 'devtools::test(filter = "wc-list-unscored")'`
Expected: PASS — 1 test, 0 failures.

- [ ] **Step 5: Regenerate docs/NAMESPACE**

Run: `Rscript -e 'devtools::document()'`
Expected: writes `man/wc_list_unscored_fixtures.Rd`, adds the export.

- [ ] **Step 6: Commit**

```bash
git add R/wc-ingest.R tests/testthat/test-wc-list-unscored.R NAMESPACE man/wc_list_unscored_fixtures.Rd
git commit -m "feat(wc): wc_list_unscored_fixtures — scaffold the overlay rows"
```

---

### Task 3: Wire the overlay into ingest + commit the overlay file

**Files:**
- Modify: `R/wc-ingest.R` (`wc_ingest_internationals` signature + body)
- Create: `data/wc/manual_results.csv` (committed, header + comments only)
- Create: `tests/testthat/test-wc-ingest-overlay.R`
- Regenerate: `man/wc_ingest_internationals.Rd`

**Interfaces:**
- Consumes: `wc_apply_manual_results()` (Task 1); `read_table(table, root, filter)` (existing, `R/storage.R:384`) for the test.
- Produces: `wc_ingest_internationals(csv_path, window_start, min_team_matches, root, manual_overlay_path)` — same as today plus `manual_overlay_path` (default `here::here("data","wc","manual_results.csv")`); applies the overlay when the file exists and has data rows.

- [ ] **Step 1: Write the failing integration test**

Create `tests/testthat/test-wc-ingest-overlay.R`:

```r
test_that("wc_ingest_internationals merges the overlay into results", {
  tmp <- withr::local_tempdir()
  csv <- file.path(tmp, "results.csv")
  writeLines(c(
    "date,home_team,away_team,home_score,away_score,tournament,city,country,neutral",
    "2026-06-20,Aland,Bland,NA,NA,FIFA World Cup,X,US,TRUE",
    "2026-06-20,Cland,Dland,NA,NA,FIFA World Cup,X,US,TRUE",
    "2026-06-23,Aland,Cland,NA,NA,FIFA World Cup,X,US,TRUE",
    "2026-06-23,Bland,Dland,NA,NA,FIFA World Cup,X,US,TRUE",
    "2026-06-25,Aland,Dland,NA,NA,FIFA World Cup,X,US,TRUE",
    "2026-06-25,Bland,Cland,NA,NA,FIFA World Cup,X,US,TRUE"
  ), csv)
  overlay <- file.path(tmp, "manual_results.csv")
  writeLines(c(
    "date,home_team,away_team,home_score,away_score",
    "2026-06-20,Aland,Bland,2,1"
  ), overlay)

  root <- file.path(tmp, "data")
  wc_ingest_internationals(
    csv_path = csv, manual_overlay_path = overlay, root = root
  )

  res <- read_table("results", root = root, filter = list(country = "world"))
  expect_equal(nrow(res), 1L) # only the overlay-filled match is "played"
  expect_equal(res$home_team, "Aland")
  expect_equal(res$home_score, 2L)
  expect_equal(res$away_score, 1L)
})

test_that("a missing overlay file is a clean no-op", {
  tmp <- withr::local_tempdir()
  csv <- file.path(tmp, "results.csv")
  writeLines(c(
    "date,home_team,away_team,home_score,away_score,tournament,city,country,neutral",
    "2026-06-20,Aland,Bland,3,0,FIFA World Cup,X,US,TRUE",
    "2026-06-23,Aland,Cland,NA,NA,FIFA World Cup,X,US,TRUE",
    "2026-06-25,Bland,Cland,NA,NA,FIFA World Cup,X,US,TRUE"
  ), csv)
  root <- file.path(tmp, "data")
  wc_ingest_internationals(
    csv_path = csv,
    manual_overlay_path = file.path(tmp, "does-not-exist.csv"),
    root = root
  )
  res <- read_table("results", root = root, filter = list(country = "world"))
  expect_equal(nrow(res), 1L)
  expect_equal(res$home_team, "Aland")
})
```

- [ ] **Step 2: Run to verify it fails**

Run: `Rscript -e 'devtools::test(filter = "wc-ingest-overlay")'`
Expected: FAIL — `unused argument (manual_overlay_path = ...)`.

- [ ] **Step 3: Add the parameter and merge call**

In `R/wc-ingest.R`, change the signature of `wc_ingest_internationals` (currently ends `root = here::here("data"))`) to add the new parameter:

```r
wc_ingest_internationals <- function(csv_path = here::here("data", "wc", "raw", "results.csv"),
                                     window_start = as.Date("2022-01-01"),
                                     min_team_matches = 8L,
                                     root = here::here("data"),
                                     manual_overlay_path = here::here("data", "wc", "manual_results.csv")) {
```

Then, immediately after the `raw <- readr::read_csv(...)` block (before `d <- raw |> ...`), insert:

```r
  # Operator overlay: fill scores martj42 hasn't published yet (martj42 stays
  # canonical once it catches up). Default path => the CI cron honours it too.
  if (!is.null(manual_overlay_path) && file.exists(manual_overlay_path)) {
    overlay <- readr::read_csv(
      manual_overlay_path,
      comment = "#",
      col_types = readr::cols(
        date = readr::col_date(),
        home_team = readr::col_character(),
        away_team = readr::col_character(),
        home_score = readr::col_integer(),
        away_score = readr::col_integer()
      )
    )
    raw <- wc_apply_manual_results(raw, overlay)
  }
```

Add the roxygen line for the new param (next to the existing `@param root`):

```r
#' @param manual_overlay_path Optional CSV of operator-supplied scores
#'   (`date, home_team, away_team, home_score, away_score`) merged onto martj42's
#'   `NA`-score rows via [wc_apply_manual_results()]. Default
#'   `data/wc/manual_results.csv`; skipped when absent.
```

- [ ] **Step 4: Run to verify it passes**

Run: `Rscript -e 'devtools::test(filter = "wc-ingest-overlay")'`
Expected: PASS — 2 tests, 0 failures.

- [ ] **Step 5: Create the committed overlay file**

Create `data/wc/manual_results.csv` with exactly:

```
# Manual WC results overlay — scores martj42 hasn't published yet.
# Filled into the model at ingest; martj42 stays canonical once it catches up.
#
# HOW TO USE:
#   1. scripts/wc/refresh_now.sh --list-missing   (prints the rows below, unfilled)
#   2. Paste rows here and fill home_score,away_score. Team names MUST match
#      martj42 spelling exactly (the --list-missing output already does).
#   3. scripts/wc/refresh_now.sh                  (re-fits, re-forecasts, publishes)
#   4. When martj42 publishes the real score you'll get a warning at ingest —
#      delete that row here (the overlay self-drains).
date,home_team,away_team,home_score,away_score
```

- [ ] **Step 6: Regenerate docs and run the full ingest test set**

Run: `Rscript -e 'devtools::document(); devtools::test(filter = "wc-ingest")'`
Expected: `man/wc_ingest_internationals.Rd` updated; ingest tests PASS.

- [ ] **Step 7: Commit**

```bash
git add R/wc-ingest.R data/wc/manual_results.csv tests/testthat/test-wc-ingest-overlay.R man/wc_ingest_internationals.Rd
git commit -m "feat(wc): apply manual_results.csv overlay during ingest"
```

---

### Task 4: `scripts/wc/list_missing.R`

**Files:**
- Create: `scripts/wc/list_missing.R`

**Interfaces:**
- Consumes: `wc_list_unscored_fixtures()` (Task 2).
- Produces: a runnable script printing paste-ready overlay rows (no return value).

- [ ] **Step 1: Write the script**

Create `scripts/wc/list_missing.R`:

```r
#!/usr/bin/env Rscript
# scripts/wc/list_missing.R --
# Print the WC fixtures that should have kicked off by today but martj42 hasn't
# scored yet, as paste-ready rows for data/wc/manual_results.csv. Read-only;
# downloads the current martj42 CSV but writes nothing to the facts store.
#
# Usage:
#   Rscript scripts/wc/list_missing.R

invisible(Sys.setlocale("LC_ALL", "en_US.UTF-8"))
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))

martj42_url <- paste0(
  "https://raw.githubusercontent.com/",
  "martj42/international_results/master/results.csv"
)
csv_path <- here::here("data", "wc", "raw", "results.csv")
dir.create(dirname(csv_path), recursive = TRUE, showWarnings = FALSE)
utils::download.file(martj42_url, csv_path, mode = "wb", quiet = TRUE)

raw <- readr::read_csv(csv_path, col_types = readr::cols(
  date = readr::col_date(),
  home_team = readr::col_character(),
  away_team = readr::col_character(),
  home_score = readr::col_integer(),
  away_score = readr::col_integer(),
  tournament = readr::col_character(),
  city = readr::col_character(),
  country = readr::col_character(),
  neutral = readr::col_logical()
))

missing <- wc_list_unscored_fixtures(raw, as_of = Sys.Date())

if (nrow(missing) == 0L) {
  cli::cli_alert_success("No unscored WC fixtures past kickoff — martj42 is current.")
} else {
  cli::cli_alert_info(
    "{nrow(missing)} WC fixture{?s} played but unscored in martj42. \\
    Paste into data/wc/manual_results.csv and fill the scores:"
  )
  out <- missing
  out$home_score <- ""
  out$away_score <- ""
  cat(readr::format_csv(out, col_names = FALSE))
}
```

- [ ] **Step 2: Smoke-test it (network)**

Run: `Rscript scripts/wc/list_missing.R`
Expected: either the green "martj42 is current" line, or an info line followed by CSV rows like `2026-06-26,Algeria,Austria,,`. (This downloads the live CSV; output depends on martj42's state.)

- [ ] **Step 3: Commit**

```bash
git add scripts/wc/list_missing.R
git commit -m "feat(wc): list_missing.R — print overlay rows to fill"
```

---

### Task 5: `scripts/wc/refresh_now.sh` — the orchestrator

**Files:**
- Create: `scripts/wc/refresh_now.sh` (executable)

**Interfaces:**
- Consumes: `scripts/wc/{ingest,fit,forecast}.R` (existing), `scripts/wc/list_missing.R` (Task 4), `gh` CLI, the overlay file (Task 3).
- Produces: an end-to-end local refresh + publish command.

- [ ] **Step 1: Write the wrapper**

Create `scripts/wc/refresh_now.sh`:

```bash
#!/usr/bin/env bash
# scripts/wc/refresh_now.sh --
# Manual on-demand World Cup refresh for when martj42 lags. Inject known scores
# via data/wc/manual_results.csv, then re-fit, re-forecast, publish to
# metill-is/sports, and trigger the metill-platform pull. Runs from anywhere.
#
# Usage:
#   scripts/wc/refresh_now.sh --list-missing  # show fixtures to fill, then edit the CSV
#   scripts/wc/refresh_now.sh                 # full run; pauses to confirm before publish
#   scripts/wc/refresh_now.sh --yes           # full run; no confirm prompt
#   scripts/wc/refresh_now.sh --no-push       # run + preview only (no commit/push/trigger)
#   scripts/wc/refresh_now.sh --no-pull       # skip the pre-run git pull --rebase
set -euo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$REPO"

LIST_MISSING=0; ASSUME_YES=0; DO_PUSH=1; DO_PULL=1
for arg in "$@"; do
  case "$arg" in
    --list-missing) LIST_MISSING=1 ;;
    --yes)          ASSUME_YES=1 ;;
    --no-push)      DO_PUSH=0 ;;
    --no-pull)      DO_PULL=0 ;;
    *) echo "unknown flag: $arg" >&2; exit 2 ;;
  esac
done

if [[ "$LIST_MISSING" -eq 1 ]]; then
  exec Rscript scripts/wc/list_missing.R
fi

if [[ "$DO_PULL" -eq 1 ]]; then
  echo "==> git pull --rebase origin main"
  git pull --rebase origin main
fi

echo "==> ingest (martj42 + manual overlay)"
Rscript scripts/wc/ingest.R
echo "==> fit (Stan, ~46 min)"
Rscript scripts/wc/fit.R
echo "==> forecast + publish JSON"
Rscript scripts/wc/forecast.R

echo
echo "Preview: open $REPO/data/wc/forecast.html and check the champion table above."

if [[ "$DO_PUSH" -eq 0 ]]; then
  echo "--no-push: stopping before commit. JSON is in data/publish/world_cup/karla/."
  exit 0
fi

if [[ "$ASSUME_YES" -ne 1 ]]; then
  read -r -p "Publish to metill-is/sports and trigger the platform pull? [y/N] " reply
  case "$reply" in
    [yY]|[yY][eE][sS]) ;;
    *) echo "Aborted; nothing pushed."; exit 0 ;;
  esac
fi

echo "==> commit + push"
git add \
  data/publish/world_cup \
  data/facts/results/sport=football/country=world \
  data/facts/schedules/sport=football/country=world \
  data/wc/manual_results.csv
if git diff --cached --quiet; then
  echo "No changes to publish (forecast output identical). Nothing pushed."
  exit 0
fi
git commit -m "data(wc): manual refresh $(date -u +%Y-%m-%dT%H:%MZ) — martj42 lag"
git pull --rebase origin main
git push

echo "==> trigger metill-platform pull"
gh workflow run pull-sports-data.yml -R metill-is/metill-platform

echo
echo "Done. Watch the platform pull + deploy:"
echo "  gh run list -R metill-is/metill-platform --workflow=pull-sports-data.yml"
```

- [ ] **Step 2: Make it executable + syntax-check**

```bash
chmod +x scripts/wc/refresh_now.sh
bash -n scripts/wc/refresh_now.sh
```
Expected: no output (syntax OK).

- [ ] **Step 3: Lint (if shellcheck is installed)**

Run: `command -v shellcheck >/dev/null && shellcheck scripts/wc/refresh_now.sh || echo "shellcheck not installed — skipping"`
Expected: no warnings, or the skip message.

- [ ] **Step 4: Smoke-test the no-run path**

Run: `scripts/wc/refresh_now.sh --list-missing`
Expected: delegates to `list_missing.R` (same output as Task 4 Step 2).

> **NOTE — full end-to-end is NOT run in this task.** The fit is ~46 min, so a complete `refresh_now.sh` run is validated on the operator's next live use, not here. This is a deliberate, logged cap — the wrapper's logic is covered by `bash -n` + shellcheck + the `--list-missing` smoke; the R stages it calls are covered by Tasks 1–4 and the existing suite.

- [ ] **Step 5: Commit**

```bash
git add scripts/wc/refresh_now.sh
git commit -m "feat(wc): refresh_now.sh — local on-demand WC refresh + publish"
```

---

### Task 6a: Runbook in `~/sports/CLAUDE.md`

**Files:**
- Modify: `~/sports/CLAUDE.md`

- [ ] **Step 1: Find an insertion point**

Run: `grep -n "World Cup\|world_cup\|^## " ~/sports/CLAUDE.md | head -40`
Pick the heading nearest the World Cup / operational runbooks. Insert the block below as a new `### World Cup — manual refresh (martj42 lag)` subsection under the most relevant `##` section (e.g. an operations / pipelines section).

- [ ] **Step 2: Add the runbook**

Insert:

```markdown
### World Cup — manual refresh (martj42 lag)

martj42 backfills match scores ~1 day late, so during the tournament the daily
`world-cup.yml` cron is structurally a day behind. To publish a forecast that
includes scores martj42 hasn't logged yet:

1. `scripts/wc/refresh_now.sh --list-missing` — prints the played-but-unscored
   WC fixtures as CSV rows (team names already match martj42).
2. Paste them into `data/wc/manual_results.csv` and fill `home_score,away_score`.
3. `scripts/wc/refresh_now.sh` — pulls, ingests (overlay merged onto martj42's
   `NA` rows), re-fits (~46 min), re-forecasts, shows the champion table +
   `data/wc/forecast.html`, then on `y` commits + pushes and triggers the
   metill-platform pull (`gh workflow run pull-sports-data.yml -R
   metill-is/metill-platform`).

The overlay is committed and self-draining: once martj42 publishes the real
score, ingest warns and martj42 wins — delete that row. The manual path does not
touch `data/wc/martj42_pointer.txt`, so the next cron run is unaffected. Flags:
`--yes` (skip confirm), `--no-push` (preview only), `--no-pull` (offline).
```

- [ ] **Step 3: Commit**

```bash
git add CLAUDE.md
git commit -m "docs(wc): runbook for the manual martj42-lag refresh"
```

---

### Task 6b: Update `~/metill-platform/.claude/rules/hm2026.md` (separate repo)

**Files:**
- Modify: `~/metill-platform/.claude/rules/hm2026.md`

- [ ] **Step 1: Branch in the platform repo**

```bash
cd ~/metill-platform
git stash list >/dev/null 2>&1 # (informational)
git checkout -b hm2026-manual-refresh-doc
```

- [ ] **Step 2: Locate the manual-fallback line**

Run: `grep -n "manual fallback\|ingest,fit,forecast\|scripts/wc" ~/metill-platform/.claude/rules/hm2026.md`

- [ ] **Step 3: Replace it**

Replace the matched "manual fallback" sentence with:

```markdown
- **Manual fallback / martj42 lag:** `~/sports/scripts/wc/refresh_now.sh` runs the
  full pipeline on demand (`--list-missing` to scaffold `data/wc/manual_results.csv`
  with scores martj42 hasn't published yet, then re-fit → re-forecast → publish →
  trigger this repo's `pull-sports-data.yml`). The overlay is committed and
  self-draining; see `~/sports` CLAUDE.md.
```

- [ ] **Step 4: Commit (do not push without the user's go-ahead)**

```bash
cd ~/metill-platform
git add .claude/rules/hm2026.md
git commit -m "docs(hm2026): point manual-refresh at refresh_now.sh + overlay"
```

---

### Task 7: Full regression gate

**Files:** none (verification only).

- [ ] **Step 1: Run the whole test suite in `~/sports`**

```bash
cd ~/sports
Rscript -e 'devtools::test()'
```
Expected: all tests pass (the new `test-wc-manual-results`, `test-wc-list-unscored`, `test-wc-ingest-overlay` included; no regressions in the existing 93 files).

- [ ] **Step 2: Confirm the branch is clean and review the diff**

```bash
git status
git log --oneline main..wc-manual-overlay
```
Expected: working tree clean; commits from Tasks 1–6a present on `wc-manual-overlay`.

---

## Self-Review

**Spec coverage:** overlay CSV → Task 3 Step 5; `wc_apply_manual_results` (fill/typo-abort/drain/no-op/ambiguous) → Task 1; `wc_list_unscored_fixtures` → Task 2; ingest `+param` (default path ⇒ CI honours overlay free) → Task 3; `list_missing.R` → Task 4; `refresh_now.sh` (flags, all-three-in-order, preview/confirm, git-add set, trigger pull, pointer untouched) → Task 5; error handling → Tasks 1 & 5 (`set -euo pipefail`, `pull --rebase`, `git diff --cached --quiet`); tests → Tasks 1–3 + Task 7; docs → Tasks 6a/6b. All spec sections map to a task.

**Placeholder scan:** no TBD/TODO; every code step shows full code; no "similar to Task N". The one deliberate non-run (full `refresh_now.sh`) is logged, not hidden (Task 5 Step 4 NOTE).

**Type consistency:** `wc_apply_manual_results(raw, overlay) -> df` used identically in Task 3's ingest body; `wc_list_unscored_fixtures(raw, as_of) -> df` used in Task 4; `manual_overlay_path` default path identical in Task 3 (signature) and the spec; git-add set identical in Task 5 and the Global Constraints; column names use raw schema (`tournament`/`date`) in Tasks 2/4 and renamed schema is never touched by the helpers.
