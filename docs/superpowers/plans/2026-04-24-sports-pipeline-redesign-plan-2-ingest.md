# Sports Pipeline Redesign — Plan 2: Ingest (Federation Scrapers) + Backfill

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build the `R/ingest/` module with federation scrapers for the three active Icelandic leagues (KSÍ football, KKÍ basketball, HSÍ handball), backfill historical match results and schedules into `data/facts/{results,schedules}/`, and validate the populated Parquet stores.

**Architecture:** One module per data source under `R/ingest/` exposing a consistent contract — `fetch_results(league, sex, seasons = NULL)` and `fetch_schedule(league, sex)` — returning tibbles that conform to the canonical Arrow schemas defined in Plan 1 Task 7. A thin dispatcher `R/ingest/ingest.R::ingest_league(league_key, sex)` reads `config/leagues.yml`'s `data_source.{results,schedule}` fields and routes to the right module, then calls `write_table()`. Network-dependent scraping functions are kept small and testable by separating HTTP / Chrome fetches from HTML / XLSX parsing (fixture-based unit tests for parsers).

**Tech Stack:** R (≥ 4.0), `{rvest}`, `{httr2}`, `{chromote}` (only where dynamic rendering is unavoidable), `{readxl}`, `{lubridate}`, plus the Plan 1 stack (`{arrow}`, `{DBI}`, `{duckdb}`, `{testthat}`).

**Scope (Plan 2):**

- `R/ingest/ingest.R` — dispatcher with `ingest_league()`
- `R/ingest/kki_basketball.R` — basketball_iceland scraper (Baskethotel widget XLSX downloads)
- `R/ingest/hsi_handball.R` — handball_iceland scraper (HSÍ website, rvest-parseable)
- `R/ingest/ksi_football.R` — football_iceland scraper (KSÍ website, rvest-parseable)
- Fixture-based parser unit tests under `tests/testthat/`
- Integration test: after backfill, `data/facts/{results,schedules}/` has plausible row counts and clean team-name distributions
- Backfill all 3 leagues × both sexes into Parquet
- Update CLAUDE.md to reflect ingest availability

**Closes Plan 1's deferred Task 10** (results/schedules ETL). The dispatcher replaces the deferred `scripts/etl/01_etl_results.R` + `02_etl_schedules.R` with live scraping.

**Out of scope (still):**

- Lengjan odds scraper port (odds data already ETL'd in Plan 1 Task 11; refreshing stays in `_legacy/lengjan-odds/` for now — Plan 5 or 6 integrates it)
- Livesport scraper (no Icelandic league uses it)
- Model fitting (Plan 3)
- Orchestration, CI, scheduling (final plan)
- Cutting over from legacy pipeline runners to the new ones

---

## File structure created by this plan

```
sports/
├── R/ingest/
│   ├── ingest.R                  # Dispatcher: ingest_league(league_key, sex)
│   ├── kki_basketball.R          # Baskethotel XLSX downloader + parser
│   ├── hsi_handball.R            # HSÍ website scraper
│   └── ksi_football.R            # KSÍ website scraper
├── tests/testthat/
│   ├── fixtures/
│   │   ├── kki_basketball/       # Cached XLSX snippets
│   │   ├── hsi_handball/         # Cached HTML pages
│   │   └── ksi_football/         # Cached HTML pages
│   ├── test-ingest-dispatcher.R
│   ├── test-ingest-kki.R
│   ├── test-ingest-hsi.R
│   ├── test-ingest-ksi.R
│   └── test-ingest-integration.R
├── scripts/
│   └── backfill_ingest.R         # One-shot backfill for all 3 leagues
└── data/facts/
    ├── results/sport=X/country=iceland/sex=Y/season=YYYY/*.parquet
    └── schedules/sport=X/country=iceland/sex=Y/season=YYYY/*.parquet
```

---

## Task 1: `R/ingest/` interface contract + dispatcher (TDD)

**Files:**

- Create: `R/ingest/ingest.R`
- Create: `tests/testthat/test-ingest-dispatcher.R`

**Purpose:** Define the module contract once, then the three per-source modules plug into it. The dispatcher reads `leagues.yml` and routes to the configured source.

- [ ] **Step 1: Write the failing tests**

```r
# tests/testthat/test-ingest-dispatcher.R

test_that("ingest_league dispatches to the source named in data_source.results", {
  # Create a stub module registered by name
  fake <- list(
    fetch_results  = function(league, sex, seasons = NULL) {
      tibble::tibble(
        sport = league$sport, country = league$country, sex = sex,
        season = 2026L,
        match_date = as.Date("2026-01-01"),
        home_team = "A", away_team = "B",
        home_score = 10L, away_score = 8L,
        division = "D1", round = 1L
      )
    },
    fetch_schedule = function(league, sex) {
      tibble::tibble(
        sport = league$sport, country = league$country, sex = sex,
        season = 2026L,
        match_date = as.Date("2026-02-01"),
        home_team = "C", away_team = "D",
        division = "D1", round = 2L
      )
    }
  )

  # Register via dependency injection (tests override the registry)
  register_ingest_source("test_stub", fake)
  on.exit(unregister_ingest_source("test_stub"), add = TRUE)

  league <- list(
    sport = "basketball", country = "iceland",
    sexes = c("male"),
    data_source = list(results = "test_stub", schedule = "test_stub")
  )

  tmp <- withr::local_tempdir()
  ingest_league(league, "male", root = tmp)

  r <- read_table("results",  root = tmp, filter = list(sport = "basketball"))
  s <- read_table("schedules", root = tmp, filter = list(sport = "basketball"))
  expect_equal(nrow(r), 1L)
  expect_equal(nrow(s), 1L)
  expect_equal(r$home_team, "A")
  expect_equal(s$home_team, "C")
})

test_that("ingest_league errors clearly when the source isn't registered", {
  league <- list(
    sport = "basketball", country = "iceland",
    data_source = list(results = "does_not_exist", schedule = "does_not_exist")
  )
  expect_error(ingest_league(league, "male"),
               regexp = "does_not_exist")
})

test_that("ingest_league skips gracefully when the source returns 0 rows", {
  fake <- list(
    fetch_results  = function(...) tibble::tibble(),
    fetch_schedule = function(...) tibble::tibble()
  )
  register_ingest_source("empty_stub", fake)
  on.exit(unregister_ingest_source("empty_stub"), add = TRUE)

  league <- list(
    sport = "football", country = "iceland",
    data_source = list(results = "empty_stub", schedule = "empty_stub")
  )
  tmp <- withr::local_tempdir()
  expect_no_error(ingest_league(league, "male", root = tmp))
  expect_equal(nrow(read_table("results",  root = tmp)), 0L)
})
```

- [ ] **Step 2: Verify tests fail**

```bash
cd /Users/brynjolfurjonsson/sports
Rscript -e 'devtools::test(filter = "ingest-dispatcher")'
```

Expected: 3 errors (`could not find function "register_ingest_source"` etc.).

- [ ] **Step 3: Implement the dispatcher**

```r
# R/ingest/ingest.R

.ingest_registry <- new.env(parent = emptyenv())

#' Register an ingest source module. Used by R/ingest/{kki,hsi,ksi}_*.R on load.
#'
#' @param name Source name referenced in leagues.yml's data_source field.
#' @param module A list with `fetch_results` and `fetch_schedule` functions.
#' @keywords internal
#' @noRd
register_ingest_source <- function(name, module) {
  stopifnot(is.list(module),
            is.function(module$fetch_results),
            is.function(module$fetch_schedule))
  assign(name, module, envir = .ingest_registry)
  invisible(NULL)
}

#' Remove a registered ingest source (used by tests).
#' @keywords internal
#' @noRd
unregister_ingest_source <- function(name) {
  if (exists(name, envir = .ingest_registry, inherits = FALSE)) {
    rm(list = name, envir = .ingest_registry)
  }
  invisible(NULL)
}

#' Look up a source module by name; errors if not registered.
#' @keywords internal
#' @noRd
get_ingest_source <- function(name) {
  if (!exists(name, envir = .ingest_registry, inherits = FALSE)) {
    stop("Ingest source not registered: ", name, call. = FALSE)
  }
  get(name, envir = .ingest_registry)
}

#' Ingest results + schedules for one (league, sex) pair.
#'
#' Reads `league$data_source$results` and `league$data_source$schedule` to pick
#' the right source module, calls fetch_*, and writes to `data/facts/{results,
#' schedules}/` via `write_table()`.
#'
#' @param league A single entry from `load_leagues()`.
#' @param sex "male" or "female".
#' @param root Data root. Defaults to `here::here("data")`.
#' @param seasons Optional integer vector to pass through to `fetch_results`.
#' @return invisible(NULL)
#' @export
ingest_league <- function(league, sex,
                          root = here::here("data"),
                          seasons = NULL) {
  results_mod  <- get_ingest_source(league$data_source$results)
  schedule_mod <- get_ingest_source(league$data_source$schedule)

  results  <- results_mod$fetch_results(league, sex, seasons = seasons)
  schedule <- schedule_mod$fetch_schedule(league, sex)

  if (nrow(results)  > 0) write_table(results,  "results",   root = root)
  if (nrow(schedule) > 0) write_table(schedule, "schedules", root = root)

  invisible(NULL)
}
```

- [ ] **Step 4: Verify tests pass**

```bash
Rscript -e 'devtools::test(filter = "ingest-dispatcher")'
```

Expected: 3 passes, 6 assertions.

- [ ] **Step 5: Commit**

```bash
cd /Users/brynjolfurjonsson/sports
git add R/ingest/ingest.R tests/testthat/test-ingest-dispatcher.R
git commit -m "feat: R/ingest/ dispatcher with source-registry contract

ingest_league(league, sex) reads data_source from leagues.yml, looks
up the registered source module, calls fetch_results + fetch_schedule,
and writes to facts/results + facts/schedules via write_table(). Sources
register themselves via register_ingest_source() in their own R file.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>"
```

---

## Task 2: KKÍ basketball scraper (Baskethotel XLSX) with fixture tests

**Files:**

- Create: `R/ingest/kki_basketball.R`
- Create: `tests/testthat/test-ingest-kki.R`
- Create: `tests/testthat/fixtures/kki_basketball/sample_results.xlsx` (captured once live)

**Purpose:** Port the legacy `_legacy/sports/basketball/iceland/R/prep_data_kk.R` Baskethotel widget URLs into a clean module that registers as `kki_basketball` and returns canonical-schema tibbles.

**Baskethotel URL pattern** (from legacy code, `_legacy/sports/basketball/iceland/R/prep_data_kk.R`):

```
https://widgets.baskethotel.com/widget-service/export/view/schedule_and_results
  ?api={API_KEY}
  &season_id={SEASON_ID}
  &lang=is
  &month=all
  &type={schedule_only|results_only}
```

The legacy code hardcodes the API key. Keep that for now — if Baskethotel rotates it, we'll catch it at scrape time.

**Season IDs per division** (from legacy — verify/update live during execution):

- Male div1: 2025=190366, 2026=7025510
- Male div2: 2025=190359, 2026=7025540
- (Female — check `prep_data_kvk.R` for the equivalent IDs)

These change each season. Store them as a nested list in `R/ingest/kki_basketball.R`. When a new season starts, this list gains one entry.

- [ ] **Step 1: Capture a fixture** (one-time, live network)

```bash
cd /Users/brynjolfurjonsson/sports
mkdir -p tests/testthat/fixtures/kki_basketball
curl -s -o tests/testthat/fixtures/kki_basketball/sample_male_div1_2026.xlsx \
  "https://widgets.baskethotel.com/widget-service/export/view/schedule_and_results?api=a0d07178160bf749eb6e5e761fc623fe42e2bb57&season_id=7025510&lang=is&month=all&type=results_only"
file tests/testthat/fixtures/kki_basketball/sample_male_div1_2026.xlsx
# Expected: Microsoft Excel 2007+ (XLSX)
```

If the download fails or the file isn't XLSX (HTML error page), Baskethotel may have rotated the API key or decommissioned the widget. In that case, STOP and surface the issue — don't proceed blind.

- [ ] **Step 2: Write the failing parser test**

```r
# tests/testthat/test-ingest-kki.R

fixture <- function(name) {
  testthat::test_path("fixtures", "kki_basketball", name)
}

test_that("parse_baskethotel_xlsx reads a results file into canonical columns", {
  skip_if_not(file.exists(fixture("sample_male_div1_2026.xlsx")),
              "no kki fixture captured")

  parsed <- parse_baskethotel_xlsx(
    fixture("sample_male_div1_2026.xlsx"),
    sport = "basketball", country = "iceland", sex = "male",
    division = "BD", season = 2026L
  )

  expect_named(parsed, c("sport", "country", "sex", "season", "match_date",
                         "home_team", "away_team", "home_score", "away_score",
                         "division", "round"))
  expect_s3_class(parsed$match_date, "Date")
  expect_type(parsed$season, "integer")
  expect_gt(nrow(parsed), 5)
})

test_that("kki_basketball registers itself as a source module", {
  devtools::load_all()
  # After load_all, the source should be in the registry
  src <- get_ingest_source("kki_basketball")
  expect_true(is.function(src$fetch_results))
  expect_true(is.function(src$fetch_schedule))
})

test_that("kki_basketball$fetch_results errors clearly if the XLSX download fails", {
  # Override the download function via module-local `download_xlsx` injection
  # or just test the parser path (which the fixture test already covers).
  skip("tested at integration-layer during backfill")
})
```

- [ ] **Step 3: Verify failure**

```bash
Rscript -e 'devtools::test(filter = "ingest-kki")'
```

Expected: `could not find function "parse_baskethotel_xlsx"`.

- [ ] **Step 4: Implement `R/ingest/kki_basketball.R`**

Reuse the shape of `_legacy/sports/basketball/iceland/R/prep_data_kk.R` but strip `box::use`, `here::here`, `with_dir` gymnastics. Structure:

```r
# R/ingest/kki_basketball.R

BASKETHOTEL_API <- "a0d07178160bf749eb6e5e761fc623fe42e2bb57"

# Season -> division -> numeric ID. Extend this when a new season starts.
KKI_SEASON_IDS <- list(
  male = list(
    div1 = c("2025" = 190366, "2026" = 7025510),
    div2 = c("2025" = 190359, "2026" = 7025540)
  ),
  female = list(
    # Fill from _legacy/sports/basketball/iceland/R/prep_data_kvk.R during impl
  )
)

baskethotel_url <- function(season_id, type) {
  sprintf(
    "https://widgets.baskethotel.com/widget-service/export/view/schedule_and_results?api=%s&season_id=%s&lang=is&month=all&type=%s",
    BASKETHOTEL_API, season_id, type
  )
}

download_baskethotel_xlsx <- function(season_id, type) {
  url <- baskethotel_url(season_id, type)
  tmp <- tempfile(fileext = ".xlsx")
  # Use httr2 for proper error handling
  resp <- httr2::request(url) |> httr2::req_perform()
  writeBin(httr2::resp_body_raw(resp), tmp)
  # Sanity: must start with PK (XLSX magic)
  raw_hdr <- readBin(tmp, what = "raw", n = 4)
  if (!identical(as.character(raw_hdr[1:2]), c("50", "4b"))) {
    stop("Baskethotel returned non-XLSX content — check API key or season_id ",
         season_id, call. = FALSE)
  }
  tmp
}

#' Parse a Baskethotel XLSX into the canonical results tibble.
#'
#' @param path Path to downloaded XLSX.
#' @param sport, country, sex, division, season Metadata to attach.
#' @return tibble matching schemas()$results
#' @keywords internal
#' @noRd
parse_baskethotel_xlsx <- function(path, sport, country, sex, division, season) {
  raw <- readxl::read_excel(path, sheet = 1, skip = 1, col_types = "text")
  # Legacy raw columns (confirmed 2026-04-24): varies — inspect with str() during
  # implementation and rename accordingly. Target: match_date, home_team,
  # away_team, home_score, away_score columns as character -> coerce at end.
  # See _legacy/sports/basketball/iceland/R/prep_data_kk.R for the exact
  # read_excel + rename pattern (lines 36-75 in that file).
  # ... map raw columns to canonical ...

  dplyr::tibble(
    sport      = sport,
    country    = country,
    sex        = sex,
    season     = as.integer(season),
    match_date = as.Date(raw$date_col),          # rename as needed after inspecting str(raw)
    home_team  = as.character(raw$home_col),
    away_team  = as.character(raw$away_col),
    home_score = as.integer(raw$home_score_col),
    away_score = as.integer(raw$away_score_col),
    division   = as.character(division),
    round      = NA_integer_
  ) |>
    dplyr::filter(!is.na(home_score))   # drop unplayed rows
}

fetch_results_kki <- function(league, sex, seasons = NULL) {
  div_ids <- KKI_SEASON_IDS[[sex]]
  if (is.null(div_ids)) return(tibble::tibble())

  rows <- list()
  for (div_name in names(div_ids)) {
    for (season_key in names(div_ids[[div_name]])) {
      season <- as.integer(season_key)
      if (!is.null(seasons) && !(season %in% seasons)) next

      sid <- div_ids[[div_name]][[season_key]]
      path <- tryCatch(download_baskethotel_xlsx(sid, "results_only"),
                       error = function(e) { cli::cli_alert_warning(conditionMessage(e)); NULL })
      if (is.null(path)) next
      rows[[length(rows) + 1L]] <- parse_baskethotel_xlsx(
        path, sport = league$sport, country = league$country, sex = sex,
        division = toupper(div_name), season = season
      )
    }
  }
  dplyr::bind_rows(rows)
}

fetch_schedule_kki <- function(league, sex) {
  # Same as fetch_results but type = "schedule_only" and drop home_score/away_score.
  # Canonical schedule schema: sport, country, sex, season, match_date,
  # home_team, away_team, division, round.
  ...
}

# Register on load
register_ingest_source("kki_basketball", list(
  fetch_results  = fetch_results_kki,
  fetch_schedule = fetch_schedule_kki
))
```

Important: the rename step for raw Baskethotel columns MUST be verified against the fixture before committing — Baskethotel may have changed column headers since 2024. Inspect `readxl::read_excel(fixture_path) |> str()` and write the rename explicitly.

- [ ] **Step 5: Verify fixture-based parser test passes**

```bash
Rscript -e 'devtools::test(filter = "ingest-kki")'
```

Expected: 3 passes (fixture parser, registration, skip). If `parse_baskethotel_xlsx` fails because legacy column names differ, inspect and adjust. Do not commit with failing tests.

- [ ] **Step 6: Commit**

```bash
git add R/ingest/kki_basketball.R \
        tests/testthat/test-ingest-kki.R \
        tests/testthat/fixtures/kki_basketball/
git commit -m "feat: kki_basketball ingest source

Ports legacy Baskethotel XLSX-downloading logic to a clean module
that registers as 'kki_basketball' and returns canonical-schema
results + schedule tibbles. Fixture-tested parser.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>"
```

---

## Task 3: HSÍ handball scraper with fixture tests

**Files:**

- Create: `R/ingest/hsi_handball.R`
- Create: `tests/testthat/test-ingest-hsi.R`
- Create: `tests/testthat/fixtures/hsi_handball/*.html` (cached HTML pages from HSÍ)

**Purpose:** Port the legacy `_legacy/sports/handball/iceland/R/utils/{male,female}/download_*.R` logic. The HSÍ website lists match results per division per season; the legacy code scrapes ~3 URLs per (sex, division, year).

**Pattern:** legacy code uses `rvest::read_html()` on specific HSÍ URLs. Identify URL templates and selectors by reading one or two of the `download_*_div1.R` scripts.

- [ ] **Step 1: Read the legacy HSÍ scrapers**

```bash
cd /Users/brynjolfurjonsson/sports
cat _legacy/sports/handball/iceland/R/utils/male/download_newest_data_div1.R
cat _legacy/sports/handball/iceland/R/utils/male/process_data.R | head -80
```

Record: URL pattern, selectors used, parse logic. These become the hsi_handball module.

- [ ] **Step 2: Capture fixture HTML**

```bash
mkdir -p tests/testthat/fixtures/hsi_handball
# Pick a representative current-season div1 page — URL derived from the legacy scraper
curl -sL -o tests/testthat/fixtures/hsi_handball/male_div1_current.html \
  "<URL discovered in Step 1>"
file tests/testthat/fixtures/hsi_handball/male_div1_current.html
# Expected: HTML document, not an error page
```

- [ ] **Step 3: Write the failing parser test**

```r
# tests/testthat/test-ingest-hsi.R

fixture <- function(name) {
  testthat::test_path("fixtures", "hsi_handball", name)
}

test_that("parse_hsi_results_page extracts matches with canonical columns", {
  skip_if_not(file.exists(fixture("male_div1_current.html")),
              "no hsi fixture captured")

  html <- rvest::read_html(fixture("male_div1_current.html"))
  parsed <- parse_hsi_results_page(
    html,
    sport = "handball", country = "iceland", sex = "male",
    division = "OD", season = 2026L
  )

  expect_named(parsed, c("sport", "country", "sex", "season", "match_date",
                         "home_team", "away_team", "home_score", "away_score",
                         "division", "round"))
  expect_s3_class(parsed$match_date, "Date")
  expect_gt(nrow(parsed), 5)
})

test_that("hsi_handball registers on load", {
  devtools::load_all()
  src <- get_ingest_source("hsi_handball")
  expect_true(is.function(src$fetch_results))
  expect_true(is.function(src$fetch_schedule))
})
```

- [ ] **Step 4: Verify failure**

```bash
Rscript -e 'devtools::test(filter = "ingest-hsi")'
```

- [ ] **Step 5: Implement `R/ingest/hsi_handball.R`**

Port the legacy download + process pattern. Separate:

- `hsi_url(sex, division, season, kind = c("results", "schedule"))` — URL builder
- `fetch_hsi_html(url)` — network call (rvest::read_html or chromote if JS-rendered)
- `parse_hsi_results_page(html, sport, country, sex, division, season)` — pure parser, fixture-testable
- `parse_hsi_schedule_page(html, sport, country, sex, division, season)` — pure parser
- `fetch_results_hsi(league, sex, seasons = NULL)` / `fetch_schedule_hsi(league, sex)` — orchestration
- `register_ingest_source("hsi_handball", ...)` at bottom

Divisions to cover (per `_legacy/.claude/rules/sports-per-sport.md`):

- Male: div1 (Olís deild), div2 (Grill 66), cup
- Female: div1, div2, playoffs

- [ ] **Step 6: Verify parser test passes with fixture**

```bash
Rscript -e 'devtools::test(filter = "ingest-hsi")'
```

- [ ] **Step 7: Commit**

```bash
git add R/ingest/hsi_handball.R \
        tests/testthat/test-ingest-hsi.R \
        tests/testthat/fixtures/hsi_handball/
git commit -m "feat: hsi_handball ingest source

Ports legacy _legacy/sports/handball/iceland/R/utils/ download +
process scripts into a clean module. Separate URL builder / HTTP
fetch / pure parser so the parser is fixture-testable.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>"
```

---

## Task 4: KSÍ football scraper with fixture tests

**Files:**

- Create: `R/ingest/ksi_football.R`
- Create: `tests/testthat/test-ingest-ksi.R`
- Create: `tests/testthat/fixtures/ksi_football/*.html`

**Purpose:** Port `_legacy/sports/football/iceland/R/utils/scrape_ksi.R`. The legacy code has a clean structure — `parse_ksi_date()`, `fetch_ksi_page()`, etc. — that largely maps to the new interface.

**Divisions per legacy code:**

- Male: div1 (Besta deild), div2, div3, div4, div5, cup, div1 playoffs (upper/lower), div2 playoffs
- Female: div1, div2, div3, cup, div1 playoffs (upper/lower)

**URL pattern:**

```
https://www.ksi.is/oll-mot/mot?id=<ksi_id>&banner-tab=matches-and-results
```

`ksi_id` values are hardcoded in the legacy file as a `list` of `c("YYYY" = ...)` per sex × division.

- [ ] **Step 1: Read legacy scraper fully**

```bash
cat _legacy/sports/football/iceland/R/utils/scrape_ksi.R
```

Record the `parse_ksi_date()`, selectors, and parse logic. These become the new module's parser.

- [ ] **Step 2: Capture fixture HTML**

Use one `ksi_id` from current season (e.g. `7025510`-ish for male div1 2026):

```bash
mkdir -p tests/testthat/fixtures/ksi_football
curl -sL -o tests/testthat/fixtures/ksi_football/male_div1_2026.html \
  "https://www.ksi.is/oll-mot/mot?id=<ID>&banner-tab=matches-and-results"
```

- [ ] **Step 3: Write failing parser test**

Mirror Task 3 Step 3 structure but for `parse_ksi_results_page`.

- [ ] **Step 4: Verify failure** — standard

- [ ] **Step 5: Implement `R/ingest/ksi_football.R`** — port from `_legacy/sports/football/iceland/R/utils/scrape_ksi.R`. Preserve the `KSI_IDS` nested list verbatim. Separate fetch from parse.

- [ ] **Step 6: Verify parser test passes**

- [ ] **Step 7: Commit**

---

## Task 5: Backfill all 3 Icelandic leagues + integration quality test

**Files:**

- Create: `scripts/backfill_ingest.R`
- Create: `tests/testthat/test-ingest-integration.R`

**Purpose:** Run the three scrapers live and populate `data/facts/results/` + `data/facts/schedules/`. Integration test validates the resulting Parquet has plausible shape.

- [ ] **Step 1: Write `scripts/backfill_ingest.R`**

```r
#!/usr/bin/env Rscript
# Backfill data/facts/{results,schedules}/ for all 3 active Icelandic leagues.

suppressPackageStartupMessages(devtools::load_all(here::here()))

leagues <- load_leagues()
for (key in names(leagues)) {
  league <- leagues[[key]]
  cli::cli_h1(key)
  for (sex in league$sexes) {
    cli::cli_h2("sex = {sex}")
    tryCatch(
      ingest_league(league, sex),
      error = function(e) cli::cli_alert_danger("Failed: {conditionMessage(e)}")
    )
  }
}
cli::cli_alert_success("Backfill complete.")
```

- [ ] **Step 2: Run it live**

```bash
cd /Users/brynjolfurjonsson/sports
Rscript scripts/backfill_ingest.R 2>&1 | tee /tmp/backfill.log
```

Expected: rows written for each (league, sex) combination. Scrape failures (dead URLs, changed selectors) surface as `cli_alert_danger`. If any hard failure, investigate before proceeding — selectors may have drifted.

- [ ] **Step 3: Write integration test**

```r
# tests/testthat/test-ingest-integration.R

skip_if_no_data <- function() {
  if (!dir.exists(here::here("data", "facts", "results"))) {
    testthat::skip("data/facts/results absent; backfill hasn't run")
  }
}

test_that("results table has rows for all 3 Icelandic leagues × both sexes", {
  skip_if_no_data()
  r <- read_table("results", filter = list(country = "iceland"))

  counts <- dplyr::count(r, sport, sex)
  expect_equal(nrow(counts), 6L)
  expect_gt(min(counts$n), 10L, label = "every (sport, sex) has > 10 rows")
})

test_that("match_date is a proper Date, scores are integers", {
  skip_if_no_data()
  r <- read_table("results", filter = list(country = "iceland"))
  expect_s3_class(r$match_date, "Date")
  expect_type(r$home_score, "integer")
  expect_type(r$away_score, "integer")
  expect_true(all(!is.na(r$home_score)))
  expect_true(all(!is.na(r$away_score)))
})

test_that("no duplicate matches per league (sport, sex, match_date, home_team, away_team)", {
  skip_if_no_data()
  r <- read_table("results", filter = list(country = "iceland"))
  key <- paste(r$sport, r$sex, r$match_date, r$home_team, r$away_team, sep = "||")
  expect_equal(length(key), length(unique(key)))
})

test_that("schedules for the 3 Icelandic leagues", {
  skip_if_no_data()
  s <- read_table("schedules", filter = list(country = "iceland"))
  expect_gt(nrow(s), 10L)
  expect_s3_class(s$match_date, "Date")
})
```

- [ ] **Step 4: Run tests**

```bash
Rscript -e 'devtools::test(filter = "ingest-integration")'
```

Expected: all pass. If duplicates appear, inspect — the parsers may be double-counting playoff matches or cup matches.

- [ ] **Step 5: Rebuild DuckDB + sanity query**

```bash
Rscript -e 'sports::rebuild_duckdb()'
Rscript -e '
con <- DBI::dbConnect(duckdb::duckdb(), here::here("sports.duckdb"), read_only = TRUE)
print(DBI::dbGetQuery(con, "SELECT sport, sex, COUNT(*) AS n, MIN(match_date) AS first, MAX(match_date) AS last FROM results WHERE country = \"iceland\" GROUP BY 1, 2 ORDER BY 1, 2"))
'
```

Expected: six rows (3 sports × 2 sexes), each with a reasonable match range. Record the numbers — they become the baseline for Plan 3's fit validation.

- [ ] **Step 6: Commit**

```bash
git add scripts/backfill_ingest.R \
        tests/testthat/test-ingest-integration.R \
        data/facts/
git commit -m "feat: backfill facts/results + facts/schedules for 3 Icelandic leagues

Runs the three federation scrapers (KKÍ basketball, HSÍ handball,
KSÍ football) end-to-end, populating data/facts/{results,schedules}/
for all active Icelandic leagues × both sexes. Closes Plan 1 Task 10
(deferred because legacy match-data CSVs were lost in the migration).

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>"
```

---

## Task 6: Update CLAUDE.md

**Files:**

- Modify: `CLAUDE.md` (change Plan 2 status from "Pending" to "Complete")

- [ ] **Step 1: Update the Status table**

Change Plan 2's row from "Pending" to "✅ Complete — federation scrapers under `R/ingest/`, `data/facts/{results,schedules}/` backfilled for 3 active Icelandic leagues × both sexes."

Mark Plan 1 Task 10 as closed (note under the Status table).

Remove the `(Plan 4 backfill)` placeholders under `data/facts/results/` and `data/facts/schedules/` in the directory tree.

- [ ] **Step 2: Add a short "Ingest" section under Conventions**

Covering:
- `ingest_league(league, sex)` is the only public entry point — routes via `leagues.yml` `data_source`.
- New data sources register via `register_ingest_source(name, module)` in their own R file.
- Parsers are kept pure and fixture-tested; HTTP/Chrome fetches are isolated.

- [ ] **Step 3: Commit**

```bash
git add CLAUDE.md
git commit -m "docs: update CLAUDE.md — Plan 2 complete, Task 10 closed

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>"
```

---

## Final validation

- [ ] **Step 1: Run full test suite**

```bash
cd /Users/brynjolfurjonsson/sports
Rscript -e 'devtools::test()'
```

Expected: 64 (Plan 1 baseline) + ~15 new assertions ≈ 80+ passing, zero failures.

- [ ] **Step 2: Verify Parquet + DuckDB query**

```bash
Rscript -e '
sports::rebuild_duckdb()
con <- DBI::dbConnect(duckdb::duckdb(), here::here("sports.duckdb"), read_only = TRUE)
print(DBI::dbGetQuery(con, "SELECT sport, sex, COUNT(*) AS n_matches FROM results WHERE country = \"iceland\" GROUP BY 1, 2 ORDER BY 1, 2"))
'
```

Expected: 6 rows, every (sport, sex) has historically-plausible match counts.

- [ ] **Step 3: Push**

```bash
git push origin main
```

---

## What this plan achieves

- Federation scrapers live under `R/ingest/` with a clean, testable interface.
- `data/facts/results/` and `data/facts/schedules/` populated with historical match data for the three Icelandic leagues — closes Plan 1's deferred Task 10.
- Schema validation catches any parser mistake at write time.
- Plan 3 (Model layer) can now use `read_table("results", filter = ...)` as its input.

## Risks & mitigations

- **Selector drift on federation sites.** Mitigated by separating fetch from parse (fixture-tested parsers), and by the `cli_alert_danger` fail-soft wrapper in the backfill script.
- **Network flakes during backfill.** The backfill is idempotent — re-running with the same seasons overwrites the affected partitions. Safe to retry.
- **Unknown column renames in Baskethotel XLSX.** The Task 2 parser implementation step explicitly requires inspecting the fixture before committing; don't commit with a broken rename.
- **Legacy `box::use()` patterns in scraper code.** Do NOT preserve them during the port. Use plain `library()` or explicit `pkg::fn()` calls.
