# Handball + basketball Lengjan odds, Milestone A: implementation plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Start scraping Icelandic handball and basketball odds from Lengjan's JSON API now, route handball
into paper recommendations, and enforce a five-stage betting ladder at every layer. Football stays as it
is.

**Architecture:**
- **Betting ladder.** An ordinal `betting.mode` (`off < scrape < paper < manual < auto`) replaces the
  boolean `betting.enabled`. Each layer (odds ingest, decide, placer, autoplace, health) asks for the
  stage it needs.
- **API client.** A new `R/lengjan-api.R` reads the public API. `current-program` gives the events;
  `markets` gives the prices, at most 20 events per call.
- **Canonical odds rows.** The client maps prices onto the existing odds vocabulary and stamps four new
  nullable columns: `sex`, `event_id`, `competition_id` and `kickoff_at`.
- **Per-league source.** `lengjan.source: api | dom` picks the scraper per league. Football stays on
  the Chromote DOM scraper (the Milestone B cutover is a separate plan).

**Tech Stack:** R package (devtools/testthat 3e/roxygen2), `httr2`, `arrow` (hive-partitioned Parquet),
`jsonvalidate` (ajv), GitHub Actions.

**Spec:** `docs/superpowers/specs/2026-09-23-hb-bb-lengjan-odds-staged-betting-design.md`. This plan
implements Milestone A: WS1, WS2, WS4, WS5, WS7 and WS9. Milestone B (WS3, the football cutover) and
Milestone C (WS6 placer, WS8 paper report) get their own plans. B needs the API scraper to have run
across at least 2 matchdays, and C needs settled paper candidates, so neither can be judged yet.

## Global Constraints

- **Where to work.** All work happens in the worktree `/Users/brynjolfurjonsson/sports/.worktrees/hb-bb-lengjan-odds`,
  on branch `feat/hb-bb-lengjan-odds`, which was cut from `origin/main`. Never `cd` to `~/sports`. Never
  touch `data/decisions/ledger/`.
- **How to run tests.** Always use this exact form. With a relative path or `devtools::test()`, the
  harness can resolve to the main checkout:
  `LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 Rscript -e 'devtools::load_all(".", quiet = TRUE); testthat::test_file(normalizePath("tests/testthat/<file>.R"))'`.
  The UTF-8 locale is required, because under the shell's `LC_CTYPE=C` Icelandic rows are silently
  mangled.
- **ASCII-only R sources.** In `R/`, every Icelandic character is written as a `\uXXXX` escape
  (`.claude/rules/sports-betting.md`, "C-locale R + non-ASCII literals"). Test files and fixtures may
  hold UTF-8, as the existing tests do.
- **No network in tests.** API tests read `tests/testthat/fixtures/lengjan-api/current-program.json`
  and `markets.json`. Both were captured live on 2026-09-23 and are committed in Task 5.
- **Far-future dates.** Any "upcoming match" fixture uses a far-future date (`2100-…`) or an injected
  `now`. The real-clock filters ignore `now` (the time-bomb fixture gotcha).
- **Betting ladder semantics.**
  - The order is `off < scrape < paper < manual < auto`.
  - An absent `mode` with no `enabled` key means `auto`; `enabled: false` means `off`.
  - Setting both keys is a schema error.
  - Each layer's gate: ingest needs `scrape`, decide needs `paper`, the placer needs `manual`,
    autoplace needs `auto`.
- **API facts** (spec L1–L3). Base `https://games.lotto.is/api/proxy/lengjan`, no login.
  - `markets` takes at most **20** `eventIds[i]` per call.
  - Prices are integers ×100 (`139` = 1.39).
  - The user agent is `sports-pipeline (+https://github.com/metill-is/sports)`.
- **Market mapping** (spec WS2, pinned by the fixture):
  - The full-time 1X2 is the market with `primary: TRUE` and `typeName: "3WAY"`; its outcomes
    `1/X/2` become `home/draw/away`.
  - Totals are group `OU_FT` with `typeName: "OU"`; `Yfir/Undir` become `over/under`, and the line is
    `specialValue`.
  - Spread is group `HC_FT` with `typeName: "HC"`; its outcomes `1/X/2` become `home/draw/away`, and
    the line is `specialValue`, i.e. home's signed handicap.
  - Everything else is dropped.
- **`match_date`** is `as.Date(kickoff_at, tz = "UTC")`. Iceland is on UTC all year.
- **Placer and discovery isolation.** The placer never runs on CI (`test-placer-ci-isolation.R`), and
  discovery never references ledger or placer tokens (`test-discover-ci-isolation.R`).
- **Exports.** After adding or removing an `@export`, or adding an `R/` file with `@include`, run
  `Rscript -e 'devtools::document(".")'` and commit the regenerated `NAMESPACE`, `DESCRIPTION`
  (Collate) and `man/`.
- **Commits.**
  - Run `git fetch -q origin && git branch --show-current` immediately before every commit, and
    expect `feat/hb-bb-lengjan-odds`.
  - Every commit message ends with `Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>`.
  - Commit prose goes in single quotes; backticks inside double quotes would run as commands.

## Baseline

**Recorded by the planning session, run started 2026-09-23T21:48Z, on the worktree at `cb0cd7a02`
(spec commit on `e9d60192e`):** `failed = 0`, `errors = 0`, `skipped = 50`, `passed = 6085`. No file
was failing. Re-run it before Task 1 only if `origin/main` has moved and the branch was rebased:

```bash
cd /Users/brynjolfurjonsson/sports/.worktrees/hb-bb-lengjan-odds
LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 Rscript -e 'devtools::load_all(".", quiet = TRUE); r <- as.data.frame(testthat::test_dir(normalizePath("tests/testthat"), load_package = "none", reporter = "summary", stop_on_failure = FALSE)); cat("failed=", sum(r$failed), " errors=", sum(r$error), "\n"); print(unique(r$file[r$failed > 0 | r$error]))'
```

Write the failing files down. Task 12 compares against this list, so only *new* failures count.

## Review Focus

These are the five inputs most likely to hurt a real run that the spec implies but does not spell out.
Each one has a pinning test in the task named.

1. **Mixed-format partition on the first scrape after deploy.** Today's `scraped_date` partition
   already holds the morning's legacy 10-column rows, which have a tz-naive `scraped_at` and no `sex`.
   The new upsert must keep them; odds snapshots cannot be re-scraped. (Task 1)
2. **The API changes shape.** If the `events`/`liveSoon` arrays or a group's `markets` array go
   missing, the run must fail loudly. It must not "succeed" with 0 rows and hide the break. (Task 5)
3. **Suspended or odd prices.** A closed market, a `null` price or `100` (1.00) must be dropped row by
   row. Otherwise it aborts the whole write through `validate_values()`. (Task 5)
4. **Same clubs, both sexes, same day, one scrape.** Both rows must be kept by the natural key, and
   each (league, sex) decide must see only its own. (Task 1 and Task 7)
5. **Paper recommendations pending when autoplace fires.** They must never reach `place_fn`, and the
   run must record `nothing_pending`, not `failed`. (Task 4)

---

### Task 1: Odds schema evolution (optional columns, `sex` in the key, a read path that surfaces new columns)

**Why:** Two data-loss traps were verified on the real store on 2026-09-23.

- **(a) Upsert overwrites a day's snapshots.** `upsert_table()` merges with existing rows only when
  `all(nat_key %in% names(existing))` (`R/storage.R`). Once `sex` joins the key, the first scrape into
  a legacy-format partition would overwrite that day's earlier snapshots.
- **(b) Reads drop new columns.** The odds store mixes `scraped_at` as `timestamp[us, tz=UTC]` (148
  files) and tz-naive `timestamp[us]` (47 files). So `unify_schemas = TRUE` fails, and `read_table()`
  falls back to Arrow's first-fragment-wins open, which **drops** `sex`/`event_id`/… for every reader.
  An explicit read schema with `timestamp[us, tz=UTC]` reads all 66,337 rows with identical instants
  and surfaces the new columns. `timestamp[s]` fails with "would lose data".

**Files:**
- Modify: `R/storage-schemas.R` (the `odds` schema, plus a new `optional_columns()`)
- Modify: `R/storage.R` (new `fill_optional_columns()` and `read_schema()`; changes to `write_table()`,
  `upsert_table()`, `natural_key_for()` and `read_table()`)
- Create: `tests/testthat/test-storage-odds-evolution.R`

**Interfaces:**
- Produces:
  - `schemas()$odds` gains `sex` (string), `event_id` (string), `competition_id` (string) and
    `kickoff_at` (timestamp UTC).
  - `optional_columns(table)` returns a named list mapping each column to a length-1 typed NA.
  - `fill_optional_columns(df, table)` returns `df`.
  - The odds natural key gains `sex`.
  - `read_table("odds")` always returns the four new columns.
- Consumes: nothing new.

- [ ] **Step 1: Write the failing tests**

Create `tests/testthat/test-storage-odds-evolution.R`:

```r
# Odds schema evolution (spec 2026-09-23 WS2): four nullable columns, `sex` in
# the natural key, and a read path that surfaces columns older fragments lack.

.evo_odds <- function(scraped_at, sex = NULL, event_id = NULL, odds = 2.0) {
  df <- tibble::tibble(
    sport = "handball", country = "iceland",
    scraped_at = as.POSIXct(scraped_at, tz = "UTC"),
    match_date = as.Date("2100-01-05"),
    home_team = "Valur", away_team = "Haukar",
    market = "moneyline", outcome = "home", line = NA_real_, odds = odds
  )
  if (!is.null(sex)) df$sex <- sex
  if (!is.null(event_id)) df$event_id <- event_id
  df
}

# Recreate the legacy on-disk format 47 real odds files have: only the 10
# original columns and a tz-NAIVE scraped_at. Written straight through arrow so
# neither write_table() nor its optional-column fill touches it.
.write_legacy_odds <- function(root, scraped_at) {
  t <- as.POSIXct(scraped_at, tz = "UTC")
  tab <- arrow::arrow_table(
    scraped_at = arrow::Array$create(t)$cast(arrow::timestamp("us")),
    match_date = as.Date("2100-01-05"),
    home_team = "Valur", away_team = "Haukar",
    market = "moneyline", outcome = "home", line = NA_real_, odds = 1.9,
    sport = "handball", country = "iceland",
    scraped_date = format(as.Date(t, tz = "UTC"))
  )
  arrow::write_dataset(
    tab, fs::path(root, "facts", "odds"),
    partitioning = c("sport", "country", "scraped_date")
  )
}

test_that("a writer that omits the optional columns still writes; they read back NA", {
  root <- withr::local_tempdir()
  write_table(.evo_odds("2100-01-01 08:00:00"), "odds", root = root)
  back <- read_table("odds", root = root)
  expect_true(all(c("sex", "event_id", "competition_id", "kickoff_at") %in% names(back)))
  expect_true(is.na(back$sex))
  expect_s3_class(back$kickoff_at, "POSIXct")
})

test_that("upserting into a legacy-format same-day partition keeps the legacy rows", {
  # The first post-deploy scrape lands in a scraped_date partition the morning's
  # legacy-format scrape already wrote. With `sex` in the natural key but absent
  # from the existing rows, upsert_table() must still merge, not overwrite:
  # odds snapshots cannot be re-scraped. A new-format partition on another day
  # makes the store mixed-tz like production, so upsert's partition read goes
  # through read_table()'s explicit-schema fallback with a Date filter -- the
  # exact path the real store takes (verified 2026-09-23).
  root <- withr::local_tempdir()
  write_table(.evo_odds("2100-01-03 08:00:00", sex = "male"), "odds", root = root)
  .write_legacy_odds(root, "2100-01-01 08:00:00")
  upsert_table(
    .evo_odds("2100-01-01 14:00:00", sex = "male", event_id = "4602104"),
    "odds",
    root = root
  )
  back <- read_table("odds", root = root)
  day1 <- back[as.Date(back$scraped_at, tz = "UTC") == as.Date("2100-01-01"), ]
  hh <- format(day1$scraped_at, "%H", tz = "UTC")
  expect_equal(nrow(back), 3L)
  expect_equal(nrow(day1), 2L)
  expect_setequal(hh, c("08", "14"))
  expect_equal(day1$sex[hh == "14"], "male")
  expect_true(is.na(day1$sex[hh == "08"]))
})

test_that("read_table surfaces a new column when fragments cannot be unified", {
  # tz-naive legacy fragment + tz-aware new fragment: unify_schemas fails, and a
  # first-fragment-wins open would drop `event_id` for every reader.
  root <- withr::local_tempdir()
  .write_legacy_odds(root, "2100-01-01 08:00:00")
  write_table(
    .evo_odds("2100-01-02 08:00:00", sex = "male", event_id = "42"),
    "odds",
    root = root
  )
  back <- read_table("odds", root = root)
  expect_equal(nrow(back), 2L)
  expect_true("event_id" %in% names(back))
  expect_equal(sort(back$event_id, na.last = TRUE), c("42", NA))
  expect_equal(attr(back$scraped_at, "tzone"), "UTC")
  # The tz-naive instant reads back unchanged.
  expect_true(as.POSIXct("2100-01-01 08:00:00", tz = "UTC") %in% back$scraped_at)
})

test_that("upsert fills optional columns a legacy-only partition lacks before its key check", {
  # Pins fill_optional_columns(existing): with ONLY legacy fragments the store
  # unifies cleanly, so read_table() returns rows with no `sex` column, and
  # without the fill the natural-key check fails and the partition is
  # overwritten. Passes on the pre-change code (whose key lacks `sex`); Step 5
  # proves it guards the fill by deleting the fill line and watching it fail.
  root <- withr::local_tempdir()
  .write_legacy_odds(root, "2100-01-01 08:00:00")
  upsert_table(.evo_odds("2100-01-01 14:00:00", sex = "male"), "odds", root = root)
  back <- read_table("odds", root = root)
  expect_equal(nrow(back), 2L)
  expect_setequal(format(back$scraped_at, "%H", tz = "UTC"), c("08", "14"))
})

test_that("men's and women's odds between the same clubs in one scrape are both kept", {
  # Two upserts, so the second meets the first in an existing partition and the
  # natural key decides: without `sex` in it, the female row replaces the male.
  root <- withr::local_tempdir()
  upsert_table(.evo_odds("2100-01-01 08:00:00", sex = "male", odds = 1.8), "odds", root = root)
  upsert_table(.evo_odds("2100-01-01 08:00:00", sex = "female", odds = 2.4), "odds", root = root)
  back <- read_table("odds", root = root)
  expect_equal(nrow(back), 2L)
  expect_setequal(back$sex, c("male", "female"))
})
```

- [ ] **Step 2: Run the tests and confirm they fail**

Run: `LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 Rscript -e 'devtools::load_all(".", quiet = TRUE); testthat::test_file(normalizePath("tests/testthat/test-storage-odds-evolution.R"))'`
Expected: tests 1, 3 and 5 FAIL; tests 2 and 4 PASS.
- Test 1: the four columns are absent.
- Test 3: the mixed-tz store falls back to first-fragment-wins, which drops `event_id` on read.
- Test 5: `sex` isn't in the key yet, so the second upsert's female row replaces the male one.
- Tests 2 and 4 already pass: the old key has no `sex`, so the upsert merges. They guard the
  *post-change* key against overwriting a legacy partition. Step 5 proves test 4 does that with a
  mutation check.

- [ ] **Step 3: Extend the odds schema and add `optional_columns()`**

In `R/storage-schemas.R`, replace the `odds = arrow::schema(...)` entry with:

```r
    odds = arrow::schema(
      sport       = arrow::string(),
      country     = arrow::string(),
      scraped_at  = ts,
      match_date  = arrow::date32(),
      home_team   = arrow::string(),
      away_team   = arrow::string(),
      market      = arrow::string(),
      outcome     = arrow::string(),
      line        = arrow::float64(),
      odds        = arrow::float64(),
      # Nullable, added 2026-09-23 with the Lengjan JSON-API scraper (spec
      # 2026-09-23 WS2). Writers may omit them -- optional_columns() fills typed
      # NA -- so DOM-scraper rows and all earlier history carry NA.
      sex            = arrow::string(),
      event_id       = arrow::string(),
      competition_id = arrow::string(),
      kickoff_at     = ts
    ),
```

At the end of `R/storage-schemas.R`, add:

```r
#' Nullable columns a writer may omit, with the typed NA each is filled with.
#'
#' Schema evolution without touching every writer: `write_table()` and
#' `upsert_table()` add any of these a frame lacks before validating, so legacy
#' writers (the DOM scraper, test fixtures, the ETL) keep working and legacy
#' partitions merge cleanly with new rows.
#'
#' @param table Table name.
#' @return Named list: column name -> length-1 typed NA; `list()` when the
#'   table has no optional columns.
#' @noRd
optional_columns <- function(table) {
  switch(table,
    odds = list(
      sex = NA_character_,
      event_id = NA_character_,
      competition_id = NA_character_,
      kickoff_at = .POSIXct(NA_real_, tz = "UTC")
    ),
    list()
  )
}
```

- [ ] **Step 4: Fill optional columns on write and upsert, key odds on `sex`, and read with an explicit schema**

In `R/storage.R`:

(a) Directly after `add_virtual_partitions()`'s closing brace, add:

```r
#' Add any missing optional columns (see [optional_columns()]) as typed NA.
#' @noRd
fill_optional_columns <- function(df, table) {
  opt <- optional_columns(table)
  for (nm in setdiff(names(opt), names(df))) {
    df[[nm]] <- rep(opt[[nm]], nrow(df))
  }
  df
}

#' Explicit read schema for a table whose fragments cannot be unified.
#'
#' The odds store mixes `scraped_at` as timestamp[us, tz=UTC] (148 files) and
#' tz-naive timestamp[us] (47 files; counted 2026-09-23), so
#' `unify_schemas = TRUE` fails and Arrow's default open takes the first
#' fragment's schema -- silently dropping any column added later (odds.sex,
#' odds.event_id, ...). Casting every fragment to this schema null-fills absent
#' columns and reads the tz-naive values as the same UTC instants (verified
#' 2026-09-23: 66,337 rows either way). Timestamps are microsecond because that
#' is what arrow writes from POSIXct; a second-unit schema fails with "would
#' lose data". Hive partition columns are strings, as the default open infers.
#' @noRd
read_schema <- function(table) {
  s <- schemas()[[table]]
  fields <- lapply(s$names, function(nm) {
    type <- s$GetFieldByName(nm)$type
    if (grepl("^timestamp", type$ToString())) {
      type <- arrow::timestamp(unit = "us", timezone = "UTC")
    }
    arrow::field(nm, type)
  })
  parts <- setdiff(table_partitions()[[table]], s$names)
  fields <- c(fields, lapply(parts, function(p) arrow::field(p, arrow::string())))
  do.call(arrow::schema, fields)
}
```

(b) In `write_table()`, change

```r
  df <- add_virtual_partitions(df, table)
  validate_against_schema(df, table)
```

to

```r
  df <- fill_optional_columns(add_virtual_partitions(df, table), table)
  validate_against_schema(df, table)
```

(c) In `upsert_table()`, change `df <- add_virtual_partitions(df, table)` to
`df <- fill_optional_columns(add_virtual_partitions(df, table), table)`. Then, directly after the
`existing <- tryCatch(read_table(...), ...)` assignment, add:

```r
    # A partition written before a column became optional lacks it; fill it so
    # the natural-key check below merges instead of overwriting the partition
    # (spec 2026-09-23 Review Focus 1: odds snapshots cannot be re-scraped).
    if (nrow(existing) > 0L) existing <- fill_optional_columns(existing, table)
```

(d) In `natural_key_for()`, change the odds key to:

```r
    odds = c(
      "sport", "country", "sex", "scraped_at", "match_date",
      "home_team", "away_team", "market", "outcome", "line"
    ),
```

(e) In `read_table()`, change the fallback open

```r
    error = function(e) arrow::open_dataset(src, partitioning = partitioning)
```

to

```r
    error = function(e) {
      arrow::open_dataset(src, partitioning = partitioning, schema = read_schema(table))
    }
```

In the comment above that `tryCatch`, replace these two lines:

```r
  # timestamp[us]) that unification cannot merge; there we fall back to the
  # default open, preserving today's behaviour. Arrow raises the merge error at
```

with:

```r
  # timestamp[us]) that unification cannot merge; there we fall back to an
  # explicit read schema (read_schema()), which reads every fragment and keeps
  # columns added after the first. Arrow raises the merge error at
```

- [ ] **Step 5: Run the new tests and the existing storage tests**

Run the new file, then each of `test-storage.R`, `test-storage-upsert.R`, `test-storage-schemas.R`,
`test-storage-validate.R`, `test-decide-odds.R` and `test-backtest-walkforward.R`, all in the same
test command form.
Expected: all PASS. The schema tests use `%in%`, so the extra columns don't break them.

Next, prove that test 4 guards the fill with a mutation check.
1. Temporarily delete the line
   `if (nrow(existing) > 0L) existing <- fill_optional_columns(existing, table)` from
   `upsert_table()`.
2. Re-run `test-storage-odds-evolution.R`. Test 4 must FAIL with 1 row, because the legacy partition
   was overwritten.
3. Put the line back, check with `git diff R/storage.R` that it's present, and re-run. Everything
   must PASS.

- [ ] **Step 6: Check the real store still reads end to end**

Run: `LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 Rscript -e 'devtools::load_all(".", quiet = TRUE); o <- read_table("odds"); cat(nrow(o), paste(names(o), collapse=","), "\n")'`
Expected: `66337` or more rows. The printed names include `sex,event_id,competition_id,kickoff_at`.

- [ ] **Step 7: Commit**

```bash
git fetch -q origin && git branch --show-current
git add R/storage-schemas.R R/storage.R tests/testthat/test-storage-odds-evolution.R
git commit -m 'feat(storage): nullable odds sex/event_id/competition_id/kickoff_at

Writers may omit the new columns (optional_columns() fills typed NA), sex
joins the odds natural key so both sexes of a club pairing survive one
scrape, and upsert fills legacy partitions before its key check so the
first post-deploy scrape merges instead of overwriting the day.
read_table() falls back to an explicit schema: the store mixes tz-aware
and tz-naive scraped_at, so the old first-fragment-wins open would have
dropped every new column.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>'
```

---

### Task 2: Betting-ladder predicates and schema

**Files:**
- Modify: `R/config.R` (replace `betting_enabled()`; add `betting_mode()`, `betting_mode_at_least()`
  and `normalise_betting_modes()`, which `load_leagues()` calls). YAML 1.1 reads an unquoted
  `mode: off` as logical `FALSE`, so a hand-written `off` would fail the string schema, and
  `load_leagues()` would abort every pipeline script.
- Modify: `config/leagues.schema.json` (`betting.mode`, a rule against setting both keys,
  `lengjan.source`, and competition `division`)
- Create: `tests/testthat/test-betting-ladder.R`
- Modify: `tests/testthat/test-config-betting-schema.R` (append tests)

**Interfaces:**
- Produces:
  - `betting_mode(league)` returns one of `"off"`, `"scrape"`, `"paper"`, `"manual"`, `"auto"`.
  - `betting_mode_at_least(league, stage)` returns a logical.
  - `betting_enabled(league)` becomes `betting_mode_at_least(league, "paper")`.
  - The schema accepts `betting.mode`, `lengjan.source` (`api`/`dom`) and `competitions[].division`.
- Consumes: nothing new.

- [ ] **Step 1: Write the failing tests**

Create `tests/testthat/test-betting-ladder.R`:

```r
# Betting ladder (spec 2026-09-23 WS1): off < scrape < paper < manual < auto.

test_that("betting_mode reads betting.mode when set", {
  for (m in c("off", "scrape", "paper", "manual", "auto")) {
    expect_equal(betting_mode(list(betting = list(mode = m))), m)
  }
})

test_that("betting_mode falls back to the legacy enabled flag", {
  expect_equal(betting_mode(list(betting = list(enabled = FALSE))), "off")
  expect_equal(betting_mode(list(betting = list(enabled = TRUE))), "auto")
  expect_equal(betting_mode(list(betting = list(kelly_frac = 0.1))), "auto")
  expect_equal(betting_mode(list(sport = "football")), "auto")
  expect_equal(betting_mode(list(betting = NULL)), "auto")
})

test_that("betting_mode rejects an unknown stage", {
  expect_error(betting_mode(list(betting = list(mode = "live"))), "betting.mode")
})

test_that("betting_mode reads a YAML-boolean FALSE mode as 'off'", {
  # YAML 1.1: an unquoted `mode: off` parses as FALSE.
  expect_equal(betting_mode(list(betting = list(mode = FALSE))), "off")
})

test_that("betting_mode_at_least orders the ladder", {
  modes <- c("off", "scrape", "paper", "manual", "auto")
  for (i in seq_along(modes)) {
    for (j in seq_along(modes)) {
      expect_identical(
        betting_mode_at_least(list(betting = list(mode = modes[[i]])), modes[[j]]),
        i >= j,
        info = paste(modes[[i]], ">=", modes[[j]])
      )
    }
  }
})

test_that("betting_enabled is exactly 'mode at least paper'", {
  expect_false(betting_enabled(list(betting = list(mode = "scrape"))))
  expect_true(betting_enabled(list(betting = list(mode = "paper"))))
  expect_false(betting_enabled(list(betting = list(enabled = FALSE))))
  expect_true(betting_enabled(list(sport = "football")))
})
```

Append to `tests/testthat/test-config-betting-schema.R`:

```r
# --- 2026-09-23 WS1/WS2: betting.mode, lengjan.source, competition division ---

test_that("schema accepts every betting.mode stage", {
  for (m in c("off", "scrape", "paper", "manual", "auto")) {
    expect_no_error(load_leagues(path = .bt_write(.bt_league(mode = m))))
  }
})

test_that("schema rejects an unknown betting.mode", {
  expect_error(
    load_leagues(path = .bt_write(.bt_league(mode = "live"))),
    "schema validation"
  )
})

test_that("an unquoted YAML `mode: off` loads as mode off", {
  # yaml::yaml.load("mode: off") is FALSE (YAML 1.1). load_leagues() maps it
  # back before validating, so a hand-edited config neither fails the string
  # schema nor aborts every pipeline script.
  txt <- yaml::as.yaml(list(handball_iceland = .bt_league()))
  txt <- sub("  betting:\n", "  betting:\n    mode: off\n", txt, fixed = TRUE)
  tmp <- withr::local_tempfile(fileext = ".yml")
  writeLines(txt, tmp)
  lg <- load_leagues(path = tmp)
  expect_equal(betting_mode(lg$handball_iceland), "off")
})

test_that("schema rejects betting.mode and betting.enabled together", {
  expect_error(
    load_leagues(path = .bt_write(.bt_league(mode = "paper", enabled = FALSE))),
    "schema validation"
  )
})

test_that("schema accepts lengjan.source and a competition division", {
  lg <- .bt_league(mode = "scrape")
  lg$lengjan <- list(
    source = "api",
    competitions = list(list(id = "1269", name = "x", sex = "male", division = "OD"))
  )
  expect_no_error(load_leagues(path = .bt_write(lg)))
})

test_that("schema rejects an unknown lengjan.source", {
  lg <- .bt_league()
  lg$lengjan <- list(source = "html", competitions = list())
  expect_error(load_leagues(path = .bt_write(lg)), "schema validation")
})
```

- [ ] **Step 2: Run the tests and confirm they fail**

Run `test-betting-ladder.R` and `test-config-betting-schema.R`.
Expected: FAIL, because `betting_mode` doesn't exist and the schema rejects `mode` (`betting` is
`additionalProperties: false`).

- [ ] **Step 3: Add the predicates**

In `R/config.R`, replace the whole `betting_enabled()` roxygen block and function (from the
`#' Is betting enabled for a league?` line through the closing brace) with:

```r
#' The betting ladder, lowest stage first (spec 2026-09-23 WS1).
#'
#' `off`: nothing. `scrape`: odds only. `paper`: + decide (candidates and
#' recommendations the placer never places). `manual`: + `place_bets.R`.
#' `auto`: + the unattended launchd placer.
#' @noRd
.BETTING_MODES <- c("off", "scrape", "paper", "manual", "auto")

#' A league's stage on the betting ladder.
#'
#' Reads `betting$mode`. Without it the legacy boolean decides: an explicit
#' `enabled: false` is `"off"`; anything else (absent key, `TRUE`, no betting
#' block) is `"auto"`, so football, which carries neither key, is unchanged.
#' The schema forbids setting both keys.
#'
#' @param league A league definition (an element of [load_leagues()]), or any
#'   list carrying a `betting` slice.
#' @return One of `"off"`, `"scrape"`, `"paper"`, `"manual"`, `"auto"`.
#' @export
betting_mode <- function(league) {
  mode <- league$betting$mode
  # YAML 1.1 reads an unquoted `off` as FALSE (see normalise_betting_modes()).
  if (isFALSE(mode)) {
    return("off")
  }
  if (!is.null(mode)) {
    if (!(is.character(mode) && length(mode) == 1L && mode %in% .BETTING_MODES)) {
      stop(
        "betting.mode must be one of: ", paste(.BETTING_MODES, collapse = ", "),
        call. = FALSE
      )
    }
    return(mode)
  }
  if (isFALSE(league$betting$enabled)) "off" else "auto"
}

#' Is a league at or above a stage of the betting ladder?
#'
#' Each layer asks for the stage it needs: odds ingest `"scrape"`, decide
#' `"paper"`, the placer `"manual"`, the unattended placer `"auto"`.
#'
#' @param league A league definition.
#' @param stage One of `"off"`, `"scrape"`, `"paper"`, `"manual"`, `"auto"`.
#' @return `TRUE` or `FALSE`.
#' @export
betting_mode_at_least <- function(league, stage) {
  stage <- match.arg(stage, .BETTING_MODES)
  match(betting_mode(league), .BETTING_MODES) >= match(stage, .BETTING_MODES)
}

#' Does the decide layer run for a league?
#'
#' `betting.mode` at least `"paper"`. Kept under this name for readers that
#' predate the ladder -- [order_fit_targets()] fits these leagues first, which
#' is right for paper leagues too (their recommendations want fresh fits).
#'
#' @param league A league definition.
#' @return `TRUE` when decide runs for the league.
#' @export
betting_enabled <- function(league) {
  betting_mode_at_least(league, "paper")
}

#' Map a YAML-boolean `betting.mode` back to `"off"`.
#'
#' YAML 1.1 (the `yaml` package) reads an unquoted `mode: off` as logical
#' `FALSE` -- the "Norway problem". Undone before schema validation so a
#' hand-written `off` loads instead of failing the string schema.
#' @noRd
normalise_betting_modes <- function(leagues) {
  for (key in names(leagues)) {
    if (isFALSE(leagues[[key]]$betting$mode)) {
      leagues[[key]]$betting$mode <- "off"
    }
  }
  leagues
}
```

In `load_leagues()`, directly after the `if (is.null(leagues)) { stop(...) }` block, add:

```r
  leagues <- normalise_betting_modes(leagues)
```

- [ ] **Step 4: Extend the schema**

Edit `config/leagues.schema.json` with Python. JSON is not hand-written, and every anchor is asserted
unique:

```bash
python3 - <<'EOF'
from pathlib import Path
p = Path("config/leagues.schema.json")
s = p.read_text(encoding="utf-8")
def swap(old, new):
    global s
    assert s.count(old) == 1, (s.count(old), old[:70])
    s = s.replace(old, new)

swap('''                  "sex":  { "type": "string", "enum": ["male", "female"] }
                }''',
'''                  "sex":  { "type": "string", "enum": ["male", "female"] },
                  "division": {
                    "type": "string",
                    "description": "Division code this competition covers (e.g. OD, BD). Optional; when every competition of a league carries one, odds_freshness expects odds only in these (sex, division) cells (spec 2026-09-23 WS7)."
                  }
                }''')

swap('''        "lengjan": {
          "type": "object",
          "properties": {
            "competitions": {''',
'''        "lengjan": {
          "type": "object",
          "properties": {
            "source": {
              "type": "string",
              "enum": ["api", "dom"],
              "description": "Which Lengjan scraper serves this league: api = the public JSON API (R/lengjan-api.R), dom = the Chromote DOM scraper (R/ingest-lengjan-odds.R). Absent means dom (spec 2026-09-23 WS2/WS3)."
            },
            "competitions": {''')

swap('''          "required": ["kelly_frac", "ev_threshold", "markets", "scoring", "min_bet"],
          "properties": {''',
'''          "required": ["kelly_frac", "ev_threshold", "markets", "scoring", "min_bet"],
          "not": { "required": ["mode", "enabled"] },
          "properties": {
            "mode": {
              "type": "string",
              "enum": ["off", "scrape", "paper", "manual", "auto"],
              "description": "Betting ladder (spec 2026-09-23 WS1): off < scrape (odds only) < paper (+ decide; recommendations never placed) < manual (+ place_bets.R) < auto (+ launchd autoplace). Absent: `enabled` decides (false -> off, else auto). Never set both."
            },''')

swap('''              "description": "Publish-only switch. When false, decide/placer/odds-ingest refuse this league (spec 2026-09-02 §3, decision D2). Absent means enabled -- football carries no key."''',
'''              "description": "Legacy switch, superseded by `mode` (spec 2026-09-23 WS1): false is mode off, true or absent is mode auto. Kept so configs that predate the ladder still load. Never set together with `mode`."''')

p.write_text(s, encoding="utf-8")
import json; json.loads(s); print("schema OK")
EOF
```

Expected output: `schema OK`.

- [ ] **Step 5: Run the tests and confirm they pass**

Run `test-betting-ladder.R`, `test-config-betting-schema.R`, `test-betting-interlock.R` and
`test-config.R`.
Expected: all PASS. The interlock tests still pass because `enabled: false` now maps to `off`, which
is below every stage, and nothing in the shipped config has changed yet.

- [ ] **Step 6: Regenerate docs and commit**

```bash
Rscript -e 'devtools::document(".")'
git fetch -q origin && git branch --show-current
git add R/config.R config/leagues.schema.json tests/testthat/test-betting-ladder.R tests/testthat/test-config-betting-schema.R NAMESPACE man/
git commit -m 'feat(config): betting.mode ladder off < scrape < paper < manual < auto

betting_mode() and betting_mode_at_least() replace the boolean
betting.enabled; enabled: false maps to off and an absent key to auto, so
football is unchanged. The schema gains betting.mode (never together with
enabled), lengjan.source (api|dom) and an optional competition division.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>'
```

---

### Task 3: Enforce the ladder in odds ingest, decide, the placer and fit priority

**Files:**
- Modify: `R/ingest.R` (the `ingest_one_lengjan()` gate and its roxygen `@param betting`)
- Modify: `R/decide-pipeline.R:83-96` (the decide gate)
- Modify: `R/placer-load.R` (`drop_betting_disabled()`)
- Modify: `R/placer-validate.R` (`validate_betting_enabled()`)
- Modify: `R/model-league.R` (`order_fit_targets()`: rank by ladder stage)
- Modify: `tests/testthat/test-betting-ladder.R` (append the per-layer tests)

**Why fit priority is here.** `order_fit_targets()` puts `betting_enabled()` leagues first so a
`fit.yml` timeout (240 min; football alone took 196 min on 2026-08-28) cuts a publish-only fit, never
the money fit. Config order is basketball, handball, football. Once handball is `paper`, a two-tier
sort would fit **handball before football**, and the timeout would cut football. Ranking by ladder
stage keeps money first: `auto` > `manual` > `paper` > `scrape`/`off`.

**Interfaces:**
- Consumes: `betting_mode()` and `betting_mode_at_least()` from Task 2, and the internal `.BETTING_MODES`.
- Produces:
  - Ingest runs from `scrape` up.
  - Decide returns empty below `paper`.
  - The placer loader and pre-flight keep or accept only `manual` and above.
  - `order_fit_targets()` sorts by stage, highest first, and is stable within a stage.

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-betting-ladder.R`:

```r
# --- one gate per layer -------------------------------------------------------

.ladder_recs <- function() {
  tibble::tibble(
    sport = "handball", country = "iceland", sex = "male",
    run_id = as.POSIXct("2026-09-23 10:00:00", tz = "UTC"),
    match_date = as.Date("2100-01-05"), home_team = "Valur", away_team = "FH",
    market = "moneyline", outcome = "home", line = NA_real_,
    p = 0.6, odds = 2.0, ev = 0.2, kelly = 0.05, bet_amount = 500
  )
}

.ladder_cfg <- function(mode) {
  list(handball_iceland = list(
    sport = "handball", country = "iceland", betting = list(mode = mode)
  ))
}

test_that("odds ingest runs from 'scrape' up and never below", {
  called <- 0L
  testthat::local_mocked_bindings(
    .is_league_active = function(active_path, key) TRUE,
    ingest_lengjan_odds = function(...) {
      called <<- called + 1L
      3L
    }
  )
  for (m in c("off", "scrape", "paper", "manual", "auto")) {
    called <- 0L
    suppressMessages(ingest_one_lengjan(
      list(sport = "handball", country = "iceland"),
      list(competitions = list(list(id = "1269", name = "x", sex = "male"))),
      "handball_iceland", "active.json",
      betting = list(mode = m)
    ))
    expect_identical(called, if (m == "off") 0L else 1L, info = m)
  }
})

test_that("decide produces candidates from 'paper' up and nothing below", {
  run <- function(mode) {
    root <- withr::local_tempdir()
    md <- Sys.Date() + 1L
    set.seed(11)
    write_table(tibble::tibble(
      sport = "handball", country = "iceland", sex = "male",
      fit_date = Sys.Date(), match_date = md,
      home_team = "Valur", away_team = "FH", draw_id = 1:1000L,
      home_goals = rpois(1000, 30), away_goals = rpois(1000, 26)
    ), "beliefs_latest", root = root)
    write_table(tibble::tibble(
      sport = "handball", country = "iceland", scraped_at = Sys.time(),
      match_date = md, home_team = "Valur", away_team = "FH",
      market = "moneyline", outcome = c("home", "away"),
      line = NA_real_, odds = c(2.60, 2.20)
    ), "odds", root = root)
    league <- list(
      sport = "handball", country = "iceland", sexes = "male",
      active = TRUE, stan_model = "x.stan",
      betting = list(
        mode = mode, kelly_frac = 0.10, ev_threshold = 0.0,
        markets = list(moneyline = TRUE, spread = FALSE, total = FALSE),
        scoring = list(has_ties = TRUE, tie_threshold = 0.5),
        min_bet = 1L, max_age_hours = 999999L
      )
    )
    nrow(suppressMessages(decide_league(
      league = league, sex = "male", root = root, return_candidates = TRUE,
      bankroll = list(
        initial_pool = 23610, current_pool = 23610,
        daily_budget_frac = 0.5, daily_budget_min_isk = 1000
      )
    )))
  }
  expect_gt(run("paper"), 0L) # positive control: the fixture does yield candidates
  expect_gt(run("auto"), 0L)
  expect_equal(run("scrape"), 0L)
  expect_equal(run("off"), 0L)
})

test_that("the placer loader keeps recommendations only from 'manual' up", {
  for (m in c("off", "scrape", "paper", "manual", "auto")) {
    out <- suppressMessages(
      drop_betting_disabled(.ladder_recs(), leagues_cfg = .ladder_cfg(m))
    )
    expect_equal(nrow(out), if (m %in% c("manual", "auto")) 1L else 0L, info = m)
  }
})

test_that("the placer pre-flight refuses leagues below 'manual'", {
  expect_error(
    validate_betting_enabled(.ladder_cfg("paper"), .ladder_recs()),
    "handball_iceland"
  )
  expect_true(validate_betting_enabled(.ladder_cfg("manual"), .ladder_recs()))
})

test_that("fit priority follows the ladder: money first, then paper, then scrape", {
  # Config order is basketball, handball, football. A two-tier sort on
  # betting_enabled() would fit paper handball BEFORE auto football, so the
  # fit.yml timeout would cut the money fit.
  targets <- tibble::tibble(
    key = c("basketball_iceland", "handball_iceland", "football_iceland"),
    sex = "male"
  )
  leagues <- list(
    basketball_iceland = list(betting = list(mode = "scrape")),
    handball_iceland = list(betting = list(mode = "paper")),
    football_iceland = list(betting = NULL)
  )
  expect_equal(
    order_fit_targets(targets, leagues)$key,
    c("football_iceland", "handball_iceland", "basketball_iceland")
  )
})
```

- [ ] **Step 2: Run the tests and confirm they fail**

Run `test-betting-ladder.R`.
Expected: FAIL on four assertions. (The decide test already passes: Task 2 made `betting_enabled()`
mean "≥ paper", and decide still calls it.)
- Ingest at `scrape` is skipped, because the gate is `betting_enabled()`, i.e. "≥ paper".
- The placer loader keeps the `paper` recommendations.
- The pre-flight accepts a `paper` league.
- The fit order puts handball first: a two-tier sort, with config order kept within a tier.

- [ ] **Step 3: Rewrite the four gates**

In `R/ingest.R`, change the `@param betting` roxygen to:

```r
#' @param betting Per-league `betting` slice, or `NULL`. Odds are scraped from
#'   `betting.mode` "scrape" up (spec 2026-09-23 WS1); at "off" the scrape is
#'   refused outright. Trailing and defaulted so existing four-arg calls keep
#'   working.
```

and replace the D2 gate block, from `  # D2 interlock.` through its `  }`, with:

```r
  # Betting ladder (spec 2026-09-23 WS1): odds are scraped from mode "scrape"
  # up. Checked before the activation gate, so a league at "off" never touches
  # Lengjan even when it has fixtures today. An absent `betting` slice is
  # "auto" (football carries neither mode nor enabled).
  if (!betting_mode_at_least(list(betting = betting), "scrape")) {
    cli::cli_alert_info("{key}: skipped (betting.mode: off)")
    return(0L)
  }
```

In `R/decide-pipeline.R`, replace the `# 2b. Betting interlock` comment block and its `if` statement
with:

```r
  # 2b. Betting ladder -------------------------------------------------------
  # Spec 2026-09-23 WS1: decide runs from betting.mode "paper" up. A "scrape"
  # league collects odds but is never priced (basketball: Lengjan settles on
  # regulation time, our model does not). decide_league() is the single funnel
  # for decide_one(), scripts/04_decide.R, the walk-forward harness and the
  # replay script, so one guard covers all of them. The placer keeps its own
  # guards: recommendations written before a league was lowered outlive the
  # config change.
  if (!betting_mode_at_least(league, "paper")) {
    cli::cli_alert_info(
      "{league$sport}/{league$country}/{sex}: betting.mode \\
       {betting_mode(league)} is below paper -- no candidates or recommendations"
    )
    # Mirrors the two sibling early-exits below. Note this is currently a
    # no-op: decide_write_empty() passes zero-row frames to write_table(),
    # which returns early without creating a partition. Called anyway so this
    # path stays consistent with them if that ever changes.
    if (write) decide_write_empty(league, sex, run_id, root)
    return(empty_return())
  }
```

In `R/placer-load.R`, replace the roxygen title and description of `drop_betting_disabled()`, and its
body from `disabled <-` down to the warning, with:

```r
#' Drop recommendations from leagues below `betting.mode` "manual".
#'
#' Spec 2026-09-23 WS1: a "paper" league's recommendations exist to be read,
#' never placed, and rows written before a league was lowered outlive the
#' config change -- `run_auto_place()` places every pending recommendation by
#' design, so the placer filters on read as well.
#'
#' @param recs Recommendation rows.
#' @param leagues_cfg Named league config, defaulting to [load_leagues()].
#'   Named distinctly from `load_recommendations()`'s `leagues`, which is a
#'   character vector of keys to keep, not a config.
#' @return `recs` without rows for leagues below "manual".
#' @keywords internal
#' @noRd
drop_betting_disabled <- function(recs, leagues_cfg = NULL) {
  if (nrow(recs) == 0L) {
    return(recs)
  }
  if (is.null(leagues_cfg)) leagues_cfg <- load_leagues()

  disabled <- names(leagues_cfg)[
    !vapply(leagues_cfg, betting_mode_at_least, logical(1), stage = "manual")
  ]
  if (length(disabled) == 0L) {
    return(recs)
  }

  drop <- paste0(recs$sport, "_", recs$country) %in% disabled
  if (any(drop)) {
    cli::cli_alert_warning(
      "Dropping {sum(drop)} recommendation{?s} for league{?s} below \\
       betting.mode manual: \\
       {.val {unique(paste0(recs$sport, '_', recs$country)[drop])}}"
    )
  }
  recs[!drop, , drop = FALSE]
}
```

In `R/placer-validate.R` `validate_betting_enabled()`, replace the `disabled <- …` line and the `stop()`
message with:

```r
  disabled <- names(leagues)[
    !vapply(leagues, betting_mode_at_least, logical(1), stage = "manual")
  ]
  offending <- intersect(unique(paste0(recs$sport, "_", recs$country)), disabled)
  if (length(offending) > 0L) {
    stop(
      "validate_betting_enabled: refusing to place bets on ",
      "league(s) below betting.mode manual: ", paste(offending, collapse = ", "),
      ". Set betting.mode: manual (or auto) in config/leagues.yml to arm.",
      call. = FALSE
    )
  }
```

In the same function's roxygen, change "betting-disabled" to "below `betting.mode` manual".

In `R/model-league.R` `order_fit_targets()`, replace the body from `armed <- vapply(` through
`targets[order(!armed), , drop = FALSE]` with:

```r
  # Rank by ladder stage (spec 2026-09-23 WS1): auto > manual > paper >
  # scrape/off, so the money fit always precedes a paper fit, which precedes
  # a scrape-only one. A missing league or slice is "auto" (betting_mode()'s
  # default), keeping the conservative tier on top. order() is stable, so
  # config order (and each league's sex order) holds within a stage.
  stage <- vapply(
    targets$key,
    function(k) match(betting_mode(leagues[[k]]), .BETTING_MODES),
    integer(1),
    USE.NAMES = FALSE
  )
  targets[order(-stage), , drop = FALSE]
```

In its roxygen, replace the paragraph starting `[betting_enabled()] is the data-driven expression`
with:

```r
#' The league's [betting_mode()] stage is the data-driven expression of that
#' difference -- auto, then manual, then paper, then scrape/off -- so no sport
#' name is hardcoded and a league that moves up the ladder moves up here on
#' its own. The sort is stable: within a stage, config order (and each
#' league's declared sex order) is preserved, so a league's rows are never
#' interleaved.
```

and replace these two roxygen lines:

```r
#'   slice. A missing league or slice counts as betting-enabled (the
#'   [betting_enabled()] default), which keeps the conservative tier on top.
```

with:

```r
#'   slice. A missing league or slice counts as mode "auto" (the
#'   [betting_mode()] default), which keeps the conservative tier on top.
```

- [ ] **Step 4: Run the tests and confirm they pass**

Run `test-betting-ladder.R`, `test-betting-interlock.R`, `test-placer-load.R`, `test-placer-preview.R`,
`test-ingest-lengjan-odds.R`, `test-decide-pipeline.R` and `test-pipeline-run-isolation.R`.
Expected: all PASS. The interlock's health test still sees PAUSED, because `check_odds_freshness` is
unchanged until Task 8. `test-pipeline-run-isolation.R`'s fit-order tests use synthetic leagues
(`enabled: FALSE` is `off`, absent is `auto`), so both tiers keep their order.

- [ ] **Step 5: Commit**

```bash
git fetch -q origin && git branch --show-current
git add R/ingest.R R/decide-pipeline.R R/placer-load.R R/placer-validate.R R/model-league.R tests/testthat/test-betting-ladder.R
git commit -m 'feat(betting): gate ingest/decide/placer and fit order on the ladder

Odds ingest runs from scrape up, decide from paper up, and the placer
loader and pre-flight accept manual and auto only, so a paper league
yields recommendations that are never placed. order_fit_targets() ranks
by stage: config order puts handball before football, and a two-tier
sort would have let the fit.yml timeout cut the money fit.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>'
```

---

### Task 4: The autoplace gate (places `auto` leagues only)

**Files:**
- Modify: `R/auto-place.R` (add `auto_place_leagues()`; give `run_auto_place()` a `leagues_cfg`
  parameter and a league filter)
- Modify: `tests/testthat/test-auto-place.R` (append tests)

**Interfaces:**
- Consumes: `betting_mode_at_least()` (Task 2), and `preview_pending(leagues=, leagues_cfg=)` and
  `place_bets(leagues=)` (existing).
- Produces:
  - `auto_place_leagues(leagues_cfg)` returns a character vector of league keys.
  - `run_auto_place(..., leagues_cfg = NULL)` passes `leagues = auto_place_leagues(cfg)` to
    `preview_pending()` and `place_fn()`.

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-auto-place.R`:

```r
# --- betting ladder: autoplace acts on betting.mode "auto" only (spec 2026-09-23)

.ap_bankroll <- function() {
  list(daily_budget_frac = 0.05, current_pool = 1e5, daily_budget_min_isk = 1000)
}

test_that("auto_place_leagues keeps only betting.mode auto", {
  cfg <- list(
    football_iceland = list(sport = "football", betting = NULL),
    handball_iceland = list(sport = "handball", betting = list(mode = "paper")),
    basketball_iceland = list(sport = "basketball", betting = list(mode = "manual"))
  )
  expect_identical(auto_place_leagues(cfg), "football_iceland")
  # Fail closed: NULL would mean "every league" to load_recommendations().
  expect_identical(auto_place_leagues(list()), character(0))
})

test_that("run_auto_place records a config error pulled by the sync, then re-throws", {
  # scripts/auto_place.R only logs a thrown error: it relies on run_auto_place
  # having recorded failed:<reason> itself.
  root <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    load_leagues = function(...) stop("leagues.yml failed schema validation")
  )
  expect_error(
    run_auto_place(
      root = root, now = as.POSIXct("2026-06-01 12:00:00", tz = "UTC"),
      sync_fn = function(...) TRUE,
      place_fn = function(...) stop("must not be reached"),
      bankroll_fn = .ap_bankroll
    ),
    "schema validation"
  )
  expect_match(read_placement_status(root)$status, "^failed:config")
})

test_that("run_auto_place never places a league below betting.mode auto", {
  root <- withr::local_tempdir()
  seed_pending_rec(root) # a football recommendation
  called <- FALSE
  run_auto_place(
    root = root, now = as.POSIXct("2026-06-01 12:00:00", tz = "UTC"),
    sync_fn = function(...) TRUE,
    place_fn = function(...) {
      called <<- TRUE
      tibble::tibble(status = "placed")
    },
    bankroll_fn = .ap_bankroll,
    leagues_cfg = list(football_iceland = list(
      sport = "football", country = "iceland", betting = list(mode = "manual")
    ))
  )
  expect_false(called)
  expect_equal(read_placement_status(root)$status, "nothing_pending")
})

test_that("run_auto_place hands place_fn only the auto leagues", {
  root <- withr::local_tempdir()
  seed_pending_rec(root)
  got <- "unset"
  run_auto_place(
    root = root, now = as.POSIXct("2026-06-01 12:00:00", tz = "UTC"),
    sync_fn = function(...) TRUE,
    place_fn = function(leagues = NULL, ...) {
      got <<- leagues
      tibble::tibble(status = "placed")
    },
    bankroll_fn = .ap_bankroll,
    leagues_cfg = list(
      football_iceland = list(sport = "football", country = "iceland", betting = list(mode = "auto")),
      handball_iceland = list(sport = "handball", country = "iceland", betting = list(mode = "paper"))
    )
  )
  expect_identical(got, "football_iceland")
  expect_equal(read_placement_status(root)$status, "placed")
})
```

- [ ] **Step 2: Run the tests and confirm they fail**

Run `test-auto-place.R`.
Expected: FAIL, because `auto_place_leagues` doesn't exist and `run_auto_place` has no `leagues_cfg`.

- [ ] **Step 3: Implement**

In `R/auto-place.R`, add above `run_auto_place()`'s roxygen:

```r
#' League keys the unattended placer may act on.
#'
#' Only `betting.mode` "auto" (spec 2026-09-23 WS1). A "manual" league is
#' placed by a human running `scripts/place_bets.R --league <key>`; "paper"
#' and below are never placed.
#'
#' @param leagues_cfg Named league config (from [load_leagues()]).
#' @return Character vector of league keys (possibly empty).
#' @export
auto_place_leagues <- function(leagues_cfg) {
  keep <- vapply(leagues_cfg, betting_mode_at_least, logical(1), stage = "auto")
  # as.character(): names(list()) is NULL, and a NULL `leagues` means "all
  # leagues" downstream -- an empty config must place nothing, not everything.
  as.character(names(leagues_cfg)[keep])
}
```

In `run_auto_place()`:
- Add `#' @param leagues_cfg Named league config; \code{NULL} (default) reads [load_leagues()] after the sync, so a config change pulled this cycle applies this cycle.`
  to the roxygen.
- Add the parameter `leagues_cfg = NULL` after `headless = TRUE`.
- Replace

```r
  sync_ok <- isTRUE(tryCatch(sync_fn(here::here()), error = function(e) FALSE))
  pending <- tryCatch(suppressMessages(preview_pending(root = root)),
    error = function(e) tibble::tibble()
  )
```

with

```r
  sync_ok <- isTRUE(tryCatch(sync_fn(here::here()), error = function(e) FALSE))
  # Read the ladder after the sync: a betting.mode change pulled this cycle
  # applies this cycle. Only "auto" leagues are placed unattended. A config the
  # sync pulled that fails to load is recorded before re-throwing, as a
  # placement error is: scripts/auto_place.R only logs what it catches.
  if (is.null(leagues_cfg)) {
    leagues_cfg <- tryCatch(load_leagues(), error = function(e) {
      record_placement_status(
        paste0("failed:config: ", conditionMessage(e)),
        run_at = now, root = root
      )
      stop(e)
    })
  }
  auto_keys <- auto_place_leagues(leagues_cfg)
  pending <- tryCatch(
    suppressMessages(preview_pending(
      leagues = auto_keys, root = root, leagues_cfg = leagues_cfg
    )),
    error = function(e) tibble::tibble()
  )
```

and change the `place_fn(...)` call to

```r
    place_fn(
      leagues = auto_keys, dry_run = FALSE, interactive = FALSE,
      headless = headless, root = root
    ),
```

- [ ] **Step 4: Run the tests and confirm they pass**

Run `test-auto-place.R` and `test-placer-ci-isolation.R`.
Expected: all PASS. The earlier tests' `place_fn = function(...)` mocks accept `leagues=`, and the real
config has football at `auto`.

- [ ] **Step 5: Regenerate docs and commit**

```bash
Rscript -e 'devtools::document(".")'
git fetch -q origin && git branch --show-current
git add R/auto-place.R tests/testthat/test-auto-place.R NAMESPACE man/
git commit -m 'feat(autoplace): place betting.mode auto leagues only

run_auto_place() reads the ladder after its sync and hands preview and
place_fn the auto league keys, so a paper or manual league can never be
placed unattended; with none pending it records nothing_pending.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>'
```

---

### Task 5: The Lengjan JSON API client (fetch + parse + map)

**Files:**
- Create: `R/lengjan-api.R`
- Create: `tests/testthat/test-lengjan-api.R`
- Fixtures: `tests/testthat/fixtures/lengjan-api/current-program.json` and `markets.json`. They were
  already committed with this plan (captured live 2026-09-23, trimmed, UTF-8), because the events in
  them expire within days. The `git add` of that directory in Step 5 is then a no-op.

**The fixture, as pinned by the planning session:**
- **Program:** 3 `events` (handball `4616742` DHB Pokal comp `9281`; basketball `4412294` WNBA comp
  `9445`; football `4601670` CONCACAF comp `10046`), 3 Icelandic `liveSoon` events with
  `marketCount: 0` (`4602104` FH – Haukar comp `1269`; `4622914` comp `27524`; `4631057` comp
  `20442`), and `popular` repeating `4601670`.
- **Markets:** all three `events`, 50 markets, all `open`.
- **Canonical rows:** handball **9**, basketball **5**, football **25**.

**Interfaces:**
- Produces (all exported unless marked `@noRd`):
  - `lengjan_api_get(path, query = list())` returns parsed JSON lists and raises `lengjan_fetch_error` (`@noRd`)
  - `.lengjan_perform(req)` is a one-line `httr2::req_perform` wrapper, the seam tests mock (`@noRd`)
  - `lengjan_fetch_program()` returns a list
  - `lengjan_fetch_markets(event_ids, batch_size = 20L, pause_s = 1)` returns a list of groups
  - `parse_lengjan_program(prog)` returns a tibble with `event_id` (chr), `sport_id` (int),
    `competition_id` (chr), `competition_name` (chr), `country_code` (chr), `kickoff_at` (POSIXct UTC),
    `home_team`, `away_team` (chr) and `market_count` (int)
  - `parse_lengjan_markets(groups)` returns a tibble with `event_id`, `group`, `type`, `type_name`,
    `primary` (lgl), `status`, `special_value`, `selection` (chr) and `odds` (dbl, already ÷100)
  - `lengjan_markets_to_odds(markets)` returns a tibble with `event_id`, `market`, `outcome`, `line`
    and `odds`
- Consumes: `%||%` (defined in `R/ingest-lengjan-odds.R`, which is why the file uses `@include`).

- [ ] **Step 1: Write the failing tests**

Create `tests/testthat/test-lengjan-api.R`:

```r
# Lengjan JSON API client (spec 2026-09-23 WS2). Fixtures captured live
# 2026-09-23 -- see docs/superpowers/plans/2026-09-23-hb-bb-lengjan-odds-milestone-a.md
# Task 5 for their contents. No network.

.fx <- function(name) {
  jsonlite::read_json(
    testthat::test_path("fixtures", "lengjan-api", name),
    simplifyVector = FALSE
  )
}

test_that("parse_lengjan_program unions the arrays and de-duplicates events", {
  ev <- parse_lengjan_program(.fx("current-program.json"))
  # 3 in events + 3 in liveSoon; the football event repeated in popular once.
  expect_equal(nrow(ev), 6L)
  expect_equal(anyDuplicated(ev$event_id), 0L)
  fh <- ev[ev$event_id == "4602104", ]
  expect_equal(fh$sport_id, 6L)
  expect_equal(fh$competition_id, "1269")
  expect_equal(fh$country_code, "IS") # countryName fallback: no countryCode
  expect_equal(fh$home_team, "FH")
  expect_equal(fh$away_team, "Haukar")
  expect_equal(fh$market_count, 0L)
  expect_equal(fh$kickoff_at, as.POSIXct("2026-09-24 19:30:00", tz = "UTC"))
  expect_equal(ev$country_code[ev$event_id == "4616742"], "DE")
})

test_that("parse_lengjan_program fails loudly on an unexpected shape", {
  expect_error(
    parse_lengjan_program(list(data = list())),
    "unexpected current-program shape"
  )
})

test_that("parse_lengjan_markets fails loudly on a group without markets", {
  expect_error(
    parse_lengjan_markets(list(list(name = "OU_FT", rows = list()))),
    "without a `markets` array"
  )
})

test_that("parse_lengjan_markets divides the integer prices by 100", {
  mk <- parse_lengjan_markets(.fx("markets.json"))
  ml <- mk[mk$event_id == "4616742" & mk$primary, ]
  expect_equal(ml$odds[match(c("1", "X", "2"), ml$selection)], c(1.39, 8.83, 3.23))
})

test_that("lengjan_markets_to_odds keeps exactly 1X2, OU_FT totals and HC_FT spreads", {
  od <- lengjan_markets_to_odds(parse_lengjan_markets(.fx("markets.json")))
  n <- table(od$event_id)
  expect_equal(as.integer(n[["4616742"]]), 9L) # handball: 1X2 + 3 total lines
  expect_equal(as.integer(n[["4412294"]]), 5L) # basketball: regulation 1X2 + 1 total line
  expect_equal(as.integer(n[["4601670"]]), 25L) # football: 1X2 + 2 totals + 6 handicaps
  expect_setequal(unique(od$market[od$event_id == "4616742"]), c("moneyline", "total"))
  tot <- od[od$event_id == "4616742" & od$market == "total", ]
  expect_setequal(tot$line, c(56.5, 57.5, 58.5))
  expect_equal(tot$odds[tot$line == 56.5 & tot$outcome == "over"], 1.60)
  # The half-time 3WAY (type "2", not primary) must not become the moneyline.
  ml <- od[od$event_id == "4616742" & od$market == "moneyline", ]
  expect_equal(ml$odds[match(c("home", "draw", "away"), ml$outcome)], c(1.39, 8.83, 3.23))
})

test_that("HC_FT specialValue is home's signed handicap, as parse_handicap() encodes it", {
  od <- lengjan_markets_to_odds(parse_lengjan_markets(.fx("markets.json")))
  sp <- od[od$event_id == "4601670" & od$market == "spread", ]
  expect_setequal(unique(sp$line), c(1, -1, 2, -2, 3, -3))
  expect_equal(parse_handicap("1-0"), 1) # the DOM path's encoding of "Forgjof 1-0"
  expect_equal(sp$odds[sp$line == 1 & sp$outcome == "home"], 1.24)
})

test_that("closed markets and invalid prices are dropped row by row", {
  groups <- list(list(name = "single-1", markets = list(
    list(
      eventId = "1", type = "1", typeName = "3WAY", primary = TRUE, status = "open",
      selections = list(
        list(name = "1", odds = 100), # 1.00: not a valid decimal price
        list(name = "X", odds = NULL), # no price
        list(name = "2", odds = 250)
      )
    ),
    list(
      eventId = "2", type = "1", typeName = "3WAY", primary = TRUE, status = "suspended",
      selections = list(
        list(name = "1", odds = 180), list(name = "X", odds = 340), list(name = "2", odds = 400)
      )
    )
  )))
  od <- lengjan_markets_to_odds(parse_lengjan_markets(groups))
  expect_equal(nrow(od), 1L)
  expect_equal(od$outcome, "away")
  expect_equal(od$odds, 2.5)
})

test_that("lengjan_fetch_markets sends at most 20 ids per request", {
  seen <- list()
  testthat::local_mocked_bindings(lengjan_api_get = function(path, query = list()) {
    seen[[length(seen) + 1L]] <<- query
    list()
  })
  lengjan_fetch_markets(as.character(1:45), pause_s = 0)
  expect_equal(length(seen), 3L)
  n_ids <- vapply(seen, function(q) sum(startsWith(names(q), "eventIds[")), integer(1))
  expect_equal(n_ids, c(20L, 20L, 5L))
  expect_equal(names(seen[[2]])[[1]], "eventIds[0]") # indices restart per batch
  expect_true(all(vapply(seen, function(q) identical(q$live, "false"), logical(1))))
})

test_that("lengjan_fetch_markets makes no request for an empty id set", {
  testthat::local_mocked_bindings(lengjan_api_get = function(...) stop("no request expected"))
  expect_equal(lengjan_fetch_markets(character(0)), list())
})

test_that("lengjan_api_get raises lengjan_fetch_error on transport failure", {
  testthat::local_mocked_bindings(.lengjan_perform = function(req) stop("Could not resolve host"))
  expect_error(lengjan_api_get("current-program"), class = "lengjan_fetch_error")
})
```

- [ ] **Step 2: Run the tests and confirm they fail**

Run `test-lengjan-api.R`.
Expected: FAIL with `could not find function "parse_lengjan_program"` and similar.

- [ ] **Step 3: Implement `R/lengjan-api.R`**

```r
#' @include ingest-lengjan-odds.R storage.R config.R
NULL

# Lengjan's public JSON API (spec 2026-09-23 WS2, finding L1). The site's own
# Next.js front end calls these; no login, no browser. current-program lists
# every event; markets returns all markets for up to 20 events per call (L2:
# past index 20 the server's query parser turns eventIds into an object and
# answers HTTP 400).
.LENGJAN_API_BASE <- "https://games.lotto.is/api/proxy/lengjan"
.LENGJAN_MARKETS_BATCH <- 20L
.LENGJAN_UA <- "sports-pipeline (+https://github.com/metill-is/sports)"

#' One-line seam over httr2::req_perform() so tests can fail the transport.
#' @noRd
.lengjan_perform <- function(req) httr2::req_perform(req)

#' GET one Lengjan API path and return the parsed JSON (lists, not simplified).
#'
#' Any transport failure or non-2xx status (after httr2's retries) is raised
#' as `lengjan_fetch_error`, the class `ingest_one_lengjan()` already
#' soft-fails to 0 rows, so an API blip behaves like a DOM navigate timeout.
#' @noRd
lengjan_api_get <- function(path, query = list()) {
  req <- httr2::request(.LENGJAN_API_BASE) |>
    httr2::req_url_path_append(path) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_user_agent(.LENGJAN_UA) |>
    httr2::req_retry(max_tries = 3L) |>
    httr2::req_timeout(30L)
  resp <- tryCatch(.lengjan_perform(req), error = function(e) e)
  if (inherits(resp, "error")) {
    stop(structure(
      class = c("lengjan_fetch_error", "error", "condition"),
      list(
        message = paste0("Lengjan API /", path, ": ", conditionMessage(resp)),
        call = NULL
      )
    ))
  }
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

#' Fetch Lengjan's current program (every listed event).
#'
#' @return Parsed JSON: a list with `events`, `liveSoon`, `popular`, ...
#' @export
lengjan_fetch_program <- function() {
  lengjan_api_get("current-program")
}

#' Fetch all markets for Lengjan events, at most 20 per request.
#'
#' @param event_ids Character vector of Lengjan event ids.
#' @param batch_size Ids per request (at most 20: the server rejects more).
#' @param pause_s Seconds between requests (politeness; 0 in tests).
#' @return List of market groups: the requests' JSON arrays, concatenated.
#' @export
lengjan_fetch_markets <- function(event_ids, batch_size = .LENGJAN_MARKETS_BATCH,
                                  pause_s = 1) {
  ids <- unique(as.character(event_ids))
  ids <- ids[!is.na(ids) & nzchar(ids)]
  if (length(ids) == 0L) {
    return(list())
  }
  stopifnot(batch_size >= 1L, batch_size <= .LENGJAN_MARKETS_BATCH)
  chunks <- split(ids, ceiling(seq_along(ids) / batch_size))
  out <- list()
  for (k in seq_along(chunks)) {
    chunk <- chunks[[k]]
    query <- stats::setNames(
      as.list(chunk), sprintf("eventIds[%d]", seq_along(chunk) - 1L)
    )
    query$live <- "false"
    out <- c(out, lengjan_api_get("markets", query))
    if (k < length(chunks) && pause_s > 0) Sys.sleep(pause_s)
  }
  out
}

#' Flatten Lengjan's current-program JSON into one row per event.
#'
#' Unions `events`, `liveSoon`, `popular` and `liveNow` -- an Icelandic event
#' with no open market yet sits only in `liveSoon` (spec L7) -- keeping the
#' first copy of each event id. `country_code` falls back to `countryName`:
#' Icelandic events carry `countryName: "IS"` and no `countryCode` (L4).
#'
#' @param prog Parsed JSON from [lengjan_fetch_program()].
#' @return Tibble: event_id, sport_id, competition_id, competition_name,
#'   country_code, kickoff_at (POSIXct UTC), home_team, away_team, market_count.
#' @export
parse_lengjan_program <- function(prog) {
  if (!is.list(prog) || !all(c("events", "liveSoon") %in% names(prog))) {
    stop(
      "parse_lengjan_program: unexpected current-program shape ",
      "(no events/liveSoon arrays); Lengjan's API may have changed.",
      call. = FALSE
    )
  }
  evs <- c(prog$events, prog$liveSoon, prog$popular, prog$liveNow)
  if (length(evs) == 0L) {
    return(empty_lengjan_events())
  }
  out <- dplyr::bind_rows(lapply(evs, .lengjan_event_row))
  out[!duplicated(out$event_id), , drop = FALSE]
}

#' @noRd
.lengjan_event_row <- function(e) {
  parts <- e$participants %||% list()
  side <- function(k) {
    for (p in parts) {
      if (identical(as.integer(p$homeOrAway), k)) {
        return(as.character(p$name))
      }
    }
    NA_character_
  }
  cc <- e$countryCode
  if (is.null(cc) || !nzchar(cc)) cc <- e$countryName %||% NA_character_
  tibble::tibble(
    event_id = as.character(e$id),
    sport_id = as.integer(e$sportId),
    competition_id = as.character(e$compId),
    competition_name = trimws(as.character(e$compName %||% NA_character_)),
    country_code = as.character(cc),
    kickoff_at = lubridate::ymd_hms(
      e$datePlayed %||% NA_character_, tz = "UTC", quiet = TRUE
    ),
    home_team = side(1L),
    away_team = side(2L),
    market_count = as.integer(e$marketCount %||% 0L)
  )
}

#' @noRd
empty_lengjan_events <- function() {
  tibble::tibble(
    event_id = character(), sport_id = integer(), competition_id = character(),
    competition_name = character(), country_code = character(),
    kickoff_at = as.POSIXct(character(), tz = "UTC"),
    home_team = character(), away_team = character(), market_count = integer()
  )
}

#' Flatten Lengjan's markets JSON into one row per selection.
#'
#' @param groups Parsed JSON from [lengjan_fetch_markets()]: market groups,
#'   each with `name` ("OU_FT", "HC_FT", "single-<id>", ...) and `markets`.
#' @return Tibble: event_id, group, type, type_name, primary, status,
#'   special_value, selection, odds (decimal: the API's integer / 100, L3).
#' @export
parse_lengjan_markets <- function(groups) {
  rows <- list()
  for (g in groups) {
    if (!is.list(g) || is.null(g$markets)) {
      stop(
        "parse_lengjan_markets: market group without a `markets` array; ",
        "Lengjan's API may have changed.",
        call. = FALSE
      )
    }
    for (m in g$markets) {
      sels <- m$selections %||% list()
      if (length(sels) == 0L) next
      rows[[length(rows) + 1L]] <- tibble::tibble(
        event_id = as.character(m$eventId),
        group = as.character(g$name %||% NA_character_),
        type = as.character(m$type %||% NA_character_),
        type_name = as.character(m$typeName %||% NA_character_),
        primary = isTRUE(m$primary),
        status = as.character(m$status %||% NA_character_),
        special_value = as.character(m$specialValue %||% NA_character_),
        selection = vapply(sels, function(s) {
          as.character(s$name %||% NA_character_)
        }, character(1)),
        odds = vapply(sels, function(s) {
          as.numeric(s$odds %||% NA_real_)
        }, numeric(1)) / 100
      )
    }
  }
  if (length(rows) == 0L) {
    return(empty_lengjan_markets())
  }
  dplyr::bind_rows(rows)
}

#' @noRd
empty_lengjan_markets <- function() {
  tibble::tibble(
    event_id = character(), group = character(), type = character(),
    type_name = character(), primary = logical(), status = character(),
    special_value = character(), selection = character(), odds = numeric()
  )
}

#' Map parsed Lengjan markets onto the canonical odds vocabulary.
#'
#' The event's primary 3WAY (full-time 1X2) -> moneyline home/draw/away;
#' group OU_FT -> total over/under at `special_value`; group HC_FT (3-way
#' handicap) -> spread home/draw/away at `special_value`, which is home's
#' signed handicap ("Forgjof 1-0" -> 1, as [parse_handicap()] encodes it).
#' Everything else -- half-time, double chance, Asian handicap, BTTS -- is
#' dropped, as is any selection that is not open or whose price is not a
#' valid decimal (> 1): one suspended market must not abort the whole write
#' through validate_values().
#'
#' @param markets Output of [parse_lengjan_markets()].
#' @return Tibble: event_id, market, outcome, line, odds.
#' @export
lengjan_markets_to_odds <- function(markets) {
  m <- markets[markets$status %in% "open" & is.finite(markets$odds) &
    markets$odds > 1, , drop = FALSE]
  three_way <- c("1" = "home", "X" = "draw", "2" = "away")
  over_under <- c(Yfir = "over", Undir = "under")

  ml <- m[m$primary & m$type_name %in% "3WAY" &
    m$selection %in% names(three_way), , drop = FALSE]
  ml$market <- rep("moneyline", nrow(ml))
  ml$outcome <- unname(three_way[ml$selection])
  ml$line <- rep(NA_real_, nrow(ml))

  tot <- m[m$group %in% "OU_FT" & m$type_name %in% "OU" &
    m$selection %in% names(over_under), , drop = FALSE]
  tot$market <- rep("total", nrow(tot))
  tot$outcome <- unname(over_under[tot$selection])
  tot$line <- suppressWarnings(as.numeric(tot$special_value))

  hc <- m[m$group %in% "HC_FT" & m$type_name %in% "HC" &
    m$selection %in% names(three_way), , drop = FALSE]
  hc$market <- rep("spread", nrow(hc))
  hc$outcome <- unname(three_way[hc$selection])
  hc$line <- suppressWarnings(as.numeric(hc$special_value))

  out <- dplyr::bind_rows(ml, tot, hc)
  out <- out[out$market == "moneyline" | is.finite(out$line), , drop = FALSE]
  out[, c("event_id", "market", "outcome", "line", "odds"), drop = FALSE]
}
```

- [ ] **Step 4: Regenerate docs, then run the tests and confirm they pass**

Run `Rscript -e 'devtools::document(".")'`. `DESCRIPTION` Collate gains `'lengjan-api.R'` after
`'ingest-lengjan-odds.R'`, and `NAMESPACE` gains the five exports. Then run `test-lengjan-api.R`.
Expected: all PASS.

- [ ] **Step 5: Commit**

```bash
git fetch -q origin && git branch --show-current
git add R/lengjan-api.R tests/testthat/test-lengjan-api.R tests/testthat/fixtures/lengjan-api/ NAMESPACE DESCRIPTION man/
git commit -m 'feat(odds): Lengjan JSON API client (program, batched markets, mapping)

current-program lists every event (Icelandic ones sit in liveSoon with
countryName IS and no countryCode); markets takes at most 20 event ids per
call. Prices are integer x100. The primary 3WAY maps to moneyline, OU_FT to
total and HC_FT to spread, with specialValue as the line. Closed markets
and invalid prices drop row by row, and a changed JSON shape fails loudly.
Fixtures were captured live on 2026-09-23.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>'
```

---

### Task 6: API odds ingest and per-league `lengjan.source` dispatch

**Files:**
- Modify: `R/lengjan-api.R` (add `lengjan_api_odds_rows()`, `empty_lengjan_api_odds()` and `ingest_lengjan_api()`)
- Modify: `R/ingest.R` (the `ingest_one_lengjan()` dispatch)
- Create: `tests/testthat/test-lengjan-api-ingest.R`

**Interfaces:**
- Consumes: from Task 5, `parse_lengjan_program()`, `parse_lengjan_markets()`,
  `lengjan_markets_to_odds()`, `lengjan_fetch_program()` and `lengjan_fetch_markets()`. Also the
  existing `.lengjan_sport_id(sport)` (`R/ingest-lengjan-odds.R`: football 1, basketball 2,
  handball 6) and `upsert_table()` (Task 1).
- Produces:
  - `lengjan_api_odds_rows(events, markets, league, scraped_at)` returns a tibble matching
    `schemas()$odds`.
  - `ingest_lengjan_api(leagues, scraped_at = Sys.time(), root = here::here("data"), fetch_program = lengjan_fetch_program, fetch_markets = lengjan_fetch_markets)`
    returns the number of rows written (invisible).
  - `ingest_one_lengjan()` calls `ingest_lengjan_api` when `lengjan$source == "api"`, and otherwise
    `ingest_lengjan_odds`.

- [ ] **Step 1: Write the failing tests**

Create `tests/testthat/test-lengjan-api-ingest.R`:

```r
# API odds ingest (spec 2026-09-23 WS2). Fixture events: handball 4616742
# (comp 9281, 9 rows), basketball 4412294 (comp 9445, 5 rows), football
# 4601670 (comp 10046, 25 rows); FH - Haukar 4602104 (comp 1269) has no market.

.fx <- function(name) {
  jsonlite::read_json(
    testthat::test_path("fixtures", "lengjan-api", name),
    simplifyVector = FALSE
  )
}

.api_league <- function(comp_id, sport, sex = "male") {
  list(
    sport = sport, country = "iceland",
    lengjan = list(
      source = "api",
      competitions = list(list(id = comp_id, name = "x", sex = sex))
    )
  )
}

.t0 <- as.POSIXct("2026-09-23 17:17:00", tz = "UTC")

test_that("lengjan_api_odds_rows selects by competition and stamps sex + ids", {
  ev <- parse_lengjan_program(.fx("current-program.json"))
  mk <- parse_lengjan_markets(.fx("markets.json"))
  rows <- lengjan_api_odds_rows(ev, mk, .api_league("9281", "handball"), .t0)
  expect_equal(nrow(rows), 9L)
  expect_true(all(rows$sex == "male"))
  expect_true(all(rows$event_id == "4616742"))
  expect_true(all(rows$competition_id == "9281"))
  expect_equal(unique(rows$home_team), "Stuttgart")
  expect_equal(unique(rows$match_date), as.Date("2026-09-24"))
  expect_equal(unique(rows$kickoff_at), as.POSIXct("2026-09-24 17:00:00", tz = "UTC"))
  expect_true(all(rows$sport == "handball" & rows$country == "iceland"))
  # A competition id asked for under the wrong sport never matches.
  expect_equal(nrow(lengjan_api_odds_rows(ev, mk, .api_league("9281", "basketball"), .t0)), 0L)
})

test_that("ingest_lengjan_api writes schema-valid rows and asks only for open events", {
  root <- withr::local_tempdir()
  asked <- NULL
  n <- suppressMessages(ingest_lengjan_api(
    list(
      handball_x = .api_league("9281", "handball"),
      basketball_x = .api_league("9445", "basketball", sex = "female"),
      handball_is = .api_league("1269", "handball")
    ),
    scraped_at = .t0, root = root,
    fetch_program = function() .fx("current-program.json"),
    fetch_markets = function(ids) {
      asked <<- ids
      .fx("markets.json")
    }
  ))
  expect_equal(n, 14L) # 9 handball + 5 basketball; FH - Haukar has no market yet
  expect_setequal(asked, c("4616742", "4412294")) # 4602104 (marketCount 0) not requested
  back <- read_table("odds", root = root)
  expect_equal(nrow(back), 14L)
  expect_setequal(unique(back$sex), c("male", "female"))
  expect_setequal(unique(back$sport), c("handball", "basketball"))
})

test_that("ingest_lengjan_api makes no markets request when nothing is open", {
  called <- FALSE
  n <- suppressMessages(ingest_lengjan_api(
    list(handball_is = .api_league("1269", "handball")),
    root = withr::local_tempdir(),
    fetch_program = function() .fx("current-program.json"),
    fetch_markets = function(ids) {
      called <<- TRUE
      list()
    }
  ))
  expect_equal(n, 0L)
  expect_false(called)
})

test_that("ingest_one_lengjan dispatches on lengjan.source", {
  testthat::local_mocked_bindings(
    .is_league_active = function(active_path, key) TRUE,
    ingest_lengjan_api = function(...) 11L,
    ingest_lengjan_odds = function(...) 22L
  )
  one <- function(src) {
    lj <- list(competitions = list(list(id = "1269", name = "x", sex = "male")))
    if (!is.null(src)) lj$source <- src
    suppressMessages(ingest_one_lengjan(
      list(sport = "handball", country = "iceland"), lj,
      "handball_iceland", "active.json"
    ))
  }
  expect_identical(one("api"), 11L)
  expect_identical(one("dom"), 22L)
  expect_identical(one(NULL), 22L) # absent source keeps the DOM scraper
})

test_that("ingest_one_lengjan soft-fails an API fetch error to 0 rows", {
  testthat::local_mocked_bindings(
    .is_league_active = function(active_path, key) TRUE,
    ingest_lengjan_api = function(...) {
      stop(structure(
        class = c("lengjan_fetch_error", "error", "condition"),
        list(message = "Lengjan API /current-program: HTTP 503")
      ))
    }
  )
  res <- suppressMessages(ingest_one_lengjan(
    list(sport = "handball", country = "iceland"),
    list(source = "api", competitions = list(list(id = "1269", name = "x", sex = "male"))),
    "handball_iceland", "active.json"
  ))
  expect_identical(res, 0L)
})
```

- [ ] **Step 2: Run the tests and confirm they fail**

Run `test-lengjan-api-ingest.R`.
Expected: FAIL, because `lengjan_api_odds_rows` doesn't exist and the dispatch returns 22 for `"api"`.

- [ ] **Step 3: Implement the rows builder and the orchestrator**

Append to `R/lengjan-api.R`:

```r
#' Canonical odds rows for one league from a parsed program and markets.
#'
#' An event belongs to the league when its sport matches and its competition
#' id is one of the league's `lengjan.competitions`; the row takes that
#' competition's `sex`. Team names stay as Lengjan renders them -- decide maps
#' them to canonical per sex ([normalise_lengjan_team_names()]).
#' `match_date` is the kickoff's UTC date: Iceland keeps UTC all year.
#'
#' @param events Output of [parse_lengjan_program()].
#' @param markets Output of [parse_lengjan_markets()].
#' @param league A league definition carrying `sport`, `country`, `lengjan`.
#' @param scraped_at Single timestamp for the run.
#' @return Tibble matching `schemas()$odds`.
#' @export
lengjan_api_odds_rows <- function(events, markets, league, scraped_at) {
  comps <- league$lengjan$competitions %||% list()
  comp_ids <- vapply(comps, function(cp) as.character(cp$id), character(1))
  comp_sex <- stats::setNames(
    vapply(comps, function(cp) as.character(cp$sex), character(1)),
    comp_ids
  )
  ev <- events[events$sport_id == .lengjan_sport_id(league$sport) &
    events$competition_id %in% comp_ids, , drop = FALSE]
  j <- dplyr::inner_join(lengjan_markets_to_odds(markets), ev, by = "event_id")
  j <- j[!is.na(j$home_team) & !is.na(j$away_team), , drop = FALSE]
  if (nrow(j) == 0L) {
    return(empty_lengjan_api_odds())
  }
  tibble::tibble(
    sport = league$sport, country = league$country,
    scraped_at = rep(scraped_at, nrow(j)),
    match_date = as.Date(j$kickoff_at, tz = "UTC"),
    home_team = j$home_team, away_team = j$away_team,
    market = j$market, outcome = j$outcome, line = j$line, odds = j$odds,
    sex = unname(comp_sex[j$competition_id]),
    event_id = j$event_id, competition_id = j$competition_id,
    kickoff_at = j$kickoff_at
  )
}

#' @noRd
empty_lengjan_api_odds <- function() {
  tibble::tibble(
    sport = character(), country = character(),
    scraped_at = as.POSIXct(character(), tz = "UTC"),
    match_date = as.Date(character()),
    home_team = character(), away_team = character(),
    market = character(), outcome = character(),
    line = numeric(), odds = numeric(),
    sex = character(), event_id = character(), competition_id = character(),
    kickoff_at = as.POSIXct(character(), tz = "UTC")
  )
}

#' Scrape odds from Lengjan's JSON API for leagues and upsert them.
#'
#' One program request, then markets for the leagues' open events in batches
#' of 20 -- typically 2-4 requests in all. The API counterpart of
#' [ingest_lengjan_odds()] (the Chromote DOM scraper), chosen per league by
#' `lengjan.source: api` in [ingest_one_lengjan()].
#'
#' @param leagues Named list of league definitions (each with `lengjan`).
#' @param scraped_at Single timestamp for the whole run.
#' @param root Storage root.
#' @param fetch_program,fetch_markets Injectable fetchers; tests pass fixtures.
#' @return Number of odds rows written (invisible integer).
#' @export
ingest_lengjan_api <- function(leagues, scraped_at = Sys.time(),
                               root = here::here("data"),
                               fetch_program = lengjan_fetch_program,
                               fetch_markets = lengjan_fetch_markets) {
  stopifnot(is.list(leagues), length(leagues) > 0L)
  events <- parse_lengjan_program(fetch_program())

  wanted <- character(0)
  for (lg in leagues) {
    ids <- vapply(lg$lengjan$competitions %||% list(), function(cp) {
      as.character(cp$id)
    }, character(1))
    hit <- events$sport_id == .lengjan_sport_id(lg$sport) &
      events$competition_id %in% ids & events$market_count > 0L
    wanted <- c(wanted, events$event_id[hit])
  }
  wanted <- unique(wanted)
  if (length(wanted) == 0L) {
    cli::cli_alert_info("Lengjan API: no open markets for {.val {names(leagues)}}")
    return(invisible(0L))
  }

  markets <- parse_lengjan_markets(fetch_markets(wanted))
  rows <- dplyr::bind_rows(lapply(leagues, lengjan_api_odds_rows,
    events = events, markets = markets, scraped_at = scraped_at
  ))
  if (nrow(rows) == 0L) {
    cli::cli_alert_info("Lengjan API: {length(wanted)} event{?s} but no mappable odds")
    return(invisible(0L))
  }
  upsert_table(rows, "odds", root = root)
  cli::cli_alert_success(
    "Lengjan API: wrote {nrow(rows)} odds rows for {length(wanted)} event{?s}"
  )
  invisible(nrow(rows))
}
```

- [ ] **Step 4: Dispatch on `lengjan.source` in `ingest_one_lengjan()`**

In `R/ingest.R` `ingest_one_lengjan()`, replace

```r
  league <- static
  league$lengjan <- lengjan
  tryCatch(
    as.integer(ingest_lengjan_odds(stats::setNames(list(league), key))),
```

with

```r
  league <- static
  league$lengjan <- lengjan
  # lengjan.source picks the scraper (spec 2026-09-23 WS2/WS3): "api" is the
  # JSON API, anything else (absent = football until its Milestone B cutover)
  # the Chromote DOM scraper. Both raise lengjan_fetch_error on transport
  # failure, soft-failed below.
  scrape_fn <- if (identical(lengjan$source, "api")) {
    ingest_lengjan_api
  } else {
    ingest_lengjan_odds
  }
  tryCatch(
    as.integer(scrape_fn(stats::setNames(list(league), key))),
```

In the soft-fail handler's warning text, replace `Lengjan fetch timed out after retries` with
`Lengjan fetch failed after retries`, since it now covers API HTTP errors too. Update the
`@param lengjan` roxygen to `Per-league \code{lengjan} slice (source, competitions, team_names).`

- [ ] **Step 5: Regenerate docs, then run the tests and confirm they pass**

Run `Rscript -e 'devtools::document(".")'`, then `test-lengjan-api-ingest.R`,
`test-ingest-lengjan-odds.R` and `test-betting-ladder.R`.
Expected: all PASS.

- [ ] **Step 6: Commit**

```bash
git fetch -q origin && git branch --show-current
git add R/lengjan-api.R R/ingest.R tests/testthat/test-lengjan-api-ingest.R NAMESPACE man/
git commit -m 'feat(odds): ingest Lengjan odds from the JSON API per lengjan.source

ingest_lengjan_api() makes one program request plus batched markets for
the leagues'"'"' open events only, stamps sex from the competition, and
upserts. ingest_one_lengjan() picks the API when lengjan.source is api and
otherwise keeps the DOM scraper; both soft-fail a fetch error to 0 rows.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>'
```

---

### Task 7: Decide sees only its own sex's odds

**Files:**
- Modify: `R/decide-odds.R` (`prepare_odds()` and the roxygen of its `sex` param)
- Modify: `tests/testthat/test-decide-odds.R` (append a test)

**Interfaces:**
- Consumes: the `odds.sex` column (Task 1).
- Produces: `prepare_odds(league, sex, ...)` drops rows whose non-NA `sex` differs from `sex`. NA rows
  pass through, which covers history and the DOM scraper.

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-decide-odds.R`:

```r
test_that("prepare_odds keeps only the requested sex when odds carry one", {
  # Spec 2026-09-23 Review Focus 4: the API stamps each row's sex from its
  # competition; a men's and a women's fixture between the same clubs on the
  # same day must never price each other. NA rows (DOM scraper, history) stay
  # sex-agnostic.
  root <- withr::local_tempdir()
  now <- as.POSIXct("2100-01-01 12:00:00", tz = "UTC")
  row <- function(sex, home, away, odds) {
    tibble::tibble(
      sport = "handball", country = "iceland", scraped_at = now - 3600,
      match_date = as.Date("2100-01-02"), home_team = home, away_team = away,
      market = "moneyline", outcome = "home", line = NA_real_, odds = odds,
      sex = sex
    )
  }
  write_table(dplyr::bind_rows(
    row("male", "Valur", "Haukar", 1.80),
    row("female", "Valur", "Haukar", 2.60),
    row(NA_character_, "FH", "HK", 3.10)
  ), "odds", root = root)
  tn <- list(Valur = "Valur", Haukar = "Haukar", FH = "FH", HK = "HK")
  league <- list(
    sport = "handball", country = "iceland",
    lengjan = list(team_names = list(male = tn, female = tn))
  )
  get <- function(sex) {
    prepare_odds(league, sex,
      end_date = as.Date("2100-01-01"), max_age_hours = 48,
      now = now, root = root
    )
  }
  m <- get("male")
  f <- get("female")
  expect_equal(m$odds[m$home_team == "Valur"], 1.80)
  expect_equal(f$odds[f$home_team == "Valur"], 2.60)
  expect_equal(m$odds[m$home_team == "FH"], 3.10) # NA sex passes through
  expect_equal(f$odds[f$home_team == "FH"], 3.10)
  expect_equal(nrow(m), 2L)
  expect_equal(nrow(f), 2L)
})
```

- [ ] **Step 2: Run the test and confirm it fails**

Run `test-decide-odds.R`.
Expected: FAIL. Without a filter the male and female Valur rows collide in the dedup group, so
`m$odds[m$home_team == "Valur"]` is whichever row survives, not reliably 1.80, and `nrow(m)` is 2 only
by accident. The female assertion fails.

- [ ] **Step 3: Implement**

In `R/decide-odds.R`, change the `@param sex` roxygen to:

```r
#' @param sex "male" or "female". Rows carrying a non-NA `sex` (stamped by the
#'   JSON-API scraper from the Lengjan competition, spec 2026-09-23 WS2) are
#'   kept only for that sex; NA rows (DOM scraper, history) are sex-agnostic
#'   and kept for both, as before -- the per-sex team_names join then decides.
```

and insert, directly after the `if (nrow(raw) == 0L) { return(empty_odds()) }` block that follows the
age filter:

```r
  if ("sex" %in% names(raw)) {
    raw <- raw[is.na(raw$sex) | raw$sex == sex, , drop = FALSE]
    if (nrow(raw) == 0L) {
      return(empty_odds())
    }
  }
```

- [ ] **Step 4: Run the tests and confirm they pass**

Run `test-decide-odds.R`, `test-decide-pipeline.R` and `test-decide-normalise.R`.
Expected: all PASS.

- [ ] **Step 5: Commit**

```bash
Rscript -e 'devtools::document(".")'
git fetch -q origin && git branch --show-current
git add R/decide-odds.R tests/testthat/test-decide-odds.R man/prepare_odds.Rd
git commit -m 'feat(decide): price each sex only against its own odds rows

prepare_odds() keeps rows whose sex matches the cell or is NA, so a men'"'"'s
and a women'"'"'s fixture between the same clubs on one day never price each
other. DOM-scraped and historical rows have NA sex and pass through as
before.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>'
```

---

### Task 8: Health checks on the ladder (`odds_freshness`, `capture_rate`)

**Files:**
- Modify: `R/health.R` (`check_odds_freshness()`, a new `.configured_divisions()`, `check_capture_rate()`,
  and the `pipeline_health()` call site)
- Create: `tests/testthat/test-health-ladder.R`

**Interfaces:**
- Consumes: `betting_mode()` and `betting_mode_at_least()` (Task 2), and `lengjan.competitions[].division` (schema, Task 2).
- Produces:
  - `check_capture_rate(root, now, th, leagues = NULL)`: when `leagues` is given, it counts only
    recommendations from leagues at `manual` or above.
  - `odds_freshness` behaves as follows:
    - `off` is PAUSED with the text "betting disabled (betting.mode: off)".
    - Below `manual` with no competitions it is PAUSED with "no Lengjan competitions wired".
    - Below `manual`, a FAIL is capped to WARN.
    - Configured `(sex, division)` cells scope the expectations.

- [ ] **Step 1: Write the failing tests**

Create `tests/testthat/test-health-ladder.R`:

```r
# Health on the betting ladder (spec 2026-09-23 WS7).

.hl_sched <- function(match_date, sex = "male", division = "OD",
                      home = "Valur", away = "FH") {
  tibble::tibble(
    sport = "handball", country = "iceland", sex = sex, season = 2027L,
    match_date = as.Date(match_date), home_team = home, away_team = away,
    division = division, round = 1L, kickoff_time = NA_character_
  )
}

.hl_league <- function(mode,
                       comps = list(list(id = "1269", name = "x", sex = "male", division = "OD"))) {
  list(handball_iceland = list(
    sport = "handball", country = "iceland", sexes = list("male", "female"),
    lengjan = list(competitions = comps), betting = list(mode = mode)
  ))
}

.hl_now <- as.POSIXct("2026-10-01 12:00", tz = "UTC")

test_that("odds_freshness caps a stall at WARN below betting.mode manual", {
  root <- withr::local_tempdir()
  write_table(.hl_sched("2026-10-01"), "schedules", root = root) # fixture today, no odds
  paper <- check_odds_freshness(.hl_league("paper"), root, .hl_now, health_thresholds())
  expect_equal(paper$status, "WARN")
  expect_match(paper$value, "capped at WARN")
  # Positive control: identical data with money at stake still FAILs.
  manual <- check_odds_freshness(.hl_league("manual"), root, .hl_now, health_thresholds())
  expect_equal(manual$status, "FAIL")
})

test_that("odds_freshness expects odds only in configured (sex, division) cells", {
  root <- withr::local_tempdir()
  # Today's only fixtures are women's OD and men's G66: no configured competition.
  write_table(dplyr::bind_rows(
    .hl_sched("2026-10-01", sex = "female"),
    .hl_sched("2026-10-01", division = "G66", home = "Hordur", away = "Fjolnir")
  ), "schedules", root = root)
  res <- check_odds_freshness(.hl_league("manual"), root, .hl_now, health_thresholds())
  expect_equal(res$status, "OK")
  expect_match(res$value, "no configured Lengjan competition")
})

test_that("odds_freshness is PAUSED for a league below manual with no competitions", {
  root <- withr::local_tempdir()
  write_table(.hl_sched("2026-10-01"), "schedules", root = root)
  res <- check_odds_freshness(.hl_league("scrape", comps = list()), root, .hl_now, health_thresholds())
  expect_equal(res$status, "PAUSED")
  expect_match(res$value, "no Lengjan competitions wired")
})

test_that("odds_freshness keeps the 'betting disabled' wording at mode off", {
  root <- withr::local_tempdir()
  write_table(.hl_sched("2026-10-01"), "schedules", root = root)
  res <- check_odds_freshness(.hl_league("off"), root, .hl_now, health_thresholds())
  expect_equal(res$status, "PAUSED")
  expect_match(res$value, "betting disabled")
})

test_that("capture_rate ignores recommendations from leagues below manual", {
  root <- withr::local_tempdir()
  write_table(tibble::tibble(
    run_id = as.POSIXct("2026-05-20", tz = "UTC"),
    sport = "handball", country = "iceland", sex = "male",
    match_date = as.Date("2026-05-22"),
    home_team = paste0("H", 1:25), away_team = paste0("A", 1:25),
    market = "moneyline", outcome = "home", line = NA_real_,
    p = 0.55, odds = 2.0, ev = 0.1, kelly = 0.02, bet_amount = 250
  ), "recommendations", root = root)
  at <- as.POSIXct("2026-05-30", tz = "UTC")
  paper <- check_capture_rate(root, at, health_thresholds(), leagues = .hl_league("paper"))
  expect_equal(paper$status, "OK")
  expect_match(paper$value, "no settled-window recs")
  # Positive control: the same 25 unplaced recs FAIL once money is at stake.
  manual <- check_capture_rate(root, at, health_thresholds(), leagues = .hl_league("manual"))
  expect_equal(manual$status, "FAIL")
})
```

- [ ] **Step 2: Run the tests and confirm they fail**

Run `test-health-ladder.R`.
Expected: FAIL, because there's no `leagues` argument, no cap and no configured-division scoping.

- [ ] **Step 3: Implement**

In `R/health.R`, add directly above `check_odds_freshness()`'s roxygen:

```r
#' (sex, division) cells a league's configured Lengjan competitions cover, or
#' `NULL` when any competition lacks a `division` (candidate history then
#' decides, as before). Spec 2026-09-23 WS7.
#' @noRd
.configured_divisions <- function(lg) {
  comps <- lg$lengjan$competitions %||% list()
  if (length(comps) == 0L) {
    return(NULL)
  }
  div <- vapply(comps, function(cp) {
    as.character(cp$division %||% NA_character_)
  }, character(1))
  if (anyNA(div)) {
    return(NULL)
  }
  unique(tibble::tibble(
    sex = vapply(comps, function(cp) as.character(cp$sex), character(1)),
    division = div
  ))
}
```

In `check_odds_freshness()`, replace the D2 block (the comment and `if (!betting_enabled(lg)) {...}`)
with:

```r
    # Betting ladder (spec 2026-09-23 WS1/WS7). "off" is never scraped, so
    # absent odds are correct: PAUSED, which overall_health_status() does not
    # escalate. Below "manual" a league with no competitions wired (basketball
    # before its comps appear) cannot have odds either.
    mode <- betting_mode(lg)
    if (!betting_mode_at_least(lg, "scrape")) {
      rows[[key]] <- health_row(
        "odds_freshness", key, "PAUSED",
        "betting disabled (betting.mode: off)", thr_lbl
      )
      next
    }
    if (!betting_mode_at_least(lg, "manual") &&
      length(lg$lengjan$competitions %||% list()) == 0L) {
      rows[[key]] <- health_row(
        "odds_freshness", key, "PAUSED",
        sprintf("no Lengjan competitions wired (betting.mode: %s)", mode), thr_lbl
      )
      next
    }
```

Replace

```r
    covered <- .covered_divisions(static, sch, root)
```

with

```r
    configured <- .configured_divisions(lg)
    covered <- if (is.null(configured)) .covered_divisions(static, sch, root) else configured
```

and in the `if (nrow(expected) == 0L)` block, replace the `"upcoming fixtures only in divisions Lengjan has never priced"`
string with:

```r
        if (is.null(configured)) {
          "upcoming fixtures only in divisions Lengjan has never priced"
        } else {
          "upcoming fixtures only in cells with no configured Lengjan competition"
        },
```

Directly before `rows[[key]] <- health_row("odds_freshness", key, status, value, thr_lbl)`, add:

```r
    # Below "manual" no money rides on these odds: a stall is worth a WARN,
    # never the FAIL that fires the alert email (spec 2026-09-23 WS7).
    if (status == "FAIL" && !betting_mode_at_least(lg, "manual")) {
      status <- "WARN"
      value <- sprintf("%s (betting.mode: %s, capped at WARN)", value, mode)
    }
```

In `check_capture_rate()`:
- Add `leagues = NULL` as the last parameter.
- Add the roxygen line `#' @param leagues Named league config; when given, only recommendations from leagues at betting.mode "manual" or above count (paper recommendations are never placed, spec 2026-09-23 WS7). NULL or an empty list counts all.`
  (if the function has no roxygen `@param` block, add it as a `#'` comment above the function).
- Insert directly after the early return that follows the `read_table("recommendations")` call:

```r
  if (!is.null(leagues) && length(leagues) > 0L) {
    # pipeline_health() passes list() when load_leagues() failed: judge every
    # rec then rather than none (the config-error row already FAILs).
    placeable <- names(leagues)[
      vapply(leagues, betting_mode_at_least, logical(1), stage = "manual")
    ]
    recs <- recs[paste0(recs$sport, "_", recs$country) %in% placeable, , drop = FALSE]
  }
```

In `pipeline_health()`, change `safe(check_capture_rate(root, now, th)),` to
`safe(check_capture_rate(root, now, th, leagues)),`.

- [ ] **Step 4: Run the tests and confirm they pass**

Run `test-health-ladder.R`, `test-health.R`, `test-betting-interlock.R`, `test-discover-health.R` and
`test-healthcheck-ci-isolation.R`.
Expected: all PASS. The old tests use leagues with no `lengjan` block and no `betting`, i.e. mode
`auto` with no configured divisions, which is unchanged behaviour. The interlock's PAUSED test still
matches `"betting disabled"`.

- [ ] **Step 5: Commit**

```bash
git fetch -q origin && git branch --show-current
git add R/health.R tests/testthat/test-health-ladder.R
git commit -m 'feat(health): odds_freshness and capture_rate on the betting ladder

odds_freshness is PAUSED at off and for a league below manual with no
competitions wired, caps a stall at WARN below manual, and expects odds
only in the (sex, division) cells of configured competitions.
capture_rate counts manual and auto leagues only, so paper
recommendations never read as missed placements.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>'
```

---

### Task 9: Discovery via the API (no browser)

**Files:**
- Modify: `R/discover-lengjan.R`:
  - remove `parse_competition_dropdown()`;
  - rewrite `lengjan_list_competitions()` and `propose_team_names()` on top of the program;
  - make `classify_competition()` sport-aware;
  - `discover_new_competitions()` gets an `events` argument and a ladder filter.
- Modify: `scripts/0N_discover.R` (drop Chromote)
- Modify: `.github/workflows/discover-leagues.yml` (drop Chrome)
- Modify: `tests/testthat/test-discover-lengjan.R` (remove the 3 dropdown tests; add new ones)
- Delete: `tests/testthat/fixtures/lengjan-parent-page.html` (used only by the dropdown tests)
- Modify: `.claude/rules/ci-conventions.md:102` (the workflow inventory row)

**Interfaces:**
- Consumes: `parse_lengjan_program()` and `lengjan_fetch_program()` (Task 5), and `betting_mode_at_least()` (Task 2).
- Produces:
  - `lengjan_list_competitions(sport, country, events)` returns a tibble with `sport`, `country`,
    `comp_id` and `lengjan_name`.
  - `propose_team_names(comp_id, events, known_teams)` returns a tibble with `lengjan`,
    `canonical_guess` and `confidence`.
  - `discover_new_competitions(leagues, events = NULL, root = here::here("data"), list_fn = NULL, team_names_fn = NULL)`.
  - `classify_competition(lengjan_name, sport, country)` maps handball names to OD/G66 and basketball
    names to BD/1D.

- [ ] **Step 1: Write the failing tests**

In `tests/testthat/test-discover-lengjan.R`, delete the three `parse_competition_dropdown` tests at the
top of the file (from the first `test_that("parse_competition_dropdown extracts` through the end of
`test_that("parse_competition_dropdown finds the league select when placeholder is not first"`). Then
append:

```r
.fx <- function(name) {
  jsonlite::read_json(
    testthat::test_path("fixtures", "lengjan-api", name),
    simplifyVector = FALSE
  )
}

test_that("classify_competition knows handball and basketball divisions", {
  expect_equal(
    classify_competition("Olísdeild karla", "handball", "iceland")[c("sex", "division")],
    tibble::tibble(sex = "male", division = "OD")
  )
  expect_equal(classify_competition("Olísdeild kvenna", "handball", "iceland")$sex, "female")
  expect_equal(classify_competition("Grill 66 deild karla", "handball", "iceland")$division, "G66")
  expect_equal(classify_competition("Bónusdeild karla", "basketball", "iceland")$division, "BD")
  expect_equal(
    classify_competition("1. deild kvenna", "basketball", "iceland")[c("sex", "division")],
    tibble::tibble(sex = "female", division = "1D")
  )
  expect_equal(classify_competition("Powerade bikarinn", "handball", "iceland")$division, "CUP")
  # Football keeps its own vocabulary: "1. deild" is not a football code.
  expect_true(is.na(classify_competition("1. deild karla", "football", "iceland")$division))
})

test_that("lengjan_list_competitions lists Icelandic competitions per sport", {
  ev <- parse_lengjan_program(.fx("current-program.json"))
  hb <- lengjan_list_competitions("handball", "iceland", ev)
  expect_equal(hb$comp_id, "1269")
  expect_equal(hb$lengjan_name, "Olísdeild karla")
  fb <- lengjan_list_competitions("football", "iceland", ev)
  expect_setequal(fb$comp_id, c("27524", "20442"))
  # The fixture's only basketball event is the WNBA (country US).
  expect_equal(nrow(lengjan_list_competitions("basketball", "iceland", ev)), 0L)
})

test_that("propose_team_names drafts from the program's participants", {
  ev <- parse_lengjan_program(.fx("current-program.json"))
  tn <- propose_team_names("1269", ev, known_teams = c("FH", "Haukar", "Valur"))
  expect_setequal(tn$lengjan, c("FH", "Haukar"))
  expect_true(all(tn$confidence == "high"))
})

test_that("discover_new_competitions visits a scrape-mode league with no competitions", {
  ev <- parse_lengjan_program(.fx("current-program.json"))
  leagues <- list(handball_iceland = list(
    sport = "handball", country = "iceland", active = TRUE,
    lengjan = list(competitions = list()),
    betting = list(mode = "scrape"),
    publish_divisions = list(male = list(list(code = "OD"), list(code = "G66")))
  ))
  res <- discover_new_competitions(leagues, events = ev, root = withr::local_tempdir())
  expect_length(res$competitions, 1L)
  f <- res$competitions[[1L]]
  expect_equal(f$comp_id, "1269")
  expect_equal(f$inferred_division, "OD")
  expect_equal(f$inferred_sex, "male")
})

test_that("discover_new_competitions skips a league at betting.mode off", {
  ev <- parse_lengjan_program(.fx("current-program.json"))
  leagues <- list(handball_iceland = list(
    sport = "handball", country = "iceland", active = TRUE,
    lengjan = list(competitions = list()), betting = list(mode = "off")
  ))
  res <- discover_new_competitions(leagues, events = ev, root = withr::local_tempdir())
  expect_length(res$competitions, 0L)
})
```

- [ ] **Step 2: Run the tests and confirm they fail**

Run `test-discover-lengjan.R`.
Expected: FAIL. `classify_competition` returns NA for Olís/Grill/1. deild, and the list/propose
functions still take a `session`.

- [ ] **Step 3: Rewrite the discovery engine**

In `R/discover-lengjan.R`:

(a) Change the `@include` line to `#' @include ingest-lengjan-odds.R lengjan-api.R storage.R config.R`,
then delete `parse_competition_dropdown()` and its roxygen block.

(b) Replace `classify_competition()` (roxygen and body) with:

```r
#' Classify a Lengjan competition name into (sex, division).
#'
#' Deterministic, advisory name match. Sex from a "kvenna"/"kv" marker;
#' division from the league-name pattern, per sport -- each federation has its
#' own codes (football BD/LD1-4, handball OD/G66, basketball BD/1D; spec
#' 2026-09-23 WS4). A name that matches no pattern is `division = NA`,
#' `confidence = "low"` -- surfaced for a human, never auto-wired. Non-ASCII
#' letters are written as `\u` escapes (R-source non-ASCII rule).
#'
#' @param lengjan_name Competition display name from Lengjan.
#' @param sport "football", "handball" or "basketball".
#' @param country Pass-through context.
#' @return Tibble `{sex, division, confidence}` (one row).
#' @export
classify_competition <- function(lengjan_name, sport, country) {
  nm <- lengjan_name
  Encoding(nm) <- "UTF-8"
  has <- function(p) grepl(p, nm, ignore.case = TRUE)
  female <- has("kvenna") || has("(^| )kv\\.?( |$)")
  sex <- if (female) "female" else "male"

  division <- if (has("bikar")) {
    "CUP"
  } else if (identical(sport, "handball")) {
    if (has("ol[i\u00ed]s")) {
      "OD"
    } else if (has("grill *66")) {
      "G66"
    } else {
      NA_character_
    }
  } else if (identical(sport, "basketball")) {
    if (has("b[o\u00f3]nus")) {
      "BD"
    } else if (has("1\\. *deild")) {
      "1D"
    } else {
      NA_character_
    }
  } else if (has("3\\. *deild")) {
    "LD3"
  } else if (has("4\\. *deild")) {
    "LD4"
  } else if (has("2\\. *deild")) {
    "LD2"
  } else if (has("lengjudeild")) {
    "LD1"
  } else if (has("besta *deild")) {
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

(c) Replace `lengjan_list_competitions()` and `propose_team_names()` (roxygen and bodies) with:

```r
#' List every competition Lengjan currently offers for a (sport, country).
#'
#' Read from the JSON program, not the site's country dropdown: Icelandic
#' events carry no `countryCode`, so the dropdown omits Iceland for every sport
#' (spec 2026-09-23 L4); [parse_lengjan_program()] falls back to `countryName`.
#'
#' @param sport,country Canonical names ("handball", "iceland").
#' @param events Output of [parse_lengjan_program()].
#' @return Tibble `{sport, country, comp_id, lengjan_name}` (possibly empty).
#' @export
lengjan_list_competitions <- function(sport, country, events) {
  hit <- events[events$sport_id == .lengjan_sport_id(sport) &
    events$country_code %in% .lengjan_country_code(country), , drop = FALSE]
  hit <- hit[!duplicated(hit$competition_id), , drop = FALSE]
  tibble::tibble(
    sport = rep(sport, nrow(hit)),
    country = rep(country, nrow(hit)),
    comp_id = hit$competition_id,
    lengjan_name = hit$competition_name
  )
}

#' Draft team_names for a competition from the program's participants.
#'
#' @param comp_id Lengjan competition id.
#' @param events Output of [parse_lengjan_program()].
#' @param known_teams Canonical team names to match against.
#' @return Tibble `{lengjan, canonical_guess, confidence}`.
#' @export
propose_team_names <- function(comp_id, events, known_teams) {
  ev <- events[events$competition_id == comp_id, , drop = FALSE]
  renderings <- unique(c(ev$home_team, ev$away_team))
  renderings <- renderings[!is.na(renderings) & nzchar(renderings)]
  match_team_names(renderings, known_teams)
}
```

(d) In `discover_new_competitions()`:
- Replace the `@param session` roxygen with
  `#' @param events Output of [parse_lengjan_program()]; \code{NULL} fetches the live program once.`
- Change the signature to
  `discover_new_competitions <- function(leagues, events = NULL, root = here::here("data"), list_fn = NULL, team_names_fn = NULL) {`
- Replace the two `if (is.null(list_fn))` / `if (is.null(team_names_fn))` blocks and the `active <- …`
  line with:

```r
  if ((is.null(list_fn) || is.null(team_names_fn)) && is.null(events)) {
    events <- parse_lengjan_program(lengjan_fetch_program())
  }
  if (is.null(list_fn)) {
    list_fn <- function(sport, country) lengjan_list_competitions(sport, country, events)
  }
  if (is.null(team_names_fn)) {
    team_names_fn <- function(comp_id, sport, country, sex, division) {
      kt <- .known_teams_for(sport, country, sex, division, root)
      propose_team_names(comp_id, events, kt)
    }
  }

  # Every active league from betting.mode "scrape" up -- including one with no
  # competitions yet, which is exactly the league discovery exists for (spec
  # 2026-09-23 WS4; the old has_lengjan filter made HB/BB invisible).
  active <- filter_leagues(leagues, active_only = TRUE)
  active <- active[vapply(active, betting_mode_at_least, logical(1), stage = "scrape")]
```

In the function's description, replace "lists the live competitions" with "lists the live competitions
from Lengjan's JSON program".

- [ ] **Step 4: Drop Chrome from the discovery script and workflow**

Replace `scripts/0N_discover.R` in full with:

```r
#!/usr/bin/env Rscript
# scripts/0N_discover.R --
# Discover Lengjan competitions we model but do not yet scrape.
#
# Reads Lengjan's public JSON program (no login, no browser) for each active
# (sport, country) at betting.mode "scrape" or above, diffs its competitions
# against config/leagues.yml, and writes data/discovery/proposals.json +
# SUMMARY.md. Read-only on our data (results only) -- never touches the ledger
# or the placer. CI-safe.
#
# Usage:
#   Rscript scripts/0N_discover.R

invisible(Sys.setlocale("LC_ALL", "en_US.UTF-8"))
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))

leagues <- load_leagues()

findings <- tryCatch(
  discover_new_competitions(leagues),
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
  "Discovery wrote {n} proposed competition(s) to {path} ({findings$unmodelled_offered_count} unmodelled offered)."
)
```

In `.github/workflows/discover-leagues.yml`:
- delete the `- uses: browser-actions/setup-chrome@v2` step (with its `id:` and `with:` lines);
- delete the `            any::chromote` line;
- in the `Discover Lengjan leagues` step, delete the `env:` block and its
  `CHROMOTE_CHROME: ${{ steps.setup-chrome.outputs.chrome-path }}` line;
- keep the V8 step (`jsonvalidate` needs it);
- in the schedule comment, replace `(8,11,14,20)` with `(minute 17 of 8,11,14,17,20)`.

In `.claude/rules/ci-conventions.md`, in the `discover-leagues.yml` row, replace
`Read-only Lengjan competition-dropdown discovery` with
`Read-only Lengjan JSON-API discovery (no browser; visits every league at betting.mode scrape or above)`.

- [ ] **Step 5: Delete the dead fixture, regenerate docs, then run the tests and confirm they pass**

```bash
git rm tests/testthat/fixtures/lengjan-parent-page.html
Rscript -e 'devtools::document(".")'
```

Run `test-discover-lengjan.R`, `test-discover-ci-isolation.R`, `test-discover-health.R` and
`test-placer-ci-isolation.R`.
Expected: all PASS. The existing `discover_new_competitions flags a new modelled comp…` test passes
unchanged: it injects `list_fn` and `team_names_fn`, and its league has no `betting`, i.e. mode `auto`.

- [ ] **Step 6: Commit**

```bash
git fetch -q origin && git branch --show-current
git add R/discover-lengjan.R scripts/0N_discover.R .github/workflows/discover-leagues.yml tests/testthat/test-discover-lengjan.R .claude/rules/ci-conventions.md NAMESPACE DESCRIPTION man/
git commit -m 'feat(discovery): read Lengjan competitions from the JSON program

The country dropdown omits Iceland (Icelandic events carry no countryCode)
and needed a browser. Discovery now lists competitions and team renderings
from current-program, visits every active league at betting.mode scrape or
above (including those with no competitions yet), and classifies handball
(OD/G66) and basketball (BD/1D) names. Chrome is gone from the workflow.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>'
```

---

### Task 10: Put handball and basketball on the ladder (config, interlock test, `/bet`, docs)

**Files:**
- Modify: `config/leagues.yml` (the handball and basketball `lengjan` and `betting` blocks)
- Modify: `tests/testthat/test-betting-interlock.R` (the shipped-config assertions at `:24-39`, plus the
  header comment)
- Modify: `.claude/skills/bet/SKILL.md` (paper labelling)
- Modify: `.claude/rules/sports-betting.md` (a `mode` line in the betting schema block)
- Modify: `docs/runbooks/season-restart.md`, `docs/runbooks/stale-odds.md` and `docs/runbooks/README.md`

**Interfaces:**
- Consumes: everything above. Also `lengjan.source`/`division` (Task 2), the API ingest (Task 6) and
  the gates (Tasks 3, 4 and 8).
- Produces the shipped state:
  - football `auto`, still on the DOM scraper;
  - handball `paper`, API, comp `1269`/OD, 12 teams mapped;
  - basketball `scrape`, API, no competitions yet.

- [ ] **Step 1: Write the failing test**

In `tests/testthat/test-betting-interlock.R`, replace the two tests under
`# --- the shipped config is disarmed (D2) ---` (from that comment line down to and including the test
`"the disarmed leagues have no Lengjan competitions to scrape"`) with:

```r
# --- the shipped config sits on the betting ladder (spec 2026-09-23 WS5) ------

test_that("the shipped config pins each league's betting.mode", {
  lg <- load_leagues()
  expect_equal(betting_mode(lg$football_iceland), "auto")
  expect_equal(betting_mode(lg$handball_iceland), "paper")
  expect_equal(betting_mode(lg$basketball_iceland), "scrape")
})

test_that("handball scrapes Olisdeild karla via the API; basketball has no comps yet", {
  lg <- load_leagues()
  hb <- lg$handball_iceland$lengjan
  expect_equal(hb$source, "api")
  expect_equal(vapply(hb$competitions, function(cp) cp$id, character(1)), "1269")
  expect_equal(hb$competitions[[1]]$division, "OD")
  expect_equal(lg$basketball_iceland$lengjan$source, "api")
  expect_length(lg$basketball_iceland$lengjan$competitions, 0L)
  # Football keeps its full slate on the DOM scraper until the Milestone B cutover.
  expect_gt(length(lg$football_iceland$lengjan$competitions), 0L)
  expect_null(lg$football_iceland$lengjan$source)
})

test_that("handball team_names cover all 12 Olisdeild karla teams", {
  lg <- load_leagues()
  expect_true(all(c(
    "Afturelding", "FH", "Fram", "Haukar", "HK", "ÍBV", "KA",
    "Selfoss", "Stjarnan", "Valur", "Víkingur", "Þór"
  ) %in% names(lg$handball_iceland$lengjan$team_names$male)))
})
```

In the same file's first comment block, replace the two lines
`# One predicate, four enforcement points: odds ingest, decide, the placer's` and
`# recommendation loader, and the placer's pre-flight validator.` with the single line
`# Superseded by the betting ladder (spec 2026-09-23 WS1): off still means nothing at any layer.`. In
the comment of `"load_recommendations drops disabled leagues under the SHIPPED config"`, replace
`if basketball or handball is ever re-armed` with `if basketball or handball reaches betting.mode manual`.

- [ ] **Step 2: Run the test and confirm it fails**

Run `test-betting-interlock.R`.
Expected: FAIL, because handball's mode is `off` (it has `enabled: false`) and `source` is NULL.

- [ ] **Step 3: Edit `config/leagues.yml` with anchored Python replacements**

```bash
python3 - <<'EOF'
from pathlib import Path
p = Path("config/leagues.yml")
s = p.read_text(encoding="utf-8")
def swap(old, new):
    global s
    assert s.count(old) == 1, (s.count(old), old[:90])
    s = s.replace(old, new)

# --- basketball: lengjan -----------------------------------------------------
swap('''  lengjan:
    competitions: []
    # D2 (2026-09-02): publish-only this season -- competitions emptied so the
    # odds scrape has nothing to fetch, alongside betting.enabled: false below.
    # The schema permits an empty array (competitions has no minItems). Restore
    # these ids when betting resumes; they are last season's playoff umbrella
    # and should be re-verified against Lengjan before use:
    #  # 2026-04-28: Lengjan restructured kvenna efri/neðri (30774/30773) into a
    #  # single playoff umbrella per sex. Old IDs now return empty placeholder
    #  # pages (identical char count, no team hits). Men's Bónusdeild is now
    #  # also on Lengjan (was missing at migration time).
    #  - { id: "1519", name: "Bónusdeild karla úrslitakeppni", sex: male }
    #  - { id: "1528", name: "Bónusdeild kvenna úrslitakeppni", sex: female }
    team_names:
''', '''  lengjan:
    # 2026-09-23 (spec 2026-09-23 WS2/WS5): scraped from Lengjan's JSON API.
    # The 2026-27 Bónus deild competitions are not listed yet (season opens
    # 2026-09-29/30 and 10-08); discovery (0N_discover.R) proposes them and
    # /wire-league adds them here with `division: BD` or `1D`. Last season's
    # playoff umbrellas were 1519 (karla) and 1528 (kvenna).
    source: api
    competitions: []
    team_names:
''')

# --- basketball: betting ------------------------------------------------------
swap('''    # D2 (2026-09-02): publish-only. Enforced at four layers via
    # betting_enabled() -- odds ingest, decide, the placer loader and its
    # pre-flight. Absent means enabled, so football is untouched.
    enabled: false
    # kelly_frac is the §7.2 multiplicative shrinkage (Browne γ for finite-
''', '''    # Betting ladder (spec 2026-09-23 WS1): off < scrape < paper < manual <
    # auto. Scrape only: Lengjan settles basketball on regulation time
    # ("Framlenging gildir ekki") while our model and results use final
    # scores, so nothing is priced until Phase C adds regulation-time scores.
    mode: scrape
    # kelly_frac is the §7.2 multiplicative shrinkage (Browne γ for finite-
''')

# --- handball: lengjan --------------------------------------------------------
swap('''  lengjan:
    competitions: []
    # D2 (2026-09-02): publish-only this season -- see basketball_iceland above.
    # Restore when betting resumes:
    #  - { id: "1269", name: "Olísdeild karla", sex: male }
    team_names:
      male:
        Þór: Þór
        HK: HK
        ÍBV: ÍBV
        ÍR: ÍR
        FH: FH
        KA: KA
        ÍH: ÍH Keflavík
        HBH: HB Hafnarfjörður
        Valur: Valur
      female: {}                       # No women's handball on Lengjan currently
''', '''  lengjan:
    # 2026-09-23 (spec 2026-09-23 WS2/WS5): scraped from Lengjan's JSON API.
    # 1269 verified live 2026-09-23 (FH - Haukar, event 4602104). No Grill 66
    # or women's competition was listed.
    source: api
    competitions:
      - { id: "1269", name: "Olísdeild karla", sex: male, division: OD }
    team_names:
      # All 12 Olísdeild karla 2026-27 teams. Lengjan renders them as our
      # canonical names (every handball odds row 2026-03-04..05-07), except
      # Þór, which was also "Þór Akureyri". Víkingur has never appeared on
      # Lengjan; "Víkingur Rvk" is a guess from football's rendering, and an
      # unmapped name fails safe (decide skips the match).
      male:
        Afturelding: Afturelding
        FH: FH
        Fram: Fram
        Haukar: Haukar
        HK: HK
        ÍBV: ÍBV
        KA: KA
        Selfoss: Selfoss
        Stjarnan: Stjarnan
        Valur: Valur
        Víkingur: [Víkingur, Víkingur Rvk]
        Þór: [Þór, Þór Akureyri]
        ÍR: ÍR
        ÍH: ÍH Keflavík
        HBH: HB Hafnarfjörður
      female: {}                       # No women's handball on Lengjan currently
''')

# --- handball: betting --------------------------------------------------------
swap('''    # D2 (2026-09-02): publish-only. Enforced at four layers via
    # betting_enabled() -- odds ingest, decide, the placer loader and its
    # pre-flight. Absent means enabled, so football is untouched.
    enabled: false
    # 2026-05-02: scaled to 25 % of Browne default (user request — overall
''', '''    # Betting ladder (spec 2026-09-23 WS1): off < scrape < paper < manual <
    # auto. Paper: decide writes candidates + recommendations, the placer
    # never places them. Promotion to manual is the user's call after the
    # paper report (spec WS8); kelly_frac is decided then too (spec U3).
    mode: paper
    # 2026-05-02: scaled to 25 % of Browne default (user request — overall
''')

swap('''      spread: true
      total: true
    scoring:
      has_ties: true
      tie_threshold: 0.5
''', '''      spread: false            # Lengjan offers no handball handicap (spec L5)
      total: true
    scoring:
      has_ties: true
      tie_threshold: 0.5
''')

p.write_text(s, encoding="utf-8")
print("leagues.yml OK")
EOF
```

Expected output: `leagues.yml OK`. Every `assert` guards against a stale anchor.

- [ ] **Step 4: Confirm the config loads, and run the tests**

Run: `LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 Rscript -e 'devtools::load_all(".", quiet = TRUE); lg <- load_leagues(); print(vapply(lg, betting_mode, ""))'`
Expected: `basketball_iceland "scrape"`, `handball_iceland "paper"`, `football_iceland "auto"`, plus any
other active leagues shown as `auto`.

Run `test-betting-interlock.R`, `test-config.R`, `test-config-betting-schema.R`,
`test-publish-divisions-config.R` and `test-pipeline-run-isolation.R`.
Expected: all PASS. The fit-order tests build synthetic leagues and don't read the shipped config.
With the shipped config, Task 3's stage ranking fits football (`auto`) first, then handball
(`paper`), then basketball (`scrape`).

- [ ] **Step 5: Label paper recommendations in `/bet`, and update the betting rules**

In `.claude/skills/bet/SKILL.md`, directly after the paragraph `Present the table to the user.`, insert:

````markdown
**Paper recommendations.** Leagues below `betting.mode: manual` (see
`config/leagues.yml`) write recommendations the placer never places --
handball is `paper` since 2026-09-23. `preview_bets.R` omits them; the DuckDB
query above shows them. Label them `paper` when presenting:

```bash
cd /Users/brynjolfurjonsson/sports && Rscript -e '
suppressPackageStartupMessages(devtools::load_all(quiet = TRUE))
print(vapply(load_leagues(), betting_mode, character(1)))
'
```

A row whose `<sport>_<country>` maps to `scrape` or `paper` is a paper
recommendation.
````

In `.claude/rules/sports-betting.md`, inside the `betting:` YAML block of the
`` ## `config/leagues.yml::*.betting` schema `` section, insert directly after the `betting:` line:

```yaml
  # Ladder (spec 2026-09-23 WS1): off < scrape < paper < manual < auto. Ingest
  # needs scrape, decide paper, place_bets.R manual, launchd autoplace auto.
  # Absent: enabled decides (false -> off, else auto). Never set both.
  mode: auto
```

- [ ] **Step 6: Update the three runbooks with anchored Python replacements**

```bash
python3 - <<'EOF'
from pathlib import Path
def swap(path, old, new):
    p = Path(path); s = p.read_text(encoding="utf-8")
    assert s.count(old) == 1, (path, s.count(old), old[:70])
    p.write_text(s.replace(old, new), encoding="utf-8")

swap("docs/runbooks/season-restart.md",
'''- Do not enable betting to "wake a league up". Odds and results are separate
  paths: `betting.enabled: false` stops odds and placement, and has no effect
  on results ingest, fitting or publishing.''',
'''- Do not raise `betting.mode` to "wake a league up". Odds and results are
  separate paths: `betting.mode` (off < scrape < paper < manual < auto) gates
  odds, decide and placement, and has no effect on results ingest, fitting or
  publishing.''')

swap("docs/runbooks/stale-odds.md",
'''   They are no longer on seasonal pause, and both are configured
   `betting.enabled: false` -- so they produce **no odds rows at all, by
   design**, and `odds_freshness` has nothing to say about them. An absent
   odds row for basketball or handball is correct, not a fault. Only
   `football_iceland` is bet.''',
'''   Since 2026-09-23 both are scraped from Lengjan's JSON API
   (`lengjan.source: api`): handball at `betting.mode: paper` (Olísdeild
   karla, comp 1269), basketball at `betting.mode: scrape` with no
   competitions wired until the 2026-27 Bónus deild appears. Below `manual`,
   `odds_freshness` caps a stall at WARN, and a league with no competitions
   reports PAUSED. Icelandic handball odds may post only on matchday. Only
   `football_iceland` is bet.''')

swap("docs/runbooks/README.md",
'''a basketball/handball cell producing no ODDS (both are
  `betting.enabled: false` -- publish-only, decision D2),''',
'''a basketball cell with no odds
  (`betting.mode: scrape`, no competitions wired yet) or a handball cell
  whose recommendations are never placed (`betting.mode: paper`; both
  spec 2026-09-23),''')
print("runbooks OK")
EOF
```

Expected output: `runbooks OK`.

- [ ] **Step 7: Commit**

```bash
git fetch -q origin && git branch --show-current
git add config/leagues.yml tests/testthat/test-betting-interlock.R .claude/skills/bet/SKILL.md .claude/rules/sports-betting.md docs/runbooks/season-restart.md docs/runbooks/stale-odds.md docs/runbooks/README.md
git commit -m 'feat(config): handball to paper, basketball to scrape, both on the API

Handball scrapes Olisdeild karla (Lengjan comp 1269, verified live
2026-09-23) and writes paper recommendations; its team_names now cover
all 12 teams and spread is off because Lengjan offers no handball
handicap. Basketball only scrapes, because Lengjan settles on regulation
time. Football is unchanged. /bet labels paper recommendations.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>'
```

---

### Task 11: Scrape cadence (offset minute, closing-line slot)

**Files:**
- Modify: `.github/workflows/scrape-odds.yml` (the cron and its comment)
- Modify: `.claude/rules/ci-conventions.md:96` (the workflow inventory row)

**Interfaces:** none (CI schedule only). Football keeps Chrome in this workflow until Milestone B.

- [ ] **Step 1: Edit the cron with an anchored replacement**

```bash
python3 - <<'EOF'
from pathlib import Path
def swap(path, old, new):
    p = Path(path); s = p.read_text(encoding="utf-8")
    assert s.count(old) == 1, (path, s.count(old), old[:70])
    p.write_text(s.replace(old, new), encoding="utf-8")

swap(".github/workflows/scrape-odds.yml",
'''    # 4x daily UTC. The 11:00 slot exists because Lengjan sometimes posts
    # same-day odds late morning (2026-06-10: posted ~11:00 for evening
    # kickoffs) -- without it those odds wait for the 14:00 cycle, recs
    # regenerate hours later, and the 12:00 healthcheck can false-FAIL on
    # "fixture today, no odds scraped".
    - cron: '0 8,11,14,20 * * *\'''',
'''    # 5x daily UTC at minute 17, not :00 -- GitHub queues :00 crons hardest
    # and this job ran 2-4.5h behind them (2026-09-08), so the 11:00 slot
    # (Lengjan posts same-day odds late morning, 2026-06-10) was rarely
    # honoured. 17:17 adds a pre-kickoff snapshot for Icelandic evening games,
    # whose handball odds may post only on matchday (spec 2026-09-23 WS9).
    - cron: '17 8,11,14,17,20 * * *\'''')

swap(".claude/rules/ci-conventions.md",
"| `scrape-odds.yml` | cron 4×/day (08,11,14,20 UTC) | Lengjan odds snapshot |",
"| `scrape-odds.yml` | cron 5×/day (08,11,14,17,20 UTC, minute 17) | Lengjan odds snapshot (Chromote DOM for football; JSON API for handball/basketball) |")
print("cadence OK")
EOF
```

Expected output: `cadence OK`.

- [ ] **Step 2: Validate the workflow YAML and the CI guards**

Run: `Rscript -e 'w <- yaml::read_yaml(".github/workflows/scrape-odds.yml"); cat(unlist(w[["TRUE"]]$schedule), "\n")'`
(YAML 1.1 reads the key `on:` as boolean `TRUE`. PyYAML isn't installed in this environment, so use
R's `yaml`.)
Expected: `17 8,11,14,17,20 * * *`.

Run `test-placer-ci-isolation.R` and any `test-ci-*.R`/`test-workflow*.R` files present
(`ls tests/testthat | grep -E 'ci-isolation|workflow'`).
Expected: PASS.

- [ ] **Step 3: Commit**

```bash
git fetch -q origin && git branch --show-current
git add .github/workflows/scrape-odds.yml .claude/rules/ci-conventions.md
git commit -m 'ci(odds): scrape at minute 17, add a 17:17 closing-line slot

The :00 slots drifted 2-4.5h behind schedule, so the 11:00 same-day slot
was rarely honoured. The offset minute sidesteps the most congested
queue, and 17:17 catches Icelandic evening kickoffs whose handball odds
may post only on matchday.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>'
```

---

### Task 12: Whole-branch verification, a live smoke test, and a PR

**Files:** none new, unless verification finds a defect. In that case fix it in the owning task's
files with its own test, and commit it separately.

- [ ] **Step 1: The full suite against the baseline**

```bash
LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 Rscript -e 'devtools::load_all(".", quiet = TRUE); r <- as.data.frame(testthat::test_dir(normalizePath("tests/testthat"), load_package = "none", reporter = "summary", stop_on_failure = FALSE)); cat("failed=", sum(r$failed), " errors=", sum(r$error), "\n"); print(unique(r$file[r$failed > 0 | r$error]))'
```

Expected: the failing-file list is a subset of the Baseline list. Any new failing file is a defect;
fix it before continuing.

- [ ] **Step 2: `NAMESPACE` and `man/` are in sync**

Run: `Rscript -e 'devtools::document(".")' && git status --short NAMESPACE DESCRIPTION man/`
Expected: no output. If there is output, commit it as `docs: regenerate roxygen`.

- [ ] **Step 3: Live API smoke test, written into a temporary root only (never `data/`)**

```bash
LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 Rscript -e '
devtools::load_all(".", quiet = TRUE)
lg <- load_leagues()
root <- tempfile("odds-smoke-"); dir.create(root)
n <- ingest_lengjan_api(lg["handball_iceland"], root = root)
cat("rows written:", n, "\n")
if (n > 0) print(read_table("odds", root = root)[, c("match_date","home_team","away_team","market","outcome","line","odds","sex","event_id")])
f <- discover_new_competitions(lg)
cat("discovery proposals:", length(f$competitions), "\n")
for (x in f$competitions) cat(" ", x$sport, x$comp_id, x$lengjan_name, x$inferred_sex, x$inferred_division, "\n")
'
```

Expected: the command exits cleanly. `rows written` is ≥ 0; it is 0 when no Olísdeild market is open
at that moment (spec L7). Discovery lists the proposals it finds. Football 27524, and the Bónus deild
comps if they are listed by then, are expected. Nothing is written under `data/`:
`git status --short data/` must be empty.

- [ ] **Step 4: A whole-branch review**

Invoke `superpowers:requesting-code-review` over `origin/main...HEAD`. Trace one handball odds row end
to end across the task boundaries: API JSON → `lengjan_api_odds_rows` → `upsert_table` → `read_table`
→ `prepare_odds` (sex filter) → `decide_league` (paper) → `load_recommendations` (dropped below manual)
→ `check_capture_rate` (excluded). Fix every finding that holds up, with a test.

- [ ] **Step 5: Push and open the PR (confirm with the user first; this is outward-facing)**

```bash
git fetch -q origin && git branch --show-current
git log --oneline origin/main..HEAD
git push -u origin feat/hb-bb-lengjan-odds
gh pr create --base main --title 'Handball + basketball Lengjan odds via the JSON API (Milestone A)' --body-file - <<'EOF'
Implements Milestone A of docs/superpowers/specs/2026-09-23-hb-bb-lengjan-odds-staged-betting-design.md
(plan: docs/superpowers/plans/2026-09-23-hb-bb-lengjan-odds-milestone-a.md).

- betting.mode ladder (off < scrape < paper < manual < auto) enforced at ingest, decide, placer, autoplace and health
- Lengjan JSON API client (current-program + batched markets) and API odds ingest, chosen per league by lengjan.source
- odds gain nullable sex/event_id/competition_id/kickoff_at; read path fixed for the mixed-tz store
- handball: paper (Olisdeild karla 1269); basketball: scrape; football unchanged (DOM scraper until Milestone B)
- discovery reads the JSON program (no browser); scrape cron moves to minute 17 with a 17:17 slot

Not in this PR: the football cutover (Milestone B), the paper report and the placer event_id path (Milestone C).

🤖 Generated with [Claude Code](https://claude.com/claude-code)
EOF
```

Do **not** use `gh pr merge --auto`: `protect-main` has no required checks, so it fails
(`.claude/rules/git-hygiene.md`).
