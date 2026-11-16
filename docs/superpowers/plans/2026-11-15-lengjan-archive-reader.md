# Lengjan Archive Reader Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Read a local copy of the raw Lengjan odds archive into odds-shaped tibbles, using the parse functions the live scrape already uses.

**Architecture:** One new file, `R/lengjan-archive.R`. It reads the archive's manifest, loads the gzipped JSON bodies each capture points at, and passes them through `parse_lengjan_program()`, `parse_lengjan_markets()` and `lengjan_markets_to_odds()`. It reads only; it never writes to `data/facts/odds/` and makes no network request.

**Tech Stack:** R (`jsonlite`, `tibble`, `dplyr`, `cli`, all already in `Imports`), `testthat` edition 3.

**Spec:** the archive is produced by a capture job in a separate private repository; its format contract is that repository's `docs/archive-format.md`. Everything this plan needs from it is restated under "Archive format" below.

## Archive format (version 1)

```
<dir>/manifest/YYYY-MM.jsonl     one JSON object per line, one line per capture
<dir>/raw/YYYY/MM/DD/<ts>-program.json.gz        a current-program body
<dir>/raw/YYYY/MM/DD/<ts>-markets-NNN.json.gz    a markets body (a JSON array of market groups)
```

A manifest line:

```json
{"v": 1, "ts": "2026-11-19T18:13:00Z", "reason": "baseline", "status": "ok",
 "program": {"file": "raw/2026/11/19/20261119T181300Z-program.json.gz", "bytes": 1239107, "sha256": "…", "http_s": 0.29, "n_events": 1007},
 "markets": [{"file": "raw/2026/11/19/20261119T181300Z-markets-001.json.gz", "event_ids": ["4602104"], "bytes": 38211, "sha256": "…"}],
 "n_detail_events": 1, "n_requests": 2, "error": null}
```

`status` is `ok`, `partial` (programme present, some markets missing), `error` (body kept but invalid) or `skipped` (`program` is `null`, `markets` is empty). `file` paths are relative to `<dir>`. All times are UTC.

## Global Constraints

- Work on a feature branch in a git worktree (the main checkout must stay on `main` for the autoplace agent); land through a PR, merged with `gh pr merge --rebase --delete-branch`.
- No machine-specific path in any tracked file. The archive directory comes from option `sports.lengjan_archive_dir`, else environment variable `LENGJAN_ARCHIVE_DIR`.
- Non-ASCII characters in R code as `\uXXXX` escapes. `devtools::document()` after adding exports; never edit `NAMESPACE` by hand.
- Run tests with `LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 Rscript -e 'devtools::test(filter = "lengjan-archive")'`, and the whole suite once before the PR.
- Returned `scraped_at` is the manifest `ts`. Team names stay as Lengjan renders them.
- End commit messages with `Co-Authored-By: Claude Fable 5.2 <noreply@anthropic.com>`.

## Review Focus

1. **A capture whose manifest line names a file that is not on disk** (a pull that was interrupted). Expected: that capture is skipped with a warning naming the file; the rest are returned. Test in Task 3.
2. **A manifest line that is not valid JSON** (a truncated last line). Expected: the line is skipped with a warning; the index still loads. Test in Task 1.
3. **An empty window** (`from`/`to` match no capture, or the archive has no manifest yet). Expected: a zero-row tibble with the full column set, not an error. Test in Task 3.
4. **An event listed in two captures.** Expected: one set of rows per capture, distinguished by `scraped_at`; nothing is de-duplicated across captures. Test in Task 3.
5. **A sport id or country code the package does not know.** Expected: `sport` or `country` is `NA`, and `sport_id`, `sport_name`, `country_code` carry the payload's values. Test in Task 3.

---

### Task 1: Archive directory and index

**Files:**
- Create: `R/lengjan-archive.R`, `tests/testthat/helper-lengjan-archive.R`
- Test: `tests/testthat/test-lengjan-archive.R`

**Interfaces:**
- Produces: `.lengjan_archive_dir(dir = NULL)` (internal; returns a path or stops); exported `lengjan_archive_index(dir = NULL)` returning a tibble sorted by `ts` with columns `ts` (POSIXct, UTC), `reason`, `status`, `program_file`, `n_events` (integer), `markets_files` (list of character vectors), `n_detail_events` (integer), `n_requests` (integer), `error`.
- Test helper: `.local_archive(captures, env = parent.frame())` builds an archive in `withr::local_tempdir()` and returns its path. Each element of `captures` is `list(ts = "2026-09-23T12:00:00Z", program = <fixture file or NULL>, markets = <character vector of fixture files>, status = "ok", reason = "baseline")`. It gzips the named files from `tests/testthat/fixtures/lengjan-api/` into `raw/YYYY/MM/DD/` and writes the manifest lines.

- [ ] **Step 1: Write the failing tests**

```r
test_that("the archive directory comes from the argument, the option, then the env var", {
  withr::local_options(sports.lengjan_archive_dir = NULL)
  withr::local_envvar(LENGJAN_ARCHIVE_DIR = NA)
  expect_error(.lengjan_archive_dir(), "sports.lengjan_archive_dir|LENGJAN_ARCHIVE_DIR")
  withr::local_envvar(LENGJAN_ARCHIVE_DIR = "/env")
  expect_equal(.lengjan_archive_dir(), "/env")
  withr::local_options(sports.lengjan_archive_dir = "/opt")
  expect_equal(.lengjan_archive_dir(), "/opt")
  expect_equal(.lengjan_archive_dir("/arg"), "/arg")
})

test_that("lengjan_archive_index returns one row per capture, sorted", {
  dir <- .local_archive(list(
    list(ts = "2026-09-23T12:30:00Z", program = "current-program.json", markets = "markets.json"),
    list(ts = "2026-09-23T12:00:00Z", program = "current-program.json", markets = character())
  ))
  ix <- lengjan_archive_index(dir)
  expect_equal(format(ix$ts, "%H:%M", tz = "UTC"), c("12:00", "12:30"))
  expect_s3_class(ix$ts, "POSIXct")
  expect_equal(lengths(ix$markets_files), c(0L, 1L))
  expect_equal(ix$status, c("ok", "ok"))
})

test_that("a truncated manifest line is skipped with a warning", {       # Review Focus 2
  dir <- .local_archive(list(list(ts = "2026-09-23T12:00:00Z", program = "current-program.json", markets = character())))
  cat('{"v": 1, "ts": "2026-09-23T12:3', file = file.path(dir, "manifest", "2026-09.jsonl"), append = TRUE)
  expect_warning(ix <- lengjan_archive_index(dir), "manifest")
  expect_equal(nrow(ix), 1L)
})

test_that("an archive with no manifest gives a zero-row index", {
  ix <- lengjan_archive_index(withr::local_tempdir())
  expect_equal(nrow(ix), 0L)
  expect_named(ix, c("ts", "reason", "status", "program_file", "n_events", "markets_files", "n_detail_events", "n_requests", "error"))
})
```

- [ ] **Step 2: Run to verify failure** — expected: `could not find function ".lengjan_archive_dir"`.
- [ ] **Step 3: Implement** the helper file, `.lengjan_archive_dir()` and `lengjan_archive_index()`. Read each line with `jsonlite::fromJSON(line, simplifyVector = FALSE)` inside `tryCatch`.
- [ ] **Step 4: Run to verify pass** — 4 tests pass.
- [ ] **Step 5: Commit** — `feat(odds): index a local Lengjan odds archive`.

---

### Task 2: The programme's embedded main market

**Files:**
- Modify: `R/lengjan-archive.R`
- Test: `tests/testthat/test-lengjan-archive.R`

**Interfaces:**
- Produces: exported `parse_lengjan_program_markets(prog)` returning a tibble `event_id`, `market`, `outcome`, `line`, `odds`: one moneyline row per selection of each event's embedded `market`, for events where that market has `typeName` `"3WAY"`, `primary` true and `status` `"open"`, the selection name is `1`, `X` or `2`, and the price (integer / 100) is above 1. Events from all four programme arrays, first copy of each id. Embedded `2WAY` and `NWAY` markets are dropped.

- [ ] **Step 1: Write the failing tests**

```r
test_that("parse_lengjan_program_markets maps the embedded 3WAY to moneyline", {
  prog <- list(events = list(
    list(id = "1", market = list(typeName = "3WAY", primary = TRUE, status = "open",
      selections = list(list(name = "1", odds = 194L), list(name = "X", odds = 264L), list(name = "2", odds = 406L)))),
    list(id = "2", market = list(typeName = "2WAY", primary = TRUE, status = "open",
      selections = list(list(name = "1", odds = 150L), list(name = "2", odds = 250L)))),
    list(id = "3", market = NULL),
    list(id = "4", market = list(typeName = "3WAY", primary = TRUE, status = "suspended",
      selections = list(list(name = "1", odds = 194L))))
  ), liveSoon = list(list(id = "1", market = NULL)))
  out <- parse_lengjan_program_markets(prog)
  expect_equal(out$event_id, c("1", "1", "1"))
  expect_equal(out$outcome, c("home", "draw", "away"))
  expect_equal(out$odds, c(1.94, 2.64, 4.06))
  expect_equal(unique(out$market), "moneyline")
  expect_true(all(is.na(out$line)))
})

test_that("parse_lengjan_program_markets returns the empty shape when nothing qualifies", {
  out <- parse_lengjan_program_markets(list(events = list(), liveSoon = list()))
  expect_equal(nrow(out), 0L)
  expect_named(out, c("event_id", "market", "outcome", "line", "odds"))
})
```

- [ ] **Step 2 to 4:** run to fail, implement, run to pass.
- [ ] **Step 5: Commit** — `feat(odds): parse the main market embedded in Lengjan's programme`.

---

### Task 3: `read_lengjan_archive()`

**Files:**
- Modify: `R/lengjan-archive.R`; `R/ingest-lengjan-odds.R` (lift the country-code vector inside `.lengjan_country_code()` to a file-level constant `.LENGJAN_COUNTRY_CODES`, used by that function unchanged)
- Test: `tests/testthat/test-lengjan-archive.R`

**Interfaces:**
- Consumes: `lengjan_archive_index()`, `parse_lengjan_program_markets()`, and the existing `parse_lengjan_program()`, `parse_lengjan_markets()`, `lengjan_markets_to_odds()`, `lengjan_api_odds_rows()`, `load_leagues()`.
- Produces: exported `read_lengjan_archive(dir = NULL, from = NULL, to = NULL, what = c("detail", "program"), leagues = load_leagues())`. Returns a tibble with columns, in order: `sport`, `country`, `scraped_at`, `match_date`, `home_team`, `away_team`, `market`, `outcome`, `line`, `odds`, `sex`, `event_id`, `competition_id`, `kickoff_at`, `sport_id`, `sport_name`, `country_code`, `competition_name`.
  - `from` and `to` are inclusive bounds on the capture `ts`; each accepts a `POSIXct`, a `Date` or a string; `NULL` means unbounded.
  - `sport`: `football`, `basketball`, `handball` for sport ids 1, 2, 6, else `NA`. `country`: the name in `.LENGJAN_COUNTRY_CODES` whose code equals `country_code`, else `NA`. `sex`: the `sex` of the competition in `leagues` whose `id` equals `competition_id` and whose league `sport` equals the row's `sport`, else `NA`. `sport_name` is the event's `sportName` from the raw programme. `match_date` is the UTC date of `kickoff_at`.
  - Rows whose event has no home or away team are dropped, as `lengjan_api_odds_rows()` does.
  - Captures with `status` `error` or `skipped` are skipped; `error` ones with a `cli::cli_alert_warning` naming the `ts`.
- Also produces two internal, vectorised helpers: `.lengjan_sport_from_id(sport_id)` and `.lengjan_country_from_code(code)`, each returning `NA` for an unknown value.

- [ ] **Step 1: Write the failing tests**

```r
.two_captures <- function(env = parent.frame()) {
  .local_archive(list(
    list(ts = "2026-09-23T12:00:00Z", program = "current-program.json", markets = "markets.json"),
    list(ts = "2026-09-23T12:30:00Z", program = "current-program.json", markets = "markets.json")
  ), env = env)
}
# The fixtures' only events WITH markets are 4616742 (handball, comp 9281, DE,
# 9 rows), 4412294 (basketball, comp 9445, US, 5 rows) and 4601670 (football,
# comp 10046, INT, 25 rows). The Icelandic events carry none, so a league keyed
# on comp 1269 would make every assertion below vacuous.
.hb <- function() {
  list(handball_x = list(sport = "handball", country = "germany",
    lengjan = list(source = "api", competitions = list(list(id = "9281", name = "x", sex = "male", division = "OD")))))
}

test_that("detail rows for a configured competition equal lengjan_api_odds_rows()", {
  dir <- .two_captures()
  got <- read_lengjan_archive(dir, what = "detail", leagues = .hb())
  ts <- as.POSIXct("2026-09-23 12:00:00", tz = "UTC")
  want <- lengjan_api_odds_rows(parse_lengjan_program(.fx("current-program.json")),
    parse_lengjan_markets(.fx("markets.json")), .hb()$handball_x, ts)
  expect_equal(nrow(want), 9L)                                   # guards against a vacuous comparison
  mine <- got[got$scraped_at == ts & got$competition_id %in% "9281", names(want)]
  key <- c("event_id", "market", "outcome", "line")
  expect_equal(dplyr::arrange(mine, dplyr::across(dplyr::all_of(key))),
               dplyr::arrange(want, dplyr::across(dplyr::all_of(key))))
  expect_equal(nrow(got[got$scraped_at == ts, ]), 39L)           # 9 + 5 + 25: every event, configured or not
})

test_that("an event in two captures yields rows per capture", {            # Review Focus 4
  got <- read_lengjan_archive(.two_captures(), what = "detail", leagues = .hb())
  expect_equal(length(unique(got$scraped_at)), 2L)
  n <- table(got$scraped_at)
  expect_equal(n[[1]], n[[2]])
})

test_that("unconfigured competitions come back with sex NA and payload fields", {
  got <- read_lengjan_archive(.two_captures(), what = "detail", leagues = list())
  expect_gt(nrow(got), 0L)
  expect_true(all(is.na(got$sex)))
  expect_false(anyNA(got$sport_id)); expect_false(anyNA(got$country_code)); expect_false(anyNA(got$sport_name))
})

test_that("unknown sport ids and country codes give NA sport and country", {  # Review Focus 5
  expect_equal(.lengjan_sport_from_id(c(1L, 2L, 6L, 4L)), c("football", "basketball", "handball", NA))
  expect_equal(.lengjan_country_from_code(c("IS", "DE", "US", NA)), c("iceland", "germany", NA, NA))
  got <- read_lengjan_archive(.two_captures(), what = "program", leagues = list())
  expect_equal(unique(got$country[got$country_code == "DE"]), "germany")
  expect_true(all(is.na(got$country[got$country_code == "US"])))   # the fixture's basketball event
  expect_equal(unique(got$sport_name[got$sport_id == 2L]), "Karfa")
})

test_that("program mode returns moneyline rows for events with an embedded market", {
  got <- read_lengjan_archive(.two_captures(), what = "program", leagues = .hb())
  expect_equal(nrow(got), 18L)                                     # 3 events x 3 selections x 2 captures
  expect_equal(unique(got$sex[got$competition_id == "9281"]), "male")
  expect_equal(unique(got$market), "moneyline")
  expect_named(got, c("sport", "country", "scraped_at", "match_date", "home_team", "away_team", "market",
    "outcome", "line", "odds", "sex", "event_id", "competition_id", "kickoff_at", "sport_id", "sport_name",
    "country_code", "competition_name"))
})

test_that("from and to bound the captures, and an empty window is a zero-row tibble", {  # Review Focus 3
  dir <- .two_captures()
  one <- read_lengjan_archive(dir, from = "2026-09-23T12:15:00Z", leagues = .hb())
  expect_equal(format(unique(one$scraped_at), "%H:%M", tz = "UTC"), "12:30")
  none <- read_lengjan_archive(dir, from = as.Date("2027-01-01"), leagues = .hb())
  expect_equal(nrow(none), 0L); expect_equal(ncol(none), 18L)
  expect_equal(nrow(read_lengjan_archive(withr::local_tempdir(), leagues = .hb())), 0L)
})

test_that("a capture naming a missing file is skipped with a warning", {    # Review Focus 1
  dir <- .two_captures()
  unlink(list.files(file.path(dir, "raw"), pattern = "123000Z-markets", recursive = TRUE, full.names = TRUE))
  expect_warning(got <- read_lengjan_archive(dir, what = "detail", leagues = .hb()), "123000Z-markets")
  expect_equal(length(unique(got$scraped_at)), 1L)
})

test_that("error and skipped captures are not read", {
  dir <- .local_archive(list(
    list(ts = "2026-09-23T12:00:00Z", program = "current-program.json", markets = "markets.json"),
    list(ts = "2026-09-23T12:30:00Z", program = "current-program.json", markets = character(), status = "error"),
    list(ts = "2026-09-23T13:00:00Z", program = NULL, markets = character(), status = "skipped", reason = "budget")
  ))
  got <- suppressMessages(read_lengjan_archive(dir, what = "program", leagues = .hb()))
  expect_equal(length(unique(got$scraped_at)), 1L)
})

test_that(".lengjan_country_code still maps canonical names", {
  expect_equal(.lengjan_country_code("iceland"), "IS")
  expect_error(.lengjan_country_code("atlantis"), "Unknown Lengjan country code")
})
```

- [ ] **Step 2: Run to verify failure.**
- [ ] **Step 3: Implement.** Read a body with `jsonlite::fromJSON(gzfile(path), simplifyVector = FALSE)`. Build each capture's rows, then `dplyr::bind_rows()`.
- [ ] **Step 4: Run to verify pass** — the file's tests pass; then run `devtools::test(filter = "lengjan|ingest")` to confirm the country-code refactor broke nothing.
- [ ] **Step 5: Commit** — `feat(odds): read a raw Lengjan archive into odds-shaped rows`.

---

### Task 4: Documentation and the full suite

**Files:**
- Create: `docs/runbooks/odds-archive.md`
- Modify: `.Renviron.example`, `CLAUDE.md` (one bullet under "Quick reference" pointing at the runbook), `NAMESPACE` and `man/` via `devtools::document()`

- [ ] **Step 1: Write the runbook** (under 40 lines): what the archive is and that a separate private repository captures it; set `LENGJAN_ARCHIVE_DIR` in `.Renviron`; the three exported functions with one example each; that the reader never writes to `data/facts/odds/`; that the format contract lives with the capture job.
- [ ] **Step 2: Add to `.Renviron.example`** a commented block: `# Local copy of the raw Lengjan odds archive (see docs/runbooks/odds-archive.md).` and `LENGJAN_ARCHIVE_DIR=`.
- [ ] **Step 3: Run `devtools::document()`**; confirm `NAMESPACE` gained exactly `export(lengjan_archive_index)`, `export(parse_lengjan_program_markets)`, `export(read_lengjan_archive)`.
- [ ] **Step 4: Run the whole suite** — `LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 Rscript -e 'devtools::test()'`; expected: no failures. (Check first that no other session is timing benchmarks on the machine.)
- [ ] **Step 5: Commit, push the branch, open the PR.**
- [ ] **Step 6: After the first pull of the real archive,** the owner adds `LENGJAN_ARCHIVE_DIR=<path to the pulled archive>` to `.Renviron` (that file holds credentials: do not read, print or edit it from a session). Then read one known Icelandic event with `read_lengjan_archive(what = "detail")` and compare its prices with the CI scrape's row for the nearest `scraped_at` in `data/facts/odds/`. Record the comparison in the PR.
