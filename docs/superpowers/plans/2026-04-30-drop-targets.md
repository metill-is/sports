# Drop {targets}: Replace DAG with Discrete Scripts + Freshness Guards

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the `{targets}` DAG with five plain R scripts (one per pipeline layer), each with an explicit "should I run?" guard. Eliminate the cross-workflow git race that hit CI on 2026-04-30, and align the runtime model with the actual problem shape: a linear chain over Parquet files in git.

**Architecture:**
- Five entry-point scripts in `scripts/0N_*.R`, one per layer (active-competitions → ingest → odds → fit → decide → publish; the active-competitions script is a precondition shared by ingest and odds, not a separate "layer").
- Each script reads from disk (existing `R/` functions), checks a freshness predicate, runs the layer or exits 0.
- A new `R/pipeline-freshness.R` module exports two unit-testable predicates: `needs_refit()` and `has_upcoming_games()`.
- CI workflows become one-line callers; `decide.yml` and `publish.yml` chain via `workflow_run` so published JSONs refresh on every odds scrape (3× daily) instead of once.
- `_targets.R`, the `targets` dependency, and the always-cue gymnastics are deleted entirely. The underlying `R/` functions (`ingest_one_league`, `fit_one`, `decide_one`, `publish_one`) are unchanged — they were always disk-driven (`fit_one`'s `ingest_dep` parameter has a `NULL` default).

**Tech Stack:** R, arrow (Parquet), cli (CLI output), fs (paths), the existing `sports` package functions, GitHub Actions (`workflow_run` chaining).

**Scope note:** The current active league set is `football_iceland` only; basketball/handball restart in autumn. Scripts iterate over `filter_leagues(active_only = TRUE)` so the league set is config-driven — no hardcoded league names. When other sports flip back to `active: true` in `config/leagues.yml`, the scripts work unchanged.

---

## File Structure

### Created

| Path | Responsibility |
|---|---|
| `R/pipeline-freshness.R` | `needs_refit(static, sex, root)`, `has_upcoming_games(static, root, days = 14)`, `latest_completed_match_date(static, sex, root)` |
| `scripts/_lib.R` | `parse_pipeline_args()` — shared `--league/--sex/--force` parser for all entry scripts |
| `scripts/00_active_competitions.R` | Generate `config/active_competitions.json` (was the `active_competitions` static target) |
| `scripts/01_ingest_results.R` | Federation results + schedules; iterates active leagues |
| `scripts/02_scrape_odds.R` | Lengjan odds; skip leagues with no upcoming games |
| `scripts/03_fit.R` | Stan posteriors; skip (league × sex) pairs that don't need refitting |
| `scripts/04_decide.R` | Kelly + portfolio + calibration; skip when no current odds |
| `scripts/05_publish.R` | Publish JSONs |
| `tests/testthat/test-pipeline-freshness.R` | Unit tests for the predicates using temp-dir fixtures |
| `.github/workflows/fit.yml` | Runs `03_fit.R` after `scrape-results.yml` |
| `.github/workflows/decide-publish.yml` | Runs `04_decide.R` then `05_publish.R` after `fit.yml` AND after `scrape-odds.yml` |

### Modified

| Path | Change |
|---|---|
| `DESCRIPTION` | Remove `targets` from Imports |
| `.gitignore` | Remove `_targets/` entry |
| `.github/workflows/scrape-results.yml` | Replace `Rscript run.R --all --step data` with `Rscript scripts/00_active_competitions.R && Rscript scripts/01_ingest_results.R`; drop the targets cache step |
| `.github/workflows/scrape-odds.yml` | Replace `Rscript run.R --all --step odds` with `Rscript scripts/00_active_competitions.R && Rscript scripts/02_scrape_odds.R`; drop the targets cache step |
| `CLAUDE.md` | Update directory structure, Quick Reference, Conventions sections; remove `_targets.R` references |
| `.claude/skills/sports-update/SKILL.md` | Replace `run.R --step ...` invocations with `scripts/0N_*.R` calls |
| `.claude/skills/bet/SKILL.md` | Same |
| `.claude/skills/place-bets/SKILL.md` | Same |
| `.claude/skills/add-league/SKILL.md` | Same; remove DAG-specific guidance |

### Deleted

| Path | Reason |
|---|---|
| `_targets.R` | Replaced by `scripts/0N_*.R` |
| `run.R` | Replaced by direct script invocation |
| `tests/testthat/test-targets-dag.R` | Asserts targets DAG structure |
| `tests/testthat/test-targets-slice-isolation.R` | Asserts targets cache isolation |
| `.github/workflows/fit-and-publish.yml` | Split into `fit.yml` + `decide-publish.yml` |
| `scripts/fit_all.R` | Deprecated, now obsolete |
| `scripts/decide_all.R` | Same |
| `scripts/publish_all.R` | Same |
| `scripts/backfill_ingest.R` | Same |

---

## Task 1: Add freshness predicates with unit tests

**Files:**
- Create: `R/pipeline-freshness.R`
- Test: `tests/testthat/test-pipeline-freshness.R`

The predicates are the architectural core: they replace `cue = "always"` with explicit, human-readable rules. TDD them so the contract is clear.

- [ ] **Step 1: Write failing tests**

Create `tests/testthat/test-pipeline-freshness.R`:

```r
test_that("needs_refit() is TRUE when no fit exists", {
  root <- withr::local_tempdir()
  static <- list(sport = "football", country = "iceland")

  # Seed minimal results parquet — completed game on 2026-04-29
  results_dir <- fs::path(root, "facts", "results",
                          "sport=football", "country=iceland",
                          "sex=male", "season=2026")
  fs::dir_create(results_dir)
  arrow::write_parquet(
    tibble::tibble(
      home_team = "A", away_team = "B",
      match_date = as.Date("2026-04-29"),
      home_score = 1L, away_score = 0L
    ),
    fs::path(results_dir, "part-0.parquet")
  )

  expect_true(needs_refit(static, "male", root = root))
})

test_that("needs_refit() is FALSE when fit covers all completed games", {
  root <- withr::local_tempdir()
  static <- list(sport = "football", country = "iceland")

  results_dir <- fs::path(root, "facts", "results",
                          "sport=football", "country=iceland",
                          "sex=male", "season=2026")
  fs::dir_create(results_dir)
  arrow::write_parquet(
    tibble::tibble(
      home_team = "A", away_team = "B",
      match_date = as.Date("2026-04-29"),
      home_score = 1L, away_score = 0L
    ),
    fs::path(results_dir, "part-0.parquet")
  )

  fit_dir <- fs::path(root, "beliefs", "archive",
                      "sport=football", "country=iceland",
                      "sex=male", "fit_date=2026-04-30")
  fs::dir_create(fit_dir)
  fs::file_create(fs::path(fit_dir, "beliefs.parquet"))

  expect_false(needs_refit(static, "male", root = root))
})

test_that("needs_refit() is TRUE when a new game was played after last fit", {
  root <- withr::local_tempdir()
  static <- list(sport = "football", country = "iceland")

  results_dir <- fs::path(root, "facts", "results",
                          "sport=football", "country=iceland",
                          "sex=male", "season=2026")
  fs::dir_create(results_dir)
  arrow::write_parquet(
    tibble::tibble(
      home_team = c("A", "C"), away_team = c("B", "D"),
      match_date = as.Date(c("2026-04-29", "2026-05-02")),
      home_score = c(1L, 2L), away_score = c(0L, 1L)
    ),
    fs::path(results_dir, "part-0.parquet")
  )

  fit_dir <- fs::path(root, "beliefs", "archive",
                      "sport=football", "country=iceland",
                      "sex=male", "fit_date=2026-04-30")
  fs::dir_create(fit_dir)
  fs::file_create(fs::path(fit_dir, "beliefs.parquet"))

  expect_true(needs_refit(static, "male", root = root))
})

test_that("has_upcoming_games() filters by horizon", {
  root <- withr::local_tempdir()
  static <- list(sport = "football", country = "iceland")

  sched_dir <- fs::path(root, "facts", "schedules",
                        "sport=football", "country=iceland",
                        "sex=male", "season=2026")
  fs::dir_create(sched_dir)
  today <- Sys.Date()
  arrow::write_parquet(
    tibble::tibble(
      home_team = c("A", "B", "C"), away_team = c("X", "Y", "Z"),
      match_date = c(today + 3L, today + 30L, today - 1L)
    ),
    fs::path(sched_dir, "part-0.parquet")
  )

  expect_true(has_upcoming_games(static, "male", root = root, days = 14))
  expect_false(has_upcoming_games(static, "male", root = root, days = 1))
})

test_that("has_upcoming_games() returns FALSE when schedule dir missing", {
  root <- withr::local_tempdir()
  static <- list(sport = "football", country = "iceland")
  expect_false(has_upcoming_games(static, "male", root = root))
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e 'devtools::test_active_file("tests/testthat/test-pipeline-freshness.R")'`
Expected: 5 failures, all "could not find function".

- [ ] **Step 3: Implement `R/pipeline-freshness.R`**

Create `R/pipeline-freshness.R`:

```r
#' Decide whether a (league, sex) pair needs refitting.
#'
#' Returns `TRUE` when there is at least one completed match with
#' `match_date` strictly later than the most recent `fit_date` partition
#' under `data/beliefs/archive/`. Returns `TRUE` when no fit exists yet.
#' Returns `FALSE` when no results exist (cannot fit on empty data).
#'
#' @param static Per-league static slice with `$sport` and `$country`.
#' @param sex `"male"` or `"female"`.
#' @param root Filesystem root (defaults to `here::here("data")`).
#' @return Logical scalar.
#' @export
needs_refit <- function(static, sex, root = here::here("data")) {
  results <- read_table(
    "results",
    root = root,
    filter = list(sport = static$sport, country = static$country, sex = sex)
  )
  if (nrow(results) == 0L) return(FALSE)

  completed <- dplyr::filter(
    results, !is.na(.data$home_score), !is.na(.data$away_score)
  )
  if (nrow(completed) == 0L) return(FALSE)
  latest_match <- max(completed$match_date)

  archive_dir <- fs::path(
    root, "beliefs", "archive",
    paste0("sport=", static$sport),
    paste0("country=", static$country),
    paste0("sex=", sex)
  )
  if (!fs::dir_exists(archive_dir)) return(TRUE)

  fit_dirs <- fs::dir_ls(archive_dir, type = "directory")
  if (length(fit_dirs) == 0L) return(TRUE)

  fit_dates <- as.Date(stringr::str_remove(fs::path_file(fit_dirs), "^fit_date="))
  last_fit <- max(fit_dates)

  latest_match > last_fit
}

#' Are there any matches scheduled in the next `days` days?
#'
#' Reads `data/facts/schedules/` for the (sport, country, sex) partition.
#' Returns `FALSE` when the schedule directory is missing or empty.
#'
#' @param static Per-league static slice with `$sport` and `$country`.
#' @param sex `"male"` or `"female"`.
#' @param root Filesystem root.
#' @param days Horizon in days (default 14).
#' @return Logical scalar.
#' @export
has_upcoming_games <- function(static, sex,
                               root = here::here("data"),
                               days = 14L) {
  sched_dir <- fs::path(
    root, "facts", "schedules",
    paste0("sport=", static$sport),
    paste0("country=", static$country),
    paste0("sex=", sex)
  )
  if (!fs::dir_exists(sched_dir)) return(FALSE)

  schedules <- read_table(
    "schedules",
    root = root,
    filter = list(sport = static$sport, country = static$country, sex = sex)
  )
  if (nrow(schedules) == 0L) return(FALSE)

  today <- Sys.Date()
  any(
    schedules$match_date >= today &
      schedules$match_date <= today + as.integer(days)
  )
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e 'devtools::document(); devtools::test_active_file("tests/testthat/test-pipeline-freshness.R")'`
Expected: 5 PASS.

- [ ] **Step 5: Run the full test suite to confirm no regressions**

Run: `Rscript -e 'devtools::test()'`
Expected: existing 511+ assertions pass; new 5 added.

- [ ] **Step 6: Commit**

```bash
git add R/pipeline-freshness.R tests/testthat/test-pipeline-freshness.R NAMESPACE man/
git commit -m "feat(pipeline): add freshness predicates needs_refit + has_upcoming_games

Replaces the always-cue mechanic in _targets.R with explicit, unit-testable
rules. Used by the upcoming scripts/0N_*.R entry points to decide whether
to run their layer or exit 0."
```

---

## Task 2: Add shared CLI parsing helper

**Files:**
- Create: `scripts/_lib.R`

Each entry script needs the same `--league`, `--sex`, `--force` flags. Factor the parser out so changes are local.

- [ ] **Step 1: Create `scripts/_lib.R`**

```r
# scripts/_lib.R -- Shared helpers for scripts/0N_*.R entry points.
# Sourced by each script via `source(here::here("scripts", "_lib.R"))`.

#' Parse the standard pipeline-script CLI: `--league`, `--sex`, `--force`.
#'
#' @return A list with `$league` (character or NULL), `$sex` (character or
#'   NULL), `$force` (logical).
parse_pipeline_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  get_flag <- function(name, default = NULL) {
    i <- which(args == paste0("--", name))
    if (length(i) == 0L) default else args[[i + 1L]]
  }
  has_flag <- function(name) paste0("--", name) %in% args

  list(
    league = get_flag("league"),
    sex    = get_flag("sex"),
    force  = has_flag("force")
  )
}

#' Resolve the (league, sex) pairs to operate on, respecting --league/--sex
#' filters. Reads active leagues from config/leagues.yml.
#'
#' @param opts Output of `parse_pipeline_args()`.
#' @param require_lengjan If TRUE, restricts to leagues that publish on Lengjan.
#' @return Tibble with one row per (key, sex) pair, plus the static slice.
resolve_targets <- function(opts, require_lengjan = FALSE) {
  leagues <- load_leagues()
  active <- filter_leagues(leagues, active_only = TRUE)
  if (require_lengjan) {
    active <- filter_leagues(active, has_lengjan = TRUE)
  }

  if (!is.null(opts$league)) {
    if (!opts$league %in% names(active)) {
      stop("--league '", opts$league, "' not active. Active: ",
           paste(names(active), collapse = ", "))
    }
    active <- active[opts$league]
  }

  rows <- list()
  for (key in names(active)) {
    league_def <- active[[key]]
    sexes <- if (is.null(opts$sex) || opts$sex == "all") {
      league_def$sexes
    } else {
      if (!opts$sex %in% league_def$sexes) {
        next
      }
      opts$sex
    }
    for (sx in sexes) {
      rows[[length(rows) + 1L]] <- tibble::tibble(
        key = key,
        sex = sx,
        sport = league_def$sport,
        country = league_def$country
      )
    }
  }
  if (length(rows) == 0L) {
    return(tibble::tibble(key = character(), sex = character(),
                          sport = character(), country = character()))
  }
  dplyr::bind_rows(rows)
}
```

- [ ] **Step 2: Smoke-test the parser**

Run: `Rscript -e 'source("scripts/_lib.R"); print(parse_pipeline_args(c("--league", "football_iceland", "--force")))'`
Expected: list with `$league = "football_iceland"`, `$sex = NULL`, `$force = TRUE`.

- [ ] **Step 3: Commit**

```bash
git add scripts/_lib.R
git commit -m "feat(scripts): add _lib.R with pipeline CLI parser

Shared by upcoming scripts/0N_*.R entry points so --league/--sex/--force
flags behave identically across the chain."
```

---

## Task 3: Create `scripts/00_active_competitions.R`

**Files:**
- Create: `scripts/00_active_competitions.R`

Replaces the `active_competitions` static target. Run as a precondition before ingest and odds.

- [ ] **Step 1: Write the script**

```r
#!/usr/bin/env Rscript
# scripts/00_active_competitions.R --
# Generate config/active_competitions.json from leagues.yml + schedules.
#
# Was the `active_competitions` target in _targets.R. Run before ingest
# and odds scripts.

Sys.setlocale("LC_ALL", "en_US.UTF-8")
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))

leagues <- load_leagues()
out_path <- generate_active_competitions(leagues, lookahead_days = 14L)
cli::cli_alert_success("Wrote {.file {out_path}}")
```

- [ ] **Step 2: Smoke-test locally**

Run: `Rscript scripts/00_active_competitions.R`
Expected: writes `config/active_competitions.json`, prints success line.

- [ ] **Step 3: Commit**

```bash
git add scripts/00_active_competitions.R
git commit -m "feat(scripts): add 00_active_competitions.R

Replaces the active_competitions static target. Run as a precondition
before scripts/01_ingest_results.R and scripts/02_scrape_odds.R."
```

---

## Task 4: Create `scripts/01_ingest_results.R`

**Files:**
- Create: `scripts/01_ingest_results.R`

Loops over active leagues, calls `ingest_one_league()`. No freshness guard — federation scrape is cheap and has its own internal idempotency via `upsert_table()`.

- [ ] **Step 1: Write the script**

```r
#!/usr/bin/env Rscript
# scripts/01_ingest_results.R --
# Scrape federation results + schedules for active leagues.
#
# Writes data/facts/results/ and data/facts/schedules/ via upsert_table()
# (idempotent merge -- safe to re-run).
#
# Usage:
#   Rscript scripts/01_ingest_results.R                    # all active leagues
#   Rscript scripts/01_ingest_results.R --league football_iceland

Sys.setlocale("LC_ALL", "en_US.UTF-8")
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))
source(here::here("scripts", "_lib.R"))

opts <- parse_pipeline_args()
leagues <- load_leagues()
active <- filter_leagues(leagues, active_only = TRUE)
if (!is.null(opts$league)) {
  if (!opts$league %in% names(active)) {
    stop("--league '", opts$league, "' is not active.")
  }
  active <- active[opts$league]
}

active_path <- here::here("config", "active_competitions.json")
if (!fs::file_exists(active_path)) {
  stop("config/active_competitions.json missing. ",
       "Run scripts/00_active_competitions.R first.")
}

cli::cli_h1("Ingest results + schedules ({length(active)} leagues)")
for (key in names(active)) {
  static <- active[[key]][c(
    "sport", "country", "sexes", "active", "stan_model", "data_source"
  )]
  cli::cli_h2("{key}")
  ingest_one_league(static, key, active_path)
}
cli::cli_alert_success("Ingest complete")
```

- [ ] **Step 2: Smoke-test locally**

Run: `Rscript scripts/01_ingest_results.R --league football_iceland`
Expected: scrape runs, writes parquets under `data/facts/results/sport=football/...`, prints success.

- [ ] **Step 3: Commit**

```bash
git add scripts/01_ingest_results.R
git commit -m "feat(scripts): add 01_ingest_results.R

Direct replacement for run.R --step data. Calls ingest_one_league() per
active league. No freshness guard -- upsert_table() handles idempotency."
```

---

## Task 5: Create `scripts/02_scrape_odds.R` with skip-if-no-upcoming-games

**Files:**
- Create: `scripts/02_scrape_odds.R`

First script with a freshness guard. Uses `has_upcoming_games()` per (league × sex) and skips Lengjan-less leagues entirely.

- [ ] **Step 1: Write the script**

```r
#!/usr/bin/env Rscript
# scripts/02_scrape_odds.R --
# Scrape Lengjan odds for active leagues with upcoming games.
#
# Writes data/facts/odds/sport=*/country=*/scraped_date=YYYY-MM-DD/.
# Skips leagues with no game in the next 14 days (rule: "don't scrape
# odds if there are no upcoming games").
#
# Usage:
#   Rscript scripts/02_scrape_odds.R                    # all active leagues
#   Rscript scripts/02_scrape_odds.R --league football_iceland
#   Rscript scripts/02_scrape_odds.R --force            # bypass guard

Sys.setlocale("LC_ALL", "en_US.UTF-8")
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))
source(here::here("scripts", "_lib.R"))

opts <- parse_pipeline_args()
leagues <- load_leagues()
active <- filter_leagues(leagues, active_only = TRUE, has_lengjan = TRUE)
if (!is.null(opts$league)) {
  if (!opts$league %in% names(active)) {
    cli::cli_alert_warning(
      "--league '{opts$league}' has no Lengjan config or is not active; nothing to do."
    )
    quit(save = "no", status = 0L)
  }
  active <- active[opts$league]
}

active_path <- here::here("config", "active_competitions.json")
if (!fs::file_exists(active_path)) {
  stop("config/active_competitions.json missing. ",
       "Run scripts/00_active_competitions.R first.")
}

cli::cli_h1("Scrape odds ({length(active)} Lengjan-eligible leagues)")
for (key in names(active)) {
  league_def <- active[[key]]
  static <- league_def[c(
    "sport", "country", "sexes", "active", "stan_model", "data_source"
  )]
  lengjan <- league_def$lengjan

  any_upcoming <- FALSE
  for (sx in league_def$sexes) {
    if (has_upcoming_games(static, sx)) {
      any_upcoming <- TRUE
      break
    }
  }

  if (!any_upcoming && !opts$force) {
    cli::cli_alert_info(
      "Skipping {key}: no games in the next 14 days (use --force to override)."
    )
    next
  }

  cli::cli_h2("{key}")
  ingest_one_lengjan(static, lengjan, key, active_path)
}
cli::cli_alert_success("Odds scrape complete")
```

- [ ] **Step 2: Smoke-test locally**

Run: `Rscript scripts/02_scrape_odds.R --league football_iceland`
Expected: either scrapes Lengjan and writes today's parquet partition, or prints "Skipping football_iceland: no games in the next 14 days" and exits 0.

- [ ] **Step 3: Commit**

```bash
git add scripts/02_scrape_odds.R
git commit -m "feat(scripts): add 02_scrape_odds.R with upcoming-games guard

Calls ingest_one_lengjan() per league that has at least one upcoming
game in the next 14 days. Skips quietly otherwise (rule: don't scrape
odds if there are no upcoming games)."
```

---

## Task 6: Create `scripts/03_fit.R` with skip-if-no-new-games

**Files:**
- Create: `scripts/03_fit.R`

The marquee script: 30-90min Stan fits get short-circuited when results haven't moved.

- [ ] **Step 1: Write the script**

```r
#!/usr/bin/env Rscript
# scripts/03_fit.R --
# Fit Stan posteriors for (league x sex) pairs that have new completed
# games since the last fit.
#
# Writes data/beliefs/latest/ (overwrite) and data/beliefs/archive/
# (accretive per fit_date).
#
# Usage:
#   Rscript scripts/03_fit.R                                    # all needing refit
#   Rscript scripts/03_fit.R --league football_iceland --sex male
#   Rscript scripts/03_fit.R --force                            # refit everything

Sys.setlocale("LC_ALL", "en_US.UTF-8")
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))
source(here::here("scripts", "_lib.R"))

opts <- parse_pipeline_args()
targets <- resolve_targets(opts)

if (nrow(targets) == 0L) {
  cli::cli_alert_warning("No (league, sex) pairs match the filters; nothing to do.")
  quit(save = "no", status = 0L)
}

cli::cli_h1("Fit Stan models ({nrow(targets)} candidate (league, sex) pairs)")
fitted <- 0L
skipped <- 0L
for (i in seq_len(nrow(targets))) {
  row <- targets[i, ]
  static <- list(
    sport = row$sport, country = row$country,
    sexes = row$sex, active = TRUE,
    stan_model = load_leagues()[[row$key]]$stan_model,
    data_source = load_leagues()[[row$key]]$data_source
  )

  if (!opts$force && !needs_refit(static, row$sex)) {
    cli::cli_alert_info("Skipping {row$key} ({row$sex}): no new games since last fit.")
    skipped <- skipped + 1L
    next
  }

  cli::cli_h2("{row$key} ({row$sex})")
  fit_one(static, row$sex)
  fitted <- fitted + 1L
}
cli::cli_alert_success("Fit complete: {fitted} fitted, {skipped} skipped")
```

- [ ] **Step 2: Smoke-test locally (dry — freshness guard should skip)**

Run: `Rscript scripts/03_fit.R --league football_iceland --sex male`
Expected: prints "Skipping football_iceland (male): no new games since last fit." and exits 0 (assuming a fit already exists from yesterday).

- [ ] **Step 3: Commit**

```bash
git add scripts/03_fit.R
git commit -m "feat(scripts): add 03_fit.R with needs_refit guard

Replaces run.R --step fit. Skips (league, sex) pairs whose last fit_date
is at least the latest completed match_date. --force bypasses for ad-hoc
reruns. This is the rule 'don't update Stan models if there are no new
games', enforced explicitly."
```

---

## Task 7: Create `scripts/04_decide.R`

**Files:**
- Create: `scripts/04_decide.R`

Decide is cheap (seconds). Skip when there's no current odds partition for today (decide depends on fresh odds to be useful).

- [ ] **Step 1: Write the script**

```r
#!/usr/bin/env Rscript
# scripts/04_decide.R --
# Run decide layer (Kelly + portfolio + calibration) for active
# (league, sex) pairs. Reads beliefs_latest + today's odds, writes
# data/decisions/candidates/ and data/decisions/recommendations/.
#
# Usage:
#   Rscript scripts/04_decide.R
#   Rscript scripts/04_decide.R --league football_iceland --sex female

Sys.setlocale("LC_ALL", "en_US.UTF-8")
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))
source(here::here("scripts", "_lib.R"))

opts <- parse_pipeline_args()
targets <- resolve_targets(opts)

if (nrow(targets) == 0L) {
  cli::cli_alert_warning("No (league, sex) pairs match the filters; nothing to do.")
  quit(save = "no", status = 0L)
}

bankroll <- load_bankroll(here::here("config", "bankroll.yml"))
leagues <- load_leagues()

cli::cli_h1("Decide ({nrow(targets)} (league, sex) pairs)")
for (i in seq_len(nrow(targets))) {
  row <- targets[i, ]
  league_def <- leagues[[row$key]]
  static  <- league_def[c(
    "sport", "country", "sexes", "active", "stan_model", "data_source"
  )]
  lengjan <- league_def$lengjan
  betting <- league_def$betting

  cli::cli_h2("{row$key} ({row$sex})")
  decide_one(static, lengjan, betting, row$sex, bankroll)
}
cli::cli_alert_success("Decide complete")
```

- [ ] **Step 2: Smoke-test locally**

Run: `Rscript scripts/04_decide.R --league football_iceland --sex male`
Expected: writes candidates + recommendations parquets; prints success.

- [ ] **Step 3: Commit**

```bash
git add scripts/04_decide.R
git commit -m "feat(scripts): add 04_decide.R

Direct replacement for run.R --step decide. Calls decide_one() per
active (league, sex) pair. No freshness guard -- decide is seconds,
cheap to run on every odds scrape."
```

---

## Task 8: Create `scripts/05_publish.R`

**Files:**
- Create: `scripts/05_publish.R`

- [ ] **Step 1: Write the script**

```r
#!/usr/bin/env Rscript
# scripts/05_publish.R --
# Generate publish JSONs (data/publish/<sport>/iceland/{karla,kvenna}/)
# for active (league, sex) pairs.
#
# Usage:
#   Rscript scripts/05_publish.R
#   Rscript scripts/05_publish.R --league football_iceland

Sys.setlocale("LC_ALL", "en_US.UTF-8")
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))
source(here::here("scripts", "_lib.R"))

opts <- parse_pipeline_args()
targets <- resolve_targets(opts)

if (nrow(targets) == 0L) {
  cli::cli_alert_warning("No (league, sex) pairs match the filters; nothing to do.")
  quit(save = "no", status = 0L)
}

leagues <- load_leagues()

cli::cli_h1("Publish ({nrow(targets)} (league, sex) pairs)")
for (i in seq_len(nrow(targets))) {
  row <- targets[i, ]
  league_def <- leagues[[row$key]]
  static  <- league_def[c(
    "sport", "country", "sexes", "active", "stan_model", "data_source"
  )]
  betting <- league_def$betting

  cli::cli_h2("{row$key} ({row$sex})")
  publish_one(static, betting, row$key, row$sex)
}
cli::cli_alert_success("Publish complete")
```

- [ ] **Step 2: Smoke-test locally**

Run: `Rscript scripts/05_publish.R --league football_iceland --sex male`
Expected: writes JSONs under `data/publish/football/iceland/karla/`; prints success.

- [ ] **Step 3: Commit**

```bash
git add scripts/05_publish.R
git commit -m "feat(scripts): add 05_publish.R

Direct replacement for run.R --step publish."
```

---

## Task 9: Update `scrape-results.yml` to call the new script

**Files:**
- Modify: `.github/workflows/scrape-results.yml`

Drops the targets cache step, replaces `run.R --step data` with the script call. Adds `outputs.changed` so `fit.yml` can chain via `workflow_run`.

- [ ] **Step 1: Read the current file**

Run: `cat .github/workflows/scrape-results.yml`
(Already shown — see plan setup.)

- [ ] **Step 2: Replace contents**

Write the file:

```yaml
name: Scrape Federation Results + Schedules

on:
  schedule:
    - cron: '0 6 * * *'   # 1x daily UTC
  workflow_dispatch: {}

permissions:
  contents: write

jobs:
  scrape:
    runs-on: ubuntu-latest
    timeout-minutes: 60

    env:
      GITHUB_PAT: ${{ secrets.GITHUB_TOKEN }}

    steps:
      - uses: actions/checkout@v4

      - uses: r-lib/actions/setup-r@v2
        with:
          r-version: 'release'
          use-public-rspm: true

      - uses: browser-actions/setup-chrome@v1
        id: setup-chrome
        with:
          chrome-version: stable

      - uses: r-lib/actions/setup-r-dependencies@v2
        with:
          dependencies: '"hard"'
          extra-packages: |
            any::devtools
            any::chromote

      - name: Refresh active_competitions
        run: Rscript scripts/00_active_competitions.R

      - name: Scrape results + schedules
        env:
          CHROMOTE_CHROME: ${{ steps.setup-chrome.outputs.chrome-path }}
        run: Rscript scripts/01_ingest_results.R

      - name: Commit if data changed
        run: |
          git config user.name  "github-actions[bot]"
          git config user.email "github-actions[bot]@users.noreply.github.com"
          git add data/facts/results/ data/facts/schedules/ config/active_competitions.json
          if git diff --cached --quiet; then
            echo "No data changed"
            exit 0
          fi
          git commit -m "data: federation scrape $(date -u +%Y-%m-%dT%H:%MZ)"
          git pull --rebase origin main
          git push
```

- [ ] **Step 3: Commit**

```bash
git add .github/workflows/scrape-results.yml
git commit -m "ci(scrape-results): drop targets, call scripts/01_ingest_results.R

Removes the targets cache step (no longer needed). Adds an explicit
00_active_competitions step so the JSON precondition is fresh. Restricts
git add to data/facts/results/, data/facts/schedules/, and the active
competitions JSON -- no more 'autostash unstaged metadata' workaround."
```

---

## Task 10: Update `scrape-odds.yml` to call the new script

**Files:**
- Modify: `.github/workflows/scrape-odds.yml`

- [ ] **Step 1: Read the current file**

Run: `cat .github/workflows/scrape-odds.yml`

- [ ] **Step 2: Replace contents**

Write the file:

```yaml
name: Scrape Lengjan Odds

on:
  schedule:
    - cron: '0 10,16,21 * * *'   # 3x daily UTC
  workflow_dispatch: {}

permissions:
  contents: write

jobs:
  scrape:
    runs-on: ubuntu-latest
    timeout-minutes: 30

    env:
      GITHUB_PAT: ${{ secrets.GITHUB_TOKEN }}

    steps:
      - uses: actions/checkout@v4

      - uses: r-lib/actions/setup-r@v2
        with:
          r-version: 'release'
          use-public-rspm: true

      - uses: browser-actions/setup-chrome@v1
        id: setup-chrome
        with:
          chrome-version: stable

      - uses: r-lib/actions/setup-r-dependencies@v2
        with:
          dependencies: '"hard"'
          extra-packages: |
            any::devtools
            any::chromote

      - name: Refresh active_competitions
        run: Rscript scripts/00_active_competitions.R

      - name: Scrape Lengjan odds
        env:
          CHROMOTE_CHROME: ${{ steps.setup-chrome.outputs.chrome-path }}
        run: Rscript scripts/02_scrape_odds.R

      - name: Commit if data changed
        run: |
          git config user.name  "github-actions[bot]"
          git config user.email "github-actions[bot]@users.noreply.github.com"
          git add data/facts/odds/ config/active_competitions.json
          if git diff --cached --quiet; then
            echo "No data changed"
            exit 0
          fi
          git commit -m "data: lengjan odds $(date -u +%Y-%m-%dT%H:%MZ)"
          git pull --rebase origin main
          git push
```

- [ ] **Step 3: Commit**

```bash
git add .github/workflows/scrape-odds.yml
git commit -m "ci(scrape-odds): drop targets, call scripts/02_scrape_odds.R

Same shape as scrape-results: precondition step writes
active_competitions.json, then a single Rscript call. No targets cache."
```

---

## Task 11: Replace `fit-and-publish.yml` with `fit.yml` and `decide-publish.yml`

**Files:**
- Delete: `.github/workflows/fit-and-publish.yml`
- Create: `.github/workflows/fit.yml`
- Create: `.github/workflows/decide-publish.yml`

The split fixes the original race (each workflow writes a disjoint set of paths) and gives 3× freshness on published JSONs.

- [ ] **Step 1: Create `.github/workflows/fit.yml`**

```yaml
name: Fit Stan Models

on:
  workflow_run:
    workflows: ["Scrape Federation Results + Schedules"]
    types: [completed]
    branches: [main]
  workflow_dispatch: {}

permissions:
  contents: write

concurrency:
  group: fit
  cancel-in-progress: false

jobs:
  fit:
    if: ${{ github.event.workflow_run.conclusion == 'success' || github.event_name == 'workflow_dispatch' }}
    runs-on: ubuntu-latest
    timeout-minutes: 180

    env:
      GITHUB_PAT: ${{ secrets.GITHUB_TOKEN }}
      CMDSTAN_VERSION: '2.38.0'

    steps:
      - uses: actions/checkout@v4

      - uses: r-lib/actions/setup-r@v2
        with:
          r-version: 'release'
          use-public-rspm: true

      - uses: r-lib/actions/setup-r-dependencies@v2
        with:
          extra-repositories: 'https://stan-dev.r-universe.dev'
          extra-packages: |
            any::devtools
            any::cmdstanr

      - name: Cache CmdStan installation
        id: cache-cmdstan
        uses: actions/cache@v4
        with:
          path: ~/.cmdstan
          key: cmdstan-${{ env.CMDSTAN_VERSION }}-ubuntu-latest

      - name: Install CmdStan if not cached
        if: steps.cache-cmdstan.outputs.cache-hit != 'true'
        run: Rscript -e 'cmdstanr::install_cmdstan(version = Sys.getenv("CMDSTAN_VERSION"), cores = 2)'

      - name: Fit (skips when no new games)
        run: Rscript scripts/03_fit.R

      - name: Commit if beliefs changed
        run: |
          git config user.name  "github-actions[bot]"
          git config user.email "github-actions[bot]@users.noreply.github.com"
          git add data/beliefs/latest/ data/beliefs/archive/
          if git diff --cached --quiet; then
            echo "No new fits"
            exit 0
          fi
          git commit -m "data: stan fit $(date -u +%Y-%m-%dT%H:%MZ)"
          git pull --rebase origin main
          git push
```

- [ ] **Step 2: Create `.github/workflows/decide-publish.yml`**

```yaml
name: Decide + Publish

on:
  workflow_run:
    workflows: ["Fit Stan Models", "Scrape Lengjan Odds"]
    types: [completed]
    branches: [main]
  workflow_dispatch: {}

permissions:
  contents: write

concurrency:
  group: decide-publish
  cancel-in-progress: false

jobs:
  decide-publish:
    if: ${{ github.event.workflow_run.conclusion == 'success' || github.event_name == 'workflow_dispatch' }}
    runs-on: ubuntu-latest
    timeout-minutes: 30

    env:
      GITHUB_PAT: ${{ secrets.GITHUB_TOKEN }}

    steps:
      - uses: actions/checkout@v4

      - uses: r-lib/actions/setup-r@v2
        with:
          r-version: 'release'
          use-public-rspm: true

      - uses: r-lib/actions/setup-r-dependencies@v2
        with:
          extra-packages: |
            any::devtools

      - name: Decide (Kelly + portfolio + calibration)
        run: Rscript scripts/04_decide.R

      - name: Publish JSONs
        run: Rscript scripts/05_publish.R

      - name: Commit if outputs changed
        run: |
          git config user.name  "github-actions[bot]"
          git config user.email "github-actions[bot]@users.noreply.github.com"
          git add data/decisions/candidates/ data/decisions/recommendations/ data/publish/
          if git diff --cached --quiet; then
            echo "No decisions or publish JSONs changed"
            exit 0
          fi
          git commit -m "data: decide + publish $(date -u +%Y-%m-%dT%H:%MZ)"
          git pull --rebase origin main
          git push
```

- [ ] **Step 3: Delete the old workflow**

Run: `rm .github/workflows/fit-and-publish.yml`

- [ ] **Step 4: Commit**

```bash
git add .github/workflows/fit.yml .github/workflows/decide-publish.yml
git rm .github/workflows/fit-and-publish.yml
git commit -m "ci: split fit-and-publish into fit + decide-publish

- fit.yml triggers on workflow_run from scrape-results, runs daily Stan
  fits with the needs_refit guard.
- decide-publish.yml triggers on workflow_run from BOTH fit and
  scrape-odds, so published JSONs refresh on every odds scrape (3x daily)
  rather than once.
- Each workflow writes a disjoint set of paths, so the cross-workflow
  git race that hit on 2026-04-30 cannot recur.
- The needs_refit guard means scheduled re-runs short-circuit when
  results haven't moved -- no more 1-2h pointless rebuilds."
```

---

## Task 12: Delete `_targets.R` and drop the dependency

**Files:**
- Delete: `_targets.R`, `tests/testthat/test-targets-dag.R`, `tests/testthat/test-targets-slice-isolation.R`
- Modify: `DESCRIPTION`, `.gitignore`

- [ ] **Step 1: Delete the targets-specific files**

Run:
```bash
rm _targets.R tests/testthat/test-targets-dag.R tests/testthat/test-targets-slice-isolation.R
rm -rf _targets/
```

- [ ] **Step 2: Remove `targets` from `DESCRIPTION`**

Edit `DESCRIPTION`:

```
# Find:
    targets,
# Delete that line.
```

- [ ] **Step 3: Remove `_targets/` from `.gitignore`**

Edit `.gitignore`:

```
# Find any `_targets` or `_targets/` line and delete it.
```

- [ ] **Step 4: Run the test suite — must still pass**

Run: `Rscript -e 'devtools::test()'`
Expected: 5 fewer test files (the two targets ones gone), but no failures elsewhere.

- [ ] **Step 5: Commit**

```bash
git add -A
git commit -m "chore: remove {targets} dependency

- Delete _targets.R and the two targets-specific tests.
- Drop targets from DESCRIPTION Imports.
- Drop _targets/ from .gitignore.

The DAG is replaced by scripts/0N_*.R; freshness rules previously
encoded as cue = 'always' are now explicit predicates in
R/pipeline-freshness.R."
```

---

## Task 13: Replace `run.R` and delete deprecated scripts

**Files:**
- Delete: `run.R`, `scripts/fit_all.R`, `scripts/decide_all.R`, `scripts/publish_all.R`, `scripts/backfill_ingest.R`

`run.R` was a thin wrapper around `targets::tar_make()`. With targets gone, calling `Rscript scripts/0N_*.R` is the new daily driver; `run.R` adds nothing.

- [ ] **Step 1: Search for `run.R` references in the codebase**

Run: `grep -rn "run\.R\|Rscript run" --include='*.R' --include='*.md' --include='*.yml' .`
Expected: matches in CLAUDE.md, skills, docs, possibly tests. Each one needs to be updated in Task 14 to point at the new scripts.

- [ ] **Step 2: Delete the obsolete scripts**

```bash
rm run.R scripts/fit_all.R scripts/decide_all.R scripts/publish_all.R scripts/backfill_ingest.R
```

- [ ] **Step 3: Verify the test suite still passes**

Run: `Rscript -e 'devtools::test()'`
Expected: pass.

- [ ] **Step 4: Commit**

```bash
git add -A
git commit -m "chore: remove run.R and deprecated scripts

run.R was a wrapper around targets::tar_make(); with targets gone,
scripts/0N_*.R are the daily driver. The fit_all/decide_all/publish_all/
backfill_ingest scripts were already marked deprecated in CLAUDE.md and
are now obsolete."
```

---

## Task 14: Update CLAUDE.md, skills, and verify model-invocability

**Files:**
- Modify: `CLAUDE.md`, `.claude/skills/sports-update/SKILL.md`, `.claude/skills/bet/SKILL.md`, `.claude/skills/place-bets/SKILL.md`, `.claude/skills/add-league/SKILL.md`

Documentation must match the new layout. The skills test (`tests/testthat/test-skill-conventions.R`) guards against drift; check it allows `scripts/0N_*.R` and disallows the old `run.R --step` pattern.

- [ ] **Step 1: Update `CLAUDE.md` directory tree**

Open `CLAUDE.md`. In the `Directory structure` section, replace the lines describing `_targets.R` and `run.R` with:

```
sports/
├── scripts/                         # Pipeline entry points (one per layer)
│   ├── _lib.R                       # Shared CLI parser + target resolver
│   ├── 00_active_competitions.R
│   ├── 01_ingest_results.R
│   ├── 02_scrape_odds.R             # Skips when no upcoming games
│   ├── 03_fit.R                     # Skips when results haven't moved
│   ├── 04_decide.R
│   └── 05_publish.R
```

Remove the `_targets.R` and `run.R` lines.

- [ ] **Step 2: Update `CLAUDE.md` Quick reference**

Replace the existing "Run the pipeline (daily driver)" block with:

```bash
# Development
Rscript -e 'devtools::load_all()'
Rscript -e 'devtools::test()'

# Run the pipeline (daily driver)
Rscript scripts/00_active_competitions.R                   # generate JSON
Rscript scripts/01_ingest_results.R                        # all active leagues
Rscript scripts/01_ingest_results.R --league football_iceland
Rscript scripts/02_scrape_odds.R                           # skips no-upcoming
Rscript scripts/03_fit.R                                   # skips no-new-games
Rscript scripts/03_fit.R --force                           # bypass guard
Rscript scripts/04_decide.R
Rscript scripts/05_publish.R

# Local placer (NEVER on CI)
Rscript scripts/place_bets.R --dry-run
Rscript scripts/place_bets.R --live
Rscript scripts/preview_bets.R                            # no browser

# Rebuild sports.duckdb after fresh Parquet writes
Rscript -e 'sports::rebuild_duckdb()'
```

Delete the `# Targets directly (advanced)` block — there's nothing to invoke targets-wise.

- [ ] **Step 3: Update CLAUDE.md "Deprecated runners" section**

Remove the entire `## Deprecated runners` block (no more deprecated runners — they were deleted).

- [ ] **Step 4: Update CLAUDE.md Skills section**

The existing `## Skills` section mentions `Rscript run.R --step ...`. Replace with:

```markdown
## Skills

The four model-invocable skills under `.claude/skills/` (`/bet`,
`/sports-update`, `/add-league`, `/place-bets`) call `scripts/0N_*.R`
directly. A drift guard in `tests/testthat/test-skill-conventions.R`
fails CI if any skill references the legacy `run.R --step` pattern,
the four-repo layout, or the `--sync` flag.

**Do not add `disable-model-invocation: true` to these skills.** They
are intentionally model-invocable.
```

- [ ] **Step 5: Update each skill in `.claude/skills/`**

For each of `sports-update`, `bet`, `place-bets`, `add-league`:

1. Open `SKILL.md`.
2. Replace any `Rscript run.R --step <step>` invocation with the matching `Rscript scripts/0N_*.R`.
3. Replace `--all --step <step>` with no flag (default is all active leagues).

Mapping:
| Old | New |
|---|---|
| `Rscript run.R --all --step data` | `Rscript scripts/00_active_competitions.R && Rscript scripts/01_ingest_results.R` |
| `Rscript run.R --all --step odds` | `Rscript scripts/00_active_competitions.R && Rscript scripts/02_scrape_odds.R` |
| `Rscript run.R --all --step fit` | `Rscript scripts/03_fit.R` |
| `Rscript run.R --all --step decide` | `Rscript scripts/04_decide.R` |
| `Rscript run.R --all --step publish` | `Rscript scripts/05_publish.R` |
| `Rscript run.R --league X --step Y` | `Rscript scripts/0N_step.R --league X` |

- [ ] **Step 6: Update the skill drift guard**

Open `tests/testthat/test-skill-conventions.R`. Find the forbidden-patterns list and add `run\\.R --step` to it (so future drift toward the old pattern fails CI).

- [ ] **Step 7: Run the test suite**

Run: `Rscript -e 'devtools::test()'`
Expected: pass, including the skill drift guard.

- [ ] **Step 8: Commit**

```bash
git add CLAUDE.md .claude/skills/ tests/testthat/test-skill-conventions.R
git commit -m "docs: update CLAUDE.md and skills for scripts/0N_*.R layout

- CLAUDE.md directory tree, quick reference, conventions section.
- Each model-invocable skill (/bet, /sports-update, /add-league,
  /place-bets) now calls scripts/0N_*.R directly.
- Drift guard adds run.R --step to its forbidden-patterns list."
```

---

## Self-Review

**Spec coverage:**
- Goal 1: Drop `targets` ✓ (Tasks 12, 13)
- Goal 2: Rule "don't refit if no new games" ✓ (Task 1 `needs_refit`, Task 6)
- Goal 3: Rule "don't scrape odds if no upcoming games" ✓ (Task 1 `has_upcoming_games`, Task 5)
- Goal 4: Maintain football_iceland focus ✓ (config-driven via `filter_leagues(active_only = TRUE)`)
- Goal 5: Fix the cross-workflow race ✓ (Task 11 — disjoint paths per workflow)

**Placeholder scan:** None. Every step has the actual code or command.

**Type consistency:**
- `static$sport`, `static$country`, `static$sexes` used consistently across freshness predicates and entry scripts.
- `parse_pipeline_args()` returns `list(league, sex, force)` and every script uses those exact names.
- `resolve_targets(opts)` returns a tibble with `key, sex, sport, country` and every script that uses it accesses those exact columns.

**Risks worth flagging during execution:**
1. The freshness predicates assume `home_score`/`away_score` are NA for unplayed games. Confirm this when writing the test fixtures — if results uses a different "completed" marker, adjust both `needs_refit()` and the test fixtures accordingly.
2. `workflow_run` chained workflows can be silently flaky when the parent's `conclusion` isn't `success`. Watch for the first chained run to confirm the trigger fires.
3. The skill drift guard test must be updated *before* Task 14 step 5 commits, or the skill changes will fail CI mid-task. Step 6 captures this; do not reorder.

---

## Execution Notes

The migration is sequenced so each commit leaves the repo in a working state:
- Tasks 1-8 add the new scripts alongside the existing `_targets.R` (everything still works via either path).
- Tasks 9-11 cut CI over to the scripts (existing `_targets.R` still works locally).
- Tasks 12-13 demolish the old infrastructure.
- Task 14 documents the result.

If a task fails mid-way, the working tree is at most one commit removed from a known-good state.
