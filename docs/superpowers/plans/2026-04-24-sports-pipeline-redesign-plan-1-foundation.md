# Sports Pipeline Redesign — Plan 1: Foundation + Storage + ETL

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Initialise the monorepo, merge the four legacy repos as subtrees preserving history, build the storage layer (Arrow schemas + Parquet I/O + DuckDB views), and migrate existing Icelandic-league data (results, schedules, odds, ledger) to the new Parquet stores with row-count and PnL-total validation.

**Architecture:** Fresh git repo at `/Users/brynjolfurjonsson/sports`, four legacy repos merged under `_legacy/` via `git subtree`. Storage primitives in `R/storage.R` wrap `arrow::write_parquet()` with Arrow-schema validation. DuckDB views regenerated on demand from Parquet. ETL scripts in `scripts/etl/` port legacy CSVs to Parquet, each with a validation test comparing row counts and numeric totals.

**Tech Stack:** R (≥ 4.0), `{arrow}`, `{duckdb}`, `{DBI}`, `{yaml}`, `{jsonvalidate}`, `{testthat}`, `{cli}`, git + git-subtree.

**Scope (Plan 1):** Phase A of the spec — foundation, storage, ETL. Validation gate: the legacy ledger's PnL total must match the new Parquet ledger's PnL total to the cent.

**Out of scope (later plans):**

- Plan 2: Model layer (prepare_data + fit + posteriors for 3 Icelandic leagues)
- Plan 3: Decide + Placer + Publish
- Plan 4: Ingest (scrapers) + Orchestration + CI + metill-platform integration + cutover

---

## File structure created by this plan

```
/Users/brynjolfurjonsson/sports/
├── .git/                          (new)
├── .gitignore                     (new)
├── .Renviron.example              (new)
├── .Rbuildignore                  (new)
├── DESCRIPTION                    (new)
├── NAMESPACE                      (new)
├── README.md                      (new)
├── sports.Rproj                   (new)
├── CLAUDE.md                      (kept from pre-migration workspace)
├── config/
│   ├── leagues.yml                (new — 3 Icelandic leagues)
│   └── leagues.schema.json        (new)
├── R/
│   ├── config.R                   (new — leagues loader + filter)
│   ├── storage-schemas.R          (new — Arrow schemas for 8 tables)
│   ├── storage.R                  (new — read_table/write_table primitives)
│   └── duckdb-views.R             (new — rebuild sports.duckdb from Parquet)
├── tests/
│   ├── testthat.R                 (new)
│   └── testthat/
│       ├── test-config.R
│       ├── test-storage-schemas.R
│       ├── test-storage.R
│       ├── test-duckdb-views.R
│       └── test-etl-validation.R
├── scripts/etl/
│   ├── 01_etl_results.R           (new)
│   ├── 02_etl_schedules.R         (new)
│   ├── 03_etl_odds.R              (new)
│   └── 04_etl_ledger.R            (new)
├── docs/superpowers/
│   ├── specs/                     (already exists)
│   └── plans/                     (this file)
├── data/                          (created by ETL; Parquet files)
│   ├── facts/
│   │   ├── results/
│   │   ├── schedules/
│   │   └── odds/
│   └── decisions/
│       └── ledger/
└── _legacy/                       (subtree-merged)
    ├── sports/
    ├── lengjan-odds/
    ├── livesport-data/
    └── lengjan-bets/
```

Empty subdirectories (e.g. `R/placer/`, `Stan/`, `data/beliefs/`) will be added by later plans.

---

## Task 1: Pre-flight checks and backup

**Files:** none (shell only)

**Purpose:** Before any destructive restructure, verify that all four legacy repos' work is safely on GitHub and that critical local-only state (ledger CSVs, fit.rds for reference) is backed up.

- [ ] **Step 1: Verify all legacy repos are clean and pushed**

Run:

```bash
for d in Sports lengjan-odds livesport-data lengjan-bets; do
  echo "=== $d ==="
  cd /Users/brynjolfurjonsson/sports/$d
  git fetch origin
  git status --short
  echo "unpushed commits:"
  git log --oneline origin/main..HEAD 2>&1 | head
  cd /Users/brynjolfurjonsson/sports
done
```

Expected: empty output under `git status --short` and `unpushed commits:` for each. If anything shows dirty state or unpushed work, commit and push within that repo before proceeding.

- [ ] **Step 2: Back up critical local-only data**

```bash
BACKUP=/Users/brynjolfurjonsson/sports-backup-$(date +%Y%m%d-%H%M%S)
mkdir -p "$BACKUP"

cd /Users/brynjolfurjonsson/sports

# Ledger CSVs — most critical (gitignored in legacy so won't survive re-clone)
rsync -av --include='*/' --include='bets_log.csv' --exclude='*' Sports/ "$BACKUP/Sports/"

# Latest fit.rds for reference comparisons in Plan 2
find Sports -maxdepth 6 -name "fit.rds" -print -exec cp --parents {} "$BACKUP/" \; 2>/dev/null || \
  (cd Sports && find . -name "fit.rds" | while read f; do mkdir -p "$BACKUP/Sports/$(dirname "$f")"; cp "$f" "$BACKUP/Sports/$f"; done)

# Recommendations file (ephemeral but useful as reference)
[ -f Sports/recommendations.csv ] && cp Sports/recommendations.csv "$BACKUP/"

echo "Backup at $BACKUP"
du -sh "$BACKUP"
find "$BACKUP" -name "bets_log.csv" -o -name "fit.rds" | wc -l
```

Expected: positive file count for ledger + fit.rds backups.

- [ ] **Step 3: Record pre-migration state for ETL validation (Task 13)**

```bash
STATE=/tmp/legacy-state.md
cd /Users/brynjolfurjonsson/sports

{
  echo "# Legacy state recorded $(date -u +%FT%TZ)"
  echo
  echo "## Ledger rows per league"
  find Sports -name "bets_log.csv" | while read f; do
    rows=$(($(wc -l < "$f") - 1))
    echo "- $f: $rows rows"
  done
  echo
  echo "## Ledger PnL totals (ISK)"
  Rscript -e '
    files <- list.files("Sports", "bets_log.csv$", recursive = TRUE, full.names = TRUE)
    totals <- numeric(length(files))
    names(totals) <- files
    for (i in seq_along(files)) {
      df <- readr::read_csv(files[i], show_col_types = FALSE)
      totals[i] <- sum(df$pnl, na.rm = TRUE)
      cat(sprintf("- %s: pnl=%.2f (n=%d)\n", files[i], totals[i], nrow(df)))
    }
    cat(sprintf("\n**TOTAL PnL**: %.2f ISK\n", sum(totals)))
    cat(sprintf("**TOTAL rows**: %d\n", sum(vapply(files, function(f) nrow(readr::read_csv(f, show_col_types = FALSE)), integer(1)))))
  '
} > "$STATE"

cat "$STATE"
```

Expected: a markdown file showing per-league row counts and PnL. **Copy the "TOTAL PnL" and "TOTAL rows" numbers** — Task 13 compares against them.

- [ ] **Step 4: Confirm with user before destructive restructure**

Pause and ask:

> "Backup at `$BACKUP`, legacy state recorded at `/tmp/legacy-state.md`. The next step deletes the current `Sports/`, `lengjan-odds/`, `livesport-data/`, `lengjan-bets/` directories from the workspace and replaces them with a fresh git-initialised monorepo. Proceed? (yes/no)"

Do not continue without explicit `yes`.

---

## Task 2: Initialise the monorepo

**Files:**

- Create: `/Users/brynjolfurjonsson/sports/README.md`
- Create: `/Users/brynjolfurjonsson/sports/.gitignore`
- Create: `/Users/brynjolfurjonsson/sports/.Renviron.example`

- [ ] **Step 1: Remove existing sub-repo directories** (destructive; user confirmed in Task 1)

```bash
cd /Users/brynjolfurjonsson/sports
rm -rf Sports lengjan-odds livesport-data lengjan-bets
rm -rf .claude         # old per-workspace config, restored later from legacy if desired
ls -la
```

Expected remaining: `CLAUDE.md`, `docs/` (with specs + plans), plus hidden files.

- [ ] **Step 2: `git init`**

```bash
cd /Users/brynjolfurjonsson/sports
git init
git branch -m main
git config user.name  "Brynjólfur Gauti Jónsson"
git config user.email "bgautijonsson@gmail.com"
```

- [ ] **Step 3: Create `.gitignore`**

Write `/Users/brynjolfurjonsson/sports/.gitignore`:

```gitignore
# R
.Rproj.user
.Rhistory
.RData
.Ruserdata

# Environment
.Renviron

# Derived artefacts (regenerable from Parquet)
sports.duckdb
sports.duckdb.wal

# Testing/research
research/results/
.testthat/

# Editor
.DS_Store
*.swp

# Temporary
*.tmp
_legacy-tmp/

# Stan build artefacts
*.exe
*.hpp
**/*.stan.d
```

- [ ] **Step 4: Create `.Renviron.example`**

Write `/Users/brynjolfurjonsson/sports/.Renviron.example`:

```
# Placer credentials — only required on the laptop running R/placer/.
# Copy to .Renviron and fill in. .Renviron is .gitignored.

LENGJAN_USER=
LENGJAN_PASS=
```

- [ ] **Step 5: Create `README.md`**

Write `/Users/brynjolfurjonsson/sports/README.md`:

````markdown
# sports

Bayesian sports-prediction and automated-betting monorepo for Icelandic football, basketball, and handball.

**Status:** mid-migration. See [`docs/superpowers/specs/2026-04-24-sports-pipeline-redesign-design.md`](docs/superpowers/specs/2026-04-24-sports-pipeline-redesign-design.md) for the end-state design.

## Local-only subsystem

`R/placer/` places bets against Lengjan (games.lotto.is) using credentials in `.Renviron` (see `.Renviron.example`). It is **never** executed on CI — no workflow invokes it and no GitHub Actions secret is configured for it.

## Layout

- `config/leagues.yml` — single source of truth for league metadata
- `R/` — package source (storage, config, model, decide, placer, publish, research)
- `Stan/` — per-league Stan models
- `data/` — Parquet stores (facts, beliefs, decisions, publish), hive-partitioned
- `scripts/etl/` — one-time migration of legacy CSVs to Parquet
- `_legacy/` — subtree-merged histories of the four predecessor repos

## Development

```bash
Rscript -e 'devtools::load_all()'
Rscript -e 'devtools::test()'
```
````

````

- [ ] **Step 6: First commit**

```bash
cd /Users/brynjolfurjonsson/sports
git add README.md .gitignore .Renviron.example CLAUDE.md docs/
git commit -m "Initial commit: monorepo scaffold

Includes the redesign spec under docs/superpowers/specs/ and this
plan under docs/superpowers/plans/.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>"
git log --oneline
```

Expected: single commit, `main` branch.

---

## Task 3: Subtree-merge the four legacy repos

**Files:** creates `_legacy/sports/`, `_legacy/lengjan-odds/`, `_legacy/livesport-data/`, `_legacy/lengjan-bets/`

- [ ] **Step 1: Add remotes**

```bash
cd /Users/brynjolfurjonsson/sports

git remote add sports-code    git@github.com:metill-is/sports.git
git remote add lengjan-odds   git@github.com:metill-is/lengjan-odds.git
git remote add livesport-data git@github.com:metill-is/livesport-data.git
git remote add lengjan-bets   git@github.com:metill-is/lengjan-bets.git

git remote -v
git fetch --all --tags
```

Expected: four remotes, all fetches succeed.

- [ ] **Step 2: Subtree-add each legacy repo** (preserves full history)

```bash
cd /Users/brynjolfurjonsson/sports

git subtree add --prefix=_legacy/sports          sports-code    main
git subtree add --prefix=_legacy/lengjan-odds    lengjan-odds   main
git subtree add --prefix=_legacy/livesport-data  livesport-data main
git subtree add --prefix=_legacy/lengjan-bets    lengjan-bets   main

git log --oneline | head -20
ls _legacy/
```

Expected: `_legacy/` has 4 directories, each with its original tree; `git log` shows 4 merge commits from the subtrees plus the initial commit.

- [ ] **Step 3: Verify history preservation**

```bash
cd /Users/brynjolfurjonsson/sports
git log --follow --oneline _legacy/sports/R/bets/kelly_joint.R | head -5
git log --follow --oneline _legacy/lengjan-bets/R/place_bet.R | head -5
git log --follow --oneline _legacy/lengjan-odds/R/scrape.R | head -5
```

Expected: each command shows ≥3 commits with dates predating today — confirms per-file history is preserved.

- [ ] **Step 4: Restore local-only data (ledgers) from backup**

```bash
cd /Users/brynjolfurjonsson/sports
BACKUP=$(ls -dt /Users/brynjolfurjonsson/sports-backup-* | head -1)
echo "Restoring ledgers from $BACKUP"

# Copy ledger CSVs into _legacy/sports/ per-league history/ dirs
rsync -av --include='*/' --include='bets_log.csv' --exclude='*' "$BACKUP/Sports/" _legacy/sports/

# Verify three Icelandic ledgers are back
ls _legacy/sports/basketball/iceland/history/bets_log.csv \
   _legacy/sports/handball/iceland/history/bets_log.csv \
   _legacy/sports/football/iceland/history/bets_log.csv
```

Expected: three ledger files listed.

- [ ] **Step 5: Commit restored ledgers**

```bash
cd /Users/brynjolfurjonsson/sports
# The legacy repos had bets_log.csv gitignored, so these files are untracked
git add -f _legacy/sports/basketball/iceland/history/bets_log.csv \
           _legacy/sports/handball/iceland/history/bets_log.csv \
           _legacy/sports/football/iceland/history/bets_log.csv
git add -A _legacy/sports/  # catch any other restored ledgers
git status --short | head
git commit -m "Restore local-only ledger CSVs under _legacy/sports/

These bets_log.csv files were gitignored in the Sports repo but
are the production ledger — needed for ETL validation in Task 13
and for the bankroll computation in Plan 3.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>"
```

---

## Task 4: Package scaffolding

**Files:**

- Create: `/Users/brynjolfurjonsson/sports/DESCRIPTION`
- Create: `/Users/brynjolfurjonsson/sports/NAMESPACE`
- Create: `/Users/brynjolfurjonsson/sports/.Rbuildignore`
- Create: `/Users/brynjolfurjonsson/sports/sports.Rproj`
- Create: `/Users/brynjolfurjonsson/sports/tests/testthat.R`
- Create: `/Users/brynjolfurjonsson/sports/R/.gitkeep`
- Create: `/Users/brynjolfurjonsson/sports/tests/testthat/.gitkeep`

**Purpose:** Treat the monorepo as an R package so `devtools::load_all()` works and `testthat` runs from a conventional path.

- [ ] **Step 1: Write `DESCRIPTION`**

```
Package: sports
Title: Bayesian Sports Prediction and Automated Betting
Version: 0.1.0
Authors@R: person("Brynjólfur Gauti", "Jónsson", email = "bgautijonsson@gmail.com", role = c("aut", "cre"))
Description: Consolidated pipeline for ingesting Icelandic sports data,
    fitting Bayesian Stan models, producing Kelly-optimal bets, and
    publishing posterior summaries to metill.is.
License: file LICENSE
Encoding: UTF-8
Depends: R (>= 4.0)
Imports:
    arrow,
    cli,
    DBI,
    dplyr,
    duckdb,
    fs,
    here,
    jsonlite,
    jsonvalidate,
    lubridate,
    purrr,
    readr,
    rlang,
    stringr,
    tibble,
    tidyr,
    yaml
Suggests:
    testthat (>= 3.0.0),
    withr
Config/testthat/edition: 3
```

- [ ] **Step 2: Write `NAMESPACE`**

```
# Generated by roxygen2 — do not edit by hand
exportPattern("^[^.]")
```

- [ ] **Step 3: Write `.Rbuildignore`**

```
^\.github$
^\.vscode$
^docs$
^_legacy$
^scripts$
^Stan$
^data$
^config$
^research$
^sports\.Rproj$
^\.Rproj\.user$
^\.Renviron$
^\.Renviron\.example$
^CLAUDE\.md$
^README\.md$
^sports\.duckdb.*$
^\.claude$
^sports-backup-.*$
```

- [ ] **Step 4: Write `sports.Rproj`**

```
Version: 1.0

RestoreWorkspace: No
SaveWorkspace: No
AlwaysSaveHistory: Default

EnableCodeIndexing: Yes
UseSpacesForTab: Yes
NumSpacesForTab: 2
Encoding: UTF-8

BuildType: Package
PackageUseDevtools: Yes
PackageInstallArgs: --no-multiarch --with-keep.source
PackageRoxygenize: rd,collate,namespace
```

- [ ] **Step 5: Write `tests/testthat.R`**

```r
library(testthat)
library(sports)
test_check("sports")
```

- [ ] **Step 6: Create empty directories**

```bash
cd /Users/brynjolfurjonsson/sports
mkdir -p R tests/testthat config scripts/etl data
touch R/.gitkeep tests/testthat/.gitkeep
```

- [ ] **Step 7: Verify the package loads**

```bash
cd /Users/brynjolfurjonsson/sports
Rscript -e 'devtools::load_all()'
```

Expected: `ℹ Loading sports` with no errors. (With no functions defined yet this is a structural check.)

- [ ] **Step 8: Commit**

```bash
cd /Users/brynjolfurjonsson/sports
git add DESCRIPTION NAMESPACE .Rbuildignore sports.Rproj tests/ R/ config/ scripts/ data/
git commit -m "feat: R package scaffolding

Treats the monorepo as a package for devtools::load_all() ergonomics
and conventional testthat wiring.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>"
```

---

## Task 5: Config schema + loader (TDD)

**Files:**

- Create: `/Users/brynjolfurjonsson/sports/config/leagues.schema.json`
- Create: `/Users/brynjolfurjonsson/sports/R/config.R`
- Create: `/Users/brynjolfurjonsson/sports/tests/testthat/test-config.R`

**Purpose:** Validate that any `config/leagues.yml` conforms to the spec before downstream code reads it. Replaces the three separate per-repo YAMLs.

- [ ] **Step 1: Write the JSON Schema**

Create `/Users/brynjolfurjonsson/sports/config/leagues.schema.json`:

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "title": "sports/config/leagues.yml",
  "type": "object",
  "patternProperties": {
    "^[a-z]+_[a-z]+$": {
      "type": "object",
      "required": ["sport", "country", "sexes", "active", "data_source", "stan_model"],
      "properties": {
        "sport":    { "type": "string", "enum": ["football", "basketball", "handball"] },
        "country":  { "type": "string", "minLength": 2 },
        "sexes":    { "type": "array", "items": { "type": "string", "enum": ["male", "female"] }, "minItems": 1 },
        "active":   { "type": "boolean" },
        "data_source": {
          "type": "object",
          "required": ["results", "schedule"],
          "properties": {
            "results":  { "type": "string" },
            "schedule": { "type": "string" },
            "odds":     { "type": ["string", "null"] }
          }
        },
        "lengjan": {
          "type": "object",
          "properties": {
            "competitions": {
              "type": "array",
              "items": {
                "type": "object",
                "required": ["id", "sex"],
                "properties": {
                  "id":   { "type": "string" },
                  "name": { "type": "string" },
                  "sex":  { "type": "string", "enum": ["male", "female"] }
                }
              }
            },
            "team_names": {
              "type": "object",
              "additionalProperties": { "type": "string" }
            }
          }
        },
        "stan_model": { "type": "string" },
        "betting": {
          "type": "object",
          "properties": {
            "kelly_fraction": { "type": "number", "minimum": 0, "maximum": 1 },
            "markets":        { "type": "array", "items": { "type": "string", "enum": ["moneyline", "spread", "total"] } },
            "min_bet":        { "type": "number", "minimum": 0 }
          }
        }
      }
    }
  },
  "additionalProperties": false
}
```

- [ ] **Step 2: Write the failing tests**

Create `/Users/brynjolfurjonsson/sports/tests/testthat/test-config.R`:

```r
test_that("load_leagues() reads a valid leagues.yml", {
  tmp <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(list(
    basketball_iceland = list(
      sport = "basketball",
      country = "iceland",
      sexes = list("male", "female"),
      active = TRUE,
      data_source = list(results = "kki_basketball", schedule = "kki_basketball", odds = "lengjan_odds"),
      stan_model = "basketball_iceland/2d_student_t_scalarsigma.stan"
    )
  ), tmp)

  leagues <- load_leagues(tmp, validate = FALSE)  # skip validation: the in-memory schema path resolves via here()

  expect_equal(names(leagues), "basketball_iceland")
  expect_equal(leagues$basketball_iceland$sport, "basketball")
  expect_equal(leagues$basketball_iceland$sexes, c("male", "female"))
})

test_that("load_leagues() rejects a schema-invalid yml when validation on", {
  tmp <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(list(
    basketball_iceland = list(
      sport = "curling",  # invalid enum
      country = "iceland",
      sexes = list("male"),
      active = TRUE,
      data_source = list(results = "x", schedule = "x"),
      stan_model = "x.stan"
    )
  ), tmp)

  schema <- file.path(rprojroot::find_package_root_file(), "config", "leagues.schema.json")
  expect_error(load_leagues(tmp, schema_path = schema), regexp = "curling|sport")
})

test_that("filter_leagues() narrows by selector", {
  leagues <- list(
    football_iceland   = list(sport = "football",   country = "iceland", active = TRUE),
    basketball_iceland = list(sport = "basketball", country = "iceland", active = TRUE),
    football_england   = list(sport = "football",   country = "england", active = FALSE)
  )

  expect_setequal(names(filter_leagues(leagues, sport = "football")),
                  c("football_iceland", "football_england"))
  expect_setequal(names(filter_leagues(leagues, active_only = TRUE)),
                  c("football_iceland", "basketball_iceland"))
  expect_equal(names(filter_leagues(leagues, league = "basketball_iceland")),
               "basketball_iceland")
})
```

- [ ] **Step 3: Run the test — verify it fails**

```bash
cd /Users/brynjolfurjonsson/sports
Rscript -e 'devtools::test(filter = "config")'
```

Expected: 3 failures with `could not find function "load_leagues"` (and `"filter_leagues"`).

- [ ] **Step 4: Implement the loader**

Create `/Users/brynjolfurjonsson/sports/R/config.R`:

```r
#' Load and validate leagues.yml
#'
#' @param path Path to leagues.yml.
#' @param schema_path Path to leagues.schema.json. Defaults to here().
#' @param validate If TRUE (default), validate the loaded YAML against the schema.
#' @return Named list keyed by league_key (e.g. "basketball_iceland").
#' @export
load_leagues <- function(path = here::here("config", "leagues.yml"),
                         schema_path = here::here("config", "leagues.schema.json"),
                         validate = TRUE) {
  raw <- readr::read_file(path)
  leagues <- yaml::yaml.load(raw)

  if (isTRUE(validate) && file.exists(schema_path)) {
    validate_leagues(leagues, schema_path)
  }

  leagues
}

validate_leagues <- function(leagues, schema_path) {
  json_text   <- jsonlite::toJSON(leagues, auto_unbox = TRUE, null = "null", na = "null")
  schema_text <- readr::read_file(schema_path)

  result <- jsonvalidate::json_validate(json_text, schema_text, verbose = TRUE, engine = "ajv")
  if (!isTRUE(result)) {
    errors <- attr(result, "errors")
    err_lines <- if (!is.null(errors) && nrow(errors) > 0) {
      paste(sprintf("  %s: %s", errors$instancePath, errors$message), collapse = "\n")
    } else {
      "  (no detailed errors returned by validator)"
    }
    stop(paste0("leagues.yml failed schema validation:\n", err_lines), call. = FALSE)
  }
  invisible(TRUE)
}

#' Filter a loaded leagues list by selector.
#'
#' @param leagues Named list from `load_leagues()`.
#' @param sport,country,league Optional filters.
#' @param active_only If TRUE, keep only leagues with `active = TRUE`.
#' @return Filtered named list.
#' @export
filter_leagues <- function(leagues, sport = NULL, country = NULL,
                           league = NULL, active_only = FALSE) {
  keep <- rep(TRUE, length(leagues))
  names(keep) <- names(leagues)

  if (!is.null(sport))
    keep <- keep & vapply(leagues, function(l) identical(l$sport, sport), logical(1))
  if (!is.null(country))
    keep <- keep & vapply(leagues, function(l) identical(l$country, country), logical(1))
  if (!is.null(league))
    keep <- keep & (names(leagues) == league)
  if (isTRUE(active_only))
    keep <- keep & vapply(leagues, function(l) isTRUE(l$active), logical(1))

  leagues[keep]
}
```

- [ ] **Step 5: Run the test — verify it passes**

```bash
cd /Users/brynjolfurjonsson/sports
Rscript -e 'devtools::test(filter = "config")'
```

Expected: 3 passes.

- [ ] **Step 6: Commit**

```bash
cd /Users/brynjolfurjonsson/sports
git add R/config.R tests/testthat/test-config.R config/leagues.schema.json
git commit -m "feat: unified leagues.yml loader with JSON Schema validation

Replaces three separate per-repo YAMLs with a single schema-validated
source of truth. Validation runs at load time and fails loud.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>"
```

---

## Task 6: Populate `config/leagues.yml` for 3 Icelandic leagues

**Files:**

- Create: `/Users/brynjolfurjonsson/sports/config/leagues.yml`

**Purpose:** Migrate league metadata for the three Icelandic leagues from `_legacy/sports/config/leagues.yml` + `_legacy/lengjan-odds/config/competitions.yml` + `_legacy/lengjan-odds/config/team_names_{sport}_iceland.csv` into one file.

- [ ] **Step 1: Inspect legacy sources**

```bash
cd /Users/brynjolfurjonsson/sports
grep -A 12 '^basketball_iceland:'  _legacy/sports/config/leagues.yml
grep -A 12 '^handball_iceland:'    _legacy/sports/config/leagues.yml
grep -A 12 '^football_iceland:'    _legacy/sports/config/leagues.yml
echo "---"
grep -A 4 '^basketball_iceland:'  _legacy/lengjan-odds/config/competitions.yml
grep -A 4 '^handball_iceland:'    _legacy/lengjan-odds/config/competitions.yml
grep -A 6 '^football_iceland:'    _legacy/lengjan-odds/config/competitions.yml
echo "---"
head -5 _legacy/lengjan-odds/config/team_names_basketball_iceland.csv
head -5 _legacy/lengjan-odds/config/team_names_handball_iceland.csv
head -5 _legacy/lengjan-odds/config/team_names_football_iceland.csv
```

Expected output confirms competition IDs: `basketball_iceland` → kvenna efri (30774) + neðri (30773); `handball_iceland` → Olísdeild karla (1269); `football_iceland` → Besta Deildin (746) + Efri Hluti (20443) + Neðri Hluti (20442).

**Note during execution:** if any Icelandic league has Lengjan competitions for only one sex (per the legacy competitions.yml), that is expected and correct — mark it faithfully in the new yml. Missing-sex coverage means "no odds, no bets for that sex" and is handled downstream.

- [ ] **Step 2: Write `config/leagues.yml`** using the IDs confirmed in Step 1

Create `/Users/brynjolfurjonsson/sports/config/leagues.yml`. The executor must:

1. Translate each Lengjan competition into the `lengjan.competitions` list with `id`, `name`, `sex` (inferring sex from the league name — "kvenna" = female, "karla" = male, absence of either = consult legacy leagues.yml for the default sex).
2. Read each `team_names_{sport}_iceland.csv` file and copy all `lengjan,pipeline` (or `in,out` — legacy column order) pairs into the `lengjan.team_names` block. Key is the Lengjan display name, value is the canonical pipeline name.
3. Stan model file names come from `_legacy/sports/config/leagues.yml` field `stan_model` for each league.

Template (executor fills in `competitions` and `team_names`):

```yaml
# config/leagues.yml — single source of truth for the three active Icelandic leagues.
# Migrated from _legacy/sports/config/leagues.yml + _legacy/lengjan-odds/config/.

basketball_iceland:
  sport: basketball
  country: iceland
  sexes: [male, female]
  active: true
  data_source:
    results: kki_basketball
    schedule: kki_basketball
    odds: lengjan_odds
  lengjan:
    competitions:
      - { id: "30774", name: "Bónusdeild kvenna efri", sex: female }
      - { id: "30773", name: "Bónusdeild kvenna neðri", sex: female }
      # NOTE during execution: if men's basketball Lengjan IDs exist (check Lengjan
      # product page or legacy scraped data), add them here. If not, men's has no
      # Lengjan coverage and the league still fits — bets just won't be recommended.
    team_names:
      # <paste all rows from _legacy/lengjan-odds/config/team_names_basketball_iceland.csv>
  stan_model: basketball_iceland/2d_student_t_scalarsigma.stan
  betting:
    kelly_fraction: 0.10
    markets: [moneyline, spread, total]
    min_bet: 200

handball_iceland:
  sport: handball
  country: iceland
  sexes: [male, female]
  active: true
  data_source:
    results: hsi_handball
    schedule: hsi_handball
    odds: lengjan_odds
  lengjan:
    competitions:
      - { id: "1269", name: "Olísdeild karla", sex: male }
      # NOTE during execution: women's handball (Grill 66 / Olís deild kvenna)
      # may have a Lengjan ID — check. If not, women's has no odds coverage.
    team_names:
      # <paste from _legacy/lengjan-odds/config/team_names_handball_iceland.csv>
  stan_model: handball_iceland/2d_student_t.stan
  betting:
    kelly_fraction: 0.10
    markets: [moneyline, spread, total]
    min_bet: 200

football_iceland:
  sport: football
  country: iceland
  sexes: [male, female]
  active: true
  data_source:
    results: ksi_football
    schedule: ksi_football
    odds: lengjan_odds
  lengjan:
    competitions:
      - { id: "746",   name: "Besta Deildin",            sex: male }
      - { id: "20443", name: "Besta Deildin Efri Hluti", sex: male }
      - { id: "20442", name: "Besta Deildin Neðri Hluti", sex: male }
      # NOTE: women's Besta Deild IDs should be added if Lengjan covers them.
    team_names:
      # <paste from _legacy/lengjan-odds/config/team_names_football_iceland.csv>
  stan_model: football_iceland/bivariate_poisson_no_inflation.stan
  betting:
    kelly_fraction: 0.10
    markets: [moneyline, spread, total]
    min_bet: 200
```

When executing: replace each `# <paste ...>` marker with the real team-name pairs. The final committed file must have **no `<paste>` markers and no `# NOTE during execution` comments** — resolve them concretely.

- [ ] **Step 3: Validate by running the loader**

```bash
cd /Users/brynjolfurjonsson/sports
Rscript -e '
leagues <- sports::load_leagues()
stopifnot(setequal(names(leagues), c("basketball_iceland", "handball_iceland", "football_iceland")))
stopifnot(all(vapply(leagues, function(l) isTRUE(l$active), logical(1))))
cat("OK — 3 active Icelandic leagues loaded and schema-validated.\n")
'
```

Expected: `OK — 3 active Icelandic leagues loaded and schema-validated.`

- [ ] **Step 4: Commit**

```bash
cd /Users/brynjolfurjonsson/sports
git add config/leagues.yml
git commit -m "feat: populate leagues.yml for 3 Icelandic leagues

Migrated from three legacy YAMLs (Sports/config/leagues.yml,
lengjan-odds/config/competitions.yml) and the three
team_names_*_iceland.csv files into one schema-validated source
of truth.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>"
```

---

## Task 7: Storage schemas (TDD)

**Files:**

- Create: `/Users/brynjolfurjonsson/sports/R/storage-schemas.R`
- Create: `/Users/brynjolfurjonsson/sports/tests/testthat/test-storage-schemas.R`

**Purpose:** Arrow schemas for all 8 tables. Every storage write validates against its schema.

- [ ] **Step 1: Write the failing tests**

Create `/Users/brynjolfurjonsson/sports/tests/testthat/test-storage-schemas.R`:

```r
test_that("all 8 schemas are defined by name", {
  s <- schemas()
  expect_setequal(
    names(s),
    c("results", "schedules", "odds",
      "beliefs_latest", "beliefs_archive",
      "candidates", "recommendations", "ledger")
  )
})

test_that("facts/results schema has the spec columns", {
  f <- schemas()$results$names
  expect_true(all(c("sport", "country", "sex", "season", "match_date",
                    "home_team", "away_team", "home_score", "away_score")
                  %in% f))
})

test_that("facts/odds schema includes scraped_at and line", {
  f <- schemas()$odds$names
  expect_true(all(c("sport", "country", "scraped_at", "match_date",
                    "home_team", "away_team", "market", "outcome",
                    "line", "odds") %in% f))
})

test_that("decisions/ledger schema has 17 canonical columns", {
  f <- schemas()$ledger$names
  expected <- c("placed_at", "match_date", "sport", "country", "sex",
                "home_team", "away_team", "market", "outcome", "line",
                "odds_placed", "p", "kelly", "bet_amount",
                "settled", "win", "pnl")
  expect_true(all(expected %in% f))
})

test_that("recommendations schema has run_id timestamp", {
  s <- schemas()$recommendations
  expect_true("run_id" %in% s$names)
  # run_id must be a timestamp, not string
  field <- s$GetFieldByName("run_id")
  expect_s4_class(field$type, "TimestampType")
})
```

- [ ] **Step 2: Run — verify failure**

```bash
cd /Users/brynjolfurjonsson/sports
Rscript -e 'devtools::test(filter = "storage-schemas")'
```

Expected: 5 errors with `could not find function "schemas"`.

- [ ] **Step 3: Implement schemas**

Create `/Users/brynjolfurjonsson/sports/R/storage-schemas.R`:

```r
#' Arrow schemas for every storage table
#'
#' Referenced by every read/write primitive in R/storage.R. Column-naming
#' convention follows the spec §3.3 (English underscore_case, `home_team`/
#' `away_team`, `match_date` not `date`, `p` for probabilities).
#'
#' @return Named list of arrow::Schema.
#' @export
schemas <- function() {
  ts <- arrow::timestamp(unit = "s", timezone = "UTC")

  list(
    results = arrow::schema(
      sport       = arrow::string(),
      country     = arrow::string(),
      sex         = arrow::string(),
      season      = arrow::int32(),
      match_date  = arrow::date32(),
      home_team   = arrow::string(),
      away_team   = arrow::string(),
      home_score  = arrow::int32(),
      away_score  = arrow::int32(),
      division    = arrow::string(),
      round       = arrow::int32()
    ),

    schedules = arrow::schema(
      sport       = arrow::string(),
      country     = arrow::string(),
      sex         = arrow::string(),
      season      = arrow::int32(),
      match_date  = arrow::date32(),
      home_team   = arrow::string(),
      away_team   = arrow::string(),
      division    = arrow::string(),
      round       = arrow::int32()
    ),

    odds = arrow::schema(
      sport       = arrow::string(),
      country     = arrow::string(),
      scraped_at  = ts,
      match_date  = arrow::date32(),
      home_team   = arrow::string(),
      away_team   = arrow::string(),
      market      = arrow::string(),   # moneyline | spread | total
      outcome     = arrow::string(),   # home | draw | away | over | under
      line        = arrow::float64(),  # NA for moneyline
      odds        = arrow::float64()
    ),

    beliefs_latest = arrow::schema(
      sport       = arrow::string(),
      country     = arrow::string(),
      sex         = arrow::string(),
      fit_date    = arrow::date32(),
      match_date  = arrow::date32(),
      home_team   = arrow::string(),
      away_team   = arrow::string(),
      draw_id     = arrow::int32(),
      home_goals  = arrow::float64(),
      away_goals  = arrow::float64()
    ),

    beliefs_archive = arrow::schema(
      sport       = arrow::string(),
      country     = arrow::string(),
      sex         = arrow::string(),
      fit_date    = arrow::date32(),
      match_date  = arrow::date32(),
      home_team   = arrow::string(),
      away_team   = arrow::string(),
      draw_id     = arrow::int32(),
      home_goals  = arrow::float64(),
      away_goals  = arrow::float64()
    ),

    candidates = arrow::schema(
      run_id      = ts,
      sport       = arrow::string(),
      country     = arrow::string(),
      sex         = arrow::string(),
      match_date  = arrow::date32(),
      home_team   = arrow::string(),
      away_team   = arrow::string(),
      market      = arrow::string(),
      outcome     = arrow::string(),
      line        = arrow::float64(),
      p           = arrow::float64(),
      odds        = arrow::float64(),
      ev          = arrow::float64(),
      kelly_raw   = arrow::float64(),
      stage       = arrow::string()
    ),

    recommendations = arrow::schema(
      run_id      = ts,
      sport       = arrow::string(),
      country     = arrow::string(),
      sex         = arrow::string(),
      match_date  = arrow::date32(),
      home_team   = arrow::string(),
      away_team   = arrow::string(),
      market      = arrow::string(),
      outcome     = arrow::string(),
      line        = arrow::float64(),
      p           = arrow::float64(),
      odds        = arrow::float64(),
      ev          = arrow::float64(),
      kelly       = arrow::float64(),
      bet_amount  = arrow::float64()
    ),

    ledger = arrow::schema(
      placed_at   = ts,
      match_date  = arrow::date32(),
      sport       = arrow::string(),
      country     = arrow::string(),
      sex         = arrow::string(),
      home_team   = arrow::string(),
      away_team   = arrow::string(),
      market      = arrow::string(),
      outcome     = arrow::string(),
      line        = arrow::float64(),
      odds_placed = arrow::float64(),
      p           = arrow::float64(),
      kelly       = arrow::float64(),
      bet_amount  = arrow::float64(),
      settled     = arrow::bool(),
      win         = arrow::bool(),
      pnl         = arrow::float64()
    )
  )
}
```

- [ ] **Step 4: Run — verify pass**

```bash
cd /Users/brynjolfurjonsson/sports
Rscript -e 'devtools::test(filter = "storage-schemas")'
```

Expected: 5 passes.

- [ ] **Step 5: Commit**

```bash
cd /Users/brynjolfurjonsson/sports
git add R/storage-schemas.R tests/testthat/test-storage-schemas.R
git commit -m "feat: Arrow schemas for 8 storage tables

Defines the contract every storage primitive validates against.
Column names follow spec §3.3: English underscore_case,
home_team/away_team, match_date, p (not probability), line
(signed for spread, positive for total).

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>"
```

---

## Task 8: Storage primitives (TDD)

**Files:**

- Create: `/Users/brynjolfurjonsson/sports/R/storage.R`
- Create: `/Users/brynjolfurjonsson/sports/tests/testthat/test-storage.R`

**Purpose:** `write_table()` and `read_table()` wrappers that enforce the Arrow schemas. Partitioning is hive-style per the spec.

- [ ] **Step 1: Write the failing tests**

Create `/Users/brynjolfurjonsson/sports/tests/testthat/test-storage.R`:

```r
make_ledger_row <- function(...) {
  defaults <- list(
    placed_at   = as.POSIXct("2026-04-24 10:00:00", tz = "UTC"),
    match_date  = as.Date("2026-04-24"),
    sport       = "basketball", country = "iceland", sex = "male",
    home_team   = "KR", away_team = "Stjarnan",
    market      = "moneyline", outcome = "home",
    line        = NA_real_, odds_placed = 2.10, p = 0.55,
    kelly       = 0.02, bet_amount = 500,
    settled     = FALSE, win = NA, pnl = NA_real_
  )
  tibble::as_tibble(modifyList(defaults, list(...)))
}

test_that("write_table writes Parquet and round-trips via read_table", {
  tmp <- withr::local_tempdir()
  row <- make_ledger_row()

  write_table(row, table = "ledger", root = tmp)

  back <- read_table(table = "ledger", root = tmp)
  expect_equal(nrow(back), 1L)
  expect_equal(back$home_team, "KR")
  expect_equal(back$bet_amount, 500)
})

test_that("write_table rejects rows missing a required column", {
  tmp <- withr::local_tempdir()
  row <- make_ledger_row()
  row$odds_placed <- NULL

  expect_error(write_table(row, table = "ledger", root = tmp),
               regexp = "odds_placed")
})

test_that("write_table rejects wrong type for a column", {
  tmp <- withr::local_tempdir()
  row <- make_ledger_row(odds_placed = "2.10")   # string not double

  expect_error(write_table(row, table = "ledger", root = tmp),
               regexp = "odds_placed")
})

test_that("write_table partitions by the table's partition columns", {
  tmp <- withr::local_tempdir()
  rows <- dplyr::bind_rows(
    make_ledger_row(sport = "basketball"),
    make_ledger_row(sport = "handball")
  )

  write_table(rows, table = "ledger", root = tmp)

  dirs <- fs::dir_ls(fs::path(tmp, "decisions", "ledger"), type = "directory")
  expect_true(any(grepl("sport=basketball", dirs)))
  expect_true(any(grepl("sport=handball", dirs)))
})

test_that("read_table with predicate filters before materialising", {
  tmp <- withr::local_tempdir()
  rows <- dplyr::bind_rows(
    make_ledger_row(sport = "basketball"),
    make_ledger_row(sport = "handball")
  )
  write_table(rows, table = "ledger", root = tmp)

  b <- read_table(table = "ledger", root = tmp, filter = list(sport = "basketball"))
  expect_equal(nrow(b), 1L)
  expect_equal(b$sport, "basketball")
})
```

- [ ] **Step 2: Run — verify failure**

```bash
cd /Users/brynjolfurjonsson/sports
Rscript -e 'devtools::test(filter = "^storage$")'
```

Expected: 5 errors with `could not find function "write_table"`.

- [ ] **Step 3: Implement storage primitives**

Create `/Users/brynjolfurjonsson/sports/R/storage.R`:

```r
#' Partitioning rules per table (spec §3.2)
table_partitions <- function() {
  list(
    results         = c("sport", "country", "sex", "season"),
    schedules       = c("sport", "country", "sex", "season"),
    odds            = c("sport", "country", "scraped_date"),
    beliefs_latest  = c("sport", "country", "sex"),
    beliefs_archive = c("sport", "country", "sex", "fit_date"),
    candidates      = c("sport", "country", "run_date"),
    recommendations = c("sport", "country", "run_date"),
    ledger          = c("sport", "country")
  )
}

#' Map each table to its subdirectory under `root` (spec §3.1)
table_subdir <- function(table) {
  switch(table,
    results         = c("facts", "results"),
    schedules       = c("facts", "schedules"),
    odds            = c("facts", "odds"),
    beliefs_latest  = c("beliefs", "latest"),
    beliefs_archive = c("beliefs", "archive"),
    candidates      = c("decisions", "candidates"),
    recommendations = c("decisions", "recommendations"),
    ledger          = c("decisions", "ledger"),
    stop("Unknown table: ", table, call. = FALSE)
  )
}

#' Derive any virtual partition columns the table needs (e.g. scraped_date from
#' scraped_at, run_date from run_id) before validation.
add_virtual_partitions <- function(df, table) {
  if (table == "odds" && !("scraped_date" %in% names(df)) && "scraped_at" %in% names(df)) {
    df$scraped_date <- as.Date(df$scraped_at)
  }
  if (table %in% c("candidates", "recommendations") &&
      !("run_date" %in% names(df)) && "run_id" %in% names(df)) {
    df$run_date <- as.Date(df$run_id)
  }
  if (table == "beliefs_archive" && !("fit_date" %in% names(df))) {
    stop("beliefs_archive requires fit_date", call. = FALSE)
  }
  df
}

#' Validate a data frame against a schema.
#' Raises a diagnostic error on the first problem found.
validate_against_schema <- function(df, table) {
  s <- schemas()[[table]]
  if (is.null(s)) stop("Unknown table: ", table, call. = FALSE)

  required_cols <- s$names
  missing <- setdiff(required_cols, names(df))
  if (length(missing) > 0) {
    stop(sprintf("Table '%s' missing column(s): %s",
                 table, paste(missing, collapse = ", ")),
         call. = FALSE)
  }

  # Try materialising an Arrow Table; Arrow raises on type mismatch.
  df_ordered <- df[, required_cols]
  tryCatch(
    arrow::as_arrow_table(df_ordered, schema = s),
    error = function(e) {
      msg <- conditionMessage(e)
      stop(sprintf("Schema validation failed for table '%s': %s", table, msg),
           call. = FALSE)
    }
  )
}

#' Write a data frame to the store as hive-partitioned Parquet.
#'
#' @param df data frame or tibble.
#' @param table one of names(schemas()).
#' @param root filesystem root (defaults to here::here("data")).
#' @return invisible(NULL)
#' @export
write_table <- function(df, table, root = here::here("data")) {
  if (nrow(df) == 0) return(invisible(NULL))

  df <- add_virtual_partitions(df, table)
  tbl <- validate_against_schema(df, table)
  partitions <- table_partitions()[[table]]

  dest <- do.call(fs::path, c(list(root), table_subdir(table)))
  fs::dir_create(dest, recurse = TRUE)

  arrow::write_dataset(
    tbl,
    path          = dest,
    format        = "parquet",
    partitioning  = partitions,
    existing_data_behavior = "overwrite_or_ignore"
  )

  invisible(NULL)
}

#' Read a table back as a tibble.
#'
#' @param table one of names(schemas()).
#' @param root filesystem root.
#' @param filter optional named list of column=value filters pushed down to Arrow.
#' @return tibble
#' @export
read_table <- function(table, root = here::here("data"), filter = list()) {
  src <- do.call(fs::path, c(list(root), table_subdir(table)))
  if (!fs::dir_exists(src)) return(tibble::tibble())

  ds <- arrow::open_dataset(src, partitioning = table_partitions()[[table]])

  if (length(filter) > 0) {
    for (col in names(filter)) {
      val <- filter[[col]]
      ds <- dplyr::filter(ds, .data[[col]] == val)
    }
  }

  dplyr::collect(ds) |> tibble::as_tibble()
}
```

- [ ] **Step 4: Run — verify pass**

```bash
cd /Users/brynjolfurjonsson/sports
Rscript -e 'devtools::test(filter = "^storage$")'
```

Expected: 5 passes.

- [ ] **Step 5: Commit**

```bash
cd /Users/brynjolfurjonsson/sports
git add R/storage.R tests/testthat/test-storage.R
git commit -m "feat: schema-validated Parquet write/read primitives

write_table() validates against the Arrow schema before writing
hive-partitioned Parquet. read_table() pushes equality filters
down via arrow::open_dataset() for efficient partition pruning.
Virtual partitions (scraped_date, run_date) are derived from the
underlying timestamp columns automatically.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>"
```

---

## Task 9: DuckDB view rebuilder (TDD)

**Files:**

- Create: `/Users/brynjolfurjonsson/sports/R/duckdb-views.R`
- Create: `/Users/brynjolfurjonsson/sports/tests/testthat/test-duckdb-views.R`

**Purpose:** Expose every Parquet store as a DuckDB view for ad-hoc SQL + research queries. The `.duckdb` file is rebuildable from Parquet and gitignored.

- [ ] **Step 1: Write the failing test**

Create `/Users/brynjolfurjonsson/sports/tests/testthat/test-duckdb-views.R`:

```r
test_that("rebuild_duckdb creates a file with views over Parquet", {
  tmp    <- withr::local_tempdir()
  db_path <- fs::path(tmp, "sports.duckdb")

  # Seed a small ledger Parquet
  row <- tibble::tibble(
    placed_at   = as.POSIXct("2026-04-24 10:00:00", tz = "UTC"),
    match_date  = as.Date("2026-04-24"),
    sport = "basketball", country = "iceland", sex = "male",
    home_team = "KR", away_team = "Stjarnan",
    market = "moneyline", outcome = "home",
    line = NA_real_, odds_placed = 2.10, p = 0.55,
    kelly = 0.02, bet_amount = 500,
    settled = TRUE, win = TRUE, pnl = 550
  )
  write_table(row, "ledger", root = tmp)

  rebuild_duckdb(root = tmp, db_path = db_path)

  expect_true(file.exists(db_path))

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  views <- DBI::dbGetQuery(con, "SELECT view_name FROM duckdb_views() WHERE schema_name = 'main'")$view_name
  expect_true("ledger" %in% views)

  sum_pnl <- DBI::dbGetQuery(con, "SELECT SUM(pnl) AS pnl FROM ledger WHERE settled")$pnl
  expect_equal(sum_pnl, 550)
})
```

- [ ] **Step 2: Run — verify failure**

```bash
cd /Users/brynjolfurjonsson/sports
Rscript -e 'devtools::test(filter = "duckdb-views")'
```

Expected: failure with `could not find function "rebuild_duckdb"`.

- [ ] **Step 3: Implement**

Create `/Users/brynjolfurjonsson/sports/R/duckdb-views.R`:

```r
#' Regenerate sports.duckdb with views over every Parquet store.
#'
#' Non-destructive: the old file is deleted and a fresh one written.
#' Because views are just SQL over Parquet paths, the resulting DB file
#' is small (<1 MB) and fully regenerable.
#'
#' @param root Filesystem root containing data/.
#' @param db_path Where to write the DuckDB file.
#' @return invisible(db_path)
#' @export
rebuild_duckdb <- function(root    = here::here("data"),
                           db_path = here::here("sports.duckdb")) {
  if (file.exists(db_path))        fs::file_delete(db_path)
  wal <- paste0(db_path, ".wal")
  if (file.exists(wal))            fs::file_delete(wal)

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = FALSE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  subdirs <- list(
    results         = c("facts", "results"),
    schedules       = c("facts", "schedules"),
    odds            = c("facts", "odds"),
    beliefs_latest  = c("beliefs", "latest"),
    beliefs_archive = c("beliefs", "archive"),
    candidates      = c("decisions", "candidates"),
    recommendations = c("decisions", "recommendations"),
    ledger          = c("decisions", "ledger")
  )

  for (view_name in names(subdirs)) {
    dir <- do.call(fs::path, c(list(root), subdirs[[view_name]]))
    if (!fs::dir_exists(dir)) next

    glob <- sprintf("%s/**/*.parquet", dir)
    sql <- sprintf(
      "CREATE OR REPLACE VIEW %s AS SELECT * FROM read_parquet('%s', hive_partitioning = TRUE)",
      view_name, glob
    )
    DBI::dbExecute(con, sql)
  }

  cli::cli_inform("Rebuilt DuckDB at {.path {db_path}}")
  invisible(db_path)
}
```

- [ ] **Step 4: Run — verify pass**

```bash
cd /Users/brynjolfurjonsson/sports
Rscript -e 'devtools::test(filter = "duckdb-views")'
```

Expected: 1 pass.

- [ ] **Step 5: Commit**

```bash
cd /Users/brynjolfurjonsson/sports
git add R/duckdb-views.R tests/testthat/test-duckdb-views.R
git commit -m "feat: rebuild_duckdb() exposes Parquet stores as DuckDB views

Creates sports.duckdb with one view per table, sourced via
read_parquet(hive_partitioning = TRUE). Rebuildable on demand,
gitignored. Research queries go through this.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>"
```

---

## Task 10: ETL — facts/results and facts/schedules

**Files:**

- Create: `/Users/brynjolfurjonsson/sports/scripts/etl/01_etl_results.R`
- Create: `/Users/brynjolfurjonsson/sports/scripts/etl/02_etl_schedules.R`

**Purpose:** Port legacy per-league CSV match results and schedules for the three Icelandic leagues into `data/facts/results/` and `data/facts/schedules/`.

The legacy layout differs per sport:

- `_legacy/sports/basketball/iceland/data/{sex}/data.csv` (combined result rows)
- `_legacy/sports/handball/iceland/data/{sex}/data.csv` (combined)
- `_legacy/sports/football/iceland/data/{sex}/data.csv`

Schedule CSVs live alongside data:

- `_legacy/sports/{sport}/iceland/data/{sex}/schedule.csv`

Exact column names vary; the ETL script must read each and rename to the canonical schema. During execution, inspect legacy files first and build the rename map.

- [ ] **Step 1: Inspect legacy file layouts**

```bash
cd /Users/brynjolfurjonsson/sports
for sport in basketball handball football; do
  for sex in male female; do
    d=_legacy/sports/$sport/iceland/data/$sex
    [ -d "$d" ] && echo "== $d ==" && ls "$d" | head -5
  done
done
```

Record: for each (sport, sex), does `data.csv` (results) + `schedule.csv` exist? What are their column names (`head -1 file.csv`)?

- [ ] **Step 2: Write the ETL script for results**

Create `/Users/brynjolfurjonsson/sports/scripts/etl/01_etl_results.R`:

```r
#!/usr/bin/env Rscript
# ETL: legacy per-league CSV match results -> data/facts/results/*.parquet

devtools::load_all(here::here())
library(dplyr)

leagues <- load_leagues()

# Map legacy {sport}/{country}/data/{sex}/data.csv columns to canonical schema.
# The legacy CSVs use Icelandic-inflected headers; executor must confirm and
# adjust these renames based on Task 10 Step 1 inspection.
rename_results <- function(df, sport, country, sex) {
  df |>
    mutate(
      sport      = sport,
      country    = country,
      sex        = sex,
      match_date = as.Date(match_date),
      # season is usually present; if missing (basketball historical rows),
      # infer from match_date:
      season     = if ("season" %in% names(df)) as.integer(season)
                   else as.integer(format(as.Date(match_date), "%Y")),
      home_score = as.integer(home_score),
      away_score = as.integer(away_score),
      division   = if ("division" %in% names(df)) as.character(division) else NA_character_,
      # No legacy CSV has a round column — fill with NA:
      round      = NA_integer_
    ) |>
    select(sport, country, sex, season, match_date,
           home_team, away_team, home_score, away_score, division, round)
}

etl_league <- function(key, league) {
  cli::cli_h2(key)
  for (sex in league$sexes) {
    csv <- here::here("_legacy", "sports", league$sport, league$country,
                      "data", sex, "data.csv")
    if (!file.exists(csv)) {
      cli::cli_alert_warning("Skip {.path {csv}} (missing)")
      next
    }
    raw <- readr::read_csv(csv, show_col_types = FALSE,
                           locale = readr::locale(encoding = "UTF-8"))
    # Legacy column names confirmed against Sports/*/iceland/data/*/data.csv
    # on 2026-04-24 (see Task 10 Step 1):
    #   basketball data.csv: timabil,division,dags,heima,gestir,stig_heima,stig_gestir
    #   handball   data.csv: dagsetning,home,away,home_goals,away_goals,season,division
    #   football   data.csv: timabil,dags,heima,stig_heima,gestir,stig_gestir,division,finals
    raw <- switch(league$sport,
      basketball = raw |> rename(season = timabil, match_date = dags,
                                 home_team = heima, away_team = gestir,
                                 home_score = stig_heima, away_score = stig_gestir),
      handball   = raw |> rename(match_date = dagsetning,
                                 home_team = home, away_team = away,
                                 home_score = home_goals, away_score = away_goals),
      football   = raw |> rename(season = timabil, match_date = dags,
                                 home_team = heima, away_team = gestir,
                                 home_score = stig_heima, away_score = stig_gestir),
      raw
    )

    renamed <- rename_results(raw, league$sport, league$country, sex)
    write_table(renamed, "results")
    cli::cli_alert_success("{key}/{sex}: {nrow(renamed)} rows written")
  }
}

for (key in names(leagues)) etl_league(key, leagues[[key]])
cli::cli_alert_success("Done.")
```

**Execution note:** the `switch()` on `league$sport` inside the script has the rename map in-line. Executor must verify each mapping matches the actual legacy CSV column names from Step 1 and adjust. If a column in the canonical schema has no source column, compute or set to `NA_*`.

- [ ] **Step 3: Write the ETL script for schedules**

Create `/Users/brynjolfurjonsson/sports/scripts/etl/02_etl_schedules.R`:

```r
#!/usr/bin/env Rscript
# ETL: legacy per-league CSV schedules -> data/facts/schedules/*.parquet

devtools::load_all(here::here())
library(dplyr)

leagues <- load_leagues()

etl_league <- function(key, league) {
  cli::cli_h2(key)
  for (sex in league$sexes) {
    csv <- here::here("_legacy", "sports", league$sport, league$country,
                      "data", sex, "schedule.csv")
    if (!file.exists(csv)) {
      cli::cli_alert_warning("Skip {.path {csv}} (missing)")
      next
    }
    raw <- readr::read_csv(csv, show_col_types = FALSE,
                           locale = readr::locale(encoding = "UTF-8"))
    # Legacy schedule columns confirmed on 2026-04-24:
    #   basketball: dags,heima,gestir,division
    #   handball:   dagsetning,home,away,division
    #   football:   dags,heima,gestir,division
    renamed <- switch(league$sport,
      basketball = raw |> rename(match_date = dags,       home_team = heima, away_team = gestir),
      handball   = raw |> rename(match_date = dagsetning, home_team = home,  away_team = away),
      football   = raw |> rename(match_date = dags,       home_team = heima, away_team = gestir),
      raw
    ) |>
    mutate(
      sport      = league$sport,
      country    = league$country,
      sex        = sex,
      match_date = as.Date(match_date),
      season     = as.integer(format(match_date, "%Y")),
      home_team  = as.character(home_team),
      away_team  = as.character(away_team),
      division   = as.character(division),
      round      = NA_integer_
    ) |>
    select(sport, country, sex, season, match_date,
           home_team, away_team, division, round)

    write_table(renamed, "schedules")
    cli::cli_alert_success("{key}/{sex}: {nrow(renamed)} rows written")
  }
}

for (key in names(leagues)) etl_league(key, leagues[[key]])
cli::cli_alert_success("Done.")
```

- [ ] **Step 4: Run both scripts**

```bash
cd /Users/brynjolfurjonsson/sports
Rscript scripts/etl/01_etl_results.R
Rscript scripts/etl/02_etl_schedules.R
```

Expected: per-league success messages with row counts; no errors from `write_table()` (which would indicate a rename-map mistake vs the schema).

- [ ] **Step 5: Spot-check the output**

```bash
cd /Users/brynjolfurjonsson/sports
Rscript -e '
devtools::load_all()
r <- read_table("results",  filter = list(sport = "basketball"))
s <- read_table("schedules", filter = list(sport = "basketball"))
cat("basketball results:", nrow(r), "rows\n")
cat("basketball schedules:", nrow(s), "rows\n")
head(r, 3) |> print()
'
```

Expected: positive row counts; dates in ISO format; team names are character.

- [ ] **Step 6: Commit**

```bash
cd /Users/brynjolfurjonsson/sports
git add scripts/etl/01_etl_results.R scripts/etl/02_etl_schedules.R data/facts/
git commit -m "etl: results and schedules for 3 Icelandic leagues

One-time migration from _legacy/sports/{sport}/iceland/data/{sex}/
CSVs into data/facts/{results,schedules}/ as hive-partitioned Parquet.
Column renames handled in 01_etl_results.R switch().

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>"
```

---

## Task 11: ETL — facts/odds from lengjan-odds

**Files:**

- Create: `/Users/brynjolfurjonsson/sports/scripts/etl/03_etl_odds.R`

**Purpose:** Port `_legacy/lengjan-odds/data/{sport_key}/{odds_1x2,odds_handicap,odds_totals}.csv` for the three Icelandic league keys into `data/facts/odds/` as a single long-form table.

Legacy per-sport layout (from spec §3.2 of Plan 1 and prior exploration):

- `odds_1x2.csv`      → `market = "moneyline"`, outcomes `home/draw/away`, `line = NA`
- `odds_handicap.csv` → `market = "spread"`,    outcomes `home/draw/away`, `line = change` (signed)
- `odds_totals.csv`   → `market = "total"`,     outcomes `over/under`,      `line = limit` (positive)

- [ ] **Step 1: Confirm the three Icelandic sport_keys exist in legacy**

```bash
cd /Users/brynjolfurjonsson/sports
ls _legacy/lengjan-odds/data/basketball_iceland/ \
   _legacy/lengjan-odds/data/handball_iceland/ \
   _legacy/lengjan-odds/data/football_iceland/ 2>/dev/null || \
echo "One or more missing; football_iceland may never have been scraped"
```

If `football_iceland/` is missing from legacy, note it. The ETL should skip it gracefully; Plan 4 (scrapers) will backfill once running.

- [ ] **Step 2: Write the ETL script**

Create `/Users/brynjolfurjonsson/sports/scripts/etl/03_etl_odds.R`:

```r
#!/usr/bin/env Rscript
# ETL: _legacy/lengjan-odds/data/{sport_key}/*.csv -> data/facts/odds/

devtools::load_all(here::here())
library(dplyr)
library(tidyr)

sport_keys <- list(
  basketball_iceland = list(sport = "basketball", country = "iceland"),
  handball_iceland   = list(sport = "handball",   country = "iceland"),
  football_iceland   = list(sport = "football",   country = "iceland")
)

pivot_1x2 <- function(df, meta) {
  df |>
    tidyr::pivot_longer(
      cols = c("o_home", "o_draw", "o_away"),
      names_to = "outcome",
      values_to = "odds"
    ) |>
    mutate(
      sport      = meta$sport,
      country    = meta$country,
      market     = "moneyline",
      outcome    = sub("^o_", "", outcome),
      line       = NA_real_,
      scraped_at = as.POSIXct(scraped_at, tz = "UTC"),
      match_date = as.Date(date),
      home_team  = as.character(home),
      away_team  = as.character(away),
      odds       = as.numeric(odds)
    ) |>
    filter(!is.na(odds)) |>
    select(sport, country, scraped_at, match_date, home_team, away_team,
           market, outcome, line, odds)
}

pivot_handicap <- function(df, meta) {
  df |>
    tidyr::pivot_longer(
      cols = c("o_home", "o_draw", "o_away"),
      names_to = "outcome",
      values_to = "odds"
    ) |>
    mutate(
      sport      = meta$sport,
      country    = meta$country,
      market     = "spread",
      outcome    = sub("^o_", "", outcome),
      line       = as.numeric(change),   # signed
      scraped_at = as.POSIXct(scraped_at, tz = "UTC"),
      match_date = as.Date(date),
      home_team  = as.character(home),
      away_team  = as.character(away),
      odds       = as.numeric(odds)
    ) |>
    filter(!is.na(odds)) |>
    select(sport, country, scraped_at, match_date, home_team, away_team,
           market, outcome, line, odds)
}

pivot_totals <- function(df, meta) {
  df |>
    tidyr::pivot_longer(
      cols = c("o_over", "o_under"),
      names_to = "outcome",
      values_to = "odds"
    ) |>
    mutate(
      sport      = meta$sport,
      country    = meta$country,
      market     = "total",
      outcome    = sub("^o_", "", outcome),
      line       = as.numeric(limit),    # positive
      scraped_at = as.POSIXct(scraped_at, tz = "UTC"),
      match_date = as.Date(date),
      home_team  = as.character(home),
      away_team  = as.character(away),
      odds       = as.numeric(odds)
    ) |>
    filter(!is.na(odds)) |>
    select(sport, country, scraped_at, match_date, home_team, away_team,
           market, outcome, line, odds)
}

etl_sport_key <- function(sport_key, meta) {
  cli::cli_h2(sport_key)
  dir <- here::here("_legacy", "lengjan-odds", "data", sport_key)
  if (!fs::dir_exists(dir)) {
    cli::cli_alert_warning("Skip {.path {dir}} (no scraped data)")
    return(invisible())
  }

  chunks <- list()
  f1 <- fs::path(dir, "odds_1x2.csv")
  if (file.exists(f1))
    chunks$m <- pivot_1x2(readr::read_csv(f1, show_col_types = FALSE), meta)

  f2 <- fs::path(dir, "odds_handicap.csv")
  if (file.exists(f2))
    chunks$s <- pivot_handicap(readr::read_csv(f2, show_col_types = FALSE), meta)

  f3 <- fs::path(dir, "odds_totals.csv")
  if (file.exists(f3))
    chunks$t <- pivot_totals(readr::read_csv(f3, show_col_types = FALSE), meta)

  all <- dplyr::bind_rows(chunks)
  if (nrow(all) == 0) {
    cli::cli_alert_warning("{sport_key}: no rows after pivot")
    return(invisible())
  }

  write_table(all, "odds")
  cli::cli_alert_success("{sport_key}: {nrow(all)} odds rows written")
}

for (k in names(sport_keys)) etl_sport_key(k, sport_keys[[k]])
cli::cli_alert_success("Done.")
```

- [ ] **Step 3: Run**

```bash
cd /Users/brynjolfurjonsson/sports
Rscript scripts/etl/03_etl_odds.R
```

Expected: per-sport success with row counts (may be 0 for football_iceland if never scraped).

- [ ] **Step 4: Spot check**

```bash
cd /Users/brynjolfurjonsson/sports
Rscript -e '
devtools::load_all()
o <- read_table("odds", filter = list(sport = "basketball"))
cat("basketball odds rows:", nrow(o), "\n")
cat("markets:", paste(unique(o$market), collapse=", "), "\n")
cat("scraped_at range:", format(range(o$scraped_at)), "\n")
'
```

Expected: rows > 0 for basketball + handball, `markets: moneyline, spread, total` (or subset thereof if the scraper didn't cover all markets).

- [ ] **Step 5: Commit**

```bash
cd /Users/brynjolfurjonsson/sports
git add scripts/etl/03_etl_odds.R data/facts/odds/
git commit -m "etl: Lengjan odds for 3 Icelandic sport_keys -> long-form Parquet

Pivots the legacy three-CSV layout (odds_1x2 / odds_handicap /
odds_totals) into a single long table with market in {moneyline,
spread, total} and line holding the handicap/total.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>"
```

---

## Task 12: ETL — decisions/ledger from bets_log.csv

**Files:**

- Create: `/Users/brynjolfurjonsson/sports/scripts/etl/04_etl_ledger.R`

**Purpose:** Port `_legacy/sports/{sport}/iceland/history/bets_log.csv` → `data/decisions/ledger/*.parquet`.

Legacy ledger columns (from Plan 1 spec §3.2 and prior exploration):

```
date_recommended, date_match, sport, country, sex, market, home, away,
outcome, odds, probability, ev, kelly_frac, bet_amount, info, win, pnl, source
```

Canonical ledger columns (spec §3.3):

```
placed_at, match_date, sport, country, sex, home_team, away_team, market,
outcome, line, odds_placed, p, kelly, bet_amount, settled, win, pnl
```

Column mapping:

- `date_recommended` → `placed_at` (as POSIXct UTC, midnight)
- `date_match`       → `match_date`
- `home`, `away`     → `home_team`, `away_team`
- `odds`             → `odds_placed`
- `probability`      → `p`
- `kelly_frac`       → `kelly`
- `info`             → `line` (numeric parse — empty for moneyline, signed for spread, positive for total)
- `settled`          → derived: `!is.na(win)`
- Drop: `ev` (recompute at read time), `source` (all "pipeline" today)

- [ ] **Step 1: Write the ETL script**

Create `/Users/brynjolfurjonsson/sports/scripts/etl/04_etl_ledger.R`:

```r
#!/usr/bin/env Rscript
# ETL: _legacy/sports/{sport}/iceland/history/bets_log.csv -> data/decisions/ledger/*.parquet

devtools::load_all(here::here())
library(dplyr)

leagues <- load_leagues()

parse_line <- function(info, market) {
  # info is free text; handle the three markets:
  #   moneyline -> empty string or NA -> NA_real_
  #   spread    -> signed handicap like "-1.5" or "0-1" (scoreline) -> numeric
  #   total     -> positive like "2.5" or "59.5"                     -> numeric
  dplyr::case_when(
    market == "moneyline"       ~ NA_real_,
    is.na(info) | info == ""    ~ NA_real_,
    market == "total"           ~ suppressWarnings(as.numeric(info)),
    market == "spread"          ~ suppressWarnings(as.numeric(
                                    sub("^([-+]?\\d+(?:\\.\\d+)?).*", "\\1", info)
                                  )),
    TRUE                        ~ NA_real_
  )
}

# legacy market vocabulary -> canonical
market_map <- c(
  outcome    = "moneyline",
  handicap   = "spread",
  totals     = "total",
  moneyline  = "moneyline",
  spread     = "spread",
  total      = "total"
)

etl_league <- function(key, league) {
  csv <- here::here("_legacy", "sports", league$sport, league$country,
                    "history", "bets_log.csv")
  if (!file.exists(csv)) {
    cli::cli_alert_warning("Skip {.path {csv}} (no ledger)")
    return(invisible(NULL))
  }

  raw <- readr::read_csv(csv, show_col_types = FALSE,
                         locale = readr::locale(encoding = "UTF-8"))

  canonical <- raw |>
    mutate(
      placed_at   = as.POSIXct(as.character(date_recommended), tz = "UTC",
                               tryFormats = c("%Y-%m-%d", "%Y-%m-%d %H:%M:%S")),
      match_date  = as.Date(date_match),
      sport       = as.character(sport),
      country     = as.character(country),
      sex         = as.character(sex),
      home_team   = as.character(home),
      away_team   = as.character(away),
      market      = unname(market_map[as.character(market)]),
      outcome     = as.character(outcome),
      line        = parse_line(info, market),
      odds_placed = as.numeric(odds),
      p           = as.numeric(probability),
      kelly       = as.numeric(kelly_frac),
      bet_amount  = as.numeric(bet_amount),
      settled     = !is.na(win),
      win         = suppressWarnings(as.logical(win)),
      pnl         = as.numeric(pnl)
    ) |>
    select(placed_at, match_date, sport, country, sex,
           home_team, away_team, market, outcome, line,
           odds_placed, p, kelly, bet_amount,
           settled, win, pnl)

  # Filter out the sex=all settlement partition hack (spec §3.3)
  canonical <- canonical |> filter(sex != "all")

  write_table(canonical, "ledger")
  cli::cli_alert_success("{key}: {nrow(canonical)} ledger rows written")
}

for (key in names(leagues)) etl_league(key, leagues[[key]])
cli::cli_alert_success("Done.")
```

- [ ] **Step 2: Run**

```bash
cd /Users/brynjolfurjonsson/sports
Rscript scripts/etl/04_etl_ledger.R
```

Expected: per-league row counts approximately matching the numbers in `/tmp/legacy-state.md` (Task 1 Step 3). Note that `sex=all` rows are excluded, so counts may be slightly lower than the raw CSV row counts.

- [ ] **Step 3: Commit**

```bash
cd /Users/brynjolfurjonsson/sports
git add scripts/etl/04_etl_ledger.R data/decisions/ledger/
git commit -m "etl: legacy bets_log.csv -> canonical Parquet ledger

Applies spec §3.3 column renames (home_team/away_team/match_date/
odds_placed/p/kelly) and drops the sex=all settlement partition
hack. 'info' is parsed into the numeric 'line' column based on
market type.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>"
```

---

## Task 13: ETL validation (integration test)

**Files:**

- Create: `/Users/brynjolfurjonsson/sports/tests/testthat/test-etl-validation.R`

**Purpose:** Lock down the ETL gate: the Parquet ledger's PnL total must match the legacy CSV ledger's PnL total for every Icelandic league, to the cent.

- [ ] **Step 1: Write the validation test**

Create `/Users/brynjolfurjonsson/sports/tests/testthat/test-etl-validation.R`:

```r
# These tests run against the real on-disk data/ after the ETL scripts have run.
# They live in testthat/ so `devtools::test()` picks them up as a gate.

skip_if_no_legacy <- function() {
  if (!dir.exists(here::here("_legacy", "sports"))) {
    testthat::skip("_legacy/sports/ absent; ETL hasn't run")
  }
}

iceland_leagues <- function() {
  list(
    basketball_iceland = here::here("_legacy", "sports", "basketball", "iceland", "history", "bets_log.csv"),
    handball_iceland   = here::here("_legacy", "sports", "handball",   "iceland", "history", "bets_log.csv"),
    football_iceland   = here::here("_legacy", "sports", "football",   "iceland", "history", "bets_log.csv")
  )
}

test_that("Parquet ledger row count matches legacy (modulo sex=all)", {
  skip_if_no_legacy()

  for (key in names(iceland_leagues())) {
    csv <- iceland_leagues()[[key]]
    if (!file.exists(csv)) next

    legacy <- readr::read_csv(csv, show_col_types = FALSE) |>
      dplyr::filter(sex != "all")

    meta <- strsplit(key, "_")[[1]]
    par <- read_table("ledger",
                      filter = list(sport = meta[1], country = meta[2]))

    expect_equal(nrow(par), nrow(legacy),
                 info = sprintf("%s: parquet=%d legacy(non-all)=%d",
                                key, nrow(par), nrow(legacy)))
  }
})

test_that("Parquet ledger PnL total matches legacy to 0.01", {
  skip_if_no_legacy()

  for (key in names(iceland_leagues())) {
    csv <- iceland_leagues()[[key]]
    if (!file.exists(csv)) next

    legacy_pnl <- readr::read_csv(csv, show_col_types = FALSE) |>
      dplyr::filter(sex != "all") |>
      dplyr::pull(pnl) |>
      sum(na.rm = TRUE)

    meta <- strsplit(key, "_")[[1]]
    par_pnl <- read_table("ledger",
                          filter = list(sport = meta[1], country = meta[2])) |>
      dplyr::pull(pnl) |>
      sum(na.rm = TRUE)

    expect_equal(par_pnl, legacy_pnl, tolerance = 0.01,
                 info = sprintf("%s: parquet=%.2f legacy=%.2f",
                                key, par_pnl, legacy_pnl))
  }
})

test_that("DuckDB view reports same totals as the Parquet via R", {
  skip_if_no_legacy()

  rebuild_duckdb()
  con <- DBI::dbConnect(duckdb::duckdb(),
                        dbdir = here::here("sports.duckdb"),
                        read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  sql_pnl <- DBI::dbGetQuery(con, "SELECT SUM(pnl) AS pnl FROM ledger")$pnl %||% 0
  r_pnl   <- sum(read_table("ledger")$pnl, na.rm = TRUE)

  expect_equal(sql_pnl, r_pnl, tolerance = 0.01)
})

test_that("Odds Parquet has rows for basketball_iceland and handball_iceland", {
  skip_if_no_legacy()
  bb <- read_table("odds", filter = list(sport = "basketball"))
  hb <- read_table("odds", filter = list(sport = "handball"))

  expect_gt(nrow(bb), 0)
  expect_gt(nrow(hb), 0)
})
```

- [ ] **Step 2: Run the full test suite**

```bash
cd /Users/brynjolfurjonsson/sports
Rscript -e 'devtools::test()'
```

Expected: all tests pass. Integration tests (ETL validation) skip cleanly if `_legacy/sports/` is absent (defensive for CI that won't have it).

If any test fails with a PnL mismatch:

- Inspect the failing row set in both Parquet and legacy CSV
- Most likely cause: a `sex = "all"` partition edge case, or an un-settled bet with `win = NA` that got coerced wrong by `as.logical()`
- Fix the ETL script, re-run, re-test. Do not adjust the tolerance.

- [ ] **Step 3: Commit**

```bash
cd /Users/brynjolfurjonsson/sports
git add tests/testthat/test-etl-validation.R
git commit -m "test: ETL validation — Parquet ledger must match legacy PnL

Integration test that runs against the real data/. Gates the ETL:
row counts (modulo sex=all) + PnL totals (tolerance 0.01 ISK) +
DuckDB view parity with the Parquet reader.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>"
```

---

## Task 14: Update top-level CLAUDE.md for the new structure

**Files:**

- Modify: `/Users/brynjolfurjonsson/sports/CLAUDE.md`

**Purpose:** The workspace CLAUDE.md currently describes the four-repo topology. Replace with a placeholder pointing at the spec + plans, and document the new directory layout. A full rewrite is premature (model/decide/placer/publish aren't migrated yet) — do the minimum to prevent confusion for anyone arriving mid-migration.

- [ ] **Step 1: Rewrite CLAUDE.md**

Replace with:

```markdown
# Sports Workspace — CLAUDE.md

> **Migration in progress (2026-04-24).** This workspace is being consolidated
> from four repos into a single monorepo. See
> [docs/superpowers/specs/2026-04-24-sports-pipeline-redesign-design.md](docs/superpowers/specs/2026-04-24-sports-pipeline-redesign-design.md)
> for the end-state design and the `docs/superpowers/plans/` directory for the
> migration plan.

Bayesian sports prediction and automated betting for Icelandic football, basketball, and handball. Consolidated monorepo.

## Current directory structure

```

sports/
├── config/leagues.yml single source of truth (3 active Icelandic leagues)
├── R/ package source (storage so far; model/decide/placer/publish land in later plans)
├── scripts/etl/ one-time migration scripts from \_legacy/
├── Stan/ per-league Stan models (populated in Plan 2)
├── data/ Parquet stores (facts, decisions so far)
├── sports.duckdb derived DuckDB view layer (gitignored, rebuild with rebuild_duckdb())
├── docs/superpowers/ design + implementation plans
└── \_legacy/ preserved histories of sports/, lengjan-odds/, livesport-data/, lengjan-bets/

```

## Quick reference

```

# Load the package for development

Rscript -e 'devtools::load_all()'

# Run tests

Rscript -e 'devtools::test()'

# Rebuild sports.duckdb after fresh Parquet writes

Rscript -e 'sports::rebuild_duckdb()'

# Query any table

Rscript -e '
con <- DBI::dbConnect(duckdb::duckdb(), "sports.duckdb", read_only = TRUE)
DBI::dbGetQuery(con, "SELECT sport, country, SUM(pnl) FROM ledger WHERE settled GROUP BY 1,2")
'

```

## Plans

- Plan 1 (this one): Foundation + Storage + ETL
- Plan 2: Model layer (prepare_data + fit + posteriors for 3 leagues)
- Plan 3: Decide + Placer + Publish
- Plan 4: Ingest (scrapers) + Orchestration + CI + metill-platform integration + cutover

## Obsidian Output

Vault: `Metill` (MCP) / `~/Obsidian/Metill/` (direct path). Prefer MCP `write_note`.
Handoff: `Sports/Sports Handoff.md`.

## Things 3

Route actionable tasks to the **Metill.is** area (ID: `4WyyavEFjCPunRi9iD5tKe`), project **Sports**.
```

- [ ] **Step 2: Commit**

```bash
cd /Users/brynjolfurjonsson/sports
git add CLAUDE.md
git commit -m "docs: rewrite workspace CLAUDE.md for monorepo structure

Points at the spec + plans. Documents the directory layout as it
stands at the end of Plan 1 (storage + ETL only; model/decide/placer/
publish land in Plans 2-4).

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>"
```

---

## Final validation

- [ ] **Step 1: Run everything end-to-end**

```bash
cd /Users/brynjolfurjonsson/sports
Rscript -e 'devtools::load_all(); devtools::test()'
Rscript -e 'sports::rebuild_duckdb()'
```

Expected: all tests pass, `sports.duckdb` regenerates without error.

- [ ] **Step 2: Sanity query**

```bash
cd /Users/brynjolfurjonsson/sports
Rscript -e '
con <- DBI::dbConnect(duckdb::duckdb(), here::here("sports.duckdb"), read_only = TRUE)
print(DBI::dbGetQuery(con, "
  SELECT sport, country, COUNT(*) AS n_bets, SUM(pnl) AS total_pnl
  FROM ledger
  WHERE settled
  GROUP BY 1, 2
  ORDER BY 1, 2
"))
'
```

Expected: one row per (sport, country), with totals matching `/tmp/legacy-state.md` from Task 1.

- [ ] **Step 3: Push to GitHub**

```bash
cd /Users/brynjolfurjonsson/sports
gh repo create metill-is/sports --private --source . --remote origin --push
# (or --public; confirm with user at execution time)
git log --oneline | head -20
```

Expected: repo created, all commits pushed.

---

## What this plan achieves

By the end of Plan 1 you have:

- A single monorepo at `/Users/brynjolfurjonsson/sports` with all four legacy repos' history preserved under `_legacy/`
- A schema-validated `config/leagues.yml` as the single source of truth for the three active Icelandic leagues
- Arrow schemas for all eight storage tables, enforced on every write
- Hive-partitioned Parquet stores for: historical results, schedules, odds (Icelandic leagues only), and the placed-bets ledger
- A DuckDB view layer rebuildable on demand via `rebuild_duckdb()`
- Integration tests that gate the ETL: row counts and PnL totals must match the legacy CSVs to the cent

What you do **not** have yet (next plans):

- The ability to fit a Stan model from the new pipeline (Plan 2)
- The ability to produce recommendations or place bets (Plan 3)
- Automated scrapers, CI workflows, or website data refresh (Plan 4)

Plan 2 begins after Plan 1 is executed, tests pass, and the sanity query in Final Validation matches `/tmp/legacy-state.md`.
````
