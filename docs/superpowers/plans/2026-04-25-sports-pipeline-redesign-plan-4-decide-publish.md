# Sports Pipeline Redesign — Plan 4: Decide + Publish

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build the **decide** layer (joint Kelly + cross-match portfolio + Bayesian calibration → `data/decisions/{candidates,recommendations}/`) and the **publish** layer (website JSON producers in `data/publish/`) for the three active Icelandic leagues. Defer the placer (real-money browser automation) to Plan 5 to keep this plan focused on pure compute that builds cleanly on Plans 1–3.

**Architecture:** Five flat R files for decide (one per concern) + one publish file per sport. Decide is a pure pipeline: `decide_league(league_key, sex)` reads `beliefs/latest/`, `facts/odds/`, and `decisions/ledger/`; passes per-match posterior draws + odds to `kelly_joint()`; then `portfolio_optimise()` for cross-match scaling subject to daily budget; finally applies a sex-specific Bayesian calibration multiplier from the settled ledger. Both `candidates` (every stage including drops) and `recommendations` (post-filter winners) are written via `write_table()`. Publish is also a pure transformer: reads beliefs + facts + recommendations and writes JSON snapshots that the metill-platform website pulls.

The legacy decide implementation lives at `_legacy/sports/R/bets/` with `box::use()` modules and `withr::with_dir()` path-hacking; the port drops both. Joint Kelly is the only mode (per memory note 2026-03-06 — `kelly_mode` config key removed). Per-sex `kelly_frac` is preserved (e.g. football_iceland female uses half-stake after the 2026-04-19 PnL audit).

**Tech Stack:** R (≥ 4.0), `{nloptr}` (Kelly + portfolio convex optimisation), `{jsonlite}` (publish JSON), `{jsonvalidate}` (publish JSONSchema gate), Plan 1–3 stack (`{arrow}`, `{dplyr}`, `{cmdstanr}` etc), `{testthat}` ed 3. New runtime dep: `nloptr` (already in `_legacy/` so should already be installed locally).

**Scope (Plan 4):**

- `config/bankroll.yml` — global Kelly cap + daily budget
- `config/leagues.yml` — `betting:` field expanded to cover ev_threshold, kelly_frac per sex, daily_budget, market toggles, scoring (has_ties)
- `config/leagues.schema.json` — JSON Schema validates the expanded shape
- `R/decide-odds.R` — `prepare_odds(league, sex)` reads `facts/odds`, parses handicap strings, returns canonical (match × market × outcome × line) tibble
- `R/decide-kelly.R` — `kelly_joint(beliefs, bets)` ports `_legacy/.../kelly_joint.R::get_kelly_joint()` + `build_return_matrix()`. Per-match bet package.
- `R/decide-portfolio.R` — `portfolio_optimise(packages, max_daily, mode)` ports `_legacy/.../portfolio.R`. Cross-match scaling.
- `R/decide-calibration.R` — `compute_calibration(league, sex)` reads `decisions/ledger/`, returns Beta-Binomial multiplier per sex.
- `R/decide-pipeline.R` — `decide_league(league_key, sex, run_date, root)` orchestrator. Writes `candidates` + `recommendations` Parquet via `write_table()`.
- `R/publish-football-iceland.R` — full port of `_legacy/sports/football/iceland/R/export_website_data.R`. 7 JSONs per sex (meta, next_games, standings, team_strengths, final_positions, points_distribution, home_advantage).
- `R/publish-basketball-iceland.R` + `R/publish-handball-iceland.R` — scaffolds. Produce `meta.json` + `next_games.json` (the two trivially-portable ones); leave the league-table / season-projection JSONs as `# TODO Plan 6` stubs.
- `scripts/decide_all.R` — backfill entrypoint, loops active (league, sex) × `decide_league()`.
- `scripts/publish_all.R` — backfill entrypoint, loops active leagues × publish.
- Fixture-based unit tests for each decide module + per-sport publish output snapshot test.
- Validation gate: replay `decide_league()` against a historical ledger date; recommendations within tolerance of legacy `bets_log.csv` for that date.
- Update `CLAUDE.md` (Plan 4 row → ✅; new R/decide-*.R + R/publish-*.R + scripts/ entries).

**Out of scope (still):**

- **Placer** (`R/placer/` — Lengjan browser automation, real-money) — Plan 5
- **Orchestration via `{targets}`, CI workflows, metill-platform integration, cutover** — Plan 6
- New Kelly variants (`kelly_cvxr.R` is research code in `_legacy/`; keep there)
- CLV tracker, ROI report, settlement (`clv_tracker.R`, `roi_report.R`, `settle.R` — Plan 5+ when placer revives them)
- Reviving paused non-Icelandic leagues
- Any betting-policy change vs current production

---

## File structure created by this plan

```
sports/
├── config/
│   ├── bankroll.yml                # Global Kelly cap + daily budget (NEW)
│   ├── leagues.yml                 # `betting:` field expanded
│   └── leagues.schema.json         # Validates expanded shape
├── R/
│   ├── decide-odds.R               # prepare_odds(league, sex)
│   ├── decide-kelly.R              # kelly_joint(beliefs, bets)
│   ├── decide-portfolio.R          # portfolio_optimise(packages, max_daily)
│   ├── decide-calibration.R        # compute_calibration(league, sex)
│   ├── decide-pipeline.R           # decide_league(league_key, sex)
│   ├── publish-football-iceland.R  # 7 JSONs per sex
│   ├── publish-basketball-iceland.R  # 2 JSONs scaffold
│   └── publish-handball-iceland.R    # 2 JSONs scaffold
├── scripts/
│   ├── decide_all.R                # Backfill recommendations
│   └── publish_all.R               # Backfill JSON
├── tests/testthat/
│   ├── fixtures/decide/            # Tiny posterior + odds fixtures
│   ├── fixtures/publish/           # Cached JSON snapshots
│   ├── test-decide-odds.R
│   ├── test-decide-kelly.R
│   ├── test-decide-portfolio.R
│   ├── test-decide-calibration.R
│   ├── test-decide-pipeline.R
│   ├── test-decide-validation.R    # Historical-replay gate (skips on CI)
│   ├── test-publish-football.R
│   ├── test-publish-basketball.R   # Smoke-only
│   └── test-publish-handball.R     # Smoke-only
└── data/
    ├── decisions/
    │   ├── candidates/sport=X/country=Y/run_date=YYYY-MM-DD/  # All stages
    │   └── recommendations/sport=X/country=Y/run_date=YYYY-MM-DD/  # Post-filter
    └── publish/
        ├── football/iceland/{karla,kvenna}/*.json
        ├── basketball/iceland/{karla,kvenna}/*.json
        └── handball/iceland/{karla,kvenna}/*.json
```

---

## Task 1: Config — `bankroll.yml` + expand `betting:` in `leagues.yml`

**Files:**

- Create: `config/bankroll.yml`
- Modify: `config/leagues.yml` — expand `betting:` per league
- Modify: `config/leagues.schema.json` — validate expanded shape
- Modify: `R/config.R` — add `load_bankroll()` exported fn; extend `validate_leagues()` to cover the new `betting:` keys

**Purpose:** Decide layer needs (a) global daily-budget cap and (b) per-league policy (Kelly fraction per sex, EV threshold, market toggles, scoring rules). Spec §3.6 puts everything in `leagues.yml`; `bankroll.yml` is one extra global file (small, intentional split — bankroll changes monthly, league policy rarely).

- [ ] **Step 1: Create `config/bankroll.yml`**

```yaml
# config/bankroll.yml — global betting bankroll + daily-budget cap.
#
# `initial_pool` — known deposits at workspace start (matches the figure in
#   /Users/brynjolfurjonsson/.claude/projects/-Users-brynjolfurjonsson-sports/memory/MEMORY.md, set 2026-03-06 = 23,610 ISK).
# `current_pool` — runtime bankroll derived from `initial_pool` + ledger PnL,
#   computed by `load_bankroll()` if `current_pool` is missing here.
# `daily_budget_frac` — max fraction of current bankroll wagered per day. Caps
#   the Stage-2 portfolio optimiser's total stake.
# `daily_budget_min_isk` — never under-bet on a quiet day; floor in absolute ISK
#   so a low-volume day doesn't turn into 100-ISK bets due to fractional caps.
initial_pool: 23610
daily_budget_frac: 0.05
daily_budget_min_isk: 1000
```

- [ ] **Step 2: Expand `betting:` per league in `leagues.yml`**

Replace each league's existing `betting:` block. Football example:

```yaml
football_iceland:
  # ... (unchanged sport, country, sexes, active, data_source, lengjan, stan_model)
  betting:
    kelly_frac:
      male: 0.15
      female: 0.075   # half stake after 2026-04-19 audit (memory: feedback_calibration_aggregates)
    ev_threshold: 0.0
    markets:
      moneyline: true
      spread: true
      total: true
    scoring:
      has_ties: true
      tie_threshold: 0
    min_bet: 200
    max_age_hours: 48     # beliefs older than this are stale; pipeline skips
```

For basketball + handball Iceland: `kelly_frac` may be a single number (no per-sex split — basketball today uses 0.10 across both sexes per current `leagues.yml`). The schema accepts either form via `oneOf`.

Document in this commit what each new key controls. The schema (next step) gates structural drift.

- [ ] **Step 3: Update `config/leagues.schema.json`**

Add:

```json
"betting": {
  "type": "object",
  "additionalProperties": false,
  "required": ["kelly_frac", "ev_threshold", "markets", "scoring", "min_bet"],
  "properties": {
    "kelly_frac": {
      "oneOf": [
        { "type": "number", "minimum": 0, "maximum": 1 },
        {
          "type": "object",
          "additionalProperties": false,
          "properties": {
            "male":   { "type": "number", "minimum": 0, "maximum": 1 },
            "female": { "type": "number", "minimum": 0, "maximum": 1 }
          }
        }
      ]
    },
    "ev_threshold":  { "type": "number" },
    "markets": {
      "type": "object",
      "additionalProperties": false,
      "properties": {
        "moneyline": { "type": "boolean" },
        "spread":    { "type": "boolean" },
        "total":     { "type": "boolean" }
      }
    },
    "scoring": {
      "type": "object",
      "additionalProperties": false,
      "properties": {
        "has_ties":      { "type": "boolean" },
        "tie_threshold": { "type": "number" }
      }
    },
    "min_bet":        { "type": "number", "minimum": 0 },
    "max_age_hours":  { "type": "number", "minimum": 0 }
  }
}
```

- [ ] **Step 4: Add `load_bankroll()` to `R/config.R`**

```r
#' Load + validate the global bankroll config.
#'
#' Reads `config/bankroll.yml`. If `current_pool` is missing from the YAML
#' (the usual case), derives it as `initial_pool + sum(ledger$pnl[settled])`
#' so the bankroll evolves with realised PnL.
#'
#' @return List with `initial_pool`, `current_pool`, `daily_budget_frac`,
#'   `daily_budget_min_isk`.
#' @export
load_bankroll <- function(path = here::here("config", "bankroll.yml"),
                          ledger_root = here::here("data")) {
  cfg <- yaml::yaml.load(readr::read_file(path))
  if (is.null(cfg$current_pool)) {
    led <- tryCatch(
      read_table("ledger"),
      error = function(e) tibble::tibble(pnl = numeric(), settled = logical())
    )
    realised_pnl <- sum(led$pnl[isTRUE(led$settled)], na.rm = TRUE)
    cfg$current_pool <- cfg$initial_pool + realised_pnl
  }
  cfg
}
```

- [ ] **Step 5: Tests**

```r
# tests/testthat/test-config-bankroll.R
test_that("load_bankroll returns expected fields", {
  cfg <- load_bankroll()
  expect_named(cfg, c("initial_pool", "current_pool",
                      "daily_budget_frac", "daily_budget_min_isk"),
               ignore.order = TRUE)
  expect_gt(cfg$current_pool, 0)
})

# tests/testthat/test-config-betting-schema.R
test_that("expanded betting block validates against schema", {
  expect_no_error(load_leagues())   # validates internally
  leagues <- load_leagues()
  expect_true(all(vapply(leagues, function(l) !is.null(l$betting$kelly_frac),
                         logical(1))))
})

test_that("malformed betting block fails validation", {
  bad <- yaml::as.yaml(list(
    bad_league = list(
      sport = "football", country = "iceland", sexes = list("male"),
      active = TRUE,
      data_source = list(results = "x", schedule = "x", odds = "y"),
      stan_model = "x.stan",
      betting = list(kelly_frac = 2.5)   # > 1, invalid
    )
  ))
  tmp <- withr::local_tempfile(fileext = ".yml")
  writeLines(bad, tmp)
  expect_error(load_leagues(path = tmp), "kelly_frac|maximum")
})
```

- [ ] **Step 6: Verify**

```bash
Rscript -e 'devtools::test(filter = "config-bankroll|config-betting-schema")'
```

Expected: 4 passes.

- [ ] **Step 7: Commit**

```bash
git add config/ R/config.R tests/testthat/test-config-bankroll.R \
        tests/testthat/test-config-betting-schema.R NAMESPACE DESCRIPTION
git commit -m "feat: bankroll.yml + expanded betting: leagues.yml schema

Adds config/bankroll.yml (initial_pool 23,610 ISK, daily_budget_frac
0.05, daily_budget_min_isk 1,000). Expands per-league betting config
to cover kelly_frac (per-sex object form), ev_threshold, markets
(boolean toggles), scoring (has_ties, tie_threshold), min_bet,
max_age_hours. JSON Schema gates the shape.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>"
```

---

## Task 2: `R/decide-odds.R` — odds → canonical (match × market × outcome × line)

**Files:**

- Create: `R/decide-odds.R`
- Create: `tests/testthat/fixtures/decide/mini_odds.parquet`
- Create: `tests/testthat/test-decide-odds.R`

**Purpose:** Read `data/facts/odds/` for a (sport, country, sex), grab the most recent odds per (match, market, outcome, line), parse handicap strings (`"0-1"` → `-1`), and return a tidy bet-list ready for `kelly_joint()`. Replaces `_legacy/sports/R/bets/odds.R` which both fetched + parsed; here we only parse because `facts/odds` already has the latest scrape.

**Signature:**

```r
prepare_odds <- function(league, sex,
                         end_date = Sys.Date(),
                         max_age_hours = 48,
                         root = here::here("data")) -> tibble
```

Returns columns: `match_date`, `home_team`, `away_team`, `market` ∈ {moneyline, spread, total}, `outcome` ∈ {home, draw, away, over, under}, `line`, `odds`, `scraped_at`. One row per (match × market × outcome × line) using the most recent `scraped_at` snapshot per group.

- [ ] **Step 1: Create the mini odds fixture**

Run interactively:

```r
library(arrow); library(tibble)
mini <- tibble::tibble(
  sport = "basketball", country = "iceland",
  scraped_at = as.POSIXct(c("2026-04-25 08:00", "2026-04-25 12:00",
                            "2026-04-25 12:00"), tz = "UTC"),
  match_date = as.Date(c("2026-04-26", "2026-04-26", "2026-04-26")),
  home_team = c("Alpha", "Alpha", "Alpha"),
  away_team = c("Bravo", "Bravo", "Bravo"),
  market    = c("moneyline", "moneyline", "spread"),
  outcome   = c("home", "away", "home"),
  line      = c(NA_real_, NA_real_, -3.5),
  odds      = c(1.85, 2.10, 1.95)
)
arrow::write_parquet(mini, "tests/testthat/fixtures/decide/mini_odds.parquet")
```

- [ ] **Step 2: Failing tests**

```r
# tests/testthat/test-decide-odds.R
setup_odds_root <- function() {
  tmp <- withr::local_tempdir(.local_envir = parent.frame())
  odds <- arrow::read_parquet(testthat::test_path("fixtures", "decide",
                                                   "mini_odds.parquet"))
  write_table(odds, "odds", root = tmp)
  tmp
}

test_that("prepare_odds returns the latest odds per (match, market, outcome, line)", {
  root <- setup_odds_root()
  league <- list(sport = "basketball", country = "iceland")

  out <- prepare_odds(league, sex = "male",
                      end_date = as.Date("2026-04-25"),
                      max_age_hours = 24, root = root)

  expect_named(out, c("match_date", "home_team", "away_team",
                      "market", "outcome", "line", "odds", "scraped_at"),
               ignore.order = TRUE)
  # All rows from the same scrape — no dedup needed; expect 3 rows.
  expect_equal(nrow(out), 3L)
})

test_that("prepare_odds drops odds older than max_age_hours", {
  root <- setup_odds_root()
  league <- list(sport = "basketball", country = "iceland")

  out <- prepare_odds(league, sex = "male",
                      end_date = as.Date("2026-04-25"),
                      max_age_hours = 1, root = root)
  expect_equal(nrow(out), 0L)   # all scrapes > 1h before now
})

test_that("parse_handicap converts Lengjan score-style strings to signed numeric", {
  expect_equal(parse_handicap(c("0-1", "1-0", "0-2")),  c(-1, 1, -2))
  expect_warning(parse_handicap("not-a-handicap"), "Could not parse")
})
```

- [ ] **Step 3: Verify failure**

```bash
Rscript -e 'devtools::test(filter = "decide-odds")'
```

- [ ] **Step 4: Implement `R/decide-odds.R`**

```r
#' @include storage.R
NULL

#' Parse a Lengjan handicap string ("0-1") into signed numeric (-1).
#' Positive = home gets head start; negative = away gets head start.
#' @export
parse_handicap <- function(change_str) {
  parts <- stringr::str_split_fixed(change_str, "-", n = 2)
  result <- as.numeric(parts[, 1]) - as.numeric(parts[, 2])
  if (any(is.na(result))) {
    warning("Could not parse handicap values: ",
            paste(change_str[is.na(result)], collapse = ", "), call. = FALSE)
  }
  result
}

#' Read facts/odds for one (sport, country) and return latest-snapshot rows
#' for matches at or after `end_date`, no older than `max_age_hours`.
#'
#' @param league List with `sport` + `country`.
#' @param sex   "male" or "female". (Lengjan odds are sex-agnostic per
#'   competition; the parameter is here for symmetry but does not filter.
#'   Sex-aware filtering happens at the kelly_joint stage where beliefs are
#'   sex-keyed.)
#' @param end_date Drop matches before this date.
#' @param max_age_hours Drop scrapes older than this (vs `Sys.time()`).
#' @return Tibble with (match_date, home_team, away_team, market, outcome,
#'   line, odds, scraped_at).
#' @export
prepare_odds <- function(league, sex,
                         end_date = Sys.Date(),
                         max_age_hours = 48,
                         root = here::here("data")) {
  raw <- read_table("odds",
                    root = root,
                    filter = list(sport = league$sport, country = league$country))

  if (nrow(raw) == 0L) return(empty_odds())

  cutoff_t <- Sys.time() - lubridate::dhours(max_age_hours)
  raw <- raw[raw$match_date >= end_date &
             raw$scraped_at  >= cutoff_t, , drop = FALSE]

  if (nrow(raw) == 0L) return(empty_odds())

  # Dedup to latest scrape per (match × market × outcome × line)
  raw |>
    dplyr::group_by(.data$match_date, .data$home_team, .data$away_team,
                    .data$market, .data$outcome, .data$line) |>
    dplyr::slice_max(.data$scraped_at, n = 1L, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(.data$match_date, .data$home_team, .data$away_team,
                  .data$market, .data$outcome, .data$line, .data$odds,
                  .data$scraped_at)
}

#' @keywords internal
#' @noRd
empty_odds <- function() {
  tibble::tibble(
    match_date = as.Date(character()),
    home_team  = character(), away_team = character(),
    market     = character(), outcome   = character(),
    line       = numeric(), odds       = numeric(),
    scraped_at = as.POSIXct(character(), tz = "UTC")
  )
}
```

- [ ] **Step 5: Verify + commit**

```bash
Rscript -e 'roxygen2::roxygenise(); roxygen2::update_collate(".")'
Rscript -e 'devtools::test(filter = "decide-odds")'
```

Expected: 3 passes (or 4 with the warning test).

```bash
git add R/decide-odds.R NAMESPACE DESCRIPTION \
        tests/testthat/fixtures/decide/ tests/testthat/test-decide-odds.R
git commit -m "feat: R/decide-odds.R — facts/odds -> canonical bet list

prepare_odds(league, sex, end_date, max_age_hours) reads
data/facts/odds/, drops stale scrapes (max_age_hours from now),
keeps only the latest snapshot per (match × market × outcome ×
line), and returns a tidy tibble ready for kelly_joint(). Also
exports parse_handicap() for the shared score-style string format.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>"
```

---

## Task 3: `R/decide-kelly.R` — joint Kelly per match (TDD)

**Files:**

- Create: `R/decide-kelly.R`
- Create: `tests/testthat/test-decide-kelly.R`

**Purpose:** Port `_legacy/sports/R/bets/kelly_joint.R::get_kelly_joint()` + `build_return_matrix()`. Joint Kelly is the only mode (per memory note 2026-03-06).

**Contract:**

```r
kelly_joint(beliefs, bets, kelly_frac = 0.10, ev_threshold = 0.0,
            tie_threshold = 0) -> list(
  bets         = <tibble: input bets + p, ev, kelly_raw>,
  match_pnl    = <numeric S>,
  match_kelly_sum = <numeric>,
  diagnostics  = <list>
)
```

- `beliefs` — tibble of MCMC draws for ONE match: columns `draw_id`, `home_goals`, `away_goals`. Typically 4000 rows.
- `bets`    — tibble of bet options for that match: columns `market`, `outcome`, `line`, `odds`. Marketwise filtered upstream by `betting$markets` toggles.
- Returns the bet table augmented with model probability `p` (frac of draws where bet wins), `ev` (`p * (odds-1) - (1-p)`), and the convex-optimised joint `kelly_raw` (per bet).

The internal helpers are:

- `build_return_matrix(beliefs, bets)` — S × B matrix, entry (s, j) = net return of bet j under draw s.
- `solve_kelly_joint(net_return, max_stake)` — convex optimisation via `nloptr` (maximise `mean(log(1 + R %*% f))`).

- [ ] **Step 1: Write failing tests** with hand-computed posteriors.

```r
# tests/testthat/test-decide-kelly.R
mini_beliefs <- function(n_draws = 1000L) {
  tibble::tibble(
    draw_id = seq_len(n_draws),
    home_goals = rpois(n_draws, lambda = 1.5),
    away_goals = rpois(n_draws, lambda = 1.0)
  )
}

test_that("build_return_matrix produces an S x B numeric matrix", {
  draws <- mini_beliefs(500L)
  bets <- tibble::tibble(
    market = c("moneyline", "moneyline", "moneyline"),
    outcome = c("home", "draw", "away"),
    line = NA_real_,
    odds = c(2.0, 3.5, 4.2)
  )
  R <- build_return_matrix(draws, bets)
  expect_equal(dim(R), c(500L, 3L))
  expect_true(all(R %in% c(-1, bets$odds - 1)))
})

test_that("kelly_joint returns kelly_raw weights summing within max_stake", {
  set.seed(7)
  draws <- mini_beliefs(2000L)
  # Construct bets where home is favourite at slight ev>0
  bets <- tibble::tibble(
    market = c("moneyline", "moneyline", "moneyline"),
    outcome = c("home", "draw", "away"),
    line = NA_real_,
    odds = c(1.85, 4.0, 4.5)
  )
  out <- kelly_joint(draws, bets, kelly_frac = 1.0, ev_threshold = 0.0)
  expect_named(out, c("bets", "match_pnl", "match_kelly_sum", "diagnostics"),
               ignore.order = TRUE)
  expect_named(out$bets, c("market", "outcome", "line", "odds",
                           "p", "ev", "kelly_raw"), ignore.order = TRUE)
  expect_lte(sum(pmax(out$bets$kelly_raw, 0)), 1.0 + 1e-6)
  expect_equal(length(out$match_pnl), nrow(draws))
})

test_that("kelly_joint zeroes bets below ev_threshold", {
  set.seed(7)
  draws <- mini_beliefs(2000L)
  # Construct bets all with ev < 0
  bets <- tibble::tibble(
    market = "moneyline",
    outcome = c("home", "draw", "away"),
    line = NA_real_,
    odds = c(1.10, 1.10, 1.10)   # all priced rich; ev clearly negative
  )
  out <- kelly_joint(draws, bets, ev_threshold = 0.0)
  expect_true(all(out$bets$kelly_raw <= 1e-6))
})

test_that("kelly_joint handles spread bets via line argument", {
  set.seed(7)
  draws <- mini_beliefs(2000L)
  bets <- tibble::tibble(
    market = c("spread", "spread"),
    outcome = c("home", "away"),
    line = c(-1.5, 1.5),
    odds = c(1.95, 1.95)
  )
  out <- kelly_joint(draws, bets, kelly_frac = 1.0)
  # The two bets are EXACTLY mutually exclusive; sum of p must be 1
  # (modulo ties-counted-as-loss for both, which we exclude in football)
  expect_lte(abs(sum(out$bets$p) - 1.0), 0.05)
})
```

- [ ] **Step 2: Verify failure**

- [ ] **Step 3: Implement `R/decide-kelly.R`**

Port `_legacy/sports/R/bets/kelly_joint.R` lines 50–250. Drop `box::use`. Concrete:

```r
#' Build the S x B net-return matrix.
#'
#' For each posterior draw s and each bet j, returns the bet's net return:
#' (odds_j - 1) if bet j wins, else -1.
#'
#' Bet-resolution by market:
#' - moneyline: outcome == "home" wins iff home_goals > away_goals (and similarly)
#' - spread:    outcome == "home" wins iff (home_goals + line) > away_goals
#' - total:     outcome == "over" wins iff (home_goals + away_goals) > line
#'
#' @keywords internal
#' @noRd
build_return_matrix <- function(beliefs, bets) {
  S <- nrow(beliefs); B <- nrow(bets)
  R <- matrix(NA_real_, nrow = S, ncol = B)
  hg <- beliefs$home_goals; ag <- beliefs$away_goals; total <- hg + ag

  for (j in seq_len(B)) {
    win <- switch(bets$market[[j]],
      moneyline = switch(bets$outcome[[j]],
        home = hg >  ag,
        draw = hg == ag,
        away = hg <  ag
      ),
      spread = if (bets$outcome[[j]] == "home")
                 (hg + bets$line[[j]]) >  ag
               else
                 (ag - bets$line[[j]]) >  hg,
      total  = if (bets$outcome[[j]] == "over")
                 total >  bets$line[[j]]
               else
                 total <  bets$line[[j]],
      stop("Unknown market: ", bets$market[[j]], call. = FALSE)
    )
    R[, j] <- ifelse(win, bets$odds[[j]] - 1, -1)
  }
  R
}

#' Convex-optimise joint Kelly stakes f >= 0 maximising
#' E[log(1 + R %*% f)] subject to sum(f) <= max_stake.
#' @keywords internal
#' @noRd
solve_kelly_joint <- function(net_return, max_stake) {
  B <- ncol(net_return)
  if (B == 0L) return(numeric(0))

  # Objective: -mean(log(1 + R %*% f)) (minimise negative log-growth)
  obj <- function(f) {
    g <- log1p(as.numeric(net_return %*% f))
    if (any(!is.finite(g))) return(Inf)
    -mean(g)
  }

  # Gradient (analytical): d(-mean(log(1 + R%*%f))) / df_j = -mean(R[,j] / (1 + R%*%f))
  grad <- function(f) {
    Rf <- as.numeric(net_return %*% f)
    if (any(Rf <= -1)) return(rep(NA_real_, length(f)))
    -as.numeric(crossprod(net_return, 1 / (1 + Rf))) / nrow(net_return)
  }

  res <- nloptr::nloptr(
    x0          = rep(0, B),
    eval_f      = obj,
    eval_grad_f = grad,
    lb          = rep(0, B),
    ub          = rep(max_stake, B),
    eval_g_ineq = function(f) sum(f) - max_stake,
    eval_jac_g_ineq = function(f) matrix(1, nrow = 1, ncol = B),
    opts = list(algorithm = "NLOPT_LD_SLSQP", xtol_rel = 1e-7, maxeval = 500)
  )
  pmax(res$solution, 0)
}

#' Joint Kelly for a single match.
#'
#' @param beliefs Tibble with `draw_id`, `home_goals`, `away_goals`.
#' @param bets    Tibble with `market`, `outcome`, `line`, `odds`.
#' @param kelly_frac Max total fraction of bankroll for this match.
#' @param ev_threshold Min EV per bet to consider.
#' @return List with `bets` (input + p + ev + kelly_raw), `match_pnl`,
#'   `match_kelly_sum`, `diagnostics`.
#' @export
kelly_joint <- function(beliefs, bets,
                        kelly_frac = 0.10,
                        ev_threshold = 0.0,
                        tie_threshold = 0) {
  R <- build_return_matrix(beliefs, bets)
  p <- as.numeric(colMeans(R > 0))
  ev <- p * (bets$odds - 1) - (1 - p)

  keep <- ev >= ev_threshold
  R_keep <- R[, keep, drop = FALSE]

  f <- numeric(nrow(bets))
  if (sum(keep) > 0L) {
    f[keep] <- solve_kelly_joint(R_keep, max_stake = kelly_frac)
  }

  match_pnl <- as.numeric(R %*% f)

  list(
    bets = tibble::tibble(
      market = bets$market, outcome = bets$outcome, line = bets$line,
      odds = bets$odds, p = p, ev = ev, kelly_raw = f
    ),
    match_pnl       = match_pnl,
    match_kelly_sum = sum(f),
    diagnostics     = list(n_bets = nrow(bets),
                           n_kept = sum(keep),
                           kelly_frac = kelly_frac)
  )
}
```

- [ ] **Step 4: Verify + commit** as `feat: R/decide-kelly.R — joint Kelly per match`.

---

## Task 4: `R/decide-portfolio.R` — cross-match scaling subject to daily budget

**Files:**

- Create: `R/decide-portfolio.R`
- Create: `tests/testthat/test-decide-portfolio.R`

**Purpose:** Port `_legacy/sports/R/bets/portfolio.R::portfolio_optimize()`. Given each match's `match_pnl` vector + `match_kelly_sum`, find per-match scale factors `λ_m ∈ [0, 1]` that maximise total expected log-growth subject to `Σ λ_m * match_kelly_sum_m ≤ max_daily`.

**Contract:**

```r
portfolio_optimise(packages, max_daily, mode = "optimal") -> list(
  lambdas     = <named numeric M>,
  diagnostics = list(...)
)
```

`packages` is a list of M match-packages each with `match_key`, `match_pnl`, `match_kelly_sum`. The `mode` switches between `"proportional"` (uniform scaling, fast) and `"optimal"` (convex optimisation across packages, slower but better for capacity-bound days).

- [ ] **Step 1: Failing tests** — construct 2 matches with PnL vectors that have negatively-correlated returns; assert that `optimal` mode finds different λ for the two matches than `proportional`.

- [ ] **Step 2-4: Implement** by porting `_legacy/.../portfolio.R` lines 50–180 verbatim modulo `box::use` removal.

- [ ] **Step 5: Verify + commit** as `feat: R/decide-portfolio.R — cross-match scaling`.

---

## Task 5: `R/decide-calibration.R` — Beta-Binomial multiplier from settled ledger

**Files:**

- Create: `R/decide-calibration.R`
- Create: `tests/testthat/test-decide-calibration.R`

**Purpose:** Port `_legacy/sports/R/bets/calibration.R::compute_calibration()`. Reads `data/decisions/ledger/`, filters to (sport, country, sex, settled), computes a Beta-Binomial pseudo-count multiplier:

```
multiplier = (prior_weight * prior_ratio + sum(win)) / (prior_weight + sum(p))
```

Where `prior_ratio = 1.0` (model is calibrated by default) and `prior_weight = 30` (so 30 settled bets equally weighted with prior).

**Contract:**

```r
compute_calibration(league, sex, root = here::here("data"),
                    prior_weight = 30, prior_ratio = 1.0) -> numeric(1)
```

- [ ] **Step 1: Failing tests** — feed a synthetic ledger with known win-rate / probability mismatch; assert the multiplier matches the closed-form formula within 1e-9.

- [ ] **Step 2-4: Implement** by porting `_legacy/.../calibration.R` lines 30–100 verbatim modulo `box::use`.

- [ ] **Step 5: Edge case** — empty ledger should return `prior_ratio` (no signal → prior wins).

- [ ] **Step 6: Verify + commit** as `feat: R/decide-calibration.R — Bayesian calibration multiplier`.

---

## Task 6: `R/decide-pipeline.R::decide_league()` — orchestrator + write tables

**Files:**

- Create: `R/decide-pipeline.R`
- Create: `tests/testthat/test-decide-pipeline.R`

**Purpose:** Stitch Tasks 2–5 together. The end-to-end contract:

```r
decide_league(league_key = NULL, league = NULL, sex,
              run_date = Sys.Date(),
              root      = here::here("data"),
              bankroll  = NULL,        # NULL = load_bankroll()
              write     = TRUE) -> invisible(tibble of recommendations)
```

Steps:

1. Load `league` from `load_leagues()` if `league_key` set.
2. Read `beliefs_latest` for (league, sex) — fail if empty (Plan 3 must have run).
3. `prepare_odds()` → bet list.
4. Apply `betting$markets` toggles; drop bets in disabled markets.
5. For each match in beliefs ∩ odds: call `kelly_joint()`. Collect packages.
6. `portfolio_optimise()` to get λ per match.
7. `compute_calibration(league, sex)` → multiplier.
8. Final stake per bet = `kelly_raw * λ * calibration_multiplier * bankroll$current_pool`.
9. Apply `min_bet` floor (drop bets below it).
10. Round to whole ISK.
11. Write `candidates` (every stage with `stage` column ∈ {candidate, post_portfolio, post_calibration, kept, dropped_min_bet}) and `recommendations` (post-filter winners only) Parquet via `write_table()` with partition `(sport, country, run_date)`.
12. Return the recommendations tibble invisibly.

The `candidates` table is for research (every drop is logged). The `recommendations` table is what Plan 5's placer reads.

- [ ] **Step 1: Failing test** that mocks `kelly_joint`, `portfolio_optimise`, `compute_calibration` (each independently unit-tested) and asserts the orchestrator wires them correctly + writes both tables.

- [ ] **Step 2-5: Implement** the orchestrator. Heavy on plumbing, light on logic since the modules are already tested.

- [ ] **Step 6: Verify + commit** as `feat: decide_league() orchestrator — Kelly + portfolio + calibration`.

---

## Task 7: Decide validation gate — historical replay vs ledger

**Files:**

- Create: `tests/testthat/test-decide-validation.R`

**Purpose:** Spec §4.4 says "must match current production". The pre-migration `recommendations.csv` is empty (1 line — the workspace recently emptied it), so a snapshot-equality test isn't possible. Instead, gate on the historical ledger:

For each settled bet in `data/decisions/ledger/` from a recent date:

1. Reconstruct the input state at that bet's `placed_at` (use `beliefs_archive` if available for that fit_date, else skip the row).
2. Run `decide_league()` for that league × sex × date.
3. Look up the corresponding bet in the new `recommendations`. Assert `bet_amount` is within ± 25% of the legacy `bet_amount` (modelling drift acceptable at this magnitude given new beliefs ≠ frozen legacy beliefs).

If `data/beliefs/archive/` only has 2026-04-24/2026-04-25 entries (likely until walk-forward research starts collecting more), the test will skip with `"insufficient archive coverage"`. That's correct — without lookahead-free reconstruction the test can't run.

- [ ] **Step 1: Write the test, primarily as a skip-friendly stub.**

- [ ] **Step 2: Run on local** — likely all skip on this machine (only 1–2 archive partitions yet). That's fine; the test wires up correctly for when archive coverage grows in future plans.

- [ ] **Step 3: Commit** as `test: decide validation gate — replay against ledger`.

---

## Task 8: `R/publish-football-iceland.R` — full port

**Files:**

- Create: `R/publish-football-iceland.R`
- Create: `tests/testthat/fixtures/publish/football_iceland_male_*.json` (golden snapshots from local run)
- Create: `tests/testthat/test-publish-football.R`

**Purpose:** Port `_legacy/sports/football/iceland/R/export_website_data.R` (583 lines). Produces 7 JSONs per sex into `data/publish/football/iceland/{karla,kvenna}/`:

- `meta.json` — generation metadata (fit_date, model name, n_draws)
- `next_games.json` — posterior over goal-diffs for upcoming matches
- `standings.json` — current league table (male only, top division — depends on `facts/results`)
- `team_strengths.json` — per-team home/away × offence/defence/total CIs (extracted from fit MCMC, requires keeping the fit object accessible — see below)
- `final_positions.json` — placement probability matrix + top-six probs (Monte Carlo sim from posterior)
- `points_distribution.json` — per-team end-of-season points distribution
- `home_advantage.json` — per-team home-advantage effects

**Key design choice:** `team_strengths`, `final_positions`, `points_distribution`, and `home_advantage` need access to the fit's full posterior (off0, def0, etc — not just `goals_pred`). Plan 3's `beliefs_latest` only stores predictive draws. Two options:

- **Option A** (recommended for Plan 4): publish runs after a fresh fit and is given the in-memory `fit` object. Plan 5's `{targets}` DAG wires this. For now, `publish_football_iceland(fit, league, sex)` accepts the fit directly.
- **Option B** (deferred): expand `beliefs_latest` schema with team strengths. Bigger schema change; Plan 5 territory.

Use Option A. `fit` is the same `CmdStanMCMC` returned by `fit_model()`. Publish calls `fit$draws("off0")` etc directly. The `scripts/publish_all.R` will refit if needed before publishing.

**Signature:**

```r
publish_football_iceland(fit, league, sex,
                          beliefs_root = here::here("data"),
                          output_root  = here::here("data", "publish")) -> invisible(NULL)
```

- [ ] **Step 1: Read `_legacy/sports/football/iceland/R/export_website_data.R` end-to-end. Identify the per-JSON build functions.**

- [ ] **Step 2: Write a skeleton** `publish_football_iceland(fit, league, sex)` that calls per-JSON helpers (`build_meta_json`, `build_next_games_json`, etc) and writes each.

- [ ] **Step 3-7: Implement each per-JSON helper** by porting from legacy. Drop `box::use`, `withr::with_dir`, the Sys.setlocale call (set globally via DESCRIPTION). Read inputs via `read_table()` instead of `read_csv(here("results", ...))`.

- [ ] **Step 8: Snapshot test** — run publish locally on the current Plan 3 fit, capture the 7 JSONs as fixtures, then assert subsequent publish runs match (modulo `fit_date` in meta.json).

- [ ] **Step 9: Commit** as `feat: R/publish-football-iceland.R — 7 JSONs per sex`.

---

## Task 9: `R/publish-{basketball,handball}-iceland.R` — scaffolds

**Files:**

- Create: `R/publish-basketball-iceland.R`
- Create: `R/publish-handball-iceland.R`
- Create: `tests/testthat/test-publish-basketball.R`, `test-publish-handball.R`

**Purpose:** Per spec §2: "JSON producers wired up, templates deferred unless trivial". Produce **only** `meta.json` + `next_games.json` (the cheap, model-agnostic ones). League-table / season-projection JSONs sit as `# TODO Plan 6` stubs because the metill-platform doesn't render basketball/handball pages yet.

- [ ] **Step 1-2: Implement** with the same fit-in-hand signature as football. Each scaffold ~30 lines including roxygen.

- [ ] **Step 3: Smoke-test commit** as `feat: publish-{basketball,handball}-iceland.R — scaffolds`.

---

## Task 10: Backfill scripts + integration test + CLAUDE.md

**Files:**

- Create: `scripts/decide_all.R`
- Create: `scripts/publish_all.R`
- Create: `tests/testthat/test-decide-publish-integration.R`
- Modify: `CLAUDE.md`

- [ ] **Step 1: Write `scripts/decide_all.R`** — loops active leagues × sexes, calls `decide_league()`. Locks `run_date <- Sys.Date()` once at top (per Plan 3's lessons-learned about midnight straddles).

- [ ] **Step 2: Write `scripts/publish_all.R`** — loops active leagues × sexes, refits or loads fit, calls per-sport publisher.

- [ ] **Step 3: Run both backfills locally + commit data/.**

- [ ] **Step 4: Write integration test**:

```r
test_that("decisions/recommendations covers all active leagues x sexes", {
  skip_if_no_recommendations()
  recs <- read_table("recommendations", filter = list(country = "iceland"))
  expect_gt(nrow(recs), 0L)
  expect_setequal(unique(recs$sport), c("basketball", "football", "handball"))
})

test_that("publish/ has required JSONs for football", {
  skip_if_no_publish()
  for (sex_dir in c("karla", "kvenna")) {
    out <- here::here("data", "publish", "football", "iceland", sex_dir)
    expect_true(file.exists(file.path(out, "meta.json")))
    expect_true(file.exists(file.path(out, "next_games.json")))
    expect_true(file.exists(file.path(out, "standings.json")))
    # ... 7 total
  }
})
```

- [ ] **Step 5: CLAUDE.md update** — Plan 4 row → ✅, new R/decide-*.R + R/publish-*.R + scripts/ entries in directory tree, "Decide layer" + "Publish layer" sections under Conventions.

- [ ] **Step 6: Final test suite run + commit + push.**

---

## What this plan achieves

- Bayesian decide layer: posterior + odds + ledger → recommendations Parquet.
- Football website JSON producer ported with full 7-file output.
- Basketball + handball publish scaffolded; templates deferred to Plan 6.
- Plan 5 (placer) consumes `recommendations.parquet`; Plan 6 (orchestration) wires `{targets}` + CI.

## Risks & mitigations

- **Joint Kelly numerical instability.** Bets with very lopsided p (~1.0) can blow up `log(1 + R*f)`. Mitigation: `solve_kelly_joint` returns `f >= 0` and SLSQP with `xtol_rel = 1e-7` is well-tested in legacy; ev_threshold filter strips the long-tail.
- **Publish snapshot brittleness.** Floating-point drift across `posterior::as_draws_df` versions could change JSON byte-for-byte. Mitigation: snapshot test asserts numeric values within `tolerance = 1e-3` (use `expect_equal` with custom comparator), not byte-identical files.
- **Validation gate's archive coverage gap.** Until walk-forward research collects more `beliefs_archive` entries, Task 7's test mostly skips. Mitigation: structural skip with informative reason; Plan 5+ revisit.
- **Bankroll drift between `current_pool` derivation and ledger settlement.** `load_bankroll()` derives `current_pool = initial_pool + sum(pnl)`. If a bet is settled out of order, the bankroll diverges briefly. Mitigation: settlement always happens via the placer (Plan 5), which writes to ledger atomically. For Plan 4 standalone runs the derivation is fine.
