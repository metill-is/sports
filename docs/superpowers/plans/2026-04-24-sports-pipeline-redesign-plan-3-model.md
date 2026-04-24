# Sports Pipeline Redesign — Plan 3: Model Layer (prepare_data + fit + posteriors)

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build the model layer — `R/model-prepare.R`, `R/model-fit.R`, `R/model-posteriors.R`, `R/model-league.R` — that consumes `data/facts/{results,schedules}/` and produces `data/beliefs/{latest,archive}/` posterior-draw tables for the three active Icelandic leagues, with golden-output sanity gates against the pre-migration `fit.rds` backups.

**Architecture:** Four flat R files, one per concern. `prepare_data(league, sex, end_date)` reads Parquet facts via `read_table()`, builds the team registry + time-between-matches matrix + prediction tibble, and returns `list(stan_data, pred_d, teams)`. `fit_model(stan_data, stan_model_path, ...)` is a thin `{cmdstanr}` wrapper returning the CmdStanMCMC object. `extract_posteriors(fit, pred_d, league, sex, fit_date)` materialises the per-draw-per-match tibble matching `schemas()$beliefs_latest`. `fit_league(league_key, sex)` chains the three together and writes to both `beliefs/latest/` (snapshot, overwrite via `write_table`) and `beliefs/archive/` (accretive per `fit_date` partition).

The three-way legacy pipeline split (`shared` / `football` / `handball_other`) collapses into **one** `prepare_data()` because all three active Icelandic leagues now read from the same canonical-English Parquet tables. `withr::with_dir()` gymnastics, `box::use()` modules, and Icelandic-column rename maps all disappear.

**Tech Stack:** R (≥ 4.0), `{cmdstanr}`, `{posterior}`, `{tibble}`, `{dplyr}`, `{tidyr}`, `{arrow}` (Plan 1 stack), `{testthat}` ed 3. No new dependencies.

**Scope (Plan 3):**

- `Stan/{league_key}/*.stan` — three production Stan models copied from `_legacy/`
- `R/model-prepare.R` — `prepare_data(league, sex, end_date, root)` → `list(stan_data, pred_d, teams)`
- `R/model-fit.R` — `fit_model(stan_data, stan_model_path, method, chains, iter_warmup, iter_sampling, seed)` → CmdStanMCMC
- `R/model-posteriors.R` — `extract_posteriors(fit, pred_d, league, sex, fit_date)` → tibble matching `schemas()$beliefs_latest`
- `R/model-league.R` — `fit_league(league_key, sex, ...)` end-to-end orchestrator + `write_table()` to beliefs tables
- `scripts/fit_all.R` — backfill entrypoint; fits all 3 Icelandic × both sexes
- Fixture-based unit tests for `prepare_data` + `extract_posteriors` (no network, no Stan compilation)
- Smoke test for `fit_model` using a minimal inline Stan program (fast)
- Integration test: after backfill, `beliefs/{latest,archive}/` schema + row-count plausibility
- Golden-output sanity test comparing new posterior predictive means vs backup `fit.rds` (local-only, skips on CI)
- CLAUDE.md update

**Out of scope (still):**

- Decide layer — Kelly + portfolio + recommendations (Plan 4)
- Placer — Lengjan bet placement (Plan 4+)
- Publish — website JSON (Plan 4+)
- Research — walk-forward backtester (Plan 5)
- Orchestration via `{targets}` + CI workflows (Plan 5)
- Any new Stan model or likelihood change (copying the three production models verbatim — further model research sits in `_legacy/`'s Knowledge vault and is independent of the migration)
- Reviving paused non-Icelandic leagues

---

## File structure created by this plan

```
sports/
├── Stan/
│   ├── basketball_iceland/2d_student_t_scalarsigma.stan
│   ├── handball_iceland/2d_student_t.stan
│   └── football_iceland/bivariate_poisson_no_inflation.stan
├── R/
│   ├── model-prepare.R           # prepare_data()
│   ├── model-fit.R               # fit_model() cmdstanr wrapper
│   ├── model-posteriors.R        # extract_posteriors()
│   └── model-league.R            # fit_league() orchestrator
├── scripts/
│   └── fit_all.R                 # Backfill entrypoint
├── tests/testthat/
│   ├── fixtures/model/
│   │   ├── mini_results.parquet  # Tiny hand-crafted dataset for prepare_data
│   │   └── mini_schedules.parquet
│   ├── test-model-prepare.R
│   ├── test-model-fit.R          # Uses inline 8schools.stan, fast
│   ├── test-model-posteriors.R
│   ├── test-model-league.R       # Mocks fit_model to skip Stan compilation
│   ├── test-model-integration.R  # Runs after backfill
│   └── test-model-golden.R       # Skips when backup absent
├── data/beliefs/
│   ├── latest/sport={X}/country={Y}/sex={Z}/beliefs.parquet
│   └── archive/sport={X}/country={Y}/sex={Z}/fit_date={YYYY-MM-DD}/beliefs.parquet
```

---

## Task 1: Stan models — copy from `_legacy/` and verify compilation

**Files:**

- Create: `Stan/basketball_iceland/2d_student_t_scalarsigma.stan` (copy from `_legacy/sports/basketball/iceland/Stan/`)
- Create: `Stan/handball_iceland/2d_student_t.stan` (copy from `_legacy/sports/handball/iceland/Stan/`)
- Create: `Stan/football_iceland/bivariate_poisson_no_inflation.stan` (copy from `_legacy/sports/football/iceland/Stan/`)
- Create: `tests/testthat/test-stan-compile.R`

**Purpose:** The three production Stan files for the active Icelandic leagues live at the root `Stan/{league_key}/` directory (matches the `stan_model:` path in `config/leagues.yml`). Copy them verbatim — no model edits in this plan — and gate future development with a compile test that `cmdstanr::cmdstan_model()` returns without error for each.

- [ ] **Step 1: Copy the three Stan files**

```bash
cd /Users/brynjolfurjonsson/sports
mkdir -p Stan/basketball_iceland Stan/handball_iceland Stan/football_iceland

cp _legacy/sports/basketball/iceland/Stan/2d_student_t_scalarsigma.stan \
   Stan/basketball_iceland/2d_student_t_scalarsigma.stan

cp _legacy/sports/handball/iceland/Stan/2d_student_t.stan \
   Stan/handball_iceland/2d_student_t.stan

cp _legacy/sports/football/iceland/Stan/bivariate_poisson_no_inflation.stan \
   Stan/football_iceland/bivariate_poisson_no_inflation.stan

ls -l Stan/*/*.stan
```

Expected: three `.stan` files, each > 1 KB.

- [ ] **Step 2: Sanity-check `stan_model` paths in `config/leagues.yml` resolve**

```bash
Rscript -e '
leagues <- sports::load_leagues()
for (k in names(leagues)) {
  p <- here::here("Stan", leagues[[k]]$stan_model)
  cat(sprintf("%-20s %s  %s\n", k, if (file.exists(p)) "OK" else "MISSING", p))
}
'
```

Expected: three `OK` lines — the paths `basketball_iceland/2d_student_t_scalarsigma.stan` etc in `leagues.yml` resolve under `Stan/`.

- [ ] **Step 3: Write a compile test**

```r
# tests/testthat/test-stan-compile.R

test_that("all three league Stan models compile", {
  skip_on_cran()
  skip_if_not_installed("cmdstanr")
  skip_if(is.null(tryCatch(cmdstanr::cmdstan_path(), error = function(e) NULL)),
          "cmdstan not installed")

  leagues <- load_leagues()
  for (k in names(leagues)) {
    stan_path <- here::here("Stan", leagues[[k]]$stan_model)
    expect_true(file.exists(stan_path),
                info = paste0("missing Stan file for ", k, ": ", stan_path))

    mod <- tryCatch(
      cmdstanr::cmdstan_model(stan_path, compile = TRUE, quiet = TRUE),
      error = function(e) {
        fail(paste0("compile failed for ", k, ": ", conditionMessage(e)))
        NULL
      }
    )
    expect_s3_class(mod, "CmdStanModel",
                    label = paste0("compiled model for ", k))
  }
})
```

- [ ] **Step 4: Run the test**

```bash
cd /Users/brynjolfurjonsson/sports
Rscript -e 'devtools::test(filter = "stan-compile")'
```

Expected: 1 test, 6 assertions (3 files-exist + 3 compile), all pass. First run will take ~30-60 s per model for compilation; subsequent runs use the cached binaries.

- [ ] **Step 5: Commit**

```bash
git add Stan/ tests/testthat/test-stan-compile.R
git commit -m "feat: copy 3 production Stan models + compile test

Ports the three active Icelandic-league Stan models from _legacy/
into Stan/{league_key}/*.stan. Test gates future development by
verifying each compiles via cmdstanr. No model changes — straight
verbatim copy.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>"
```

---

## Task 2: `prepare_data()` — canonical Parquet → stan_data (TDD)

**Files:**

- Create: `R/model-prepare.R`
- Create: `tests/testthat/fixtures/model/mini_results.parquet`
- Create: `tests/testthat/fixtures/model/mini_schedules.parquet`
- Create: `tests/testthat/test-model-prepare.R`

**Purpose:** Port the legacy `_legacy/sports/R/shared/prep_data.R` + `prep_data_football.R` into one unified function that reads from the Parquet facts tables. Returns everything the Stan model needs plus the prediction tibble for later posterior extraction.

The three legacy pipelines differ only in small ways that now collapse because Parquet columns are already canonical English:

| Legacy behaviour                                        | Plan 3 equivalent                                                                          |
| ------------------------------------------------------- | ------------------------------------------------------------------------------------------ |
| Icelandic-column rename (`heima`→`home`, `dags`→`date`) | **Gone** — Parquet columns are `home_team`, `match_date`, etc.                             |
| `config$divisions$filter_top_teams` (basketball FALSE)  | Always include all teams seen in the filter window — division filtering moves to `decide/` |
| `config$divisions$filter_next_games`                    | Always emit a `pred_d` for every upcoming match                                            |
| Football's 14-day horizon filter (`date <= today + 14`) | Same — applied via `schedule_horizon_days` arg (default 14)                                |
| Football's `from_season` filter                         | Configurable via `from_season` arg (default NULL = use all seasons)                        |

**Signature:**

```r
prepare_data <- function(league,
                         sex,
                         end_date = Sys.Date(),
                         root = here::here("data"),
                         from_season = NULL,
                         schedule_horizon_days = 14) { ... }
```

Returns `list(stan_data, pred_d, teams)` where:

- `stan_data` — list matching the Stan `data{}` block for all three models (superset; each model reads only its fields)
- `pred_d` — tibble with (game_nr, match_date, home_team, away_team, division, home_nr, away_nr, home_timediff, away_timediff) for downstream posterior join
- `teams` — tibble with (team, team_nr) — the reference registry

- [ ] **Step 1: Create a tiny hand-crafted fixture**

```r
# Save once, re-used by all prepare_data tests.
# Run this from an interactive R session and commit the resulting parquet.
library(arrow)
library(tibble)

# 4 teams, 3 seasons (2024/2025/2026), 6 played matches, 2 upcoming.
# Two divisions "D1" and "D2" so prepare_data exercises the division column.
results <- tibble::tibble(
  sport      = "basketball",
  country    = "iceland",
  sex        = "male",
  season     = c(2024L, 2024L, 2025L, 2025L, 2026L, 2026L),
  match_date = as.Date(c("2024-10-12", "2024-11-03",
                         "2025-01-15", "2025-02-20",
                         "2026-01-10", "2026-02-14")),
  home_team  = c("Alpha", "Bravo", "Charlie", "Alpha",   "Bravo", "Delta"),
  away_team  = c("Bravo", "Charlie", "Alpha", "Delta",   "Charlie", "Alpha"),
  home_score = c(85L,     72L,      90L,     77L,        82L,       95L),
  away_score = c(80L,     75L,      88L,     70L,        79L,       89L),
  division   = c("D1",    "D1",     "D1",    "D2",       "D1",      "D1"),
  round      = c(1L,      2L,       1L,      2L,         1L,        2L)
)

schedules <- tibble::tibble(
  sport      = "basketball",
  country    = "iceland",
  sex        = "male",
  season     = c(2026L, 2026L),
  match_date = Sys.Date() + c(3L, 7L),
  home_team  = c("Alpha", "Charlie"),
  away_team  = c("Bravo", "Delta"),
  division   = c("D1", "D1"),
  round      = c(3L, 3L)
)

dir.create("tests/testthat/fixtures/model", recursive = TRUE, showWarnings = FALSE)
arrow::write_parquet(results,   "tests/testthat/fixtures/model/mini_results.parquet")
arrow::write_parquet(schedules, "tests/testthat/fixtures/model/mini_schedules.parquet")
```

Commit the parquet files as test fixtures. `prepare_data`'s tests write them into a temp dir in the hive layout before reading.

- [ ] **Step 2: Write failing tests**

```r
# tests/testthat/test-model-prepare.R

# Helper: materialise the mini fixture into a temporary hive-partitioned root,
# mimicking data/facts/results/sport=X/country=Y/sex=Z/season=N/*.parquet
setup_mini_root <- function() {
  tmp <- withr::local_tempdir(.local_envir = parent.frame())
  results  <- arrow::read_parquet(testthat::test_path("fixtures", "model",
                                                      "mini_results.parquet"))
  schedules <- arrow::read_parquet(testthat::test_path("fixtures", "model",
                                                       "mini_schedules.parquet"))
  write_table(results,   "results",   root = tmp)
  write_table(schedules, "schedules", root = tmp)
  tmp
}

test_that("prepare_data builds a stan_data list with all expected fields", {
  root <- setup_mini_root()
  league <- list(
    sport = "basketball", country = "iceland", sexes = "male",
    stan_model = "basketball_iceland/2d_student_t_scalarsigma.stan"
  )

  out <- prepare_data(league, sex = "male", end_date = Sys.Date(), root = root)

  expect_type(out, "list")
  expect_named(out, c("stan_data", "pred_d", "teams"), ignore.order = TRUE)

  sd <- out$stan_data
  required <- c("K", "N", "N_pred", "N_rounds", "N_seasons",
                "team1", "team2", "round1", "round2",
                "time_between_matches",
                "goals1", "goals2", "division", "season", "season_first",
                "team1_pred", "team2_pred",
                "pred_timediff1", "pred_timediff2", "pred_division",
                "time_to_next_games", "top_teams", "N_top_teams")
  expect_true(all(required %in% names(sd)),
              info = paste("missing:",
                           paste(setdiff(required, names(sd)), collapse = ", ")))

  expect_equal(sd$K, 4L)                  # Alpha, Bravo, Charlie, Delta
  expect_equal(sd$N, 6L)                  # played matches in fixture
  expect_equal(sd$N_pred, 2L)             # scheduled matches
  expect_equal(sd$N_seasons, 3L)          # 2024, 2025, 2026
  expect_equal(length(sd$goals1), sd$N)
  expect_equal(length(sd$goals2), sd$N)
  expect_equal(dim(sd$time_between_matches), c(sd$K, sd$N_rounds))
  expect_true(all(sd$time_between_matches >= 0))
})

test_that("prepare_data returns teams tibble with sequential team_nr", {
  root <- setup_mini_root()
  league <- list(sport = "basketball", country = "iceland", sexes = "male",
                 stan_model = "basketball_iceland/2d_student_t_scalarsigma.stan")

  out <- prepare_data(league, sex = "male", end_date = Sys.Date(), root = root)

  expect_equal(sort(out$teams$team), sort(c("Alpha", "Bravo", "Charlie", "Delta")))
  expect_equal(out$teams$team_nr, seq_len(nrow(out$teams)))
  expect_equal(out$stan_data$K, nrow(out$teams))
})

test_that("prepare_data pred_d has canonical columns and numeric team indices", {
  root <- setup_mini_root()
  league <- list(sport = "basketball", country = "iceland", sexes = "male",
                 stan_model = "basketball_iceland/2d_student_t_scalarsigma.stan")

  out <- prepare_data(league, sex = "male", end_date = Sys.Date(), root = root)

  expect_true(all(c("game_nr", "match_date", "home_team", "away_team",
                    "division", "home_nr", "away_nr",
                    "home_timediff", "away_timediff") %in% names(out$pred_d)))
  expect_type(out$pred_d$home_nr, "integer")
  expect_type(out$pred_d$away_nr, "integer")
  expect_true(all(out$pred_d$home_nr <= out$stan_data$K))
  expect_true(all(out$pred_d$away_nr <= out$stan_data$K))
})

test_that("prepare_data filters results by end_date", {
  root <- setup_mini_root()
  league <- list(sport = "basketball", country = "iceland", sexes = "male",
                 stan_model = "basketball_iceland/2d_student_t_scalarsigma.stan")

  # end_date before some of the fixture matches -> fewer matches
  out <- prepare_data(league, sex = "male",
                      end_date = as.Date("2025-06-01"), root = root)
  expect_equal(out$stan_data$N, 4L)   # only 2024-10-12, 2024-11-03, 2025-01-15, 2025-02-20
})

test_that("prepare_data respects from_season when supplied", {
  root <- setup_mini_root()
  league <- list(sport = "basketball", country = "iceland", sexes = "male",
                 stan_model = "basketball_iceland/2d_student_t_scalarsigma.stan")

  out <- prepare_data(league, sex = "male",
                      end_date = Sys.Date(), from_season = 2025L, root = root)
  expect_equal(out$stan_data$N, 4L)   # drops the two 2024 matches
  expect_equal(out$stan_data$N_seasons, 2L)
})

test_that("prepare_data returns N_pred = 0 when no upcoming schedule matches", {
  # No schedule rows scenario: remove the schedule fixture's rows via a
  # filter end_date far in the future.
  root <- setup_mini_root()
  league <- list(sport = "basketball", country = "iceland", sexes = "male",
                 stan_model = "basketball_iceland/2d_student_t_scalarsigma.stan")

  out <- prepare_data(league, sex = "male",
                      end_date = Sys.Date() + 30L, root = root)
  expect_equal(out$stan_data$N_pred, 0L)
  expect_equal(nrow(out$pred_d), 0L)
})
```

- [ ] **Step 3: Verify tests fail**

```bash
Rscript -e 'devtools::test(filter = "model-prepare")'
```

Expected: 6 errors (`could not find function "prepare_data"`).

- [ ] **Step 4: Implement `R/model-prepare.R`**

```r
#' @include storage.R
NULL

#' Build stan_data + pred_d + teams from the facts store.
#'
#' Reads `data/facts/results/` and `data/facts/schedules/` for the given
#' (league, sex), assembles the canonical Stan input list, and returns it
#' along with the prediction tibble (for posterior-draw joining) and the
#' team registry. Pure function — no file I/O beyond read_table().
#'
#' The returned stan_data is a superset covering every field consumed by
#' the three production models (basketball scalar-sigma, handball per-team
#' sigma, football BVP). Each Stan model's data{} block picks only the
#' fields it declares.
#'
#' @param league A single entry from `load_leagues()` (must have `sport` +
#'   `country` set; `stan_model` is not read here).
#' @param sex "male" or "female".
#' @param end_date Cutoff date — matches on or before this go into training.
#' @param root Data root. Default `here::here("data")`.
#' @param from_season Optional integer. If supplied, drop matches with
#'   `season < from_season`.
#' @param schedule_horizon_days How far ahead to look for prediction targets.
#'   Matches a schedule's match_date is kept if it falls in
#'   `[end_date, end_date + schedule_horizon_days]`.
#' @return `list(stan_data, pred_d, teams)`.
#' @export
prepare_data <- function(league,
                         sex,
                         end_date = Sys.Date(),
                         root = here::here("data"),
                         from_season = NULL,
                         schedule_horizon_days = 14L) {
  stopifnot(sex %in% c("male", "female"))
  stopifnot(!is.null(league$sport), !is.null(league$country))

  # ── Results (training matches) ─────────────────────────────────────────
  results <- read_table(
    "results",
    root = root,
    filter = list(sport = league$sport, country = league$country, sex = sex)
  )

  if (!is.null(from_season)) {
    results <- results[results$season >= as.integer(from_season), , drop = FALSE]
  }
  results <- results[results$match_date <= end_date, , drop = FALSE]
  results <- results[!is.na(results$home_score) & !is.na(results$away_score), , drop = FALSE]
  results <- results[order(results$match_date), , drop = FALSE]
  results$game_nr <- seq_len(nrow(results))

  # ── Team registry ─────────────────────────────────────────────────────
  teams <- tibble::tibble(
    team = sort(unique(c(results$home_team, results$away_team)))
  )
  teams$team_nr <- seq_len(nrow(teams))

  # ── Schedules (prediction matches) ────────────────────────────────────
  schedules <- read_table(
    "schedules",
    root = root,
    filter = list(sport = league$sport, country = league$country, sex = sex)
  )

  horizon_end <- end_date + as.integer(schedule_horizon_days)
  next_games <- schedules[
    !is.na(schedules$match_date) &
      schedules$match_date >= end_date &
      schedules$match_date <= horizon_end, , drop = FALSE]

  # Only predict matches whose teams appear in the training set
  next_games <- next_games[
    next_games$home_team %in% teams$team & next_games$away_team %in% teams$team,
    , drop = FALSE]
  next_games <- next_games[order(next_games$match_date), , drop = FALSE]
  if (nrow(next_games) > 0L) next_games$game_nr <- seq_len(nrow(next_games))

  # ── Per-team time-between-matches and round index ─────────────────────
  long <- dplyr::bind_rows(
    dplyr::transmute(results, game_nr, match_date,
                     team = .data$home_team, side = "home"),
    dplyr::transmute(results, game_nr, match_date,
                     team = .data$away_team, side = "away")
  )
  long <- long[order(long$team, long$match_date), , drop = FALSE]
  long <- long |>
    dplyr::group_by(.data$team) |>
    dplyr::mutate(
      round     = dplyr::row_number(),
      time_diff = as.numeric(.data$match_date - dplyr::lag(.data$match_date)),
      time_diff = dplyr::if_else(is.na(.data$time_diff), 7, .data$time_diff),
      time_diff = pmin(.data$time_diff, 100)
    ) |>
    dplyr::ungroup()

  home_long <- long[long$side == "home", c("game_nr", "round", "time_diff"), drop = FALSE]
  names(home_long) <- c("game_nr", "home_round", "home_timediff")
  away_long <- long[long$side == "away", c("game_nr", "round", "time_diff"), drop = FALSE]
  names(away_long) <- c("game_nr", "away_round", "away_timediff")

  # Season-first flag: 1 at the first appearance of each team in each season
  season_first_long <- dplyr::bind_rows(
    dplyr::transmute(results, game_nr, season, team = .data$home_team, side = "home"),
    dplyr::transmute(results, game_nr, season, team = .data$away_team, side = "away")
  )
  season_first_long <- season_first_long |>
    dplyr::group_by(.data$team, .data$season) |>
    dplyr::mutate(season_round = dplyr::row_number(),
                  is_first = as.integer(.data$season_round == 1L)) |>
    dplyr::ungroup()
  season_first_by_game <- season_first_long |>
    dplyr::filter(.data$side == "home") |>
    dplyr::select(game_nr, season_first = .data$is_first)

  model_d <- results |>
    dplyr::inner_join(home_long, by = "game_nr") |>
    dplyr::inner_join(away_long, by = "game_nr") |>
    dplyr::inner_join(season_first_by_game, by = "game_nr") |>
    dplyr::inner_join(teams, by = c("home_team" = "team")) |>
    dplyr::rename(home_nr = .data$team_nr) |>
    dplyr::inner_join(teams, by = c("away_team" = "team")) |>
    dplyr::rename(away_nr = .data$team_nr) |>
    dplyr::arrange(.data$match_date)

  # Division as integer factor (1 = first level, 2 = second, ...)
  model_d$division_int <- as.integer(factor(model_d$division))

  N_rounds <- max(c(model_d$home_round, model_d$away_round))
  tbm <- matrix(0, nrow = nrow(teams), ncol = N_rounds)
  for (i in seq_len(nrow(model_d))) {
    tbm[model_d$home_nr[i], model_d$home_round[i]] <- model_d$home_timediff[i]
    tbm[model_d$away_nr[i], model_d$away_round[i]] <- model_d$away_timediff[i]
  }

  # ── Prediction tibble ────────────────────────────────────────────────
  if (nrow(next_games) > 0L) {
    latest_game_dates <- long |>
      dplyr::group_by(.data$team) |>
      dplyr::summarise(latest_date = max(.data$match_date), .groups = "drop")

    # Time-to-next-match per team-with-upcoming-fixture
    upcoming_per_team <- next_games |>
      tidyr::pivot_longer(c(.data$home_team, .data$away_team),
                          names_to = "side", values_to = "team") |>
      dplyr::group_by(.data$team) |>
      dplyr::arrange(.data$match_date) |>
      dplyr::mutate(team_game = dplyr::row_number()) |>
      dplyr::ungroup()

    first_upcoming <- upcoming_per_team |>
      dplyr::filter(.data$team_game == 1L) |>
      dplyr::select(team, next_date = .data$match_date) |>
      dplyr::inner_join(latest_game_dates, by = "team") |>
      dplyr::mutate(timediff = pmin(as.numeric(.data$next_date - .data$latest_date), 100))

    time_to_next <- first_upcoming$timediff
    top_teams_df <- teams[teams$team %in% first_upcoming$team, , drop = FALSE]

    # pred_timediff per (game_nr, side) — time since each team's last match
    # (training-set last match for first upcoming; preceding upcoming for later)
    pred_timediffs <- upcoming_per_team |>
      dplyr::left_join(latest_game_dates, by = "team") |>
      dplyr::group_by(.data$team) |>
      dplyr::mutate(
        prev_date = dplyr::if_else(.data$team_game == 1L,
                                   .data$latest_date,
                                   dplyr::lag(.data$match_date)),
        timediff = pmin(as.numeric(.data$match_date - .data$prev_date), 50)
      ) |>
      dplyr::ungroup() |>
      dplyr::select(.data$game_nr, .data$side, .data$timediff) |>
      tidyr::pivot_wider(names_from = .data$side, values_from = .data$timediff) |>
      dplyr::rename(home_timediff = .data$home_team,
                    away_timediff = .data$away_team)

    pred_d <- next_games |>
      dplyr::inner_join(pred_timediffs, by = "game_nr") |>
      dplyr::inner_join(teams, by = c("home_team" = "team")) |>
      dplyr::rename(home_nr = .data$team_nr) |>
      dplyr::inner_join(teams, by = c("away_team" = "team")) |>
      dplyr::rename(away_nr = .data$team_nr)

    pred_d$division_int <- as.integer(factor(pred_d$division,
                                             levels = levels(factor(model_d$division))))
  } else {
    time_to_next <- numeric(0)
    top_teams_df <- teams[0, , drop = FALSE]
    pred_d <- tibble::tibble(
      game_nr = integer(0),
      match_date = as.Date(character()),
      home_team = character(), away_team = character(),
      division = character(),
      home_nr = integer(), away_nr = integer(),
      home_timediff = numeric(), away_timediff = numeric(),
      division_int = integer()
    )
  }

  stan_data <- list(
    K                     = nrow(teams),
    N                     = nrow(model_d),
    N_pred                = nrow(pred_d),
    N_rounds              = N_rounds,
    N_seasons             = length(unique(model_d$season)),
    season                = as.integer(as.factor(model_d$season)),
    season_first          = as.integer(model_d$season_first),
    team1                 = as.integer(model_d$home_nr),
    team2                 = as.integer(model_d$away_nr),
    round1                = as.integer(model_d$home_round),
    round2                = as.integer(model_d$away_round),
    time_between_matches  = tbm,
    goals1                = as.integer(model_d$home_score),
    goals2                = as.integer(model_d$away_score),
    division              = as.integer(model_d$division_int),
    team1_pred            = as.integer(pred_d$home_nr),
    team2_pred            = as.integer(pred_d$away_nr),
    pred_timediff1        = as.numeric(pred_d$home_timediff),
    pred_timediff2        = as.numeric(pred_d$away_timediff),
    pred_division         = as.integer(pred_d$division_int),
    time_to_next_games    = as.numeric(time_to_next),
    top_teams             = as.integer(top_teams_df$team_nr),
    N_top_teams           = nrow(top_teams_df)
  )

  list(
    stan_data = stan_data,
    pred_d    = pred_d[, c("game_nr", "match_date", "home_team", "away_team",
                           "division", "home_nr", "away_nr",
                           "home_timediff", "away_timediff"), drop = FALSE],
    teams     = teams
  )
}
```

- [ ] **Step 5: Update roxygen + collate order**

```bash
Rscript -e 'roxygen2::roxygenise()'
Rscript -e 'roxygen2::update_collate(".")'
```

- [ ] **Step 6: Verify tests pass**

```bash
Rscript -e 'devtools::test(filter = "model-prepare")'
```

Expected: 6 passes. If any fail, inspect the fixture + function output side-by-side and fix before committing.

- [ ] **Step 7: Commit**

```bash
git add R/model-prepare.R NAMESPACE DESCRIPTION \
        tests/testthat/fixtures/model/ tests/testthat/test-model-prepare.R
git commit -m "feat: R/model-prepare.R — canonical Parquet -> stan_data

prepare_data(league, sex) reads data/facts/{results,schedules}/ via
read_table() and returns list(stan_data, pred_d, teams). Unifies the
three legacy pipelines (shared / football / handball_other) into one
path because Parquet columns are already canonical English. Drops
withr::with_dir, box::use, Icelandic column renames. Fixture-tested
with a mini (4-team, 6-match) dataset.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>"
```

---

## Task 3: `fit_model()` — `{cmdstanr}` wrapper (smoke test)

**Files:**

- Create: `R/model-fit.R`
- Create: `tests/testthat/test-model-fit.R`

**Purpose:** Port `_legacy/sports/R/shared/model_fitting.R::fit_model()` into a cleaner wrapper. Key simplification vs legacy: the function no longer writes `fit.rds` — it returns the CmdStanMCMC object, and the caller handles persistence. Pathfinder + variational paths preserved because Plan 1's memory-note policy ("approximate inference not viable") might flip once model research resumes; keeping the plumbing costs nothing.

The smoke test uses a minimal inline Stan program (8schools, ~20 lines) to avoid depending on the 3 production models which take 30-60 s to compile.

- [ ] **Step 1: Write failing smoke test**

```r
# tests/testthat/test-model-fit.R

test_that("fit_model returns a CmdStanMCMC for MCMC method", {
  skip_on_cran()
  skip_if_not_installed("cmdstanr")
  skip_if(is.null(tryCatch(cmdstanr::cmdstan_path(), error = function(e) NULL)),
          "cmdstan not installed")

  tmp <- withr::local_tempfile(fileext = ".stan")
  writeLines(
    c("data {",
      "  int<lower=0> N;",
      "  vector[N] y;",
      "  vector<lower=0>[N] sigma;",
      "}",
      "parameters {",
      "  real mu;",
      "  real<lower=0> tau;",
      "  vector[N] theta;",
      "}",
      "model {",
      "  tau ~ cauchy(0, 5);",
      "  theta ~ normal(mu, tau);",
      "  y ~ normal(theta, sigma);",
      "}"),
    tmp
  )

  fit <- fit_model(
    stan_data = list(N = 8L,
                     y = c(28, 8, -3, 7, -1, 1, 18, 12),
                     sigma = c(15, 10, 16, 11, 9, 11, 10, 18)),
    stan_model_path = tmp,
    method = "sample",
    chains = 2L,
    iter_warmup = 200L,
    iter_sampling = 200L,
    seed = 42L,
    show_progress = FALSE
  )

  expect_s3_class(fit, "CmdStanMCMC")
  expect_equal(fit$num_chains(), 2L)
  # Variable names should include mu + tau + theta[1..8]
  vars <- fit$metadata()$variables
  expect_true("mu" %in% vars)
  expect_true("tau" %in% vars)
})

test_that("fit_model errors clearly on unknown method", {
  skip_if_not_installed("cmdstanr")
  expect_error(
    fit_model(stan_data = list(), stan_model_path = "ignored", method = "foo"),
    regexp = "method"
  )
})
```

- [ ] **Step 2: Verify failure**

```bash
Rscript -e 'devtools::test(filter = "model-fit")'
```

Expected: 2 errors (`could not find function "fit_model"`).

- [ ] **Step 3: Implement `R/model-fit.R`**

```r
#' Fit a Stan model and return the result object.
#'
#' Thin wrapper over `cmdstanr::cmdstan_model()$sample()` (or `$pathfinder()`,
#' `$variational()`). Unlike the legacy counterpart, this does **not** write
#' to disk — callers handle `save_object()`.
#'
#' For approximate methods, `generate_quantities()` is called automatically
#' to produce predictive draws so downstream code (extract_posteriors) works
#' unchanged.
#'
#' @param stan_data Named list matching the model's `data {}` block.
#' @param stan_model_path Absolute path to a `.stan` file.
#' @param method "sample" (default), "pathfinder", or "variational".
#' @param chains Number of MCMC chains (MCMC only).
#' @param parallel_chains Number of chains to run in parallel.
#' @param iter_warmup,iter_sampling Iteration counts (MCMC only).
#' @param num_paths Number of Pathfinder paths.
#' @param draws Number of draws for approximate methods.
#' @param seed Integer seed for reproducibility. NULL = cmdstanr default.
#' @param init Initial values passed to cmdstanr. Default 0 matches legacy.
#' @param show_progress Print cmdstanr progress bar? Default TRUE.
#' @return CmdStanMCMC (sample) or CmdStanGQ (pathfinder / variational).
#' @export
fit_model <- function(stan_data,
                      stan_model_path,
                      method = c("sample", "pathfinder", "variational"),
                      chains = 4L,
                      parallel_chains = chains,
                      iter_warmup = 1000L,
                      iter_sampling = 1000L,
                      num_paths = 4L,
                      draws = 4000L,
                      seed = NULL,
                      init = 0,
                      show_progress = TRUE) {
  method <- match.arg(method)

  model <- cmdstanr::cmdstan_model(stan_model_path, quiet = TRUE)

  common_quiet <- list(show_messages = FALSE, show_exceptions = FALSE)

  if (method == "sample") {
    args <- c(list(
      data            = stan_data,
      chains          = chains,
      parallel_chains = parallel_chains,
      iter_warmup     = iter_warmup,
      iter_sampling   = iter_sampling,
      init            = init,
      refresh         = if (show_progress) 100L else 0L
    ), if (!is.null(seed)) list(seed = seed), common_quiet)
    do.call(model$sample, args)
  } else if (method == "pathfinder") {
    args <- c(list(
      data      = stan_data,
      num_paths = num_paths,
      draws     = draws,
      init      = init
    ), if (!is.null(seed)) list(seed = seed))
    approx <- do.call(model$pathfinder, args)
    pf_draws <- posterior::as_draws_matrix(approx$draws())
    model$generate_quantities(fitted_params = pf_draws, data = stan_data)
  } else {
    args <- c(list(
      data      = stan_data,
      algorithm = "fullrank",
      draws     = draws,
      init      = init
    ), if (!is.null(seed)) list(seed = seed))
    approx <- do.call(model$variational, args)
    model$generate_quantities(fitted_params = approx, data = stan_data)
  }
}
```

- [ ] **Step 4: Verify tests pass**

```bash
Rscript -e 'devtools::test(filter = "model-fit")'
```

Expected: 2 passes. First run will compile the inline 8schools model (~20 s); subsequent runs cached.

- [ ] **Step 5: Commit**

```bash
git add R/model-fit.R NAMESPACE tests/testthat/test-model-fit.R
git commit -m "feat: R/model-fit.R — cmdstanr fit wrapper, pure (no file I/O)

fit_model(stan_data, stan_model_path, method = 'sample') returns the
CmdStanMCMC object. Callers handle save_object(). Approximate methods
(pathfinder, variational) plumbed through with generate_quantities()
so extract_posteriors() works unchanged, even though current memory
policy says they're not viable on production models.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>"
```

---

## Task 4: `extract_posteriors()` — fit → canonical beliefs tibble (TDD)

**Files:**

- Create: `R/model-posteriors.R`
- Create: `tests/testthat/test-model-posteriors.R`

**Purpose:** Port `_legacy/sports/R/shared/extract_posterior.R::extract_posterior_goals()`. Produces a tibble matching `schemas()$beliefs_latest` — one row per (match, draw).

Signature:

```r
extract_posteriors(fit, pred_d, league, sex, fit_date = Sys.Date()) -> tibble
```

Output columns (match `schemas()$beliefs_latest` exactly):

- sport, country, sex, fit_date, match_date, home_team, away_team, draw_id, home_goals, away_goals

- [ ] **Step 1: Write failing tests (fixture-based, no real Stan fit)**

```r
# tests/testthat/test-model-posteriors.R

# Build a lightweight fit-like object with just the methods extract_posteriors
# calls — avoids depending on a real cmdstanr fit for this unit test.
make_fake_fit <- function(N_pred = 2L, n_draws = 10L) {
  # Build a draws_df with goals1_pred[1..N_pred] and goals2_pred[1..N_pred]
  vars <- c(paste0("goals1_pred[", seq_len(N_pred), "]"),
            paste0("goals2_pred[", seq_len(N_pred), "]"))
  mat <- matrix(
    runif(n_draws * length(vars), min = 0, max = 5),
    nrow = n_draws, ncol = length(vars),
    dimnames = list(NULL, vars)
  )
  draws_arr <- posterior::as_draws_array(mat)

  structure(
    list(draws = function(variables = NULL) {
      if (is.null(variables)) return(draws_arr)
      posterior::subset_draws(draws_arr, variable = variables)
    }),
    class = c("fake_fit", "CmdStanMCMC")
  )
}

test_that("extract_posteriors returns a tibble with the beliefs_latest schema", {
  pred_d <- tibble::tibble(
    game_nr = 1:2,
    match_date = as.Date(c("2026-05-01", "2026-05-03")),
    home_team = c("Alpha", "Bravo"),
    away_team = c("Charlie", "Delta"),
    division = c("D1", "D1"),
    home_nr = 1:2, away_nr = 3:4,
    home_timediff = c(7, 7), away_timediff = c(7, 7)
  )
  fit <- make_fake_fit(N_pred = 2L, n_draws = 10L)
  league <- list(sport = "basketball", country = "iceland")

  out <- extract_posteriors(fit, pred_d, league = league, sex = "male",
                            fit_date = as.Date("2026-04-24"))

  expect_s3_class(out, "tbl_df")
  expected_cols <- c("sport", "country", "sex", "fit_date",
                     "match_date", "home_team", "away_team",
                     "draw_id", "home_goals", "away_goals")
  expect_named(out, expected_cols, ignore.order = TRUE)
  expect_equal(nrow(out), 2L * 10L)     # N_pred * n_draws
  expect_type(out$draw_id, "integer")
  expect_type(out$home_goals, "double")
  expect_type(out$away_goals, "double")
  expect_true(all(out$fit_date == as.Date("2026-04-24")))
})

test_that("extract_posteriors handles empty pred_d gracefully", {
  empty_pred <- tibble::tibble(
    game_nr = integer(0), match_date = as.Date(character()),
    home_team = character(), away_team = character(),
    division = character(),
    home_nr = integer(), away_nr = integer(),
    home_timediff = numeric(), away_timediff = numeric()
  )
  fit <- make_fake_fit(N_pred = 0L, n_draws = 10L)
  league <- list(sport = "basketball", country = "iceland")

  out <- extract_posteriors(fit, empty_pred, league = league, sex = "male")
  expect_equal(nrow(out), 0L)
  expect_named(out, c("sport", "country", "sex", "fit_date",
                      "match_date", "home_team", "away_team",
                      "draw_id", "home_goals", "away_goals"),
               ignore.order = TRUE)
})

test_that("extract_posteriors output round-trips through the beliefs_latest schema", {
  pred_d <- tibble::tibble(
    game_nr = 1L, match_date = as.Date("2026-05-01"),
    home_team = "Alpha", away_team = "Bravo",
    division = "D1", home_nr = 1L, away_nr = 2L,
    home_timediff = 7, away_timediff = 7
  )
  fit <- make_fake_fit(N_pred = 1L, n_draws = 5L)
  league <- list(sport = "basketball", country = "iceland")

  out <- extract_posteriors(fit, pred_d, league, sex = "male",
                            fit_date = Sys.Date())

  tmp <- withr::local_tempdir()
  expect_no_error(write_table(out, "beliefs_latest", root = tmp))
  back <- read_table("beliefs_latest", root = tmp)
  expect_equal(nrow(back), nrow(out))
})
```

- [ ] **Step 2: Verify failure**

```bash
Rscript -e 'devtools::test(filter = "model-posteriors")'
```

- [ ] **Step 3: Implement `R/model-posteriors.R`**

```r
#' Extract posterior predictive draws into the canonical beliefs tibble.
#'
#' Pulls `goals1_pred` + `goals2_pred` draws from a fit, long-pivots them
#' by match (game_nr) and draw, then joins pred_d for match_date +
#' team-name metadata. Returns a tibble matching `schemas()$beliefs_latest`.
#'
#' @param fit CmdStanMCMC / CmdStanGQ with `goals1_pred`, `goals2_pred`
#'   generated quantities.
#' @param pred_d Tibble from `prepare_data()$pred_d` with `game_nr`,
#'   `match_date`, `home_team`, `away_team`.
#' @param league `list(sport = ..., country = ...)` — used to attach
#'   schema columns.
#' @param sex "male" or "female".
#' @param fit_date Date to stamp on every row. Default today.
#' @return Tibble with one row per (match, draw).
#' @export
extract_posteriors <- function(fit, pred_d, league, sex,
                               fit_date = Sys.Date()) {
  stopifnot(!is.null(league$sport), !is.null(league$country))
  stopifnot(sex %in% c("male", "female"))

  empty_out <- function() {
    tibble::tibble(
      sport      = character(),
      country    = character(),
      sex        = character(),
      fit_date   = as.Date(character()),
      match_date = as.Date(character()),
      home_team  = character(),
      away_team  = character(),
      draw_id    = integer(),
      home_goals = numeric(),
      away_goals = numeric()
    )
  }

  if (nrow(pred_d) == 0L) return(empty_out())

  draws_df <- posterior::as_draws_df(
    fit$draws(c("goals1_pred", "goals2_pred"))
  ) |> tibble::as_tibble()

  if (nrow(draws_df) == 0L) return(empty_out())

  long <- draws_df |>
    tidyr::pivot_longer(
      cols = -c(".chain", ".iteration", ".draw"),
      names_to = "parameter", values_to = "value"
    ) |>
    dplyr::mutate(
      type    = dplyr::if_else(
        stringr::str_detect(.data$parameter, "^goals1_pred"),
        "home_goals", "away_goals"
      ),
      game_nr = as.integer(
        stringr::str_match(.data$parameter, "\\[(\\d+)\\]$")[, 2]
      )
    ) |>
    dplyr::select(draw_id = .data$`.draw`, .data$type, .data$game_nr, .data$value) |>
    tidyr::pivot_wider(names_from = .data$type, values_from = .data$value)

  joined <- long |>
    dplyr::inner_join(
      pred_d[, c("game_nr", "match_date", "home_team", "away_team"), drop = FALSE],
      by = "game_nr"
    )

  tibble::tibble(
    sport      = league$sport,
    country    = league$country,
    sex        = sex,
    fit_date   = as.Date(fit_date),
    match_date = joined$match_date,
    home_team  = joined$home_team,
    away_team  = joined$away_team,
    draw_id    = as.integer(joined$draw_id),
    home_goals = as.numeric(joined$home_goals),
    away_goals = as.numeric(joined$away_goals)
  )
}
```

- [ ] **Step 4: Verify tests pass**

```bash
Rscript -e 'devtools::test(filter = "model-posteriors")'
```

Expected: 3 passes.

- [ ] **Step 5: Commit**

```bash
git add R/model-posteriors.R NAMESPACE tests/testthat/test-model-posteriors.R
git commit -m "feat: R/model-posteriors.R — fit -> beliefs_latest tibble

extract_posteriors(fit, pred_d, league, sex) pulls goals1_pred/
goals2_pred draws via posterior::as_draws_df, pivots by (game_nr,
draw), joins pred_d for match metadata, and returns a tibble
matching schemas()\$beliefs_latest. Fixture-tested with a fake
fit object — no Stan compilation needed for this unit test.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>"
```

---

## Task 5: `fit_league()` orchestrator + beliefs writes

**Files:**

- Create: `R/model-league.R`
- Create: `tests/testthat/test-model-league.R`

**Purpose:** Chain `prepare_data` → `fit_model` → `extract_posteriors` → `write_table()` to both `beliefs/latest/` (snapshot, overwrite) and `beliefs/archive/` (accretive per fit_date). This is the thing CI + `scripts/fit_all.R` will call.

Signature:

```r
fit_league(
  league_key, sex,
  fit_date = Sys.Date(),
  end_date = fit_date,
  root = here::here("data"),
  stan_dir = here::here("Stan"),
  method = "sample",
  iter_warmup = 1000L, iter_sampling = 1000L, chains = 4L, seed = NULL,
  from_season = NULL, schedule_horizon_days = 14L,
  write_archive = TRUE
) -> invisible(tibble)
```

`beliefs_latest`: overwrite at partition `sport={X}/country={Y}/sex={Z}` — only the latest fit is kept.
`beliefs_archive`: accretive at `sport={X}/country={Y}/sex={Z}/fit_date={YYYY-MM-DD}` — every fit kept forever so walk-forward research can reconstruct "what did the model believe on date D?".

- [ ] **Step 1: Verify beliefs_archive partitioning expectation in storage.R**

```bash
grep -n "fit_date\|beliefs_latest\|beliefs_archive" /Users/brynjolfurjonsson/sports/R/storage.R
```

Expected: `table_partitions()` and / or `table_subdir()` return the correct hive keys for the beliefs tables. If not, fix `R/storage.R` in this step before the test.

Required behaviour:

| Table             | Partition keys                       | Write mode          |
| ----------------- | ------------------------------------ | ------------------- |
| `beliefs_latest`  | `sport`, `country`, `sex`            | `write_table` (overwrite partition) |
| `beliefs_archive` | `sport`, `country`, `sex`, `fit_date` | `write_table` (unique per fit_date so overwrite-partition is safe) |

If `table_partitions()` doesn't already list these, add them:

```r
# R/storage.R — excerpt for reference; confirm/edit.
table_partitions <- function(table) {
  switch(table,
    results          = c("sport", "country", "sex", "season"),
    schedules        = c("sport", "country", "sex", "season"),
    odds             = c("sport", "country", "scraped_date"),
    beliefs_latest   = c("sport", "country", "sex"),
    beliefs_archive  = c("sport", "country", "sex", "fit_date"),
    candidates       = c("sport", "country", "run_date"),
    recommendations  = c("sport", "country", "run_date"),
    ledger           = c("sport", "country"),
    stop("Unknown table: ", table)
  )
}
```

If this mapping is missing, patch `R/storage.R` and rerun Plan 1's storage tests before continuing (`devtools::test(filter = "storage")`).

- [ ] **Step 2: Write the orchestrator test (mocks Stan)**

Stan compilation is slow; we don't want `devtools::test()` to take a minute per run. Use `testthat::with_mocked_bindings()` to stub `fit_model` + `extract_posteriors`.

```r
# tests/testthat/test-model-league.R

test_that("fit_league writes beliefs_latest and beliefs_archive", {
  # Set up a tmp data root with the mini fixture materialised.
  root <- withr::local_tempdir()
  results <- arrow::read_parquet(
    testthat::test_path("fixtures", "model", "mini_results.parquet")
  )
  schedules <- arrow::read_parquet(
    testthat::test_path("fixtures", "model", "mini_schedules.parquet")
  )
  write_table(results,   "results",   root = root)
  write_table(schedules, "schedules", root = root)

  # Register a league in the runtime so load_leagues() returns it. We bypass
  # the YAML by building a minimal list ourselves.
  mini_league <- list(
    sport = "basketball", country = "iceland",
    sexes = c("male"), active = TRUE,
    data_source = list(results = "kki_basketball",
                       schedule = "kki_basketball",
                       odds = "lengjan_odds"),
    stan_model = "basketball_iceland/2d_student_t_scalarsigma.stan"
  )

  # Build a mocked posterior — 5 draws × N_pred matches
  fake_beliefs <- tibble::tibble(
    sport = "basketball", country = "iceland", sex = "male",
    fit_date = Sys.Date(),
    match_date = rep(Sys.Date() + c(3L, 7L), each = 5L),
    home_team = rep(c("Alpha", "Charlie"), each = 5L),
    away_team = rep(c("Bravo",  "Delta"),   each = 5L),
    draw_id = rep(1:5, times = 2L),
    home_goals = runif(10, 70, 100),
    away_goals = runif(10, 70, 100)
  )

  testthat::local_mocked_bindings(
    fit_model          = function(...) structure(list(), class = "CmdStanMCMC"),
    extract_posteriors = function(...) fake_beliefs,
    # Bypass load_leagues — fit_league accepts a league list directly via the
    # `league` override arg (see impl).
    .package = "sports"
  )

  out <- fit_league(league = mini_league, sex = "male",
                    fit_date = Sys.Date(), root = root,
                    stan_dir = here::here("Stan"))

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 10L)

  # beliefs_latest readable
  bl <- read_table("beliefs_latest", root = root,
                   filter = list(sport = "basketball", country = "iceland", sex = "male"))
  expect_equal(nrow(bl), 10L)

  # beliefs_archive readable (partition includes fit_date)
  ba <- read_table("beliefs_archive", root = root,
                   filter = list(sport = "basketball", country = "iceland", sex = "male"))
  expect_equal(nrow(ba), 10L)
})

test_that("fit_league (write_archive = FALSE) only writes latest", {
  root <- withr::local_tempdir()
  results <- arrow::read_parquet(
    testthat::test_path("fixtures", "model", "mini_results.parquet")
  )
  schedules <- arrow::read_parquet(
    testthat::test_path("fixtures", "model", "mini_schedules.parquet")
  )
  write_table(results,   "results",   root = root)
  write_table(schedules, "schedules", root = root)

  mini_league <- list(
    sport = "basketball", country = "iceland", sexes = "male",
    stan_model = "basketball_iceland/2d_student_t_scalarsigma.stan"
  )

  fake_beliefs <- tibble::tibble(
    sport = "basketball", country = "iceland", sex = "male",
    fit_date = Sys.Date(),
    match_date = as.Date("2026-05-01"),
    home_team = "Alpha", away_team = "Bravo",
    draw_id = 1L, home_goals = 85, away_goals = 80
  )

  testthat::local_mocked_bindings(
    fit_model          = function(...) structure(list(), class = "CmdStanMCMC"),
    extract_posteriors = function(...) fake_beliefs,
    .package = "sports"
  )

  fit_league(league = mini_league, sex = "male", root = root, write_archive = FALSE)

  expect_equal(nrow(read_table("beliefs_latest", root = root)), 1L)
  expect_false(
    dir.exists(file.path(root, "beliefs", "archive"))
  )
})
```

- [ ] **Step 3: Verify failure**

```bash
Rscript -e 'devtools::test(filter = "model-league")'
```

- [ ] **Step 4: Implement `R/model-league.R`**

```r
#' @include model-prepare.R model-fit.R model-posteriors.R storage.R config.R
NULL

#' End-to-end: prepare data, fit Stan, extract posteriors, write beliefs.
#'
#' Signature supports two call modes:
#'   1. By league_key: `fit_league("basketball_iceland", "male")`
#'      — looks up the league via `load_leagues()`.
#'   2. By league list:  `fit_league(league = <list>, sex = "male")`
#'      — bypasses load_leagues; used by tests + one-off runs.
#'
#' Writes `data/beliefs/latest/` (snapshot — overwritten per call) and
#' optionally `data/beliefs/archive/sport=X/country=Y/sex=Z/fit_date=D/`.
#'
#' @param league_key Key into `load_leagues()`. Mutually exclusive with `league`.
#' @param league Pre-loaded league list. Mutually exclusive with `league_key`.
#' @param sex "male" or "female".
#' @param fit_date Date stamped on every posterior row. Default today.
#' @param end_date Training cutoff — matches on/before this go into stan_data.
#'   Default = `fit_date`.
#' @param root Data root. Default `here::here("data")`.
#' @param stan_dir Stan-model root. Default `here::here("Stan")`.
#' @param method,iter_warmup,iter_sampling,chains,seed Passed to `fit_model()`.
#' @param from_season Optional integer — drop training matches with season <.
#' @param schedule_horizon_days How far ahead to predict. Default 14.
#' @param write_archive Write to beliefs/archive/ in addition to latest?
#'   Default TRUE.
#' @return Tibble of beliefs (invisibly).
#' @export
fit_league <- function(league_key = NULL,
                       league     = NULL,
                       sex,
                       fit_date   = Sys.Date(),
                       end_date   = fit_date,
                       root       = here::here("data"),
                       stan_dir   = here::here("Stan"),
                       method     = "sample",
                       iter_warmup   = 1000L,
                       iter_sampling = 1000L,
                       chains        = 4L,
                       seed          = NULL,
                       from_season           = NULL,
                       schedule_horizon_days = 14L,
                       write_archive = TRUE) {
  if (is.null(league) == is.null(league_key)) {
    stop("Exactly one of `league_key` or `league` must be supplied",
         call. = FALSE)
  }
  if (is.null(league)) {
    leagues <- load_leagues()
    if (!league_key %in% names(leagues)) {
      stop("Unknown league: ", league_key,
           " (available: ", paste(names(leagues), collapse = ", "), ")",
           call. = FALSE)
    }
    league <- leagues[[league_key]]
  }
  stopifnot(sex %in% c("male", "female"))
  stopifnot(!is.null(league$stan_model))

  prep <- prepare_data(league, sex,
                       end_date = end_date, root = root,
                       from_season = from_season,
                       schedule_horizon_days = schedule_horizon_days)

  stan_path <- file.path(stan_dir, league$stan_model)
  if (!file.exists(stan_path)) {
    stop("Stan model missing: ", stan_path, call. = FALSE)
  }

  fit <- fit_model(
    stan_data       = prep$stan_data,
    stan_model_path = stan_path,
    method          = method,
    chains          = chains,
    iter_warmup     = iter_warmup,
    iter_sampling   = iter_sampling,
    seed            = seed
  )

  beliefs <- extract_posteriors(fit, prep$pred_d,
                                league = league, sex = sex,
                                fit_date = fit_date)

  if (nrow(beliefs) > 0L) {
    write_table(beliefs, "beliefs_latest", root = root)
    if (isTRUE(write_archive)) {
      write_table(beliefs, "beliefs_archive", root = root)
    }
  } else {
    cli::cli_alert_warning(
      "fit_league({league$sport}/{league$country}/{sex}): no predictions — skipping belief writes")
  }

  invisible(beliefs)
}
```

- [ ] **Step 5: Update roxygen + collate, verify tests**

```bash
Rscript -e 'roxygen2::roxygenise(); roxygen2::update_collate(".")'
Rscript -e 'devtools::test(filter = "model-league")'
```

Expected: 2 passes.

- [ ] **Step 6: Commit**

```bash
git add R/model-league.R NAMESPACE DESCRIPTION tests/testthat/test-model-league.R \
        R/storage.R
git commit -m "feat: fit_league() orchestrator — prep_data + fit + write beliefs

fit_league(league_key, sex) chains prepare_data -> fit_model ->
extract_posteriors -> write_table. Writes beliefs/latest/ (snapshot,
sport/country/sex-partitioned) and beliefs/archive/ (accretive,
additionally fit_date-partitioned). beliefs_archive is the only way
to later reconstruct 'what did the model believe at time T?' for
walk-forward research.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>"
```

---

## Task 6: Backfill — fit all 3 Icelandic × both sexes + integration test

**Files:**

- Create: `scripts/fit_all.R`
- Create: `tests/testthat/test-model-integration.R`

**Purpose:** Run fits end-to-end for all six (league, sex) combos and validate that `beliefs/{latest,archive}/` is populated + schema-clean. Wall-clock ~30–60 min total (basketball + handball ~5–8 min each, football ~10–15 min each).

- [ ] **Step 1: Write `scripts/fit_all.R`**

```r
#!/usr/bin/env Rscript
# Fit all active-league x both-sex combos and write beliefs/.
#
# Wall-clock ~30-60 min dominated by football Iceland (BVP ~10-15 min/sex)
# and handball (~5-8 min/sex). Basketball ~5 min/sex.
#
# Usage:
#   Rscript scripts/fit_all.R                      # MCMC 1000+1000 iters, seed 42
#   Rscript scripts/fit_all.R --iter 200 --seed 7  # quick smoke

suppressPackageStartupMessages(devtools::load_all(here::here()))

args <- commandArgs(trailingOnly = TRUE)
get_flag <- function(name, default = NULL, parse = identity) {
  i <- which(args == paste0("--", name))
  if (length(i) == 0L) return(default)
  parse(args[[i + 1L]])
}
iter <- get_flag("iter", 1000L, as.integer)
seed <- get_flag("seed", 42L, as.integer)

leagues <- load_leagues()
active <- leagues[vapply(leagues, function(l) isTRUE(l$active), logical(1))]

t0 <- Sys.time()
for (key in names(active)) {
  league <- active[[key]]
  for (sex in league$sexes) {
    cli::cli_h1("{key} / {sex}")
    tryCatch({
      beliefs <- fit_league(
        league = league, sex = sex,
        iter_warmup = iter, iter_sampling = iter, seed = seed
      )
      cli::cli_alert_success("wrote {nrow(beliefs)} beliefs rows")
    }, error = function(e) {
      cli::cli_alert_danger("FAILED {key}/{sex}: {conditionMessage(e)}")
    })
  }
}
cli::cli_alert_info("Total wall-clock: {round(difftime(Sys.time(), t0, units = 'mins'), 1)} min")
```

- [ ] **Step 2: Run the backfill (detached)**

Use `nohup ... & disown` per the 2026-04-24 memory-note about CCD forks killing long-running child processes.

```bash
cd /Users/brynjolfurjonsson/sports
nohup Rscript scripts/fit_all.R > /tmp/fit_all.log 2>&1 &
disown

# Monitor:
tail -f /tmp/fit_all.log
```

Expected: six successful fits, total ~30-60 min. First run compiles each Stan model (~60 s each); subsequent runs reuse the cache.

If any fit fails:

1. Read the error in `/tmp/fit_all.log`
2. If Stan diagnostic (Rhat > 1.1, ESS_bulk < 400, divergences > 5%), note it but do not block — document in the commit message
3. If prepare_data / write_table error, fix the underlying issue and re-run (idempotent: beliefs_latest is overwrite-semantics)

- [ ] **Step 3: Write integration test**

```r
# tests/testthat/test-model-integration.R

skip_if_no_beliefs <- function() {
  if (!dir.exists(here::here("data", "beliefs", "latest"))) {
    testthat::skip("data/beliefs/latest absent; fit_all.R hasn't run")
  }
}

test_that("beliefs/latest covers all 3 Icelandic sports x both sexes", {
  skip_if_no_beliefs()
  bl <- read_table("beliefs_latest", filter = list(country = "iceland"))

  counts <- dplyr::count(bl, sport, sex)
  expect_equal(nrow(counts), 6L, label = "3 sports x 2 sexes")
  # At least one belief row — if predictions are empty-window, the backfill
  # would have skipped with a warning, not written anything. Expect > 100
  # rows per bucket (≥ 1 match * 100+ draws).
  expect_gt(min(counts$n), 100L)
})

test_that("beliefs columns match schemas()\$beliefs_latest exactly", {
  skip_if_no_beliefs()
  bl <- read_table("beliefs_latest", filter = list(country = "iceland"))

  expected <- names(schemas()$beliefs_latest)
  expect_setequal(names(bl), expected)
  expect_s3_class(bl$match_date, "Date")
  expect_s3_class(bl$fit_date, "Date")
  expect_type(bl$draw_id, "integer")
  expect_type(bl$home_goals, "double")
  expect_type(bl$away_goals, "double")
})

test_that("beliefs have physically plausible values per sport", {
  skip_if_no_beliefs()
  bl <- read_table("beliefs_latest", filter = list(country = "iceland"))

  by_sport <- dplyr::summarise(
    dplyr::group_by(bl, sport),
    mean_home = mean(home_goals), mean_away = mean(away_goals), .groups = "drop"
  )

  # Basketball: mean ~70-120 per team
  basket <- by_sport[by_sport$sport == "basketball", , drop = FALSE]
  expect_true(all(basket$mean_home > 50 & basket$mean_home < 150))

  # Handball: mean ~20-35 per team
  handball <- by_sport[by_sport$sport == "handball", , drop = FALSE]
  expect_true(all(handball$mean_home > 15 & handball$mean_home < 45))

  # Football: mean ~0.5-3 per team
  football <- by_sport[by_sport$sport == "football", , drop = FALSE]
  expect_true(all(football$mean_home > 0 & football$mean_home < 5))
})

test_that("beliefs/archive contains at least today's fit_date partition", {
  skip_if_no_beliefs()
  ba <- read_table("beliefs_archive", filter = list(country = "iceland"))
  expect_true(Sys.Date() %in% ba$fit_date)
})

test_that("per-match draw count is constant within a (sport, sex) bucket", {
  skip_if_no_beliefs()
  bl <- read_table("beliefs_latest", filter = list(country = "iceland"))

  per_bucket <- bl |>
    dplyr::group_by(sport, sex, match_date, home_team, away_team) |>
    dplyr::summarise(n_draws = dplyr::n(), .groups = "drop") |>
    dplyr::group_by(sport, sex) |>
    dplyr::summarise(sd_draws = stats::sd(n_draws), .groups = "drop")

  # Within each (sport, sex), every match should have the same number of
  # draws (chains * iter_sampling, usually 4000).
  expect_true(all(per_bucket$sd_draws == 0))
})
```

- [ ] **Step 4: Run integration test**

```bash
Rscript -e 'devtools::test(filter = "model-integration")'
```

Expected: 5 passes. If any plausibility check fails, DO NOT silently loosen the bound — investigate the underlying fit (likely a prepare_data or Stan data-block mismatch).

- [ ] **Step 5: Rebuild DuckDB + sanity query**

```bash
Rscript -e 'sports::rebuild_duckdb()'
Rscript -e '
con <- DBI::dbConnect(duckdb::duckdb(), here::here("sports.duckdb"), read_only = TRUE)
print(DBI::dbGetQuery(con, "
  SELECT sport, sex,
         COUNT(DISTINCT match_date) AS n_matches,
         COUNT(*) AS n_rows,
         MIN(fit_date) AS fit_date,
         ROUND(AVG(home_goals), 2) AS mean_home,
         ROUND(AVG(away_goals), 2) AS mean_away
  FROM beliefs_latest
  WHERE country = '\''iceland'\''
  GROUP BY 1, 2 ORDER BY 1, 2
"))
'
```

Record the numbers — they become the baseline for Plan 4 (decide layer).

- [ ] **Step 6: Commit**

```bash
git add scripts/fit_all.R tests/testthat/test-model-integration.R \
        data/beliefs/
git commit -m "feat: fit all 3 Icelandic leagues, populate beliefs/{latest,archive}/

Runs scripts/fit_all.R end-to-end for all 6 (league, sex) combos.
Integration test validates schema + per-sport score plausibility.
Wall-clock ~30-60 min; basketball + handball ~5-8 min each, football
BVP ~10-15 min each.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>"
```

---

## Task 7: Golden-output sanity gate vs `_legacy/` backup (local-only)

**Files:**

- Create: `tests/testthat/test-model-golden.R`

**Purpose:** Compare the new pipeline's posterior predictive means against the pre-migration `fit.rds` backups at `/Users/brynjolfurjonsson/sports-backup-20260424-163153/`. Skips on CI (the backup isn't committed). This is NOT a bit-exact reproducibility test — the data sources are different (new federation scrapers vs legacy Excel / Chromote / CSV) and Stan MCMC is stochastic. It's a sanity check: if either dataset changed dramatically, this catches it.

Tolerance: per-match posterior home-goals mean within `tol` of the legacy fit's mean for the same (home_team, away_team) match where it exists in both. `tol` is sport-specific (basketball ± 8, handball ± 3, football ± 0.5).

- [ ] **Step 1: Write the golden test**

```r
# tests/testthat/test-model-golden.R
#
# Sanity gate: new pipeline's posterior predictive means vs the pre-migration
# fit.rds backups. Local-only — skips on CI (backup isn't committed).
#
# This is NOT bit-exact reproducibility. Data sources differ (new federation
# scrapers vs legacy Excel/Chromote), Stan MCMC is stochastic, and 2 months
# of matches have been played since the backup was taken. The gate asks:
#   does the new pipeline produce predictions in the same ballpark as the
#   old, for the matches the two share?

BACKUP_ROOT <- "/Users/brynjolfurjonsson/sports-backup-20260424-163153"

skip_if_no_backup <- function() {
  if (!dir.exists(BACKUP_ROOT)) {
    testthat::skip(paste("backup absent:", BACKUP_ROOT))
  }
  if (!dir.exists(here::here("data", "beliefs", "latest"))) {
    testthat::skip("new beliefs absent; fit_all.R hasn't run")
  }
}

# Load a legacy fit's posterior-means-per-match (sport, sex) from backup.
# Returns a tibble with (home_team, away_team, home_mean_legacy, away_mean_legacy).
legacy_means <- function(sport, sex) {
  fit_path <- file.path(BACKUP_ROOT, "Sports", sport, "iceland",
                        "results", sex, "fit.rds")
  pred_path <- file.path(BACKUP_ROOT, "Sports", sport, "iceland",
                         "results", sex, "pred_d.csv")
  if (!file.exists(fit_path) || !file.exists(pred_path)) return(NULL)

  fit <- readRDS(fit_path)
  pred_d <- readr::read_csv(pred_path, show_col_types = FALSE,
                            locale = readr::locale(encoding = "UTF-8"))

  draws <- posterior::as_draws_df(fit$draws(c("goals1_pred", "goals2_pred"))) |>
    tibble::as_tibble()
  long <- draws |>
    tidyr::pivot_longer(-c(.chain, .iteration, .draw),
                        names_to = "parameter", values_to = "value") |>
    dplyr::mutate(
      type = dplyr::if_else(stringr::str_detect(parameter, "^goals1_pred"),
                            "home_goals", "away_goals"),
      game_nr = as.integer(stringr::str_match(parameter, "\\[(\\d+)\\]$")[, 2])
    ) |>
    dplyr::group_by(.data$game_nr, .data$type) |>
    dplyr::summarise(mean_val = mean(.data$value), .groups = "drop") |>
    tidyr::pivot_wider(names_from = .data$type, values_from = .data$mean_val)

  long |>
    dplyr::inner_join(pred_d[, c("game_nr", "home", "away")], by = "game_nr") |>
    dplyr::transmute(home_team = .data$home, away_team = .data$away,
                     home_mean_legacy = .data$home_goals,
                     away_mean_legacy = .data$away_goals)
}

new_means <- function(sport, sex) {
  bl <- read_table("beliefs_latest",
                   filter = list(sport = sport, country = "iceland", sex = sex))
  bl |>
    dplyr::group_by(home_team, away_team) |>
    dplyr::summarise(
      home_mean_new = mean(home_goals),
      away_mean_new = mean(away_goals),
      .groups = "drop"
    )
}

TOLERANCES <- list(
  basketball = c(home = 8, away = 8),
  handball   = c(home = 3, away = 3),
  football   = c(home = 0.5, away = 0.5)
)

check_one <- function(sport, sex) {
  legacy <- legacy_means(sport, sex)
  skip_if(is.null(legacy), paste("no legacy fit for", sport, "/", sex))

  new <- new_means(sport, sex)
  merged <- dplyr::inner_join(legacy, new,
                              by = c("home_team", "away_team"))
  skip_if(nrow(merged) == 0L,
          paste("no overlapping matches for", sport, "/", sex))

  tol <- TOLERANCES[[sport]]
  home_diff <- abs(merged$home_mean_new - merged$home_mean_legacy)
  away_diff <- abs(merged$away_mean_new - merged$away_mean_legacy)

  # Allow up to 20% of the overlap to exceed tolerance — this covers matches
  # where the post-backup 2 months of data meaningfully shifted beliefs.
  home_bad <- mean(home_diff > tol[["home"]], na.rm = TRUE)
  away_bad <- mean(away_diff > tol[["away"]], na.rm = TRUE)
  expect_lt(home_bad, 0.20,
            label = paste(sport, sex, "home-mean within", tol[["home"]]))
  expect_lt(away_bad, 0.20,
            label = paste(sport, sex, "away-mean within", tol[["away"]]))
}

test_that("basketball male beliefs are sane vs backup", {
  skip_if_no_backup(); check_one("basketball", "male")
})
test_that("basketball female beliefs are sane vs backup", {
  skip_if_no_backup(); check_one("basketball", "female")
})
test_that("handball male beliefs are sane vs backup", {
  skip_if_no_backup(); check_one("handball", "male")
})
test_that("handball female beliefs are sane vs backup", {
  skip_if_no_backup(); check_one("handball", "female")
})
test_that("football male beliefs are sane vs backup", {
  skip_if_no_backup(); check_one("football", "male")
})
test_that("football female beliefs are sane vs backup", {
  skip_if_no_backup(); check_one("football", "female")
})
```

- [ ] **Step 2: Run the golden gate**

```bash
Rscript -e 'devtools::test(filter = "model-golden")'
```

Expected: 6 test blocks. On a fresh clone where the backup directory is absent, all skip. On the dev machine, all pass if the new pipeline is sane.

If any fail by a wide margin: compare `pred_d` between backup and new — often the discrepancy is upcoming matches not being in both (backup's `pred_d.csv` is a frozen snapshot). The test ignores non-overlapping matches via `inner_join`, so failures point at genuine model drift.

- [ ] **Step 3: Commit**

```bash
git add tests/testthat/test-model-golden.R
git commit -m "test: golden-output sanity gate vs _legacy/ backup fits

Compares new posterior predictive means vs pre-migration fit.rds
backups at /Users/brynjolfurjonsson/sports-backup-20260424-163153/.
Local-only — skips on CI. Not bit-exact reproducibility (data
sources + samples differ); rather, asks 'are we in the same
ballpark?'.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>"
```

---

## Task 8: Update `CLAUDE.md` + commit

**Files:**

- Modify: `CLAUDE.md`

- [ ] **Step 1: Update the Status table**

Change Plan 3's row:

```
| **3: Model layer** | prepare_data + fit_league + beliefs store for 3 Icelandic leagues | ✅ Complete — ~N beliefs rows; ~M draws/match |
```

(fill `N` / `M` from Task 6's DuckDB query).

- [ ] **Step 2: Add a "Model layer" section under Conventions**

```markdown
### Model layer

- `fit_league(league_key, sex)` is the only public entry point. Loads config,
  calls `prepare_data()` → `fit_model()` → `extract_posteriors()`, writes
  `beliefs/latest/` (overwrite) and `beliefs/archive/` (accretive per `fit_date`).
- `prepare_data()` is pure — reads Parquet facts, returns
  `list(stan_data, pred_d, teams)`. No file I/O beyond `read_table()`.
- `fit_model()` is a pure cmdstanr wrapper — takes stan_data + stan_path, returns
  the fit. Callers save to disk (or hold in memory).
- `extract_posteriors()` materialises posterior draws as the canonical
  `beliefs_latest` tibble (per-draw-per-match).
- Stan models live in `Stan/{league_key}/{file}.stan`. `leagues.yml`'s
  `stan_model` field uses this relative path.
- Backfill with `Rscript scripts/fit_all.R`. Wall-clock ~30-60 min for all
  6 (league, sex) combos. Run detached: `nohup Rscript scripts/fit_all.R & disown`.
```

- [ ] **Step 3: Update the directory tree**

Add under `R/`:

```
│   ├── model-prepare.R             # prepare_data()
│   ├── model-fit.R                 # fit_model() cmdstanr wrapper
│   ├── model-posteriors.R          # extract_posteriors()
│   ├── model-league.R              # fit_league() orchestrator
```

Replace the `Stan/` placeholder line with the concrete tree:

```
├── Stan/
│   ├── basketball_iceland/2d_student_t_scalarsigma.stan
│   ├── handball_iceland/2d_student_t.stan
│   └── football_iceland/bivariate_poisson_no_inflation.stan
```

Update the `data/beliefs/` section:

```
│   ├── beliefs/
│   │   ├── latest/sport=X/country=Y/sex=Z/beliefs.parquet
│   │   └── archive/sport=X/country=Y/sex=Z/fit_date=YYYY-MM-DD/beliefs.parquet
```

Remove `(Plan 3 populates)` annotations on beliefs paths.

- [ ] **Step 4: Commit**

```bash
git add CLAUDE.md
git commit -m "docs: update CLAUDE.md — Plan 3 (model layer) complete

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>"
```

---

## Final validation

- [ ] **Step 1: Full test suite**

```bash
cd /Users/brynjolfurjonsson/sports
Rscript -e 'devtools::test()'
```

Expected: 169 (Plan 2 baseline) + ~20 new assertions ≈ 190+ passing, 0 failures. Skips allowed on the golden gate + Stan compile on machines without cmdstan.

- [ ] **Step 2: Query the belief layer end-to-end**

```bash
Rscript -e '
sports::rebuild_duckdb()
con <- DBI::dbConnect(duckdb::duckdb(), here::here("sports.duckdb"), read_only = TRUE)
print(DBI::dbGetQuery(con, "
  SELECT sport, sex,
         COUNT(DISTINCT match_date) AS n_upcoming_matches,
         COUNT(DISTINCT draw_id) AS n_draws,
         ROUND(AVG(home_goals), 2) AS mean_home,
         ROUND(AVG(away_goals), 2) AS mean_away,
         MAX(fit_date) AS fit_date
  FROM beliefs_latest
  WHERE country = '\''iceland'\''
  GROUP BY 1, 2 ORDER BY 1, 2
"))
'
```

- [ ] **Step 3: Push**

```bash
git push origin main
```

---

## What this plan achieves

- Canonical model layer — one `prepare_data()` path replaces the legacy three-way split.
- `data/beliefs/{latest,archive}/` populated for all three active Icelandic leagues × both sexes.
- Golden-output sanity gate against the pre-migration backup, skippable on CI.
- Plan 4 (decide layer) now reads beliefs + odds, no compute dependency on Stan.

## Risks & mitigations

- **prepare_data drift vs legacy.** The three-way collapse means a subtle column or timediff-matrix difference could shift every fit. Mitigation: Task 2's tests pin the expected stan_data shape against a hand-crafted mini fixture; Task 7's golden gate catches large drifts vs the 2026-04-24 backup.
- **Stan compilation wall-clock.** Each of the three models takes 30-60 s to compile on first run. Mitigation: cmdstanr caches compiled binaries in `~/.cmdstanr/`; backfill scripts only pay this cost once per machine.
- **Backfill failures mid-run.** One of six fits erroring shouldn't lose the other five. Mitigation: `scripts/fit_all.R` wraps each `fit_league()` call in `tryCatch` with a red `cli_alert_danger` and continues.
- **beliefs_archive partition explosion.** Running the backfill daily adds one `fit_date` partition per day per (sport, sex) = 6 partitions/day = ~2200/year. Parquet on hive can handle this but the repo size grows. Mitigation: beliefs_archive entries are small (~10 KB each), ~25 MB/year — manageable. If it becomes a problem, add a prune script in Plan 5's orchestration layer.
- **Golden gate false positives from OOS data drift.** Two months of new matches between backup and new fit will legitimately shift some predictions. Mitigation: the gate allows up to 20% of overlapping matches to exceed tolerance, and uses generous per-sport tolerances.
