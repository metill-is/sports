# Final-Positions History Backfill — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Regenerate `final_positions_history.json` for every football league division (male + female) so the finishing-heatmap round-slider shows a *correct full-season* placement forecast at each past round — each round computed from a model fit on games up to and including that round (no lookahead).

**Architecture:** A retrospective backfill. For each past round `R` of the 2026 season, fit the cross-division football model on games through round `R` (`prepare_data` + `fit_model` at the round-`R` cutoff date), then run the Part-1 full-season simulator (`simulate_league_season()`) on each league division using that fit's latest-round strengths and the schedule remaining after round `R`. Stamp each round's placements with `as_of = round-R cutoff date` and assemble all rounds into a fresh `final_positions_history.json` per (sex, division). Reuses the existing `round-cutoff.R`, `prepare_data`, `fit_model`, `.extract_sim_inputs_pfi`, and `simulate_league_season` machinery — no Stan model change, no new inference math.

**Tech Stack:** R (package `sports`), cmdstanr (MCMC), testthat 3e, arrow/jsonlite, dplyr/tidyr. Output JSONs land in `data/publish/football/iceland/{karla,kvenna}-{bd,ld,2deild,3deild}/` and reach the platform via the existing hourly `pull-sports-data.yml` rsync.

**Prerequisite:** Part 1 (the `simulate_league_season()` full-season fix, branch `fix/league-season-full-horizon`) is merged or present — this plan builds directly on it.

---

## Background facts (verified)

- **Round grid.** Rounds are derived chronologically (the `round` column is `NA` in the 2026 schedule). `compute_round_cutoff_date(results, season, round_cutoff, top_division = "BD")` (`R/round-cutoff.R:26`) returns the date by which all BD teams have played `round_cutoff` matches, or `NULL` if that round is not yet complete. The same BD-round grid is used for both sexes (each has a Besta deild). LD divisions are snapshotted at the *same dates*; each division's per-record `round` label is that division's own games-played count at the cutoff (see Task 3).
- **One fit covers all divisions.** The football model is a single cross-division fit; `extract_football_iceland()` slices it per division. So the backfill needs **one fit per (sex, round)**, not per division.
- **Strength as of round R.** The Stan GQ exports `cur_offense_away = offense[N_rounds]` etc. — the latest *training* round's strength. A fit trained through round `R` therefore has `cur_*` = strength as of round `R`. `.extract_sim_inputs_pfi(fit, prep$teams)` (`R/extract-football-iceland.R:451`) pulls the per-draw `cur_offense/cur_defense/home_advantage_off/home_advantage_def` + `mean_log_goals/alpha_mu3/beta_mu3_strength_diff`. **The fit MUST be paired with the `prep` that built its `stan_data`** (same `end_date` + `schedule_horizon_days`) or `teams$team[team_idx]` misassigns strengths (the `.extract_sim_inputs_pfi` warning at line 463). This plan always fits and extracts in the same pass, so the pairing is guaranteed.
- **Output file shape.** `final_positions_history.json` = `{schema_version: 1, records: [...]}`, each record `{as_of, generated_at, round, season, team, placement, probability}`, keyed by `(as_of, team, placement)` (`R/publish-football-iceland.R:239`). The platform's `finishing-heatmap.js` builds the round-slider from the distinct `as_of` values (label = the record's `round`). The backfill **replaces** the file wholesale (the existing rows are the buggy 2-round projections).
- **Publish dirs.** `data/publish/football/iceland/{sex_slug}-{div_slug}/` with `sex_slug ∈ {karla=male, kvenna=female}`, `div_slug` from `config/leagues.yml::publish_divisions` (`BD→bd`, `LD1→ld`, `LD2→2deild`, `LD3→3deild`). CUP is excluded (knockout — no league table).
- **Existing per-round fits.** 5 already on disk (`fits_by_round/.../round={01..04}` male, `round=01` female). The backfill **re-fits all rounds in one pass by default** (guarantees fit/prep consistency against current facts data); reuse is an optional speedup gated on a clean `.extract_sim_inputs_pfi` (no team-count-mismatch warning).
- **Cost.** Male BD ≈ 10 completed rounds, female BD ≈ 12 → ~22 MCMC fits (4 chains × 1000). Minutes each; the whole backfill is an overnight / `workflow_dispatch` one-off. The simulations themselves are seconds.

---

## File Structure

| File | Responsibility | Action |
|---|---|---|
| `R/extract-football-iceland.R` | Daily extract; gains a shared base/remaining helper + a per-round entry point | Modify |
| `R/round-cutoff.R` | Round-cutoff dates; gains `completed_bd_rounds()` enumerator | Modify |
| `R/backfill-final-positions.R` | The per-round fit→sim→records driver function | Create |
| `scripts/backfill_final_positions_history.R` | Runnable orchestrator (loops sexes × rounds, writes JSONs) | Create |
| `tests/testthat/test-backfill-final-positions.R` | Unit tests for the helper, the round enumerator, the record builder | Create |
| `.github/workflows/backfill-final-positions.yml` | Optional `workflow_dispatch` runner | Create (Task 6, optional) |

---

## Task 1: Extract a shared base-standings + remaining-fixtures helper (DRY)

Part 1 put the base-standings + remaining-fixtures construction inline in `.extract_division_parquets_pfi`. The backfill needs the identical logic, so extract it into one tested helper that both call.

**Files:**
- Modify: `R/extract-football-iceland.R` (the block inside `.extract_division_parquets_pfi` that builds `realised`/`base_standings`/`remaining_fixtures`)
- Test: `tests/testthat/test-backfill-final-positions.R`

- [ ] **Step 1: Write the failing test**

```r
# tests/testthat/test-backfill-final-positions.R
test_that(".league_base_and_remaining_pfi builds the realised table and unplayed fixtures", {
  played <- tibble::tibble(
    home_team = c("A", "B", "C"), away_team = c("B", "C", "A"),
    home_score = c(2L, 0L, 1L), away_score = c(0L, 0L, 1L),
    division = "BD", season = 2026L,
    match_date = as.Date(c("2026-05-01", "2026-05-02", "2026-05-03"))
  )
  current_top_teams <- tibble::tibble(team = c("A", "B", "C"))
  schedule <- tibble::tibble(
    home_team = c("A", "B", "C", "A"), away_team = c("C", "A", "B", "B"),
    division = c("BD", "BD", "BD", "LD1"),
    match_date = as.Date(c("2026-05-10", "2026-05-11", "2026-05-12", "2026-05-10"))
  )

  out <- .league_base_and_remaining_pfi(played, current_top_teams, schedule, "BD")

  a <- out$base_standings[out$base_standings$team == "A", ]
  expect_equal(a$base_points, 4L)   # win vs B (3) + draw vs C (1)
  expect_equal(a$base_gd, 2L)       # +2 vs B, 0 vs C
  expect_equal(a$base_gf, 3L)       # 2 vs B + 1 vs C
  # remaining: BD-only, both teams known, deduped, the LD1 row excluded
  expect_equal(nrow(out$remaining_fixtures), 3L)
  expect_true(all(c("home_team", "away_team") %in% names(out$remaining_fixtures)))
})

test_that(".league_base_and_remaining_pfi drops reschedule ghosts (same ordered pair twice)", {
  played <- tibble::tibble(
    home_team = character(), away_team = character(),
    home_score = integer(), away_score = integer(),
    division = character(), season = integer(), match_date = as.Date(character())
  )
  ctt <- tibble::tibble(team = c("A", "B"))
  schedule <- tibble::tibble(
    home_team = c("A", "A"), away_team = c("B", "B"), division = "BD",
    match_date = as.Date(c("2026-06-01", "2026-06-08")) # ghost + reschedule
  )
  out <- .league_base_and_remaining_pfi(played, ctt, schedule, "BD")
  expect_equal(nrow(out$remaining_fixtures), 1L) # deduped to the later date
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-backfill-final-positions.R")'`
Expected: FAIL — `could not find function ".league_base_and_remaining_pfi"`.

- [ ] **Step 3: Add the helper and call it from `.extract_division_parquets_pfi`**

Add to `R/extract-football-iceland.R` (top-level, near the other `.*_pfi` helpers):

```r
# Build the realised league table (points/GD/GF from played matches, every
# current-division team present) plus the unplayed fixtures (this division's
# scheduled matches not yet played, deduped on the ordered pair). Shared by the
# daily extract and the per-round backfill so the two never diverge.
.league_base_and_remaining_pfi <- function(played, current_top_teams,
                                           season_schedule, target_div) {
  played <- played[
    !is.na(played$home_score) & !is.na(played$away_score), ,
    drop = FALSE
  ]
  realised <- if (nrow(played) > 0L) {
    played |>
      dplyr::mutate(
        result = dplyr::case_when(
          .data$home_score > .data$away_score ~ "home",
          .data$home_score < .data$away_score ~ "away",
          TRUE ~ "tie"
        )
      ) |>
      tidyr::pivot_longer(c("home_team", "away_team"),
        names_to = "loc", values_to = "team"
      ) |>
      dplyr::mutate(
        loc = dplyr::if_else(.data$loc == "home_team", "home", "away"),
        gf = dplyr::if_else(.data$loc == "home", .data$home_score, .data$away_score),
        ga = dplyr::if_else(.data$loc == "home", .data$away_score, .data$home_score),
        pts = dplyr::case_when(
          .data$result == "tie" ~ 1L,
          .data$result == .data$loc ~ 3L,
          TRUE ~ 0L
        )
      ) |>
      dplyr::summarise(
        base_points = as.integer(sum(.data$pts)),
        base_gf = as.integer(sum(.data$gf)),
        base_ga = as.integer(sum(.data$ga)),
        .by = "team"
      ) |>
      dplyr::mutate(base_gd = .data$base_gf - .data$base_ga)
  } else {
    tibble::tibble(
      team = character(), base_points = integer(),
      base_gf = integer(), base_ga = integer(), base_gd = integer()
    )
  }

  base_standings <- current_top_teams |>
    dplyr::left_join(realised, by = "team") |>
    dplyr::mutate(
      base_points = dplyr::coalesce(.data$base_points, 0L),
      base_gf = dplyr::coalesce(.data$base_gf, 0L),
      base_gd = dplyr::coalesce(.data$base_gd, 0L)
    ) |>
    dplyr::select("team", "base_points", "base_gd", "base_gf")

  remaining_fixtures <- if (!is.null(season_schedule) && nrow(season_schedule) > 0L) {
    played_pair <- paste(played$home_team, played$away_team)
    season_schedule |>
      dplyr::filter(
        .data$division == target_div,
        .data$home_team %in% current_top_teams$team,
        .data$away_team %in% current_top_teams$team
      ) |>
      dplyr::mutate(.pair = paste(.data$home_team, .data$away_team)) |>
      dplyr::filter(!(.data$.pair %in% played_pair)) |>
      dplyr::arrange(dplyr::desc(.data$match_date)) |>
      dplyr::distinct(.data$.pair, .keep_all = TRUE) |>
      dplyr::select("home_team", "away_team")
  } else {
    tibble::tibble(home_team = character(), away_team = character())
  }

  list(base_standings = base_standings, remaining_fixtures = remaining_fixtures)
}
```

Then in `.extract_division_parquets_pfi`, replace the inline `played`/`realised`/`base_standings`/`remaining_fixtures` block (Part-1 code) with:

```r
  br <- .league_base_and_remaining_pfi(
    top_results, current_top_teams, season_schedule, target_div
  )
  base_standings <- br$base_standings
  remaining_fixtures <- br$remaining_fixtures
```

(Leave the `has_sim_inputs`/`teams_covered`/`simulate_league_season` call below it unchanged.)

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-backfill-final-positions.R")'`
Expected: PASS (2 tests). Then re-run the Part-1 suites to confirm no regression:
`Rscript -e 'devtools::load_all(quiet=TRUE); for (f in c("test-simulate-league-season.R","test-extract-football-iceland.R")) testthat::test_file(file.path("tests/testthat",f))'`
Expected: PASS / SKIP, 0 FAIL.

- [ ] **Step 5: Commit**

```bash
git add R/extract-football-iceland.R tests/testthat/test-backfill-final-positions.R
git commit -m "refactor(extract): share base/remaining helper for the season sim"
```

---

## Task 2: `completed_bd_rounds()` — enumerate past rounds with cutoff dates

**Files:**
- Modify: `R/round-cutoff.R`
- Test: `tests/testthat/test-backfill-final-positions.R`

- [ ] **Step 1: Write the failing test**

```r
test_that("completed_bd_rounds enumerates 1..R_max with ascending cutoff dates", {
  # 2 BD teams playing 3 synchronised rounds -> 3 completed rounds.
  results <- tibble::tibble(
    home_team = c("A", "B", "A"), away_team = c("B", "A", "B"),
    home_score = c(1L, 1L, 2L), away_score = c(0L, 1L, 1L),
    division = "BD", season = 2026L,
    match_date = as.Date(c("2026-04-10", "2026-04-17", "2026-04-24"))
  )
  rounds <- completed_bd_rounds(results, season = 2026L)
  expect_equal(rounds$round, 1:3)
  expect_true(all(diff(as.integer(rounds$cutoff_date)) > 0))
  expect_equal(rounds$cutoff_date[1], as.Date("2026-04-10"))
})
```

- [ ] **Step 2: Run to verify it fails**

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-backfill-final-positions.R")'`
Expected: FAIL — `could not find function "completed_bd_rounds"`.

- [ ] **Step 3: Implement**

Add to `R/round-cutoff.R`:

```r
#' Enumerate completed top-division rounds with their cutoff dates.
#'
#' Walks `round_cutoff = 1, 2, ...` calling [compute_round_cutoff_date()] until
#' it returns `NULL` (round not yet complete). Returns one row per completed
#' round.
#'
#' @inheritParams compute_round_cutoff_date
#' @return tibble(`round` int, `cutoff_date` Date), ascending. Empty if none.
#' @export
completed_bd_rounds <- function(results, season, top_division = "BD") {
  out <- list()
  r <- 1L
  repeat {
    d <- compute_round_cutoff_date(results, season, r, top_division)
    if (is.null(d)) break
    out[[length(out) + 1L]] <- tibble::tibble(round = r, cutoff_date = d)
    r <- r + 1L
  }
  if (length(out) == 0L) {
    return(tibble::tibble(round = integer(), cutoff_date = as.Date(character())))
  }
  dplyr::bind_rows(out)
}
```

- [ ] **Step 4: Run to verify it passes**

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-backfill-final-positions.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/round-cutoff.R tests/testthat/test-backfill-final-positions.R
git commit -m "feat(round-cutoff): enumerate completed rounds with cutoff dates"
```

---

## Task 3: `build_round_final_positions()` — one round's placement records from a paired (fit, prep)

Given a fit + its prep (paired), the realised results, the schedule, and the round/cutoff, produce the stamped `final_positions` records for every league division. The per-record `round` label is each division's own games-played count at the cutoff (BD's is the slider round; LD's reflects its own cadence).

**Files:**
- Create: `R/backfill-final-positions.R`
- Test: `tests/testthat/test-backfill-final-positions.R`

- [ ] **Step 1: Write the failing test** (pure record-shaping; strengths mocked via a tiny sim_inputs so no fit is needed)

```r
test_that("build_round_final_positions stamps as_of/round/division and rows sum to 1 per team", {
  # Mock the strength extraction so the test needs no Stan fit.
  local_mocked_bindings(
    .extract_sim_inputs_pfi = function(fit, teams) {
      tm <- tidyr::expand_grid(team = c("A", "B", "C"), .draw = 1:40) |>
        dplyr::mutate(cur_offense = 0, cur_defense = 0,
                      home_advantage_off = 0, home_advantage_def = 0)
      sc <- tibble::tibble(.draw = 1:40, mean_log_goals = log(1.5),
                           alpha_mu3 = -3, beta_mu3_strength_diff = 0)
      list(team = tm, scalar = sc)
    },
    .package = "sports"
  )
  results <- tibble::tibble(
    home_team = c("A", "B"), away_team = c("B", "C"),
    home_score = c(2L, 0L), away_score = c(0L, 1L),
    division = "BD", season = 2026L,
    match_date = as.Date(c("2026-05-01", "2026-05-01"))
  )
  schedule <- tibble::tibble(
    home_team = c("C", "A"), away_team = c("A", "C"), division = "BD",
    match_date = as.Date(c("2026-05-08", "2026-05-15"))
  )
  recs <- build_round_final_positions(
    fit = NULL, prep = list(teams = tibble::tibble(team = c("A", "B", "C"))),
    results = results, season_schedule = schedule,
    round_idx = 1L, cutoff_date = as.Date("2026-05-01"),
    season = 2026L, target_divs = "BD", generated_at = "2026-06-11T00:00:00+0000"
  )
  expect_setequal(names(recs),
    c("as_of", "generated_at", "round", "season", "division",
      "team", "placement", "probability"))
  expect_true(all(recs$as_of == "2026-05-01"))
  expect_true(all(recs$division == "BD"))
  sums <- tapply(recs$probability, recs$team, sum)
  expect_true(all(abs(sums - 1) < 1e-9))
})
```

- [ ] **Step 2: Run to verify it fails**

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-backfill-final-positions.R")'`
Expected: FAIL — `could not find function "build_round_final_positions"`.

- [ ] **Step 3: Implement**

Create `R/backfill-final-positions.R`:

```r
#' @include extract-football-iceland.R simulate-league-season.R round-cutoff.R
NULL

#' Build one round's final-position records from a paired (fit, prep).
#'
#' For each league `target_div`, simulates the remaining season from the fit's
#' latest-round strengths (which, for a fit trained through this round, are the
#' as-of-round strengths) and ranks to season-end placements. Stamps each row
#' with the round's `as_of` cutoff date and the division's own games-played
#' round label.
#'
#' @param fit CmdStan fit trained on games <= `cutoff_date`.
#' @param prep `prepare_data()` output that built `fit`'s `stan_data` (pairing
#'   is required for correct team-index -> name mapping).
#' @param results Played results (will be filtered to `<= cutoff_date`).
#' @param season_schedule Full-season schedule (this season).
#' @param round_idx Integer BD round.
#' @param cutoff_date Date of the round-`round_idx` completion.
#' @param season Integer season.
#' @param target_divs League division codes (no CUP).
#' @param generated_at ISO timestamp string stamped on every row.
#' @return tibble(as_of, generated_at, round, season, division, team,
#'   placement, probability). Empty when no league team is covered.
#' @export
build_round_final_positions <- function(fit, prep, results, season_schedule,
                                        round_idx, cutoff_date, season,
                                        target_divs, generated_at) {
  results <- results[
    !is.na(results$match_date) & results$match_date <= cutoff_date, ,
    drop = FALSE
  ]
  sim_inputs <- .extract_sim_inputs_pfi(fit, prep$teams)

  rows <- lapply(target_divs, function(div) {
    top <- results[results$season == season & results$division == div, , drop = FALSE]
    if (nrow(top) == 0L) {
      return(NULL)
    }
    ctt <- top |>
      dplyr::select("home_team", "away_team") |>
      tidyr::pivot_longer(c("home_team", "away_team"), values_to = "team") |>
      dplyr::distinct(.data$team)
    if (!all(ctt$team %in% sim_inputs$team$team)) {
      warning(sprintf(
        "round %d %s: %d team(s) lack strength draws; skipping.",
        round_idx, div, sum(!(ctt$team %in% sim_inputs$team$team))
      ), call. = FALSE)
      return(NULL)
    }
    br <- .league_base_and_remaining_pfi(top, ctt, season_schedule, div)
    sim <- simulate_league_season(
      sim_inputs$team, sim_inputs$scalar,
      br$remaining_fixtures, br$base_standings
    )
    # division's own games-played round = min matches any of its teams has played
    div_round <- top |>
      tidyr::pivot_longer(c("home_team", "away_team"), values_to = "team") |>
      dplyr::count(.data$team) |>
      dplyr::summarise(r = min(.data$n)) |>
      dplyr::pull(.data$r)
    sim$final_positions |>
      dplyr::transmute(
        as_of = format(cutoff_date, "%Y-%m-%d"),
        generated_at = generated_at,
        round = as.integer(div_round),
        season = as.integer(season),
        division = div,
        team = .data$team,
        placement = as.integer(.data$placement),
        probability = .data$probability
      )
  })
  dplyr::bind_rows(rows)
}
```

- [ ] **Step 4: Run to verify it passes**

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-backfill-final-positions.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/backfill-final-positions.R tests/testthat/test-backfill-final-positions.R
git commit -m "feat(backfill): build one round's final-position records from a fit"
```

---

## Task 4: Driver script — fit per round, assemble, write history JSONs

**Files:**
- Create: `scripts/backfill_final_positions_history.R`

This is a runnable orchestrator (not a unit test target). It loops sexes × completed rounds, fits each round (re-fit by default for fit/prep consistency), collects records, and writes one `final_positions_history.json` per (sex, league division), replacing the buggy files.

- [ ] **Step 1: Write the script**

```r
#!/usr/bin/env Rscript
# Backfill final_positions_history.json for all football league divisions.
#
# For each (sex, completed BD round R): fit the model on games <= round-R
# cutoff, run the full-season simulator per league division, and assemble a
# corrected per-round placement history. Writes data/publish/.../<sex>-<div>/
# final_positions_history.json (full replace).
#
# Usage:
#   uv-style: Rscript scripts/backfill_final_positions_history.R [--sex male|female|both] [--dry-run]
# Env knobs (for a fast dry run): SPORTS_FIT_ITER_WARMUP=250 SPORTS_FIT_ITER_SAMPLING=250
suppressMessages(devtools::load_all(quiet = TRUE))
library(dplyr)

args <- commandArgs(trailingOnly = TRUE)
sex_arg <- {
  i <- which(args == "--sex"); if (length(i)) args[i + 1L] else "both"
}
dry_run <- "--dry-run" %in% args
sexes <- if (sex_arg == "both") c("male", "female") else sex_arg

root <- here::here("data")
league <- load_leagues()[["football_iceland"]]
stan_path <- here::here("Stan", league$stan_model)
GENERATED_AT <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S+0000", tz = "UTC")

SEX_SLUG <- c(male = "karla", female = "kvenna")
DIV_SLUG <- c(BD = "bd", LD1 = "ld", LD2 = "2deild", LD3 = "3deild")

for (sex in sexes) {
  league_divs <- setdiff(.football_iceland_division_codes(sex), "CUP")
  results_all <- read_table("results", root = root,
    filter = list(sport = "football", country = "iceland", sex = sex))
  schedule_all <- read_table("schedules", root = root,
    filter = list(sport = "football", country = "iceland", sex = sex))
  season <- max(results_all$season, na.rm = TRUE)
  schedule_season <- schedule_all[
    !is.na(schedule_all$match_date) & schedule_all$season == season, , drop = FALSE]

  rounds <- completed_bd_rounds(results_all, season)
  cli::cli_alert_info("{sex}: {nrow(rounds)} completed BD round(s); divisions {toString(league_divs)}")

  all_recs <- list()
  for (i in seq_len(nrow(rounds))) {
    R <- rounds$round[i]; cutoff <- rounds$cutoff_date[i]
    cli::cli_alert_info("  fitting {sex} round {R} (cutoff {format(cutoff)}) ...")
    prep <- prepare_data(league, sex, end_date = cutoff, root = root,
      schedule_horizon_days = 200L)
    fit <- fit_model(
      stan_data = prep$stan_data, stan_model_path = stan_path,
      method = "sample", chains = 4L,
      iter_warmup = .env_iter("SPORTS_FIT_ITER_WARMUP"),
      iter_sampling = .env_iter("SPORTS_FIT_ITER_SAMPLING"),
      adapt_delta = 0.95, seed = 1000L + R
    )
    recs <- build_round_final_positions(
      fit, prep, results_all, schedule_season, R, cutoff, season,
      league_divs, GENERATED_AT)
    all_recs[[length(all_recs) + 1L]] <- recs
    rm(fit); gc()
  }
  history <- dplyr::bind_rows(all_recs)

  for (div in league_divs) {
    div_hist <- history[history$division == div, , drop = FALSE] |>
      dplyr::select(-"division") |>
      dplyr::arrange(.data$as_of, .data$team, .data$placement)
    if (nrow(div_hist) == 0L) next
    out_dir <- file.path(root, "publish", "football", "iceland",
      paste0(SEX_SLUG[[sex]], "-", DIV_SLUG[[div]]))
    path <- file.path(out_dir, "final_positions_history.json")
    n_rounds <- dplyr::n_distinct(div_hist$as_of)
    cli::cli_alert_success("  {sex}/{div}: {nrow(div_hist)} rows over {n_rounds} round(s) -> {path}")
    if (!dry_run) {
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      write_json_consistent(
        list(schema_version = 1L, records = div_hist), path,
        auto_unbox = TRUE, dataframe = "rows", digits = 5, na = "null")
    }
  }
}
cli::cli_alert_success("Backfill complete{if (dry_run) ' (dry run — no files written)' else ''}.")
```

- [ ] **Step 2: Dry-run on a single sex with fast fits to validate plumbing**

Run: `SPORTS_FIT_ITER_WARMUP=250 SPORTS_FIT_ITER_SAMPLING=250 Rscript scripts/backfill_final_positions_history.R --sex male --dry-run`
Expected: logs one "fitting male round R" line per completed round, then per-division "N rows over K round(s)" lines, and "Backfill complete (dry run — no files written)". No files changed.

- [ ] **Step 3: Commit the script**

```bash
git add scripts/backfill_final_positions_history.R
git commit -m "feat(backfill): driver for per-round final_positions_history"
```

---

## Task 5: Run the real backfill + validate + sync

**Files:**
- Writes: `data/publish/football/iceland/{karla,kvenna}-{bd,ld,2deild,3deild}/final_positions_history.json`

- [ ] **Step 1: Run production-quality backfill (both sexes)**

Run: `Rscript scripts/backfill_final_positions_history.R --sex both`
Expected: ~22 fits; one history file written per (sex, league division). Wall time: tens of minutes.

- [ ] **Step 2: Validate the output**

```r
# Rscript -e '...'
suppressMessages(library(jsonlite)); library(dplyr)
chk <- function(p) {
  d <- fromJSON(p)$records
  cat(basename(dirname(p)), ":", n_distinct(d$as_of), "rounds,",
      nrow(d), "rows\n")
  s <- d |> group_by(as_of, team) |> summarise(s = sum(probability), .groups = "drop")
  stopifnot(all(abs(s$s - 1) < 1e-6))                       # each (round, team) sums to 1
  lead <- d |> filter(placement == 1) |> group_by(as_of) |>
    slice_max(probability, n = 1) |> arrange(as_of)
  print(lead |> select(as_of, round, team, probability))   # leader P(1st) over rounds
}
chk("data/publish/football/iceland/karla-bd/final_positions_history.json")
chk("data/publish/football/iceland/kvenna-bd/final_positions_history.json")
```

Expected: probabilities valid (sum to 1 per round/team); the BD leader's `p_winner` is plausible (early rounds lower/flatter, rising as a leader pulls away) — **not** pinned near 1.0 at every round (that was the 2-round bug). Spot-check the final round matches the current `final_positions.json` leader (~0.94 male Víkingur).

- [ ] **Step 3: Commit the regenerated history**

```bash
git add data/publish/football/iceland/*/final_positions_history.json
git commit -m "data(backfill): corrected full-season final_positions_history for all league divisions"
```

- [ ] **Step 4: Sync to the platform**

The platform's `pull-sports-data.yml` rsyncs `data/publish/` -> `metill-platform/data/ithrottir/` hourly and auto-deploys. Either wait for the cron, force it (`gh workflow run pull-sports-data.yml` in `metill-platform`), or — if running the backfill against the `metill-is/sports` remote — push so the cron picks it up. Verify the platform heatmap's round-slider shows the corrected forecast at each round.

---

## Task 6 (optional): `workflow_dispatch` runner

Only if the backfill should be re-runnable in CI (e.g. after a data correction) rather than locally.

**Files:**
- Create: `.github/workflows/backfill-final-positions.yml`

- [ ] **Step 1: Add the workflow**

```yaml
name: backfill-final-positions
on: { workflow_dispatch: { inputs: { sex: { default: both } } } }
jobs:
  backfill:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: r-lib/actions/setup-r@v2
      - run: Rscript -e 'install.packages("cmdstanr", repos=c("https://stan-dev.r-universe.dev", getOption("repos"))); cmdstanr::install_cmdstan()'
      - run: Rscript scripts/backfill_final_positions_history.R --sex "${{ inputs.sex }}"
      - run: |
          git config user.name "metill-bot"; git config user.email "bot@metill.is"
          git add data/publish/football/iceland/*/final_positions_history.json
          git commit -m "data(backfill): regenerate final_positions_history" || echo "no changes"
          git push
```

- [ ] **Step 2: Commit**

```bash
git add .github/workflows/backfill-final-positions.yml
git commit -m "ci(backfill): workflow_dispatch runner for final_positions_history"
```

---

## Decisions captured (and their rationale)

- **Re-fit per round (no lookahead).** The only correct way to answer "placement forecast as of round R" — using current strengths would leak future games. The `round_cutoff` machinery + a fresh fit per round guarantees as-of-round strengths.
- **One fit per (sex, round).** The model is cross-division, so a single fit serves BD/LD1/LD2/LD3 at each cutoff. ~22 fits total.
- **Frozen strength** (matches Part 1 / the model's own predictions). No random-walk drift; a deliberate consistency choice the user confirmed.
- **BD-round time grid, per-division round labels.** One fit per BD round (cheap, shared); each division's record carries its own games-played round so LD labels stay honest even though the snapshot dates follow BD's cadence.
- **Full file replacement, not append.** The existing history rows are the buggy 2-round projections at irregular daily-fit dates; the backfill writes a clean per-round series.
- **Scope = `final_positions_history.json` only.** `standings_history` is realised (already correct); `points_distribution` has no history file. Going-forward correctness is already handled by Part 1 (daily fits now append full-season snapshots).
- **CUP excluded** (knockout — `tournament_placements` is its analogue, out of scope here).

## Self-review checklist

- Spec coverage: per-round retrospective placement forecasts for all league divisions, both sexes ✔ (Tasks 3–5); no-lookahead fits ✔ (Task 4); corrected history file the platform already reads ✔ (Task 5 Step 4).
- Placeholders: none — every function body and command is concrete.
- Type consistency: `.league_base_and_remaining_pfi` returns `list(base_standings, remaining_fixtures)` (Task 1) consumed verbatim in Task 3; `completed_bd_rounds` returns `tibble(round, cutoff_date)` (Task 2) consumed in Task 4; `build_round_final_positions` returns the 8-column record tibble (Task 3) assembled in Task 4.
