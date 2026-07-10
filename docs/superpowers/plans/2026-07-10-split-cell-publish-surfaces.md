# Split-Cell Publish Surfaces Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the three BD-cell publish surfaces that currently filter `division == "BD"` (standings.json tabulation + rank, next_games.json fixtures, xG/xPts round aggregation) treat a split cell's season as `BD + BD_UPPER_PO + BD_LOWER_PO`, with a group-locked standings rank once the split phase starts.

**Architecture:** A tiny shared helper maps a publish cell to its "family" of divisions (flat cell = itself; split cell = itself + `<div>_UPPER_PO` + `<div>_LOWER_PO`). The extract layer widens its `predicted_matches` and strength-trajectory filters to the family; the publisher widens `bd_results` / `bd_played` / next_games / `round_num` / `top_teams_upcoming` to the family and group-locks the standings rank using split-group membership refactored out of `.league_split_state_pfi()` (so publisher and season-simulator share one membership derivation). No JSON schema changes: next_games rows for split fixtures carry `division: "BD_UPPER_PO"` (schema: free string) and `division_code: "BDU"` (already in `.football_iceland_division_code_labels()` and schema regex); the standings group boundary is derivable client-side from rank + the `meta.split` object shipped by PR #72.

**Tech Stack:** R package (`devtools::load_all()` / `testthat::test_dir()`), arrow Parquet fixtures, existing helpers `.mini_reg_results()` (helper-split-season.R), `.realised_league_table_pfi()`, `.split_fixture_template()` (supports group sizes 4 and 6 ONLY).

## Global Constraints

- Verified split semantics (design doc `docs/superpowers/specs/2026-07-10-split-season-simulator-design.md`): male BD 6/6, female BD 6/4, single RR in each group, **full carry-over** (points just sum), **group-locked** final table (every efri team above every nedri team regardless of points).
- Split config source: `config/leagues.yml::football_iceland.publish_divisions[*].split`, loaded via `.football_iceland_division_split(sex)` (R/extract-football-iceland.R).
- Group membership override semantics must match `.league_split_state_pfi()`: observed playoff-division appearances win over the computed points -> GD -> GF ranking; teams observed in both groups fall back to ranking (warning); observations exceeding configured sizes are ignored (warning).
- Do NOT touch the ledger, decide, or placer layers. Read-only on all money paths.
- Worktree gotcha: run tests via `testthat::test_dir()`/`test_file()` with `load_all("<abs worktree path>")` — plain `devtools::test()` resolves to the MAIN checkout.
- Tests must be CI-runnable: no Stan fits, no backup-fit dependency. Use synthetic Parquet roots via `write_table()` + `withr::local_tempdir()`.
- Any "upcoming match" fixture must key off the test's own `end_date`, never `Sys.Date()` arithmetic against real KSI dates (time-bomb rule); all publisher filters here are `end_date`-driven so fixed 2026 dates are safe.
- British spelling in comments; comments only where behaviour is non-obvious (`# WHY:` style); no non-ASCII in code (use `\uxxxx` if ever needed).
- Worktree branch: `claude/confident-dirac-443846`; PR to `main` at the end; commit after each task.

---

### Task 1: `.split_family_divisions_pfi()` helper

**Files:**
- Modify: `R/extract-football-iceland.R` (insert after `.football_iceland_division_split`, ~line 123)
- Test: `tests/testthat/test-league-split-state.R` (append)

**Interfaces:**
- Produces: `.split_family_divisions_pfi(target_div, split_config = NULL)` -> character vector. `("BD", NULL)` -> `"BD"`; `("BD", list(upper=6, lower=6))` -> `c("BD", "BD_UPPER_PO", "BD_LOWER_PO")`. Used by Tasks 3 and 4.

- [ ] **Step 1: Write the failing test** (append to `tests/testthat/test-league-split-state.R`)

```r
test_that(".split_family_divisions_pfi: flat cell -> own code; split cell -> family", {
  expect_equal(.split_family_divisions_pfi("BD", NULL), "BD")
  expect_equal(
    .split_family_divisions_pfi("BD", list(upper = 6L, lower = 6L)),
    c("BD", "BD_UPPER_PO", "BD_LOWER_PO")
  )
  expect_equal(
    .split_family_divisions_pfi("LD1", list(upper = 4L, lower = 4L)),
    c("LD1", "LD1_UPPER_PO", "LD1_LOWER_PO")
  )
})
```

(Match the file's existing call style — it calls package internals bare under `load_all()`; check the top of the file and use `sports:::` prefix only if the existing tests do.)

- [ ] **Step 2: Run test to verify it fails**

```bash
Rscript -e 'devtools::load_all("/Users/brynjolfurjonsson/sports/.claude/worktrees/crazy-rhodes-a4ad59", quiet = TRUE); testthat::test_file("/Users/brynjolfurjonsson/sports/.claude/worktrees/crazy-rhodes-a4ad59/tests/testthat/test-league-split-state.R")'
```

Expected: FAIL — `could not find function ".split_family_divisions_pfi"`.

- [ ] **Step 3: Implement** (in `R/extract-football-iceland.R`, after `.football_iceland_division_split`)

```r
# Divisions comprising a publish cell's season. A flat cell maps to its own
# code; a cell with a configured split (config/leagues.yml::
# publish_divisions[*].split) also spans its split-phase playoff divisions --
# post-split, the "BD season" is BD + BD_UPPER_PO + BD_LOWER_PO.
.split_family_divisions_pfi <- function(target_div, split_config = NULL) {
  if (is.null(split_config)) {
    return(target_div)
  }
  c(target_div, paste0(target_div, c("_UPPER_PO", "_LOWER_PO")))
}
```

- [ ] **Step 4: Run test to verify it passes** (same command as Step 2). Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git -C /Users/brynjolfurjonsson/sports/.claude/worktrees/crazy-rhodes-a4ad59 add R/extract-football-iceland.R tests/testthat/test-league-split-state.R
git -C /Users/brynjolfurjonsson/sports/.claude/worktrees/crazy-rhodes-a4ad59 commit -m "feat(publish): family-divisions helper for split cells"
```

---

### Task 2: Refactor split-group membership out of `.league_split_state_pfi()`

**Files:**
- Modify: `R/extract-football-iceland.R` — `.league_split_state_pfi()` phase-2 block (~lines 601–661)
- Test: `tests/testthat/test-league-split-state.R` (append; existing tests must stay green)

**Interfaces:**
- Produces: `.split_group_membership_pfi(ranked_teams, observed, upper_n, lower_n, target_div)` -> `tibble(team, group)` with `group %in% c("upper","lower")`, covering every ranked team. `observed` is a tibble with `home_team`, `away_team`, `division` columns already restricted to the current season's playoff-division rows (played and/or scheduled). Used by Task 4's publisher standings block.
- Consumes: nothing new; behaviour of `.league_split_state_pfi()` must be byte-identical for existing callers (its warnings may change the function-name prefix; keep the distinctive message tails identical so existing `expect_warning` regexps still match — verify in `tests/testthat/test-league-split-state.R` before renaming anything).

- [ ] **Step 1: Write failing unit tests** (append to `tests/testthat/test-league-split-state.R`)

```r
test_that(".split_group_membership_pfi: fills open upper slots by rank", {
  out <- .split_group_membership_pfi(
    ranked_teams = c("A", "B", "C", "D"),
    observed = tibble::tibble(
      home_team = character(), away_team = character(), division = character()
    ),
    upper_n = 2L, lower_n = 2L, target_div = "BD"
  )
  expect_equal(out$team, c("A", "B", "C", "D"))
  expect_equal(out$group, c("upper", "upper", "lower", "lower"))
})

test_that(".split_group_membership_pfi: observed appearance overrides rank", {
  obs <- tibble::tibble(
    home_team = "C", away_team = "A", division = "BD_UPPER_PO"
  )
  out <- .split_group_membership_pfi(
    ranked_teams = c("A", "B", "C", "D"),
    observed = obs, upper_n = 2L, lower_n = 2L, target_div = "BD"
  )
  expect_equal(out$group[out$team == "C"], "upper")
  expect_equal(out$group[out$team == "B"], "lower")
})

test_that(".split_group_membership_pfi: both-groups conflict falls back with warning", {
  obs <- tibble::tibble(
    home_team = c("A", "A"), away_team = c("B", "C"),
    division = c("BD_UPPER_PO", "BD_LOWER_PO")
  )
  expect_warning(
    out <- .split_group_membership_pfi(
      ranked_teams = c("A", "B", "C", "D"),
      observed = obs, upper_n = 2L, lower_n = 2L, target_div = "BD"
    ),
    "observed in both split groups"
  )
  expect_equal(out$group, c("upper", "upper", "lower", "lower"))
})
```

- [ ] **Step 2: Run to verify failure** (same test_file command). Expected: FAIL — function not found.

- [ ] **Step 3: Implement.** Add the helper directly above `.league_split_state_pfi()`; its body is the existing membership block moved verbatim (with an added `intersect(..., ranked_teams)` guard so unknown team names can never extend the named vector):

```r
# Split-group membership for a completed regular phase. `ranked_teams` is the
# regular-table ranking (points -> GD -> GF, best first); `observed` carries
# (home_team, away_team, division) rows from split-phase results/schedules.
# Observed appearances override the computed ranking (KSI's deeper tiebreaks
# can diverge from ours); remaining teams fill the open upper slots by rank.
# Shared by `.league_split_state_pfi()` and the publisher's standings block so
# the two can never disagree on membership.
.split_group_membership_pfi <- function(ranked_teams, observed,
                                        upper_n, lower_n, target_div) {
  po_divs <- paste0(target_div, c("_UPPER_PO", "_LOWER_PO"))
  obs_of <- function(div) {
    intersect(
      unique(as.character(unlist(
        observed[observed$division == div, c("home_team", "away_team")]
      ))),
      ranked_teams
    )
  }
  obs_upper <- obs_of(po_divs[1])
  obs_lower <- obs_of(po_divs[2])
  both <- intersect(obs_upper, obs_lower)
  if (length(both) > 0L) {
    warning(sprintf(
      ".split_group_membership_pfi[%s]: team(s) observed in both split groups: %s. Falling back to the computed ranking for them.",
      target_div, paste(both, collapse = ", ")
    ), call. = FALSE)
    obs_upper <- setdiff(obs_upper, both)
    obs_lower <- setdiff(obs_lower, both)
  }
  if (length(obs_upper) > upper_n || length(obs_lower) > lower_n) {
    warning(sprintf(
      ".split_group_membership_pfi[%s]: observed group memberships exceed the configured sizes (%d upper / %d lower observed vs %d/%d); ignoring observations.",
      target_div, length(obs_upper), length(obs_lower), upper_n, lower_n
    ), call. = FALSE)
    obs_upper <- character()
    obs_lower <- character()
  }

  group <- setNames(rep(NA_character_, length(ranked_teams)), ranked_teams)
  group[obs_upper] <- "upper"
  group[obs_lower] <- "lower"
  unobserved <- ranked_teams[is.na(group[ranked_teams])]
  slots_upper <- upper_n - sum(group == "upper", na.rm = TRUE)
  if (slots_upper > 0L) {
    group[unobserved[seq_len(min(slots_upper, length(unobserved)))]] <- "upper"
  }
  group[is.na(group)] <- "lower"
  tibble::tibble(team = ranked_teams, group = unname(group[ranked_teams]))
}
```

Then replace the corresponding block inside `.league_split_state_pfi()` (from `observed <- dplyr::bind_rows(` through the `split_groups <- tibble::tibble(...)` assignment) with:

```r
  split_groups <- .split_group_membership_pfi(
    ranked_teams = ranked_teams,
    observed = dplyr::bind_rows(
      po_played[, c("home_team", "away_team", "division")],
      po_sched[, c("home_team", "away_team", "division")]
    ),
    upper_n = upper_n, lower_n = lower_n,
    target_div = target_div
  )
```

IMPORTANT: check `tests/testthat/test-league-split-state.R` for `expect_warning(..., regexp)` patterns first. If any regexp anchors on `.league_split_state_pfi`, keep the sprintf prefix as `"%s[%s]: ..."` with the CALLER's name passed in, or simply keep the old prefix text `.league_split_state_pfi[%s]` in both warnings. Do whatever keeps existing tests green without editing them.

- [ ] **Step 4: Run the whole file + extract suite**

```bash
Rscript -e 'devtools::load_all("/Users/brynjolfurjonsson/sports/.claude/worktrees/crazy-rhodes-a4ad59", quiet = TRUE); testthat::test_file("/Users/brynjolfurjonsson/sports/.claude/worktrees/crazy-rhodes-a4ad59/tests/testthat/test-league-split-state.R"); testthat::test_file("/Users/brynjolfurjonsson/sports/.claude/worktrees/crazy-rhodes-a4ad59/tests/testthat/test-extract-football-iceland.R")'
```

Expected: all PASS (new + pre-existing).

- [ ] **Step 5: Commit**

```bash
git -C /Users/brynjolfurjonsson/sports/.claude/worktrees/crazy-rhodes-a4ad59 add R/extract-football-iceland.R tests/testthat/test-league-split-state.R
git -C /Users/brynjolfurjonsson/sports/.claude/worktrees/crazy-rhodes-a4ad59 commit -m "refactor(extract): share split-group membership derivation"
```

---

### Task 3: Extract layer — split-phase fixtures in `predicted_matches` + trajectory

**Files:**
- Modify: `R/extract-football-iceland.R` — `.extract_division_parquets_pfi()` (predicted_matches filter ~line 194; trajectory call ~line 243)
- Modify: `R/publish-football-iceland.R` — `.compute_team_strength_trajectory_pfi()` (~line 85: `division == top_div` -> `%in%`)
- Test: `tests/testthat/test-extract-football-iceland.R` (append)

**Interfaces:**
- Consumes: `.split_family_divisions_pfi()` (Task 1); `split_config` argument already threaded into `.extract_division_parquets_pfi()` by PR #72.
- Produces: BD-cell `predicted_matches.parquet` rows now include upcoming `BD_UPPER_PO`/`BD_LOWER_PO` fixtures (stamped `division = "BD"` by the caller's payload-column mutate — this is what `.aggregate_round_predictions_pfi()`'s stored-beliefs `division == target_div` filter expects, so no change needed there). `round_strengths_quantiles` rounds continue past the regular phase.

- [ ] **Step 1: Write failing test** (append to `tests/testthat/test-extract-football-iceland.R`). Use 10 teams and the female 6/4 split so `.split_fixture_template()` (sizes 4/6 only) is satisfied by `.league_split_state_pfi()`'s template-completion path. Pass `sim_inputs = NULL` so the season simulation is skipped (empty final_positions) — this test targets `predicted_matches` only.

```r
test_that(".extract_division_parquets_pfi: split cell keeps split-phase predicted matches", {
  teams10 <- LETTERS[1:10]
  teams_df <- tibble::tibble(team = teams10, team_nr = seq_along(teams10))
  results <- .mini_reg_results(teams10) |>
    dplyr::mutate(sport = "football", country = "iceland", sex = "female")

  pgl <- tidyr::expand_grid(
    .draw = 1:5,
    tibble::tibble(
      game_nr = c(1L, 2L),
      home_team = c("A", "G"), away_team = c("B", "H"),
      match_date = as.Date("2026-09-12"),
      division = c("BD_UPPER_PO", "BD_LOWER_PO")
    )
  ) |>
    dplyr::mutate(home_goals = 1, away_goals = 0)

  empty_draws <- tibble::tibble(
    team = character(), component = character(),
    .draw = integer(), value = numeric()
  )
  testthat::local_mocked_bindings(
    .compute_team_strength_trajectory_pfi = function(...) {
      tibble::tibble(
        round = integer(), team = character(), .draw = integer(),
        component = character(), location = character(), value = numeric()
      )
    }
  )

  parts <- .extract_division_parquets_pfi(
    target_div = "BD", fit = NULL, teams = teams_df, results = results,
    current_season = 2026L,
    posterior_goals_long = pgl,
    team_strengths_draws = empty_draws,
    home_advantage_draws = empty_draws,
    n_pred_fit = 2L, n_pred_data = 2L,
    sim_inputs = NULL, season_schedule = NULL,
    split_config = list(upper = 6L, lower = 4L)
  )

  expect_setequal(
    paste(parts$predicted_matches$home_team, parts$predicted_matches$away_team),
    c("A B", "G H")
  )

  # Flat call (no split config) must keep excluding the playoff fixtures.
  parts_flat <- .extract_division_parquets_pfi(
    target_div = "BD", fit = NULL, teams = teams_df, results = results,
    current_season = 2026L,
    posterior_goals_long = pgl,
    team_strengths_draws = empty_draws,
    home_advantage_draws = empty_draws,
    n_pred_fit = 2L, n_pred_data = 2L,
    sim_inputs = NULL, season_schedule = NULL,
    split_config = NULL
  )
  expect_equal(nrow(parts_flat$predicted_matches), 0L)
})
```

Note: `.mini_reg_results()` (helper-split-season.R) leaves `match_date = 2026-06-01` and has no `home/away` NA rows; the full all-pairs grid means zero remaining regular fixtures, so `.league_split_state_pfi()` enters phase 2 and completes both groups from the KSI templates — sizes 6 and 4, both supported. If `.summarise_quantile_band_pfi()` errors on the empty draws tibbles, give `empty_draws` one dummy row per required column instead (`team = "A", component = "offence", location = "home", .draw = 1L, value = 0`) — the assertion only reads `predicted_matches`.

- [ ] **Step 2: Run to verify failure.** Expected: `parts$predicted_matches` empty -> `expect_setequal` FAILS.

- [ ] **Step 3: Implement.**

In `.extract_division_parquets_pfi()`, immediately after the function's opening (`top_results <- ...` block is fine to leave; insert before the predicted_matches section):

```r
  family_divs <- .split_family_divisions_pfi(target_div, split_config)
```

Change the predicted_matches filter (currently `dplyr::filter(.data$division == target_div)`) to:

```r
      dplyr::filter(.data$division %in% family_divs) |>
```

Change the trajectory call from `top_div = target_div` to:

```r
    top_div = family_divs
```

In `.compute_team_strength_trajectory_pfi()` (R/publish-football-iceland.R), change the `bd_chrono` filter line `.data$division == top_div` to:

```r
      .data$division %in% top_div
```

and update the function's header comment to say `top_div` accepts a vector of division codes (the cell's family).

- [ ] **Step 4: Run the extract suite** (same test_file command targeting `test-extract-football-iceland.R`). Expected: all PASS.

- [ ] **Step 5: Commit**

```bash
git -C /Users/brynjolfurjonsson/sports/.claude/worktrees/crazy-rhodes-a4ad59 add R/extract-football-iceland.R R/publish-football-iceland.R tests/testthat/test-extract-football-iceland.R
git -C /Users/brynjolfurjonsson/sports/.claude/worktrees/crazy-rhodes-a4ad59 commit -m "feat(extract): split-phase fixtures flow into BD-cell predicted matches + trajectory"
```

---

### Task 4: Publisher — family-wide surfaces + group-locked standings

**Files:**
- Modify: `R/publish-football-iceland.R` — inside the per-division loop of `publish_football_iceland()`
- Test: Create `tests/testthat/test-publish-football-split.R`

**Interfaces:**
- Consumes: `.split_family_divisions_pfi()` (Task 1), `.split_group_membership_pfi()` (Task 2), `.realised_league_table_pfi()` (existing), `.football_iceland_division_split()` (existing).
- Produces: BD-cell standings.json tabulated over the family with group-locked `rank`; next_games.json rows for `BD_UPPER_PO`/`BD_LOWER_PO` fixtures (`division_code` `BDU`/`BDL`); meta.json `round` continues past the regular phase; xG/xPts aggregation receives family matches (surface 3 — asserted in Task 5).

- [ ] **Step 1: Write the failing integration test.** Create `tests/testthat/test-publish-football-split.R`. Synthetic data root; 8 teams A–H; real male config (split 6/6) supplies the split object; group-lock must BITE: nedri G ends on more points than efri F, yet ranks 7th.

```r
# Post-split BD-cell publish surfaces: standings tabulate over
# BD + BD_UPPER_PO + BD_LOWER_PO with a group-locked rank, next_games
# carries split-phase fixtures, meta.round continues counting.
#
# Synthetic 8-team season, male config (split 6/6). Regular phase:
# .mini_reg_results() all-pairs -> ranking = team order A..H, 14 games each.
# Split phase: A beats F (upper), G beats H three times (lower) -> G ends on
# 6 + 9 = 15 pts, F on 12 + 0 = 12 pts. Group-locked table must still rank
# every efri team (A..F) above every nedri team (G, H).

.write_split_fixture_root <- function(root, end_date) {
  teams8 <- LETTERS[1:8]
  regular <- .mini_reg_results(teams8) |>
    dplyr::mutate(round = 1L)
  po <- tibble::tibble(
    home_team  = c("A", "G", "H", "G"),
    away_team  = c("F", "H", "G", "H"),
    home_score = c(1L, 2L, 0L, 3L),
    away_score = c(0L, 0L, 2L, 1L),
    division   = c("BD_UPPER_PO", "BD_LOWER_PO", "BD_LOWER_PO", "BD_LOWER_PO"),
    season     = 2026L,
    match_date = as.Date(c("2026-09-07", "2026-09-07", "2026-09-14", "2026-09-21")),
    round      = 23L
  )
  results <- dplyr::bind_rows(regular, po) |>
    dplyr::mutate(sport = "football", country = "iceland", sex = "male")
  write_table(results, "results", root = root)

  schedules <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male",
    season = 2026L, match_date = end_date + 2L,
    home_team = "B", away_team = "C",
    division = "BD_UPPER_PO", round = 24L,
    kickoff_time = NA_character_
  )
  write_table(schedules, "schedules", root = root)
  invisible(NULL)
}

.split_test_extracted <- function(end_date) {
  bd <- .empty_extracted_pfi()
  bd$predicted_matches <- tibble::tibble(
    home_team = "B", away_team = "C",
    match_date = end_date + 2L,
    home_goals = c(0L, 1L, 2L), away_goals = c(0L, 0L, 1L),
    count = c(10L, 20L, 10L)
  )
  extracted <- list(BD = bd, fit_date = end_date)
  extracted
}

test_that("split cell: standings tabulate the family and rank group-locked", {
  root <- withr::local_tempdir()
  out <- withr::local_tempdir()
  end_date <- as.Date("2026-09-22")
  .write_split_fixture_root(root, end_date)

  league <- load_leagues()[["football_iceland"]]
  suppressWarnings(suppressMessages(
    publish_football_iceland(
      extracted = .split_test_extracted(end_date),
      league = league, sex = "male", end_date = end_date,
      root = root, output_root = out,
      extracts_root = withr::local_tempdir(),
      archive_root = withr::local_tempdir(),
      round_predictions_history_root = withr::local_tempdir()
    )
  ))

  standings <- jsonlite::fromJSON(
    file.path(out, "football", "iceland", "karla-bd", "standings.json"),
    simplifyDataFrame = TRUE
  )
  rows <- tibble::as_tibble(standings$rows)

  # Family tabulation: split matches count. G played 14 regular + 3 split.
  expect_equal(rows$played[rows$team == "G"], 17L)
  expect_equal(rows$points[rows$team == "G"], 15L)
  expect_equal(rows$points[rows$team == "F"], 12L)

  # Group lock: A..F occupy ranks 1..6 even though G out-points F.
  expect_setequal(rows$team[rows$rank <= 6L], LETTERS[1:6])
  expect_equal(rows$team[rows$rank == 7L], "G")
  expect_equal(rows$team[rows$rank == 8L], "H")

  # as_of advances to the last split match.
  expect_equal(standings$as_of, "2026-09-21")
})

test_that("split cell: next_games carries split-phase fixtures with BDU code", {
  root <- withr::local_tempdir()
  out <- withr::local_tempdir()
  end_date <- as.Date("2026-09-22")
  .write_split_fixture_root(root, end_date)

  league <- load_leagues()[["football_iceland"]]
  suppressWarnings(suppressMessages(
    publish_football_iceland(
      extracted = .split_test_extracted(end_date),
      league = league, sex = "male", end_date = end_date,
      root = root, output_root = out,
      extracts_root = withr::local_tempdir(),
      archive_root = withr::local_tempdir(),
      round_predictions_history_root = withr::local_tempdir()
    )
  ))

  ng <- jsonlite::fromJSON(
    file.path(out, "football", "iceland", "karla-bd", "next_games.json"),
    simplifyDataFrame = TRUE
  )
  matches <- tibble::as_tibble(ng$matches)
  expect_equal(nrow(matches), 1L)
  expect_equal(matches$home, "B")
  expect_equal(matches$away, "C")
  expect_equal(matches$division, "BD_UPPER_PO")
  expect_equal(matches$division_code, "BDU")

  meta <- jsonlite::read_json(
    file.path(out, "football", "iceland", "karla-bd", "meta.json")
  )
  # round = min per-team appearance count over the family (B..E have 14).
  expect_equal(meta$round, 14L)

  # The published cell must stay schema-valid.
  v <- validate_publish_dir(
    file.path(out, "football", "iceland", "karla-bd"),
    schema_dir = here::here("config", "publish-schemas")
  )
  expect_equal(v$n_failed, 0L)
})
```

(Adjust the `validate_publish_dir` return-field names to the actual implementation in `R/validate-publish.R` — inspect before asserting; assert "no failures" whichever way the function reports it. If `here::here()` resolves outside the worktree, build `schema_dir` from the test file location via `testthat::test_path("..", "..", "config", "publish-schemas")`.)

- [ ] **Step 2: Run to verify failure.**

```bash
Rscript -e 'devtools::load_all("/Users/brynjolfurjonsson/sports/.claude/worktrees/crazy-rhodes-a4ad59", quiet = TRUE); testthat::test_file("/Users/brynjolfurjonsson/sports/.claude/worktrees/crazy-rhodes-a4ad59/tests/testthat/test-publish-football-split.R")'
```

Expected: FAIL — `played[G] == 14` (split matches excluded), next_games empty (division filter drops BD_UPPER_PO).

- [ ] **Step 3: Implement in `publish_football_iceland()`.**

(a) Top of the per-division loop (right after `is_cup <- identical(target_div, "CUP")`):

```r
    div_split <- .football_iceland_division_split(sex)[[target_div]]
    family_divs <- .split_family_divisions_pfi(target_div, div_split)
```

and DELETE the now-duplicate `div_split <- .football_iceland_division_split(sex)[[target_div]]` line further down in the meta.json block (keep the `meta$split` assignment reading the hoisted variable).

(b) Widen the four family filters:

- `top_teams_upcoming`: `pred_d[pred_d$division %in% family_divs, , drop = FALSE]`
- `bd_played`: `results$season == current_season & results$division %in% family_divs`
- `round_num` source: same `%in% family_divs` change
- next_games: `.data$division %in% family_divs`
- `bd_results`: `results$season == current_season & results$division %in% family_divs`

Leave `current_top_teams` on `top_div` (every family team appears in the regular phase; the venue/team registry semantics are unchanged).

(c) Group-locked rank. After `bd_results` is built (before the `if (!is_cup && nrow(bd_results) > 0L)` block), derive membership once:

```r
    split_groups_pub <- NULL
    if (!is.null(div_split) && nrow(bd_results) > 0L) {
      po_divs <- family_divs[-1]
      po_observed <- dplyr::bind_rows(
        bd_results[
          bd_results$division %in% po_divs,
          c("home_team", "away_team", "division")
        ],
        pred_d[
          pred_d$division %in% po_divs,
          c("home_team", "away_team", "division")
        ]
      )
      if (nrow(po_observed) > 0L) {
        reg_table <- .realised_league_table_pfi(
          bd_results[bd_results$division == top_div, , drop = FALSE],
          current_top_teams
        )
        ranked_teams <- reg_table$team[
          order(-reg_table$base_points, -reg_table$base_gd, -reg_table$base_gf)
        ]
        split_groups_pub <- .split_group_membership_pfi(
          ranked_teams = ranked_teams,
          observed = po_observed,
          upper_n = as.integer(div_split$upper),
          lower_n = as.integer(div_split$lower),
          target_div = target_div
        )
      }
    }
```

Then replace the `standings_rows` arrange+rank (currently `arrange(desc(points), desc(goal_diff), desc(goals_for)) |> mutate(rank = row_number(), ...)`) with a group-aware sort:

```r
      standings_rows <- standings_rows |>
        dplyr::left_join(
          if (is.null(split_groups_pub)) {
            tibble::tibble(team = character(), .split_group = character())
          } else {
            dplyr::rename(split_groups_pub, .split_group = "group")
          },
          by = "team"
        ) |>
        dplyr::arrange(
          # Group-locked once the split is known: every efri team above every
          # nedri team regardless of carried points (KSI split-table rule).
          dplyr::coalesce(.data$.split_group, "upper") != "upper",
          dplyr::desc(.data$points), dplyr::desc(.data$goal_diff),
          dplyr::desc(.data$goals_for)
        ) |>
        dplyr::select(-".split_group") |>
        dplyr::mutate(
          rank = dplyr::row_number(),
          short = short_code(.data$team),
          goals_trend = lapply(.data$goals_trend, I),
          goals_against_trend = lapply(.data$goals_against_trend, I)
        )
```

(the summarise upstream stays untouched; when `split_groups_pub` is NULL every row coalesces to "upper" and the sort reduces to the current one).

- [ ] **Step 4: Run the new test file + the existing publish suites**

```bash
Rscript -e 'devtools::load_all("/Users/brynjolfurjonsson/sports/.claude/worktrees/crazy-rhodes-a4ad59", quiet = TRUE); testthat::test_file("/Users/brynjolfurjonsson/sports/.claude/worktrees/crazy-rhodes-a4ad59/tests/testthat/test-publish-football-split.R"); testthat::test_file("/Users/brynjolfurjonsson/sports/.claude/worktrees/crazy-rhodes-a4ad59/tests/testthat/test-publish-football.R"); testthat::test_file("/Users/brynjolfurjonsson/sports/.claude/worktrees/crazy-rhodes-a4ad59/tests/testthat/test-publish-football-round-predictions.R")'
```

Expected: all PASS (backup-fit tests run locally; they exercise the flat path and must stay green).

- [ ] **Step 5: Commit**

```bash
git -C /Users/brynjolfurjonsson/sports/.claude/worktrees/crazy-rhodes-a4ad59 add R/publish-football-iceland.R tests/testthat/test-publish-football-split.R
git -C /Users/brynjolfurjonsson/sports/.claude/worktrees/crazy-rhodes-a4ad59 commit -m "feat(publish): split-aware BD-cell standings, next_games and round surfaces"
```

---

### Task 5: xG/xPts round aggregation across the split (surface 3 end-to-end)

**Files:**
- Test: `tests/testthat/test-publish-football-split.R` (append; no production change expected — the fix is Task 4's `bd_played` widening; this test pins it)

**Interfaces:**
- Consumes: Task 4's fixture helpers; `.aggregate_round_predictions_pfi()`'s stored-beliefs contract (extracts stamp `division = target_div` for the whole cell — Task 3).

- [ ] **Step 1: Write the test.** Fixture extracts partition at `fit_date=2026-09-06` (strictly before the split round's first kickoff) whose `predicted_matches.parquet` carries the four split matches stamped `division = "BD"` (exactly what the post-Task-3 extract layer writes for the BD cell):

```r
test_that("split cell: xG/xPts round aggregation crosses into the split phase", {
  root <- withr::local_tempdir()
  out <- withr::local_tempdir()
  extracts_root <- withr::local_tempdir()
  hist_root <- withr::local_tempdir()
  end_date <- as.Date("2026-09-22")
  .write_split_fixture_root(root, end_date)

  pdir <- file.path(
    extracts_root, "sport=football", "country=iceland", "sex=male",
    "fit_date=2026-09-06"
  )
  dir.create(pdir, recursive = TRUE)
  po_beliefs <- tidyr::expand_grid(
    tibble::tibble(
      home_team  = c("A", "G", "H", "G"),
      away_team  = c("F", "H", "G", "H"),
      match_date = as.Date(c(
        "2026-09-07", "2026-09-07", "2026-09-14", "2026-09-21"
      ))
    ),
    tibble::tibble(
      home_goals = c(1L, 0L), away_goals = c(0L, 1L), count = c(30L, 10L)
    )
  ) |>
    dplyr::mutate(division = "BD")
  arrow::write_parquet(
    po_beliefs, file.path(pdir, "predicted_matches.parquet")
  )

  league <- load_leagues()[["football_iceland"]]
  suppressWarnings(suppressMessages(
    publish_football_iceland(
      extracted = .split_test_extracted(end_date),
      league = league, sex = "male", end_date = end_date,
      root = root, output_root = out,
      extracts_root = extracts_root,
      archive_root = withr::local_tempdir(),
      round_predictions_history_root = hist_root
    )
  ))

  standings <- jsonlite::fromJSON(
    file.path(out, "football", "iceland", "karla-bd", "standings.json"),
    simplifyDataFrame = TRUE
  )
  rows <- tibble::as_tibble(standings$rows)

  # G's three split matches have pre-round predictions; regular rounds have
  # no qualifying earlier fit, so exactly the split matches accrue.
  expect_equal(rows$n_predicted_matches[rows$team == "G"], 3L)
  expect_false(is.na(rows$xpts[rows$team == "G"]))
  expect_equal(rows$n_predicted_matches[rows$team == "B"], 0L)

  # The history file carries the split matchweeks (15th+ chrono match).
  hist <- jsonlite::fromJSON(
    file.path(
      hist_root, "football", "iceland", "karla-bd",
      "round_predictions_history.json"
    ),
    simplifyDataFrame = TRUE
  )
  recs <- tibble::as_tibble(hist$records)
  expect_true(all(c(15L, 16L, 17L) %in% recs$round[recs$team == "G"]))
})
```

(Verify the history file's actual directory layout under `round_predictions_history_root` before asserting — check how `publish_football_iceland` composes that path, e.g. `{root}/football/iceland/karla-bd/round_predictions_history.json`.)

- [ ] **Step 2: Run it.** Expected: PASS immediately if Task 4 landed correctly (the widened `bd_played` feeds `.aggregate_round_predictions_pfi`). If it fails, the aggregation path has a real gap — debug before proceeding (do NOT weaken the assertions).

- [ ] **Step 3: Commit**

```bash
git -C /Users/brynjolfurjonsson/sports/.claude/worktrees/crazy-rhodes-a4ad59 add tests/testthat/test-publish-football-split.R
git -C /Users/brynjolfurjonsson/sports/.claude/worktrees/crazy-rhodes-a4ad59 commit -m "test(publish): pin xG/xPts accrual across the split phase"
```

---

### Task 6: Docs + full-suite verification + PR

**Files:**
- Modify: `.claude/rules/publish-layer.md` — replace the "Still open (flagged as follow-up chips)" bullet in the split-season section with the shipped behaviour (family tabulation, group-locked rank, BDU/BDL next_games codes, round continuation; note that no schema changed and the platform derives the group boundary from rank + `meta.split`).
- Modify: `docs/superpowers/specs/2026-07-10-split-season-simulator-design.md` — annotate the first "Out of scope" bullet with "(shipped: this plan, PR #NN)".

- [ ] **Step 1: Update the two docs** as above (keep diffs minimal).

- [ ] **Step 2: Full test run**

```bash
Rscript -e 'devtools::load_all("/Users/brynjolfurjonsson/sports/.claude/worktrees/crazy-rhodes-a4ad59", quiet = TRUE); testthat::test_dir("/Users/brynjolfurjonsson/sports/.claude/worktrees/crazy-rhodes-a4ad59/tests/testthat")'
```

Expected: 0 failures (skips for chromote/live-only suites are normal).

- [ ] **Step 3: Commit docs, push, open PR**

```bash
git -C /Users/brynjolfurjonsson/sports/.claude/worktrees/crazy-rhodes-a4ad59 add .claude/rules/publish-layer.md docs/superpowers/specs/2026-07-10-split-season-simulator-design.md docs/superpowers/plans/2026-07-10-split-cell-publish-surfaces.md
git -C /Users/brynjolfurjonsson/sports/.claude/worktrees/crazy-rhodes-a4ad59 commit -m "docs: split-cell publish surfaces shipped"
git -C /Users/brynjolfurjonsson/sports/.claude/worktrees/crazy-rhodes-a4ad59 push -u origin claude/confident-dirac-443846
gh pr create --repo metill-is/sports --base main --head claude/confident-dirac-443846 --title "Split-aware BD-cell publish surfaces (standings, next_games, xG/xPts)" --body "..."
gh pr merge --repo metill-is/sports --rebase --auto --delete-branch <PR>
```

---

## Self-review notes

- **Spec coverage:** surface 1 (standings tabulation + group-locked rank) = Task 4; surface 2 (next_games) = Task 3 (extract) + Task 4 (publisher filter); surface 3 (xG/xPts) = Task 4 (`bd_played`) + Task 5 (pin). Bonus same-root-cause fixes, explicitly flagged: `round_num` (meta.json + standings_history round), `top_teams_upcoming` (home_advantage.json scoping), strength trajectory (team_strengths_history rounds continue). All are one-word filter widenings behind the same `family_divs`; a reviewer can revert any individually.
- **No-schema-change check:** next_games `division` is a free string in the schema; `division_code` BDU/BDL already satisfy `^[A-Z][A-Z0-9_]*$` and exist in `.football_iceland_division_code_labels()`. standings.json gains no fields; the group boundary is rank-derivable via `meta.split`. metill-platform coordination is limited to the existing site-label chip.
- **Type consistency:** `.split_group_membership_pfi` returns `tibble(team, group)` — Task 4 renames `group` to `.split_group` at the join to avoid colliding with user columns; `.split_family_divisions_pfi` returns character vector consumed by `%in%` filters (scalar-safe for flat cells).
- **Timing:** must merge before ~2026-09-06 (first male split round). Pre-fix extracts partitions lack split-phase predicted matches, but every split round's pre-round fit will post-date the merge, so no backfill is needed.
