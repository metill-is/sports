# 2DT Full-Season Projection Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Basketball and handball publish a real regular-season projection (realised results plus a simulation of every remaining fixture), including before a season's first match, by generalising football's `simulate_league_season()`.

**Architecture:** `simulate_league_season()` gains three injected behaviours (match generator, points rule, tie-break) with football's current behaviour as the byte-identical default. A new 2DT generator reproduces each Stan model's generated quantities from per-draw parameters. The 2DT extractor and publisher resolve the season from the schedule as well as results, derive remaining fixtures structurally, and give teams without history a below-average prior. `needs_refit()` learns to refit when the horizon holds fixtures the last fit never predicted. A small metill-platform change keeps the heatmap slider on the current season.

**Tech Stack:** R package `sports` (testthat 3e, dplyr/tidyr, arrow, posterior, cmdstanr, withr, jsonvalidate); CmdStan 2.38.0; metill-platform (FastAPI, pytest, vanilla JS modules).

**Spec:** `docs/superpowers/specs/2026-09-16-2dt-full-season-projection-design.md` (commit `32275bdef`). Read it before starting; this plan argues from it.

## Global Constraints

- Deadline: merged code plus a completed refit by **2026-09-26** (women's BD opens 2026-09-29, men's 2026-09-30).
- **D2:** football's simulator output must stay byte-identical. The digests in Task 4 were taken from the pre-refactor simulator on 2026-09-16 (sports `32275bdef`, simulator file identical to `origin/main` `04acad370`).
- **D3:** strengths are frozen at the last fitted round (`offense[N_rounds]`); no forward drift.
- **D4:** football's exact-tie handling is not changed (`tie_break = "first"`).
- **D5:** season detection changes only for basketball and handball.
- British spelling in comments and prose. Match the surrounding code's comment density (these files explain *why* at length).
- Every R test command runs from the repo root with a UTF-8 locale (the HSÍ fixture parse silently drops rows under the C locale):
  `cd /Users/brynjolfurjonsson/sports && NOT_CRAN=true LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 Rscript -e 'devtools::test(filter = "<stem>")'`
  where `<stem>` is a regex on the test file name without `test-`.
- Fixture dates are far-future (2099-2101). No near-date literals in tests (the repo's time-bomb rule); inject `today` instead.
- Branch: `feat/2dt-full-season-projection`, based on `origin/main`. Never base on local `main` (it carries unpushed betting-ledger commit `4afe054f5`). Run `git fetch` and `git branch --show-current` before every commit.
- JSON files are edited with Python `str.replace` anchored on existing text, never hand-rewritten; `ensure_ascii=False` when dumping.
- Every commit message ends with `Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>`.
- Merging to `main` (either repo) needs the user's explicit OK in chat. Do not self-merge.

## Where this plan departs from the spec (read first)

Each is a judgement call made while reading the code; the user may overturn any of them.

1. **§9.2 is already done.** `fit_league()` has passed `prep = prep` to every Iceland extractor since `7a7d94d04` (`R/model-league.R`, extractor dispatch). Task 2 only pins it with a regression test and fixes two stale comments.
2. **§8 trigger narrowed to "no overlap".** Read literally ("any horizon fixture the last fit did not predict"), the rule fires on most in-season days, because a 14-day window gains a fixture almost daily — every league, football included, would refit daily without a game played. Task 3 fires only when the horizon holds fixtures and the newest fit predicted **none** of them. That still covers every case the rule exists for (season rollover, long breaks).
3. **§3 match function signature.** `match_fn(off_h, def_h, off_a, def_a, scalars, n)` cannot carry handball's per-team `sigma_team`, which §4 itself requires. Match functions take `(home, away, scalars)`, where `home`/`away` are named lists of draw-aligned vectors (`off`, `def`, plus any extra team columns). Each declares the columns it reads (`.new_match_fn()`), so the column check still lives with the match model.
4. **§6 multiplicity order is schedule → config → prior results** (spec: schedule → prior results → config). Prior results use football's `.division_rr_multiplicity_pfi()`, whose max-over-pairs reads basketball's embedded úrslitakeppni as 4-5 meetings; a stated format beats that guess. The schedule is trusted only when it covers ≥ 90 % of pairings, one meetings count covers ≥ 75 % of them, and every team plays about the same number of games. Without those checks, the committed synthetic fixture (a single round robin plus three extra fixtures) reads as multiplicity 1 and would cut played rounds.
5. **§6: a stated `regular_season_rounds` disables structural completion** (meetings unknown → remaining fixtures = the schedule). Basketball women's 1D is the only such cell, and its 11th, playoff-only team would otherwise be scheduled into a round robin.
6. **§7 step sizes are not drawn.** With strengths frozen (D3), random-walk step sizes are never used. Only handball's `sigma_team` is drawn from the fitted hierarchy. A newcomer's home advantage is the division's per-draw mean.
7. **The old Stan-window helpers stay** (`.compute_final_positions_2dt`, `.compute_points_distribution_2dt`, `.compute_iter_team_points_2dt`, `.compute_base_points_2dt`) as the oracle for the Task 8 equivalence test and their existing tests. Production stops calling them. `.regular_season_game_nrs_2dt` loses its only caller and is deleted.
8. **Standings stay empty pre-season** (§10 promises no contract change). Task 16 makes sure metill-platform renders that, and fixes a slider that would otherwise mix last season's rounds into this season's heatmap.
9. **The 2DT simulation is seeded from `fit_date`** (football's league simulation is unseeded). Otherwise every regeneration of the committed extracts fixture would churn.
10. **`preseason_hold` is a season number** (`preseason_hold: 2026`), not `true`. A flag would release the division at its next season's first result — while its stated `regular_season_rounds: 18` still describes the old 10-team format — and publish a 2027 table cut at the wrong round. A number pins the division to that season until someone removes it.

## File Structure

| File | Change | Responsibility |
|---|---|---|
| `R/publish-iceland-league.R` | modify | `.write_empty_standings_pfi()` (history guard); schedule-aware season + meetings for 2DT cells |
| `R/publish-format.R` | modify | `.build_publish_meta()` emits `n_rounds_meetings_source` when present; comment update |
| `R/publish-profile.R` | modify | `season_rule` per sport |
| `R/pipeline-freshness.R` | modify | `needs_refit(today, horizon_days)`; `.horizon_unpredicted()`, `.latest_predicted_fixtures()` |
| `R/simulate-league-season.R` | modify | injectable `match_fn`/`points_fn`/`tie_break`; `.new_match_fn()`, `.match_fn_football`, `.points_fn_football`, `.rank_table_slss()`, `.require_cols_slss()` |
| `R/simulate-2dt.R` | **create** | 2DT generators, points, sim-input extraction, season level, newcomer priors |
| `R/season-structure-2dt.R` | **create** | `.current_season_2dt()`, `.division_format_2dt()`, `.remaining_fixtures_2dt()`, `.base_standings_2dt()` |
| `R/extract-iceland-2dt-shared.R` | modify | per-division block drives the simulator; delete `.regular_season_game_nrs_2dt()` |
| `R/publish-iceland-2dt-helpers.R` | modify | stale comment fix; oracle note |
| `R/publish-divisions.R` | modify | `.iceland_division_preseason_hold()` |
| `config/leagues.yml`, `config/leagues.schema.json` | modify | `preseason_hold` |
| `config/publish-schemas/_base/meta.json` (+ generated) | modify | `n_rounds_meetings_source` |
| `Stan/basketball_iceland/2d_student_t_scalarsigma.stan`, `Stan/handball_iceland/2d_student_t.stan` | modify | comments only |
| `tests/testthat/helper-stub-fit.R`, `tools/make-extract-fixtures.R` | modify | stub carries the Student-t parameter surface |
| `tests/testthat/fixtures/extracts/**` | regenerate | new table semantics |
| tests (new) | create | `test-publish-empty-standings.R`, `test-fit-league-prep-handoff.R`, `test-simulate-2dt.R`, `test-simulate-2dt-equivalence.R`, `test-iceland-division-preseason-hold.R`, `test-season-structure-2dt.R`, `test-extract-2dt-season-projection.R`, `test-publish-2dt-preseason.R` |
| tests (modify) | modify | `test-simulate-league-season.R`, `test-pipeline-freshness.R`, `test-extract-2dt-divisions.R`, `test-stub-fit.R`, `test-publish-profile.R` |
| metill-platform `app/static/js/finishing-heatmap.js`, `app/templates/ithrottir_league.html`, `tests/test_ithrottir_preseason.py` | modify/create | season-scoped slider; pre-season render test |

Task order follows the spec's workstreams: WS1 (Tasks 1-2), WS2 (3), WS3 (4-5), WS4 (6-8), WS5 (9-14), WS6 (15), platform (16), WS7 (17).

---

### Task 1: Keep a league's history on an empty-standings publish (WS1, §9.1)

**Files:**
- Modify: `R/publish-iceland-league.R` (the `else` branch after `if (!is_cup && nrow(bd_results) > 0L) {`, today ~lines 1320-1338)
- Test: `tests/testthat/test-publish-empty-standings.R` (create)

**Interfaces:**
- Produces: `.write_empty_standings_pfi(out_dir, generated_at, season, end_date, is_cup)` → `invisible(NULL)`. Writes `standings.json` with `rows = []`; truncates `standings_history.json` only when `is_cup`, or creates an empty one when the file does not exist yet.

- [ ] **Step 1: Write the failing test**

```r
# tests/testthat/test-publish-empty-standings.R
# A cell with no standings rows is either a cup or a league before its first
# match. Only the cup may lose its history: truncating a league's history there
# would erase every earlier season on the first pre-season publish
# (spec 2026-09-16 §9.1, F13).

.seed_history <- function(dir) {
  write_json_consistent(
    list(
      schema_version = 1L,
      records = data.frame(as_of = "2100-04-01", team = "A", season = 2100L)
    ),
    file.path(dir, "standings_history.json"),
    auto_unbox = TRUE, dataframe = "rows", digits = 5, na = "null"
  )
}

test_that("an empty league table keeps the cell's standings history", {
  dir <- withr::local_tempdir()
  .seed_history(dir)
  .write_empty_standings_pfi(
    dir,
    generated_at = "2100-09-16T00:00:00+0000", season = 2101L,
    end_date = as.Date("2100-09-16"), is_cup = FALSE
  )
  kept <- jsonlite::read_json(file.path(dir, "standings_history.json"))
  expect_length(kept$records, 1L)
  st <- jsonlite::read_json(file.path(dir, "standings.json"))
  expect_identical(st$season, 2101L)
  expect_identical(st$as_of, "2100-09-16")
  expect_length(st$rows, 0L)
})

test_that("a cup still truncates its standings history", {
  dir <- withr::local_tempdir()
  .seed_history(dir)
  .write_empty_standings_pfi(
    dir,
    generated_at = "2100-09-16T00:00:00+0000", season = 2100L,
    end_date = as.Date("2100-09-16"), is_cup = TRUE
  )
  cut <- jsonlite::read_json(file.path(dir, "standings_history.json"))
  expect_length(cut$records, 0L)
})

test_that("a league with no history yet still gets an empty history file", {
  dir <- withr::local_tempdir()
  .write_empty_standings_pfi(
    dir,
    generated_at = "2100-09-16T00:00:00+0000", season = 2101L,
    end_date = as.Date("2100-09-16"), is_cup = FALSE
  )
  fresh <- jsonlite::read_json(file.path(dir, "standings_history.json"))
  expect_identical(fresh$schema_version, 1L)
  expect_length(fresh$records, 0L)
})
```

- [ ] **Step 2: Run it to verify it fails**

Run the test command with `<stem>` = `publish-empty-standings`.
Expected: FAIL, `could not find function ".write_empty_standings_pfi"`.

- [ ] **Step 3: Implement**

Add the helper directly below `.append_to_history_pfi()` in `R/publish-iceland-league.R`:

```r
# The standings pair for a cell with no table rows. Two different cells land
# here and only one of them may lose its history:
#   * a cup, which has no league table at all -- any prior records are stale
#     and the append helper would otherwise keep them;
#   * a league before its first match of the season. Truncating there erased
#     every earlier season's history on the first pre-season publish (spec
#     2026-09-16 §9.1), so a league keeps what it has. A league cell that has
#     never published still gets an empty file, because the publish schema
#     set expects one.
.write_empty_standings_pfi <- function(out_dir, generated_at, season,
                                       end_date, is_cup) {
  write_json_consistent(
    list(
      generated_at = generated_at, season = season,
      as_of = format(end_date, "%Y-%m-%d"), rows = list()
    ),
    file.path(out_dir, "standings.json"),
    auto_unbox = TRUE, dataframe = "rows", digits = 5, na = "null"
  )
  history_path <- file.path(out_dir, "standings_history.json")
  if (isTRUE(is_cup) || !file.exists(history_path)) {
    write_json_consistent(
      list(schema_version = 1L, records = list()),
      history_path,
      auto_unbox = TRUE, dataframe = "rows", digits = 5, na = "null"
    )
  }
  invisible(NULL)
}
```

Then replace the whole `else { ... }` body of the standings branch (the two `write_json_consistent()` calls and the "Truncate standings_history.json" comment) with:

```r
    } else {
      .write_empty_standings_pfi(
        out_dir,
        generated_at = generated_at,
        season = current_season,
        end_date = end_date,
        is_cup = is_cup
      )
    }
```

- [ ] **Step 4: Run the new test and football's golden publish test**

Run `<stem>` = `publish-empty-standings`, then `<stem>` = `publish-football-golden`, then `<stem>` = `publish-profile`.
Expected: all PASS. The golden test passing shows no football cell reached the changed branch with a history file to keep.

- [ ] **Step 5: Commit**

```bash
cd /Users/brynjolfurjonsson/sports && git fetch -q && git branch --show-current
git add R/publish-iceland-league.R tests/testthat/test-publish-empty-standings.R
git commit -F - <<'EOF'
fix(publish): keep a league's standings history when its table is empty

The zero-standings branch truncated standings_history.json for any cell
without rows, not just cups. Once 2DT season detection reads the schedule,
the first pre-season publish of every league would have erased its history.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>
EOF
```

---

### Task 2: Pin the fit-to-extractor `prep` hand-off (WS1, §9.2)

**Files:**
- Test: `tests/testthat/test-fit-league-prep-handoff.R` (create)
- Modify: `tests/testthat/test-stub-fit.R` (comment at ~lines 118-121), `R/publish-iceland-2dt-helpers.R` (header comment of `.compute_posterior_goals_2dt`)

**Interfaces:** none new.

- [ ] **Step 1: Write the test**

```r
# tests/testthat/test-fit-league-prep-handoff.R
# Spec 2026-09-16 §9.2. Given no prep, the 2DT extractor rebuilds one at the
# DEFAULT 14-day horizon; if the fit used any other, .compute_posterior_goals_2dt()
# sees an N_pred mismatch, warns, and predicted_matches ships empty. fit_league()
# has handed the extractor its own prep since 7a7d94d04 -- this pins it.
test_that("fit_league hands its own prep to the extractor", {
  body_txt <- paste(deparse(body(fit_league)), collapse = "\n")
  expect_match(body_txt, "prep = prep", fixed = TRUE)
})
```

- [ ] **Step 2: Prove the test can fail**

Temporarily delete the `prep = prep` argument (and the comma before it) from the `extract_fn(...)` call in `R/model-league.R`. Run `<stem>` = `fit-league-prep-handoff`. Expected: FAIL. Restore the line with `git checkout -- R/model-league.R` and re-run. Expected: PASS.

- [ ] **Step 3: Fix the two stale comments**

In `tests/testthat/test-stub-fit.R`, replace:

```r
  # The publishers call prepare_data() internally at the DEFAULT
  # schedule_horizon_days = 14L and take no prep= argument, so a stub sized at
  # any other horizon would make .compute_posterior_goals_2dt warn and return
  # zero rows. Assert the match rather than trusting it.
```

with:

```r
  # The extractors take prep= (fit_league passes its own), but
  # publish_iceland_league() still rebuilds prep at the DEFAULT
  # schedule_horizon_days = 14L, so a stub sized at any other horizon would
  # make .compute_posterior_goals_2dt warn and return zero rows. Assert the
  # match rather than trusting it.
```

In `R/publish-iceland-2dt-helpers.R`, replace:

```r
# scores). Returns NULL with a warning if the fit's N_pred disagrees with
# the prepared pred_d (caller writes empty placeholder JSONs).
```

with:

```r
# scores). Returns an EMPTY tibble with a warning if the fit's N_pred
# disagrees with the prepared pred_d; the caller then writes an empty
# predicted_matches. Only counts are compared, so the fit and the caller must
# share one prep object (fit_league() passes its own to the extractor).
```

- [ ] **Step 4: Run** `<stem>` = `fit-league-prep-handoff|stub-fit`. Expected: PASS.

- [ ] **Step 5: Commit**

```bash
cd /Users/brynjolfurjonsson/sports && git fetch -q && git branch --show-current
git add tests/testthat/test-fit-league-prep-handoff.R tests/testthat/test-stub-fit.R R/publish-iceland-2dt-helpers.R
git commit -F - <<'EOF'
test(fit): pin fit_league's prep hand-off to the extractor

Spec §9.2 asked for this plumbing; it has existed since 7a7d94d04. The
test keeps it, and two comments that still described the old contract
now describe the current one.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>
EOF
```

---

### Task 3: `needs_refit()` refits when the horizon holds only unpredicted fixtures (WS2, §8)

**Files:**
- Modify: `R/pipeline-freshness.R` (`needs_refit()` signature, roxygen and return line; two new helpers below it)
- Test: `tests/testthat/test-pipeline-freshness.R` (append)
- Regenerate: `man/needs_refit.Rd`

**Interfaces:**
- Produces: `needs_refit(static, sex, root = here::here("data"), today = Sys.Date(), horizon_days = 14L)` → logical scalar. Existing callers are unchanged.
- Produces (internal): `.latest_predicted_fixtures(static, sex, root)` → tibble(`home_team`, `away_team`, `match_date`) or `NULL` when unknown; `.horizon_unpredicted(static, sex, root, today, horizon_days)` → logical scalar.

- [ ] **Step 1: Write the failing tests** (append to `tests/testthat/test-pipeline-freshness.R`)

```r
# --- needs_refit(): fixtures the newest fit never predicted ------------------
# Pre-season nothing is played after the last fit, so the played-games rule
# never fires, and basketball was never refit before its season (spec
# 2026-09-16 §8, F15). Dates are far-future and `today` is injected.

.seed_refit_cell <- function(root, fit_date, predicted = NULL,
                             schedule = NULL, store = "extracts") {
  cell <- c("sport=basketball", "country=iceland", "sex=male")
  at <- function(...) do.call(fs::path, as.list(c(root, ...)))

  results_dir <- at("facts", "results", cell, "season=2100")
  fs::dir_create(results_dir)
  arrow::write_parquet(
    tibble::tibble(
      home_team = "A", away_team = "B", match_date = as.Date("2100-04-01"),
      home_score = 80L, away_score = 70L, division = "BD", round = 22L
    ),
    fs::path(results_dir, "part-0.parquet")
  )
  latest_dir <- at("beliefs", "latest", cell)
  fs::dir_create(latest_dir)
  fs::file_create(fs::path(latest_dir, "part-0.parquet"))

  fit_dir <- at("beliefs", store, cell, paste0("fit_date=", fit_date))
  fs::dir_create(fit_dir)
  if (!is.null(predicted)) {
    file <- if (identical(store, "extracts")) {
      "predicted_matches.parquet"
    } else {
      "part-0.parquet"
    }
    arrow::write_parquet(predicted, fs::path(fit_dir, file))
  }
  if (!is.null(schedule)) {
    sched_dir <- at("facts", "schedules", cell, "season=2101")
    fs::dir_create(sched_dir)
    arrow::write_parquet(schedule, fs::path(sched_dir, "part-0.parquet"))
  }
  invisible(fit_dir)
}

.fixture_rows <- function(dates, home = "A", away = "B") {
  tibble::tibble(
    home_team = home, away_team = away, match_date = as.Date(dates),
    division = "BD", round = NA_integer_
  )
}

.bb_static <- list(sport = "basketball", country = "iceland")

test_that("needs_refit() is TRUE when the horizon holds only fixtures the newest fit never predicted", {
  root <- withr::local_tempdir()
  .seed_refit_cell(root, "2100-04-02",
    predicted = .fixture_rows("2100-04-05"),
    schedule = .fixture_rows(c("2100-09-29", "2100-09-30"))
  )
  expect_true(needs_refit(.bb_static, "male", root = root, today = as.Date("2100-09-20")))
})

test_that("needs_refit() stays FALSE while the newest fit predicted part of the horizon", {
  # In season the window gains a fixture most days; refitting for that alone
  # would refit daily without a game having been played.
  root <- withr::local_tempdir()
  .seed_refit_cell(root, "2100-09-21",
    predicted = .fixture_rows("2100-09-29"),
    schedule = .fixture_rows(
      c("2100-09-29", "2100-10-05"), home = c("A", "C"), away = c("B", "D")
    )
  )
  expect_false(needs_refit(.bb_static, "male", root = root, today = as.Date("2100-09-24")))
})

test_that("needs_refit() ignores fixtures beyond the horizon", {
  root <- withr::local_tempdir()
  .seed_refit_cell(root, "2100-04-02",
    predicted = .fixture_rows("2100-04-05"),
    schedule = .fixture_rows("2100-10-20")
  )
  expect_false(needs_refit(.bb_static, "male", root = root, today = as.Date("2100-09-20")))
})

test_that("needs_refit() reads an archive partition when it is the newest fit", {
  long_form <- function(date) {
    tibble::tibble(
      match_date = as.Date(date), home_team = "A", away_team = "B",
      draw_id = 1:3, home_goals = 80, away_goals = 75
    )
  }
  covered <- withr::local_tempdir()
  .seed_refit_cell(covered, "2100-09-21",
    predicted = long_form("2100-09-29"),
    schedule = .fixture_rows("2100-09-29"), store = "archive"
  )
  expect_false(needs_refit(.bb_static, "male", root = covered, today = as.Date("2100-09-24")))

  stale <- withr::local_tempdir()
  .seed_refit_cell(stale, "2100-04-02",
    predicted = long_form("2100-04-05"),
    schedule = .fixture_rows("2100-09-29"), store = "archive"
  )
  expect_true(needs_refit(.bb_static, "male", root = stale, today = as.Date("2100-09-24")))
})

test_that("needs_refit() does not trigger on a prediction file it cannot read", {
  # Unknown is not uncovered: an unreadable file must not start a fit.
  root <- withr::local_tempdir()
  fit_dir <- .seed_refit_cell(root, "2100-04-02",
    schedule = .fixture_rows("2100-09-29")
  )
  fs::file_create(fs::path(fit_dir, "predicted_matches.parquet"))
  expect_false(needs_refit(.bb_static, "male", root = root, today = as.Date("2100-09-20")))
})
```

- [ ] **Step 2: Run to verify they fail**

`<stem>` = `pipeline-freshness`. Expected: the new tests FAIL with `unused argument (today = ...)`.

- [ ] **Step 3: Implement**

In `R/pipeline-freshness.R`, change the signature and final line of `needs_refit()`:

```r
needs_refit <- function(static, sex, root = here::here("data"),
                        today = Sys.Date(), horizon_days = 14L) {
```

```r
  last_fit <- max(fit_dates)

  latest_match > last_fit ||
    .horizon_unpredicted(
      static, sex, root,
      today = as.Date(today), horizon_days = horizon_days
    )
}
```

Add to the roxygen block, after the paragraph ending "when no game has been played since the last fit.":

```r
#' Also returns `TRUE` when fixtures fall inside the next `horizon_days` and
#' the newest fit predicted none of them -- the season-rollover case, where
#' nothing has been played since the last fit (spec 2026-09-16 §8).
```

and the params:

```r
#' @param today Reference date for the fixture horizon. Default `Sys.Date()`.
#' @param horizon_days Horizon in days; keep equal to `fit_league()`'s
#'   `schedule_horizon_days` (14).
```

Add below `needs_refit()`:

```r
# TRUE when the refit horizon holds fixtures and the newest fit predicted none
# of them.
#
# "None of them", not "any one of them": a 14-day window gains a fixture on
# most in-season days, so an any-uncovered rule would refit every league daily
# whether or not a game had been played. Zero overlap still catches every gap
# the rule exists for -- a season rollover and a long break -- because a fit
# made before either predicted nothing inside today's window.
#
# An unreadable or missing prediction file is "unknown", and unknown does not
# start a fit. A cell with no fit at all never gets here: needs_refit() has
# already returned TRUE.
.horizon_unpredicted <- function(static, sex, root, today, horizon_days) {
  sched <- read_table(
    "schedules",
    root = root,
    filter = list(sport = static$sport, country = static$country, sex = sex)
  )
  if (nrow(sched) == 0L) {
    return(FALSE)
  }
  in_window <- !is.na(sched$match_date) &
    sched$match_date > today &
    sched$match_date <= today + as.integer(horizon_days)
  if (!any(in_window)) {
    return(FALSE)
  }
  predicted <- .latest_predicted_fixtures(static, sex, root)
  if (is.null(predicted)) {
    return(FALSE)
  }
  key <- function(d) {
    paste(d$home_team, d$away_team, format(as.Date(d$match_date)), sep = "|")
  }
  !any(key(sched[in_window, , drop = FALSE]) %in% key(predicted))
}

# The fixtures the newest fit predicted, from whichever store holds that fit:
# extracts/ (predicted_matches.parquet) or archive/ (long-form beliefs). On a
# date tie the extracts partition wins. NULL when nothing readable exists.
.latest_predicted_fixtures <- function(static, sex, root) {
  cell <- c(
    paste0("sport=", static$sport),
    paste0("country=", static$country),
    paste0("sex=", sex)
  )
  parts <- dplyr::bind_rows(lapply(c("extracts", "archive"), function(store) {
    dir <- do.call(fs::path, as.list(c(root, "beliefs", store, cell)))
    if (!fs::dir_exists(dir)) {
      return(NULL)
    }
    fits <- fs::dir_ls(
      dir,
      type = "directory", regexp = "fit_date=[0-9]{4}-[0-9]{2}-[0-9]{2}$"
    )
    if (length(fits) == 0L) {
      return(NULL)
    }
    tibble::tibble(
      store = store,
      path = as.character(fits),
      fit_date = as.Date(sub("^fit_date=", "", fs::path_file(fits)))
    )
  }))
  if (nrow(parts) == 0L) {
    return(NULL)
  }
  newest <- parts[order(-as.numeric(parts$fit_date), parts$store != "extracts"), ][1, ]
  files <- if (identical(newest$store, "extracts")) {
    fs::path(newest$path, "predicted_matches.parquet")
  } else {
    fs::dir_ls(newest$path, glob = "*.parquet")
  }
  files <- files[fs::file_exists(files)]
  if (length(files) == 0L) {
    return(NULL)
  }
  tryCatch(
    dplyr::bind_rows(lapply(files, function(f) {
      tibble::as_tibble(arrow::read_parquet(
        f,
        col_select = c("home_team", "away_team", "match_date")
      ))
    })),
    error = function(e) NULL
  )
}
```

- [ ] **Step 4: Document and run**

```bash
cd /Users/brynjolfurjonsson/sports && LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 Rscript -e 'devtools::document()'
```

Run `<stem>` = `pipeline-freshness|health`. Expected: all PASS. The `health` files call `needs_refit()` with seeded partitions, so a regression in the new branch shows up there.

- [ ] **Step 5: Dry-run the daily decision on this branch's data**

```bash
cd /Users/brynjolfurjonsson/sports && LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 Rscript -e '
devtools::load_all(quiet = TRUE)
lg <- load_leagues()
for (key in c("basketball_iceland", "handball_iceland", "football_iceland")) {
  static <- lg[[key]][c("sport", "country", "sexes", "active", "stan_model", "data_source")]
  for (sx in static$sexes) {
    r <- fit_skip_reason(static, sx, force = FALSE, league_named = FALSE)
    cat(key, sx, if (is.null(r)) "FIT" else paste("skip:", r), "\n")
  }
}'
```

Expected on 2026-09-16 data: `basketball_iceland male FIT` and `female FIT` (first fixtures 2026-09-29/30 are inside 14 days of any date from 2026-09-15). Football and handball lines must match what they would print on `origin/main` (run the same script from an `origin/main` worktree if unsure). Record the output with its timestamp in the commit body.

- [ ] **Step 6: Commit**

```bash
cd /Users/brynjolfurjonsson/sports && git fetch -q && git branch --show-current
git add R/pipeline-freshness.R man/needs_refit.Rd tests/testthat/test-pipeline-freshness.R
git commit -F - <<'EOF'
feat(freshness): refit when the horizon holds only unpredicted fixtures

needs_refit() only looked for games played since the last fit, so a league
between seasons was never refit before its first match. It now also fires
when fixtures sit inside the 14-day horizon and the newest fit predicted
none of them. Partial overlap does not count: in season the window gains a
fixture most days, and that alone must not refit every league daily.

Dry run on this branch's data (<timestamp>): <paste the Step 5 output>

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>
EOF
```

---

### Task 4: Golden guard and a lexicographic ranker (WS3, §3, F5)

**Files:**
- Modify: `R/simulate-league-season.R` (ranking call sites, `gd`/`gf` type, `.rank_rows_desc_slss` replaced)
- Test: `tests/testthat/test-simulate-league-season.R` (append)

**Interfaces:**
- Produces: `.rank_table_slss(pts, gd, gf, teams, group = NULL, tie_break = c("first", "jitter"))` → integer matrix (draws x teams, `dimnames = list(NULL, teams)`), 1 = best. Orders by `group` (TRUE first), then points, goal difference, goals for, then team order (`"first"`) or a seeded per-cell uniform (`"jitter"`, seed `20260905L`, caller's RNG state preserved).
- Removes: `.rank_rows_desc_slss()` (no other callers).

- [ ] **Step 1: Add the golden guard and run it on the UNCHANGED simulator**

Append to `tests/testthat/test-simulate-league-season.R`:

```r
# ---- Byte-identity guard for the 2DT generalisation (spec 2026-09-16 D2) ----
# The digests were taken from the simulator BEFORE the refactor (sports
# 32275bdef, 2026-09-16). A change to either is a change to football's
# published tables: find the cause, never re-take the digest.

.golden_digest <- function(out) {
  digest::digest(
    as.character(jsonlite::toJSON(out, digits = NA)),
    algo = "sha256", serialize = FALSE
  )
}

.golden_case <- function() {
  teams <- sprintf("T%02d", 1:12)
  strengths <- stats::setNames(lapply(seq_along(teams), function(i) {
    c(0.04 * (6 - i), 0.03 * (6 - i), 0.2, 0.1)
  }), teams)
  grid <- expand.grid(home_team = teams, away_team = teams, stringsAsFactors = FALSE)
  list(
    si = .league_inputs(strengths, 400L),
    fixtures = tibble::as_tibble(grid[grid$home_team != grid$away_team, ]),
    base = .spread_base(teams, rep(c(10, 10, 9, 9, 8, 8), 2))
  )
}

test_that("football's flat season simulation is unchanged (golden)", {
  g <- .golden_case()
  out <- simulate_league_season(g$si$team, g$si$scalar, g$fixtures, g$base, seed = 20260916L)
  expect_identical(
    .golden_digest(out),
    "96b7c05939f0d90224735582cade0b7d14e9c72adbfeeb938fc15898dba8279b"
  )
})

test_that("football's split season simulation is unchanged (golden)", {
  g <- .golden_case()
  out <- simulate_league_season(
    g$si$team, g$si$scalar, g$fixtures, g$base, seed = 20260916L,
    split_format = list(upper = 6L, lower = 6L)
  )
  expect_identical(
    .golden_digest(out),
    "80562a07aedf8ba5346dab5bd7a3c8d2426d9df0e7237a748dbaf29e4ff90323"
  )
})
```

Run `<stem>` = `simulate-league-season`. Expected: **PASS** (the guard describes today's behaviour; the 19 existing tests pass too). If a digest differs here, before any code change, the environment differs from the one the digest was taken in (jsonlite or R version): re-take both digests on this unchanged code with `.golden_digest()`, and say so in the commit.

- [ ] **Step 2: Write the failing ranker tests** (append)

```r
# ---- .rank_table_slss(): lexicographic, sport-agnostic ranking --------------

# The ranking the simulator used before 2026-09-16, kept as the oracle.
.packed_rank <- function(pts, gd, gf, teams, group = NULL) {
  key <- pts * 1e6 + gd * 1e3 + gf
  if (!is.null(group)) key <- group * 1e12 + key
  out <- t(apply(key, 1L, function(r) rank(-r, ties.method = "first")))
  if (nrow(key) == 1L) out <- matrix(out, nrow = 1L)
  colnames(out) <- teams
  out
}

test_that("with tie_break = 'first' the ranking equals the packed key on football tables", {
  set.seed(7)
  nd <- 300L
  teams <- sprintf("T%02d", 1:12)
  pts <- matrix(sample(20:40, nd * 12, replace = TRUE), nd, 12)
  gd <- matrix(sample(-3:3, nd * 12, replace = TRUE), nd, 12)
  gf <- matrix(sample(20:23, nd * 12, replace = TRUE), nd, 12)
  grp <- matrix(rep(rep(c(TRUE, FALSE), each = 6), each = nd), nd, 12)
  expect_identical(.rank_table_slss(pts, gd, gf, teams), .packed_rank(pts, gd, gf, teams))
  expect_identical(
    .rank_table_slss(pts, gd, gf, teams, group = grp),
    .packed_rank(pts, gd, gf, teams, group = grp)
  )
  one <- function(m) m[1, , drop = FALSE]
  expect_identical(
    .rank_table_slss(one(pts), one(gd), one(gf), teams),
    .packed_rank(one(pts), one(gd), one(gf), teams)
  )
})

test_that("goal difference outranks goals for however large the scores (F5)", {
  pts <- matrix(c(10, 10), 1)
  gd <- matrix(c(5, 4), 1)
  gf <- matrix(c(1000, 2100), 1)
  expect_identical(.rank_table_slss(pts, gd, gf, c("A", "B"))[1, ], c(A = 1L, B = 2L))
  # The packed key gets this wrong: basketball goals-for swamps the difference.
  expect_identical(.packed_rank(pts, gd, gf, c("A", "B"))[1, ], c(A = 2L, B = 1L))
})

test_that("tie_break = 'jitter' splits exact ties evenly and leaves the caller's RNG alone", {
  nd <- 4000L
  z <- matrix(0, nd, 2)
  set.seed(11)
  before <- stats::runif(1)
  set.seed(11)
  pl <- .rank_table_slss(z, z, z, c("A", "B"), tie_break = "jitter")
  expect_identical(stats::runif(1), before)
  expect_lt(abs(mean(pl[, "A"] == 1L) - 0.5), 0.03)
  expect_identical(pl, .rank_table_slss(z, z, z, c("A", "B"), tie_break = "jitter"))
  expect_true(all(.rank_table_slss(z, z, z, c("A", "B"))[, "A"] == 1L))
})
```

Run `<stem>` = `simulate-league-season`. Expected: the three new ranker tests FAIL (`could not find function ".rank_table_slss"`); everything else passes.

- [ ] **Step 3: Implement**

Replace `.rank_rows_desc_slss()` (whole roxygen block and function) in `R/simulate-league-season.R` with:

```r
#' Rank each draw's table (1 = best)
#'
#' Lexicographic: split group (upper first), then points, goal difference and
#' goals for, all descending. This replaces a packed numeric key
#' (`pts * 1e6 + gd * 1e3 + gf`) that was only order-preserving for football's
#' small integer scores: a basketball season's goals-for (~2000) swamps goal
#' difference (spec 2026-09-16 F5). For every football table the two orders are
#' identical.
#'
#' Residual exact ties: `"first"` gives them to the earlier column (the
#' `base_standings` team order), as football always has. `"jitter"` breaks them
#' with a per-(draw, team) uniform under a fixed seed, with the caller's RNG
#' state preserved -- a team must not win every tie in every draw, or one
#' systematic bias is swapped for another (see `.compute_final_positions_2dt`).
#'
#' @param pts,gd,gf Numeric matrices (draws x teams).
#' @param teams Character vector naming the columns.
#' @param group Optional logical matrix; `TRUE` ranks above `FALSE`.
#' @param tie_break `"first"` or `"jitter"`.
#' @return Integer matrix (draws x teams) of placements.
#' @noRd
.rank_table_slss <- function(pts, gd, gf, teams, group = NULL,
                             tie_break = c("first", "jitter")) {
  tie_break <- match.arg(tie_break)
  nd <- nrow(pts)
  nt <- ncol(pts)
  # Column-major positions of every cell.
  draw <- rep(seq_len(nd), times = nt)
  col <- rep(seq_len(nt), each = nd)
  last <- if (identical(tie_break, "jitter")) {
    withr::with_preserve_seed({
      set.seed(20260905L)
      stats::runif(nd * nt)
    })
  } else {
    col
  }
  grp <- if (is.null(group)) numeric(nd * nt) else as.numeric(group)
  o <- order(
    draw, -grp, -as.vector(pts), -as.vector(gd), -as.vector(gf), last
  )
  placement <- matrix(0L, nd, nt, dimnames = list(NULL, teams))
  # `o` walks draw 1's teams best-first, then draw 2's, ...
  placement[cbind(draw[o], col[o])] <- rep(seq_len(nt), times = nd)
  placement
}
```

In `simulate_league_season()`:

1. Change the accumulators (keep `pts` integer, its `table()` feeds `points_distribution`):

```r
  pts <- matrix(0L, nd, n_teams, dimnames = list(NULL, teams))
  # Double, not integer: 2DT scores are continuous and would be truncated.
  gd <- matrix(0, nd, n_teams, dimnames = list(NULL, teams))
  gf <- matrix(0, nd, n_teams, dimnames = list(NULL, teams))
```

2. In the realised-table block, use `as.numeric(bs$base_gd)` and `as.numeric(bs$base_gf)` in the two `sweep()` calls (leave `as.integer(bs$base_points)`).

3. Replace the comment block that begins "Rank each draw's table by points -> goal difference -> goals for (desc)." with:

```r
  # Rank each draw's table by points -> goal difference -> goals for (desc);
  # see `.rank_table_slss()`.
```

4. Replace `split_placement <- .rank_rows_desc_slss(pts * 1e6 + gd * 1e3 + gf, teams)` with:

```r
      split_placement <- .rank_table_slss(pts, gd, gf, teams)
```

5. Replace, at the end of the split branch, `key <- in_upper * 1e12 + pts * 1e6 + gd * 1e3 + gf` / `} else {` / `key <- pts * 1e6 + gd * 1e3 + gf` / `}` / `placement <- .rank_rows_desc_slss(key, teams)` with:

```r
    group <- in_upper
  } else {
    group <- NULL
  }
  placement <- .rank_table_slss(pts, gd, gf, teams, group = group)
```

(keep the "Group-locked final table" comment above `group <- in_upper`).

- [ ] **Step 4: Run** `<stem>` = `simulate-league-season|backfill-final-positions|league-split-state|league-fixture-completion`. Expected: all PASS, **both golden digests unchanged**.

- [ ] **Step 5: Commit**

```bash
cd /Users/brynjolfurjonsson/sports && git fetch -q && git branch --show-current
git add R/simulate-league-season.R tests/testthat/test-simulate-league-season.R
git commit -F - <<'EOF'
refactor(simulator): rank tables lexicographically, not by a packed key

The packed key pts*1e6 + gd*1e3 + gf only preserves order for football's
small integer scores; a basketball season's goals-for swamps goal
difference. The lexicographic order is identical on football tables (the
new golden digests, taken before this change, still match) and adds an
opt-in seeded jitter for residual ties. Goal difference and goals-for now
accumulate as doubles.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>
EOF
```

---

### Task 5: Injectable match model, points rule and tie-break (WS3, §2-§3)

**Files:**
- Modify: `R/simulate-league-season.R` (header comment, new "match models" block, `simulate_league_season()` signature/body/roxygen)
- Regenerate: `man/simulate_league_season.Rd`
- Test: `tests/testthat/test-simulate-league-season.R` (append)

**Interfaces:**
- Consumes: `.rank_table_slss()` (Task 4).
- Produces:
  - `simulate_league_season(sim_inputs_team, sim_inputs_scalar, remaining_fixtures, base_standings, seed = NULL, split_format = NULL, split_groups = NULL, match_fn = .match_fn_football, points_fn = .points_fn_football, tie_break = c("first", "jitter"))`, return value unchanged.
  - `.new_match_fn(fn, scalar_cols, team_cols = character())` → `fn` with attributes `scalar_cols`, `team_cols`. A match function has the signature `function(home, away, scalars)` → `list(home = <numeric nd>, away = <numeric nd>)`. `home`/`away` are named lists holding `off`, `def` and every declared team column; the home side's `off`/`def` already include its home advantage.
  - `.match_fn_football` (declares `mean_log_goals`, `alpha_mu3`, `beta_mu3_strength_diff`); `.points_fn_football(g_h, g_a)` → `list(home, away)` integer.

- [ ] **Step 1: Write the failing tests** (append)

```r
# ---- Injected behaviours (spec 2026-09-16 §2-§3) ---------------------------

.two_team_case <- function(n_draws = 200L) {
  list(
    si = .league_inputs(list(A = c(0, 0, 0, 0), B = c(0, 0, 0, 0)), n_draws),
    base = .base(list("A", 0, 0, 0), list("B", 0, 0, 0)),
    fixtures = tibble::tibble(home_team = c("A", "B"), away_team = c("B", "A"))
  )
}

test_that("a custom points rule sets the table's points", {
  k <- .two_team_case()
  two_nil <- function(g_h, g_a) {
    list(home = ifelse(g_h > g_a, 2L, 0L), away = ifelse(g_a > g_h, 2L, 0L))
  }
  out <- simulate_league_season(
    k$si$team, k$si$scalar, k$fixtures, k$base, seed = 3L, points_fn = two_nil
  )
  expect_true(all(out$points_distribution$points %in% c(0L, 2L, 4L)))
})

test_that("points_fn must return integer points", {
  k <- .two_team_case(5L)
  dbl <- function(g_h, g_a) {
    list(home = as.numeric(g_h > g_a), away = as.numeric(g_a > g_h))
  }
  expect_error(
    simulate_league_season(k$si$team, k$si$scalar, k$fixtures, k$base, points_fn = dbl),
    "integer points"
  )
})

test_that("a match model receives its declared team columns on both sides", {
  seen <- new.env()
  spy <- .new_match_fn(
    function(home, away, scalars) {
      seen$home <- home
      seen$away <- away
      list(home = rep(1, nrow(scalars)), away = rep(0, nrow(scalars)))
    },
    scalar_cols = "level", team_cols = "spread"
  )
  team <- tibble::tibble(
    team = rep(c("A", "B"), each = 3), .draw = rep(1:3, 2),
    cur_offense = rep(c(1, 2), each = 3),
    cur_defense = rep(c(10, 20), each = 3),
    home_advantage_off = rep(c(0.1, 0.2), each = 3),
    home_advantage_def = rep(c(0.01, 0.02), each = 3),
    spread = rep(c(5, 7), each = 3)
  )
  scalar <- tibble::tibble(.draw = 1:3, level = 0)
  base <- .base(list("A", 0, 0, 0), list("B", 0, 0, 0))
  out <- simulate_league_season(
    team, scalar, tibble::tibble(home_team = "A", away_team = "B"), base,
    match_fn = spy
  )
  # Home advantage lands on BOTH the home offence and the home defence.
  expect_equal(seen$home$off, rep(1.1, 3))
  expect_equal(seen$home$def, rep(10.01, 3))
  expect_equal(seen$away$off, rep(2, 3))
  expect_equal(seen$away$def, rep(20, 3))
  expect_equal(seen$home$spread, rep(5, 3))
  expect_equal(seen$away$spread, rep(7, 3))
  fp <- out$final_positions
  expect_equal(fp$probability[fp$team == "A" & fp$placement == 1L], 1)
})

test_that("the columns a match model lacks are named", {
  spy <- .new_match_fn(
    function(home, away, scalars) NULL,
    scalar_cols = "level", team_cols = "spread"
  )
  si <- .league_inputs(list(A = c(0, 0, 0, 0)), 5L)
  base <- .base(list("A", 0, 0, 0))
  expect_error(
    simulate_league_season(si$team, si$scalar, .no_fixtures(), base, match_fn = spy),
    "spread"
  )
  si$team$spread <- 1
  expect_error(
    simulate_league_season(si$team, si$scalar, .no_fixtures(), base, match_fn = spy),
    "level"
  )
  expect_error(
    simulate_league_season(
      si$team, si$scalar, .no_fixtures(), base, match_fn = function(...) NULL
    ),
    ".new_match_fn"
  )
  si$scalar$mean_log_goals <- NULL
  expect_error(
    simulate_league_season(si$team, si$scalar, .no_fixtures(), base),
    "mean_log_goals"
  )
})

test_that("jitter tie-breaking shares an exact tie instead of giving it to team order", {
  si <- .league_inputs(list(A = c(0, 0, 0, 0), B = c(0, 0, 0, 0)), 2000L)
  base <- .base(list("A", 5, 0, 0), list("B", 5, 0, 0))
  first <- simulate_league_season(si$team, si$scalar, .no_fixtures(), base)$final_positions
  jitter <- simulate_league_season(
    si$team, si$scalar, .no_fixtures(), base, tie_break = "jitter"
  )$final_positions
  expect_equal(first$probability[first$team == "A" & first$placement == 1L], 1)
  expect_lt(
    abs(jitter$probability[jitter$team == "A" & jitter$placement == 1L] - 0.5),
    0.05
  )
})
```

Run `<stem>` = `simulate-league-season`. Expected: the five new tests FAIL (`unused argument`).

- [ ] **Step 2: Add the match-model block**

Insert directly after the file header comment of `R/simulate-league-season.R` (after the "Strength is held at the latest-round posterior ..." paragraph), and add one sentence to that header: "The match model, points rule and tie-break are injected (`match_fn`, `points_fn`, `tie_break`); football's are the defaults, and the 2DT sports supply their own (R/simulate-2dt.R)."

```r
# ---- Match models -------------------------------------------------------------
#
# A match model plays one fixture across every posterior draw at once. It is a
# function `(home, away, scalars)` returning `list(home, away)` score vectors.
# `home` and `away` are named lists of draw-aligned vectors: `off`, `def` (the
# home side's already carry its home advantage, on both) and every extra team
# column the model declares. `scalars` is the scalar input sorted by `.draw`.
#
# A model declares the columns it reads, so the simulator can refuse bad inputs
# before any work and name what is missing (spec 2026-09-16 §3).

.new_match_fn <- function(fn, scalar_cols, team_cols = character()) {
  stopifnot(
    is.function(fn), is.character(scalar_cols), is.character(team_cols)
  )
  structure(fn, scalar_cols = scalar_cols, team_cols = team_cols)
}

# Football: frozen-strength bivariate Poisson (`.simulate_match_goals_slss`).
.match_fn_football <- .new_match_fn(
  function(home, away, scalars) {
    .simulate_match_goals_slss(
      off_h = home$off, def_h = home$def,
      off_a = away$off, def_a = away$def,
      mlg = scalars$mean_log_goals,
      amu3 = scalars$alpha_mu3,
      bmu3 = scalars$beta_mu3_strength_diff
    )
  },
  scalar_cols = c("mean_log_goals", "alpha_mu3", "beta_mu3_strength_diff")
)

# Football: 3 / 1 / 0 on exact equality.
.points_fn_football <- function(g_h, g_a) {
  list(
    home = ifelse(g_h > g_a, 3L, ifelse(g_h == g_a, 1L, 0L)),
    away = ifelse(g_a > g_h, 3L, ifelse(g_h == g_a, 1L, 0L))
  )
}

.require_cols_slss <- function(df, cols, what) {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0L) {
    stop(
      "simulate_league_season: ", what,
      " lacks column(s) the match model needs: ",
      paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }
  invisible(df)
}

.check_points_slss <- function(p) {
  if (!is.integer(p$home) || !is.integer(p$away)) {
    stop(
      "simulate_league_season: `points_fn` must return integer points ",
      "(list(home, away)); points_distribution tabulates exact totals.",
      call. = FALSE
    )
  }
  p
}
```

- [ ] **Step 3: Rewrite `simulate_league_season()`**

Roxygen: replace the `@param sim_inputs_team` and `@param sim_inputs_scalar` text with the lines below, and add the three new params after `@param split_groups`:

```r
#' @param sim_inputs_team Tibble with columns `team`, `.draw`, `cur_offense`,
#'   `cur_defense`, `home_advantage_off`, `home_advantage_def` (latest-round
#'   strengths, on the match model's own scale), plus any team columns
#'   `match_fn` declares. Football's comes from `.extract_sim_inputs_pfi()`.
#' @param sim_inputs_scalar Tibble with `.draw` plus the scalar columns
#'   `match_fn` declares (football: `mean_log_goals`, `alpha_mu3`,
#'   `beta_mu3_strength_diff`).
```

```r
#' @param match_fn Match model built with `.new_match_fn()`: `(home, away,
#'   scalars)` -> `list(home, away)` score vectors, one element per draw.
#'   Default: football's bivariate Poisson.
#' @param points_fn `(g_h, g_a)` -> `list(home, away)` of INTEGER points.
#'   Default: 3/1/0 on exact equality.
#' @param tie_break `"first"` (default) gives exact points/GD/GF ties to the
#'   earlier `base_standings` team, as football always has; `"jitter"` breaks
#'   them with a seeded per-(draw, team) uniform, as the 2DT sports need.
```

Body: replace the function from its signature down to (not including) the `# Add the realised (already-played) table.` comment with:

```r
simulate_league_season <- function(sim_inputs_team,
                                   sim_inputs_scalar,
                                   remaining_fixtures,
                                   base_standings,
                                   seed = NULL,
                                   split_format = NULL,
                                   split_groups = NULL,
                                   match_fn = .match_fn_football,
                                   points_fn = .points_fn_football,
                                   tie_break = c("first", "jitter")) {
  tie_break <- match.arg(tie_break)
  stopifnot(
    is.data.frame(sim_inputs_team),
    is.data.frame(sim_inputs_scalar),
    is.data.frame(remaining_fixtures),
    is.data.frame(base_standings),
    is.function(points_fn),
    all(c("team", "base_points", "base_gd", "base_gf") %in% names(base_standings))
  )
  if (!is.function(match_fn) || is.null(attr(match_fn, "scalar_cols"))) {
    stop(
      "simulate_league_season: `match_fn` must be built with .new_match_fn(), ",
      "which records the columns it reads.",
      call. = FALSE
    )
  }
  extra_cols <- attr(match_fn, "team_cols")
  team_cols <- c(
    "cur_offense", "cur_defense", "home_advantage_off", "home_advantage_def",
    extra_cols
  )
  .require_cols_slss(
    sim_inputs_team, c("team", ".draw", team_cols), "sim_inputs_team"
  )
  .require_cols_slss(
    sim_inputs_scalar, c(".draw", attr(match_fn, "scalar_cols")),
    "sim_inputs_scalar"
  )

  if (!is.null(seed)) {
    set.seed(seed)
  }
```

...then keep, **unchanged**, everything from `teams <- as.character(base_standings$team)` through the `to_matrix <- function(col) { ... }` definition, but in the "Align scalar draws" block delete the three lines `mlg <- ...`, `amu3 <- ...`, `bmu3 <- ...` and add `draws <- seq_len(nd)` after `nd <- length(draw_order)`. Then replace the four `OFF`/`DEF`/`HAO`/`HAD` lines, the accumulator block and the flat fixture loop with:

```r
  M <- lapply(stats::setNames(team_cols, team_cols), to_matrix)

  # One side of a fixture, draw-aligned. `idx` is a (draw, team column) index
  # matrix. The home side carries its home advantage on BOTH offence and
  # defence, as every model here does; declared extra columns (handball's
  # sigma_team) travel unchanged.
  side <- function(idx, home) {
    off <- M$cur_offense[idx]
    def <- M$cur_defense[idx]
    if (home) {
      off <- off + M$home_advantage_off[idx]
      def <- def + M$home_advantage_def[idx]
    }
    c(list(off = off, def = def), lapply(M[extra_cols], function(m) m[idx]))
  }

  pts <- matrix(0L, nd, n_teams, dimnames = list(NULL, teams))
  # Double, not integer: 2DT scores are continuous and would be truncated.
  gd <- matrix(0, nd, n_teams, dimnames = list(NULL, teams))
  gf <- matrix(0, nd, n_teams, dimnames = list(NULL, teams))

  fx <- remaining_fixtures[
    remaining_fixtures$home_team %in% teams &
      remaining_fixtures$away_team %in% teams, ,
    drop = FALSE
  ]
  for (i in seq_len(nrow(fx))) {
    idx_h <- cbind(draws, match(as.character(fx$home_team[i]), teams))
    idx_a <- cbind(draws, match(as.character(fx$away_team[i]), teams))

    goals <- match_fn(side(idx_h, home = TRUE), side(idx_a, home = FALSE), scalar)
    p <- .check_points_slss(points_fn(goals$home, goals$away))

    pts[idx_h] <- pts[idx_h] + p$home
    pts[idx_a] <- pts[idx_a] + p$away
    gf[idx_h] <- gf[idx_h] + goals$home
    gf[idx_a] <- gf[idx_a] + goals$away
    gd[idx_h] <- gd[idx_h] + (goals$home - goals$away)
    gd[idx_a] <- gd[idx_a] + (goals$away - goals$home)
  }
```

In the phase-1 split loop, replace the `idx_h`/`idx_a` lines and everything through the last `gd[idx_a] <- ...` with:

```r
          idx_h <- cbind(draws, ord[, g$offset + tpl$home_rank[k]])
          idx_a <- cbind(draws, ord[, g$offset + tpl$away_rank[k]])

          goals <- match_fn(
            side(idx_h, home = TRUE), side(idx_a, home = FALSE), scalar
          )
          p <- .check_points_slss(points_fn(goals$home, goals$away))

          pts[idx_h] <- pts[idx_h] + p$home
          pts[idx_a] <- pts[idx_a] + p$away
          gf[idx_h] <- gf[idx_h] + goals$home
          gf[idx_a] <- gf[idx_a] + goals$away
          gd[idx_h] <- gd[idx_h] + (goals$home - goals$away)
          gd[idx_a] <- gd[idx_a] + (goals$away - goals$home)
```

and pass the tie-break to both rankings:

```r
      split_placement <- .rank_table_slss(pts, gd, gf, teams, tie_break = tie_break)
```

```r
  placement <- .rank_table_slss(
    pts, gd, gf, teams, group = group, tie_break = tie_break
  )
```

- [ ] **Step 4: Document and run**

```bash
cd /Users/brynjolfurjonsson/sports && LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 Rscript -e 'devtools::document()'
```

Run `<stem>` = `simulate-league-season|backfill-final-positions|league-split-state|extract-football|publish-football`. Expected: all PASS; both golden digests unchanged; the 48 original expectations untouched (`git diff origin/main -- tests/testthat/test-simulate-league-season.R` shows only appended lines).

- [ ] **Step 5: Commit**

```bash
cd /Users/brynjolfurjonsson/sports && git fetch -q && git branch --show-current
git add R/simulate-league-season.R man/simulate_league_season.Rd tests/testthat/test-simulate-league-season.R
git commit -F - <<'EOF'
feat(simulator): inject the match model, points rule and tie-break

simulate_league_season() now takes match_fn, points_fn and tie_break,
with football's bivariate Poisson, 3/1/0 and team-order ties as the
defaults. A match model declares the columns it reads, so bad inputs fail
up front with the missing names. Football's digests are unchanged.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>
EOF
```

---

### Task 6: The 2DT match generators and points rule (WS4, §4)

**Files:**
- Create: `R/simulate-2dt.R`
- Regenerate: `DESCRIPTION` Collate (via `devtools::document()`)
- Test: `tests/testthat/test-simulate-2dt.R` (create)

**Interfaces:**
- Consumes: `.new_match_fn()` (Task 5), `.points_2dt(home_score, away_score, name, has_ties, tie_threshold)` (existing, `R/publish-iceland-2dt-helpers.R`).
- Produces:
  - `.draw_bivariate_t_2dt(mu_h, mu_a, s_h, s_a, rho, nu)` → `list(home, away)`.
  - `.match_fn_basketball` (scalars `mean_goals`, `nu`, `sigma`, `alpha_rho`, `beta_rho`, `beta2_rho`, `beta3_rho`); `.match_fn_handball` (scalars `mean_goals`, `nu`, `rho`; team column `sigma_team`).
  - `.match_fn_2dt(sport)` → one of the two; errors on anything else.
  - `.points_fn_2dt(has_ties, tie_threshold)` → points function for `simulate_league_season()`.

- [ ] **Step 1: Write the failing tests**

```r
# tests/testthat/test-simulate-2dt.R
# The 2DT match generators reproduce the generated-quantities blocks of
# Stan/basketball_iceland/2d_student_t_scalarsigma.stan and
# Stan/handball_iceland/2d_student_t.stan (spec 2026-09-16 §4). Task 8's
# equivalence test pins them against a real fit; these pin the pieces.

.const_scalars <- function(n, ...) tibble::tibble(.draw = seq_len(n), ...)
.flat_side <- function(n, off = 0, def = 0, ...) {
  c(list(off = rep(off, n), def = rep(def, n)), lapply(list(...), rep, n))
}

test_that("each side's mean is the level plus its offence minus the other's defence", {
  n <- 5L
  sc <- .const_scalars(n,
    mean_goals = 80, nu = 30, sigma = 1e-9,
    alpha_rho = 0, beta_rho = 0, beta2_rho = 0, beta3_rho = 0
  )
  g <- .match_fn_basketball(.flat_side(n, off = 5, def = 5), .flat_side(n, off = -1, def = 0.5), sc)
  expect_equal(g$home, rep(80 + 5 - 0.5, n), tolerance = 1e-6)
  expect_equal(g$away, rep(80 - 1 - 5, n), tolerance = 1e-6)
})

test_that("handball scales each side by its own sigma_team", {
  n <- 200000L
  sc <- .const_scalars(n, mean_goals = 28, nu = 1e6, rho = 0)
  set.seed(1)
  g <- .match_fn_handball(
    .flat_side(n, sigma_team = 6), .flat_side(n, sigma_team = 3), sc
  )
  expect_lt(abs(stats::sd(g$home) - 6), 0.05)
  expect_lt(abs(stats::sd(g$away) - 3), 0.03)
  expect_lt(abs(mean(g$home) - 28), 0.05)
})

test_that("handball scores are correlated at the model's rho", {
  n <- 200000L
  sc <- .const_scalars(n, mean_goals = 28, nu = 1e6, rho = 0.4)
  set.seed(2)
  side <- .flat_side(n, sigma_team = 5)
  g <- .match_fn_handball(side, side, sc)
  expect_lt(abs(stats::cor(g$home, g$away) - 0.4), 0.01)
})

test_that("basketball's rho follows each fixture's strength gap and total", {
  n <- 200000L
  sc <- .const_scalars(n,
    mean_goals = 80, nu = 1e6, sigma = 10,
    alpha_rho = 0.2, beta_rho = 0.05, beta2_rho = -0.03, beta3_rho = 0.004
  )
  # d = |off_h + def_h - off_a - def_a| = 5, t = |off_h + def_h + off_a + def_a| = 5
  target <- 2 * stats::plogis(0.2 + 0.05 * 5 - 0.03 * 5 + 0.004 * 5 * 5) - 1
  set.seed(3)
  g <- .match_fn_basketball(.flat_side(n, off = 4, def = 1), .flat_side(n), sc)
  expect_lt(abs(stats::cor(g$home, g$away) - target), 0.01)
})

test_that("both scores share one mixing variable (a bivariate t, not two t's)", {
  n <- 200000L
  sc <- .const_scalars(n, mean_goals = 0, nu = 5, rho = 0)
  set.seed(4)
  side <- .flat_side(n, sigma_team = 1)
  g <- .match_fn_handball(side, side, sc)
  # Uncorrelated scores, but a shared scale makes their magnitudes move
  # together (population value ~0.21 at nu = 5).
  expect_lt(abs(stats::cor(g$home, g$away)), 0.03)
  expect_gt(stats::cor(abs(g$home), abs(g$away)), 0.1)
  # Two independent t draws -- the bug this guards against -- show nothing.
  set.seed(4)
  wrong_h <- stats::rt(n, df = 5)
  wrong_a <- stats::rt(n, df = 5)
  expect_lt(abs(stats::cor(abs(wrong_h), abs(wrong_a))), 0.03)
})

test_that("2DT points: handball draws inside the threshold, basketball never draws", {
  hb <- .points_fn_2dt(has_ties = TRUE, tie_threshold = 0.5)(c(30.3, 31, 28), c(30, 29, 30))
  expect_identical(hb$home, c(1L, 2L, 0L))
  expect_identical(hb$away, c(1L, 0L, 2L))
  bb <- .points_fn_2dt(has_ties = FALSE, tie_threshold = 0)(c(80.2, 70), c(80, 71))
  expect_identical(bb$home, c(2L, 0L))
  expect_identical(bb$away, c(0L, 2L))
})

test_that(".match_fn_2dt dispatches by sport and declares its columns", {
  expect_identical(.match_fn_2dt("handball"), .match_fn_handball)
  expect_setequal(
    attr(.match_fn_2dt("basketball"), "scalar_cols"),
    c("mean_goals", "nu", "sigma", "alpha_rho", "beta_rho", "beta2_rho", "beta3_rho")
  )
  expect_identical(attr(.match_fn_2dt("handball"), "team_cols"), "sigma_team")
  expect_error(.match_fn_2dt("football"), "2DT")
})
```

Run `<stem>` = `simulate-2dt$`. Expected: FAIL (`could not find function`). (The `$` keeps Task 8's `simulate-2dt-equivalence` out.)

- [ ] **Step 2: Create `R/simulate-2dt.R`**

```r
#' @include simulate-league-season.R publish-iceland-2dt-helpers.R
NULL

# Season simulation for the 2DT sports (basketball, handball).
#
# The match generators below replay each Stan model's generated quantities
# from per-draw parameters, so simulate_league_season() can play out every
# remaining fixture of a season rather than the ~2 rounds inside Stan's 14-day
# prediction window (spec 2026-09-16 §1-§4). Strength is frozen at the last
# fitted round, exactly as the models themselves predict (D3).
#
# Scale: the 2DT models are additive in RAW points/goals. Home advantage is
# never exponentiated or halved here (B5, F11).

# One fixture's scores across draws: what multi_student_t_rng(nu, mu, Sigma)
# does, with Sigma = [[s_h^2, rho s_h s_a], [rho s_h s_a, s_a^2]] as a SCALE
# matrix (Stan/handball_iceland/2d_student_t.stan GQ, and the basketball model
# with s_h = s_a = sigma).
#
# ONE mixing variable per draw, shared by both scores. Two independent
# chi-square draws would give two univariate t's whose joint tails are wrong:
# a blowout in one score would no longer tend to come with an extreme in the
# other.
.draw_bivariate_t_2dt <- function(mu_h, mu_a, s_h, s_a, rho, nu) {
  n <- length(mu_h)
  z1 <- stats::rnorm(n)
  z2 <- stats::rnorm(n)
  w <- sqrt(nu / stats::rchisq(n, df = nu))
  list(
    home = mu_h + s_h * z1 * w,
    away = mu_a + s_a * (rho * z1 + sqrt(1 - rho^2) * z2) * w
  )
}

# Basketball (2d_student_t_scalarsigma.stan GQ): one scalar sigma, and a
# per-fixture correlation from the strength gap `d` and total `t`, both
# computed on home-advantage-adjusted strengths.
.match_fn_basketball <- .new_match_fn(
  function(home, away, scalars) {
    d <- abs(home$off + home$def - away$off - away$def)
    t <- abs(home$off + home$def + away$off + away$def)
    rho <- 2 * stats::plogis(
      scalars$alpha_rho + scalars$beta_rho * d +
        scalars$beta2_rho * t + scalars$beta3_rho * t * d
    ) - 1
    .draw_bivariate_t_2dt(
      mu_h = scalars$mean_goals + home$off - away$def,
      mu_a = scalars$mean_goals + away$off - home$def,
      s_h = scalars$sigma, s_a = scalars$sigma,
      rho = rho, nu = scalars$nu
    )
  },
  scalar_cols = c(
    "mean_goals", "nu", "sigma",
    "alpha_rho", "beta_rho", "beta2_rho", "beta3_rho"
  )
)

# Handball (2d_student_t.stan GQ): per-team sigma_team, one scalar rho.
.match_fn_handball <- .new_match_fn(
  function(home, away, scalars) {
    .draw_bivariate_t_2dt(
      mu_h = scalars$mean_goals + home$off - away$def,
      mu_a = scalars$mean_goals + away$off - home$def,
      s_h = home$sigma_team, s_a = away$sigma_team,
      rho = scalars$rho, nu = scalars$nu
    )
  },
  scalar_cols = c("mean_goals", "nu", "rho"),
  team_cols = "sigma_team"
)

.match_fn_2dt <- function(sport) {
  switch(sport,
    basketball = .match_fn_basketball,
    handball = .match_fn_handball,
    stop("No 2DT match model for sport '", sport, "'.", call. = FALSE)
  )
}

# The published points scheme (`.points_2dt`: 2 / 1 / 0, a tie only within
# `tie_threshold`) in the shape simulate_league_season() takes.
.points_fn_2dt <- function(has_ties, tie_threshold) {
  force(has_ties)
  force(tie_threshold)
  function(g_h, g_a) {
    list(
      home = .points_2dt(g_h, g_a, "home",
        has_ties = has_ties, tie_threshold = tie_threshold
      ),
      away = .points_2dt(g_h, g_a, "away",
        has_ties = has_ties, tie_threshold = tie_threshold
      )
    )
  }
}
```

- [ ] **Step 3: Update Collate and run**

```bash
cd /Users/brynjolfurjonsson/sports && LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 Rscript -e 'devtools::document()' && git diff --stat DESCRIPTION
```

Expected: `DESCRIPTION` gains `'simulate-2dt.R'` after `'simulate-league-season.R'` in Collate. Run `<stem>` = `simulate-2dt$`. Expected: PASS.

- [ ] **Step 4: Commit**

```bash
cd /Users/brynjolfurjonsson/sports && git fetch -q && git branch --show-current
git add R/simulate-2dt.R DESCRIPTION tests/testthat/test-simulate-2dt.R
git commit -F - <<'EOF'
feat(2dt): match generators that replay the Stan models' predictions

Basketball (scalar sigma, per-fixture rho) and handball (per-team
sigma_team, scalar rho) bivariate Student-t draws with one shared mixing
variable per draw, plus the 2DT points rule in the simulator's shape.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>
EOF
```

---

### Task 7: 2DT simulation inputs, the season level, and the stub surface (WS4, §4, F12)

**Files:**
- Modify: `R/simulate-2dt.R` (append), `tests/testthat/helper-stub-fit.R`, `tools/make-extract-fixtures.R`
- Test: `tests/testthat/test-simulate-2dt.R` (append)

**Interfaces:**
- Produces:
  - `.extract_sim_inputs_2dt(fit, teams, sport, n_seasons)` → `list(team, scalar)`.
    - `team`: `team`, `.draw`, `cur_offense`, `cur_defense`, `home_advantage_off`, `home_advantage_def`, plus `sigma_team` (handball).
    - `scalar`: `.draw`, `mean_goals_fit` (= `mean_goals[n_seasons]`), `delta_mean_goals`, `sigma_mean_goals`, `nu`, plus basketball `sigma`, `alpha_rho`, `beta_rho`, `beta2_rho`, `beta3_rho`, or handball `rho`, `mean_sigma_team`, `scale_sigma_team`.
    - Sorted by `.draw`. Orphan team indices (beyond `nrow(teams)`) are dropped.
  - `.season_level_2dt(scalar, seasons_ahead)` → `scalar` with `mean_goals`. Needs `z_level` (one standard normal per draw) in `scalar`.
  - `stub_2dt_draws(..., n_seasons = 2L, level = 26)` (test helper) also emits `nu`, `mean_goals[1..n_seasons]`, `delta_mean_goals`, `sigma_mean_goals`, `sigma`, `alpha_rho`, `beta_rho`, `beta2_rho`, `beta3_rho`, `rho`, `sigma_team[k]`, `mean_sigma_team`, `scale_sigma_team`. Every existing variable keeps its exact values.

- [ ] **Step 1: Extend the stub** (`tests/testthat/helper-stub-fit.R`)

Add to the roxygen of `stub_2dt_draws()`:

```r
#' @param n_seasons Length of `mean_goals` -- pass `prep$stan_data$N_seasons`.
#' @param level Centre of `mean_goals` (points or goals per team per game).
```

Change the signature to:

```r
stub_2dt_draws <- function(teams, n_pred, n_draws = 50L, seed = 2100L,
                           n_rounds = 10L, constants = list(),
                           n_seasons = 2L, level = 26) {
```

Change `list(` (the returned list that starts with `cur_offense_home = cur_offense_home,`) to `core <- list(`, and after its closing `)` add:

```r
  # The Student-t and scoring-level surface the season simulation reads
  # (R/simulate-2dt.R). Drawn AFTER everything above, so every variable above
  # keeps its exact values -- the committed extracts fixture depends on them.
  # Small scales keep the fixture's simulated tables ordered like its
  # deterministic results.
  sigma_team <- block("sigma_team", k, rep(2, k), 0.1)
  c(core, list(
    nu               = block("nu", 1L, 12, 0.5, indexed = FALSE),
    mean_goals       = block("mean_goals", n_seasons, rep(level, n_seasons), 0.3),
    delta_mean_goals = block("delta_mean_goals", 1L, 0.5, 0.1, indexed = FALSE),
    sigma_mean_goals = abs(block("sigma_mean_goals", 1L, 0.5, 0.05, indexed = FALSE)),
    sigma            = abs(block("sigma", 1L, 2, 0.1, indexed = FALSE)),
    alpha_rho        = block("alpha_rho", 1L, 0, 0.1, indexed = FALSE),
    beta_rho         = block("beta_rho", 1L, 0, 0.001, indexed = FALSE),
    beta2_rho        = block("beta2_rho", 1L, 0, 0.001, indexed = FALSE),
    beta3_rho        = block("beta3_rho", 1L, 0, 0.0001, indexed = FALSE),
    rho              = block("rho", 1L, 0.1, 0.05, indexed = FALSE),
    sigma_team       = abs(sigma_team),
    mean_sigma_team  = block("mean_sigma_team", 1L, log(2), 0.05, indexed = FALSE),
    scale_sigma_team = abs(block("scale_sigma_team", 1L, 0.2, 0.02, indexed = FALSE))
  ))
}
```

In `local_stub_2dt()`, add two arguments to the `stub_2dt_draws(` call:

```r
      n_seasons = prep$stan_data$N_seasons,
      level = if (identical(league$sport, "basketball")) 85 else 26,
```

In `tools/make-extract-fixtures.R` (`.write_2dt_extract_fixtures`), add the same to its `stub_env$stub_2dt_draws(` call:

```r
        n_rounds = prep$stan_data$N_rounds,
        n_seasons = prep$stan_data$N_seasons,
        level = if (identical(sport, "basketball")) 85 else 26
```

Run `<stem>` = `stub-fit|extract-2dt|extract-basketball|extract-handball|fixture-harness`. Expected: PASS (nothing reads the new variables yet).

- [ ] **Step 2: Write the failing tests** (append to `tests/testthat/test-simulate-2dt.R`)

```r
test_that(".extract_sim_inputs_2dt returns the simulator's columns, draw-aligned", {
  teams <- tibble::tibble(team = c("A", "B", "C"))
  fit <- stub_fit(stub_2dt_draws(
    teams$team, n_pred = 1L, n_draws = 20L, n_seasons = 3L, level = 85
  ))
  bb <- .extract_sim_inputs_2dt(fit, teams, "basketball", n_seasons = 3L)
  expect_setequal(names(bb$team), c(
    "team", ".draw", "cur_offense", "cur_defense",
    "home_advantage_off", "home_advantage_def"
  ))
  expect_equal(nrow(bb$team), 60L)
  expect_setequal(names(bb$scalar), c(
    ".draw", "mean_goals_fit", "delta_mean_goals", "sigma_mean_goals", "nu",
    "sigma", "alpha_rho", "beta_rho", "beta2_rho", "beta3_rho"
  ))
  expect_identical(bb$scalar$.draw, sort(bb$scalar$.draw))

  # cur_offense is the strength WITHOUT home advantage, i.e. offense[N_rounds].
  raw <- posterior::as_draws_df(fit$draws("cur_offense_away"))
  b <- bb$team[bb$team$team == "B", ]
  expect_equal(b$cur_offense[order(b$.draw)], raw[["cur_offense_away[2]"]])
  # The level is the LAST fitted season's.
  lv <- posterior::as_draws_df(fit$draws("mean_goals"))
  expect_equal(bb$scalar$mean_goals_fit, lv[["mean_goals[3]"]])

  hb <- .extract_sim_inputs_2dt(fit, teams, "handball", n_seasons = 3L)
  expect_true("sigma_team" %in% names(hb$team))
  expect_setequal(
    setdiff(names(hb$scalar), c(
      ".draw", "mean_goals_fit", "delta_mean_goals", "sigma_mean_goals", "nu"
    )),
    c("rho", "mean_sigma_team", "scale_sigma_team")
  )
})

test_that(".extract_sim_inputs_2dt drops team indices the team list does not cover", {
  fit <- stub_fit(stub_2dt_draws(c("A", "B", "C"), n_pred = 1L, n_draws = 5L))
  out <- .extract_sim_inputs_2dt(
    fit, tibble::tibble(team = c("A", "B")), "basketball", n_seasons = 2L
  )
  expect_setequal(unique(out$team$team), c("A", "B"))
})

test_that("a season the fit has not seen is stepped forward on the model's trend (F12)", {
  sc <- tibble::tibble(
    .draw = 1:3, mean_goals_fit = 80, delta_mean_goals = 2,
    sigma_mean_goals = 1.5, z_level = c(-1, 0, 1)
  )
  expect_equal(.season_level_2dt(sc, 0L)$mean_goals, c(80, 80, 80))
  expect_equal(.season_level_2dt(sc, 1L)$mean_goals, c(80.5, 82, 83.5))
  expect_equal(.season_level_2dt(sc, 2L)$mean_goals, 84 + sqrt(2) * 1.5 * c(-1, 0, 1))
  expect_error(.season_level_2dt(sc, -1L))
})
```

Run `<stem>` = `simulate-2dt$`. Expected: the three new tests FAIL (`could not find function`).

- [ ] **Step 3: Implement** (append to `R/simulate-2dt.R`)

```r
# ---- Inputs --------------------------------------------------------------------

# Per-draw parameters for the season simulation, on the models' raw scale.
#
# Team strengths are the LAST fitted round's (`cur_*_away` = offense /
# defense[N_rounds]); home advantage is the raw additive parameter, applied to
# both the home offence and defence by the simulator. The scoring level is
# `mean_goals[n_seasons]` -- prepare_data() indexes seasons in order
# (R/model-prepare.R, `as.integer(as.factor(season))`), so the last index is
# the latest season with results. `n_seasons` must come from the same prep
# the fit was trained on.
#
# `fit$draws()` is asked for whole variables (`mean_goals`, not
# `mean_goals[3]`): cmdstanr accepts either, the test stub only the former.
.extract_sim_inputs_2dt <- function(fit, teams, sport, n_seasons) {
  stopifnot(sport %in% c("basketball", "handball"))
  team_var <- function(var, col) {
    fit$draws(var) |>
      posterior::as_draws_df() |>
      tibble::as_tibble() |>
      tidyr::pivot_longer(
        c(-".chain", -".draw", -".iteration"),
        names_to = "name", values_to = col
      ) |>
      dplyr::mutate(
        team = teams$team[as.integer(readr::parse_number(.data$name))]
      ) |>
      # A fit paired with a shorter team list would otherwise inject NA teams
      # (see .extract_sim_inputs_pfi, issue #14).
      dplyr::filter(!is.na(.data$team)) |>
      dplyr::select("team", ".draw", dplyr::all_of(col))
  }
  team_vars <- c(
    cur_offense = "cur_offense_away",
    cur_defense = "cur_defense_away",
    home_advantage_off = "home_advantage_off",
    home_advantage_def = "home_advantage_def"
  )
  if (identical(sport, "handball")) {
    team_vars <- c(team_vars, sigma_team = "sigma_team")
  }
  team <- Reduce(
    function(a, b) dplyr::inner_join(a, b, by = c("team", ".draw")),
    Map(team_var, unname(team_vars), names(team_vars))
  )

  level_col <- sprintf("mean_goals[%d]", as.integer(n_seasons))
  sport_vars <- switch(sport,
    basketball = c("sigma", "alpha_rho", "beta_rho", "beta2_rho", "beta3_rho"),
    handball = c("rho", "mean_sigma_team", "scale_sigma_team")
  )
  shared_vars <- c("delta_mean_goals", "sigma_mean_goals", "nu")
  scalar <- fit$draws(c("mean_goals", shared_vars, sport_vars)) |>
    posterior::as_draws_df() |>
    tibble::as_tibble()
  if (!level_col %in% names(scalar)) {
    stop(
      ".extract_sim_inputs_2dt: the fit has no ", level_col,
      " -- pass the N_seasons of the prep the fit was trained on.",
      call. = FALSE
    )
  }
  scalar <- scalar |>
    dplyr::select(".draw", mean_goals_fit = dplyr::all_of(level_col),
                  dplyr::all_of(c(shared_vars, sport_vars))) |>
    dplyr::arrange(.data$.draw)

  list(team = team, scalar = scalar)
}

# The scoring level of the season being projected. A season the fit has no
# results for yet is stepped forward on the model's own trend (F12):
#   mean_goals[s + 1] = mean_goals[s] + delta_mean_goals + sigma_mean_goals * z
# `k` steps add k deltas and a sqrt(k)-scaled shock. `z_level` is drawn ONCE per
# draw by the caller, so every division of a cell projects the same
# league-wide level.
.season_level_2dt <- function(scalar, seasons_ahead) {
  k <- as.integer(seasons_ahead)
  stopifnot(length(k) == 1L, !is.na(k), k >= 0L, "z_level" %in% names(scalar))
  scalar$mean_goals <- scalar$mean_goals_fit +
    k * scalar$delta_mean_goals +
    sqrt(k) * scalar$sigma_mean_goals * scalar$z_level
  scalar
}
```

- [ ] **Step 4: Run** `<stem>` = `simulate-2dt$`. Expected: PASS.

- [ ] **Step 5: Commit**

```bash
cd /Users/brynjolfurjonsson/sports && git fetch -q && git branch --show-current
git add R/simulate-2dt.R tests/testthat/test-simulate-2dt.R tests/testthat/helper-stub-fit.R tools/make-extract-fixtures.R
git commit -F - <<'EOF'
feat(2dt): pull the season simulation's parameters from a 2DT fit

Latest-round strengths, raw home advantage, the likelihood scalars each
model needs, and the last fitted season's scoring level, stepped forward on
the model's own trend for a season it has no results for. The test stub
gains the same surface, drawn after its existing variables so their values
do not move.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>
EOF
```

---

### Task 8: The generators reproduce the Stan models (WS4, §11 "2DT equivalence")

**Files:**
- Test: `tests/testthat/test-simulate-2dt-equivalence.R` (create)

**Interfaces:**
- Consumes: `.extract_sim_inputs_2dt`, `.season_level_2dt`, `.match_fn_2dt`, `.points_fn_2dt` (Tasks 6-7); `simulate_league_season` (Task 5); the oracle `.compute_posterior_goals_2dt`, `.compute_base_points_2dt`, `.compute_final_positions_2dt` (existing).

This test compiles both Stan models and samples a small fit on the committed facts fixture. It needs CmdStan (CI installs it since sports#88) and adds a few minutes to the suite.

- [ ] **Step 1: Write the test**

```r
# tests/testthat/test-simulate-2dt-equivalence.R
# Pins the R match generators to the Stan models they copy (spec 2026-09-16
# §11). A real fit on the synthetic facts fixture; the generator then replays
# Stan's own prediction fixtures from the SAME posterior draws. Per fixture the
# score moments must agree, and the league table built from the replay must
# match the one the pre-2026-09-16 path built from Stan's goals*_pred, within
# Monte Carlo error. Sampler health is not under test, so the model is run
# directly rather than through fit_model()'s diagnostics gate.

.equivalence_fit <- function(sport, env = parent.frame()) {
  skip_on_cran()
  skip_if_not_installed("cmdstanr")
  skip_if(
    is.null(tryCatch(cmdstanr::cmdstan_path(), error = function(e) NULL)),
    "cmdstan not installed"
  )
  root <- fixture_facts_root(env = env)
  league <- load_leagues()[[paste0(sport, "_iceland")]]
  prep <- prepare_data(league, "male", end_date = FIXTURE_END_DATE, root = root)
  model <- cmdstanr::cmdstan_model(here::here("Stan", league$stan_model))
  fit <- model$sample(
    data = prep$stan_data, chains = 2L, parallel_chains = 2L,
    iter_warmup = 500L, iter_sampling = 1000L, seed = 20260916L,
    refresh = 0L, show_messages = FALSE, show_exceptions = FALSE
  )
  list(fit = fit, prep = prep, root = root)
}

# One fixture replayed from the simulation inputs, sides built the way
# simulate_league_season() builds them.
.replay_fixture <- function(si, match_fn, home, away) {
  pick <- function(team, col) {
    d <- si$team[si$team$team == team, , drop = FALSE]
    d[[col]][match(si$scalar$.draw, d$.draw)]
  }
  side <- function(team, is_home) {
    out <- list(off = pick(team, "cur_offense"), def = pick(team, "cur_defense"))
    if (is_home) {
      out$off <- out$off + pick(team, "home_advantage_off")
      out$def <- out$def + pick(team, "home_advantage_def")
    }
    for (col in attr(match_fn, "team_cols")) out[[col]] <- pick(team, col)
    out
  }
  match_fn(side(home, TRUE), side(away, FALSE), si$scalar)
}

.expect_generator_matches_stan <- function(sport) {
  f <- .equivalence_fit(sport)
  si <- .extract_sim_inputs_2dt(
    f$fit, f$prep$teams, sport, n_seasons = f$prep$stan_data$N_seasons
  )
  si$scalar$z_level <- 0
  si$scalar <- .season_level_2dt(si$scalar, 0L)
  match_fn <- .match_fn_2dt(sport)

  pg <- .compute_posterior_goals_2dt(f$fit, f$prep$pred_d)
  expect_gt(nrow(pg), 0L)

  withr::local_seed(20260917L)
  for (g in unique(pg$game_nr)) {
    stan <- pg[pg$game_nr == g, ]
    stan <- stan[order(stan$.draw), ]
    r <- .replay_fixture(si, match_fn, stan$home_team[1], stan$away_team[1])
    what <- paste(sport, stan$home_team[1], "v", stan$away_team[1])
    se <- function(a, b) sqrt(stats::var(a) / length(a) + stats::var(b) / length(b))

    expect_lt(abs(mean(r$home) - mean(stan$home_score)),
              5 * se(r$home, stan$home_score), label = paste(what, "home mean"))
    expect_lt(abs(mean(r$away) - mean(stan$away_score)),
              5 * se(r$away, stan$away_score), label = paste(what, "away mean"))
    expect_lt(abs(stats::sd(r$home) / stats::sd(stan$home_score) - 1), 0.1,
              label = paste(what, "home sd"))
    expect_lt(abs(stats::sd(r$away) / stats::sd(stan$away_score) - 1), 0.1,
              label = paste(what, "away sd"))
    expect_lt(abs(stats::cor(r$home, r$away) -
                  stats::cor(stan$home_score, stan$away_score)), 0.12,
              label = paste(what, "correlation"))
    expect_lt(abs(mean(r$home > r$away) -
                  mean(stan$home_score > stan$away_score)), 0.06,
              label = paste(what, "P(home win)"))
  }

  # The table: Stan's window through the old path vs the same fixtures
  # through the simulator.
  div <- .iceland_division_codes(paste0(sport, "_iceland"), "male")[[1]]
  results <- read_table("results", root = f$root)
  played <- results[
    results$sport == sport & results$sex == "male" &
      results$season == 2100L & results$division == div, ,
    drop = FALSE
  ]
  tp <- .tie_params_pfi(sport)
  base_old <- .compute_base_points_2dt(
    played, has_ties = tp$has_ties, tie_threshold = tp$tie_threshold
  )
  old <- .compute_final_positions_2dt(
    pg, div, base_old, tp$has_ties, tp$tie_threshold, current_top_teams = NULL
  )
  window <- unique(pg[pg$division == div, c("game_nr", "home_team", "away_team")])
  window <- window[order(window$game_nr), c("home_team", "away_team")]
  base_new <- tibble::tibble(
    team = base_old$team,
    base_points = base_old$base_points,
    base_gd = as.integer(base_old$base_diff),
    base_gf = 0L
  )
  new <- withr::with_seed(20260918L, simulate_league_season(
    si$team, si$scalar, window, base_new,
    match_fn = match_fn,
    points_fn = .points_fn_2dt(tp$has_ties, tp$tie_threshold),
    tie_break = "jitter"
  ))$final_positions
  both <- dplyr::inner_join(
    old, new, by = c("team", "placement"), suffix = c("_stan", "_r")
  )
  expect_equal(nrow(both), nrow(old))
  expect_lt(max(abs(both$probability_stan - both$probability_r)), 0.06)
}

test_that("the basketball generator reproduces the Stan model's predictions", {
  .expect_generator_matches_stan("basketball")
})

test_that("the handball generator reproduces the Stan model's predictions", {
  .expect_generator_matches_stan("handball")
})
```

- [ ] **Step 2: Run it**

`<stem>` = `simulate-2dt-equivalence`. Expected: PASS for both sports, with `[ FAIL 0 | SKIP 0 ...]` — a SKIP means CmdStan was not found, which is not a pass: check `cmdstanr::cmdstan_path()` first.

- [ ] **Step 3: Prove it can fail**

Temporarily change `.draw_bivariate_t_2dt()` to draw two independent mixing variables (`w_a <- sqrt(nu / stats::rchisq(n, df = nu))`, used for `away`), and, separately, change `.match_fn_basketball`'s `mu_a` to subtract `away$def` instead of `home$def` (the away score then ignores the home defence and its home advantage). Re-run after each change. Expected: at least one expectation FAILS for each change. Revert both with `git checkout -- R/simulate-2dt.R` and re-run to PASS. Note the observed failures in the commit body.

- [ ] **Step 4: Commit**

```bash
cd /Users/brynjolfurjonsson/sports && git fetch -q && git branch --show-current
git add tests/testthat/test-simulate-2dt-equivalence.R
git commit -F - <<'EOF'
test(2dt): the R generators reproduce the Stan models' predictions

A real fit on the facts fixture, replayed through the generators from the
same draws: per-fixture moments and the resulting table match Stan's own
goals*_pred within Monte Carlo error. Red-checked against a second mixing
variable and a dropped home advantage: <observed failures>.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>
EOF
```

---

### Task 9: `preseason_hold` in the league config (WS5, §5.1, D7)

**Files:**
- Modify: `config/leagues.schema.json` (`definitions.publishDivisionList.items.properties`), `config/leagues.yml` (basketball female 1D), `R/publish-divisions.R`
- Test: `tests/testthat/test-iceland-division-preseason-hold.R` (create)

**Interfaces:**
- Produces: `.iceland_division_preseason_hold(key, sex)` → integer vector named by division code, `NA_integer_` where unset. Basketball female: `c(BD = NA, "1D" = 2026L)`.

- [ ] **Step 1: Write the failing tests**

```r
# tests/testthat/test-iceland-division-preseason-hold.R
# Spec 2026-09-16 §5.1 / D7: basketball women's 1. deild is held on its 2026
# season until its 2027 reserve-side aliases and format are confirmed.

test_that("only basketball women's 1. deild is held, on season 2026", {
  expect_identical(
    .iceland_division_preseason_hold("basketball_iceland", "female"),
    c(BD = NA_integer_, `1D` = 2026L)
  )
  expect_true(all(is.na(.iceland_division_preseason_hold("basketball_iceland", "male"))))
  for (sex in c("male", "female")) {
    expect_true(all(is.na(.iceland_division_preseason_hold("handball_iceland", sex))))
    expect_true(all(is.na(.iceland_division_preseason_hold("football_iceland", sex))))
  }
})

test_that("the leagues schema types preseason_hold as a season number", {
  leagues <- load_leagues(validate = FALSE)
  leagues$basketball_iceland$publish_divisions$female[[2]]$preseason_hold <- "yes"
  expect_error(
    validate_leagues(leagues, here::here("config", "leagues.schema.json")),
    "preseason_hold"
  )
})
```

Run `<stem>` = `iceland-division-preseason-hold`. Expected: FAIL (`could not find function`).

- [ ] **Step 2: Add the schema property**

```bash
cd /Users/brynjolfurjonsson/sports && python3 - <<'PY'
import json
p = "config/leagues.schema.json"
s = open(p, encoding="utf-8").read()
old = '''              "lower": { "type": "integer", "minimum": 2 }
            }
          }
'''
new = '''              "lower": { "type": "integer", "minimum": 2 }
            }
          },
          "preseason_hold": {
            "type": "integer",
            "minimum": 2000,
            "description": "Hold this division on the given season: the 2DT season resolver ignores its future schedule and never moves past this season, so metill-platform's min_season gate keeps the cell out of view. For a division whose next season is not modelled yet (basketball female 1. deild, 2027: renamed reserve sides and a new 12-team format). Remove the key to release it. Spec 2026-09-16 section 5.1."
          }
'''
assert s.count(old) == 1, s.count(old)
s = s.replace(old, new)
json.loads(s)
open(p, "w", encoding="utf-8").write(s)
PY
```

- [ ] **Step 3: Set it on basketball female 1D**

```bash
cd /Users/brynjolfurjonsson/sports && python3 - <<'PY'
p = "config/leagues.yml"
s = open(p, encoding="utf-8").read()
old = "          code_badge: B1D, regular_season_rounds: 18 }\n"
new = "          code_badge: B1D, regular_season_rounds: 18, preseason_hold: 2026 }\n"
assert s.count(old) == 1, s.count(old)
anchor = "      # and the fix is to re-measure the constant, not to loosen the test.\n"
note = anchor + (
    "      # preseason_hold: 2026 -- the 2027 season (12 teams, reserve sides renamed\n"
    "      # between seasons, 22 rounds) is not modelled yet. Held, the division keeps\n"
    "      # publishing 2026 and metill-platform keeps it out of view (spec\n"
    "      # 2026-09-16 section 5.1). Release it once the aliases are confirmed with\n"
    "      # KKI and regular_season_rounds is re-measured for the new format.\n"
)
assert s.count(anchor) == 1, s.count(anchor)
s = s.replace(old, new).replace(anchor, note)
open(p, "w", encoding="utf-8").write(s)
PY
```

If an assertion fires, open `config/leagues.yml`, find the `female:` entry `- { code: 1D, slug: 1d, label_is: "1. deild", ...`, add `preseason_hold: 2026` inside its braces, and put the five comment lines directly above that entry.

- [ ] **Step 4: Add the accessor** (`R/publish-divisions.R`, after `.iceland_division_regular_season_rounds()`)

```r
# code -> the season this division is held on, NA_integer_ where unset. A held
# division ignores its future schedule and never resolves past this season
# (`.current_season_2dt()`), so the platform's min_season gate keeps it out of
# view until the key is removed (spec 2026-09-16 §5.1).
.iceland_division_preseason_hold <- function(key, sex) {
  cfg <- .iceland_division_entries(key, sex, ".iceland_division_preseason_hold")
  .name_by_code(
    vapply(cfg, function(d) .as_opt_int(d$preseason_hold), integer(1)),
    cfg
  )
}
```

- [ ] **Step 5: Run** `<stem>` = `iceland-division|config|leagues`. Expected: PASS.

- [ ] **Step 6: Commit**

```bash
cd /Users/brynjolfurjonsson/sports && git fetch -q && git branch --show-current
git add config/leagues.schema.json config/leagues.yml R/publish-divisions.R tests/testthat/test-iceland-division-preseason-hold.R
git commit -F - <<'EOF'
feat(config): hold basketball women's 1. deild on season 2026

preseason_hold pins a division to a season so the schedule-aware season
resolver cannot move it on. Women's 1. deild needs it: its 2027 reserve
sides were renamed and its format grew to 12 teams, and neither is
modelled yet.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>
EOF
```

---

### Task 10: `.current_season_2dt()` (WS5, §5, F8)

**Files:**
- Create: `R/season-structure-2dt.R`
- Test: `tests/testthat/test-season-structure-2dt.R` (create)

**Interfaces:**
- Produces:
  - `.current_season_2dt(results, schedules, end_date, division, hold = NA_integer_)` → integer. It is the latest season in `division` with a played result (`match_date <= end_date`, both scores present) or a fixture scheduled after `end_date`, falling back to `end_date`'s year. A set `hold` ignores the schedule and caps the result at `hold`.
  - `.is_set_2dt(x)` → `TRUE` for a length-1 non-NA value.

- [ ] **Step 1: Write the failing tests**

```r
# tests/testthat/test-season-structure-2dt.R
# Season, format and fixtures for the 2DT sports (spec 2026-09-16 §5-§6).

.res <- function(season, division = "BD", date = "2100-03-01") {
  tibble::tibble(
    season = as.integer(season), division = division,
    match_date = as.Date(date), home_team = "A", away_team = "B",
    home_score = 80L, away_score = 70L
  )
}
.sch <- function(season, division = "BD", date) {
  tibble::tibble(
    season = as.integer(season), division = division,
    match_date = as.Date(date), home_team = "A", away_team = "B"
  )
}
.end <- as.Date("2100-09-16")

test_that("a published next season is current before any of it is played", {
  expect_identical(
    .current_season_2dt(.res(2100), .sch(2101, date = "2100-09-29"), .end, "BD"),
    2101L
  )
})

test_that("without a future fixture the last results season stands", {
  expect_identical(.current_season_2dt(.res(2100), NULL, .end, "BD"), 2100L)
  expect_identical(
    .current_season_2dt(.res(2100), .sch(2101, date = "2100-09-16"), .end, "BD"),
    2100L
  )
})

test_that("another division's schedule does not move this one", {
  expect_identical(
    .current_season_2dt(.res(2100, "1D"), .sch(2101, "BD", "2100-09-29"), .end, "1D"),
    2100L
  )
})

test_that("a held division ignores its schedule and never passes its hold", {
  expect_identical(
    .current_season_2dt(.res(2100, "1D"), .sch(2101, "1D", "2100-09-29"), .end, "1D", hold = 2100L),
    2100L
  )
  started <- dplyr::bind_rows(.res(2100, "1D"), .res(2101, "1D", "2100-09-10"))
  expect_identical(
    .current_season_2dt(started, NULL, .end, "1D", hold = 2100L),
    2100L
  )
})

test_that("results after end_date do not count and an empty cell uses the calendar year", {
  expect_identical(
    .current_season_2dt(.res(2101, date = "2100-10-01"), NULL, .end, "BD"),
    2100L
  )
})
```

Run `<stem>` = `season-structure-2dt`. Expected: FAIL.

- [ ] **Step 2: Create `R/season-structure-2dt.R`**

```r
#' @include publish-format.R extract-football-iceland.R publish-iceland-2dt-helpers.R
NULL

# Season, format and remaining fixtures for the 2DT sports (spec 2026-09-16
# §5-§6). The extractor and the publisher both call these, so the two layers
# cannot disagree about which season is current or how long it is.

.is_set_2dt <- function(x) {
  length(x) == 1L && !is.na(x)
}

# The current season of one division: the latest season with a played result
# on or before `end_date`, or with a fixture scheduled after it.
#
# Results alone (the old rule, F8) keep a finished season current until its
# successor's first match, so a pre-season forecast showed last season's
# table. Football keeps that rule (D5): KSI publishes its schedule in halves.
#
# `hold` pins a division (config `preseason_hold`): its schedule is ignored and
# the result never passes the held season, even once the next season starts.
.current_season_2dt <- function(results, schedules, end_date, division,
                                hold = NA_integer_) {
  end_date <- as.Date(end_date)
  seasons <- integer()
  if (!is.null(results) && nrow(results) > 0L) {
    played <- results$division %in% division &
      !is.na(results$match_date) & results$match_date <= end_date &
      !is.na(results$home_score) & !is.na(results$away_score)
    seasons <- c(seasons, results$season[played])
  }
  held <- .is_set_2dt(hold)
  if (!held && !is.null(schedules) && nrow(schedules) > 0L) {
    ahead <- schedules$division %in% division &
      !is.na(schedules$match_date) & schedules$match_date > end_date
    seasons <- c(seasons, schedules$season[ahead])
  }
  seasons <- seasons[!is.na(seasons)]
  season <- if (length(seasons) == 0L) {
    as.integer(format(end_date, "%Y"))
  } else {
    as.integer(max(seasons))
  }
  if (held) {
    season <- min(season, as.integer(hold))
  }
  season
}
```

- [ ] **Step 3: Collate and run**

```bash
cd /Users/brynjolfurjonsson/sports && LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 Rscript -e 'devtools::document()'
```

Run `<stem>` = `season-structure-2dt`. Expected: PASS.

- [ ] **Step 4: Commit**

```bash
cd /Users/brynjolfurjonsson/sports && git fetch -q && git branch --show-current
git add R/season-structure-2dt.R DESCRIPTION tests/testthat/test-season-structure-2dt.R
git commit -F - <<'EOF'
feat(2dt): resolve a division's current season from its schedule too

A published next season is current from the day its fixtures appear, so
a pre-season forecast is no longer last season's table. A held division
ignores its schedule and stays on its held season.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>
EOF
```

---

### Task 11: Season format, remaining fixtures and the base table (WS5, §6, F9, F16, F17)

**Files:**
- Modify: `R/season-structure-2dt.R` (append)
- Test: `tests/testthat/test-season-structure-2dt.R` (append)

**Interfaces:**
- Consumes: `.is_set_2dt()` (Task 10); `.division_rr_multiplicity_pfi(results, current_season, division)` and `.points_2dt()` (existing).
- Produces:
  - `.division_format_2dt(results, schedules, season, division, expected_meetings = NA_integer_, regular_season_rounds = NA_integer_)` → `list(meetings = <int or NA>, source = <"schedule" | "config" | "prior_results" | "none" | "not_applicable">)`.
  - `.remaining_fixtures_2dt(teams, played, scheduled, meetings)` → tibble(`home_team`, `away_team`). `played` has `home_team`/`away_team`. `scheduled` has `home_team`/`away_team`/`match_date` and must already be restricted to fixtures after `end_date`.
  - `.base_standings_2dt(played, teams, has_ties = FALSE, tie_threshold = 0)` → tibble(`team`, `base_points`, `base_gd`, `base_gf`), all integer, one row per team, sorted by team.

- [ ] **Step 1: Write the failing tests** (append)

```r
.double_rr <- function(teams) {
  g <- expand.grid(home_team = teams, away_team = teams, stringsAsFactors = FALSE)
  tibble::as_tibble(g[g$home_team != g$away_team, ])
}
.single_rr <- function(teams) {
  p <- utils::combn(teams, 2L)
  tibble::tibble(home_team = p[1, ], away_team = p[2, ])
}
.dated <- function(fx, season, division = "OD",
                   start = as.Date("2100-01-01"), scored = FALSE) {
  fx$season <- as.integer(season)
  fx$division <- division
  fx$match_date <- start + seq_len(nrow(fx))
  if (scored) {
    fx$home_score <- 25L
    fx$away_score <- 24L
    fx$round <- seq_len(nrow(fx))
  }
  fx
}

# ---- .division_format_2dt() -------------------------------------------------

test_that("a format change at the season boundary follows the new schedule (F17)", {
  old <- LETTERS[1:8]
  new <- LETTERS[1:10]
  triple <- .dated(dplyr::bind_rows(.double_rr(old), .single_rr(old)), 2100L, scored = TRUE)
  next_season <- .dated(.double_rr(new), 2101L, start = as.Date("2100-09-05"))
  expect_identical(
    .division_format_2dt(triple, next_season, 2101L, "OD",
      expected_meetings = 3L, regular_season_rounds = NA_integer_
    ),
    list(meetings = 2L, source = "schedule")
  )
})

test_that("a partial fixture list is not a format signal", {
  teams <- LETTERS[1:6]
  played <- .dated(.single_rr(teams), 2100L, scored = TRUE)
  legs <- .single_rr(teams)[1:3, ]
  return_legs <- .dated(
    tibble::tibble(home_team = legs$away_team, away_team = legs$home_team),
    2100L, start = as.Date("2100-02-01")
  )
  expect_identical(
    .division_format_2dt(played, return_legs, 2100L, "OD",
      expected_meetings = 2L, regular_season_rounds = NA_integer_
    ),
    list(meetings = 2L, source = "config")
  )
})

test_that("with no usable schedule and no config, the last completed season decides", {
  teams <- LETTERS[1:4]
  prior <- .dated(.double_rr(teams), 2099L, scored = TRUE)
  fragment <- .dated(.single_rr(teams)[1:2, ], 2100L, scored = TRUE)
  expect_identical(
    .division_format_2dt(dplyr::bind_rows(prior, fragment), NULL, 2100L, "OD",
      expected_meetings = NA_integer_, regular_season_rounds = NA_integer_
    ),
    list(meetings = 2L, source = "prior_results")
  )
})

test_that("a stated regular_season_rounds leaves the meetings unknown", {
  expect_identical(
    .division_format_2dt(NULL, NULL, 2100L, "1D",
      expected_meetings = NA_integer_, regular_season_rounds = 18L
    ),
    list(meetings = NA_integer_, source = "not_applicable")
  )
})

test_that("nothing to go on is 'none'", {
  expect_identical(
    .division_format_2dt(NULL, NULL, 2100L, "OD", NA_integer_, NA_integer_),
    list(meetings = NA_integer_, source = "none")
  )
})

# ---- .remaining_fixtures_2dt() ----------------------------------------------

.pairs <- function(df) paste(df$home_team, df$away_team)

test_that("a double round robin is completed with the right venues", {
  played <- tibble::tibble(home_team = c("A", "B", "C"), away_team = c("B", "C", "A"))
  out <- .remaining_fixtures_2dt(c("A", "B", "C"), played, NULL, meetings = 2L)
  expect_setequal(.pairs(out), c("B A", "C B", "A C"))
})

test_that("a pairing missing from the schedule is simulated anyway (F16)", {
  teams <- c("A", "B", "C", "D")
  sched <- .dated(.double_rr(teams), 2101L)
  sched <- sched[!(sched$home_team == "C" & sched$away_team == "D"), ]
  out <- .remaining_fixtures_2dt(teams, NULL, sched, meetings = 2L)
  expect_equal(nrow(out), 12L)
  expect_true("C D" %in% .pairs(out))
  expect_false(any(duplicated(.pairs(out))))
})

test_that("a triple round robin splits each pairing's venues two and one", {
  out <- .remaining_fixtures_2dt(c("A", "B", "C"), NULL, NULL, meetings = 3L)
  expect_equal(nrow(out), 9L)
  unordered <- paste(pmin(out$home_team, out$away_team), pmax(out$home_team, out$away_team))
  expect_true(all(table(unordered) == 3L))
  expect_setequal(as.integer(table(.pairs(out))), c(1L, 2L))
})

test_that("scheduled games past a pairing's meetings are not simulated", {
  # An embedded play-off game between two sides whose regular meetings are
  # done: the forward half of the regular-season cut (F9).
  played <- tibble::tibble(home_team = c("A", "B"), away_team = c("B", "A"))
  sched <- tibble::tibble(home_team = "A", away_team = "B", match_date = as.Date("2100-04-01"))
  expect_equal(nrow(.remaining_fixtures_2dt(c("A", "B"), played, sched, 2L)), 0L)
})

test_that("scheduled venues are kept and a venue already used up is skipped", {
  played <- tibble::tibble(home_team = "A", away_team = "B")
  sched <- tibble::tibble(
    home_team = c("A", "B"), away_team = c("B", "A"),
    match_date = as.Date(c("2100-02-01", "2100-03-01"))
  )
  out <- .remaining_fixtures_2dt(c("A", "B"), played, sched, meetings = 2L)
  expect_identical(.pairs(out), "B A")
})

test_that("unknown meetings fall back to the schedule alone", {
  sched <- tibble::tibble(home_team = "A", away_team = "B", match_date = as.Date("2100-02-01"))
  out <- .remaining_fixtures_2dt(c("A", "B", "C"), NULL, sched, meetings = NA_integer_)
  expect_identical(.pairs(out), "A B")
})

# ---- .base_standings_2dt() --------------------------------------------------

test_that("the base table carries every division team, played or not", {
  played <- tibble::tibble(
    home_team = "A", away_team = "B", home_score = 30L, away_score = 30L
  )
  hb <- .base_standings_2dt(played, c("A", "B", "C"), has_ties = TRUE, tie_threshold = 0.5)
  expect_identical(hb$team, c("A", "B", "C"))
  expect_identical(hb$base_points, c(1L, 1L, 0L))
  expect_identical(hb$base_gd, c(0L, 0L, 0L))
  expect_identical(hb$base_gf, c(30L, 30L, 0L))

  bb <- .base_standings_2dt(NULL, c("B", "A"))
  expect_identical(bb$team, c("A", "B"))
  expect_identical(bb$base_points, c(0L, 0L))
  expect_identical(bb$base_gf, c(0L, 0L))
})
```

Run `<stem>` = `season-structure-2dt`. Expected: the new tests FAIL.

- [ ] **Step 2: Implement** (append to `R/season-structure-2dt.R`)

```r
# ---- Format -------------------------------------------------------------------

# When the season's own fixture list is trusted as a format statement: it must
# name (nearly) every pairing, agree with itself on one meetings count, and
# give every team about the same number of games. A partial list -- a few
# dated fixtures, or a regular season with its play-offs appended -- fails at
# least one test and falls through to config.
MULTIPLICITY_SCHEDULE_COVERAGE <- 0.9
MULTIPLICITY_SCHEDULE_AGREEMENT <- 0.75
MULTIPLICITY_SCHEDULE_BALANCE <- 0.1

# Meetings per pairing for one division's season (spec 2026-09-16 §6).
#
# Order: the season's own fixtures, then config `expected_meetings`, then the
# last completed season's results. The season's own fixtures come first
# because formats change between seasons: women's Olisdeild went from a triple
# round robin of 8 (2026) to a double of 10 (2027) while config and history
# both still said 3 (F17).
#
# Config comes before history, the reverse of the spec. History is read with
# football's `.division_rr_multiplicity_pfi()`, whose max over pairs reads
# basketball's embedded urslitakeppni as 4-5 meetings; a stated format beats
# that guess, and on every configured cell the two agree anyway.
#
# A stated `regular_season_rounds` means no meetings constant describes the
# cell (basketball female 1D); the meetings stay unknown and the remaining
# fixtures are the schedule's.
.division_format_2dt <- function(results, schedules, season, division,
                                 expected_meetings = NA_integer_,
                                 regular_season_rounds = NA_integer_) {
  if (.is_set_2dt(regular_season_rounds)) {
    return(list(meetings = NA_integer_, source = "not_applicable"))
  }
  from_schedule <- .schedule_meetings_2dt(results, schedules, season, division)
  if (!is.na(from_schedule)) {
    return(list(meetings = from_schedule, source = "schedule"))
  }
  if (.is_set_2dt(expected_meetings)) {
    return(list(meetings = as.integer(expected_meetings), source = "config"))
  }
  prior <- .division_rr_multiplicity_pfi(results, season, division)
  if (!is.na(prior)) {
    return(list(meetings = as.integer(prior), source = "prior_results"))
  }
  list(meetings = NA_integer_, source = "none")
}

# The modal meetings count over the season's played and scheduled fixtures
# (de-duplicated on date: the current season's schedule keeps its played
# rows), or NA when the list is not a trustworthy format statement.
.schedule_meetings_2dt <- function(results, schedules, season, division) {
  cols <- c("home_team", "away_team", "match_date")
  pick <- function(df) {
    if (is.null(df) || nrow(df) == 0L) {
      return(NULL)
    }
    df[df$season == season & df$division == division &
      !is.na(df$match_date), cols, drop = FALSE]
  }
  fx <- dplyr::distinct(dplyr::bind_rows(pick(results), pick(schedules)))
  if (nrow(fx) == 0L) {
    return(NA_integer_)
  }
  teams <- unique(c(fx$home_team, fx$away_team))
  n_pairs <- length(teams) * (length(teams) - 1L) / 2L
  pair <- paste(
    pmin(fx$home_team, fx$away_team), pmax(fx$home_team, fx$away_team),
    sep = "|"
  )
  meetings <- as.integer(table(pair))
  if (n_pairs < 1L || length(meetings) / n_pairs < MULTIPLICITY_SCHEDULE_COVERAGE) {
    return(NA_integer_)
  }
  counts <- table(meetings)
  top <- max(counts)
  if (top / length(meetings) < MULTIPLICITY_SCHEDULE_AGREEMENT) {
    return(NA_integer_)
  }
  games <- as.integer(table(c(fx$home_team, fx$away_team)))
  slack <- max(1L, as.integer(floor(MULTIPLICITY_SCHEDULE_BALANCE * max(games))))
  if (max(games) - min(games) > slack) {
    return(NA_integer_)
  }
  # A tie between two counts takes the larger: the fuller format.
  max(as.integer(names(counts)[counts == top]))
}

# ---- Remaining fixtures ------------------------------------------------------

# Every fixture of the season still to be played, derived structurally
# (football's approach, R/extract-football-iceland.R): each pairing meets
# `meetings` times in all, each side hosting at most ceiling(meetings / 2).
#
# Scheduled fixtures are taken first, in date order, because their venues are
# real; a scheduled game beyond the pairing's meetings (an embedded play-off)
# or on a venue already used up is skipped. What the schedule does not cover
# is generated, alternating venues (F16). This replaces the per-team cap that
# counted last season's games (F9) and any dependence on Stan's 14-day window.
#
# Unknown `meetings` returns the schedule as is.
.remaining_fixtures_2dt <- function(teams, played, scheduled, meetings) {
  teams <- sort(unique(as.character(teams)))
  sched <- if (is.null(scheduled) || nrow(scheduled) == 0L) {
    tibble::tibble(
      home_team = character(), away_team = character(),
      match_date = as.Date(character())
    )
  } else {
    scheduled[
      scheduled$home_team %in% teams & scheduled$away_team %in% teams,
      c("home_team", "away_team", "match_date"),
      drop = FALSE
    ]
  }
  sched <- sched[order(sched$match_date), , drop = FALSE]
  if (!.is_set_2dt(meetings)) {
    return(tibble::tibble(home_team = sched$home_team, away_team = sched$away_team))
  }
  if (length(teams) < 2L) {
    return(tibble::tibble(home_team = character(), away_team = character()))
  }

  m <- as.integer(meetings)
  cap <- (m + 1L) %/% 2L
  key <- function(h, a) paste(h, a, sep = "|")
  played_n <- if (is.null(played) || nrow(played) == 0L) {
    integer()
  } else {
    table(key(played$home_team, played$away_team))
  }
  n_played <- function(h, a) {
    k <- key(h, a)
    if (k %in% names(played_n)) as.integer(played_n[[k]]) else 0L
  }
  sched_pair <- key(
    pmin(sched$home_team, sched$away_team),
    pmax(sched$home_team, sched$away_team)
  )

  out_h <- character()
  out_a <- character()
  pairs <- utils::combn(teams, 2L)
  for (j in seq_len(ncol(pairs))) {
    a <- pairs[1L, j]
    b <- pairs[2L, j]
    n_ab <- n_played(a, b)
    n_ba <- n_played(b, a)
    for (r in which(sched_pair == key(a, b))) {
      if (n_ab + n_ba >= m) {
        break
      }
      if (sched$home_team[r] == a && n_ab < cap) {
        out_h <- c(out_h, a)
        out_a <- c(out_a, b)
        n_ab <- n_ab + 1L
      } else if (sched$home_team[r] == b && n_ba < cap) {
        out_h <- c(out_h, b)
        out_a <- c(out_a, a)
        n_ba <- n_ba + 1L
      }
    }
    while (n_ab + n_ba < m) {
      if (n_ab <= n_ba && n_ab < cap) {
        out_h <- c(out_h, a)
        out_a <- c(out_a, b)
        n_ab <- n_ab + 1L
      } else {
        out_h <- c(out_h, b)
        out_a <- c(out_a, a)
        n_ba <- n_ba + 1L
      }
    }
  }
  tibble::tibble(home_team = out_h, away_team = out_a)
}

# ---- Base table ----------------------------------------------------------------

# The realised table the season simulation starts from: every division team,
# played or not, with points on the published 2DT scheme.
.base_standings_2dt <- function(played, teams, has_ties = FALSE,
                                tie_threshold = 0) {
  teams <- sort(unique(as.character(teams)))
  if (is.null(played) || nrow(played) == 0L) {
    return(tibble::tibble(
      team = teams, base_points = 0L, base_gd = 0L, base_gf = 0L
    ))
  }
  side <- function(name) {
    is_home <- identical(name, "home")
    tibble::tibble(
      team = if (is_home) played$home_team else played$away_team,
      gf = if (is_home) played$home_score else played$away_score,
      ga = if (is_home) played$away_score else played$home_score,
      pts = .points_2dt(
        played$home_score, played$away_score, name,
        has_ties = has_ties, tie_threshold = tie_threshold
      )
    )
  }
  realised <- dplyr::bind_rows(side("home"), side("away")) |>
    dplyr::summarise(
      base_points = sum(.data$pts),
      base_gd = sum(.data$gf - .data$ga),
      base_gf = sum(.data$gf),
      .by = "team"
    )
  tibble::tibble(team = teams) |>
    dplyr::left_join(realised, by = "team") |>
    dplyr::transmute(
      team = .data$team,
      base_points = as.integer(dplyr::coalesce(.data$base_points, 0L)),
      base_gd = as.integer(dplyr::coalesce(.data$base_gd, 0L)),
      base_gf = as.integer(dplyr::coalesce(.data$base_gf, 0L))
    )
}
```

- [ ] **Step 3: Run** `<stem>` = `season-structure-2dt`. Expected: PASS.

- [ ] **Step 4: Commit**

```bash
cd /Users/brynjolfurjonsson/sports && git fetch -q && git branch --show-current
git add R/season-structure-2dt.R tests/testthat/test-season-structure-2dt.R
git commit -F - <<'EOF'
feat(2dt): season format, remaining fixtures and base table

Meetings per pairing come from the season's own fixtures when they form a
complete, self-consistent list (women's Olisdeild changed from a triple
round robin to a double), else from config, else from history. Remaining
fixtures are every unplayed meeting: scheduled venues first, the rest
generated, play-off games past the regular meetings skipped.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>
EOF
```

---

### Task 12: Priors for teams without history (WS5, §7, D8, F7)

**Files:**
- Modify: `R/simulate-2dt.R` (append)
- Test: `tests/testthat/test-simulate-2dt.R` (append)

**Interfaces:**
- Consumes: the `.extract_sim_inputs_2dt()` output shape (Task 7).
- Produces: `.add_new_team_priors_2dt(sim_inputs, division_teams)` → `sim_inputs` with rows added, same columns, for every division team missing from `sim_inputs$team`. It returns `sim_inputs` unchanged when none is missing.

- [ ] **Step 1: Write the failing tests** (append)

```r
.rated_inputs <- function(n_draws = 4000L, handball = FALSE) {
  teams <- paste0("T", 1:5)
  team <- tidyr::expand_grid(team = teams, .draw = seq_len(n_draws))
  i <- match(team$team, teams)
  team$cur_offense <- c(4, 2, 0, -2, -4)[i]
  team$cur_defense <- c(2, 1, 0, -1, -2)[i]
  team$home_advantage_off <- c(1, 2, 3, 4, 5)[i]
  team$home_advantage_def <- 1
  if (handball) team$sigma_team <- 5
  scalar <- tibble::tibble(
    .draw = seq_len(n_draws), mean_sigma_team = log(4), scale_sigma_team = 0.2
  )
  list(team = team, scalar = scalar)
}

test_that("a team without history is centred on the division's bottom two (§7)", {
  set.seed(5)
  out <- .add_new_team_priors_2dt(.rated_inputs(), c(paste0("T", 1:5), "NEW"))
  new <- out$team[out$team$team == "NEW", ]
  expect_equal(nrow(new), 4000L)
  # Bottom two by offence + defence are T5 and T4: offence -3, defence -1.5.
  expect_lt(abs(mean(new$cur_offense) + 3), 0.25)
  expect_lt(abs(mean(new$cur_defense) + 1.5), 0.12)
  # Spread: 1.5 x the rated division's between-team SD.
  expect_lt(abs(stats::sd(new$cur_offense) / (1.5 * stats::sd(c(4, 2, 0, -2, -4))) - 1), 0.05)
  expect_equal(unique(new$home_advantage_off), 3)
  expect_equal(unique(new$home_advantage_def), 1)
  expect_false("sigma_team" %in% names(out$team))
})

test_that("handball newcomers draw sigma_team from the fitted hierarchy", {
  set.seed(6)
  # A division with one rated team borrows the whole league's bottom two.
  out <- .add_new_team_priors_2dt(.rated_inputs(handball = TRUE), c("T1", "NEW"))
  new <- out$team[out$team$team == "NEW", ]
  expect_lt(abs(mean(log(new$sigma_team)) - log(4)), 0.02)
  expect_lt(abs(stats::sd(log(new$sigma_team)) - 0.2), 0.01)
  expect_lt(abs(mean(new$cur_offense) + 3), 0.25)
})

test_that("a division whose teams are all rated is returned unchanged", {
  si <- .rated_inputs(10L)
  expect_identical(.add_new_team_priors_2dt(si, paste0("T", 1:5)), si)
})
```

Run `<stem>` = `simulate-2dt$`. Expected: the three new tests FAIL.

- [ ] **Step 2: Implement** (append to `R/simulate-2dt.R`)

```r
# ---- Teams without history ------------------------------------------------------

# A scheduled team the fit has never seen (a promoted reserve side, a new
# club) would otherwise empty the whole table: simulate_league_season() stops
# on a team without draws, and the extractor used to skip the division (F7).
# It gets prior draws instead (spec 2026-09-16 §7, D8), per draw:
#   * centre: the mean offence and mean defence of the division's
#     NEW_TEAM_PRIOR_BOTTOM_N weakest rated teams by offence + defence (a
#     higher defence concedes less). One set of teams for both components
#     keeps them coherent;
#   * spread: NEW_TEAM_PRIOR_SPREAD x the rated division's between-team SD;
#   * home advantage: the rated division's mean;
#   * handball sigma_team: exp(mean_sigma_team + scale_sigma_team * z), the
#     model's own hierarchy.
# Random-walk step sizes are not drawn: strengths are frozen (D3). A division
# with fewer rated teams than NEW_TEAM_PRIOR_BOTTOM_N borrows the whole
# league. Such a team still has no next_games rows until it has been fitted.
NEW_TEAM_PRIOR_BOTTOM_N <- 2L
NEW_TEAM_PRIOR_SPREAD <- 1.5

.add_new_team_priors_2dt <- function(sim_inputs, division_teams) {
  team <- sim_inputs$team
  new <- sort(setdiff(division_teams, unique(team$team)))
  if (length(new) == 0L) {
    return(sim_inputs)
  }
  rated <- team[team$team %in% division_teams, , drop = FALSE]
  if (length(unique(rated$team)) < NEW_TEAM_PRIOR_BOTTOM_N) {
    rated <- team
  }
  if (length(unique(rated$team)) < NEW_TEAM_PRIOR_BOTTOM_N) {
    stop(
      ".add_new_team_priors_2dt: fewer than ", NEW_TEAM_PRIOR_BOTTOM_N,
      " rated teams in the whole fit; cannot place ",
      paste(new, collapse = ", "), ".",
      call. = FALSE
    )
  }
  bottom <- seq_len(NEW_TEAM_PRIOR_BOTTOM_N)
  per_draw <- rated |>
    dplyr::mutate(total = .data$cur_offense + .data$cur_defense) |>
    dplyr::summarise(
      centre_off = mean(.data$cur_offense[order(.data$total)][bottom]),
      centre_def = mean(.data$cur_defense[order(.data$total)][bottom]),
      spread_off = NEW_TEAM_PRIOR_SPREAD * stats::sd(.data$cur_offense),
      spread_def = NEW_TEAM_PRIOR_SPREAD * stats::sd(.data$cur_defense),
      ha_off = mean(.data$home_advantage_off),
      ha_def = mean(.data$home_advantage_def),
      .by = ".draw"
    )

  grid <- tidyr::expand_grid(team = new, .draw = per_draw$.draw)
  pd <- per_draw[match(grid$.draw, per_draw$.draw), , drop = FALSE]
  n <- nrow(grid)
  added <- tibble::tibble(
    team = grid$team,
    .draw = grid$.draw,
    cur_offense = pd$centre_off + pd$spread_off * stats::rnorm(n),
    cur_defense = pd$centre_def + pd$spread_def * stats::rnorm(n),
    home_advantage_off = pd$ha_off,
    home_advantage_def = pd$ha_def
  )
  if ("sigma_team" %in% names(team)) {
    sc <- sim_inputs$scalar[match(grid$.draw, sim_inputs$scalar$.draw), , drop = FALSE]
    added$sigma_team <- exp(
      sc$mean_sigma_team + sc$scale_sigma_team * stats::rnorm(n)
    )
  }
  sim_inputs$team <- dplyr::bind_rows(team, added[, names(team)])
  sim_inputs
}
```

- [ ] **Step 3: Run** `<stem>` = `simulate-2dt$`. Expected: PASS.

- [ ] **Step 4: Commit**

```bash
cd /Users/brynjolfurjonsson/sports && git fetch -q && git branch --show-current
git add R/simulate-2dt.R tests/testthat/test-simulate-2dt.R
git commit -F - <<'EOF'
feat(2dt): below-average priors for scheduled teams without history

A team the fit has never seen no longer empties its division's table. It
is centred on the division's two weakest rated teams, 1.5x as uncertain as
the spread between teams, with the division's mean home advantage and, for
handball, a sigma_team from the fitted hierarchy.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>
EOF
```

---

### Task 13: The extractor projects the whole regular season (WS5, §1-§2, §5-§7)

**Files:**
- Modify: `R/extract-iceland-2dt-shared.R` (`.extract_2dt_iceland_pfi`; delete `.regular_season_game_nrs_2dt`; oracle note on `.compute_final_positions_2dt`), `R/publish-format.R` (one comment), `R/extract-basketball-iceland.R` (roxygen line)
- Test: `tests/testthat/test-extract-2dt-season-projection.R` (create); `tests/testthat/test-extract-2dt-divisions.R` (two tests encode the 14-day window)
- Regenerate: `tests/testthat/fixtures/extracts/**`

**Interfaces:**
- Consumes: everything from Tasks 5-12.
- Produces: the same six division-keyed parquets with the same columns; `final_positions` and `points_distribution` now describe the whole regular season of each division's current season.

- [ ] **Step 1: Write the failing tests**

```r
# tests/testthat/test-extract-2dt-season-projection.R
# The 2DT extractor's table covers the whole regular season of each
# division's current season (spec 2026-09-16 §1, §11).
#
# Every simulated game hands out exactly two points (2-0, or 1-1 inside
# handball's tie threshold), so a division's expected points total is
# 2 x its games -- an invariant that tells a full season from a 14-day window
# and a new season from last season's table.

# The fixture's handball men's OD (4 teams) between seasons: the 2100 season
# is complete, the 2101 double round robin is published a week apart from
# FIXTURE_END_DATE + 1, so only its first two games sit inside Stan's window.
.preseason_cell <- function(new_team = NULL, drop_pair = NULL, n_draws = 50L,
                            env = parent.frame()) {
  root <- fixture_facts_root(env = env)
  teams <- fixture_division_teams("handball", "male", "OD")
  season_teams <- c(teams, new_team)
  g <- expand.grid(home_team = season_teams, away_team = season_teams,
                   stringsAsFactors = FALSE)
  g <- g[g$home_team != g$away_team, ]
  if (!is.null(drop_pair)) {
    g <- g[!(g$home_team == drop_pair[1] & g$away_team == drop_pair[2]), ]
  }
  next_season <- tibble::tibble(
    sport = "handball", country = "iceland", sex = "male", season = 2101L,
    match_date = FIXTURE_END_DATE + 7L * seq_len(nrow(g)) - 6L,
    home_team = g$home_team, away_team = g$away_team, division = "OD",
    round = seq_len(nrow(g)), kickoff_time = "19:30"
  )
  sched <- read_table("schedules", root = root)
  keep <- !(sched$sport == "handball" & sched$sex == "male" & sched$division == "OD")
  write_table(dplyr::bind_rows(sched[keep, ], next_season), "schedules", root = root)

  league <- load_leagues()[["handball_iceland"]]
  st <- suppressMessages(local_stub_2dt(league, "male", root = root, n_draws = n_draws))
  extracts_root <- file.path(withr::local_tempdir(.local_envir = env), "extracts")
  suppressMessages(extract_handball_iceland(
    fit = st$fit, league = league, sex = "male",
    fit_date = FIXTURE_FIT_DATE, end_date = FIXTURE_END_DATE,
    root = root, extracts_root = extracts_root, prep = st$prep
  ))
  part <- file.path(
    extracts_root, "sport=handball", "country=iceland", "sex=male",
    paste0("fit_date=", format(FIXTURE_FIT_DATE, "%Y-%m-%d"))
  )
  list(
    teams = teams,
    read = function(ft) {
      d <- arrow::read_parquet(file.path(part, paste0(ft, ".parquet")))
      d[d$division == "OD", ]
    }
  )
}

.expected_total <- function(pd) sum(pd$points * pd$probability)

test_that("before a ball is played the table is the whole new season", {
  cell <- .preseason_cell()
  fp <- cell$read("final_positions")
  expect_setequal(unique(fp$team), cell$teams)
  expect_setequal(unique(fp$placement), 1:4)
  pd <- cell$read("points_distribution")
  # Twelve 2101 fixtures and nothing banked. The old path would have added
  # 2100's 12 banked points to two windowed games: 16.
  expect_equal(.expected_total(pd), 24, tolerance = 1e-9)
  expect_lte(max(pd$points), 12)
})

test_that("a pairing missing from the new schedule is still played out (F16)", {
  cell <- .preseason_cell(drop_pair = c("HAM OD 03", "HAM OD 04"))
  expect_equal(.expected_total(cell$read("points_distribution")), 24, tolerance = 1e-9)
})

test_that("a scheduled team with no history is tabled below the division median (§7)", {
  cell <- .preseason_cell(new_team = "HAM OD NEW", n_draws = 400L)
  fp <- cell$read("final_positions")
  expect_setequal(unique(fp$team), c(cell$teams, "HAM OD NEW"))
  newcomer <- fp[fp$team == "HAM OD NEW", ]
  expect_equal(sum(newcomer$probability), 1, tolerance = 1e-9)
  expect_gt(sum(newcomer$placement * newcomer$probability), 3)
  expect_equal(.expected_total(cell$read("points_distribution")), 40, tolerance = 1e-9)
  # The strength surfaces still describe only teams the fit knows.
  expect_false("HAM OD NEW" %in% cell$read("team_strengths_quantiles")$team)
})

test_that("re-extracting the same fit reproduces its tables", {
  a <- .preseason_cell()$read("final_positions")
  b <- .preseason_cell()$read("final_positions")
  expect_identical(a, b)
})
```

In `tests/testthat/test-extract-2dt-divisions.R`, the test "played post-season rounds never reach the 2DT league table" encodes the 14-day window. Replace its body from `pd <- read_part(cell, "points_distribution")` to the end of the test with:

```r
  pd <- read_part(cell, "points_distribution")
  pd_04 <- pd[pd$division == "BD" & pd$team == teams[4L], ]
  # The weakest team finished its three regular games on 0 points and has its
  # three return legs left, so 6 is its ceiling. With the four playoff rounds
  # counted its BASE alone would be 8.
  expect_lte(max(pd_04$points), 6)

  fp <- read_part(cell, "final_positions")
  bd <- fp[fp$division == "BD" & fp$placement == 1L, ]
  # Counted, the injected playoff wins put it on 8 and it took the title in
  # 40 % of draws.
  expect_lt(bd$probability[bd$team == teams[4L]], 0.05)
  expect_gt(bd$probability[bd$team == teams[1L]], 0.5)
```

In the test "upcoming post-season fixtures publish but score no points", replace only the comment above `expect_lte(max(pd_01$points), 12)` with:

```r
  # 6 realised points plus its three return legs at 2 points each. The
  # scheduled games past the double round robin are skipped; counted, the
  # ceiling would be 16.
```

Run `<stem>` = `extract-2dt-season-projection`. Expected: FAIL (the expected totals come out 16, not 24; the newcomer is missing from the table).

- [ ] **Step 2: Rewire `.extract_2dt_iceland_pfi()`**

(a) After `division_is_cup <- .iceland_division_is_cup(key, sex)` add:

```r
  division_hold <- .iceland_division_preseason_hold(key, sex)
```

(b) Replace the `current_season <- if (nrow(results) > 0L) { ... }` block with:

```r
  # The fit's scoring level belongs to the latest season it has results for;
  # a division projecting a later season steps it forward (F12). Each
  # division resolves its own season below.
  last_fitted_season <- if (nrow(results) > 0L) {
    max(results$season, na.rm = TRUE)
  } else {
    NA_integer_
  }
```

(c) After `home_advantage_draws <- .extract_home_advantage_draws_2dt(fit, teams)` add:

```r
  # Season-simulation inputs, pulled once (spec 2026-09-16 §4). Seeded from
  # fit_date, so re-extracting a fit reproduces its tables and the committed
  # fixture does not churn.
  sim_seed <- as.integer(format(as.Date(fit_date), "%Y%m%d"))
  sim_inputs <- .extract_sim_inputs_2dt(
    fit, teams, sport, n_seasons = prep$stan_data$N_seasons
  )
  sim_inputs$scalar$z_level <- withr::with_seed(
    sim_seed, stats::rnorm(nrow(sim_inputs$scalar))
  )
  match_fn <- .match_fn_2dt(sport)
  points_fn <- .points_fn_2dt(has_ties, tie_threshold)
```

(d) Replace the per-division head — from `per_div <- lapply(divisions, function(div) {` down to (not including) `# ---- round_strengths_quantiles` — with the block below. Keep the existing "THE REGULAR-SEASON CUT (D3)" comment where marked.

```r
  per_div <- lapply(divisions, function(div) {
    # The division's own season: its schedule counts as well as its results,
    # so a published next season is current before its first match (spec
    # 2026-09-16 §5). A held division stays on its held season (§5.1).
    season_div <- .current_season_2dt(
      results, schedules, end_date, div,
      hold = division_hold[[div]]
    )
    # Meetings per pairing, from the season's own fixtures where they form a
    # complete list (§6, F17). The publisher calls the same helper.
    division_format <- .division_format_2dt(
      results, schedules, season_div, div,
      expected_meetings = expected_meetings[[div]],
      regular_season_rounds = regular_season_rounds[[div]]
    )

    # <keep the existing "THE REGULAR-SEASON CUT (D3)" comment block here>
    rounds <- .publish_n_rounds(
      results = results,
      schedules = schedules,
      season = season_div,
      division_codes = div,
      end_date = as.Date(end_date),
      expected_meetings = division_format$meetings,
      regular_season_rounds = regular_season_rounds[[div]],
      is_cup = isTRUE(division_is_cup[[div]])
    )

    top_results <- results[
      results$season == season_div & results$division == div, ,
      drop = FALSE
    ]
    top_results <- .regular_season_cut(top_results, rounds)

    # The division's teams are the SEASON's: those who have played inside the
    # regular cut and those only scheduled so far. From played results alone,
    # handball one round into 2026-27 published 8 of its 24 teams on some
    # surfaces and all 24 on others.
    season_fixtures <- schedules[
      schedules$season == season_div & schedules$division == div, ,
      drop = FALSE
    ]
    div_teams <- sort(unique(c(
      top_results$home_team, top_results$away_team,
      season_fixtures$home_team, season_fixtures$away_team
    )))
    # The strength surfaces describe only teams the fit knows.
    current_top_teams <- tibble::tibble(
      team = div_teams[div_teams %in% teams$team]
    )

    # ---- final_positions + points_distribution: the whole regular season ---
    # Realised results plus a simulation of every remaining regular-season
    # fixture from the fit's latest-round strengths, not the ~2 rounds inside
    # Stan's 14-day prediction window (spec 2026-09-16 §1-§2, F1). Stan's
    # window now feeds predicted_matches (next_games) and nothing else.
    upcoming <- season_fixtures[
      !is.na(season_fixtures$match_date) &
        season_fixtures$match_date > as.Date(end_date), ,
      drop = FALSE
    ]
    remaining <- .remaining_fixtures_2dt(
      teams = div_teams,
      played = top_results,
      scheduled = upcoming,
      meetings = division_format$meetings
    )
    base_standings <- .base_standings_2dt(
      top_results, div_teams,
      has_ties = has_ties, tie_threshold = tie_threshold
    )
    seasons_ahead <- if (is.na(last_fitted_season)) {
      0L
    } else {
      max(0L, as.integer(season_div - last_fitted_season))
    }
    season_sim <- withr::with_seed(sim_seed, {
      div_inputs <- .add_new_team_priors_2dt(sim_inputs, div_teams)
      simulate_league_season(
        sim_inputs_team = div_inputs$team,
        sim_inputs_scalar = .season_level_2dt(div_inputs$scalar, seasons_ahead),
        remaining_fixtures = remaining,
        base_standings = base_standings,
        match_fn = match_fn,
        points_fn = points_fn,
        tie_break = "jitter"
      )
    })
```

(e) In the trajectory call, change `current_season = current_season` to `current_season = season_div`.

(f) In the returned `list(...)`, replace the `final_positions = .compute_final_positions_2dt(...)` and `points_distribution = .compute_points_distribution_2dt(...)` entries with:

```r
      final_positions = season_sim$final_positions,
      points_distribution = season_sim$points_distribution
```

(g) Delete `.regular_season_game_nrs_2dt()` and its comment block (its only caller is gone). In `R/publish-format.R`, replace the paragraph

```r
#' The FORWARD half of the cut (`.regular_season_game_nrs_2dt()`) is NOT gated:
#' capping how many fixtures are left to play is a question about season
#' length, which both sources answer.
```

with

```r
#' The FORWARD half of the cut does not read this boundary at all:
#' `.remaining_fixtures_2dt()` caps what is left to play at each pairing's
#' meetings (R/season-structure-2dt.R).
```

(h) Directly above `.compute_final_positions_2dt <- function(`, add to its comment:

```r
# Not called by the extractor since 2026-09-16 -- the season table comes from
# simulate_league_season() -- but kept: it is the Stan-window oracle that
# test-simulate-2dt-equivalence.R compares the R generators against.
```

(i) In `R/extract-basketball-iceland.R` roxygen, change the `final_positions.parquet` bullet to:

```r
#' * `final_positions.parquet` — per-team placement probability
#'   (1..n_teams) at the end of the REGULAR season: realised results plus a
#'   simulation of every remaining fixture (`simulate_league_season()`).
```

- [ ] **Step 3: Run the 2DT extract suites**

`<stem>` = `extract-2dt|extract-basketball|extract-handball|stub-fit`. Expected: PASS, including the updated divisions tests. If "a team scheduled but not yet played this season is in every division surface" fails, check that `current_top_teams` still includes HAM OD 03/04 (they are fitted from 2099, so `%in% teams$team` keeps them).

- [ ] **Step 4: Regenerate the committed extracts fixture and inspect the diff**

```bash
cd /Users/brynjolfurjonsson/sports && LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 Rscript tools/make-extract-fixtures.R && git status --short tests/testthat/fixtures
```

Expected: only `final_positions.parquet` and `points_distribution.parquet` under `tests/testthat/fixtures/extracts/` change (8 files: 2 sports x 2 sexes x 2). If any other file changes, find out why before continuing; if `facts/*.parquet` show as modified with identical content (`arrow::read_parquet` equal), restore them with `git checkout --`. The script prints the extracts-tree size, which must stay under the 2048 KB budget.

- [ ] **Step 5: Run everything that reads the fixture or the extractor**

`<stem>` = `fixture|publish|extract|simulate|season-structure`. Expected: PASS. `test-publish-football-golden` must pass unchanged.

- [ ] **Step 6: Commit**

```bash
cd /Users/brynjolfurjonsson/sports && git fetch -q && git branch --show-current
git add R/extract-iceland-2dt-shared.R R/publish-format.R R/extract-basketball-iceland.R man tests/testthat/test-extract-2dt-season-projection.R tests/testthat/test-extract-2dt-divisions.R tests/testthat/fixtures/extracts
git commit -F - <<'EOF'
feat(2dt): project the whole regular season, including pre-season

The 2DT extractor now resolves each division's season from its schedule
as well as its results, derives the remaining fixtures structurally,
gives unrated teams a below-average prior, and simulates the rest of the
season with simulate_league_season(). final_positions and
points_distribution previously covered realised results plus the ~2
rounds inside Stan's 14-day window, and before a season started they
showed last season's table. Stan's window now feeds next_games only.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>
EOF
```

---

### Task 14: The publisher follows the same season and format (WS5, §5-§6, §10)

**Files:**
- Modify: `R/publish-profile.R` (`season_rule`), `R/publish-iceland-league.R` (division accessors, season line, `.publish_n_rounds` call), `R/publish-format.R` (`.build_publish_meta`), `config/publish-schemas/_base/meta.json` (+ generated sport schemas)
- Test: `tests/testthat/test-publish-2dt-preseason.R` (create); `tests/testthat/test-publish-profile.R` (append)

**Interfaces:**
- Consumes: `.current_season_2dt()`, `.division_format_2dt()` (Tasks 10-11), `.iceland_division_preseason_hold()` (Task 9), `.write_empty_standings_pfi()` (Task 1).
- Produces:
  - `sport_publish_profile(sport)$season_rule`: `"results"` (football) or `"schedule_aware"` (basketball, handball).
  - `meta.json` for 2DT cells gains `n_rounds_meetings_source` (`"schedule"`, `"config"`, `"prior_results"`, `"none"` or `"not_applicable"`) directly after `n_rounds_source`. Football's meta is unchanged.

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-publish-profile.R`:

```r
test_that("only the 2DT sports read the season off the schedule (spec 2026-09-16 D5)", {
  expect_identical(sport_publish_profile("football")$season_rule, "results")
  for (sport in c("basketball", "handball")) {
    expect_identical(sport_publish_profile(sport)$season_rule, "schedule_aware", info = sport)
  }
})
```

Create `tests/testthat/test-publish-2dt-preseason.R`:

```r
# A 2DT cell between seasons publishes the NEW season: round 0, its whole
# team list, a full projected table, and an empty standings table that does
# not erase last season's history (spec 2026-09-16 §5, §9.1, §10, §11).

.hb_publish <- function(root, out, extracts_root) {
  league <- load_leagues()[["handball_iceland"]]
  st <- suppressMessages(local_stub_2dt(league, "male", root = root, n_draws = 200L))
  suppressMessages(extract_handball_iceland(
    fit = st$fit, league = league, sex = "male",
    fit_date = FIXTURE_FIT_DATE, end_date = FIXTURE_END_DATE,
    root = root, extracts_root = extracts_root, prep = st$prep
  ))
  extracted <- read_extracted_iceland(
    league, sex = "male", fit_date = FIXTURE_FIT_DATE,
    extracts_root = extracts_root
  )
  suppressMessages(suppressWarnings(publish_iceland_league(
    extracted = extracted, league = league, sex = "male",
    end_date = FIXTURE_END_DATE,
    root = root, output_root = out, extracts_root = extracts_root,
    archive_root = file.path(root, "beliefs", "archive"),
    round_predictions_history_root = file.path(root, "beliefs", "round_predictions_history")
  )))
  file.path(out, "handball", "iceland", "karla-od")
}

test_that("a cell between seasons publishes the new season at round 0 and keeps its history", {
  root <- fixture_facts_root()
  out <- file.path(withr::local_tempdir(), "publish")
  extracts_root <- file.path(withr::local_tempdir(), "extracts")

  # 1. In season: the fixture's 2100 table publishes and seeds the history.
  cell <- .hb_publish(root, out, extracts_root)
  before <- jsonlite::read_json(file.path(cell, "standings_history.json"))
  expect_gt(length(before$records), 0L)
  expect_identical(jsonlite::read_json(file.path(cell, "meta.json"))$season, 2100L)

  # 2. The 2101 double round robin is published; none of it is played.
  teams <- fixture_division_teams("handball", "male", "OD")
  g <- expand.grid(home_team = teams, away_team = teams, stringsAsFactors = FALSE)
  g <- g[g$home_team != g$away_team, ]
  sched <- read_table("schedules", root = root)
  keep <- !(sched$sport == "handball" & sched$sex == "male" & sched$division == "OD")
  write_table(dplyr::bind_rows(sched[keep, ], tibble::tibble(
    sport = "handball", country = "iceland", sex = "male", season = 2101L,
    match_date = FIXTURE_END_DATE + 7L * seq_len(nrow(g)) - 6L,
    home_team = g$home_team, away_team = g$away_team, division = "OD",
    round = seq_len(nrow(g)), kickoff_time = "19:30"
  )), "schedules", root = root)
  cell <- .hb_publish(root, out, extracts_root)

  meta <- jsonlite::read_json(file.path(cell, "meta.json"))
  expect_identical(meta$season, 2101L)
  expect_identical(meta$round, 0L)
  expect_identical(meta$n_rounds, 6L)
  expect_identical(meta$n_rounds_meetings_source, "schedule")
  keys <- names(meta)
  expect_identical(keys[match("n_rounds_source", keys) + 1L], "n_rounds_meetings_source")

  fp <- jsonlite::read_json(file.path(cell, "final_positions.json"))
  expect_identical(fp$season, 2101L)
  expect_identical(fp$n_teams, 4L)
  expect_setequal(vapply(fp$records, function(r) r$team, ""), teams)

  st <- jsonlite::read_json(file.path(cell, "standings.json"))
  expect_identical(st$season, 2101L)
  expect_length(st$rows, 0L)
  after <- jsonlite::read_json(file.path(cell, "standings_history.json"))
  expect_length(after$records, length(before$records))

  v <- validate_publish_dir(
    file.path(out, "handball"),
    schema_dir = here::here("config", "publish-schemas"),
    sport = "handball"
  )
  expect_true(v$ok, info = paste(v$errors, collapse = "\n"))
  expect_gt(v$n_passed, 0L)
})
```

Run `<stem>` = `publish-2dt-preseason|publish-profile`. Expected: FAIL (`season_rule` is NULL; meta reads season 2100).

- [ ] **Step 2: Profile**

In `R/publish-profile.R`, inside `twodt()`'s returned list, after `placement_basis = "regular_season_table"` add:

```r
      placement_basis = "regular_season_table",
      # The season is read off the schedule as well as the results, so a
      # published next season is current before its first match (spec
      # 2026-09-16 §5). Football keeps "results" (D5).
      season_rule = "schedule_aware"
```

In football's list, add `season_rule = "results"` after its `placement_basis` entry (with a trailing comma on the preceding line as needed).

- [ ] **Step 3: Publisher**

In `R/publish-iceland-league.R`:

(a) After `division_regular_rounds <- .iceland_division_regular_season_rounds(league_key, sex)` add:

```r
  division_hold <- .iceland_division_preseason_hold(league_key, sex)
  schedule_aware <- identical(profile$season_rule, "schedule_aware")
```

(b) Replace `current_season <- max(results$season, na.rm = TRUE)` with:

```r
    # The 2DT sports read the season off the schedule as well as the results,
    # so a published next season is current before its first match; the
    # extractor calls the same helper, so the two layers agree (spec
    # 2026-09-16 §5). Football keeps its own rule (D5).
    current_season <- if (schedule_aware) {
      .current_season_2dt(
        results, schedules, end_date, target_div,
        hold = division_hold[[target_div]]
      )
    } else {
      max(results$season, na.rm = TRUE)
    }
```

(c) Replace the `format_facts <- .publish_n_rounds(...)` call with:

```r
    # 2DT: meetings per pairing from the season's own fixtures where they form
    # a complete list (§6, F17) -- the same call the extractor makes.
    division_format <- if (schedule_aware) {
      .division_format_2dt(
        results, schedules, current_season, target_div,
        expected_meetings = division_cfg$expected_meetings,
        regular_season_rounds = division_cfg$regular_season_rounds
      )
    } else {
      NULL
    }
    format_facts <- .publish_n_rounds(
      results = results,
      schedules = schedules,
      season = current_season,
      division_codes = family_divs,
      end_date = end_date,
      expected_meetings = if (is.null(division_format)) {
        division_cfg$expected_meetings
      } else {
        division_format$meetings
      },
      regular_season_rounds = division_cfg$regular_season_rounds,
      is_cup = is_cup
    )
    format_facts$meetings_source <- division_format$source
```

(Assigning `NULL` leaves football's `format_facts` untouched.)

- [ ] **Step 4: Meta**

In `R/publish-format.R`, `.build_publish_meta()`: replace the final `c(base, list(n_rounds = ..., ...))` with:

```r
  # Where the meetings count behind n_rounds came from -- 2DT cells only, so
  # football's key order (hashed by the golden manifest) is untouched.
  meetings_source <- if (is.null(format$meetings_source)) {
    list()
  } else {
    list(n_rounds_meetings_source = as.character(format$meetings_source))
  }

  c(
    base,
    list(
      n_rounds        = n_rounds,
      n_rounds_source = as.character(format$source)
    ),
    meetings_source,
    list(
      units           = profile$units,
      points          = profile$points,
      season_scope    = profile$season_scope,
      postseason      = profile$postseason,
      qualify         = division_cfg$qualify,
      relegation      = list(slots = as.integer(relegation_slots))
    )
  )
```

Add to its roxygen `@param format`: "May carry `meetings_source` (2DT), published as `n_rounds_meetings_source`."

- [ ] **Step 5: Schema**

```bash
cd /Users/brynjolfurjonsson/sports && python3 - <<'PY'
import json
p = "config/publish-schemas/_base/meta.json"
s = open(p, encoding="utf-8").read()
anchor = 'a cup, which has no round count."\n    },\n'
assert s.count(anchor) == 1, s.count(anchor)
prop = anchor + '''    "n_rounds_meetings_source": {
      "enum": [
        "schedule",
        "config",
        "prior_results",
        "none",
        "not_applicable"
      ],
      "description": "Basketball and handball only (additive, 2026-09-16): where the meetings-per-pairing count behind n_rounds came from. `schedule` = the season's own complete fixture list; `config` = the division's expected_meetings; `prior_results` = the last completed season; `none` = no count, n_rounds fell back to the schedule; `not_applicable` = the division states regular_season_rounds instead. When set, n_rounds_source reads `config` whatever this says."
    },
'''
s = s.replace(anchor, prop)
json.loads(s)
open(p, "w", encoding="utf-8").write(s)
PY
LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 Rscript tools/gen-publish-schemas.R && git status --short config/publish-schemas
```

Expected: `_base/meta.json` plus the generated sport meta schemas change; nothing else.

- [ ] **Step 6: Run the publish suites**

`<stem>` = `publish|health`. Expected: PASS, with `test-publish-football-golden` unchanged. If a test pins the exact list of 2DT meta keys (`grep -rln "n_rounds_source" tests/testthat` finds the candidates), add `"n_rounds_meetings_source"` directly after `"n_rounds_source"` in its basketball/handball expectation — that is the intended contract addition — and leave football's expectation alone.

If the pre-season publish aborts inside a publisher section on zero played rows, guard that section on `nrow(bd_results) > 0L` the way the standings branch is guarded. Do not special-case the test.

- [ ] **Step 7: Commit**

```bash
cd /Users/brynjolfurjonsson/sports && git fetch -q && git branch --show-current
git add R/publish-profile.R R/publish-iceland-league.R R/publish-format.R config/publish-schemas tests/testthat/test-publish-2dt-preseason.R tests/testthat/test-publish-profile.R
git commit -F - <<'EOF'
feat(publish): 2DT cells publish the season their schedule says is current

The publisher resolves basketball and handball seasons and formats with
the extractor's helpers, so a cell between seasons publishes the new
season at round 0 with its full projected table, and women's Olisdeild
reads 18 rounds rather than 27. meta.json records where the meetings
count came from. Football is unchanged.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>
EOF
```

---

### Task 15: Stan comments give the right time unit (WS6, F18)

**Files:**
- Modify: `Stan/basketball_iceland/2d_student_t_scalarsigma.stan`, `Stan/handball_iceland/2d_student_t.stan` (comments only)

- [ ] **Step 1: Count, replace, recount**

```bash
cd /Users/brynjolfurjonsson/sports && grep -c "sqrt-week" Stan/basketball_iceland/2d_student_t_scalarsigma.stan Stan/handball_iceland/2d_student_t.stan
sed -i '' 's/sqrt-week/sqrt-day/g' Stan/basketball_iceland/2d_student_t_scalarsigma.stan Stan/handball_iceland/2d_student_t.stan
grep -c "sqrt-week" Stan/basketball_iceland/2d_student_t_scalarsigma.stan Stan/handball_iceland/2d_student_t.stan
git diff --stat Stan/
```

Expected: non-zero counts before, `0` after, and a diff touching only comment lines (`git diff Stan/ | grep '^[+-]' | grep -v '^[+-]\s*//' | grep -v '^+++\|^---'` prints nothing).

- [ ] **Step 2: Compile**

`<stem>` = `stan-compile|simulate-2dt-equivalence`. Expected: PASS, `SKIP 0`.

- [ ] **Step 3: Commit**

```bash
cd /Users/brynjolfurjonsson/sports && git fetch -q && git branch --show-current
git add Stan/basketball_iceland/2d_student_t_scalarsigma.stan Stan/handball_iceland/2d_student_t.stan
git commit -F - <<'EOF'
docs(stan): random-walk steps are per sqrt-day, not sqrt-week

time_between_matches is built in days (R/model-prepare.R). Comments
only; the models are unchanged, though the edit triggers one recompile.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>
EOF
```

---

### Task 16: metill-platform renders a pre-season cell and keeps the slider on one season

Repo: `/Users/brynjolfurjonsson/metill-platform`. Branch `fix/ithrottir-preseason` from `origin/main`. This must be **live before** Task 17 publishes 2027 data.

Why: `final_positions_history.json` is append-only, so after the rollover it holds 2026 rounds 1-22 beside 2027 round 0. `uniqueRoundsFromHistory()` lists every `as_of`, so the heatmap slider would offer last season's rounds as if they were this season's. Pre-season `standings.json` also has no rows. `_team_rank_map()` already falls back to the projection for that; the new test pins it.

**Files:**
- Modify: `app/static/js/finishing-heatmap.js` (`uniqueRoundsFromHistory`, `renderFinishingHeatmap`), `app/templates/ithrottir_league.html` (cache-buster `finishing-heatmap.js?v=7` → `?v=8`)
- Test: `tests/test_ithrottir_preseason.py` (create)

- [ ] **Step 1: Branch**

```bash
cd /Users/brynjolfurjonsson/metill-platform && git fetch -q && git switch -c fix/ithrottir-preseason origin/main
```

- [ ] **Step 2: Write the tests**

```python
"""A 2DT cell between seasons: an empty table beside a full projection.

Before a ball is played, sports publishes standings.json with no rows while
final_positions.json holds the whole projected season (sports spec 2026-09-16
§10). The page must render it and rank the heatmap from the projection.

Fixture cells are season 2100; rewriting them to 2101 keeps them above
korfubolti's min_season.
"""

import json
import shutil
from pathlib import Path

import pytest
from httpx import ASGITransport, AsyncClient

from app.main import app
from app.routes import ithrottir

FIXTURE_ROOT = Path(__file__).parent / "fixtures" / "ithrottir"


@pytest.fixture
def fixture_root(tmp_path, monkeypatch):
    shutil.copytree(FIXTURE_ROOT, tmp_path / "ithrottir")
    monkeypatch.setattr(ithrottir, "DATA_DIR", tmp_path)
    return tmp_path


@pytest.fixture
async def client():
    async with AsyncClient(transport=ASGITransport(app=app), base_url="http://test") as c:
        yield c


def _rewrite(path: Path, **changes) -> None:
    data = json.loads(path.read_text(encoding="utf-8"))
    data.update(changes)
    path.write_text(json.dumps(data, ensure_ascii=False), encoding="utf-8")
    # The parse cache is keyed on mtime, which can tie within one second.
    ithrottir._json_cache.pop(path, None)


def _preseason(sport: str, sex: str, div: str) -> Path:
    d = ithrottir._league_data_dir(sport, sex, div)
    _rewrite(d / "meta.json", season=2101, round=0)
    _rewrite(d / "standings.json", season=2101, rows=[])
    return d


def test_team_ranks_fall_back_to_the_projection(fixture_root):
    d = _preseason("korfubolti", "karla", "bonus")
    final_pos = json.loads((d / "final_positions.json").read_text(encoding="utf-8"))
    standings = json.loads((d / "standings.json").read_text(encoding="utf-8"))
    ranks = ithrottir._team_rank_map(standings, final_pos)
    assert ranks
    assert set(ranks) == {r["team"] for r in final_pos["summary"]}


async def test_preseason_page_renders(fixture_root, client):
    _preseason("korfubolti", "karla", "bonus")
    r = await client.get("/ithrottir/korfubolti/iceland/karla/bonus/")
    assert r.status_code == 200
    assert 'data-chart="ith-heatmap"' in r.text
    assert 'finishing-heatmap.js?v=8' in r.text
```

Run: `cd /Users/brynjolfurjonsson/metill-platform && uv run --extra dev pytest tests/test_ithrottir_preseason.py -v`
Expected: `test_team_ranks_fall_back_to_the_projection` PASSES (the fallback exists; the test pins it); `test_preseason_page_renders` FAILS on the `?v=8` assertion.

- [ ] **Step 3: Scope the slider to the snapshot's season**

In `app/static/js/finishing-heatmap.js`, replace `uniqueRoundsFromHistory`:

```js
// History files are append-only, so after a season rollover they still hold
// last season's rounds. Only the snapshot's season is offered on the slider;
// records without a season (older files) are kept.
function uniqueRoundsFromHistory(history, season = null) {
  const records = (history?.records ?? []).filter(
    r => season == null || r.season == null || r.season === season,
  );
  const seen = new Map();  // as_of → round
  for (const r of records) seen.set(r.as_of, r.round);
  return [...seen.entries()]
    .map(([asOf, round]) => ({ asOf, round }))
    .sort((a, b) => a.asOf < b.asOf ? -1 : a.asOf > b.asOf ? 1 : 0);
}
```

and in `renderFinishingHeatmap`:

```js
  const rounds = uniqueRoundsFromHistory(history, snapshot?.season ?? null);
```

Add to the file's header comment, after "History semantics: ...": ` * Only the snapshot's season is offered: history files keep earlier seasons.`

In `app/templates/ithrottir_league.html`, change `/static/js/finishing-heatmap.js?v=7` to `?v=8`.

- [ ] **Step 4: Run tests and lint**

```bash
cd /Users/brynjolfurjonsson/metill-platform && uv run --extra dev pytest tests/ -q && uv run --extra dev ruff check .
```

Expected: all pass, ruff clean. Record the pass count and time.

- [ ] **Step 5: Check the slider in the browser**

Make a two-season history on the local handball men's Olísdeild data (tracked files; restored in Step 6):

```bash
cd /Users/brynjolfurjonsson/metill-platform && uv run python - <<'PY'
import json
from pathlib import Path
d = Path("data/ithrottir/handball/iceland/karla-od")
fp = json.loads((d / "final_positions.json").read_text(encoding="utf-8"))
hist = json.loads((d / "final_positions_history.json").read_text(encoding="utf-8"))
latest = max(r["as_of"] for r in hist["records"])
nxt = fp["season"] + 1
clone = [dict(r, as_of="2099-12-31", round=0, season=nxt)
         for r in hist["records"] if r["as_of"] == latest]
hist["records"] += clone
fp["season"] = nxt
(d / "final_positions_history.json").write_text(json.dumps(hist, ensure_ascii=False), encoding="utf-8")
(d / "final_positions.json").write_text(json.dumps(fp, ensure_ascii=False), encoding="utf-8")
print("history seasons:", sorted({r.get("season") for r in hist["records"]}))
PY
```

Start the `metill-dev` preview, open `http://localhost:8000/ithrottir/handbolti/iceland/karla/olis/`, and check with `javascript_tool`:

```js
[document.getElementById("heatmap-history-controls").hidden,
 document.querySelectorAll("#heatmap-body *").length > 0]
```

Expected: `[true, true]` — one round in the new season, so no slider, and the grid drawn. With the Step 3 change reverted (`git stash`), the same check gives `[false, true]`: the old rounds were on offer. Take one screenshot of the heatmap panel for the PR.

- [ ] **Step 6: Restore the data, commit, open the PR**

```bash
cd /Users/brynjolfurjonsson/metill-platform && git checkout -- data/ithrottir && git status --short
git fetch -q && git branch --show-current
git add app/static/js/finishing-heatmap.js app/templates/ithrottir_league.html tests/test_ithrottir_preseason.py
git commit -F - <<'EOF'
fix(ithrottir): keep the heatmap slider on the current season

History files are append-only, so after a season rollover the slider
would have offered last season's rounds beside this season's round 0.
Also pins that a pre-season cell (empty standings, full projection)
renders and ranks its heatmap from the projection.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>
EOF
git pull --rebase origin main && git log --oneline origin/main..HEAD
git push -u origin fix/ithrottir-preseason
gh pr create --title "fix(ithrottir): keep the heatmap slider on the current season" --body-file - <<'EOF'
Needed before sports publishes the 2027 basketball and handball projections (sports spec 2026-09-16).

- `final_positions_history.json` is append-only, so after the rollover it holds 2026 rounds next to 2027's round 0. The slider now lists only the snapshot's season.
- New `tests/test_ithrottir_preseason.py` pins that a pre-season cell (empty `standings.rows`, full `final_positions`) renders and ranks its heatmap from the projection.
- Cache-buster `finishing-heatmap.js?v=8`.

Checked in the browser against a two-season history: slider hidden, grid drawn (with the fix reverted, the slider offered the old rounds).

🤖 Generated with [Claude Code](https://claude.com/claude-code)
EOF
```

Then ask the user for an explicit OK to merge. After the merge, confirm the Fly deploy ran for the merge commit (`gh run list --workflow=deploy.yml -L 3`) before dispatching one by hand.

---

### Task 17: Ship, refit, publish, verify (WS7, §12)

- [ ] **Step 1: Full sports suite**

```bash
cd /Users/brynjolfurjonsson/sports && date -u +%FT%TZ && NOT_CRAN=true LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 Rscript -e 'devtools::test()' 2>&1 | tail -15
```

Expected: `FAIL 0`. Record the pass/skip counts with the timestamp. Any SKIP in `simulate-2dt-equivalence` or `stan-compile` means CmdStan was not found, which is not a pass.

- [ ] **Step 2: Clean tree, rebased on origin/main**

```bash
cd /Users/brynjolfurjonsson/sports && LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 Rscript -e 'devtools::document()' && git status --short
git fetch -q && git rebase origin/main && git log --oneline origin/main..HEAD
```

Expected: no uncommitted changes. The log lists only this branch's commits (spec, plan, Tasks 1-15), and **not** `4afe054f5`. If the rebase pulled in new data commits, re-run Step 1's `publish|extract` subset.

- [ ] **Step 3: Push and open the sports PR**

```bash
cd /Users/brynjolfurjonsson/sports && git branch --show-current && git push -u origin feat/2dt-full-season-projection
gh pr create --title "2DT sports: a real full-season projection, including pre-season" --body-file - <<'EOF'
Implements docs/superpowers/specs/2026-09-16-2dt-full-season-projection-design.md (plan: docs/superpowers/plans/2026-09-16-2dt-full-season-projection.md).

- simulate_league_season() takes an injected match model, points rule and tie-break; football's digests are unchanged.
- R generators replay the basketball and handball Stan models (pinned against a real fit).
- 2DT seasons resolve from the schedule too; remaining fixtures are derived structurally; unrated teams get a below-average prior; women's Olísdeild reads 18 rounds.
- needs_refit() refits when the horizon holds only fixtures the newest fit never predicted.
- Basketball women's 1. deild is held on 2026 (preseason_hold).
- An empty league table no longer erases standings_history.json.

Departures from the spec are listed at the top of the plan. Visible change: handball's "Líkur á sæti" becomes a whole-season projection and will move noticeably.

Test run: <Step 1 counts and timestamp>

🤖 Generated with [Claude Code](https://claude.com/claude-code)
EOF
```

Wait for CI (`gh pr checks --watch`). Then ask the user for an explicit OK to merge. Do not merge before metill-platform Task 16 is live.

- [ ] **Step 4: Refit basketball, then handball**

After the merge:

```bash
gh workflow run fit.yml -R metill-is/sports -f force=true -f league=basketball_iceland
sleep 20 && gh run list -R metill-is/sports --workflow=fit.yml -L 1
```

Watch the run (`gh run watch <id> -R metill-is/sports`). When it succeeds, confirm `decide-publish.yml` ran after it and succeeded (`gh run list -R metill-is/sports --workflow=decide-publish.yml -L 2`), then dispatch `-f league=handball_iceland` and watch it the same way. A failed run: read `gh run view <id> --log-failed` before retrying.

- [ ] **Step 5: Check the published data on origin/main**

```bash
cd /Users/brynjolfurjonsson/sports && git fetch -q && python3 - <<'PY'
import json, subprocess
def show(path):
    return json.loads(subprocess.check_output(["git", "show", f"origin/main:data/publish/{path}"]))
for cell in ["basketball/iceland/karla-bd", "basketball/iceland/karla-1d",
             "basketball/iceland/kvenna-bd", "basketball/iceland/kvenna-1d",
             "handball/iceland/karla-od", "handball/iceland/karla-g66",
             "handball/iceland/kvenna-od", "handball/iceland/kvenna-g66"]:
    m = show(f"{cell}/meta.json")
    fp = show(f"{cell}/final_positions.json")
    hist = show(f"{cell}/standings_history.json")
    print(cell, m["season"], "round", m["round"], "/", m["n_rounds"],
          m.get("n_rounds_meetings_source"), "teams", fp["n_teams"],
          "history", len(hist["records"]), "fit", m["fit_date"])
PY
```

Expected, on or after 2026-09-2x:
- basketball karla-bd, karla-1d, kvenna-bd: season 2027, round 0, n_rounds 22 / 22 / 18, 12 / 12 / 10 teams, today's fit date.
- basketball kvenna-1d: season 2026 (held).
- handball kvenna-od: season 2027, n_rounds 18, meetings source `schedule`.
- Every cell's `standings_history` still has records.
Record the output with a timestamp.

- [ ] **Step 6: Platform pull and deploy**

```bash
gh workflow run pull-sports-data.yml -R metill-is/metill-platform
sleep 20 && gh run list -R metill-is/metill-platform --workflow=pull-sports-data.yml -L 1
```

Watch it, then confirm a deploy ran for the resulting data commit (`gh run list -R metill-is/metill-platform --workflow=deploy.yml -L 2`, headSha equal to the pull commit). Dispatch `deploy.yml` only if none did.

- [ ] **Step 7: Verify the live site**

With the Browser tools, on https://metill.is:
- The nav dropdown shows Körfubolti (the #58 gate lifted).
- `/ithrottir/korfubolti/iceland/karla/bonus/` and `/ithrottir/korfubolti/iceland/kvenna/bonus/` render, are indexable (no `noindex` meta), show a 12-team / 10-team heatmap with plausible spreads (no team above ~60 % for first place pre-season), and hide the heatmap slider.
- The women's 1. deild tab is disabled.
- `/ithrottir/handbolti/iceland/kvenna/olis/` reads 18 rounds.
- `read_console_messages` shows no errors on those pages.
One screenshot per sport for the summary.

- [ ] **Step 8: Record**

- Metill vault: append to `log.md` (`## [2026-09-DD] ship | 2DT full-season projection`), update `Sports/Knowledge/Sports Models/next-actions.md` (drop the shipped items; add: forward drift for all sports; football tie bias; women's 1D aliases and rounds, then remove `preseason_hold`; revisit the §8 trigger if refits lag).
- Memory: a `project_2dt_full_season_projection.md` note with both merge SHAs, plus its `MEMORY.md` line.
- Update the vault `Handoff.md`.
