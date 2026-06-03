# Unattended Auto-Placement Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A scheduled, local-only wrapper that places +EV bets unattended with zero operator effort, gating on a zero-Lengjan local preview so authenticated sessions only happen when there is something to place, with a true daily cap, kill switch, lock, and health-surfaced failures.

**Architecture:** New pure helpers in `R/auto-place.R` (kill switch, lock, daily-room, status store, decision planner, injectable orchestrator) compose into `scripts/auto_place.R`, a thin shell a launchd agent fires on a jittered daytime schedule. A new `check_placement_health()` in `R/health.R` surfaces unattended failures through the existing health layer. The placer's existing `sample_delay()` pacing and P1-P4 rules are reused unchanged.

**Tech Stack:** R (devtools package), testthat 3, `fs`, `jsonlite`, `arrow` (via existing `read_table`), Chromote (existing placer), macOS launchd.

**Design source:** [`docs/superpowers/specs/2026-06-01-unattended-auto-placement-design.md`](../specs/2026-06-01-unattended-auto-placement-design.md)

---

## File Structure

- `R/auto-place.R` (create) — all pure/injectable helpers: kill switch, lock, daily-room, status store, decision planner, `run_auto_place()` orchestrator.
- `R/health.R` (modify) — add `check_placement_health()` + thresholds + compose into `pipeline_health()`.
- `scripts/auto_place.R` (create) — thin CLI shell: jitter + daytime guard + `sync_recs()` + `run_auto_place()`.
- `tools/launchd/is.metill.sports.autoplace.plist.template` (create) — launchd agent template.
- `tools/install-autoplace.sh` (create) — render + bootstrap/bootout the agent.
- `tests/testthat/test-auto-place.R` (create) — unit tests for the helpers + orchestrator.
- `tests/testthat/test-health.R` (modify) — tests for `check_placement_health()`.
- `tests/testthat/test-placer-ci-isolation.R` (modify) — forbid auto-place tokens in CI.
- `.gitignore` (modify) — ignore the lock + sentinel runtime files.
- `.claude/rules/sports-betting.md`, `CLAUDE.md`, `docs/runbooks/auto-place.md` (modify/create) — docs.

**Conventions:** base pipe `|>`, explicit `pkg::fn()` namespacing in package code, roxygen on exports, `here::here()` for paths, testthat edition 3. Run `devtools::document()` after adding exports.

---

### Task 1: Lock down CI-isolation first (safety before any code exists)

**Files:**
- Modify: `tests/testthat/test-placer-ci-isolation.R:18-25`

- [ ] **Step 1: Extend the forbidden-token list**

In `tests/testthat/test-placer-ci-isolation.R`, replace the `forbidden` vector (lines 18-25) with:

```r
  forbidden <- c(
    "R/placer-",
    "placer_pipeline",
    "place_bets",
    "preview_bets",
    "auto_place",
    "autoplace",
    "AUTO_PLACE",
    "run_auto_place",
    "LENGJAN_USER",
    "LENGJAN_PASS"
  )
```

- [ ] **Step 2: Run the test, expect PASS (no workflow references these yet)**

Run: `Rscript -e 'suppressMessages(devtools::load_all(quiet=TRUE)); testthat::test_file("tests/testthat/test-placer-ci-isolation.R")'`
Expected: `[ FAIL 0 | ... | PASS 1 ]` — the invariant now also guards auto-place, before the code exists.

- [ ] **Step 3: Commit**

```bash
git add tests/testthat/test-placer-ci-isolation.R
git commit -m "test(ci-isolation): forbid auto-place tokens in workflows before building them"
```

---

### Task 2: Kill-switch helper

**Files:**
- Create: `R/auto-place.R`
- Test: `tests/testthat/test-auto-place.R`

- [ ] **Step 1: Write the failing test**

Create `tests/testthat/test-auto-place.R`:

```r
test_that("placement_kill_switched reflects the sentinel file", {
  root <- withr::local_tempdir()
  expect_false(placement_kill_switched(root))
  fs::file_create(fs::path(root, "AUTO_PLACE_DISABLED"))
  expect_true(placement_kill_switched(root))
})
```

- [ ] **Step 2: Run it, verify it fails**

Run: `Rscript -e 'suppressMessages(devtools::load_all(quiet=TRUE)); testthat::test_file("tests/testthat/test-auto-place.R")'`
Expected: FAIL — `could not find function "placement_kill_switched"`.

- [ ] **Step 3: Create `R/auto-place.R` with the helper**

```r
#' @include config.R storage.R placer-preview.R placer-load.R
NULL

#' Is unattended placement disabled by the operator kill switch?
#'
#' @param root Data root.
#' @return `TRUE` if `data/AUTO_PLACE_DISABLED` exists.
#' @export
placement_kill_switched <- function(root = here::here("data")) {
  fs::file_exists(fs::path(root, "AUTO_PLACE_DISABLED"))
}
```

- [ ] **Step 4: Run the test, verify it passes**

Run: `Rscript -e 'suppressMessages(devtools::load_all(quiet=TRUE)); testthat::test_file("tests/testthat/test-auto-place.R")'`
Expected: `[ FAIL 0 | ... | PASS 2 ]`.

- [ ] **Step 5: Commit**

```bash
git add R/auto-place.R tests/testthat/test-auto-place.R
git commit -m "feat(auto-place): kill-switch sentinel check"
```

---

### Task 3: Lock helpers (no overlapping sessions)

**Files:**
- Modify: `R/auto-place.R`
- Test: `tests/testthat/test-auto-place.R`

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-auto-place.R`:

```r
test_that("acquire_auto_place_lock blocks a live lock and reclaims a dead one", {
  root <- withr::local_tempdir()
  expect_true(acquire_auto_place_lock(root))            # fresh acquire
  expect_true(fs::file_exists(fs::path(root, ".auto_place.lock")))

  expect_false(acquire_auto_place_lock(root))           # held by this (live) PID

  writeLines("999999", fs::path(root, ".auto_place.lock"))  # simulate dead holder
  expect_true(acquire_auto_place_lock(root))            # stale lock reclaimed

  release_auto_place_lock(root)
  expect_false(fs::file_exists(fs::path(root, ".auto_place.lock")))
})
```

- [ ] **Step 2: Run it, verify it fails**

Run: `Rscript -e 'suppressMessages(devtools::load_all(quiet=TRUE)); testthat::test_file("tests/testthat/test-auto-place.R")'`
Expected: FAIL — `could not find function "acquire_auto_place_lock"`.

- [ ] **Step 3: Add the lock helpers to `R/auto-place.R`**

```r
#' @noRd
.auto_place_lock_path <- function(root) fs::path(root, ".auto_place.lock")

#' @noRd
.pid_alive <- function(pid) {
  if (is.na(pid)) return(FALSE)
  isTRUE(tryCatch(
    system2("kill", c("-0", as.character(pid)), stdout = FALSE, stderr = FALSE) == 0L,
    error = function(e) FALSE
  ))
}

#' Acquire the unattended-placement lock.
#'
#' Writes the current PID to `data/.auto_place.lock`. Returns `FALSE` if a
#' lock is already held by a live process; reclaims a lock whose PID is dead.
#' @param root Data root.
#' @return `TRUE` if the lock was acquired.
#' @export
acquire_auto_place_lock <- function(root = here::here("data")) {
  lock <- .auto_place_lock_path(root)
  if (fs::file_exists(lock)) {
    pid <- suppressWarnings(as.integer(readLines(lock, n = 1L, warn = FALSE)[1L]))
    if (.pid_alive(pid)) return(FALSE)
  }
  fs::dir_create(fs::path_dir(lock))
  writeLines(as.character(Sys.getpid()), lock)
  TRUE
}

#' Release the unattended-placement lock.
#' @param root Data root.
#' @return `TRUE`, invisibly.
#' @export
release_auto_place_lock <- function(root = here::here("data")) {
  lock <- .auto_place_lock_path(root)
  if (fs::file_exists(lock)) fs::file_delete(lock)
  invisible(TRUE)
}
```

- [ ] **Step 4: Run the tests, verify they pass**

Run: `Rscript -e 'suppressMessages(devtools::load_all(quiet=TRUE)); testthat::test_file("tests/testthat/test-auto-place.R")'`
Expected: all PASS.

- [ ] **Step 5: Commit**

```bash
git add R/auto-place.R tests/testthat/test-auto-place.R
git commit -m "feat(auto-place): PID-aware lock with stale reclaim"
```

---

### Task 4: Daily-room cap (pure core + reader)

**Files:**
- Modify: `R/auto-place.R`
- Test: `tests/testthat/test-auto-place.R`

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-auto-place.R`:

```r
test_that(".daily_room never goes negative", {
  expect_equal(.daily_room(daily_budget = 5000, placed_today = 2000), 3000)
  expect_equal(.daily_room(daily_budget = 5000, placed_today = 9000), 0)
})

test_that(".placed_today_stake sums only today's placed stakes", {
  now <- as.POSIXct("2026-06-01 12:00:00", tz = "UTC")
  led <- tibble::tibble(
    placed_at  = as.POSIXct(c("2026-06-01 09:00:00", "2026-05-31 20:00:00"), tz = "UTC"),
    bet_amount = c(1500, 4000)
  )
  expect_equal(.placed_today_stake(led, now), 1500)
  expect_equal(.placed_today_stake(tibble::tibble(), now), 0)
})
```

- [ ] **Step 2: Run, verify failure**

Run: `Rscript -e 'suppressMessages(devtools::load_all(quiet=TRUE)); testthat::test_file("tests/testthat/test-auto-place.R")'`
Expected: FAIL — `could not find function ".daily_room"`.

- [ ] **Step 3: Add the daily-room helpers to `R/auto-place.R`**

```r
#' @noRd
.daily_room <- function(daily_budget, placed_today) {
  max(0, daily_budget - placed_today)
}

#' @noRd
.placed_today_stake <- function(ledger_df, now) {
  if (nrow(ledger_df) == 0L ||
    !all(c("placed_at", "bet_amount") %in% names(ledger_df))) {
    return(0)
  }
  today <- as.Date(now, tz = "UTC")
  sum(ledger_df$bet_amount[as.Date(ledger_df$placed_at, tz = "UTC") == today],
    na.rm = TRUE)
}

#' Remaining ISK that may still be staked today under the daily budget.
#'
#' `daily_budget = max(daily_budget_frac * current_pool, daily_budget_min_isk)`,
#' minus stakes already placed today (by `placed_at`). The wrapper-level
#' backstop for the cross-session daily cap (design Risk 3).
#' @param root Data root.
#' @param bankroll A `load_bankroll()` list.
#' @param now Current time.
#' @return Non-negative ISK room.
#' @export
auto_place_daily_room <- function(root = here::here("data"),
                                  bankroll = load_bankroll(ledger_root = root),
                                  now = Sys.time()) {
  daily_budget <- max(
    bankroll$daily_budget_frac * bankroll$current_pool,
    bankroll$daily_budget_min_isk
  )
  led <- tryCatch(read_table("ledger", root = root),
    error = function(e) tibble::tibble())
  .daily_room(daily_budget, .placed_today_stake(led, now))
}
```

- [ ] **Step 4: Run, verify pass**

Run: `Rscript -e 'suppressMessages(devtools::load_all(quiet=TRUE)); testthat::test_file("tests/testthat/test-auto-place.R")'`
Expected: all PASS.

- [ ] **Step 5: Commit**

```bash
git add R/auto-place.R tests/testthat/test-auto-place.R
git commit -m "feat(auto-place): cross-session daily-budget room helper"
```

---

### Task 5: Placement-status store

**Files:**
- Modify: `R/auto-place.R`
- Test: `tests/testthat/test-auto-place.R`

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-auto-place.R`:

```r
test_that("placement status round-trips and missing reads as NULL", {
  root <- withr::local_tempdir()
  expect_null(read_placement_status(root))

  record_placement_status("placed", n_pending = 3L, n_placed = 2L,
    run_at = as.POSIXct("2026-06-01 12:00:00", tz = "UTC"), root = root)
  got <- read_placement_status(root)
  expect_equal(got$status, "placed")
  expect_equal(got$n_placed, 2L)
  expect_match(got$run_at, "^2026-06-01T12:00")
})
```

- [ ] **Step 2: Run, verify failure**

Run: `Rscript -e 'suppressMessages(devtools::load_all(quiet=TRUE)); testthat::test_file("tests/testthat/test-auto-place.R")'`
Expected: FAIL — `could not find function "read_placement_status"`.

- [ ] **Step 3: Add the status store to `R/auto-place.R`**

```r
#' @noRd
placement_status_path <- function(root = here::here("data")) {
  fs::path(root, "health", "placement_status.json")
}

#' Record the outcome of one unattended placement run.
#'
#' Statuses: `placed`, `nothing_pending`, `ev_rejected`, `disabled`, `locked`,
#' `sync_failed`, `daily_cap_reached`, or `failed:<reason>`.
#' @param status,n_pending,n_placed,error,run_at,root Run fields.
#' @return The record list, invisibly.
#' @export
record_placement_status <- function(status,
                                     n_pending = NA_integer_,
                                     n_placed = NA_integer_,
                                     error = NA_character_,
                                     run_at = Sys.time(),
                                     root = here::here("data")) {
  path <- placement_status_path(root)
  fs::dir_create(fs::path_dir(path))
  rec <- list(
    run_at = format(run_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    status = status,
    n_pending = n_pending,
    n_placed = n_placed,
    error = error
  )
  jsonlite::write_json(rec, path, auto_unbox = TRUE, pretty = TRUE)
  invisible(rec)
}

#' Read the last placement-run status (`NULL` if none).
#' @param root Data root.
#' @return A list, or `NULL`.
#' @export
read_placement_status <- function(root = here::here("data")) {
  path <- placement_status_path(root)
  if (!fs::file_exists(path)) return(NULL)
  jsonlite::read_json(path, simplifyVector = TRUE)
}
```

- [ ] **Step 4: Run, verify pass**

Run: `Rscript -e 'suppressMessages(devtools::load_all(quiet=TRUE)); testthat::test_file("tests/testthat/test-auto-place.R")'`
Expected: all PASS.

- [ ] **Step 5: Commit**

```bash
git add R/auto-place.R tests/testthat/test-auto-place.R
git commit -m "feat(auto-place): committed placement-status store"
```

---

### Task 6: Decision planner (pure)

**Files:**
- Modify: `R/auto-place.R`
- Test: `tests/testthat/test-auto-place.R`

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-auto-place.R`:

```r
test_that("auto_place_decide resolves the action precedence", {
  base <- list(kill = FALSE, locked = FALSE, sync = TRUE, pending = 3L, room = 5000)
  d <- function(o = list()) {
    a <- utils::modifyList(base, o)
    auto_place_decide(a$kill, a$locked, a$sync, a$pending, a$room)
  }
  expect_equal(d(list(kill = TRUE)), "disabled")
  expect_equal(d(list(locked = TRUE)), "locked")
  expect_equal(d(list(sync = FALSE)), "sync_failed")
  expect_equal(d(list(pending = 0L)), "nothing_pending")
  expect_equal(d(list(room = 0)), "daily_cap_reached")
  expect_equal(d(), "place")
})
```

- [ ] **Step 2: Run, verify failure**

Run: `Rscript -e 'suppressMessages(devtools::load_all(quiet=TRUE)); testthat::test_file("tests/testthat/test-auto-place.R")'`
Expected: FAIL — `could not find function "auto_place_decide"`.

- [ ] **Step 3: Add the planner to `R/auto-place.R`**

```r
#' Decide the next unattended-placement action from gathered signals.
#'
#' Precedence mirrors the wrapper sequence: kill switch, lock, sync, gate,
#' daily cap, then place.
#' @param kill_switched,locked,sync_ok,pending_n,daily_room Gathered signals.
#' @return One of `disabled`, `locked`, `sync_failed`, `nothing_pending`,
#'   `daily_cap_reached`, `place`.
#' @export
auto_place_decide <- function(kill_switched, locked, sync_ok, pending_n, daily_room) {
  if (isTRUE(kill_switched)) return("disabled")
  if (isTRUE(locked)) return("locked")
  if (!isTRUE(sync_ok)) return("sync_failed")
  if (pending_n == 0L) return("nothing_pending")
  if (daily_room <= 0) return("daily_cap_reached")
  "place"
}
```

- [ ] **Step 4: Run, verify pass**

Run: `Rscript -e 'suppressMessages(devtools::load_all(quiet=TRUE)); testthat::test_file("tests/testthat/test-auto-place.R")'`
Expected: all PASS.

- [ ] **Step 5: Commit**

```bash
git add R/auto-place.R tests/testthat/test-auto-place.R
git commit -m "feat(auto-place): pure decision planner"
```

---

### Task 7: Orchestrator with injectable sync/place

**Files:**
- Modify: `R/auto-place.R`
- Test: `tests/testthat/test-auto-place.R`

The orchestrator is the testable heart: it manages lock + gate + decision + status, with `sync_fn` and `place_fn` injected so tests use fakes (no git, no browser, no money).

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-auto-place.R`. Helper writes a one-row pending recommendation the gate will see:

```r
seed_pending_rec <- function(root) {
  recs <- tibble::tibble(
    run_id = "t", sex = "male",
    match_date = as.Date("2026-06-02"),
    home_team = "A", away_team = "B",
    market = "moneyline", outcome = "home", line = NA_real_,
    p = 0.6, odds = 2.1, ev = 0.26, kelly = 0.02, bet_amount = 1500,
    sport = "football", country = "iceland", run_date = as.Date("2026-06-01")
  )
  write_table(recs, "recommendations", root = root)
}

test_that("run_auto_place records 'placed' when a pending bet is placed", {
  root <- withr::local_tempdir()
  seed_pending_rec(root)
  fake_place <- function(...) tibble::tibble(status = "placed")
  run_auto_place(
    root = root, now = as.POSIXct("2026-06-01 12:00:00", tz = "UTC"),
    sync_fn = function(...) TRUE, place_fn = fake_place,
    bankroll_fn = function() list(daily_budget_frac = 0.05, current_pool = 1e5,
      daily_budget_min_isk = 1000)
  )
  expect_equal(read_placement_status(root)$status, "placed")
})

test_that("run_auto_place short-circuits on the kill switch", {
  root <- withr::local_tempdir()
  seed_pending_rec(root)
  fs::file_create(fs::path(root, "AUTO_PLACE_DISABLED"))
  called <- FALSE
  run_auto_place(root = root, sync_fn = function(...) TRUE,
    place_fn = function(...) { called <<- TRUE; tibble::tibble(status = "placed") },
    bankroll_fn = function() list(daily_budget_frac = 0.05, current_pool = 1e5,
      daily_budget_min_isk = 1000))
  expect_false(called)
  expect_equal(read_placement_status(root)$status, "disabled")
})

test_that("run_auto_place records 'nothing_pending' with no recs", {
  root <- withr::local_tempdir()
  run_auto_place(root = root, sync_fn = function(...) TRUE,
    place_fn = function(...) stop("should not be called"),
    bankroll_fn = function() list(daily_budget_frac = 0.05, current_pool = 1e5,
      daily_budget_min_isk = 1000))
  expect_equal(read_placement_status(root)$status, "nothing_pending")
})
```

- [ ] **Step 2: Run, verify failure**

Run: `Rscript -e 'suppressMessages(devtools::load_all(quiet=TRUE)); testthat::test_file("tests/testthat/test-auto-place.R")'`
Expected: FAIL — `could not find function "run_auto_place"`.

- [ ] **Step 3: Add the orchestrator to `R/auto-place.R`**

```r
#' Run one unattended placement cycle (kill -> lock -> sync -> gate ->
#' daily-cap -> place), recording the outcome to the status store.
#'
#' `sync_fn`/`place_fn`/`bankroll_fn` are injected for testability; the shell
#' `scripts/auto_place.R` passes the real `sync_recs`, `place_bets`,
#' `load_bankroll`.
#' @param root Data root.
#' @param now Current time.
#' @param sync_fn `function(repo_root) -> logical` (TRUE on clean sync).
#' @param place_fn `place_bets`-compatible function returning a status tibble.
#' @param bankroll_fn `function() -> load_bankroll()` list.
#' @return The recorded status list, invisibly.
#' @export
run_auto_place <- function(root = here::here("data"),
                           now = Sys.time(),
                           sync_fn = sync_recs,
                           place_fn = place_bets,
                           bankroll_fn = function() load_bankroll(ledger_root = root)) {
  if (placement_kill_switched(root)) {
    return(record_placement_status("disabled", run_at = now, root = root))
  }
  if (!acquire_auto_place_lock(root)) {
    return(record_placement_status("locked", run_at = now, root = root))
  }
  on.exit(release_auto_place_lock(root), add = TRUE)

  sync_ok <- isTRUE(tryCatch(sync_fn(here::here()), error = function(e) FALSE))
  pending <- tryCatch(suppressMessages(preview_pending(root = root)),
    error = function(e) tibble::tibble())
  n_pending <- nrow(pending)
  room <- tryCatch(auto_place_daily_room(root, bankroll_fn(), now),
    error = function(e) 0)

  action <- auto_place_decide(FALSE, FALSE, sync_ok, n_pending, room)
  if (action != "place") {
    return(record_placement_status(action, n_pending = n_pending,
      run_at = now, root = root))
  }

  res <- tryCatch(
    place_fn(dry_run = FALSE, interactive = FALSE, headless = FALSE, root = root),
    error = function(e) {
      record_placement_status(paste0("failed:", conditionMessage(e)),
        n_pending = n_pending, run_at = now, root = root)
      stop(e)
    }
  )
  n_placed <- if ("status" %in% names(res)) {
    sum(res$status == "placed", na.rm = TRUE)
  } else {
    0L
  }
  record_placement_status(
    if (n_placed > 0L) "placed" else "ev_rejected",
    n_pending = n_pending, n_placed = n_placed, run_at = now, root = root
  )
}
```

- [ ] **Step 4: Run, verify pass**

Run: `Rscript -e 'suppressMessages(devtools::load_all(quiet=TRUE)); testthat::test_file("tests/testthat/test-auto-place.R")'`
Expected: all PASS.

- [ ] **Step 5: `devtools::document()` then commit**

Run: `Rscript -e 'devtools::document()'`

```bash
git add R/auto-place.R tests/testthat/test-auto-place.R NAMESPACE man/
git commit -m "feat(auto-place): injectable orchestrator (kill/lock/sync/gate/cap/place)"
```

---

### Task 8: `check_placement_health()` + compose into `pipeline_health()`

**Files:**
- Modify: `R/health.R:9-24` (thresholds), after `check_bankroll` (~`R/health.R:290`), and `R/health.R:304-311` (compose)
- Test: `tests/testthat/test-health.R`

Keys on **operational** health, not residual pending (a forever-EV-rejecting rec must NOT alarm — design §3).

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-health.R`:

```r
test_that("check_placement_health is OK when nothing is pending", {
  root <- withr::local_tempdir()
  th <- health_thresholds()
  row <- check_placement_health(root, Sys.time(), th)
  expect_equal(row$status, "OK")
})

test_that("check_placement_health FAILs on a failed last run with pending bets", {
  root <- withr::local_tempdir()
  th <- health_thresholds()
  recs <- tibble::tibble(
    run_id = "t", sex = "male", match_date = as.Date("2100-01-01"),
    home_team = "A", away_team = "B", market = "moneyline", outcome = "home",
    line = NA_real_, p = 0.6, odds = 2.1, ev = 0.26, kelly = 0.02,
    bet_amount = 1500, sport = "football", country = "iceland",
    run_date = as.Date("2026-06-01")
  )
  write_table(recs, "recommendations", root = root)
  record_placement_status("failed:boom", n_pending = 1L,
    run_at = Sys.time(), root = root)
  row <- check_placement_health(root, Sys.time(), th)
  expect_equal(row$status, "FAIL")
})

test_that("check_placement_health is OK after a healthy run cleared the queue", {
  root <- withr::local_tempdir()
  th <- health_thresholds()
  record_placement_status("placed", n_placed = 2L, run_at = Sys.time(), root = root)
  row <- check_placement_health(root, Sys.time(), th)
  expect_equal(row$status, "OK")
})
```

- [ ] **Step 2: Run, verify failure**

Run: `Rscript -e 'suppressMessages(devtools::load_all(quiet=TRUE)); testthat::test_file("tests/testthat/test-health.R")'`
Expected: FAIL — `could not find function "check_placement_health"`.

- [ ] **Step 3a: Add thresholds**

In `R/health.R`, inside `health_thresholds()` (before the closing `)` at line ~23, after the `capture_fail_rate` line — add a comma to the previous line and append:

```r
    capture_fail_rate = 0.3, # below this -> FAIL (near-total placement collapse)
    placement_stale_warn_hours = 6, # pending bets + last healthy run older -> WARN
    placement_stale_fail_hours = 14 # ...older still -> FAIL
```

- [ ] **Step 3b: Add the check function** (place it immediately after `check_bankroll()` ends, ~`R/health.R:290`)

```r
#' @noRd
check_placement_health <- function(root, now, th) {
  healthy <- c("placed", "nothing_pending", "ev_rejected", "daily_cap_reached")
  thr_lbl <- paste0("healthy run < ", th$placement_stale_fail_hours, "h when pending")

  pending <- tryCatch({
    recs <- load_recommendations(root = root)
    if (nrow(recs) == 0L) {
      recs
    } else {
      recs <- recs[as.Date(recs$match_date) >= as.Date(now, tz = "UTC"), , drop = FALSE]
      dedup_against_ledger(recs, root = root)
    }
  }, error = function(e) tibble::tibble())
  n_pending <- nrow(pending)

  last <- read_placement_status(root)

  if (n_pending == 0L) {
    return(health_row("placement_health", "auto_place", "OK",
      "no pending bets", thr_lbl))
  }
  if (is.null(last)) {
    return(health_row("placement_health", "auto_place", "WARN",
      sprintf("%d pending, auto-place never run", n_pending), thr_lbl))
  }

  failed <- !(last$status %in% healthy)
  age_h <- as.numeric(difftime(now,
    as.POSIXct(last$run_at, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
    units = "hours"))

  status <- if (failed || age_h > th$placement_stale_fail_hours) {
    "FAIL"
  } else if (age_h > th$placement_stale_warn_hours) {
    "WARN"
  } else {
    "OK"
  }
  health_row("placement_health", "auto_place", status,
    sprintf("%d pending, last=%s (%.0fh)", n_pending, last$status, age_h), thr_lbl)
}
```

- [ ] **Step 3c: Compose into `pipeline_health()`** — in the `dplyr::bind_rows(...)` block (`R/health.R:304-311`), add a line after `safe(check_capture_rate(root, now, th)),`:

```r
    safe(check_placement_health(root, now, th)),
```

- [ ] **Step 4: Run, verify pass**

Run: `Rscript -e 'suppressMessages(devtools::load_all(quiet=TRUE)); testthat::test_file("tests/testthat/test-health.R")'`
Expected: all PASS.

- [ ] **Step 5: Commit**

```bash
git add R/health.R tests/testthat/test-health.R
git commit -m "feat(health): placement_health check keyed on operational failure"
```

---

### Task 9: `sync_recs()` + `scripts/auto_place.R` shell

**Files:**
- Modify: `R/auto-place.R` (add `sync_recs`)
- Create: `scripts/auto_place.R`
- Test: manual (browser/git not unit-tested)

- [ ] **Step 1: Add `sync_recs()` to `R/auto-place.R`**

```r
#' Stash-safe `git pull --rebase` so the placer acts on CI's latest recs.
#'
#' Follows the `.claude/rules/git-hygiene.md` cron-collision pattern. Returns
#' `TRUE` on a clean fast-forward/rebase, `FALSE` on any conflict or error.
#' @param repo_root Repository root.
#' @return Logical scalar.
#' @export
sync_recs <- function(repo_root = here::here()) {
  g <- function(...) {
    system2("git", c("-C", repo_root, ...), stdout = TRUE, stderr = TRUE)
  }
  attr_ok <- function(out) is.null(attr(out, "status")) || attr(out, "status") == 0L
  g("stash", "push", "-u", "-m", "auto_place sync")
  out <- g("pull", "--rebase", "origin", "main")
  ok <- attr_ok(out)
  pop <- g("stash", "pop")            # no-op if nothing was stashed
  ok && (attr_ok(pop) || any(grepl("No stash entries", pop)))
}
```

- [ ] **Step 2: Create `scripts/auto_place.R`**

```r
#!/usr/bin/env Rscript
# scripts/auto_place.R -- unattended low-footprint placer.
# LOCAL ONLY -- never referenced by CI (test-placer-ci-isolation.R enforces).
# Driven by the launchd agent is.metill.sports.autoplace.
#
# Flow: jitter -> daytime guard -> run_auto_place (kill/lock/sync/gate/cap/place)
# -> status recorded for the health layer. Relies on the placer's existing
# P1-P4 rules, sample_delay() pacing, and the daily/per-match caps.

suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))
root <- here::here("data")

Sys.sleep(stats::runif(1, 0, 1200)) # 0-20 min jitter; irregular timing

hr <- as.integer(format(Sys.time(), "%H"))
if (hr < 9L || hr >= 22L) {
  cli::cli_alert_info("Outside daytime window ({hr}:00); skipping.")
  quit(save = "no", status = 0L)
}

rec <- run_auto_place(root = root)
cli::cli_alert_info("auto_place: {rec$status}")
```

- [ ] **Step 3: Manual verification — kill switch path (no browser, no money)**

```bash
cd /Users/brynjolfurjonsson/sports
touch data/AUTO_PLACE_DISABLED
TZ=UTC Rscript -e 'Sys.setenv(); source("scripts/auto_place.R")' 2>&1 | tail -3 || true
Rscript -e 'cat(sports::read_placement_status("data")$status, "\n")'   # expect: disabled
rm data/AUTO_PLACE_DISABLED
```

Expected: status `disabled`, no browser opens. (Jitter sleeps up to 20 min — for the manual check, temporarily comment the `Sys.sleep` line or wrap with `if (!nzchar(Sys.getenv("AUTO_PLACE_NOJITTER")))` and set the env.)

- [ ] **Step 4: Manual verification — gate path with nothing pending**

With the ledger fully caught up (or no upcoming recs), run the shell once and confirm status `nothing_pending` and that **no Lengjan login occurred** (no chromote in logs).

- [ ] **Step 5: Commit**

```bash
git add R/auto-place.R scripts/auto_place.R
git commit -m "feat(auto-place): sync_recs + scripts/auto_place.R shell"
```

---

### Task 10: launchd agent + installer + gitignore

**Files:**
- Create: `tools/launchd/is.metill.sports.autoplace.plist.template`
- Create: `tools/install-autoplace.sh`
- Modify: `.gitignore`

- [ ] **Step 1: Create the plist template** (`__REPO__`/`__RSCRIPT__` rendered by the installer)

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>Label</key>            <string>is.metill.sports.autoplace</string>
  <key>ProgramArguments</key>
  <array>
    <string>__RSCRIPT__</string>
    <string>__REPO__/scripts/auto_place.R</string>
  </array>
  <key>WorkingDirectory</key> <string>__REPO__</string>
  <key>StartInterval</key>    <integer>7200</integer>
  <key>RunAtLoad</key>        <true/>
  <key>StandardOutPath</key>  <string>__HOME__/Library/Logs/sports-autoplace.log</string>
  <key>StandardErrorPath</key><string>__HOME__/Library/Logs/sports-autoplace.log</string>
  <key>EnvironmentVariables</key>
  <dict>
    <key>PATH</key> <string>/usr/local/bin:/opt/homebrew/bin:/usr/bin:/bin</string>
    <key>TZ</key>   <string>Atlantic/Reykjavik</string>
  </dict>
</dict>
</plist>
```

Note: the script's own daytime guard (09:00-22:00) bounds the `StartInterval` firings; `.Renviron` (with `LENGJAN_*`) is loaded by R from `WorkingDirectory`.

- [ ] **Step 2: Create `tools/install-autoplace.sh`**

```bash
#!/usr/bin/env bash
set -euo pipefail
REPO="$(cd "$(dirname "$0")/.." && pwd)"
RSCRIPT="$(command -v Rscript)"
PLIST="$HOME/Library/LaunchAgents/is.metill.sports.autoplace.plist"
TEMPLATE="$REPO/tools/launchd/is.metill.sports.autoplace.plist.template"

case "${1:-install}" in
  install)
    mkdir -p "$HOME/Library/LaunchAgents"
    sed -e "s#__REPO__#$REPO#g" -e "s#__RSCRIPT__#$RSCRIPT#g" \
        -e "s#__HOME__#$HOME#g" "$TEMPLATE" > "$PLIST"
    launchctl bootout "gui/$(id -u)/is.metill.sports.autoplace" 2>/dev/null || true
    launchctl bootstrap "gui/$(id -u)" "$PLIST"
    echo "Installed + loaded. Tail: ~/Library/Logs/sports-autoplace.log"
    echo "Kill switch: touch $REPO/data/AUTO_PLACE_DISABLED"
    ;;
  uninstall)
    launchctl bootout "gui/$(id -u)/is.metill.sports.autoplace" 2>/dev/null || true
    rm -f "$PLIST"
    echo "Uninstalled."
    ;;
  *) echo "usage: $0 [install|uninstall]"; exit 1 ;;
esac
```

- [ ] **Step 3: Ignore runtime files** — append to `.gitignore`:

```
# Unattended placer runtime (local only)
data/.auto_place.lock
data/AUTO_PLACE_DISABLED
```

- [ ] **Step 4: Manual verification**

```bash
chmod +x tools/install-autoplace.sh
bash tools/install-autoplace.sh install
launchctl print "gui/$(id -u)/is.metill.sports.autoplace" | grep -E "state|program" | head
# watch one cycle, then confirm status surfaced:
Rscript -e 'print(sports::pipeline_health())' | grep placement_health
bash tools/install-autoplace.sh uninstall   # until you choose to enable for real
```

- [ ] **Step 5: Commit**

```bash
git add tools/launchd/ tools/install-autoplace.sh .gitignore
git commit -m "feat(auto-place): launchd agent + installer (local-only, opt-in)"
```

---

### Task 11: Documentation

**Files:**
- Modify: `.claude/rules/sports-betting.md` (Local-only enforcement section)
- Modify: `CLAUDE.md` (Local-only subsystem section)
- Create: `docs/runbooks/auto-place.md`

- [ ] **Step 1: Document the subsystem in `.claude/rules/sports-betting.md`**

Add under "## Local-only enforcement", after the existing paragraph:

```markdown
### Unattended auto-placement (local-only)

`scripts/auto_place.R` (+ `R/auto-place.R`) is a launchd-scheduled wrapper that
runs `run_auto_place()` on a jittered daytime cadence. It gates on
`preview_pending()` (zero Lengjan contact) and opens an authenticated session
only when a new bet is pending, enforcing a cross-session daily cap, a kill
switch (`data/AUTO_PLACE_DISABLED`), and a PID lock. It is **never** wired into
CI; `test-placer-ci-isolation.R` forbids `auto_place`/`autoplace`/`AUTO_PLACE`/
`run_auto_place` tokens in workflows. Failures surface via the
`placement_health` health check. Install/remove: `tools/install-autoplace.sh`.
```

- [ ] **Step 2: Cross-reference in `CLAUDE.md`** — add one bullet to the "Local-only subsystem" section:

```markdown
- **Unattended placement (opt-in):** `scripts/auto_place.R` via the launchd
  agent `is.metill.sports.autoplace` (installed by `tools/install-autoplace.sh`).
  Kill switch: `touch data/AUTO_PLACE_DISABLED`. Health: the `placement_health`
  check in `/pipeline-doctor`. Design + plan under `docs/superpowers/`.
```

- [ ] **Step 3: Create `docs/runbooks/auto-place.md`** with the operator triage:

```markdown
# Runbook: unattended auto-placement

## Enable / disable
- Enable: `bash tools/install-autoplace.sh install`
- Disable now (no unload): `touch data/AUTO_PLACE_DISABLED`
- Remove the agent: `bash tools/install-autoplace.sh uninstall`

## Health says placement_health WARN/FAIL
1. `Rscript -e 'print(sports::read_placement_status("data"))'` -- last run.
2. `failed:*` -> read `~/Library/Logs/sports-autoplace.log`; common causes:
   Lengjan login (`LENGJAN_*` in `.Renviron`), Chromote launch, parser
   disagreement (Lengjan UI change -> see `.claude/rules/sports-betting.md`).
3. `sync_failed` -> resolve the git state manually (`/sync-main`), then let the
   next cycle run.
4. Stale (no recent healthy run) while pending -> check the Mac was awake in the
   daytime window and the agent is loaded (`launchctl print ...`).

## Confirm it is not on CI
`Rscript -e 'devtools::test_file("tests/testthat/test-placer-ci-isolation.R")'`
```

- [ ] **Step 4: Verify the full suite still passes**

Run: `Rscript -e 'devtools::test()'`
Expected: 0 failures (existing 1120+ assertions + the new auto-place/health tests).

- [ ] **Step 5: Commit**

```bash
git add .claude/rules/sports-betting.md CLAUDE.md docs/runbooks/auto-place.md
git commit -m "docs(auto-place): subsystem rules, CLAUDE.md pointer, runbook"
```

---

## Self-Review

**Spec coverage:**
- Wrapper sequence (jitter/kill/daytime/lock/sync/gate/daily-cap/place/record) -> Tasks 2,3,4,5,6,7,9.
- launchd agent -> Task 10. placement_health surfacing -> Task 8. CI-isolation -> Task 1.
- Safety rails: daily cap (Task 4), kill switch (Task 2), lock (Task 3), failure visibility (Task 8). Human-pacing -> reused existing `sample_delay()` (noted, YAGNI). Testing + CI-isolation -> Tasks 1,8, all unit tasks. Risks/docs -> Task 11.

**Placeholder scan:** No TBD/TODO; every code/test step shows full code; manual-only steps (browser/git/launchd) are explicitly marked and given exact commands.

**Type consistency:** `record_placement_status`/`read_placement_status`, `acquire/release_auto_place_lock`, `auto_place_decide(kill,locked,sync,pending_n,room)`, `run_auto_place(root,now,sync_fn,place_fn,bankroll_fn)`, `check_placement_health(root,now,th)`, `sync_recs(repo_root)`, `auto_place_daily_room(root,bankroll,now)` — names and signatures match across tasks. Status vocabulary (`placed/nothing_pending/ev_rejected/disabled/locked/sync_failed/daily_cap_reached/failed:*`) is consistent between `run_auto_place`, `auto_place_decide`, and `check_placement_health`'s `healthy` set.

**Known follow-ups (out of plan scope, noted in spec Open Items):** confirm `load_recommendations` column set on first run; tune jitter/window/thresholds after observing real cadence.
