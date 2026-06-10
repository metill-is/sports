#' @include config.R storage.R placer-preview.R placer-load.R commit-ledger.R
NULL

#' Is unattended placement disabled by the operator kill switch?
#'
#' @param root Data root.
#' @return `TRUE` if `data/AUTO_PLACE_DISABLED` exists.
#' @export
placement_kill_switched <- function(root = here::here("data")) {
  fs::file_exists(fs::path(root, "AUTO_PLACE_DISABLED"))
}

#' @noRd
.auto_place_lock_path <- function(root) fs::path(root, ".auto_place.lock")

#' @noRd
.pid_alive <- function(pid) {
  if (is.na(pid)) {
    return(FALSE)
  }
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
    if (.pid_alive(pid)) {
      return(FALSE)
    }
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
    na.rm = TRUE
  )
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
    error = function(e) tibble::tibble()
  )
  .daily_room(daily_budget, .placed_today_stake(led, now))
}

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
  if (!fs::file_exists(path)) {
    return(NULL)
  }
  jsonlite::read_json(path, simplifyVector = TRUE)
}

#' Decide the next unattended-placement action from gathered signals.
#'
#' Precedence mirrors the wrapper sequence: kill switch, lock, sync, gate,
#' daily cap, then place.
#' @param kill_switched,locked,sync_ok,pending_n,daily_room Gathered signals.
#' @return One of `disabled`, `locked`, `sync_failed`, `nothing_pending`,
#'   `daily_cap_reached`, `place`.
#' @export
auto_place_decide <- function(kill_switched, locked, sync_ok, pending_n, daily_room) {
  if (isTRUE(kill_switched)) {
    return("disabled")
  }
  if (isTRUE(locked)) {
    return("locked")
  }
  if (!isTRUE(sync_ok)) {
    return("sync_failed")
  }
  if (pending_n == 0L) {
    return("nothing_pending")
  }
  if (daily_room <= 0) {
    return("daily_cap_reached")
  }
  "place"
}

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
#' @param headless Passed through to `place_fn`. `TRUE` (default) is the robust
#'   choice for unattended/launchd runs (no dependency on an active GUI session).
#'   Set `FALSE` to watch a visible browser during a supervised run; the
#'   human-paced `sample_delay()` in the placer applies either way.
#' @return The recorded status list, invisibly.
#' @export
run_auto_place <- function(root = here::here("data"),
                           now = Sys.time(),
                           sync_fn = sync_recs,
                           place_fn = place_bets,
                           bankroll_fn = function() load_bankroll(ledger_root = root),
                           headless = TRUE) {
  if (placement_kill_switched(root)) {
    return(record_placement_status("disabled", run_at = now, root = root))
  }
  if (!acquire_auto_place_lock(root)) {
    return(record_placement_status("locked", run_at = now, root = root))
  }
  on.exit(release_auto_place_lock(root), add = TRUE)

  sync_ok <- isTRUE(tryCatch(sync_fn(here::here()), error = function(e) FALSE))
  pending <- tryCatch(suppressMessages(preview_pending(root = root)),
    error = function(e) tibble::tibble()
  )
  n_pending <- nrow(pending)
  room <- tryCatch(auto_place_daily_room(root, bankroll_fn(), now),
    error = function(e) 0
  )

  action <- auto_place_decide(FALSE, FALSE, sync_ok, n_pending, room)
  if (action != "place") {
    return(record_placement_status(action,
      n_pending = n_pending,
      run_at = now, root = root
    ))
  }

  res <- tryCatch(
    place_fn(dry_run = FALSE, interactive = FALSE, headless = headless, root = root),
    error = function(e) {
      record_placement_status(paste0("failed:", conditionMessage(e)),
        n_pending = n_pending, run_at = now, root = root
      )
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

#' Stash-safe `git pull --rebase` so the placer acts on CI's latest recs.
#'
#' Follows the `.claude/rules/git-hygiene.md` cron-collision pattern, hardened
#' for an unattended caller sharing the user's working tree:
#'
#' * **Entry guards.** Returns `FALSE` (skip the cycle; placement is gated on
#'   `sync_ok`) unless `HEAD` is `main` — `git pull --rebase origin main`
#'   rebases the *checked-out* branch, so a launchd run firing while a feature
#'   branch is checked out would silently rewrite it and land ledger commits
#'   off-main. Likewise skips when a rebase or merge is already in progress
#'   (a human may be mid-conflict-resolution; never touch their state).
#' * **Ledger rescue.** Uncommitted rows under `data/decisions/ledger/` (left
#'   behind by a run that died between the ledger write and its commit) are
#'   committed via [commit_ledger_changes()] *before* the stash dance, so
#'   real-money rows are never carried through a stash (L1) and a dirty
#'   ledger can never wedge the sync.
#' * **Own-stash discipline.** Pops only the stash entry *this run's* push
#'   created (detected via `refs/stash` before/after). A bare `git stash pop`
#'   after a no-op push would pop a pre-existing user stash — and a stale
#'   ledger parquet inside one would then be auto-committed to the canonical
#'   ledger by the next cycle's rescue.
#' * **Conflict unwind.** A failed pull aborts the rebase it started, and a
#'   conflicted pop leaves unmerged index entries that the entry guard then
#'   detects -- either way a conflict degrades to skipped cycles (visible as
#'   `sync_failed` in the placement status) until a human resolves, never a
#'   half-applied state the next cycle builds on. A pop that git refuses
#'   keeps the run's own `auto_place sync` stash entry; `git stash list`
#'   after any `sync_failed` is part of the triage.
#'
#' All git calls go through `.git_run()`, which shell-quotes each argument.
#' `system2(..., stdout = TRUE)` routes through `sh`, so an unquoted argument
#' containing a space — like the stash message here — is split into separate
#' words: git read `-m auto_place` plus pathspec `sync`, matched nothing,
#' silently stashed nothing (exit 0), and every post-placement sync then died
#' on the dirty ledger (incident 2026-06-10).
#' @param repo_root Repository root.
#' @return Logical scalar: `TRUE` on a clean fast-forward/rebase.
#' @export
sync_recs <- function(repo_root = here::here()) {
  g <- function(...) .git_run(c("-C", repo_root, ...))

  head_ref <- g("rev-parse", "--abbrev-ref", "HEAD")
  if (!head_ref$ok || !identical(head_ref$lines, "main")) {
    return(FALSE)
  }
  if (.git_operation_in_progress(repo_root)) {
    return(FALSE)
  }

  rescue <- commit_ledger_changes(
    repo_root, "data(ledger): rescue uncommitted rows found at auto-place sync"
  )
  if (identical(rescue$status, "failed")) {
    return(FALSE) # never stash money rows; skip this cycle instead
  }

  stash_before <- g("rev-parse", "-q", "--verify", "refs/stash")
  g("stash", "push", "-u", "-m", "auto_place sync")
  stash_after <- g("rev-parse", "-q", "--verify", "refs/stash")
  stash_created <- stash_after$ok &&
    (!stash_before$ok || !identical(stash_before$lines, stash_after$lines))

  pull <- g("pull", "--rebase", "origin", "main")
  if (!pull$ok && .git_operation_in_progress(repo_root)) {
    g("rebase", "--abort")
  }

  ok <- pull$ok
  if (stash_created) {
    pop <- g("stash", "pop")
    ok <- ok && pop$ok
  }
  ok
}

#' Is a rebase, merge, cherry-pick, or revert already in progress -- or the
#' index left with unmerged entries (e.g. a conflicted stash pop, which
#' creates no marker file)? Either way a human owns the tree; skip the cycle.
#' @noRd
.git_operation_in_progress <- function(repo_root) {
  markers <- c(
    "rebase-merge", "rebase-apply", "MERGE_HEAD", "CHERRY_PICK_HEAD",
    "REVERT_HEAD"
  )
  marker_hit <- any(vapply(markers, function(m) {
    res <- .git_run(c("-C", repo_root, "rev-parse", "--git-path", m))
    if (!res$ok || length(res$lines) == 0L) {
      return(FALSE)
    }
    p <- res$lines[[1L]]
    if (!fs::is_absolute_path(p)) p <- fs::path(repo_root, p)
    fs::file_exists(p) || fs::dir_exists(p)
  }, logical(1)))
  if (marker_hit) {
    return(TRUE)
  }
  unmerged <- .git_run(c("-C", repo_root, "ls-files", "-u"))
  unmerged$ok && length(unmerged$lines) > 0L
}
