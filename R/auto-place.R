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
