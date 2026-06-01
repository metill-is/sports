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
