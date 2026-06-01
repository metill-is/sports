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
