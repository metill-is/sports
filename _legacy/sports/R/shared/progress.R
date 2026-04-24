#### Pipeline Progress Tracker ####
#
# Compact progress display with timing cache and ETA estimation.
# Optionally writes fit progress to JSON for external consumers (Raycast).
#
# Usage:
#   cache <- load_timing_cache("config/timing_cache.json")
#   tracker <- create_tracker(step_keys, cache, progress_path = "~/.cache/raycast-pipeline/fit-progress.json")
#   tracker$init_fit_progress(fit_leagues)  # before fit phase
#   tracker$start_step("fit: football_england male")
#   # ... do work ...
#   tracker$end_step()           # marks success
#   tracker$end_step("FAILED")   # marks failure
#   tracker$finish_fit_progress() # after fit phase
#   tracker$summary()            # print final summary
#   tracker$save_cache("config/timing_cache.json")

#' Load timing cache from JSON
#'
#' @param path Path to timing_cache.json
#' @return Named list of step_key → duration (seconds)
#' @export
load_timing_cache <- function(path) {
  if (!file.exists(path)) {
    return(list())
  }
  tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) list()
  )
}

#' Save timing cache to JSON
#'
#' @param cache Named list of step_key → duration
#' @param path Path to write
#' @export
save_timing_cache <- function(cache, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  jsonlite::write_json(cache, path, auto_unbox = TRUE, pretty = TRUE)
}

#' Atomically write fit progress JSON
#'
#' Writes to a .tmp file then renames, preventing partial reads by consumers.
#' @param data List to serialise as JSON
#' @param path Destination path
#' @export
write_fit_progress <- function(data, path = "~/.cache/raycast-pipeline/fit-progress.json") {
  path <- path.expand(path)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  tmp <- paste0(path, ".tmp")
  jsonlite::write_json(data, tmp, auto_unbox = TRUE, pretty = FALSE)
  file.rename(tmp, path)
}

#' Create a pipeline progress tracker
#'
#' @param step_keys Character vector of ordered step keys
#'   (e.g. "data_football_england_male", "fit_football_england_male")
#' @param cache Named list from load_timing_cache() (optional)
#' @return List of functions: start_step, end_step, summary, save_cache, get_cache
#' @export
create_tracker <- function(step_keys, cache = list(), progress_path = NULL) {
  env <- new.env(parent = emptyenv())
  env$step_keys <- step_keys
  env$total <- length(step_keys)
  env$current <- 0L
  env$pipeline_start <- Sys.time()
  env$step_start <- NULL
  env$step_label <- ""
  env$step_key <- ""
  env$results <- list()
  env$cache <- cache
  env$progress_path <- if (!is.null(progress_path)) path.expand(progress_path) else NULL
  env$fit_leagues <- NULL
  env$fit_started_at <- NULL
  env$completed_fits <- list()
  env$fit_offset <- 0L # tracker$current value before first fit step
  env$fit_finished <- FALSE

  format_duration <- function(secs) {
    secs <- round(secs)
    if (secs < 60) {
      return(paste0(secs, "s"))
    }
    mins <- secs %/% 60
    remaining_secs <- secs %% 60
    if (mins < 60) {
      if (remaining_secs == 0) {
        return(paste0(mins, "m"))
      }
      return(paste0(mins, "m ", remaining_secs, "s"))
    }
    hrs <- mins %/% 60
    remaining_mins <- mins %% 60
    paste0(hrs, "h ", remaining_mins, "m")
  }

  estimate_remaining <- function() {
    if (env$current >= env$total) {
      return("")
    }

    remaining_keys <- env$step_keys[(env$current + 1):env$total]
    total_est <- 0
    n_uncached <- 0L

    # Sum cached durations for remaining steps
    for (key in remaining_keys) {
      if (!is.null(env$cache[[key]])) {
        total_est <- total_est + env$cache[[key]]
      } else {
        n_uncached <- n_uncached + 1L
      }
    }

    # For uncached steps, use average of completed steps this run
    if (n_uncached > 0 && length(env$results) > 0) {
      completed_durations <- vapply(
        env$results, function(r) r$elapsed, numeric(1)
      )
      avg_duration <- mean(completed_durations)
      total_est <- total_est + n_uncached * avg_duration
    }

    if (total_est <= 0) {
      return("")
    }
    paste0("~", format_duration(total_est))
  }

  progress_bar <- function() {
    done <- env$current
    total <- env$total
    width <- 20
    filled <- round(width * done / total)
    bar <- paste0(
      strrep("#", filled),
      strrep("-", width - filled)
    )
    pct <- round(100 * done / total)
    paste0("[", bar, "] ", pct, "%")
  }

  start_step <- function(label, key = NULL) {
    env$current <- env$current + 1L
    env$step_label <- label
    env$step_key <- key %||% env$step_keys[env$current]
    env$step_start <- Sys.time()

    cat(sprintf(
      " [%d/%d] %s...",
      env$current, env$total, label
    ))
    utils::flush.console()

    # Write fit progress JSON when starting a fit step
    if (!is.null(env$progress_path) && !is.null(env$fit_leagues) &&
      startsWith(env$step_key, "fit_")) {
      fit_idx <- env$current - env$fit_offset
      write_fit_progress(list(
        status = "fitting",
        league = sub("^fit_", "", env$step_key),
        league_index = fit_idx,
        total_leagues = length(env$fit_leagues),
        leagues = env$fit_leagues,
        phase = "starting",
        iteration = 0L,
        total_iterations = 0L,
        started_at = format(env$fit_started_at, "%Y-%m-%dT%H:%M:%S%z"),
        league_started_at = format(env$step_start, "%Y-%m-%dT%H:%M:%S%z"),
        completed_leagues = env$completed_fits
      ), env$progress_path)
    }
  }

  end_step <- function(status = "OK") {
    step_elapsed <- as.numeric(
      difftime(Sys.time(), env$step_start, units = "secs")
    )

    env$results[[length(env$results) + 1]] <- list(
      step = env$current,
      label = env$step_label,
      key = env$step_key,
      status = status,
      elapsed = step_elapsed
    )

    # Update cache with exponential moving average (0.7 new / 0.3 old)
    key <- env$step_key
    if (nzchar(key) && status == "OK") {
      old <- env$cache[[key]]
      env$cache[[key]] <- if (is.null(old)) {
        step_elapsed
      } else {
        0.7 * step_elapsed + 0.3 * old
      }
    }

    # Track completed fit steps for JSON progress
    if (!is.null(env$progress_path) && !is.null(env$fit_leagues) &&
      startsWith(env$step_key, "fit_")) {
      env$completed_fits[[length(env$completed_fits) + 1]] <- list(
        league = sub("^fit_", "", env$step_key),
        status = status,
        duration_s = round(step_elapsed, 1)
      )
      fit_idx <- env$current - env$fit_offset
      write_fit_progress(list(
        status = "fitting",
        league = sub("^fit_", "", env$step_key),
        league_index = fit_idx,
        total_leagues = length(env$fit_leagues),
        leagues = env$fit_leagues,
        phase = "done",
        iteration = 0L,
        total_iterations = 0L,
        started_at = format(env$fit_started_at, "%Y-%m-%dT%H:%M:%S%z"),
        completed_leagues = env$completed_fits
      ), env$progress_path)
    }

    icon <- if (status == "OK") "OK" else "FAIL"
    cat(sprintf(" %s %s\n", icon, format_duration(step_elapsed)))

    # Progress bar + ETA
    total_elapsed <- format_duration(
      as.numeric(difftime(Sys.time(), env$pipeline_start, units = "secs"))
    )
    eta <- estimate_remaining()
    cat(sprintf(
      "       %s  Elapsed: %s  ETA: %s\n",
      progress_bar(), total_elapsed, eta
    ))

    utils::flush.console()
  }

  summary <- function() {
    total_elapsed <- as.numeric(
      difftime(Sys.time(), env$pipeline_start, units = "secs")
    )

    cat("\n")
    cat(strrep("-", 60))
    cat("\n")

    n_ok <- sum(vapply(env$results, function(r) r$status == "OK", logical(1)))
    n_fail <- length(env$results) - n_ok

    cat(sprintf(
      " Pipeline complete: %d/%d steps succeeded in %s\n",
      n_ok, length(env$results), format_duration(total_elapsed)
    ))

    if (n_fail > 0) {
      cat("\n Failed steps:\n")
      for (r in env$results) {
        if (r$status != "OK") {
          cat(sprintf("   [%d] %s: %s\n", r$step, r$label, r$status))
        }
      }
    }

    cat(strrep("-", 60))
    cat("\n")
  }

  get_cache <- function() env$cache

  save_cache_fn <- function(path) {
    save_timing_cache(env$cache, path)
  }

  get_state <- function() {
    list(
      current = env$current,
      total = env$total,
      pipeline_start = env$pipeline_start,
      step_key = env$step_key,
      step_start = env$step_start,
      step_keys = env$step_keys,
      cache = env$cache,
      results = env$results,
      fit_leagues = env$fit_leagues,
      fit_started_at = env$fit_started_at,
      fit_offset = env$fit_offset,
      completed_fits = env$completed_fits
    )
  }

  init_fit_progress <- function(fit_leagues) {
    if (is.null(env$progress_path)) {
      return(invisible(NULL))
    }
    env$fit_leagues <- fit_leagues
    env$fit_started_at <- Sys.time()
    env$completed_fits <- list()
    env$fit_offset <- env$current # steps completed before fit phase
    write_fit_progress(list(
      status = "starting",
      leagues = fit_leagues,
      total_leagues = length(fit_leagues),
      started_at = format(env$fit_started_at, "%Y-%m-%dT%H:%M:%S%z"),
      completed_leagues = list()
    ), env$progress_path)
  }

  finish_fit_progress <- function(error_msg = NULL) {
    if (is.null(env$progress_path) || is.null(env$fit_leagues)) {
      return(invisible(NULL))
    }
    if (isTRUE(env$fit_finished)) {
      return(invisible(NULL))
    } # idempotent
    env$fit_finished <- TRUE
    status <- if (!is.null(error_msg)) "error" else "complete"
    payload <- list(
      status = status,
      total_leagues = length(env$fit_leagues),
      started_at = format(env$fit_started_at, "%Y-%m-%dT%H:%M:%S%z"),
      finished_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
      completed_leagues = env$completed_fits
    )
    if (!is.null(error_msg)) payload$error <- error_msg
    write_fit_progress(payload, env$progress_path)
  }

  get_completed_fits <- function() env$completed_fits

  get_progress_path <- function() env$progress_path

  # Parallel-safe step recorder: used by fit_parallel.R, which runs futures
  # concurrently so cannot use the serial start_step/end_step pair (the
  # tracker's env$current and env$step_key are per-call state and would
  # race between overlapping fits). record_step captures a single
  # atomic unit — start, end, cache, JSON update all at once.
  record_step <- function(step_key, label, elapsed, status = "OK") {
    # Coerce missing/NA elapsed to 0 — a crashed worker may return NA but
    # we still need to render the step line and progress bar rather than
    # crash the master in format_duration's `if (secs < 60)` check.
    if (is.null(elapsed) || !is.finite(elapsed)) elapsed <- 0
    env$current <- env$current + 1L
    env$step_label <- label
    env$step_key <- step_key
    env$step_start <- Sys.time() - elapsed

    env$results[[length(env$results) + 1]] <- list(
      step = env$current,
      label = label,
      key = step_key,
      status = status,
      elapsed = elapsed
    )

    if (nzchar(step_key) && status == "OK") {
      old <- env$cache[[step_key]]
      env$cache[[step_key]] <- if (is.null(old)) {
        elapsed
      } else {
        0.7 * elapsed + 0.3 * old
      }
    }

    if (!is.null(env$progress_path) && !is.null(env$fit_leagues) &&
      startsWith(step_key, "fit_")) {
      env$completed_fits[[length(env$completed_fits) + 1]] <- list(
        league = sub("^fit_", "", step_key),
        status = status,
        duration_s = round(elapsed, 1)
      )
    }

    icon <- if (status == "OK") "OK" else "FAIL"
    cat(sprintf(
      " [%d/%d] %s... %s %s\n",
      env$current, env$total, label, icon, format_duration(elapsed)
    ))

    total_elapsed <- format_duration(
      as.numeric(difftime(Sys.time(), env$pipeline_start, units = "secs"))
    )
    eta <- estimate_remaining()
    cat(sprintf(
      "       %s  Elapsed: %s  ETA: %s\n",
      progress_bar(), total_elapsed, eta
    ))
    utils::flush.console()
  }

  list(
    start_step = start_step,
    end_step = end_step,
    summary = summary,
    get_cache = get_cache,
    save_cache = save_cache_fn,
    get_state = get_state,
    init_fit_progress = init_fit_progress,
    finish_fit_progress = finish_fit_progress,
    get_completed_fits = get_completed_fits,
    get_progress_path = get_progress_path,
    record_step = record_step
  )
}
