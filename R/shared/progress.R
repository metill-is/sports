#### Pipeline Progress Tracker ####
#
# Provides compact progress display for the Sports update pipeline.
# Tracks steps, elapsed time, and estimates remaining time.
#
# Usage:
#   tracker <- create_tracker(total_steps)
#   tracker$start_step("Downloading basketball male data")
#   # ... do work ...
#   tracker$end_step()           # marks success
#   tracker$end_step("FAILED")   # marks failure
#   tracker$summary()            # print final summary

#' Create a pipeline progress tracker
#'
#' @param total_steps Total number of steps in the pipeline
#' @return List of functions: start_step, end_step, summary
#' @export
create_tracker <- function(total_steps) {
  env <- new.env(parent = emptyenv())
  env$total <- total_steps
  env$current <- 0L
  env$pipeline_start <- Sys.time()
  env$step_start <- NULL
  env$step_label <- ""
  env$results <- list()

  format_duration <- function(secs) {
    secs <- round(secs)
    if (secs < 60) {
      return(paste0(secs, "s"))
    }
    mins <- secs %/% 60
    remaining_secs <- secs %% 60
    if (mins < 60) {
      if (remaining_secs == 0) return(paste0(mins, "m"))
      return(paste0(mins, "m ", remaining_secs, "s"))
    }
    hrs <- mins %/% 60
    remaining_mins <- mins %% 60
    paste0(hrs, "h ", remaining_mins, "m")
  }

  estimate_remaining <- function() {
    elapsed <- as.numeric(difftime(Sys.time(), env$pipeline_start, units = "secs"))
    if (env$current == 0) return("")
    avg_per_step <- elapsed / env$current
    remaining_steps <- env$total - env$current
    est_secs <- avg_per_step * remaining_steps
    paste0("~", format_duration(est_secs))
  }

  progress_bar <- function() {
    done <- env$current
    total <- env$total
    width <- 20
    filled <- round(width * done / total)
    bar <- paste0(
      strrep("\u2588", filled),
      strrep("\u2591", width - filled)
    )
    pct <- round(100 * done / total)
    paste0("[", bar, "] ", pct, "%")
  }

  start_step <- function(label) {
    env$current <- env$current + 1L
    env$step_label <- label
    env$step_start <- Sys.time()

    elapsed <- format_duration(
      as.numeric(difftime(Sys.time(), env$pipeline_start, units = "secs"))
    )

    cat(sprintf(
      " [%d/%d] %s...",
      env$current, env$total, label
    ))
    utils::flush.console()
  }

  end_step <- function(status = "OK") {
    step_elapsed <- as.numeric(
      difftime(Sys.time(), env$step_start, units = "secs")
    )

    env$results[[length(env$results) + 1]] <- list(
      step = env$current,
      label = env$step_label,
      status = status,
      elapsed = step_elapsed
    )

    icon <- if (status == "OK") "\u2713" else "\u2717"

    cat(sprintf(" %s %s\n", icon, format_duration(step_elapsed)))

    # Show progress bar + ETA after each step
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
    cat(strrep("\u2500", 60))
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

    cat(strrep("\u2500", 60))
    cat("\n")
  }

  list(
    start_step = start_step,
    end_step = end_step,
    summary = summary
  )
}
