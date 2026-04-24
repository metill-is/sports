#### Custom progressr handler for Stan fitting ####
#
# Renders a compact progress bar to stderr with live ETA based on
# observed iteration rate. Designed for non-interactive Rscript sessions
# where cli-based handlers buffer output.
#
# When a tracker (from progress.R) is provided, renders a two-line
# combined display showing both fitting progress and pipeline progress.
#
# Usage (from run.R):
#   source("R/shared/stan_progress_handler.R", local = TRUE)
#   progressr::handlers(global = TRUE)
#   progressr::handlers(stan_progress_handler(tracker = tracker))

stan_progress_handler <- function(width = 25, tracker = NULL, progress_path = NULL) {
  progressr::make_progression_handler(
    "stan_eta",
    reporter = local({
      start_time <- NULL
      has_tracker <- !is.null(tracker)
      last_json_write <- NULL

      fmt_duration <- function(secs) {
        if (is.na(secs) || secs < 0) {
          return("?")
        }
        secs <- round(secs)
        if (secs < 60) {
          return(paste0(secs, "s"))
        }
        mins <- secs %/% 60
        remaining <- secs %% 60
        if (mins < 60) {
          if (remaining == 0) {
            return(paste0(mins, "m"))
          }
          return(paste0(mins, "m ", remaining, "s"))
        }
        hrs <- mins %/% 60
        remaining_mins <- mins %% 60
        paste0(hrs, "h ", remaining_mins, "m")
      }

      make_bar <- function(frac, w = width) {
        filled <- round(w * frac)
        paste0(
          strrep("#", filled),
          strrep("-", w - filled)
        )
      }

      # Estimate remaining pipeline time using tracker state + current fit rate
      pipeline_eta <- function(state, fit_remaining_secs) {
        if (state$current >= state$total) {
          return(0)
        }
        remaining_keys <- state$step_keys[(state$current + 1):state$total]
        total_est <- 0
        n_uncached <- 0L
        for (key in remaining_keys) {
          if (!is.null(state$cache[[key]])) {
            total_est <- total_est + state$cache[[key]]
          } else {
            n_uncached <- n_uncached + 1L
          }
        }
        if (n_uncached > 0 && length(state$results) > 0) {
          completed <- vapply(state$results, function(r) r$elapsed, numeric(1))
          total_est <- total_est + n_uncached * mean(completed)
        }
        # Add remaining time for the current fit step
        total_est + fit_remaining_secs
      }

      render_single <- function(step, total) {
        pct <- round(100 * step / total)
        elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
        eta_str <- if (step > 0 && elapsed > 0) {
          fmt_duration((total - step) / (step / elapsed))
        } else {
          "?"
        }
        line <- sprintf(
          "\r         [%s] %3d%% | %s elapsed | %s remaining",
          make_bar(step / total), pct, fmt_duration(elapsed), eta_str
        )
        cat(line, file = stderr())
      }

      render_combined <- function(step, total) {
        state <- tracker$get_state()
        now <- Sys.time()

        # Fitting line
        fit_pct <- round(100 * step / total)
        fit_elapsed <- as.numeric(difftime(now, start_time, units = "secs"))
        fit_rate <- if (step > 0 && fit_elapsed > 0) step / fit_elapsed else 0
        fit_remaining <- if (fit_rate > 0) (total - step) / fit_rate else NA
        fit_eta_str <- if (!is.na(fit_remaining)) {
          paste0("~", fmt_duration(fit_elapsed + fit_remaining))
        } else {
          "?"
        }
        fit_line <- sprintf(
          "         Fitting [%s] %3d%% | %s / %s",
          make_bar(step / total), fit_pct,
          fmt_duration(fit_elapsed), fit_eta_str
        )

        # Pipeline line
        pipe_frac <- state$current / state$total
        pipe_pct <- round(100 * pipe_frac)
        pipe_elapsed <- as.numeric(difftime(now, state$pipeline_start, units = "secs"))
        pipe_remaining <- pipeline_eta(state, if (!is.na(fit_remaining)) fit_remaining else 0)
        pipe_total_est <- pipe_elapsed + pipe_remaining
        pipe_eta_str <- if (pipe_remaining > 0) paste0("~", fmt_duration(pipe_total_est)) else "?"
        pipe_line <- sprintf(
          "         Pipeline [%s] %3d%% | %s / %s",
          make_bar(pipe_frac, 20), pipe_pct,
          fmt_duration(pipe_elapsed), pipe_eta_str
        )

        # Move cursor up one line, clear, write both lines
        cat(sprintf("\r\033[A\033[K%s\n\033[K%s", fit_line, pipe_line), file = stderr())
      }

      list(
        initiate = function(config, state, progression, ...) {
          start_time <<- Sys.time()
          if (has_tracker) {
            # Reserve two lines for combined display
            cat("\n\n", file = stderr())
          }
        },
        update = function(config, state, progression, ...) {
          if (state$delta > 0) {
            if (has_tracker) {
              render_combined(state$step, config$max_steps)
            } else {
              render_single(state$step, config$max_steps)
            }

            # Write iteration-level progress to JSON (throttled to every ~5s)
            if (!is.null(progress_path) && has_tracker) {
              now <- Sys.time()
              if (is.null(last_json_write) ||
                as.numeric(difftime(now, last_json_write, units = "secs")) >= 5) {
                tracker_state <- tracker$get_state()
                fit_offset <- if (!is.null(tracker_state$fit_offset)) {
                  tracker_state$fit_offset
                } else {
                  0L
                }
                # Determine warmup vs sampling phase
                warmup_iters <- config$max_steps %/% 2
                phase <- if (state$step <= warmup_iters) "warmup" else "sampling"
                write_fit_progress(list(
                  status = "fitting",
                  league = sub("^fit_", "", tracker_state$step_key),
                  league_index = tracker_state$current - fit_offset,
                  total_leagues = length(tracker_state$fit_leagues),
                  leagues = tracker_state$fit_leagues,
                  phase = phase,
                  chain = NULL,
                  total_chains = 4L,
                  iteration = state$step,
                  total_iterations = config$max_steps,
                  started_at = format(tracker_state$fit_started_at, "%Y-%m-%dT%H:%M:%S%z"),
                  league_started_at = format(tracker_state$step_start, "%Y-%m-%dT%H:%M:%S%z"),
                  completed_leagues = tracker_state$completed_fits
                ), progress_path)
                last_json_write <<- now
              }
            }
          }
        },
        finish = function(config, state, progression, ...) {
          if (has_tracker) {
            # Clear both lines so end_step() renders cleanly
            cat("\r\033[A\033[K\033[A\033[K", file = stderr())
          } else {
            render_single(state$step, config$max_steps)
            cat("\n", file = stderr())
          }
        }
      )
    }),
    enable = TRUE,
    target = "terminal"
  )
}


#### Fragment-writing progress handler (parallel workers) ####
#
# Used when a fit runs inside a future::multisession worker — no shared
# tracker is visible, so instead of updating fit-progress.json directly
# this handler writes a per-fit fragment that the master aggregates.
#
# Does not render to stderr (multiple concurrent workers would interleave
# their output into a garbled mess). All UI lives in the master's
# aggregated fit-progress.json.

stan_fragment_handler <- function(fragment_path, league_key, label) {
  progressr::make_progression_handler(
    "stan_fragment",
    reporter = local({
      start_time <- NULL
      last_write <- NULL
      write_interval <- 2

      write_frag <- function(path, data) {
        dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
        tmp <- paste0(path, ".tmp.", Sys.getpid())
        jsonlite::write_json(data, tmp, auto_unbox = TRUE, pretty = FALSE)
        file.rename(tmp, path)
      }

      list(
        initiate = function(config, state, progression, ...) {
          start_time <<- Sys.time()
          write_frag(fragment_path, list(
            league = league_key,
            label = label,
            phase = "starting",
            iteration = 0L,
            total_iterations = config$max_steps,
            started_at = format(start_time, "%Y-%m-%dT%H:%M:%S%z"),
            updated_at = format(start_time, "%Y-%m-%dT%H:%M:%S%z"),
            status = "running"
          ))
        },
        update = function(config, state, progression, ...) {
          if (state$delta > 0) {
            now <- Sys.time()
            if (is.null(last_write) ||
              as.numeric(difftime(now, last_write, units = "secs")) >= write_interval) {
              warmup_iters <- config$max_steps %/% 2
              phase <- if (state$step <= warmup_iters) "warmup" else "sampling"
              write_frag(fragment_path, list(
                league = league_key,
                label = label,
                phase = phase,
                iteration = state$step,
                total_iterations = config$max_steps,
                started_at = format(start_time, "%Y-%m-%dT%H:%M:%S%z"),
                updated_at = format(now, "%Y-%m-%dT%H:%M:%S%z"),
                status = "running"
              ))
              last_write <<- now
            }
          }
        },
        finish = function(config, state, progression, ...) {
          # Final "done" write is performed by the worker in fit_parallel.R
          # after run_fit_step returns, so it can record pass/fail status
          # beyond what progressr sees.
          invisible(NULL)
        }
      )
    }),
    enable = TRUE,
    target = "terminal"
  )
}
