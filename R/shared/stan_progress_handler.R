#### Custom progressr handler for Stan fitting ####
#
# Renders a compact progress bar to stderr with live ETA based on
# observed iteration rate. Designed for non-interactive Rscript sessions
# where cli-based handlers buffer output.
#
# Usage (from run.R):
#   source("R/shared/stan_progress_handler.R", local = TRUE)
#   progressr::handlers(global = TRUE)
#   progressr::handlers(stan_progress_handler())

stan_progress_handler <- function(width = 25) {
  progressr::make_progression_handler(
    "stan_eta",
    reporter = local({
      start_time <- NULL

      fmt_duration <- function(secs) {
        if (is.na(secs) || secs < 0) return("?")
        secs <- round(secs)
        if (secs < 60) return(paste0(secs, "s"))
        mins <- secs %/% 60
        remaining <- secs %% 60
        if (mins < 60) {
          if (remaining == 0) return(paste0(mins, "m"))
          return(paste0(mins, "m ", remaining, "s"))
        }
        hrs <- mins %/% 60
        remaining_mins <- mins %% 60
        paste0(hrs, "h ", remaining_mins, "m")
      }

      render <- function(step, total) {
        pct <- round(100 * step / total)
        filled <- round(width * step / total)
        bar <- paste0(
          strrep("\u2588", filled),
          strrep("\u2591", width - filled)
        )

        elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

        eta_str <- if (step > 0 && elapsed > 0) {
          rate <- step / elapsed
          remaining <- (total - step) / rate
          fmt_duration(remaining)
        } else {
          "?"
        }

        line <- sprintf(
          "\r         [%s] %3d%% | %s elapsed | %s remaining",
          bar, pct, fmt_duration(elapsed), eta_str
        )
        cat(line, file = stderr())
      }

      list(
        initiate = function(config, state, progression, ...) {
          start_time <<- Sys.time()
        },
        update = function(config, state, progression, ...) {
          # Skip 0-delta updates (cmdstanr config messages)
          if (state$delta > 0) {
            render(state$step, config$max_steps)
          }
        },
        finish = function(config, state, progression, ...) {
          render(state$step, config$max_steps)
          cat("\n", file = stderr())
        }
      )
    }),
    enable = TRUE,
    target = "terminal"
  )
}
