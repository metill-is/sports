#!/usr/bin/env Rscript
#### Unified Sports Pipeline — CLI Entry Point ####
#
# Usage:
#   Rscript run.R --sport handball --step data,fit,bet
#   Rscript run.R --league football_england --step bet
#   Rscript run.R --all --step data
#   Rscript run.R --active --step data,fit,bet
#   Rscript run.R --all --step bet --dry-run
#
# Flags:
#   --sport <name>     Filter by sport (basketball, handball, football)
#   --country <name>   Filter by country (iceland, england, ...)
#   --league <key>     Filter by league key (football_england, handball_iceland, ...)
#   --all              Run all leagues
#   --active           Run only leagues with upcoming games (via schedule scanner)
#   --step <steps>     Comma-separated: data, fit, results, bet, settle (default: all)
#   --sex <sex>        Override sex filter (male, female)
#   --iter <n>         Override sampling iterations
#   --log              Log bets to history CSV (default: recommend only, no logging)
#   --no-plots         Skip plot generation (posterior CSV still written)
#   --dry-run          Print plan without executing

library(here)
here::i_am(".here")

sports_dir <- here::here()

# ── Parse CLI args ────────────────────────────────────────────────────────────

args <- commandArgs(trailingOnly = TRUE)

parse_arg <- function(flag) {
  idx <- which(args == flag)
  if (length(idx) == 0) return(NULL)
  if (idx == length(args)) stop(paste(flag, "requires a value"))
  args[idx + 1]
}

has_flag <- function(flag) flag %in% args

arg_sport   <- parse_arg("--sport")
arg_country <- parse_arg("--country")
arg_league  <- parse_arg("--league")
arg_all     <- has_flag("--all")
arg_active  <- has_flag("--active")
arg_step    <- parse_arg("--step")
arg_sex     <- parse_arg("--sex")
arg_iter    <- parse_arg("--iter")
arg_dry_run  <- has_flag("--dry-run")
arg_log      <- has_flag("--log")
arg_no_plots <- has_flag("--no-plots")

# Validate: at least one selector
if (is.null(arg_sport) && is.null(arg_country) && is.null(arg_league) &&
    !arg_all && !arg_active) {
  cat("Error: specify --sport, --country, --league, --all, or --active\n")
  cat("Run 'Rscript run.R --help' for usage.\n")
  quit(status = 1)
}

if (has_flag("--help")) {
  cat("Usage: Rscript run.R [selector] [options]\n\n")
  cat("Selectors (pick one):\n")
  cat("  --sport <name>     Filter by sport\n")
  cat("  --country <name>   Filter by country\n")
  cat("  --league <key>     Filter by league key\n")
  cat("  --all              All leagues\n")
  cat("  --active           Leagues with upcoming games\n\n")
  cat("Options:\n")
  cat("  --step <steps>     data,fit,results,bet,settle (default: all)\n")
  cat("  --sex <sex>        Override: male or female\n")
  cat("  --iter <n>         Override sampling iterations\n")
  cat("  --log              Log bets to history (default: recommend only)\n")
  cat("  --no-plots         Skip plot generation (posterior CSV still written)\n")
  cat("  --dry-run          Print plan, don't execute\n")
  quit(status = 0)
}

# Parse steps
all_steps <- c("data", "fit", "results", "bet", "settle")
steps <- if (!is.null(arg_step)) {
  s <- strsplit(arg_step, ",")[[1]]
  bad <- setdiff(s, all_steps)
  if (length(bad) > 0) stop("Unknown steps: ", paste(bad, collapse = ", "))
  s
} else {
  all_steps
}

# Parse iterations override
iter_override <- if (!is.null(arg_iter)) as.integer(arg_iter) else NULL

# ── Load and filter leagues ───────────────────────────────────────────────────

box::use(R/pipeline/config[load_leagues, filter_leagues, schedule_to_league_keys])

leagues <- load_leagues(here("config", "leagues.yml"))

# Apply --active filter via schedule scanner
active_keys <- NULL
if (arg_active) {
  box::use(R/schedule/scan[scan_schedules])
  scan_result <- scan_schedules(sports_dir, lookahead_days = 7)
  schedule_active <- scan_result$summary$key[scan_result$summary$status == "active"]
  active_keys <- schedule_to_league_keys(schedule_active, names(leagues))

  if (length(active_keys) == 0) {
    cat("No leagues have upcoming games in the next 7 days.\n")
    quit(status = 0)
  }
}

selected <- filter_leagues(
  leagues,
  sport = arg_sport,
  country = arg_country,
  league_key = arg_league,
  active_keys = active_keys
)

if (length(selected) == 0) {
  cat("No leagues match the given filters.\n")
  quit(status = 1)
}

# ── Dry-run: print plan and exit ──────────────────────────────────────────────

cat("\n")
cat(strrep("\u2500", 60), "\n")
cat(" Sports Pipeline", if (arg_dry_run) " [DRY RUN]" else "", "\n")
cat(strrep("\u2500", 60), "\n\n")

cat("Steps:", paste(steps, collapse = ", "), "\n")
if (!is.null(iter_override)) cat("Iterations override:", iter_override, "\n")
if (!is.null(arg_sex)) cat("Sex override:", arg_sex, "\n")
if (arg_no_plots) cat("Plots: disabled (--no-plots)\n")
cat("Leagues:", length(selected), "\n\n")

for (key in names(selected)) {
  league <- selected[[key]]
  sexes <- if (!is.null(arg_sex)) arg_sex else league$sex
  cat(sprintf("  %-30s %s  [%s]\n",
    key,
    paste(sexes, collapse = "+"),
    league$pipeline
  ))
}
cat("\n")

# ── Build step manifest ─────────────────────────────────────────────────────

step_keys <- character(0)
for (key in names(selected)) {
  league <- selected[[key]]
  sexes <- if (!is.null(arg_sex)) arg_sex else league$sex

  for (sex in sexes) {
    if ("data" %in% steps)                          step_keys <- c(step_keys, paste("data", key, sex, sep = "_"))
    if ("fit" %in% steps)                           step_keys <- c(step_keys, paste("fit", key, sex, sep = "_"))
    if ("results" %in% steps && !"fit" %in% steps)  step_keys <- c(step_keys, paste("results", key, sex, sep = "_"))
  }
  if ("bet" %in% steps)    step_keys <- c(step_keys, paste("bet", key, sep = "_"))
  if ("settle" %in% steps) step_keys <- c(step_keys, paste("settle", key, sep = "_"))
}

cat(sprintf("Total steps: %d\n", length(step_keys)))

if (arg_dry_run) {
  cat("\nDry run complete. Remove --dry-run to execute.\n")
  quit(status = 0)
}

# ── Execute pipeline ──────────────────────────────────────────────────────────

source(here("R", "shared", "progress.R"), local = TRUE)

timing_cache_path <- here("config", "timing_cache.json")
cache <- load_timing_cache(timing_cache_path)
tracker <- create_tracker(step_keys, cache)

# Register progressr handler for cmdstanr progress bars (PR #1138) — must be
# done once at top level, not inside tryCatch/handlers.
# txtprogressbar to stderr flushes immediately; cli handler buffers on Rscript.
if ("fit" %in% steps && requireNamespace("cmdstanr", quietly = TRUE) &&
    exists("register_default_progress_handler", where = asNamespace("cmdstanr")) &&
    requireNamespace("progressr", quietly = TRUE)) {
  options(progressr.enable = TRUE)
  progressr::handlers(global = TRUE)
  progressr::handlers(
    progressr::handler_txtprogressbar(style = 3, file = stderr())
  )
}

# Helpers
quiet_here <- function(...) suppressMessages(here::i_am(...))

tracked_step <- function(step_fn, label, step_key, ...) {
  tracker$start_step(label, key = step_key)
  ok <- tryCatch(
    { step_fn(...); TRUE },
    error = function(e) {
      cat(sprintf("\n         %s\n", conditionMessage(e)))
      FALSE
    }
  )
  tracker$end_step(if (ok) "OK" else "FAILED")
  ok
}

# Load step modules lazily
if ("data" %in% steps) box::use(R/pipeline/step_data[run_data_step])
if ("fit" %in% steps || "results" %in% steps) box::use(R/pipeline/step_fit[run_fit_step])
if ("bet" %in% steps) box::use(R/pipeline/step_bet[run_bet_step])
if ("settle" %in% steps) box::use(R/pipeline/step_settle[run_settle_step])

all_results <- list()
all_recommendations <- list()

for (key in names(selected)) {
  league <- selected[[key]]
  sexes <- if (!is.null(arg_sex)) arg_sex else league$sex

  for (sex in sexes) {
    # Data step
    if ("data" %in% steps) {
      step_key <- paste("data", key, sex, sep = "_")
      ok <- tracked_step(
        run_data_step,
        paste("data:", key, sex),
        step_key,
        league = league,
        sex = sex,
        sports_dir = sports_dir
      )
      all_results[[length(all_results) + 1]] <- list(step = "data", league = key, sex = sex, ok = ok)
      quiet_here(".here")
    }

    # Fit step (includes results generation)
    if ("fit" %in% steps) {
      step_key <- paste("fit", key, sex, sep = "_")
      ok <- tracked_step(
        run_fit_step,
        paste("fit:", key, sex),
        step_key,
        league = league,
        sex = sex,
        sports_dir = sports_dir,
        iter_warmup = iter_override %||% league$iter_warmup,
        iter_sampling = iter_override %||% league$iter_sampling,
        generate_results = TRUE,
        generate_plots = !arg_no_plots,
        expected_duration = cache[[step_key]]
      )
      all_results[[length(all_results) + 1]] <- list(step = "fit", league = key, sex = sex, ok = ok)
      quiet_here(".here")
    }

    # Results-only step (if fit wasn't requested)
    if ("results" %in% steps && !"fit" %in% steps) {
      step_key <- paste("results", key, sex, sep = "_")
      ok <- tracked_step(
        run_fit_step,
        paste("results:", key, sex),
        step_key,
        league = league,
        sex = sex,
        sports_dir = sports_dir,
        fit_model = FALSE,
        generate_results = TRUE,
        generate_plots = !arg_no_plots
      )
      all_results[[length(all_results) + 1]] <- list(step = "results", league = key, sex = sex, ok = ok)
      quiet_here(".here")
    }
  }

  # Bet step (runs once per league, iterates sexes internally)
  if ("bet" %in% steps) {
    step_key <- paste("bet", key, sep = "_")
    bet_res <- NULL
    tracker$start_step(paste("bet:", key), key = step_key)
    ok <- tryCatch({
      bet_res <- run_bet_step(
        league = league,
        sports_dir = sports_dir,
        log = arg_log
      )
      TRUE
    }, error = function(e) {
      cat(sprintf("\n         %s\n", conditionMessage(e)))
      FALSE
    })
    tracker$end_step(if (ok) "OK" else "FAILED")
    all_results[[length(all_results) + 1]] <- list(step = "bet", league = key, sex = NA, ok = ok)
    if (!is.null(bet_res) && nrow(bet_res) > 0) {
      all_recommendations <- c(all_recommendations, list(bet_res))
    }
    quiet_here(".here")
  }

  # Settle step (runs once per league, checks all sexes)
  if ("settle" %in% steps) {
    step_key <- paste("settle", key, sep = "_")
    ok <- tracked_step(
      run_settle_step,
      paste("settle:", key),
      step_key,
      league = league,
      sports_dir = sports_dir
    )
    all_results[[length(all_results) + 1]] <- list(step = "settle", league = key, sex = NA, ok = ok)
    quiet_here(".here")
  }
}

# ── Summary ───────────────────────────────────────────────────────────────────

tracker$summary()
tracker$save_cache(timing_cache_path)

n_fail <- sum(!vapply(all_results, `[[`, logical(1), "ok"))

# ── Write combined recommendations ────────────────────────────────────────────

if ("bet" %in% steps && length(all_recommendations) > 0) {
  recs <- do.call(rbind, all_recommendations)
  recs_path <- here("recommendations.csv")
  readr::write_csv(recs, recs_path)
  cat(sprintf("\nWrote %d recommendation(s) to %s\n", nrow(recs), recs_path))
}

if (n_fail > 0) quit(status = 1)
