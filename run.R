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
#   --stale            Filter to leagues with upcoming odds + stale/missing fit
#   --no-plots         Skip plot generation (posterior CSV still written)
#   --sync             Git-pull livesport-data and lengjan-odds before running
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
arg_stale    <- has_flag("--stale")
arg_dry_run  <- has_flag("--dry-run")
arg_no_plots <- has_flag("--no-plots")
arg_sync     <- has_flag("--sync")

# ── Sync upstream data repos ────────────────────────────────────────────────

if (arg_sync) {
  sync_repos <- list(
    list(name = "livesport-data", path = file.path(dirname(sports_dir), "livesport-data")),
    list(name = "lengjan-odds",   path = file.path(dirname(sports_dir), "lengjan-odds"))
  )
  for (repo in sync_repos) {
    if (dir.exists(file.path(repo$path, ".git"))) {
      cat(sprintf("Syncing %s... ", repo$name))
      res <- system2("git", c("-C", repo$path, "pull", "--ff-only", "-q"), stdout = TRUE, stderr = TRUE)
      status <- attr(res, "status")
      if (is.null(status) || status == 0L) {
        cat("OK\n")
      } else {
        cat(sprintf("WARN: %s\n", paste(res, collapse = " ")))
      }
    }
  }
  cat("\n")
}

# Validate: at least one selector
if (is.null(arg_sport) && is.null(arg_country) && is.null(arg_league) &&
    !arg_all && !arg_active && !arg_stale) {
  cat("Error: specify --sport, --country, --league, --all, --active, or --stale\n")
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
  cat("  --active           Leagues with upcoming games\n")
  cat("  --stale            Leagues with upcoming odds + stale/missing fit\n\n")
  cat("Options:\n")
  cat("  --step <steps>     data,fit,results,bet,settle (default: all)\n")
  cat("  --sex <sex>        Override: male or female\n")
  cat("  --iter <n>         Override sampling iterations\n")
  cat("  --no-plots         Skip plot generation (posterior CSV still written)\n")
  cat("  --sync             Git-pull livesport-data and lengjan-odds first\n")
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

# Auto-inject 'results' when 'bet' needs posterior CSVs
if ("bet" %in% steps && !"results" %in% steps) {
  steps <- append(steps, "results", after = which(steps == "bet") - 1)
  cat("Note: 'results' step added (required by 'bet')\n")
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

# Apply --stale filter: select all betting leagues when used standalone
if (arg_stale && is.null(arg_sport) && is.null(arg_country) &&
    is.null(arg_league) && !arg_all && !arg_active) {
  selected <- filter_leagues(leagues, has_bets_only = TRUE)
}

if (length(selected) == 0) {
  cat("No leagues match the given filters.\n")
  quit(status = 1)
}

# Apply --stale filter: narrow to stale league×sex combos
stale_sex_map <- NULL
if (arg_stale) {
  box::use(R/pipeline/staleness[find_stale_league_sexes])
  stale <- find_stale_league_sexes(selected, sports_dir)

  if (length(stale) == 0) {
    cat("No stale leagues found. All fits are fresh.\n")
    quit(status = 0)
  }

  stale_keys <- unique(vapply(stale, `[[`, character(1), "key"))
  selected <- selected[names(selected) %in% stale_keys]

  # Build sex override map: key -> c("male", "female")
  stale_sex_map <- list()
  stale_reason_map <- list()
  for (s in stale) {
    stale_sex_map[[s$key]] <- c(stale_sex_map[[s$key]], s$sex)
    stale_reason_map[[paste(s$key, s$sex)]] <- s$reason
  }
}

# ── Dry-run: print plan and exit ──────────────────────────────────────────────

cat("\n")
cat(strrep("\u2500", 60), "\n")
cat(" Sports Pipeline", if (arg_dry_run) " [DRY RUN]" else "", "\n")
cat(strrep("\u2500", 60), "\n\n")

cat("Steps:", paste(steps, collapse = ", "), "\n")
if (!is.null(iter_override)) cat("Iterations override:", iter_override, "\n")
if (!is.null(arg_sex)) cat("Sex override:", arg_sex, "\n")
if (arg_stale) cat("Filter: --stale (upcoming odds + stale fit)\n")
if (arg_no_plots) cat("Plots: disabled (--no-plots)\n")
cat("Leagues:", length(selected), "\n\n")

for (key in names(selected)) {
  league <- selected[[key]]
  sexes <- if (!is.null(arg_sex)) arg_sex
           else if (!is.null(stale_sex_map)) stale_sex_map[[key]]
           else league$sex
  for (sex in sexes) {
    reason <- if (!is.null(stale_sex_map)) {
      stale_reason_map[[paste(key, sex)]]
    } else NULL
    cat(sprintf("  %-30s %-7s [%s]%s\n",
      key, sex, league$pipeline,
      if (!is.null(reason)) paste0("  [stale: ", reason, "]") else ""
    ))
  }
}
cat("\n")

# Helper: resolve sexes for a league key
resolve_sexes <- function(key, league) {
  if (!is.null(arg_sex)) return(arg_sex)
  if (!is.null(stale_sex_map)) return(stale_sex_map[[key]] %||% league$sex)
  league$sex
}

# ── Build step manifest ─────────────────────────────────────────────────────

step_keys <- character(0)

# Build manifest grouped by step type (all data first, then all fit, etc.)
for (step_type in c("data", "fit", "results", "bet", "settle")) {
  if (!step_type %in% steps) next
  for (key in names(selected)) {
    league <- selected[[key]]
    sexes <- resolve_sexes(key, league)
    if (step_type %in% c("bet", "settle")) {
      step_keys <- c(step_keys, paste(step_type, key, sep = "_"))
    } else {
      for (sex in sexes) {
        step_keys <- c(step_keys, paste(step_type, key, sex, sep = "_"))
      }
    }
  }
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
# Custom handler writes to stderr (unbuffered) with live ETA from iteration rate.
if ("fit" %in% steps && requireNamespace("cmdstanr", quietly = TRUE) &&
    exists("register_default_progress_handler", where = asNamespace("cmdstanr")) &&
    requireNamespace("progressr", quietly = TRUE)) {
  options(progressr.enable = TRUE)
  source(here("R", "shared", "stan_progress_handler.R"), local = TRUE)
  progressr::handlers(global = TRUE)
  progressr::handlers(stan_progress_handler(tracker = tracker))
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

# Phase 1: All data steps
if ("data" %in% steps) {
  for (key in names(selected)) {
    league <- selected[[key]]
    sexes <- resolve_sexes(key, league)
    for (sex in sexes) {
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
  }
}

# Phase 2: All fit steps (fit only, no results)
if ("fit" %in% steps) {
  for (key in names(selected)) {
    league <- selected[[key]]
    sexes <- resolve_sexes(key, league)
    for (sex in sexes) {
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
        generate_results = FALSE,
        generate_plots = FALSE,
        expected_duration = cache[[step_key]]
      )
      all_results[[length(all_results) + 1]] <- list(step = "fit", league = key, sex = sex, ok = ok)
      quiet_here(".here")
    }
  }
}

# Phase 3: All results steps (generate posterior CSVs + plots from .rds)
if ("results" %in% steps) {
  for (key in names(selected)) {
    league <- selected[[key]]
    sexes <- resolve_sexes(key, league)
    for (sex in sexes) {
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
}

# Phase 4: All bet steps (once per league, iterates sexes internally)
if ("bet" %in% steps) {
  for (key in names(selected)) {
    league <- selected[[key]]
    step_key <- paste("bet", key, sep = "_")
    bet_res <- NULL
    tracker$start_step(paste("bet:", key), key = step_key)
    ok <- tryCatch({
      bet_res <- run_bet_step(
        league = league,
        sports_dir = sports_dir
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
}

# Phase 5: All settle steps (once per league, checks all sexes)
if ("settle" %in% steps) {
  for (key in names(selected)) {
    league <- selected[[key]]
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
  new_recs <- do.call(rbind, all_recommendations)

  # Daily bankroll budget: cap total exposure across simultaneous matches
  # Reads max_daily_exposure from bankroll.yml (default 0.75 = 75% of bankroll)
  bankroll_yml <- here("config", "bankroll.yml")
  if (file.exists(bankroll_yml)) {
    global_bankroll <- yaml::yaml.load(readr::read_file(bankroll_yml))
    max_daily <- global_bankroll$max_daily_exposure %||% 0.75
    cur_pool <- global_bankroll$initial_pool  # fallback; step_bet computes actual

    if ("kelly" %in% names(new_recs) && "date" %in% names(new_recs)) {
      for (d in unique(as.character(new_recs$date))) {
        day_idx <- as.character(new_recs$date) == d
        total_kelly <- sum(new_recs$kelly[day_idx], na.rm = TRUE)
        if (total_kelly > max_daily) {
          scale_factor <- max_daily / total_kelly
          new_recs$kelly[day_idx] <- new_recs$kelly[day_idx] * scale_factor
          new_recs$bet_amount[day_idx] <- round(
            new_recs$bet_amount[day_idx] * scale_factor,
            global_bankroll$bet_digits %||% 0
          )
          cat(sprintf(
            "  Daily budget: %s total kelly=%.2f > %.2f, scaled by %.2f\n",
            d, total_kelly, max_daily, scale_factor
          ))
        }
      }
    }
  }

  recs_path <- here("recommendations.csv")

  # Merge with existing: replace leagues that were re-run, keep others
  if (file.exists(recs_path)) {
    old_recs <- readr::read_csv(recs_path, show_col_types = FALSE)
    rerun_leagues <- unique(paste(new_recs$sport, new_recs$country))
    kept <- old_recs |>
      dplyr::filter(!paste(sport, country) %in% rerun_leagues)
    recs <- dplyr::bind_rows(kept, new_recs)
  } else {
    recs <- new_recs
  }

  # Drop recommendations for past matches
  recs <- recs |> dplyr::filter(as.Date(date) >= Sys.Date())

  # Re-apply min_bet_amount after daily budget scaling
  if (exists("global_bankroll")) {
    min_bet <- global_bankroll$min_bet_amount %||% 200
    recs <- recs |> dplyr::filter(is.na(bet_amount) | bet_amount >= min_bet)
  }

  readr::write_csv(recs, recs_path)
  cat(sprintf("\nWrote %d recommendation(s) to %s\n", nrow(recs), recs_path))
}

if (n_fail > 0) quit(status = 1)
