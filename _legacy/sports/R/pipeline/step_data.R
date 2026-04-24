#' Step: Data — download/update data for a league
#'
#' Dispatches on league$data_source to call the right existing scripts.
#' Livesport sources prefer reading from the local livesport-data repo clone
#' (../livesport-data/) and fall back to direct scraping if unavailable.
#'
#' @usage
#' box::use(R/pipeline/step_data[run_data_step])
#' run_data_step(league, sex, sports_dir)

#' @param league League config list from leagues.yml
#' @param sex "male" or "female"
#' @param sports_dir Absolute path to Sports/ root
#' @export
run_data_step <- function(league, sex, sports_dir) {
  handler <- switch(league$data_source,
    baskethotel        = data_baskethotel,
    hsi                = data_hsi,
    iceland_ksi        = data_iceland_ksi,
    livesport_football = data_livesport_football,
    livesport_handball = data_livesport_handball,
    stop("Unknown data_source: ", league$data_source)
  )
  handler(league, sex, sports_dir)
}


# ── Helpers ───────────────────────────────────────────────────────────────────

quiet_source <- function(script) {
  env <- new.env(parent = globalenv())
  suppressPackageStartupMessages(suppressWarnings(
    utils::capture.output(
      utils::capture.output(source(script, local = env), type = "message"),
      type = "output"
    )
  ))
  invisible(NULL)
}

quiet_here <- function(...) suppressMessages(here::i_am(...))


# ── Data source: baskethotel (basketball/iceland) ────────────────────────────

data_baskethotel <- function(league, sex, sports_dir) {
  league_dir <- file.path(sports_dir, league$dir)
  withr::with_dir(league_dir, {
    quiet_here(league$rproj %||% ".here")
    script <- if (sex == "male") "R/prep_data_kk.R" else "R/prep_data_kvk.R"
    quiet_source(script)
  })
}


# ── Data source: hsi (handball/iceland) ──────────────────────────────────────

# Sources div1 + div2 (always), then playoffs (both sexes) and cup (male only)
# inside tryCatch. The playoff/cup scrapers call stop() when their HSÍ
# tournament page returns fewer than 2 tables — the normal off-season state.
# Containing those failures here keeps the data step green year-round while
# still refreshing data/{sex}/current_{playoffs,cup}.csv in-season (needed for
# settlement of playoff/cup bets).
data_hsi <- function(league, sex, sports_dir) {
  league_dir <- file.path(sports_dir, league$dir)
  withr::with_dir(league_dir, {
    quiet_here(league$rproj %||% ".here")
    quiet_source(here::here("R", "utils", sex, "download_newest_data_div1.R"))
    quiet_source(here::here("R", "utils", sex, "download_newest_data_div2.R"))
    tryCatch(
      quiet_source(here::here("R", "utils", sex, "download_newest_data_playoffs.R")),
      error = function(e) message("  playoffs skipped (", sex, "): ", conditionMessage(e))
    )
    if (sex == "male") {
      tryCatch(
        quiet_source(here::here("R", "utils", sex, "download_newest_data_cup.R")),
        error = function(e) message("  cup skipped (male): ", conditionMessage(e))
      )
    }
    quiet_source(here::here("R", "utils", sex, "process_data.R"))
  })
}


# ── Data source: iceland_ksi (football/iceland) ─────────────────────────────

data_iceland_ksi <- function(league, sex, sports_dir) {
  league_dir <- file.path(sports_dir, league$dir)
  withr::with_dir(league_dir, {
    quiet_here(league$rproj %||% ".here")
    quiet_source(here::here("R", sex, "update_data.R"))
  })
}


# ── Livesport data repo path ──────────────────────────────────────────────────

livesport_data_dir <- function(sports_dir) {
  file.path(dirname(sports_dir), "livesport-data", "data")
}


# ── Data source: livesport_football (football/{country}) ─────────────────────

data_livesport_football <- function(league, sex, sports_dir) {
  ls_dir <- livesport_data_dir(sports_dir)
  league_dir <- file.path(sports_dir, league$dir)

  if (dir.exists(file.path(ls_dir, "soccer", league$country))) {
    # Sync from livesport-data repo
    sync_livesport_football(league, league_dir, ls_dir)
  } else {
    # Fallback: direct Chromote scraping
    withr::with_dir(league_dir, {
      quiet_here(league$rproj %||% ".here")
      quiet_source("R/update_schedules.R")
    })
  }
}

sync_livesport_football <- function(league, league_dir, ls_dir) {
  country <- league$country
  src_base <- file.path(ls_dir, "soccer", country, "male")
  dst_base <- file.path(league_dir, "data", "male")
  season <- current_season_year()

  league_dirs <- list.dirs(src_base, full.names = FALSE, recursive = FALSE)
  for (lg in league_dirs) {
    # Results → data/male/{league}/{season}/results.csv
    src_results <- file.path(src_base, lg, "results.csv")
    if (file.exists(src_results)) {
      dst_dir <- file.path(dst_base, lg, season)
      if (!dir.exists(dst_dir)) dir.create(dst_dir, recursive = TRUE)
      file.copy(src_results, file.path(dst_dir, "results.csv"), overwrite = TRUE)
    }

    # Schedule → data/male/{league}/schedule.csv
    src_schedule <- file.path(src_base, lg, "schedule.csv")
    if (file.exists(src_schedule)) {
      dst_dir <- file.path(dst_base, lg)
      if (!dir.exists(dst_dir)) dir.create(dst_dir, recursive = TRUE)
      file.copy(src_schedule, file.path(dst_dir, "schedule.csv"), overwrite = TRUE)
    }
  }

  # Run process_data to combine per-league CSVs
  withr::with_dir(league_dir, {
    quiet_here(league$rproj %||% ".here")
    env <- new.env(parent = globalenv())
    tryCatch(
      {
        source(here::here("R", "common", "process_data.R"), local = env)
        env$process_leagues_historical_data()
        env$process_schedule()
      },
      error = function(e) message("  process_data: ", conditionMessage(e))
    )
  })
}


# ── Data source: livesport_handball (handball/other → specific country) ──────

data_livesport_handball <- function(league, sex, sports_dir) {
  ls_dir <- livesport_data_dir(sports_dir)

  if (dir.exists(file.path(ls_dir, "handball"))) {
    # Sync from livesport-data repo
    sync_livesport_handball(league, sex, sports_dir, ls_dir)
  } else {
    # Fallback: direct Chromote scraping
    other_dir <- file.path(sports_dir, "handball", "other")
    withr::with_dir(other_dir, {
      quiet_here("handball_other.Rproj")
      quiet_source(here::here("R", "update_historical_data.R"))
      quiet_source(here::here("R", "update_schedules.R"))
    })
  }
}

sync_livesport_handball <- function(league, sex, sports_dir, ls_dir) {
  country <- league$country
  src_base <- file.path(ls_dir, "handball", country)
  other_dir <- file.path(sports_dir, "handball", "other")
  season <- current_season_year()

  if (!dir.exists(src_base)) {
    return(invisible(NULL))
  }

  # Copy all sex/division combos
  sexes <- list.dirs(src_base, full.names = FALSE, recursive = FALSE)
  for (sx in sexes) {
    divs <- list.dirs(file.path(src_base, sx), full.names = FALSE, recursive = FALSE)
    for (dv in divs) {
      # Results → handball/other/data/{country}/{sex}/{div}/{season}/results.csv
      # (resolves through symlink: handball/{country}/data/...)
      src_results <- file.path(src_base, sx, dv, "results.csv")
      if (file.exists(src_results)) {
        dst_dir <- file.path(other_dir, "data", country, sx, dv, season)
        if (!dir.exists(dst_dir)) dir.create(dst_dir, recursive = TRUE)
        file.copy(src_results, file.path(dst_dir, "results.csv"), overwrite = TRUE)
      }

      # Schedule → handball/other/data/{country}/{sex}/{div}/schedule.csv
      src_schedule <- file.path(src_base, sx, dv, "schedule.csv")
      if (file.exists(src_schedule)) {
        dst_dir <- file.path(other_dir, "data", country, sx, dv)
        if (!dir.exists(dst_dir)) dir.create(dst_dir, recursive = TRUE)
        file.copy(src_schedule, file.path(dst_dir, "schedule.csv"), overwrite = TRUE)
      }
    }
  }

  # Run process_data for this country
  withr::with_dir(other_dir, {
    quiet_here("handball_other.Rproj")
    env <- new.env(parent = globalenv())
    tryCatch(
      {
        source(here::here("R", "utils", "process_data.R"), local = env)
        env$process_leagues_historical_data(
          list(country = country, sexes = sexes)
        )
        for (sx in sexes) {
          env$process_schedule(country)
        }
      },
      error = function(e) message("  process_data: ", conditionMessage(e))
    )
  })
}


# ── Season year helper ────────────────────────────────────────────────────────

current_season_year <- function() {
  today <- Sys.Date()
  yr <- as.integer(format(today, "%Y"))
  mo <- as.integer(format(today, "%m"))
  # Aug-Jul season convention: if before August, season started previous year
  if (mo >= 8) yr else yr - 1L
}
