#' Step: Data — download/update data for a league
#'
#' Dispatches on league$data_source to call the right existing scripts.
#'
#' @usage
#' box::use(R/pipeline/step_data[run_data_step])
#' run_data_step(league, sex, sports_dir)

#' @param league League config list from leagues.yml
#' @param sex "male" or "female"
#' @param sports_dir Absolute path to Sports/ root
#' @export
run_data_step <- function(league, sex, sports_dir) {
  handler <- switch(
    league$data_source,
    baskethotel        = data_baskethotel,
    hsi                = data_hsi,
    livesport_football = data_livesport_football,
    livesport_handball = data_livesport_handball,
    stop("Unknown data_source: ", league$data_source)
  )
  handler(league, sex, sports_dir)
}


# ── Helpers ───────────────────────────────────────────────────────────────────

quiet_source <- function(script) {
  suppressPackageStartupMessages(suppressWarnings(
    utils::capture.output(
      utils::capture.output(source(script), type = "message"),
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

data_hsi <- function(league, sex, sports_dir) {
  league_dir <- file.path(sports_dir, league$dir)
  withr::with_dir(league_dir, {
    quiet_here(league$rproj %||% ".here")
    quiet_source(here::here("R", "utils", sex, "download_newest_data_div1.R"))
    quiet_source(here::here("R", "utils", sex, "download_newest_data_div2.R"))
    quiet_source(here::here("R", "utils", sex, "process_data.R"))
  })
}


# ── Data source: livesport_football (football/{country}) ─────────────────────

data_livesport_football <- function(league, sex, sports_dir) {
  league_dir <- file.path(sports_dir, league$dir)
  withr::with_dir(league_dir, {
    quiet_here(league$rproj %||% ".here")
    quiet_source("R/update_schedules.R")
  })
}


# ── Data source: livesport_handball (handball/other → specific country) ──────

data_livesport_handball <- function(league, sex, sports_dir) {
  # European handball data is managed through handball/other/
  other_dir <- file.path(sports_dir, "handball", "other")
  withr::with_dir(other_dir, {
    quiet_here("handball_other.Rproj")
    # Download + update for all countries (the scripts handle all countries)
    quiet_source(here::here("R", "update_historical_data.R"))
    quiet_source(here::here("R", "update_schedules.R"))
  })
}
