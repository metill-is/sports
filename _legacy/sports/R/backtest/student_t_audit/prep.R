#' Shared data prep for the Student-t football audit.
#'
#' Wraps `R/shared/prep_data_football.R::prepare_football_data()` with the
#' same `withr::with_dir()` dance as the production football pipeline
#' (`R/pipeline/step_fit.R::fit_football`). Reset `here()` back to the
#' Sports root after prep so Stan paths resolve correctly.
#'
#' Returns a `stan_data` list ready to pass to `cmdstan_model$sample()`.

suppressPackageStartupMessages({
  library(withr)
})

#' @param sports_dir Absolute path to Sports/ root.
#' @param league Subdirectory relative to Sports/ (default "football/iceland").
#' @param sex "male" or "female".
#' @return list with stan_data
prepare_student_t_audit_data <- function(sports_dir, league = "football/iceland", sex = "male") {
  league_dir <- file.path(sports_dir, league)

  stan_data <- with_dir(league_dir, {
    here::i_am(".here")
    env <- new.env(parent = globalenv())
    source(file.path(sports_dir, "R", "shared", "prep_data_football.R"),
      local = env
    )
    suppressMessages(env$prepare_football_data(sex = sex))
  })

  # Reset here() back to Sports root for downstream Stan path resolution.
  here::i_am("run.R")

  stan_data
}
