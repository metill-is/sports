#### Update all sports models: download data, fit models, generate results ####
#
# Run from Sports/ directory:
#   source("update.R")
#

box::use(
  R / config / basketball_iceland[get_basketball_config = get_config],
  R / config / handball_iceland[get_handball_config = get_config],
  R / shared / model_fitting[fit_model],
  R / shared / get_model_results[generate_model_results],
  R / shared / progress[create_tracker]
)

end_date <- Sys.Date()
sports_dir <- here::here()
tracker <- create_tracker(12)

cat("\n")
cat(strrep("\u2500", 60))
cat("\n Sports Model Update Pipeline\n")
cat(strrep("\u2500", 60))
cat("\n")

# ── Helpers: suppress noisy messages ────────────────────────────────────────

quiet_source <- function(script) {
  suppressPackageStartupMessages(suppressWarnings(
    utils::capture.output(
      utils::capture.output(source(script), type = "message"),
      type = "output"
    )
  ))
  invisible(NULL)
}

quiet_here <- function(...) {
  suppressMessages(here::i_am(...))
}

# ── Basketball ────────────────────────────────────────────────────────────────

basketball_config <- get_basketball_config()

for (sex in c("male", "female")) {
  # Data download
  tracker$start_step(paste("Downloading basketball", sex, "data"))
  dl_ok <- tryCatch(
    {
      withr::with_dir(file.path(sports_dir, "basketball/iceland"), {
        quiet_here("basketball_iceland.Rproj")
        script <- if (sex == "male") "R/prep_data_kk.R" else "R/prep_data_kvk.R"
        quiet_source(script)
      })
      quiet_here(".here")
      TRUE
    },
    error = function(e) {
      quiet_here(".here")
      FALSE
    }
  )
  tracker$end_step(if (dl_ok) "OK" else "FAILED")

  # Model fit
  tracker$start_step(paste("Fitting basketball", sex, "(4 chains \u00d7 1000)"))
  fit_ok <- tryCatch(
    {
      fit_model(
        config = basketball_config,
        sex = sex,
        iter_warmup = 1000,
        iter_sampling = 1000,
        end_date = end_date
      )
      TRUE
    },
    error = function(e) {
      cat(sprintf("\n       Error: %s\n", conditionMessage(e)))
      FALSE
    }
  )
  tracker$end_step(if (fit_ok) "OK" else "FAILED")

  # Results
  tracker$start_step(paste("Generating basketball", sex, "results"))
  res_ok <- tryCatch(
    {
      suppressMessages(generate_model_results(
        config = basketball_config,
        sex = sex,
        end_date = end_date
      ))
      TRUE
    },
    error = function(e) {
      cat(sprintf("\n       Error: %s\n", conditionMessage(e)))
      FALSE
    }
  )
  tracker$end_step(if (res_ok) "OK" else "FAILED")
}

# ── Handball ──────────────────────────────────────────────────────────────────

handball_config <- get_handball_config()

for (sex in c("male", "female")) {
  # Data download
  tracker$start_step(paste("Downloading handball", sex, "data"))
  dl_ok <- tryCatch(
    {
      withr::with_dir(file.path(sports_dir, "handball/iceland"), {
        quiet_here("handball_iceland.Rproj")
        quiet_source(here::here("R", "utils", sex, "download_newest_data_div1.R"))
        quiet_source(here::here("R", "utils", sex, "download_newest_data_div2.R"))
        quiet_source(here::here("R", "utils", sex, "process_data.R"))
      })
      quiet_here(".here")
      TRUE
    },
    error = function(e) {
      quiet_here(".here")
      FALSE
    }
  )
  tracker$end_step(if (dl_ok) "OK" else "FAILED")

  # Model fit
  tracker$start_step(paste("Fitting handball", sex, "(4 chains \u00d7 1000)"))
  fit_ok <- tryCatch(
    {
      fit_model(
        config = handball_config,
        sex = sex,
        iter_warmup = 1000,
        iter_sampling = 1000,
        end_date = end_date
      )
      TRUE
    },
    error = function(e) {
      cat(sprintf("\n       Error: %s\n", conditionMessage(e)))
      FALSE
    }
  )
  tracker$end_step(if (fit_ok) "OK" else "FAILED")

  # Results
  tracker$start_step(paste("Generating handball", sex, "results"))
  res_ok <- tryCatch(
    {
      suppressMessages(generate_model_results(
        config = handball_config,
        sex = sex,
        end_date = end_date
      ))
      TRUE
    },
    error = function(e) {
      cat(sprintf("\n       Error: %s\n", conditionMessage(e)))
      FALSE
    }
  )
  tracker$end_step(if (res_ok) "OK" else "FAILED")
}

# ── Summary ───────────────────────────────────────────────────────────────────

tracker$summary()

expected_figures <- c(
  "next_round_predictions.png",
  "group_table.png",
  "deild_points.png",
  "strengths_table.png",
  "styrkur.png",
  "home_advantage.png"
)

cat("\n Figures:\n")
for (sport_dir in c("basketball/iceland", "handball/iceland")) {
  for (sex in c("male", "female")) {
    fig_dir <- file.path(sports_dir, sport_dir, "results", sex, end_date, "figures")
    figs <- expected_figures
    if (sport_dir == "basketball/iceland") figs <- c(figs, "deild_top.png")

    found <- sum(file.exists(file.path(fig_dir, figs)))
    cat(sprintf("   %s %s: %d/%d\n", sport_dir, sex, found, length(figs)))
  }
}

cat("\nDone!\n")
