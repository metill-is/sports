# Shadow-fit the Student-t v3 model alongside production BVP for football_iceland.
#
# Given a prepared stan_data list and the on-disk model_d / pred_d CSVs (from
# prep_data_football.R), this function:
#   1. Compiles + samples the v3 Stan model with audit knobs (4 chains,
#      1000 warmup, 1000 sampling, init=0, adapt_delta=0.95).
#   2. Extracts (total_goals_rep, goal_diff_rep) for in-sample matches and
#      (total_goals_pred, goal_diff_pred) for upcoming matches.
#   3. Joins with model_d.csv / pred_d.csv for match metadata.
#   4. Archives both scopes to variant=v3_free_nu with a scope column.
#
# Invocation:
#   source(here::here("R", "backtest", "betting_pnl", "fit_v3_shadow.R"))
#   fit_v3_shadow(sports_dir = here::here(), sex = "male")

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(cmdstanr)
})

source(here::here("R", "storage", "store.R"))

#' Fit v3 shadow, archive predictions with scope column.
#'
#' @param sports_dir Absolute path to Sports/ root.
#' @param sex "male" or "female".
#' @param fit_date Archive fit_date (default = today).
#' @param seed RNG seed (default: derived from fit_date for reproducibility).
#' @param iter_warmup,iter_sampling,chains cmdstanr knobs (defaults match production).
fit_v3_shadow <- function(sports_dir, sex,
                          fit_date = Sys.Date(),
                          seed = as.integer(as.Date(fit_date)),
                          iter_warmup = 1000, iter_sampling = 1000, chains = 4) {
  league_dir <- file.path(sports_dir, "football", "iceland")

  # Produce stan_data via the shared prep harness.
  source(file.path(sports_dir, "R", "backtest", "student_t_audit", "prep.R"))
  stan_data <- prepare_student_t_audit_data(sports_dir, league = "football/iceland", sex = sex)

  # Compile and sample v3.
  stan_file <- file.path(league_dir, "Stan", "2d_student_t_SD_v3_free_nu.stan")
  mod <- cmdstan_model(stan_file)

  cat("[v3 shadow] sampling: ", iter_warmup, " warmup + ", iter_sampling,
    " sampling x ", chains, " chains, seed=", seed, "\n",
    sep = ""
  )
  fit <- mod$sample(
    data = stan_data,
    seed = seed,
    chains = chains,
    parallel_chains = chains,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    init = 0,
    adapt_delta = 0.95
  )
  cat("[v3 shadow] sampling complete\n")

  # Read match metadata - model_d for in-sample, pred_d for OOS.
  model_d <- readr::read_csv(
    file.path(league_dir, "results", sex, "model_d.csv"),
    show_col_types = FALSE
  ) |>
    mutate(
      match_idx = dplyr::row_number(),
      date = as.character(as.Date(date)),
      match_id = paste(date, home, away, sep = "|"),
      scope = "in_sample"
    ) |>
    select(match_idx, date, home, away, division, home_goals, away_goals, scope)

  pred_d <- readr::read_csv(
    file.path(league_dir, "results", sex, "pred_d.csv"),
    show_col_types = FALSE
  ) |>
    rename(match_idx = game_nr) |>
    mutate(
      date = as.character(as.Date(date)),
      match_id = paste(date, home, away, sep = "|"),
      scope = "oos",
      home_goals = NA_real_,
      away_goals = NA_real_
    ) |>
    select(match_idx, date, home, away, division, home_goals, away_goals, scope)

  # Extract posterior draws for both scopes.
  extract_scope <- function(var_prefix_total, var_prefix_diff, meta_frame) {
    draws <- fit$draws(
      variables = c(var_prefix_total, var_prefix_diff),
      format = "draws_df"
    )
    total_long <- draws |>
      select(.draw, dplyr::starts_with(var_prefix_total)) |>
      pivot_longer(cols = -.draw, names_to = "raw", values_to = "total_goals") |>
      mutate(match_idx = as.integer(sub(".*\\[(\\d+)\\]$", "\\1", raw))) |>
      select(-raw)
    diff_long <- draws |>
      select(.draw, dplyr::starts_with(var_prefix_diff)) |>
      pivot_longer(cols = -.draw, names_to = "raw", values_to = "goal_diff") |>
      mutate(match_idx = as.integer(sub(".*\\[(\\d+)\\]$", "\\1", raw))) |>
      select(-raw)
    inner_join(total_long, diff_long, by = c(".draw", "match_idx")) |>
      inner_join(meta_frame, by = "match_idx") |>
      mutate(
        home_goals_recon = (total_goals + goal_diff) / 2,
        away_goals_recon = (total_goals - goal_diff) / 2,
        iteration = as.integer(.draw)
      ) |>
      select(iteration,
        game_nr = match_idx, division, date, home, away,
        home_goals = home_goals_recon, away_goals = away_goals_recon, scope
      )
  }

  in_sample_df <- extract_scope("total_goals_rep", "goal_diff_rep", model_d)
  oos_df <- extract_scope("total_goals_pred", "goal_diff_pred", pred_d)

  archive_df <- bind_rows(in_sample_df, oos_df)

  cat("[v3 shadow] archiving: in_sample=", nrow(in_sample_df),
    " rows, oos=", nrow(oos_df), " rows\n",
    sep = ""
  )

  archive_predictions(
    df = archive_df,
    sport = "football", country = "iceland", sex = sex,
    sports_dir = sports_dir,
    variant = "v3_free_nu",
    fit_date = fit_date
  )

  # Save the fit object for inspection - same convention as v5/v6 audit fits.
  audit_dir <- file.path(league_dir, "results", sex, "audits", "student_t")
  if (!dir.exists(audit_dir)) dir.create(audit_dir, recursive = TRUE)
  saveRDS(fit, file.path(audit_dir, paste0("fit_v3_shadow_", as.character(fit_date), ".rds")))

  cat("[v3 shadow] done\n")
  invisible(fit)
}
