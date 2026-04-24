#### Football Model Backtest ####
#
# Compares diagonal-inflated bivariate Poisson vs bivariate Student-t
# on held-out data using a train/test split.
#
# Usage:
#   cd Sports
#   Rscript R/backtest/backtest_football.R --league football_england --iter 500
#   Rscript R/backtest/backtest_football.R --league football_england --iter 1000 --chains 4

library(tidyverse)
library(here)
library(cmdstanr)
library(posterior)
library(metill)
theme_set(theme_metill())
Sys.setlocale("LC_ALL", "is_IS.UTF-8")

#### CLI Arguments ####
args <- commandArgs(trailingOnly = TRUE)

get_arg <- function(flag, default) {
  idx <- match(flag, args)
  if (is.na(idx) || idx >= length(args)) return(default)
  args[idx + 1]
}

league <- get_arg("--league", "football_england")
iter <- as.integer(get_arg("--iter", "500"))
chains <- as.integer(get_arg("--chains", "2"))
train_frac <- as.numeric(get_arg("--train-frac", "0.6"))

# Resolve league directory
league_parts <- strsplit(league, "_")[[1]]
sport <- league_parts[1]
country <- paste(league_parts[-1], collapse = "_")
league_dir <- here(sport, country)

if (!dir.exists(league_dir)) stop("League directory not found: ", league_dir)

cat(sprintf(
  "Backtest: %s | %d iterations | %d chains | %.0f%% train\n",
  league, iter, chains, train_frac * 100
))

#### Compute cutoff date ####
sex <- "male"
d <- read_csv(file.path(league_dir, "data", sex, "data.csv"), show_col_types = FALSE)
# Support Icelandic column names
if ("dags" %in% names(d)) d <- rename(d, date = dags)
n_train <- floor(nrow(d) * train_frac)
cutoff_date <- sort(d$date)[n_train]

cat(sprintf(
  "Data: %d games | Cutoff: %s | Train: %d | Test: ~%d\n",
  nrow(d), cutoff_date, n_train, nrow(d) - n_train
))

#### Run inside league directory ####
sports_dir <- getwd()
old_wd <- setwd(league_dir)
on.exit(setwd(old_wd))

# Resolve project root — prefer .Rproj, fall back to .here
rproj <- paste0(league, ".Rproj")
if (file.exists(rproj)) {
  suppressMessages(here::i_am(rproj))
} else if (file.exists(".here")) {
  suppressMessages(here::i_am(".here"))
} else {
  stop("No .Rproj or .here file found in ", league_dir)
}

#### Prepare data with backtest split ####
env <- new.env(parent = globalenv())
source(file.path(sports_dir, "R", "shared", "prep_data_football.R"), local = env)
prep <- suppressMessages(env$prepare_football_data(sex, test_date_cutoff = cutoff_date))

stan_data <- prep$stan_data
teams <- prep$teams
test_actuals <- prep$test_actuals

# pred_d for extract_posterior_goals must not have home_goals/away_goals
# (those are posterior draw column names)
pred_d <- prep$pred_d |>
  select(-any_of(c("home_goals", "away_goals")))

cat(sprintf(
  "Stan data: N=%d (train), N_pred=%d (test), K=%d teams\n",
  stan_data$N, stan_data$N_pred, stan_data$K
))

#### Define models ####
models <- list(
  poisson = here::here("Stan", "bivariate_poisson_inflated_diagonal_corrmodel.stan"),
  student_t = here::here("Stan", "2d_student_t_totalgoals-v-goaldiff.stan")
)

for (nm in names(models)) {
  if (!file.exists(models[[nm]])) {
    stop(sprintf("Stan model not found: %s (%s)", nm, models[[nm]]))
  }
}

#### Fit both models ####
source(file.path(sports_dir, "R", "shared", "model_fitting.R"))
source(file.path(sports_dir, "R", "shared", "extract_posterior.R"))

out_dir <- here::here("results", "backtest")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

fits <- list()
posteriors <- list()

for (nm in names(models)) {
  cat(sprintf("\n=== Fitting %s model ===\n", nm))

  fit_path <- file.path(out_dir, paste0("fit_", nm, ".rds"))

  fit_model(
    stan_data = stan_data,
    stan_model_path = models[[nm]],
    output_path = fit_path,
    chains = chains,
    parallel_chains = chains,
    iter_warmup = iter,
    iter_sampling = iter,
    init = 0
  )

  fit <- readRDS(fit_path)
  fits[[nm]] <- fit

  post <- extract_posterior_goals(fit, pred_d) |>
    select(
      iteration = .draw,
      game_nr,
      division,
      date,
      home,
      away,
      home_goals,
      away_goals
    )

  posteriors[[nm]] <- post
  write_csv(post, file.path(out_dir, paste0("posterior_", nm, ".csv")))
  cat(sprintf("  %s: %d draws x %d games\n", nm, max(post$iteration), stan_data$N_pred))
}

#### Compute metrics ####
cat("\n=== Computing metrics ===\n")

compute_metrics <- function(post, actuals, model_name) {
  # 1x2 outcome probabilities per game
  outcome_probs <- post |>
    mutate(
      pred_outcome = case_when(
        home_goals > away_goals ~ "home",
        home_goals < away_goals ~ "away",
        TRUE ~ "draw"
      )
    ) |>
    summarise(
      p_home = mean(pred_outcome == "home"),
      p_draw = mean(pred_outcome == "draw"),
      p_away = mean(pred_outcome == "away"),
      .by = c(game_nr, date, home, away)
    ) |>
    inner_join(
      actuals |>
        mutate(
          actual_outcome = case_when(
            home_goals > away_goals ~ "home",
            home_goals < away_goals ~ "away",
            TRUE ~ "draw"
          )
        ) |>
        select(game_nr, date, home, away, actual_outcome,
               actual_home_goals = home_goals,
               actual_away_goals = away_goals),
      by = c("game_nr", "date", "home", "away")
    )

  # Log-score: log(p_actual_outcome)
  outcome_probs <- outcome_probs |>
    mutate(
      p_actual = case_when(
        actual_outcome == "home" ~ p_home,
        actual_outcome == "draw" ~ p_draw,
        actual_outcome == "away" ~ p_away
      ),
      log_score = log(pmax(p_actual, 1e-10))
    )

  # Brier score per outcome class
  outcome_probs <- outcome_probs |>
    mutate(
      brier_home = (p_home - (actual_outcome == "home"))^2,
      brier_draw = (p_draw - (actual_outcome == "draw"))^2,
      brier_away = (p_away - (actual_outcome == "away"))^2,
      brier = brier_home + brier_draw + brier_away
    )

  # PIT values: P(observed < predicted) for continuous-ish quantities
  pit <- post |>
    mutate(
      goal_diff = home_goals - away_goals,
      total_goals = home_goals + away_goals
    ) |>
    inner_join(
      actuals |>
        mutate(
          obs_diff = home_goals - away_goals,
          obs_total = home_goals + away_goals
        ) |>
        select(game_nr, obs_diff, obs_total,
               obs_home = home_goals, obs_away = away_goals),
      by = "game_nr"
    ) |>
    summarise(
      pit_diff = mean(goal_diff < obs_diff) +
        0.5 * mean(goal_diff == obs_diff),
      pit_total = mean(total_goals < obs_total) +
        0.5 * mean(total_goals == obs_total),
      pit_home = mean(home_goals < obs_home) +
        0.5 * mean(home_goals == obs_home),
      pit_away = mean(away_goals < obs_away) +
        0.5 * mean(away_goals == obs_away),
      .by = game_nr
    )

  # Coverage: fraction within X% intervals
  coverage_levels <- c(0.10, 0.50, 0.90)
  coverage <- post |>
    mutate(goal_diff = home_goals - away_goals) |>
    inner_join(
      actuals |>
        mutate(obs_diff = home_goals - away_goals) |>
        select(game_nr, obs_diff),
      by = "game_nr"
    ) |>
    reframe(
      nominal = coverage_levels,
      lower = quantile(goal_diff, 0.5 - nominal / 2),
      upper = quantile(goal_diff, 0.5 + nominal / 2),
      obs = first(obs_diff),
      .by = game_nr
    ) |>
    summarise(
      empirical = mean(obs >= lower & obs <= upper),
      .by = nominal
    )

  list(
    model = model_name,
    outcome_probs = outcome_probs,
    pit = pit,
    coverage = coverage,
    mean_log_score = mean(outcome_probs$log_score),
    mean_brier = mean(outcome_probs$brier)
  )
}

metrics <- list()
for (nm in names(posteriors)) {
  metrics[[nm]] <- compute_metrics(posteriors[[nm]], test_actuals, nm)
  cat(sprintf(
    "  %s: mean log-score = %.4f, mean Brier = %.4f\n",
    nm, metrics[[nm]]$mean_log_score, metrics[[nm]]$mean_brier
  ))
}

# Save summary
summary_df <- tibble(
  model = names(metrics),
  mean_log_score = map_dbl(metrics, "mean_log_score"),
  mean_brier = map_dbl(metrics, "mean_brier")
) |>
  bind_cols(
    map_dfr(metrics, \(m) {
      m$coverage |>
        mutate(label = paste0("coverage_", nominal * 100, "pct")) |>
        select(label, empirical) |>
        pivot_wider(names_from = label, values_from = empirical)
    })
  )

write_csv(summary_df, file.path(out_dir, "summary.csv"))
cat("\nSummary:\n")
print(summary_df)

# Save per-game metrics for plotting
for (nm in names(metrics)) {
  write_csv(
    metrics[[nm]]$outcome_probs,
    file.path(out_dir, paste0("outcome_probs_", nm, ".csv"))
  )
  write_csv(
    metrics[[nm]]$pit,
    file.path(out_dir, paste0("pit_", nm, ".csv"))
  )
}

#### Generate calibration plots ####
cat("\n=== Generating calibration plots ===\n")
source(file.path(sports_dir, "R", "backtest", "plot_backtest.R"))
plot_backtest(metrics, out_dir)

#### Generate model comparison plots ####
cat("\n=== Generating model comparison plots ===\n")
source(file.path(sports_dir, "R", "backtest", "plot_model_comparison.R"))
plot_model_comparison(posteriors, fits, metrics, test_actuals, teams, out_dir)

cat(sprintf("\nDone. Results in %s\n", out_dir))
