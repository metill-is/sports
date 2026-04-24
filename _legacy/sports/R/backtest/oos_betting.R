#### Out-of-Sample Betting Backtest ####
#
# Expanding-window OOS evaluation: for each refit date, train on all data
# before that date, predict the next month's matches, and evaluate against
# actual bets from bets_log.csv.
#
# Usage:
#   cd Sports
#   Rscript R/backtest/oos_betting.R --league football_iceland --iter 500

library(tidyverse)
library(here)
library(cmdstanr)
library(posterior)
library(patchwork)
library(scales)
library(nloptr)
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

league <- get_arg("--league", "football_iceland")
iter <- as.integer(get_arg("--iter", "500"))
chains <- as.integer(get_arg("--chains", "4"))

league_parts <- strsplit(league, "_")[[1]]
sport <- league_parts[1]
country <- paste(league_parts[-1], collapse = "_")
league_dir <- here(sport, country)

if (!dir.exists(league_dir)) stop("League directory not found: ", league_dir)

cat(sprintf("OOS betting backtest: %s | %d iterations | %d chains\n",
  league, iter, chains))

#### Source Kelly code ####
sports_dir <- getwd()
source(file.path(sports_dir, "R", "bets", "kelly_joint.R"))
source(file.path(sports_dir, "R", "shared", "model_fitting.R"))

#### Switch to league directory ####
old_wd <- setwd(league_dir)
on.exit(setwd(old_wd))

if (file.exists(".here")) {
  suppressMessages(here::i_am(".here"))
}

#### Load prep_data_football ####
sex <- "male"
env <- new.env(parent = globalenv())
source(file.path(sports_dir, "R", "shared", "prep_data_football.R"), local = env)

#### Load historical bets ####
bets_log <- read_csv(here::here("history", "bets_log.csv"), show_col_types = FALSE)
cat(sprintf("Historical bets: %d rows\n", nrow(bets_log)))

# Filter to supported markets
bets_log <- bets_log |> filter(market %in% c("outcome", "handicap", "totals"))

#### Model paths ####
models <- list(
  poisson = here::here("Stan", "bivariate_poisson_ppc.stan"),
  student_t = here::here("Stan", "2d_student_t_direct_SD.stan")
)

#### Kelly parameters (match in-sample) ####
KELLY_MULT <- 0.10
MAX_STAKE_FRAC <- 0.05
EV_THRESHOLD <- 0.0
TIE_THRESHOLD <- 0.5

#### Output directory ####
out_dir <- file.path(sports_dir, "R", "backtest", "output")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

#### Expanding-window folds ####
refit_dates <- seq(as.Date("2025-04-01"), as.Date("2025-09-01"), by = "month")
n_folds <- length(refit_dates)

cat(sprintf("Folds: %d (%s to %s)\n", n_folds,
  format(refit_dates[1]), format(refit_dates[n_folds])))

#### Helper: convert bets_log row to bet spec ####
make_bet_row <- function(bet, tie_threshold = TIE_THRESHOLD) {
  market <- bet$market
  outcome <- bet$outcome
  odds <- bet$odds
  info <- bet$info

  if (market == "outcome") {
    bt <- paste0("1x2_", outcome)
    tibble(
      bet_type = bt, outcome = outcome, market = "outcome",
      o = odds, booker = "Lengjan",
      change = NA_real_, limit = NA_real_,
      tie_threshold = tie_threshold, hc_threshold = NA_real_
    )
  } else if (market == "handicap") {
    bt <- paste0("hc_", outcome)
    change_val <- as.numeric(info)
    tibble(
      bet_type = bt, outcome = outcome, market = "handicap",
      o = odds, booker = "Lengjan",
      change = change_val, limit = NA_real_,
      tie_threshold = tie_threshold, hc_threshold = tie_threshold
    )
  } else if (market == "totals") {
    bt <- ifelse(outcome == "over", "over", "under")
    limit_val <- as.numeric(info)
    tibble(
      bet_type = bt, outcome = outcome, market = "totals",
      o = odds, booker = "Lengjan",
      change = NA_real_, limit = limit_val,
      tie_threshold = tie_threshold, hc_threshold = NA_real_
    )
  }
}

#### Helper: pivot Stan draws to long format ####
pivot_draws <- function(fit, param) {
  as_draws_df(fit$draws(param)) |>
    as_tibble() |>
    select(-c(.chain, .iteration)) |>
    pivot_longer(-.draw, names_to = "p", values_to = "value") |>
    mutate(game_nr = str_match(p, "\\[(.*)\\]")[, 2] |> as.integer()) |>
    select(.draw, game_nr, value)
}

#### Main loop over folds ####
all_results <- list()

for (fold_idx in seq_along(refit_dates)) {
  refit_date <- refit_dates[fold_idx]
  next_date <- if (fold_idx < n_folds) refit_dates[fold_idx + 1] else refit_date + 31

  cat(sprintf("\n======== Fold %d/%d: train < %s, test [%s, %s) ========\n",
    fold_idx, n_folds, refit_date, refit_date, next_date))

  # Prepare data with expanding window
  prep <- tryCatch(
    suppressMessages(env$prepare_football_data(
      sex, from_season = 2021, test_date_cutoff = refit_date
    )),
    error = function(e) {
      cat(sprintf("  ERROR in data prep: %s\n", e$message))
      NULL
    }
  )
  if (is.null(prep)) next

  stan_data <- prep$stan_data
  test_actuals <- prep$test_actuals
  pred_d <- prep$pred_d

  cat(sprintf("  Train: N=%d games, K=%d teams | Holdout: %d games, Pred: %d\n",
    stan_data$N, stan_data$K, nrow(test_actuals), stan_data$N_pred))

  if (stan_data$N_pred == 0) {
    cat("  No prediction matches (all holdout teams unknown). Skipping.\n")
    next
  }

  # Match bets to holdout matches within [refit_date, next_date)
  fold_bets <- bets_log |>
    filter(
      date_match >= refit_date,
      date_match < next_date
    ) |>
    inner_join(
      test_actuals |>
        select(pred_nr = game_nr, date, home, away,
               home_goals, away_goals),
      by = c("date_match" = "date", "home", "away")
    )

  if (nrow(fold_bets) == 0) {
    cat("  No matching bets in this window. Skipping.\n")
    next
  }

  cat(sprintf("  Matched bets: %d across %d matches\n",
    nrow(fold_bets),
    length(unique(paste(fold_bets$date_match, fold_bets$home, fold_bets$away)))))

  # Fit both models
  for (nm in names(models)) {
    cat(sprintf("  Fitting %s...", nm))
    t0 <- Sys.time()

    fit <- tryCatch({
      model <- cmdstan_model(models[[nm]])
      result <- model$sample(
        data = stan_data,
        chains = chains,
        parallel_chains = chains,
        iter_warmup = iter,
        iter_sampling = iter,
        init = 0,
        refresh = 0,
        show_messages = FALSE,
        show_exceptions = FALSE
      )
      result
    }, error = function(e) {
      cat(sprintf(" FAILED: %s\n", e$message))
      NULL
    })

    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
    if (is.null(fit)) next

    # Check for issues
    diags <- fit$diagnostic_summary(quiet = TRUE)
    n_div <- sum(diags$num_divergent)
    if (n_div > 0) {
      cat(sprintf(" WARNING: %d divergent transitions.", n_div))
    }
    cat(sprintf(" done (%.1f min)\n", elapsed))

    # Extract prediction draws (N_pred indices)
    available <- fit$metadata()$stan_variables

    if (nm == "poisson" && "goals1_pred" %in% available) {
      home_long <- pivot_draws(fit, "goals1_pred") |> rename(home_goals = value)
      away_long <- pivot_draws(fit, "goals2_pred") |> rename(away_goals = value)
      draws_all <- inner_join(home_long, away_long, by = c(".draw", "game_nr"))
    } else if (nm == "student_t" && "total_goals_pred" %in% available) {
      total_long <- pivot_draws(fit, "total_goals_pred") |> rename(total_goals = value)
      diff_long <- pivot_draws(fit, "goal_diff_pred") |> rename(goal_diff = value)
      draws_all <- inner_join(total_long, diff_long, by = c(".draw", "game_nr"))
    } else {
      cat(sprintf("    No prediction variables found for %s. Skipping.\n", nm))
      next
    }

    # Process each match with bets
    bet_pred_nrs <- sort(unique(fold_bets$pred_nr))
    cat(sprintf("    Evaluating %d matches with prediction draws...\n", length(bet_pred_nrs)))

    for (g in bet_pred_nrs) {
      match_bets <- fold_bets |> filter(pred_nr == g)
      match_info <- test_actuals |> filter(game_nr == g)

      # Build bet specification rows
      bet_specs <- map_dfr(seq_len(nrow(match_bets)), \(i) {
        make_bet_row(match_bets[i, ])
      })

      draws <- draws_all |>
        filter(game_nr == g) |>
        select(any_of(c("home_goals", "away_goals", "total_goals", "goal_diff")))

      if (nrow(draws) == 0) next

      # Build return matrix and run Kelly
      net_return <- build_return_matrix(draws, bet_specs)
      p_win <- colMeans(net_return > 0)
      kelly_result <- get_kelly_joint(net_return = net_return, max_stake = 1.0)
      fracs <- kelly_result$solution

      # Record results
      for (i in seq_len(nrow(match_bets))) {
        bet <- match_bets[i, ]
        prob <- p_win[i]
        kelly_f <- fracs[i]
        ev_val <- prob * (bet$odds - 1) - (1 - prob)

        # Kelly scaling
        do_bet <- kelly_f > 1e-4 & ev_val > EV_THRESHOLD
        stake <- if (do_bet) pmin(kelly_f * KELLY_MULT, MAX_STAKE_FRAC) else 0
        pnl_per_unit <- ifelse(bet$win, bet$odds - 1, -1)

        # Log-score: log(p) if win, log(1-p) if loss
        log_score <- if (bet$win) log(max(prob, 1e-10)) else log(max(1 - prob, 1e-10))

        all_results <- c(all_results, list(tibble(
          fold_date = refit_date,
          model = nm,
          date_match = bet$date_match,
          home = match_info$home,
          away = match_info$away,
          market = bet$market,
          outcome = bet$outcome,
          odds = bet$odds,
          probability = prob,
          ev = ev_val,
          kelly_frac = kelly_f,
          stake_frac = stake,
          win = bet$win,
          pnl = stake * pnl_per_unit,
          log_score = log_score
        )))
      }
    }
  }
}

#### Collect results ####
if (length(all_results) == 0) {
  cat("\nNo results produced across any fold. Exiting.\n")
  quit(save = "no", status = 0)
}

results_df <- bind_rows(all_results)
write_csv(results_df, file.path(out_dir, "oos_results.csv"))
cat(sprintf("\n=== Results: %d bet evaluations saved to oos_results.csv ===\n",
  nrow(results_df)))

#### Summary table ####
cat("\n=== OOS Betting Summary ===\n")

summary_df <- results_df |>
  filter(stake_frac > 0) |>
  summarise(
    n_bets = n(),
    win_rate = mean(win),
    mean_ev = mean(ev),
    total_staked = sum(stake_frac),
    total_pnl = sum(pnl),
    roi = total_pnl / total_staked,
    mean_log_score = mean(log_score),
    .by = model
  )

for (nm in unique(summary_df$model)) {
  s <- summary_df |> filter(model == nm)
  cat(sprintf("\n  %s:\n", nm))
  cat(sprintf("    Bets: %d, Win rate: %.1f%%\n", s$n_bets, s$win_rate * 100))
  cat(sprintf("    Mean EV: %.3f, Total PnL: %.4f, ROI: %.1f%%\n",
    s$mean_ev, s$total_pnl, s$roi * 100))
  cat(sprintf("    Mean log-score: %.4f\n", s$mean_log_score))
}

# Per-fold summary
cat("\n=== Per-Fold Summary ===\n")
fold_summary <- results_df |>
  filter(stake_frac > 0) |>
  summarise(
    n_bets = n(),
    win_rate = mean(win),
    total_pnl = sum(pnl),
    .by = c(fold_date, model)
  ) |>
  arrange(fold_date, model)

print(fold_summary, width = 120)

#### Plots ####
cat("\n=== Generating plots ===\n")

model_colours <- c(poisson = "#2c5aa0", student_t = "#c44e52")
model_labels <- c(poisson = "Poisson (tvíþætt)", student_t = "Student-t (beint S,D)")

#### Plot 1: Cumulative OOS PnL by date ####
p_pnl <- results_df |>
  filter(stake_frac > 0) |>
  arrange(date_match) |>
  mutate(cum_pnl = cumsum(pnl), .by = model) |>
  ggplot(aes(date_match, cum_pnl, colour = model)) +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.5) +
  geom_step(linewidth = 0.8) +
  scale_colour_manual(values = model_colours, labels = model_labels) +
  scale_y_continuous(labels = label_percent(accuracy = 0.1)) +
  labs(
    title = "Uppsöfnuð PnL — out-of-sample veðmálaprófun",
    subtitle = sprintf("Kelly×%.0f%%, hámark %.0f%% á veðmál | %d veðmál (Poisson), %d (Student-t)",
      KELLY_MULT * 100, MAX_STAKE_FRAC * 100,
      sum(results_df$stake_frac > 0 & results_df$model == "poisson"),
      sum(results_df$stake_frac > 0 & results_df$model == "student_t")),
    x = NULL, y = "Uppsöfnuð PnL (hlutfall bankroll)",
    colour = NULL
  ) +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "oos_cumulative_pnl.png"),
  p_pnl, width = 10, height = 6, dpi = 150, bg = "white")
cat("  Saved oos_cumulative_pnl.png\n")

#### Plot 2: ROI by market ####
market_pnl <- results_df |>
  filter(stake_frac > 0) |>
  summarise(
    total_pnl = sum(pnl),
    total_staked = sum(stake_frac),
    roi = total_pnl / total_staked,
    n_bets = n(),
    .by = c(model, market)
  )

p_market <- market_pnl |>
  ggplot(aes(market, roi, fill = model)) +
  geom_col(position = "dodge", alpha = 0.7) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_fill_manual(values = model_colours, labels = model_labels) +
  scale_y_continuous(labels = label_percent()) +
  labs(
    title = "Arðsemi eftir markaði — out-of-sample",
    x = NULL, y = "ROI", fill = NULL
  ) +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "oos_roi_by_market.png"),
  p_market, width = 8, height = 5, dpi = 150, bg = "white")
cat("  Saved oos_roi_by_market.png\n")

#### Plot 3: Calibration ####
calibration_df <- results_df |>
  filter(stake_frac > 0) |>
  mutate(prob_bin = cut(probability, breaks = seq(0, 1, by = 0.2),
    include.lowest = TRUE)) |>
  summarise(
    mean_prob = mean(probability),
    observed_rate = mean(win),
    n = n(),
    .by = c(model, prob_bin)
  ) |>
  filter(!is.na(prob_bin))

p_cal <- calibration_df |>
  ggplot(aes(mean_prob, observed_rate, colour = model, size = n)) +
  geom_abline(intercept = 0, slope = 1, lty = 2, alpha = 0.5) +
  geom_point(alpha = 0.7) +
  scale_colour_manual(values = model_colours, labels = model_labels) +
  scale_size_continuous(range = c(2, 8)) +
  scale_x_continuous(labels = label_percent(), limits = c(0, 1)) +
  scale_y_continuous(labels = label_percent(), limits = c(0, 1)) +
  coord_equal() +
  labs(
    title = "Kvörðun líkana — spáð líkindi vs. raunveruleg tíðni",
    subtitle = "Out-of-sample, 5 hólf",
    x = "Meðaltal spáðra líkinda", y = "Raunveruleg vinningshlutfall",
    colour = NULL, size = "Fjöldi"
  ) +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "oos_calibration.png"),
  p_cal, width = 8, height = 7, dpi = 150, bg = "white")
cat("  Saved oos_calibration.png\n")

#### Plot 4: PnL per fold ####
fold_pnl <- results_df |>
  filter(stake_frac > 0) |>
  summarise(
    total_pnl = sum(pnl),
    n_bets = n(),
    .by = c(fold_date, model)
  )

p_fold <- fold_pnl |>
  ggplot(aes(factor(fold_date), total_pnl, fill = model)) +
  geom_col(position = "dodge", alpha = 0.7) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_fill_manual(values = model_colours, labels = model_labels) +
  scale_y_continuous(labels = label_percent(accuracy = 0.01)) +
  labs(
    title = "PnL eftir mánuði — out-of-sample",
    subtitle = "Hvert fold: þjálfun á öllu fyrir tímabil, próf á næsta mánuði",
    x = "Endurþjálfunardagsetning", y = "PnL (hlutfall bankroll)",
    fill = NULL
  ) +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "oos_fold_summary.png"),
  p_fold, width = 10, height = 5, dpi = 150, bg = "white")
cat("  Saved oos_fold_summary.png\n")

cat(sprintf("\nDone. Results in %s\n", out_dir))
