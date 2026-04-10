#### In-Sample Betting Backtest ####
#
# Uses in-sample posterior draws from each model to replay historical bets.
# Same odds, same markets — only the model's probability estimates differ.
#
# Usage:
#   cd Sports
#   Rscript R/backtest/insample_betting.R --league football_iceland --iter 500

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

cat(sprintf("In-sample betting backtest: %s | %d iterations | %d chains\n",
  league, iter, chains))

#### Source Kelly code ####
sports_dir <- getwd()
source(file.path(sports_dir, "R", "bets", "kelly_joint.R"))

#### Prepare data (full training set) ####
old_wd <- setwd(league_dir)
on.exit(setwd(old_wd))

if (file.exists(".here")) {
  suppressMessages(here::i_am(".here"))
}

sex <- "male"
env <- new.env(parent = globalenv())
source(file.path(sports_dir, "R", "shared", "prep_data_football.R"), local = env)

# Full data, no holdout — we want in-sample reps for all matches
prep <- suppressMessages(env$prepare_football_data(sex, test_date_cutoff = "2099-01-01"))
stan_data <- prep$stan_data

cat(sprintf("Data: N=%d games, K=%d teams\n", stan_data$N, stan_data$K))

# Training data (matches the stan_data)
d <- read_csv(here::here("data", sex, "data.csv"), show_col_types = FALSE)
if ("dags" %in% names(d)) {
  d <- rename(d, season = timabil, date = dags, home = heima, away = gestir,
              home_goals = stig_heima, away_goals = stig_gestir)
}
d <- d |>
  filter(season >= 2021) |>
  arrange(date) |>
  mutate(game_nr = row_number())

#### Load historical bets ####
bets_log <- read_csv(here::here("history", "bets_log.csv"), show_col_types = FALSE)
cat(sprintf("Historical bets: %d rows, %d unique matches\n",
  nrow(bets_log), length(unique(paste(bets_log$date_match, bets_log$home, bets_log$away)))))

# Sex-filtering diagnostics
cat(sprintf("Bets by sex: male=%d, female=%d\n",
  sum(bets_log$sex == "male"), sum(bets_log$sex == "female")))

n_original <- nrow(bets_log)
n_original_matches <- length(unique(paste(bets_log$date_match, bets_log$home, bets_log$away)))

# Match bets to training data game_nr
bets_log <- bets_log |>
  inner_join(
    d |> select(game_nr, date, home, away),
    by = c("date_match" = "date", "home", "away")
  )
n_matched_matches <- length(unique(paste(bets_log$date_match, bets_log$home, bets_log$away)))
cat(sprintf("Matched bets: %d (of %d) | Matched matches: %d (of %d)\n",
  nrow(bets_log), n_original, n_matched_matches, n_original_matches))

# Filter to supported markets (drop btts — not in our Stan models)
bets_log <- bets_log |> filter(market %in% c("outcome", "handicap", "totals"))
cat(sprintf("After market filter: %d bets across %d matches\n",
  nrow(bets_log), length(unique(bets_log$game_nr))))

#### Define and fit models ####
models <- list(
  poisson = here::here("Stan", "bivariate_poisson_ppc.stan"),
  student_t = here::here("Stan", "2d_student_t_direct_SD.stan")
)

out_dir <- here::here("results", "insample_betting")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

source(file.path(sports_dir, "R", "shared", "model_fitting.R"))

fits <- list()
for (nm in names(models)) {
  cat(sprintf("\n=== Fitting %s model ===\n", nm))
  fit_path <- file.path(out_dir, paste0("fit_", nm, ".rds"))

  # Reuse existing fit if available
  if (file.exists(fit_path)) {
    cat("  Using cached fit\n")
  } else {
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
  }
  fits[[nm]] <- readRDS(fit_path)
  cat(sprintf("  %s: done\n", nm))
}

#### Extract per-match posterior draws ####
cat("\n=== Extracting posterior draws ===\n")

# Helper to pivot Stan draws to long format
pivot_draws <- function(fit, param) {
  as_draws_df(fit$draws(param)) |>
    as_tibble() |>
    select(-c(.chain, .iteration)) |>
    pivot_longer(-.draw, names_to = "p", values_to = "value") |>
    mutate(game_nr = str_match(p, "\\[(.*)\\]")[, 2] |> as.integer()) |>
    select(.draw, game_nr, value)
}

# Only extract draws for matches that have bets
bet_games <- sort(unique(bets_log$game_nr))
cat(sprintf("  Extracting draws for %d matches with bets\n", length(bet_games)))

model_draws <- list()
for (nm in names(fits)) {
  fit <- fits[[nm]]
  available <- fit$metadata()$stan_variables

  if ("goals1_rep" %in% available) {
    # Poisson model: has individual goals
    home_long <- pivot_draws(fit, "goals1_rep") |> rename(home_goals = value)
    away_long <- pivot_draws(fit, "goals2_rep") |> rename(away_goals = value)
    draws_all <- inner_join(home_long, away_long, by = c(".draw", "game_nr"))
  } else {
    # Student-t model: total_goals_rep (int) + goal_diff_rep (continuous)
    # Pass (total_goals, goal_diff) directly — build_return_matrix accepts these natively
    total_long <- pivot_draws(fit, "total_goals_rep") |> rename(total_goals = value)
    diff_long <- pivot_draws(fit, "goal_diff_rep") |> rename(goal_diff = value)
    draws_all <- inner_join(total_long, diff_long, by = c(".draw", "game_nr"))
  }

  # Filter to bet matches only
  model_draws[[nm]] <- draws_all |> filter(game_nr %in% bet_games)
  cat(sprintf("  %s: %d draws x %d matches\n", nm,
    max(model_draws[[nm]]$.draw), length(unique(model_draws[[nm]]$game_nr))))
}

#### Run Kelly on each bet using each model ####
cat("\n=== Computing Kelly fractions ===\n")

# Football tie threshold: 0.5 works for both discrete (Poisson) and continuous (Student-t)
# For Poisson: D > 0.5 ≡ D >= 1 (home win). |D| <= 0.5 ≡ D == 0 (draw).
# For Student-t: D > 0.5 = home win. |D| <= 0.5 = draw region.
TIE_THRESHOLD <- 0.5

# Convert bets_log rows to the format expected by build_return_matrix
make_bet_row <- function(bet, tie_threshold = TIE_THRESHOLD) {
  market <- bet$market
  outcome <- bet$outcome
  odds <- bet$odds
  info <- bet$info  # handicap change or totals limit

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

# Process each match: collect all bets, run joint Kelly
results <- list()

for (g in bet_games) {
  match_bets <- bets_log |> filter(game_nr == g)
  match_info <- d |> filter(game_nr == g)

  # Build bet specification rows
  bet_specs <- map_dfr(seq_len(nrow(match_bets)), \(i) {
    make_bet_row(match_bets[i, ])
  })

  for (nm in names(model_draws)) {
    draws <- model_draws[[nm]] |>
      filter(game_nr == g) |>
      select(any_of(c("home_goals", "away_goals", "total_goals", "goal_diff")))
    if (nrow(draws) == 0) next

    # Build return matrix
    net_return <- build_return_matrix(draws, bet_specs)

    # Posterior win probabilities
    p_win <- colMeans(net_return > 0)

    # Run joint Kelly (max_stake = 1.0, raw fractions)
    kelly_result <- get_kelly_joint(net_return = net_return, max_stake = 1.0)
    fracs <- kelly_result$solution

    # Record results
    for (i in seq_len(nrow(match_bets))) {
      bet <- match_bets[i, ]
      results <- c(results, list(tibble(
        model = nm,
        game_nr = g,
        date = match_info$date,
        home = match_info$home,
        away = match_info$away,
        market = bet$market,
        outcome = bet$outcome,
        odds = bet$odds,
        info = bet$info,
        p_model = p_win[i],
        p_implied = 1 / bet$odds,
        kelly = fracs[i],
        ev = p_win[i] * (bet$odds - 1) - (1 - p_win[i]),
        actual_win = bet$win,
        actual_pnl_per_unit = ifelse(bet$win, bet$odds - 1, -1)
      )))
    }
  }
}

results_df <- bind_rows(results)
cat(sprintf("  Total bet evaluations: %d (%d per model)\n",
  nrow(results_df), nrow(results_df) / length(models)))

#### Apply Kelly scaling and compute PnL ####
# Use a fixed kelly_frac multiplier (e.g., 0.10) and fixed bankroll
KELLY_FRAC <- 0.10
INITIAL_BANKROLL <- 10000

# Filter to positive-EV bets only (same as production pipeline)
EV_THRESHOLD <- 0.00

pnl_df <- results_df |>
  mutate(
    # Only bet when Kelly > 0 and EV > threshold
    bet = kelly > 1e-4 & ev > EV_THRESHOLD,
    # Scale Kelly fraction
    stake_frac = ifelse(bet, kelly * KELLY_FRAC, 0),
    # Cap individual bet at 5% of bankroll
    stake_frac = pmin(stake_frac, 0.05)
  )

# Compute cumulative PnL chronologically
pnl_df <- pnl_df |>
  arrange(date, game_nr) |>
  mutate(
    pnl_per_unit = ifelse(bet, actual_pnl_per_unit * stake_frac, 0),
    cum_pnl = cumsum(pnl_per_unit),
    .by = model
  )

write_csv(pnl_df, file.path(out_dir, "betting_results.csv"))

#### Summary statistics ####
cat("\n=== Betting Summary ===\n")

summary_df <- pnl_df |>
  filter(bet) |>
  summarise(
    n_bets = n(),
    n_wins = sum(actual_win),
    win_rate = mean(actual_win),
    mean_odds = mean(odds),
    mean_kelly = mean(kelly),
    mean_ev = mean(ev),
    total_staked = sum(stake_frac),
    total_pnl = sum(pnl_per_unit),
    roi = total_pnl / total_staked,
    .by = model
  )

for (nm in unique(summary_df$model)) {
  s <- summary_df |> filter(model == nm)
  cat(sprintf("\n  %s:\n", nm))
  cat(sprintf("    Bets: %d (of %d available), Win rate: %.1f%%\n",
    s$n_bets, nrow(bets_log), s$win_rate * 100))
  cat(sprintf("    Mean odds: %.2f, Mean Kelly: %.4f, Mean EV: %.3f\n",
    s$mean_odds, s$mean_kelly, s$mean_ev))
  cat(sprintf("    Total staked: %.2f units, Total PnL: %.4f units, ROI: %.1f%%\n",
    s$total_staked, s$total_pnl, s$roi * 100))
}

# Per-market breakdown
cat("\n=== Per-Market Breakdown ===\n")
market_summary <- pnl_df |>
  filter(bet) |>
  summarise(
    n_bets = n(),
    win_rate = mean(actual_win),
    mean_ev = mean(ev),
    total_pnl = sum(pnl_per_unit),
    roi = total_pnl / sum(stake_frac),
    .by = c(model, market)
  ) |>
  arrange(model, market)

print(market_summary, width = 120)

#### Plots ####
cat("\n=== Generating plots ===\n")

model_colours <- c(poisson = "#2c5aa0", student_t = "#c44e52")
model_labels <- c(poisson = "Poisson (tvíþætt)", student_t = "Student-t (beint S,D)")

#### Plot 1: Cumulative PnL ####
p_pnl <- pnl_df |>
  filter(bet) |>
  mutate(bet_nr = row_number(), .by = model) |>
  ggplot(aes(bet_nr, cum_pnl, colour = model)) +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.5) +
  geom_step(linewidth = 0.8) +
  scale_colour_manual(values = model_colours, labels = model_labels) +
  scale_y_continuous(labels = label_percent(accuracy = 0.1)) +
  labs(
    title = "Uppsöfnuð hagnaður/tap — in-sample veðmálaprófun",
    subtitle = sprintf("Kelly×%.0f%%, hámark 5%% á veðmál, N=%d leikir",
      KELLY_FRAC * 100, length(bet_games)),
    x = "Veðmálanúmer", y = "Uppsöfnuð PnL (hlutfall bankroll)",
    colour = NULL
  ) +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "cumulative_pnl.png"),
  p_pnl, width = 10, height = 6, dpi = 150, bg = "white")
cat("  Saved cumulative_pnl.png\n")

#### Plot 2: PnL by date ####
daily_pnl <- pnl_df |>
  filter(bet) |>
  summarise(
    daily_pnl = sum(pnl_per_unit),
    n_bets = n(),
    .by = c(model, date)
  ) |>
  arrange(model, date) |>
  mutate(cum_pnl = cumsum(daily_pnl), .by = model)

p_pnl_date <- daily_pnl |>
  ggplot(aes(date, cum_pnl, colour = model)) +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.5) +
  geom_step(linewidth = 0.8) +
  scale_colour_manual(values = model_colours, labels = model_labels) +
  scale_y_continuous(labels = label_percent(accuracy = 0.1)) +
  labs(
    title = "Uppsöfnuð PnL eftir dagsetningu",
    x = NULL, y = "Uppsöfnuð PnL (hlutfall bankroll)",
    colour = NULL
  ) +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "cumulative_pnl_by_date.png"),
  p_pnl_date, width = 10, height = 6, dpi = 150, bg = "white")
cat("  Saved cumulative_pnl_by_date.png\n")

#### Plot 3: Probability comparison scatter ####
prob_compare <- pnl_df |>
  select(model, game_nr, market, outcome, info, p_model) |>
  pivot_wider(
    id_cols = c(game_nr, market, outcome, info),
    names_from = model,
    values_from = p_model
  )

p_prob <- prob_compare |>
  ggplot(aes(poisson, student_t, colour = market)) +
  geom_abline(intercept = 0, slope = 1, lty = 2, alpha = 0.5) +
  geom_point(alpha = 0.6, size = 2) +
  coord_equal() +
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_percent()) +
  labs(
    title = "Líkindamat líkana — hvert veðmál",
    subtitle = "Á hornalínu = líkön sammála",
    x = "Poisson P(vinna)", y = "Student-t P(vinna)",
    colour = "Markaður"
  ) +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "probability_scatter.png"),
  p_prob, width = 8, height = 7, dpi = 150, bg = "white")
cat("  Saved probability_scatter.png\n")

#### Plot 4: EV distribution by model ####
p_ev <- pnl_df |>
  filter(bet) |>
  ggplot(aes(ev, fill = model)) +
  geom_histogram(bins = 30, alpha = 0.5, position = "identity") +
  geom_vline(xintercept = 0, lty = 2) +
  scale_fill_manual(values = model_colours, labels = model_labels) +
  labs(
    title = "Dreifing EV — veðmál valin af hverju líkani",
    x = "Expected Value", y = "Fjöldi veðmála", fill = NULL
  ) +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "ev_distribution.png"),
  p_ev, width = 10, height = 5, dpi = 150, bg = "white")
cat("  Saved ev_distribution.png\n")

#### Plot 5: Per-market PnL comparison ####
market_pnl <- pnl_df |>
  filter(bet) |>
  summarise(
    total_pnl = sum(pnl_per_unit),
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
    title = "Arðsemi eftir markaði",
    x = NULL, y = "ROI", fill = NULL
  ) +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "market_roi.png"),
  p_market, width = 8, height = 5, dpi = 150, bg = "white")
cat("  Saved market_roi.png\n")

#### Plot 6: Kelly fraction comparison ####
kelly_compare <- pnl_df |>
  select(model, game_nr, market, outcome, info, kelly) |>
  pivot_wider(
    id_cols = c(game_nr, market, outcome, info),
    names_from = model,
    values_from = kelly
  )

p_kelly <- kelly_compare |>
  ggplot(aes(poisson, student_t, colour = market)) +
  geom_abline(intercept = 0, slope = 1, lty = 2, alpha = 0.5) +
  geom_point(alpha = 0.6, size = 2) +
  labs(
    title = "Kelly-hlutfall — Poisson vs Student-t",
    subtitle = "Hærra = sterkari sannfæring",
    x = "Poisson Kelly", y = "Student-t Kelly",
    colour = "Markaður"
  ) +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "kelly_scatter.png"),
  p_kelly, width = 8, height = 7, dpi = 150, bg = "white")
cat("  Saved kelly_scatter.png\n")

cat(sprintf("\nDone. Results in %s\n", out_dir))
