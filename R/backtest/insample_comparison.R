#### In-Sample Model Comparison ####
#
# Fits Poisson and Student-t (direct SD) on FULL data (no holdout),
# then compares posterior predictive checks in-sample.
#
# Usage:
#   cd Sports
#   Rscript R/backtest/insample_comparison.R --league football_iceland --iter 500

library(tidyverse)
library(here)
library(cmdstanr)
library(posterior)
library(patchwork)
library(scales)
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

cat(sprintf("In-sample comparison: %s | %d iterations | %d chains\n",
  league, iter, chains))

#### Prepare data (full, no holdout) ####
sports_dir <- getwd()
old_wd <- setwd(league_dir)
on.exit(setwd(old_wd))

rproj <- paste0(league, ".Rproj")
if (file.exists(rproj)) {
  suppressMessages(here::i_am(rproj))
} else if (file.exists(".here")) {
  suppressMessages(here::i_am(".here"))
}

sex <- "male"
env <- new.env(parent = globalenv())
source(file.path(sports_dir, "R", "shared", "prep_data_football.R"), local = env)
# Use almost all data for training, hold out the last ~week for predictions
# This keeps the RW parameters fresh (no long extrapolation)
d_raw <- read_csv(here::here("data", sex, "data.csv"), show_col_types = FALSE)
if ("dags" %in% names(d_raw)) d_raw <- rename(d_raw, date = dags)
cutoff <- sort(d_raw$date, decreasing = TRUE)[8]  # ~1 week from end
cat(sprintf("Test cutoff: %s (last date: %s)\n", cutoff, max(d_raw$date)))

prep <- suppressMessages(env$prepare_football_data(sex, test_date_cutoff = cutoff))
stan_data <- prep$stan_data

cat(sprintf("Data: N=%d games, K=%d teams, N_seasons=%d\n",
  stan_data$N, stan_data$K, stan_data$N_seasons))

# Training data for in-sample comparison (matches the stan_data)
d <- read_csv(here::here("data", sex, "data.csv"), show_col_types = FALSE)
if ("dags" %in% names(d)) {
  d <- rename(d, season = timabil, date = dags, home = heima, away = gestir,
              home_goals = stig_heima, away_goals = stig_gestir)
}
d <- d |>
  filter(season >= 2021, date < cutoff) |>
  arrange(date) |>
  mutate(game_nr = row_number())

cat(sprintf("Training games: %d, Test games: %d\n", nrow(d), stan_data$N_pred))

#### Define models ####
models <- list(
  poisson = here::here("Stan", "bivariate_poisson_ppc.stan"),
  student_t = here::here("Stan", "2d_student_t_direct_SD.stan")
)

for (nm in names(models)) {
  if (!file.exists(models[[nm]])) {
    stop(sprintf("Model not found: %s (%s)", nm, models[[nm]]))
  }
}

#### Fit both models ####
source(file.path(sports_dir, "R", "shared", "model_fitting.R"))

out_dir <- here::here("results", "insample")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

fits <- list()

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

  fits[[nm]] <- readRDS(fit_path)
  cat(sprintf("  %s: done\n", nm))
}

#### Extract in-sample replications ####
cat("\n=== Extracting replications ===\n")

# Helper to pivot Stan draws to long format
pivot_draws <- function(fit, param) {
  as_draws_df(fit$draws(param)) |>
    as_tibble() |>
    select(-c(.chain, .iteration)) |>
    pivot_longer(-.draw, names_to = "p", values_to = "value") |>
    mutate(game_nr = str_match(p, "\\[(.*)\\]")[, 2] |> as.integer()) |>
    select(.draw, game_nr, value)
}

reps <- list()
for (nm in names(fits)) {
  fit <- fits[[nm]]
  available <- fit$metadata()$stan_variables

  if ("goals1_rep" %in% available) {
    # Poisson model: has individual goals
    home_long <- pivot_draws(fit, "goals1_rep") |> rename(home_goals = value)
    away_long <- pivot_draws(fit, "goals2_rep") |> rename(away_goals = value)
    reps[[nm]] <- inner_join(home_long, away_long, by = c(".draw", "game_nr")) |>
      mutate(
        total_goals = home_goals + away_goals,
        goal_diff = home_goals - away_goals,
        model = nm
      )
  } else {
    # Student-t model: has total_goals_rep and goal_diff_rep directly
    total_long <- pivot_draws(fit, "total_goals_rep") |> rename(total_goals = value)
    diff_long <- pivot_draws(fit, "goal_diff_rep") |> rename(goal_diff = value)
    reps[[nm]] <- inner_join(total_long, diff_long, by = c(".draw", "game_nr")) |>
      mutate(model = nm)
  }

  cat(sprintf("  %s: %d draws x %d games\n", nm,
    max(reps[[nm]]$.draw), max(reps[[nm]]$game_nr)))
}

#### Compute PIT values ####
cat("\n=== Computing PIT values ===\n")

obs_d <- d |>
  mutate(
    obs_total = home_goals + away_goals,
    obs_diff = home_goals - away_goals
  ) |>
  select(game_nr, obs_total, obs_diff)

pit_data <- map_dfr(names(reps), \(nm) {
  reps[[nm]] |>
    inner_join(obs_d, by = "game_nr") |>
    summarise(
      pit_total = mean(total_goals < obs_total) + 0.5 * mean(total_goals == obs_total),
      pit_diff = mean(goal_diff < obs_diff) + 0.5 * mean(goal_diff == obs_diff),
      .by = game_nr
    ) |>
    mutate(model = nm)
})

#### Compute 1x2 calibration ####
cat("=== Computing 1x2 probabilities ===\n")

# For Student-t (continuous goal_diff): D > 0.5 → home, D < -0.5 → away, else draw
# For Poisson (integer goal_diff): standard sign comparison
outcome_data <- map_dfr(names(reps), \(nm) {
  has_home_goals <- "home_goals" %in% names(reps[[nm]])
  reps[[nm]] |>
    mutate(
      pred_outcome = if (has_home_goals) {
        # Poisson: integer goals, exact comparison
        case_when(
          goal_diff > 0 ~ "home",
          goal_diff < 0 ~ "away",
          TRUE ~ "draw"
        )
      } else {
        # Student-t: continuous D, use rounding to nearest integer
        case_when(
          round(goal_diff) > 0 ~ "home",
          round(goal_diff) < 0 ~ "away",
          TRUE ~ "draw"
        )
      }
    ) |>
    summarise(
      p_home = mean(pred_outcome == "home"),
      p_draw = mean(pred_outcome == "draw"),
      p_away = mean(pred_outcome == "away"),
      .by = game_nr
    ) |>
    inner_join(
      d |>
        mutate(
          actual_outcome = case_when(
            home_goals > away_goals ~ "home",
            home_goals < away_goals ~ "away",
            TRUE ~ "draw"
          )
        ) |>
        select(game_nr, actual_outcome),
      by = "game_nr"
    ) |>
    mutate(model = nm)
})

#### Key parameter summary for Student-t ####
cat("\n=== Student-t (direct SD) key parameters ===\n")
st_params <- c("sigma_S", "sigma_D", "rho_SD", "nu")
st_summary <- fits$student_t$summary(st_params)
print(st_summary, width = 120)

#### Generate plots ####
cat("\n=== Generating plots ===\n")

model_colours <- c(poisson = "#2c5aa0", student_t = "#c44e52")
model_labels <- c(poisson = "Poisson (tvíþætt)", student_t = "Student-t (beint S,D)")

#### Plot 1: PIT histograms (2 panel) ####
pit_long <- pit_data |>
  pivot_longer(starts_with("pit_"), names_to = "quantity", values_to = "pit") |>
  mutate(quantity = str_remove(quantity, "pit_"))

quantity_labels <- c(
  total = "Heildarmark", diff = "Markamismunur"
)

p_pit <- pit_long |>
  mutate(quantity = quantity_labels[quantity]) |>
  ggplot(aes(pit, fill = model)) +
  geom_histogram(bins = 10, alpha = 0.5, position = "identity", boundary = 0) +
  geom_hline(
    aes(yintercept = y),
    data = tibble(y = nrow(d) / 10),
    lty = 2, alpha = 0.5
  ) +
  facet_wrap(~quantity) +
  scale_fill_manual(values = model_colours, labels = model_labels) +
  labs(
    title = "PIT stuðlarit — in-sample",
    subtitle = "Einsleit dreifing = vel kvörðuð spá",
    x = "PIT gildi", y = "Fjöldi", fill = NULL
  ) +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "pit_histograms.png"),
  p_pit, width = 10, height = 7, dpi = 150, bg = "white")
cat("  Saved pit_histograms.png\n")

#### Plot 2: Score marginals — replicated vs observed ####
rep_marginals <- bind_rows(reps)

obs_marginals <- d |>
  mutate(
    total_goals = home_goals + away_goals,
    goal_diff = home_goals - away_goals
  )

# Total goals
p_total <- rep_marginals |>
  mutate(total_goals = pmin(total_goals, 10)) |>
  count(model, total_goals) |>
  mutate(prop = n / sum(n), .by = model) |>
  ggplot(aes(total_goals, prop, fill = model)) +
  geom_col(position = "dodge", alpha = 0.7) +
  geom_point(
    aes(x = total_goals, y = prop),
    data = obs_marginals |>
      mutate(total_goals = pmin(total_goals, 10)) |>
      count(total_goals) |>
      mutate(prop = n / sum(n)),
    inherit.aes = FALSE, size = 3, shape = 4, stroke = 1.5
  ) +
  scale_fill_manual(values = model_colours, labels = model_labels) +
  scale_x_continuous(breaks = 0:10, labels = c(0:9, "10+")) +
  scale_y_continuous(labels = label_percent()) +
  labs(title = "Heildarmark", x = NULL, y = "Hlutfall", fill = NULL) +
  theme(legend.position = "none")

# Goal difference (round continuous D to nearest integer for binning)
p_diff <- rep_marginals |>
  mutate(goal_diff = pmax(pmin(round(goal_diff), 6), -6)) |>
  count(model, goal_diff) |>
  mutate(prop = n / sum(n), .by = model) |>
  ggplot(aes(goal_diff, prop, fill = model)) +
  geom_col(position = "dodge", alpha = 0.7) +
  geom_point(
    aes(x = goal_diff, y = prop),
    data = obs_marginals |>
      mutate(goal_diff = pmax(pmin(goal_diff, 6), -6)) |>
      count(goal_diff) |>
      mutate(prop = n / sum(n)),
    inherit.aes = FALSE, size = 3, shape = 4, stroke = 1.5
  ) +
  scale_fill_manual(values = model_colours, labels = model_labels) +
  scale_y_continuous(labels = label_percent()) +
  labs(title = "Markamismunur", x = NULL, y = NULL, fill = NULL) +
  theme(legend.position = "none")

# Draw rate (for Student-t: round(D) == 0; for Poisson: exact D == 0)
obs_draw_rate <- mean(obs_marginals$goal_diff == 0)
rep_draw_rates <- rep_marginals |>
  summarise(draw_rate = mean(round(goal_diff) == 0), .by = model)

p_draw <- rep_draw_rates |>
  ggplot(aes(model, draw_rate, fill = model)) +
  geom_col(alpha = 0.7, width = 0.5) +
  geom_hline(yintercept = obs_draw_rate, lty = 2, linewidth = 0.8) +
  annotate("text", x = 1.5, y = obs_draw_rate + 0.01,
    label = sprintf("Raunverulegt: %.1f%%", obs_draw_rate * 100), size = 3.5) +
  scale_fill_manual(values = model_colours) +
  scale_x_discrete(labels = model_labels) +
  scale_y_continuous(labels = label_percent()) +
  labs(title = "Jafnteflishlutfall", x = NULL, y = NULL, fill = NULL) +
  theme(legend.position = "none")

p_marginals <- (p_total + p_diff + p_draw) +
  plot_annotation(
    title = "Jaðardreifingar — endurtekningargögn (litað) vs raunverulegt (×)",
    subtitle = sprintf("N = %d leikir, in-sample posterior predictive check", nrow(d))
  )

ggsave(file.path(out_dir, "score_marginals.png"),
  p_marginals, width = 12, height = 8, dpi = 150, bg = "white")
cat("  Saved score_marginals.png\n")

#### Plot 3: 1x2 Calibration ####
ntiles <- 10
cal_data <- outcome_data |>
  pivot_longer(
    c(p_home, p_draw, p_away),
    names_to = "outcome_type",
    values_to = "p"
  ) |>
  mutate(
    outcome_type = str_remove(outcome_type, "^p_"),
    correct = as.integer(actual_outcome == outcome_type)
  )

p_cal <- cal_data |>
  mutate(bin = floor(p * ntiles) / ntiles) |>
  summarise(
    pred = mean(p),
    obs = mean(correct),
    n = n(),
    .by = c(model, bin)
  ) |>
  ggplot(aes(pred, obs, colour = model)) +
  geom_abline(intercept = 0, slope = 1, lty = 2, alpha = 0.5) +
  geom_point(aes(size = n), alpha = 0.7) +
  geom_line(alpha = 0.5) +
  scale_colour_manual(values = model_colours, labels = model_labels) +
  scale_size_continuous(range = c(1, 4), guide = "none") +
  scale_x_continuous(labels = label_percent(), limits = c(0, 1)) +
  scale_y_continuous(labels = label_percent(), limits = c(0, 1)) +
  labs(
    title = "Kvörðun (1x2) — in-sample",
    x = "Spáð líkindi",
    y = "Raunverulegt hlutfall",
    colour = NULL
  ) +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "calibration_1x2.png"),
  p_cal, width = 7, height = 6, dpi = 150, bg = "white")
cat("  Saved calibration_1x2.png\n")

#### Plot 4: Team strengths comparison ####
strength_data <- map_dfr(names(fits), \(nm) {
  draws <- as_draws_df(fits[[nm]]$draws("cur_strength"))
  teams <- read_csv(here::here("results", sex, "teams.csv"), show_col_types = FALSE)

  draws |>
    as_tibble() |>
    pivot_longer(starts_with("cur_strength"), names_to = "param", values_to = "strength") |>
    mutate(
      team_nr = str_match(param, "\\[(.*)\\]")[, 2] |> as.integer(),
      model = nm
    ) |>
    inner_join(teams, by = "team_nr") |>
    summarise(
      str_mean = mean(strength),
      q10 = quantile(strength, 0.1),
      q90 = quantile(strength, 0.9),
      .by = c(model, team, team_nr)
    )
})

top20 <- strength_data |>
  summarise(avg = mean(str_mean), .by = team) |>
  slice_max(avg, n = 20) |>
  pull(team)

p_strength <- strength_data |>
  filter(team %in% top20) |>
  mutate(team = fct_reorder(team, str_mean)) |>
  ggplot(aes(y = team, x = str_mean, colour = model)) +
  geom_pointrange(
    aes(xmin = q10, xmax = q90),
    position = position_dodge(width = 0.5),
    size = 0.4
  ) +
  scale_colour_manual(values = model_colours, labels = model_labels) +
  labs(
    title = "Styrkur liða (efstu 20)",
    subtitle = "80% trúnaðarbil — in-sample",
    x = "Heildarstyrkur (off + def)",
    y = NULL,
    colour = NULL
  ) +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "team_strength.png"),
  p_strength, width = 8, height = 8, dpi = 150, bg = "white")
cat("  Saved team_strength.png\n")

#### Plot 5: Draw probability comparison ####
draw_compare <- outcome_data |>
  select(game_nr, model, p_draw, actual_outcome) |>
  pivot_wider(
    id_cols = c(game_nr, actual_outcome),
    names_from = model,
    values_from = p_draw
  ) |>
  mutate(is_draw = actual_outcome == "draw")

p_drawcomp <- draw_compare |>
  ggplot(aes(poisson, student_t, colour = is_draw)) +
  geom_abline(intercept = 0, slope = 1, lty = 2, alpha = 0.5) +
  geom_point(alpha = 0.5, size = 1.5) +
  scale_colour_manual(
    values = c("FALSE" = "grey60", "TRUE" = "#e6a532"),
    labels = c("FALSE" = "Ekki jafntefli", "TRUE" = "Jafntefli")
  ) +
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_percent()) +
  coord_equal() +
  labs(
    title = "P(jafntefli) — Poisson vs Student-t (beint S,D)",
    x = "Poisson", y = "Student-t", colour = NULL
  ) +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "draw_probability.png"),
  p_drawcomp, width = 7, height = 7, dpi = 150, bg = "white")
cat("  Saved draw_probability.png\n")

#### Summary stats ####
cat("\n=== Summary ===\n")

# Mean log-score per model
for (nm in names(fits)) {
  od <- outcome_data |> filter(model == nm)
  od <- od |>
    mutate(
      p_actual = case_when(
        actual_outcome == "home" ~ p_home,
        actual_outcome == "draw" ~ p_draw,
        actual_outcome == "away" ~ p_away
      ),
      log_score = log(pmax(p_actual, 1e-10)),
      brier = (p_home - (actual_outcome == "home"))^2 +
              (p_draw - (actual_outcome == "draw"))^2 +
              (p_away - (actual_outcome == "away"))^2
    )
  cat(sprintf("  %s: mean log-score = %.4f, mean Brier = %.4f, draw_rate = %.1f%%\n",
    nm, mean(od$log_score), mean(od$brier),
    100 * mean(round(reps[[nm]]$goal_diff) == 0)))
}
cat(sprintf("  Observed draw rate: %.1f%%\n", 100 * obs_draw_rate))

cat(sprintf("\nDone. Results in %s\n", out_dir))
