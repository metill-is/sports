#### Explore: 2D Student-t Football Model ####
#
# Interactive exploration of the bivariate Student-t model for English football.
# Models (total_goals, goal_difference) jointly with team-specific variability,
# match-dependent correlation, and Student-t tails for extreme results.
#
# Run section-by-section in Positron (Cmd+Enter).
# After fitting once, skip the "Fit model" section and load from disk.

library(tidyverse)
library(cmdstanr)
library(posterior)
library(progressr)
library(ggrepel)
library(metill)
theme_set(theme_metill())
Sys.setlocale("LC_ALL", "is_IS.UTF-8")

# Enable progressr so cmdstanr shows a progress bar during fitting
handlers(global = TRUE)

#### Config ####

sex <- "male"
iter <- 1000
chains <- 4

# Absolute paths — edit these if your layout differs
sports_dir <- "/Users/brynjolfurjonsson/sports/Sports"
league_dir <- file.path(sports_dir, "football", "england")

#### Prepare data ####

setwd(league_dir)
here::i_am("football_england.Rproj")

source(file.path(sports_dir, "R", "shared", "prep_data_football.R"))
source(file.path(sports_dir, "R", "shared", "model_fitting.R"))
source(file.path(sports_dir, "R", "shared", "extract_posterior.R"))

stan_data <- prepare_football_data(sex)

teams <- read_csv(
  file.path(league_dir, "results", sex, "teams.csv"),
  show_col_types = FALSE
)
pred_d <- read_csv(
  file.path(league_dir, "results", sex, "pred_d.csv"),
  show_col_types = FALSE
)
model_d <- read_csv(
  file.path(league_dir, "results", sex, "model_d.csv"),
  show_col_types = FALSE
)

tibble(
  N = stan_data$N,
  K = stan_data$K,
  N_pred = stan_data$N_pred,
  N_seasons = stan_data$N_seasons,
  N_rounds = stan_data$N_rounds
)

#### Fit model ####
# Skip this section if you already have a fit — jump to "Load fit"

out_dir <- file.path(league_dir, "results", "explore_2d_student_t")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

stan_file <- file.path(
  league_dir,
  "Stan",
  "2d_student_t_totalgoals-v-goaldiff.stan"
)
fit_path <- file.path(out_dir, "fit.rds")

fit_model(
  stan_data = stan_data,
  stan_model_path = stan_file,
  output_path = fit_path,
  chains = chains,
  parallel_chains = chains,
  iter_warmup = iter,
  iter_sampling = iter,
  init = 0
)

#### Load fit ####

out_dir <- file.path(league_dir, "results", "explore_2d_student_t")
fit <- readRDS(file.path(out_dir, "fit.rds"))

#### Diagnostics ####

fit$diagnostic_summary()

fit$summary(
  c(
    "nu",
    "mean_goals0",
    "delta_mean_goals",
    "alpha_rho",
    "beta_rho",
    "beta2_rho",
    "beta3_rho",
    "mean_sigma_team",
    "scale_sigma_team",
    "mean_sigma_off",
    "scale_sigma_off",
    "mean_sigma_def",
    "scale_sigma_def"
  )
)

#### Team strengths ####

strength_df <- fit$summary(
  variables = c(
    "cur_offense",
    "cur_defense",
    "cur_strength",
    "cur_offense_home",
    "cur_defense_home",
    "cur_offense_away",
    "cur_defense_away",
    "home_advantage_tot"
  ),
  mean,
  median,
  sd,
  ~ quantile(.x, c(0.05, 0.95))
) |>
  mutate(
    param = str_extract(variable, "^[^\\[]+"),
    team_idx = str_match(variable, "\\[(\\d+)\\]")[, 2] |> as.integer()
  ) |>
  inner_join(teams |> rename(team_idx = team_nr), by = "team_idx")

# Overall strength ranking
strength_df |>
  filter(param == "cur_strength") |>
  arrange(desc(mean)) |>
  select(team, mean, sd, `5%`, `95%`)

# Offense vs defense
strength_df |>
  filter(param %in% c("cur_offense", "cur_defense")) |>
  select(team, param, mean) |>
  pivot_wider(names_from = param, values_from = mean) |>
  ggplot(aes(cur_offense, cur_defense, label = team)) +
  geom_point(size = 2) +
  geom_text_repel(size = 3, max.overlaps = 25) +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.3) +
  geom_vline(xintercept = 0, lty = 2, alpha = 0.3) +
  labs(
    title = "Offense vs Defense — 2D Student-t",
    subtitle = "Top-right = strong overall",
    x = "Offensive strength (goals)",
    y = "Defensive strength (goals)"
  )

# Strength ranking plot
strength_df |>
  filter(param == "cur_strength") |>
  mutate(team = fct_reorder(team, mean)) |>
  ggplot(aes(x = mean, y = team)) +
  geom_pointrange(aes(xmin = `5%`, xmax = `95%`), size = 0.3) +
  geom_vline(xintercept = 0, lty = 2, alpha = 0.5) +
  labs(
    title = "Team Strength — 2D Student-t",
    subtitle = "Offense + Defense (neutral venue), 90% CI",
    x = "Strength (goal units)",
    y = NULL
  )

#### Team variability (sigma_team) ####
# Each team has its own sigma — how much variance it contributes to any match

sigma_df <- fit$summary("sigma_team", mean, median, sd) |>
  mutate(team_idx = str_match(variable, "\\[(\\d+)\\]")[, 2] |> as.integer()) |>
  inner_join(teams |> rename(team_idx = team_nr), by = "team_idx") |>
  arrange(desc(mean))

sigma_df |> select(team, mean, sd)

sigma_df |>
  mutate(team = fct_reorder(team, mean)) |>
  ggplot(aes(x = mean, y = team)) +
  geom_point(size = 2) +
  labs(
    title = "Team Match Variability (sigma_team)",
    subtitle = "Higher = more unpredictable scorelines",
    x = "sigma_team",
    y = NULL
  )

#### Degrees of freedom (nu) ####
# Low nu = heavy tails = more extreme results. nu → ∞ recovers Gaussian.

nu_draws <- as_draws_df(fit$draws("nu"))

tibble(
  mean = mean(nu_draws$nu),
  median = median(nu_draws$nu),
  sd = sd(nu_draws$nu),
  q05 = quantile(nu_draws$nu, 0.05),
  q95 = quantile(nu_draws$nu, 0.95)
)

ggplot(nu_draws, aes(nu)) +
  geom_histogram(bins = 50, fill = "#2c5aa0", alpha = 0.7) +
  geom_vline(xintercept = median(nu_draws$nu), lty = 2) +
  labs(
    title = "Posterior: Degrees of Freedom (nu)",
    subtitle = "Lower = heavier tails",
    x = "nu",
    y = "Count"
  )

#### Correlation structure (rho) ####
# rho(match) = inv_logit(alpha + beta * |strength_diff| + beta2 * |total_strength| + beta3 * interaction)
# Maps to [-1, 1] — correlation between home and away goals

fit$summary(
  c("alpha_rho", "beta_rho", "beta2_rho", "beta3_rho"),
  mean,
  sd,
  ~ quantile(.x, c(0.05, 0.95))
)

# Visualise implied rho across strength differences
rho_draws <- as_draws_df(fit$draws(c(
  "alpha_rho",
  "beta_rho",
  "beta2_rho",
  "beta3_rho"
)))

expand_grid(
  strength_diff = seq(0, 3, by = 0.1),
  total_strength = c(0, 1, 2)
) |>
  crossing(rho_draws |> slice_sample(n = 200)) |>
  mutate(
    logit_rho = alpha_rho +
      beta_rho * strength_diff +
      beta2_rho * total_strength +
      beta3_rho * strength_diff * total_strength,
    rho = 2 * plogis(logit_rho) - 1
  ) |>
  summarise(
    mean = mean(rho),
    q10 = quantile(rho, 0.1),
    q90 = quantile(rho, 0.9),
    .by = c(strength_diff, total_strength)
  ) |>
  ggplot(aes(strength_diff, mean, fill = factor(total_strength))) +
  geom_ribbon(aes(ymin = q10, ymax = q90), alpha = 0.2) +
  geom_line(aes(colour = factor(total_strength))) +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.3) +
  labs(
    title = "Implied Goal Correlation (rho) by Match Characteristics",
    subtitle = "Correlation between home and away goals",
    x = "|Strength difference|",
    y = "rho",
    colour = "|Total strength|",
    fill = "|Total strength|"
  )

#### Mean goals across seasons ####

season_levels <- sort(unique(model_d$season))

fit$summary("mean_goals", mean, sd, ~ quantile(.x, c(0.05, 0.95))) |>
  mutate(
    season = season_levels[
      str_match(variable, "\\[(\\d+)\\]")[, 2] |> as.integer()
    ]
  ) |>
  ggplot(aes(x = factor(season), y = mean)) +
  geom_pointrange(aes(ymin = `5%`, ymax = `95%`)) +
  labs(
    title = "Mean Goals Per Team Per Match",
    subtitle = "Total per match ≈ 2× this",
    x = "Season",
    y = "Goals per team"
  )

#### Home advantage ####

ha_df <- fit$summary(
  "home_advantage_tot",
  mean,
  sd,
  ~ quantile(.x, c(0.05, 0.95))
) |>
  mutate(team_idx = str_match(variable, "\\[(\\d+)\\]")[, 2] |> as.integer()) |>
  inner_join(teams |> rename(team_idx = team_nr), by = "team_idx")

ha_df |>
  mutate(team = fct_reorder(team, mean)) |>
  ggplot(aes(x = mean, y = team)) +
  geom_pointrange(aes(xmin = `5%`, xmax = `95%`), size = 0.3) +
  geom_vline(xintercept = mean(ha_df$mean), lty = 2, alpha = 0.5) +
  labs(
    title = "Home Advantage (Total)",
    subtitle = "Goals advantage when playing at home, 90% CI",
    x = "Home advantage (goal units)",
    y = NULL
  )

# Separate offensive and defensive home advantage
fit$summary(c("home_advantage_off", "home_advantage_def"), mean) |>
  mutate(
    param = str_extract(variable, "^[^\\[]+"),
    team_idx = str_match(variable, "\\[(\\d+)\\]")[, 2] |> as.integer()
  ) |>
  inner_join(teams |> rename(team_idx = team_nr), by = "team_idx") |>
  select(team, param, mean) |>
  pivot_wider(names_from = param, values_from = mean) |>
  ggplot(aes(home_advantage_off, home_advantage_def, label = team)) +
  geom_point(size = 2) +
  geom_text_repel(size = 3, max.overlaps = 25) +
  geom_abline(slope = 1, intercept = 0, lty = 2, alpha = 0.3) +
  labs(
    title = "Home Advantage: Offensive vs Defensive Component",
    subtitle = "Above diagonal = more defensive home advantage",
    x = "Offensive HA (more goals scored at home)",
    y = "Defensive HA (fewer goals conceded at home)"
  )

#### Predictions ####

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

# Summary table
pred_summary <- post |>
  mutate(
    total = home_goals + away_goals,
    diff = home_goals - away_goals,
    outcome = case_when(
      home_goals > away_goals ~ "H",
      home_goals < away_goals ~ "A",
      TRUE ~ "D"
    )
  ) |>
  summarise(
    mean_total = mean(total),
    sd_total = sd(total),
    mean_diff = mean(diff),
    sd_diff = sd(diff),
    p_home = mean(outcome == "H"),
    p_draw = mean(outcome == "D"),
    p_away = mean(outcome == "A"),
    .by = c(game_nr, date, home, away, division)
  ) |>
  arrange(date, game_nr)

pred_summary |>
  mutate(
    match = paste(home, "v", away),
    total = sprintf("%.1f ± %.1f", mean_total, sd_total),
    diff = sprintf("%+.1f ± %.1f", mean_diff, sd_diff),
    probs = sprintf(
      "%.0f / %.0f / %.0f",
      p_home * 100,
      p_draw * 100,
      p_away * 100
    )
  ) |>
  select(date, div = division, match, total, diff, `H/D/A %` = probs) |>
  print(n = 50)

#### Posterior predictive: score distributions ####
# Pick a match and look at the full predicted score distribution

# Top division matches only
top_matches <- pred_summary |> filter(division == 1)
top_matches

# Distribution for first match
match_idx <- top_matches$game_nr[1]

post |>
  filter(game_nr == match_idx) |>
  count(home_goals, away_goals) |>
  mutate(prob = n / sum(n)) |>
  ggplot(aes(away_goals, home_goals, fill = prob)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.1f%%", prob * 100)), size = 3) +
  scale_fill_viridis_c(option = "magma", labels = scales::label_percent()) +
  coord_equal() +
  labs(
    title = sprintf(
      "%s v %s — Score Probabilities",
      top_matches$home[1],
      top_matches$away[1]
    ),
    x = "Away goals",
    y = "Home goals",
    fill = "P(score)"
  )

#### Random walk trajectories ####
# Visualise how team strengths evolved over time

# Extract offense trajectories for top 6 teams
top_team_nrs <- teams |>
  inner_join(
    strength_df |>
      filter(param == "cur_strength") |>
      arrange(desc(mean)) |>
      head(6),
    by = "team"
  ) |>
  pull(team_nr)

offense_draws <- as_draws_df(fit$draws("offense")) |>
  as_tibble() |>
  select(-.chain, -.iteration) |>
  pivot_longer(-.draw, names_to = "variable", values_to = "value") |>
  mutate(
    round = str_match(variable, "\\[(\\d+),(\\d+)\\]")[, 2] |> as.integer(),
    team_idx = str_match(variable, "\\[(\\d+),(\\d+)\\]")[, 3] |> as.integer()
  ) |>
  filter(team_idx %in% top_team_nrs) |>
  inner_join(teams |> rename(team_idx = team_nr), by = "team_idx") |>
  summarise(
    mean = mean(value),
    q10 = quantile(value, 0.1),
    q90 = quantile(value, 0.9),
    .by = c(round, team)
  )

offense_draws |>
  ggplot(aes(round, mean, colour = team, fill = team)) +
  geom_ribbon(aes(ymin = q10, ymax = q90), alpha = 0.1, colour = NA) +
  geom_line() +
  labs(
    title = "Offensive Strength Over Time (Top 6 Teams)",
    x = "Round",
    y = "Offensive strength (goals)",
    colour = NULL,
    fill = NULL
  )
