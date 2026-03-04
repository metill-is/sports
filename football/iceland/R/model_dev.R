#### Packages ####
library(tidyverse)
library(scales)
library(cmdstanr)
library(posterior)
library(metill)
library(geomtextpath)
library(ggtext)
library(glue)
library(here)
theme_set(theme_metill())
Sys.setlocale("LC_ALL", "is_IS.UTF-8")

source(
  here("R", "male", "prep_data.R")
)

#### Compile and fit model ####
model <- cmdstan_model(
  here(
    "Stan",
    "bivariate_poisson_inflated_diagonal_corrmodel.stan"
  )
)

# Fit model
results <- model$sample(
  data = stan_data,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 200,
  iter_sampling = 200,
  init = 0
)

results$summary(
  c(
    "mean_log_goals",
    "alpha_mu3",
    "beta_mu3_strength_diff",
    "logit_p0",
    "beta_logit_p_strength_diff",
    "tie_alpha",
    "tie_beta"
  )
)

results$draws(c("mean_log_goals")) |>
  bayesplot::mcmc_hist_by_chain()

results$summary(
  c("percent_home")
) |>
  mutate(
    team = row_number() |>
      as.factor() |>
      fct_reorder(mean)
  ) |>
  ggplot(aes(mean, team)) +
  geom_pointrange(
    aes(xmin = q5, xmax = q95)
  )

results$summary(
  c("total_home_advantage")
) |>
  mutate(
    team = row_number() |>
      as.factor() |>
      fct_reorder(mean)
  ) |>
  ggplot(aes(mean, team)) +
  geom_pointrange(
    aes(xmin = q5, xmax = q95)
  )


results$draws("home_advantage_tot") |>
  as_draws_df() |>
  as_tibble() |>
  pivot_longer(c(-.chain, -.draw, -.iteration)) |>
  mutate(
    team_nr = name |> parse_number(),
    type = "Samanlögð áhrif á heildarstyrk",
    value = value / 2
  ) |>
  bind_rows(
    results$draws("home_advantage_def") |>
      as_draws_df() |>
      as_tibble() |>
      pivot_longer(c(-.chain, -.draw, -.iteration)) |>
      mutate(
        team_nr = name |> parse_number(),
        type = "Áhrif á varnarstyrk heimaliðs"
      )
  ) |>
  bind_rows(
    results$draws("home_advantage_off") |>
      as_draws_df() |>
      as_tibble() |>
      pivot_longer(c(-.chain, -.draw, -.iteration)) |>
      mutate(
        team_nr = name |> parse_number(),
        type = "Áhrif á sóknarstyrk heimaliðs"
      )
  ) |>
  inner_join(
    teams,
    by = join_by(team_nr)
  ) |>
  semi_join(
    d |>
      filter(season == 2025, division == 1) |>
      select(home, away) |>
      pivot_longer(c(everything()), values_to = "team") |>
      distinct(team)
  ) |>
  reframe(
    median = median(value),
    coverage = c(
      0.025,
      0.05,
      0.1,
      0.2,
      0.3,
      0.4,
      0.5,
      0.6,
      0.7,
      0.8,
      0.9,
      0.95,
      0.975
    ),
    lower = quantile(value, 0.5 - coverage / 2),
    upper = quantile(value, 0.5 + coverage / 2),
    .by = c(team, type)
  ) |>
  mutate(
    team = factor(
      team,
      levels = unique(team)[order(unique(median[
        type == "Samanlögð áhrif á heildarstyrk"
      ]))]
    )
  ) |>
  mutate_at(
    vars(median, lower, upper),
    exp
  ) |>
  ggplot(aes(median, team)) +
  geom_vline(
    xintercept = 1,
    lty = 2,
    alpha = 0.4,
    linewidth = 0.3
  ) +
  geom_hline(
    yintercept = seq(1, nrow(teams), 2),
    linewidth = 7,
    alpha = 0.03
  ) +
  geom_point(
    shape = "|",
    size = 5
  ) +
  geom_segment(
    aes(
      x = lower,
      xend = upper,
      yend = team,
      alpha = -coverage
    ),
    linewidth = 3
  ) +
  scale_alpha_continuous(
    range = c(0, 0.3),
    guide = guide_none()
  ) +
  scale_x_continuous(
    guide = guide_axis(cap = "both"),
    breaks = c(1, 1.25, 1.5, 1.75, 2, 2.5),
    labels = \(x) paste0("+", label_hlutf(accuracy = 1)(x - 1)),
    # trans = "log"
  ) +
  scale_y_discrete(
    guide = guide_axis(cap = "both")
  ) +
  facet_wrap("type") +
  coord_cartesian(
    xlim = c(1, 1.25)
  ) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 10, 5, 5)
  ) +
  labs(
    x = NULL,
    y = NULL,
    colour = NULL,
    title = "Heimavallaráhrif í Bestu deild karla",
    subtitle = "Hlutfallsleg áhrif heimavallar á sóknar- varnar- og heildarstyrk félagsliða"
  )
