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

from_season <- 2021

#### Data Prep ####
results <- read_rds(here("results", "male", "fit.rds"))

d <- read_csv(here("results", "male", "d.csv"))
teams <- read_csv(here("results", "male", "teams.csv"))
next_games <- read_csv(here("results", "male", "next_games.csv"))
top_teams <- read_csv(here("results", "male", "top_teams.csv"))
pred_d <- read_csv(here("results", "male", "pred_d.csv"))


#### Next-Round Predictions ####

posterior_goals <- results$draws(c("goals1_pred", "goals2_pred")) |>
  as_draws_df() |>
  as_tibble() |>
  pivot_longer(
    -c(.draw, .chain, .iteration),
    names_to = "parameter",
    values_to = "value"
  ) |>
  mutate(
    type = if_else(str_detect(parameter, "goals1"), "home_goals", "away_goals"),
    game_nr = str_match(parameter, "d\\[(.*)\\]$")[, 2] |> as.numeric()
  ) |>
  select(.draw, type, game_nr, value) |>
  pivot_wider(names_from = type, values_from = value) |>
  inner_join(
    pred_d, 
    by = "game_nr"
  ) |>
  filter(
    date <= today() + 7,
    date >= today()
  ) |>
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

posterior_goals |>
  write_csv(
    here("results", "male", "posterior_goals.csv")
  )


posterior_goals |>
  mutate(
    goal_diff = abs(away_goals - home_goals),
    .by = c(home, away)
  ) |>
  arrange(desc(goal_diff)) |>
  filter(goal_diff > 12)

predictions <- posterior_goals |>
  mutate(
    goal_diff = away_goals - home_goals,
    .by = c(home, away)
  ) |>
  filter(
    abs(goal_diff) <= 9,
    division %in% c(1, 2, 3, 6)
  ) |>
  count(date, division, home, away, goal_diff, game_nr) |>
  mutate(
    p = n / sum(n),
    mean_value = sum(goal_diff * p),
    home_win = sum(p[goal_diff < 0]),
    away_win = sum(p[goal_diff > 0]),
    draw = sum(p[goal_diff == 0]),
    p = p / max(p), # Normalize for visualization
    .by = c(home, away)
  ) |>
  mutate(
    game_nr = as.numeric(as.factor(game_nr)),
    game_nr = max(game_nr) - game_nr + 1,
    match = str_c(home, " - ", away) |>
      fct_reorder(game_nr),
    division = c("BD", "LD", "ÖD", "ÞD", "FjD", "MB")[division]
  ) |>
  mutate(
    home = glue(
      "{home} ({percent(home_win, accuracy = 1)}) | {format(date, '%d. %B')} - {division}"
    ),
    away = glue("{away} ({percent(away_win, accuracy = 1)})"),
    .by = c(home, away)
  ) |>
  arrange(desc(game_nr)) |>
  mutate(
    outcome = case_when(
      goal_diff == 0 ~ "Tie",
      goal_diff < 0 ~ "Home win",
      goal_diff > 0 ~ "Away win",
      TRUE ~ "Tie"
    ),
    .by = c(home, away)
  )


tie_color <- "#252525"
home_win_color <- "#377eb8"
away_win_color <- "#e41a1c"

plot <- predictions |>
  filter(
    abs(goal_diff) <= 8 # Limit to reasonable goal differences
  ) |>
  ggplot(aes(y = game_nr)) +
  geom_segment(
    aes(
      x = goal_diff,
      xend = goal_diff,
      y = game_nr,
      yend = game_nr + 0.8 * p,
      col = outcome
    ),
    linewidth = 14
  ) +
  geom_segment(
    aes(
      x = mean_value,
      xend = mean_value,
      y = game_nr,
      yend = game_nr + 0.9
    ),
    linewidth = 3,
    col = "#4daf4a"
  ) +
  geom_vline(
    xintercept = 0,
    lty = 2,
    col = "grey50"
  ) +
  geom_richtext(
    data = data.frame(x = 1),
    inherit.aes = FALSE,
    x = -8,
    y = -Inf,
    label.colour = NA,
    fill = NA,
    label = "&larr; Heimalið vinnur",
    hjust = 0,
    vjust = 0,
    size = 4.5,
    colour = "grey40"
  ) +
  geom_richtext(
    data = data.frame(x = 1),
    inherit.aes = FALSE,
    x = 8,
    y = -Inf,
    label.colour = NA,
    fill = NA,
    label = "Gestir vinna &rarr;",
    hjust = 1,
    vjust = 0,
    size = 4.5,
    colour = "grey40"
  ) +
  scale_x_continuous(
    guide = guide_axis(cap = "both"),
    breaks = seq(-8, 8),
    labels = function(x) abs(x),
    expand = expansion(add = 1),
    limits = c(-8, 8),
  ) +
  scale_y_continuous(
    guide = guide_axis(cap = "both"),
    breaks = seq(max(predictions$game_nr), 1),
    labels = predictions |>
      distinct(game_nr, home, away, match) |>
      arrange(desc(game_nr)) |>
      pull(home),
    expand = expansion(),
    sec.axis = sec_axis(
      transform = function(x) x,
      breaks = seq(max(predictions$game_nr), 1),
      labels = predictions |>
        distinct(game_nr, home, away, match) |>
        arrange(desc(game_nr)) |>
        pull(away),
      guide = guide_axis(cap = "both")
    )
  ) +
  scale_colour_manual(
    values = c(
      "Tie" = tie_color,
      "Home win" = home_win_color,
      "Away win" = away_win_color
    ),
    guide = "none"
  ) +
  coord_cartesian(
    ylim = c(0.5, max(predictions$game_nr) + 0.9),
    clip = "off"
  ) +
  theme(
    axis.text.y = element_text(
      face = "bold"
    )
  ) +
  labs(
    x = "Markamismunur",
    y = NULL,
    colour = NULL,
    title = paste0(
      "Vikuleg fótboltaspá Metils fyrir Bestu deild (BD) karla og suma Mjólkurbikarsleiki (MB)"
    ),
    subtitle = str_c(
      "Líkindadreifing spár fyrir komandi leiki",
      " | ",
      "Sigurlíkur merktar innan sviga",
      " | ",
      "Líkur á jafntefli eru 100% mínus líkur hvors liðs",
      " | ",
      "\nGrænar línur eru meðalspár"
    ),
    caption = "metill.is"
  )

plot


n_games <- predictions |>
  distinct(game_nr) |>
  pull(game_nr) |>
  length()

ratio <- 0.6 + 0.3 * n_games / 10

ggsave(
  plot = plot,
  filename = here(
    "results",
    "male",
    "figures",
    "next_week_preds.png"
  ),
  width = 8,
  height = ratio * 8,
  scale = 1.7
)

#### League Result Prediction ####

posterior_goals <- results$draws(c("goals1_pred", "goals2_pred")) |>
  as_draws_df() |>
  as_tibble() |>
  pivot_longer(
    -c(.draw, .chain, .iteration),
    names_to = "parameter",
    values_to = "value"
  ) |>
  mutate(
    type = if_else(str_detect(parameter, "goals1"), "home_goals", "away_goals"),
    game_nr = str_match(parameter, "d\\[(.*)\\]$")[, 2] |> as.numeric()
  ) |>
  select(.draw, type, game_nr, value) |>
  pivot_wider(names_from = type, values_from = value) |>
  inner_join(pred_d, by = "game_nr") |>
  filter(
    division == 1
  ) |>
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

base_points <- d |>
  filter(season == 2025, division == 1) |>
  mutate(
    result = case_when(
      home_goals > away_goals ~ "home",
      home_goals < away_goals ~ "away",
      TRUE ~ "tie"
    )
  ) |>
  pivot_longer(c(home, away), values_to = "team") |>
  mutate(
    points = case_when(
      result == "tie" ~ 1,
      result == name ~ 3,
      TRUE ~ 0
    )
  ) |>
  summarise(
    base_points = sum(points),
    .by = c(team)
  ) |>
  arrange(desc(base_points))

p_top <- posterior_goals |>
  mutate(
    result = case_when(
      home_goals > away_goals ~ "home",
      home_goals < away_goals ~ "away",
      TRUE ~ "tie"
    )
  ) |>
  pivot_longer(c(home, away), values_to = "team") |>
  mutate(
    points = case_when(
      result == "tie" ~ 1,
      result == name ~ 3,
      TRUE ~ 0
    )
  ) |>
  summarise(
    points = sum(points),
    .by = c(iteration, team)
  ) |>
  left_join(
    base_points
  ) |>
  mutate(
    base_points = coalesce(base_points, 0),
    points = points + base_points
  ) |>
  arrange(desc(points)) |>
  mutate(
    position = row_number(),
    .by = iteration
  ) |>
  summarise(
    p_top = mean(position <= 6),
    .by = team
  ) |>
  arrange(desc(p_top))

plot_dat <- posterior_goals |>
  mutate(
    result = case_when(
      home_goals > away_goals ~ "home",
      home_goals < away_goals ~ "away",
      TRUE ~ "tie"
    )
  ) |>
  pivot_longer(c(home, away), values_to = "team") |>
  mutate(
    points = case_when(
      result == "tie" ~ 1,
      result == name ~ 3,
      TRUE ~ 0
    )
  ) |>
  summarise(
    points = sum(points),
    .by = c(iteration, team)
  ) |>
  left_join(
    base_points
  ) |>
  mutate(
    base_points = coalesce(base_points, 0),
    points = points + base_points
  ) |>
  count(team, points) |>
  mutate(
    p_raw = n / sum(n),
    p = p_raw / max(p_raw),
    mean = sum(p_raw * points),
    .by = team
  ) |>
  inner_join(p_top) |>
  mutate(
    p_top = scales::percent(p_top, accuracy = 1),
    team = glue("{team} ({p_top})"),
    team = fct_reorder(team, mean),
    team_nr = as.numeric(team)
  )


plot_dat |>
  ggplot(aes(y = team_nr)) +
  geom_segment(
    aes(
      x = points,
      xend = points,
      y = team_nr,
      yend = team_nr + 0.8 * p
    ),
    linewidth = 5
  ) +
  geom_segment(
    aes(
      x = mean,
      xend = mean,
      y = team_nr,
      yend = team_nr + 0.9
    ),
    linewidth = 2,
    col = "#4daf4a"
  ) +
  scale_x_continuous(
    limits = c(0, NA),
    expand = expansion(add = c(0, 7)),
    breaks = breaks_width(10),
    guide = guide_axis(cap = "both")
  ) +
  scale_y_continuous(
    guide = guide_axis(cap = "both"),
    breaks = seq(1, 12),
    labels = plot_dat |>
      distinct(team, team_nr) |>
      arrange(team_nr) |>
      pull(team),
    expand = expansion()
  ) +
  labs(
    title = "Hvaða lið munu komast í efri hluta umspils Bestu deildar karla",
    subtitle = str_c(
      "Líkindadreifing spár um stigafjölda liða í lok deildar",
      " | ",
      "Líkur á að ná í efri hluta umspils innan sviga",
      " | ",
      "Grænar línur eru meðalspár"
    ),
    caption = "metill.is",
    x = "Stigafjöldi í lok tímabils",
    y = NULL
  )

ggsave(
  filename = here("results", "male", "figures", "umspil_top.png"),
  width = 8,
  height = 0.5 * 8,
  scale = 1.6
)

#### League Winner ####

plot_dat <- posterior_goals |>
  mutate(
    result = case_when(
      home_goals > away_goals ~ "home",
      home_goals < away_goals ~ "away",
      TRUE ~ "tie"
    )
  ) |>
  pivot_longer(c(home, away), values_to = "team") |>
  mutate(
    points = case_when(
      result == "tie" ~ 1,
      result == name ~ 3,
      TRUE ~ 0
    )
  ) |>
  summarise(
    points = sum(points),
    .by = c(iteration, team)
  ) |>
  left_join(
    base_points
  ) |>
  mutate(
    base_points = coalesce(base_points, 0),
    points = points + base_points
  ) |>
  arrange(iteration, desc(points)) |>
  mutate(
    placement = row_number(),
    .by = iteration
  ) |>
  count(team, placement) |>
  mutate(
    p_raw = n / sum(n),
    p = p_raw / max(p_raw),
    mean = sum(p_raw * placement),
    .by = team
  ) |>
  inner_join(p_top) |>
  mutate(
    team = fct_reorder(team, mean),
    team_nr = as.numeric(team)
  )

plot_dat |>
  mutate(
    p = n / sum(n),
    .by = placement
  ) |>
  ggplot(aes(placement, p)) +
  geom_col(
    position = "stack",
    aes(fill = team),
    width = 0.95,
    col = "black",
    linewidth = 0.1
  ) +
  scale_x_continuous(
    guide = guide_axis(cap = "both"),
    breaks = 1:12
  ) +
  scale_y_continuous(
    guide = guide_axis(cap = "both"),
    breaks = breaks_width(0.25),
    labels = label_hlutf()
  ) +
  scale_fill_manual(
    values = c(
      "Víkingur R." = "#b30000",
      "Breiðablik" = "#006d2c",
      "Valur" = "#ce1256",
      "KR" = "white",
      "Stjarnan" = "#08519c",
      "ÍA" = "#fec44f",
      "KA" = "#9ecae1",
      "ÍBV" = "black",
      "Fram" = "#4292c6",
      "FH" = "#d9d9d9",
      "Vestri" = "#08306b",
      "Afturelding" = "#e31a1c"
    )
  ) +
  facet_wrap("team", ncol = 4) +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Líkindadreifing yfir lokasæti í Bestu deild karla fyrir umspil haustsins"
  )

ggsave(
  filename = here("results", "male", "figures", "deild_top.png"),
  width = 8,
  height = 0.5 * 8,
  scale = 1.2
)

#### Posterior Results ####

#### Current Strengths ####

plot_dat_away <- results$draws("cur_strength_away") |>
  as_draws_df() |>
  as_tibble() |>
  pivot_longer(c(-.chain, -.draw, -.iteration)) |>
  mutate(
    team = teams$team[parse_number(name)],
    type = "Samtals"
  ) |>
  bind_rows(
    results$draws("cur_offense_away") |>
      as_draws_df() |>
      as_tibble() |>
      pivot_longer(c(-.chain, -.draw, -.iteration)) |>
      mutate(
        team = teams$team[parse_number(name)],
        type = "Sókn"
      )
  ) |>
  bind_rows(
    results$draws("cur_defense_away") |>
      as_draws_df() |>
      as_tibble() |>
      pivot_longer(c(-.chain, -.draw, -.iteration)) |>
      mutate(
        team = teams$team[parse_number(name)],
        type = "Vörn"
      )
  ) |>
  reframe(
    median = median(value),
    coverage = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
    lower = quantile(value, 0.5 - coverage / 2),
    upper = quantile(value, 0.5 + coverage / 2),
    .by = c(team, type)
  ) |>
  mutate(
    type = as_factor(type) |>
      fct_relevel("Sókn", "Vörn", "Samtals"),
    team = factor(
      team,
      levels = unique(team)[order(unique(median[type == "Samtals"]))]
    )
  )

plot_dat_home <- results$draws("cur_strength_home") |>
  as_draws_df() |>
  as_tibble() |>
  pivot_longer(c(-.chain, -.draw, -.iteration)) |>
  mutate(
    team = teams$team[parse_number(name)],
    type = "Samtals"
  ) |>
  bind_rows(
    results$draws("cur_offense_home") |>
      as_draws_df() |>
      as_tibble() |>
      pivot_longer(c(-.chain, -.draw, -.iteration)) |>
      mutate(
        team = teams$team[parse_number(name)],
        type = "Sókn"
      )
  ) |>
  bind_rows(
    results$draws("cur_defense_home") |>
      as_draws_df() |>
      as_tibble() |>
      pivot_longer(c(-.chain, -.draw, -.iteration)) |>
      mutate(
        team = teams$team[parse_number(name)],
        type = "Vörn"
      )
  ) |>
  reframe(
    median = median(value),
    coverage = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
    lower = quantile(value, 0.5 - coverage / 2),
    upper = quantile(value, 0.5 + coverage / 2),
    .by = c(team, type)
  ) |>
  mutate(
    type = as_factor(type) |>
      fct_relevel("Sókn", "Vörn", "Samtals"),
    team = factor(
      team,
      levels = unique(team)[order(unique(median[type == "Samtals"]))]
    )
  )

plot_dat <- plot_dat_away |>
  mutate(
    loc = "Gestir"
  ) |>
  bind_rows(
    plot_dat_home |>
      mutate(
        loc = "Heima"
      )
  ) |>
  mutate(
    loc = as_factor(loc) |>
      fct_relevel("Heima")
  ) 

dodge <- 0.3

plot_dat |>
  semi_join(
    d |>
      filter(season == 2025, division == 1) |>
      pivot_longer(c(home, away), values_to = "team") |>
      distinct(team)
  ) |> 
  ggplot(aes(median, team)) +
  geom_hline(
    yintercept = seq(1, 12, 2),
    linewidth = 8,
    alpha = 0.05
  ) +
  geom_point(
    shape = "|",
    size = 5,
    aes(col = loc)
  ) +
  geom_segment(
    aes(
      x = lower,
      xend = upper,
      yend = team,
      alpha = -coverage,
      col = loc
    ),
    linewidth = 2
  ) +
  scale_alpha_continuous(
    range = c(0, 0.3),
    guide = guide_none()
  ) +
  scale_x_continuous(
    guide = guide_none(),
    expand = expansion(mult = c(0.01, 0.05))
  ) +
  scale_y_discrete(
    guide = guide_axis(cap = "both")
  ) +
  scale_colour_brewer(
    palette = "Set1",
    direction = -1
  ) +
  facet_wrap("type", scales = "free_x") +
  theme(
    plot.margin = margin(5, 5, 5, 5),
    legend.position = c(0.5, 1.128),
    legend.background = element_blank(),
    legend.direction = "horizontal",
    legend.text = element_text(family = "Lato", colour = "#525252")
  ) +
  labs(
    x = NULL,
    y = NULL,
    colour = NULL,
    title = "Styrkur félagsliða í Bestu deild karla",
    subtitle = "Metið með fótboltalíkani Metils"
  )

ggsave(
  filename = here("results", "male", "figures", "styrkur.png"),
  width = 8,
  height = 0.4 * 8,
  scale = 1.1
)


#### Home Advantages ####

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
    top_teams |>
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
    xlim = c(1, 2)
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

ggsave(
  filename = here(
    "results",
    "male",
    "figures",
    "home_advantage.png"
  ),
  width = 8,
  height = 0.35 * 8,
  scale = 1.4
)
