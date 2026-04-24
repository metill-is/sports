library(tidyverse)
library(here)
library(purrr)
library(glue)
library(scales)
library(ggtext)
library(metill)
theme_set(theme_metill())

lw <- 5

hosts <- c("Norway", "Denmark", "Sweden")
d <- read_csv(
  here("results", "male", today() - 1, "d.csv")
) |>
  select(-division) |>
  filter(
    date >= clock::date_build(2026, 01, 8)
  ) |>
  arrange(date) |>
  mutate(
    game_nr = row_number() |> rev(),
    .by = date
  ) |>
  arrange(date, game_nr) |>
  mutate(
    game_nr = row_number()
  ) |>
  arrange(game_nr) |>
  mutate(
    home2 = case_when(
      away %in% hosts ~ away,
      TRUE ~ home
    ),
    away2 = case_when(
      home2 != home ~ home,
      TRUE ~ away
    ),
    home_goals2 = case_when(
      away %in% hosts ~ away_goals,
      TRUE ~ home_goals
    ),
    away_goals2 = case_when(
      home2 != home ~ home_goals,
      TRUE ~ away_goals
    ),
  ) |>
  select(-home, -away, -home_goals, -away_goals) |>
  rename(
    home = home2,
    away = away2,
    home_goals = home_goals2,
    away_goals = away_goals2
  )

posterior_goals <- here("results", "male") |>
  list.files(pattern = "[0-9]+-[0-9]+-[0-9]+", full.names = TRUE) |>
  list.files(pattern = "posterior_goals", full.names = TRUE) |>
  map(
    function(x) {
      read_csv(x) |>
        mutate(
          fit_date = ymd(x)
        )
    }
  ) |>
  list_rbind() |>
  select(-game_nr, -division)

posterior_goals

#### Goal Diff ####

plot_dat <- posterior_goals |>
  semi_join(
    d,
    by = join_by(
      date,
      home,
      away
    )
  ) |>
  mutate(
    goal_diff = away_goals - home_goals
  ) |>
  reframe(
    median = median(goal_diff),
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
    lower = quantile(goal_diff, 0.5 - coverage / 2),
    upper = quantile(goal_diff, 0.5 + coverage / 2),
    home_win = mean(goal_diff < -0.5),
    away_win = mean(goal_diff > 0.5),
    .by = c(date, home, away, fit_date)
  ) |>
  mutate(
    home_win = percent(home_win, accuracy = 1),
    away_win = percent(away_win, accuracy = 1),
    home_label = glue("{home} ({home_win})"),
    away_label = glue("{away} ({away_win})")
  ) |>
  inner_join(
    d,
    by = join_by(
      date,
      home,
      away
    )
  ) |>
  filter(
    fit_date < date
  ) |>
  filter(
    fit_date == max(fit_date),
    .by = c(home, away)
  )

p1 <- plot_dat |>
  ggplot(aes(median, max(game_nr) - game_nr + 1)) +
  geom_vline(
    xintercept = 0,
    lty = 2,
    alpha = 0.4,
    linewidth = 0.3
  ) +
  geom_hline(
    yintercept = seq(1, length(unique(plot_dat$game_nr)), 2),
    linewidth = lw,
    alpha = 0.1
  ) +
  geom_point(
    shape = "|",
    size = 5
  ) +
  geom_point(
    shape = "|",
    size = 5,
    col = "red",
    aes(x = away_goals - home_goals)
  ) +
  geom_segment(
    aes(
      x = lower,
      xend = upper,
      yend = max(game_nr) - game_nr + 1,
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
    labels = \(x) abs(x),
    breaks = seq(-30, 30, by = 10)
  ) +
  scale_y_continuous(
    guide = guide_axis(cap = "both"),
    breaks = seq(length(unique(plot_dat$game_nr)), 1),
    labels = \(x) {
      plot_dat |>
        distinct(game_nr, home, away) |>
        arrange(game_nr) |>
        pull(home)
    },
    sec.axis = sec_axis(
      transform = \(x) x,
      breaks = seq(length(unique(plot_dat$game_nr)), 1),
      labels = \(x) {
        plot_dat |>
          distinct(game_nr, home, away) |>
          arrange(game_nr) |>
          pull(away)
      },
      guide = guide_axis(cap = "both")
    )
  ) +
  coord_cartesian(
    ylim = c(1, max(plot_dat$game_nr) - min(plot_dat$game_nr) + 1),
    xlim = c(-25, 25),
    clip = "off"
  ) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 5)
  ) +
  labs(
    x = "Markamismunur",
    y = NULL,
    colour = NULL,
    title = "Hversu vel hefur Handboltaspá Metils gengið?",
    subtitle = str_c(
      "Niðurstaða leiks sýnd með rauðu",
      " | ",
      "Sigurlíkur merktar inni í sviga"
    )
  )


p11 <- posterior_goals |>
  mutate(
    diff_pred = home_goals - away_goals
  ) |>
  select(
    iteration,
    date,
    fit_date,
    home,
    away,
    diff_pred
  ) |>
  inner_join(
    d |>
      mutate(
        diff_obs = home_goals - away_goals
      ) |>
      select(
        game_nr,
        date,
        home,
        away,
        diff_obs
      ),
    by = join_by(
      date,
      home,
      away
    )
  ) |>
  summarise(
    p = mean(diff_obs < diff_pred),
    .by = c(game_nr, date, fit_date, home, away)
  ) |>
  filter(
    fit_date < date
  ) |>
  filter(
    fit_date == max(fit_date),
    .by = c(game_nr)
  ) |>
  arrange(p) |>
  mutate(
    o = row_number() / (n() + 1)
  ) |>
  ggplot(aes(o, p)) +
  geom_abline(
    intercept = 0,
    slope = 1
  ) +
  geom_point() +
  scale_x_continuous(
    guide = guide_axis(cap = "both"),
    breaks = seq(0, 1, by = 0.1),
    labels = label_percent(),
    limits = c(0, 1)
  ) +
  scale_y_continuous(
    guide = guide_axis(cap = "both"),
    breaks = seq(0, 1, by = 0.1),
    labels = label_percent(),
    limits = c(0, 1)
  ) +
  labs(
    subtitle = "Markamismunur"
  )


#### Total Goals ####

plot_dat <- posterior_goals |>
  semi_join(
    d,
    by = join_by(
      date,
      home,
      away
    )
  ) |>
  mutate(
    total_goals = home_goals + away_goals
  ) |>
  reframe(
    median = median(total_goals),
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
    lower = quantile(total_goals, 0.5 - coverage / 2),
    upper = quantile(total_goals, 0.5 + coverage / 2),
    .by = c(date, home, away, fit_date)
  ) |>
  inner_join(
    d,
    by = join_by(
      date,
      home,
      away
    )
  ) |>
  filter(
    fit_date < date
  ) |>
  filter(
    fit_date == max(fit_date),
    .by = c(home, away)
  )

p2 <- plot_dat |>
  ggplot(aes(median, max(game_nr) - game_nr + 1)) +
  geom_hline(
    yintercept = seq(1, length(unique(plot_dat$game_nr)), 2),
    linewidth = lw,
    alpha = 0.1
  ) +
  geom_point(
    shape = "|",
    size = 5
  ) +
  geom_point(
    shape = "|",
    size = 5,
    col = "red",
    aes(x = away_goals + home_goals)
  ) +
  geom_segment(
    aes(
      x = lower,
      xend = upper,
      yend = max(game_nr) - game_nr + 1,
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
    breaks = seq(30, 80, by = 10)
  ) +
  scale_y_continuous(
    guide = guide_axis(cap = "both"),
    breaks = seq(length(unique(plot_dat$game_nr)), 1),
    labels = \(x) {
      plot_dat |>
        distinct(game_nr, home, away) |>
        arrange(game_nr) |>
        pull(home)
    },
    sec.axis = sec_axis(
      transform = \(x) x,
      breaks = seq(length(unique(plot_dat$game_nr)), 1),
      labels = \(x) {
        plot_dat |>
          distinct(game_nr, home, away) |>
          arrange(game_nr) |>
          pull(away)
      },
      guide = guide_axis(cap = "both")
    )
  ) +
  coord_cartesian(
    ylim = c(1, max(plot_dat$game_nr) - min(plot_dat$game_nr) + 1),
    clip = "off"
  ) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 5)
  ) +
  labs(
    x = "Markafjöldi",
    y = NULL,
    colour = NULL,
    title = "Hversu vel hefur Handboltaspá Metils gengið að spá markafjölda?",
    subtitle = str_c(
      "Raunverulegur fjöldi marka leikja sýndur með rauðu"
    )
  )


p22 <- posterior_goals |>
  mutate(
    diff_pred = home_goals + away_goals
  ) |>
  select(
    iteration,
    date,
    fit_date,
    home,
    away,
    diff_pred
  ) |>
  inner_join(
    d |>
      mutate(
        diff_obs = home_goals + away_goals
      ) |>
      select(
        game_nr,
        date,
        home,
        away,
        diff_obs
      ),
    by = join_by(
      date,
      home,
      away
    )
  ) |>
  summarise(
    p = mean(diff_obs < diff_pred),
    .by = c(game_nr, date, fit_date, home, away)
  ) |>
  filter(
    fit_date < date
  ) |>
  filter(
    fit_date == max(fit_date),
    .by = c(game_nr)
  ) |>
  arrange(p) |>
  mutate(
    o = row_number() / (n() + 1)
  ) |>
  ggplot(aes(o, p)) +
  geom_abline(
    intercept = 0,
    slope = 1
  ) +
  geom_point() +
  scale_x_continuous(
    guide = guide_axis(cap = "both"),
    breaks = seq(0, 1, by = 0.1),
    labels = label_percent(),
    limits = c(0, 1)
  ) +
  scale_y_continuous(
    guide = guide_axis(cap = "both"),
    breaks = seq(0, 1, by = 0.1),
    labels = label_percent(),
    limits = c(0, 1)
  ) +
  labs(
    subtitle = "Heildarfjöldi stiga"
  )


#### Home Goals ####

plot_dat <- posterior_goals |>
  semi_join(
    d,
    by = join_by(
      date,
      home,
      away
    )
  ) |>
  mutate(
    total_goals = home_goals
  ) |>
  reframe(
    median = median(total_goals),
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
    lower = quantile(total_goals, 0.5 - coverage / 2),
    upper = quantile(total_goals, 0.5 + coverage / 2),
    .by = c(date, home, away, fit_date)
  ) |>
  inner_join(
    d,
    by = join_by(
      date,
      home,
      away
    )
  ) |>
  filter(
    fit_date < date
  ) |>
  filter(
    fit_date == max(fit_date),
    .by = c(home, away)
  )

p3 <- plot_dat |>
  ggplot(aes(median, max(game_nr) - game_nr + 1)) +
  geom_hline(
    yintercept = seq(1, length(unique(plot_dat$game_nr)), 2),
    linewidth = lw,
    alpha = 0.1
  ) +
  geom_point(
    shape = "|",
    size = 5
  ) +
  geom_point(
    shape = "|",
    size = 5,
    col = "red",
    aes(x = home_goals)
  ) +
  geom_segment(
    aes(
      x = lower,
      xend = upper,
      yend = max(game_nr) - game_nr + 1,
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
    breaks = seq(10, 50, by = 5)
  ) +
  scale_y_continuous(
    guide = guide_axis(cap = "both"),
    breaks = seq(length(unique(plot_dat$game_nr)), 1),
    labels = \(x) {
      plot_dat |>
        distinct(game_nr, home, away) |>
        arrange(game_nr) |>
        pull(home)
    },
    sec.axis = sec_axis(
      transform = \(x) x,
      breaks = seq(length(unique(plot_dat$game_nr)), 1),
      labels = \(x) {
        plot_dat |>
          distinct(game_nr, home, away) |>
          arrange(game_nr) |>
          pull(away)
      },
      guide = guide_axis(cap = "both")
    )
  ) +
  coord_cartesian(
    ylim = c(1, max(plot_dat$game_nr) - min(plot_dat$game_nr) + 1),
    clip = "off"
  ) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 5),
    axis.text.y.left = element_text(face = "bold")
  ) +
  labs(
    x = "Markafjöldi",
    y = NULL,
    colour = NULL,
    title = 'Hversu vel hefur gengið að spá markafjölda "heimaliðs"?',
    subtitle = str_c(
      'Raunverulegur fjöldi marka "heimaliða" sýndur með rauðu'
    )
  )

p33 <- posterior_goals |>
  mutate(
    diff_pred = home_goals
  ) |>
  select(
    iteration,
    date,
    fit_date,
    home,
    away,
    diff_pred
  ) |>
  inner_join(
    d |>
      mutate(
        diff_obs = home_goals
      ) |>
      select(
        game_nr,
        date,
        home,
        away,
        diff_obs
      ),
    by = join_by(
      date,
      home,
      away
    )
  ) |>
  summarise(
    p = mean(diff_obs < diff_pred),
    .by = c(game_nr, date, fit_date, home, away)
  ) |>
  filter(
    fit_date < date
  ) |>
  filter(
    fit_date == max(fit_date),
    .by = c(game_nr)
  ) |>
  arrange(p) |>
  mutate(
    o = row_number() / (n() + 1)
  ) |>
  ggplot(aes(o, p)) +
  geom_abline(
    intercept = 0,
    slope = 1
  ) +
  geom_point() +
  scale_x_continuous(
    guide = guide_axis(cap = "both"),
    breaks = seq(0, 1, by = 0.1),
    labels = label_percent(),
    limits = c(0, 1)
  ) +
  scale_y_continuous(
    guide = guide_axis(cap = "both"),
    breaks = seq(0, 1, by = 0.1),
    labels = label_percent(),
    limits = c(0, 1)
  ) +
  labs(
    subtitle = "Heimalið"
  )

#### Away Goals ####

plot_dat <- posterior_goals |>
  semi_join(
    d,
    by = join_by(
      date,
      home,
      away
    )
  ) |>
  mutate(
    total_goals = away_goals
  ) |>
  reframe(
    median = median(total_goals),
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
    lower = quantile(total_goals, 0.5 - coverage / 2),
    upper = quantile(total_goals, 0.5 + coverage / 2),
    .by = c(date, home, away, fit_date)
  ) |>
  inner_join(
    d,
    by = join_by(
      date,
      home,
      away
    )
  ) |>
  filter(
    fit_date < date
  ) |>
  filter(
    fit_date == max(fit_date),
    .by = c(home, away)
  )

p4 <- plot_dat |>
  ggplot(aes(median, max(game_nr) - game_nr + 1)) +
  geom_hline(
    yintercept = seq(1, length(unique(plot_dat$game_nr)), 2),
    linewidth = lw,
    alpha = 0.1
  ) +
  geom_point(
    shape = "|",
    size = 5
  ) +
  geom_point(
    shape = "|",
    size = 5,
    col = "red",
    aes(x = away_goals)
  ) +
  geom_segment(
    aes(
      x = lower,
      xend = upper,
      yend = max(game_nr) - game_nr + 1,
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
    breaks = seq(10, 45, by = 5)
  ) +
  scale_y_continuous(
    guide = guide_axis(cap = "both"),
    breaks = seq(length(unique(plot_dat$game_nr)), 1),
    labels = \(x) {
      plot_dat |>
        distinct(game_nr, home, away) |>
        arrange(game_nr) |>
        pull(home)
    },
    sec.axis = sec_axis(
      transform = \(x) x,
      breaks = seq(length(unique(plot_dat$game_nr)), 1),
      labels = \(x) {
        plot_dat |>
          distinct(game_nr, home, away) |>
          arrange(game_nr) |>
          pull(away)
      },
      guide = guide_axis(cap = "both")
    )
  ) +
  coord_cartesian(
    ylim = c(1, max(plot_dat$game_nr) - min(plot_dat$game_nr) + 1),
    clip = "off"
  ) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 5),
    axis.text.y.right = element_text(face = "bold")
  ) +
  labs(
    x = "Markafjöldi",
    y = NULL,
    colour = NULL,
    title = 'Hversu vel hefur gengið að spá markafjölda "gestaliðs"?',
    subtitle = str_c(
      'Raunverulegur fjöldi stiga "gestaliða" sýndur með rauðu'
    )
  )


p44 <- posterior_goals |>
  mutate(
    diff_pred = away_goals
  ) |>
  select(
    iteration,
    date,
    fit_date,
    home,
    away,
    diff_pred
  ) |>
  inner_join(
    d |>
      mutate(
        diff_obs = away_goals
      ) |>
      select(
        game_nr,
        date,
        home,
        away,
        diff_obs
      ),
    by = join_by(
      date,
      home,
      away
    )
  ) |>
  summarise(
    p = mean(diff_obs < diff_pred),
    .by = c(game_nr, date, fit_date, home, away)
  ) |>
  filter(
    fit_date < date
  ) |>
  filter(
    fit_date == max(fit_date),
    .by = c(game_nr)
  ) |>
  arrange(p) |>
  mutate(
    o = row_number() / (n() + 1)
  ) |>
  ggplot(aes(o, p)) +
  geom_abline(
    intercept = 0,
    slope = 1
  ) +
  geom_point() +
  scale_x_continuous(
    guide = guide_axis(cap = "both"),
    breaks = seq(0, 1, by = 0.1),
    labels = label_percent(),
    limits = c(0, 1)
  ) +
  scale_y_continuous(
    guide = guide_axis(cap = "both"),
    breaks = seq(0, 1, by = 0.1),
    labels = label_percent(),
    limits = c(0, 1)
  ) +
  labs(
    subtitle = "Gestalið"
  )


library(patchwork)


(p3 +
  theme(
    axis.text.y.right = element_blank(),
    axis.line.y.right = element_blank(),
    axis.ticks.y.right = element_blank()
  ) +
  labs(
    title = NULL,
    subtitle = 'Mörk "heimaliðs"',
    x = NULL
  )) +
  (p1 +
    theme(
      axis.text.y.right = element_blank(),
      axis.line.y.right = element_blank(),
      axis.ticks.y.right = element_blank(),
      axis.text.y.left = element_blank(),
      axis.line.y.left = element_blank(),
      axis.ticks.y.left = element_blank()
    ) +
    labs(
      title = NULL,
      subtitle = "Markamismunur",
      x = NULL
    )) +
  (p2 +
    theme(
      axis.text.y.right = element_blank(),
      axis.line.y.right = element_blank(),
      axis.ticks.y.right = element_blank(),
      axis.text.y.left = element_blank(),
      axis.line.y.left = element_blank(),
      axis.ticks.y.left = element_blank()
    ) +
    labs(
      title = NULL,
      subtitle = "Heildarfjöldi marka",
      x = NULL
    )) +
  (p4 +
    theme(
      axis.text.y.left = element_blank(),
      axis.line.y.left = element_blank(),
      axis.ticks.y.left = element_blank()
    ) +
    labs(
      title = NULL,
      subtitle = 'Mörk "gestaliðs"',
      x = NULL
    )) +
  plot_layout(
    nrow = 1
  ) +
  plot_annotation(
    title = "Hversu vel hefur gengið að spá markafjölda keppandi liða?",
    subtitle = "Raunverulegur fjöldi marka liðanna sýndur með rauðu | Meðalspá og óvissubil sýnd í svörtum lit"
  )


ggsave(
  filename = here("results", "male", "accuracy_home_away.png"),
  width = 8,
  height = 0.9 * 8,
  scale = 1.5
)


p11 +
  p22 +
  p33 +
  p44 +
  plot_annotation(
    title = "Kvörðun á spám og niðurstöðu leikja"
  )


ggsave(
  filename = here("results", "male", "calibration.png"),
  width = 8,
  height = 0.621 * 8,
  scale = 1.5
)



posterior_goals |>
  semi_join(
    d,
    by = join_by(
      date,
      home,
      away
    )
  ) |>
  mutate(
    total_goals = home_goals + away_goals
  ) |>
  summarise(
    home_win = mean(home_goals - away_goals > 0.5),
    away_win = mean(away_goals - home_goals > 0.5),
    tie = 1 - home_win - away_win,
    .by = c(date, home, away, fit_date)
  ) |>
  inner_join(
    d |> 
      mutate(
        result = case_when(
          home_goals - away_goals > 0.5 ~ "home_win",
          away_goals - home_goals > 0.5 ~ "away_win",
          TRUE ~ "tie"
        )
      ),
    by = join_by(
      date,
      home,
      away
    )
  ) |> 
  filter(fit_date < date) |> 
  filter(
    fit_date == max(fit_date),
    .by = c(home, away, date)
  ) |> 
  select(date, home, away, home_win:tie, result) |> 
  pivot_longer(c(home_win:tie)) |> 
  mutate(
    correct = 1 * (name == result)
  ) |> 
  mutate(
    group = ntile(value, 8),
    .by = name
  ) |> 
  summarise(
    p = mean(correct),
    q = mean(value),
    .by = c(group, name)
  ) |> 
  arrange(name, q)
