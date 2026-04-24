library(tidyverse)
library(here)
library(metill)
theme_set(theme_metill())
Sys.setlocale("LC_ALL", "is_IS.UTF-8")

sex <- "female"
ncol <- if (sex == "female") 5 else 4

aspect_ratio <- 0.6
breaks <- breaks_pretty(10)

#### League Placement ####
d <- here("results", sex, "historical") |>
  list.files(full.names = TRUE, pattern = "^[0-9].*") |>
  here("placement.csv") |>
  map(
    \(x) {
      read_csv(x) |>
        mutate(
          fit_date = x
        )
    }
  ) |>
  list_rbind() |>
  mutate(
    fit_date = as_date(fit_date)
  ) |>
  # filter(
  #   !fit_date %in% clock::date_build(
  #     2025,
  #     c(4, 5, 5),
  #     c(27, 4, 10)
  #   )
  # ) |>
  mutate(
    ordered_mean = unique(mean[fit_date == max(fit_date)]),
    .by = team
  )


p <- d |>
  arrange(
    fit_date,
    team,
    placement
  ) |>
  mutate(
    team = fct_reorder(team, ordered_mean, .na_rm = TRUE)
  ) |>
  inner_join(
    d |>
      distinct(fit_date) |>
      mutate(
        from_date = fit_date,
        to_date = lead(fit_date, default = max(fit_date) + 1)
      )
  ) |>
  ggplot(aes(fit_date, placement)) +
  geom_rect(
    aes(
      xmin = from_date,
      xmax = to_date,
      ymin = placement - 0.5,
      ymax = placement + 0.5,
      fill = p,
      alpha = p
    )
  ) +
  geom_step(
    data = d |>
      filter(
        p >= 0.9,
        .by = c(fit_date, team)
      ) |>
      summarise(
        mean = mean(placement),
        .by = c(fit_date, team, ordered_mean)
      ) |>
      mutate(
        team = fct_reorder(team, ordered_mean, .na_rm = TRUE)
      ),
    aes(y = mean),
    col = "red",
    linewidth = 1,
    position = position_nudge(x = 0.5)
  ) +
  scale_x_date(
    guide = guide_axis(cap = "both"),
    labels = label_date_short(
      format = c("", "%B", "%d", "")
    ),
    breaks = breaks
  ) +
  scale_y_reverse(
    guide = guide_axis(cap = "both"),
    breaks = 1:12
  ) +
  scale_fill_gradient(
    low = "#fdfcfc",
    high = "black"
  ) +
  facet_wrap(
    "team",
    ncol = ncol
  ) +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Þróun spár fótboltalíkans metils fyrir Bestu deild kvenna",
    subtitle = str_c(
      "Líkindadreifing yfir sæti liða í deildinni",
      " | ",
      "Líklegri niðurstaða er dekkri",
      " | ",
      "Rauð lína er líklegasta niðurstaða hvers liðs"
    )
  )


ggsave(
  plot = p,
  filename = here("results", sex, "historical", "placement.png"),
  width = 8,
  height = aspect_ratio * 8,
  scale = 1.3
)


p <- d |>
  arrange(
    fit_date,
    team,
    placement
  ) |>
  mutate(
    team = fct_reorder(team, ordered_mean, .na_rm = TRUE)
  ) |>
  summarise(
    mean = unique(mean),
    q5 = quantile(rep(placement, n), 0.055),
    q95 = quantile(rep(placement, n), 0.945),
    .by = c(team, fit_date)
  ) |>
  group_by(team) |>
  mutate_at(
    vars(mean, q5, q95),
    \(x) loess(x ~ seq_along(x)) |> predict()
  ) |>
  ggplot(aes(fit_date, mean)) +
  geom_ribbon(
    aes(ymin = q5, ymax = q95),
    alpha = 0.1
  ) +
  geom_line(
    linewidth = 1,
    col = "black"
  ) +
  scale_x_date(
    guide = guide_axis(cap = "both"),
    labels = label_date_short(
      format = c("", "%B", "%d", "")
    ),
    breaks = breaks
  ) +
  scale_y_reverse(
    guide = guide_axis(cap = "both"),
    breaks = 1:12
  ) +
  facet_wrap(
    "team",
    ncol = ncol
  ) +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Þróun spár fótboltalíkans metils fyrir Bestu deild kvenna",
    subtitle = str_c(
      "Hvaða sæti spáði líkanið liðunum hverju sinni?",
      " | ",
      "Svört lína er meðalspá",
      " | ",
      "Grátt svæði er 89% spábil"
    ),
    caption = "Af hverju 89? 89 er hæsta prímtalan sem er lægri en 95 sem eru álíka góð rök og fyrir að nota 95%."
  )


ggsave(
  plot = p,
  filename = here("results", sex, "historical", "placement_simple.png"),
  width = 8,
  height = aspect_ratio * 8,
  scale = 1.3
)

#### League Points ####
d <- here("results", sex, "historical") |>
  list.files(full.names = TRUE, pattern = "^[0-9].*") |>
  here("league_points.csv") |>
  map(
    \(x) {
      read_csv(x) |>
        mutate(
          fit_date = x
        )
    }
  ) |>
  list_rbind() |>
  mutate(
    fit_date = as_date(fit_date),
    team = str_replace(team, " \\([0-9]+\\%\\)", "")
  ) |>
  # filter(
  #   !fit_date %in% clock::date_build(
  #     2025,
  #     c(4, 5, 5),
  #     c(27, 4, 10)
  #   )
  # ) |>
  mutate(
    ordered_mean = unique(mean[fit_date == max(fit_date)]),
    .by = team
  )

p <- d |>
  arrange(
    fit_date,
    team,
    points
  ) |>
  mutate(
    team = fct_reorder(team, -ordered_mean, .na_rm = TRUE)
  ) |>
  inner_join(
    d |>
      distinct(fit_date) |>
      mutate(
        from_date = fit_date,
        to_date = lead(fit_date, default = max(fit_date) + 1)
      )
  ) |>
  ggplot(aes(fit_date, points)) +
  geom_rect(
    aes(
      xmin = from_date,
      xmax = to_date,
      ymin = points - 0.5,
      ymax = points + 0.5,
      fill = p
    )
  ) +
  geom_step(
    data = d |>
      distinct(fit_date, team, mean, ordered_mean) |>
      mutate(
        team = fct_reorder(team, -ordered_mean, .na_rm = TRUE)
      ),
    aes(y = mean),
    col = "red",
    linewidth = 1,
    position = position_nudge(x = 0.5)
  ) +
  scale_x_date(
    guide = guide_axis(cap = "both"),
    labels = label_date_short(
      format = c("", "%B", "%d", "")
    ),
    breaks = breaks
  ) +
  scale_y_continuous(
    guide = guide_axis(cap = "both")
  ) +
  scale_fill_gradient(
    low = "#fdfcfc",
    high = "black"
  ) +
  facet_wrap(
    "team",
    ncol = ncol
  ) +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Þróun spár fótboltalíkans metils fyrir Bestu deild kvenna",
    subtitle = str_c(
      "Líkindadreifing yfir stigafjölda liða",
      " | ",
      "Líklegri niðurstaða er dekkri",
      " | ",
      "Rauð lína er meðalspá hvers liðs"
    )
  )

ggsave(
  plot = p,
  filename = here("results", sex, "historical", "points.png"),
  width = 8,
  height = aspect_ratio * 8,
  scale = 1.3
)


p <- d |>
  arrange(
    fit_date,
    team,
    points
  ) |>
  mutate(
    team = fct_reorder(team, -ordered_mean, .na_rm = TRUE)
  ) |>
  summarise(
    mean = unique(mean),
    q5 = quantile(rep(points, n), 0.055),
    q95 = quantile(rep(points, n), 0.945),
    .by = c(team, fit_date)
  ) |>
  group_by(team) |>
  mutate_at(
    vars(mean, q5, q95),
    \(x) loess(x ~ seq_along(x)) |> predict()
  ) |>
  ggplot(aes(fit_date, mean)) +
  geom_ribbon(
    aes(ymin = q5, ymax = q95),
    alpha = 0.1
  ) +
  geom_line(
    linewidth = 1,
    col = "black"
  ) +
  scale_x_date(
    guide = guide_axis(cap = "both"),
    labels = label_date_short(
      format = c("", "%B", "%d", "")
    ),
    breaks = breaks
  ) +
  scale_y_continuous(
    guide = guide_axis(cap = "both")
  ) +
  facet_wrap(
    "team",
    ncol = ncol
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Þróun spár fótboltalíkans metils fyrir Bestu deild kvenna",
    subtitle = str_c(
      "Hvað spáði líkanið liðum mörgum stigum hverju sinni?",
      " | ",
      "Svört lína er meðalspá",
      " | ",
      "Grátt svæði er 89% spábil"
    ),
    caption = "Af hverju 89? 89 er hæsta prímtalan sem er lægri en 95 sem eru álíka góð rök og fyrir að nota 95%."
  )


ggsave(
  plot = p,
  filename = here("results", sex, "historical", "points_simple.png"),
  width = 8,
  height = aspect_ratio * 8,
  scale = 1.3
)
