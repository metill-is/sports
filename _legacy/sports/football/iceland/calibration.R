library(tidyverse)
library(here)
library(metill)
theme_set(theme_metill())
Sys.setlocale("LC_ALL", "is_IS.UTF-8")

sex <- "male"
ncol <- if (sex == "male") 4 else 5

ntiles <- 10

#### League Placement ####
d <- here("results", sex, "historical") |>
  list.files(full.names = TRUE, pattern = "^[0-9].*") |>
  here("posterior_goals.csv") |>
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
  )

results <- here("results", "male", "d.csv") |>
  read_csv()

d |>
  mutate(
    pred = case_when(
      home_goals > away_goals ~ "home",
      home_goals < away_goals ~ "away",
      TRUE ~ "tie"
    )
  ) |>
  summarise(
    p = sum(p),
    .by = c(date, fit_date, home, away, pred)
  ) |>
  inner_join(
    results |>
      mutate(
        result = case_when(
          home_goals > away_goals ~ "home",
          home_goals < away_goals ~ "away",
          TRUE ~ "tie"
        )
      ) |>
      select(
        date,
        home,
        away,
        result
      )
  ) |>
  filter(
    fit_date < date
  ) |>
  filter(
    fit_date == max(fit_date),
    .by = c(date, home, away)
  ) |>
  mutate(
    outcome = 1 * (result == pred)
  ) |>
  mutate(
    group = floor(p * ntiles) / ntiles
  ) |>
  summarise(
    pred = mean(p),
    obs = mean(outcome),
    .by = c(group)
  ) |>
  ggplot(aes(pred, obs)) +
  geom_abline(
    intercept = 0,
    slope = 1,
    lty = 2
  ) +
  geom_point() +
  scale_x_continuous(
    limits = c(0, 1),
    labels = label_percent(),
    guide = guide_axis(cap = "both")
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = label_percent(),
    guide = guide_axis(cap = "both")
  ) +
  labs(
    title = "Kvörðun á spá fótboltalíkans um niðurstöðu (heima/gestir/jafntefli) hjá körlum",
    subtitle = "Í fullkomnum heimi gerast hlutir jafnoft (sama %) og líkanið spáir fyrir um",
    x = "P(X = x)",
    y = "Hlutfall leikja þar sem niðurstaðan var X = x"
  )

ggsave(
  filename = "calibration_male.png",
  width = 8,
  height = 0.5 * 8,
  scale = 1.3
)

# results |>
#   select(date, home, away, home_goals, away_goals) |>
#   inner_join(
#     d |>
#       select(
#         date, home, away, fit_date, p,
#         home_pred = home_goals,
#         away_pred = away_goals
#       )
#   ) |>
#   filter(
#     fit_date < date
#   ) |>
#   filter(
#     fit_date == max(fit_date),
#     .by = c(date, home, away)
#   ) |>
#   mutate(
#     correct = 1 * (home_goals == home_pred) * (away_goals == away_pred),
#     group = ntile(p, 10)
#   ) |>
#   summarise(
#     p = mean(p),
#     obs = mean(correct),
#     .by = group
#   ) |>
#   ggplot(aes(p, obs)) +
#   geom_abline(
#     intercept = 0,
#     slope = 1,
#     lty = 2
#   ) +
#   geom_point() +
#   scale_x_continuous(
#     labels = label_percent(),
#     guide = guide_axis(cap = "both")
#   ) +
#   scale_y_continuous(
#     labels = label_percent(),
#     guide = guide_axis(cap = "both")
#   ) +
#   labs(
#     title = "Kvörðun á spá fótboltalíkans um markafjölda hvors liðs",
#     x = "P(X = x, Y = y)",
#     y = "P(spá er rétt)"
#   )

# d |>
#   mutate(
#     pred = home_goals + away_goals
#   ) |>
#   summarise(
#     p = sum(p),
#     .by = c(date, fit_date, home, away, pred)
#   ) |>
#   inner_join(
#     results |>
#       mutate(
#         total_goals = home_goals + away_goals
#       ) |>
#       select(
#         date, home, away, total_goals
#       )
#   ) |>
#   filter(
#     fit_date < date
#   ) |>
#   filter(
#     fit_date == max(fit_date),
#     .by = c(date, home, away)
#   ) |>
# mutate(
#   outcome = 1 * (result == pred)
# ) |>
#   mutate(
#     group = ntile(p, 10)
#   ) |>
#   summarise(
#     pred = mean(p),
#     obs = mean(outcome),
#     .by = c(group)
#   ) |>
#   ggplot(aes(pred, obs)) +
#   geom_abline(
#     intercept = 0,
#     slope = 1,
#     lty = 2
#   ) +
#   geom_point() +
#   scale_x_continuous(
#     limits = c(0, 1),
#     labels = label_percent(),
#     guide = guide_axis(cap = "both")
#   ) +
#   scale_y_continuous(
#     limits = c(0, 1),
#     labels = label_percent(),
#     guide = guide_axis(cap = "both")
#   ) +
#   labs(
#     title = "Kvörðun á spá fótboltalíkans um niðurstöðu (Heima/Gestir/Jafntefli)",
#     subtitle = "Í fullkomnum heimi gerast hlutir jafnoft (sama %) og líkanið spáir fyrir um",
#     x = "P(X = x)",
#     y = "Hlutfall leikja þar sem niðurstaðan var X = x"
#   )

#### Female ####

library(tidyverse)
library(here)
library(metill)
theme_set(theme_metill())
Sys.setlocale("LC_ALL", "is_IS.UTF-8")

sex <- "female"
ncol <- if (sex == "male") 4 else 5

#### League Placement ####
d <- here("results", sex, "historical") |>
  list.files(full.names = TRUE, pattern = "^[0-9].*") |>
  here("posterior_goals.csv") |>
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
  )

results <- here("results", sex, "d.csv") |>
  read_csv()

d |>
  mutate(
    pred = case_when(
      home_goals > away_goals ~ "home",
      home_goals < away_goals ~ "away",
      TRUE ~ "tie"
    )
  ) |>
  summarise(
    p = sum(p),
    .by = c(date, fit_date, home, away, pred)
  ) |>
  inner_join(
    results |>
      mutate(
        result = case_when(
          home_goals > away_goals ~ "home",
          home_goals < away_goals ~ "away",
          TRUE ~ "tie"
        )
      ) |>
      select(
        date,
        home,
        away,
        result
      )
  ) |>
  filter(
    fit_date < date
  ) |>
  filter(
    fit_date == max(fit_date),
    .by = c(date, home, away)
  ) |>
  mutate(
    outcome = 1 * (result == pred)
  ) |>
  mutate(
    group = floor(p * ntiles) / ntiles
  ) |>
  summarise(
    pred = mean(p),
    obs = mean(outcome),
    .by = c(group)
  ) |>
  ggplot(aes(pred, obs)) +
  geom_abline(
    intercept = 0,
    slope = 1,
    lty = 2
  ) +
  geom_point() +
  scale_x_continuous(
    limits = c(0, 1),
    labels = label_percent(),
    guide = guide_axis(cap = "both")
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = label_percent(),
    guide = guide_axis(cap = "both")
  ) +
  labs(
    title = "Kvörðun á spá fótboltalíkans um niðurstöðu (heima/gestir/jafntefli) hjá konum",
    subtitle = "Í fullkomnum heimi gerast hlutir jafnoft (sama %) og líkanið spáir fyrir um",
    x = "P(X = x)",
    y = "Hlutfall leikja þar sem niðurstaðan var X = x"
  )

ggsave(
  filename = "calibration_female.png",
  width = 8,
  height = 0.5 * 8,
  scale = 1.3
)
