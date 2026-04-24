get_kelly <- function(p, o, p0 = NULL) {
  if (is.null(p0)) {
    p0 <- 1 - p
  }

  R <- o - 1

  kelly_objective <- function(f) {
    sum(p * log(1 + f * R) + p0 * log(1 - f))
  }

  f_init <- rep(1 / length(p), length(p))

  constraint <- function(f) {
    sum(f) - 1
  }

  lb <- rep(0, length(p))
  ub <- rep(1, length(p))

  # Run optimization
  result <- nloptr(
    x0 = f_init,
    eval_f = function(f) -kelly_objective(f), # Negative to maximize
    eval_g_ineq = function(f) constraint(f), # Inequality constraint
    lb = lb,
    ub = ub, # Bound constraints
    opts = list("algorithm" = "NLOPT_LN_COBYLA", "xtol_rel" = 1e-8)
  )

  result$solution
}

library(tidyverse)
library(googlesheets4)
library(metill)
library(purrr)
library(nloptr)
library(here)
library(gt)
library(gtExtras)
theme_set(theme_metill())
gs4_auth(email = Sys.getenv("GOOGLE_MAIL"))

sheet_url <- "https://docs.google.com/spreadsheets/d/1cJJo8SVJsMQvdzkM3h_pltjS55gbxvuSu_4ID_f8RUM/edit?gid=278114765#gid=278114765"

outcome_odds <- read_sheet(
  sheet_url,
  sheet = "Outcome"
) |>
  mutate_at(vars(date_obs, date_game), as_date) |>
  filter(date_game >= today(), booker != "Bet365") |>
  fill(booker, date_obs, date_game, .direction = "down")

total_goals_odds <- read_sheet(
  sheet_url,
  sheet = "Total Goals"
) |>
  mutate_at(vars(date_obs, date_game), as_date) |>
  filter(date_game >= today(), booker != "Bet365") |>
  fill(booker, date_obs, date_game, .direction = "down")


post <- c("male") |>
  map(
    \(x) {
      here("results", x, "posterior_goals.csv") |>
        read_csv() |>
        mutate(
          sex = x
        )
    }
  ) |>
  list_rbind() |>
  filter(
    date >= today()
  )

# What percent of optimal Kelly am I willing to bet?
kelly_frac <- 0.01
bet_digits <- 1
min_bet_amount <- 0.3

# What's my current pool size?
cur_pool <- 400

#### Outcome ####
outcome_results <- post |>
  mutate(
    goal_diff = home_goals - away_goals
  ) |>
  summarise(
    p_home = mean(goal_diff > 0),
    p_away = mean(goal_diff < 0),
    p_tie = mean(goal_diff == 0),
    .by = c(date, division, sex, home, away)
  ) |>
  inner_join(
    outcome_odds |>
      select(
        date = date_game,
        booker,
        country,
        sex,
        home,
        away,
        o_home:o_away
      )
  ) |>
  select(date, division, sex, booker, country, home, away, everything()) |>
  pivot_longer(
    -c(date, division, booker, country, sex, home, away),
    names_to = c("type", "outcome"),
    names_sep = "_"
  ) |>
  pivot_wider(names_from = type) |>
  rename(
    heima = home,
    gestir = away
  ) |>
  mutate(
    kelly = get_kelly(p, o),
    .by = c(gestir, division, date, heima, booker, sex)
  ) |>
  filter(
    kelly == max(kelly),
    .by = c(heima, division, date, gestir, outcome, sex)
  ) |>
  group_by(date, division, heima, gestir, outcome, sex) |>
  slice(1) |>
  ungroup() |>
  mutate(
    kelly = get_kelly(p, o),
    .by = c(date, division, heima, gestir, sex)
  ) |>
  mutate(
    ev = round(p * (o - 1) - (1 - p), 2),
    kelly = kelly * kelly_frac,
    bet_amount = round(kelly * cur_pool, bet_digits),
    kelly = round(kelly, 2),
    pred = round(p, 3),
    p_o = round(1 / o, 3),
    text = glue::glue(
      "€={bet_amount} (f={kelly})[ev={ev},p={p},o={p_o}=1/{o}]"
    ),
    text = if_else(bet_amount < min_bet_amount, "", text)
  ) |>
  select(
    date,
    division,
    booker,
    sex,
    heima,
    gestir,
    outcome,
    text,
    bet_amount
  ) |>
  pivot_wider(
    names_from = outcome,
    values_from = text,
    values_fill = ""
  ) |>
  select(
    date,
    division,
    booker,
    sex,
    heima,
    gestir,
    home,
    tie,
    away,
    bet_amount
  ) |>
  arrange(date, division, sex, booker)

stigafjoldi_results <- post |>
  inner_join(
    total_goals_odds |>
      rename(date = date_game)
  ) |>
  mutate(
    total_goals = home_goals + away_goals
  ) |>
  select(
    date,
    division,
    booker,
    country,
    sex,
    heima = home,
    gestir = away,
    total_goals,
    limit,
    o_over,
    o_under
  ) |>
  summarise(
    p_over = mean(total_goals > limit),
    p_under = 1 - p_over,
    .by = c(date, division, heima, sex, gestir, booker, o_over, o_under, limit)
  ) |>
  pivot_longer(
    c(o_over:p_under, -limit),
    names_to = c("type", "outcome"),
    names_sep = "_"
  ) |>
  pivot_wider(names_from = type) |>
  mutate(
    kelly = get_kelly(p, o),
    .by = c(booker, sex, gestir, heima, limit)
  ) |>
  filter(
    kelly == max(kelly),
    .by = c(heima, gestir, outcome, sex)
  ) |>
  group_by(date, heima, gestir, outcome, sex) |>
  slice(1) |>
  ungroup() |>
  mutate(
    kelly = get_kelly(p, o),
    .by = c(sex, gestir, heima)
  ) |>
  mutate(
    ev = round(p * (o - 1) - (1 - p), 2),
    kelly = kelly * kelly_frac,
    bet_amount = round(kelly * cur_pool, bet_digits),
    kelly = round(kelly, 2),
    pred = round(p, 3),
    p_o = round(1 / o, 3),
    text = glue::glue(
      "€={bet_amount} (f={kelly})[ev={ev},p={p},o={p_o}=1/{o}]"
    ),
    text = if_else(bet_amount < min_bet_amount, "", text)
  ) |>
  select(
    date,
    division,
    booker,
    sex,
    heima,
    gestir,
    limit,
    outcome,
    text,
    bet_amount
  ) |>
  pivot_wider(
    names_from = outcome,
    values_from = text,
    values_fill = ""
  ) |>
  select(
    date,
    division,
    booker,
    sex,
    heima,
    gestir,
    limit,
    over,
    under,
    bet_amount
  ) |>
  arrange(date, division, sex, booker)

outcome_results |>
  filter(bet_amount > min_bet_amount) |>
  select(-bet_amount)

stigafjoldi_results |>
  filter(bet_amount > min_bet_amount) |>
  select(-bet_amount)
