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

kelly_fraction <- function(q, n_eff,
                           f_min = 0.05, f_max = 0.50,
                           kappa = 6, mu = 0.5, alpha = 1.0,
                           n0 = 80,
                           prev_f = NA, hysteresis = 0.05,
                           league_cap = 0.50) {
  # 1) shrink quantile toward 0.5 with prior strength n0
  q_tilde <- (n_eff * q + n0 * 0.02) / (n_eff + n0)
  
  # 2) smooth logistic map
  g <- 1 / (1 + exp(-kappa * (q_tilde - mu)))
  
  # 3) scaled fraction with curvature
  f_new <- f_min + (f_max - f_min) * (g ^ alpha)
  
  # 4) apply league-level cap
  f_new <- pmin(f_new, league_cap)
  
  # 5) hysteresis (optional)
  if (!is.na(prev_f) && abs(f_new - prev_f) < hysteresis) {
    return(prev_f)
  }
  f_new
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

sheet_url <- "https://docs.google.com/spreadsheets/d/1Q2OIOTgKNZ1w-9Drgth6MT9OzI7LU6aD_OTfB0tr8WU/edit?gid=574008674#gid=574008674"

outcome_odds <- read_sheet(
  sheet_url,
  sheet = "Niðurstaða"
) |>
  mutate_at(vars(date_obs, date_game), as_date) |>
  filter(date_game >= today()) |>
  fill(booker, date_obs, date_game, .direction = "down") |> 
  filter(
    date_obs == max(date_obs),
    .by = c(booker, date_game, country, home, away)
  )

total_goals_odds <- read_sheet(
  sheet_url,
  sheet = "Mörk"
) |>
  mutate_at(vars(date_obs, date_game), as_date) |>
  filter(date_game >= today()) |>
  fill(booker, date_obs, date_game, .direction = "down") |> 
  filter(
    date_obs == max(date_obs),
    .by = c(booker, date_game, country, home, away, limit)
  )

handicap_odds <- read_sheet(
  sheet_url,
  sheet = "Forgjöf"
) |>
  mutate_at(vars(date_obs, date_game), as_date) |>
  filter(date_game >= today()) |>
  fill(booker, date_obs, date_game, .direction = "down") |> 
  filter(
    date_obs == max(date_obs),
    .by = c(booker, date_game, country, home, away, change)
  )

bets <- read_sheet(
  sheet_url,
  sheet = "Bets"
) |>
  select(
    date = dags_leikur,
    heima,
    gestir,
    tegund = type
  ) |> 
  mutate_at(vars(date), as_date) |>
  filter(date >= today())

#### Country & Sex ####
country <- c("denmark", "france", "germany", "hungary", "poland", "sweden", "norway", "spain", "austria", "portugal")
sex <- c("male", "female")

post <- crossing(
  country = country,
  sex = sex
) |> 
  mutate(
    id = row_number() 
  ) |> 
  group_by(id) |> 
  group_map(
    safely(
      \(d, ...) {
        here("results", d$country, d$sex, today(), "posterior_goals.csv") |>
          read_csv() |>
          mutate(
            sex = d$sex,
            country = d$country |> str_replace("-", " ") |> str_to_title(),
            iteration = as.numeric(iteration),
            game_nr = as.numeric(game_nr),
            division = as.numeric(division),
            date = as_date(date)
          )
      },
      otherwise = tibble()
    )
  ) |> 
  map("result") |> 
  list_rbind() |> 
  ungroup() |> 
  mutate(
    sex = if_else(
      sex == "male",
      "kk",
      "kvk"
    )
  )

crossing(
  country = str_to_title(country),
  sex = sex
) |> 
  mutate(
    sex = if_else(sex == "male", "kk", "kvk")
  ) |> 
  anti_join(
    post |> 
      distinct(country, sex) 
  )



# What percent of optimal Kelly am I willing to bet?
kelly_frac <- 0.02
bet_digits <- 1
min_bet_amount <- 1

# What's my current pool size?
cur_pool <- 208 + 243

#### Outcome ####
outcome_results <- post |>
  mutate(
    goal_diff = home_goals - away_goals
  ) |>
  summarise(
    p_home = mean(goal_diff > 0.5),
    p_away = mean(goal_diff < -0.5),
    p_tie = mean(abs(goal_diff) <= 0.5),
    .by = c(date, division, country, home, away, sex)
  ) |>
  inner_join(
    outcome_odds |>
      select(
        date = date_game,
        booker,
        sex,
        home,
        away,
        o_home:o_away
      ) 
  ) |>
  select(date, division, country, booker, sex, home, away, everything()) |>
  pivot_longer(
    -c(date, division, country, booker, sex, home, away),
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
    .by = c(gestir, division, country, date, sex, heima, booker)
  ) |>
  filter(
    kelly == max(kelly),
    .by = c(heima, division, country, date, sex, gestir, outcome)
  ) |>
  group_by(date, division, country, heima, sex, gestir, outcome) |>
  slice(1) |>
  ungroup() |>
  mutate(
    kelly = get_kelly(p, o),
    .by = c(date, division, country, sex, heima, gestir)
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
    country,
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
    country,
    division,
    booker,
    sex,
    heima,
    gestir,
    home,
    away,
    bet_amount
  ) |>
  arrange(date, division, booker)

#### Handicap ####

forgjof_results <- handicap_odds |>
  rename(date = date_game) |>
  inner_join(
    post |>
      mutate(
        value = home_goals - away_goals
      ) |>
      select(
        date,
        country,
        division,
        sex,
        game_nr,
        home,
        away,
        value
      ),
    relationship = "many-to-many"
  ) |>
  select(
    date,
    country,
    division,
    booker,
    sex,
    leikur = game_nr,
    heima = home,
    gestir = away,
    change,
    o_home,
    o_away,
    value
  ) |>
  summarise(
    p_home = mean(value + change > 0),
    p_away = mean(value + change < 0),
    .by = c(
      date,
      country,
      division,
      booker,
      sex,
      leikur,
      heima,
      gestir,
      change,
      o_home,
      o_away
    )
  ) |>
  pivot_longer(
    c(o_home:p_away),
    names_to = c("type", "outcome"),
    names_sep = "_"
  ) |>
  pivot_wider(names_from = type) |>
  mutate(
    kelly = get_kelly(p, o),
    .by = c(booker, country, sex, gestir, heima, change)
  ) |>
  filter(
    kelly == max(kelly),
    .by = c(heima, country, gestir, outcome, sex)
  ) |>
  group_by(heima, country, gestir, outcome, sex) |>
  slice(1) |>
  ungroup() |>
  mutate(
    kelly = get_kelly(p, o),
    .by = c(booker, gestir, heima, sex)
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
    country,
    division,
    booker,
    sex,
    heima,
    gestir,
    change,
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
    country,
    division,
    booker,
    sex,
    heima,
    gestir,
    change,
    home,
    away,
    bet_amount
  ) |>
  arrange(date, division, booker)

#### Stigafjöldi ####
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
    country,
    division,
    booker,
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
    .by = c(date, country, division, sex, heima, gestir, booker, o_over, o_under, limit)
  ) |>
  pivot_longer(
    c(o_over:p_under, -limit),
    names_to = c("type", "outcome"),
    names_sep = "_"
  ) |>
  pivot_wider(names_from = type) |>
  mutate(
    kelly = get_kelly(p, o),
    .by = c(booker, country, gestir, sex, heima, limit)
  ) |>
  filter(
    kelly == max(kelly),
    .by = c(heima, country, gestir, outcome, sex)
  ) |>
  group_by(sex, country, date, heima, gestir, outcome) |>
  slice(1) |>
  ungroup() |>
  mutate(
    kelly = get_kelly(p, o),
    .by = c(gestir, country, heima, sex)
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
    country,
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
    country,
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
  arrange(date,country, division, booker)

#### Results Overview ####


outcome_results |>
  filter(bet_amount > min_bet_amount) |>
  select(-bet_amount) |> 
  anti_join(
    bets |>
      filter(tegund == "Niðurstaða")
  ) |>
  arrange(division, date)

forgjof_results |>
  filter(bet_amount > min_bet_amount) |>
  select(-bet_amount) |> 
  anti_join(
    bets |>
      filter(tegund == "Forgjöf")
  ) |>
  arrange(division, date)

stigafjoldi_results |>
  filter(bet_amount > min_bet_amount) |>
  select(-bet_amount) |> 
  anti_join(
    bets |>
      filter(tegund == "Markafjöldi")
  ) |>
  arrange(division, date)

remove_colnames <- function(d) {
  first_row <- d[1, ] |> purrr:::map_chr(as.character)
  names(d) <- first_row
  d[-1, ]
}

post |>
  mutate(
    goal_diff = home_goals - away_goals
  ) |>
  summarise(
    p_home = mean(goal_diff > 0.5),
    p_away = mean(goal_diff < -0.5),
    p_tie = mean(abs(goal_diff) <= 0.5),
    .by = c(date, division, sex, home, away, country)
  ) |>
  inner_join(
    outcome_odds |>
      select(
        date = date_game,
        booker,
        sex,
        home,
        away,
        o_home:o_away
      )
  ) |>
  select(date, division, country, sex, booker,  home, away, everything()) |>
  pivot_longer(
    -c(date, division, booker, sex, home, away, country),
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
    .by = c(gestir, sex, division, date, heima, booker, country)
  ) |>
  filter(
    kelly == max(kelly),
    .by = c(heima, sex, division, date, gestir, outcome, country)
  ) |>
  group_by(date, division, sex, heima, gestir, outcome, country) |>
  slice(1) |>
  ungroup() |>
  mutate(
    kelly = get_kelly(p, o),
    .by = c(date, division, heima, gestir, country)
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
    text = if_else(bet_amount < min_bet_amount, "", text),
    dags_bet = today(),
    type = "Niðurstaða",
    deild = "iceland",
    bet = if_else(outcome == "home", "heima", "gestir"),
    info = ""
  ) |> 
  select(
    dags_bet,
    booker,
    type,
    sex,
    country,
    dags_leikur = date,
    heima,
    gestir,
    bet,
    info,
    amount = bet_amount,
    odds = o,
    prob = p
  ) |> 
  filter(amount > min_bet_amount) |> 
  anti_join(
    bets
  ) |>  
  remove_colnames() |> 
  clipr::write_clip(return_new = TRUE)


handicap_odds |>
  rename(date = date_game) |>
  inner_join(
    post |>
      mutate(
        value = home_goals - away_goals
      ) |>
      select(
        date,
        division,
        game_nr,
        sex,
        home,
        away,
        value
      ),
    relationship = "many-to-many"
  ) |>
  select(
    date,
    division,
    country,
    booker,
    sex,
    leikur = game_nr,
    heima = home,
    gestir = away,
    change,
    o_home,
    o_away,
    value
  ) |>
  summarise(
    p_home = mean(value + change > 0),
    p_away = mean(value + change < 0),
    .by = c(
      date,
      division,
      country,
      booker,
      sex,
      leikur,
      heima,
      gestir,
      change,
      o_home,
      o_away
    )
  ) |>
  pivot_longer(
    c(o_home:p_away),
    names_to = c("type", "outcome"),
    names_sep = "_"
  ) |>
  pivot_wider(names_from = type) |>
  mutate(
    kelly = get_kelly(p, o),
    .by = c(country, booker, gestir, heima, change, sex)
  ) |>
  filter(
    kelly == max(kelly),
    .by = c(country, heima, gestir, outcome, sex)
  ) |>
  group_by(heima, gestir, country, outcome, sex) |>
  slice(1) |>
  ungroup() |>
  mutate(
    kelly = get_kelly(p, o),
    .by = c(booker, gestir, heima, country, sex)
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
    text = if_else(bet_amount < min_bet_amount, "", text),
    dags_bet = today(),
    type = "Forgjöf",
    deild = "iceland",
    bet = if_else(outcome == "home", "heima", "gestir"),
    info = change
  ) |> 
  select(
    dags_bet,
    booker,
    type,
    sex,
    country,
    dags_leikur = date,
    heima,
    gestir,
    bet,
    info,
    amount = bet_amount,
    odds = o,
    prob = p
  ) |> 
  filter(amount > min_bet_amount) |> 
  anti_join(
    bets
  ) |>
  remove_colnames() |> 
  clipr::write_clip(return_new = TRUE)

post |>
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
    sex,
    country,
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
    .by = c(date, division, country, sex, heima, gestir, booker, o_over, o_under, limit)
  ) |>
  pivot_longer(
    c(o_over:p_under, -limit),
    names_to = c("type", "outcome"),
    names_sep = "_"
  ) |>
  pivot_wider(names_from = type) |>
  mutate(
    kelly = get_kelly(p, o),
    .by = c(booker, country, sex, gestir, heima, limit)
  ) |>
  filter(
    kelly == max(kelly),
    .by = c(heima, country, sex, gestir, outcome)
  ) |>
  group_by(date, country, sex, heima, gestir, outcome) |>
  slice(1) |>
  ungroup() |>
  mutate(
    kelly = get_kelly(p, o),
    .by = c(gestir, heima, sex)
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
    text = if_else(bet_amount < min_bet_amount, "", text),
    dags_bet = today(),
    type = "Markafjöldi",
    deild = "iceland",
    bet = if_else(outcome == "over", "yfir", "undir"),
    info = limit
  ) |> 
  select(
    dags_bet,
    booker,
    type,
    sex,
    country,
    dags_leikur = date,
    heima,
    gestir,
    bet,
    info,
    amount = bet_amount,
    odds = o,
    prob = p
  ) |> 
  filter(amount > min_bet_amount) |> 
  anti_join(
    bets,
    by = join_by(type == tegund, heima, gestir)
  ) |>
  remove_colnames() |> 
  clipr::write_clip(return_new = TRUE)
