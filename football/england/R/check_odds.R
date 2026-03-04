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

sheet_url <- "https://docs.google.com/spreadsheets/d/1n5Wcrg-eO3urluOrm5bTwbe4B3yUPGFkVQ0_0h-t_jg/edit?gid=1774709455#gid=1774709455"

outcome_odds <- read_sheet(
  sheet_url,
  sheet = "Outcome"
) |>
  mutate_at(vars(date_obs, date_game), as_date) |>
  filter(date_game >= today() - 1, booker != "Bet365") |>
  fill(booker, date_obs, date_game, .direction = "down")

total_goals_odds <- read_sheet(
  sheet_url,
  sheet = "Total Goals"
) |>
  mutate_at(vars(date_obs, date_game), as_date) |>
  filter(date_game >= today() - 1, booker != "Bet365") |>
  fill(booker, date_obs, date_game, .direction = "down")

handicap_odds <- read_sheet(
  sheet_url,
  sheet = "Handicap"
) |>
  mutate_at(vars(date_obs, date_game), as_date) |>
  filter(date_game >= today() - 1) |>
  fill(booker, date_obs, date_game, .direction = "down")

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
kelly_frac <- 0.015
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
    .by = c(date, division, home, away)
  ) |>
  inner_join(
    outcome_odds |>
      select(
        date = date_game,
        booker,
        home,
        away,
        o_home:o_away
      )
  ) |>
  select(date, division, booker, home, away, everything()) |>
  pivot_longer(
    -c(date, division, booker, home, away),
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
    .by = c(gestir, division, date, heima, booker)
  ) |>
  filter(
    kelly == max(kelly),
    .by = c(heima, division, date, gestir, outcome)
  ) |>
  group_by(date, division, heima, gestir, outcome) |>
  slice(1) |>
  ungroup() |>
  mutate(
    kelly = get_kelly(p, o),
    .by = c(date, division, heima, gestir)
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
    heima,
    gestir,
    home,
    tie,
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
        division,
        game_nr,
        home,
        away,
        value
      ),
    relationship = "many-to-many"
  ) |>
  select(
    date,
    division,
    booker,
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
      booker,
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
    .by = c(booker, gestir, heima, change)
  ) |>
  filter(
    kelly == max(kelly),
    .by = c(heima, gestir, outcome)
  ) |>
  group_by(heima, gestir, outcome) |>
  slice(1) |>
  ungroup() |>
  mutate(
    kelly = get_kelly(p, o),
    .by = c(booker, gestir, heima)
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
    division,
    booker,
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
    division,
    booker,
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
    .by = c(date, division, heima, gestir, booker, o_over, o_under, limit)
  ) |>
  pivot_longer(
    c(o_over:p_under, -limit),
    names_to = c("type", "outcome"),
    names_sep = "_"
  ) |>
  pivot_wider(names_from = type) |>
  mutate(
    kelly = get_kelly(p, o),
    .by = c(booker, gestir, heima, limit)
  ) |>
  filter(
    kelly == max(kelly),
    .by = c(heima, gestir, outcome)
  ) |>
  group_by(date, heima, gestir, outcome) |>
  slice(1) |>
  ungroup() |>
  mutate(
    kelly = get_kelly(p, o),
    .by = c(gestir, heima)
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
    heima,
    gestir,
    limit,
    over,
    under,
    bet_amount
  ) |>
  arrange(date, division, booker)


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
    p_home = mean(goal_diff > 0),
    p_away = mean(goal_diff < 0),
    p_tie = mean(goal_diff == 0),
    .by = c(date, division, home, away)
  ) |>
  inner_join(
    outcome_odds |>
      select(
        date = date_game,
        booker,
        home,
        away,
        o_home:o_away
      )
  ) |>
  select(date, division, booker,  home, away, everything()) |>
  pivot_longer(
    -c(date, division, booker, home, away),
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
    .by = c(gestir, division, date, heima, booker)
  ) |>
  filter(
    kelly == max(kelly),
    .by = c(heima, division, date, gestir, outcome)
  ) |>
  group_by(date, division, heima, gestir, outcome) |>
  slice(1) |>
  ungroup() |>
  mutate(
    kelly = get_kelly(p, o),
    .by = c(date, division, heima, gestir)
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
        home,
        away,
        value
      ),
    relationship = "many-to-many"
  ) |>
  select(
    date,
    division,
    booker,
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
      booker,
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
    .by = c(booker, gestir, heima, change)
  ) |>
  filter(
    kelly == max(kelly),
    .by = c(heima, gestir, outcome)
  ) |>
  group_by(heima, gestir, outcome) |>
  slice(1) |>
  ungroup() |>
  mutate(
    kelly = get_kelly(p, o),
    .by = c(booker, gestir, heima)
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
    .by = c(date, division, heima, gestir, booker, o_over, o_under, limit)
  ) |>
  pivot_longer(
    c(o_over:p_under, -limit),
    names_to = c("type", "outcome"),
    names_sep = "_"
  ) |>
  pivot_wider(names_from = type) |>
  mutate(
    kelly = get_kelly(p, o),
    .by = c(booker, gestir, heima, limit)
  ) |>
  filter(
    kelly == max(kelly),
    .by = c(heima, gestir, outcome)
  ) |>
  group_by(date, heima, gestir, outcome) |>
  slice(1) |>
  ungroup() |>
  mutate(
    kelly = get_kelly(p, o),
    .by = c(gestir, heima)
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
