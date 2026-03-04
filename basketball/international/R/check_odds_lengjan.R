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

leagues <- c(
  "Premier League",
  "Championship",
  "League 1",
  "League 2"
)

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

gs4_auth(email = Sys.getenv("GOOGLE_MAIL"))

sheet_url <- "https://docs.google.com/spreadsheets/d/1n5Wcrg-eO3urluOrm5bTwbe4B3yUPGFkVQ0_0h-t_jg/edit?gid=1774709455#gid=1774709455"

# What percent of optimal Kelly am I willing to bet?
kelly_frac <- 0.2
bet_digits <- 0
min_bet_amount <- 200

# What's my current pool size?
cur_pool <- 5952

bets <- read_sheet(
  sheet_url,
  sheet = "Bets_Lengjan"
) |>
  select(
    date = dags_leikur,
    heima,
    gestir,
    tegund = type
  ) |> 
  mutate_at(vars(date), as_date) |>
  filter(date >= today())

outcome_odds <- here("data", "odds.csv") |> 
  read_csv() |> 
  mutate(
    booker = "Lengjan"
  ) |> 
  rename(
    date_game = date,
    o_tie = o_draw
  ) 

post |> 
  filter(division <= 4) |> 
  mutate(
    division = leagues[division]
  ) |> 
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
  arrange(date, division, booker) |>
  filter(bet_amount > min_bet_amount) |>
  select(-bet_amount) |> 
  arrange(division, date) |> 
  anti_join(
    bets
  ) |>
  print(n = Inf)



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
