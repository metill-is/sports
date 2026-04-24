library(tidyverse)
library(googlesheets4)
library(metill)
library(purrr)
library(nloptr)
library(here)
theme_set(theme_metill())
gs4_auth(email = Sys.getenv("GOOGLE_MAIL"))

odds_sheet <- "https://docs.google.com/spreadsheets/d/1ZFa79A3fEXcAkxDz6cxbCMby4Q2WUT0CI9dp6N4UQlk/edit?gid=1774709455#gid=1774709455"

# What percent of optimal Kelly am I willing to bet?
kelly_frac <- 0.1
bet_digits <- 1

# What's my current pool size?
cur_pool <- 150

odds <- c("Niðurstaða") |>
  map(
    \(x) read_sheet(odds_sheet, sheet = x)
  ) |>
  list_rbind() |>
  fill(date_obs) |>
  mutate_at(
    vars(contains("date")),
    as_date
  )


change <- c("Forgjöf") |>
  map(
    \(x) read_sheet(odds_sheet, sheet = x)
  ) |>
  list_rbind() |>
  fill(date_obs, booker) |>
  mutate_at(
    vars(contains("date")),
    as_date
  ) |>
  rename(
    heima = home,
    gestir = away
  )

points <- c("Mörk") |>
  map(
    \(x) read_sheet(odds_sheet, sheet = x)
  ) |>
  list_rbind() |>
  fill(date_obs, booker) |>
  mutate_at(
    vars(contains("date")),
    as_date
  ) |>
  rename(
    heima = home,
    gestir = away
  )

game <- read_csv(here("Basketball", "results", "next_games_post_kvk.csv")) |>
  filter(heima == "Aþena") |>
  select(
    iteration = .iteration,
    home = heima,
    away = gestir,
    home_goals = heima_stig,
    away_goals = gestir_stig
  )


outcome <- odds |>
  filter(home == "Aþena", away == "Stjarnan") |>
  slice(1) |>
  select(
    home,
    away,
    o_home,
    o_away
  )

total_goals <- points |>
  rename(
    home = heima
  ) |>
  filter(home == "Aþena", gestir == "Stjarnan") |>
  slice(1) |>
  select(
    home,
    away = gestir,
    limit,
    o_over,
    o_under
  )

handicap <- change |>
  filter(heima == "Aþena", gestir == "Stjarnan") |>
  slice(1) |>
  select(
    home = heima,
    away = gestir,
    change,
    o_home,
    o_away
  )

results <- game |>
  inner_join(
    total_goals |>
      select(home, away, limit)
  ) |>
  inner_join(
    handicap |>
      select(home, away, change)
  ) |>
  mutate(
    total_goals = home_goals + away_goals,
    outcome_home = 1 * (home_goals > away_goals),
    outcome_away = 1 * (away_goals > home_goals),
    totalgoals_over = 1 * (total_goals > limit),
    totalgoals_under = 1 * (total_goals < limit),
    handicap_home = 1 * (home_goals + change > away_goals),
    handicap_away = 1 * (home_goals + change < away_goals)
  )

result_matrix <- results |>
  select(outcome_home:handicap_away) |>
  as.matrix()

o <- c(
  outcome$o_home,
  outcome$o_away,
  total_goals$o_over,
  total_goals$o_under,
  handicap$o_home,
  handicap$o_away
)

names(o) <- c("home", "away", "over", "under", "h_home", "h_away")

b <- o - 1

m <- ncol(result_matrix)

library(nloptr)


opt_fun <- function(f, gamma = 2) {
  o <- c(o, 1)
  X <- cbind(result_matrix, 1)

  R <- X %*% (o * f)

  if (min(R) < 1e-8) {
    return(-1e5)
  }

  if (gamma == 1) {
    mean(log(R))
  } else {
    mean((R^(1 - gamma) - 1) / (1 - gamma))
  }

  R
}

f_init <- rep(1 / length(b), length(b) + 1)

constraint <- function(f) {
  sum(f) - 1
}

lb <- rep(0, length(b) + 1)
ub <- rep(1, length(b) + 1)

# Run optimization
result <- nloptr(
  x0 = f_init,
  eval_f = function(f) -opt_fun(f, gamma = 2), # Negative to maximize
  eval_g_eq = function(f) constraint(f), # Inequality constraint
  lb = lb,
  ub = ub, # Bound constraints
  opts = list(
    "algorithm" = "NLOPT_LN_COBYLA",
    # "xtol_rel" = 1e-8,
    maxeval = 1e4
  )
)


result


f <- result$solution


f |> round(2)

R <- cbind(result_matrix, 1) %*% (c(b, 0) * f)

sd(R)

mean(R < 1)

R |> table() |> prop.table()

tibble(
  bet = names(o),
  o = o,
  f = round(f[1:m], 2),
  p = colMeans(result_matrix),
  p_o = 1 / o,
) |>
  mutate(
    ev = p * o - (1 - p)
  )


# Multi-constraint utility function with nloptr
multi_constraint_utility <- function() {
  # Main objective function
  obj_fun <- function(f) {
    o <- c(o, 1)
    X <- cbind(result_matrix, 1)

    R <- X %*% (o * f)
    mean(R) # Maximize excess return
  }

  eq_constraints <- list(
    # Constraint 1: sum(f) <= 1
    function(f) sum(f) - 1
  )

  # Constraint functions
  ineq_constraints <- list(
    # Constraint 1: Probability of loss <= 0.05
    function(f) {
      o <- c(o, 1)
      X <- cbind(result_matrix, 1)

      R <- X %*% (o * f)
      mean(R < 1) - 0.3
    },

    # Constraint 2: 5% VaR >= 0.9 (lose at most 10% in worst 5% of cases)
    function(f) {
      o <- c(o, 1)
      X <- cbind(result_matrix, 1)

      R <- X %*% (o * f)
      0.5 - quantile(R, 0.05)
    }
  )

  # Run optimization with multiple constraints
  result <- nloptr(
    x0 = f_init,
    eval_f = function(f) -obj_fun(f), # Negative for maximization
    eval_g_eq = function(f) {
      sapply(eq_constraints, function(con) con(f))
    },
    eval_g_ineq = function(f) {
      # Evaluate all constraints
      sapply(ineq_constraints, function(con) con(f))
    },
    lb = lb,
    ub = ub,
    opts = list(
      "algorithm" = "NLOPT_LN_COBYLA",
      "xtol_rel" = 1e-8,
      maxeval = 1e4
    )
  )

  return(result)
}

# Usage
result <- multi_constraint_utility()

result


f <- result$solution


f |> round(2)

R <- cbind(result_matrix, 1) %*% (c(o, 1) * f)

mean(R)
sd(R)

mean(R < 1)


R |> table() |> prop.table()

tibble(
  bet = names(o),
  o = o,
  f = round(f[1:m], 2),
  p = colMeans(result_matrix),
  p_o = 1 / o,
) |>
  mutate(
    ev = p * o - (1 - p)
  )
