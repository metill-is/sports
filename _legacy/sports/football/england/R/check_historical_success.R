library(tidyverse)
library(googlesheets4)
library(metill)
library(purrr)
library(here)
library(gt)
library(gtExtras)
library(geomtextpath)
theme_set(theme_metill())
gs4_auth(email = Sys.getenv("GOOGLE_MAIL"))

bets <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1n5Wcrg-eO3urluOrm5bTwbe4B3yUPGFkVQ0_0h-t_jg/edit?gid=508088312#gid=508088312", 
  sheet = "Bets"
) |> 
  filter(
    dags_bet >= clock::date_build(2025, 4, 16)
  ) |> 
  mutate(
    dags_leikur = as_date(dags_leikur)
  ) |> 
  drop_na(prob)


#### Overall ####

bets |> 
  drop_na(prob) |> 
  # drop_na(win) |>
  arrange(dags_leikur) |> 
  crossing(
    iter = 1:4000
  ) |> 
  mutate(
    win = rbinom(n(), size = 1, prob = prob),
    change = win * payout - amount
  ) |> 
  mutate(
    cumul_change = cumsum(change),
    .by = iter
  ) |> 
  summarise(
    cumul_change = tail(cumul_change, 1),
    .by = c(dags_leikur, iter)
  ) |> 
  summarise(
    lower = quantile(cumul_change, 0.025),
    upper = quantile(cumul_change, 0.975),
    .by = c(dags_leikur)
  ) |> 
  inner_join(
    bets |> 
      drop_na(prob, ev, win) |> 
      arrange(dags_leikur) |> 
      mutate(
        cumul_ev = cumsum(ev * amount),
        cumul_change = cumsum(change),
        change = na_if(change, 0),
        type = if_else(
          is.na(change),
          "Prediction",
          "Observed"
        )
      ) |> 
      summarise(
        cumul_ev = tail(na.omit(cumul_ev), 1),
        cumul_change = tail(na.omit(cumul_change), 1),
        type = unique(type),
        .by = c(dags_leikur)
      )
  ) |> 
  ggplot(aes(dags_leikur, cumul_ev)) +
  geom_hline(
    yintercept = 0,
    lty = 2
  ) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.1
  ) +
  geom_line(
    data = ~filter(
      .x, 
      dags_leikur >= max(dags_leikur - 4)
    ),
    lty = 3,
    col = "#969696",
    # vjust = 1.5,
    linewidth = 1
  ) +
  geom_textline(
    data = ~filter(.x, type == "Observed"),
    aes(label = "Expected"),
    col = "#969696",
    size = 5,
    hjust = 0.85,
    # vjust = 1.5,
    linewidth = 1,
    text_smoothing = 40
  ) +
  geom_textline(
    aes(y = cumul_change, label = "Observed"),
    col = "#252525",
    size = 5,
    hjust = 0.1,
    # vjust = -0.5,
    linewidth = 1,
    text_smoothing = 30
  ) +
  scale_x_date(
    guide = guide_axis(cap = "both"),
    breaks = breaks_pretty(10),
    labels = label_date_short()
  ) +
  scale_y_continuous(
    guide = guide_axis(cap = "both"),
    breaks = breaks_pretty(10),
    labels = label_currency(prefix = "", suffix = "€")
  ) +
  theme(
    plot.margin = margin(5, 15, 5, 15)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Samanburður á væntum og raunverulegum gróða í fótboltaveðmálum"
  )

ggsave(
  "football_bets.png",
  width = 8,
  height = 0.5 * 8,
  scale = 1.5
)

bets |> 
  drop_na(prob) |> 
  drop_na(win) |>
  arrange(dags_leikur) |> 
  crossing(
    iter = 1:4000
  ) |> 
  mutate(
    win = rbinom(n(), size = 1, prob = prob),
    change = win * payout - amount
  ) |> 
  summarise(
    change = sum(change),
    .by = c(dags_leikur, iter)
  ) |> 
  summarise(
    lower = quantile(change, 0.025),
    upper = quantile(change, 0.975),
    .by = c(dags_leikur)
  ) |> 
  inner_join(
    bets |> 
      drop_na(prob) |> 
      # drop_na(win) |>
      filter(
        !is.na(ev),
        # (!is.na(ev) | !is.na(p_push))
      ) |> 
      arrange(dags_leikur) |> 
      mutate(
        ev = ev * amount,
        change = na_if(change, 0),
        type = if_else(
          is.na(change),
          "Prediction",
          "Observed"
        )
      ) |> 
      summarise(
        ev = sum(ev, na.rm = TRUE),
        change = sum(change, na.rm = TRUE),
        .by = c(dags_leikur)
      )
  ) |> 
  ggplot(aes(dags_leikur, ev)) +
  geom_hline(
    yintercept = 0,
    lty = 2
  ) +
  geom_linerange(
    aes(ymin = lower, ymax = upper),
    alpha = 0.3
  ) +
  # geom_point(
  #   data = ~filter(.x, type == "Observed"),
  #   aes(col = "Expected"),
  #   size = 5,
  # ) +
  geom_point(
    aes(y = change, col = "Observed"),
    size = 5,
  ) +
  scale_x_date(
    guide = guide_axis(cap = "both"),
    breaks = breaks_pretty(20),
    labels = label_date_short()
  ) +
  scale_y_continuous(
    guide = guide_axis(cap = "both"),
    breaks = breaks_pretty(10),
    labels = label_currency(prefix = "", suffix = "€")
  ) +
  theme(
    plot.margin = margin(5, 15, 5, 15),
    legend.position = "none"
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Comparing observed profit to 95% prediction intervals of expected profit for each day"
  )

bets |> 
  drop_na(prob) |> 
  drop_na(win) |>
  arrange(dags_leikur) |> 
  crossing(
    iter = 1:4000
  ) |> 
  mutate(
    win = rbinom(n(), size = 1, prob = prob),
    change = win * payout - amount
  ) |> 
  summarise(
    sim = sum(change),
    .by = c(dags_leikur, iter)
  ) |> 
  inner_join(
    bets |> 
      drop_na(prob) |> 
      # drop_na(win) |>
      filter(
        !is.na(ev),
        # (!is.na(ev) | !is.na(p_push))
      ) |> 
      arrange(dags_leikur) |> 
      mutate(
        ev = ev * amount,
        change = na_if(change, 0),
        type = if_else(
          is.na(change),
          "Prediction",
          "Observed"
        )
      ) |> 
      summarise(
        ev = sum(ev, na.rm = TRUE),
        change = sum(change, na.rm = TRUE),
        .by = c(dags_leikur)
      )
  ) |> 
  summarise(
    p = mean(sim <= change),
    .by = c(dags_leikur)
  ) |> 
  arrange(p) |> 
  mutate(
    group = ntile(p, 10)
  ) |> 
  summarise(
    p = mean(p),
    .by = group
  ) |> 
  arrange(p) |> 
  mutate(
    q = row_number() / (n() + 1)
  ) |> 
  ggplot(aes(q, p)) +
  geom_abline(
    intercept = 0,
    slope = 1
  ) +
  geom_point()

bets |> 
  drop_na(prob) |> 
  # drop_na(win) |>
  arrange(dags_leikur) |> 
  crossing(
    iter = 1:4000
  ) |> 
  mutate(
    win = rbinom(n(), size = 1, prob = prob),
    change = win * payout - amount
  ) |> 
  summarise(
    sim_change = sum(change),
    .by = c(dags_leikur, iter)
  ) |> 
  inner_join(
    bets |> 
      drop_na(prob, ev, win) |> 
      arrange(dags_leikur) |> 
      mutate(
        cumul_ev = cumsum(ev * amount),
        cumul_change = cumsum(change),
        change = na_if(change, 0),
        type = if_else(
          is.na(change),
          "Prediction",
          "Observed"
        )
      ) |> 
      summarise(
        obs_change = sum(change),
        .by = c(dags_leikur)
      )
  ) |> 
  arrange(iter, dags_leikur) |> 
  mutate(
    sim_change = slider::slide_index_dbl(
      sim_change, dags_leikur, sum, .before = 30
    ),
    obs_change = slider::slide_index_dbl(
      obs_change, dags_leikur, sum, .before = 30
    ),
    .by = iter
  ) |> 
  summarise(
    q = mean(sim_change <= obs_change),
    .by = c(dags_leikur)
  ) |> 
  ggplot(aes(dags_leikur, q)) +
  geom_line() +
  scale_x_date(
    guide = guide_axis(cap = "both"),
    breaks = breaks_pretty(20),
    labels = label_date_short()
  ) +
  scale_y_continuous(
    guide = guide_axis(cap = "both"),
    breaks = breaks_pretty(10),
    labels = label_percent(),
    limits = c(0, 1)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "30 day rolling average of P(Obs Profit <= profit)"
  )

#### ROI ####

bets |> 
  drop_na(prob, ev, win) |> 
  arrange(dags_leikur) |> 
  mutate(
    cumul_bet = cumsum(amount),
    cumul_change = cumsum(change),
    change = na_if(change, 0),
    type = if_else(
      is.na(change),
      "Prediction",
      "Observed"
    )
  ) |> 
  summarise(
    cumul_bet = tail(na.omit(cumul_bet), 1),
    cumul_change = tail(na.omit(cumul_change), 1),
    roi = cumul_change / cumul_bet,
    .by = c(dags_leikur)
  ) |> 
  arrange(desc(dags_leikur))



#### Type ####

bets |> 
  drop_na(prob, win) |> 
  arrange(dags_leikur) |> 
  crossing(
    iter = 1:1000
  ) |> 
  mutate(
    win = rbinom(n(), size = 1, prob = prob),
    change = win * payout - amount
  ) |> 
  mutate(
    cumul_change = cumsum(change),
    .by = c(iter, type)
  ) |> 
  summarise(
    cumul_change = tail(cumul_change, 1),
    .by = c(dags_leikur, iter, type)
  ) |> 
  summarise(
    lower = quantile(cumul_change, 0.025),
    upper = quantile(cumul_change, 0.975),
    .by = c(dags_leikur, type)
  ) |> 
  inner_join(
    bets |> 
      drop_na(prob, ev, win) |> 
      arrange(dags_leikur) |> 
      mutate(
        cumul_ev = cumsum(ev * amount),
        cumul_change = cumsum(change),
        change = na_if(change, 0),
        .by = type
      ) |> 
      summarise(
        cumul_ev = tail(na.omit(cumul_ev), 1),
        cumul_change = tail(na.omit(cumul_change), 1),
        .by = c(dags_leikur, type)
      )
  ) |> 
  ggplot(aes(dags_leikur, cumul_ev)) +
  geom_hline(
    yintercept = 0,
    lty = 2
  ) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.1
  ) +
  geom_textline(
    aes(label = "Expected"),
    col = "#969696",
    size = 5,
    hjust = 0.85,
    # vjust = 1.5,
    linewidth = 1,
    text_smoothing = 40
  ) +
  geom_textline(
    aes(y = cumul_change, label = "Observed"),
    col = "#252525",
    size = 5,
    hjust = 0.1,
    # vjust = -0.5,
    linewidth = 1,
    text_smoothing = 30
  ) +
  scale_x_date(
    guide = guide_axis(cap = "both"),
    breaks = breaks_pretty(10),
    labels = label_date_short()
  ) +
  scale_y_continuous(
    guide = guide_axis(cap = "both"),
    breaks = breaks_pretty(10),
    labels = label_currency(prefix = "", suffix = "€")
  ) +
  facet_wrap("type") +
  theme(
    plot.margin = margin(5, 15, 5, 15)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Comparing cumulative expected profit to cumulative observed profit",
    subtitle = "Cumulative expected profit = cumsum(bet_amount * ev)"
  )

#### P and EV ####



bets |> 
  drop_na(prob, win) |> 
  select(prob, win) |> 
  mutate(
    group = ntile(prob, 10)
  ) |> 
  summarise(
    prob = mean(prob),
    win = mean(win),
    .by = group
  ) |> 
  ggplot(aes(prob, win)) +
  geom_abline(
    intercept = 0, 
    slope = 1,
    lty = 2
  ) +
  geom_point() +
  scale_x_continuous(
    limits = c(0, 1),
    guide = guide_axis(cap = "both"),
    labels = label_percent(),
    breaks = breaks_width(0.2)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    guide = guide_axis(cap = "both"),
    labels = label_percent(),
    breaks = breaks_width(0.2)
  ) +
  labs(
    title = "Kvörðun fótboltalíkansins á veðmálum",
    x = "Spáðar líkur á að vinna veðmál",
    y = "Hlutfall veðmála sem unnust"
  )


bets |> 
  drop_na(prob, win) |> 
  select(ev, change, win, amount) |> 
  mutate(
    group = ntile(ev, 10),
    obs = change / amount
  )  |> 
  summarise(
    ev = mean(ev),
    obs = mean(obs),
    .by = group
  ) |> 
  ggplot(aes(ev, obs)) +
  geom_hline(
    yintercept = 0,
    alpha = 0.2
  ) +
  geom_vline(
    xintercept = 0,
    alpha = 0.2
  ) +
  geom_abline(
    intercept = 0, 
    slope = 1,
    lty = 2
  ) +
  geom_point() +
  scale_x_continuous(
    limits = c(-1, 1),
    guide = guide_axis(cap = "both"),
    labels = label_percent()
  ) +
  scale_y_continuous(
    limits = c(-1, 1),
    guide = guide_axis(cap = "both"),
    labels = label_percent()
  )
