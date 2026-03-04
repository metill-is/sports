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
  "https://docs.google.com/spreadsheets/d/1ZFa79A3fEXcAkxDz6cxbCMby4Q2WUT0CI9dp6N4UQlk/edit?gid=508088312#gid=508088312",
  sheet = "Bets"
) |>
  mutate(
    dags_leikur = as_date(dags_leikur),
    sport = "Handball"
  ) |>
  mutate(
    season = year(dags_leikur) + 1 * (month(dags_leikur) >= 9),
    season = glue::glue("{season - 1}-{season}")
  )

#### Overall ####

bets |>
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
    .by = c(iter, season)
  ) |>
  summarise(
    cumul_change = tail(cumul_change, 1),
    .by = c(dags_leikur, iter, season)
  ) |>
  summarise(
    lower = quantile(cumul_change, 0.025),
    upper = quantile(cumul_change, 0.975),
    .by = c(dags_leikur, season)
  ) |>
  inner_join(
    bets |>
      filter(
        sport == "Handball"
      ) |>
      # drop_na(win) |>
      filter(
        !is.na(ev),
        # (!is.na(ev) | !is.na(p_push))
      ) |>
      arrange(dags_leikur) |>
      mutate(
        cumul_ev = cumsum(ev * amount),
        change = na_if(change, 0),
        cumul_change = cumsum(change),
        type = if_else(
          is.na(change),
          "Prediction",
          "Observed"
        ),
        .by = season
      ) |>
      summarise(
        cumul_ev = tail(cumul_ev, 1),
        cumul_change = tail(cumul_change, 1),
        type = unique(type),
        .by = c(dags_leikur, season)
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
    data = ~ filter(
      .x,
      dags_leikur >= max(dags_leikur - 4)
    ),
    lty = 3,
    col = "#969696",
    # vjust = 1.5,
    linewidth = 1
  ) +
  geom_textline(
    data = ~ filter(.x, type == "Observed"),
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
  facet_wrap("season", scales = "free") +
  theme(
    plot.margin = margin(5, 15, 5, 15)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Samanburður á væntum og raunverulegum gróða í körfuboltaveðmálum"
  )

ggsave(
  "bets.png",
  width = 8,
  height = 0.4 * 8,
  scale = 1.5
)

bets |>
  # filter(sport == "Handball") |>
  drop_na(win) |>
  filter(
    !is.na(ev),
    # (!is.na(ev) | !is.na(p_push))
  ) |>
  arrange(dags_leikur) |>
  mutate(
    cumul_ev = cumsum(ev * amount),
    cumul_change = cumsum(change),
    cumul_amount = cumsum(amount),
    roi = cumul_change / cumul_amount,
    .by = season
  ) |>
  summarise(
    cumul_ev = tail(cumul_ev, 1),
    cumul_change = tail(cumul_change, 1),
    cumul_amount = tail(cumul_amount, 1),
    n_bets = n(),
    .by = c(dags_leikur, season)
  ) |>
  mutate(
    min_date = min(dags_leikur),
    max_date = max(dags_leikur),
    roi = cumul_change / cumul_amount,
    n_bets = cumsum(n_bets),
    .by = season
  ) |>
  filter(dags_leikur == max(dags_leikur), .by = season) |>
  mutate(
    days = as.numeric(max_date - min_date),
    bets_per_day = n_bets / days
  )


bets |>
  drop_na(win) |>
  filter(
    !is.na(ev),
    # (!is.na(ev) | !is.na(p_push))
  ) |>
  arrange(dags_leikur) |>
  mutate(
    cumul_ev = cumsum(ev * amount),
    change = na_if(change, 0),
    cumul_change = cumsum(change),
    .by = c(country, sex)
  ) |>
  summarise(
    cumul_ev = tail(cumul_ev, 1),
    cumul_change = tail(cumul_change, 1),
    .by = c(country, sex, dags_leikur)
  ) |>
  filter(
    dags_leikur == max(dags_leikur),
    .by = c(country, sex)
  ) |>
  arrange(country, sex) |>
  select(-dags_leikur) |>
  rename(
    expected = cumul_ev,
    observed = cumul_change
  )


p_vals <- bets |>
  # drop_na(win) |>
  arrange(dags_leikur) |>
  crossing(
    iter = 1:2000
  ) |>
  mutate(
    win_samp = rbinom(n(), size = 1, prob = prob),
    change_samp = win_samp * payout - amount
  ) |>
  mutate(
    cumul_change = cumsum(change),
    cumul_change_samp = cumsum(change_samp),
    .by = c(country, sex, iter)
  ) |>
  summarise(
    cumul_change = tail(cumul_change, 1),
    cumul_change_samp = tail(cumul_change_samp, 1),
    .by = c(dags_leikur, country, sex, iter)
  ) |>
  filter(
    dags_leikur == max(dags_leikur),
    .by = c(country, sex, iter)
  ) |>
  summarise(
    p_val = mean(cumul_change_samp <= cumul_change),
    .by = c(country, sex)
  ) |>
  pivot_wider(names_from = sex, values_from = p_val)

p_vals |>
  write_csv(
    here("results", "betting_quantiles.csv")
  )

p_vals

bets |>
  filter(sport == "Handball") |>
  drop_na(win) |>
  filter(
    !is.na(ev),
    # (!is.na(ev) | !is.na(p_push))
  ) |>
  arrange(dags_leikur) |>
  mutate(
    cumul_ev = cumsum(ev * amount),
    cumul_change = cumsum(change),
    cumul_amount = cumsum(amount),
    roi = cumul_change / cumul_amount,
    .by = season
  ) |>
  summarise(
    cumul_ev = tail(cumul_ev, 1),
    cumul_change = tail(cumul_change, 1),
    cumul_amount = tail(cumul_amount, 1),
    n_bets = n(),
    .by = c(dags_leikur, season)
  ) |>
  mutate(
    min_date = min(dags_leikur),
    max_date = max(dags_leikur),
    roi = cumul_change / cumul_amount,
    n_bets = cumsum(n_bets),
    .by = season
  ) |>
  filter(dags_leikur == max(dags_leikur), .by = season) |>
  mutate(
    days = as.numeric(max_date - min_date),
    bets_per_day = n_bets / days
  )


bets |>
  filter(sport == "Handball") |>
  drop_na(win) |>
  filter(
    !is.na(ev),
    # (!is.na(ev) | !is.na(p_push))
  ) |>
  arrange(dags_leikur) |>
  mutate(
    cumul_ev = cumsum(ev * amount),
    cumul_change = cumsum(change),
    cumul_amount = cumsum(amount),
    roi = cumul_change / cumul_amount,
    .by = season
  ) |>
  summarise(
    cumul_ev = tail(cumul_ev, 1),
    cumul_change = tail(cumul_change, 1),
    cumul_amount = tail(cumul_amount, 1),
    .by = c(dags_leikur, season)
  ) |>
  ggplot(aes(dags_leikur, cumul_change / cumul_amount)) +
  geom_texthline(
    data = ~ filter(.x, dags_leikur == max(dags_leikur), .by = season),
    aes(yintercept = cumul_change / cumul_amount),
    label = "Meðaltal",
    lty = 2,
    size = 5
  ) +
  geom_line(
    col = "#969696",
    linewidth = 1
  ) +
  scale_x_date(
    guide = ggh4x::guide_axis_truncated(),
    breaks = breaks_pretty(10),
    labels = label_date_short()
  ) +
  scale_y_continuous(
    guide = ggh4x::guide_axis_truncated(),
    breaks = breaks_width(0.05),
    labels = label_percent(),
    limits = c(0, 1)
  ) +
  facet_wrap("season", scales = "free_x") +
  theme(
    plot.margin = margin(5, 15, 5, 15)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Cumulative return on investment"
  )

bets |>
  filter(sport == "Handball") |>
  drop_na(win) |>
  arrange(dags_leikur) |>
  mutate(
    cumul_ev = cumsum(ev * amount),
    cumul_change = cumsum(change),
    cumul_amount = cumsum(amount),
    n_bets = n(),
    .by = c(country, sex)
  ) |>
  summarise(
    cumul_ev = tail(cumul_ev, 1),
    cumul_change = tail(cumul_change, 1),
    cumul_amount = tail(cumul_amount, 1),
    n_bets = unique(n_bets),
    .by = c(country, sex, dags_leikur)
  ) |>
  filter(
    dags_leikur == max(dags_leikur),
    .by = c(country, sex)
  ) |>
  mutate(
    roi = percent(cumul_change / cumul_amount)
  ) |>
  select(
    country,
    sex,
    n_bets,
    total_bet = cumul_amount,
    total_profit = cumul_change,
    expected_profit = cumul_ev,
    roi
  ) |>
  arrange(desc(total_profit / total_bet))

bets |>
  filter(
    sport == "Handball"
  ) |>
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
    .by = c(country, sex, iter)
  ) |>
  summarise(
    cumul_change = tail(cumul_change, 1),
    .by = c(dags_leikur, country, sex, iter)
  ) |>
  summarise(
    lower = quantile(cumul_change, 0.025),
    upper = quantile(cumul_change, 0.975),
    .by = c(dags_leikur, country, sex)
  ) |>
  inner_join(
    bets |>
      filter(
        sport == "Handball"
      ) |>
      # drop_na(win) |>
      filter(
        !is.na(ev),
        # (!is.na(ev) | !is.na(p_push))
      ) |>
      arrange(dags_leikur) |>
      mutate(
        cumul_ev = cumsum(ev * amount),
        change = na_if(change, 0),
        cumul_change = cumsum(change),
        type = if_else(
          is.na(change),
          "Prediction",
          "Observed"
        ),
        .by = c(country, sex)
      ) |>
      filter(n() > 2, .by = c(country, sex)) |>
      summarise(
        cumul_ev = tail(cumul_ev, 1),
        cumul_change = tail(cumul_change, 1),
        type = unique(type),
        .by = c(country, sex, dags_leikur)
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
    data = ~ filter(
      .x,
      dags_leikur >= max(dags_leikur - 4)
    ),
    lty = 3,
    col = "#969696",
    # vjust = 1.5,
    linewidth = 1
  ) +
  geom_line(
    data = ~ filter(.x, type == "Observed"),
    col = "#969696",
    linewidth = 1
  ) +
  geom_line(
    aes(y = cumul_change),
    col = "#252525",
    linewidth = 1
  ) +
  scale_x_date(
    guide = ggh4x::guide_axis_truncated(),
    breaks = breaks_pretty(10),
    labels = label_date_short()
  ) +
  scale_y_continuous(
    guide = ggh4x::guide_axis_truncated(),
    breaks = breaks_pretty(),
    labels = label_currency(prefix = "", suffix = "€")
  ) +
  facet_wrap(
    vars(country, sex),
    scales = "free"
  ) +
  theme(
    plot.margin = margin(5, 15, 5, 15)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Comparing cumulative expected profit to cumulative observed profit",
    subtitle = "Cumulative expected profit = cumsum(bet_amount * ev)"
  )

#### Bet Type ####

bets |>
  filter(
    sport == "Handball",
    type != "Markafjöldi (Liðs)"
  ) |>
  drop_na(win) |>
  filter(
    !is.na(ev),
    # (!is.na(ev) | !is.na(p_push))
  ) |>
  arrange(dags_leikur) |>
  mutate(
    cumul_ev = cumsum(ev * amount),
    cumul_change = cumsum(change),
    .by = type
  ) |>
  summarise(
    cumul_ev = tail(cumul_ev, 1),
    cumul_change = tail(cumul_change, 1),
    .by = c(dags_leikur, type)
  ) |>
  ggplot(aes(dags_leikur, cumul_ev)) +
  geom_hline(
    yintercept = 0,
    lty = 2,
    alpha = 0.3
  ) +
  geom_textline(
    aes(label = "Expected"),
    col = "#969696",
    size = 4,
    hjust = "auto",
    linewidth = 1,
    text_smoothing = 30
  ) +
  geom_textline(
    aes(y = cumul_change, label = "Observed"),
    col = "#252525",
    size = 4,
    hjust = "auto",
    linewidth = 1,
    text_smoothing = 30
  ) +
  scale_x_date(
    guide = ggh4x::guide_axis_truncated(),
    breaks = breaks_pretty(10),
    labels = label_date_short()
  ) +
  scale_y_continuous(
    guide = ggh4x::guide_axis_truncated(),
    breaks = breaks_width(10),
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


bets |>
  drop_na(win) |>
  filter(
    !is.na(ev),
    # (!is.na(ev) | !is.na(p_push))
  ) |>
  arrange(dags_leikur) |>
  mutate(
    cumul_ev = cumsum(ev * amount),
    cumul_change = cumsum(change),
    cumul_amount = cumsum(amount),
    roi = cumul_change / cumul_amount,
    .by = type
  ) |>
  summarise(
    cumul_ev = tail(cumul_ev, 1),
    cumul_change = tail(cumul_change, 1),
    cumul_amount = tail(cumul_amount, 1),
    .by = c(dags_leikur, type)
  ) |>
  ggplot(aes(dags_leikur, cumul_change / cumul_amount)) +
  geom_hline(
    yintercept = 0,
    lty = 2,
    alpha = 0.3
  ) +
  geom_line(
    col = "#969696",
    linewidth = 1
  ) +
  scale_x_date(
    guide = ggh4x::guide_axis_truncated(),
    breaks = breaks_pretty(10),
    labels = label_date_short()
  ) +
  scale_y_continuous(
    guide = ggh4x::guide_axis_truncated(),
    # breaks = breaks_width(10),
    labels = label_percent()
  ) +
  facet_wrap("type") +
  coord_cartesian(
    ylim = c(-0.1, NA)
  ) +
  theme(
    plot.margin = margin(5, 15, 5, 15)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Cumulative return on investment"
  )


bets |>
  drop_na(win) |>
  filter(
    !is.na(ev),
    (!is.na(ev) | !is.na(p_push))
  ) |>
  arrange(dags_leikur) |>
  mutate(
    cumul_ev = cumsum(ev * amount),
    cumul_change = cumsum(change),
    cumul_amount = cumsum(amount),
    roi = cumul_change / cumul_amount,
    .by = type
  ) |>
  summarise(
    cumul_ev = tail(cumul_ev, 1),
    cumul_change = tail(cumul_change, 1),
    cumul_amount = tail(cumul_amount, 1),
    .by = c(dags_leikur, type)
  ) |>
  ggplot(aes(dags_leikur, cumul_change / 130)) +
  geom_hline(
    yintercept = 0,
    lty = 2,
    alpha = 0.2
  ) +
  geom_line(
    col = "#969696",
    linewidth = 1
  ) +
  scale_x_date(
    guide = ggh4x::guide_axis_truncated(),
    breaks = breaks_pretty(10),
    labels = label_date_short()
  ) +
  scale_y_continuous(
    guide = ggh4x::guide_axis_truncated(),
    breaks = breaks_width(0.05),
    labels = label_percent(),
    # limits = c(0, NA)
  ) +
  facet_wrap("type") +
  theme(
    plot.margin = margin(5, 15, 5, 15)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "% profit from original 130€"
  )


#### Sport ####

bets |>
  drop_na(win) |>
  filter(
    !is.na(ev),
    (!is.na(ev) | !is.na(p_push))
  ) |>
  arrange(dags_leikur) |>
  mutate(
    cumul_ev = cumsum(ev * amount),
    cumul_change = cumsum(change),
    .by = sport
  ) |>
  summarise(
    cumul_ev = tail(cumul_ev, 1),
    cumul_change = tail(cumul_change, 1),
    .by = c(dags_leikur, sport)
  ) |>
  ggplot(aes(dags_leikur, cumul_ev)) +
  geom_hline(
    yintercept = 0,
    lty = 2,
    alpha = 0.2
  ) +
  geom_textline(
    aes(label = "Expected Profit"),
    col = "#969696",
    size = 5,
    hjust = 0.3,
    linewidth = 1
  ) +
  geom_textline(
    aes(y = cumul_change, label = "Observed Profit"),
    col = "#252525",
    size = 5,
    hjust = 0.3,
    linewidth = 1
  ) +
  scale_x_date(
    guide = ggh4x::guide_axis_truncated(),
    breaks = breaks_pretty(10),
    labels = label_date_short()
  ) +
  scale_y_continuous(
    guide = ggh4x::guide_axis_truncated(),
    breaks = breaks_width(10),
    labels = label_currency(prefix = "", suffix = "€")
  ) +
  facet_wrap("sport") +
  theme(
    plot.margin = margin(5, 15, 5, 15)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Comparing cumulative expected profit to cumulative observed profit",
    subtitle = "Cumulative expected profit = cumsum(bet_amount * ev)"
  )


bets |>
  drop_na(win) |>
  filter(
    !is.na(ev),
    (!is.na(ev) | !is.na(p_push))
  ) |>
  arrange(dags_leikur) |>
  mutate(
    cumul_ev = cumsum(ev * amount),
    cumul_change = cumsum(change),
    cumul_amount = cumsum(amount),
    roi = cumul_change / cumul_amount,
    .by = sport
  ) |>
  summarise(
    cumul_ev = tail(cumul_ev, 1),
    cumul_change = tail(cumul_change, 1),
    cumul_amount = tail(cumul_amount, 1),
    .by = c(dags_leikur, sport)
  ) |>
  ggplot(aes(dags_leikur, cumul_change / cumul_amount)) +
  geom_hline(
    yintercept = 0,
    lty = 2,
    alpha = 0.3
  ) +
  geom_line(
    col = "#969696",
    linewidth = 1
  ) +
  scale_x_date(
    guide = ggh4x::guide_axis_truncated(),
    breaks = breaks_pretty(10),
    labels = label_date_short()
  ) +
  scale_y_continuous(
    guide = ggh4x::guide_axis_truncated(),
    # breaks = breaks_width(10),
    labels = label_percent()
  ) +
  facet_wrap("sport") +
  theme(
    plot.margin = margin(5, 15, 5, 15)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Cumulative return on investment"
  )


bets |>
  drop_na(win) |>
  filter(
    !is.na(ev),
    (!is.na(ev) | !is.na(p_push))
  ) |>
  arrange(dags_leikur) |>
  mutate(
    cumul_ev = cumsum(ev * amount),
    cumul_change = cumsum(change),
    cumul_amount = cumsum(amount),
    roi = cumul_change / cumul_amount,
    .by = sport
  ) |>
  summarise(
    cumul_ev = tail(cumul_ev, 1),
    cumul_change = tail(cumul_change, 1),
    cumul_amount = tail(cumul_amount, 1),
    .by = c(dags_leikur, sport)
  ) |>
  ggplot(aes(dags_leikur, cumul_change / 130)) +
  geom_hline(
    yintercept = 0,
    lty = 2,
    alpha = 0.2
  ) +
  geom_line(
    col = "#969696",
    linewidth = 1
  ) +
  scale_x_date(
    guide = ggh4x::guide_axis_truncated(),
    breaks = breaks_pretty(10),
    labels = label_date_short()
  ) +
  scale_y_continuous(
    guide = ggh4x::guide_axis_truncated(),
    breaks = breaks_width(0.05),
    labels = label_percent(),
    # limits = c(0, NA)
  ) +
  facet_wrap("sport") +
  theme(
    plot.margin = margin(5, 15, 5, 15)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "% profit from original 130€"
  )

#### Football ####

bets |>
  filter(sport == "Football") |>
  drop_na(win) |>
  filter(
    !is.na(ev),
    (!is.na(ev) | !is.na(p_push))
  ) |>
  arrange(dags_leikur) |>
  mutate(
    cumul_ev = cumsum(ev * amount),
    cumul_change = cumsum(change),
    .by = deild
  ) |>
  summarise(
    cumul_ev = tail(cumul_ev, 1),
    cumul_change = tail(cumul_change, 1),
    .by = c(dags_leikur, deild)
  ) |>
  ggplot(aes(dags_leikur, cumul_ev)) +
  geom_hline(
    yintercept = 0,
    lty = 2,
    alpha = 0.3
  ) +
  geom_textline(
    aes(label = "Expected"),
    col = "#969696",
    size = 4,
    hjust = "auto",
    linewidth = 1,
    text_smoothing = 30
  ) +
  geom_textline(
    aes(y = cumul_change, label = "Observed"),
    col = "#252525",
    size = 4,
    hjust = "auto",
    linewidth = 1,
    text_smoothing = 30
  ) +
  scale_x_date(
    guide = ggh4x::guide_axis_truncated(),
    breaks = breaks_pretty(10),
    labels = label_date_short()
  ) +
  scale_y_continuous(
    guide = ggh4x::guide_axis_truncated(),
    breaks = breaks_width(10),
    labels = label_currency(prefix = "", suffix = "€")
  ) +
  facet_wrap(
    "deild",
    scales = "free_y"
  ) +
  theme(
    plot.margin = margin(5, 15, 5, 15)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Comparing cumulative expected profit to cumulative observed profit",
    subtitle = "Cumulative expected profit = cumsum(bet_amount * ev)"
  )

bets |>
  filter(sport == "Football") |>
  drop_na(win) |>
  filter(
    !is.na(ev),
    (!is.na(ev) | !is.na(p_push))
  ) |>
  arrange(dags_leikur) |>
  mutate(
    cumul_ev = cumsum(ev * amount),
    cumul_change = cumsum(change),
    .by = type
  ) |>
  summarise(
    cumul_ev = tail(cumul_ev, 1),
    cumul_change = tail(cumul_change, 1),
    .by = c(dags_leikur, type)
  ) |>
  ggplot(aes(dags_leikur, cumul_ev)) +
  geom_hline(
    yintercept = 0,
    lty = 2,
    alpha = 0.3
  ) +
  geom_textline(
    aes(label = "Expected"),
    col = "#969696",
    size = 4,
    hjust = "auto",
    linewidth = 1,
    text_smoothing = 30
  ) +
  geom_textline(
    aes(y = cumul_change, label = "Observed"),
    col = "#252525",
    size = 4,
    hjust = "auto",
    linewidth = 1,
    text_smoothing = 30
  ) +
  scale_x_date(
    guide = ggh4x::guide_axis_truncated(),
    breaks = breaks_pretty(10),
    labels = label_date_short()
  ) +
  scale_y_continuous(
    guide = ggh4x::guide_axis_truncated(),
    breaks = breaks_width(10),
    labels = label_currency(prefix = "", suffix = "€")
  ) +
  facet_wrap(
    "type",
    scales = "free_y"
  ) +
  theme(
    plot.margin = margin(5, 15, 5, 15)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Comparing cumulative expected profit to cumulative observed profit",
    subtitle = "Cumulative expected profit = cumsum(bet_amount * ev)"
  )

#### Calibration ####

bets |>
  filter(sport == "Handball") |>
  mutate(
    ev = amount * ev
  ) |>
  select(
    ev,
    change
  ) |>
  drop_na() |>
  mutate(
    group = ntile(ev, 10)
  ) |>
  summarise(
    sd = sd(change),
    ev = mean(ev),
    change = mean(change),
    .by = group
  ) |>
  arrange(ev) |>
  ggplot(aes(ev, change)) +
  geom_hline(
    yintercept = 0,
    lty = 2,
    alpha = 0.3
  ) +
  geom_abline(
    intercept = 0,
    slope = 1
  ) +
  geom_pointrange(
    aes(ymin = change - 2 * sd, ymax = change + 2 * sd)
  )
