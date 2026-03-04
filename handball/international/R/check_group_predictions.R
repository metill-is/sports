sex <- "male"
end_date <- clock::date_build(2026, 1, 14)

#### Data Prep ####
results <- read_rds(here("results", sex, end_date, "fit.rds"))

d <- read_csv(here("results", sex, end_date, "d.csv"))
teams <- read_csv(here("results", sex, end_date, "teams.csv"))
next_games <- read_csv(here("results", sex, end_date, "next_games.csv"))
top_teams <- read_csv(here("results", sex, end_date, "top_teams.csv"))
pred_d <- read_csv(here("results", sex, end_date, "pred_d.csv"))

groups <- tibble(
  data = list(
    group_a = list(
      "Germany",
      "Spain",
      "Serbia",
      "Austria"
    ),
    group_b = list(
      "Denmark",
      "North Macedonia",
      "Portugal",
      "Romania"
    ),
    group_c = list(
      "Norway",
      "France",
      "Czech Republic",
      "Ukraine"
    ),
    group_d = list(
      "Montenegro",
      "Slovenia",
      "Switzerland",
      "Faroe Islands"
    ),
    group_e = list(
      "Croatia",
      "Georgia",
      "Netherlands",
      "Sweden"
    ),
    group_f = list(
      "Hungary",
      "Iceland",
      "Italy",
      "Poland"
    )
  )
) |>
  unnest_wider(data, names_sep = "_") |>
  pivot_longer(everything()) |>
  mutate(
    name = parse_number(name),
    group = LETTERS[row_number()],
    .by = name
  ) |>
  select(group, team = value)

posterior_goals <- results$draws(c("goals1_pred", "goals2_pred")) |>
  as_draws_df() |>
  as_tibble() |>
  pivot_longer(
    -c(.draw, .chain, .iteration),
    names_to = "parameter",
    values_to = "value"
  ) |>
  mutate(
    type = if_else(
      str_detect(parameter, "goals1"),
      "home_goals",
      "away_goals"
    ),
    game_nr = str_match(parameter, "d\\[(.*)\\]$")[, 2] |> as.numeric()
  ) |>
  select(.draw, type, game_nr, value) |>
  pivot_wider(names_from = type, values_from = value) |>
  inner_join(
    pred_d,
    by = "game_nr"
  ) |>
  filter(
    date >= end_date,
    date <= clock::date_build(2026, 12, 2)
  ) |>
  select(
    iteration = .draw,
    game_nr,
    division,
    date,
    home,
    away,
    home_goals,
    away_goals
  )

base_points <- d |>
  filter(
    date >= clock::date_build(2025, 11, 26),
    date <= clock::date_build(2026, 12, 2),
    division == 1
  ) |>
  mutate(
    result = case_when(
      home_goals > away_goals + 0.5 ~ "home",
      home_goals < away_goals - 0.5 ~ "away",
      TRUE ~ "draw"
    )
  ) |>
  pivot_longer(c(home, away), values_to = "team") |>
  mutate(
    points = case_when(
      result == name ~ 2,
      result == "draw" ~ 1,
      TRUE ~ 0
    ),
    scored = if_else(
      name == "home",
      home_goals,
      away_goals
    ),
    conceded = if_else(
      name == "away",
      home_goals,
      away_goals
    )
  ) |>
  summarise(
    base_points = sum(points),
    base_scored = sum(scored),
    base_conceded = sum(conceded),
    base_wins = sum(points == 2),
    base_losses = sum(points == 0),
    .by = c(team)
  ) |>
  arrange(desc(base_points))

p_top <- posterior_goals |>
  mutate(
    result = case_when(
      home_goals > away_goals + 0.5 ~ "home",
      home_goals < away_goals - 0.5 ~ "away",
      TRUE ~ "draw"
    )
  ) |>
  pivot_longer(c(home, away), values_to = "team") |>
  mutate(
    points = case_when(
      result == name ~ 2,
      result == "draw" ~ 1,
      TRUE ~ 0
    ),
    scored = if_else(
      name == "home",
      home_goals,
      away_goals
    ),
    conceded = if_else(
      name == "away",
      home_goals,
      away_goals
    )
  ) |>
  summarise(
    wins = sum(points == 2),
    losses = sum(points == 0),
    points = sum(points),
    scored = sum(scored),
    conceded = sum(conceded),
    .by = c(iteration, team)
  ) |>
  left_join(
    base_points
  ) |>
  mutate_at(
    vars(starts_with("base")),
    coalesce,
    0
  ) |>
  mutate(
    base_points = coalesce(base_points, 0),
    points = points + base_points,
    wins = wins + base_wins,
    losses = losses + base_losses,
    scored = scored + base_scored,
    conceded = conceded + base_conceded
  ) |>
  arrange(desc(points)) |>
  inner_join(
    groups
  ) |>
  mutate(
    position = row_number(),
    .by = c(iteration, group)
  ) |>
  summarise(
    p_top = mean(position <= 2),
    mean_pos = mean(position),
    lower_pos = quantile(position, 0.25),
    upper_pos = quantile(position, 0.75),
    pos_interval = str_c(
      quantile(position, 0.25),
      "-",
      quantile(position, 0.75)
    ),
    pos_interval = if_else(
      lower_pos == upper_pos,
      as.character(lower_pos),
      pos_interval
    ),
    mean_points = mean(points),
    wins = mean(wins),
    losses = mean(losses),
    # scored = quantile(scored, c(0.25, 0.75)) |> round() |>  str_c(collapse = "-"),
    # conceded = quantile(conceded, c(0.25, 0.75)) |> round() |>  str_c(collapse = "-"),
    scored = mean(scored),
    conceded = mean(conceded),
    .by = c(team, group)
  ) |>
  arrange(group, desc(mean_points)) |>
  select(-lower_pos, -upper_pos)

p_top |>
  mutate(
    group = str_c(group, "-riðill")
  ) |>
  group_by(group) |>
  gt() |>
  fmt_number(
    c(mean_pos:losses),
    decimals = 1
  ) |>
  fmt_number(
    c(scored:conceded),
    decimals = 0
  ) |>
  fmt_percent(
    p_top,
    decimals = 0
  ) |>
  cols_hide(mean_pos) |>
  cols_label(
    team = "",
    p_top = "Áfram",
    mean_pos = "Sæti",
    pos_interval = "Sæti",
    mean_points = "Stig",
    wins = "Sigrar",
    losses = "Töp",
    scored = "Skoruð:Fengin",
    conceded = "Fengin"
  ) |>
  cols_merge(
    c(scored, conceded),
    pattern = "{1}:{2}"
  ) |>
  cols_move_to_end(mean_points) |>
  tab_spanner(
    columns = c(scored, conceded),
    label = "Mörk"
  ) |>
  tab_style(
    locations = cells_body(columns = mean_points),
    style = cell_text(
      weight = 800
    )
  ) |>
  tab_style(
    locations = cells_body(columns = c(wins)),
    style = cell_text(
      color = "#00441b",
      weight = 500
    )
  ) |>
  tab_style(
    locations = cells_body(columns = c(losses)),
    style = cell_text(
      color = "#67000d",
      weight = 500
    )
  ) |>
  tab_style(
    locations = cells_body(columns = scored),
    style = cell_text(
      weight = 400,
      align = "center"
    )
  ) |>
  tab_style(
    locations = cells_row_groups(),
    style = cell_text(
      weight = 900
    )
  ) |>
  tab_style(
    locations = cells_column_labels(),
    style = cell_text(
      align = "center"
    )
  ) |>
  tab_footnote(
    footnote = "Líkur á að enda meðal tveggja efstu liðanna",
    locations = cells_column_labels(p_top)
  ) |>
  tab_header(
    title = "Hvernig endar riðlakeppnin á HM karla í handbolta?",
    subtitle = "Handboltalíkan Metils fengið til að spá fyrir um niðurstöðu allra leikja í riðlum"
  ) |>
  tab_options(
    table.background.color = "#fdfcfc"
  )



read_csv(here("results", sex, clock::date_build(2026, 1, 21), "d.csv")) |> 
  filter(
    date >= clock::date_build(2026, 1, 15),
    division == 1
  ) |> 
  mutate(
    result = case_when(
      home_goals > away_goals + 0.5 ~ "home",
      home_goals < away_goals - 0.5 ~ "away",
      TRUE ~ "draw"
    )
  ) |>
  pivot_longer(c(home, away), values_to = "team") |>
  mutate(
    points = case_when(
      result == name ~ 2,
      result == "draw" ~ 1,
      TRUE ~ 0
    ),
    scored = if_else(
      name == "home",
      home_goals,
      away_goals
    ),
    conceded = if_else(
      name == "away",
      home_goals,
      away_goals
    )
  ) |>
  summarise(
    base_points = sum(points),
    base_scored = sum(scored),
    base_conceded = sum(conceded),
    base_wins = sum(points == 2),
    base_losses = sum(points == 0),
    .by = c(team)
  ) |>
  arrange(desc(base_points)) |> 
  inner_join(
    groups
  ) |> 
  arrange(group, desc(base_points), desc(base_scored)) |> 
  mutate(
    pos = row_number(),
    top = if_else(pos <= 2, "Já", "Nei"),
    .by = group
  ) |> 
  select(
    group, 
    team, 
    top, 
    pos,
    wins = base_wins, 
    losses = base_losses, 
    scored = base_scored, 
    conceded = base_conceded, 
    points = base_points
  ) |> 
  mutate(
    group = str_c(group, "-riðill")
  ) |>
  group_by(group) |>
  gt() |>
  cols_label(
    team = "",
    top = "Áfram",
    pos = "Sæti",
    points = "Stig",
    wins = "Sigrar",
    losses = "Töp",
    scored = "Skoruð:Fengin",
    conceded = "Fengin"
  ) |>
  cols_merge(
    c(scored, conceded),
    pattern = "{1}:{2}"
  ) |>
  tab_spanner(
    columns = c(scored, conceded),
    label = "Mörk"
  ) |>
  tab_style(
    locations = cells_body(columns = points),
    style = cell_text(
      weight = 800
    )
  ) |>
  tab_style(
    locations = cells_body(columns = c(wins)),
    style = cell_text(
      color = "#00441b",
      weight = 500
    )
  ) |>
  tab_style(
    locations = cells_body(columns = c(losses)),
    style = cell_text(
      color = "#67000d",
      weight = 500
    )
  ) |>
  tab_style(
    locations = cells_body(columns = scored),
    style = cell_text(
      weight = 400,
      align = "center"
    )
  ) |>
  tab_style(
    locations = cells_row_groups(),
    style = cell_text(
      weight = 900
    )
  ) |>
  tab_style(
    locations = cells_column_labels(),
    style = cell_text(
      align = "center"
    )
  ) |>
  tab_footnote(
    footnote = "Komst liðið áfram úr riðlakeppninni",
    locations = cells_column_labels(top)
  ) |>
  tab_header(
    title = "Hvernig fór riðlakeppnin á HM karla í handbolta?",
    subtitle = "Samantekt á niðurstöðum riðlanna"
  ) |>
  tab_options(
    table.background.color = "#fdfcfc"
  ) |> 
  gtsave(
    filename = here(
      "results",
      sex,
      "finished_group_table.png"
    ),
    expand = c(1, 5, 1, -2)
  )


