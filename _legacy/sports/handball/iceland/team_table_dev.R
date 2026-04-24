#### League Table ####

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
    division == 1
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
    date >= clock::date_build(2025, 09, 04),
    division == 1
  ) |>
  mutate(
    result = case_when(
      home_goals > away_goals ~ "home",
      TRUE ~ "away"
    )
  ) |>
  pivot_longer(c(home, away), values_to = "team") |>
  mutate(
    points = case_when(
      result == name ~ 2,
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
      home_goals > away_goals ~ "home",
      TRUE ~ "away"
    )
  ) |>
  pivot_longer(c(home, away), values_to = "team") |>
  mutate(
    points = case_when(
      result == name ~ 2,
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
  mutate(
    position = row_number(),
    .by = c(iteration)
  ) |>
  summarise(
    p_top = mean(position == 1),
    p_bottom = mean(position >= max(position) - 1),
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
    .by = c(team)
  ) |>
  arrange(desc(mean_points)) |>
  select(-lower_pos, -upper_pos)

p_top |>
  full_join(
    tribble(
      ~team         , ~logo                                                                                                                                                    ,
      "Afturelding" , "https://afturelding.is/wp-content/uploads/2018/08/og-image.jpg"                                                                                         ,
      "Haukar"      , "https://www.haukar.is/wp-content/uploads/2014/12/haukar_200.png"                                                                                        ,
      "FH"          , "https://fh.is/wp-content/themes/fh/library/images/fh-cover.png"                                                                                         ,
      "Fram"        , "https://upload.wikimedia.org/wikipedia/en/c/c0/Knattspyrnuf%C3%A9lagi%C3%B0_Fram.png"                                                                   ,
      "Valur"       , "https://upload.wikimedia.org/wikipedia/is/thumb/e/e4/Valur.svg/1062px-Valur.svg.png"                                                                    ,
      "ÍBV"         , "https://upload.wikimedia.org/wikipedia/is/4/41/Ibv-logo.png"                                                                                            ,
      "KA"          , "https://api.stubb.is/assets/teamlogos/600.png"                                                                                                          ,
      "Stjarnan"    , "https://static1.squarespace.com/static/643c69cfa3d6b94d7a3e66d6/t/67857f66c7f7c37374353353/1736802169011/Stjarnan_skjoldur_blatt%402x.png?format=1500w" ,
      "Selfoss"     , "https://www.selfoss.net/static/news/1739893140_selfoss.merki.png"                                                                                       ,
      "HK"          , "https://upload.wikimedia.org/wikipedia/commons/3/3d/Logo_HK_Kopavogur.svg"                                                                              ,
      "Þór"         , "https://www.ksi.is/library/motakerfi/lid/603.png"                                                                                                       ,
      "ÍR"          , "https://toppng.com/uploads/preview/ir-reykjavik-vector-logo-11573731311gqjxkq6lgz.png"
    )
  ) |>
  select(team, logo, everything()) |>
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
    c(p_top, p_bottom),
    decimals = 0
  ) |>
  cols_hide(columns = c(mean_pos, scored, conceded)) |>
  cols_label(
    team = "",
    logo = "",
    p_top = "Vinnur",
    p_bottom = "Fellur",
    mean_pos = "Sæti",
    pos_interval = "Sæti",
    mean_points = "Stig",
    wins = "Sigrar",
    losses = "Töp"
  ) |>
  gtExtras::gt_img_rows(
    columns = logo,
    height = 30
  ) |>
  cols_align(
    columns = logo,
    align = "center"
  ) |>
  cols_move_to_end(mean_points) |>
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
      weight = 700
    )
  ) |>
  tab_style(
    locations = cells_body(columns = c(losses)),
    style = cell_text(
      color = "#67000d",
      weight = 700
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
    footnote = "Líkur á að vinna deildina",
    locations = cells_column_labels(p_top)
  ) |>
  tab_footnote(
    footnote = "Líkur á að falla niður um deild",
    locations = cells_column_labels(p_bottom)
  ) |>
  tab_header(
    title = md(
      str_c(
        glue("Spá um niðurstöðu Olís deildar {translate_sex(sex)} "),
        "<img src='https://ob.olis.is/assets/images/logos/olisdeildin_graent.png?2022' style='height:50px;vertical-align:text-bottom;'>"
      )
    ),
    subtitle = "Handboltalíkan Metils fengið til að spá fyrir um niðurstöðu eftirstandandi leikja á tímabilinu"
  ) |>
  tab_options(
    # table.background.color = "#fdfcfc"
  ) |>
  gtExtras::gt_theme_538() |>
  tab_style(
    style = "vertical-align:bottom",
    locations = cells_title()
  )


d |>
  filter(
    date >= clock::date_build(2025, 09, 04),
    division == 1
  ) |>
  mutate(
    result = case_when(
      home_goals > away_goals ~ "home",
      TRUE ~ "away"
    )
  ) |>
  pivot_longer(c(home, away), values_to = "team") |>
  mutate(
    points = case_when(
      result == name ~ 2,
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
    ),
    won = 1 * (points == 2)
  ) |>
  summarise(
    base_points = sum(points),
    base_scored = sum(scored),
    base_conceded = sum(conceded),
    base_wins = sum(points == 2),
    base_losses = sum(points == 0),
    outcomes = list(won),
    .by = c(team)
  ) |>
  arrange(desc(base_points)) |>
  gt() |>
  gtExtras::gt_plt_winloss(
    column = outcomes,
    type = "pill",
    max_wins = 22
  )
