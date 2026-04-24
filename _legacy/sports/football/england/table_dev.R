tab_dat <- plot_dat |> 
  filter(placement %in% range(placement)) |> 
  select(team, placement, p_raw) |> 
  complete(team, placement, fill = list(p_raw = 0)) |> 
  pivot_wider(names_from = placement, values_from = p_raw) |> 
  rename(p1 = "1", p20 = "20")

tab <- d |>
  filter(season == 2025, division == 1) |>
  mutate(
    result = case_when(
      home_goals > away_goals ~ "home",
      home_goals < away_goals ~ "away",
      TRUE ~ "tie"
    )
  ) |>
  pivot_longer(c(home, away), values_to = "team") |>
  mutate(
    points = case_when(
      result == "tie" ~ 1,
      result == name ~ 3,
      TRUE ~ 0
    ),
    goals_for = if_else(
      name == "home",
      home_goals,
      away_goals
    ),
    goals_against = if_else(
      name == "away",
      home_goals,
      away_goals
    )
  ) |>
  summarise(
    base_points = sum(points),
    wins = sum(points == 3),
    ties = sum(points == 1),
    losses = sum(points == 0),
    goals_for = sum(goals_for),
    goals_against = sum(goals_against),
    goals_diff = goals_for - goals_against,
    .by = c(team)
  ) |>
  arrange(desc(base_points)) |> 
  inner_join(tab_dat)

max_goals <- tab$goals_diff |> 
  range() |> 
  abs() |> 
  max()

add_color_cols <- function(d, cols) {
  for(col in cols) {
    col_name <- str_c("color_", col)
    d <- d |> 
      mutate(
        "{col_name}" := "",
        .after = {col}
      )
  }
  
  d
}

tab |> 
  add_color_cols("p1")

tab |> 
  gt() |> 
  cols_label(
    team = "",
    base_points = "Stig",
    wins = "Sigrar",
    ties = "Jafntefli",
    losses = "Töp",
    goals_for = "Skoruð",
    goals_against = "Fengin",
    goals_diff = "Munur",
    p1 = "Efsta",
    p20 = "Neðsta"
  ) |> 
  tab_spanner(
    label = "Leikir",
    columns = c("wins", "ties", "losses") 
  ) |> 
  tab_spanner(
    label = "Mörk",
    columns = c("goals_for", "goals_against", "goals_diff")
  ) |> 
  tab_spanner(
    label = c("Líkur á að lenda í sæti"),
    columns = c(p1, p20)
  ) |> 
  fmt_percent(c(p1, p20), decimals = 0) |> 
  data_color(
    columns = p1, 
    method = "numeric",
    palette = "Blues", 
    domain = c(0, 1)
  ) |> 
  data_color(
    columns = p20, 
    method = "numeric",
    palette = "Reds",
    domain = c(0, 1)
  ) |>
  data_color(
    columns = c(base_points, wins, goals_for),
    palette = "Blues", 
  ) |> 
  # gt_color_rows(c(base_points, wins, goals_for), palette = "Blues") |> 
  # gt_color_rows(ties, palette = "Greys") |> 
  # gt_color_rows(c(losses, goals_against), palette = "Reds") |> 
  # gt_color_rows(goals_diff, palette = "RdBu", domain = 3 * c(-1, 1) * max_goals) |> 
  # gt_color_rows(p1, palette = "Blues", domain = c(0, 1)) |> 
  # gt_color_rows(p20, palette = "Reds", domain = c(0, 1)) |> 
  tab_header(
    title = "Enska deildin"
  )  
