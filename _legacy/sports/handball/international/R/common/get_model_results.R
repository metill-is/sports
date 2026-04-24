#### Packages ####
box::use(
  stats[median, quantile],
  readr[read_rds, read_csv, write_csv, parse_number],
  dplyr[
    mutate,
    select,
    filter,
    arrange,
    count,
    summarise,
    distinct,
    pull,
    inner_join,
    left_join,
    right_join,
    semi_join,
    bind_rows,
    reframe,
    mutate_at,
    vars,
    if_else,
    case_when,
    row_number,
    coalesce,
    join_by,
    group_by
  ],
  tidyr[pivot_longer, pivot_wider, unnest_wider],
  stringr[str_detect, str_match, str_c],
  forcats[fct_reorder, fct_relevel, as_factor],
  lubridate[today],
  tibble[as_tibble, tibble],
  ggplot2[
    ggplot,
    aes,
    geom_segment,
    geom_vline,
    geom_hline,
    geom_point,
    geom_col,
    scale_x_continuous,
    scale_y_continuous,
    scale_y_discrete,
    scale_colour_manual,
    scale_colour_brewer,
    scale_fill_manual,
    scale_alpha_continuous,
    coord_cartesian,
    facet_wrap,
    labs,
    theme,
    element_text,
    element_blank,
    margin,
    expansion,
    guide_axis,
    guide_none,
    sec_axis,
    ggsave,
    theme_set
  ],
  scales[percent, breaks_width],
  posterior[as_draws_df],
  metill[theme_metill, label_hlutf],
  ggtext[geom_richtext],
  glue[glue],
  here[here],
  gt[
    gt,
    fmt_number,
    fmt_percent,
    cols_hide,
    cols_label,
    cols_merge,
    cols_move_to_end,
    tab_spanner,
    tab_style,
    cells_body,
    cell_text,
    cells_row_groups,
    cells_column_labels,
    tab_header,
    tab_footnote,
    gtsave,
    cols_align,
    md,
    cells_title,
    tab_options
  ],
  gtExtras[
    gt_plt_bar
  ]
)

theme_set(theme_metill())
Sys.setlocale("LC_ALL", "is_IS.UTF-8")

#' Generate model results and visualizations
#'
#' @param sex Character string, either "male" or "female"
#' @param from_season Integer, starting season for analysis (default: 2021)
#'
#' @export
generate_model_results <- function(sex = "female", end_date = Sys.Date()) {
  # Validate input
  if (!sex %in% c("male", "female")) {
    stop("Sex must be either 'male' or 'female'")
  }

  #### Data Prep ####
  results <- read_rds(here("results", sex, end_date, "fit.rds"))

  d <- read_csv(here("results", sex, end_date, "d.csv"))
  teams <- read_csv(here("results", sex, end_date, "teams.csv"))
  next_games <- read_csv(here("results", sex, end_date, "next_games.csv"))
  top_teams <- read_csv(here("results", sex, end_date, "top_teams.csv"))
  pred_d <- read_csv(here("results", sex, end_date, "pred_d.csv"))

  #### Next-Round Predictions ####

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
      date <= end_date + 7,
      date > end_date
    ) |>
    mutate(
      game_nr = game_nr - min(game_nr) + 1
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

  posterior_goals |>
    write_csv(here("results", sex, end_date, "posterior_goals.csv"))

  plot_dat <- posterior_goals |>
    mutate(
      goal_diff = away_goals - home_goals
    ) |>
    reframe(
      median = median(goal_diff),
      coverage = c(
        0.025,
        0.05,
        0.1,
        0.2,
        0.3,
        0.4,
        0.5,
        0.6,
        0.7,
        0.8,
        0.9,
        0.95,
        0.975
      ),
      lower = quantile(goal_diff, 0.5 - coverage / 2),
      upper = quantile(goal_diff, 0.5 + coverage / 2),
      home_win = mean(goal_diff <= -0.5),
      away_win = mean(goal_diff >= 0.5),
      .by = c(game_nr, date, home, away)
    ) |>
    filter(
      date <= end_date + 2
    ) |>
    mutate(
      home_win = percent(home_win, accuracy = 1),
      away_win = percent(away_win, accuracy = 1),
      home = glue("{home} ({home_win})"),
      away = glue("{away} ({away_win})")
    )


  plot_dat |>
    ggplot(aes(median, max(game_nr) - game_nr + 1)) +
    geom_vline(
      xintercept = 0,
      lty = 2,
      alpha = 0.4,
      linewidth = 0.3
    ) +
    geom_hline(
      yintercept = seq(1, length(unique(plot_dat$game_nr)), 2),
      linewidth = 8,
      alpha = 0.1
    ) +
    geom_point(
      shape = "|",
      size = 5
    ) +
    geom_segment(
      aes(
        x = lower,
        xend = upper,
        yend = max(game_nr) - game_nr + 1,
        alpha = -coverage
      ),
      linewidth = 3
    ) +
    geom_richtext(
      data = tibble(x = 1),
      inherit.aes = FALSE,
      x = -50,
      y = -0.55,
      label.colour = NA,
      fill = NA,
      label = "&larr; Heimalið vinnur",
      hjust = 0,
      size = 4.5,
      colour = "grey40"
    ) +
    geom_richtext(
      data = tibble(x = 1),
      inherit.aes = FALSE,
      x = 50,
      y = -0.55,
      label.colour = NA,
      fill = NA,
      label = "Gestir vinna &rarr;",
      hjust = 1,
      size = 4.5,
      colour = "grey40"
    ) +
    scale_alpha_continuous(
      range = c(0, 0.3),
      guide = guide_none()
    ) +
    scale_x_continuous(
      guide = guide_axis(cap = "both"),
      labels = \(x) abs(x)
    ) +
    scale_y_continuous(
      guide = guide_axis(cap = "both"),
      breaks = seq(length(unique(plot_dat$game_nr)), 1),
      labels = \(x) {
        plot_dat |>
          distinct(game_nr, home, away) |>
          pull(home)
      },
      sec.axis = sec_axis(
        transform = \(x) x,
        breaks = seq(length(unique(plot_dat$game_nr)), 1),
        labels = \(x) {
          plot_dat |>
            distinct(game_nr, home, away) |>
            pull(away)
        },
        guide = guide_axis(cap = "both")
      )
    ) +
    coord_cartesian(
      ylim = c(1, max(plot_dat$game_nr)),
      xlim = c(-20, 20),
      clip = "off"
    ) +
    theme(
      legend.position = "none",
      plot.margin = margin(5, 5, 5, 5)
    ) +
    labs(
      x = "Markamismunur",
      y = NULL,
      colour = NULL,
      title = "Handboltaspá Metils",
      subtitle = str_c(
        "Líkindadreifing spár um úrslit næstu leikja",
        " | ",
        "Sigurlíkur merktar inni í sviga"
      )
    )

  ggsave(
    filename = here(
      "results",
      sex,
      end_date,
      "figures",
      "next_round_predictions.png"
    ),
    width = 8,
    height = 0.4 * 8,
    scale = 1.2
  )
  
   #### Iceland Games Predictions ####
  
  plot_dat <- posterior_goals |>
    mutate(
      goal_diff = away_goals - home_goals
    ) |>
    reframe(
      median = median(goal_diff),
      coverage = c(
        0.025,
        0.05,
        0.1,
        0.2,
        0.3,
        0.4,
        0.5,
        0.6,
        0.7,
        0.8,
        0.9,
        0.95,
        0.975
      ),
      lower = quantile(goal_diff, 0.5 - coverage / 2),
      upper = quantile(goal_diff, 0.5 + coverage / 2),
      home_win = mean(goal_diff <= -0.5),
      away_win = mean(goal_diff >= 0.5),
      .by = c(game_nr, date, home, away)
    ) |>
    filter(
      (home == "Iceland") | (away == "Iceland")
    ) |>
    mutate(
      home_win = percent(home_win, accuracy = 1),
      away_win = percent(away_win, accuracy = 1),
      home = glue("{home} ({home_win})"),
      away = glue("{away} ({away_win})"),
      game_nr = as.numeric(as.factor(game_nr))
    )
  
  
  plot_dat |>
    ggplot(aes(median, max(game_nr) - game_nr + 1)) +
    geom_vline(
      xintercept = 0,
      lty = 2,
      alpha = 0.4,
      linewidth = 0.3
    ) +
    geom_hline(
      yintercept = seq(1, length(unique(plot_dat$game_nr)), 2),
      linewidth = 8,
      alpha = 0.1
    ) +
    geom_point(
      shape = "|",
      size = 5
    ) +
    geom_segment(
      aes(
        x = lower,
        xend = upper,
        yend = max(game_nr) - game_nr + 1,
        alpha = -coverage
      ),
      linewidth = 3
    ) +
    geom_richtext(
      data = tibble(x = 1),
      inherit.aes = FALSE,
      x = -50,
      y = -0.55,
      label.colour = NA,
      fill = NA,
      label = "&larr; Heimalið vinnur",
      hjust = 0,
      size = 4.5,
      colour = "grey40"
    ) +
    geom_richtext(
      data = tibble(x = 1),
      inherit.aes = FALSE,
      x = 50,
      y = -0.55,
      label.colour = NA,
      fill = NA,
      label = "Gestir vinna &rarr;",
      hjust = 1,
      size = 4.5,
      colour = "grey40"
    ) +
    scale_alpha_continuous(
      range = c(0, 0.3),
      guide = guide_none()
    ) +
    scale_x_continuous(
      guide = guide_axis(cap = "both"),
      labels = \(x) abs(x)
    ) +
    scale_y_continuous(
      guide = guide_axis(cap = "both"),
      breaks = seq(length(unique(plot_dat$game_nr)), 1),
      labels = \(x) {
        plot_dat |>
          distinct(game_nr, home, away) |>
          pull(home)
      },
      sec.axis = sec_axis(
        transform = \(x) x,
        breaks = seq(length(unique(plot_dat$game_nr)), 1),
        labels = \(x) {
          plot_dat |>
            distinct(game_nr, home, away) |>
            pull(away)
        },
        guide = guide_axis(cap = "both")
      )
    ) +
    coord_cartesian(
      ylim = c(0.95, max(plot_dat$game_nr)),
      xlim = c(-20, 20),
      clip = "off"
    ) +
    theme(
      legend.position = "none",
      plot.margin = margin(5, 5, 5, 5)
    ) +
    labs(
      x = "Markamismunur",
      y = NULL,
      colour = NULL,
      title = "Handboltaspá Metils",
      subtitle = str_c(
        "Líkindadreifing spár um úrslit næstu leikja",
        " | ",
        "Sigurlíkur merktar inni í sviga"
      )
    )
  
  ggsave(
    filename = here(
      "results",
      "male",
      end_date,
      "figures",
      "iceland_games.png"
    ),
    width = 8,
    height = 0.3 * 8,
    scale = 1.3
  )

  #### Group Stage Predictions ####

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
    ) |>
    gtsave(
      filename = here(
        "results",
        sex,
        end_date,
        "figures",
        "group_table.png"
      ),
      expand = c(1, 5, 1, -2)
    )
  
  #### Main Round Group Stage Predictions ####
  
  groups <- tibble(
    data = list(
      group_1 = list(
        "France",
        "Germany",
        "Portugal",
        "Spain",
        "Denmark",
        "Norway"
      ),
      group_2 = list(
        "Sweden",
        "Slovenia",
        "Iceland",
        "Hungary",
        "Switzerland",
        "Croatia"
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
    select(group, team = value) |> 
    mutate(
      pre_points = c(2, 2, 2, 0, 0, 0, 2, 2, 2, 0, 0, 0),
      pre_scored = c(38, 34, 31, 32, 29, 34, 33, 38, 24, 23, 35, 25),
      pre_conceded = c(34, 32, 29, 34, 31, 38, 25, 35, 23, 24, 38, 33),
      pre_wins = c(1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0),
      pre_losses = c(0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1)
    )
  
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
      date >= clock::date_build(2026, 1, 22),
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
    arrange(desc(base_points)) |> 
    right_join(
      groups
    ) |> 
    mutate_at(
      vars(c(base_points:base_losses)),
      coalesce, 0
    ) |> 
    mutate(
      base_points = base_points + pre_points,
      base_scored = base_scored + pre_scored,
      base_conceded = base_conceded + pre_conceded,
      base_wins = base_wins + pre_wins,
      base_losses = base_losses + pre_losses
    ) |> 
    select(-(group:pre_losses))
  
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
      conceded = conceded + base_conceded,
      goal_diff = scored - conceded
    ) |>
    arrange(desc(points), desc(goal_diff)) |>
    select(-goal_diff) |> 
    inner_join(
      groups
    ) |>
    mutate(
      position = row_number(),
      .by = c(iteration, group)
    ) |>
    summarise(
      p_top = mean(position <= 2),
      p_middle = mean(position == 3),
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
      c(p_top, p_middle),
      decimals = 0
    ) |>
    cols_hide(mean_pos) |>
    cols_label(
      team = "",
      p_top = "Úrslit",
      p_middle = "Umspil",
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
    tab_footnote(
      footnote = "Líkur á að enda í þriðja sæti",
      locations = cells_column_labels(p_middle)
    ) |>
    tab_header(
      title = "Hvernig endar seinni riðlakeppnin á HM karla í handbolta?",
      subtitle = "Handboltalíkan Metils fengið til að spá fyrir um niðurstöðu allra leikja í riðlum"
    ) |>
    tab_options(
      table.background.color = "#fdfcfc"
    ) |>
    gtsave(
      filename = here(
        "results",
        sex,
        end_date,
        "figures",
        "main_group_table.png"
      ),
      expand = c(1, 5, 1, -2)
    )

  #### Posterior Results ####

  #### Current Strengths ####

  plot_dat_away <- results$draws("cur_strength_away") |>
    as_draws_df() |>
    as_tibble() |>
    pivot_longer(c(-.chain, -.draw, -.iteration)) |>
    mutate(
      team = teams$team[parse_number(name)],
      type = "Samtals"
    ) |>
    bind_rows(
      results$draws("cur_offense_away") |>
        as_draws_df() |>
        as_tibble() |>
        pivot_longer(c(-.chain, -.draw, -.iteration)) |>
        mutate(
          team = teams$team[parse_number(name)],
          type = "Sókn"
        )
    ) |>
    bind_rows(
      results$draws("cur_defense_away") |>
        as_draws_df() |>
        as_tibble() |>
        pivot_longer(c(-.chain, -.draw, -.iteration)) |>
        mutate(
          team = teams$team[parse_number(name)],
          type = "Vörn"
        )
    ) |>
    reframe(
      median = median(value),
      coverage = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
      lower = quantile(value, 0.5 - coverage / 2),
      upper = quantile(value, 0.5 + coverage / 2),
      .by = c(team, type)
    ) |>
    mutate(
      type = as_factor(type) |>
        fct_relevel("Sókn", "Vörn", "Samtals"),
      team = factor(
        team,
        levels = unique(team)[order(unique(median[type == "Samtals"]))]
      )
    )

  plot_dat_home <- results$draws("cur_strength_home") |>
    as_draws_df() |>
    as_tibble() |>
    pivot_longer(c(-.chain, -.draw, -.iteration)) |>
    mutate(
      team = teams$team[parse_number(name)],
      type = "Samtals"
    ) |>
    bind_rows(
      results$draws("cur_offense_home") |>
        as_draws_df() |>
        as_tibble() |>
        pivot_longer(c(-.chain, -.draw, -.iteration)) |>
        mutate(
          team = teams$team[parse_number(name)],
          type = "Sókn"
        )
    ) |>
    bind_rows(
      results$draws("cur_defense_home") |>
        as_draws_df() |>
        as_tibble() |>
        pivot_longer(c(-.chain, -.draw, -.iteration)) |>
        mutate(
          team = teams$team[parse_number(name)],
          type = "Vörn"
        )
    ) |>
    reframe(
      median = median(value),
      coverage = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
      lower = quantile(value, 0.5 - coverage / 2),
      upper = quantile(value, 0.5 + coverage / 2),
      .by = c(team, type)
    ) |>
    mutate(
      type = as_factor(type) |>
        fct_relevel("Sókn", "Vörn", "Samtals"),
      team = factor(
        team,
        levels = unique(team)[order(unique(median[type == "Samtals"]))]
      )
    )

  plot_dat <- plot_dat_away |>
    mutate(
      loc = "Gestir"
    ) |>
    bind_rows(
      plot_dat_home |>
        mutate(
          loc = "Heima"
        )
    ) |>
    mutate(
      loc = as_factor(loc) |>
        fct_relevel("Heima")
    ) |>
    semi_join(
      d |>
        filter(season == 2026, division == 1) |>
        pivot_longer(c(home, away), values_to = "team") |>
        distinct(team) |> 
        bind_rows(
          next_games |> 
            pivot_longer(c(home, away), values_to = "team") |>
            distinct(team)
        ) |> 
        distinct(team)
    )

  dodge <- 0.3

  plot_dat |>
    semi_join(
      groups
    ) |>
    ggplot(aes(median, team)) +
    geom_hline(
      yintercept = seq(1, 24, 2),
      linewidth = 8,
      alpha = 0.05
    ) +
    geom_point(
      shape = "|",
      size = 5,
      aes(col = loc)
    ) +
    geom_segment(
      aes(
        x = lower,
        xend = upper,
        yend = team,
        alpha = -coverage,
        col = loc
      ),
      linewidth = 2
    ) +
    scale_alpha_continuous(
      range = c(0, 0.3),
      guide = guide_none()
    ) +
    scale_x_continuous(
      guide = guide_none(),
      expand = expansion(mult = c(0.01, 0.05))
    ) +
    scale_y_discrete(
      guide = guide_axis(cap = "both")
    ) +
    scale_colour_brewer(
      palette = "Set1",
      direction = -1
    ) +
    facet_wrap("type", scales = "free_x") +
    theme(
      plot.margin = margin(5, 5, 5, 5),
      legend.position = c(0.54, 1.064),
      legend.background = element_blank(),
      legend.direction = "horizontal",
      legend.text = element_text(family = "Lato", colour = "#525252")
    ) +
    labs(
      x = NULL,
      y = NULL,
      colour = NULL,
      title = "Styrkur landsliða fyrir HM karla í handbolta",
      subtitle = "Metið með handboltalíkani Metils"
    )

  ggsave(
    filename = here("results", sex, end_date, "figures", "styrkur.png"),
    width = 8,
    height = 0.7 * 8,
    scale = 1.1
  )

  hosts <- c("Norway", "Denmark", "Sweden")

  plot_dat |>
    semi_join(groups) |>
    filter(
      ((!team %in% hosts) & (loc == "Gestir")) |
        ((team %in% hosts) & (loc == "Heima"))
    ) |>
    distinct(
      team,
      type,
      median
    ) |>
    pivot_wider(names_from = type, values_from = median) |>
    select(team, Sókn, Vörn, Samtals) |>
    arrange(desc(Samtals)) |>
    mutate(
      Sókn = Sókn - Sókn[Samtals == min(Samtals)],
      Vörn = Vörn - Vörn[Samtals == min(Samtals)],
      Samtals = Samtals - Samtals[Samtals == min(Samtals)]
    ) |>
    gt() |>
    fmt_number(-team, decimals = 1) |>
    cols_align(columns = team, "left") |>
    cols_label(
      team = "",
      Sókn = md(
        "**Sókn**<br>*Hvað skorar liðið að jafnaði<br>mörgum mörkum fleiri en veikasta liðið?*"
      ),
      Vörn = md(
        "**Vörn**<br>*Hvað verst liðið að jafnaði gegn<br>mörgum mörkum fleiri en veikasta liðið?*"
      ),
      Samtals = md(
        "**Samtals**<br>*Með hve miklum mun myndi<br>liðið að jafnaði vinna veikasta liðið?*"
      )
    ) |>
    gt_plt_bar(column = Sókn, scale_type = "number", color = "#08306b") |>
    gt_plt_bar(column = Vörn, scale_type = "number", color = "#67000d") |>
    gt_plt_bar(column = Samtals, scale_type = "number", color = "#000000") |>
    tab_style(
      locations = cells_title(groups = "title"),
      style = cell_text(
        weight = 1000,
        align = "left"
      )
    ) |>
    tab_style(
      locations = cells_title(groups = "subtitle"),
      style = cell_text(
        weight = 7000,
        align = "left"
      )
    ) |>
    tab_footnote(
      footnote = "Styrkur Noregs, Svíþjóðar og Danmerkur á heimavelli notaður við útreikninga",
      locations = cells_body(
        columns = team,
        rows = team %in% hosts
      )
    ) |>
    tab_header(
      title = "Samantekt á sóknar- og varnarstyrk landsliða sem eftir eru á EM karla í handbolta",
      subtitle = "Til að auðvelda túlkun eru lið borin saman við veikasta liðið hverju sinni að mati líkansins"
    ) |>
    tab_options(
      table.background.color = "#fdfcfc"
    ) |>
    gtsave(
      filename = here(
        "results",
        sex,
        end_date,
        "figures",
        "styrkur_table.png"
      ),
      expand = c(1, 5, 1, -2)
    )

  #### Home Advantages ####

  results$draws("home_advantage_tot") |>
    as_draws_df() |>
    as_tibble() |>
    pivot_longer(c(-.chain, -.draw, -.iteration)) |>
    mutate(
      team_nr = name |> parse_number(),
      type = "Samanlögð áhrif á heildarstyrk",
      value = value
    ) |>
    bind_rows(
      results$draws("home_advantage_def") |>
        as_draws_df() |>
        as_tibble() |>
        pivot_longer(c(-.chain, -.draw, -.iteration)) |>
        mutate(
          team_nr = name |> parse_number(),
          type = "Áhrif á varnarstyrk heimaliðs"
        )
    ) |>
    bind_rows(
      results$draws("home_advantage_off") |>
        as_draws_df() |>
        as_tibble() |>
        pivot_longer(c(-.chain, -.draw, -.iteration)) |>
        mutate(
          team_nr = name |> parse_number(),
          type = "Áhrif á sóknarstyrk heimaliðs"
        )
    ) |>
    inner_join(
      teams,
      by = join_by(team_nr)
    ) |>
    semi_join(
      groups
    ) |>
    reframe(
      median = median(value),
      coverage = c(
        0.025,
        0.05,
        0.1,
        0.2,
        0.3,
        0.4,
        0.5,
        0.6,
        0.7,
        0.8,
        0.9,
        0.95,
        0.975
      ),
      lower = quantile(value, 0.5 - coverage / 2),
      upper = quantile(value, 0.5 + coverage / 2),
      .by = c(team, type)
    ) |>
    mutate(
      team = factor(
        team,
        levels = unique(team)[order(unique(median[
          type == "Samanlögð áhrif á heildarstyrk"
        ]))]
      )
    ) |>
    ggplot(aes(median, team)) +
    geom_vline(
      xintercept = 0,
      lty = 2,
      alpha = 0.4,
      linewidth = 0.3
    ) +
    geom_hline(
      yintercept = seq(1, nrow(teams), 2),
      linewidth = 7,
      alpha = 0.03
    ) +
    geom_point(
      shape = "|",
      size = 5
    ) +
    geom_segment(
      aes(
        x = lower,
        xend = upper,
        yend = team,
        alpha = -coverage
      ),
      linewidth = 3
    ) +
    scale_alpha_continuous(
      range = c(0, 0.3),
      guide = guide_none()
    ) +
    scale_x_continuous(
      guide = guide_axis(cap = "both"),
      breaks = seq(0, 12, by = 2)
    ) +
    scale_y_discrete(
      guide = guide_axis(cap = "both")
    ) +
    facet_wrap("type") +
    coord_cartesian(
      xlim = c(0, 12)
    ) +
    theme(
      legend.position = "none",
      plot.margin = margin(5, 10, 5, 5)
    ) +
    labs(
      x = NULL,
      y = NULL,
      colour = NULL,
      title = "Heimavallaráhrif landsliða karla í handbolta",
      subtitle = "Skora lið fleiri mörk á heimavelli? Skora gestirnir þeirra færri mörk?"
    )

  ggsave(
    filename = here(
      "results",
      sex,
      end_date,
      "figures",
      "home_advantage.png"
    ),
    width = 8,
    height = 0.6 * 8,
    scale = 1.4
  )

  #### Strength gains during championships ####

  plot_dat <- results$draws("off_casual") |>
    as_draws_df() |>
    as_tibble() |>
    pivot_longer(c(-.chain, -.draw, -.iteration)) |>
    mutate(
      team_nr = name |> parse_number(),
      type = "Sókn",
      value = value
    ) |>
    bind_rows(
      results$draws("def_casual") |>
        as_draws_df() |>
        as_tibble() |>
        pivot_longer(c(-.chain, -.draw, -.iteration)) |>
        mutate(
          team_nr = name |> parse_number(),
          type = "Vörn"
        )
    ) |>
    inner_join(
      teams,
      by = join_by(team_nr)
    ) |>
    semi_join(
      groups
    ) |>
    select(-name) |>
    pivot_wider(names_from = type) |>
    mutate(
      Samtals = Sókn + Vörn
    ) |>
    pivot_longer(
      c(Sókn, Vörn, Samtals),
      names_to = "type",
      values_to = "value"
    ) |>
    reframe(
      median = median(value),
      coverage = c(
        0.025,
        0.05,
        0.1,
        0.2,
        0.3,
        0.4,
        0.5,
        0.6,
        0.7,
        0.8,
        0.9,
        0.95,
        0.975
      ),
      lower = quantile(value, 0.5 - coverage / 2),
      upper = quantile(value, 0.5 + coverage / 2),
      .by = c(team, type)
    ) |>
    mutate(
      team = factor(
        team,
        levels = unique(team)[order(unique(median[
          type == "Samtals"
        ]))]
      ),
      type = fct_relevel(type, "Sókn", "Vörn", "Samtals")
    )

  plot_dat |>
    ggplot(aes(median, team)) +
    geom_vline(
      xintercept = 0,
      lty = 2,
      alpha = 0.4,
      linewidth = 0.3
    ) +
    geom_hline(
      yintercept = seq(1, nrow(teams), 2),
      linewidth = 7,
      alpha = 0.03
    ) +
    geom_point(
      shape = "|",
      size = 5
    ) +
    geom_segment(
      aes(
        x = lower,
        xend = upper,
        yend = team,
        alpha = -coverage
      ),
      linewidth = 3
    ) +
    scale_alpha_continuous(
      range = c(0, 0.3),
      guide = guide_none()
    ) +
    scale_x_continuous(
      guide = guide_axis(cap = "both"),
      breaks = seq(0, 40, by = 5)
    ) +
    scale_y_discrete(
      guide = guide_axis(cap = "both")
    ) +
    facet_wrap("type") +
    coord_cartesian(
      xlim = c(0, 20)
    ) +
    theme(
      legend.position = "none",
      plot.margin = margin(5, 10, 5, 5)
    ) +
    labs(
      x = NULL,
      y = NULL,
      colour = NULL,
      title = "Spila lið verr í vináttuleikjum?",
      subtitle = "Skora lið færri mörk í vináttuleikjum? Fá þau á sig fleiri mörk?"
    )

  ggsave(
    filename = here("results", sex, end_date, "figures", "casual.png"),
    width = 8,
    height = 0.9 * 8,
    scale = 1.1
  )

  plot_dat_away <- results$draws("cur_strength_away") |>
    as_draws_df() |>
    as_tibble() |>
    pivot_longer(c(-.chain, -.draw, -.iteration)) |>
    mutate(
      team = teams$team[parse_number(name)],
      type = "Samtals"
    ) |>
    bind_rows(
      results$draws("cur_offense_away") |>
        as_draws_df() |>
        as_tibble() |>
        pivot_longer(c(-.chain, -.draw, -.iteration)) |>
        mutate(
          team = teams$team[parse_number(name)],
          type = "Sókn"
        )
    ) |>
    bind_rows(
      results$draws("cur_defense_away") |>
        as_draws_df() |>
        as_tibble() |>
        pivot_longer(c(-.chain, -.draw, -.iteration)) |>
        mutate(
          team = teams$team[parse_number(name)],
          type = "Vörn"
        )
    ) |>
    reframe(
      median = median(value),
      coverage = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
      lower = quantile(value, 0.5 - coverage / 2),
      upper = quantile(value, 0.5 + coverage / 2),
      .by = c(team, type)
    ) |>
    mutate(
      type = as_factor(type) |>
        fct_relevel("Sókn", "Vörn", "Samtals"),
      team = factor(
        team,
        levels = unique(team)[order(unique(median[type == "Samtals"]))]
      )
    )

  plot_dat |>
    distinct(
      team,
      type,
      median
    ) |>
    pivot_wider(names_from = type, values_from = median) |>
    select(team, Sókn, Vörn, Samtals) |>
    arrange(desc(Samtals)) |>
    gt() |>
    fmt_number(-team, decimals = 1) |>
    cols_align(columns = team, "left") |>
    cols_label(
      team = "",
      Sókn = md(
        "**Sókn**<br>*Hvað skorar liðið að jafnaði mikið meira á stórmótum?*"
      ),
      Vörn = md(
        "**Vörn**<br>*Hvað verst liðið að jafnaði gegn mörgum mörkum fleiri á stórmótum?*"
      ),
      Samtals = md(
        "**Samtals**<br>*Hver eru áhrifin í heild sinni?*"
      )
    ) |>
    gt_plt_bar(column = Sókn, scale_type = "number", color = "#08306b") |>
    gt_plt_bar(column = Vörn, scale_type = "number", color = "#67000d") |>
    gt_plt_bar(column = Samtals, scale_type = "number", color = "#000000") |>
    tab_style(
      locations = cells_title(groups = "title"),
      style = cell_text(
        weight = 1000,
        align = "left"
      )
    ) |>
    tab_style(
      locations = cells_title(groups = "subtitle"),
      style = cell_text(
        weight = 7000,
        align = "left"
      )
    ) |>
    tab_header(
      title = "Spila þjóðir betur á stórmótum?",
      subtitle = "Landslið fá oft ekki aðgang að stórsjörnum nema á stórmótum. Sjáum við merki um að styrkur þeirra aukist á stórmótum vegna þessa?"
    ) |>
    tab_options(
      table.background.color = "#fdfcfc"
    ) |>
    gtsave(
      filename = here(
        "results",
        sex,
        end_date,
        "figures",
        "casual_table.png"
      ),
      expand = c(1, 5, 1, -2)
    )
}
