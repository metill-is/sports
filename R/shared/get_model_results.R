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
    rename,
    summarise,
    distinct,
    pull,
    inner_join,
    left_join,
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
    group_by,
    desc,
    starts_with,
    contains,
    n
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
  scales[percent, breaks_width, label_percent],
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
#' @param config Config list from sport-specific config file
#' @param sex Character string, either "male" or "female"
#' @param end_date Date for filtering data
#'
#' @export
generate_model_results <- function(
  config,
  sex = "male",
  end_date = Sys.Date(),
  make_plots = TRUE
) {
  if (!sex %in% c("male", "female")) {
    stop("Sex must be either 'male' or 'female'")
  }

  sport_dir <- config$sport_dir
  labels <- config$labels
  scoring <- config$scoring
  plots <- config$plots

  translate_sex <- function(sex) {
    if_else(
      sex == "male",
      "karla",
      "kvenna"
    )
  }

  #### Data Prep ####
  results <- read_rds(here(sport_dir, "results", sex, "fit.rds"))

  d <- read_csv(here(sport_dir, "results", sex, "d.csv"))
  teams <- read_csv(here(sport_dir, "results", sex, "teams.csv"))
  next_games <- read_csv(here(
    sport_dir,
    "results",
    sex,
    "next_games.csv"
  ))
  top_teams <- read_csv(here(
    sport_dir,
    "results",
    sex,
    "top_teams.csv"
  ))
  pred_d <- read_csv(here(sport_dir, "results", sex, "pred_d.csv"))

  cur_teams <- next_games |>
    pivot_longer(c(home, away)) |>
    distinct(value) |>
    rename(team = value)

  #### Next-Round Predictions ####

  # Determine filter date for handball-style filtering
  if (!is.null(config$divisions$schedule_filter)) {
    next_game_date <- min(next_games$date)
    filter_date <- c(end_date + 14, next_game_date + 7) |> max()
  } else {
    filter_date <- NULL
  }

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
    )

  # Apply sport-specific filtering for next-round predictions
  if (!is.null(filter_date)) {
    # Handball: filter by date range
    posterior_goals <- posterior_goals |>
      filter(
        date <= filter_date,
        date >= end_date
      )
  } else {
    # Basketball: filter by top 20 per division
    posterior_goals <- posterior_goals |>
      arrange(date) |>
      filter(
        game_nr <= 20,
        date >= end_date,
        .by = division
      )
  }

  posterior_goals <- posterior_goals |>
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
    write_csv(here(sport_dir, "results", sex, "posterior_goals.csv"))

  if (!make_plots) return(invisible(NULL))

  # Build plot data with optional division labels
  if (plots$next_round$show_division) {
    div_labels <- labels$division_labels
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
        home_win = mean(goal_diff < 0),
        away_win = 1 - home_win,
        .by = c(game_nr, division, date, home, away)
      ) |>
      mutate(
        home_win = percent(home_win, accuracy = 1),
        away_win = percent(away_win, accuracy = 1),
        div = div_labels[division],
        home = glue("{home} ({home_win}) | {div}"),
        away = glue("{away} ({away_win}) | {div}")
      )
  } else {
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
        home_win = mean(goal_diff < 0),
        away_win = 1 - home_win,
        .by = c(game_nr, date, home, away)
      )

    # Apply additional date filter for handball
    if (!is.null(filter_date)) {
      plot_dat <- plot_dat |>
        filter(date <= filter_date)
    }

    plot_dat <- plot_dat |>
      mutate(
        home_win = percent(home_win, accuracy = 1),
        away_win = percent(away_win, accuracy = 1),
        home = glue("{home} ({home_win})"),
        away = glue("{away} ({away_win})")
      )
  }

  annotation_y <- plots$next_round$annotation_y
  xlim_val <- plots$next_round$xlim

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
      x = xlim_val[1],
      y = annotation_y,
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
      x = xlim_val[2],
      y = annotation_y,
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
      xlim = xlim_val,
      clip = "off"
    ) +
    theme(
      legend.position = "none",
      plot.margin = margin(5, 5, 5, 5)
    ) +
    labs(
      x = labels$goal_diff_label,
      y = NULL,
      colour = NULL,
      title = glue(
        "{labels$prediction_name} Metils fyrir {labels$leagues_full} {translate_sex(sex)}"
      ),
      subtitle = str_c(
        "Líkindadreifing spár um úrslit næstu leikja",
        " | ",
        "Sigurlíkur merktar inni í sviga"
      )
    )

  ggsave(
    filename = here(
      sport_dir,
      "results",
      sex,
      "figures",
      "next_round_predictions.png"
    ),
    width = 8,
    height = 0.8 * 8,
    scale = plots$next_round$scale
  )

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

  # Base points from current season
  base_points <- d |>
    filter(
      season == max(season),
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
        result == name ~ scoring$win_points,
        TRUE ~ scoring$loss_points
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
      base_wins = sum(points == scoring$win_points),
      base_losses = sum(points == scoring$loss_points),
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
        result == name ~ scoring$win_points,
        TRUE ~ scoring$loss_points
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
      wins = sum(points == scoring$win_points),
      losses = sum(points == scoring$loss_points),
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
      scored = mean(scored),
      conceded = mean(conceded),
      .by = c(team)
    ) |>
    arrange(desc(mean_points)) |>
    select(-lower_pos, -upper_pos)

  p_top |>
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
      p_top = "Vinnur",
      p_bottom = "Fellur",
      mean_pos = "Sæti",
      pos_interval = "Sæti",
      mean_points = "Stig",
      wins = "Sigrar",
      losses = "Töp"
    ) |>
    cols_merge(
      c(scored, conceded),
      pattern = "{1}:{2}"
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
      footnote = "Líkur á að vinna deildina",
      locations = cells_column_labels(p_top)
    ) |>
    tab_footnote(
      footnote = "Líkur á að falla niður um deild",
      locations = cells_column_labels(p_bottom)
    ) |>
    tab_header(
      title = glue("Hvernig endar {labels$league_name} {translate_sex(sex)}?"),
      subtitle = glue(
        "{labels$model_description_cap} fengið til að spá fyrir um niðurstöðu allra leikja á tímabilinu"
      )
    ) |>
    tab_options(
      table.background.color = "#fdfcfc"
    ) |>
    gtsave(
      filename = here(
        sport_dir,
        "results",
        sex,
        "figures",
        "group_table.png"
      ),
      expand = c(1, 5, 1, -2)
    )

  #### League Result Prediction ####

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
    inner_join(pred_d, by = "game_nr") |>
    filter(
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

  # Base points for league result prediction - use max season
  if (scoring$has_ties) {
    base_points <- d |>
      filter(season == max(season), division == 1) |>
      mutate(
        result = case_when(
          abs(home_goals - away_goals) < scoring$tie_threshold ~ "tie",
          home_goals > away_goals ~ "home",
          home_goals < away_goals ~ "away"
        )
      ) |>
      pivot_longer(c(home, away), values_to = "team") |>
      mutate(
        points = case_when(
          result == "tie" ~ scoring$tie_points,
          result == name ~ scoring$win_points,
          TRUE ~ scoring$loss_points
        )
      ) |>
      summarise(
        base_points = sum(points),
        .by = c(team)
      ) |>
      arrange(desc(base_points))
  } else {
    base_points <- d |>
      filter(season == max(season), division == 1) |>
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
          result == name ~ scoring$win_points,
          TRUE ~ scoring$loss_points
        )
      ) |>
      summarise(
        base_points = sum(points),
        .by = c(team)
      ) |>
      arrange(desc(base_points))
  }

  # Points calculation for p_top
  if (scoring$has_ties) {
    p_top <- posterior_goals |>
      mutate(
        result = case_when(
          abs(home_goals - away_goals) < scoring$tie_threshold ~ "tie",
          home_goals > away_goals ~ "home",
          home_goals < away_goals ~ "away"
        )
      ) |>
      pivot_longer(c(home, away), values_to = "team") |>
      mutate(
        points = case_when(
          result == name ~ scoring$win_points,
          result == "tie" ~ scoring$tie_points,
          TRUE ~ scoring$loss_points
        )
      ) |>
      summarise(
        points = sum(points),
        .by = c(iteration, team)
      ) |>
      left_join(
        base_points
      ) |>
      mutate(
        base_points = coalesce(base_points, 0),
        points = points + base_points
      ) |>
      arrange(desc(points)) |>
      mutate(
        position = row_number(),
        .by = iteration
      ) |>
      summarise(
        p_top = mean(position <= plots$league_winner$top_n),
        .by = team
      ) |>
      arrange(desc(p_top))
  } else {
    p_top <- posterior_goals |>
      mutate(
        result = case_when(
          home_goals > away_goals ~ "home",
          home_goals < away_goals ~ "away"
        )
      ) |>
      pivot_longer(c(home, away), values_to = "team") |>
      mutate(
        points = case_when(
          result == name ~ scoring$win_points,
          TRUE ~ scoring$loss_points
        )
      ) |>
      summarise(
        points = sum(points),
        .by = c(iteration, team)
      ) |>
      left_join(
        base_points
      ) |>
      mutate(
        base_points = coalesce(base_points, 0),
        points = points + base_points
      ) |>
      arrange(desc(points)) |>
      mutate(
        position = row_number(),
        .by = iteration
      ) |>
      summarise(
        p_top = mean(position <= plots$league_winner$top_n),
        .by = team
      ) |>
      arrange(desc(p_top))
  }

  # Points distribution plot data
  if (scoring$has_ties) {
    plot_dat <- posterior_goals |>
      mutate(
        result = case_when(
          home_goals > away_goals ~ "home",
          home_goals < away_goals ~ "away"
        )
      ) |>
      pivot_longer(c(home, away), values_to = "team") |>
      mutate(
        points = case_when(
          result == name ~ scoring$win_points,
          TRUE ~ scoring$tie_points
        )
      ) |>
      summarise(
        points = sum(points),
        .by = c(iteration, team)
      ) |>
      left_join(
        base_points
      ) |>
      mutate(
        base_points = coalesce(base_points, 0),
        points = points + base_points
      ) |>
      count(team, points) |>
      mutate(
        p_raw = n / sum(n),
        p = p_raw / max(p_raw),
        mean = sum(p_raw * points),
        .by = team
      ) |>
      inner_join(p_top) |>
      mutate(
        p_top = scales::percent(p_top, accuracy = 1),
        team = glue("{team} ({p_top})"),
        team = fct_reorder(team, mean),
        team_nr = as.numeric(team)
      )
  } else {
    plot_dat <- posterior_goals |>
      mutate(
        result = case_when(
          home_goals > away_goals ~ "home",
          home_goals < away_goals ~ "away"
        )
      ) |>
      pivot_longer(c(home, away), values_to = "team") |>
      mutate(
        points = case_when(
          result == name ~ scoring$win_points,
          TRUE ~ scoring$loss_points
        )
      ) |>
      summarise(
        points = sum(points),
        .by = c(iteration, team)
      ) |>
      left_join(
        base_points
      ) |>
      mutate(
        base_points = coalesce(base_points, 0),
        points = points + base_points
      ) |>
      count(team, points) |>
      mutate(
        p_raw = n / sum(n),
        p = p_raw / max(p_raw),
        mean = sum(p_raw * points),
        .by = team
      ) |>
      inner_join(p_top) |>
      mutate(
        p_top = scales::percent(p_top, accuracy = 1),
        team = fct_reorder(team, mean),
        team_nr = as.numeric(team)
      )
  }

  # League points subtitle varies by sport
  if (scoring$has_ties) {
    points_subtitle <- str_c(
      "Líkindadreifing spár um stigafjölda liða í lok deildar",
      " | ",
      "Líkur á að ná í úrslit innan sviga",
      " | ",
      "Grænar línur eru meðalspár"
    )
  } else {
    points_subtitle <- str_c(
      "Líkindadreifing spár um stigafjölda liða í lok deildar",
      " | ",
      "Grænar línur eru meðalspár"
    )
  }

  plot_dat |>
    ggplot(aes(y = team_nr)) +
    geom_segment(
      aes(
        x = points,
        xend = points,
        y = team_nr,
        yend = team_nr + 0.8 * p
      ),
      linewidth = plots$league_points$linewidth_bar
    ) +
    geom_segment(
      aes(
        x = mean,
        xend = mean,
        y = team_nr,
        yend = team_nr + 0.9
      ),
      linewidth = plots$league_points$linewidth_mean,
      col = "#4daf4a"
    ) +
    scale_x_continuous(
      limits = c(NA, NA),
      expand = expansion(add = c(5, 5)),
      breaks = breaks_width(5),
      guide = guide_axis(cap = "both")
    ) +
    scale_y_continuous(
      guide = guide_axis(cap = "both"),
      breaks = plot_dat |>
        distinct(team, team_nr) |>
        arrange(team_nr) |>
        pull(team) |>
        seq_along(),
      labels = plot_dat |>
        distinct(team, team_nr) |>
        arrange(team_nr) |>
        pull(team),
      expand = expansion()
    ) +
    labs(
      title = if (scoring$has_ties) {
        glue(
          "Spá um stigafjölda í lok {labels$league_name}ar {translate_sex(sex)}"
        )
      } else {
        glue(
          "Hvað munu lið hafa mörg stig í lok {labels$league_name}ar {translate_sex(sex)}?"
        )
      },
      subtitle = points_subtitle,
      caption = "metill.is",
      x = "Stigafjöldi í lok tímabils",
      y = NULL
    )

  ggsave(
    filename = here(
      sport_dir,
      "results",
      sex,
      "figures",
      "deild_points.png"
    ),
    width = 8,
    height = plots$league_points$height_ratio * 8,
    scale = plots$league_points$scale
  )

  #### League Winner (optional) ####

  if (plots$league_winner$show) {
    plot_dat <- posterior_goals |>
      mutate(
        result = case_when(
          home_goals > away_goals ~ "home",
          home_goals < away_goals ~ "away"
        )
      ) |>
      pivot_longer(c(home, away), values_to = "team") |>
      mutate(
        points = case_when(
          result == name ~ scoring$win_points,
          TRUE ~ scoring$loss_points
        )
      ) |>
      summarise(
        points = sum(points),
        .by = c(iteration, team)
      ) |>
      left_join(
        base_points
      ) |>
      mutate(
        base_points = coalesce(base_points, 0),
        points = points + base_points
      ) |>
      arrange(iteration, desc(points)) |>
      mutate(
        placement = row_number(),
        .by = iteration
      ) |>
      count(team, placement) |>
      mutate(
        p_raw = n / sum(n),
        p = p_raw / max(p_raw),
        mean = sum(p_raw * placement),
        .by = team
      ) |>
      inner_join(p_top) |>
      mutate(
        team = fct_reorder(team, mean),
        team_nr = as.numeric(team)
      )

    ncols <- plots$league_winner$ncols[[sex]]

    plot_dat |>
      mutate(
        p = n / sum(n),
        .by = placement
      ) |>
      ggplot(aes(placement, p)) +
      geom_col(
        position = "stack",
        aes(fill = team),
        width = 0.95,
        col = "black",
        linewidth = 0.1
      ) +
      scale_x_continuous(
        guide = guide_axis(cap = "both"),
        breaks = 1:12
      ) +
      scale_y_continuous(
        guide = guide_axis(cap = "both"),
        breaks = breaks_width(0.1),
        labels = label_hlutf()
      ) +
      scale_fill_manual(
        values = config$team_colors
      ) +
      facet_wrap("team", ncol = ncols) +
      theme(
        legend.position = "none"
      ) +
      labs(
        x = NULL,
        y = NULL,
        title = glue(
          "Hve líklegt er að hvert lið lendi í ákveðnu sæti í {labels$league_name} {translate_sex(sex)}?"
        ),
        subtitle = glue(
          "Líkindadreifing yfir lokasæti fengin úr {labels$model_description}"
        )
      )

    ggsave(
      filename = here(
        sport_dir,
        "results",
        sex,
        "figures",
        "deild_top.png"
      ),
      width = 8,
      height = 1 * 8,
      scale = 1.1
    )
  }

  #### Posterior Results ####

  #### Current Strengths (Table) ####

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
    semi_join(cur_teams)

  strengths_tbl <- plot_dat |>
    mutate(
      value = (value - min(value)),
      value = 100 * value / max(value),
      .by = c(.draw, type, loc)
    ) |>
    summarise(
      value = mean(value),
      .by = c(team, type, loc)
    ) |>
    pivot_wider(names_from = type) |>
    mutate(
      team = fct_reorder(team, Samtals)
    ) |>
    arrange(desc(team)) |>
    pivot_wider(
      names_from = loc,
      values_from = c(Samtals:Vörn),
      names_vary = "slowest"
    ) |>
    select(
      team,
      Sókn_Heima,
      Vörn_Heima,
      Samtals_Heima,
      Sókn_Gestir,
      Vörn_Gestir,
      Samtals_Gestir
    )

  gt_tbl <- strengths_tbl |>
    gt() |>
    fmt_number(
      decimals = 0
    ) |>
    cols_label(
      team = "",
      Sókn_Gestir = "Sókn",
      Vörn_Gestir = "Vörn",
      Samtals_Gestir = "Heild",
      Sókn_Heima = "Sókn",
      Vörn_Heima = "Vörn",
      Samtals_Heima = "Heild"
    ) |>
    tab_spanner(
      label = "Heima",
      columns = contains("Heima")
    ) |>
    tab_spanner(
      label = "Gestir",
      columns = contains("Gestir")
    ) |>
    cols_align(
      align = "center",
      columns = -1
    ) |>
    cols_align(
      align = "left",
      columns = 1
    )

  # Apply bar charts - basketball uses contains(), handball uses individual columns
  if (plots$strengths_table$gt_plt_bar_pct_method == "contains") {
    gt_tbl <- gt_tbl |>
      gtExtras::gt_plt_bar_pct(
        column = contains("Sókn"),
        scaled = TRUE,
        labels = TRUE,
        fill = "#08306b",
        background = "white",
        decimals = 0
      ) |>
      gtExtras::gt_plt_bar_pct(
        column = contains("Vörn"),
        scaled = TRUE,
        labels = TRUE,
        fill = "#67000d",
        background = "white",
        decimals = 0
      ) |>
      gtExtras::gt_plt_bar_pct(
        column = contains("Samtals"),
        scaled = TRUE,
        labels = TRUE,
        background = "white",
        fill = "#252525",
        decimals = 0
      )
  } else {
    gt_tbl <- gt_tbl |>
      gtExtras::gt_plt_bar_pct(
        column = "Sókn_Heima",
        scaled = TRUE,
        labels = TRUE,
        fill = "#08306b",
        background = "white",
        decimals = 0
      ) |>
      gtExtras::gt_plt_bar_pct(
        column = "Sókn_Gestir",
        scaled = TRUE,
        labels = TRUE,
        fill = "#08306b",
        background = "white",
        decimals = 0
      ) |>
      gtExtras::gt_plt_bar_pct(
        column = "Vörn_Heima",
        scaled = TRUE,
        labels = TRUE,
        fill = "#67000d",
        background = "white",
        decimals = 0
      ) |>
      gtExtras::gt_plt_bar_pct(
        column = "Vörn_Gestir",
        scaled = TRUE,
        labels = TRUE,
        fill = "#67000d",
        background = "white",
        decimals = 0
      ) |>
      gtExtras::gt_plt_bar_pct(
        column = "Samtals_Heima",
        scaled = TRUE,
        labels = TRUE,
        background = "white",
        fill = "#252525",
        decimals = 0
      ) |>
      gtExtras::gt_plt_bar_pct(
        column = "Samtals_Gestir",
        scaled = TRUE,
        labels = TRUE,
        background = "white",
        fill = "#252525",
        decimals = 0
      )
  }

  gt_tbl |>
    tab_header(
      title = glue(
        "Greining á sóknar-, varnar- og heildarstyrk í {labels$league_name} {translate_sex(sex)}"
      ),
      subtitle = str_c(
        "Einkunn gefin á kvarða þar sem 100% er það besta sem líkanið telur raunhæft hámark",
        " og 0% er raunhæft lágmark"
      )
    ) |>
    gt::cols_width(
      1 ~ px(100),
      -1 ~ px(80)
    ) |>
    gtExtras::gt_add_divider(
      columns = c(1, 4),
      include_labels = FALSE,
      color = "grey60"
    ) |>
    tab_style(
      style = cell_text(align = "center"),
      locations = cells_column_labels(columns = 1:7)
    ) |>
    tab_style(
      style = cell_text(align = "left"),
      locations = cells_title()
    ) |>
    gtsave(
      filename = here(
        sport_dir,
        "results",
        sex,
        "figures",
        "strengths_table.png"
      ),
      expand = c(1, 5, 1, -2)
    )

  #### Current Strengths (Plot) ####

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

  # Filter to current teams - use cur_teams for basketball, div 1 season filter for handball
  if (config$divisions$filter_top_teams) {
    strength_filter <- d |>
      filter(season == max(season), division == 1) |>
      pivot_longer(c(home, away), values_to = "team") |>
      distinct(team)
  } else {
    strength_filter <- cur_teams
  }

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
    semi_join(strength_filter)

  dodge <- 0.3

  legend_text_args <- if (!is.null(plots$strengths_plot$legend_text_size)) {
    element_text(
      size = plots$strengths_plot$legend_text_size,
      family = "Lato",
      colour = "#525252"
    )
  } else {
    element_text(family = "Lato", colour = "#525252")
  }

  plot_dat |>
    ggplot(aes(median, team)) +
    geom_hline(
      yintercept = seq(1, nrow(teams), 2),
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
      legend.text = legend_text_args
    ) +
    labs(
      x = NULL,
      y = NULL,
      colour = NULL,
      title = glue(
        "Styrkur félagsliða í {labels$leagues_full} {translate_sex(sex)}"
      ),
      subtitle = glue("Metið með {labels$model_description}")
    )

  ggsave(
    filename = here(
      sport_dir,
      "results",
      sex,
      "figures",
      "styrkur.png"
    ),
    width = 8,
    height = plots$strengths_plot$height_ratio * 8,
    scale = plots$strengths_plot$scale
  )

  #### Home Advantages ####

  home_adv_dat <- results$draws("home_advantage_tot") |>
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
    semi_join(
      cur_teams
    ) |>
    mutate(
      team = factor(
        team,
        levels = unique(team)[order(unique(median[
          type == "Samanlögð áhrif á heildarstyrk"
        ]))]
      )
    )

  home_adv_dat |>
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
      breaks = plots$home_advantage$breaks
    ) +
    scale_y_discrete(
      guide = guide_axis(cap = "both")
    ) +
    facet_wrap("type") +
    coord_cartesian(
      xlim = plots$home_advantage$xlim
    ) +
    theme(
      legend.position = "none",
      plot.margin = margin(5, 10, 5, 5)
    ) +
    labs(
      x = NULL,
      y = NULL,
      colour = NULL,
      title = glue(
        "Heimavallaráhrif félagsliða í {labels$leagues_full} {translate_sex(sex)}"
      ),
      subtitle = "Skora lið fleiri stig á heimavelli? Skora gestirnir þeirra færri stig?"
    )

  ggsave(
    filename = here(
      sport_dir,
      "results",
      sex,
      "figures",
      "home_advantage.png"
    ),
    width = 8,
    height = plots$home_advantage$height_ratio * 8,
    scale = plots$home_advantage$scale
  )
}
