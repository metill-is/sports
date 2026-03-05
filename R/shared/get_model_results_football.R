#### Packages ####
box::use(
  stats[median, quantile],
  readr[read_rds, read_csv, write_csv, parse_number],
  dplyr[mutate, select, filter, arrange, count, summarise, distinct, pull,
        inner_join, left_join, semi_join, bind_rows, reframe, mutate_at, vars,
        if_else, case_when, row_number, coalesce, join_by],
  tidyr[pivot_longer, pivot_wider],
  stringr[str_detect, str_match, str_c],
  forcats[fct_reorder, fct_relevel, as_factor],
  lubridate[today],
  tibble[as_tibble],
  ggplot2[ggplot, aes, geom_segment, geom_vline, geom_hline, geom_point, geom_col,
          scale_x_continuous, scale_y_continuous, scale_y_discrete,
          scale_colour_manual, scale_colour_brewer, scale_fill_manual,
          scale_alpha_continuous, coord_cartesian, facet_wrap, labs, theme,
          element_text, element_blank, margin, expansion, guide_axis, guide_none,
          sec_axis, ggsave, theme_set],
  scales[percent, breaks_width],
  posterior[as_draws_df],
  metill[theme_metill, label_hlutf],
  ggtext[geom_richtext],
  glue[glue],
  here[here]
)

theme_set(theme_metill())
Sys.setlocale("LC_ALL", "is_IS.UTF-8")

#' Generate model results and visualisations for football leagues
#'
#' @param sex Character string, either "male" or "female"
#' @param from_season Integer, starting season for analysis (default: 2021)
#' @param make_plots Logical, whether to generate plots
#' @param league_labels List with division_names (character vector) and league_name (string)
#' @export
generate_model_results <- function(
  sex = "male",
  from_season = 2021,
  make_plots = TRUE,
  league_labels = list(
    division_names = "PL",
    league_name = "Premier League"
  )
) {
  if (!sex %in% c("male", "female")) {
    stop("Sex must be either 'male' or 'female'")
  }

  division_names <- league_labels$division_names
  league_name <- league_labels$league_name

  #### Data Prep ####
  results <- read_rds(here("results", sex, "fit.rds"))

  d <- read_csv(here("results", sex, "d.csv"))
  teams <- read_csv(here("results", sex, "teams.csv"))
  next_games <- read_csv(here("results", sex, "next_games.csv"))
  top_teams <- read_csv(here("results", sex, "top_teams.csv"))
  pred_d <- read_csv(here("results", sex, "pred_d.csv"))


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
      type = if_else(str_detect(parameter, "goals1"), "home_goals", "away_goals"),
      game_nr = str_match(parameter, "d\\[(.*)\\]$")[, 2] |> as.numeric()
    ) |>
    select(.draw, type, game_nr, value) |>
    pivot_wider(names_from = type, values_from = value) |>
    inner_join(pred_d, by = "game_nr") |>
    filter(
      date < today() + 14 + 1,
      date >= today()
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
    write_csv(
      here("results", sex, "posterior_goals.csv")
    )

  if (!make_plots) return(invisible(NULL))

  predictions <- posterior_goals |>
    mutate(
      goal_diff = away_goals - home_goals,
      .by = c(home, away)
    ) |>
    filter(
      abs(goal_diff) <= 9,
      division %in% 1
    ) |>
    count(date, division, home, away, goal_diff, game_nr) |>
    mutate(
      p = n / sum(n),
      mean_value = sum(goal_diff * p),
      home_win = sum(p[goal_diff < 0]),
      away_win = sum(p[goal_diff > 0]),
      draw = sum(p[goal_diff == 0]),
      p = p / max(p),
      .by = c(home, away)
    ) |>
    mutate(
      game_nr = as.numeric(as.factor(game_nr)),
      game_nr = max(game_nr) - game_nr + 1,
      match = str_c(home, " - ", away) |>
        fct_reorder(game_nr),
      division = division_names[division]
    ) |>
    mutate(
      home = glue(
        "{home} ({percent(home_win, accuracy = 1)}) | {format(date, '%d. %B')} - {division}"
      ),
      away = glue("{away} ({percent(away_win, accuracy = 1)})"),
      .by = c(home, away)
    ) |>
    arrange(desc(game_nr)) |>
    mutate(
      outcome = case_when(
        goal_diff == 0 ~ "Tie",
        goal_diff < 0 ~ "Home win",
        goal_diff > 0 ~ "Away win",
        TRUE ~ "Tie"
      ),
      .by = c(home, away)
    )


  tie_color <- "#252525"
  home_win_color <- "#377eb8"
  away_win_color <- "#e41a1c"

  plot <- predictions |>
    filter(
      abs(goal_diff) <= 8
    ) |>
    ggplot(aes(y = game_nr)) +
    geom_segment(
      aes(
        x = goal_diff,
        xend = goal_diff,
        y = game_nr,
        yend = game_nr + 0.8 * p,
        col = outcome
      ),
      linewidth = 14
    ) +
    geom_segment(
      aes(
        x = mean_value,
        xend = mean_value,
        y = game_nr,
        yend = game_nr + 0.9
      ),
      linewidth = 3,
      col = "#4daf4a"
    ) +
    geom_vline(
      xintercept = 0,
      lty = 2,
      col = "grey50"
    ) +
    geom_richtext(
      data = data.frame(x = 1),
      inherit.aes = FALSE,
      x = -8,
      y = -Inf,
      label.colour = NA,
      fill = NA,
      label = "&larr; Heimalið vinnur",
      hjust = 0,
      vjust = 0,
      size = 4.5,
      colour = "grey40"
    ) +
    geom_richtext(
      data = data.frame(x = 1),
      inherit.aes = FALSE,
      x = 8,
      y = -Inf,
      label.colour = NA,
      fill = NA,
      label = "Gestir vinna &rarr;",
      hjust = 1,
      vjust = 0,
      size = 4.5,
      colour = "grey40"
    ) +
    scale_x_continuous(
      guide = guide_axis(cap = "both"),
      breaks = seq(-8, 8),
      labels = function(x) abs(x),
      expand = expansion(add = 1),
      limits = c(-8, 8),
    ) +
    scale_y_continuous(
      guide = guide_axis(cap = "both"),
      breaks = seq(max(predictions$game_nr), 1),
      labels = predictions |>
        distinct(game_nr, home, away, match) |>
        arrange(desc(game_nr)) |>
        pull(home),
      expand = expansion(),
      sec.axis = sec_axis(
        transform = function(x) x,
        breaks = seq(max(predictions$game_nr), 1),
        labels = predictions |>
          distinct(game_nr, home, away, match) |>
          arrange(desc(game_nr)) |>
          pull(away),
        guide = guide_axis(cap = "both")
      )
    ) +
    scale_colour_manual(
      values = c(
        "Tie" = tie_color,
        "Home win" = home_win_color,
        "Away win" = away_win_color
      ),
      guide = "none"
    ) +
    coord_cartesian(
      ylim = c(0.5, max(predictions$game_nr) + 0.9),
      clip = "off"
    ) +
    theme(
      axis.text.y = element_text(
        face = "bold"
      )
    ) +
    labs(
      x = "Markamismunur",
      y = NULL,
      colour = NULL,
      title = glue("Vikuleg fótboltaspá Metils fyrir {league_name}"),
      subtitle = str_c(
        "Líkindadreifing spár fyrir komandi leiki",
        " | ",
        "Sigurlíkur merktar innan sviga",
        " | ",
        "Líkur á jafntefli eru 100% mínus líkur hvors liðs",
        " | ",
        "\nGrænar línur eru meðalspár"
      ),
      caption = "metill.is"
    )


  n_games <- predictions |>
    distinct(game_nr) |>
    pull(game_nr) |>
    length()

  ratio <- 0.6 + 0.3 * n_games / 10

  ggsave(
    plot = plot,
    filename = here(
      "results",
      sex,
      "figures",
      "next_week_preds.png"
    ),
    width = 8,
    height = ratio * 8,
    scale = 1.7
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
      type = if_else(str_detect(parameter, "goals1"), "home_goals", "away_goals"),
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

  posterior_goals |>
    pivot_longer(c(home, away)) |>
    count(iteration, value, sort = TRUE)

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
        result == "tie" ~ 1,
        result == name ~ 3,
        TRUE ~ 0
      )
    ) |>
    summarise(
      base_points = sum(points),
      .by = c(team)
    ) |>
    arrange(desc(base_points))

  p_top <- posterior_goals |>
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
      p_top = mean(position <= 3),
      .by = team
    ) |>
    arrange(desc(p_top))

  plot_dat <- posterior_goals |>
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


  plot_dat |>
    ggplot(aes(y = team_nr)) +
    geom_segment(
      aes(
        x = points,
        xend = points,
        y = team_nr,
        yend = team_nr + 0.8 * p
      ),
      linewidth = 5
    ) +
    geom_segment(
      aes(
        x = mean,
        xend = mean,
        y = team_nr,
        yend = team_nr + 0.9
      ),
      linewidth = 2,
      col = "#4daf4a"
    ) +
    scale_x_continuous(
      limits = c(0, NA),
      expand = expansion(add = c(0, 7)),
      breaks = breaks_width(10),
      guide = guide_axis(cap = "both")
    ) +
    scale_y_continuous(
      guide = guide_axis(cap = "both"),
      breaks = seq(1, 20),
      labels = plot_dat |>
        distinct(team, team_nr) |>
        arrange(team_nr) |>
        pull(team),
      expand = expansion()
    ) +
    labs(
      title = glue(
        "Líkindadreifing yfir stigafjölda liða í {league_name} undir lok tímabils"
      ),
      subtitle = str_c(
        "Líkindadreifing spár um stigafjölda liða í lok deildar",
        " | ",
        "Líkur á að vera meðal þriggja efstu innan sviga",
        " | ",
        "Grænar línur eru meðalspár"
      ),
      caption = "metill.is",
      x = "Stigafjöldi í lok tímabils",
      y = NULL
    )

  ggsave(
    filename = here("results", sex, "figures", "umspil_top.png"),
    width = 8,
    height = 0.6 * 8,
    scale = 1.6
  )

  #### League Winner ####

  plot_dat <- posterior_goals |>
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



  plot_dat |>
    mutate(
      p = n / sum(n),
      .by = placement
    ) |>
    ggplot(aes(placement, p)) +
    geom_col(
      position = "stack",
      width = 0.95,
      col = "black",
      linewidth = 0.1
    ) +
    scale_x_continuous(
      guide = guide_axis(cap = "both"),
      breaks = c(1, 5, 10, 15, 20)
    ) +
    scale_y_continuous(
      guide = guide_axis(cap = "both"),
      breaks = breaks_width(0.25),
      labels = label_hlutf()
    ) +
    facet_wrap("team", ncol = 4) +
    theme(
      legend.position = "none"
    ) +
    labs(
      x = NULL,
      y = NULL,
      title = glue("Líkindadreifing yfir lokasæti í {league_name}")
    )

  ggsave(
    filename = here("results", sex, "figures", "deild_top.png"),
    width = 8,
    height = 0.6 * 8,
    scale = 1.2
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
        filter(season == max(season), division == 1) |>
        pivot_longer(c(home, away), values_to = "team") |>
        distinct(team)
    )

  dodge <- 0.3

  plot_dat |>
    semi_join(
      next_games |>
        filter(division == 1) |>
        distinct(home, away) |>
        pivot_longer(c(everything()), values_to = "team") |>
        distinct(team)
    ) |>
    ggplot(aes(median, team)) +
    geom_hline(
      yintercept = seq(1, 20, 2),
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
      legend.position = c(0.5, 1.078),
      legend.background = element_blank(),
      legend.direction = "horizontal",
      legend.text = element_text(family = "Lato", colour = "#525252")
    ) +
    labs(
      x = NULL,
      y = NULL,
      colour = NULL,
      title = glue("Styrkur félagsliða í {league_name}"),
      subtitle = "Metið með fótboltalíkani Metils"
    )

  ggsave(
    filename = here("results", sex, "figures", "styrkur.png"),
    width = 8,
    height = 0.6 * 8,
    scale = 1.1
  )


  #### Home Advantages ####

  results$draws("home_advantage_tot") |>
    as_draws_df() |>
    as_tibble() |>
    pivot_longer(c(-.chain, -.draw, -.iteration)) |>
    mutate(
      team_nr = name |> parse_number(),
      type = "Samanlögð áhrif á heildarstyrk",
      value = value / 2
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
      d |>
        filter(season == max(season), division == 1) |>
        select(home, away) |>
        pivot_longer(c(everything()), values_to = "team") |>
        distinct(team)
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
    mutate_at(
      vars(median, lower, upper),
      exp
    ) |>
    ggplot(aes(median, team)) +
    geom_vline(
      xintercept = 1,
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
      breaks = c(1, 1.25, 1.5, 1.75, 2, 2.5),
      labels = \(x) paste0("+", label_hlutf(accuracy = 1)(x - 1)),
    ) +
    scale_y_discrete(
      guide = guide_axis(cap = "both")
    ) +
    facet_wrap("type") +
    coord_cartesian(
      xlim = c(1, 2)
    ) +
    theme(
      legend.position = "none",
      plot.margin = margin(5, 10, 5, 5)
    ) +
    labs(
      x = NULL,
      y = NULL,
      colour = NULL,
      title = glue("Heimavallaráhrif í {league_name}"),
      subtitle = "Hlutfallsleg áhrif heimavallar á sóknar- varnar- og heildarstyrk félagsliða"
    )

  ggsave(
    filename = here(
      "results",
      sex,
      "figures",
      "home_advantage.png"
    ),
    width = 8,
    height = 0.35 * 8,
    scale = 1.4
  )

}
