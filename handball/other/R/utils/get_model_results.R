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
generate_model_results <- function(
  country,
  sex = "male",
  end_date = Sys.Date(),
  make_plots = TRUE
) {
  country_isl <- function(country) {
    case_when(
      country == "austria" ~ "austurísku",
      country == "czech-republic" ~ "tékknesku",
      country == "denmark" ~ "dönsku",
      country == "finland" ~ "finnsku",
      country == "france" ~ "frönsku",
      country == "germany" ~ "þýsku",
      country == "hungary" ~ "ungversku",
      country == "norway" ~ "norsku",
      country == "poland" ~ "pólsku",
      country == "portugal" ~ "portugölsku",
      country == "spain" ~ "spænsku",
      country == "sweden" ~ "sænsku"
    )
  }

  sex_isl <- function(sex) {
    if_else(
      sex == "male",
      "karla",
      "kvenna"
    )
  }

  #### Data Prep ####
  results <- read_rds(here("results", country, sex, "fit.rds"))

  d <- read_csv(here("results", country, sex, "d.csv"))
  teams <- read_csv(here("results", country, sex, "teams.csv"))
  next_games <- read_csv(here(
    "results",
    country,
    sex,
    "next_games.csv"
  ))
  top_teams <- read_csv(here(
    "results",
    country,
    sex,
    "top_teams.csv"
  ))
  pred_d <- read_csv(here("results", country, sex, "pred_d.csv"))

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
      date >= end_date
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
    write_csv(here("results", country, sex, "posterior_goals.csv"))

  if (!make_plots) return(invisible(NULL))

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
    ) |>
    filter(
      date <= end_date + 7
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
      xlim = c(-25, 25),
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
      title = glue(
        "Handboltaspá Metils fyrir {country_isl(country)} deild {sex_isl(sex)}"
      ),
      subtitle = str_c(
        "Líkindadreifing spár um úrslit næstu leikja",
        " | ",
        "Sigurlíkur merktar inni í sviga"
      )
    )

  ggsave(
    filename = here(
      "results",
      country,
      sex,
      "figures",
      "next_round_predictions.png"
    ),
    width = 8,
    height = 0.8 * 8,
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
        filter(season == 2025, division == 1) |>
        pivot_longer(c(home, away), values_to = "team") |>
        distinct(team)
    )

  plot_dat |>
    write_csv(here("results", country, sex, "current_strengths.csv"))

  dodge <- 0.3

  plot_dat |>
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
      title = glue(
        "Styrkur félagsliða í {country_isl(country)} deild {sex_isl(sex)}"
      ),
      subtitle = "Metið með handboltalíkani Metils"
    )

  ggsave(
    filename = here(
      "results",
      country,
      sex,
      "figures",
      "styrkur.png"
    ),
    width = 8,
    height = 0.7 * 8,
    scale = 1.1
  )

  plot_dat |>
    filter(
      loc == "Gestir"
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
    tab_header(
      title = glue(
        "Samantekt á sóknar- og varnarstyrk félagsliða í {country_isl(country)} deild {sex_isl(sex)}"
      ),
      subtitle = "Til að auðvelda túlkun er liðunum borið saman við veikasta liðið"
    ) |>
    tab_options(
      table.background.color = "#fdfcfc"
    ) |>
    gtsave(
      filename = here(
        "results",
        country,
        sex,
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
      top_teams,
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
      title = glue(
        "Heimavallaráhrif félagsliða í {country_isl(country)} deild {sex_isl(sex)}"
      ),
      subtitle = "Skora lið fleiri mörk á heimavelli? Skora gestirnir þeirra færri mörk?"
    )

  ggsave(
    filename = here(
      "results",
      country,
      sex,
      "figures",
      "home_advantage.png"
    ),
    width = 8,
    height = 0.6 * 8,
    scale = 1.4
  )

  file.remove(here("results", country, sex, "fit.rds"))
}
