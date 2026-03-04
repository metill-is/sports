#' @export
get_config <- function() {
  list(
    sport = "basketball",
    sport_dir = "basketball/iceland",
    stan_model = "2d_student_t.stan",

    columns = list(
      data = c(
        season = "timabil",
        date = "dags",
        home = "heima",
        away = "gestir",
        home_goals = "stig_heima",
        away_goals = "stig_gestir"
      ),
      schedule = c(
        date = "dags",
        home = "heima",
        away = "gestir"
      )
    ),

    divisions = list(
      filter_top_teams = FALSE,
      filter_next_games = FALSE,
      schedule_filter = NULL
    ),

    scoring = list(
      has_ties = FALSE,
      win_points = 2,
      tie_points = 0,
      loss_points = 0,
      tie_threshold = 0.5
    ),

    labels = list(
      prediction_name = "Körfuboltaspá",
      league_name = "Bónusdeild",
      leagues_full = "Bónusdeild (BD) og fyrstu deild (1D)",
      model_description = "körfuboltalíkani Metils",
      model_description_cap = "Körfuboltalíkan Metils",
      goal_diff_label = "Stigamismunur",
      division_labels = c("BD", "1D")
    ),

    plots = list(
      next_round = list(
        xlim = c(-50, 50),
        scale = 1.4,
        annotation_y = -0.8,
        show_division = TRUE
      ),
      league_points = list(
        height_ratio = 1,
        scale = 1.1,
        linewidth_bar = 7,
        linewidth_mean = 3
      ),
      league_winner = list(
        show = TRUE,
        top_n = 3,
        ncols = c(male = 4, female = 5)
      ),
      strengths_table = list(
        gt_plt_bar_pct_method = "contains"
      ),
      strengths_plot = list(
        height_ratio = 0.5,
        scale = 1.6,
        legend_text_size = 14
      ),
      home_advantage = list(
        xlim = c(0, 15),
        breaks = seq(0, 15, by = 2),
        height_ratio = 0.6,
        scale = 1.4
      )
    ),

    team_colors = c(
      "Stjarnan" = "#08519c",
      "Tindastóll" = "#08306b",
      "Njarðvík" = "#006d2c",
      "Grindavík" = "#fec44f",
      "Valur" = "#ce1256",
      "Álftanes" = "#54278f",
      "KR" = "white",
      "Keflavík" = "#4292c6",
      "Þór Þ." = "#d9d9d9",
      "ÍR" = "#08306b",
      "ÍA" = "#fec44f",
      "Ármann" = "#e31a1c",
      "Haukar" = "#cb181d",
      "Hamar/Þór" = "#02818a"
    )
  )
}
