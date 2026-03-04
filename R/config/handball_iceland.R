box::use(
  dplyr[filter]
)

#' @export
get_config <- function() {
  list(
    sport = "handball",
    sport_dir = "handball/iceland",
    stan_model = "2d_student_t.stan",

    columns = list(
      data = c(
        date = "dagsetning"
      ),
      schedule = c(
        date = "dagsetning"
      )
    ),

    divisions = list(
      filter_top_teams = TRUE,
      filter_next_games = TRUE,
      schedule_filter = function(df, end_date) {
        filter(df, (date <= end_date + 14) | (division == 1))
      }
    ),

    scoring = list(
      has_ties = TRUE,
      win_points = 2,
      tie_points = 1,
      loss_points = 0,
      tie_threshold = 0.5
    ),

    labels = list(
      prediction_name = "Handboltaspá",
      league_name = "Olís deild",
      leagues_full = "Olís og Grill 66 deildir",
      model_description = "handboltalíkani Metils",
      model_description_cap = "Handboltalíkan Metils",
      goal_diff_label = "Markamismunur",
      division_labels = c("OD", "G66")
    ),

    plots = list(
      next_round = list(
        xlim = c(-25, 25),
        scale = 1.2,
        annotation_y = -0.55,
        show_division = FALSE
      ),
      league_points = list(
        height_ratio = 0.5,
        scale = 1.6,
        linewidth_bar = 5,
        linewidth_mean = 2
      ),
      league_winner = list(
        show = FALSE,
        top_n = 8,
        ncols = c(male = 4, female = 5)
      ),
      strengths_table = list(
        gt_plt_bar_pct_method = "individual"
      ),
      strengths_plot = list(
        height_ratio = 0.7,
        scale = 1.1,
        legend_text_size = NULL
      ),
      home_advantage = list(
        xlim = c(0, 12),
        breaks = seq(0, 10, by = 2),
        height_ratio = 0.6,
        scale = 1.4
      )
    ),

    team_colors = c()
  )
}
