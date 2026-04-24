#### Model Comparison Plots ####
#
# Visual comparison of Poisson vs Student-t model outputs:
# 1. Team strength rankings (do they agree on who's good?)
# 2. Expected goals scatter (E[home] vs E[away] per match)
# 3. Score distribution marginals (aggregate posterior histograms)
# 4. Match-level posterior predictive (select matches, overlay distributions)
# 5. Draw probability comparison

library(tidyverse)
library(patchwork)
library(scales)
library(posterior)
library(metill)
theme_set(theme_metill())

model_colours <- c(poisson = "#2c5aa0", student_t = "#c44e52")
model_labels <- c(poisson = "Poisson (tvíþætt)", student_t = "Student-t (S,D)")

plot_model_comparison <- function(posteriors, fits, metrics, test_actuals, teams, out_dir) {

  #### Panel 1: Team Strength Rankings ####
  # Extract cur_strength from both models
  strength_data <- map_dfr(names(fits), \(nm) {
    fit <- fits[[nm]]
    draws <- as_draws_df(fit$draws("cur_strength"))
    draws |>
      as_tibble() |>
      pivot_longer(
        starts_with("cur_strength"),
        names_to = "param",
        values_to = "strength"
      ) |>
      mutate(
        team_nr = str_match(param, "\\[(.*)\\]")[, 2] |> as.integer(),
        model = nm
      ) |>
      inner_join(teams, by = "team_nr") |>
      summarise(
        str_mean = mean(strength),
        q10 = quantile(strength, 0.1),
        q90 = quantile(strength, 0.9),
        .by = c(model, team, team_nr)
      )
  })

  # Pick top 20 teams by average strength across models
  top_teams <- strength_data |>
    summarise(avg = mean(str_mean), .by = team) |>
    slice_max(avg, n = 20) |>
    pull(team)

  p1 <- strength_data |>
    filter(team %in% top_teams) |>
    mutate(team = fct_reorder(team, str_mean)) |>
    ggplot(aes(y = team, x = str_mean, colour = model)) +
    geom_pointrange(
      aes(xmin = q10, xmax = q90),
      position = position_dodge(width = 0.5),
      size = 0.4
    ) +
    scale_colour_manual(values = model_colours, labels = model_labels) +
    labs(
      title = "Styrkur liða (efstu 20)",
      subtitle = "80% trúnaðarbil",
      x = "Heildarstyrkur (off + def)",
      y = NULL,
      colour = NULL
    ) +
    theme(legend.position = "bottom")

  ggsave(
    file.path(out_dir, "team_strength_comparison.png"),
    p1, width = 8, height = 8, dpi = 150, bg = "white"
  )
  cat("  Saved team_strength_comparison.png\n")

  #### Panel 2: Expected Goals Scatter ####
  # Per-match expected home/away goals from each model
  eg_data <- map_dfr(names(posteriors), \(nm) {
    posteriors[[nm]] |>
      summarise(
        e_home = mean(home_goals),
        e_away = mean(away_goals),
        .by = c(game_nr, date, home, away)
      ) |>
      mutate(model = nm)
  })

  eg_wide <- eg_data |>
    pivot_wider(
      id_cols = c(game_nr, date, home, away),
      names_from = model,
      values_from = c(e_home, e_away)
    )

  p2a <- eg_wide |>
    ggplot(aes(e_home_poisson, e_home_student_t)) +
    geom_abline(intercept = 0, slope = 1, lty = 2, alpha = 0.5) +
    geom_point(alpha = 0.5, size = 1.5) +
    coord_equal() +
    labs(
      title = "E[heimamark]",
      x = "Poisson",
      y = "Student-t"
    )

  p2b <- eg_wide |>
    ggplot(aes(e_away_poisson, e_away_student_t)) +
    geom_abline(intercept = 0, slope = 1, lty = 2, alpha = 0.5) +
    geom_point(alpha = 0.5, size = 1.5) +
    coord_equal() +
    labs(
      title = "E[gestimark]",
      x = "Poisson",
      y = "Student-t"
    )

  p2 <- (p2a | p2b) +
    plot_annotation(
      title = "Samanburður á meðaltalsspám milli líkana",
      subtitle = "Hvert stig = einn leikur í prófunargögnum"
    )

  ggsave(
    file.path(out_dir, "expected_goals_scatter.png"),
    p2, width = 10, height = 5, dpi = 150, bg = "white"
  )
  cat("  Saved expected_goals_scatter.png\n")

  #### Panel 3: Score Distribution Marginals ####
  marginals <- map_dfr(names(posteriors), \(nm) {
    posteriors[[nm]] |>
      mutate(
        total_goals = home_goals + away_goals,
        goal_diff = home_goals - away_goals,
        model = nm
      )
  })

  # Home goals marginal
  p3a <- marginals |>
    mutate(home_goals = pmin(home_goals, 8)) |>
    count(model, home_goals) |>
    mutate(prop = n / sum(n), .by = model) |>
    ggplot(aes(home_goals, prop, fill = model)) +
    geom_col(position = "dodge", alpha = 0.7) +
    scale_fill_manual(values = model_colours, labels = model_labels) +
    scale_x_continuous(breaks = 0:8, labels = c(0:7, "8+")) +
    scale_y_continuous(labels = label_percent()) +
    labs(title = "Heimamark", x = NULL, y = "Hlutfall", fill = NULL) +
    theme(legend.position = "none")

  # Away goals marginal
  p3b <- marginals |>
    mutate(away_goals = pmin(away_goals, 8)) |>
    count(model, away_goals) |>
    mutate(prop = n / sum(n), .by = model) |>
    ggplot(aes(away_goals, prop, fill = model)) +
    geom_col(position = "dodge", alpha = 0.7) +
    scale_fill_manual(values = model_colours, labels = model_labels) +
    scale_x_continuous(breaks = 0:8, labels = c(0:7, "8+")) +
    scale_y_continuous(labels = label_percent()) +
    labs(title = "Gestimark", x = NULL, y = NULL, fill = NULL) +
    theme(legend.position = "none")

  # Total goals marginal
  p3c <- marginals |>
    mutate(total_goals = pmin(total_goals, 10)) |>
    count(model, total_goals) |>
    mutate(prop = n / sum(n), .by = model) |>
    ggplot(aes(total_goals, prop, fill = model)) +
    geom_col(position = "dodge", alpha = 0.7) +
    scale_fill_manual(values = model_colours, labels = model_labels) +
    scale_x_continuous(breaks = 0:10, labels = c(0:9, "10+")) +
    scale_y_continuous(labels = label_percent()) +
    labs(title = "Heildarmark", x = NULL, y = NULL, fill = NULL) +
    theme(legend.position = "none")

  # Goal difference marginal
  p3d <- marginals |>
    mutate(goal_diff = pmax(pmin(goal_diff, 6), -6)) |>
    count(model, goal_diff) |>
    mutate(prop = n / sum(n), .by = model) |>
    ggplot(aes(goal_diff, prop, fill = model)) +
    geom_col(position = "dodge", alpha = 0.7) +
    scale_fill_manual(values = model_colours, labels = model_labels) +
    scale_y_continuous(labels = label_percent()) +
    labs(title = "Markamismunur", x = NULL, y = NULL, fill = NULL) +
    theme(legend.position = "bottom")

  p3 <- (p3a | p3b | p3c | p3d) +
    plot_annotation(
      title = "Jaðardreifingar markaspáa — heildarsamanburður",
      subtitle = "Öll prófunarleikjagögn saman"
    )

  ggsave(
    file.path(out_dir, "score_marginals.png"),
    p3, width = 14, height = 5, dpi = 150, bg = "white"
  )
  cat("  Saved score_marginals.png\n")

  #### Panel 4: Match-Level Posterior Predictive ####
  # Pick a diverse set of matches: biggest upset, highest-scoring, and a typical draw
  actuals_summary <- test_actuals |>
    mutate(
      total = home_goals + away_goals,
      diff = home_goals - away_goals,
      outcome = case_when(
        home_goals > away_goals ~ "home",
        home_goals < away_goals ~ "away",
        TRUE ~ "draw"
      )
    )

  # Select interesting matches
  pick_games <- c(
    actuals_summary |> slice_max(total, n = 1, with_ties = FALSE) |> pull(game_nr),
    actuals_summary |> filter(outcome == "draw") |> slice_sample(n = 1) |> pull(game_nr),
    actuals_summary |> slice_min(abs(diff - 1), n = 1, with_ties = FALSE) |> pull(game_nr),
    actuals_summary |> slice_max(abs(diff), n = 1, with_ties = FALSE) |> pull(game_nr)
  ) |> unique()
  pick_games <- head(pick_games, 4)

  if (length(pick_games) >= 2) {
    match_post <- map_dfr(names(posteriors), \(nm) {
      posteriors[[nm]] |>
        filter(game_nr %in% pick_games) |>
        mutate(
          goal_diff = home_goals - away_goals,
          model = nm
        )
    })

    match_labels <- actuals_summary |>
      filter(game_nr %in% pick_games) |>
      mutate(
        label = paste0(home, " ", home_goals, "-", away_goals, " ", away)
      ) |>
      select(game_nr, label)

    p4 <- match_post |>
      inner_join(match_labels, by = "game_nr") |>
      ggplot(aes(goal_diff, fill = model)) +
      geom_histogram(
        aes(y = after_stat(density)),
        bins = 15, alpha = 0.5, position = "identity"
      ) +
      geom_vline(
        aes(xintercept = diff),
        data = actuals_summary |>
          filter(game_nr %in% pick_games) |>
          inner_join(match_labels, by = "game_nr"),
        lty = 2, linewidth = 0.8
      ) +
      facet_wrap(~label, scales = "free_y") +
      scale_fill_manual(values = model_colours, labels = model_labels) +
      labs(
        title = "Markamismunur — dreifing á eftir líkani",
        subtitle = "Brotalína = raunveruleg útkoma",
        x = "Markamismunur (heima - gestir)",
        y = "Þéttni",
        fill = NULL
      ) +
      theme(legend.position = "bottom")

    ggsave(
      file.path(out_dir, "match_posteriors.png"),
      p4, width = 10, height = 7, dpi = 150, bg = "white"
    )
    cat("  Saved match_posteriors.png\n")
  }

  #### Panel 5: Draw Probability Comparison ####
  draw_compare <- map_dfr(names(metrics), \(nm) {
    metrics[[nm]]$outcome_probs |>
      select(game_nr, date, home, away, p_draw, actual_outcome) |>
      mutate(model = nm)
  }) |>
    pivot_wider(
      id_cols = c(game_nr, date, home, away, actual_outcome),
      names_from = model,
      values_from = p_draw
    ) |>
    mutate(is_draw = actual_outcome == "draw")

  p5 <- draw_compare |>
    ggplot(aes(poisson, student_t, colour = is_draw)) +
    geom_abline(intercept = 0, slope = 1, lty = 2, alpha = 0.5) +
    geom_point(alpha = 0.6, size = 2) +
    scale_colour_manual(
      values = c("FALSE" = "grey60", "TRUE" = "#e6a532"),
      labels = c("FALSE" = "Ekki jafntefli", "TRUE" = "Jafntefli")
    ) +
    scale_x_continuous(labels = label_percent()) +
    scale_y_continuous(labels = label_percent()) +
    coord_equal() +
    labs(
      title = "Jafnteflislíkindi — Poisson vs Student-t",
      subtitle = "Poisson hefur sértæka jafnteflismódelun (diagonal inflation)",
      x = "Poisson P(jafntefli)",
      y = "Student-t P(jafntefli)",
      colour = NULL
    ) +
    theme(legend.position = "bottom")

  ggsave(
    file.path(out_dir, "draw_probability_comparison.png"),
    p5, width = 7, height = 7, dpi = 150, bg = "white"
  )
  cat("  Saved draw_probability_comparison.png\n")

  #### Panel 6: Tail Behaviour ####
  # Compare how often each model predicts extreme outcomes
  tail_data <- map_dfr(names(posteriors), \(nm) {
    posteriors[[nm]] |>
      mutate(
        total_goals = home_goals + away_goals,
        model = nm
      ) |>
      summarise(
        p_0goals = mean(total_goals == 0),
        p_6plus = mean(total_goals >= 6),
        p_diff_4plus = mean(abs(home_goals - away_goals) >= 4),
        .by = c(model, game_nr)
      )
  })

  p6a <- tail_data |>
    ggplot(aes(p_6plus, fill = model)) +
    geom_histogram(bins = 25, alpha = 0.5, position = "identity") +
    scale_fill_manual(values = model_colours, labels = model_labels) +
    scale_x_continuous(labels = label_percent()) +
    labs(
      title = "P(heildarmark >= 6)",
      x = "Líkindi", y = "Fjöldi leikja", fill = NULL
    ) +
    theme(legend.position = "none")

  p6b <- tail_data |>
    ggplot(aes(p_diff_4plus, fill = model)) +
    geom_histogram(bins = 25, alpha = 0.5, position = "identity") +
    scale_fill_manual(values = model_colours, labels = model_labels) +
    scale_x_continuous(labels = label_percent()) +
    labs(
      title = "P(|markamismunur| >= 4)",
      x = "Líkindi", y = NULL, fill = NULL
    ) +
    theme(legend.position = "bottom")

  p6 <- (p6a | p6b) +
    plot_annotation(
      title = "Halasviðsmyndir — Student-t ætti að hafa þyngri hala",
      subtitle = "Dreifing yfir prófunarleiki"
    )

  ggsave(
    file.path(out_dir, "tail_behaviour.png"),
    p6, width = 10, height = 5, dpi = 150, bg = "white"
  )
  cat("  Saved tail_behaviour.png\n")
}
