#### Backtest Diagnostic Plots ####
#
# Six-panel comparison of two football models.
# Called from backtest_football.R with pre-computed metrics.

library(tidyverse)
library(patchwork)
library(scales)
library(metill)
theme_set(theme_metill())

model_colours <- c(poisson = "#2c5aa0", student_t = "#c44e52")
model_labels <- c(poisson = "Poisson (tvíþætt)", student_t = "Student-t (tvívíð)")

plot_backtest <- function(metrics, out_dir) {

  #### Panel 1: Calibration (1x2) ####
  ntiles <- 10

  cal_data <- map_dfr(metrics, \(m) {
    m$outcome_probs |>
      select(game_nr, p_home, p_draw, p_away, actual_outcome) |>
      pivot_longer(
        c(p_home, p_draw, p_away),
        names_to = "outcome_type",
        values_to = "p"
      ) |>
      mutate(
        outcome_type = str_remove(outcome_type, "^p_"),
        correct = as.integer(actual_outcome == outcome_type),
        model = m$model
      )
  })

  p1 <- cal_data |>
    mutate(bin = floor(p * ntiles) / ntiles) |>
    summarise(
      pred = mean(p),
      obs = mean(correct),
      n = n(),
      .by = c(model, bin)
    ) |>
    ggplot(aes(pred, obs, colour = model)) +
    geom_abline(intercept = 0, slope = 1, lty = 2, alpha = 0.5) +
    geom_point(aes(size = n), alpha = 0.7) +
    geom_line(alpha = 0.5) +
    scale_colour_manual(values = model_colours, labels = model_labels) +
    scale_size_continuous(range = c(1, 4), guide = "none") +
    scale_x_continuous(labels = label_percent(), limits = c(0, 1)) +
    scale_y_continuous(labels = label_percent(), limits = c(0, 1)) +
    labs(
      title = "Kvörðun (1x2)",
      x = "Spáð líkindi",
      y = "Raunverulegt hlutfall",
      colour = NULL
    ) +
    theme(legend.position = "bottom")

  #### Panel 2: PIT histogram — goal difference ####
  pit_data <- map_dfr(metrics, \(m) {
    m$pit |> mutate(model = m$model)
  })

  p2 <- pit_data |>
    ggplot(aes(pit_diff, fill = model)) +
    geom_histogram(
      bins = 10, alpha = 0.5, position = "identity",
      boundary = 0
    ) +
    geom_hline(
      aes(yintercept = y),
      data = tibble(y = nrow(metrics[[1]]$pit) / 10),
      lty = 2, alpha = 0.5
    ) +
    scale_fill_manual(values = model_colours, labels = model_labels) +
    labs(
      title = "PIT — markamismunur",
      x = "PIT gildi",
      y = "Fjöldi",
      fill = NULL
    ) +
    theme(legend.position = "none")

  #### Panel 3: PIT histogram — total goals ####
  p3 <- pit_data |>
    ggplot(aes(pit_total, fill = model)) +
    geom_histogram(
      bins = 10, alpha = 0.5, position = "identity",
      boundary = 0
    ) +
    geom_hline(
      aes(yintercept = y),
      data = tibble(y = nrow(metrics[[1]]$pit) / 10),
      lty = 2, alpha = 0.5
    ) +
    scale_fill_manual(values = model_colours, labels = model_labels) +
    labs(
      title = "PIT — heildarmarkafjöldi",
      x = "PIT gildi",
      y = "Fjöldi",
      fill = NULL
    ) +
    theme(legend.position = "none")

  #### Panel 4: Coverage plot ####
  cov_data <- map_dfr(metrics, \(m) {
    m$coverage |> mutate(model = m$model)
  })

  p4 <- cov_data |>
    ggplot(aes(nominal, empirical, colour = model)) +
    geom_abline(intercept = 0, slope = 1, lty = 2, alpha = 0.5) +
    geom_point(size = 3) +
    geom_line() +
    scale_colour_manual(values = model_colours, labels = model_labels) +
    scale_x_continuous(labels = label_percent(), limits = c(0, 1)) +
    scale_y_continuous(labels = label_percent(), limits = c(0, 1)) +
    labs(
      title = "Þekjubil — markamismunur",
      x = "Nafnverð þekja",
      y = "Raunveruleg þekja",
      colour = NULL
    ) +
    theme(legend.position = "none")

  #### Panel 5: Cumulative log-score ####
  logscore_data <- map_dfr(metrics, \(m) {
    m$outcome_probs |>
      arrange(date, game_nr) |>
      mutate(
        cum_log_score = cumsum(log_score),
        match_idx = row_number(),
        model = m$model
      )
  })

  p5 <- logscore_data |>
    ggplot(aes(match_idx, cum_log_score, colour = model)) +
    geom_line(alpha = 0.8) +
    scale_colour_manual(values = model_colours, labels = model_labels) +
    labs(
      title = "Uppsöfnuð log-skor",
      x = "Leikur (í tímaröð)",
      y = "Uppsöfnuð log-skor",
      colour = NULL
    ) +
    theme(legend.position = "none")

  #### Panel 6: Draw rate by strength gap ####
  draw_data <- map_dfr(metrics, \(m) {
    m$outcome_probs |>
      mutate(
        strength_gap = abs(p_home - p_away),
        is_draw = as.integer(actual_outcome == "draw"),
        model = m$model
      )
  })

  p6 <- draw_data |>
    mutate(
      gap_bin = ntile(strength_gap, 8),
      .by = model
    ) |>
    summarise(
      mean_gap = mean(strength_gap),
      pred_draw = mean(p_draw),
      obs_draw = mean(is_draw),
      .by = c(model, gap_bin)
    ) |>
    pivot_longer(
      c(pred_draw, obs_draw),
      names_to = "type",
      values_to = "rate"
    ) |>
    ggplot(aes(mean_gap, rate, colour = model, linetype = type)) +
    geom_line() +
    geom_point(size = 2) +
    scale_colour_manual(values = model_colours, labels = model_labels) +
    scale_linetype_manual(
      values = c(pred_draw = "solid", obs_draw = "dashed"),
      labels = c(pred_draw = "Spáð", obs_draw = "Raunverulegt")
    ) +
    scale_y_continuous(labels = label_percent()) +
    labs(
      title = "Jafnteflishlutfall eftir styrkjamun",
      x = "|P(heima) - P(gestir)|",
      y = "Hlutfall jafntefla",
      colour = NULL,
      linetype = NULL
    ) +
    theme(legend.position = "bottom")

  #### Compose ####
  combined <- (p1 + p2 + p3) / (p4 + p5 + p6) +
    plot_annotation(
      title = "Samanburður fótboltalíkana — Poisson vs Student-t",
      subtitle = "Prófunargögn (síðustu 40% leikja)"
    )

  ggsave(
    file.path(out_dir, "backtest_comparison.png"),
    combined,
    width = 14,
    height = 9,
    dpi = 150,
    bg = "white"
  )

  cat("  Saved backtest_comparison.png\n")

  # Also save individual panels for detail inspection
  ggsave(
    file.path(out_dir, "calibration_1x2.png"),
    p1, width = 7, height = 5, dpi = 150, bg = "white"
  )
  ggsave(
    file.path(out_dir, "cumulative_logscore.png"),
    p5, width = 7, height = 5, dpi = 150, bg = "white"
  )
  ggsave(
    file.path(out_dir, "draw_rate_by_strength.png"),
    p6, width = 7, height = 5, dpi = 150, bg = "white"
  )

  cat("  Saved individual panels\n")
}
