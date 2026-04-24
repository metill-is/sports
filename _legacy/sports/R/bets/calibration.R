#' Bayesian adaptive calibration for Kelly fraction scaling
#'
#' Uses a Beta-Binomial inspired pseudo-count model to estimate a
#' calibration multiplier from settled bet history. The multiplier
#' adjusts the static kelly_frac from bets.yml based on observed
#' win rates vs model probabilities.
#'
#' Unlike the previous frequentist approach (min_sample=30 hard cutoff),
#' this starts updating immediately from bet 1, with configurable
#' prior strength controlling how quickly data overwhelms the prior.
#'
#' Multiplier = (prior_weight * prior_ratio + sum(win)) / (prior_weight + sum(p))
#'
#' Usage:
#'   box::use(R/bets/calibration[compute_calibration])
#'   cal <- compute_calibration(sports_dir, "handball", "iceland", c("male", "female"))
#'   # Returns list(male = 0.95, female = 1.02)

box::use(
  readr[read_csv],
  dplyr[filter, mutate]
)

#' Compute Bayesian calibration multiplier per sex for a league
#'
#' Reads settled bets from bets_log.csv and computes a calibration
#' multiplier using pseudo-count smoothing. The multiplier scales
#' the base kelly_frac from bets.yml:
#'
#'   effective_kelly = base_kelly * multiplier
#'
#' Returns prior_ratio (default 1.0 = no adjustment) when no history exists.
#'
#' @param sports_dir Absolute path to Sports/ root
#' @param sport Sport name (e.g., "handball")
#' @param country Country name (e.g., "iceland")
#' @param sexes Character vector (e.g., c("male", "female"))
#' @param prior_weight Pseudo-count strength — equivalent expected wins of
#'   prior data. Higher = slower adaptation. Default 10.
#' @param prior_ratio Prior belief about calibration ratio. 1.0 = model is
#'   well-calibrated. Default 1.0.
#' @param floor Minimum multiplier (prevents over-correction). Default 0.5.
#' @param ceiling Maximum multiplier (prevents runaway scaling). Default 1.5.
#' @return Named list keyed by sex: e.g. list(male = 0.95, female = 1.02)
#' @export
compute_calibration <- function(
  sports_dir, sport, country, sexes,
  prior_weight = 10, prior_ratio = 1.0,
  floor = 0.5, ceiling = 1.5
) {
  log_path <- file.path(sports_dir, sport, country, "history", "bets_log.csv")

  if (!file.exists(log_path)) {
    result <- stats::setNames(
      as.list(rep(prior_ratio, length(sexes))),
      sexes
    )
    for (sex in sexes) {
      cat(sprintf("  Calibration %s/%s [%s]: no history \u2014 multiplier=%.2f (prior)\n",
                  sport, country, sex, prior_ratio))
    }
    return(result)
  }

  log <- read_csv(log_path, show_col_types = FALSE) |>
    mutate(
      win = as.logical(win),
      probability = as.numeric(probability)
    ) |>
    filter(!is.na(win), !is.na(probability))

  result <- list()

  for (sex in sexes) {
    sex_bets <- log |> filter(sex == !!sex)
    n_settled <- nrow(sex_bets)

    if (n_settled == 0) {
      cat(sprintf("  Calibration %s/%s [%s]: 0 settled \u2014 multiplier=%.2f (prior)\n",
                  sport, country, sex, prior_ratio))
      result[[sex]] <- prior_ratio
      next
    }

    actual_wins <- sum(sex_bets$win, na.rm = TRUE)
    expected_wins <- sum(sex_bets$probability, na.rm = TRUE)

    # Beta-Binomial pseudo-count update:
    # multiplier = (prior_wins + actual_wins) / (prior_expected + expected_wins)
    # where prior_wins = prior_weight * prior_ratio, prior_expected = prior_weight
    multiplier <- (prior_weight * prior_ratio + actual_wins) /
                  (prior_weight + expected_wins)
    clamped <- max(floor, min(ceiling, multiplier))

    cat(sprintf(
      "  Calibration %s/%s [%s]: %d/%d wins (exp %.1f) \u2192 multiplier=%.3f%s (%d bets)\n",
      sport, country, sex,
      actual_wins, n_settled, expected_wins,
      multiplier,
      if (abs(multiplier - clamped) > 0.001) sprintf(" [clamped\u2192%.3f]", clamped) else "",
      n_settled
    ))

    result[[sex]] <- round(clamped, 3)
  }

  result
}
