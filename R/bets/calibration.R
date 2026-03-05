#' Adaptive Kelly fraction via calibration ratio
#'
#' Computes kelly_frac per league from historical performance using
#' the ratio cumsum(win) / cumsum(p). This replaces static bets.yml
#' values with data-driven fractions at pipeline runtime.
#'
#' See: ~/Obsidian/Metill/Sports/betting-system-rules.md (rules K1–K6)
#'
#' Usage:
#'   box::use(R/bets/calibration[compute_calibration])
#'   cal <- compute_calibration(sports_dir, "handball", "iceland", c("male", "female"))
#'   # Returns list(kelly_frac_male = 0.18, kelly_frac_female = NULL)

box::use(
  readr[read_csv],
  dplyr[filter, mutate]
)

#' Calibration ratio: realised wins per expected win
#'
#' @param win Logical vector of outcomes
#' @param prob Numeric vector of model probabilities
#' @return Numeric ratio (> 1 = underconfident, < 1 = overconfident)
#' @export
calibration_ratio <- function(win, prob) {
  if (length(win) == 0 || sum(prob) == 0) return(NA_real_)
  sum(win, na.rm = TRUE) / sum(prob, na.rm = TRUE)
}

#' Compute adaptive kelly_frac per sex for a league
#'
#' Reads settled bets from bets_log.csv, computes the calibration ratio,
#' and returns clamped kelly_frac values. Returns NULL for a sex when
#' insufficient data exists (caller keeps the static bets.yml value).
#'
#' @param sports_dir Absolute path to Sports/ root
#' @param sport Sport name (e.g., "handball")
#' @param country Country name (e.g., "iceland")
#' @param sexes Character vector (e.g., c("male", "female"))
#' @param floor Minimum kelly_frac — ensures exploration (K4). Default 0.02
#' @param ceiling Maximum kelly_frac — prevents overbet (K5). Default 0.25
#' @param min_sample Minimum settled bets for data-driven fraction (K3). Default 30
#' @return Named list: e.g. list(kelly_frac_male = 0.18, kelly_frac_female = NULL)
#' @export
compute_calibration <- function(
  sports_dir, sport, country, sexes,
  floor = 0.02, ceiling = 0.25,
  min_sample = 30
) {
  log_path <- file.path(sports_dir, sport, country, "history", "bets_log.csv")

  if (!file.exists(log_path)) {
    result <- stats::setNames(
      as.list(rep(NA, length(sexes))),
      paste0("kelly_frac_", sexes)
    )
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
    sex_key <- paste0("kelly_frac_", sex)
    sex_bets <- log |> filter(sex == !!sex)
    n_settled <- nrow(sex_bets)

    if (n_settled < min_sample) {
      # K3: insufficient data — return NULL so caller keeps bets.yml value
      cat(sprintf("  Calibration %s/%s [%s]: %d settled (< %d min) — using config default\n",
                  sport, country, sex, n_settled, min_sample))
      result[[sex_key]] <- NULL
      next
    }

    ratio <- calibration_ratio(sex_bets$win, sex_bets$probability)
    clamped <- max(floor, min(ceiling, ratio))

    cat(sprintf("  Calibration %s/%s [%s]: ratio=%.3f → kelly_frac=%.3f (%d bets)\n",
                sport, country, sex, ratio, clamped, n_settled))

    result[[sex_key]] <- round(clamped, 3)
  }

  result
}
