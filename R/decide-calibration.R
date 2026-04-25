#' @include storage.R
NULL

#' Bayesian Beta-Binomial calibration multiplier for one (league, sex).
#'
#' Reads settled bets from `data/decisions/ledger/` and computes a
#' multiplier that scales the base kelly_frac in `leagues.yml`. Pseudo-count
#' Beta-Binomial smoothing starts updating from bet 1.
#'
#' multiplier = (prior_weight * prior_ratio + sum(win)) / (prior_weight + sum(p))
#'
#' Returns `prior_ratio` when no settled history exists.
#'
#' @param league List with `sport` + `country`.
#' @param sex "male" or "female".
#' @param root Data root. Default `here::here("data")`.
#' @param prior_weight Pseudo-count strength. Higher = slower adaptation.
#'   Default 30.
#' @param prior_ratio Prior calibration ratio. 1.0 = model is well-calibrated.
#'   Default 1.0.
#' @param floor Lower clamp on multiplier. Default 0.5.
#' @param ceiling Upper clamp. Default 1.5.
#' @return Numeric scalar in `[floor, ceiling]`, rounded to 3 decimals.
#' @export
compute_calibration <- function(league, sex,
                                root = here::here("data"),
                                prior_weight = 30, prior_ratio = 1.0,
                                floor = 0.5, ceiling = 1.5) {
  stopifnot(sex %in% c("male", "female"))
  stopifnot(!is.null(league$sport), !is.null(league$country))

  ledger_dir <- file.path(root, "decisions", "ledger")
  if (!dir.exists(ledger_dir)) {
    return(prior_ratio)
  }

  led <- tryCatch(
    read_table("ledger",
      root = root,
      filter = list(sport = league$sport, country = league$country)
    ),
    error = function(e) tibble::tibble()
  )

  if (nrow(led) == 0L) {
    return(prior_ratio)
  }

  # Filter to settled bets matching sex with non-NA win + p
  led <- led[!is.na(led$sex) & led$sex == sex, , drop = FALSE]
  led <- led[!is.na(led$settled) & led$settled, , drop = FALSE]
  led <- led[!is.na(led$win) & !is.na(led$p), , drop = FALSE]

  if (nrow(led) == 0L) {
    return(prior_ratio)
  }

  actual_wins <- sum(as.numeric(led$win), na.rm = TRUE)
  expected_wins <- sum(led$p, na.rm = TRUE)

  multiplier <- (prior_weight * prior_ratio + actual_wins) /
    (prior_weight + expected_wins)

  clamped <- max(floor, min(ceiling, multiplier))
  round(clamped, 3)
}
