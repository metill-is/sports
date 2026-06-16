# R/backtest-divisions.R
#' Attach division + round to a model-arm bet/candidate tibble.
#' @importFrom rlang .data
NULL

#' Attach `division` + `round` from `results` on the federation-name match key.
#'
#' The betting stores carry no `division`; the model arm recovers it from
#' `results` on `(sex, match_date, home_team, away_team)` -- clean federation
#' names on both sides (no Lengjan name-join). Unmatched fixtures get
#' `division = "unknown"` (never dropped); `round` stays `NA`.
#' @param bets Tibble with the match key columns.
#' @param results Results store (`division`, `round`, key cols).
#' @return `bets` with `division` (NA -> "unknown") and `round` columns added.
#' @export
bt_attach_division <- function(bets, results) {
  key <- c("sex", "match_date", "home_team", "away_team")
  div <- results |>
    dplyr::group_by(dplyr::across(dplyr::all_of(key))) |>
    dplyr::summarise(
      division = dplyr::first(.data$division),
      round = dplyr::first(.data$round),
      .groups = "drop"
    )
  out <- dplyr::left_join(bets, div, by = key)
  out$division <- dplyr::coalesce(out$division, "unknown")
  out
}
