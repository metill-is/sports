#' @include wc-simulate.R
NULL

# Knockout-stage conditioning for the World Cup forecast: read played knockout
# results, resolve penalty-shootout winners, and build the {match_no -> winner}
# pins that collapse the forecast onto reality once the knockout rounds are
# played. The forward bracket model (R/wc-simulate.R) re-simulates from the
# group stage every run; without this it keeps showing eliminated teams with a
# chance. See docs/superpowers/plans/2026-06-29-wc-knockout-conditioning.md.

#' Read played knockout-stage results from the facts store.
#'
#' The mirror of [wc_group_fixtures()] for the knockout rounds: WC-2026 results
#' whose two teams are in different groups (in this tournament every cross-group
#' pairing is a knockout match) and that carry a score. Empty during the group
#' stage (no knockout teams are decided yet).
#'
#' @param structure Output of [wc_structure()].
#' @param root Data root.
#' @return Tibble: `match_date`, `home_team`, `away_team`, `home_score` (int),
#'   `away_score` (int). Zero rows when no knockout match has been played.
#' @export
wc_knockout_results <- function(structure, root = here::here("data")) {
  empty <- tibble::tibble(
    match_date = as.Date(character(0)), home_team = character(0),
    away_team = character(0), home_score = integer(0), away_score = integer(0)
  )
  flt <- list(sport = "football", country = "world", sex = "male")
  res <- read_table("results", root = root, filter = flt)
  if (nrow(res) == 0L) {
    return(empty)
  }
  res <- res[res$division == "FIFA World Cup" & res$season == 2026L, , drop = FALSE]
  go <- structure$group_of
  cross <- !is.na(go[res$home_team]) & !is.na(go[res$away_team]) &
    go[res$home_team] != go[res$away_team]
  played <- !is.na(res$home_score) & !is.na(res$away_score)
  k <- res[cross & played, , drop = FALSE]
  if (nrow(k) == 0L) {
    return(empty)
  }
  tibble::tibble(
    match_date = k$match_date,
    home_team = k$home_team, away_team = k$away_team,
    home_score = as.integer(k$home_score), away_score = as.integer(k$away_score)
  )
}
