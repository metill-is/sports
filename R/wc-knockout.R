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

#' Read the World Cup penalty-shootout winners store.
#'
#' `data/wc/shootouts.csv` (`date,home_team,away_team,winner`) is maintained by
#' [wc_ingest_shootouts()] from martj42's `shootouts.csv` plus the manual
#' overlay's `pen_winner`. The simulator models extra-time / shootouts as more
#' 90' play, so a knockout level on score has no winner in the facts store; this
#' is where the actual shootout winner is recorded for pinning.
#'
#' @param root Data root.
#' @return Named character map `pair_key -> winner team`, or `NULL` when the
#'   store is absent or header-only.
#' @export
wc_shootout_winners <- function(root = here::here("data")) {
  path <- file.path(root, "wc", "shootouts.csv")
  if (!file.exists(path)) {
    return(NULL)
  }
  s <- utils::read.csv(path, colClasses = "character", stringsAsFactors = FALSE)
  if (nrow(s) == 0L) {
    return(NULL)
  }
  stats::setNames(s$winner, .wc_pair_key(s$home_team, s$away_team))
}

# Winner team name of one played knockout result: the higher score, or — when
# level on score (the shootout case the simulator does not model) — the
# shootout-winners map's entry. NA when the scores are level with no shootout
# winner on record (the match stays a probabilistic forecast until one lands).
.wc_knockout_winner_of <- function(res, shootout_winners = NULL) {
  hs <- res$home_score[[1L]]
  as_ <- res$away_score[[1L]]
  if (is.na(hs) || is.na(as_)) {
    return(NA_character_)
  }
  if (hs > as_) {
    return(res$home_team[[1L]])
  }
  if (as_ > hs) {
    return(res$away_team[[1L]])
  }
  if (is.null(shootout_winners)) {
    return(NA_character_)
  }
  w <- shootout_winners[[.wc_pair_key(res$home_team[[1L]], res$away_team[[1L]])]]
  if (is.null(w) || is.na(w)) {
    return(NA_character_)
  }
  w
}
