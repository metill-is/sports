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

# Build the {match_no -> winner index} pins (and per-match `played` records) that
# collapse the forward bracket onto played knockout results. Walks
# `structure$bracket` R32 -> Final, mirroring wc_forward_bracket's order so the
# R32 occupancy rows (`occ_a`/`occ_b`) line up. A match resolves only when both
# its competitors are certain — R32 from the (post-group, one-hot) occupancy,
# later rounds from the winners pinned so far — so the map self-gates round by
# round (an R16 match cannot pin until both its R32 feeders have). Drawn knockouts
# resolve via `shootout_winners`; without one they stay probabilistic.
# Returns 1-based winner/loser team indices (publish converts to 0-based).
.wc_knockout_pins <- function(bracket, teams, occ_a, occ_b,
                              knockout_results, shootout_winners = NULL,
                              third_place = NULL) {
  pins <- list()
  played <- list()
  if (is.null(knockout_results) || nrow(knockout_results) == 0L) {
    return(list(pins = pins, played = played))
  }
  tidx <- stats::setNames(seq_along(teams), teams)
  rk <- .wc_pair_key(knockout_results$home_team, knockout_results$away_team)
  certain <- function(v) {
    i <- which(v >= 0.9995)
    if (length(i) == 1L) i else NA_integer_
  }
  feeder_no <- function(f) as.integer(sub("W", "", f))
  winner_of <- list()
  r32_i <- 0L
  for (i in seq_len(nrow(bracket))) {
    m <- bracket$match_no[i]
    if (bracket$round[i] == "R32") {
      r32_i <- r32_i + 1L
      tA <- certain(occ_a[r32_i, ])
      tB <- certain(occ_b[r32_i, ])
    } else {
      tA <- winner_of[[as.character(feeder_no(bracket$feeder_a[i]))]]
      tB <- winner_of[[as.character(feeder_no(bracket$feeder_b[i]))]]
      if (is.null(tA)) tA <- NA_integer_
      if (is.null(tB)) tB <- NA_integer_
    }
    if (is.na(tA) || is.na(tB)) {
      next
    }
    hit <- which(rk == .wc_pair_key(teams[tA], teams[tB]))
    if (length(hit) == 0L) {
      next
    }
    row <- knockout_results[hit[[1L]], , drop = FALSE]
    w_name <- .wc_knockout_winner_of(row, shootout_winners)
    if (is.na(w_name)) {
      next
    }
    wi <- tidx[[w_name]]
    li <- if (wi == tA) tB else tA
    home_wins <- identical(row$home_team[[1L]], w_name)
    pins[[as.character(m)]] <- wi
    winner_of[[as.character(m)]] <- wi
    played[[length(played) + 1L]] <- list(
      match_no = as.integer(m), winner = wi, loser = li,
      winner_score = as.integer(if (home_wins) row$home_score[[1L]] else row$away_score[[1L]]),
      loser_score = as.integer(if (home_wins) row$away_score[[1L]] else row$home_score[[1L]]),
      shootout = isTRUE(row$home_score[[1L]] == row$away_score[[1L]])
    )
  }
  # ---- Bronze (3rd place): SF losers, resolved only once both SFs are pinned --
  # Precondition: third_pin (if set) must be a loser of one of the two SFs;
  # passing a non-SF-loser would shift probability mass incorrectly (Sigma_fourth
  # slightly < 1). Structurally unreachable here — we only set it to the actual
  # bronze match winner, who must be an SF loser by tournament rules.
  if (!is.null(third_place) && nrow(third_place) == 1L) {
    sf <- bracket$match_no[bracket$round == "SF"]
    loser_sf <- function(mno) {
      i <- which(bracket$match_no == mno)
      fa <- as.integer(sub("W", "", bracket$feeder_a[i]))
      fb <- as.integer(sub("W", "", bracket$feeder_b[i]))
      wsf <- winner_of[[as.character(mno)]]
      parts <- c(winner_of[[as.character(fa)]], winner_of[[as.character(fb)]])
      if (is.null(wsf) || length(parts) < 2L) {
        return(NA_integer_)
      }
      setdiff(parts, wsf)
    }
    lA <- loser_sf(sf[1])
    lB <- loser_sf(sf[2])
    if (!is.na(lA) && !is.na(lB)) {
      hit <- which(rk == .wc_pair_key(teams[lA], teams[lB]))
      if (length(hit) > 0L) {
        row <- knockout_results[hit[[1L]], , drop = FALSE]
        w_name <- .wc_knockout_winner_of(row, shootout_winners)
        if (!is.na(w_name)) {
          wi <- tidx[[w_name]]
          li <- if (wi == lA) lB else lA
          home_wins <- identical(row$home_team[[1L]], w_name)
          pins[["103"]] <- wi
          played[[length(played) + 1L]] <- list(
            match_no = 103L, winner = wi, loser = li,
            winner_score = as.integer(if (home_wins) row$home_score[[1L]] else row$away_score[[1L]]),
            loser_score = as.integer(if (home_wins) row$away_score[[1L]] else row$home_score[[1L]]),
            shootout = isTRUE(row$home_score[[1L]] == row$away_score[[1L]])
          )
        }
      }
    }
  }
  list(pins = pins, played = played)
}
