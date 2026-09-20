#' @include extract-football-iceland.R
NULL

# Cup bracket simulator
#
# Forward-simulates a knockout cup bracket from R16 through Final, per
# posterior draw. Reads team-strength + scalar parameter draws (extracted
# by `.extract_sim_inputs_pfi()`), walks each draw through the bracket
# using a bivariate-Poisson match model with rejection-sample tiebreaking
# (P(winner | someone wins) at the 90' lambdas). Aggregates to
# per-(team, round_name) cumulative probabilities.
#
# Design doc: Sports/Mjólkurbikar Bracket Simulator Design (Metill vault).

# ---- Bivariate Poisson RNG --------------------------------------------------

# Trivariate-reduction bivariate Poisson: (Y1, Y2) where
#   Y1 = X1 + X3, Y2 = X2 + X3, X_i ~ Poisson(lambda_i) independently.
# Matches the Stan helper `poisson_2d_log_rng` in
# `Stan/football_iceland/bivariate_poisson_no_inflation.stan`.
.rbvpois <- function(lambda1, lambda2, lambda3) {
  x1 <- stats::rpois(1L, lambda1)
  x2 <- stats::rpois(1L, lambda2)
  x3 <- stats::rpois(1L, lambda3)
  c(x1 + x3, x2 + x3)
}

# ---- One-match simulator ---------------------------------------------------

# Simulate one cup match using one posterior draw's parameters. Returns the
# winning team's name.
#
# Tiebreak chain:
#   1. Sample (g_h, g_a) at 90' from bivariate Poisson. If g_h != g_a -> winner.
#   2. Else sample extra time at scaled rate (default: 1/3 of full-time rate).
#      If et_g_h != et_g_a -> winner.
#   3. Else penalty shootout: 50/50 coin OR Bradley-Terry on offence diff,
#      depending on `tiebreak_opts$shootout`.
.simulate_cup_match_pfi <- function(home_team, away_team, venue,
                                    off, def, ha_off, ha_def,
                                    mean_log_goals, alpha_mu3, beta_mu3_diff,
                                    tiebreak_opts) {
  off_h <- off[[home_team]]
  def_h <- def[[home_team]]
  off_a <- off[[away_team]]
  def_a <- def[[away_team]]

  if (identical(venue, "home")) {
    off_h <- off_h + ha_off[[home_team]]
    def_h <- def_h + ha_def[[home_team]]
  } else if (identical(venue, "away")) {
    off_a <- off_a + ha_off[[away_team]]
    def_a <- def_a + ha_def[[away_team]]
  }
  # venue == "neutral": no home-advantage adjustment

  mu_h <- mean_log_goals + off_h - def_a
  mu_a <- mean_log_goals + off_a - def_h
  strength_diff <- abs(off_h + def_h - off_a - def_a)
  logit_rho <- alpha_mu3 + beta_mu3_diff * strength_diff
  mu3 <- stats::plogis(logit_rho, log.p = TRUE) + 0.5 * (mu_h + mu_a)

  lambda_h <- exp(mu_h)
  lambda_a <- exp(mu_a)
  lambda3 <- exp(mu3)

  # Rejection-sample from the bivariate Poisson until a non-tied draw
  # emerges. Mathematically: P(home wins) / (P(home wins) + P(away wins)),
  # i.e. the conditional win probability given that *someone* wins. This
  # assumes ET / shootouts are "more 90' play at the same strengths" —
  # consistent with the prediction layer's existing assumption that
  # strengths are frozen at training cutoff for all future matches. Avoids
  # arbitrary ET-rate-scale and shootout-model parameters.
  #
  # Expected attempts ≈ 1 / (1 - P(tie at 90')) ≈ 1.28 for typical
  # Iceland-football lambdas. `max_iter` is a paranoia guard for the
  # degenerate case where both teams are projected to score ~0 goals,
  # which would make most draws (0, 0); the 50/50 fallback in that case
  # is mathematically correct (the conditional distribution is undefined
  # when P(non-tie) = 0).
  for (i in seq_len(tiebreak_opts$max_iter)) {
    goals <- .rbvpois(lambda_h, lambda_a, lambda3)
    if (goals[1] > goals[2]) {
      return(home_team)
    }
    if (goals[2] > goals[1]) {
      return(away_team)
    }
  }
  if (stats::runif(1L) < 0.5) home_team else away_team
}

# ---- One-draw bracket walker -----------------------------------------------

# Resolve one round's pairings from `bracket_state$rounds[[round_name]]`.
#
# Returns the round's match tibble when the draw is known, or NULL when the
# round is legitimately undrawn — KSÍ has not made the draw yet, so the
# caller pairs the previous round's winners uniformly at random, which IS
# the forecast for a free draw.
#
# WHY this is strict about every other shape: the walker used to treat the
# drawn case as a narrow `if` and send *everything else* down the random
# branch. A round that was half-built, row-short, or mis-shaped therefore
# became a uniform re-pairing with no message at all. For a late round that
# merely throws away information; for the entry round it discards the real
# draw AND every played-match pin, and the simulator then publishes a
# plausible, wholly fictional bracket into a public forecast and the
# betting pipeline. Exactly one shape means "undrawn" — `pairings_known`
# FALSE, no matches attached, and no `partial_pairs`: the builder found
# NONE of the round's ties. A round it found only SOME of carries
# `partial_pairs` and aborts here, because the real draw exists and
# re-pairing over it is the failure above. Anything else is a bug in
# `.build_bracket_state_pfi()` or a corrupted store, and must stop the run.
.cup_round_matches_pfi <- function(round, round_name, n_m) {
  if (!is.list(round)) {
    cli::cli_abort(c(
      "Cup round {.val {round_name}} is missing or malformed in {.arg bracket_state$rounds}.",
      "x" = "Expected a list; got {.cls {class(round)}}.",
      "i" = "An undrawn round still needs an entry: {.code list(pairings_known = FALSE, matches = NULL)}."
    ))
  }

  known <- round$pairings_known
  if (!is.logical(known) || length(known) != 1L || is.na(known)) {
    cli::cli_abort(c(
      "Cup round {.val {round_name}} has a malformed {.field pairings_known}.",
      "x" = "Expected a single non-NA logical; got {.cls {class(known)}} of length {length(known)}.",
      "i" = "Without a usable flag the round's real draw cannot be told apart from an undrawn one."
    ))
  }

  n_rows <- if (is.null(round$matches)) 0L else nrow(round$matches)

  if (!known) {
    # A round the builder saw only PART of is not undrawn. It is the one
    # shape that reaches here looking exactly like a free draw while the
    # real ties already exist — a quarter-final leg dropped because it sits
    # beyond the model's prediction horizon, a filtered TBD stub, a
    # mis-ranked bracket window. Re-pairing the survivors at random then
    # throws away the drawn ties AND every played-match pin, and publishes a
    # bracket that simply is not the competition being played.
    if (length(round$partial_pairs) > 0L) {
      cli::cli_abort(c(
        "Cup round {.val {round_name}} is only partly drawn: {length(round$partial_pairs)} of {n_m} tie{?s} found.",
        "x" = "Found {.val {round$partial_pairs}}.",
        "i" = "Complete the round in {.fun .build_bracket_state_pfi} — a missing leg usually means it lies beyond {.fun prepare_data}'s prediction horizon and the raw schedule store was not passed in."
      ))
    }

    # Pairings carried under a FALSE flag are a contradiction: either the
    # draw is known and the flag is wrong, or the rows are stale. Falling
    # through to the random branch would silently bin a real draw, which is
    # the failure this guard exists for.
    if (n_rows > 0L) {
      cli::cli_abort(c(
        "Cup round {.val {round_name}} is flagged undrawn but carries {n_rows} match row{?s}.",
        "x" = "{.field pairings_known} is FALSE while {.field matches} is non-empty.",
        "i" = "Set {.field pairings_known} to TRUE for a drawn round, or drop the rows — the simulator will not re-draw over real pairings."
      ))
    }
    return(NULL)
  }

  if (!is.data.frame(round$matches)) {
    cli::cli_abort(c(
      "Cup round {.val {round_name}} is flagged drawn but has no match table.",
      "x" = "{.field pairings_known} is TRUE while {.field matches} is {.cls {class(round$matches)}}.",
      "i" = "A drawn round must carry its {n_m} pairing{?s}."
    ))
  }

  # A row-count mismatch is the dangerous case: a partly-populated round
  # (say 3 of 4 quarter-finals scraped) used to be discarded wholesale and
  # re-drawn at random, so the published bracket disagreed with the
  # competition while looking entirely normal.
  if (n_rows != n_m) {
    cli::cli_abort(c(
      "Cup round {.val {round_name}} has {n_rows} match row{?s}; expected {n_m}.",
      "i" = "A short or over-long round means the bracket builder mis-ranked the cup matches — fix the state rather than simulating a different competition."
    ))
  }

  # venue and known_winner are read positionally per match. A missing venue
  # column silently degrades every tie to neutral (no home advantage), so
  # check the full contract rather than only the pairing columns.
  required <- c("home_team", "away_team", "venue", "known_winner")
  missing_cols <- setdiff(required, names(round$matches))
  if (length(missing_cols) > 0L) {
    cli::cli_abort(c(
      "Cup round {.val {round_name}} is missing match column{?s} {.val {missing_cols}}.",
      "i" = "Required: {.val {required}}."
    ))
  }

  round$matches
}

# Walks R16 -> (QF draw) -> QF -> (SF draw) -> SF -> Final for one draw.
#
# Returns a named integer vector: team -> max round reached.
#   0 = eliminated in R16 (did not reach QF)
#   1 = lost in QF (reached QF)
#   2 = lost in SF (reached SF)
#   3 = lost the Final (reached Final)
#   4 = Champion
.simulate_cup_bracket_one_draw_pfi <- function(bracket_state, draw_team, draw_scalars,
                                               tiebreak_opts) {
  off <- stats::setNames(draw_team$cur_offense, draw_team$team)
  def <- stats::setNames(draw_team$cur_defense, draw_team$team)
  ha_off <- stats::setNames(draw_team$home_advantage_off, draw_team$team)
  ha_def <- stats::setNames(draw_team$home_advantage_def, draw_team$team)
  mlg <- draw_scalars$mean_log_goals
  amu3 <- draw_scalars$alpha_mu3
  bmu3 <- draw_scalars$beta_mu3_strength_diff

  sim_match <- function(home_team, away_team, venue) {
    .simulate_cup_match_pfi(
      home_team, away_team, venue,
      off, def, ha_off, ha_def, mlg, amu3, bmu3, tiebreak_opts
    )
  }

  cup_teams <- bracket_state$cup_teams
  round_reached <- stats::setNames(rep(0L, length(cup_teams)), cup_teams)

  # The bracket walker. Each round either has known pairings (from results /
  # schedule) or is explicitly undrawn, in which case its pairings are
  # simulated via a uniform random permutation of the previous round's
  # winners. Any other shape aborts in .cup_round_matches_pfi() — a
  # malformed round must not decay into a random re-pairing. Per-match
  # outcomes are similarly either known (played match) or simulated via
  # sim_match.
  ROUND_SEQ <- c("R16", "R8", "SF", "Final")
  ROUND_SIZE <- c(R16 = 8L, R8 = 4L, SF = 2L, Final = 1L)

  current_winners <- cup_teams
  for (r_idx in seq_along(ROUND_SEQ)) {
    round_name <- ROUND_SEQ[r_idx]
    round <- bracket_state$rounds[[round_name]]
    n_m <- ROUND_SIZE[[round_name]]

    matches <- .cup_round_matches_pfi(round, round_name, n_m)
    if (is.null(matches)) {
      # Genuinely undrawn round: a uniform permutation of the surviving
      # teams is the correct forecast for a free draw that has not been
      # made yet. Guard the pool size first — a short winner list would
      # index past the end and yield NA "teams", which .simulate_cup_match_pfi
      # then fails on with an opaque subscript error one layer down.
      if (length(current_winners) != 2L * n_m) {
        cli::cli_abort(c(
          "Cannot draw cup round {.val {round_name}}: {length(current_winners)} team{?s} available, {2L * n_m} needed.",
          "i" = "The previous round produced the wrong number of winners — check {.field cup_teams} and the round sizes."
        ))
      }
      perm <- sample.int(length(current_winners))
      paired <- current_winners[perm]
      matches <- tibble::tibble(
        home_team    = paired[seq.int(1L, 2L * n_m, by = 2L)],
        away_team    = paired[seq.int(2L, 2L * n_m, by = 2L)],
        venue        = rep("neutral", n_m),
        known_winner = rep(NA_character_, n_m)
      )
    }

    winners <- character(n_m)
    for (m in seq_len(n_m)) {
      if (!is.na(matches$known_winner[m])) {
        winners[m] <- matches$known_winner[m]
      } else {
        winners[m] <- sim_match(
          matches$home_team[m], matches$away_team[m], matches$venue[m]
        )
      }
    }

    round_reached[winners] <- pmax(round_reached[winners], r_idx)
    current_winners <- winners
  }

  round_reached
}

# ---- Public API -----------------------------------------------------------

#' Default tiebreak options for the cup bracket simulator
#'
#' Rejection-sample bivariate Poisson outcomes at the 90' lambdas until a
#' non-tied draw emerges. Mathematically equivalent to drawing from the
#' conditional distribution P(winner | someone wins) under the model — i.e.
#' treats ET / shootouts as "more 90' play at the same strengths", which is
#' consistent with the prediction layer's assumption that strengths are
#' frozen at training cutoff for all future matches.
#'
#' @param max_iter Maximum number of bivariate-Poisson draws per match
#'   before falling back to a 50/50 coin. Expected attempts are ≈ 1.28 for
#'   typical Icelandic-football lambdas; the cap exists only to defend
#'   against the degenerate "both teams project near-zero goals" case.
#'
#' @return A list with `max_iter`.
#' @export
default_tiebreak_opts <- function(max_iter = 50L) {
  list(max_iter = max_iter)
}

#' Simulate a knockout cup bracket across posterior draws
#'
#' For each posterior draw of team-strength parameters, forward-simulates the
#' remaining cup bracket (R16 -> QF -> SF -> Final) using a bivariate-Poisson
#' match model. Per-match tiebreaking is rejection-sampling at the 90'
#' lambdas (no separate ET / shootout model — see [`default_tiebreak_opts()`]).
#' A round whose pairings are known is walked exactly as drawn, played-match
#' pins included; a round explicitly marked undrawn is paired uniformly at
#' random per posterior draw. Any other round shape is an error, never a
#' silent re-draw — see `bracket_state` below.
#' Aggregates outcomes to cumulative per-(team, round_name) probabilities.
#'
#' @param sim_inputs_team Tibble with columns `team`, `.draw`, `cur_offense`,
#'   `cur_defense`, `home_advantage_off`, `home_advantage_def`. Produced by
#'   `.extract_sim_inputs_pfi()`.
#' @param sim_inputs_scalar Tibble with columns `.draw`, `mean_log_goals`,
#'   `alpha_mu3`, `beta_mu3_strength_diff`. Produced by
#'   `.extract_sim_inputs_pfi()`.
#' @param bracket_state Named list, as built by `.build_bracket_state_pfi()`:
#'   - `cup_teams`: character(16) — the R16 entrants.
#'   - `rounds`: named list keyed `"R16"`, `"R8"`, `"SF"`, `"Final"`. Each
#'     entry is a list of `pairings_known` (a single non-`NA` logical) and
#'     `matches`. When `pairings_known` is `TRUE`, `matches` is a data frame
#'     with exactly 8 / 4 / 2 / 1 rows and columns `home_team`, `away_team`,
#'     `venue` ("home" / "away" / "neutral") and `known_winner` (team name
#'     for a played match, `NA` otherwise). When it is `FALSE` the round is
#'     undrawn, `matches` must be absent, and the pairings are drawn
#'     uniformly at random per posterior draw. A round the builder found
#'     only some of the ties for is neither: it carries `partial_pairs`
#'     (the ties it did find) and aborts.
#'   Any other shape — a missing round, a non-logical `pairings_known`, a
#'   row-count mismatch, missing match columns, a partly-drawn round, or
#'   pairings carried under `pairings_known = FALSE` — aborts. Randomising over a malformed round
#'   would publish a bracket that does not match the real competition.
#' @param tiebreak_opts Optional override of [`default_tiebreak_opts()`].
#' @param pairing_seed Optional integer. When set, `set.seed()` is called
#'   once at the entry point so the full simulation is reproducible.
#'
#' @return Tibble with columns `team` (chr), `round_name` (factor with levels
#'   `"R16"`, `"QF"`, `"SF"`, `"Final"`, `"Champion"`), `probability` (dbl).
#'   `probability` is `P(team reaches at least this round)` — cumulative and
#'   monotonically non-increasing in `round_name` for each team. 5 rows per
#'   team in the bracket.
#'
#' @export
simulate_cup_bracket <- function(sim_inputs_team,
                                 sim_inputs_scalar,
                                 bracket_state,
                                 tiebreak_opts = default_tiebreak_opts(),
                                 pairing_seed = NULL) {
  stopifnot(
    is.data.frame(sim_inputs_team),
    is.data.frame(sim_inputs_scalar),
    is.list(bracket_state),
    !is.null(bracket_state$cup_teams),
    length(bracket_state$cup_teams) == 16L,
    !is.null(bracket_state$rounds),
    all(c("R16", "R8", "SF", "Final") %in% names(bracket_state$rounds)),
    all(c(
      "team", ".draw", "cur_offense", "cur_defense",
      "home_advantage_off", "home_advantage_def"
    )
    %in% names(sim_inputs_team)),
    all(c(".draw", "mean_log_goals", "alpha_mu3", "beta_mu3_strength_diff")
    %in% names(sim_inputs_scalar))
  )

  if (!is.null(pairing_seed)) {
    set.seed(pairing_seed)
  }

  draws_team <- split(sim_inputs_team, sim_inputs_team$.draw)
  draws_scalar <- split(sim_inputs_scalar, sim_inputs_scalar$.draw)

  draw_keys <- intersect(names(draws_team), names(draws_scalar))
  if (length(draw_keys) == 0L) {
    stop("simulate_cup_bracket: no overlapping .draw keys between sim_inputs_team and sim_inputs_scalar")
  }

  all_results <- vector("list", length(draw_keys))
  for (i in seq_along(draw_keys)) {
    k <- draw_keys[i]
    round_reached <- .simulate_cup_bracket_one_draw_pfi(
      bracket_state, draws_team[[k]], draws_scalar[[k]], tiebreak_opts
    )
    all_results[[i]] <- tibble::tibble(
      .draw = as.integer(k),
      team = names(round_reached),
      round_reached = unname(round_reached)
    )
  }

  per_draw <- dplyr::bind_rows(all_results)

  rounds <- tibble::tibble(
    round_name = c("R16", "QF", "SF", "Final", "Champion"),
    min_round_reached = c(0L, 1L, 2L, 3L, 4L)
  )

  per_draw |>
    tidyr::expand_grid(rounds) |>
    dplyr::mutate(reached = .data$round_reached >= .data$min_round_reached) |>
    dplyr::summarise(
      probability = mean(.data$reached),
      .by = c("team", "round_name")
    ) |>
    dplyr::mutate(
      round_name = factor(.data$round_name,
        levels = c("R16", "QF", "SF", "Final", "Champion")
      )
    ) |>
    dplyr::arrange(.data$team, .data$round_name)
}
