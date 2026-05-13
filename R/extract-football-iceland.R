#' @include model-prepare.R storage.R config.R publish-football-iceland.R
NULL

# ---- Internal helpers --------------------------------------------------------

# Summarise per-draw values into 99-quantile bands per group.
# Input: tibble with `value` column + grouping columns.
# Output: tibble with same grouping columns + `quantile` (int 1..99) + `value`.
# Uses group_modify rather than reframe to dodge dplyr's per-row recycle
# heuristics on multi-column same-length expressions inside .by.
.summarise_quantile_band_pfi <- function(draws, group_keys) {
  probs <- seq(0.01, 0.99, by = 0.01)
  draws |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_keys))) |>
    dplyr::group_modify(~ tibble::tibble(
      quantile = seq_len(99L),
      value = unname(stats::quantile(
        .x$value,
        probs = probs,
        names = FALSE
      ))
    )) |>
    dplyr::ungroup()
}

# Canonical division codes the football iceland fit covers.
# `BD` = Besta deild (top tier), `LD1` = Lengjudeild (second tier),
# `CUP` = Mjólkurbikar (knockout cup). Order matches
# publish_football_iceland()'s loop and read_extracted_football()'s return
# list ordering. CUP rows skip the league-table simulation (final_positions /
# points_distribution) since a knockout has no points table — those parquets
# are written as empty tibbles for the CUP partition.
.FOOTBALL_ICELAND_DIVISIONS_PFI <- c("BD", "LD1", "CUP")

# Per-division extraction. Returns a named list of 6 tibbles (one per parquet
# file type) for `target_div`. The caller binds rows across divisions and
# writes one parquet per file type with `division` as a payload column.
# Cross-division inputs (`posterior_goals_long`, `team_strengths_draws`,
# `home_advantage_draws`, `results`, `teams`) are computed once by the caller.
.extract_division_parquets_pfi <- function(target_div,
                                           fit,
                                           teams,
                                           results,
                                           current_season,
                                           posterior_goals_long,
                                           team_strengths_draws,
                                           home_advantage_draws,
                                           n_pred_fit,
                                           n_pred_data,
                                           sim_inputs = NULL,
                                           bracket_state = NULL) {
  top_results <- results[
    results$season == current_season & results$division == target_div, ,
    drop = FALSE
  ]
  current_top_teams <- if (nrow(top_results) > 0L) {
    top_results |>
      dplyr::select("home_team", "away_team") |>
      tidyr::pivot_longer(c("home_team", "away_team"), values_to = "team") |>
      dplyr::distinct(.data$team)
  } else if (nrow(posterior_goals_long) > 0L) {
    # Pre-round-1 fallback: when end_date precedes the current season's
    # first match in target_div (e.g. backfill fits), the played-results
    # filter is empty. Fall back to teams in the upcoming schedule for
    # the same division so team_strengths_quantiles still ships.
    posterior_goals_long |>
      dplyr::filter(.data$division == target_div) |>
      dplyr::distinct(.data$home_team, .data$away_team) |>
      tidyr::pivot_longer(c("home_team", "away_team"), values_to = "team") |>
      dplyr::distinct(.data$team)
  } else {
    tibble::tibble(team = character())
  }

  # ---- 1. predicted_matches.parquet (count of integer score pairs) ---------

  predicted_matches <- if (n_pred_fit != n_pred_data || n_pred_data == 0L) {
    tibble::tibble(
      home_team = character(), away_team = character(),
      match_date = as.Date(character()),
      home_goals = integer(), away_goals = integer(),
      count = integer()
    )
  } else {
    posterior_goals_long |>
      dplyr::filter(.data$division == target_div) |>
      dplyr::mutate(
        home_goals = as.integer(round(.data$home_goals)),
        away_goals = as.integer(round(.data$away_goals))
      ) |>
      dplyr::count(
        .data$home_team, .data$away_team, .data$match_date,
        .data$home_goals, .data$away_goals,
        name = "count"
      ) |>
      dplyr::mutate(count = as.integer(.data$count)) |>
      dplyr::arrange(
        .data$match_date, .data$home_team, .data$away_team,
        .data$home_goals, .data$away_goals
      )
  }

  # ---- 2. team_strengths_quantiles.parquet --------------------------------
  # 9-cell grid: component ∈ {offence, defence, total} × location ∈ {home, away, avg}.
  # `avg` is per-draw mean of home/away pre-quantile so uncertainty intervals
  # reflect the joint posterior, not a post-hoc point average.

  team_strengths_avg <- team_strengths_draws |>
    dplyr::summarise(
      value = mean(.data$value),
      .by = c(".draw", "team", "component")
    ) |>
    dplyr::mutate(location = "avg")

  team_strengths_quantiles <- dplyr::bind_rows(
    team_strengths_draws,
    team_strengths_avg
  ) |>
    dplyr::semi_join(current_top_teams, by = "team") |>
    .summarise_quantile_band_pfi(c("team", "component", "location"))

  # ---- 3. round_strengths_quantiles.parquet -------------------------------
  # Per (round, team) trajectory, scoped to the division's chronological
  # matchweeks. The fit holds offense/defense matrices indexed by the
  # team's global round number; the helper maps that to division-specific
  # matchweeks. For LD this gives an LD-only round trajectory.

  trajectory_long <- .compute_team_strength_trajectory_pfi(
    fit = fit,
    results = results,
    teams = teams,
    current_top_teams = current_top_teams,
    current_season = current_season,
    top_div = target_div
  )

  round_strengths_quantiles <- if (nrow(trajectory_long) > 0L) {
    avg_long <- trajectory_long |>
      dplyr::summarise(
        value = mean(.data$value),
        .by = c(".draw", "round", "team", "component")
      ) |>
      dplyr::mutate(location = "avg")

    dplyr::bind_rows(trajectory_long, avg_long) |>
      .summarise_quantile_band_pfi(c("round", "team", "component", "location"))
  } else {
    tibble::tibble(
      round = integer(), team = character(),
      component = character(), location = character(),
      quantile = integer(), value = numeric()
    )
  }

  # ---- 4. home_advantage_quantiles.parquet --------------------------------
  # Multiplicative form: exp(home_advantage_*). `total` halved per the
  # publisher's per-side allocation convention. Cross-division draws are
  # filtered to the division's teams here.

  home_advantage_quantiles <- home_advantage_draws |>
    dplyr::semi_join(current_top_teams, by = "team") |>
    .summarise_quantile_band_pfi(c("team", "component"))

  # ---- 5 + 6. final_positions.parquet + points_distribution.parquet -------
  # Pre-computed at extract time (when full draws are still in memory).
  # Logic mirrors the publisher's existing simulation block, scoped to
  # the target division's teams + matches.
  #
  # CUP (Mjólkurbikar) is a knockout — there is no points table to
  # integrate over, so we emit empty tibbles for league-table outputs.
  # Bracket-progression probabilities live in `tournament_placements`
  # and come from `simulate_cup_bracket()` when sim_inputs + bracket_state
  # are provided by the caller.
  if (identical(target_div, "CUP")) {
    empty_placements <- tibble::tibble(
      team        = character(),
      round_name  = character(),
      probability = numeric()
    )
    tournament_placements <- if (!is.null(sim_inputs) && !is.null(bracket_state)) {
      # Defensive: skip simulator when cup teams aren't present in sim_inputs.
      # This happens when a fit's team registry doesn't cover all cup
      # entrants (e.g. legacy backup fit + fresh schedule); the simulator's
      # by-name lookups would otherwise crash with "subscript out of bounds".
      needed_teams <- bracket_state$cup_teams
      sim_team_names <- unique(sim_inputs$team$team)
      missing_in_sim <- setdiff(needed_teams, sim_team_names)
      if (length(missing_in_sim) > 0L) {
        warning(
          sprintf(
            paste0(
              "tournament_placements: skipping simulator -- %d cup team(s) ",
              "not in sim_inputs$team: %s. This usually means the fit's ",
              "team registry doesn't match the current schedule."
            ),
            length(missing_in_sim),
            paste(missing_in_sim, collapse = ", ")
          ),
          call. = FALSE
        )
        empty_placements
      } else {
        simulate_cup_bracket(
          sim_inputs_team   = sim_inputs$team,
          sim_inputs_scalar = sim_inputs$scalar,
          bracket_state     = bracket_state,
          pairing_seed      = 42L
        ) |>
          dplyr::mutate(round_name = as.character(.data$round_name))
      }
    } else {
      empty_placements
    }
    return(list(
      predicted_matches = predicted_matches,
      team_strengths_quantiles = team_strengths_quantiles,
      round_strengths_quantiles = round_strengths_quantiles,
      home_advantage_quantiles = home_advantage_quantiles,
      final_positions = tibble::tibble(
        team = character(),
        placement = integer(),
        probability = numeric()
      ),
      points_distribution = tibble::tibble(
        team = character(),
        points = integer(),
        probability = numeric()
      ),
      tournament_placements = tournament_placements
    ))
  }

  base_points <- if (nrow(top_results) > 0L) {
    top_results |>
      dplyr::mutate(
        result = dplyr::case_when(
          .data$home_score > .data$away_score ~ "home",
          .data$home_score < .data$away_score ~ "away",
          TRUE ~ "tie"
        )
      ) |>
      tidyr::pivot_longer(c("home_team", "away_team"), values_to = "team") |>
      dplyr::mutate(
        name = dplyr::if_else(.data$name == "home_team", "home", "away"),
        points = dplyr::case_when(
          .data$result == "tie" ~ 1L,
          .data$result == .data$name ~ 3L,
          TRUE ~ 0L
        )
      ) |>
      dplyr::summarise(base_points = sum(.data$points), .by = "team")
  } else {
    tibble::tibble(team = character(), base_points = integer())
  }

  posterior_goals <- if (n_pred_fit == n_pred_data && n_pred_data > 0L) {
    posterior_goals_long
  } else {
    tibble::tibble(
      .draw = integer(), game_nr = integer(),
      home_goals = numeric(), away_goals = numeric(),
      match_date = as.Date(character()),
      home_team = character(), away_team = character(),
      division = character()
    )
  }

  if (nrow(posterior_goals) > 0L && nrow(current_top_teams) > 0L) {
    iter_team_points <- posterior_goals |>
      dplyr::filter(.data$division == target_div) |>
      dplyr::mutate(
        result = dplyr::case_when(
          .data$home_goals > .data$away_goals ~ "home",
          .data$home_goals < .data$away_goals ~ "away",
          TRUE ~ "tie"
        )
      ) |>
      tidyr::pivot_longer(c("home_team", "away_team"), values_to = "team") |>
      dplyr::mutate(
        name = dplyr::if_else(.data$name == "home_team", "home", "away"),
        points = dplyr::case_when(
          .data$result == "tie" ~ 1L,
          .data$result == .data$name ~ 3L,
          TRUE ~ 0L
        )
      ) |>
      dplyr::summarise(points = sum(.data$points), .by = c(".draw", "team")) |>
      dplyr::left_join(base_points, by = "team") |>
      dplyr::mutate(
        base_points = dplyr::coalesce(.data$base_points, 0L),
        points      = .data$points + .data$base_points
      )
  } else {
    iter_team_points <- tibble::tibble(
      .draw = integer(), team = character(),
      points = integer(), base_points = integer()
    )
  }

  if (nrow(iter_team_points) > 0L) {
    iter_positions <- iter_team_points |>
      dplyr::arrange(.data$.draw, dplyr::desc(.data$points)) |>
      dplyr::mutate(placement = dplyr::row_number(), .by = ".draw")

    n_teams_top <- iter_positions |>
      dplyr::distinct(.data$team) |>
      nrow()

    final_positions <- iter_positions |>
      dplyr::count(.data$team, .data$placement) |>
      tidyr::complete(
        team,
        placement = seq_len(n_teams_top),
        fill = list(n = 0)
      ) |>
      dplyr::mutate(
        probability = .data$n / sum(.data$n),
        .by = "team"
      ) |>
      dplyr::select("team", "placement", "probability") |>
      dplyr::arrange(.data$team, .data$placement)

    points_distribution <- iter_team_points |>
      dplyr::count(.data$team, .data$points) |>
      dplyr::mutate(
        probability = .data$n / sum(.data$n),
        .by = "team"
      ) |>
      dplyr::select("team", "points", "probability") |>
      dplyr::arrange(.data$team, .data$points)
  } else {
    final_positions <- tibble::tibble(
      team = character(),
      placement = integer(),
      probability = numeric()
    )
    points_distribution <- tibble::tibble(
      team = character(),
      points = integer(),
      probability = numeric()
    )
  }

  list(
    predicted_matches = predicted_matches,
    team_strengths_quantiles = team_strengths_quantiles,
    round_strengths_quantiles = round_strengths_quantiles,
    home_advantage_quantiles = home_advantage_quantiles,
    final_positions = final_positions,
    points_distribution = points_distribution,
    tournament_placements = tibble::tibble(
      team        = character(),
      round_name  = character(),
      probability = numeric()
    )
  )
}

# Extract per-draw model parameters needed by the cup bracket simulator.
# Returns `list(team = <per-(team, .draw) tibble>, scalar = <per-.draw scalar tibble>)`.
# Values are raw log-scale (NOT exp-transformed) so the simulator can compose
# them additively into Poisson rates exactly as the model does in its
# generated quantities block.
.extract_sim_inputs_pfi <- function(fit, teams) {
  extract_team <- function(var) {
    fit$draws(var) |>
      posterior::as_draws_df() |>
      tibble::as_tibble() |>
      tidyr::pivot_longer(
        c(-".chain", -".draw", -".iteration"),
        names_to  = "name",
        values_to = "value"
      ) |>
      dplyr::mutate(
        team_idx = as.integer(readr::parse_number(.data$name)),
        team     = teams$team[.data$team_idx]
      ) |>
      dplyr::select("team", ".draw", "value")
  }

  team_inputs <- extract_team("cur_offense_away") |>
    dplyr::rename(cur_offense = "value") |>
    dplyr::full_join(
      extract_team("cur_defense_away") |> dplyr::rename(cur_defense = "value"),
      by = c("team", ".draw")
    ) |>
    dplyr::full_join(
      extract_team("home_advantage_off") |>
        dplyr::rename(home_advantage_off = "value"),
      by = c("team", ".draw")
    ) |>
    dplyr::full_join(
      extract_team("home_advantage_def") |>
        dplyr::rename(home_advantage_def = "value"),
      by = c("team", ".draw")
    )

  scalar_inputs <- fit$draws(c(
    "mean_log_goals", "alpha_mu3", "beta_mu3_strength_diff"
  )) |>
    posterior::as_draws_df() |>
    tibble::as_tibble() |>
    dplyr::select(
      ".draw", "mean_log_goals", "alpha_mu3", "beta_mu3_strength_diff"
    )

  list(team = team_inputs, scalar = scalar_inputs)
}

# Build a bracket_state for the cup bracket simulator.
#
# Identifies the 16 R16 teams from the season's cup matches (union of played
# results + upcoming schedule) by sliding-window detection: the first window
# of 8 chronological cup matches involving exactly 16 distinct teams within
# a ≤ 4-day span is treated as the R16 round. Subsequent cup matches in the
# same season that involve only those 16 teams are bracket matches; their
# chronological rank determines round (ranks 1-8 = R16, 9-12 = R8, 13-14 =
# SF, 15 = Final).
#
# Returns a list with:
#   * cup_teams: character(16) — the R16 entrants
#   * rounds: a named list keyed by "R16", "R8", "SF", "Final". Each round
#     entry is itself a list with:
#       - pairings_known: logical. TRUE if the round's matches are listed
#         in results or upcoming schedule; FALSE if KSÍ has not yet drawn it.
#       - matches: tibble(home_team, away_team, venue, known_winner) when
#         pairings_known; NULL otherwise. `known_winner` is the team name
#         for played matches, NA for upcoming.
#
# Returns NULL when fewer than 8 cup matches with 16 distinct teams are
# available (e.g. cup season hasn't reached R16 yet).
.build_bracket_state_pfi <- function(pred_d, results = NULL,
                                     current_season = NULL) {
  empty_or_null <- function(x) is.null(x) || nrow(x) == 0L
  if (empty_or_null(pred_d) && empty_or_null(results)) {
    return(NULL)
  }

  schedule_part <- if (!empty_or_null(pred_d)) {
    pred_d |>
      dplyr::filter(.data$division == "CUP") |>
      dplyr::transmute(
        match_date   = .data$match_date,
        home_team    = .data$home_team,
        away_team    = .data$away_team,
        played       = FALSE,
        known_winner = NA_character_
      )
  } else {
    tibble::tibble(
      match_date = as.Date(character()),
      home_team = character(), away_team = character(),
      played = logical(), known_winner = character()
    )
  }

  results_part <- if (!empty_or_null(results)) {
    res <- results |>
      dplyr::filter(.data$division == "CUP")
    if (!is.null(current_season) && "season" %in% names(res)) {
      res <- res[res$season == current_season, , drop = FALSE]
    }
    if (nrow(res) > 0L && all(c("home_score", "away_score") %in% names(res))) {
      res |>
        dplyr::transmute(
          match_date = .data$match_date,
          home_team = .data$home_team,
          away_team = .data$away_team,
          played = TRUE,
          known_winner = dplyr::case_when(
            .data$home_score > .data$away_score ~ .data$home_team,
            .data$home_score < .data$away_score ~ .data$away_team,
            TRUE ~ NA_character_ # tied at 90' results without ET info;
            # treat as unresolved -> simulator re-runs.
          )
        )
    } else {
      tibble::tibble(
        match_date = as.Date(character()),
        home_team = character(), away_team = character(),
        played = logical(), known_winner = character()
      )
    }
  } else {
    tibble::tibble(
      match_date = as.Date(character()),
      home_team = character(), away_team = character(),
      played = logical(), known_winner = character()
    )
  }

  cup_matches <- dplyr::bind_rows(results_part, schedule_part) |>
    dplyr::distinct(.data$match_date, .data$home_team, .data$away_team,
      .keep_all = TRUE
    ) |>
    dplyr::arrange(.data$match_date)

  if (nrow(cup_matches) < 8L) {
    return(NULL)
  }

  # Identify R16 via sliding-window: first 8-match window with 16 distinct
  # teams in ≤ 4 days.
  cup_teams <- NULL
  for (i in seq_len(nrow(cup_matches) - 7L)) {
    win <- cup_matches[i:(i + 7L), , drop = FALSE]
    teams_in_window <- unique(c(win$home_team, win$away_team))
    span_days <- as.numeric(max(win$match_date) - min(win$match_date))
    if (length(teams_in_window) == 16L && span_days <= 4L) {
      cup_teams <- teams_in_window
      break
    }
  }
  if (is.null(cup_teams)) {
    return(NULL)
  }

  bracket_matches <- cup_matches |>
    dplyr::filter(.data$home_team %in% cup_teams &
      .data$away_team %in% cup_teams) |>
    dplyr::mutate(rank = dplyr::row_number())

  build_round <- function(ranks) {
    m <- bracket_matches |> dplyr::filter(.data$rank %in% ranks)
    if (nrow(m) == length(ranks)) {
      list(
        pairings_known = TRUE,
        matches = tibble::tibble(
          home_team    = m$home_team,
          away_team    = m$away_team,
          venue        = "home",
          known_winner = m$known_winner
        )
      )
    } else {
      list(pairings_known = FALSE, matches = NULL)
    }
  }

  list(
    cup_teams = cup_teams,
    rounds = list(
      R16   = build_round(1:8),
      R8    = build_round(9:12),
      SF    = build_round(13:14),
      Final = build_round(15L)
    )
  )
}

# ---- Public API --------------------------------------------------------------

#' Extract publish-layer summaries from a football iceland fit
#'
#' Writes six Parquet files into the per-fit extracts partition at
#' `data/beliefs/extracts/sport=football/country=iceland/sex={male|female}/fit_date=YYYY-MM-DD/`.
#' The fit covers both Icelandic football divisions (Besta deild + Lengjudeild);
#' each parquet carries a `division` column (`"BD"` / `"LD1"`) so the publisher
#' filters to the cell it's rendering.
#'
#' Files are written under `data/beliefs/extracts/`, **not** `data/beliefs/archive/`,
#' so they don't pollute the canonical `beliefs_archive` table (per-draw-per-match
#' draws written by `model-league.R::fit_league()` for sports without a
#' dedicated extraction layer). Mixing the two trees previously caused
#' arrow/DuckDB schema-unification failures.
#'
#' Per-fit parquets (each with a `division` column):
#'
#' - `predicted_matches.parquet` — per-(division, home_team, away_team,
#'   match_date, home_goals, away_goals) row with the integer-pair occurrence
#'   count across the posterior.
#' - `team_strengths_quantiles.parquet` — 99-quantile band per
#'   (division, team, component, location) where component ∈ {offence, defence,
#'   total} and location ∈ {home, away, avg}. The `avg` row is the per-draw mean
#'   of home/away (computed pre-quantile so intervals reflect the joint
#'   posterior).
#' - `round_strengths_quantiles.parquet` — same 9-cell grid but per
#'   (division, round, team), where `round` is the team's chronological
#'   matchweek *within the division*. Drives the strength trajectory.
#' - `home_advantage_quantiles.parquet` — 99-quantile band per
#'   (division, team, component) for the multiplicative home-advantage parameter
#'   `exp(home_advantage_*)`. The `total` component is `exp(home_advantage_tot / 2)`
#'   matching the publisher's per-side allocation.
#' - `final_positions.parquet` — per-(division, team, placement) probability
#'   over the posterior, pre-computed at extract time because the count
#'   representation above doesn't preserve the cross-match draw alignment that
#'   the simulation requires.
#' - `points_distribution.parquet` — per-(division, team, points) probability,
#'   same reasoning.
#'
#' Together these six Parquets capture everything the football publisher
#' currently reads from the in-memory fit RDS, at roughly 1 MB per fit
#' (both divisions combined) vs. 100–500 MB for the RDS. See
#' `Sports/Knowledge/Publish Pipeline/extraction-layer` in the Metill Obsidian
#' vault for the design rationale.
#'
#' @param fit CmdStanMCMC fit object.
#' @param league League list with `sport == "football"` and `country == "iceland"`.
#' @param sex `"male"` or `"female"`.
#' @param fit_date Date stamped on the extracts partition. Default `Sys.Date()`.
#' @param end_date Training cutoff used for prepare_data() reconstruction.
#'   Default = `fit_date`.
#' @param root Data root. Default `here::here("data")`.
#' @param prep Optional pre-computed `prepare_data()` output. When NULL
#'   (default), reconstructs from disk; pass directly when calling from
#'   `fit_league()` to avoid a redundant prepare_data call (and to
#'   guarantee consistency with the fit).
#' @param extracts_root Optional write root for the per-fit extracts partition.
#'   Defaults to `file.path(root, "beliefs", "extracts")`. Tests can override
#'   this to write into an isolated tempdir while still reading facts from
#'   the real `root`.
#' @param target_divs Character vector of division codes to extract. Defaults
#'   to `c("BD", "LD1", "CUP")` (the full football iceland set). Useful in
#'   tests to extract one division only.
#' @return invisible(NULL). 6 Parquet files written into the extracts partition.
#' @export
extract_football_iceland <- function(fit, league, sex,
                                     fit_date = Sys.Date(),
                                     end_date = fit_date,
                                     root = here::here("data"),
                                     prep = NULL,
                                     extracts_root = NULL,
                                     target_divs = .FOOTBALL_ICELAND_DIVISIONS_PFI) {
  stopifnot(league$sport == "football", league$country == "iceland")
  stopifnot(sex %in% c("male", "female"))
  stopifnot(inherits(fit_date, "Date") || is.character(fit_date))
  stopifnot(
    is.character(target_divs),
    length(target_divs) >= 1L,
    all(target_divs %in% .FOOTBALL_ICELAND_DIVISIONS_PFI)
  )

  if (is.null(prep)) {
    prep <- prepare_data(league, sex, end_date = end_date, root = root)
  }
  teams <- prep$teams
  pred_d <- prep$pred_d

  if (is.null(extracts_root)) {
    extracts_root <- file.path(root, "beliefs", "extracts")
  }
  extracts_dir <- file.path(
    extracts_root,
    paste0("sport=", league$sport),
    paste0("country=", league$country),
    paste0("sex=", sex),
    paste0("fit_date=", format(as.Date(fit_date), "%Y-%m-%d"))
  )
  dir.create(extracts_dir, recursive = TRUE, showWarnings = FALSE)

  results <- read_table(
    "results",
    root   = root,
    filter = list(sport = league$sport, country = league$country, sex = sex)
  )
  results <- results[
    !is.na(results$match_date) & results$match_date <= end_date, ,
    drop = FALSE
  ]
  current_season <- if (nrow(results) > 0L) {
    max(results$season, na.rm = TRUE)
  } else {
    as.integer(format(as.Date(end_date), "%Y"))
  }

  # ---- Cross-division: posterior_goals long + team_strengths_draws + home_adv

  posterior_goals_raw <- fit$draws(c("goals1_pred", "goals2_pred")) |>
    posterior::as_draws_df() |>
    tibble::as_tibble() |>
    tidyr::pivot_longer(
      -c(".draw", ".chain", ".iteration"),
      names_to  = "parameter",
      values_to = "value"
    ) |>
    dplyr::mutate(
      type = dplyr::if_else(
        stringr::str_detect(.data$parameter, "goals1"),
        "home_goals", "away_goals"
      ),
      game_nr = as.integer(
        stringr::str_match(.data$parameter, "\\[(\\d+)\\]$")[, 2]
      )
    ) |>
    dplyr::select(".draw", "type", "game_nr", "value") |>
    tidyr::pivot_wider(names_from = "type", values_from = "value")

  n_pred_fit <- if (nrow(posterior_goals_raw) > 0L) {
    max(posterior_goals_raw$game_nr, na.rm = TRUE)
  } else {
    0L
  }
  n_pred_data <- nrow(pred_d)

  if (n_pred_fit != n_pred_data && n_pred_data > 0L) {
    warning(sprintf(
      paste0(
        "extract_football_iceland: fit was trained with N_pred=%d ",
        "prediction matches but prepare_data returned %d. ",
        "predicted_matches.parquet will be empty for every division."
      ),
      n_pred_fit, n_pred_data
    ))
  }

  posterior_goals_long <- if (n_pred_fit == n_pred_data && n_pred_data > 0L) {
    posterior_goals_raw |>
      dplyr::inner_join(
        pred_d[, c("game_nr", "match_date", "home_team", "away_team", "division")],
        by = "game_nr"
      )
  } else {
    tibble::tibble(
      .draw = integer(), game_nr = integer(),
      home_goals = numeric(), away_goals = numeric(),
      match_date = as.Date(character()),
      home_team = character(), away_team = character(),
      division = character()
    )
  }

  team_strengths_draws <- dplyr::bind_rows(
    .extract_team_draws_pfi(fit, "cur_offense_home", teams, "offence", "home"),
    .extract_team_draws_pfi(fit, "cur_defense_home", teams, "defence", "home"),
    .extract_team_draws_pfi(fit, "cur_strength_home", teams, "total", "home"),
    .extract_team_draws_pfi(fit, "cur_offense_away", teams, "offence", "away"),
    .extract_team_draws_pfi(fit, "cur_defense_away", teams, "defence", "away"),
    .extract_team_draws_pfi(fit, "cur_strength_away", teams, "total", "away")
  )

  extract_home_adv <- function(var, component, transform = identity) {
    fit$draws(var) |>
      posterior::as_draws_df() |>
      tibble::as_tibble() |>
      tidyr::pivot_longer(c(-".chain", -".draw", -".iteration")) |>
      dplyr::mutate(
        team_idx  = as.integer(readr::parse_number(.data$name)),
        team      = teams$team[.data$team_idx],
        component = component,
        value     = exp(transform(.data$value))
      ) |>
      dplyr::select("team", "component", ".draw", "value")
  }

  home_advantage_draws <- dplyr::bind_rows(
    extract_home_adv("home_advantage_off", "offence"),
    extract_home_adv("home_advantage_def", "defence"),
    extract_home_adv("home_advantage_tot", "total",
      transform = function(x) x / 2
    )
  )

  # Cup bracket simulator inputs: per-draw raw model parameters + bracket state.
  # `sim_inputs` is always extracted (cheap, ~5 MB on disk). `bracket_state`
  # unions played results + upcoming schedule to support any entry point —
  # R16 still upcoming, partial R16, R8 onwards once R16 plays, etc.
  sim_inputs <- .extract_sim_inputs_pfi(fit, teams)
  bracket_state <- if ("CUP" %in% target_divs) {
    .build_bracket_state_pfi(pred_d,
      results = results,
      current_season = current_season
    )
  } else {
    NULL
  }

  per_div <- lapply(target_divs, function(target_div) {
    parts <- .extract_division_parquets_pfi(
      target_div           = target_div,
      fit                  = fit,
      teams                = teams,
      results              = results,
      current_season       = current_season,
      posterior_goals_long = posterior_goals_long,
      team_strengths_draws = team_strengths_draws,
      home_advantage_draws = home_advantage_draws,
      n_pred_fit           = n_pred_fit,
      n_pred_data          = n_pred_data,
      sim_inputs           = sim_inputs,
      bracket_state        = bracket_state
    )
    lapply(parts, function(df) dplyr::mutate(df, division = target_div))
  })
  names(per_div) <- target_divs

  file_types <- names(per_div[[1]])
  for (ft in file_types) {
    bound <- dplyr::bind_rows(lapply(per_div, function(d) d[[ft]]))
    arrow::write_parquet(
      bound,
      file.path(extracts_dir, paste0(ft, ".parquet"))
    )
  }

  arrow::write_parquet(
    sim_inputs$team,
    file.path(extracts_dir, "sim_inputs_team.parquet")
  )
  arrow::write_parquet(
    sim_inputs$scalar,
    file.path(extracts_dir, "sim_inputs_scalar.parquet")
  )

  message(sprintf(
    "extract_football_iceland: wrote %d division parquets + 2 sim_inputs parquets to %s [div: %s; bracket_state: %s]",
    length(file_types),
    extracts_dir,
    paste(target_divs, collapse = ", "),
    if (is.null(bracket_state)) "absent (< 8 upcoming cup matches)" else "built"
  ))
  invisible(NULL)
}

#' Load a per-fit football iceland extraction partition
#'
#' Reads the six Parquet files written by [`extract_football_iceland()`] from
#' `data/beliefs/extracts/sport=football/country=iceland/sex=Z/fit_date=D/`,
#' splits each by the in-payload `division` column, and returns a named list
#' keyed by division code.
#'
#' Auto-discovery (default `fit_date = NULL`) walks the `fit_date=*`
#' partitions in descending order and returns the first one that contains
#' all six expected files. BD is always required (the platform always renders
#' the BD page); an absent `"LD1"` slice degrades to empty tibbles for the
#' LD1 cell.
#'
#' @param league League list with `sport == "football"` and
#'   `country == "iceland"`.
#' @param sex `"male"` or `"female"`.
#' @param fit_date `Date` or `NULL`. When `NULL` (default), reads the latest
#'   partition that contains the full six-file extracted set.
#' @param extracts_root Beliefs extracts root.
#'   Default `here::here("data", "beliefs", "extracts")`.
#' @param target_divs Character vector of divisions to load. Defaults to
#'   `c("BD", "LD1", "CUP")`. Returned list always includes a slot per requested
#'   division (with empty-tibble parquets when the `division` filter yields
#'   no rows).
#' @return Named list. Each requested division key (e.g. `"BD"`, `"LD1"`)
#'   maps to a list with the six tibbles
#'   (`predicted_matches`, `team_strengths_quantiles`,
#'   `round_strengths_quantiles`, `home_advantage_quantiles`,
#'   `final_positions`, `points_distribution`) — `division` column dropped
#'   after filtering. Plus `fit_date` (the `Date` of the partition that was
#'   loaded).
#' @export
read_extracted_football <- function(league, sex, fit_date = NULL,
                                    extracts_root = here::here(
                                      "data", "beliefs", "extracts"
                                    ),
                                    target_divs = .FOOTBALL_ICELAND_DIVISIONS_PFI) {
  stopifnot(league$sport == "football", league$country == "iceland")
  stopifnot(sex %in% c("male", "female"))
  stopifnot(
    is.character(target_divs),
    length(target_divs) >= 1L,
    all(target_divs %in% .FOOTBALL_ICELAND_DIVISIONS_PFI)
  )

  file_types <- c(
    "predicted_matches",
    "team_strengths_quantiles",
    "round_strengths_quantiles",
    "home_advantage_quantiles",
    "final_positions",
    "points_distribution"
  )
  expected <- paste0(file_types, ".parquet")

  base <- file.path(
    extracts_root,
    paste0("sport=", league$sport),
    paste0("country=", league$country),
    paste0("sex=", sex)
  )
  if (!dir.exists(base)) {
    stop("No extracts directory at ", base, call. = FALSE)
  }

  .partition_is_complete <- function(fit_dir) {
    all(file.exists(file.path(fit_dir, expected)))
  }

  if (is.null(fit_date)) {
    parts <- list.dirs(base, full.names = TRUE, recursive = FALSE)
    fit_dirs <- parts[grepl("/fit_date=", parts)]
    if (length(fit_dirs) == 0L) {
      stop("No fit_date partitions under ", base, call. = FALSE)
    }
    fit_dates_chr <- sub(".*fit_date=", "", fit_dirs)
    ord <- order(as.Date(fit_dates_chr), decreasing = TRUE)
    fit_dir <- NULL
    for (i in ord) {
      d <- fit_dirs[i]
      if (.partition_is_complete(d)) {
        fit_dir <- d
        break
      }
    }
    if (is.null(fit_dir)) {
      stop(
        "No fit_date partition under ", base,
        " contains a complete extracted set. ",
        "Force-trigger fit.yml or run extract_football_iceland() locally.",
        call. = FALSE
      )
    }
    fit_date_out <- as.Date(sub(".*fit_date=", "", fit_dir))
  } else {
    fit_date_out <- as.Date(fit_date)
    fit_dir <- file.path(
      base, paste0("fit_date=", format(fit_date_out, "%Y-%m-%d"))
    )
    if (!dir.exists(fit_dir)) {
      stop("Extracts partition not found: ", fit_dir, call. = FALSE)
    }
    if (!.partition_is_complete(fit_dir)) {
      stop(
        "Extracts partition ", fit_dir,
        " is incomplete (one or more of: ",
        paste(expected, collapse = ", "), "). ",
        "Re-run extract_football_iceland() against this fit.",
        call. = FALSE
      )
    }
  }

  empty_tibbles <- list(
    predicted_matches = tibble::tibble(
      home_team = character(), away_team = character(),
      match_date = as.Date(character()),
      home_goals = integer(), away_goals = integer(),
      count = integer()
    ),
    team_strengths_quantiles = tibble::tibble(
      team = character(), component = character(), location = character(),
      quantile = integer(), value = numeric()
    ),
    round_strengths_quantiles = tibble::tibble(
      round = integer(), team = character(),
      component = character(), location = character(),
      quantile = integer(), value = numeric()
    ),
    home_advantage_quantiles = tibble::tibble(
      team = character(), component = character(),
      quantile = integer(), value = numeric()
    ),
    final_positions = tibble::tibble(
      team = character(), placement = integer(), probability = numeric()
    ),
    points_distribution = tibble::tibble(
      team = character(), points = integer(), probability = numeric()
    ),
    tournament_placements = tibble::tibble(
      team = character(), round_name = character(),
      probability = numeric()
    )
  )

  # `tournament_placements.parquet` is a soft-required 7th file: produced by
  # extracts since the cup-bracket simulator landed. Older partitions
  # (pre-simulator) don't have it; we degrade gracefully to an empty tibble.
  per_division_file_types <- c(file_types, "tournament_placements")
  parquets <- lapply(per_division_file_types, function(ft) {
    p <- file.path(fit_dir, paste0(ft, ".parquet"))
    if (file.exists(p)) {
      arrow::read_parquet(p)
    } else {
      empty_tibbles[[ft]]
    }
  })
  names(parquets) <- per_division_file_types

  read_one_division <- function(target_div) {
    out <- lapply(per_division_file_types, function(ft) {
      df <- parquets[[ft]]
      if (!"division" %in% names(df) || nrow(df) == 0L) {
        return(empty_tibbles[[ft]])
      }
      df <- df[df$division == target_div, , drop = FALSE]
      df$division <- NULL
      tibble::as_tibble(df)
    })
    names(out) <- per_division_file_types
    out
  }

  out <- lapply(target_divs, read_one_division)
  names(out) <- target_divs

  # Optional shared sim_inputs (per-draw model parameters; not per-division).
  # Absent for pre-simulator partitions; the publisher only reads these when
  # it needs to re-run the simulator with non-default tiebreak / pairing opts.
  sim_inputs_team_path <- file.path(fit_dir, "sim_inputs_team.parquet")
  sim_inputs_scalar_path <- file.path(fit_dir, "sim_inputs_scalar.parquet")
  out$sim_inputs <- if (file.exists(sim_inputs_team_path) &&
    file.exists(sim_inputs_scalar_path)) {
    list(
      team   = tibble::as_tibble(arrow::read_parquet(sim_inputs_team_path)),
      scalar = tibble::as_tibble(arrow::read_parquet(sim_inputs_scalar_path))
    )
  } else {
    NULL
  }

  out$fit_date <- fit_date_out
  out
}
