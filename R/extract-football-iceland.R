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

# Per-sex publish division codes for football iceland.
#
# Reads `config/leagues.yml::football_iceland.publish_divisions[[sex]]` and
# returns a character vector of `code` values (canonical Stan/results division
# names — `BD`, `LD1`, `LD2`, `LD3`, `CUP`, etc.). Order is the YAML order.
#
# This replaces the pre-2026-05-24 file-scope constant
# `.FOOTBALL_ICELAND_DIVISIONS_PFI <- c("BD", "LD1", "CUP")`. The constant
# couldn't represent per-sex asymmetry (men publish LD2 + LD3; women publish
# LD2 only — no women's 3. deild exists in Iceland).
#
# CUP rows skip the league-table simulation (final_positions /
# points_distribution) since a knockout has no points table — those parquets
# are written as empty tibbles for the CUP partition.
.football_iceland_division_codes <- function(sex) {
  stopifnot(sex %in% c("male", "female"))
  cfg <- load_leagues()[["football_iceland"]][["publish_divisions"]][[sex]]
  if (is.null(cfg) || length(cfg) == 0L) {
    stop(
      ".football_iceland_division_codes: no publish_divisions[\"",
      sex,
      "\"] entry in config/leagues.yml.",
      call. = FALSE
    )
  }
  vapply(cfg, function(d) d$code, character(1))
}

# Per-sex map from canonical division code -> URL/dir slug.
# Returns a named character vector: c(BD = "bd", LD1 = "ld", LD2 = "2deild", ...)
# Used by publish_football_iceland() to build output directory names matching
# the metill-platform consumer's URL slugs.
.football_iceland_division_slugs <- function(sex) {
  stopifnot(sex %in% c("male", "female"))
  cfg <- load_leagues()[["football_iceland"]][["publish_divisions"]][[sex]]
  if (is.null(cfg) || length(cfg) == 0L) {
    stop(
      ".football_iceland_division_slugs: no publish_divisions[\"",
      sex,
      "\"] entry in config/leagues.yml.",
      call. = FALSE
    )
  }
  setNames(
    vapply(cfg, function(d) d$slug, character(1)),
    vapply(cfg, function(d) d$code, character(1))
  )
}

# Per-sex map from canonical division code -> Icelandic display label.
# Returns a named character vector: c(BD = "Besta deild", LD1 = "Lengjudeild", ...)
# Used by publish_football_iceland() to populate `meta.json::league`.
.football_iceland_division_labels <- function(sex) {
  stopifnot(sex %in% c("male", "female"))
  cfg <- load_leagues()[["football_iceland"]][["publish_divisions"]][[sex]]
  if (is.null(cfg) || length(cfg) == 0L) {
    stop(
      ".football_iceland_division_labels: no publish_divisions[\"",
      sex,
      "\"] entry in config/leagues.yml.",
      call. = FALSE
    )
  }
  setNames(
    vapply(cfg, function(d) d$label_is, character(1)),
    vapply(cfg, function(d) d$code, character(1))
  )
}

# Static map: canonical division code -> short ASCII badge code for
# `next_games.json::division_code` (client-side filter key on metill-platform).
# Values MUST match the schema regex ^[A-Z][A-Z0-9_]*$ at
# `config/publish-schemas/football/next_games.schema.json` -- regression-tested
# in tests/testthat/test-publish-divisions-config.R. The platform's
# DIVISIONS dict at app/routes/ithrottir.py mirrors these codes; coordinate
# any change there.
.football_iceland_division_code_labels <- function() {
  c(
    BD = "BD", LD1 = "LD", LD2 = "D2", LD3 = "D3",
    LD4 = "D4", CUP = "MB",
    BD_UPPER_PO = "BDU", BD_LOWER_PO = "BDL",
    LD1_PO = "LDP"
  )
}

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
                                           bracket_state = NULL,
                                           season_schedule = NULL,
                                           fit_date = NULL) {
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
        # Pairing seed derived from fit_date so each fit gets a stable but
        # distinct draw — pre-2026-05-16 the seed was hardcoded 42L, which
        # meant two consecutive fits' tournament_placements reflected
        # identical pairing draws (only strength deltas), masking the
        # bracket-sensitivity of the cup forecast. Audit publish I4.
        seed_int <- if (!is.null(fit_date)) {
          as.integer(format(as.Date(fit_date), "%Y%m%d"))
        } else {
          42L
        }
        simulate_cup_bracket(
          sim_inputs_team   = sim_inputs$team,
          sim_inputs_scalar = sim_inputs$scalar,
          bracket_state     = bracket_state,
          pairing_seed      = seed_int
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

  # ---- 5 + 6. final_positions + points_distribution (FULL remaining season) -
  # Simulate every UNPLAYED fixture of the season from the posterior (frozen
  # latest-round strengths, identical to the model's own match prediction),
  # add the realised table, and rank to season-end placements. Replaces the
  # previous logic, which integrated only the model's 14-day prediction window
  # (~2 rounds) -- a "position after the next ~2 rounds" forecast mislabelled
  # as the final table. See `simulate_league_season()`.

  br <- .league_base_and_remaining_pfi(
    top_results, current_top_teams, season_schedule, target_div
  )
  base_standings <- br$base_standings
  remaining_fixtures <- br$remaining_fixtures

  has_sim_inputs <- !is.null(sim_inputs) &&
    is.data.frame(sim_inputs$team) && nrow(sim_inputs$team) > 0L
  teams_covered <- has_sim_inputs && nrow(base_standings) > 0L &&
    all(base_standings$team %in% sim_inputs$team$team)
  if (has_sim_inputs && nrow(base_standings) > 0L && !teams_covered) {
    warning(sprintf(
      "final_positions[%s]: %d league team(s) lack strength draws; skipping season simulation.",
      target_div, sum(!(base_standings$team %in% sim_inputs$team$team))
    ), call. = FALSE)
  }

  if (teams_covered) {
    season_sim <- simulate_league_season(
      sim_inputs_team    = sim_inputs$team,
      sim_inputs_scalar  = sim_inputs$scalar,
      remaining_fixtures = remaining_fixtures,
      base_standings     = base_standings
    )
    final_positions <- season_sim$final_positions
    points_distribution <- season_sim$points_distribution
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

# Build the realised league table (points/GD/GF from played matches, every
# current-division team present) plus the unplayed fixtures (this division's
# scheduled matches not yet played, deduped on the ordered pair). Shared by the
# daily extract and the per-round backfill so the two never diverge.
.league_base_and_remaining_pfi <- function(played, current_top_teams,
                                           season_schedule, target_div) {
  played <- played[
    !is.na(played$home_score) & !is.na(played$away_score), ,
    drop = FALSE
  ]
  realised <- if (nrow(played) > 0L) {
    played |>
      dplyr::mutate(
        result = dplyr::case_when(
          .data$home_score > .data$away_score ~ "home",
          .data$home_score < .data$away_score ~ "away",
          TRUE ~ "tie"
        )
      ) |>
      tidyr::pivot_longer(c("home_team", "away_team"),
        names_to = "loc", values_to = "team"
      ) |>
      dplyr::mutate(
        loc = dplyr::if_else(.data$loc == "home_team", "home", "away"),
        gf = dplyr::if_else(.data$loc == "home", .data$home_score, .data$away_score),
        ga = dplyr::if_else(.data$loc == "home", .data$away_score, .data$home_score),
        pts = dplyr::case_when(
          .data$result == "tie" ~ 1L,
          .data$result == .data$loc ~ 3L,
          TRUE ~ 0L
        )
      ) |>
      dplyr::summarise(
        base_points = as.integer(sum(.data$pts)),
        base_gf = as.integer(sum(.data$gf)),
        base_ga = as.integer(sum(.data$ga)),
        .by = "team"
      ) |>
      dplyr::mutate(base_gd = .data$base_gf - .data$base_ga)
  } else {
    tibble::tibble(
      team = character(), base_points = integer(),
      base_gf = integer(), base_ga = integer(), base_gd = integer()
    )
  }

  base_standings <- current_top_teams |>
    dplyr::left_join(realised, by = "team") |>
    dplyr::mutate(
      base_points = dplyr::coalesce(.data$base_points, 0L),
      base_gf = dplyr::coalesce(.data$base_gf, 0L),
      base_gd = dplyr::coalesce(.data$base_gd, 0L)
    ) |>
    dplyr::select("team", "base_points", "base_gd", "base_gf")

  remaining_fixtures <- if (!is.null(season_schedule) && nrow(season_schedule) > 0L) {
    played_pair <- paste(played$home_team, played$away_team)
    season_schedule |>
      dplyr::filter(
        .data$division == target_div,
        .data$home_team %in% current_top_teams$team,
        .data$away_team %in% current_top_teams$team
      ) |>
      dplyr::mutate(.pair = paste(.data$home_team, .data$away_team)) |>
      dplyr::filter(!(.data$.pair %in% played_pair)) |>
      dplyr::arrange(dplyr::desc(.data$match_date)) |>
      dplyr::distinct(.data$.pair, .keep_all = TRUE) |>
      dplyr::select("home_team", "away_team")
  } else {
    tibble::tibble(home_team = character(), away_team = character())
  }

  list(base_standings = base_standings, remaining_fixtures = remaining_fixtures)
}

# Extract per-draw model parameters needed by the cup bracket simulator.
# Returns `list(team = <per-(team, .draw) tibble>, scalar = <per-.draw scalar tibble>)`.
# Values are raw log-scale (NOT exp-transformed) so the simulator can compose
# them additively into Poisson rates exactly as the model does in its
# generated quantities block.
.extract_sim_inputs_pfi <- function(fit, teams) {
  # WHY (issue #14): when `teams` (from prepare_data) has fewer rows than the
  # fit's team-indexed parameter count — typically because a saved fit is
  # paired with a freshly-built `prep` whose 365-day training filter has
  # aged out some of the fit's original teams — `teams$team[team_idx]`
  # injects NAs for the overflow indices. dplyr full_join treats NA==NA as a
  # match key, so the join chain below would perform an NA-cartesian
  # explosion (~80K × 80K rows) and blow past R's 16 GB vector memory ceiling.
  # We drop NA-team rows inside extract_team() before they reach the join.
  # Production never has a mismatch because fit_league() pairs each fit with
  # the prep that built its stan_data; this is purely safety for stale-fit
  # callers (ad-hoc analysis, backfill scripts).
  n_teams_fit <- length(posterior::variables(fit$draws("cur_offense_away")))
  if (n_teams_fit != nrow(teams)) {
    cli::cli_warn(
      ".extract_sim_inputs_pfi: fit has {n_teams_fit} team-indexed parameters but `teams` has {nrow(teams)} rows. Dropping {n_teams_fit - nrow(teams)} orphan team(s) from sim_inputs. Pair the fit with the prep that built its stan_data to silence this."
    )
  }

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
      # NA-safe join: drop orphan rows where team_idx exceeds nrow(teams).
      # See WHY block above.
      dplyr::filter(!is.na(.data$team)) |>
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

  # KSÍ TBD placeholder stubs ("Undanúrslit" vs ".") are parse-filtered at
  # ingest since 2026-05-13, but legacy rows persist in the schedules store
  # and would poison the window-consistency check below ("." is never a
  # window team). Results can't carry them (score filter), so this only
  # ever drops schedule-side stubs.
  cup_matches <- cup_matches[
    cup_matches$home_team != "." & cup_matches$away_team != ".", ,
    drop = FALSE
  ]

  # Reschedule ghosts: the schedules store keys on match_date, so a moved
  # fixture leaves its old-dated row behind (retracted at ingest since
  # 2026-06, but ghosts already baked into a fit's pred_d still reach this
  # builder). In a single-elimination cup a pairing meets at most once per
  # season, so two rows with the same unordered pair are the same tie —
  # keep the played row if any, else the latest-dated one (reschedules are
  # re-announced at the new date far more often than pulled forward).
  cup_matches <- cup_matches |>
    dplyr::mutate(
      .pair = paste(
        pmin(.data$home_team, .data$away_team),
        pmax(.data$home_team, .data$away_team)
      )
    ) |>
    dplyr::arrange(dplyr::desc(.data$played), dplyr::desc(.data$match_date)) |>
    dplyr::distinct(.data$.pair, .keep_all = TRUE) |>
    dplyr::select(-".pair") |>
    dplyr::arrange(.data$match_date)

  if (nrow(cup_matches) < 8L) {
    return(NULL)
  }

  # Identify the R16 via sliding window + subset-consistency: among windows
  # of 8 consecutive matches with 16 distinct teams inside ≤ 4 days, the
  # true R16 is the one whose LATER matches involve only those 16 teams.
  # Early qualifying rounds also produce 16-distinct windows (the 2026
  # Mjólkurbikar's March round hijacked the original first-hit heuristic
  # and the bracket simulated 16 minnows — no Besta-deild team at all), but
  # later rounds then field teams from outside the window, so the
  # consistency test rejects them. Keep the LAST passing window; while the
  # R16 field is not yet known no window passes and the builder returns
  # NULL — the publisher then ships empty placements rather than a
  # fictional bracket.
  cup_teams <- NULL
  window_start <- NA_integer_
  for (i in seq_len(nrow(cup_matches) - 7L)) {
    win <- cup_matches[i:(i + 7L), , drop = FALSE]
    teams_in_window <- unique(c(win$home_team, win$away_team))
    span_days <- as.numeric(max(win$match_date) - min(win$match_date))
    if (length(teams_in_window) != 16L || span_days > 4L) {
      next
    }
    later <- cup_matches[-seq_len(i + 7L), , drop = FALSE]
    if (nrow(later) > 0L &&
      !all(c(later$home_team, later$away_team) %in% teams_in_window)) {
      next
    }
    cup_teams <- teams_in_window
    window_start <- i
  }
  if (is.null(cup_teams)) {
    return(NULL)
  }

  # Anchor the bracket at the window start: in a knockout two R16 teams
  # cannot have met in an earlier round (one would have been eliminated),
  # so this is belt-and-braces against stray early-season rows.
  bracket_matches <- cup_matches[
    seq.int(window_start, nrow(cup_matches)), ,
    drop = FALSE
  ] |>
    dplyr::filter(.data$home_team %in% cup_teams &
      .data$away_team %in% cup_teams) |>
    dplyr::mutate(rank = dplyr::row_number())

  # Resolve 90' ties via next-round participation: the results store has no
  # extra-time/shootout outcome (ÍA 2-2 Grindavík stays a tie on paper),
  # but in a knockout the advancing team is whichever of the two appears in
  # a later bracket match (ÍA hosts the QF, so ÍA advanced). While the next
  # round is undrawn the winner stays NA and the simulator re-tosses the
  # tie at the model's lambdas.
  na_idx <- which(bracket_matches$played & is.na(bracket_matches$known_winner))
  for (j in na_idx) {
    later_rows <- bracket_matches[
      bracket_matches$rank > bracket_matches$rank[j], ,
      drop = FALSE
    ]
    cand <- intersect(
      c(bracket_matches$home_team[j], bracket_matches$away_team[j]),
      c(later_rows$home_team, later_rows$away_team)
    )
    if (length(cand) == 1L) {
      bracket_matches$known_winner[j] <- cand
    }
  }

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
#' @param target_divs Character vector of division codes to extract. When
#'   `NULL` (default), resolves to the per-sex publish set from
#'   `config/leagues.yml::football_iceland.publish_divisions[[sex]]` —
#'   `c("BD", "LD1", "LD2", "LD3", "CUP")` for male, `c("BD", "LD1", "LD2", "CUP")`
#'   for female. Pass an explicit subset in tests to extract one division only.
#' @return invisible(NULL). 6 Parquet files written into the extracts partition.
#' @export
extract_football_iceland <- function(fit, league, sex,
                                     fit_date = Sys.Date(),
                                     end_date = fit_date,
                                     root = here::here("data"),
                                     prep = NULL,
                                     extracts_root = NULL,
                                     target_divs = NULL) {
  stopifnot(league$sport == "football", league$country == "iceland")
  stopifnot(sex %in% c("male", "female"))
  stopifnot(inherits(fit_date, "Date") || is.character(fit_date))
  if (is.null(target_divs)) {
    target_divs <- .football_iceland_division_codes(sex)
  }
  stopifnot(
    is.character(target_divs),
    length(target_divs) >= 1L,
    all(target_divs %in% .football_iceland_division_codes(sex))
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

  # Full-season schedule for the league final-position simulation. Read once
  # (all divisions); `.extract_division_parquets_pfi()` filters to its division
  # and anti-joins played fixtures. Best-effort: a missing schedules store
  # degrades to base-table-only placements rather than failing the extract.
  season_schedule <- tryCatch(
    read_table(
      "schedules",
      root = root,
      filter = list(
        sport = league$sport, country = league$country, sex = sex
      )
    ),
    error = function(e) NULL
  )
  if (!is.null(season_schedule) && nrow(season_schedule) > 0L) {
    season_schedule <- season_schedule[
      !is.na(season_schedule$match_date) &
        season_schedule$season == current_season, ,
      drop = FALSE
    ]
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
      bracket_state        = bracket_state,
      season_schedule      = season_schedule,
      fit_date             = fit_date
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
#' @param target_divs Character vector of divisions to load. When `NULL`
#'   (default), resolves to the per-sex publish set from
#'   `config/leagues.yml::football_iceland.publish_divisions[[sex]]`. Returned
#'   list always includes a slot per requested division (with empty-tibble
#'   parquets when the `division` filter yields no rows).
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
                                    target_divs = NULL) {
  stopifnot(league$sport == "football", league$country == "iceland")
  stopifnot(sex %in% c("male", "female"))
  if (is.null(target_divs)) {
    target_divs <- .football_iceland_division_codes(sex)
  }
  stopifnot(
    is.character(target_divs),
    length(target_divs) >= 1L,
    all(target_divs %in% .football_iceland_division_codes(sex))
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
