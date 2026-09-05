#' @include publish-iceland-2dt-helpers.R model-prepare.R storage.R publish-divisions.R publish-format.R
NULL

# Shared extraction primitives for basketball + handball Iceland.
# Both sports use the same Stan family (2D Student-t on signed score diff)
# so the per-fit Parquet extraction is sport-agnostic modulo the sport
# label, the score binning for `goal_diff_distribution`, and the draws
# semantics (basketball: no draws; handball: probabilistic ties around
# zero diff). The per-sport wrappers in extract-{basketball,handball}-
# iceland.R configure these knobs and dispatch here.

# Summarise per-draw long-form `value` into 99-quantile bands keyed by
# `group_keys`. Mirrors `.summarise_quantile_band_pfi()` in extract-
# football-iceland.R but lives here too so the 2DT extractors don't take
# a hidden dependency on the football file's symbols.
.summarise_quantile_band_2dt <- function(draws, group_keys) {
  if (nrow(draws) == 0L) {
    out_cols <- c(group_keys, "quantile", "value")
    return(tibble::tibble(!!!setNames(
      lapply(out_cols, function(x) if (x == "quantile") integer() else if (x == "value") numeric() else character()),
      out_cols
    )))
  }
  # Only the stored grid, not all 99 percentiles -- see PUBLISH_QUANTILE_GRID
  # for why this is wider than what the publisher currently reads.
  probs <- PUBLISH_QUANTILE_GRID / 100
  draws |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_keys))) |>
    dplyr::group_modify(~ tibble::tibble(
      quantile = PUBLISH_QUANTILE_GRID,
      value = unname(stats::quantile(
        .x$value,
        probs = probs,
        names = FALSE
      ))
    )) |>
    dplyr::ungroup()
}

# Bin a continuous goal-diff into score-difference buckets and tabulate
# probability per bucket. Used by the predicted_matches.parquet shape:
# basketball uses a 5-point bucket in [-50, +50]; handball a 2-point in
# [-20, +20]. Returns a long tibble {game_nr, diff, p} where `diff` is
# the bucket centre (integer).
.bin_goal_diff_distribution_2dt <- function(posterior_goals,
                                            bucket_width = 1L,
                                            range_low = -50L,
                                            range_high = 50L) {
  if (nrow(posterior_goals) == 0L) {
    return(tibble::tibble(
      game_nr = integer(),
      diff = integer(),
      p = numeric()
    ))
  }
  bw <- as.integer(bucket_width)
  centres <- seq(range_low, range_high, by = bw)
  posterior_goals |>
    dplyr::mutate(
      diff_raw = .data$home_score - .data$away_score,
      bucket = .bucket_centre_2dt(.data$diff_raw, bw, range_low, range_high)
    ) |>
    dplyr::count(.data$game_nr, .data$bucket, name = "n") |>
    dplyr::mutate(
      total = sum(.data$n),
      .by = "game_nr"
    ) |>
    dplyr::mutate(p = .data$n / .data$total) |>
    dplyr::select(game_nr = "game_nr", diff = "bucket", p = "p") |>
    dplyr::arrange(.data$game_nr, .data$diff)
}

.bucket_centre_2dt <- function(x, bw, low, high) {
  clamped <- pmin(pmax(x, low), high)
  as.integer(round(clamped / bw) * bw)
}

# Extract per-match posterior summaries to a tibble for the
# predicted_matches.parquet artefact. Goal-diff distribution is binned;
# basketball + handball publishers consume this for next_games panels.
# `posterior_goals` is an optional hoist: the caller already computes it for the
# league-table simulation, and pulling goals*_pred twice per extract is the one
# avoidable duplicate read on this path. NULL keeps the standalone contract.
.compute_predicted_matches_2dt <- function(fit, pred_d,
                                           bucket_width = 1L,
                                           bucket_low = -50L,
                                           bucket_high = 50L,
                                           has_ties = FALSE,
                                           tie_threshold = 0,
                                           posterior_goals = NULL) {
  if (nrow(pred_d) == 0L) {
    return(tibble::tibble(
      game_nr = integer(),
      match_date = as.Date(character()),
      division = character(),
      home_team = character(),
      away_team = character(),
      mean_home_goals = numeric(),
      mean_away_goals = numeric(),
      mean_goal_diff = numeric(),
      p_home_win = numeric(),
      p_draw = numeric(),
      p_away_win = numeric(),
      goal_diff_distribution = list()
    ))
  }
  if (is.null(posterior_goals)) {
    posterior_goals <- .compute_posterior_goals_2dt(fit, pred_d)
  }
  if (nrow(posterior_goals) == 0L) {
    return(tibble::tibble(
      game_nr = integer(),
      match_date = as.Date(character()),
      division = character(),
      home_team = character(),
      away_team = character(),
      mean_home_goals = numeric(),
      mean_away_goals = numeric(),
      mean_goal_diff = numeric(),
      p_home_win = numeric(),
      p_draw = numeric(),
      p_away_win = numeric(),
      goal_diff_distribution = list()
    ))
  }

  per_match <- posterior_goals |>
    dplyr::mutate(diff = .data$home_score - .data$away_score) |>
    dplyr::summarise(
      mean_home_goals = mean(.data$home_score),
      mean_away_goals = mean(.data$away_score),
      mean_goal_diff = mean(.data$diff),
      p_home_win = if (isTRUE(has_ties)) {
        mean(.data$diff > tie_threshold)
      } else {
        mean(.data$diff > 0)
      },
      p_draw = if (isTRUE(has_ties)) {
        mean(abs(.data$diff) <= tie_threshold)
      } else {
        0
      },
      p_away_win = if (isTRUE(has_ties)) {
        mean(.data$diff < -tie_threshold)
      } else {
        mean(.data$diff < 0)
      },
      .by = c("game_nr", "match_date", "home_team", "away_team", "division")
    )

  bins <- .bin_goal_diff_distribution_2dt(
    posterior_goals,
    bucket_width = bucket_width,
    range_low = bucket_low,
    range_high = bucket_high
  )
  # tidyr::nest, not group_by + summarise(list(tibble(.data$diff, ...))): inside
  # summarise() the .data pronoun exposes only group keys and columns created so
  # far, so `.data$diff` there errors with "Column `diff` not found".
  bins_nested <- tidyr::nest(bins, goal_diff_distribution = c("diff", "p"))

  per_match |>
    dplyr::left_join(bins_nested, by = "game_nr") |>
    dplyr::arrange(.data$match_date, .data$game_nr)
}

# The six raw (component x location) team-strength blocks, pulled ONCE.
#
# Mirrors football's hoist (R/extract-football-iceland.R:1514-1521): the pull is
# cross-division, so it belongs above the division loop, not inside the quantile
# helper. Inside, a two-division cell would make nine `fit$draws()` calls per
# division against a 300-600 MB fit.
#
# No `avg` block here on purpose -- `avg` is a PER-DRAW mean the quantile helper
# computes, so the interval reflects the joint posterior rather than a post-hoc
# average of two independently-summarised bands.
.extract_team_strength_draws_2dt <- function(fit, teams) {
  dplyr::bind_rows(
    .extract_team_draws_2dt(fit, "cur_offense_home", teams, "offence", "home"),
    .extract_team_draws_2dt(fit, "cur_defense_home", teams, "defence", "home"),
    .extract_team_draws_2dt(fit, "cur_strength_home", teams, "total", "home"),
    .extract_team_draws_2dt(fit, "cur_offense_away", teams, "offence", "away"),
    .extract_team_draws_2dt(fit, "cur_defense_away", teams, "defence", "away"),
    .extract_team_draws_2dt(fit, "cur_strength_away", teams, "total", "away")
  )
}

# The three home-advantage components, pulled ONCE. Same hoist, same reason.
# NOTE the deliberate absence of any transform -- see the B5 block below.
.extract_home_advantage_draws_2dt <- function(fit, teams) {
  extract_one <- function(var, component) {
    fit$draws(var) |>
      posterior::as_draws_df() |>
      tibble::as_tibble() |>
      tidyr::pivot_longer(c(-".chain", -".draw", -".iteration")) |>
      dplyr::mutate(
        team_idx = as.integer(readr::parse_number(.data$name)),
        team = teams$team[.data$team_idx],
        component = component,
        value = .data$value
      ) |>
      dplyr::select("team", "component", ".draw", "value")
  }

  dplyr::bind_rows(
    extract_one("home_advantage_off", "offence"),
    extract_one("home_advantage_def", "defence"),
    extract_one("home_advantage_tot", "total")
  )
}

# Build the 9-cell strength grid (component × location) for the top-
# division teams, quantile-summarised. Replicates football's shape,
# including the hoisted draws argument: the pull is cross-division, the
# semi_join is what makes the band per-division.
.compute_team_strengths_quantiles_2dt <- function(team_strengths_draws,
                                                  current_top_teams) {
  team_strengths_avg <- team_strengths_draws |>
    dplyr::summarise(
      value = mean(.data$value),
      .by = c(".draw", "team", "component")
    ) |>
    dplyr::mutate(location = "avg")

  all_draws <- dplyr::bind_rows(team_strengths_draws, team_strengths_avg) |>
    dplyr::semi_join(current_top_teams, by = "team")

  .summarise_quantile_band_2dt(all_draws, c("team", "component", "location"))
}

# Per-team home-advantage quantile bands.
#
# Shaped like football's home_advantage_quantiles.parquet, but the VALUES are
# on a different scale and must NOT be transformed to match it. Football's
# bivariate Poisson parameterises home advantage as a log-rate, so football's
# extractor exponentiates to recover a multiplier, and halves the total to
# split that multiplier per side. The 2DT models are additive in raw
# points/goals:
#   Stan/basketball_iceland/2d_student_t_scalarsigma.stan:112,116
#     vector<lower = 0>[K] home_advantage_off / _def, prior normal(0, 10),
#     entering the mean linearly at :264
#   :277  home_advantage_tot = home_advantage_off + home_advantage_def
# so there is no log to undo and nothing meaningful to halve. Publish the
# parameter itself.
#
# This function previously carried football's exp() and /2 (B5, spec section
# 8). Measured against the real stored basketball fit: raw totals span
# 1.50..12.07 points, which exp(x/2) published as 2.12..420 -- plausible at
# the bottom, absurd at the top, so it survived review. There is deliberately
# no `transform` argument any more: the parameter is what invited the copy.
# See test-extract-2dt-home-advantage-units.R.
#
# The pull now lives in .extract_home_advantage_draws_2dt(); the units guarantee
# is the composition of the two, and neither half may reintroduce a transform.
.compute_home_advantage_quantiles_2dt <- function(home_advantage_draws,
                                                  current_top_teams) {
  home_adv_draws <- home_advantage_draws |>
    dplyr::semi_join(current_top_teams, by = "team")

  .summarise_quantile_band_2dt(home_adv_draws, c("team", "component"))
}

# Per-team placement probability (1..n_teams) — same shape as football's
# final_positions.parquet. Computed via per-draw simulation of remaining
# matches given the model's posterior; the existing
# .compute_iter_team_points_2dt() handles the per-draw bookkeeping.
.compute_final_positions_2dt <- function(posterior_goals, top_div,
                                         base_points, has_ties,
                                         tie_threshold,
                                         current_top_teams) {
  iter_team_points <- .compute_iter_team_points_2dt(
    posterior_goals,
    top_div = top_div,
    base_points = base_points,
    has_ties = has_ties,
    tie_threshold = tie_threshold
  )
  if (nrow(iter_team_points) == 0L) {
    return(tibble::tibble(
      team = character(),
      placement = integer(),
      probability = numeric()
    ))
  }

  # Rank points -> point difference -> a per-(draw, team) jitter.
  #
  # Ranking on points alone left ties to ROW ORDER, i.e. to whichever tied team
  # happened to have the earlier upcoming fixture in pred_d. With 2 points a
  # win over 22 rounds exact ties are common, so a genuine 0.37/0.36 title race
  # published as 0.62/0.11 -- a confident-looking call that was an artefact of
  # the fixture calendar. Football never had this: it ranks points -> gd -> gf
  # (R/extract-football-iceland.R:554).
  #
  # The jitter settles the residual EXACT (points, point_diff) ties. It must
  # vary per draw, or a team would win every tie in every draw and we would
  # have swapped one systematic bias for another; across 4000 draws it splits
  # the placement mass evenly in expectation. Seeded, and the caller's RNG
  # state is preserved, so output stays reproducible.
  iter_team_points$.tiebreak <- withr::with_preserve_seed({
    set.seed(20260905L)
    stats::runif(nrow(iter_team_points))
  })

  iter_positions <- iter_team_points |>
    dplyr::arrange(
      .data$.draw,
      dplyr::desc(.data$points),
      dplyr::desc(.data$point_diff),
      .data$.tiebreak
    ) |>
    dplyr::mutate(placement = dplyr::row_number(), .by = ".draw")

  n_teams_top <- iter_positions |>
    dplyr::distinct(.data$team) |>
    nrow()

  iter_positions |>
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
}

# Per-team points distribution — same shape as football's points_distribution
# (records only; summary computed at publish time from the raw distribution).
.compute_points_distribution_2dt <- function(posterior_goals, top_div,
                                             base_points, has_ties,
                                             tie_threshold,
                                             current_top_teams) {
  iter_team_points <- .compute_iter_team_points_2dt(
    posterior_goals,
    top_div = top_div,
    base_points = base_points,
    has_ties = has_ties,
    tie_threshold = tie_threshold
  )
  if (nrow(iter_team_points) == 0L) {
    return(tibble::tibble(
      team = character(),
      points = integer(),
      probability = numeric()
    ))
  }

  iter_team_points |>
    dplyr::count(.data$team, .data$points, name = "n") |>
    dplyr::mutate(
      probability = .data$n / sum(.data$n),
      .by = "team"
    ) |>
    dplyr::select("team", "points", "probability") |>
    dplyr::arrange(.data$team, .data$points)
}

# Which upcoming fixtures of `division` still belong to the REGULAR season.
#
# The played-rows cut (`.regular_season_results()`) handles history; this is its
# forward half. A basketball cell's `pred_d` carries playoff fixtures inside the
# league division exactly as its results do, and
# `.compute_iter_team_points_2dt()` would add every one of them to the simulated
# table. Walk the fixtures in date order, keeping one only while BOTH teams stay
# at or below the boundary.
#
# `predicted_matches.parquet` is built from the UNCUT set on purpose: a next
# game is a next game, and the platform's next_games panel should show a playoff
# fixture. Only the league-table simulation is capped.
#
# NA `n_rounds` means no cap at all -- an unconfigured, genuinely irregular cell
# (basketball female 1D) or a cup. Dropping every upcoming fixture there would
# be far worse than counting a post-season one, and the caller surfaces the
# caveat through `.publish_n_rounds()$source`.
.regular_season_game_nrs_2dt <- function(pred_d, division, played_results,
                                         n_rounds) {
  rows <- pred_d[pred_d$division == division, , drop = FALSE]
  if (nrow(rows) == 0L) {
    return(integer())
  }
  if (length(n_rounds) != 1L || is.na(n_rounds)) {
    return(rows$game_nr)
  }
  rows <- rows[order(rows$match_date, rows$game_nr), , drop = FALSE]

  teams <- unique(c(
    played_results$home_team, played_results$away_team,
    rows$home_team, rows$away_team
  ))
  tally <- stats::setNames(integer(length(teams)), teams)
  played <- table(c(played_results$home_team, played_results$away_team))
  if (length(played) > 0L) {
    tally[names(played)] <- as.integer(played)
  }

  keep <- logical(nrow(rows))
  for (i in seq_len(nrow(rows))) {
    home <- rows$home_team[[i]]
    away <- rows$away_team[[i]]
    if (tally[[home]] < n_rounds && tally[[away]] < n_rounds) {
      tally[[home]] <- tally[[home]] + 1L
      tally[[away]] <- tally[[away]] + 1L
      keep[i] <- TRUE
    }
  }
  rows$game_nr[keep]
}

# Shared orchestrator: takes a fit + sport-specific config and writes one
# parquet per file type into the partition, each division-keyed file carrying a
# `division` payload column covering every code in
# `config/leagues.yml::<key>.publish_divisions[[sex]]`.
#
# Shaped exactly like football's extract_football_iceland() /
# .extract_division_parquets_pfi() pair: everything CROSS-division (the fit
# pulls, prepare_data, the results read, posterior_goals, predicted_matches) is
# computed ONCE above the loop, and the loop body only slices. The alternative
# -- pulling inside -- is nine `fit$draws()` calls per division against a
# 300-600 MB fit.
.extract_2dt_iceland_pfi <- function(fit, league, sex,
                                     sport,
                                     key,
                                     bucket_width = 1L,
                                     bucket_low = -50L,
                                     bucket_high = 50L,
                                     has_ties = FALSE,
                                     tie_threshold = 0,
                                     fit_date = Sys.Date(),
                                     end_date = fit_date,
                                     root = here::here("data"),
                                     extracts_root = NULL,
                                     prep = NULL) {
  stopifnot(sex %in% c("male", "female"))
  stopifnot(sport %in% c("basketball", "handball"))
  divisions <- .iceland_division_codes(key, sex)
  expected_meetings <- .iceland_division_expected_meetings(key, sex)
  regular_season_rounds <- .iceland_division_regular_season_rounds(key, sex)
  division_is_cup <- .iceland_division_is_cup(key, sex)

  if (is.null(extracts_root)) {
    extracts_root <- file.path(root, "beliefs", "extracts")
  }
  partition <- file.path(
    extracts_root,
    paste0("sport=", sport),
    paste0("country=", league$country),
    paste0("sex=", sex),
    paste0("fit_date=", format(as.Date(fit_date), "%Y-%m-%d"))
  )
  dir.create(partition, recursive = TRUE, showWarnings = FALSE)

  if (is.null(prep)) {
    prep <- prepare_data(league, sex, end_date = end_date, root = root)
  }
  teams <- prep$teams
  pred_d <- prep$pred_d

  # WHY this guard: the per-round strength trajectory indexes the fit's
  # `offense[r, k]` with each team's cumulative appearance index derived from
  # the `results` set read below. That index only equals the model's own
  # `round1`/`round2` (R/model-prepare.R:212-221) while the two sets are the
  # same set. `prepare_data()` applies `training_filter` and this extractor does
  # not, so a filtered league would desynchronise them and the trajectory would
  # silently read a neighbouring round. Verified in config/leagues.yml: only
  # football_iceland carries a training_filter, so this holds today and this
  # line is what makes a future addition abort instead of publish nonsense.
  stopifnot(is.null(league$training_filter))

  results <- read_table(
    "results",
    root = root,
    filter = list(sport = league$sport, country = league$country, sex = sex)
  )
  results <- results[
    !is.na(results$match_date) & results$match_date <= end_date, ,
    drop = FALSE
  ]
  results <- results[
    !is.na(results$home_score) & !is.na(results$away_score), ,
    drop = FALSE
  ]
  # Same ordering prepare_data() applies before building its round index, so
  # the appearance indices agree row-for-row rather than by luck of the
  # parquet scan order.
  results <- results[order(results$match_date), , drop = FALSE]

  schedules <- read_table(
    "schedules",
    root = root,
    filter = list(sport = league$sport, country = league$country, sex = sex)
  )

  current_season <- if (nrow(results) > 0L) {
    max(results$season, na.rm = TRUE)
  } else {
    as.integer(format(as.Date(end_date), "%Y"))
  }

  # ---- Cross-division inputs, computed once --------------------------------
  posterior_goals <- .compute_posterior_goals_2dt(fit, pred_d)
  team_strengths_draws <- .extract_team_strength_draws_2dt(fit, teams)
  home_advantage_draws <- .extract_home_advantage_draws_2dt(fit, teams)

  # predicted_matches is cross-division and ALREADY carries `division` from
  # pred_d, so it is filtered rather than stamped -- mutating a second division
  # column onto it would silently overwrite the fixture's own division.
  predicted_matches <- .compute_predicted_matches_2dt(
    fit, pred_d,
    bucket_width = bucket_width,
    bucket_low = bucket_low,
    bucket_high = bucket_high,
    has_ties = has_ties,
    tie_threshold = tie_threshold,
    posterior_goals = posterior_goals
  )
  predicted_matches <- predicted_matches[
    predicted_matches$division %in% divisions, ,
    drop = FALSE
  ]

  # ---- Per-division slices -------------------------------------------------
  per_div <- lapply(divisions, function(div) {
    # THE REGULAR-SEASON CUT (D3). Basketball embeds its urslitakeppni in the
    # league division -- KKI packages it as extra rounds inside the SAME
    # season_id (R/ingest-kki-basketball.R:23-24) -- so without this the
    # published table is simulated on post-season points. Measured on
    # data/facts/results season 2026: male BD 162 rows -> 132, male 1D
    # 159 -> 132, female BD 137 -> 90; all four handball cells unchanged,
    # because handball's playoff is a separate division (`PO`).
    #
    # There is exactly ONE boundary function in the repo, in R/publish-format.R,
    # and the publisher calls the same one: the extractor's cut and the
    # publisher's cut must be the same cut or standings and final_positions
    # disagree about which matches counted. WS8 applies it; WS10 re-derives the
    # NUMBER from the same helper rather than from played + remaining, which
    # would publish 35 rounds for a 22-round division.
    rounds <- .publish_n_rounds(
      results = results,
      schedules = schedules,
      season = current_season,
      division_codes = div,
      end_date = as.Date(end_date),
      expected_meetings = expected_meetings[[div]],
      regular_season_rounds = regular_season_rounds[[div]],
      is_cup = isTRUE(division_is_cup[[div]])
    )

    top_results <- results[
      results$season == current_season & results$division == div, ,
      drop = FALSE
    ]
    top_results <- .regular_season_cut(top_results, rounds)

    # The forward half of the same cut. predicted_matches stays UNCUT.
    keep_game_nrs <- .regular_season_game_nrs_2dt(
      pred_d, div, top_results, rounds$n_rounds
    )
    # Other divisions' rows are kept so the draw index stays complete even when
    # this division has nothing left to play.
    posterior_goals_div <- posterior_goals[
      !(posterior_goals$division == div &
        !(posterior_goals$game_nr %in% keep_game_nrs)), ,
      drop = FALSE
    ]

    current_top_teams <- if (nrow(top_results) > 0L) {
      top_results |>
        dplyr::select("home_team", "away_team") |>
        tidyr::pivot_longer(c("home_team", "away_team"), values_to = "team") |>
        dplyr::distinct(.data$team)
    } else {
      tibble::tibble(team = character())
    }

    base_points <- .compute_base_points_2dt(
      top_results,
      has_ties = has_ties,
      tie_threshold = tie_threshold
    )

    # ---- round_strengths_quantiles ----------------------------------------
    # Shaped after football's block (R/extract-football-iceland.R:127-158) and
    # calling the SAME helper: all three Stan models declare
    # `array[N_rounds] vector[K] offense` / `defense` plus `vector[K]
    # home_advantage_off` / `_def`, so no variable-name parameterisation is
    # needed and none is done. The earlier claim that a 2DT round trajectory was
    # impossible was wrong about the models, and this comment is what stops it
    # being re-derived.
    #
    # `results` is passed UNCUT and the cut is applied to the OUTPUT instead.
    # The helper derives each team's global round with a row_number() over the
    # results it is handed, and that index addresses `offense[r, k]` -- so it
    # must see the same set `prepare_data()` modelled. Cutting rows out of the
    # input would renumber every later appearance and silently shift the
    # trajectory onto neighbouring rounds.
    #
    # The output `round` is the team's own division matchweek, so the cut is a
    # PER-TEAM cap: a team keeps as many matchweeks as it has rows surviving
    # `.regular_season_results()`. That is the same row set the league table is
    # built from, which a flat `round <= n_rounds` filter would not be -- the
    # boundary counts rounds, and a team with games in hand has fewer
    # appearances than the round number its matches carry.
    trajectory_long <- .compute_team_strength_trajectory(
      fit = fit,
      results = results,
      teams = teams,
      current_top_teams = current_top_teams,
      current_season = current_season,
      top_div = div
    )
    if (nrow(trajectory_long) > 0L) {
      regular_appearances <- table(c(
        top_results$home_team, top_results$away_team
      ))
      cap <- regular_appearances[trajectory_long$team]
      trajectory_long <- trajectory_long[
        !is.na(cap) & trajectory_long$round <= as.integer(cap), ,
        drop = FALSE
      ]
    }

    round_strengths_quantiles <- if (nrow(trajectory_long) > 0L) {
      trajectory_avg <- trajectory_long |>
        dplyr::summarise(
          value = mean(.data$value),
          .by = c(".draw", "round", "team", "component")
        ) |>
        dplyr::mutate(location = "avg")

      dplyr::bind_rows(trajectory_long, trajectory_avg) |>
        .summarise_quantile_band_2dt(
          c("round", "team", "component", "location")
        )
    } else {
      tibble::tibble(
        round = integer(), team = character(),
        component = character(), location = character(),
        quantile = integer(), value = numeric()
      )
    }

    list(
      team_strengths_quantiles = .compute_team_strengths_quantiles_2dt(
        team_strengths_draws, current_top_teams
      ),
      round_strengths_quantiles = round_strengths_quantiles,
      home_advantage_quantiles = .compute_home_advantage_quantiles_2dt(
        home_advantage_draws, current_top_teams
      ),
      final_positions = .compute_final_positions_2dt(
        posterior_goals_div, div, base_points,
        has_ties, tie_threshold,
        current_top_teams
      ),
      points_distribution = .compute_points_distribution_2dt(
        posterior_goals_div, div, base_points,
        has_ties, tie_threshold,
        current_top_teams
      )
    )
  })
  per_div <- lapply(seq_along(divisions), function(i) {
    lapply(per_div[[i]], function(df) dplyr::mutate(df, division = divisions[i]))
  })
  names(per_div) <- divisions

  # Predicted_matches has a list-column (goal_diff_distribution) which arrow
  # handles natively.
  arrow::write_parquet(
    predicted_matches,
    file.path(partition, "predicted_matches.parquet")
  )
  for (ft in names(per_div[[1]])) {
    arrow::write_parquet(
      dplyr::bind_rows(lapply(per_div, function(d) d[[ft]])),
      file.path(partition, paste0(ft, ".parquet"))
    )
  }

  # fit_meta describes the FIT, not a cell, so it is the one file in the
  # partition with NO `division` column and must not enter the loop above. It
  # carries the numbers the publisher would otherwise need a 300-600 MB fit in
  # memory to recompute.
  arrow::write_parquet(
    .fit_meta_tibble(fit, fit_date, league$stan_model, sport),
    file.path(partition, "fit_meta.parquet")
  )

  invisible(NULL)
}
