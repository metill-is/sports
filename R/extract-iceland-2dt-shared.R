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
# Tie handling for a 2DT sport, from the publish profile.
#
# NOT from `league$betting$scoring`, which is where this used to be read.
# run_fit_targets() hands the extractor a WHITELISTED league slice built from
# c("sport", "country", "sexes", "active", "stan_model", "data_source"), plus
# `training_filter` when set -- no `betting`. So `isTRUE(NULL)` gave
# has_ties = FALSE and the is.null() fallback gave tie_threshold = 0 on every
# production fit, and handball published p_draw = 0 for every match while its
# own meta.points.draw said 1. The season simulation was the worse half: it ran
# over a league where a draw could not occur, so every published points total
# came out even.
#
# The profile is keyed by sport and resolved on demand, so nothing can strip it
# in transit. It is also already the source of truth for meta.points, which is
# what made the contradiction visible in the first place.
#
# Note both halves are needed: 2DT posterior scores are continuous
# (multi_student_t_rng, never rounded), so P(diff == 0) is exactly 0 and
# has_ties = TRUE at a threshold of 0 is indistinguishable from FALSE.
.tie_params_pfi <- function(sport) {
  profile <- sport_publish_profile(sport)
  list(
    has_ties = isTRUE(profile$has_ties),
    tie_threshold = if (is.null(profile$tie_threshold)) {
      0
    } else {
      profile$tie_threshold
    }
  )
}

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
# Mirrors football's hoist (the `team_strengths_draws` pull in
# extract_football_iceland()): the pull is cross-division, so it belongs above
# the division loop, not inside the quantile helper. Inside, a two-division cell
# would make nine `fit$draws()` calls per division against a 300-600 MB fit.
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
#
# Not called by the extractor since 2026-09-16 -- the season table comes from
# simulate_league_season() -- but kept: it is the Stan-window oracle that
# test-simulate-2dt-equivalence.R compares the R generators against.
# test-extract-2dt-tiebreak.R and the direct-call tests in
# test-extract-2dt-divisions.R still exercise it (and
# .compute_points_distribution_2dt()), but they now guard the oracle only,
# not the published tables.
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
  # (`.league_split_state_pfi()`).
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
                                     has_ties = NULL,
                                     tie_threshold = NULL,
                                     fit_date,
                                     end_date = fit_date,
                                     root = here::here("data"),
                                     extracts_root = NULL,
                                     prep = NULL) {
  stopifnot(sex %in% c("male", "female"))
  stopifnot(sport %in% c("basketball", "handball"))

  # `fit_date` is REQUIRED, not defaulted to Sys.Date(). Three things derive
  # from it -- the partition key (`fit_date=<D>`), the season simulation's RNG
  # seed (`sim_seed`, below) and, through `end_date`, the results/schedule
  # cut-off -- so an accidental "today" does not degrade the output, it
  # relabels it: a re-extract of an August fit would land in a today-stamped
  # partition, seeded on today and cut at today, and the "re-extracting a fit
  # reproduces its tables" promise made at `sim_seed` would be quietly false.
  # Both entry points (extract_handball_iceland() /
  # extract_basketball_iceland()) always pass it, so nothing legitimate relied
  # on the default. The check is explicit rather than left to lazy evaluation
  # so the failure names the argument, instead of surfacing as an error inside
  # as.Date() several statements later; the length/NA arm catches the other
  # silent shape, an explicit NULL, which would collapse the partition path to
  # character(0) and make dir.create() a no-op.
  if (missing(fit_date) || length(fit_date) != 1L || is.na(fit_date)) {
    cli::cli_abort(
      c(
        "{.arg fit_date} must be a single non-missing date.",
        "i" = "It keys the partition, seeds the simulation and cuts the data.",
        "i" = "Pass the fit's own date; it must never fall back to today."
      ),
      call = NULL
    )
  }

  # Tie handling is RESOLVED from `sport`, never defaulted. These two arguments
  # used to default to FALSE / 0, which is the exact shape of the incident
  # documented at `.tie_params_pfi()` above: the league slice reaching the
  # extractor had already lost `betting`, the defaults quietly took over, and
  # handball published p_draw = 0 for every match while its season simulation
  # ran over a league in which a draw could not occur. FALSE / 0 is a perfectly
  # valid scoring rule for basketball, so nothing downstream could tell the
  # default apart from a deliberate setting -- which is why the only safe
  # default is no default at all. NULL now falls back to the sport's publish
  # profile, the same source `meta.points` is published from, i.e. the source
  # whose disagreement made the incident visible in the first place. An
  # explicit value is still honoured (both entry points pass one, itself
  # resolved from that profile, and the equivalence tools do the same), but a
  # silently tie-less handball extract is no longer reachable.
  tie_params <- .tie_params_pfi(sport)
  if (is.null(has_ties)) {
    has_ties <- tie_params$has_ties
  }
  if (is.null(tie_threshold)) {
    tie_threshold <- tie_params$tie_threshold
  }

  divisions <- .iceland_division_codes(key, sex)
  expected_meetings <- .iceland_division_expected_meetings(key, sex)
  regular_season_rounds <- .iceland_division_regular_season_rounds(key, sex)
  division_is_cup <- .iceland_division_is_cup(key, sex)
  division_hold <- .iceland_division_preseason_hold(key, sex)

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

  # TWO result sets, as in extract_football_iceland(). The per-round strength
  # trajectory indexes the fit's `offense[r, k]` with each team's cumulative
  # appearance index, which equals the model's own `round1`/`round2`
  # (prepare_data()'s per-team `home_round`/`away_round`) only while both are
  # built from the same rows -- hence `model_results`, from
  # model_training_results(), the helper prepare_data() itself calls. The
  # published tables (season, regular-season cut, standings, remaining
  # fixtures) read `results`, every played match: model_training_results()
  # drops forfeits, and a forfeit win is still two points in the table and a
  # pairing that must not be simulated again.
  #
  # The guard: a `training_filter` would drop real matches from
  # `model_results` too, and no 2DT test covers what that does to a division
  # whose filtered-out teams still play in it. Only football_iceland carries a
  # filter (config/leagues.yml); this line makes a 2DT one abort until such a
  # test exists.
  stopifnot(is.null(league$training_filter))

  results_all <- read_table(
    "results",
    root = root,
    filter = list(sport = league$sport, country = league$country, sex = sex)
  )
  model_results <- model_training_results(
    results_all, league,
    end_date = end_date
  )
  # Same ordering prepare_data() applies before building its round index, so
  # the appearance indices agree row-for-row rather than by luck of the
  # parquet scan order. model_training_results() already returns this order;
  # the line keeps the dependency visible where the index is built.
  model_results <- model_results[order(model_results$match_date), , drop = FALSE]
  results <- .played_results(results_all, end_date)
  results <- results[order(results$match_date), , drop = FALSE]

  schedules <- read_table(
    "schedules",
    root = root,
    filter = list(sport = league$sport, country = league$country, sex = sex)
  )

  # The fit's scoring level belongs to the latest season it has results for;
  # a division projecting a later season steps it forward (F12). Each
  # division resolves its own season below.
  last_fitted_season <- if (nrow(model_results) > 0L) {
    max(model_results$season, na.rm = TRUE)
  } else {
    NA_integer_
  }

  # ---- Cross-division inputs, computed once --------------------------------
  posterior_goals <- .compute_posterior_goals_2dt(fit, pred_d)
  team_strengths_draws <- .extract_team_strength_draws_2dt(fit, teams)
  home_advantage_draws <- .extract_home_advantage_draws_2dt(fit, teams)

  # Season-simulation inputs, pulled once (spec 2026-09-16 §4). Seeded from
  # fit_date, so re-extracting a fit reproduces its tables and the committed
  # fixture does not churn.
  #
  # Every random stream gets its own seed: the league-wide level shock here
  # (`sim_seed + 1`) and each division's simulation below (`sim_seed + 1 +` its
  # position). One seed for all of them replayed a single stream everywhere,
  # so a newcomer's strength shock equalled the level shock draw for draw and
  # every division simulated on the same numbers.
  sim_seed <- as.integer(format(as.Date(fit_date), "%Y%m%d"))
  sim_inputs <- .extract_sim_inputs_2dt(
    fit, teams, sport,
    n_seasons = prep$stan_data$N_seasons
  )
  sim_inputs$scalar$z_level <- withr::with_seed(
    sim_seed + 1L, stats::rnorm(nrow(sim_inputs$scalar))
  )
  match_fn <- .match_fn_2dt(sport)
  points_fn <- .points_fn_2dt(has_ties, tie_threshold)

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
    # The division's own season: its schedule counts as well as its results,
    # so a published next season is current before its first match (spec
    # 2026-09-16 §5). A held division stays on its held season (§5.1).
    season_div <- .current_season_2dt(
      results, schedules, end_date, div,
      hold = division_hold[[div]]
    )
    # Meetings per pairing, from the season's own fixtures where they form a
    # complete list (§6, F17). The publisher calls the same helper.
    division_format <- .division_format_2dt(
      results, schedules, season_div, div,
      expected_meetings = expected_meetings[[div]],
      regular_season_rounds = regular_season_rounds[[div]]
    )

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
      season = season_div,
      division_codes = div,
      end_date = as.Date(end_date),
      expected_meetings = division_format$meetings,
      regular_season_rounds = regular_season_rounds[[div]],
      is_cup = isTRUE(division_is_cup[[div]])
    )

    top_results <- results[
      results$season == season_div & results$division == div, ,
      drop = FALSE
    ]
    top_results <- .regular_season_cut(top_results, rounds)

    # The division's teams are the SEASON's: those who have played inside the
    # regular cut and those only scheduled so far. From played results alone,
    # handball one round into 2026-27 published 8 of its 24 teams on some
    # surfaces and all 24 on others.
    #
    # Read through the publisher's empty-safe helpers: a cell with no schedule
    # table at all reads as a zero-COLUMN tibble, and `$` on that warns.
    season_fixtures <- .publish_cell_rows(schedules, season_div, div)
    # Radix = C-locale order: team order drives the generated fixtures and the
    # per-(draw, team) jitter, so it must not follow the session's collation.
    div_teams <- sort(unique(c(
      .publish_appearances(top_results),
      .publish_appearances(season_fixtures)
    )), method = "radix")
    # The strength surfaces describe only teams the fit knows.
    current_top_teams <- tibble::tibble(
      team = div_teams[div_teams %in% teams$team]
    )

    # ---- final_positions + points_distribution: the whole regular season ---
    # Realised results plus a simulation of every remaining regular-season
    # fixture from the fit's latest-round strengths, not the ~2 rounds inside
    # Stan's 14-day prediction window (spec 2026-09-16 §1-§2, F1). Stan's
    # window now feeds predicted_matches (next_games) and nothing else.
    upcoming <- if (nrow(season_fixtures) > 0L) {
      season_fixtures[
        !is.na(season_fixtures$match_date) &
          season_fixtures$match_date > as.Date(end_date), ,
        drop = FALSE
      ]
    }
    remaining <- .remaining_fixtures_2dt(
      teams = div_teams,
      played = top_results,
      scheduled = upcoming,
      meetings = division_format$meetings,
      # Where meetings are unknown, the boundary caps each side's schedule.
      max_games = rounds$n_rounds
    )
    base_standings <- .base_standings_2dt(
      top_results, div_teams,
      has_ties = has_ties, tie_threshold = tie_threshold
    )
    seasons_ahead <- if (is.na(last_fitted_season)) {
      0L
    } else {
      max(0L, as.integer(season_div - last_fitted_season))
    }
    season_sim <- withr::with_seed(sim_seed + 1L + match(div, divisions), {
      div_inputs <- .add_new_team_priors_2dt(sim_inputs, div_teams)
      simulate_league_season(
        sim_inputs_team = div_inputs$team,
        sim_inputs_scalar = .season_level_2dt(div_inputs$scalar, seasons_ahead),
        remaining_fixtures = remaining,
        base_standings = base_standings,
        match_fn = match_fn,
        points_fn = points_fn,
        tie_break = "jitter"
      )
    })

    # ---- round_strengths_quantiles ----------------------------------------
    # Shaped after football's block (`round_strengths_quantiles` in
    # `.extract_division_parquets_pfi()`) and
    # calling the SAME helper: all three Stan models declare
    # `array[N_rounds] vector[K] offense` / `defense` plus `vector[K]
    # home_advantage_off` / `_def`, so no variable-name parameterisation is
    # needed and none is done. The earlier claim that a 2DT round trajectory was
    # impossible was wrong about the models, and this comment is what stops it
    # being re-derived.
    #
    # `model_results` is passed UNCUT and the cut is applied to the OUTPUT
    # instead. The helper derives each team's global round with a row_number()
    # over the results it is handed, and that index addresses `offense[r, k]`
    # -- so it must see the same set `prepare_data()` modelled. Cutting rows
    # out of the input would renumber every later appearance and silently
    # shift the trajectory onto neighbouring rounds.
    #
    # The output `round` is the team's own division matchweek, so the cut is a
    # PER-TEAM cap: a team keeps as many matchweeks as it has MODELLED rows
    # surviving `.regular_season_cut()`. A flat `round <= n_rounds` filter
    # would not be that -- the boundary counts rounds, and a team with games
    # in hand has fewer appearances than the round number its matches carry.
    # The cut is row-wise on `round`, so applying it to the modelled rows keeps
    # exactly the table's rows less any forfeit, which is not a fit round.
    trajectory_long <- .compute_team_strength_trajectory(
      fit = fit,
      results = model_results,
      teams = teams,
      current_top_teams = current_top_teams,
      current_season = season_div,
      top_div = div
    )
    if (nrow(trajectory_long) > 0L) {
      modelled_regular <- .regular_season_cut(
        model_results[
          model_results$season == season_div & model_results$division == div, ,
          drop = FALSE
        ],
        rounds
      )
      regular_appearances <- table(c(
        modelled_regular$home_team, modelled_regular$away_team
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
      # The two season tables carry the season they were simulated for. A new
      # season's schedule lands weeks before a refit can run, and the
      # publisher, which resolves the season from TODAY's data, would
      # otherwise label this table with a season it does not describe. The
      # reader lifts the column out before anything is published.
      final_positions = dplyr::mutate(
        season_sim$final_positions,
        season = season_div
      ),
      points_distribution = dplyr::mutate(
        season_sim$points_distribution,
        season = season_div
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
