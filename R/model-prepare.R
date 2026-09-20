#' @include storage.R
NULL

#' Filter results to matches where both teams have recently competed in
#' specified divisions.
#'
#' The bivariate-Poisson football model assigns each team a hierarchically-
#' parameterised attack/defence pair plus a random-walk innovation per round.
#' With Mjólkurbikar in the training set, ~30 of ~97 teams have ≤10 matches
#' (most cup-only amateurs scoring 0-12 against top-flight sides). Their
#' parameters are weakly identified, and the inter-tier likelihood tension
#' creates funnel-shaped curvature in the posterior — manifesting as 7 % of
#' post-warmup transitions diverging at Stan's default `adapt_delta = 0.8`.
#'
#' The fix is to align the training population with the prediction
#' population: bets are only ever placed on BD/LD1/LD2/LD3 matches and on
#' Mjólkurbikar rounds that have reached at least LD3-tier opposition. So
#' restrict training to teams that have competed in one of those divisions
#' within a recent window. Their cross-tier cup matches against other
#' qualifying teams stay in (useful for tracking promotion-track strength);
#' their cup matches against amateur sides drop out (since amateur opponents
#' fail the filter).
#'
#' Operates on rows already filtered to the (sport, country, sex) and to
#' `match_date <= end_date`. The qualifying-team set is computed within the
#' window `[end_date - lookback_days, end_date]`. The match-retention filter
#' is then applied over the *full* `results` (not the window) — so a 2023
#' BD-vs-BD match between two currently-competing teams stays in training.
#'
#' @param results Tibble with at least `home_team`, `away_team`,
#'   `match_date`, `division`.
#' @param divisions Character vector of qualifying division codes.
#' @param lookback_days Integer; window relative to `end_date`.
#' @param end_date Training cutoff date. Window is `[end_date - lookback_days,
#'   end_date]`.
#' @return Subset of `results` (same columns) keeping only matches where
#'   both teams competed in `divisions` within the window.
#' @noRd
filter_results_by_top_divisions <- function(results, divisions, lookback_days,
                                            end_date) {
  if (nrow(results) == 0L || length(divisions) == 0L) {
    return(results)
  }
  window_start <- end_date - as.integer(lookback_days)
  qualifying <- results[
    results$division %in% divisions &
      results$match_date >= window_start &
      results$match_date <= end_date, ,
    drop = FALSE
  ]
  qualifying_teams <- unique(c(qualifying$home_team, qualifying$away_team))
  results[
    results$home_team %in% qualifying_teams &
      results$away_team %in% qualifying_teams, ,
    drop = FALSE
  ]
}

#' The complete set of `training_filter` keys.
#'
#' Mirrors `training_filter.properties` in `config/leagues.schema.json`, which
#' is `additionalProperties: false`. A key added there but not here makes the
#' first fit that uses it abort, which is the direction of failure we want: a
#' key this layer does not read must never look as though it was applied.
#' @noRd
.TRAINING_FILTER_KEYS <- c("divisions", "lookback_days")

#' Validate a league's `training_filter` entry, or abort.
#'
#' An ABSENT filter is the normal case -- only `football_iceland` carries one
#' -- and stays silent. A filter that is PRESENT but incomplete must not. The
#' guard this replaces was `length(tf$divisions) > 0L &&
#' !is.null(tf$lookback_days)`: with either key missing or misspelled it fell
#' straight through to unfiltered training and reported success. `$` partial
#' matching hides a misspelling rather than exposing it -- `divisions:` typed
#' as `division:` resolves to `NULL` from here, indistinguishable from "this
#' league has no filter".
#'
#' That exact shape -- a config key quietly absent, the pipeline carrying on --
#' has already shipped twice in this repo: the whitelist that dropped `betting`
#' and gave handball `p_draw = 0`, and the whitelist that dropped
#' `training_filter` and trained football on 4120 rows instead of 2974 for
#' weeks before anyone noticed. `config/leagues.schema.json` pins the same
#' shape, but only for entries that came through [load_leagues()] with
#' validation on; the daily fit hands us a code-built slice (see
#' `run_fit_targets()`) and the tests hand us literals, and the schema sees
#' neither.
#'
#' @param tf A league's `training_filter` entry, or `NULL` when it has none.
#' @return `TRUE` when a usable filter is configured, `FALSE` when none is.
#'   Aborts when one is configured but cannot be applied as written.
#' @noRd
.check_training_filter <- function(tf) {
  if (is.null(tf)) {
    return(FALSE)
  }

  # Aliased without the leading dot: cli reads `{.NAME}` inside an inline
  # expression as a style class, not a variable.
  expected_keys <- .TRAINING_FILTER_KEYS

  abort_tf <- function(...) {
    cli::cli_abort(c(
      "League has a {.field training_filter} that cannot be applied.",
      ...,
      i = "Expected exactly {.val {expected_keys}} \\
           (see {.file config/leagues.schema.json}).",
      i = "Skipping it silently would train on the unfiltered store and \\
           still report success -- the failure this guard exists to stop."
    ))
  }

  if (!is.list(tf)) {
    abort_tf(x = "It is {.cls {class(tf)}}, not a list of settings.")
  }

  # An unnamed element is as unreadable as a misspelt one, so treat both the
  # same rather than letting `setdiff(NULL, ...)` call an unnamed list clean.
  keys <- names(tf)
  if (is.null(keys)) keys <- rep("", length(tf))

  unknown <- setdiff(keys, .TRAINING_FILTER_KEYS)
  if (length(unknown) > 0L) {
    unknown[!nzchar(unknown)] <- "<unnamed>"
    abort_tf(
      x = "Unrecognised key{?s}: {.val {unknown}}.",
      i = "A key this layer does not read is a misspelling, not an option."
    )
  }

  missing_keys <- setdiff(.TRAINING_FILTER_KEYS, keys)
  if (length(missing_keys) > 0L) {
    abort_tf(x = "Missing key{?s}: {.val {missing_keys}}.")
  }

  # `coerce_array_fields()` may have turned `divisions` into a list, so flatten
  # before checking. A nested element would survive `length() > 0L` and then be
  # mangled by `%in%`'s `as.character()` into a literal "c(\"BD\", \"LD1\")",
  # matching no division and quietly emptying the training set.
  divisions <- unlist(tf$divisions, use.names = FALSE)
  if (length(divisions) == 0L || !is.character(divisions) ||
    anyNA(divisions) || !all(nzchar(divisions))) {
    abort_tf(x = "{.field divisions} must be one or more non-empty \\
                  division codes.")
  }

  lookback <- tf$lookback_days
  if (length(lookback) != 1L || !is.numeric(lookback) || is.na(lookback) ||
    lookback < 1) {
    abort_tf(
      x = "{.field lookback_days} must be a single number >= 1; \\
           got {.val {lookback}}.",
      i = "A non-numeric or NA value makes `window_start` NA, and subsetting \\
           on an NA date comparison returns rows of NAs rather than dropping \\
           them."
    )
  }

  TRUE
}

#' Walkover scorelines by sport.
#'
#' A forfeited game is recorded at the sport's walkover score: 20-0 in
#' basketball (FIBA, which KKI follows) and 10-0 in handball (HSI). Neither is
#' a played result -- nobody scores 0 in a basketball game -- so neither says
#' anything about strength, and a thin-data team whose history ends in
#' forfeits needs an implausible random-walk jump to fit them. Measured
#' 2026-09-17: 5 men's and 2 women's basketball forfeits, 5 men's and 1
#' women's handball. Football is absent on purpose: its 3-0 walkover cannot be
#' told apart from a played 3-0.
#' @noRd
.FORFEIT_SCORES <- c(basketball = 20L, handball = 10L)

#' Which rows are forfeits under their sport's walkover score.
#'
#' @param results Rows with `home_score` / `away_score`.
#' @param sport One sport name; a sport without a walkover score (football, or
#'   `NULL`) flags nothing.
#' @return Logical vector, one per row.
#' @noRd
.is_forfeit <- function(results, sport) {
  if (length(sport) != 1L || !sport %in% names(.FORFEIT_SCORES)) {
    return(rep(FALSE, nrow(results)))
  }
  w <- .FORFEIT_SCORES[[sport]]
  home <- results$home_score
  away <- results$away_score
  !is.na(home) & !is.na(away) &
    ((home == w & away == 0L) | (home == 0L & away == w))
}

#' Scored matches on or before `end_date`: every played result.
#'
#' What a league TABLE counts, forfeits included. [model_training_results()]
#' narrows this to what the model trains on.
#' @noRd
.played_results <- function(results, end_date, from_season = NULL) {
  if (!is.null(from_season)) {
    results <- results[results$season >= as.integer(from_season), , drop = FALSE]
  }
  results <- results[results$match_date <= end_date, , drop = FALSE]
  results[
    !is.na(results$home_score) & !is.na(results$away_score), ,
    drop = FALSE
  ]
}

#' The result set a fit is trained on.
#'
#' The one definition of "the matches the model saw", shared by
#' [prepare_data()], the strength trajectories of both Iceland extractors and
#' `needs_refit()`. The trajectory reads `offense[round, team]` at each team's
#' appearance count over the rows it is handed, so they must agree row for
#' row: a row dropped on one side only shifts the published trajectory onto
#' neighbouring rounds, silently.
#'
#' Two things drop rows beyond `.played_results()`: forfeits (see
#' `.FORFEIT_SCORES`) and the league's `training_filter`. Both leave real
#' results out, so a league table is built from `.played_results()` instead.
#'
#' @param results `results` rows for one (sport, country, sex).
#' @param league League config entry; its `sport` sets the walkover score and
#'   its `training_filter` is applied when set. A `training_filter` that is
#'   present but unusable aborts (see `.check_training_filter()`) rather than
#'   falling through to unfiltered training.
#' @param end_date Training cutoff (inclusive).
#' @param from_season Optional; drop matches with `season < from_season`.
#' @param verbose Report how many forfeits and `training_filter` matches were
#'   dropped.
#' @return Scored matches on or before `end_date`, filtered, ordered by
#'   `match_date`.
#' @noRd
model_training_results <- function(results, league, end_date,
                                   from_season = NULL, verbose = FALSE) {
  results <- .played_results(results, end_date, from_season)
  forfeit <- .is_forfeit(results, league$sport)
  if (any(forfeit)) {
    if (isTRUE(verbose)) {
      cli::cli_alert_info("Dropped {sum(forfeit)} forfeit{?s} from training.")
    }
    results <- results[!forfeit, , drop = FALSE]
  }

  tf <- league$training_filter
  if (.check_training_filter(tf)) {
    divisions <- unlist(tf$divisions, use.names = FALSE)
    n_before <- nrow(results)
    results <- filter_results_by_top_divisions(
      results,
      divisions     = divisions,
      lookback_days = as.integer(tf$lookback_days),
      end_date      = end_date
    )
    if (isTRUE(verbose)) {
      cli::cli_alert_info(
        "training_filter: kept {nrow(results)}/{n_before} matches \\
        (divisions={.val {divisions}}, lookback={tf$lookback_days}d)"
      )
    }
  }

  results[order(results$match_date), , drop = FALSE]
}

#' Build stan_data + pred_d + teams from the facts store.
#'
#' Reads `data/facts/results/` and `data/facts/schedules/` for the given
#' (league, sex), assembles the canonical Stan input list, and returns it
#' along with the prediction tibble (for posterior-draw joining) and the
#' team registry. Pure function -- no file I/O beyond read_table().
#'
#' The returned stan_data is a superset covering every field consumed by
#' the three production models (basketball scalar-sigma, handball per-team
#' sigma, football BVP). Each Stan model's data{} block picks only the
#' fields it declares.
#'
#' @param league A single entry from `load_leagues()` (must have `sport` +
#'   `country` set; `stan_model` is not read here).
#' @param sex "male" or "female".
#' @param end_date Cutoff date -- matches on or before this go into training.
#' @param root Data root. Default `here::here("data")`.
#' @param from_season Optional integer. If supplied, drop matches with
#'   `season < from_season`.
#' @param schedule_horizon_days How far ahead to look for prediction targets.
#'   Matches whose match_date falls in
#'   `[end_date, end_date + schedule_horizon_days]` are kept.
#' @return `list(stan_data, pred_d, teams)`.
#' @importFrom rlang .data
#' @export
prepare_data <- function(league,
                         sex,
                         end_date = Sys.Date(),
                         root = here::here("data"),
                         from_season = NULL,
                         schedule_horizon_days = 14L) {
  stopifnot(sex %in% c("male", "female"))
  stopifnot(!is.null(league$sport), !is.null(league$country))

  # -- Results (training matches) -------------------------------------------
  results <- read_table(
    "results",
    root = root,
    filter = list(sport = league$sport, country = league$country, sex = sex)
  )

  results <- model_training_results(
    results, league,
    end_date = end_date, from_season = from_season, verbose = TRUE
  )
  results$game_nr <- seq_len(nrow(results))

  # -- Team registry ---------------------------------------------------------
  teams <- tibble::tibble(
    team = sort(unique(c(results$home_team, results$away_team)))
  )
  teams$team_nr <- seq_len(nrow(teams))

  # -- Schedules (prediction matches) ----------------------------------------
  # WHY: when end_date is in the past (historical backfill), the schedules
  # table no longer carries matches between end_date and today -- they've
  # been moved into results once they played. To produce lookahead-free
  # predictions for those rounds, we union the upcoming-schedule with
  # past-results-after-end_date (using only the (sport, country, sex,
  # season, division, match_date, home_team, away_team) columns that
  # schedule entries also carry).
  schedules <- read_table(
    "schedules",
    root = root,
    filter = list(sport = league$sport, country = league$country, sex = sex)
  )

  horizon_end <- end_date + as.integer(schedule_horizon_days)
  pred_cols <- c(
    "sport", "country", "sex", "season", "division",
    "match_date", "home_team", "away_team"
  )

  upcoming_from_schedules <- schedules[
    !is.na(schedules$match_date) &
      schedules$match_date >= end_date &
      schedules$match_date <= horizon_end, ,
    drop = FALSE
  ]
  upcoming_from_schedules <- upcoming_from_schedules[
    , intersect(pred_cols, names(upcoming_from_schedules)),
    drop = FALSE
  ]

  results_all <- read_table(
    "results",
    root = root,
    filter = list(sport = league$sport, country = league$country, sex = sex)
  )
  upcoming_from_results <- results_all[
    !is.na(results_all$match_date) &
      results_all$match_date > end_date &
      results_all$match_date <= horizon_end, ,
    drop = FALSE
  ]
  upcoming_from_results <- upcoming_from_results[
    , intersect(pred_cols, names(upcoming_from_results)),
    drop = FALSE
  ]

  next_games <- dplyr::bind_rows(
    upcoming_from_schedules, upcoming_from_results
  )
  next_games <- next_games[
    !duplicated(next_games[, c("match_date", "home_team", "away_team")]), ,
    drop = FALSE
  ]

  # Only predict matches whose teams appear in the training set
  next_games <- next_games[
    next_games$home_team %in% teams$team & next_games$away_team %in% teams$team, ,
    drop = FALSE
  ]
  next_games <- next_games[order(next_games$match_date), , drop = FALSE]
  if (nrow(next_games) > 0L) next_games$game_nr <- seq_len(nrow(next_games))

  # -- Per-team time-between-matches and round index -------------------------
  long <- dplyr::bind_rows(
    dplyr::transmute(results,
      game_nr = .data$game_nr,
      match_date = .data$match_date,
      team = .data$home_team,
      side = "home"
    ),
    dplyr::transmute(results,
      game_nr = .data$game_nr,
      match_date = .data$match_date,
      team = .data$away_team,
      side = "away"
    )
  )
  long <- long[order(long$team, long$match_date), , drop = FALSE]
  long <- long |>
    dplyr::group_by(.data$team) |>
    dplyr::mutate(
      round = dplyr::row_number(),
      time_diff = as.numeric(.data$match_date - dplyr::lag(.data$match_date)),
      time_diff = dplyr::if_else(is.na(.data$time_diff), 7, .data$time_diff),
      time_diff = pmin(.data$time_diff, 100)
    ) |>
    dplyr::ungroup()

  home_long <- long[long$side == "home", c("game_nr", "round", "time_diff"), drop = FALSE]
  names(home_long) <- c("game_nr", "home_round", "home_timediff")
  away_long <- long[long$side == "away", c("game_nr", "round", "time_diff"), drop = FALSE]
  names(away_long) <- c("game_nr", "away_round", "away_timediff")

  # Season-first flag: 1 at the first appearance of each team in each season
  season_first_long <- dplyr::bind_rows(
    dplyr::transmute(results,
      game_nr = .data$game_nr,
      season = .data$season,
      team = .data$home_team,
      side = "home"
    ),
    dplyr::transmute(results,
      game_nr = .data$game_nr,
      season = .data$season,
      team = .data$away_team,
      side = "away"
    )
  )
  season_first_long <- season_first_long |>
    dplyr::arrange(.data$game_nr) |>
    dplyr::group_by(.data$team, .data$season) |>
    dplyr::mutate(
      season_round = dplyr::row_number(),
      is_first = as.integer(.data$season_round == 1L)
    ) |>
    dplyr::ungroup()
  season_first_by_game <- season_first_long |>
    dplyr::filter(.data$side == "home") |>
    dplyr::select("game_nr", season_first = "is_first")

  model_d <- results |>
    dplyr::inner_join(home_long, by = "game_nr") |>
    dplyr::inner_join(away_long, by = "game_nr") |>
    dplyr::inner_join(season_first_by_game, by = "game_nr") |>
    dplyr::inner_join(
      dplyr::rename(teams, home_nr = "team_nr"),
      by = c("home_team" = "team")
    ) |>
    dplyr::inner_join(
      dplyr::rename(teams, away_nr = "team_nr"),
      by = c("away_team" = "team")
    ) |>
    dplyr::arrange(.data$match_date)

  # Division as integer factor (1 = first level, 2 = second, ...).
  #
  # Levels are the training divisions first, then any division that occurs ONLY
  # in the upcoming fixtures. A fixture can be scheduled in a division that has
  # no played matches yet -- the playoff codes are the standard case
  # (`LD1_PO` fixtures on 2026-09-09 were the first of their division ever
  # scheduled). Deriving the levels from `model_d` alone maps such a fixture to
  # `NA`, cmdstanr rejects `pred_division` outright ("Variable 'pred_division'
  # has NA values"), and the whole (league, sex) cell fails to fit -- taking
  # every other fixture in the cell down with it. That is what broke
  # football_iceland male on 2026-09-07 and 2026-09-08.
  #
  # Unseen levels are APPENDED rather than merged-and-re-sorted so that the
  # training `division` indices stay stable regardless of what is on the
  # schedule. No Stan model in this repo indexes anything by division (football
  # does not declare the variable; handball and basketball declare it but never
  # reference it), so an appended level is inert -- it only has to be a
  # non-NA integer >= 1 to satisfy the `array[N_pred] int<lower=1>` declaration.
  train_div_levels <- sort(unique(model_d$division))
  pred_only_div_levels <- setdiff(
    sort(unique(as.character(next_games$division))),
    train_div_levels
  )
  div_levels <- c(train_div_levels, pred_only_div_levels)
  model_d$division_int <- as.integer(factor(model_d$division, levels = div_levels))

  N_rounds <- max(c(model_d$home_round, model_d$away_round))
  tbm <- matrix(0, nrow = nrow(teams), ncol = N_rounds)
  tbm[cbind(model_d$home_nr, model_d$home_round)] <- model_d$home_timediff
  tbm[cbind(model_d$away_nr, model_d$away_round)] <- model_d$away_timediff

  # -- Prediction tibble -----------------------------------------------------
  if (nrow(next_games) > 0L) {
    latest_game_dates <- long |>
      dplyr::group_by(.data$team) |>
      dplyr::summarise(latest_date = max(.data$match_date), .groups = "drop")

    upcoming_per_team <- next_games |>
      tidyr::pivot_longer(
        c("home_team", "away_team"),
        names_to = "side",
        values_to = "team"
      ) |>
      dplyr::group_by(.data$team) |>
      dplyr::arrange(.data$match_date) |>
      dplyr::mutate(team_game = dplyr::row_number()) |>
      dplyr::ungroup()

    # NOTE (2026-09-20): the `first_upcoming` / `top_teams` /
    # `time_to_next_games` builder and its correspondence assertion were
    # removed together with the Stan-side prediction-horizon inputs. They fed a
    # forward random walk that was computed and then discarded, so no model
    # declares them any more. Nothing below needs a team's first upcoming
    # fixture as a separate table: `pred_timediffs` derives every fixture's own
    # gap, falling back to `latest_date` for a team's first upcoming game, so
    # the whole block died with the Stan inputs.

    pred_timediffs <- upcoming_per_team |>
      dplyr::inner_join(latest_game_dates, by = "team") |>
      dplyr::group_by(.data$team) |>
      dplyr::arrange(.data$match_date) |>
      dplyr::mutate(
        prev_date = dplyr::if_else(
          .data$team_game == 1L,
          .data$latest_date,
          dplyr::lag(.data$match_date)
        ),
        timediff = pmin(as.numeric(.data$match_date - .data$prev_date), 50)
      ) |>
      dplyr::ungroup() |>
      dplyr::select("game_nr", "side", "timediff") |>
      tidyr::pivot_wider(names_from = "side", values_from = "timediff") |>
      dplyr::rename(home_timediff = "home_team", away_timediff = "away_team")

    pred_d <- next_games |>
      dplyr::inner_join(pred_timediffs, by = "game_nr") |>
      dplyr::inner_join(
        dplyr::rename(teams, home_nr = "team_nr"),
        by = c("home_team" = "team")
      ) |>
      dplyr::inner_join(
        dplyr::rename(teams, away_nr = "team_nr"),
        by = c("away_team" = "team")
      )

    pred_d$division_int <- as.integer(
      factor(pred_d$division, levels = div_levels)
    )

    # Defence in depth. div_levels above is built from the union of training and
    # fixture divisions, so this cannot fire today -- but cmdstanr's own message
    # names only the variable, not the offending value, and a silent NA here
    # kills the entire cell. Fail with the division code that caused it.
    if (anyNA(pred_d$division_int)) {
      bad <- sort(unique(as.character(
        pred_d$division[is.na(pred_d$division_int)]
      )))
      cli::cli_abort(c(
        "Upcoming fixtures carry division(s) with no factor level.",
        x = "Unmapped division(s): {.val {bad}}.",
        i = "div_levels: {.val {div_levels}}.",
        i = "Passing NA in {.arg pred_division} makes cmdstanr reject the fit."
      ))
    }
  } else {
    pred_d <- tibble::tibble(
      game_nr = integer(0),
      match_date = as.Date(character()),
      home_team = character(),
      away_team = character(),
      division = character(),
      home_nr = integer(0),
      away_nr = integer(0),
      home_timediff = numeric(0),
      away_timediff = numeric(0),
      division_int = integer(0)
    )
  }

  stan_data <- list(
    K                    = nrow(teams),
    N                    = nrow(model_d),
    N_pred               = nrow(pred_d),
    N_rounds             = N_rounds,
    N_seasons            = length(unique(model_d$season)),
    season               = as.integer(as.factor(model_d$season)),
    season_first         = as.integer(model_d$season_first),
    team1                = as.integer(model_d$home_nr),
    team2                = as.integer(model_d$away_nr),
    round1               = as.integer(model_d$home_round),
    round2               = as.integer(model_d$away_round),
    time_between_matches = tbm,
    goals1               = as.integer(model_d$home_score),
    goals2               = as.integer(model_d$away_score),
    division             = as.integer(model_d$division_int),
    team1_pred           = as.integer(pred_d$home_nr),
    team2_pred           = as.integer(pred_d$away_nr),
    pred_division        = as.integer(pred_d$division_int)
  )

  list(
    stan_data = stan_data,
    pred_d = pred_d[, c(
      "game_nr", "match_date", "home_team", "away_team",
      "division", "home_nr", "away_nr",
      "home_timediff", "away_timediff"
    ), drop = FALSE],
    teams = teams
  )
}
