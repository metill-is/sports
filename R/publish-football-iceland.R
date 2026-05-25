#' @include model-prepare.R storage.R config.R
NULL

# ---- Internal helpers --------------------------------------------------------


# Empty extracted-list slot — used when a division partition is missing
# from the archive (e.g. legacy archives that only wrote BD). Mirrors the
# schema of `read_extracted_football()` slots so the publisher's per-cell
# loop can run against an "empty" division without special-casing.
.empty_extracted_pfi <- function() {
  list(
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
}


# Extract per-team draws for a single Stan parameter vector indexed by team.
.extract_team_draws_pfi <- function(fit, var, teams, component, location) {
  fit$draws(var) |>
    posterior::as_draws_df() |>
    tibble::as_tibble() |>
    tidyr::pivot_longer(
      c(-".chain", -".draw", -".iteration"),
      names_to = "name", values_to = "value"
    ) |>
    dplyr::mutate(
      team_idx  = as.integer(readr::parse_number(.data$name)),
      team      = teams$team[.data$team_idx],
      component = component,
      location  = location
    )
}

.summarise_team_intervals_pfi <- function(draws, coverages = c(0.5, 0.8, 0.95)) {
  group_keys <- c("team", "component", "location")
  if ("round" %in% names(draws)) {
    group_keys <- c("round", group_keys)
  }
  draws |>
    dplyr::reframe(
      median = stats::median(.data$value),
      coverage = coverages,
      lower = stats::quantile(.data$value, 0.5 - coverages / 2),
      upper = stats::quantile(.data$value, 0.5 + coverages / 2),
      .by = dplyr::all_of(group_keys)
    )
}

# Build per-(BD-matchweek, team) trajectory of latent strength from a
# single fit. Reads the full `offense[1..N_rounds, K]` and `defense[..]`
# matrices and slices per-team round indices that correspond to each
# team's BD-only chronological matches in the current season. Returns
# a long tibble with (round, .draw, team, component, location, value)
# where `round` is the BD matchweek (1, 2, 3, ...), not the model's
# global per-team round index.
.compute_team_strength_trajectory_pfi <- function(fit,
                                                  results,
                                                  teams,
                                                  current_top_teams,
                                                  current_season,
                                                  top_div) {
  results_played <- results[
    !is.na(results$home_score) & !is.na(results$away_score), ,
    drop = FALSE
  ]
  if (nrow(results_played) == 0L) {
    return(tibble::tibble(
      round = integer(), team = character(),
      .draw = integer(),
      component = character(), location = character(),
      value = numeric()
    ))
  }

  long_all <- dplyr::bind_rows(
    dplyr::transmute(
      results_played,
      team = .data$home_team,
      match_date = .data$match_date,
      division = .data$division,
      season = .data$season
    ),
    dplyr::transmute(
      results_played,
      team = .data$away_team,
      match_date = .data$match_date,
      division = .data$division,
      season = .data$season
    )
  ) |>
    dplyr::arrange(.data$team, .data$match_date) |>
    dplyr::group_by(.data$team) |>
    dplyr::mutate(global_round = dplyr::row_number()) |>
    dplyr::ungroup()

  bd_chrono <- long_all |>
    dplyr::filter(
      .data$season == current_season,
      .data$division == top_div
    ) |>
    dplyr::arrange(.data$team, .data$match_date) |>
    dplyr::group_by(.data$team) |>
    dplyr::mutate(bd_matchweek = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::semi_join(current_top_teams, by = "team") |>
    dplyr::inner_join(teams[, c("team", "team_nr")], by = "team") |>
    dplyr::select("team", "team_nr", "bd_matchweek", "global_round")

  if (nrow(bd_chrono) == 0L) {
    return(tibble::tibble(
      round = integer(), team = character(),
      .draw = integer(),
      component = character(), location = character(),
      value = numeric()
    ))
  }

  rv <- fit$draws(c(
    "offense", "defense", "home_advantage_off", "home_advantage_def"
  )) |>
    posterior::as_draws_rvars()
  off_arr <- posterior::draws_of(rv$offense)
  def_arr <- posterior::draws_of(rv$defense)
  ha_off <- posterior::draws_of(rv$home_advantage_off)
  ha_def <- posterior::draws_of(rv$home_advantage_def)
  n_draws <- dim(off_arr)[1]
  fit_n_rounds <- dim(off_arr)[2]
  fit_n_teams <- dim(off_arr)[3]

  usable <- bd_chrono |>
    dplyr::filter(
      .data$global_round <= fit_n_rounds,
      .data$team_nr <= fit_n_teams
    )
  if (nrow(usable) < nrow(bd_chrono)) {
    warning(sprintf(
      paste0(
        "publish_football_iceland: fit covers %d rounds x %d teams; ",
        "%d (matchweek, team) entries fall outside (likely stale fit) -- skipping."
      ),
      fit_n_rounds, fit_n_teams, nrow(bd_chrono) - nrow(usable)
    ))
  }
  if (nrow(usable) == 0L) {
    return(tibble::tibble(
      round = integer(), team = character(),
      .draw = integer(),
      component = character(), location = character(),
      value = numeric()
    ))
  }

  rows_long <- vector("list", nrow(usable))
  for (i in seq_len(nrow(usable))) {
    k <- usable$team_nr[i]
    r <- usable$global_round[i]
    mw <- usable$bd_matchweek[i]
    team_name <- usable$team[i]

    off_d <- off_arr[, r, k]
    def_d <- def_arr[, r, k]
    ha_o <- ha_off[, k]
    ha_d <- ha_def[, k]

    offence_away <- off_d
    offence_home <- off_d + ha_o
    defence_away <- def_d
    defence_home <- def_d + ha_d
    total_away <- offence_away + defence_away
    total_home <- offence_home + defence_home
    offence_avg <- (offence_home + offence_away) / 2
    defence_avg <- (defence_home + defence_away) / 2
    total_avg <- (total_home + total_away) / 2

    rows_long[[i]] <- tibble::tibble(
      .draw = rep(seq_len(n_draws), 9L),
      round = mw,
      team = team_name,
      component = rep(
        c(
          "offence", "offence", "offence",
          "defence", "defence", "defence",
          "total", "total", "total"
        ),
        each = n_draws
      ),
      location = rep(
        c(
          "home", "away", "avg",
          "home", "away", "avg",
          "home", "away", "avg"
        ),
        each = n_draws
      ),
      value = c(
        offence_home, offence_away, offence_avg,
        defence_home, defence_away, defence_avg,
        total_home, total_away, total_avg
      )
    )
  }

  dplyr::bind_rows(rows_long)
}

# Append `new_rows` to a history JSON file, dedup on `key_cols` keeping the
# row with the most recent `generated_at`. Creates the file on first call.
# Stored shape: { "schema_version": 1, "records": [ {...}, ... ] }.
# Tolerates missing or malformed existing files (treated as empty history).
.append_to_history_pfi <- function(path, new_rows, key_cols) {
  existing <- if (file.exists(path)) {
    tryCatch(
      {
        parsed <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
        records <- parsed$records
        if (is.data.frame(records)) tibble::as_tibble(records) else tibble::tibble()
      },
      error = function(e) tibble::tibble()
    )
  } else {
    tibble::tibble()
  }

  all_rows <- dplyr::bind_rows(existing, new_rows) |>
    dplyr::arrange(dplyr::desc(.data$generated_at)) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(key_cols)), .keep_all = TRUE) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(key_cols)))

  write_json_consistent(
    list(schema_version = 1L, records = all_rows),
    path,
    auto_unbox = TRUE,
    dataframe = "rows",
    digits = 5,
    na = "null"
  )
}

# Assign each match a "matchweek" derived from team-chronological match counts.
# matchweek(m) = max(home_team_chrono_idx_after_m, away_team_chrono_idx_after_m).
# A team's chrono_idx is its 1-based position when its played matches are
# sorted by match_date. For perfectly synchronised round-robins this gives
# the league matchweek; for postponed matches, the rescheduled match is
# keyed to its team-chronological round (not its calendar position).
# Output: input tibble + integer column `matchweek`. Input row order is preserved.
.assign_matchweeks_pfi <- function(matches) {
  if (nrow(matches) == 0L) {
    out <- tibble::as_tibble(matches)
    out$matchweek <- integer(0)
    return(out)
  }

  ordered <- matches |>
    dplyr::mutate(.input_idx = dplyr::row_number()) |>
    dplyr::arrange(.data$match_date, .data$.input_idx) |>
    dplyr::mutate(.chrono_idx = dplyr::row_number())

  long <- dplyr::bind_rows(
    ordered |> dplyr::transmute(
      .data$.input_idx, .data$.chrono_idx,
      team = .data$home_team
    ),
    ordered |> dplyr::transmute(
      .data$.input_idx, .data$.chrono_idx,
      team = .data$away_team
    )
  ) |>
    dplyr::arrange(.data$.chrono_idx, .data$.input_idx) |>
    dplyr::group_by(.data$team) |>
    dplyr::mutate(team_idx = dplyr::row_number()) |>
    dplyr::ungroup()

  per_match <- long |>
    dplyr::summarise(
      matchweek = as.integer(max(.data$team_idx)),
      .by = ".input_idx"
    )

  matches |>
    dplyr::mutate(.input_idx = dplyr::row_number()) |>
    dplyr::left_join(per_match, by = ".input_idx") |>
    dplyr::select(-".input_idx") |>
    tibble::as_tibble()
}

# Find the latest fit_date partition strictly less than `target_date`.
# Scans BOTH trees:
#   - extracts/sport=X/country=Y/sex=Z/fit_date=D/predicted_matches.parquet
#     (current per-fit summaries, with `division` payload column)
#   - archive/sport=X/country=Y/sex=Z/fit_date=D/part-0.parquet
#     (legacy long-form per-draw posteriors; written by model-league.R for
#     non-football sports, plus historical football fits before the
#     extraction layer landed)
#
# Returns the path of the most recent candidate, or NULL.
# `.aggregate_round_predictions_pfi()` normalises both schemas into a
# count-weighted representation; for extracts it also filters on `division`.
.find_pre_round_fit_path_pfi <- function(extracts_root, archive_root,
                                         sport, country, sex,
                                         target_date) {
  collect <- function(root, fname) {
    base <- file.path(
      root,
      paste0("sport=", sport),
      paste0("country=", country),
      paste0("sex=", sex)
    )
    if (!dir.exists(base)) {
      return(list())
    }
    parts <- list.dirs(base, full.names = TRUE, recursive = FALSE)
    fit_dirs <- parts[grepl("/fit_date=", parts)]
    out <- list()
    for (d in fit_dirs) {
      fd <- as.Date(sub(".*fit_date=", "", d))
      if (is.na(fd) || fd >= target_date) next
      p <- file.path(d, fname)
      if (file.exists(p)) {
        out[[length(out) + 1L]] <- list(path = p, fit_date = fd)
      }
    }
    out
  }

  candidates <- c(
    collect(extracts_root, "predicted_matches.parquet"),
    collect(archive_root, "part-0.parquet")
  )
  if (length(candidates) == 0L) {
    return(NULL)
  }

  fit_dates <- do.call(c, lapply(candidates, function(x) x$fit_date))
  candidates[[which.max(fit_dates)]]$path
}

# Read the team_strengths_quantiles tibble from the latest extracts fit
# strictly before `target_date` (typically the season's first kickoff for
# the cell being published) for the target division. Walks back through
# fit_dates in reverse order; returns NULL when no fit qualifies.
.read_preseason_team_strengths_pfi <- function(extracts_root,
                                               sport, country, sex,
                                               target_date, target_div) {
  base <- file.path(
    extracts_root,
    paste0("sport=", sport),
    paste0("country=", country),
    paste0("sex=", sex)
  )
  if (!dir.exists(base)) {
    return(NULL)
  }
  parts <- list.dirs(base, full.names = TRUE, recursive = FALSE)
  fit_dirs <- parts[grepl("/fit_date=", parts)]
  if (length(fit_dirs) == 0L) {
    return(NULL)
  }
  fit_dates <- as.Date(sub(".*fit_date=", "", fit_dirs))
  ord <- order(fit_dates, decreasing = TRUE)
  fit_dirs <- fit_dirs[ord]
  fit_dates <- fit_dates[ord]
  for (i in seq_along(fit_dirs)) {
    if (fit_dates[i] >= target_date) next
    f <- file.path(fit_dirs[i], "team_strengths_quantiles.parquet")
    if (!file.exists(f)) next
    df <- arrow::read_parquet(f)
    if ("division" %in% names(df)) {
      df <- df[df$division == target_div, , drop = FALSE]
      df$division <- NULL
    }
    if (nrow(df) == 0L) next
    return(.intervals_from_quantiles_pfi(
      df, c("team", "component", "location")
    ))
  }
  NULL
}

# Aggregate per-(round, team) frozen pre-round predictions for played matches.
# For each played match, find the latest archived fit strictly before its
# matchweek's first kickoff, read that fit's posterior draws for the match,
# and compute team-side xG_for / xG_against / xPts. Aggregates per
# (round, team) by summing across the team's matches in that round.
#
# Returns a tibble with columns:
#   round (int), team (chr), fit_date (chr), n_matches (int),
#   xg_for (dbl), xg_against (dbl), xpts (dbl),
#   p_win (dbl), p_draw (dbl), p_loss (dbl)
#
# Matches whose matchweek has no pre-round archive partition are silently
# skipped -- pre-archive early-season rounds simply do not appear.
.aggregate_round_predictions_pfi <- function(played_matches,
                                             extracts_root, archive_root,
                                             sport, country, sex,
                                             target_div = "BD") {
  if (nrow(played_matches) == 0L) {
    return(tibble::tibble(
      round = integer(), team = character(), fit_date = character(),
      n_matches = integer(),
      xg_for = numeric(), xg_against = numeric(), xpts = numeric(),
      p_win = numeric(), p_draw = numeric(), p_loss = numeric()
    ))
  }

  with_mw <- .assign_matchweeks_pfi(played_matches)

  per_round <- with_mw |>
    dplyr::summarise(
      first_kickoff = min(.data$match_date),
      .by = "matchweek"
    )

  rounds <- vector("list", nrow(per_round))

  for (i in seq_len(nrow(per_round))) {
    mw <- per_round$matchweek[i]
    target <- per_round$first_kickoff[i]

    fit_path <- .find_pre_round_fit_path_pfi(
      extracts_root = extracts_root,
      archive_root = archive_root,
      sport = sport, country = country, sex = sex,
      target_date = target
    )
    if (is.null(fit_path)) next

    round_matches <- with_mw[with_mw$matchweek == mw, ]
    fit_date_chr <- sub(".*fit_date=([^/]+)/.*", "\\1", fit_path)

    beliefs <- arrow::read_parquet(fit_path)
    if ("division" %in% names(beliefs)) {
      beliefs <- beliefs[beliefs$division == target_div, , drop = FALSE]
    }
    beliefs <- beliefs |>
      dplyr::semi_join(
        round_matches |> dplyr::select(
          "home_team", "away_team", "match_date"
        ),
        by = c("home_team", "away_team", "match_date")
      )

    if (nrow(beliefs) == 0L) next

    # Normalise both schemas into (home_goals, away_goals, count). The
    # extracts `predicted_matches.parquet` carries `count` per integer-pair
    # (post-aggregation); the legacy `part-0.parquet` carries one row per
    # draw, so treat its rows as count=1.
    if (!"count" %in% names(beliefs)) {
      beliefs$count <- 1L
    }

    per_match <- beliefs |>
      dplyr::summarise(
        total = sum(.data$count),
        xg_home = sum(.data$home_goals * .data$count) / sum(.data$count),
        xg_away = sum(.data$away_goals * .data$count) / sum(.data$count),
        p_home_win = sum(.data$count[.data$home_goals > .data$away_goals]) / sum(.data$count),
        p_draw_match = sum(.data$count[.data$home_goals == .data$away_goals]) / sum(.data$count),
        p_away_win = sum(.data$count[.data$home_goals < .data$away_goals]) / sum(.data$count),
        .by = c("home_team", "away_team", "match_date")
      ) |>
      dplyr::mutate(
        xpts_home = 3 * .data$p_home_win + .data$p_draw_match,
        xpts_away = 3 * .data$p_away_win + .data$p_draw_match
      ) |>
      dplyr::select(-"total")

    home_side <- per_match |>
      dplyr::transmute(
        team = .data$home_team,
        xg_for = .data$xg_home, xg_against = .data$xg_away,
        xpts = .data$xpts_home,
        p_win = .data$p_home_win, p_draw = .data$p_draw_match,
        p_loss = .data$p_away_win
      )
    away_side <- per_match |>
      dplyr::transmute(
        team = .data$away_team,
        xg_for = .data$xg_away, xg_against = .data$xg_home,
        xpts = .data$xpts_away,
        p_win = .data$p_away_win, p_draw = .data$p_draw_match,
        p_loss = .data$p_home_win
      )

    rounds[[i]] <- dplyr::bind_rows(home_side, away_side) |>
      dplyr::summarise(
        n_matches = dplyr::n(),
        xg_for = sum(.data$xg_for),
        xg_against = sum(.data$xg_against),
        xpts = sum(.data$xpts),
        p_win = mean(.data$p_win),
        p_draw = mean(.data$p_draw),
        p_loss = mean(.data$p_loss),
        .by = "team"
      ) |>
      dplyr::mutate(
        round = as.integer(mw),
        fit_date = fit_date_chr
      )
  }

  result <- dplyr::bind_rows(rounds)
  if (nrow(result) == 0L) {
    return(tibble::tibble(
      round = integer(), team = character(), fit_date = character(),
      n_matches = integer(),
      xg_for = numeric(), xg_against = numeric(), xpts = numeric(),
      p_win = numeric(), p_draw = numeric(), p_loss = numeric()
    ))
  }

  result |>
    dplyr::mutate(n_matches = as.integer(.data$n_matches)) |>
    dplyr::select(
      "round", "team", "fit_date", "n_matches",
      "xg_for", "xg_against", "xpts",
      "p_win", "p_draw", "p_loss"
    ) |>
    dplyr::arrange(.data$round, .data$team)
}

# Compute coverage intervals from a 99-quantile band tibble (Phase 2 helper).
# Input: tibble with `quantile` (1..99) + `value` + the columns named in
# `group_keys`. Output: tibble with the same group keys + `median`,
# `coverage`, `lower`, `upper`, three rows per group (50 %, 80 %, 95 %).
# 50/80 % bands come from exact quantile lookups (q25/q75, q10/q90); the
# 95 % band uses the linear interpolation midpoints (q2/q3, q97/q98)
# documented in the extraction-layer design.
.intervals_from_quantiles_pfi <- function(quantiles, group_keys) {
  needed <- c(2L, 3L, 10L, 25L, 50L, 75L, 90L, 97L, 98L)
  wide <- quantiles |>
    dplyr::filter(.data$quantile %in% needed) |>
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(group_keys),
      names_from = "quantile",
      names_prefix = "q",
      values_from = "value"
    )
  if (nrow(wide) == 0L) {
    base_cols <- lapply(group_keys, function(k) {
      x <- quantiles[[k]]
      if (is.null(x)) character(0) else x[integer(0)]
    })
    names(base_cols) <- group_keys
    return(tibble::as_tibble(c(base_cols, list(
      median = numeric(0), coverage = numeric(0),
      lower = numeric(0), upper = numeric(0)
    ))))
  }

  base <- wide[, group_keys, drop = FALSE]
  median_col <- wide$q50

  out <- dplyr::bind_rows(
    base |> dplyr::mutate(
      median = median_col, coverage = 0.5,
      lower = wide$q25, upper = wide$q75
    ),
    base |> dplyr::mutate(
      median = median_col, coverage = 0.8,
      lower = wide$q10, upper = wide$q90
    ),
    base |> dplyr::mutate(
      median = median_col, coverage = 0.95,
      lower = (wide$q2 + wide$q3) / 2,
      upper = (wide$q97 + wide$q98) / 2
    )
  )
  out |>
    dplyr::arrange(
      dplyr::across(dplyr::all_of(group_keys)),
      .data$coverage
    )
}

# Type-7 weighted quantile that matches stats::quantile() applied to the
# expanded per-draw vector. Used to reproduce the publisher's
# stats::quantile(per_draw_points, p) on the (points, count) representation.
# Inputs must be the same length; counts must be positive integers.
.weighted_quantile_pfi <- function(values, counts, probs) {
  ord <- order(values)
  values <- values[ord]
  counts <- as.numeric(counts[ord])
  cumc <- cumsum(counts)
  N <- sum(counts)
  pos <- probs * (N - 1) + 1

  rank_to_value <- function(rank) {
    idx <- which(cumc >= rank)[1L]
    if (is.na(idx)) values[length(values)] else values[idx]
  }
  vapply(pos, function(p) {
    lo <- floor(p)
    hi <- ceiling(p)
    frac <- p - lo
    v_lo <- rank_to_value(lo)
    v_hi <- rank_to_value(hi)
    v_lo + frac * (v_hi - v_lo)
  }, numeric(1))
}

#' Publish football Iceland posterior summaries as JSON
#'
#' Phase 2 entrypoint: reads from the per-fit extraction archive (the 6
#' Parquets written by [`extract_football_iceland()`]) instead of an
#' in-memory fit RDS. Use [`read_extracted_football()`] to construct
#' `extracted`, or pass a hand-built list with the same shape (tests do
#' the latter).
#'
#' Writes seven snapshot JSONs into
#' `output_root/football/iceland/{karla|kvenna}/`:
#'   - `meta.json`
#'   - `next_games.json`
#'   - `standings.json`
#'   - `team_strengths.json`
#'   - `final_positions.json`
#'   - `points_distribution.json`
#'   - `home_advantage.json`
#'
#' Plus three accretive history files written under `output_root` when
#' there is relevant data:
#'   - `team_strengths_history.json` (every fit)
#'   - `standings_history.json` (every fit with played top-flight matches)
#'   - `final_positions_history.json` (every fit with played top-flight
#'     matches)
#'
#' Plus one publisher-internal accretive file written under
#' `round_predictions_history_root` (kept out of `output_root` because
#' metill-platform has no consumer for it):
#'   - `round_predictions_history.json` (every fit; empty `records` when
#'     the archive lacks pre-round partitions yet)
#'
#' @param extracted Named list returned by [`read_extracted_football()`]:
#'   six tibbles plus optionally `fit_date`. The required tibbles are
#'   `predicted_matches`, `team_strengths_quantiles`,
#'   `round_strengths_quantiles`, `home_advantage_quantiles`,
#'   `final_positions`, `points_distribution`.
#' @param league A single entry from `load_leagues()` (must have `sport`
#'   and `country` set).
#' @param sex `"male"` or `"female"`.
#' @param end_date Training cutoff passed to `prepare_data()`. Default
#'   `Sys.Date()`.
#' @param root Data root for `read_table()`. Default `here::here("data")`.
#' @param output_root Root for JSON output. Default
#'   `here::here("data", "publish")`.
#' @param extracts_root Root of the per-fit football extracts tree used to
#'   source pre-round xG / xPts predictions and the preseason team-strength
#'   baseline. Default `here::here("data", "beliefs", "extracts")`.
#' @param archive_root Root of the beliefs archive (`part-0.parquet`,
#'   per-draw long-form). Used as a fallback for round_predictions on fit
#'   dates where no extract was written (legacy football fits before the
#'   extraction layer landed). Default `here::here("data", "beliefs", "archive")`.
#' @param round_predictions_history_root Root for the per-cell
#'   `round_predictions_history.json` accumulator. Lives outside
#'   `output_root` because the file is publisher-internal (read across
#'   fits to dedup on `(round, team)`) and has no metill-platform
#'   consumer. When `NULL` (default), derives from `output_root`:
#'   production (`output_root = "data/publish"`) writes to
#'   `data/beliefs/round_predictions_history/`; tests passing a
#'   tempdir output_root automatically get a sibling tempdir for the
#'   history tree, so no test ever accidentally writes to the
#'   project's real beliefs tree.
#' @return `invisible(NULL)`.
#' @importFrom rlang .data
#' @export
publish_football_iceland <- function(extracted,
                                     league,
                                     sex,
                                     end_date = Sys.Date(),
                                     root = here::here("data"),
                                     output_root = here::here(
                                       "data", "publish"
                                     ),
                                     extracts_root = here::here(
                                       "data", "beliefs", "extracts"
                                     ),
                                     archive_root = here::here(
                                       "data", "beliefs", "archive"
                                     ),
                                     round_predictions_history_root = NULL) {
  if (is.null(round_predictions_history_root)) {
    round_predictions_history_root <- file.path(
      dirname(output_root),
      "beliefs", "round_predictions_history"
    )
  }
  stopifnot(sex %in% c("male", "female"))
  stopifnot(!is.null(league$sport), !is.null(league$country))
  stopifnot(league$sport == "football", league$country == "iceland")
  stopifnot(inherits(end_date, "Date"))
  # WHY: meta.json::fit_date must reflect the partition the publisher
  # actually read from, not the caller's `end_date`. Otherwise a refit
  # failure that falls back to stale extracts ships a meta.json claiming
  # today's date while the posteriors are from days ago (2026-05-12 audit).
  fit_date_stamp <- if (!is.null(extracted$fit_date)) {
    as.Date(extracted$fit_date)
  } else {
    end_date
  }
  required_slots <- c(
    "predicted_matches", "team_strengths_quantiles",
    "round_strengths_quantiles", "home_advantage_quantiles",
    "final_positions", "points_distribution"
  )
  # Canonical filter codes (matching results$division), mapped to URL-friendly
  # display suffixes for the output directory name. LD1 -> "ld" because the
  # platform's URL slug is /lengja/ -> karla-ld/, not karla-ld1/. CUP ->
  # "bikar" for the Mjólkurbikar tab. Driven by
  # config/leagues.yml::football_iceland.publish_divisions[[sex]] so adding
  # a new cell is a config-only change here.
  division_dir_suffix <- .football_iceland_division_slugs(sex)
  division_codes <- names(division_dir_suffix)
  division_labels_is <- .football_iceland_division_labels(sex)

  # extracted shape: list keyed by division code (BD, LD1, ...) — see
  # read_extracted_football(). Each per-division list has the 6 parquet
  # tibbles. A non-FATAL legacy fallback: if `extracted` is flat (the
  # pre-2026-05-04 single-division shape), wrap it as BD-only and emit
  # empty cells for every other configured division.
  if (all(required_slots %in% names(extracted))) {
    flat_legacy <- extracted[required_slots]
    fit_date_keep <- extracted$fit_date
    extracted <- list(BD = flat_legacy)
    for (div in setdiff(division_codes, "BD")) {
      extracted[[div]] <- .empty_extracted_pfi()
    }
    extracted$fit_date <- fit_date_keep
  }
  for (div in division_codes) {
    if (is.null(extracted[[div]])) {
      extracted[[div]] <- .empty_extracted_pfi()
    }
    div_slots <- if (is.list(extracted[[div]])) names(extracted[[div]]) else character()
    missing_slots <- setdiff(required_slots, div_slots)
    if (length(missing_slots) > 0L) {
      stop(
        "publish_football_iceland: extracted$", div,
        " is missing slots: ", paste(missing_slots, collapse = ", "),
        call. = FALSE
      )
    }
  }

  sex_folder <- if (sex == "male") "karla" else "kvenna"

  # Reconstruct prep purely for division metadata + venue lookup. The
  # extraction layer doesn't carry division (deliberately — see Phase 1
  # design notes), so the publisher still joins it from pred_d.
  prep <- prepare_data(league, sex, end_date = end_date, root = root)
  pred_d <- prep$pred_d

  results <- read_table(
    "results",
    root   = root,
    filter = list(sport = league$sport, country = league$country, sex = sex)
  )
  results <- results[results$match_date <= end_date, , drop = FALSE]
  results <- results[
    !is.na(results$home_score) & !is.na(results$away_score), ,
    drop = FALSE
  ]

  for (target_div in division_codes) {
    top_div <- target_div
    is_cup <- identical(target_div, "CUP")
    # Per-division extracted slice — already filtered to this division's
    # teams + matches by extract_football_iceland(). The publisher used
    # to apply `semi_join(current_top_teams)` defensively; with per-cell
    # extracts those filters are redundant (and would no-op anyway).
    ext <- extracted[[target_div]]
    if (is.null(ext)) ext <- .empty_extracted_pfi()
    out_dir <- file.path(
      output_root,
      "football",
      "iceland",
      sprintf("%s-%s", sex_folder, division_dir_suffix[[target_div]])
    )
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

    current_season <- max(results$season, na.rm = TRUE)

    current_top_teams <- results[
      results$season == current_season & results$division == top_div, ,
      drop = FALSE
    ] |>
      dplyr::select("home_team", "away_team") |>
      tidyr::pivot_longer(c("home_team", "away_team"), values_to = "team") |>
      dplyr::distinct(.data$team)

    top_teams_upcoming <- pred_d[pred_d$division == top_div, , drop = FALSE] |>
      dplyr::select("home_team", "away_team") |>
      tidyr::pivot_longer(c("home_team", "away_team"), values_to = "team") |>
      dplyr::distinct(.data$team)
    if (nrow(top_teams_upcoming) == 0L) {
      top_teams_upcoming <- current_top_teams
    }

    bd_played <- results[
      results$season == current_season & results$division == top_div, ,
      drop = FALSE
    ]
    round_predictions <- .aggregate_round_predictions_pfi(
      played_matches = bd_played[, c("home_team", "away_team", "match_date")],
      extracts_root = extracts_root,
      archive_root = archive_root,
      sport = league$sport, country = league$country, sex = sex,
      target_div = target_div
    )

    team_expected <- if (nrow(bd_played) > 0L) {
      played_per_team <- bd_played |>
        tidyr::pivot_longer(
          c("home_team", "away_team"),
          values_to = "team"
        ) |>
        dplyr::count(.data$team, name = "played_count")

      if (nrow(round_predictions) == 0L) {
        played_per_team |>
          dplyr::transmute(
            .data$team,
            xg_for = NA_real_,
            xg_against = NA_real_,
            xpts = NA_real_,
            n_predicted_matches = 0L,
            n_played_matches = .data$played_count,
            xg_trend = list(I(numeric(0))),
            xg_against_trend = list(I(numeric(0)))
          )
      } else {
        team_pred <- round_predictions |>
          dplyr::arrange(.data$round) |>
          dplyr::summarise(
            n_predicted = sum(.data$n_matches),
            xg_for_sum = sum(.data$xg_for),
            xg_against_sum = sum(.data$xg_against),
            xpts_sum = sum(.data$xpts),
            xg_trend = list(.data$xg_for),
            xg_against_trend = list(.data$xg_against),
            .by = "team"
          )

        played_per_team |>
          dplyr::left_join(team_pred, by = "team") |>
          dplyr::mutate(
            n_predicted_matches = as.integer(
              dplyr::coalesce(.data$n_predicted, 0L)
            ),
            n_played_matches = as.integer(.data$played_count),
            xg_for = dplyr::if_else(
              .data$n_predicted_matches > 0L, .data$xg_for_sum, NA_real_
            ),
            xg_against = dplyr::if_else(
              .data$n_predicted_matches > 0L, .data$xg_against_sum, NA_real_
            ),
            xpts = dplyr::if_else(
              .data$n_predicted_matches > 0L, .data$xpts_sum, NA_real_
            ),
            xg_trend = lapply(.data$xg_trend, function(x) {
              if (is.null(x)) I(numeric(0)) else I(x)
            }),
            xg_against_trend = lapply(.data$xg_against_trend, function(x) {
              if (is.null(x)) I(numeric(0)) else I(x)
            })
          ) |>
          dplyr::select(
            "team", "xg_for", "xg_against", "xpts",
            "n_predicted_matches", "n_played_matches",
            "xg_trend", "xg_against_trend"
          )
      }
    } else {
      NULL
    }

    predicted_matches <- ext$predicted_matches

    predicted_with_division <- if (nrow(predicted_matches) > 0L) {
      predicted_matches |>
        dplyr::left_join(
          pred_d |> dplyr::distinct(
            .data$home_team, .data$away_team,
            .data$match_date, .data$division
          ),
          by = c("home_team", "away_team", "match_date")
        )
    } else {
      predicted_matches |>
        dplyr::mutate(division = character(0))
    }

    generated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")

    # ---- meta.json ----------------------------------------------------------

    round_num <- results[
      results$season == current_season & results$division == top_div, ,
      drop = FALSE
    ] |>
      tidyr::pivot_longer(c("home_team", "away_team"), values_to = "team") |>
      dplyr::count(.data$team) |>
      dplyr::pull("n") |>
      (\(x) if (length(x) == 0L) 0L else min(x))()

    # Source n_draws from the per-fit sim_inputs first (always populated when
    # the fit ran), then fall back to predicted_matches' per-match count for
    # legacy partitions without sim_inputs. Cup cells have predicted_matches
    # empty by design (cup matches stay out of pred_d while KSI has not yet
    # drawn the bracket); without the sim_inputs path n_draws was reported as
    # 0 in meta.json even though the cup model fit on 4000 draws -- audit
    # 2026-05-15 §E.
    n_draws <- if (!is.null(extracted$sim_inputs) &&
      !is.null(extracted$sim_inputs$scalar) &&
      nrow(extracted$sim_inputs$scalar) > 0L) {
      as.integer(dplyr::n_distinct(extracted$sim_inputs$scalar$.draw))
    } else if (nrow(predicted_matches) > 0L) {
      per_match_count <- predicted_matches |>
        dplyr::summarise(
          s = sum(.data$count),
          .by = c("home_team", "away_team", "match_date")
        )
      as.integer(round(mean(per_match_count$s)))
    } else {
      0L
    }

    league_label <- division_labels_is[[target_div]]
    meta <- list(
      sport        = "football",
      sex          = sex,
      league       = league_label,
      division     = target_div,
      is_cup       = is_cup,
      season       = current_season,
      generated_at = generated_at,
      fit_date     = format(fit_date_stamp, "%Y-%m-%d"),
      round        = as.integer(round_num),
      n_draws      = as.integer(n_draws)
    )
    write_json_consistent(
      meta,
      file.path(out_dir, "meta.json"),
      auto_unbox = TRUE
    )

    # ---- next_games.json ----------------------------------------------------

    male_top_division_venues <- tibble::tribble(
      ~team, ~venue,
      "Brei\u00f0ablik", "K\u00f3pavogsv\u00f6llur",
      "FH", "Kaplakrikav\u00f6llur",
      "Fram", "Laugardalsv\u00f6llur",
      "KA", "KA-v\u00f6llurinn",
      "KR", "KR-v\u00f6llur",
      "Keflav\u00edk", "Nettov\u00f6llurinn",
      "Stjarnan", "Stj\u00f6rnuv\u00f6llur",
      "Valur", "Hl\u00ed\u00f0arendi",
      "V\u00edkingur R.", "V\u00edkingsv\u00f6llur",
      "\u00cdA", "Nor\u00f0ur\u00e1lsv\u00f6llurinn",
      "\u00cdBV", "H\u00e1steinv\u00f6llur",
      "\u00de\u00f3r", "\u00de\u00f3rsv\u00f6llur"
    )

    division_labels <- c(
      BD = "BD", LD1 = "LD", LD2 = "\u00d6D", LD3 = "\u00deD",
      LD4 = "FjD", CUP = "MB",
      BD_UPPER_PO = "BD PO", BD_LOWER_PO = "BD N-PO",
      LD1_PO = "LD PO"
    )

    if (nrow(predicted_with_division) > 0L) {
      next_games_out <- predicted_with_division |>
        dplyr::filter(
          .data$division == top_div,
          .data$match_date >= end_date,
          .data$match_date <= end_date + 14L
        ) |>
        dplyr::mutate(goal_diff = .data$home_goals - .data$away_goals) |>
        dplyr::summarise(
          total = sum(.data$count),
          mean_home_goals = sum(.data$home_goals * .data$count) / sum(.data$count),
          mean_away_goals = sum(.data$away_goals * .data$count) / sum(.data$count),
          mean_goal_diff = sum(.data$goal_diff * .data$count) / sum(.data$count),
          p_home_win = sum(.data$count[.data$goal_diff > 0]) / sum(.data$count),
          p_draw = sum(.data$count[.data$goal_diff == 0]) / sum(.data$count),
          p_away_win = sum(.data$count[.data$goal_diff < 0]) / sum(.data$count),
          # NB: tibble::tibble() has no data-mask context, so .data$goal_diff
          # would fail with "Column `goal_diff` not found in `.data`". Bare
          # symbols resolve via summarise()'s outer mask before the call.
          goal_diff_distribution = list(
            tibble::tibble(diff = goal_diff, count = count) |>
              dplyr::summarise(n = sum(.data$count), .by = "diff") |>
              dplyr::mutate(p = .data$n / sum(.data$n)) |>
              dplyr::arrange(.data$diff) |>
              dplyr::select("diff", "p")
          ),
          .by = c("division", "match_date", "home_team", "away_team")
        ) |>
        dplyr::arrange(.data$match_date, .data$home_team, .data$away_team) |>
        dplyr::left_join(male_top_division_venues, by = c("home_team" = "team")) |>
        dplyr::mutate(
          division_code = dplyr::recode(
            .data$division, !!!division_labels,
            .default = .data$division
          ),
          date = format(.data$match_date, "%Y-%m-%d")
        ) |>
        dplyr::select(
          "date", "venue", "division", "division_code",
          home = "home_team", away = "away_team",
          "mean_home_goals", "mean_away_goals", "mean_goal_diff",
          "p_home_win", "p_draw", "p_away_win",
          "goal_diff_distribution"
        )
    } else {
      next_games_out <- tibble::tibble(
        date = character(), venue = character(),
        division = character(), division_code = character(),
        home = character(), away = character(),
        mean_home_goals = numeric(), mean_away_goals = numeric(),
        mean_goal_diff = numeric(), p_home_win = numeric(),
        p_draw = numeric(), p_away_win = numeric(),
        goal_diff_distribution = list()
      )
    }

    write_json_consistent(
      list(generated_at = generated_at, matches = next_games_out),
      file.path(out_dir, "next_games.json"),
      auto_unbox = TRUE, dataframe = "rows", digits = 5, na = "null"
    )

    # ---- standings.json + standings_history.json ----------------------------
    #
    # Cups are knockouts — there is no points table. Skip the W/L/D
    # tabulation entirely for CUP cells: tallying cup matches by points
    # would produce a misleading "table" mixing teams from every division
    # ranked by their cup runs.

    bd_results <- results[
      results$season == current_season & results$division == top_div, ,
      drop = FALSE
    ]

    pad_form <- function(x, n = 5L) {
      tail(c(rep(NA_character_, n), x), n)
    }

    short_code <- function(team) {
      team |>
        stringr::str_remove_all("\\s|\\.") |>
        stringr::str_to_upper() |>
        stringr::str_sub(1L, 3L)
    }

    if (!is_cup && nrow(bd_results) > 0L) {
      long_bd <- dplyr::bind_rows(
        dplyr::transmute(bd_results,
          team = .data$home_team, match_date = .data$match_date,
          gf = .data$home_score, ga = .data$away_score
        ),
        dplyr::transmute(bd_results,
          team = .data$away_team, match_date = .data$match_date,
          gf = .data$away_score, ga = .data$home_score
        )
      ) |>
        dplyr::mutate(
          result = dplyr::case_when(
            .data$gf > .data$ga ~ "W",
            .data$gf < .data$ga ~ "L",
            TRUE ~ "D"
          )
        ) |>
        dplyr::arrange(.data$team, .data$match_date)

      standings_rows <- long_bd |>
        dplyr::summarise(
          played = dplyr::n(),
          wins = sum(.data$result == "W"),
          draws = sum(.data$result == "D"),
          losses = sum(.data$result == "L"),
          goals_for = sum(.data$gf),
          goals_against = sum(.data$ga),
          goal_diff = .data$goals_for - .data$goals_against,
          points = 3L * .data$wins + .data$draws,
          form = list(pad_form(tail(.data$result, 5L))),
          goals_trend = list(.data$gf),
          goals_against_trend = list(.data$ga),
          .by = "team"
        ) |>
        dplyr::arrange(
          dplyr::desc(.data$points), dplyr::desc(.data$goal_diff),
          dplyr::desc(.data$goals_for)
        ) |>
        dplyr::mutate(
          rank = dplyr::row_number(),
          short = short_code(.data$team),
          goals_trend = lapply(.data$goals_trend, I),
          goals_against_trend = lapply(.data$goals_against_trend, I)
        )

      if (!is.null(team_expected)) {
        standings_rows <- standings_rows |>
          dplyr::left_join(team_expected, by = "team") |>
          dplyr::mutate(
            xg_trend = lapply(.data$xg_trend, function(x) {
              if (is.null(x)) I(numeric(0)) else I(x)
            }),
            xg_against_trend = lapply(.data$xg_against_trend, function(x) {
              if (is.null(x)) I(numeric(0)) else I(x)
            })
          )
      } else {
        standings_rows <- standings_rows |>
          dplyr::mutate(
            xg_for = NA_real_, xg_against = NA_real_, xpts = NA_real_,
            n_predicted_matches = 0L,
            n_played_matches = as.integer(.data$played),
            xg_trend = list(I(numeric(0))),
            xg_against_trend = list(I(numeric(0)))
          )
      }

      standings_rows <- standings_rows |>
        dplyr::select(
          "team", "short", "played", "wins", "draws", "losses",
          "goals_for", "goals_against", "goal_diff", "points",
          "xg_for", "xg_against", "xpts",
          "n_predicted_matches", "n_played_matches",
          "rank", "form",
          "xg_trend", "xg_against_trend",
          "goals_trend", "goals_against_trend"
        )

      write_json_consistent(
        list(
          generated_at = generated_at,
          season       = current_season,
          as_of        = format(max(bd_results$match_date), "%Y-%m-%d"),
          rows         = standings_rows
        ),
        file.path(out_dir, "standings.json"),
        auto_unbox = TRUE, dataframe = "rows", digits = 5, na = "null"
      )

      # Append per-round table to standings_history.json. Drops the snapshot-
      # only `form`, `xg_trend`, `xg_against_trend`, `goals_trend`, and
      # `goals_against_trend` columns. Dedup on (as_of, team) lets re-runs
      # against the same set of played fixtures replace rather than duplicate.
      standings_history_row <- standings_rows |>
        dplyr::mutate(
          as_of        = format(max(bd_results$match_date), "%Y-%m-%d"),
          generated_at = generated_at,
          round        = as.integer(round_num),
          season       = current_season
        ) |>
        dplyr::select(
          "as_of", "generated_at", "round", "season",
          "team", "short", "played", "wins", "draws", "losses",
          "goals_for", "goals_against", "goal_diff", "points",
          "xg_for", "xg_against", "xpts",
          "n_predicted_matches", "n_played_matches",
          "rank"
        )
      .append_to_history_pfi(
        file.path(out_dir, "standings_history.json"),
        standings_history_row,
        key_cols = c("as_of", "team")
      )
    } else {
      write_json_consistent(
        list(
          generated_at = generated_at, season = current_season,
          as_of = format(end_date, "%Y-%m-%d"), rows = list()
        ),
        file.path(out_dir, "standings.json"),
        auto_unbox = TRUE, dataframe = "rows", digits = 5, na = "null"
      )
      # Truncate standings_history.json to empty too: when the cell has no
      # league-table semantics (cups) any prior records are stale and the
      # append helper would otherwise keep them. Mirrors the pattern below
      # for final_positions_history.json.
      write_json_consistent(
        list(schema_version = 1L, records = list()),
        file.path(out_dir, "standings_history.json"),
        auto_unbox = TRUE, dataframe = "rows", digits = 5, na = "null"
      )
    }

    # ---- team_strengths.json ------------------------------------------------

    team_strengths <- ext$team_strengths_quantiles |>
      .intervals_from_quantiles_pfi(c("team", "component", "location"))

    # Pre-season comparison: latest archived fit strictly before the cell's
    # first played kickoff in the current season. Used by the platform's
    # forest plot to render a baseline (red) sub-row beneath each team.
    # NULL preseason → field omitted (frontend treats absence as "no row").
    preseason_intervals <- if (nrow(bd_played) > 0L) {
      .read_preseason_team_strengths_pfi(
        extracts_root = extracts_root,
        sport = league$sport, country = league$country, sex = sex,
        target_date = min(bd_played$match_date),
        target_div = target_div
      )
    } else {
      NULL
    }

    if (!is.null(preseason_intervals) && nrow(preseason_intervals) > 0L) {
      preseason_lookup <- preseason_intervals |>
        dplyr::transmute(
          .data$team, .data$component, .data$location, .data$coverage,
          ps_median = .data$median,
          ps_lower = .data$lower,
          ps_upper = .data$upper
        )
      team_strengths <- team_strengths |>
        dplyr::left_join(
          preseason_lookup,
          by = c("team", "component", "location", "coverage")
        )
      preseason_col <- vector("list", nrow(team_strengths))
      for (i in seq_len(nrow(team_strengths))) {
        m <- team_strengths$ps_median[i]
        if (!is.na(m)) {
          preseason_col[[i]] <- list(
            median = m,
            lower = team_strengths$ps_lower[i],
            upper = team_strengths$ps_upper[i]
          )
        }
      }
      team_strengths$preseason <- preseason_col
      team_strengths <- team_strengths |>
        dplyr::select(-"ps_median", -"ps_lower", -"ps_upper")
    }

    write_json_consistent(
      list(generated_at = generated_at, records = team_strengths),
      file.path(out_dir, "team_strengths.json"),
      auto_unbox = TRUE, dataframe = "rows", digits = 5, na = "null"
    )

    # ---- team_strengths_history.json ----------------------------------------

    rs_filtered <- ext$round_strengths_quantiles

    team_strengths_history_row <- if (nrow(rs_filtered) > 0L) {
      rs_filtered |>
        .intervals_from_quantiles_pfi(
          c("round", "team", "component", "location")
        ) |>
        dplyr::mutate(
          fit_date     = format(fit_date_stamp, "%Y-%m-%d"),
          generated_at = generated_at,
          season       = current_season
        ) |>
        dplyr::select(
          "fit_date", "generated_at", "round", "season",
          "team", "component", "location", "coverage",
          "median", "lower", "upper"
        )
    } else {
      tibble::tibble(
        fit_date = character(), generated_at = character(),
        round = integer(), season = integer(),
        team = character(), component = character(),
        location = character(), coverage = numeric(),
        median = numeric(), lower = numeric(), upper = numeric()
      )
    }
    write_json_consistent(
      list(schema_version = 1L, records = team_strengths_history_row),
      file.path(out_dir, "team_strengths_history.json"),
      auto_unbox = TRUE, dataframe = "rows", digits = 5, na = "null"
    )

    # ---- round_predictions_history.json -------------------------------------
    # WHY: this file accumulates per-(round, team) predictions across fits
    # so the publisher can dedup on (round, team) and surface the latest
    # generated_at. It is NOT consumed by metill-platform, so we keep it
    # OUT of output_root (data/publish/) to avoid bloating the rsync
    # mirror -- F7 from docs/audits/2026-05-25-pipeline-cross-project-review.html.

    round_predictions_dir <- file.path(
      round_predictions_history_root,
      "football", "iceland",
      sprintf("%s-%s", sex_folder, division_dir_suffix[[target_div]])
    )
    dir.create(round_predictions_dir, recursive = TRUE, showWarnings = FALSE)
    round_predictions_path <- file.path(
      round_predictions_dir, "round_predictions_history.json"
    )
    if (nrow(round_predictions) > 0L) {
      round_predictions_history_row <- round_predictions |>
        dplyr::mutate(
          generated_at = generated_at,
          season = current_season
        ) |>
        dplyr::select(
          "fit_date", "generated_at", "round", "season",
          "team", "n_matches",
          "xg_for", "xg_against", "xpts",
          "p_win", "p_draw", "p_loss"
        )
      .append_to_history_pfi(
        round_predictions_path,
        round_predictions_history_row,
        key_cols = c("round", "team")
      )
    } else if (!file.exists(round_predictions_path)) {
      write_json_consistent(
        list(schema_version = 1L, records = list()),
        round_predictions_path,
        auto_unbox = TRUE,
        dataframe = "rows",
        digits = 5,
        na = "null"
      )
    }

    # ---- final_positions.json + points_distribution.json --------------------

    # Per-cell extracted slices are pre-filtered to the division's teams
    # by extract_football_iceland(); no further semi_join needed here.
    final_positions <- ext$final_positions
    points_distribution <- ext$points_distribution

    if (nrow(final_positions) > 0L) {
      n_teams_top <- length(unique(final_positions$team))

      top_six <- final_positions |>
        dplyr::summarise(
          p_top_six = sum(.data$probability[.data$placement <= 6L]),
          p_winner = sum(.data$probability[.data$placement == 1L]),
          p_relegation = sum(
            .data$probability[.data$placement >= n_teams_top - 1L]
          ),
          .by = "team"
        )

      write_json_consistent(
        list(
          generated_at = generated_at,
          season       = current_season,
          n_teams      = n_teams_top,
          records      = final_positions,
          summary      = top_six
        ),
        file.path(out_dir, "final_positions.json"),
        auto_unbox = TRUE, dataframe = "rows", digits = 5
      )

      final_positions_history_row <- final_positions |>
        dplyr::mutate(
          as_of        = format(max(bd_results$match_date), "%Y-%m-%d"),
          generated_at = generated_at,
          round        = as.integer(round_num),
          season       = current_season
        ) |>
        dplyr::select(
          "as_of", "generated_at", "round", "season",
          "team", "placement", "probability"
        )
      .append_to_history_pfi(
        file.path(out_dir, "final_positions_history.json"),
        final_positions_history_row,
        key_cols = c("as_of", "team", "placement")
      )

      base_points <- bd_results |>
        dplyr::mutate(
          result = dplyr::case_when(
            .data$home_score > .data$away_score ~ "home",
            .data$home_score < .data$away_score ~ "away",
            TRUE ~ "tie"
          )
        ) |>
        tidyr::pivot_longer(
          c("home_team", "away_team"),
          values_to = "team"
        ) |>
        dplyr::mutate(
          name = dplyr::if_else(.data$name == "home_team", "home", "away"),
          points = dplyr::case_when(
            .data$result == "tie" ~ 1L,
            .data$result == .data$name ~ 3L,
            TRUE ~ 0L
          )
        ) |>
        dplyr::summarise(base_points = sum(.data$points), .by = "team")

      points_summary <- points_distribution |>
        dplyr::group_by(.data$team) |>
        dplyr::summarise(
          mean_points = sum(.data$points * .data$probability),
          median_points = .weighted_quantile_pfi(
            .data$points, round(.data$probability * 1e9), 0.5
          ),
          lower_80 = .weighted_quantile_pfi(
            .data$points, round(.data$probability * 1e9), 0.1
          ),
          upper_80 = .weighted_quantile_pfi(
            .data$points, round(.data$probability * 1e9), 0.9
          )
        ) |>
        dplyr::left_join(base_points, by = "team") |>
        dplyr::mutate(base_points = dplyr::coalesce(.data$base_points, 0L)) |>
        dplyr::left_join(top_six, by = "team")

      write_json_consistent(
        list(
          generated_at = generated_at,
          season       = current_season,
          records      = points_distribution,
          summary      = points_summary
        ),
        file.path(out_dir, "points_distribution.json"),
        auto_unbox = TRUE, dataframe = "rows", digits = 5
      )
    } else {
      write_json_consistent(
        list(
          generated_at = generated_at, season = current_season,
          n_teams = 0L, records = list(), summary = list()
        ),
        file.path(out_dir, "final_positions.json"),
        auto_unbox = TRUE, dataframe = "rows", digits = 5
      )
      write_json_consistent(
        list(
          generated_at = generated_at, season = current_season,
          records = list(), summary = list()
        ),
        file.path(out_dir, "points_distribution.json"),
        auto_unbox = TRUE, dataframe = "rows", digits = 5
      )
      # Always (re)write the history file when the snapshot is empty —
      # any pre-existing rows here are stale (e.g. BD-team rows written
      # into an LD cell by an earlier publisher version that didn't apply
      # the per-division `semi_join`). The append helper never retroacts
      # on stale rows, so the only safe move is to truncate to empty when
      # we have no current top-flight to project. The frontend's defensive
      # filter (see metill-platform finishing-heatmap.js) catches what
      # this misses for files already in flight.
      final_positions_history_path <- file.path(
        out_dir, "final_positions_history.json"
      )
      write_json_consistent(
        list(schema_version = 1L, records = list()),
        final_positions_history_path,
        auto_unbox = TRUE, dataframe = "rows", digits = 5, na = "null"
      )
    }

    # ---- home_advantage.json ------------------------------------------------

    home_advantage <- ext$home_advantage_quantiles |>
      .intervals_from_quantiles_pfi(c("team", "component")) |>
      dplyr::semi_join(top_teams_upcoming, by = "team")

    write_json_consistent(
      list(generated_at = generated_at, records = home_advantage),
      file.path(out_dir, "home_advantage.json"),
      auto_unbox = TRUE, dataframe = "rows", digits = 5
    )

    # ---- tournament_placements.json (cup cells only) ------------------------
    # Bracket-progression probabilities from the R-side simulator. The
    # extract layer writes this parquet only for the CUP cell; BD/LD1 emit
    # an empty placeholder so the JSON endpoint is stable.
    if (is_cup) {
      tournament_placements <- ext$tournament_placements
      if (nrow(tournament_placements) > 0L) {
        champion_summary <- tournament_placements |>
          dplyr::filter(.data$round_name == "Champion") |>
          dplyr::arrange(dplyr::desc(.data$probability)) |>
          dplyr::transmute(
            team        = .data$team,
            p_champion  = .data$probability
          )
        write_json_consistent(
          list(
            generated_at = generated_at,
            season       = current_season,
            n_teams      = length(unique(tournament_placements$team)),
            records      = tournament_placements,
            summary      = champion_summary
          ),
          file.path(out_dir, "tournament_placements.json"),
          auto_unbox = TRUE, dataframe = "rows", digits = 5
        )
      } else {
        write_json_consistent(
          list(
            generated_at = generated_at,
            season       = current_season,
            n_teams      = 0L,
            records      = list(),
            summary      = list()
          ),
          file.path(out_dir, "tournament_placements.json"),
          auto_unbox = TRUE, dataframe = "rows", digits = 5
        )
      }
    }

    n_files <- length(list.files(out_dir, pattern = "\\.json$"))
    message(sprintf(
      "publish_football_iceland: wrote %d JSONs to %s", n_files, out_dir
    ))
  }

  invisible(NULL)
}
