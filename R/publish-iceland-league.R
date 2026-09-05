#' @include model-prepare.R storage.R config.R
NULL

# ---- Internal helpers --------------------------------------------------------


# Empty extracted-list slot — used when a division partition is missing
# from the archive (e.g. legacy archives that only wrote BD). Mirrors the
# schema of `read_extracted_iceland()` slots so the publisher's per-cell
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


# The league points scheme as a plain named integer vector, from the sport's
# publish profile. `draw` is NULL in a profile whose sport cannot draw
# (basketball), which must read as a zero WEIGHT rather than as a dropped term
# -- `c(win = 2L, draw = NULL, loss = 0L)` silently produces a 2-element vector.
.publish_points_scheme <- function(profile) {
  pick <- function(k) {
    v <- profile$points[[k]]
    if (is.null(v)) 0L else as.integer(v)
  }
  c(win = pick("win"), draw = pick("draw"), loss = pick("loss"))
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
.empty_round_predictions_pfi <- function() {
  tibble::tibble(
    round = integer(), team = character(), fit_date = character(),
    n_matches = integer(),
    xg_for = numeric(), xg_against = numeric(), xpts = numeric(),
    p_win = numeric(), p_draw = numeric(), p_loss = numeric()
  )
}

.aggregate_round_predictions_pfi <- function(played_matches,
                                             extracts_root, archive_root,
                                             sport, country, sex,
                                             target_div = "BD") {
  if (nrow(played_matches) == 0L) {
    return(.empty_round_predictions_pfi())
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
    return(.empty_round_predictions_pfi())
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
  if (nrow(quantiles) > 0L) {
    .assert_quantiles_available(quantiles$quantile, needed, "intervals")
  }
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

#' Publish an Icelandic league's posterior summaries as JSON
#'
#' The one publisher for all three Icelandic leagues. Reads from the per-fit
#' extraction tree (the Parquets written by the sport's extractor) rather than
#' an in-memory fit RDS -- use [`read_extracted_iceland()`] to construct
#' `extracted`, or pass a hand-built list with the same shape (tests do the
#' latter).
#'
#' Everything that used to be a `sport == "football"` literal is now data:
#' the division set comes from `config/leagues.yml::<key>.publish_divisions`
#' via the `.iceland_division_*()` accessors, and the sport-specific
#' behaviour comes from `profile` (see [`sport_publish_profile()`]).
#'
#' Writes seven snapshot JSONs into
#' `output_root/<sport>/iceland/{karla|kvenna}-{slug}/`:
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
#' @param extracted Named list returned by [`read_extracted_iceland()`]:
#'   six tibbles plus optionally `fit_date`. The required tibbles are
#'   `predicted_matches`, `team_strengths_quantiles`,
#'   `round_strengths_quantiles`, `home_advantage_quantiles`,
#'   `final_positions`, `points_distribution`.
#' @param league A single entry from `load_leagues()` (must have `sport`
#'   and `country` set). Its `<sport>_<country>` key must exist in
#'   `config/leagues.yml`.
#' @param sex `"male"` or `"female"`.
#' @param profile Per-sport publish profile; see [`sport_publish_profile()`].
#' @param end_date Training cutoff passed to `prepare_data()`. Default
#'   `Sys.Date()`.
#' @param root Data root for `read_table()`. Default `here::here("data")`.
#' @param output_root Root for JSON output. Default
#'   `here::here("data", "publish")`.
#' @param extracts_root Root of the per-fit extracts tree used to
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
publish_iceland_league <- function(extracted,
                                   league,
                                   sex,
                                   profile = sport_publish_profile(
                                     league$sport
                                   ),
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
  stopifnot(league$sport %in% c("football", "basketball", "handball"))
  stopifnot(league$country == "iceland")
  stopifnot(inherits(end_date, "Date"))
  # DERIVED, then asserted -- the `<sport>_<country>` convention holds for all
  # three Icelandic leagues today, but a future league that breaks it must
  # fail here rather than silently read another cell's publish_divisions.
  league_key <- paste0(league$sport, "_", league$country)
  stopifnot(league_key %in% names(load_leagues()))
  # WHY: meta.json::fit_date must reflect the partition the publisher
  # actually read from, not the caller's `end_date`. Otherwise a refit
  # failure that falls back to stale extracts ships a meta.json claiming
  # today's date while the posteriors are from days ago (2026-05-12 audit).
  fit_date_stamp <- if (!is.null(extracted$fit_date)) {
    as.Date(extracted$fit_date)
  } else {
    end_date
  }
  required_slots <- profile$required_extracts
  empty_slot <- function() profile$empty_extracts
  # Canonical filter codes (matching results$division), mapped to URL-friendly
  # display suffixes for the output directory name. LD1 -> "ld" because the
  # platform's URL slug is /lengja/ -> karla-ld/, not karla-ld1/. CUP ->
  # "bikar" for the Mjólkurbikar tab. Driven by
  # config/leagues.yml::football_iceland.publish_divisions[[sex]] so adding
  # a new cell is a config-only change here.
  division_dir_suffix <- .iceland_division_slugs(league_key, sex)
  division_codes <- names(division_dir_suffix)
  division_labels_is <- .iceland_division_labels(league_key, sex)
  division_is_cup <- .iceland_division_is_cup(league_key, sex)
  # meta v2's per-division attributes, all absent-safe. Read through the typed
  # accessors rather than the raw config entry: `qualify` is NULL or
  # list(slots, label_is), the other two are NA_integer_ where unset.
  division_qualify <- .iceland_division_qualify(league_key, sex)
  division_relegation <- .iceland_division_relegation(league_key, sex)
  division_meetings <- .iceland_division_expected_meetings(league_key, sex)
  division_regular_rounds <-
    .iceland_division_regular_season_rounds(league_key, sex)

  # extracted shape: list keyed by division code (BD, LD1, ...) — see
  # read_extracted_iceland(). Each per-division list has the 6 parquet
  # tibbles. A non-FATAL legacy fallback: if `extracted` is flat (the
  # pre-2026-05-04 single-division shape), wrap it as BD-only and emit
  # empty cells for every other configured division.
  if (all(required_slots %in% names(extracted))) {
    flat_legacy <- extracted[required_slots]
    fit_date_keep <- extracted$fit_date
    extracted <- list(BD = flat_legacy)
    for (div in setdiff(division_codes, "BD")) {
      extracted[[div]] <- empty_slot()
    }
    extracted$fit_date <- fit_date_keep
  }
  for (div in division_codes) {
    if (is.null(extracted[[div]])) {
      extracted[[div]] <- empty_slot()
    }
    div_slots <- if (is.list(extracted[[div]])) names(extracted[[div]]) else character()
    missing_slots <- setdiff(required_slots, div_slots)
    if (length(missing_slots) > 0L) {
      stop(
        "publish_iceland_league: extracted$", div,
        " is missing slots: ", paste(missing_slots, collapse = ", "),
        call. = FALSE
      )
    }
  }

  points_scheme <- .publish_points_scheme(profile)

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

  # Forward fixtures, read once per (league, sex). They are the FALLBACK source
  # of n_rounds for a cell with no configured `expected_meetings` -- the only
  # such cell today is basketball's irregular 11-team female 1. deild. A root
  # with no schedules partition at all degrades to NULL rather than aborting a
  # publish: an absent fixture list is a missing fallback, not a broken cell.
  schedules <- tryCatch(
    read_table(
      "schedules",
      root   = root,
      filter = list(sport = league$sport, country = league$country, sex = sex)
    ),
    error = function(e) NULL
  )

  for (target_div in division_codes) {
    top_div <- target_div
    is_cup <- isTRUE(division_is_cup[[target_div]])
    # Gated: a sport with no split format never derives a split family, never
    # group-locks the standings rank and never emits meta.split. One predicate
    # switches the whole machinery because everything downstream keys off
    # `div_split` being non-NULL.
    div_split <- if ("split" %in% profile$surfaces) {
      .iceland_division_split(league_key, sex)[[target_div]]
    } else {
      NULL
    }
    # For a split cell the season spans the regular division plus its
    # split-phase playoff divisions -- every per-season surface below
    # (standings, next_games, round counting, xG/xPts aggregation input)
    # filters on the family, not the bare code.
    family_divs <- .split_family_divisions_pfi(target_div, div_split)
    # Per-division extracted slice — already filtered to this division's
    # teams + matches by extract_football_iceland(). The publisher used
    # to apply `semi_join(current_top_teams)` defensively; with per-cell
    # extracts those filters are redundant (and would no-op anyway).
    ext <- extracted[[target_div]]
    if (is.null(ext)) ext <- empty_slot()
    out_dir <- file.path(
      output_root,
      league$sport,
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

    top_teams_upcoming <- pred_d[pred_d$division %in% family_divs, , drop = FALSE] |>
      dplyr::select("home_team", "away_team") |>
      tidyr::pivot_longer(c("home_team", "away_team"), values_to = "team") |>
      dplyr::distinct(.data$team)
    if (nrow(top_teams_upcoming) == 0L) {
      top_teams_upcoming <- current_top_teams
    }

    bd_played <- results[
      results$season == current_season & results$division %in% family_divs, ,
      drop = FALSE
    ]
    # Gated: xG / xPts are a football surface. A sport without it skips the
    # pre-round archive scan entirely and its standings ship the same columns
    # as null -- config/publish-schemas/.../standings.schema.json already types
    # them ["number","null"], which is why one schema shape serves all three.
    has_xg <- "xg" %in% profile$surfaces
    round_predictions <- if (has_xg) {
      .aggregate_round_predictions_pfi(
        played_matches = bd_played[, c("home_team", "away_team", "match_date")],
        extracts_root = extracts_root,
        archive_root = archive_root,
        sport = league$sport, country = league$country, sex = sex,
        target_div = target_div
      )
    } else {
      .empty_round_predictions_pfi()
    }

    team_expected <- if (has_xg && nrow(bd_played) > 0L) {
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

    generated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")

    # ---- meta.json ----------------------------------------------------------

    # One derivation of the season's shape, shared with the extractor
    # (R/publish-format.R). `round` is the floor over the cell's teams with
    # post-season rows excluded, so basketball male Bonusdeild reads 22 of 22
    # rather than 35 of 22 -- the -13 "Umferdir eftir" the platform would
    # otherwise render.
    division_cfg <- list(
      qualify = division_qualify[[target_div]],
      relegation_slots = division_relegation[[target_div]],
      expected_meetings = division_meetings[[target_div]],
      regular_season_rounds = division_regular_rounds[[target_div]]
    )
    format_facts <- .publish_n_rounds(
      results = results,
      schedules = schedules,
      season = current_season,
      division_codes = family_divs,
      end_date = end_date,
      expected_meetings = division_cfg$expected_meetings,
      regular_season_rounds = division_cfg$regular_season_rounds,
      is_cup = is_cup
    )
    round_num <- .publish_round(
      results, current_season, family_divs,
      n_rounds = format_facts$n_rounds, cut = format_facts$cut
    )

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
    } else if (identical(profile$predicted_matches_shape, "scoreline_counts") &&
      nrow(predicted_matches) > 0L) {
      per_match_count <- predicted_matches |>
        dplyr::summarise(
          s = sum(.data$count),
          .by = c("home_team", "away_team", "match_date")
        )
      as.integer(round(mean(per_match_count$s)))
    } else if (!is.null(extracted$fit_meta) &&
      nrow(extracted$fit_meta) > 0L &&
      !is.na(extracted$fit_meta$n_draws[[1L]])) {
      # The match-summary shape carries no per-draw count to recover the count
      # from, so the 2DT sports read the partition-level fit_meta table. It was
      # already written by both extractors; the reader was splitting it by
      # `division`, a column it deliberately does not have, which emptied it on
      # every cell and is why every basketball and handball cell published
      # `n_draws: 0`.
      #
      # It sits BELOW the two football branches on purpose: on a real football
      # partition all three agree (the scoreline counts sum to the draw count),
      # so ordering fit_meta first would move no production number but would
      # move the pinned fixture, whose synthetic counts round to 48 against a
      # fit_meta of 50.
      as.integer(extracted$fit_meta$n_draws[[1L]])
    } else {
      0L
    }

    league_label <- division_labels_is[[target_div]]
    meta_base <- list(
      sport        = league$sport,
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
    if (!is.null(div_split)) {
      # Split-season cell: final_positions placement is full-season
      # (1 = champion); the platform renders group boundaries from this.
      meta_base$split <- list(
        upper = as.integer(div_split$upper),
        lower = as.integer(div_split$lower)
      )
    }
    meta <- .build_publish_meta(
      base = meta_base, profile = profile,
      format = format_facts, division_cfg = division_cfg
    )
    write_json_consistent(
      meta,
      file.path(out_dir, "meta.json"),
      # null = "null" so `postseason` (football), `points$draw` (basketball)
      # and an unconfigured `qualify` reach the consumer as JSON null instead
      # of as an empty object. No v1 key is ever NULL, so the football payload
      # is unaffected by the flag itself.
      auto_unbox = TRUE, null = "null"
    )

    # ---- next_games.json ----------------------------------------------------

    next_games_out <- .next_games_rows_pfi(
      predicted = predicted_matches,
      profile = profile,
      pred_d = pred_d,
      family_divs = family_divs,
      division_badges = .iceland_division_badges(league_key, sex),
      end_date = end_date,
      venues = .publish_venues_pfi(league$sport)
    )

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
      results$season == current_season & results$division %in% family_divs, ,
      drop = FALSE
    ]
    # THE REGULAR-SEASON BOUNDARY, the same call the extractor makes. Basketball
    # embeds its urslitakeppni in the league division, so without this the
    # standings, the base points feeding points_distribution and the as_of
    # stamp all count post-season matches -- while final_positions, cut at
    # extract time, does not. The two cuts must be the same cut.
    bd_results <- .regular_season_cut(bd_results, format_facts)

    # Split-group membership for the group-locked rank. Derived once the
    # split phase is observable (played playoff matches, or upcoming ones
    # in the prediction window); until then the plain ranking IS the
    # official table, so no lock applies.
    split_groups_pub <- NULL
    if (!is.null(div_split) && nrow(bd_results) > 0L) {
      po_divs <- family_divs[-1]
      po_observed <- dplyr::bind_rows(
        bd_results[
          bd_results$division %in% po_divs,
          c("home_team", "away_team", "division")
        ],
        pred_d[
          pred_d$division %in% po_divs,
          c("home_team", "away_team", "division")
        ]
      )
      if (nrow(po_observed) > 0L) {
        reg_table <- .realised_league_table_pfi(
          bd_results[bd_results$division == top_div, , drop = FALSE],
          current_top_teams
        )
        ranked_teams <- reg_table$team[
          order(-reg_table$base_points, -reg_table$base_gd, -reg_table$base_gf)
        ]
        split_groups_pub <- .split_group_membership_pfi(
          ranked_teams = ranked_teams,
          observed = po_observed,
          upper_n = as.integer(div_split$upper),
          lower_n = as.integer(div_split$lower),
          target_div = target_div
        )
      }
    }

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
          points = points_scheme[["win"]] * .data$wins +
            points_scheme[["draw"]] * .data$draws +
            points_scheme[["loss"]] * .data$losses,
          form = list(pad_form(tail(.data$result, 5L))),
          goals_trend = list(.data$gf),
          goals_against_trend = list(.data$ga),
          .by = "team"
        ) |>
        dplyr::left_join(
          if (is.null(split_groups_pub)) {
            tibble::tibble(team = character(), .split_group = character())
          } else {
            dplyr::rename(split_groups_pub, .split_group = "group")
          },
          by = "team"
        ) |>
        dplyr::arrange(
          # Group-locked once the split is known: every efri team above
          # every nedri team regardless of carried points (KSI rule).
          dplyr::coalesce(.data$.split_group, "upper") != "upper",
          dplyr::desc(.data$points), dplyr::desc(.data$goal_diff),
          dplyr::desc(.data$goals_for)
        ) |>
        dplyr::select(-".split_group") |>
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
    preseason_intervals <- if ("preseason_strengths" %in% profile$surfaces &&
      nrow(bd_played) > 0L) {
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

    if ("round_predictions_history" %in% profile$surfaces) {
      round_predictions_dir <- file.path(
        round_predictions_history_root,
        league$sport, "iceland",
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
    }

    # ---- final_positions.json + points_distribution.json --------------------

    # Per-cell extracted slices are pre-filtered to the division's teams
    # by extract_football_iceland(); no further semi_join needed here.
    final_positions <- ext$final_positions
    points_distribution <- ext$points_distribution

    if (nrow(final_positions) > 0L) {
      n_teams_top <- length(unique(final_positions$team))

      placement_summary <- .build_placement_summary(
        final_positions,
        n_teams = n_teams_top,
        basis = profile$placement_basis,
        qualify = division_cfg$qualify,
        relegation_slots = division_cfg$relegation_slots,
        # Football only, and deprecated: see .build_placement_summary().
        emit_top_six_alias = identical(league$sport, "football")
      )

      write_json_consistent(
        list(
          generated_at = generated_at,
          season       = current_season,
          # What a placement MEANS. "regular_season_table" says the champion of
          # this table is the deildarmeistari and the Islandsmeistari comes out
          # of an urslitakeppni the model does not simulate (design 15, D3).
          basis        = profile$placement_basis,
          n_teams      = n_teams_top,
          records      = final_positions,
          summary      = placement_summary
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
          # The SPORT's scheme, not football's. This block used to hardcode
          # 3/1/0, so every basketball cell published base_points = 3 x wins
          # against a standings table built on 2 x wins from the same data --
          # a team whose "current points" exceeded its own projected final
          # points. points_scheme is c(win, draw, loss) from the profile, and
          # is literally c(3L, 1L, 0L) for football, so football's bytes and
          # its golden hashes are unchanged.
          points = dplyr::case_when(
            .data$result == "tie" ~ points_scheme[["draw"]],
            .data$result == .data$name ~ points_scheme[["win"]],
            TRUE ~ points_scheme[["loss"]]
          )
        ) |>
        dplyr::summarise(base_points = sum(.data$points), .by = "team")

      # points_distribution.json's summary carries the placement columns it has
      # always carried and gains nothing here. It is one of the eight artefacts
      # the meta v2 golden regeneration asserts BYTE-IDENTICAL, so the generic
      # pair (p_qualify / p_top_of_table) lives in final_positions.json only;
      # the commit that retires the p_top_six alias collapses both at once.
      # bb/hb have no p_top_six and no p_winner to carry, so they ship
      # p_top_of_table in their place.
      points_placement <- placement_summary[
        , intersect(
          if ("p_top_six" %in% names(placement_summary)) {
            c("team", "p_top_six", "p_winner", "p_relegation")
          } else {
            c("team", "p_top_of_table", "p_relegation")
          },
          names(placement_summary)
        ),
        drop = FALSE
      ]

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
        dplyr::left_join(points_placement, by = "team")

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
          basis = profile$placement_basis,
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
    if (is_cup && "cup_bracket" %in% profile$surfaces) {
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

      # ---- bracket.json (cup cells with a live frontier) --------------------
      # Pre-built in the extract layer (it needs the transient bracket_state)
      # and round-tripped through cup_bracket.parquet. Mirrors the World Cup
      # bracket.json contract so the platform's interactive what-if tree
      # (cup-bracket.js) drives off the same shape. Additive: skipped when
      # there's no live frontier (fully resolved / entry round undrawn).
      cup_bracket <- extracted$cup_bracket
      if (!is.null(cup_bracket) && length(cup_bracket) > 0L) {
        jsonlite::write_json(
          cup_bracket,
          file.path(out_dir, "bracket.json"),
          auto_unbox = TRUE, matrix = "rowmajor"
        )
      }
    }

    n_files <- length(list.files(out_dir, pattern = "\\.json$"))
    message(sprintf(
      "publish_iceland_league: wrote %d JSONs to %s", n_files, out_dir
    ))
  }

  invisible(NULL)
}
