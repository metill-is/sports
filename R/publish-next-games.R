#' @include publish-profile.R
NULL

# ---- One next_games contract, two predicted_matches shapes -------------------
#
# The two shapes are a real per-sport difference in the extract parquets, not
# drift:
#   football ("scoreline_counts")  -- predicted_matches.parquet is a scoreline
#     COUNT table (home_team, away_team, match_date, home_goals, away_goals,
#     count) written by extract_football_iceland(); the per-match summary is
#     aggregated here.
#   basketball / handball ("match_summary") -- predicted_matches.parquet is
#     already per-match (game_nr, match_date, home_team, away_team, division,
#     mean_home_goals, mean_away_goals, mean_goal_diff, p_home_win, p_draw,
#     p_away_win, goal_diff_distribution), written by
#     .compute_predicted_matches_2dt(); this branch filters, joins and renames
#     and computes nothing.
#
# Both branches emit .NEXT_GAMES_COLUMNS, in that order, with the same column
# classes -- so next_games.json has one contract for all three sports and the
# platform's fixture strip reads `goal_diff_distribution` everywhere.

.NEXT_GAMES_COLUMNS <- c(
  "date", "venue", "division", "division_code", "home", "away",
  "mean_home_goals", "mean_away_goals", "mean_goal_diff",
  "p_home_win", "p_draw", "p_away_win", "goal_diff_distribution"
)

# The one empty shape both branches degrade to.
.next_games_empty_pfi <- function() {
  tibble::tibble(
    date = character(), venue = character(),
    division = character(), division_code = character(),
    home = character(), away = character(),
    mean_home_goals = numeric(), mean_away_goals = numeric(),
    mean_goal_diff = numeric(), p_home_win = numeric(),
    p_draw = numeric(), p_away_win = numeric(),
    goal_diff_distribution = list()
  )
}

# Upcoming-fixture rows for one publish cell.
#
# @param predicted The cell's `predicted_matches` tibble, in whichever shape
#   `profile$predicted_matches_shape` declares.
# @param profile `sport_publish_profile(<sport>)`.
# @param pred_d The model's prediction frame; the ONLY source of the `division`
#   column for the scoreline-counts shape (football's predicted_matches carries
#   no division). Ignored for the match-summary shape, which already has one.
# @param family_divs Division codes this cell publishes (a split-season cell
#   carries its playoff codes too).
# @param division_badges Named `code -> badge` map, from
#   `.iceland_division_badges(league_key, sex)`.
# @param end_date Publish cutoff; the window is [end_date, end_date + horizon_days].
# @param horizon_days Fixture horizon in days.
# @param venues Optional tibble(team, venue) joined on the HOME team. `NULL`
#   leaves `venue` NA -- the platform renders the field as optional.
# @noRd
.next_games_rows_pfi <- function(predicted, profile, pred_d = NULL,
                                 family_divs, division_badges, end_date,
                                 horizon_days = 14L, venues = NULL) {
  stopifnot(is.data.frame(predicted))
  shape <- profile$predicted_matches_shape

  if (identical(shape, "scoreline_counts")) {
    predicted <- if (nrow(predicted) > 0L) {
      predicted |>
        dplyr::left_join(
          pred_d |> dplyr::distinct(
            .data$home_team, .data$away_team,
            .data$match_date, .data$division
          ),
          by = c("home_team", "away_team", "match_date")
        )
    } else {
      predicted |> dplyr::mutate(division = character(0))
    }
    if (nrow(predicted) == 0L) {
      return(.next_games_empty_pfi())
    }
    out <- predicted |>
      dplyr::filter(
        .data$division %in% family_divs,
        .data$match_date >= end_date,
        .data$match_date <= end_date + horizon_days
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
      dplyr::arrange(.data$match_date, .data$home_team, .data$away_team)
  } else if (identical(shape, "match_summary")) {
    if (nrow(predicted) == 0L) {
      return(.next_games_empty_pfi())
    }
    # Two callers, two shapes. read_extracted_iceland() splits the partition by
    # `division` and then DROPS the column, so a per-division slice arrives
    # without it; a raw partition-wide parquet arrives with it and must still be
    # filtered. Restoring it from the cell's own family (first element = the
    # cell's code) keeps one filter serving both, and keeps `division` in the
    # output contract for the platform.
    if (!"division" %in% names(predicted)) {
      predicted$division <- family_divs[[1L]]
    }
    out <- predicted |>
      dplyr::filter(
        .data$division %in% family_divs,
        .data$match_date >= end_date,
        .data$match_date <= end_date + horizon_days
      ) |>
      dplyr::arrange(.data$match_date, .data$game_nr)
  } else {
    cli::cli_abort("Unknown predicted_matches_shape {.val {shape}}.", call = NULL)
  }

  out <- if (is.null(venues)) {
    dplyr::mutate(out, venue = NA_character_)
  } else {
    dplyr::left_join(out, venues, by = c("home_team" = "team"))
  }

  out |>
    dplyr::mutate(
      division_code = dplyr::recode(
        .data$division, !!!division_badges,
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
}

# Static home-ground lookup for the football male top flight, joined on the HOME
# team. Returns NULL for every other sport, and that is the point rather than a
# tidiness choice: Valur, KA, Fram, IBV, Stjarnan and Breidablik all field
# handball and basketball teams under the SAME club name, so handing this table
# to another sport publishes an outdoor football ground for an indoor fixture.
# The 2DT sports have no ground table, so `venue` is null for them -- which is
# what the platform's fixture card already treats as optional.
# @noRd
.publish_venues_pfi <- function(sport) {
  if (!identical(sport, "football")) {
    return(NULL)
  }
  tibble::tribble(
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
}
