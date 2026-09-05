#' @include config.R
NULL

#' Validate team-name configuration for a recommendations tibble.
#'
#' Checks that every (sport, country) combination present in \code{recs} has a
#' non-empty \code{lengjan$team_names} map in \code{leagues}, and that every
#' team name appearing in \code{recs} is keyed in that map.  Fails fast with
#' \code{stop()} so the caller does not waste a Chromote session on a
#' misconfigured league.
#'
#' @param leagues Named list from \code{load_leagues()}.
#' @param recs Recommendations tibble.  Must contain \code{sport},
#'   \code{country}, \code{sex}, \code{home_team}, and \code{away_team}
#'   columns.  \code{sex} must be one of \code{"male"} or \code{"female"};
#'   the team_names lookup is sex-keyed.
#' @return Invisibly \code{TRUE} on success.
#' @export
validate_team_names_config <- function(leagues, recs) {
  if (!is.list(leagues)) {
    stop(
      "validate_team_names_config: `leagues` must be a named list ",
      "from load_leagues()",
      call. = FALSE
    )
  }
  needed_cols <- c("sport", "country", "sex", "home_team", "away_team")
  missing_input <- setdiff(needed_cols, names(recs))
  if (length(missing_input) > 0L) {
    stop(
      "validate_team_names_config: `recs` missing column(s): ",
      paste(missing_input, collapse = ", "),
      call. = FALSE
    )
  }

  valid_sexes <- c("male", "female")
  bad_sex <- setdiff(unique(recs$sex), valid_sexes)
  if (length(bad_sex) > 0L) {
    stop(
      "validate_team_names_config: rec(s) have invalid sex value(s): ",
      paste(bad_sex, collapse = ", "),
      ". Expected one of: ", paste(valid_sexes, collapse = ", "),
      call. = FALSE
    )
  }

  groups <- unique(recs[, c("sport", "country", "sex"), drop = FALSE])
  for (i in seq_len(nrow(groups))) {
    sp <- groups$sport[i]
    co <- groups$country[i]
    sx <- groups$sex[i]
    key <- paste0(sp, "_", co)

    if (!key %in% names(leagues)) {
      stop(
        "validate_team_names_config: no leagues.yml entry for ", key,
        call. = FALSE
      )
    }

    league <- leagues[[key]]
    tn_all <- league$lengjan$team_names

    if (is.null(tn_all) || length(tn_all) == 0L) {
      stop(
        "validate_team_names_config: ", key,
        " has no lengjan$team_names. Add ",
        "team_names: {male: {...}, female: {...}} to config/leagues.yml.",
        call. = FALSE
      )
    }

    if (!sx %in% names(tn_all)) {
      stop(
        "validate_team_names_config: ", key,
        " is missing the `", sx, "` sub-map under lengjan.team_names. ",
        "Both `male` and `female` keys are required ",
        "(use `{}` for an empty sub-map).",
        call. = FALSE
      )
    }

    tn <- tn_all[[sx]]

    rows <- recs[
      recs$sport == sp & recs$country == co & recs$sex == sx, ,
      drop = FALSE
    ]
    teams <- unique(c(rows$home_team, rows$away_team))

    if (length(tn) == 0L) {
      stop(
        "validate_team_names_config: ", key, " (", sx, ") ",
        "has an empty team_names sub-map. Add canonical -> Lengjan-display ",
        "mappings under lengjan.team_names.", sx,
        " (source: data/facts/odds/sport=", sp, "/country=", co,
        " or wait for the next scrape). Missing teams: ",
        paste(teams, collapse = ", "),
        call. = FALSE
      )
    }

    missing_teams <- setdiff(teams, names(tn))

    if (length(missing_teams) > 0L) {
      stop(
        "validate_team_names_config: ", key, " (", sx, ") ",
        "is missing team_names for: ",
        paste(missing_teams, collapse = ", "),
        call. = FALSE
      )
    }

    # Injectivity: each Lengjan-side rendering must come from at most one
    # canonical name, or normalise_lengjan_team_names()'s inverse map silently
    # picks one. Shared with load_leagues()'s load-time guard.
    assert_injective_map(
      tn,
      label = paste0("validate_team_names_config: ", key, " (", sx, ")")
    )
  }

  invisible(TRUE)
}

#' Validate that a recommendations tibble has the expected column schema.
#'
#' Checks for the full set of columns that the placer pipeline expects
#' (i.e. what \code{decide_league()} writes).
#'
#' @param recs Tibble to validate.
#' @return Invisibly \code{TRUE} on success.
#' @export
validate_recommendations_schema <- function(recs) {
  required <- c(
    "sport", "country", "sex", "match_date",
    "home_team", "away_team",
    "market", "outcome", "line",
    "p", "odds", "ev", "kelly", "bet_amount"
  )

  missing_cols <- setdiff(required, names(recs))

  if (length(missing_cols) > 0L) {
    stop(
      "validate_recommendations_schema: missing column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Abort if any recommendation belongs to a betting-disabled league.
#'
#' Placer pre-flight, mirroring [validate_team_names_config()]: runs before
#' the browser is launched so a policy breach fails fast and loudly rather
#' than part-way through a placement run. Decision D2 (spec 2026-09-02
#' section 3).
#'
#' `load_recommendations()` already drops these rows, so reaching this error
#' means a caller bypassed the loader -- which is exactly when a loud abort
#' is wanted rather than a silent filter.
#'
#' @param leagues Named league config from [load_leagues()].
#' @param recs Recommendation rows about to be placed.
#' @return `invisible(TRUE)`, or stops naming every offending league.
#' @export
validate_betting_enabled <- function(leagues, recs) {
  if (!is.list(leagues)) {
    stop(
      "validate_betting_enabled: `leagues` must be a named list ",
      "from load_leagues()",
      call. = FALSE
    )
  }
  if (nrow(recs) == 0L) {
    return(invisible(TRUE))
  }
  missing_input <- setdiff(c("sport", "country"), names(recs))
  if (length(missing_input) > 0L) {
    stop(
      "validate_betting_enabled: `recs` missing column(s): ",
      paste(missing_input, collapse = ", "),
      call. = FALSE
    )
  }

  disabled <- names(leagues)[!vapply(leagues, betting_enabled, logical(1))]
  offending <- intersect(unique(paste0(recs$sport, "_", recs$country)), disabled)
  if (length(offending) > 0L) {
    stop(
      "validate_betting_enabled: refusing to place bets on ",
      "betting-disabled league(s): ", paste(offending, collapse = ", "),
      ". Set betting.enabled: true in config/leagues.yml to re-arm.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}
