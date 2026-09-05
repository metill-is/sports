#' @include health.R publish-profile.R publish-pipeline.R
NULL

# ---- The publish-freshness check --------------------------------------------
#
# WHY THIS EXISTS. From the Plan-7 cutover to 2026-09, basketball and handball
# published nothing at all from CI: publish_one() took the extracts branch only
# for football and every other league fell through to a gitignored fit RDS that
# CI never produces. It warned and returned invisible(NULL), the workflow
# exited 0, and pipeline_health() composed nine checks not one of which read
# data/publish/. The pipeline was green for months while two of three sports
# were dark. This check is the row that would have said so.

# profile$surfaces is NOT a list of files. Football's 16 entries include five
# payload FEATURES that live inside other JSONs (xg, split,
# preseason_strengths, round_predictions_history) and `cup_bracket`, whose file
# is bracket.json. This maps the surfaces that DO name a file to that file's
# basename; anything absent from the map is a feature and contributes no
# artefact. `bracket` is accepted as a key too so an injected test stub may use
# either name.
.PUBLISH_SURFACE_FILES <- c(
  meta = "meta",
  next_games = "next_games",
  standings = "standings",
  standings_history = "standings_history",
  team_strengths = "team_strengths",
  team_strengths_history = "team_strengths_history",
  final_positions = "final_positions",
  final_positions_history = "final_positions_history",
  points_distribution = "points_distribution",
  home_advantage = "home_advantage",
  tournament_placements = "tournament_placements",
  cup_bracket = "bracket",
  bracket = "bracket"
)

# Emitted by a cup cell and only by a cup cell. Verified on the live tree:
# football's seven league cells hold 10 JSONs each, its two bikar cells 12.
.PUBLISH_CUP_ONLY_SURFACES <- c("tournament_placements", "cup_bracket", "bracket")

#' @noRd
.publish_surfaces <- function(sport) {
  # No fallback, deliberately: a sport with no publish profile is a
  # configuration bug, and a default surface list would hide it behind a
  # plausible-looking OK row.
  sport_publish_profile(sport)$surfaces
}

#' Parse a publish timestamp, tolerating both shapes that exist on disk.
#'
#' The publishers write `%Y-%m-%dT%H:%M:%S%z`; `write_health_status()` writes a
#' literal `Z`. A parser handling only one would silently return `NA` for every
#' file it met, turning the whole check into a false FAIL. Returns `NA` rather
#' than erroring on anything unparseable, because a corrupt stamp must become a
#' FAIL row with a scope, never an abort inside `pipeline_health()`'s `safe()`
#' wrapper that collapses the entire check to one `check_error` row.
#' @noRd
.parse_publish_stamp <- function(x) {
  na <- as.POSIXct(NA_character_, tz = "UTC")
  if (is.null(x) || length(x) != 1L || is.na(x) || !nzchar(as.character(x))) {
    return(na)
  }
  x <- as.character(x)
  for (fmt in c("%Y-%m-%dT%H:%M:%S%z", "%Y-%m-%dT%H:%M:%SZ")) {
    parsed <- suppressWarnings(as.POSIXct(x, format = fmt, tz = "UTC"))
    if (!is.na(parsed)) {
      return(parsed)
    }
  }
  na
}

#' Directory a published cell lands in.
#'
#' Mirrors the publisher (`R/publish-iceland-league.R`), including its literal
#' `"iceland"` segment -- every publishing league is Icelandic, and deriving the
#' country here would let this check look somewhere the publisher never writes.
#' @noRd
.publish_cell_dir <- function(root, sport, sex, slug) {
  sex_folder <- if (identical(sex, "male")) "karla" else "kvenna"
  file.path(root, "publish", sport, "iceland", paste0(sex_folder, "-", slug))
}

#' JSON basenames (no extension) a cell of this sport is expected to hold.
#' @noRd
.expected_publish_artefacts <- function(sport, is_cup,
                                        surfaces_for = .publish_surfaces) {
  surfaces <- surfaces_for(sport)
  keep <- intersect(surfaces, names(.PUBLISH_SURFACE_FILES))
  if (!isTRUE(is_cup)) {
    keep <- setdiff(keep, .PUBLISH_CUP_ONLY_SURFACES)
  }
  unique(unname(.PUBLISH_SURFACE_FILES[keep]))
}

#' Is every in-season publishing cell actually publishing, and recently?
#'
#' One row per (league, sex, division) in `publish_divisions`, for active
#' leagues only. Statuses, and why each is the severity it is:
#'
#' * `PAUSED` -- the cell has no upcoming fixture inside
#'   [has_upcoming_games()]'s horizon. Genuinely off-season; not a fault.
#' * `FAIL` -- in-season and there is no publish directory, no `meta.json`, an
#'   unparseable or stale `generated_at`, or an EXPECTED artefact is missing.
#'   The in-season-with-nothing-on-disk case is the one this check exists for:
#'   it is precisely the state basketball and handball were in from the Plan-7
#'   cutover to 2026-09, and calling it `PAUSED` would re-hide exactly the
#'   breakage the check was built to find. When there is also no extract
#'   partition the value says so, because that names the real cause -- the fit
#'   ran but the extractor did not, or neither ran, and the publisher had
#'   nothing to read.
#' * `WARN` -- an UNEXPECTED extra artefact and nothing else wrong. The
#'   comparison is deliberately asymmetric (INT-3): a missing artefact is a 404
#'   on metill-platform, while an extra one is leftover output from an older
#'   shape -- worth looking at, not an outage.
#'
#' Every filesystem read is wrapped, so a corrupt `meta.json` becomes a FAIL
#' row with a scope rather than an abort. [pipeline_health()]'s `safe()` wrapper
#' would otherwise collapse the whole check into a single `check_error` row and
#' lose every cell's identity.
#'
#' HONEST LIMIT. The alert channel is a GitHub workflow-failure email. That is
#' signal, not a pager: `healthcheck.yml` runs twice daily and fails the run on
#' `overall == "FAIL"`; there is no push notification, no escalation and no
#' on-call. A FAIL introduced here is noticed within roughly twelve hours if the
#' maintainer reads mail, and not at all if they do not. Because the channel is
#' that low-bandwidth, a check that is permanently WARN is worse than no check.
#'
#' @param leagues Leagues list. Read as the object passed in -- never via
#'   `load_leagues()` or the `.iceland_division_*()` accessors, which call it
#'   internally with no injection seam and would make every test read the real
#'   `config/leagues.yml`.
#' @param root Data root.
#' @param now Reference time.
#' @param th Thresholds from [health_thresholds()].
#' @param surfaces_for Injectable `sport -> character()` surface lookup.
#' @param extract_exists_fn Injectable `extract_partition_exists()`.
#' @return A health tibble of `check`/`scope`/`status`/`value`/`threshold`.
#' @noRd
check_publish_freshness <- function(leagues,
                                    root = here::here("data"),
                                    now = Sys.time(),
                                    th = health_thresholds(),
                                    surfaces_for = .publish_surfaces,
                                    extract_exists_fn = extract_partition_exists) {
  max_age <- th$publish_max_age_hours
  thr_lbl <- paste0(max_age, "h")
  extracts_root <- file.path(root, "beliefs", "extracts")
  rows <- list()

  for (key in names(leagues)) {
    lg <- leagues[[key]]
    if (!isTRUE(lg$active) || is.null(lg$publish_divisions)) next

    for (sex in .cell_sexes(lg)) {
      divs <- lg$publish_divisions[[sex]]
      if (is.null(divs) || length(divs) == 0L) next

      upcoming <- tryCatch(
        has_upcoming_games(
          list(sport = lg$sport, country = lg$country), sex,
          root = root
        ),
        error = function(e) FALSE
      )

      for (d in divs) {
        scope <- paste(key, sex, d$code)
        row <- tryCatch(
          .publish_cell_status(
            lg, sex, d, root, extracts_root, now, max_age,
            upcoming, surfaces_for, extract_exists_fn
          ),
          error = function(e) list(status = "FAIL", value = conditionMessage(e))
        )
        rows[[length(rows) + 1L]] <- health_row(
          "publish_freshness", scope, row$status, row$value, thr_lbl
        )
      }
    }
  }

  if (length(rows) == 0L) health_empty() else dplyr::bind_rows(rows)
}

#' @noRd
.publish_cell_status <- function(lg, sex, d, root, extracts_root, now, max_age,
                                 upcoming, surfaces_for, extract_exists_fn) {
  if (!isTRUE(upcoming)) {
    return(list(status = "PAUSED", value = "no upcoming games (off-season)"))
  }

  dir <- .publish_cell_dir(root, lg$sport, sex, d$slug)
  meta_path <- file.path(dir, "meta.json")

  if (!dir.exists(dir) || !file.exists(meta_path)) {
    have_extract <- isTRUE(tryCatch(
      extract_exists_fn(extracts_root, lg$sport, lg$country, sex),
      error = function(e) FALSE
    ))
    what <- if (dir.exists(dir)) "meta.json missing" else "no publish output"
    why <- if (have_extract) {
      "; the extract partition exists, so the publisher ran and wrote nothing"
    } else {
      paste0(
        "; no extract partition for this cell either -- the fit ran but the ",
        "extractor did not, or neither ran, so the publisher had nothing to read"
      )
    }
    return(list(status = "FAIL", value = paste0(what, " (in-season)", why)))
  }

  meta <- jsonlite::read_json(meta_path, simplifyVector = FALSE)
  stamp <- .parse_publish_stamp(meta$generated_at)
  if (is.na(stamp)) {
    return(list(
      status = "FAIL",
      value = sprintf("unparseable generated_at (%s)", meta$generated_at %||% "absent")
    ))
  }
  age_h <- as.numeric(difftime(now, stamp, units = "hours"))
  if (age_h > max_age) {
    return(list(status = "FAIL", value = sprintf("%.0fh old", age_h)))
  }

  have <- tools::file_path_sans_ext(list.files(dir, pattern = "[.]json$"))
  want <- .expected_publish_artefacts(lg$sport, isTRUE(d$is_cup), surfaces_for)
  missing <- setdiff(want, have)
  extra <- setdiff(have, want)

  if (length(missing) > 0L) {
    return(list(
      status = "FAIL",
      value = sprintf(
        "%.0fh old; missing artefact(s): %s",
        age_h, paste(sort(missing), collapse = ", ")
      )
    ))
  }
  if (length(extra) > 0L) {
    return(list(
      status = "WARN",
      value = sprintf(
        "%.0fh old; unexpected artefact(s): %s",
        age_h, paste(sort(extra), collapse = ", ")
      )
    ))
  }
  list(
    status = "OK",
    value = sprintf("%.0fh old; %d artefact(s)", age_h, length(have))
  )
}

#' Does a published cell's derived format still match its configured one?
#'
#' WARN only, never FAIL: a competition format change is a thing to look at,
#' not an outage, and the alert channel is a twice-daily email.
#'
#' The motivating number is concrete. Icelandic women's handball plays a TRIPLE
#' round robin -- 8 teams, 84 matches, 21 rounds, `meetings = 3` -- against
#' `2 * (n_teams - 1) = 14`. Four of the seven measured 2DT cells disagree with
#' that formula, which is why `n_rounds` is derived and published upstream at
#' all instead of being recomputed by each consumer. When a federation changes a
#' format mid-season the derived and configured numbers part company, and this
#' is the row that says so -- with BOTH numbers in the value, because a WARN
#' reading only "n_rounds disagrees" costs a diagnostic round trip every time it
#' fires.
#'
#' Emits nothing at all for: a cup cell (INT-6 -- no league table, so
#' `expected_meetings * (n_teams - 1)` is meaningless); a cell with no publish
#' output or an unreadable `meta.json` (`check_publish_freshness()` owns that
#' failure, and two checks reporting one fault is noise); a pre-v2 `meta.json`
#' with no `n_rounds`; and a division with no configured `expected_meetings`,
#' which is optional in the config schema and genuinely absent for female
#' basketball 1D.
#'
#' A `config` value of `n_rounds_source` is never a disagreement: the configured
#' `expected_meetings` is where that number came from, so it cannot disagree
#' with itself. Only a `schedule`-derived count is compared.
#'
#' @param leagues Leagues list, read as passed in (INT-1).
#' @param root Data root.
#' @return A health tibble.
#' @noRd
check_publish_format_agreement <- function(leagues, root = here::here("data")) {
  rows <- list()
  for (key in names(leagues)) {
    lg <- leagues[[key]]
    if (!isTRUE(lg$active) || is.null(lg$publish_divisions)) next
    for (sex in .cell_sexes(lg)) {
      divs <- lg$publish_divisions[[sex]]
      if (is.null(divs) || length(divs) == 0L) next
      for (d in divs) {
        row <- tryCatch(
          .publish_format_row(lg, sex, d, root),
          error = function(e) NULL
        )
        if (is.null(row)) next
        rows[[length(rows) + 1L]] <- health_row(
          "publish_format", paste(key, sex, d$code),
          row$status, row$value, row$threshold
        )
      }
    }
  }
  if (length(rows) == 0L) health_empty() else dplyr::bind_rows(rows)
}

#' @noRd
.publish_format_row <- function(lg, sex, d, root) {
  if (isTRUE(d$is_cup)) {
    return(NULL)
  }
  dir <- .publish_cell_dir(root, lg$sport, sex, d$slug)
  meta_path <- file.path(dir, "meta.json")
  standings_path <- file.path(dir, "standings.json")
  if (!file.exists(meta_path) || !file.exists(standings_path)) {
    return(NULL)
  }

  meta <- jsonlite::read_json(meta_path, simplifyVector = FALSE)
  if (is.null(meta$n_rounds) || is.null(meta$n_rounds_source)) {
    return(NULL)
  }
  # Nothing configured to compare against is not a fault, and an OK row here
  # would claim an agreement that was never checked.
  if (is.null(d$expected_meetings) && is.null(d$qualify$slots)) {
    return(NULL)
  }

  # The standings row count is the cheapest honest source of n_teams: it is
  # what the publisher actually tabled for this cell.
  standings <- jsonlite::read_json(standings_path, simplifyVector = FALSE)
  n_teams <- length(standings$rows %||% list())
  if (n_teams < 2L) {
    return(NULL)
  }

  notes <- character()
  status <- "OK"

  meetings <- d$expected_meetings
  if (!is.null(meetings) && identical(as.character(meta$n_rounds_source), "schedule")) {
    derived <- as.integer(meetings) * (n_teams - 1L)
    if (!identical(as.integer(meta$n_rounds), derived)) {
      status <- "WARN"
      notes <- c(notes, sprintf(
        "published n_rounds=%d (from the schedule) vs %d configured (%d teams x %d meetings)",
        as.integer(meta$n_rounds), derived, n_teams, as.integer(meetings)
      ))
    }
  }

  # SC-5: the configured shape is `qualify: {slots, label_is}`. There is no
  # `qualify_slots` key anywhere -- writing one would fail leagues.yml schema
  # validation and take every script in the repo down with it.
  slots <- d$qualify$slots
  if (!is.null(slots) && as.integer(slots) >= n_teams) {
    status <- "WARN"
    notes <- c(notes, sprintf(
      "qualification cut takes %d of %d teams -- not a cut at all",
      as.integer(slots), n_teams
    ))
  }

  if (length(notes) == 0L) {
    notes <- sprintf(
      "n_rounds=%d (%s), %d teams",
      as.integer(meta$n_rounds), meta$n_rounds_source, n_teams
    )
  }
  list(
    status = status,
    value = paste(notes, collapse = "; "),
    threshold = "config agreement"
  )
}
