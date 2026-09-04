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
