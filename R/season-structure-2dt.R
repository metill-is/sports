#' @include publish-format.R extract-football-iceland.R publish-iceland-2dt-helpers.R
NULL

# Season, format and remaining fixtures for the 2DT sports (spec 2026-09-16
# §5-§6). The extractor and the publisher both call these, so the two layers
# cannot disagree about which season is current or how long it is.

.is_set_2dt <- function(x) {
  length(x) == 1L && !is.na(x)
}

# The current season of one division: the latest season with a played result
# on or before `end_date`, or with a fixture scheduled after it.
#
# Results alone (the old rule, F8) keep a finished season current until its
# successor's first match, so a pre-season forecast showed last season's
# table. Football keeps that rule (D5): KSI publishes its schedule in halves.
#
# `hold` pins a division (config `preseason_hold`): its schedule is ignored and
# the result never passes the held season, even once the next season starts.
.current_season_2dt <- function(results, schedules, end_date, division,
                                hold = NA_integer_) {
  end_date <- as.Date(end_date)
  seasons <- integer()
  if (!is.null(results) && nrow(results) > 0L) {
    played <- results$division %in% division &
      !is.na(results$match_date) & results$match_date <= end_date &
      !is.na(results$home_score) & !is.na(results$away_score)
    seasons <- c(seasons, results$season[played])
  }
  held <- .is_set_2dt(hold)
  if (!held && !is.null(schedules) && nrow(schedules) > 0L) {
    ahead <- schedules$division %in% division &
      !is.na(schedules$match_date) & schedules$match_date > end_date
    seasons <- c(seasons, schedules$season[ahead])
  }
  seasons <- seasons[!is.na(seasons)]
  season <- if (length(seasons) == 0L) {
    as.integer(format(end_date, "%Y"))
  } else {
    as.integer(max(seasons))
  }
  if (held) {
    season <- min(season, as.integer(hold))
  }
  season
}
