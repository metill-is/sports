#' Staleness detection — find league×sex combos that need a fresh fit
#'
#' A league×sex is "stale" when:
#'   1. It has betting enabled (has_bets: true)
#'   2. There are odds for upcoming matches (within lookahead_days)
#'   3. The model fit is missing or older than max_age_hours
#'
#' @usage
#' box::use(R/pipeline/staleness[find_stale_league_sexes])
#' stale <- find_stale_league_sexes(selected, sports_dir)

box::use(
  readr[read_file],
  yaml[read_yaml = yaml.load]
)

#' Find stale league×sex combinations
#'
#' @param selected Named list of league configs (from filter_leagues)
#' @param sports_dir Root Sports/ directory
#' @param lookahead_days How far ahead to check for odds (default 3)
#' @return List of list(key, sex, reason) for each stale combo
#' @export
find_stale_league_sexes <- function(selected, sports_dir, lookahead_days = 3) {
  today <- Sys.Date()
  stale <- list()

  for (key in names(selected)) {
    league <- selected[[key]]

    # Only check leagues with betting
    if (!isTRUE(league$has_bets)) next

    league_dir <- file.path(sports_dir, league$dir)
    bets_yml <- file.path(league_dir, "config", "bets.yml")
    if (!file.exists(bets_yml)) next

    cfg <- read_yaml(read_file(bets_yml))

    # Resolve odds path
    odds_path_base <- cfg$odds$lengjan_odds_path
    if (is.null(odds_path_base)) next

    odds_csv <- file.path(
      league_dir,
      odds_path_base,
      "odds_1x2.csv"
    )

    # Normalise (handles ../ in relative paths)
    odds_csv <- normalizePath(odds_csv, mustWork = FALSE)
    if (!file.exists(odds_csv)) next

    # Read odds and check for upcoming matches
    odds <- tryCatch(
      utils::read.csv(odds_csv, stringsAsFactors = FALSE),
      error = function(e) NULL
    )
    if (is.null(odds) || nrow(odds) == 0) next
    odds$date <- as.Date(odds$date)

    upcoming <- odds[
      odds$date >= today & odds$date <= today + lookahead_days,
    ]
    n_upcoming <- length(unique(paste(upcoming$home, upcoming$away, upcoming$date)))
    if (n_upcoming == 0) next

    # Check each sex
    max_age <- cfg$predictions$max_age_hours %||% 48
    sexes <- league$sex

    for (sex in sexes) {
      # Resolve fit path based on pipeline type
      if (league$pipeline == "handball_other") {
        fit_path <- file.path(
          sports_dir, "handball", "other", "results",
          league$country, sex, "fit.rds"
        )
      } else {
        fit_path <- file.path(league_dir, "results", sex, "fit.rds")
      }

      fit_info <- file.info(fit_path)
      if (is.na(fit_info$mtime)) {
        reason <- sprintf("no fit.rds, %d match(es) w/ odds", n_upcoming)
      } else {
        age_hours <- as.numeric(
          difftime(Sys.time(), fit_info$mtime, units = "hours")
        )
        if (age_hours <= max_age) next
        reason <- sprintf(
          "fit %dh old, %d match(es) w/ odds",
          round(age_hours), n_upcoming
        )
      }

      stale[[length(stale) + 1]] <- list(
        key = key,
        sex = sex,
        reason = reason
      )
    }
  }

  stale
}
