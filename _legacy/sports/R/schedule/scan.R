#' Schedule scanner — finds upcoming games across all sports
#'
#' Reads combined schedule.csv files from each sport directory,
#' filters to games within a lookahead window, and identifies
#' which lengjan-odds competitions have upcoming matches.
#'
#' @usage
#' box::use(R/schedule/scan[scan_schedules])
#' result <- scan_schedules(here::here(), lookahead_days = 7)

# ── Registry ──────────────────────────────────────────────────────────────────
# Each entry maps a sport to its combined schedule file.
# All combined schedules use ISO dates (YYYY-MM-DD).
# lengjan_key = NULL means no Lengjan coverage for that sport.

#' @export
registry <- list(
  # Basketball
  basketball_iceland_m = list(
    path = "basketball/iceland/data/male/schedule.csv",
    date_col = "dags",
    sport_label = "Basketball Iceland (M)",
    lengjan_key = NULL
  ),
  basketball_iceland_f = list(
    path = "basketball/iceland/data/female/schedule.csv",
    date_col = "dags",
    sport_label = "Basketball Iceland (F)",
    lengjan_key = "basketball_iceland"
  ),

  # Handball Iceland
  handball_iceland_m = list(
    path = "handball/iceland/data/male/schedule.csv",
    date_col = "dagsetning",
    sport_label = "Handball Iceland (M)",
    lengjan_key = "handball_iceland"
  ),
  handball_iceland_f = list(
    path = "handball/iceland/data/female/schedule.csv",
    date_col = "dagsetning",
    sport_label = "Handball Iceland (F)",
    lengjan_key = NULL
  ),

  # Football
  football_iceland_m = list(
    path = "football/iceland/data/male/schedule.csv",
    date_col = "dags",
    sport_label = "Football Iceland (M)",
    lengjan_key = "football_iceland"
  ),
  football_iceland_f = list(
    path = "football/iceland/data/female/schedule.csv",
    date_col = "dags",
    sport_label = "Football Iceland (F)",
    lengjan_key = NULL
  ),
  football_england = list(
    path = "football/england/data/male/schedule.csv",
    date_col = "date",
    sport_label = "Football England",
    lengjan_key = "football_england"
  ),
  football_italy = list(
    path = "football/italy/data/male/schedule.csv",
    date_col = "date",
    sport_label = "Football Italy",
    lengjan_key = "football_italy"
  ),
  football_spain = list(
    path = "football/spain/data/male/schedule.csv",
    date_col = "date",
    sport_label = "Football Spain",
    lengjan_key = "football_spain"
  ),

  # Handball European leagues
  handball_austria = list(
    path = "handball/austria/data/male/schedule.csv",
    date_col = "date",
    sport_label = "Handball Austria",
    lengjan_key = NULL
  ),
  handball_czech_republic = list(
    path = "handball/czech-republic/data/male/schedule.csv",
    date_col = "date",
    sport_label = "Handball Czech Republic",
    lengjan_key = NULL
  ),
  handball_denmark = list(
    path = "handball/denmark/data/male/schedule.csv",
    date_col = "date",
    sport_label = "Handball Denmark",
    lengjan_key = "handball_denmark"
  ),
  handball_finland = list(
    path = "handball/finland/data/male/schedule.csv",
    date_col = "date",
    sport_label = "Handball Finland",
    lengjan_key = NULL
  ),
  handball_france = list(
    path = "handball/france/data/male/schedule.csv",
    date_col = "date",
    sport_label = "Handball France",
    lengjan_key = "handball_france"
  ),
  handball_germany = list(
    path = "handball/germany/data/male/schedule.csv",
    date_col = "date",
    sport_label = "Handball Germany",
    lengjan_key = "handball_germany"
  ),
  handball_hungary = list(
    path = "handball/hungary/data/male/schedule.csv",
    date_col = "date",
    sport_label = "Handball Hungary",
    lengjan_key = NULL
  ),
  handball_norway = list(
    path = "handball/norway/data/male/schedule.csv",
    date_col = "date",
    sport_label = "Handball Norway",
    lengjan_key = "handball_norway"
  ),
  handball_poland = list(
    path = "handball/poland/data/male/schedule.csv",
    date_col = "date",
    sport_label = "Handball Poland",
    lengjan_key = NULL
  ),
  handball_portugal = list(
    path = "handball/portugal/data/male/schedule.csv",
    date_col = "date",
    sport_label = "Handball Portugal",
    lengjan_key = NULL
  ),
  handball_spain = list(
    path = "handball/spain/data/male/schedule.csv",
    date_col = "date",
    sport_label = "Handball Spain",
    lengjan_key = "handball_spain"
  ),
  handball_sweden = list(
    path = "handball/sweden/data/male/schedule.csv",
    date_col = "date",
    sport_label = "Handball Sweden",
    lengjan_key = "handball_sweden"
  )
)


#' Scan schedule files and find competitions with upcoming games
#'
#' @param sports_root Absolute path to Sports/ directory
#' @param lookahead_days Number of days to look ahead (default 7)
#' @return List with:
#'   $upcoming: tibble of all upcoming matches with sport metadata
#'   $active_lengjan: character vector of lengjan_keys with upcoming games
#'   $summary: tibble with one row per registry entry (sport, n_upcoming, next_game)
#' @export
scan_schedules <- function(sports_root, lookahead_days = 7) {
  today <- Sys.Date()
  horizon <- today + lookahead_days

  all_upcoming <- list()
  summary_rows <- list()
  active_lengjan <- character(0)

  for (key in names(registry)) {
    entry <- registry[[key]]
    path <- file.path(sports_root, entry$path)

    if (!file.exists(path)) {
      summary_rows[[key]] <- data.frame(
        sport = entry$sport_label,
        n_upcoming = 0L,
        next_game = as.Date(NA),
        status = "no file",
        stringsAsFactors = FALSE
      )
      next
    }

    d <- tryCatch(
      readr::read_csv(path, col_types = readr::cols(.default = "c"), show_col_types = FALSE),
      error = function(e) NULL
    )

    if (is.null(d) || nrow(d) == 0) {
      summary_rows[[key]] <- data.frame(
        sport = entry$sport_label,
        n_upcoming = 0L,
        next_game = as.Date(NA),
        status = "empty",
        stringsAsFactors = FALSE
      )
      next
    }

    date_col <- entry$date_col
    if (!date_col %in% names(d)) {
      summary_rows[[key]] <- data.frame(
        sport = entry$sport_label,
        n_upcoming = 0L,
        next_game = as.Date(NA),
        status = paste0("missing column '", date_col, "'"),
        stringsAsFactors = FALSE
      )
      next
    }

    dates <- tryCatch(as.Date(d[[date_col]]), error = function(e) rep(NA_real_, nrow(d)))
    in_window <- !is.na(dates) & dates >= today & dates <= horizon

    n_upcoming <- sum(in_window)
    next_game <- if (any(!is.na(dates) & dates >= today)) min(dates[!is.na(dates) & dates >= today]) else NA

    if (n_upcoming > 0) {
      upcoming_rows <- d[in_window, ]
      upcoming_rows$`.sport` <- entry$sport_label
      upcoming_rows$`.date` <- dates[in_window]
      all_upcoming[[key]] <- upcoming_rows

      if (!is.null(entry$lengjan_key)) {
        active_lengjan <- unique(c(active_lengjan, entry$lengjan_key))
      }
    }

    summary_rows[[key]] <- data.frame(
      sport = entry$sport_label,
      n_upcoming = n_upcoming,
      next_game = as.Date(next_game, origin = "1970-01-01"),
      status = if (n_upcoming > 0) "active" else "no upcoming",
      stringsAsFactors = FALSE
    )
  }

  summary_df <- dplyr::bind_rows(summary_rows, .id = "key")

  list(
    upcoming = if (length(all_upcoming) > 0) dplyr::bind_rows(all_upcoming) else data.frame(),
    active_lengjan = active_lengjan,
    summary = summary_df
  )
}
