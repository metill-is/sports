#' Decide whether a (league, sex) pair needs refitting.
#'
#' Returns `TRUE` when there is at least one completed match with
#' `match_date` strictly later than the most recent `fit_date` partition
#' under EITHER `data/beliefs/archive/` OR `data/beliefs/extracts/`
#' (whichever is fresher). The two-source check matters because
#' `fit_league()` skips the legacy `beliefs_archive` write for football
#' iceland (Phase 3b, 2026-05-04) — extracts/ is the canonical per-fit
#' accretive store for that league. Returns `TRUE` when no fit exists
#' yet OR when `beliefs/latest/` is missing/empty for the cell (so a
#' wiped canonical partition forces a refit even if archive still
#' carries yesterday's fit — defensive complement to the atomic
#' stage-then-rename in `write_table()`, audit 2026-05-15 §H + I2).
#' Returns `FALSE` when no results exist (cannot fit on empty data) or
#' when no game has been played since the last fit.
#'
#' Also returns `TRUE` when fixtures fall inside the next `horizon_days` and
#' the newest fit predicted none of them -- the season-rollover case, where
#' nothing has been played since the last fit (spec 2026-09-16 §8). Only
#' fixtures a fit could predict count: both teams must have a completed
#' result, because `prepare_data()` drops any other fixture.
#'
#' @param static Per-league static slice with `$sport` and `$country`.
#' @param sex `"male"` or `"female"`.
#' @param root Filesystem root (defaults to `here::here("data")`).
#' @param today Reference date for the fixture horizon. Default `Sys.Date()`.
#' @param horizon_days Horizon in days; keep equal to `fit_league()`'s
#'   `schedule_horizon_days` (14).
#' @return Logical scalar.
#' @export
needs_refit <- function(static, sex, root = here::here("data"),
                        today = Sys.Date(), horizon_days = 14L) {
  results <- read_table(
    "results",
    root = root,
    filter = list(sport = static$sport, country = static$country, sex = sex)
  )
  if (nrow(results) == 0L) {
    return(FALSE)
  }

  completed <- dplyr::filter(
    results, !is.na(.data$home_score), !is.na(.data$away_score)
  )
  if (nrow(completed) == 0L) {
    return(FALSE)
  }
  latest_match <- max(completed$match_date)

  # If beliefs/latest/ is missing or empty for this cell, force a refit
  # regardless of what archive says. This catches the failure mode where a
  # past write got wiped (manual rm, disk full, pre-atomic-fix Arrow crash)
  # but archive still has a fit_date partition — without this guard
  # decide_league() would silently emit empty recommendations indefinitely.
  latest_dir <- fs::path(
    root, "beliefs", "latest",
    paste0("sport=", static$sport),
    paste0("country=", static$country),
    paste0("sex=", sex)
  )
  if (!fs::dir_exists(latest_dir) ||
    length(fs::dir_ls(latest_dir, glob = "*.parquet")) == 0L) {
    return(TRUE)
  }

  # Collect fit_date partitions from both legacy archive/ and the newer
  # extracts/ tree. Either may be empty (archive/ is empty for football
  # iceland post-Phase-3b; extracts/ is empty for any league × sex that
  # hasn't been fit since its extraction layer shipped). The freshest
  # partition across both stores wins.
  collect_fit_dates <- function(subdir) {
    cell_dir <- fs::path(
      root, "beliefs", subdir,
      paste0("sport=", static$sport),
      paste0("country=", static$country),
      paste0("sex=", sex)
    )
    if (!fs::dir_exists(cell_dir)) {
      return(as.Date(character()))
    }
    fit_dirs <- fs::dir_ls(cell_dir, type = "directory")
    if (length(fit_dirs) == 0L) {
      return(as.Date(character()))
    }
    as.Date(stringr::str_remove(fs::path_file(fit_dirs), "^fit_date="))
  }

  fit_dates <- c(collect_fit_dates("archive"), collect_fit_dates("extracts"))
  if (length(fit_dates) == 0L) {
    return(TRUE)
  }
  last_fit <- max(fit_dates)

  latest_match > last_fit ||
    .horizon_unpredicted(
      static, sex, root,
      today = as.Date(today), horizon_days = horizon_days,
      completed = completed
    )
}

# TRUE when the refit horizon holds fixtures and the newest fit predicted none
# of them.
#
# "None of them", not "any one of them": a 14-day window gains a fixture on
# most in-season days, so an any-uncovered rule would refit every league daily
# whether or not a game had been played. Zero overlap still catches every gap
# the rule exists for -- a season rollover and a long break -- because a fit
# made before either predicted nothing inside today's window.
#
# Only a fixture a fit COULD predict counts. prepare_data() drops any fixture
# with a team that has no completed result, so a window holding only those
# (a newcomer's first games -- 2026 football had 02-24 to 02-27 holding only
# Ulfarnir v Hamar) would start a fit every day, and every one would abort on
# an empty prediction set.
#
# An unreadable or missing prediction set is "unknown", and unknown does not
# start a fit. A cell with no fit at all never gets here: needs_refit() has
# already returned TRUE.
.horizon_unpredicted <- function(static, sex, root, today, horizon_days,
                                 completed) {
  sched <- read_table(
    "schedules",
    root = root,
    filter = list(sport = static$sport, country = static$country, sex = sex)
  )
  if (nrow(sched) == 0L) {
    return(FALSE)
  }
  known <- unique(c(completed$home_team, completed$away_team))
  in_window <- !is.na(sched$match_date) &
    sched$match_date > today &
    sched$match_date <= today + as.integer(horizon_days) &
    sched$home_team %in% known & sched$away_team %in% known
  if (!any(in_window)) {
    return(FALSE)
  }
  predicted <- .latest_predicted_fixtures(static, sex, root)
  if (is.null(predicted)) {
    return(FALSE)
  }
  key <- function(d) {
    paste(d$home_team, d$away_team, format(as.Date(d$match_date)), sep = "|")
  }
  !any(key(sched[in_window, , drop = FALSE]) %in% key(predicted))
}

# The fixtures the newest fit predicted, read from BOTH stores at that fit
# date: extracts/ (predicted_matches.parquet) and archive/ (long-form
# beliefs). One fit writes both, and they differ: the extract keeps only the
# publish divisions' fixtures, the archive keeps them all -- handball's
# play-off (PO) games included. Reading the extract alone on a tie left every
# PO fixture uncovered, so handball refit daily through its play-offs.
#
# Each file is read on its own. An unreadable one is skipped, so one corrupt
# archive shard no longer hides the readable predictions beside it. NULL when
# no file at that date could be read.
.latest_predicted_fixtures <- function(static, sex, root) {
  cell <- c(
    paste0("sport=", static$sport),
    paste0("country=", static$country),
    paste0("sex=", sex)
  )
  parts <- dplyr::bind_rows(lapply(c("extracts", "archive"), function(store) {
    dir <- do.call(fs::path, as.list(c(root, "beliefs", store, cell)))
    if (!fs::dir_exists(dir)) {
      return(NULL)
    }
    fits <- fs::dir_ls(
      dir,
      type = "directory", regexp = "fit_date=[0-9]{4}-[0-9]{2}-[0-9]{2}$"
    )
    if (length(fits) == 0L) {
      return(NULL)
    }
    tibble::tibble(
      store = store,
      path = as.character(fits),
      fit_date = as.Date(sub("^fit_date=", "", fs::path_file(fits)))
    )
  }))
  if (nrow(parts) == 0L) {
    return(NULL)
  }
  newest <- parts[parts$fit_date == max(parts$fit_date), , drop = FALSE]
  files <- unlist(lapply(seq_len(nrow(newest)), function(i) {
    if (identical(newest$store[[i]], "extracts")) {
      file.path(newest$path[[i]], "predicted_matches.parquet")
    } else {
      as.character(fs::dir_ls(newest$path[[i]], glob = "*.parquet"))
    }
  }))
  files <- files[file.exists(files)]
  tables <- lapply(files, function(f) {
    tryCatch(
      tibble::as_tibble(arrow::read_parquet(
        f,
        col_select = c("home_team", "away_team", "match_date")
      )),
      error = function(e) NULL
    )
  })
  tables <- Filter(Negate(is.null), tables)
  if (length(tables) == 0L) {
    return(NULL)
  }
  dplyr::bind_rows(tables)
}

#' Are there any matches scheduled in the next `days` days?
#'
#' Reads `data/facts/schedules/` for the (sport, country, sex) partition.
#' Returns `FALSE` when the schedule directory is missing or empty.
#'
#' @param static Per-league static slice with `$sport` and `$country`.
#' @param sex `"male"` or `"female"`.
#' @param root Filesystem root.
#' @param days Horizon in days (default 14).
#' @return Logical scalar.
#' @export
has_upcoming_games <- function(static, sex,
                               root = here::here("data"),
                               days = 14L) {
  sched_root <- fs::path(root, "facts", "schedules")
  if (!fs::dir_exists(sched_root)) {
    return(FALSE)
  }

  schedules <- read_table(
    "schedules",
    root = root,
    filter = list(sport = static$sport, country = static$country, sex = sex)
  )
  if (nrow(schedules) == 0L) {
    return(FALSE)
  }

  today <- Sys.Date()
  any(
    schedules$match_date >= today &
      schedules$match_date <= today + as.integer(days)
  )
}

#' Decide whether `scripts/03_fit.R` should skip a (league, sex) pair.
#'
#' Centralises the per-target skip decision so the entry script stays a thin
#' loop and the branching is unit-testable. Returns `NULL` to fit, or a short
#' human-readable reason to skip.
#'
#' Rules, in order:
#' \enumerate{
#'   \item Not forced and [needs_refit()] is `FALSE` → skip (`"no new games"`):
#'     a refit would only reproduce the existing posterior.
#'   \item [has_upcoming_games()] is `FALSE` (off-season / paused) AND no single
#'     league was explicitly named → skip (`"no upcoming games"`), **even under
#'     `--force`**. Forcing a refit of a paused, unbettable league regenerates
#'     beliefs nobody consumes and risks marginal MCMC gate failures — the
#'     off-season basketball R-hat/ESS breaches that spuriously fail a manual
#'     `--force` (all-leagues) fit run.
#' }
#'
#' An explicit `--league` (`league_named = TRUE`) overrides the paused skip, so
#' `--force --league basketball_iceland` still refits the off-season league
#' (e.g. to regenerate one league's beliefs after a model change). It does not
#' bypass the "no new games" guard unless `--force` is also given.
#'
#' @param static Per-league static slice with `$sport` and `$country`.
#' @param sex `"male"` or `"female"`.
#' @param force Logical; the `--force` flag.
#' @param league_named Logical; was a single `--league` explicitly requested?
#' @param root Filesystem root (defaults to `here::here("data")`).
#' @return `NULL` to fit, otherwise a character scalar reason to skip.
#' @export
fit_skip_reason <- function(static, sex, force, league_named,
                            root = here::here("data")) {
  if (!force && !needs_refit(static, sex, root = root)) {
    return("no new games since last fit")
  }
  if (!has_upcoming_games(static, sex, root = root) && !league_named) {
    return("no upcoming games (paused); pass --league to force a refit")
  }
  NULL
}
