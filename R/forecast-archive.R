#' @include storage.R
NULL

# The forecast archive: every pre-match forecast the site published, kept for
# good under data/archive/forecasts/.
#
# WHY: a past fit's forecasts otherwise live only in its extract partition,
# and prune_extracts() deletes old partitions on every fit (14 days, plus three
# per cell and one per season). Once a partition goes, nothing records what
# the site told people before a match, and a model can no longer be scored on
# what it published. The archive is written by publish_one() after a cell has
# been validated and swapped into data/publish/, from the next_games.json it
# just wrote, so a row is the forecast exactly as published.
#
# Append-only, three ways:
#   - a forecast already stored for the same fit, file and match is never
#     rewritten: republishing it is a no-op, and a DIFFERENT forecast under the
#     same key (a same-day refit overwrites its fit_date's extract partition)
#     is refused with a warning, so the first published version stays;
#   - write_table() itself refuses any write to the table that would change or
#     drop a stored row (append_only_tables()), whoever calls it;
#   - nothing prunes it (tests/testthat/test-forecast-archive.R pins that).
#
# Only CI archives (scripts/05_publish.R, publish_archives_forecasts()): the
# site serves what CI commits, so a local publish is not a publication.

# One row per (fit, published file, match).
.FORECAST_ARCHIVE_KEY <- c(
  "sport", "country", "sex", "fit_date", "published_to",
  "match_date", "home_team", "away_team"
)

# What a republish of the same key must reproduce exactly. The first-seen
# metadata (kickoff_time, published_at, code_commit) is not compared: it
# describes the first publication, not the forecast.
.FORECAST_ARCHIVE_FORECAST <- c(
  "division", "p_home_win", "p_draw", "p_away_win", "xg_home", "xg_away",
  "engine", "stan_model", "model_version"
)

#' Does this publish run archive its forecasts?
#'
#' On GitHub Actions, or when asked with `--archive`. A local publish is not a
#' publication (the site serves what CI commits to main), and a local write
#' would put a binary file CI also rewrites into the local tree, which the
#' stash/pull/pop sync can only resolve by keeping one side's rows.
#'
#' @param flag Was `--archive` given?
#' @param github_actions Value of the `GITHUB_ACTIONS` environment variable.
#' @return `TRUE` or `FALSE`.
#' @noRd
publish_archives_forecasts <- function(flag = FALSE,
                                       github_actions = Sys.getenv("GITHUB_ACTIONS")) {
  isTRUE(flag) || identical(github_actions, "true")
}

#' Archive rows for the forecasts in a set of published cells.
#'
#' Reads each cell's `next_games.json` under `publish_root` and keeps the
#' forecasts that were published before kick-off: a row whose schedule
#' kick-off (UTC, Iceland's time all year) is at or before `published_at` is
#' dropped, as is one for a match dated before the publication date. A row
#' with no known kick-off is kept.
#'
#' @param publish_root The publish tree (`data/publish`).
#' @param sport,country,sex The cell family.
#' @param cells Cell folder names under `<sport>/<country>/`.
#' @param fit_date `Date` of the fit the cells were published from.
#' @param fit_meta The fit partition's `fit_meta` (engine, stan_model,
#'   model_version); 0 rows for a partition written before it existed.
#' @param stan_model_default Stan program recorded when `fit_meta` has no row.
#' @param schedules Schedule rows with `match_date`, `home_team`, `away_team`,
#'   `kickoff_time`.
#' @param published_at Publication time.
#' @param code_commit Commit that published, or `NA`.
#' @return Tibble in the `forecast_archive` schema.
#' @noRd
forecast_archive_rows <- function(publish_root, sport, country, sex, cells,
                                  fit_date, fit_meta, schedules,
                                  published_at, code_commit = NA_character_,
                                  stan_model_default = NA_character_) {
  published_at <- .whole_seconds(published_at)
  meta <- .forecast_archive_provenance(fit_meta, stan_model_default)

  rows <- lapply(cells, function(cell) {
    rel <- file.path(sport, country, cell, "next_games.json")
    f <- file.path(publish_root, rel)
    if (!file.exists(f)) {
      return(NULL)
    }
    m <- jsonlite::fromJSON(f, simplifyVector = TRUE)$matches
    if (!is.data.frame(m) || nrow(m) == 0L) {
      return(NULL)
    }
    num <- function(x) if (is.null(x)) rep(NA_real_, nrow(m)) else as.numeric(x)
    tibble::tibble(
      sport = sport, country = country, sex = sex,
      fit_date = as.Date(fit_date), published_to = rel,
      division = as.character(m$division),
      match_date = as.Date(m$date),
      home_team = as.character(m$home), away_team = as.character(m$away),
      p_home_win = num(m$p_home_win), p_draw = num(m$p_draw),
      p_away_win = num(m$p_away_win),
      xg_home = num(m$mean_home_goals), xg_away = num(m$mean_away_goals)
    )
  })
  rows <- dplyr::bind_rows(rows)
  if (nrow(rows) == 0L) {
    return(.forecast_archive_empty())
  }

  kickoffs <- if (is.null(schedules) || nrow(schedules) == 0L) {
    tibble::tibble(
      match_date = as.Date(character()), home_team = character(),
      away_team = character(), kickoff_time = character()
    )
  } else {
    schedules |>
      dplyr::transmute(
        match_date = as.Date(.data$match_date),
        home_team = .data$home_team, away_team = .data$away_team,
        kickoff_time = as.character(.data$kickoff_time)
      ) |>
      dplyr::distinct(.data$match_date, .data$home_team, .data$away_team, .keep_all = TRUE)
  }
  rows <- dplyr::left_join(rows, kickoffs, by = c("match_date", "home_team", "away_team"))

  kickoff_at <- as.POSIXct(
    paste(format(rows$match_date, "%Y-%m-%d"), rows$kickoff_time),
    format = "%Y-%m-%d %H:%M", tz = "UTC"
  )
  pre_match <- rows$match_date >= as.Date(published_at, tz = "UTC") &
    (is.na(kickoff_at) | kickoff_at > published_at)
  rows <- rows[pre_match, , drop = FALSE]

  rows$engine <- meta$engine
  rows$stan_model <- meta$stan_model
  rows$model_version <- meta$model_version
  rows$published_at <- rep(published_at, nrow(rows))
  rows$code_commit <- rep(as.character(code_commit), nrow(rows))
  rows[schemas()$forecast_archive$names]
}

#' Add published forecasts to the archive.
#'
#' New keys (fit, published file, match) are appended. A key already stored
#' with the same forecast is left alone -- the common case, since every
#' decide-publish run republishes the newest fit. A key already stored with a
#' different forecast is refused: it is not written, and a warning names it.
#' Stored rows are never rewritten; partitions that gain no row are not
#' touched at all.
#'
#' @param rows Tibble in the `forecast_archive` schema.
#' @param root Storage root.
#' @return Invisibly, `list(appended = <rows written>, unchanged = <count>,
#'   refused = <rows refused>)`.
#' @export
archive_forecasts <- function(rows, root = here::here("data")) {
  if (nrow(rows) == 0L) {
    return(invisible(list(appended = rows, unchanged = 0L, refused = rows)))
  }
  rows <- tibble::as_tibble(rows)
  rows$published_at <- .whole_seconds(rows$published_at)
  rows$fit_date <- as.Date(rows$fit_date)
  rows$match_date <- as.Date(rows$match_date)
  rows <- rows[!duplicated(.row_signature(rows[.FORECAST_ARCHIVE_KEY])), , drop = FALSE]

  partitions <- table_partitions()[["forecast_archive"]]
  dest <- do.call(fs::path, c(list(root), table_subdir("forecast_archive")))
  keys <- dplyr::distinct(rows[, partitions, drop = FALSE])
  stored <- dplyr::bind_rows(lapply(seq_len(nrow(keys)), function(i) {
    .read_partition_rows(dest, partitions, keys[i, , drop = FALSE])
  }))

  if (nrow(stored) == 0L) {
    is_new <- rep(TRUE, nrow(rows))
    same <- logical()
  } else {
    stored_key <- .row_signature(stored[.FORECAST_ARCHIVE_KEY])
    at <- match(.row_signature(rows[.FORECAST_ARCHIVE_KEY]), stored_key)
    is_new <- is.na(at)
    same <- .row_signature(rows[!is_new, .FORECAST_ARCHIVE_FORECAST]) ==
      .row_signature(stored[at[!is_new], .FORECAST_ARCHIVE_FORECAST])
  }
  appended <- rows[is_new, , drop = FALSE]
  refused <- rows[!is_new, , drop = FALSE][!same, , drop = FALSE]

  if (nrow(refused) > 0L) {
    what <- sprintf(
      "%s v %s on %s (fit %s, %s)", refused$home_team, refused$away_team,
      format(refused$match_date), format(refused$fit_date), refused$published_to
    )
    cli::cli_warn(c(
      "Forecast archive: {nrow(refused)} forecast{?s} refused, kept as first published: {.val {what}}.",
      "i" = "A forecast differs from the one already archived for the same fit, file and match, usually because a same-day refit overwrote that fit date's extract partition. Archived rows are never rewritten."
    ))
  }

  if (nrow(appended) > 0L) {
    touched <- dplyr::distinct(appended[, partitions, drop = FALSE])
    keep <- if (nrow(stored) == 0L) stored else dplyr::semi_join(stored, touched, by = partitions)
    write_table(dplyr::bind_rows(keep, appended), "forecast_archive", root = root)
  }

  invisible(list(appended = appended, unchanged = sum(same), refused = refused))
}

#' Read the forecast archive.
#'
#' @param root Storage root.
#' @param filter Optional named list of column = value filters.
#' @return Tibble in the `forecast_archive` schema; 0 rows when nothing has
#'   been archived.
#' @export
read_forecast_archive <- function(root = here::here("data"), filter = list()) {
  out <- read_table("forecast_archive", root = root, filter = filter)
  if (nrow(out) == 0L) {
    return(.forecast_archive_empty())
  }
  out$fit_date <- as.Date(as.character(out$fit_date))
  out$published_at <- as.POSIXct(out$published_at, tz = "UTC")
  out[schemas()$forecast_archive$names]
}

.forecast_archive_empty <- function() {
  tibble::tibble(
    sport = character(), country = character(), sex = character(),
    fit_date = as.Date(character()), published_to = character(),
    division = character(), match_date = as.Date(character()),
    home_team = character(), away_team = character(),
    kickoff_time = character(), p_home_win = numeric(), p_draw = numeric(),
    p_away_win = numeric(), xg_home = numeric(), xg_away = numeric(),
    engine = character(), stan_model = character(),
    model_version = character(),
    published_at = as.POSIXct(character(), tz = "UTC"),
    code_commit = character()
  )
}

# Engine, Stan program and model version of the fit, from its fit_meta row.
# A partition written before fit_meta existed has none; it was a Stan fit of
# the league's configured program.
.forecast_archive_provenance <- function(fit_meta, stan_model_default) {
  pick <- function(col, default) {
    if (is.null(fit_meta) || nrow(fit_meta) == 0L || !col %in% names(fit_meta)) {
      return(default)
    }
    as.character(fit_meta[[col]][[1L]])
  }
  list(
    engine = pick("engine", "stan"),
    stan_model = pick("stan_model", as.character(stan_model_default)),
    model_version = pick("model_version", NA_character_)
  )
}

# The archive's timestamps are whole seconds (timestamp[s] in the schema).
.whole_seconds <- function(x) {
  as.POSIXct(floor(as.numeric(as.POSIXct(x, tz = "UTC"))), origin = "1970-01-01", tz = "UTC")
}

# The commit a CI publish ran on; NA elsewhere.
.publish_code_commit <- function() {
  sha <- Sys.getenv("GITHUB_SHA")
  if (nzchar(sha)) sha else NA_character_
}
