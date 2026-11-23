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
# One row per published forecast: (fit, published file, match). The fit is
# its date AND `fit_id`, a fingerprint of the forecasts in the extract
# partition's predicted_matches.parquet (its rows, not its bytes), because a
# same-day refit overwrites the partition of that date: its forecasts are new
# publications and are archived beside the first fit's, told apart by fit_id.
#
# Append-only, by construction:
#   - every write is a NEW partition, `batch=` below `fit_date=`; no file is
#     ever rewritten, so two CI runs that publish at once (decide-publish's
#     queued run checks out an older commit, republish runs beside it) write
#     different files and git keeps both. A row both of them archived appears
#     twice with the same forecast; read_forecast_archive() keeps the earliest;
#   - a forecast already stored for the same key is not written again, and a
#     DIFFERENT forecast under the same key (same fit, file and match: only a
#     change to the publisher's own output does that) is refused with a
#     warning, so the first published version stays;
#   - write_table() refuses any write into an existing partition of the table,
#     and any row that contradicts a stored one under the same key
#     (append_only_tables()), whoever calls it;
#   - nothing prunes it (tests/testthat/test-forecast-archive.R pins that).
#
# Pre-match: a forecast is archived only when it was published before
# kick-off. Kick-offs come from the schedule (KSI posts them) and else from
# the newest odds scrape (Lengjan lists them for the leagues it prices, a day
# or two ahead). With no known kick-off a forecast for a later day is archived
# and one for the publication day is not: it may have been published after
# the game began. `kickoff_time` is what was known when the row was first
# archived, so for the 2DT sports it is mostly NA except close to the match;
# join today's schedule or odds for a kick-off when scoring.
#
# Only CI archives (scripts/05_publish.R, publish_archives_forecasts()): the
# site serves what CI commits, so a local publish is not a publication.

#' Does this publish run archive its forecasts?
#'
#' On GitHub Actions, or when asked with `--archive`. A local publish is not a
#' publication (the site serves what CI commits to main), and a local write
#' would put files CI also writes into the local tree.
#'
#' @param flag Was `--archive` given?
#' @param github_actions Value of the `GITHUB_ACTIONS` environment variable.
#' @return `TRUE` or `FALSE`.
#' @noRd
publish_archives_forecasts <- function(flag = FALSE,
                                       github_actions = Sys.getenv("GITHUB_ACTIONS")) {
  isTRUE(flag) || identical(github_actions, "true")
}

#' A fingerprint of the fit a publish read: an md5 of the forecasts in its
#' extract partition's `predicted_matches.parquet`, the file next_games.json
#' is made from. Of the rows, in a fixed order, not of the file's bytes: a
#' partition rewritten with the same forecasts (a cup go-live patch, another
#' arrow version) is the same fit. NA when the file is missing.
#' @noRd
forecast_archive_fit_id <- function(fit_dir) {
  f <- file.path(fit_dir, "predicted_matches.parquet")
  if (!file.exists(f)) {
    return(NA_character_)
  }
  d <- as.data.frame(arrow::read_parquet(f))
  d <- d[sort(names(d))]
  for (nm in names(d)) {
    if (is.list(d[[nm]])) {
      d[[nm]] <- vapply(d[[nm]], function(x) as.character(jsonlite::toJSON(x, digits = NA)), character(1))
    }
  }
  lines <- sort(.row_signature(d))
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c(paste(names(d), collapse = "\x1f"), lines), tmp, useBytes = TRUE)
  unname(tools::md5sum(tmp))
}

#' Kick-off instants (UTC) for a cell family's fixtures.
#'
#' The schedule's "HH:MM" first (KSI posts it; Iceland keeps UTC all year),
#' else the newest odds scrape's `kickoff_at`, with Lengjan's names mapped to
#' the federation's through the league's `team_names`. A read that fails gives
#' no kick-offs, not an error: the archive then keeps only forecasts for later
#' days.
#'
#' @param league Full league config (the odds names need `lengjan`).
#' @param from Earliest match date of interest.
#' @return Tibble match_date, home_team, away_team, kickoff_at.
#' @noRd
forecast_archive_kickoffs <- function(league, sex, root, from) {
  none <- tibble::tibble(
    match_date = as.Date(character()), home_team = character(),
    away_team = character(), kickoff_at = as.POSIXct(character(), tz = "UTC")
  )
  sched <- tryCatch(
    read_table("schedules", root = root, filter = list(sport = league$sport, country = league$country, sex = sex)),
    error = function(e) NULL
  )
  from_schedule <- if (is.null(sched) || nrow(sched) == 0L || !"kickoff_time" %in% names(sched)) {
    none
  } else {
    sched <- sched[!is.na(sched$kickoff_time) & as.Date(sched$match_date) >= from, , drop = FALSE]
    tibble::tibble(
      match_date = as.Date(sched$match_date), home_team = sched$home_team, away_team = sched$away_team,
      kickoff_at = as.POSIXct(paste(format(as.Date(sched$match_date)), sched$kickoff_time), format = "%Y-%m-%d %H:%M", tz = "UTC")
    )
  }
  odds <- tryCatch(
    read_table("odds", root = root, filter = list(sport = league$sport, country = league$country)),
    error = function(e) NULL
  )
  from_odds <- none
  if (!is.null(odds) && nrow(odds) > 0L && all(c("sex", "kickoff_at") %in% names(odds))) {
    odds <- odds[!is.na(odds$sex) & odds$sex == sex & !is.na(odds$kickoff_at) & as.Date(odds$match_date) >= from, , drop = FALSE]
    if (nrow(odds) > 0L && !is.null(league$lengjan$team_names[[sex]])) {
      odds <- odds[order(odds$scraped_at, decreasing = TRUE), , drop = FALSE]
      odds <- odds[!duplicated(odds[c("match_date", "home_team", "away_team")]), c("match_date", "home_team", "away_team", "kickoff_at")]
      # A team_names map the normaliser refuses (two canonicals on one
      # rendering) fails decide loudly; here it only costs the odds' kick-offs.
      odds <- tryCatch(suppressMessages(normalise_lengjan_team_names(odds, league, sex)), error = function(e) {
        cli::cli_alert_warning("Forecast archive: odds kick-offs unused for {league$sport}/{sex}: {conditionMessage(e)}")
        NULL
      })
      if (!is.null(odds)) {
        from_odds <- tibble::tibble(
          match_date = as.Date(odds$match_date), home_team = odds$home_team, away_team = odds$away_team,
          kickoff_at = as.POSIXct(odds$kickoff_at, tz = "UTC")
        )
      }
    }
  }
  out <- dplyr::bind_rows(from_schedule, from_odds)
  out <- out[!is.na(out$kickoff_at), , drop = FALSE]
  out[!duplicated(out[c("match_date", "home_team", "away_team")]), , drop = FALSE]
}

#' Archive rows for the forecasts in a set of published cells.
#'
#' Reads each cell's `next_games.json` under `publish_root` and keeps the
#' forecasts that were published before kick-off: one whose kick-off is at or
#' before `published_at` is dropped, as is one for a match dated before the
#' publication day, and one with no known kick-off on the publication day
#' itself.
#'
#' @param publish_root The publish tree (`data/publish`).
#' @param sport,country,sex The cell family.
#' @param cells Cell folder names under `<sport>/<country>/`.
#' @param fit_date `Date` of the fit the cells were published from.
#' @param fit_id `forecast_archive_fit_id()` of that fit's partition.
#' @param fit_meta The fit partition's `fit_meta` (engine, stan_model,
#'   model_version); 0 rows for a partition written before it existed.
#' @param kickoffs `forecast_archive_kickoffs()` output.
#' @param published_at Publication time.
#' @param code_commit Commit that published, or `NA`.
#' @param stan_model_default Stan program recorded when `fit_meta` has no row.
#' @return Tibble in the `forecast_archive` schema.
#' @noRd
forecast_archive_rows <- function(publish_root, sport, country, sex, cells,
                                  fit_date, fit_id, fit_meta, kickoffs,
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
      fit_date = as.Date(fit_date), fit_id = as.character(fit_id), published_to = rel,
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
  if (is.null(kickoffs)) {
    kickoffs <- tibble::tibble(
      match_date = as.Date(character()), home_team = character(),
      away_team = character(), kickoff_at = as.POSIXct(character(), tz = "UTC")
    )
  }
  rows <- dplyr::left_join(
    rows,
    kickoffs[!duplicated(kickoffs[c("match_date", "home_team", "away_team")]), c("match_date", "home_team", "away_team", "kickoff_at")],
    by = c("match_date", "home_team", "away_team")
  )
  publication_day <- as.Date(published_at, tz = "UTC")
  pre_match <- ifelse(
    is.na(rows$kickoff_at),
    rows$match_date > publication_day,
    rows$match_date >= publication_day & rows$kickoff_at > published_at
  )
  rows <- rows[pre_match, , drop = FALSE]

  # Not ifelse(): with no row left it returns logical(0), and a 0-row result
  # must keep the schema's types for whoever binds it to others.
  rows$kickoff_time <- rep(NA_character_, nrow(rows))
  known <- !is.na(rows$kickoff_at)
  rows$kickoff_time[known] <- format(rows$kickoff_at[known], "%H:%M", tz = "UTC")
  rows$engine <- meta$engine
  rows$stan_model <- meta$stan_model
  rows$model_version <- meta$model_version
  rows$published_at <- rep(published_at, nrow(rows))
  rows$code_commit <- rep(as.character(code_commit), nrow(rows))
  rows[schemas()$forecast_archive$names]
}

#' Add published forecasts to the archive.
#'
#' Forecasts whose key (fit, fit_id, published file, match) is not stored yet
#' are written, as one new `batch=` partition per fit date. A key already
#' stored with the same forecast is left alone -- the common case, since every
#' decide-publish run republishes the newest fit. A key already stored with a
#' different forecast is refused: it is not written, and a warning names it.
#'
#' @param rows Tibble in the `forecast_archive` schema.
#' @param root Storage root.
#' @param batch Name of the new partition; by default the publication time and
#'   the CI run (or process) id.
#' @return Invisibly, `list(appended = <rows written>, unchanged = <count>,
#'   refused = <rows refused>)`.
#' @export
archive_forecasts <- function(rows, root = here::here("data"), batch = NULL) {
  spec <- append_only_tables()$forecast_archive
  if (nrow(rows) == 0L) {
    return(invisible(list(appended = rows, unchanged = 0L, refused = rows)))
  }
  rows <- tibble::as_tibble(rows)
  rows$published_at <- .whole_seconds(rows$published_at)
  rows$fit_date <- as.Date(rows$fit_date)
  rows$match_date <- as.Date(rows$match_date)
  rows <- rows[!duplicated(.row_signature(rows[spec$key])), , drop = FALSE]

  partitions <- table_partitions()[["forecast_archive"]]
  parent <- setdiff(partitions, "batch")
  dest <- do.call(fs::path, c(list(root), table_subdir("forecast_archive")))
  keys <- dplyr::distinct(rows[, parent, drop = FALSE])
  stored <- dplyr::bind_rows(lapply(seq_len(nrow(keys)), function(i) {
    .read_partition_rows(dest, partitions, keys[i, , drop = FALSE])
  }))
  if (nrow(stored) > 0L) {
    stored <- fill_optional_columns(stored, "forecast_archive")
  }

  if (nrow(stored) == 0L) {
    is_new <- rep(TRUE, nrow(rows))
    same <- logical()
  } else {
    stored_key <- .row_signature(stored[spec$key])
    at <- match(.row_signature(rows[spec$key]), stored_key)
    is_new <- is.na(at)
    same <- .row_signature(rows[!is_new, spec$compare]) ==
      .row_signature(stored[at[!is_new], spec$compare])
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
      "i" = "The same fit (same extract partition) now publishes different numbers for these matches, so the publisher's output changed. Archived rows are never rewritten."
    ))
  }

  if (nrow(appended) > 0L) {
    appended$batch <- batch %||% .forecast_archive_batch(dest, appended, parent)
    write_table(appended, "forecast_archive", root = root)
  }

  invisible(list(appended = appended, unchanged = sum(same), refused = refused))
}

#' Read the forecast archive.
#'
#' @param root Storage root.
#' @param filter Optional named list of column = value filters.
#' @param distinct With `TRUE` (default), one row per key, the earliest
#'   publication: two CI runs that could not see each other's rows may both
#'   have archived the same forecast. `FALSE` returns every stored row with its
#'   `batch`.
#' @return Tibble in the `forecast_archive` schema; 0 rows when nothing has
#'   been archived.
#' @export
read_forecast_archive <- function(root = here::here("data"), filter = list(), distinct = TRUE) {
  out <- read_table("forecast_archive", root = root, filter = filter)
  if (nrow(out) == 0L) {
    return(.forecast_archive_empty())
  }
  out$fit_date <- as.Date(as.character(out$fit_date))
  out$published_at <- as.POSIXct(out$published_at, tz = "UTC")
  out$batch <- as.character(out$batch)
  out <- out[order(out$published_at, out$batch), , drop = FALSE]
  if (isTRUE(distinct)) {
    key <- append_only_tables()$forecast_archive$key
    out <- out[!duplicated(.row_signature(out[key])), schemas()$forecast_archive$names, drop = FALSE]
  } else {
    out <- out[c(schemas()$forecast_archive$names, "batch")]
  }
  rownames(out) <- NULL
  tibble::as_tibble(out)
}

.forecast_archive_empty <- function() {
  tibble::tibble(
    sport = character(), country = character(), sex = character(),
    fit_date = as.Date(character()), fit_id = character(), published_to = character(),
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

# A partition name no other write uses: the publication time and the CI run
# (or process) id, with a counter if this process writes twice in a second.
.forecast_archive_batch <- function(dest, rows, parent) {
  run <- Sys.getenv("GITHUB_RUN_ID")
  if (!nzchar(run)) run <- paste0("p", Sys.getpid())
  base <- paste0(format(min(rows$published_at), "%Y%m%dT%H%M%SZ", tz = "UTC"), "-", run)
  taken <- function(b) {
    keys <- dplyr::distinct(rows[, parent, drop = FALSE])
    any(vapply(seq_len(nrow(keys)), function(i) {
      segs <- vapply(parent, function(p) paste0(p, "=", as.character(keys[[p]][[i]])), character(1))
      fs::dir_exists(do.call(fs::path, c(list(dest), as.list(segs), paste0("batch=", b))))
    }, logical(1)))
  }
  b <- base
  k <- 1L
  while (taken(b)) {
    k <- k + 1L
    b <- paste0(base, "-", k)
  }
  b
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
