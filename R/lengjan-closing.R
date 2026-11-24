#' @include lengjan-archive.R decide-normalise.R
NULL

# Closing odds from a local copy of the raw Lengjan archive: each event's last
# captured prices before kick-off, joined to results. Pure functions over
# read_lengjan_archive(table = "odds_markets") and lengjan_archive_events()
# output; nothing here reads or writes the facts store.

#' Closing prices: each event's last captured market rows before kick-off.
#'
#' Closing is taken per event, not per selection: an event's closing capture
#' is its last capture holding at least one pre-close row, and only the rows
#' that capture lists are kept. A line Lengjan withdrew before that capture
#' (an old total, say) is therefore not carried forward from an earlier one.
#'
#' A row is pre-close when it was captured no later than its event's
#' kick-off and its market's close (`closes_at`, where known), and its market
#' is not in play (`is_live`). The kick-off is the capture's own `kickoff_at`,
#' so a match moved to a later time keeps the captures taken before the new
#' time. Where the event's last listing gives an earlier kick-off than a
#' capture's own (a match brought forward), the earlier one applies, so a
#' capture that still showed the old time after the match began is not
#' closing.
#'
#' @param rows Output of `read_lengjan_archive(table = "odds_markets")`, or
#'   any rows with its columns. Only detail captures give every market; rows
#'   from `what = "program"` give each event's main market only.
#' @param as_of The time the rows cover up to, as a `POSIXct`: normally the
#'   archive's last capture in the window read
#'   (`max(lengjan_archive_index()$ts)`). An event whose kick-off is later has
#'   not closed and is dropped. Required, because the rows alone cannot tell
#'   an event that closed without a late capture from one the local copy has
#'   not caught up with.
#' @return `rows`' closing rows, with `kickoff_at` and `match_date` the
#'   kick-off that applied (see above) and two added columns:
#'   `minutes_before` (kick-off minus the closing capture's `scraped_at`, in
#'   minutes) and `kickoff_changed` (whether the event's listed kick-off
#'   varied across its rows). A large `minutes_before` means the last price
#'   captured is far from the true close. Sorted by kick-off, event and
#'   market, each market's selections in the order `rows` gives them.
#' @examples
#' \dontrun{
#' rows <- read_lengjan_archive(from = "2026-11-16", table = "odds_markets")
#' closing <- lengjan_closing_odds(rows, as_of = max(lengjan_archive_index()$ts))
#' }
#' @export
lengjan_closing_odds <- function(rows, as_of) {
  need <- c(
    "sport", "event_id", "scraped_at", "kickoff_at", "match_date",
    "market_id", "selection", "closes_at", "is_live"
  )
  missing <- setdiff(need, names(rows))
  if (length(missing) > 0L) {
    stop(
      "lengjan_closing_odds: `rows` lacks column(s) ",
      paste(missing, collapse = ", "),
      "; pass read_lengjan_archive(table = \"odds_markets\") output.",
      call. = FALSE
    )
  }
  if (missing(as_of) || !inherits(as_of, "POSIXct") || length(as_of) != 1L ||
    is.na(as_of)) {
    stop(
      "lengjan_closing_odds: `as_of` must be one POSIXct, normally ",
      "max(lengjan_archive_index()$ts).",
      call. = FALSE
    )
  }
  rows$minutes_before <- rep(NA_real_, nrow(rows))
  rows$kickoff_changed <- rep(NA, nrow(rows))
  if (nrow(rows) == 0L) {
    return(rows)
  }

  ord <- order(rows$event_id, rows$scraped_at)
  last <- ord[!duplicated(rows$event_id[ord], fromLast = TRUE)]
  final_ko <- rows$kickoff_at[last][match(rows$event_id, rows$event_id[last])]
  n_ko <- tapply(rows$kickoff_at, rows$event_id, function(k) {
    length(unique(k[!is.na(k)]))
  })
  rows$kickoff_changed <- unname(n_ko[rows$event_id] > 1L)

  ko <- rows$kickoff_at
  earlier <- !is.na(final_ko) & !is.na(ko) & final_ko < ko
  ko[earlier] <- final_ko[earlier]
  rows$kickoff_at <- ko
  rows$match_date <- as.Date(ko, tz = "UTC")

  pre <- !is.na(ko) & rows$scraped_at <= ko &
    (is.na(rows$closes_at) | rows$scraped_at <= rows$closes_at) &
    !(rows$is_live %in% TRUE)
  out <- rows[pre, , drop = FALSE]
  closing_at <- stats::ave(
    as.numeric(out$scraped_at), out$event_id,
    FUN = max
  )
  out <- out[as.numeric(out$scraped_at) == closing_at, , drop = FALSE]
  out <- out[out$kickoff_at <= as_of, , drop = FALSE]
  out$minutes_before <- as.numeric(
    difftime(out$kickoff_at, out$scraped_at, units = "mins")
  )
  out[order(out$kickoff_at, out$event_id, out$market_id, method = "radix"), ,
    drop = FALSE
  ]
}

#' Join closing rows to match results.
#'
#' Results carry no Lengjan event id, so the join is the settle layer's
#' ([compute_settlement()]): team names are rewritten to canonical form with
#' [normalise_lengjan_team_names()] for the event's league and sex, then
#' matched on (sport, country, sex, match_date, home_team, away_team). An
#' event with no exact match falls back to a result for the same pairing
#' within `match_date_window_days` of the listed date (a rescheduled match),
#' when exactly one such result exists that no exact match has claimed. A
#' result is never joined to two events: one that two events reach equally
#' (exact, or both through the window) goes to neither. The join is per
#' event; every closing row of the event gets its result.
#'
#' The league is the one in `leagues` with the row's sport and country whose
#' Lengjan competitions list the row's `competition_id`. A row with no `sex`
#' (its competition is not wired in `leagues`) cannot be normalised or
#' matched and is marked `unwired`.
#'
#' Basketball's 1X2 market is settled on regulation time: compare it with
#' `home_score_reg`/`away_score_reg`, not the final score. Totals and
#' handicaps follow each market's own rules.
#'
#' @param closing Output of [lengjan_closing_odds()].
#' @param results Results rows (`read_table("results")`): `sport`, `country`,
#'   `sex`, `match_date`, `home_team`, `away_team`, `home_score`,
#'   `away_score`, and optionally `home_score_reg`, `away_score_reg`.
#' @param leagues League definitions, for team names and competitions.
#' @param match_date_window_days Non-negative integer: how many days either
#'   side of the listed date the fallback may look. `0` disables it.
#' @return `closing` with `home_team`/`away_team` in canonical form where the
#'   league maps them, Lengjan's renderings in `lengjan_home_team`/
#'   `lengjan_away_team`, and the result in `result_date`, `home_score`,
#'   `away_score`, `home_score_reg`, `away_score_reg` and `result_join`:
#'   `"exact"`, `"date_window"`, `"ambiguous"` (more than one result in the
#'   window), `"duplicate"` (its result also matches another event: a listing
#'   Lengjan voided and relisted, or one match listed twice), `"no_result"` or
#'   `"unwired"`. Unjoined events keep their rows with NA scores.
#' @examples
#' \dontrun{
#' joined <- lengjan_closing_results(closing, read_table("results"))
#' table(joined$result_join[!duplicated(joined$event_id)])
#' }
#' @export
lengjan_closing_results <- function(closing, results, leagues = load_leagues(),
                                    match_date_window_days = 3L) {
  stopifnot(match_date_window_days >= 0L)
  key <- c("sport", "country", "sex", "match_date", "home_team", "away_team")
  ev <- closing[!duplicated(closing$event_id),
    c("event_id", "competition_id", key),
    drop = FALSE
  ]
  ev$lengjan_home_team <- ev$home_team
  ev$lengjan_away_team <- ev$away_team
  ev$league <- vapply(seq_len(nrow(ev)), function(i) {
    .lengjan_league_for(leagues, ev$sport[[i]], ev$country[[i]], ev$competition_id[[i]])
  }, character(1))
  wired <- !is.na(ev$sex) & !is.na(ev$league)

  groups <- unique(ev[wired, c("league", "sex"), drop = FALSE])
  for (g in seq_len(nrow(groups))) {
    in_g <- wired & ev$league == groups$league[[g]] & ev$sex == groups$sex[[g]]
    ev[in_g, ] <- normalise_lengjan_team_names(
      ev[in_g, , drop = FALSE], leagues[[groups$league[[g]]]], groups$sex[[g]]
    )
  }

  for (col in c("home_score_reg", "away_score_reg")) {
    if (!col %in% names(results)) results[[col]] <- rep(NA_integer_, nrow(results))
  }
  scores <- c("home_score", "away_score", "home_score_reg", "away_score_reg")
  ev$result_date <- as.Date(rep(NA_character_, nrow(ev)))
  for (col in scores) ev[[col]] <- rep(NA_integer_, nrow(ev))
  ev$result_join <- ifelse(wired, "no_result", "unwired")

  exact <- near <- vector("list", nrow(ev))
  for (i in which(wired)) {
    pair <- results$sport == ev$sport[[i]] & results$country == ev$country[[i]] &
      results$sex == ev$sex[[i]] & tag_utf8(results$home_team) == tag_utf8(ev$home_team[[i]]) &
      tag_utf8(results$away_team) == tag_utf8(ev$away_team[[i]])
    pair <- pair & !is.na(pair)
    gap <- abs(as.integer(results$match_date - ev$match_date[[i]]))
    exact[[i]] <- which(pair & gap == 0L)
    near[[i]] <- which(pair & gap <= match_date_window_days)
  }

  hit <- rep(NA_integer_, nrow(ev))
  how <- rep(NA_character_, nrow(ev))
  one <- which(lengths(exact) == 1L)
  hit[one] <- unlist(exact[one])
  how[one] <- "exact"
  ev$result_join[lengths(exact) > 1L] <- "ambiguous"
  # WHY: exact matches claim their results first, so a voided listing
  # relisted on another day cannot reach the same result through the window.
  claimed <- hit[!is.na(hit)]
  for (i in which(wired & lengths(exact) == 0L & lengths(near) > 0L)) {
    free <- setdiff(near[[i]], claimed)
    if (length(free) == 1L) {
      hit[[i]] <- free
      how[[i]] <- "date_window"
    } else {
      ev$result_join[[i]] <- if (length(free) > 1L) "ambiguous" else "duplicate"
    }
  }
  twice <- !is.na(hit) & hit %in% hit[duplicated(hit)]
  ev$result_join[twice] <- "duplicate"
  hit[twice] <- NA_integer_

  ok <- which(!is.na(hit))
  ev$result_join[ok] <- how[ok]
  ev$result_date[ok] <- results$match_date[hit[ok]]
  for (col in scores) ev[[col]][ok] <- as.integer(results[[col]][hit[ok]])

  added <- c(
    "home_team", "away_team", "lengjan_home_team", "lengjan_away_team",
    "result_date", scores, "result_join"
  )
  out <- closing[, setdiff(names(closing), c("home_team", "away_team")), drop = FALSE]
  out <- dplyr::left_join(out, ev[, c("event_id", added), drop = FALSE], by = "event_id")
  out[, c(names(closing), setdiff(added, names(closing))), drop = FALSE]
}

#' Name of the league in `leagues` with this sport and country whose Lengjan
#' competitions list `competition_id`; NA when none does.
#' @noRd
.lengjan_league_for <- function(leagues, sport, country, competition_id) {
  for (nm in names(leagues)) {
    lg <- leagues[[nm]]
    if (!identical(.scalar_chr(lg$sport), sport) ||
      !identical(.scalar_chr(lg$country), country)) {
      next
    }
    ids <- vapply(lg$lengjan$competitions %||% list(), function(cp) {
      .scalar_chr(cp$id)
    }, character(1))
    if (competition_id %in% ids) {
      return(nm)
    }
  }
  NA_character_
}

#' How well the archive's captures cover each event's close.
#'
#' One row per event that was listed with at least one market before its
#' kick-off (a listing's own) and kicked off by `as_of`, with how close to
#' kick-off its closing capture came. An event seen only once in play, with
#' no pre-match listing in the window, is not counted. The listed events come
#' from [lengjan_archive_events()] (every capture's program body), so an
#' event that never got a market-detail capture still counts, as a miss.
#'
#' @param listings Output of [lengjan_archive_events()].
#' @param closing Output of [lengjan_closing_odds()] or
#'   [lengjan_closing_results()] over the same window.
#' @param as_of The time the listings cover up to; an event kicking off later
#'   is left out. Defaults to the latest listing.
#' @param within_minutes A closing capture this many minutes or fewer before
#'   kick-off counts as covering the close.
#' @return Tibble, one row per event, sorted by kick-off: `event_id`, `sport`,
#'   `competition_id`, `competition_name`, `home_team`, `away_team` (as
#'   Lengjan renders them), `kickoff_at`, `live_flagged` (the event was ever
#'   listed with Lengjan's event-level live flag, which means live betting
#'   will be offered), `closing_at`, `minutes_before`, `n_rows` (closing
#'   rows), `status` (`"within"`, `"stale"`: a closing capture, but earlier
#'   than `within_minutes`, or `"no_detail"`: none) and, when `closing` has
#'   it, `result_join`.
#' @examples
#' \dontrun{
#' listings <- lengjan_archive_events(from = "2026-11-16")
#' cov <- lengjan_closing_coverage(listings, closing)
#' table(cov$sport, cov$status)
#' }
#' @export
lengjan_closing_coverage <- function(listings, closing, as_of = NULL,
                                     within_minutes = 15) {
  as_of <- as_of %||% max(listings$scraped_at)
  ord <- order(listings$event_id, listings$scraped_at)
  last <- listings[ord, , drop = FALSE]
  last <- last[!duplicated(last$event_id, fromLast = TRUE), , drop = FALSE]
  agg <- function(x, f) unname(tapply(x, listings$event_id, f)[last$event_id])
  ev <- tibble::tibble(
    event_id = last$event_id, sport = last$sport,
    competition_id = last$competition_id,
    competition_name = last$competition_name,
    home_team = last$home_team, away_team = last$away_team,
    kickoff_at = last$kickoff_at,
    pre_match = agg(
      listings$scraped_at <= listings$kickoff_at & listings$market_count > 0L,
      function(x) any(x %in% TRUE)
    ),
    live_flagged = agg(listings$is_live, function(x) any(x %in% TRUE))
  )

  cl <- closing[!duplicated(closing$event_id), , drop = FALSE]
  i <- match(ev$event_id, cl$event_id)
  ev$closing_at <- cl$scraped_at[i]
  ev$minutes_before <- cl$minutes_before[i]
  ev$n_rows <- as.integer(table(closing$event_id)[ev$event_id])
  ev$n_rows[is.na(ev$n_rows)] <- 0L
  ko <- cl$kickoff_at[i]
  ev$kickoff_at[!is.na(ko)] <- ko[!is.na(ko)]

  ev <- ev[ev$pre_match & !is.na(ev$home_team) & !is.na(ev$away_team) &
    !is.na(ev$kickoff_at) & ev$kickoff_at <= as_of, , drop = FALSE]
  ev$status <- ifelse(is.na(ev$closing_at), "no_detail",
    ifelse(ev$minutes_before <= within_minutes, "within", "stale")
  )
  if ("result_join" %in% names(closing)) {
    ev$result_join <- cl$result_join[match(ev$event_id, cl$event_id)]
  }
  ev$pre_match <- NULL
  ev[order(ev$kickoff_at, ev$event_id), , drop = FALSE]
}

#' Where scripts/0No_closing_odds.R writes: `out`, else environment variable
#' `SPORTS_CLOSING_ODDS_DIR`, else the git-ignored
#' `research/results/closing-odds/`. A path inside the repo must be
#' git-ignored and never under `data/`: the closing table shows when the
#' archive captured, and whether that is published is the owner's call.
#' @noRd
closing_odds_out_dir <- function(out = NULL, repo = here::here()) {
  if (is.null(out) || !nzchar(out)) out <- Sys.getenv("SPORTS_CLOSING_ODDS_DIR", unset = "")
  if (!nzchar(out)) out <- file.path(repo, "research", "results", "closing-odds")
  path <- .closing_abs_path(out)
  root <- normalizePath(repo, mustWork = TRUE)
  if (!startsWith(paste0(path, "/"), paste0(root, "/"))) {
    return(path)
  }
  rel <- substring(path, nchar(root) + 2L)
  if (identical(rel, "data") || startsWith(rel, "data/")) {
    stop(
      "The closing table never goes under data/ (", rel, "): pass --out or ",
      "SPORTS_CLOSING_ODDS_DIR outside it.",
      call. = FALSE
    )
  }
  ignored <- .git_run(c("-C", root, "check-ignore", "-q", file.path(rel, "x")))
  if (!ignored$ok) {
    stop(
      "The closing table's output path must be git-ignored inside the repo; ",
      rel, " is not. Use research/results/ or a path outside the repo.",
      call. = FALSE
    )
  }
  path
}

#' An absolute path with `.` and `..` collapsed (fs::path_abs() does both)
#' and symlinks resolved as far as the path exists. normalizePath() alone
#' returns a missing relative path unchanged, which would pass for a path
#' outside the repo.
#' @noRd
.closing_abs_path <- function(p) {
  p <- as.character(fs::path_abs(path.expand(p)))
  rest <- character()
  while (!file.exists(p) && dirname(p) != p) {
    rest <- c(basename(p), rest)
    p <- dirname(p)
  }
  p <- normalizePath(p, mustWork = TRUE)
  if (length(rest) == 0L) p else paste(c(p, rest), collapse = "/")
}
