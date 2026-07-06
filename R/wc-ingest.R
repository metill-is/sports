#' @include storage.R
NULL

#' Patch martj42 results with operator-supplied scores.
#'
#' Fills the `home_score`/`away_score` of `raw` rows whose scores are still `NA`
#' from an overlay keyed on `(date, home_team, away_team)`, for use when martj42
#' lags behind played matches. martj42 stays canonical: once it carries a real
#' score the overlay row is ignored (and, if it disagrees, a warning prompts the
#' operator to prune it, so the overlay self-drains). An overlay row that matches
#' no fixture aborts loudly — a silent no-op would hide a team-name typo.
#'
#' @param raw martj42-schema data frame (`date, home_team, away_team,
#'   home_score, away_score, ...`), as read by [wc_ingest_internationals()].
#' @param overlay Data frame with `date` (Date), `home_team`, `away_team`,
#'   `home_score`, `away_score`. May be empty (no-op).
#' @return `raw` with matched `NA`-score rows filled.
#' @export
wc_apply_manual_results <- function(raw, overlay) {
  if (is.null(overlay) || nrow(overlay) == 0L) {
    return(raw)
  }
  sep <- "\u001f" # unit separator - cannot appear in a date or team name
  raw_key <- paste(raw$date, raw$home_team, raw$away_team, sep = sep)
  n_filled <- 0L
  for (i in seq_len(nrow(overlay))) {
    o <- overlay[i, , drop = FALSE]
    hits <- which(raw_key == paste(o$date, o$home_team, o$away_team, sep = sep))
    if (length(hits) == 0L) {
      cli::cli_abort(c(
        "Manual overlay row matches no martj42 fixture.",
        "x" = "{o$date} {o$home_team} vs {o$away_team}",
        "i" = "Check the team-name spelling (run scripts/wc/list_missing.R)."
      ))
    }
    if (length(hits) > 1L) {
      cli::cli_abort(
        "Overlay row {o$date} {o$home_team} vs {o$away_team} is ambiguous \\
        ({length(hits)} martj42 rows match)."
      )
    }
    j <- hits[[1L]]
    eh <- raw$home_score[[j]]
    ea <- raw$away_score[[j]]
    if (!is.na(eh) && !is.na(ea)) {
      if (!identical(as.integer(eh), as.integer(o$home_score)) ||
        !identical(as.integer(ea), as.integer(o$away_score))) {
        cli::cli_warn(c(
          "martj42 already reports a different score; keeping martj42.",
          "i" = "{o$date} {o$home_team}: {eh}-{ea} (martj42) vs \\
            {o$home_score}-{o$away_score} (overlay). Remove it from manual_results.csv."
        ))
      }
      next
    }
    raw$home_score[[j]] <- as.integer(o$home_score)
    raw$away_score[[j]] <- as.integer(o$away_score)
    n_filled <- n_filled + 1L
  }
  if (n_filled > 0L) {
    cli::cli_alert_success(
      "Applied {n_filled} manual result{?s} martj42 hasn't published yet."
    )
  }
  raw
}

#' List WC fixtures that should be played but martj42 hasn't scored.
#'
#' Operator scaffold for `data/wc/manual_results.csv`: the WC-finals fixtures
#' with a kickoff on/before `as_of` and a still-`NA` score. Operates on the raw
#' martj42 schema (`tournament`/`date`), the same `raw` [wc_apply_manual_results()]
#' receives.
#'
#' @param raw martj42-schema data frame.
#' @param as_of Latest kickoff date to include. Default today.
#' @return Data frame `date, home_team, away_team, home_score, away_score` with
#'   `NA` scores, ready to paste into the overlay.
#' @importFrom rlang .data
#' @export
wc_list_unscored_fixtures <- function(raw, as_of = Sys.Date()) {
  raw |>
    dplyr::filter(
      .data$tournament == "FIFA World Cup",
      format(.data$date, "%Y") == "2026",
      .data$date <= as_of,
      is.na(.data$home_score) | is.na(.data$away_score)
    ) |>
    dplyr::transmute(
      date = .data$date,
      home_team = .data$home_team,
      away_team = .data$away_team,
      home_score = NA_integer_,
      away_score = NA_integer_
    )
}

# Filter martj42 shootouts to 2026 WC knockout pairs, union with the manual
# overlay's pen_winner rows. martj42 is canonical on a pair conflict (same
# self-draining discipline as the score overlay). Returns
# `date,home_team,away_team,winner`.
.wc_shootouts_overlay <- function(martj42, manual, structure) {
  go <- structure$group_of
  is_wc_ko <- function(h, a) {
    !is.na(go[h]) & !is.na(go[a]) & go[h] != go[a]
  }
  empty <- tibble::tibble(
    date = character(), home_team = character(),
    away_team = character(), winner = character()
  )

  m_rows <- empty
  if (!is.null(martj42) && nrow(martj42) > 0L) {
    keep <- format(as.Date(martj42$date), "%Y") == "2026" &
      is_wc_ko(martj42$home_team, martj42$away_team)
    keep[is.na(keep)] <- FALSE
    mm <- martj42[keep, , drop = FALSE]
    if (nrow(mm) > 0L) {
      m_rows <- tibble::tibble(
        date = as.character(mm$date), home_team = mm$home_team,
        away_team = mm$away_team, winner = mm$winner
      )
    }
  }

  man_rows <- empty
  if (!is.null(manual) && nrow(manual) > 0L && "pen_winner" %in% names(manual)) {
    pw <- as.character(manual$pen_winner)
    has <- !is.na(pw) & nzchar(trimws(pw))
    mm <- manual[has, , drop = FALSE]
    if (nrow(mm) > 0L) {
      keep <- is_wc_ko(mm$home_team, mm$away_team)
      keep[is.na(keep)] <- FALSE
      mm <- mm[keep, , drop = FALSE]
      if (nrow(mm) > 0L) {
        win <- trimws(as.character(mm$pen_winner))
        # The operator types pen_winner by hand. A name that is neither team
        # (typo, or the wrong team) would be written as the winner and later
        # crash the pin-builder deep in the forecast. Abort loudly here instead,
        # mirroring wc_apply_manual_results' name-typo discipline.
        bad <- win != mm$home_team & win != mm$away_team
        if (any(bad)) {
          cli::cli_abort(c(
            "Manual pen_winner is not one of the match's two teams.",
            "x" = "{.val {win[bad]}} for {mm$home_team[bad]} vs {mm$away_team[bad]} ({mm$date[bad]})",
            "i" = "pen_winner must exactly match home_team or away_team (martj42 spelling)."
          ))
        }
        man_rows <- tibble::tibble(
          date = as.character(mm$date), home_team = mm$home_team,
          away_team = mm$away_team, winner = win
        )
      }
    }
  }

  # martj42 canonical: drop manual rows whose pair already has a martj42 winner.
  man_rows <- man_rows[
    !(.wc_pair_key(man_rows$home_team, man_rows$away_team) %in%
      .wc_pair_key(m_rows$home_team, m_rows$away_team)), ,
    drop = FALSE
  ]
  combined <- rbind(m_rows, man_rows)
  combined[!duplicated(.wc_pair_key(combined$home_team, combined$away_team)), ,
    drop = FALSE
  ]
}

#' Ingest WC penalty-shootout winners into the committed shootouts store.
#'
#' The simulator models extra-time / shootouts as more 90' play, so the facts
#' store records a drawn knockout as level on score with no winner. This writes
#' the actual shootout winner to `data/wc/shootouts.csv` (committed) from
#' martj42's `shootouts.csv` (canonical, ~1 day lagged) unioned with the manual
#' overlay's `pen_winner` column (the lag-window operator path), filtered to
#' 2026 WC knockout pairs. [wc_knockout_results()] + [wc_shootout_winners()] then
#' resolve a level knockout's winner for pinning.
#'
#' @param structure Output of [wc_structure()].
#' @param shootouts_csv Path to martj42's `shootouts.csv`
#'   (`date,home_team,away_team,winner,first_shooter`); skipped when absent.
#' @param manual_overlay_path Path to `manual_results.csv` (its optional
#'   `pen_winner` column); skipped when absent.
#' @param root Data root (writes `wc/shootouts.csv` under it).
#' @return Invisibly, the number of shootout rows written.
#' @export
wc_ingest_shootouts <- function(structure,
                                shootouts_csv = here::here("data", "wc", "raw", "shootouts.csv"),
                                manual_overlay_path = here::here("data", "wc", "manual_results.csv"),
                                root = here::here("data")) {
  martj42 <- if (file.exists(shootouts_csv)) {
    utils::read.csv(shootouts_csv, colClasses = "character", stringsAsFactors = FALSE)
  } else {
    NULL
  }
  manual <- if (file.exists(manual_overlay_path)) {
    suppressWarnings(readr::read_csv(
      manual_overlay_path,
      comment = "#", col_types = readr::cols(.default = readr::col_character())
    ))
  } else {
    NULL
  }

  tbl <- .wc_shootouts_overlay(martj42, manual, structure)
  out_path <- file.path(root, "wc", "shootouts.csv")
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(tbl, out_path, row.names = FALSE, quote = FALSE)
  cli::cli_alert_success(
    "Wrote {nrow(tbl)} WC shootout winner{?s} to {.path {out_path}}."
  )
  invisible(nrow(tbl))
}

#' Ingest international football results into the facts store.
#'
#' Reads the bulk international-results CSV (martj42 schema:
#' `date, home_team, away_team, home_score, away_score, tournament, city,
#' country, neutral`) and writes it to the canonical Parquet facts store under
#' `sport=football / country=world / sex=male`, so the standard
#' [prepare_data()] -> [fit_model()] flow can fit the existing bivariate-Poisson
#' football model on internationals unchanged.
#'
#' Played matches (non-NA scores) go to `results`; unplayed fixtures (NA scores,
#' notably the upcoming World Cup matches the source already lists) go to
#' `schedules`. `division` carries the competition type (`tournament`); the
#' football model ignores it, but it keeps the natural key unique when the same
#' two nations meet in different competitions on nearby dates.
#'
#' The training population is aligned with the prediction population the same way
#' the Iceland `training_filter` aligns cup minnows: a match is kept only if both
#' teams are either a 2026 World Cup participant or have played at least
#' `min_team_matches` internationals inside the window. This trims the long tail
#' of one-off micro-nation fixtures (the international analogue of the cup-blowout
#' funnel) while retaining every WC team's full recent history.
#'
#' @param csv_path Path to the martj42 `results.csv`.
#' @param window_start Earliest `match_date` to keep. The random-walk model
#'   down-weights older matches anyway; this bounds `N_rounds` (the busiest
#'   team's appearance count) and drops defunct teams. Default 2022-01-01 — a
#'   full post-Qatar cycle of qualifiers, continental tournaments and friendlies,
#'   which is ample for current-strength estimation and keeps the fit tractable.
#' @param min_team_matches Minimum in-window internationals for an opponent to
#'   be retained. Default 8.
#' @param root Data root. Default `here::here("data")`.
#' @param manual_overlay_path Optional CSV of operator-supplied scores
#'   (`date, home_team, away_team, home_score, away_score`) merged onto martj42's
#'   `NA`-score rows via [wc_apply_manual_results()]. Default
#'   `data/wc/manual_results.csv`; skipped when absent. Before the merge,
#'   unplayed knockout fixture dates are corrected against the vendored
#'   official calendar via [wc_correct_knockout_dates()] (martj42's upcoming
#'   dates are unreliable; the 2026-07-06 incident published the wrong day for
#'   two R16 ties).
#' @return Invisibly, a list of row counts (`n_results`, `n_schedule`,
#'   `n_teams`, `n_wc_teams`).
#' @importFrom rlang .data
#' @export
wc_ingest_internationals <- function(csv_path = here::here("data", "wc", "raw", "results.csv"),
                                     window_start = as.Date("2022-01-01"),
                                     min_team_matches = 8L,
                                     root = here::here("data"),
                                     manual_overlay_path = here::here("data", "wc", "manual_results.csv")) {
  raw <- readr::read_csv(
    csv_path,
    col_types = readr::cols(
      date = readr::col_date(),
      home_team = readr::col_character(),
      away_team = readr::col_character(),
      home_score = readr::col_integer(),
      away_score = readr::col_integer(),
      tournament = readr::col_character(),
      city = readr::col_character(),
      country = readr::col_character(),
      neutral = readr::col_logical()
    )
  )

  # martj42's dates on not-yet-played knockout fixtures are unreliable (its
  # cities are not): re-date them from the vendored official calendar BEFORE
  # the overlay merge, so overlay rows key on corrected dates.
  raw <- wc_correct_knockout_dates(raw)

  # Operator overlay: fill scores martj42 hasn't published yet (martj42 stays
  # canonical once it catches up). Default path => the CI cron honours it too.
  if (!is.null(manual_overlay_path) && file.exists(manual_overlay_path)) {
    overlay <- readr::read_csv(
      manual_overlay_path,
      comment = "#",
      col_types = readr::cols(
        date = readr::col_date(),
        home_team = readr::col_character(),
        away_team = readr::col_character(),
        home_score = readr::col_integer(),
        away_score = readr::col_integer()
      )
    )
    raw <- wc_apply_manual_results(raw, overlay)
  }

  d <- raw |>
    dplyr::transmute(
      match_date = .data$date,
      home_team  = .data$home_team,
      away_team  = .data$away_team,
      home_score = .data$home_score,
      away_score = .data$away_score,
      division   = .data$tournament
    ) |>
    dplyr::filter(.data$match_date >= window_start)

  wc_teams <- d |>
    dplyr::filter(
      .data$division == "FIFA World Cup",
      format(.data$match_date, "%Y") == "2026"
    ) |>
    (\(x) unique(c(x$home_team, x$away_team)))()

  played <- d |>
    dplyr::filter(!is.na(.data$home_score) & !is.na(.data$away_score))

  match_counts <- table(c(played$home_team, played$away_team))
  frequent <- names(match_counts)[match_counts >= as.integer(min_team_matches)]
  eligible <- union(wc_teams, frequent)

  # Align the training population with the prediction population (the 48 WC
  # teams): keep a match only if it involves at least one WC team and both
  # teams clear the activity floor. Drops minnow-vs-minnow games irrelevant to
  # WC-team strength while keeping every WC team's full recent history.
  keep_match <- function(df) {
    df[(df$home_team %in% wc_teams | df$away_team %in% wc_teams) &
      df$home_team %in% eligible & df$away_team %in% eligible, , drop = FALSE]
  }

  results <- keep_match(played) |>
    dplyr::transmute(
      sport      = "football",
      country    = "world",
      sex        = "male",
      season     = as.integer(format(.data$match_date, "%Y")),
      match_date = .data$match_date,
      home_team  = .data$home_team,
      away_team  = .data$away_team,
      home_score = .data$home_score,
      away_score = .data$away_score,
      division   = .data$division,
      round      = NA_integer_
    )

  schedule <- d |>
    dplyr::filter(is.na(.data$home_score) | is.na(.data$away_score)) |>
    keep_match() |>
    dplyr::transmute(
      sport        = "football",
      country      = "world",
      sex          = "male",
      season       = as.integer(format(.data$match_date, "%Y")),
      match_date   = .data$match_date,
      home_team    = .data$home_team,
      away_team    = .data$away_team,
      division     = .data$division,
      round        = NA_integer_,
      kickoff_time = NA_character_
    )

  write_table(results, "results", root = root)
  if (nrow(schedule) > 0L) {
    write_table(schedule, "schedules", root = root)
  }

  cli::cli_alert_success(
    "Ingested {nrow(results)} played + {nrow(schedule)} upcoming international matches \\
    ({length(unique(c(results$home_team, results$away_team)))} teams, \\
    {length(wc_teams)} WC participants)."
  )

  invisible(list(
    n_results  = nrow(results),
    n_schedule = nrow(schedule),
    n_teams    = length(unique(c(results$home_team, results$away_team))),
    n_wc_teams = length(wc_teams)
  ))
}
