#' @include placer-validate.R placer-load.R placer-ledger.R placer-login.R
#' @include placer-navigate.R placer-place.R config.R
NULL

# Lengjan sport IDs (stable integer IDs used in URL query params)
.lengjan_sport_ids <- c(
  football   = 1L,
  basketball = 2L,
  handball   = 6L
)

#' Place bets from data/decisions/recommendations/.
#'
#' Top-level orchestrator for the placer. Reads recommendations from Parquet,
#' dedups against the ledger, optionally logs into Lengjan, and walks each
#' pending bet through the place-or-reject decision tree.
#'
#' Returns one row per bet attempted (including those skipped or errored).
#' Status values: "placed", "rejected_p3", "rejected_p4", "dry_run", "error",
#' "no_match_id", "skipped_by_user".
#'
#' @param leagues Optional character vector of league keys to restrict to
#'   (e.g. \code{"football_iceland"}). NULL = all.
#' @param dry_run If \code{TRUE}, navigate and select odds but do not click
#'   "Kaupa". Default \code{TRUE}.
#' @param interactive If \code{TRUE}, prompt before each bet. Default \code{TRUE}.
#' @param today_only Restrict to matches on today.
#' @param target_date Restrict to matches on a specific \code{Date}.
#' @param headless Run Chrome headlessly. Default \code{TRUE}.
#' @param root Data root.
#' @param dual_write_csv Mirror placed bets to legacy CSV ledgers.
#'   Default \code{FALSE} (Plan 6 cutover; Parquet at
#'   \code{data/decisions/ledger/} is canonical). Set \code{TRUE} only for
#'   opt-in regression-testing while the legacy CSV files exist on disk.
#' @return Tibble of per-bet results with at minimum columns: \code{sport},
#'   \code{country}, \code{sex}, \code{match_date}, \code{home_team},
#'   \code{away_team}, \code{market}, \code{outcome}, \code{odds},
#'   \code{bet_amount}, \code{status}.
#' @export
place_bets <- function(leagues = NULL,
                       dry_run = TRUE,
                       interactive = TRUE,
                       today_only = FALSE,
                       target_date = NULL,
                       headless = TRUE,
                       root = here::here("data"),
                       dual_write_csv = FALSE) {
  cli::cli_h1("Lengjan Bet Placement Pipeline")

  if (dry_run) {
    cli::cli_alert_warning("DRY RUN mode — no bets will be placed.")
  }

  # 1. Load league config
  leagues_cfg <- load_leagues()

  # 2. Load + date-filter recommendations
  recs <- load_recommendations(
    root        = root,
    leagues     = leagues,
    today_only  = today_only,
    target_date = target_date
  )
  if (nrow(recs) == 0L) {
    cli::cli_alert_info("No pending recommendations.")
    return(empty_placement_results())
  }

  # 3. Dedup against ledger (P1 idempotency)
  recs <- dedup_against_ledger(recs, root = root)
  if (nrow(recs) == 0L) {
    cli::cli_alert_info("All recommendations already placed.")
    return(empty_placement_results())
  }

  cli::cli_alert_info(
    "Found {nrow(recs)} recommendation(s) — previewing:"
  )
  for (i in seq_len(nrow(recs))) {
    b <- recs[i, ]
    cli::cli_alert_info(
      "  {b$sport}_{b$country}/{b$sex}: {b$home_team} v {b$away_team} ",
      "{b$market} {b$outcome} @ {b$odds} -> {b$bet_amount} kr"
    )
  }

  # 4. Validate schema + team-name wiring BEFORE Chrome launches
  validate_recommendations_schema(recs)
  validate_team_names_config(leagues_cfg, recs)

  # 5. Upfront confirmation (interactive + live)
  if (interactive && !dry_run) {
    answer <- readline("Place these bets? (y/n): ")
    if (!tolower(trimws(answer)) %in% c("y", "yes")) {
      cli::cli_alert_info("Cancelled by user.")
      return(empty_placement_results())
    }
  }

  # 6. Login
  cli::cli_h1("Logging in to Lengjan")
  session <- chromote_login(headless = headless)
  on.exit(tryCatch(session$close(), error = function(e) NULL), add = TRUE)

  # 7. Per-league: resolve match IDs once per competition, then place bets
  results <- list()
  league_keys <- unique(paste0(recs$sport, "_", recs$country))

  for (league_key in league_keys) {
    cli::cli_h2("Processing: {league_key}")
    league <- leagues_cfg[[league_key]]

    if (is.null(league)) {
      cli::cli_alert_warning("No leagues.yml entry for {league_key} — skipping.")
      next
    }

    sport_id <- .lengjan_sport_ids[[league$sport]]
    if (is.null(sport_id)) {
      cli::cli_alert_warning(
        "Unknown Lengjan sport_id for sport '{league$sport}' — skipping {league_key}."
      )
      next
    }

    # Filter to this league's bets
    league_recs <- recs[
      recs$sport == league$sport & recs$country == league$country, ,
      drop = FALSE
    ]

    # Build per-sex pipeline -> Lengjan name maps.
    # team_names has shape list(male = list(...), female = list(...)); each
    # sub-map is canonical-pipeline-name -> Lengjan-display-name.
    tn_all <- league$lengjan$team_names
    pipeline_to_lengjan_by_sex <- lapply(tn_all, function(sex_map) {
      if (length(sex_map) == 0L) {
        return(character(0))
      }
      stats::setNames(as.character(unlist(sex_map)), names(sex_map))
    })

    # Resolve match IDs across all competitions for this league
    match_ids <- resolve_match_ids_new(
      session = session,
      league = league,
      sport_id = sport_id,
      pipeline_to_lengjan = character(0) # kept for API stability; unused inside
    )

    # Place each bet
    for (i in seq_len(nrow(league_recs))) {
      bet <- league_recs[i, ]
      pipeline_to_lengjan <- pipeline_to_lengjan_by_sex[[bet$sex]] %||%
        character(0)

      home_l <- pipeline_to_lengjan[[bet$home_team]]
      if (is.null(home_l) || is.na(home_l)) home_l <- bet$home_team
      away_l <- pipeline_to_lengjan[[bet$away_team]]
      if (is.null(away_l) || is.na(away_l)) away_l <- bet$away_team

      match_key <- paste(home_l, away_l, sep = " - ")
      mid <- match_ids[[match_key]]

      if (is.null(mid)) {
        cli::cli_alert_warning(
          "No match ID for {bet$home_team} vs {bet$away_team} ",
          "(Lengjan: {home_l} vs {away_l}) — skipping."
        )
        results[[length(results) + 1L]] <- bet_result_row(bet, "no_match_id")
        next
      }

      # Per-bet interactive confirm
      if (interactive && !dry_run) {
        cat(sprintf(
          "\n  -> %s vs %s: %s %s @ %.2f -> %d kr\n",
          bet$home_team, bet$away_team,
          bet$market, bet$outcome,
          bet$odds, as.integer(bet$bet_amount)
        ))
        answer <- readline("    Place this bet? (y/n/q): ")
        if (tolower(trimws(answer)) == "q") {
          cli::cli_alert_info("Stopped by user.")
          break
        }
        if (!tolower(trimws(answer)) %in% c("y", "yes")) {
          results[[length(results) + 1L]] <- bet_result_row(bet, "skipped_by_user")
          next
        }
      }

      # placer-place.R expects `bet$probability`; schema uses `p`
      bet_for_place <- bet
      bet_for_place$probability <- bet$p

      result <- place_bet(
        session  = session,
        bet      = bet_for_place,
        match_id = mid,
        sport_id = sport_id,
        dry_run  = dry_run
      )

      # L1: Write to ledger after confirmed placement only (P2 actual odds)
      if (identical(result$status, "placed")) {
        actual_odds <- result$actual_odds %||% bet$odds
        actual_amount <- result$amount %||% bet$bet_amount
        actual_ev <- round(
          bet$p * (actual_odds - 1) - (1 - bet$p),
          4
        )

        bet_row <- tibble::tibble(
          placed_at      = Sys.time(),
          match_date     = bet$match_date,
          sport          = bet$sport,
          country        = bet$country,
          sex            = bet$sex,
          home_team      = bet$home_team,
          away_team      = bet$away_team,
          market         = bet$market,
          outcome        = bet$outcome,
          line           = bet$line,
          odds_placed    = actual_odds,
          p              = bet$p,
          kelly          = bet$kelly,
          bet_amount     = actual_amount,
          settled        = FALSE,
          win            = NA,
          pnl            = NA_real_
        )

        tryCatch(
          append_to_ledger(bet_row, root = root, dual_write_csv = dual_write_csv),
          error = function(e) {
            cli::cli_alert_danger(
              "Ledger write failed for {bet$home_team} v {bet$away_team}: {e$message}"
            )
          }
        )
      }

      results[[length(results) + 1L]] <- bet_result_row(bet, result$status)

      # Safety: clear the bet slip before the next bet
      tryCatch(clear_bet_slip(session), error = function(e) NULL)
      Sys.sleep(stats::runif(1, 2, 4))
    }
  }

  # 8. Summary
  cli::cli_rule()
  cli::cli_h2("Summary")
  if (length(results) > 0L) {
    out <- dplyr::bind_rows(results)
    for (s in unique(out$status)) {
      n <- sum(out$status == s)
      cli::cli_alert_info("  {s}: {n}")
    }
    return(out)
  }

  empty_placement_results()
}

# ── Internal helpers ──────────────────────────────────────────────────────────

#' Resolve Lengjan match IDs across all competitions for a league.
#'
#' Iterates \code{league$lengjan$competitions} (each with fields \code{id},
#' \code{name}, \code{sex}), calls \code{extract_matches()} per competition,
#' and builds a named list keyed by "Home - Away" (Lengjan display names).
#'
#' @param session ChromoteSession (authenticated)
#' @param league League config entry from \code{load_leagues()}
#' @param sport_id Integer Lengjan sport ID
#' @param pipeline_to_lengjan Named character: pipeline name -> Lengjan name
#' @return Named list: key = "Lengjan Home - Lengjan Away", value = match_id
#' @keywords internal
#' @noRd
resolve_match_ids_new <- function(session, league, sport_id, pipeline_to_lengjan) {
  competitions <- league$lengjan$competitions
  if (is.null(competitions) || length(competitions) == 0L) {
    cli::cli_alert_warning("No competitions configured for this league.")
    return(list())
  }

  all_ids <- list()

  for (comp in competitions) {
    comp_id <- comp$id
    matches <- tryCatch(
      extract_matches(session, sport_id = sport_id, competition_id = comp_id),
      error = function(e) {
        cli::cli_alert_warning(
          "extract_matches failed for competition {comp_id}: {e$message}"
        )
        tibble::tibble(
          home = character(), away = character(), match_id = character()
        )
      }
    )

    if (nrow(matches) == 0L) next

    for (j in seq_len(nrow(matches))) {
      m <- matches[j, ]
      key <- paste(m$home, m$away, sep = " - ")
      all_ids[[key]] <- m$match_id
    }
  }

  all_ids
}

#' Construct a single-row result tibble for the per-bet output.
#'
#' @param bet Single-row recommendations tibble.
#' @param status Character status string.
#' @keywords internal
#' @noRd
bet_result_row <- function(bet, status) {
  tibble::tibble(
    sport      = bet$sport,
    country    = bet$country,
    sex        = bet$sex,
    match_date = bet$match_date,
    home_team  = bet$home_team,
    away_team  = bet$away_team,
    market     = bet$market,
    outcome    = bet$outcome,
    odds       = bet$odds,
    bet_amount = bet$bet_amount,
    status     = status
  )
}

#' Empty placement results tibble with the documented column set.
#'
#' @keywords internal
#' @noRd
empty_placement_results <- function() {
  tibble::tibble(
    sport      = character(),
    country    = character(),
    sex        = character(),
    match_date = as.Date(character()),
    home_team  = character(),
    away_team  = character(),
    market     = character(),
    outcome    = character(),
    odds       = numeric(),
    bet_amount = numeric(),
    status     = character()
  )
}

# Null-coalescing operator (avoids depending on rlang::`%||%` inside this file)
`%||%` <- function(x, y) if (!is.null(x)) x else y
