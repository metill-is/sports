#' Lengjan Bet Placement Pipeline
#'
#' Reads pending bets from bets_log.csv across all active leagues,
#' resolves match IDs, and places bets on Lengjan.

box::use(
  dplyr[filter, mutate, left_join, pull, bind_rows, select, arrange, distinct],
  readr[read_csv, write_csv],
  purrr[map, map_dfr, walk, imap],
  cli[
    cli_alert_info, cli_alert_success, cli_alert_warning,
    cli_alert_danger, cli_h1, cli_h2, cli_rule
  ],
  yaml[read_yaml],
  here[here]
)

box::use(
  ./login[lengjan_login, is_authenticated],
  ./navigate[extract_matches, competition_url, match_url],
  ./place_bet[place_bet]
)

# ── Main pipeline ─────────────────────────────────────────────────────────────

#' Run the bet placement pipeline
#'
#' @param leagues Character vector of league keys (e.g., "football_england").
#'   NULL = all leagues with pending bets.
#' @param dry_run If TRUE, navigate and select odds but don't click "Kaupa"
#' @param interactive If TRUE, prompt for confirmation before each bet
#' @param sports_dir Path to the Sports project root
#' @param odds_dir Path to the lengjan-odds project root
#' @return A tibble summarising all bet placement results
run_bets <- function(
  leagues = NULL,
  dry_run = TRUE,
  interactive = TRUE,
  sports_dir = here::here("../Sports"),
  odds_dir = here::here("../lengjan-odds")
) {

  cli_h1("Lengjan Bet Placement Pipeline")

  if (dry_run) {
    cli_alert_warning("DRY RUN mode — no bets will be placed.")
  }

  # ── 1. Load competitions config ──
  competitions <- read_yaml(file.path(odds_dir, "config", "competitions.yml"))

  # ── 2. Find pending bets across all leagues ──
  cli_h2("Loading pending bets")
  pending <- load_pending_bets(sports_dir, leagues)

  if (nrow(pending) == 0) {
    cli_alert_info("No pending bets found.")
    return(invisible(tibble::tibble()))
  }

  cli_alert_info("Found {nrow(pending)} pending bet(s) across {length(unique(pending$league_key))} league(s).")

  # ── 3. Print summary ──
  for (lk in unique(pending$league_key)) {
    league_bets <- filter(pending, league_key == lk)
    cli_alert_info("  {lk}: {nrow(league_bets)} bet(s)")
    for (i in seq_len(nrow(league_bets))) {
      b <- league_bets[i, ]
      cli_alert_info(
        "    {b$home} vs {b$away}: {b$market} {b$outcome} @ {b$odds} -> {b$bet_amount} kr"
      )
    }
  }

  # ── 4. Confirm ──
  if (interactive && !dry_run) {
    answer <- readline("Place these bets? (y/n): ")
    if (!tolower(answer) %in% c("y", "yes")) {
      cli_alert_info("Cancelled by user.")
      return(invisible(pending))
    }
  }

  # ── 5. Login (always visible so user can see the browser) ──
  cli_h2("Logging in to Lengjan")
  session <- lengjan_login(headless = FALSE)
  on.exit(tryCatch(session$close(), error = function(e) NULL))

  # ── 6. Process each league ──
  results <- tibble::tibble()

  for (lk in unique(pending$league_key)) {
    cli_h2("Processing: {lk}")
    league_bets <- filter(pending, league_key == lk)
    comp_config <- competitions[[lk]]

    if (is.null(comp_config)) {
      cli_alert_warning("No competition config for {lk} — skipping.")
      next
    }

    # Resolve match IDs for this league
    match_ids <- resolve_match_ids(
      session, comp_config, league_bets, odds_dir, lk
    )

    # Place each bet
    for (i in seq_len(nrow(league_bets))) {
      bet <- league_bets[i, ]
      mid <- match_ids[[paste(bet$home, bet$away, sep = " - ")]]

      if (is.null(mid)) {
        cli_alert_warning(
          "Could not find match ID for {bet$home} vs {bet$away} — skipping."
        )
        results <- bind_rows(results, mutate(bet, placement_status = "no_match_id"))
        next
      }

      if (interactive && !dry_run) {
        cat(sprintf(
          "\n  -> %s vs %s: %s %s @ %.2f -> %d kr\n",
          bet$home, bet$away, bet$market, bet$outcome, bet$odds, bet$bet_amount
        ))
        answer <- readline("    Place this bet? (y/n/q): ")
        if (tolower(answer) == "q") {
          cli_alert_info("Stopped by user.")
          break
        }
        if (!tolower(answer) %in% c("y", "yes")) {
          results <- bind_rows(results, mutate(bet, placement_status = "skipped_by_user"))
          next
        }
      }

      result <- place_bet(
        session = session,
        bet = bet,
        match_id = mid,
        sport_id = comp_config$sport,
        dry_run = dry_run
      )

      results <- bind_rows(
        results,
        mutate(bet, placement_status = result$status)
      )

      Sys.sleep(stats::runif(1, 2, 4))
    }
  }

  # ── 7. Summary ──
  cli_rule()
  cli_h2("Summary")
  if (nrow(results) > 0) {
    for (s in unique(results$placement_status)) {
      n <- sum(results$placement_status == s)
      cli_alert_info("  {s}: {n}")
    }
  }

  invisible(results)
}

# ── Helpers ───────────────────────────────────────────────────────────────────

#' Load pending bets from bets_log.csv files
#'
#' Filters for pipeline bets that haven't been placed yet (no placed_at timestamp)
#' and whose matches haven't started.
load_pending_bets <- function(sports_dir, leagues = NULL) {
  log_files <- list.files(
    sports_dir,
    pattern = "bets_log\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )

  all_bets <- map_dfr(log_files, function(f) {
    parts <- strsplit(f, "/")[[1]]
    history_idx <- which(parts == "history")
    if (length(history_idx) == 0) return(tibble::tibble())

    sport <- parts[history_idx - 2]
    country <- parts[history_idx - 1]
    league_key <- paste(sport, country, sep = "_")

    tryCatch(
      {
        read_csv(f, show_col_types = FALSE) |>
          mutate(
            league_key = league_key,
            bets_log_path = f,
            info = as.character(info)
          )
      },
      error = function(e) tibble::tibble()
    )
  })

  if (nrow(all_bets) == 0) return(tibble::tibble())

  # Filter for pending pipeline bets
  pending <- all_bets |>
    filter(
      source == "pipeline",
      is.na(win),
      date_match >= Sys.Date()
    )

  # If placed_at column exists, exclude already-placed bets
  if ("placed_at" %in% names(pending)) {
    pending <- filter(pending, is.na(placed_at))
  }

  if (!is.null(leagues)) {
    pending <- filter(pending, league_key %in% leagues)
  }

  pending
}

#' Resolve Lengjan match IDs for a set of bets
resolve_match_ids <- function(session, comp_config, bets, odds_dir, league_key) {

  # Load team name mapping
  # CSV columns: "out" = pipeline name, "in" = Lengjan name
  team_names_file <- file.path(
    odds_dir, "config", comp_config$team_names %||% ""
  )

  name_map <- if (file.exists(team_names_file)) {
    read_csv(team_names_file, show_col_types = FALSE)
  } else {
    cli_alert_warning("No team names file for {league_key}.")
    tibble::tibble(out = character(), `in` = character())
  }

  # Lengjan name -> pipeline name
  lengjan_to_pipeline <- stats::setNames(name_map$out, name_map$`in`)

  all_match_ids <- list()

  for (league_name in names(comp_config$leagues)) {
    comp_id <- comp_config$leagues[[league_name]]$competition
    matches <- extract_matches(session, comp_config$sport, comp_id)

    if (nrow(matches) == 0) next

    for (i in seq_len(nrow(matches))) {
      m <- matches[i, ]
      # Store with Lengjan names
      key_lengjan <- paste(m$home, m$away, sep = " - ")
      all_match_ids[[key_lengjan]] <- m$match_id

      # Also store with pipeline names
      pipeline_home <- lengjan_to_pipeline[m$home]
      pipeline_away <- lengjan_to_pipeline[m$away]
      if (!is.na(pipeline_home) && !is.na(pipeline_away)) {
        key_pipeline <- paste(pipeline_home, pipeline_away, sep = " - ")
        all_match_ids[[key_pipeline]] <- m$match_id
      }
    }
  }

  all_match_ids
}
