#' Lengjan Bet Placement Pipeline
#'
#' Reads recommendations from Sports/recommendations.csv, places bets on
#' Lengjan via chromote, and writes placed bets to per-league bets_log.csv.
#'
#' The pipeline is the ONLY writer to the ledger (bets_log.csv).
#' See: ~/Obsidian/Metill/Sports/betting-system-rules.md

box::use(
  dplyr[filter, mutate, bind_rows, select, case_when, anti_join],
  readr[read_csv, write_csv, read_file],
  purrr[map_dfr],
  cli[
    cli_alert_info, cli_alert_success, cli_alert_warning,
    cli_alert_danger, cli_abort, cli_h1, cli_h2, cli_rule
  ],
  yaml[yaml.load],
  here[here]
)

box::use(
  . / login[lengjan_login, is_authenticated],
  . / navigate[extract_matches, competition_url, match_url],
  . / place_bet[place_bet, clear_bet_slip],
  . / validate[validate_team_names_config]
)

# ── Main pipeline ─────────────────────────────────────────────────────────────

#' Run the bet placement pipeline
#'
#' Reads recommendations, places them on Lengjan, and logs to the ledger.
#'
#' @param leagues Character vector of league keys (e.g., "football_england").
#'   NULL = all leagues with pending recommendations.
#' @param dry_run If TRUE, navigate and select odds but don't click "Kaupa"
#' @param interactive If TRUE, prompt for confirmation before each bet
#' @param headless If TRUE (default), run Chrome headlessly. Pass FALSE to
#'   watch the automation click through the site (useful for debugging).
#' @param sports_dir Path to the Sports project root
#' @param odds_dir Path to the lengjan-odds project root
#' @return A tibble summarising all bet placement results
run_bets <- function(
  leagues = NULL,
  dry_run = TRUE,
  interactive = TRUE,
  today_only = FALSE,
  target_date = NULL,
  headless = TRUE,
  sports_dir = here::here("../Sports"),
  odds_dir = here::here("../lengjan-odds")
) {
  cli_h1("Lengjan Bet Placement Pipeline")

  if (dry_run) {
    cli_alert_warning("DRY RUN mode \u2014 no bets will be placed.")
  }

  # ── 0. Pre-flight: cross-check team_names_*.csv against competitions.yml ──
  # Fails fast on misconfigured leagues before Chrome ever launches.
  validate_team_names_config(odds_dir)

  # ── 1. Load competitions config ──
  # read_yaml() corrupts Icelandic UTF-8 — use readr + yaml.load instead
  competitions <- yaml.load(read_file(file.path(odds_dir, "config", "competitions.yml")))

  # ── 2. Load recommendations (from Sports pipeline output) ──
  cli_h2("Loading recommendations")
  pending <- load_recommendations(sports_dir, leagues, today_only, target_date)

  if (nrow(pending) == 0) {
    cli_alert_info("No pending recommendations found.")
    return(invisible(tibble::tibble()))
  }

  cli_alert_info("Found {nrow(pending)} recommendation(s) across {length(unique(pending$league_key))} league(s).")

  # ── 3. Compute bankroll (display only — P3 uses proportional scaling) ──
  bankroll <- compute_placement_bankroll(sports_dir)
  cli_alert_info("Current bankroll: {bankroll} kr")

  # ── 4. Print summary ──
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

  # ── 5. Confirm ──
  if (interactive && !dry_run) {
    answer <- readline("Place these bets? (y/n): ")
    if (!tolower(answer) %in% c("y", "yes")) {
      cli_alert_info("Cancelled by user.")
      return(invisible(pending))
    }
  }

  # ── 6. Login (headless by default; pass --show-browser to watch) ──
  cli_h2("Logging in to Lengjan")
  session <- lengjan_login(headless = headless)
  on.exit(tryCatch(session$close(), error = function(e) NULL))

  # ── 7. Process each league ──
  results <- tibble::tibble()

  for (lk in unique(pending$league_key)) {
    cli_h2("Processing: {lk}")
    league_bets <- filter(pending, league_key == lk)
    comp_config <- competitions[[lk]]

    if (is.null(comp_config)) {
      cli_alert_warning("No competition config for {lk} \u2014 skipping.")
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
          "Could not find match ID for {bet$home} vs {bet$away} \u2014 skipping."
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

      # L1: Write to ledger ONLY after confirmed placement
      if (result$status == "placed") {
        log_placed_bet(
          bet = bet,
          actual_odds = result$actual_odds %||% bet$odds,
          actual_amount = result$amount %||% bet$bet_amount,
          sports_dir = sports_dir
        )
      }

      results <- bind_rows(
        results,
        mutate(bet, placement_status = result$status)
      )

      # Safety: always clear the bet slip before the next bet to prevent compounds
      tryCatch(clear_bet_slip(session), error = function(e) NULL)
      Sys.sleep(stats::runif(1, 2, 4))
    }
  }

  # ── 8. Summary ──
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

# ── Data loading ──────────────────────────────────────────────────────────────

#' Load recommendations from Sports/recommendations.csv
#'
#' Reads the ephemeral recommendations file, maps schema to placement format,
#' and deduplicates against the ledger (already-placed bets).
#'
#' @param sports_dir Path to Sports/ root
#' @param leagues Optional character vector of league keys to filter
#' @return Tibble ready for placement
load_recommendations <- function(sports_dir, leagues = NULL, today_only = FALSE, target_date = NULL) {
  recs_path <- file.path(sports_dir, "recommendations.csv")

  if (!file.exists(recs_path)) {
    cli_alert_warning("No recommendations.csv found at {recs_path}")
    return(tibble::tibble())
  }

  recs <- read_csv(recs_path, show_col_types = FALSE) |>
    filter(
      as.Date(date) >= Sys.Date(),
      if (!is.null(target_date)) {
        as.Date(date) == target_date
      } else if (today_only) {
        as.Date(date) == Sys.Date()
      } else {
        TRUE
      }
    ) |>
    mutate(
      league_key = paste(sport, country, sep = "_"),
      home = heima,
      away = gestir,
      odds = o,
      probability = p,
      kelly_frac = kelly,
      date_match = as.Date(date),
      info = case_when(
        market == "handicap" ~ as.character(change),
        market == "totals" ~ as.character(limit),
        TRUE ~ NA_character_
      )
    )

  if (!is.null(leagues)) {
    recs <- filter(recs, league_key %in% leagues)
  }

  if (nrow(recs) == 0) {
    return(tibble::tibble())
  }

  # P1: Deduplicate against ledger (already-placed bets)
  dedup_against_ledger(recs, sports_dir)
}

#' Remove recommendations already in the ledger
#'
#' Rule P1: Placement is idempotent — re-running must not double-place.
dedup_against_ledger <- function(recs, sports_dir) {
  if (nrow(recs) == 0) {
    return(recs)
  }

  log_files <- list.files(
    sports_dir,
    pattern = "bets_log[.]csv$",
    recursive = TRUE,
    full.names = TRUE
  )
  if (length(log_files) == 0) {
    return(recs)
  }

  all_logs <- map_dfr(log_files, function(f) {
    tryCatch(
      read_csv(f, show_col_types = FALSE) |>
        mutate(info = replace(as.character(info), is.na(info), "")),
      error = function(e) tibble::tibble()
    )
  })

  if (nrow(all_logs) == 0) {
    return(recs)
  }

  recs_dedup <- recs |>
    mutate(.info_key = replace(as.character(info), is.na(info), ""))

  out <- anti_join(
    recs_dedup,
    all_logs |> mutate(.info_key = replace(as.character(info), is.na(info), "")),
    by = c(
      "date_match" = "date_match", "home", "away",
      "market", "outcome", ".info_key"
    )
  )
  out$.info_key <- NULL

  n_removed <- nrow(recs) - nrow(out)
  if (n_removed > 0) {
    cli_alert_info("  ({n_removed} already-placed bet(s) filtered out)")
  }

  out
}

# ── Bankroll ──────────────────────────────────────────────────────────────────

#' Compute current bankroll for Kelly recalculation at live odds
#'
#' Rule B2: bankroll = initial_pool + settled_pnl - unsettled_exposure
compute_placement_bankroll <- function(sports_dir) {
  bankroll_yml <- file.path(sports_dir, "config", "bankroll.yml")
  if (!file.exists(bankroll_yml)) {
    cli_alert_warning("No bankroll.yml found — using default 5000")
    return(5000)
  }

  cfg <- yaml.load(read_file(bankroll_yml))
  initial_pool <- cfg$initial_pool %||% 5000
  epoch <- cfg$epoch

  # Load all ledger files
  logs <- Sys.glob(file.path(sports_dir, "*", "*", "history", "bets_log.csv"))
  if (length(logs) == 0) {
    return(initial_pool)
  }

  all_bets <- do.call(rbind, lapply(logs, \(f) {
    read_csv(f, show_col_types = FALSE)
  })) |> filter(sex != "all")

  if (!is.null(epoch)) {
    all_bets <- all_bets |> filter(as.Date(date_recommended) >= as.Date(epoch))
  }

  if (nrow(all_bets) == 0) {
    return(initial_pool)
  }

  settled_pnl <- sum(all_bets$pnl[!is.na(all_bets$pnl)], na.rm = TRUE)
  outstanding <- sum(all_bets$bet_amount[is.na(all_bets$win)], na.rm = TRUE)

  initial_pool + settled_pnl - outstanding
}

# ── Ledger write ──────────────────────────────────────────────────────────────

#' Write a placed bet to the per-league bets_log.csv
#'
#' L1: Creates a NEW row — this is the only path that writes to the ledger.
#' P2: Records actual Lengjan odds, not recommended odds.
#'
#' @param bet Recommendation row (from load_recommendations)
#' @param actual_odds The odds confirmed from Lengjan
#' @param actual_amount The stake actually entered
#' @param sports_dir Path to Sports/ root
log_placed_bet <- function(bet, actual_odds, actual_amount, sports_dir) {
  history_dir <- file.path(sports_dir, bet$sport, bet$country, "history")
  if (!dir.exists(history_dir)) dir.create(history_dir, recursive = TRUE)

  log_path <- file.path(history_dir, "bets_log.csv")

  # Recalculate EV at actual odds (P2)
  actual_ev <- round(bet$probability * (actual_odds - 1) - (1 - bet$probability), 4)

  log_row <- tibble::tibble(
    date_recommended = Sys.Date(),
    date_match       = bet$date_match,
    sport            = bet$sport,
    country          = bet$country,
    sex              = bet$sex,
    market           = bet$market,
    home             = bet$home,
    away             = bet$away,
    outcome          = bet$outcome,
    odds             = actual_odds,
    probability      = bet$probability,
    ev               = actual_ev,
    kelly_frac       = bet$kelly_frac,
    bet_amount       = actual_amount,
    info             = bet$info %||% NA_character_,
    win              = NA,
    pnl              = NA_real_,
    source           = "pipeline"
  )

  # Append (write header only if file doesn't exist)
  write_csv(
    log_row, log_path,
    append = file.exists(log_path),
    col_names = !file.exists(log_path)
  )

  cli_alert_success("Logged to {basename(log_path)}: {bet$home} v {bet$away} [{bet$market} {bet$outcome}] @ {actual_odds}")

  # Dual-write to Parquet store
  tryCatch(
    {
      store_script <- file.path(sports_dir, "R", "storage", "store.R")
      if (file.exists(store_script)) {
        env <- new.env(parent = baseenv())
        source(store_script, local = env)
        full_log <- read_csv(log_path, show_col_types = FALSE)
        env$store_bets(full_log, bet$sport, bet$country, bet$sex, sports_dir)
      }
    },
    error = function(e) {
      cli_alert_warning("Parquet store sync failed: {e$message}")
    }
  )

  invisible(log_row)
}

# ── Match ID resolution ──────────────────────────────────────────────────────

#' Resolve Lengjan match IDs for a set of bets
resolve_match_ids <- function(session, comp_config, bets, odds_dir, league_key) {
  # Load team name mapping
  # CSV columns: "pipeline" = pipeline name, "lengjan" = Lengjan name
  team_names_file <- if (!is.null(comp_config$team_names)) {
    file.path(odds_dir, "config", comp_config$team_names)
  } else {
    NULL
  }

  # Defence-in-depth: pre-flight validator (validate_team_names_config)
  # should have caught this, but also abort here for callers that bypass run.R.
  # If a CSV matching this league_key exists on disk but the comp entry has no
  # team_names: wiring, that's a misconfiguration — fail loud.
  conventional_csv <- file.path(odds_dir, "config", paste0("team_names_", league_key, ".csv"))
  if (is.null(team_names_file) && file.exists(conventional_csv)) {
    cli_abort(c(
      "Misconfigured team_names for '{league_key}':",
      "x" = "File '{basename(conventional_csv)}' exists but competitions.yml['{league_key}'] has no 'team_names:' key.",
      "i" = "Add `team_names: \"{basename(conventional_csv)}\"` under the '{league_key}' entry in lengjan-odds/config/competitions.yml."
    ))
  }

  name_map <- if (!is.null(team_names_file) && file.exists(team_names_file)) {
    read_csv(team_names_file, show_col_types = FALSE)
  } else {
    cli_alert_warning("No team names file for {league_key} (no CSV on disk — this is only OK if DOM names match pipeline names exactly).")
    tibble::tibble(pipeline = character(), lengjan = character())
  }

  # Support both old (out/in) and new (pipeline/lengjan) column names
  lengjan_col <- if ("lengjan" %in% names(name_map)) "lengjan" else "in"
  pipeline_col <- if ("pipeline" %in% names(name_map)) "pipeline" else "out"

  # Lengjan name -> pipeline name
  lengjan_to_pipeline <- stats::setNames(name_map[[pipeline_col]], name_map[[lengjan_col]])

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

      # Also store with pipeline names (unmapped teams keep their Lengjan name)
      pipeline_home <- lengjan_to_pipeline[m$home]
      if (is.na(pipeline_home)) pipeline_home <- m$home
      pipeline_away <- lengjan_to_pipeline[m$away]
      if (is.na(pipeline_away)) pipeline_away <- m$away
      key_pipeline <- paste(pipeline_home, pipeline_away, sep = " - ")
      if (key_pipeline != key_lengjan) {
        all_match_ids[[key_pipeline]] <- m$match_id
      }
    }
  }

  all_match_ids
}
