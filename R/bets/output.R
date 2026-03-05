#' Output utilities: display, clipboard, deduplication, and history logging
#'
#' Handles the display pivot (market-agnostic via pivot_wider on outcome),
#' clipboard formatting for GSheets paste, optional deduplication
#' against existing bets, and bet history logging for future analysis.

box::use(
  dplyr[select, mutate, filter, case_when, bind_rows, anti_join, distinct, all_of],
  tidyr[pivot_wider],
  readr[read_csv, write_csv],
  clipr[write_clip]
)

#' Compute current bankroll from initial pool and pipeline-era bet history
#'
#' Only counts bets from the pipeline era (source = "pipeline", since
#' bankroll_since date). Historical GSheets bets on other bookers are excluded.
#'
#' @param initial_pool Starting bankroll amount
#' @param sports_dir Absolute path to Sports/ root
#' @param since Only count bets recommended on or after this date (default: 2026-03-01)
#' @return Current available bankroll
#' @export
compute_bankroll <- function(initial_pool, sports_dir, since = "2026-03-01") {
  since_date <- as.Date(since)

  filter_bets <- function(bets) {
    bets |>
      dplyr::filter(
        sex != "all",
        source == "pipeline",
        as.Date(date_recommended) >= since_date
      )
  }

  # Try Parquet store first (faster for many leagues)
  store_path <- file.path(sports_dir, "store", "bets")
  if (dir.exists(store_path) && requireNamespace("arrow", quietly = TRUE)) {
    result <- tryCatch({
      all_bets <- arrow::open_dataset(store_path) |>
        dplyr::collect() |>
        filter_bets()
      if (nrow(all_bets) > 0) {
        settled_pnl <- sum(all_bets$pnl[!is.na(all_bets$pnl)], na.rm = TRUE)
        outstanding <- sum(all_bets$bet_amount[is.na(all_bets$win)], na.rm = TRUE)
        initial_pool + settled_pnl - outstanding
      } else {
        NULL
      }
    }, error = function(e) NULL)
    if (!is.null(result)) return(result)
  }

  # Fall back to CSV glob
  logs <- Sys.glob(file.path(sports_dir, "*", "*", "history", "bets_log.csv"))
  if (length(logs) == 0) return(initial_pool)

  all_bets <- do.call(rbind, lapply(logs, \(f) {
    read_csv(f, show_col_types = FALSE)
  })) |> filter_bets()

  if (nrow(all_bets) == 0) return(initial_pool)

  settled_pnl <- sum(all_bets$pnl[!is.na(all_bets$pnl)], na.rm = TRUE)
  outstanding <- sum(all_bets$bet_amount[is.na(all_bets$win)], na.rm = TRUE)

  initial_pool + settled_pnl - outstanding
}

# Extract info column value, or empty string if missing
resolve_info <- function(results, info_col) {
  if (!is.null(info_col) && info_col %in% names(results)) {
    as.character(results[[info_col]])
  } else {
    ""
  }
}

#' Print a market's results in wide (display) format
#'
#' Pivots the outcome column into separate columns (e.g., home/tie/away).
#' Extra columns like `change` or `limit` are preserved automatically.
#'
#' @param results Tibble from a market module (long form with outcome column)
#' @param market_name Display label (e.g., "1x2", "Handicap", "Totals")
#' @export
print_market <- function(results, market_name) {
  cat("\n===", market_name, "===\n\n")
  if (is.null(results) || nrow(results) == 0) {
    cat("  No value bets found.\n")
    return(invisible(NULL))
  }

  # Keep only display-relevant columns before pivoting
  keep_cols <- intersect(
    names(results),
    c("date", "division", "league", "booker", "heima", "gestir", "change", "limit",
      "outcome", "text")
  )

  display <- results |>
    select(all_of(keep_cols)) |>
    pivot_wider(
      names_from = outcome,
      values_from = text,
      values_fill = ""
    )

  print(display, n = Inf)
  invisible(display)
}

#' Format results as clipboard rows for GSheets
#'
#' Produces uniform columns: dags_bet, type, dags_leikur, heima, gestir,
#' bet, info, amount, odds, prob.
#'
#' @param results Tibble from a market module
#' @param market_type Icelandic type label: "Niourstaoa", "Forgjof", "Markafjoeldi"
#' @param info_col Column name for the info field (NULL for 1x2)
#' @export
make_clipboard_rows <- function(results, market_type, info_col = NULL) {
  if (is.null(results) || nrow(results) == 0) return(NULL)

  results$info <- resolve_info(results, info_col)

  results |>
    mutate(
      dags_bet = Sys.Date(),
      type = market_type,
      bet = case_when(
        outcome == "home" ~ "heima",
        outcome == "away" ~ "gestir",
        outcome == "tie" ~ "jafntefli",
        outcome == "over" ~ "yfir",
        outcome == "under" ~ "undir"
      )
    ) |>
    select(
      dags_bet,
      type,
      dags_leikur = date,
      heima,
      gestir,
      bet,
      info,
      amount = bet_amount,
      odds = o,
      prob = p
    )
}

#' Write combined clipboard data
#'
#' @param d Combined tibble of clipboard rows
#' @export
write_to_clipboard <- function(d) {
  if (is.null(d) || nrow(d) == 0) {
    cat("\nNo bets to copy.\n")
    return(invisible(NULL))
  }
  write_clip(d, col.names = FALSE, allow_non_interactive = TRUE)
  cat("\nCopied", nrow(d), "bet(s) to clipboard.\n")
  invisible(d)
}

#' Load existing bets from GSheets for deduplication
#'
#' Only called when cfg$deduplication$use_gsheets is TRUE.
#' Uses googlesheets4 via :: to avoid requiring the package when disabled.
#'
#' @param cfg Config list
#' @return Tibble with date, heima, gestir, tegund columns, or NULL
#' @export
load_existing_bets <- function(cfg) {
  if (!cfg$deduplication$use_gsheets) return(NULL)

  if (!requireNamespace("googlesheets4", quietly = TRUE)) {
    stop("googlesheets4 must be installed to use GSheets deduplication.")
  }

  googlesheets4::gs4_auth(email = Sys.getenv("GOOGLE_MAIL"))
  googlesheets4::read_sheet(
    cfg$deduplication$gsheets_url,
    sheet = cfg$deduplication$gsheets_sheet
  ) |>
    select(date = dags_leikur, heima, gestir, tegund = type) |>
    mutate(date = as.Date(date)) |>
    filter(date >= Sys.Date())
}

#' Remove already-placed bets from results
#'
#' @param results Tibble from a market module
#' @param existing_bets Tibble from load_existing_bets (or NULL)
#' @param market_type Icelandic type label for filtering existing bets
#' @return Deduplicated results
#' @export
dedup <- function(results, existing_bets, market_type) {
  if (is.null(results) || is.null(existing_bets) || nrow(existing_bets) == 0) {
    return(results)
  }
  type_bets <- existing_bets |> filter(tegund == market_type)
  if (nrow(type_bets) == 0) return(results)
  results |> anti_join(type_bets, by = c("date", "heima", "gestir"))
}

#' Remove bets already recorded in bets_log.csv
#'
#' Prevents re-recommending (and re-logging) bets that were logged in a
#' previous pipeline run. Uses the same dedup keys as log_bets().
#'
#' @param results Tibble from a market module (or NULL)
#' @param cfg Config list (from bets.yml)
#' @param sport_dir Root directory of the sport project
#' @param sex Sex label
#' @param market Market label ("outcome", "handicap", "totals")
#' @param info_col Column in results holding the line info (e.g., "change", "limit")
#' @return Filtered results with already-logged bets removed
#' @export
dedup_against_log <- function(results, cfg, sport_dir, sex, market, info_col = NULL) {
  if (is.null(results) || nrow(results) == 0) return(results)
  if (!isTRUE(cfg$history$enabled)) return(results)

  log_path <- file.path(sport_dir, cfg$history$path, "bets_log.csv")
  if (!file.exists(log_path)) return(results)

  existing <- read_csv(log_path, show_col_types = FALSE) |>
    filter(market == !!market, sex == !!sex) |>
    mutate(info = replace(as.character(info), is.na(info), ""))

  if (nrow(existing) == 0) return(results)

  # Build info key from results to match against CSV's info column
  if (!is.null(info_col) && info_col %in% names(results)) {
    info_vals <- as.character(results[[info_col]])
  } else {
    info_vals <- rep(NA_character_, nrow(results))
  }
  info_vals <- replace(info_vals, is.na(info_vals), "")

  results$.dedup_info <- info_vals
  out <- anti_join(
    results, existing,
    by = c("date" = "date_match", "heima" = "home", "gestir" = "away",
           "outcome", ".dedup_info" = "info")
  )
  out$.dedup_info <- NULL

  n_removed <- nrow(results) - nrow(out)
  if (n_removed > 0) {
    cat("  (", n_removed, "already-logged", market, "bet(s) filtered out)\n")
  }

  out
}

#' Log bet recommendations for future kelly_frac analysis
#'
#' Appends recommended bets to a CSV history file. Each run appends new rows
#' without removing old ones, building a complete record over time.
#'
#' @param results Tibble from a market module (or NULL)
#' @param cfg Config list (from bets.yml)
#' @param sport_dir Root directory of the sport project
#' @param sex Sex label (e.g., "male", "female")
#' @param market Market label (e.g., "outcome", "handicap", "totals")
#' @param info_col Column name for extra info (e.g., "change", "limit"), or NULL
#' @export
log_bets <- function(results, cfg, sport_dir, sex, market, info_col = NULL, sports_dir = NULL) {
  if (!isTRUE(cfg$history$enabled)) return(invisible(NULL))
  if (is.null(results) || nrow(results) == 0) return(invisible(NULL))

  history_dir <- file.path(sport_dir, cfg$history$path)
  if (!dir.exists(history_dir)) dir.create(history_dir, recursive = TRUE)

  log_path <- file.path(history_dir, "bets_log.csv")

  info <- resolve_info(results, info_col)

  log_rows <- results |>
    mutate(
      date_recommended = Sys.Date(),
      sport = cfg$sport,
      country = cfg$country,
      sex = sex,
      market = market,
      info = info
    ) |>
    select(
      date_recommended,
      date_match = date,
      sport,
      country,
      sex,
      market,
      home = heima,
      away = gestir,
      outcome,
      odds = o,
      probability = p,
      ev,
      kelly_frac = kelly,
      bet_amount,
      info
    ) |>
    mutate(
      win = NA,
      pnl = NA_real_
    )

  # Deduplicate against existing log before appending
  dedup_keys <- c("date_match", "sport", "country", "sex", "market",
                  "home", "away", "outcome", "info")
  if (file.exists(log_path)) {
    existing <- read_csv(log_path, show_col_types = FALSE) |>
      mutate(
        date_match = as.Date(date_match),
        info = replace(as.character(info), is.na(info), "")
      )
    log_rows <- log_rows |>
      mutate(info = replace(info, is.na(info), "")) |>
      anti_join(existing, by = dedup_keys)
  }

  if (nrow(log_rows) == 0) {
    cat("  No new", market, "bet(s) to log (already recorded).\n")
    return(invisible(NULL))
  }

  # Append (write header only if file doesn't exist)
  write_csv(
    log_rows,
    log_path,
    append = file.exists(log_path),
    col_names = !file.exists(log_path)
  )

  cat("  Logged", nrow(log_rows), market, "bet(s) to", log_path, "\n")

  # Dual-write full log to Parquet store
  if (!is.null(sports_dir)) {
    tryCatch({
      source(file.path(sports_dir, "R", "storage", "store.R"), local = TRUE)
      full_log <- read_csv(log_path, show_col_types = FALSE)
      store_bets(full_log, cfg$sport, cfg$country, sex, sports_dir)
    }, error = function(e) warning("Store bet write failed: ", e$message))
  }

  invisible(log_rows)
}
