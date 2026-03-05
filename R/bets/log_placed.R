#' Log specific placed bets from recommendations.csv
#'
#' Reads recommendations.csv, selects rows by index, and logs them
#' to per-league history/bets_log.csv + Parquet store.
#'
#' Usage:
#'   Rscript R/bets/log_placed.R 1,3,5      # Log bets #1, #3, #5
#'   Rscript R/bets/log_placed.R all         # Log all recommendations
#'   Rscript R/bets/log_placed.R --show      # Just display the table (no logging)

library(readr)
library(dplyr, warn.conflicts = FALSE)
library(here)

here::i_am(".here")
sports_dir <- here::here()

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  cat("Usage: Rscript R/bets/log_placed.R <indices|all|--show>\n")
  cat("  1,3,5   Log specific bets by row number\n")
  cat("  all     Log all recommendations\n")
  cat("  --show  Display recommendations without logging\n")
  quit(status = 1)
}

recs_path <- here("recommendations.csv")
if (!file.exists(recs_path)) {
  cat("No recommendations.csv found. Run the pipeline with --step bet first.\n")
  quit(status = 1)
}

recs <- read_csv(recs_path, show_col_types = FALSE) |>
  mutate(
    row_id = row_number(),
    profit = round(bet_amount * (o - 1) * p - bet_amount * (1 - p), 0)
  ) |>
  arrange(desc(profit))

# Display helpers
table_header <- function() {
  cat(sprintf("%-4s %-12s %-7s %-20s %-10s %5s %6s %6s %7s %7s\n",
    "#", "League", "Market", "Match", "Bet", "Odds", "Prob", "EV", "Amount", "E[PnL]"))
  cat(strrep("-", 96), "\n")
}

table_row <- function(r) {
  match_str <- paste0(substr(r$heima, 1, 9), " v ", substr(r$gestir, 1, 9))
  league_str <- paste0(r$sport, "/", r$country)
  info <- if (r$market == "totals" && !is.na(r$limit)) {
    paste0(r$outcome, " ", r$limit)
  } else if (r$market == "handicap" && !is.na(r$change)) {
    paste0(r$outcome, " ", r$change)
  } else {
    r$outcome
  }
  cat(sprintf("%-4d %-12s %-7s %-20s %-10s %5.2f %5.1f%% %5.1f%% %6.0f %+6.0f\n",
    r$row_id, substr(league_str, 1, 12), substr(r$market, 1, 7),
    substr(match_str, 1, 20), substr(info, 1, 10),
    r$o, r$p * 100, r$ev * 100, r$bet_amount, r$profit))
}

table_total <- function(d) {
  cat(strrep("-", 96), "\n")
  cat(sprintf("%-71s %6.0f %+6.0f\n", "TOTAL",
    sum(d$bet_amount), sum(d$profit)))
}

# Display grouped by date, sorted by profit within each day
dates <- sort(unique(as.Date(recs$date)))
for (dt in dates) {
  day_recs <- recs |> filter(as.Date(date) == as.Date(dt))
  days_away <- as.integer(as.Date(dt) - Sys.Date())
  day_label <- if (days_away == 0) "TODAY"
               else if (days_away == 1) "TOMORROW"
               else format(as.Date(dt), "%A")
  cat(sprintf("\n=== %s — %s (%d bets, %s kr stake) ===\n",
    format(as.Date(dt), "%a %d %b"), day_label,
    nrow(day_recs), format(sum(day_recs$bet_amount), big.mark = ",")))
  table_header()
  for (i in seq_len(nrow(day_recs))) table_row(day_recs[i, ])
  table_total(day_recs)
  cat("\n")
}

# Grand total
if (length(dates) > 1) {
  cat(sprintf("=== ALL DAYS: %d bets, %s kr stake, %+.0f kr expected ===\n\n",
    nrow(recs), format(sum(recs$bet_amount), big.mark = ","), sum(recs$profit)))
}

if ("--show" %in% args) quit(status = 0)

# Parse indices
if ("all" %in% args) {
  selected_ids <- recs$row_id
} else {
  selected_ids <- as.integer(unlist(strsplit(args[!args %in% c("--show")], ",")))
  bad <- selected_ids[!selected_ids %in% recs$row_id]
  if (length(bad) > 0) {
    cat("Invalid row numbers:", paste(bad, collapse = ", "), "\n")
    quit(status = 1)
  }
}

placed <- recs |> filter(row_id %in% selected_ids)
cat("Logging", nrow(placed), "bet(s)...\n\n")

# Log each bet to its league's history
source(file.path(sports_dir, "R", "storage", "store.R"), local = TRUE)

for (i in seq_len(nrow(placed))) {
  r <- placed[i, ]
  league_dir <- file.path(sports_dir, r$sport, r$country)
  history_dir <- file.path(league_dir, "history")
  if (!dir.exists(history_dir)) dir.create(history_dir, recursive = TRUE)

  log_path <- file.path(history_dir, "bets_log.csv")

  info <- if (r$market == "totals" && !is.na(r$limit)) as.character(r$limit)
          else if (r$market == "handicap" && !is.na(r$change)) as.character(r$change)
          else NA_character_

  log_row <- tibble(
    date_recommended = Sys.Date(),
    date_match       = as.Date(r$date),
    sport            = r$sport,
    country          = r$country,
    sex              = r$sex,
    market           = r$market,
    home             = r$heima,
    away             = r$gestir,
    outcome          = r$outcome,
    odds             = r$o,
    probability      = r$p,
    ev               = r$ev,
    kelly_frac       = r$kelly,
    bet_amount       = r$bet_amount,
    info             = info,
    win              = NA,
    pnl              = NA_real_,
    source           = "pipeline"
  )

  # Dedup against existing log
  if (file.exists(log_path)) {
    existing <- read_csv(log_path, show_col_types = FALSE) |>
      mutate(date_match = as.Date(date_match), info = as.character(info))
    dedup_keys <- c("date_match", "sport", "country", "sex", "market",
                    "home", "away", "outcome", "info")
    dup <- nrow(semi_join(log_row |> mutate(info = replace(info, is.na(info), "")),
                          existing |> mutate(info = replace(info, is.na(info), "")),
                          by = dedup_keys))
    if (dup > 0) {
      cat("  Already logged:", r$heima, "v", r$gestir, r$market, r$outcome, "\n")
      next
    }
  }

  write_csv(log_row, log_path,
            append = file.exists(log_path),
            col_names = !file.exists(log_path))

  # Dual-write to Parquet store
  tryCatch({
    full_log <- read_csv(log_path, show_col_types = FALSE)
    store_bets(full_log, r$sport, r$country, r$sex, sports_dir)
  }, error = function(e) warning("Store write failed: ", e$message))

  cat("  Logged:", r$heima, "v", r$gestir, "(", r$market, r$outcome, ")",
      r$bet_amount, "kr @", r$o, "\n")
}

cat("\nDone.\n")
