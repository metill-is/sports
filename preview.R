#!/usr/bin/env Rscript
#' Preview pending bets — what a live run would place
#'
#' Reads recommendations.csv, deduplicates against all bets_log.csv files,
#' and prints a summary grouped by date with totals.
#'
#' Usage:
#'   Rscript preview.R                # Show all pending bets
#'   Rscript preview.R --league football_england  # Filter to one league
#'   Rscript preview.R --today        # Only today's matches

library(readr)
library(dplyr, warn.conflicts = FALSE)
library(here)

here::i_am(".here")
sports_dir <- here("../Sports")

args <- commandArgs(trailingOnly = TRUE)
today_only <- "--today" %in% args
league_idx <- which(args == "--league")
league_filter <- if (length(league_idx) > 0 && league_idx < length(args)) {
  args[league_idx + 1]
} else {
  NULL
}

# ── 1. Load recommendations ─────────────────────────────────────────────────

recs_path <- file.path(sports_dir, "recommendations.csv")
if (!file.exists(recs_path)) {
  cat("No recommendations.csv found. Run the pipeline first.\n")
  quit(status = 1)
}

recs <- read_csv(recs_path, show_col_types = FALSE) |>
  filter(
    as.Date(date) >= Sys.Date(),
    if (today_only) as.Date(date) == Sys.Date() else TRUE
  ) |>
  mutate(
    league_key = paste(sport, country, sep = "_"),
    home = heima,
    away = gestir,
    info = case_when(
      market == "handicap" ~ as.character(change),
      market == "totals" ~ as.character(limit),
      TRUE ~ NA_character_
    )
  )

if (!is.null(league_filter)) {
  recs <- filter(recs, league_key %in% league_filter)
}

if (nrow(recs) == 0) {
  cat("No future recommendations found.\n")
  quit(status = 0)
}

# ── 2. Deduplicate against ledger ────────────────────────────────────────────

log_files <- list.files(
  sports_dir,
  pattern = "bets_log[.]csv$",
  recursive = TRUE,
  full.names = TRUE
)

if (length(log_files) > 0) {
  all_logs <- do.call(rbind, lapply(log_files, function(f) {
    tryCatch(
      read_csv(f, show_col_types = FALSE) |>
        mutate(info = replace(as.character(info), is.na(info), "")),
      error = function(e) tibble()
    )
  }))

  if (nrow(all_logs) > 0) {
    n_before <- nrow(recs)
    recs <- anti_join(
      recs |> mutate(.info_key = replace(as.character(info), is.na(info), "")),
      all_logs |> mutate(
        .info_key = replace(as.character(info), is.na(info), ""),
        date_match = as.Date(date_match)
      ),
      by = c("date" = "date_match", "home", "away", "market", "outcome", ".info_key")
    )
    recs$.info_key <- NULL
    n_deduped <- n_before - nrow(recs)
    if (n_deduped > 0) {
      cat(sprintf("(%d already-placed bet(s) filtered out)\n\n", n_deduped))
    }
  }
}

if (nrow(recs) == 0) {
  cat("All recommendations have already been placed.\n")
  quit(status = 0)
}

# ── 3. Compute display columns ──────────────────────────────────────────────

recs <- recs |>
  mutate(
    row_id = row_number(),
    profit = round(bet_amount * (o - 1) * p - bet_amount * (1 - p), 0)
  ) |>
  arrange(date, league_key, home)

# ── 4. Display ───────────────────────────────────────────────────────────────

header <- function() {
  cat(sprintf(
    "  %-4s %-14s %-7s %-22s %-10s %5s %5s %5s %7s %7s\n",
    "#", "League", "Market", "Match", "Bet", "Odds", "Prob", "EV", "Stake", "E[PnL]"
  ))
  cat("  ", strrep("\u2500", 94), "\n", sep = "")
}

row_fmt <- function(r) {
  match_str <- paste0(substr(r$home, 1, 10), " v ", substr(r$away, 1, 10))
  league_str <- paste0(r$sport, "/", r$country)
  bet_str <- if (r$market == "totals" && !is.na(r$limit)) {
    paste0(r$outcome, " ", r$limit)
  } else if (r$market == "handicap" && !is.na(r$change)) {
    paste0(r$outcome, " ", r$change)
  } else {
    r$outcome
  }
  cat(sprintf(
    "  %-4d %-14s %-7s %-22s %-10s %5.2f %4.0f%% %4.0f%% %6.0f %+6.0f\n",
    r$row_id, substr(league_str, 1, 14), substr(r$market, 1, 7),
    substr(match_str, 1, 22), substr(bet_str, 1, 10),
    r$o, r$p * 100, r$ev * 100, r$bet_amount, r$profit
  ))
}

subtotal <- function(d, label) {
  cat("  ", strrep("\u2500", 94), "\n", sep = "")
  cat(sprintf(
    "  %-62s %6s %7s %+7s\n",
    label, "", format(sum(d$bet_amount), big.mark = ","), format(sum(d$profit), big.mark = ",")
  ))
}

dates <- sort(unique(as.Date(recs$date)))
for (dt in dates) {
  day_recs <- recs |> filter(as.Date(date) == as.Date(dt))
  days_away <- as.integer(as.Date(dt) - Sys.Date())
  day_label <- if (days_away == 0) "TODAY"
               else if (days_away == 1) "TOMORROW"
               else format(as.Date(dt), "%A")

  cat(sprintf(
    "\n  %s \u2014 %s  (%d bets, %s kr)\n\n",
    format(as.Date(dt), "%a %d %b"), day_label,
    nrow(day_recs), format(sum(day_recs$bet_amount), big.mark = ",")
  ))
  header()
  for (i in seq_len(nrow(day_recs))) row_fmt(day_recs[i, ])
  subtotal(day_recs, day_label)
  cat("\n")
}

# Grand total
cat(sprintf(
  "\n  TOTAL: %d pending bets | %s kr stake | %+.0f kr expected\n\n",
  nrow(recs),
  format(sum(recs$bet_amount), big.mark = ","),
  sum(recs$profit)
))
