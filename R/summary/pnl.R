#' Cross-league PnL summary using Parquet store
#'
#' Reads all bets from the centralised store and produces a summary table
#' broken down by sport/country and overall.
#'
#' Usage:
#'   Rscript R/summary/pnl.R
#'   Rscript R/summary/pnl.R --settled    # Only show settled bets

library(dplyr, warn.conflicts = FALSE)
library(arrow, warn.conflicts = FALSE)

sports_dir <- here::here()
source(file.path(sports_dir, "R", "storage", "store.R"))

args <- commandArgs(trailingOnly = TRUE)
settled_only <- "--settled" %in% args

# Read all bets, excluding sex=all duplicates
bets <- read_bets(sports_dir) |>
  filter(sex != "all")

if (is.null(bets) || nrow(bets) == 0) {
  cat("No bets found in store.\n")
  quit(save = "no")
}

# Parse columns
bets <- bets |>
  mutate(
    settled = !is.na(win),
    won = !is.na(win) & win == TRUE,
    pnl_actual = case_when(
      !is.na(win) & win == TRUE  ~ bet_amount * (odds - 1),
      !is.na(win) & win == FALSE ~ -bet_amount,
      TRUE ~ NA_real_
    )
  )

if (settled_only) bets <- filter(bets, settled)

# Per-league summary
summarise_bets <- function(df) {
  n_settled <- sum(df$settled)
  n_wins <- sum(df$won, na.rm = TRUE)
  wagered_settled <- sum(df$bet_amount[df$settled], na.rm = TRUE)
  total_pnl <- sum(df$pnl_actual, na.rm = TRUE)
  tibble::tibble(
    total_bets = nrow(df),
    settled = n_settled,
    wins = n_wins,
    losses = n_settled - n_wins,
    pending = nrow(df) - n_settled,
    win_pct = if (n_settled > 0) round(n_wins / n_settled * 100, 1) else NA_real_,
    total_wagered = sum(df$bet_amount),
    outstanding = sum(df$bet_amount[!df$settled], na.rm = TRUE),
    pnl = total_pnl,
    roi = if (wagered_settled > 0) round(total_pnl / wagered_settled * 100, 1) else NA_real_,
    avg_odds = round(mean(df$odds), 2),
    avg_ev = round(mean(df$ev) * 100, 1)
  )
}

league_summary <- bets |>
  group_by(sport, country) |>
  group_modify(~ summarise_bets(.x)) |>
  ungroup() |>
  arrange(sport, country)

overall <- summarise_bets(bets) |>
  mutate(sport = "TOTAL", country = "")

summary_table <- bind_rows(league_summary, overall)

# Display
cat("\n")
cat("══════════════════════════════════════════════════════════════\n")
cat("  Cross-League PnL Summary\n")
cat("══════════════════════════════════════════════════════════════\n\n")

for (i in seq_len(nrow(summary_table))) {
  row <- summary_table[i, ]

  if (row$sport == "TOTAL") {
    cat("──────────────────────────────────────────────────────────────\n")
  }

  label <- if (row$sport == "TOTAL") "TOTAL" else paste0(row$sport, "/", row$country)
  cat(sprintf("  %-25s", label))

  cat(sprintf("%d bets (%d/%d/%d W/L/P)",
    row$total_bets, row$wins, row$losses, row$total_bets - row$settled))

  if (row$settled > 0) {
    cat(sprintf("  Win: %4.1f%%", row$win_pct))
  }
  cat("\n")

  cat(sprintf("  %-25s", ""))
  cat(sprintf("Wagered: %s kr", format(round(row$total_wagered), big.mark = ",")))

  if (row$outstanding > 0) {
    cat(sprintf("  Outstanding: %s kr", format(round(row$outstanding), big.mark = ",")))
  }

  if (row$settled > 0) {
    pnl_sign <- if (row$pnl >= 0) "+" else ""
    cat(sprintf("  PnL: %s%s kr (ROI: %s%.1f%%)",
      pnl_sign, format(round(row$pnl), big.mark = ","),
      pnl_sign, row$roi))
  }
  cat("\n")

  cat(sprintf("  %-25s", ""))
  cat(sprintf("Avg odds: %.2f  Avg EV: %.1f%%", row$avg_odds, row$avg_ev))
  cat("\n\n")
}

# Bankroll status
initial_pool <- 10973
current <- initial_pool - sum(bets$bet_amount[!bets$settled], na.rm = TRUE) +
  sum(bets$pnl_actual, na.rm = TRUE)
cat(sprintf("  Bankroll: %s kr / %s kr initial (%.1f%%)\n",
  format(round(current), big.mark = ","),
  format(initial_pool, big.mark = ","),
  current / initial_pool * 100))
cat("\n")
