#' Cross-league PnL summary from CSV bet logs (source of truth)
#'
#' Splits output into current pipeline era and historical (pre-pipeline) bets.
#' Current era gets full per-league detail; historical gets a compact summary.
#'
#' Usage:
#'   Rscript R/summary/pnl.R                    # Current detail + historical summary
#'   Rscript R/summary/pnl.R --settled           # Only settled bets
#'   Rscript R/summary/pnl.R --since 2026-02-01  # Custom era cutoff
#'   Rscript R/summary/pnl.R --all               # Full detail for both eras

library(dplyr, warn.conflicts = FALSE)
library(readr, warn.conflicts = FALSE)

sports_dir <- here::here()

# Parse CLI args
args <- commandArgs(trailingOnly = TRUE)
settled_only <- "--settled" %in% args
show_all <- "--all" %in% args

bankroll_cfg <- yaml::yaml.load(readr::read_file(file.path(sports_dir, "config", "bankroll.yml")))

since_idx <- which(args == "--since")
cutoff_date <- if (length(since_idx) > 0 && since_idx < length(args)) {
  as.Date(args[since_idx + 1])
} else {
  as.Date(bankroll_cfg$epoch)
}

# Read all bets from CSV logs (source of truth)
log_files <- Sys.glob(file.path(sports_dir, "*", "*", "history", "bets_log.csv"))
bets <- lapply(log_files, \(f) {
  read_csv(f, show_col_types = FALSE, col_types = cols(info = "c", pnl = "d", win = "l"))
}) |> bind_rows()

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

# Split into eras
current_bets <- bets |> filter(as.Date(date_recommended) >= cutoff_date)
historical_bets <- bets |> filter(as.Date(date_recommended) < cutoff_date)

# Per-league summary helper
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

# Display a full per-league summary table
display_league_table <- function(df, show_total = TRUE) {
  if (nrow(df) == 0) {
    cat("  No bets in this period.\n\n")
    return(invisible())
  }

  league_summary <- df |>
    group_by(sport, country) |>
    group_modify(~ summarise_bets(.x)) |>
    ungroup() |>
    arrange(sport, country)

  if (show_total) {
    overall <- summarise_bets(df) |>
      mutate(sport = "TOTAL", country = "")
    summary_table <- bind_rows(league_summary, overall)
  } else {
    summary_table <- league_summary
  }

  for (i in seq_len(nrow(summary_table))) {
    row <- summary_table[i, ]

    if (row$sport == "TOTAL") {
      cat("  ............................................................\n")
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
}

# Display a compact one-line summary
display_compact_summary <- function(df) {
  if (nrow(df) == 0) {
    cat("  No bets in this period.\n")
    return(invisible())
  }

  s <- summarise_bets(df)
  pnl_sign <- if (!is.na(s$pnl) && s$pnl >= 0) "+" else ""

  cat(sprintf("  %d bets (%d/%d/%d W/L/P)",
    s$total_bets, s$wins, s$losses, s$pending))

  if (s$settled > 0) {
    cat(sprintf("  PnL: %s%s kr  ROI: %s%.1f%%",
      pnl_sign, format(round(s$pnl), big.mark = ","),
      pnl_sign, s$roi))
  }
  cat("\n")
}

# === Output ===

cat("\n")
cat("══════════════════════════════════════════════════════════════\n")
cat(sprintf("  Current Pipeline (since %s)\n", format(cutoff_date)))
cat("══════════════════════════════════════════════════════════════\n\n")

display_league_table(current_bets)

cat("──────────────────────────────────────────────────────────────\n")
cat(sprintf("  Historical (before %s)\n", format(cutoff_date)))
cat("──────────────────────────────────────────────────────────────\n")

if (show_all) {
  cat("\n")
  display_league_table(historical_bets)
} else {
  display_compact_summary(historical_bets)
  cat("\n")
}

# Bankroll status (epoch-filtered, consistent with compute_bankroll)
initial_pool <- bankroll_cfg$initial_pool
epoch_bets <- current_bets  # already filtered to >= cutoff_date
current <- initial_pool -
  sum(epoch_bets$bet_amount[!epoch_bets$settled], na.rm = TRUE) +
  sum(epoch_bets$pnl_actual, na.rm = TRUE)
cat(sprintf("  Bankroll: %s kr / %s kr initial (%.1f%%)\n",
  format(round(current), big.mark = ","),
  format(initial_pool, big.mark = ","),
  current / initial_pool * 100))
cat("\n")
