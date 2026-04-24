# Betting-PnL audit - end-to-end run.
#
# Usage: Rscript R/backtest/betting_pnl/run_pnl_audit.R
#
# Outputs (under football/iceland/results/male/audits/betting_pnl/):
#   - compare_pnl.txt              ranked summary tables per variant x mode x market
#   - disagreement_in_sample.csv   bet-level tags + stakes for in-sample universe
#   - disagreement_oos.csv         bet-level tags + stakes for OOS universe
#   - pnl_ledger.csv               appended each run for longitudinal tracking
#   - cumulative_pnl.png           cumulative-PnL line plot (if matches were settled)

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
})

Sys.setlocale("LC_ALL", "is_IS.UTF-8")

sports_dir <- here::here()
source(here::here("R", "backtest", "betting_pnl", "odds_load.R"))
source(here::here("R", "backtest", "betting_pnl", "predictions_load.R"))
source(here::here("R", "backtest", "betting_pnl", "ev_kelly.R"))
source(here::here("R", "backtest", "betting_pnl", "simulate_pnl.R"))
source(here::here("R", "backtest", "betting_pnl", "disagreement.R"))

out_dir <- file.path(
  sports_dir, "football", "iceland", "results", "male",
  "audits", "betting_pnl"
)
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ---- 1. Odds + results ----------------------------------------------------
odds_dir <- file.path(sports_dir, "..", "lengjan-odds", "data", "football_iceland")
data_path <- file.path(sports_dir, "football", "iceland", "data", "male", "data.csv")

# data.csv uses Icelandic column names - rename to English schema shared with
# odds_load / predictions_load.
results_all <- readr::read_csv(data_path, show_col_types = FALSE) |>
  rename(
    date = dags,
    home = heima,
    away = gestir,
    home_goals = stig_heima,
    away_goals = stig_gestir
  ) |>
  mutate(
    date = as.character(as.Date(date)),
    match_id = paste(date, home, away, sep = "|")
  ) |>
  select(match_id, date, home, away, home_goals, away_goals)

odds <- load_all_odds(odds_dir, results_all)
cat(
  "Odds rows:", nrow(odds),
  "| played:", sum(odds$played),
  "| unplayed:", sum(!odds$played), "\n"
)

# ---- 2. Variant predictions for both modes ------------------------------
variants <- c("bvp_noinflation", "v3_free_nu")

probs_oos <- suppressWarnings(load_variant_predictions(
  sports_dir = sports_dir,
  variants = variants,
  mode = "oos"
))

probs_in_sample <- suppressWarnings(load_variant_predictions(
  sports_dir = sports_dir,
  variants = variants,
  mode = "in_sample"
))

cat(
  "OOS prediction rows:", nrow(probs_oos),
  "| In-sample prediction rows:", nrow(probs_in_sample), "\n"
)

# ---- 3. Join odds x probs, compute EV/Kelly ------------------------------
make_bets_frame <- function(probs, odds, mode_label) {
  empty <- tibble::tibble(
    variant = character(), match_id = character(), market = character(),
    outcome = character(), line = numeric(), p_model = numeric(),
    odds = numeric(), won = logical(), played = logical(),
    date = character(), home = character(), away = character(),
    ev = numeric(), kelly_f = numeric(), stake = numeric(), pass = logical(),
    mode = character()
  )
  if (nrow(probs) == 0) {
    return(empty)
  }

  probs |>
    inner_join(
      odds |> select(match_id, market, outcome, line, odds, won, played, date, home, away),
      by = c("match_id", "market", "outcome", "line")
    ) |>
    compute_ev_kelly(
      bankroll = 50000, kelly_frac = 0.10,
      min_ev = 0.03, min_bet = 200
    ) |>
    mutate(mode = mode_label)
}

bets_oos <- make_bets_frame(probs_oos, odds, "oos")
bets_in_sample <- make_bets_frame(probs_in_sample, odds, "in_sample")

cat(
  "OOS bets joined:", nrow(bets_oos),
  "| passing filter:", sum(bets_oos$pass, na.rm = TRUE), "\n"
)
cat(
  "In-sample bets joined:", nrow(bets_in_sample),
  "| passing filter:", sum(bets_in_sample$pass, na.rm = TRUE), "\n"
)

all_bets <- bind_rows(bets_oos, bets_in_sample)

# ---- 4. Simulate PnL + aggregate -----------------------------------------
bets_with_pnl <- if (nrow(all_bets) > 0) compute_pnl(all_bets) else all_bets
summary_pnl <- if (nrow(bets_with_pnl) > 0) summarise_pnl(bets_with_pnl) else tibble::tibble()

# ---- 5. Disagreement tables (per mode) -----------------------------------
disagree_oos <- if (nrow(bets_oos) > 0) {
  classify_disagreement(bets_oos, variants = variants)
} else {
  tibble::tibble()
}
disagree_in_sample <- if (nrow(bets_in_sample) > 0) {
  classify_disagreement(bets_in_sample, variants = variants)
} else {
  tibble::tibble()
}

# ---- 6. Write artefacts --------------------------------------------------
readr::write_csv(disagree_oos, file.path(out_dir, "disagreement_oos.csv"))
readr::write_csv(disagree_in_sample, file.path(out_dir, "disagreement_in_sample.csv"))

# compare_pnl.txt - human-readable summary
sink(file.path(out_dir, "compare_pnl.txt"))
cat("Betting-PnL audit - run at", as.character(Sys.time()), "\n")
cat("======================================================================\n\n")

cat("Run parameters:\n")
cat("  bankroll=50000  kelly_frac=0.10  min_ev=0.03  min_bet=200\n")
cat("  variants:", paste(variants, collapse = ", "), "\n")
cat("  modes:    in_sample, oos (Phase 2)\n\n")

cat("Data universe:\n")
cat("  odds rows:            ", nrow(odds), "\n")
cat("  OOS prob rows:        ", nrow(probs_oos), "\n")
cat("  in-sample prob rows:  ", nrow(probs_in_sample), "\n")
cat("  OOS bets joined:      ", nrow(bets_oos), "\n")
cat("  in-sample bets joined:", nrow(bets_in_sample), "\n")
cat("  total passing filter: ", sum(all_bets$pass, na.rm = TRUE), "\n")
cat("  played bets passing:  ", sum(all_bets$pass & !is.na(all_bets$won), na.rm = TRUE), "\n\n")

cat("Summary PnL table (per variant x mode x market):\n")
if (nrow(summary_pnl) > 0) {
  print(summary_pnl |> arrange(mode, market, desc(total_pnl)), n = 50)
} else {
  cat("  (no settled bets yet)\n")
}
cat("\n")

cat("Disagreement tag counts:\n")
if (nrow(disagree_oos) > 0) {
  cat("  OOS:\n")
  print(disagree_oos |> count(tag))
}
if (nrow(disagree_in_sample) > 0) {
  cat("  in_sample:\n")
  print(disagree_in_sample |> count(tag))
}
if (nrow(disagree_oos) == 0 && nrow(disagree_in_sample) == 0) {
  cat("  (no bets where at least one variant passes)\n")
}
cat("\n")

cat("Per-variant bet counts (passing filter, by market):\n")
if (sum(all_bets$pass, na.rm = TRUE) > 0) {
  print(
    all_bets |>
      filter(pass) |>
      count(variant, mode, market, name = "n_bets")
  )
} else {
  cat("  (no bets pass filter)\n")
}
sink()

# Append to pnl_ledger.csv
ledger_path <- file.path(out_dir, "pnl_ledger.csv")
if (nrow(summary_pnl) > 0) {
  ledger_row <- summary_pnl |>
    mutate(run_at = as.character(Sys.time())) |>
    select(run_at, variant, mode, market, n_bets, total_stake, total_pnl, roi, hit_rate)
  if (file.exists(ledger_path)) {
    readr::write_csv(ledger_row, ledger_path, append = TRUE)
  } else {
    readr::write_csv(ledger_row, ledger_path)
  }
}

# Cumulative PnL plot (per mode, both variants)
cumulative <- bets_with_pnl |>
  filter(!is.na(pnl), stake > 0) |>
  arrange(date, match_id, market, outcome) |>
  group_by(variant, mode) |>
  mutate(cum_pnl = cumsum(pnl)) |>
  ungroup()

if (nrow(cumulative) > 0) {
  p <- ggplot(cumulative, aes(x = as.Date(date), y = cum_pnl, colour = variant)) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    facet_wrap(~mode, scales = "free_x") +
    labs(
      title = "Cumulative PnL by variant",
      x = "Match date", y = "Cumulative PnL (kr)"
    ) +
    theme_minimal()
  ggsave(file.path(out_dir, "cumulative_pnl.png"), p,
    width = 10, height = 5, dpi = 120
  )
  cat("Plot written to cumulative_pnl.png\n")
} else {
  cat("No settled bets - skipping plot.\n")
}

cat("\nDone. Artefacts written to:", out_dir, "\n")
cat("See compare_pnl.txt for the summary.\n")
