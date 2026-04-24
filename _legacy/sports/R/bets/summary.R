#' Print betting history summary tables
#'
#' Reads history/bets_all.csv and prints performance tables
#' grouped by sport, country, sex, market, and booker.
#'
#' Usage: Rscript R/bets/summary.R

library(readr)
library(dplyr)
library(here)

here::i_am(".here")

d <- read_csv(here("history", "bets_all.csv"), show_col_types = FALSE)
settled <- d |> filter(!is.na(win))

fmt_summary <- function(data, ..., label = "") {
  grps <- enquos(...)
  data |>
    group_by(!!!grps) |>
    summarise(
      n = n(),
      won = sum(win),
      hit_rate = mean(win),
      wagered = sum(bet_amount),
      pnl = sum(pnl),
      roi = sum(pnl) / sum(bet_amount),
      .groups = "drop"
    ) |>
    arrange(desc(n)) |>
    mutate(
      hit_rate = sprintf("%.1f%%", 100 * hit_rate),
      roi = sprintf("%.1f%%", 100 * roi),
      wagered = format(round(wagered), big.mark = ","),
      pnl = format(round(pnl), big.mark = ",")
    )
}

cat("=== BETTING HISTORY SUMMARY ===\n")
cat("Total:", nrow(d), "bets | Settled:", nrow(settled),
    "| Unsettled:", nrow(d) - nrow(settled), "\n\n")

cat("── By sport ──\n")
fmt_summary(settled, sport) |> print()

cat("\n── By sport + country ──\n")
fmt_summary(settled, sport, country) |> print(n = 30)

cat("\n── By sport + sex ──\n")
fmt_summary(settled, sport, sex) |> print(n = 20)

cat("\n── By sport + country + sex ──\n")
fmt_summary(settled, sport, country, sex) |> print(n = 40)

cat("\n── By market ──\n")
fmt_summary(settled, market) |> print()

cat("\n── By booker ──\n")
fmt_summary(settled, booker) |> print()

# ---------------------------------------------------------------------------
# Kelly fraction comparison: current bets.yml vs recommended
# ---------------------------------------------------------------------------

cat("\n── Kelly fraction: current vs recommended ──\n")

library(yaml)

leagues_yml <- here("config", "leagues.yml")
if (file.exists(leagues_yml)) {
  leagues_raw <- yaml::yaml.load(readr::read_file(leagues_yml))
  leagues_raw$defaults <- NULL

  box::use(R/bets/history[recommend_kelly])
  recs <- recommend_kelly(settled, by = c("sport", "country", "sex"), min_bets = 20)

  kelly_comp <- list()
  for (lk in names(leagues_raw)) {
    lg <- leagues_raw[[lk]]
    if (!isTRUE(lg$has_bets)) next

    bets_yml_path <- here(lg$dir, "config", "bets.yml")
    if (!file.exists(bets_yml_path)) next

    cfg <- yaml::yaml.load(readr::read_file(bets_yml_path))
    base_kf <- cfg$bankroll$kelly_frac %||% 0.10

    for (sex in lg$sex) {
      sex_key <- paste0("kelly_frac_", sex)
      cur_kf <- cfg$bankroll[[sex_key]] %||% base_kf

      rec_row <- recs |>
        filter(sport == lg$sport, country == lg$country, .data$sex == .env$sex)

      rec_kf <- if (nrow(rec_row) > 0) rec_row$recommended_kelly[1] else NA_real_
      n <- if (nrow(rec_row) > 0) rec_row$n_bets[1] else 0L

      kelly_comp <- c(kelly_comp, list(tibble(
        league = lk, sex = sex,
        current_kf = cur_kf,
        recommended_kf = rec_kf,
        n_bets = as.integer(n)
      )))
    }
  }

  kelly_df <- bind_rows(kelly_comp) |>
    mutate(
      current_kf = sprintf("%.2f", current_kf),
      recommended_kf = ifelse(is.na(recommended_kf), "—",
                               sprintf("%.2f", recommended_kf)),
      delta = ifelse(recommended_kf == "—", "—",
                     sprintf("%+.2f",
                             as.numeric(recommended_kf) - as.numeric(current_kf)))
    )

  print(kelly_df, n = 40)
} else {
  cat("  (No config/leagues.yml found — skipping kelly comparison)\n")
}
