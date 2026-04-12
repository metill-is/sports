#!/usr/bin/env Rscript
# PnL summary for Raycast extension
# Usage: Rscript R/status/pnl_summary.R [--days N]

invisible(Sys.setlocale("LC_ALL", "is_IS.UTF-8"))

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(jsonlite)
  library(yaml)
})

args <- commandArgs(trailingOnly = TRUE)

# Parse --days flag
days <- NULL
if ("--days" %in% args) {
  idx <- which(args == "--days")
  if (idx < length(args)) days <- as.integer(args[idx + 1])
}

sports_dir <- here::here()

# Read bankroll epoch
cfg <- tryCatch(
  yaml::yaml.load(read_file(file.path(sports_dir, "config", "bankroll.yml"))),
  error = function(e) list(epoch = "2026-03-01")
)
epoch <- as.Date(cfg$epoch)

# Read all bets_log.csv files
log_files <- Sys.glob(file.path(sports_dir, "*", "*", "history", "bets_log.csv"))

all_bets <- tryCatch(
  {
    do.call(rbind, lapply(log_files, function(f) {
      tryCatch(
        read_csv(f,
          col_types = cols(info = "c", pnl = "d", win = "l"),
          show_col_types = FALSE
        ),
        error = function(e) NULL
      )
    }))
  },
  error = function(e) tibble()
)

# Filter to active leagues only
active_filter <- tryCatch(
  {
    raw <- read_file(file.path(sports_dir, "config", "leagues.yml"))
    ly <- yaml::yaml.load(raw)
    ly$defaults <- NULL
    active <- Filter(function(l) isTRUE(l$active), ly)
    unique(vapply(active, function(l) paste(l$sport, l$country, sep = "_"), character(1)))
  },
  error = function(e) NULL
)

if (!is.null(active_filter) && !is.null(all_bets) && nrow(all_bets) > 0) {
  all_bets <- all_bets |>
    mutate(.league_key = paste(sport, country, sep = "_")) |>
    filter(.league_key %in% active_filter) |>
    select(-.league_key)
}

if (is.null(all_bets) || nrow(all_bets) == 0) {
  cat(toJSON(list(
    overview = list(
      total_pnl = 0, win_rate = 0, roi = 0,
      total_wagered = 0, bets_settled = 0L, avg_stake = 0
    ),
    by_sport = list(),
    by_market = list(),
    recent = list()
  ), auto_unbox = TRUE, pretty = TRUE))
  quit(save = "no", status = 0)
}

# Filter to settled bets after epoch
settled <- all_bets |>
  filter(!is.na(win), date_match >= epoch)

# Apply --days filter
if (!is.null(days)) {
  cutoff <- Sys.Date() - days
  settled <- filter(settled, date_match >= cutoff)
}

# Filter out sex="all" partition rows (avoid double-counting)
if ("sex" %in% names(settled)) {
  settled <- filter(settled, sex != "all")
}

# Overview
compute_summary <- function(d) {
  n <- nrow(d)
  if (n == 0) {
    return(list(
      total_pnl = 0, win_rate = 0, roi = 0,
      total_wagered = 0, bets_settled = 0L, avg_stake = 0
    ))
  }
  wagered <- sum(d$bet_amount, na.rm = TRUE)
  list(
    total_pnl = round(sum(d$pnl, na.rm = TRUE)),
    win_rate = round(mean(d$win, na.rm = TRUE), 3),
    roi = if (wagered > 0) round(sum(d$pnl, na.rm = TRUE) / wagered, 3) else 0,
    total_wagered = round(wagered),
    bets_settled = n,
    avg_stake = round(wagered / n)
  )
}

overview <- compute_summary(settled)

# By sport
by_sport <- settled |>
  group_by(sport) |>
  group_split() |>
  lapply(function(d) {
    s <- compute_summary(d)
    list(
      sport = d$sport[1], bets = s$bets_settled, win_rate = s$win_rate,
      pnl = s$total_pnl, roi = s$roi, wagered = s$total_wagered
    )
  })

# By market
by_market <- settled |>
  group_by(market) |>
  group_split() |>
  lapply(function(d) {
    s <- compute_summary(d)
    list(
      market = d$market[1], bets = s$bets_settled, win_rate = s$win_rate,
      pnl = s$total_pnl, roi = s$roi, wagered = s$total_wagered
    )
  })

# Recent (last 10)
recent <- settled |>
  arrange(desc(date_match)) |>
  head(10) |>
  select(date_match, sport, home, away, market, outcome, win, pnl) |>
  mutate(pnl = round(pnl))

result <- list(
  overview = overview,
  by_sport = by_sport,
  by_market = by_market,
  recent = if (nrow(recent) > 0) recent else list()
)

cat(toJSON(result, auto_unbox = TRUE, pretty = TRUE, na = "null"))
