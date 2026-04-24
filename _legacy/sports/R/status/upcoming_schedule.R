#!/usr/bin/env Rscript
# Upcoming schedule for Raycast extension
# Usage: Rscript R/status/upcoming_schedule.R

invisible(Sys.setlocale("LC_ALL", "is_IS.UTF-8"))

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(jsonlite)
  library(yaml)
})

sports_dir <- here::here()

# Read leagues config
leagues_yml <- tryCatch(
  {
    raw <- read_file(file.path(sports_dir, "config", "leagues.yml"))
    yaml::yaml.load(raw)
  },
  error = function(e) list()
)
leagues_yml$defaults <- NULL

# Read recommendations for bet overlay
recs <- tryCatch(
  {
    recs_path <- file.path(sports_dir, "recommendations.csv")
    if (file.exists(recs_path)) read_csv(recs_path, show_col_types = FALSE) else tibble()
  },
  error = function(e) tibble()
)

today <- Sys.Date()
horizon <- today + 14 # Two weeks ahead

# Collect games from all active leagues
all_games <- do.call(rbind, lapply(names(leagues_yml), function(key) {
  l <- leagues_yml[[key]]
  if (!isTRUE(l$active)) {
    return(NULL)
  }

  sexes <- if (is.list(l$sex)) unlist(l$sex) else l$sex

  do.call(rbind, lapply(sexes, function(sex) {
    # Try next_games.csv first (from model results)
    ng_path <- file.path(sports_dir, l$dir, "results", sex, "next_games.csv")
    if (file.exists(ng_path)) {
      tryCatch(
        {
          d <- read_csv(ng_path, show_col_types = FALSE)
          if (nrow(d) == 0) {
            return(NULL)
          }
          d$date <- as.Date(d$date)
          d <- d[d$date >= today & d$date <= horizon, ]
          if (nrow(d) == 0) {
            return(NULL)
          }
          d$sport <- l$sport
          d$country <- l$country
          d$sex <- sex
          if (!"division" %in% names(d)) d$division <- ""
          d <- d[, c("date", "sport", "country", "sex", "division", "home", "away")]
          d
        },
        error = function(e) NULL
      )
    } else {
      NULL
    }
  }))
}))

if (is.null(all_games) || nrow(all_games) == 0) {
  cat(toJSON(list(games = list(), summary = list()),
    auto_unbox = TRUE, pretty = TRUE
  ))
  quit(save = "no", status = 0)
}

# Check odds coverage per game
odds_base <- file.path(dirname(sports_dir), "lengjan-odds", "data")

all_games$has_odds <- vapply(seq_len(nrow(all_games)), function(i) {
  g <- all_games[i, ]
  # Build league key pattern
  league_key <- paste0(g$sport, "_", g$country)
  league_dir <- file.path(odds_base, league_key)
  if (!dir.exists(league_dir)) {
    return(FALSE)
  }

  odds_files <- list.files(league_dir, pattern = "odds_.*\\.csv$", full.names = TRUE)
  if (length(odds_files) == 0) {
    return(FALSE)
  }

  any(vapply(odds_files, function(f) {
    tryCatch(
      {
        od <- read_csv(f, show_col_types = FALSE)
        any(od$date == as.character(g$date) &
          (od$home == g$home | od$away == g$away))
      },
      error = function(e) FALSE
    )
  }, logical(1)))
}, logical(1))

# Left-join recommendations
all_games$bet <- lapply(seq_len(nrow(all_games)), function(i) {
  g <- all_games[i, ]
  if (nrow(recs) == 0) {
    return(NULL)
  }

  match <- recs |>
    filter(
      date == as.character(g$date),
      heima == g$home | gestir == g$away
    )

  if (nrow(match) == 0) {
    return(NULL)
  }

  # Take first match (could be multiple markets)
  m <- match[1, ]
  list(
    market = m$market,
    outcome = m$outcome,
    odds = round(m$o, 2),
    ev = round(m$ev, 2),
    stake = round(m$bet_amount)
  )
})

# Build games list for JSON
games_list <- lapply(seq_len(nrow(all_games)), function(i) {
  g <- all_games[i, ]
  list(
    date = as.character(g$date),
    sport = g$sport,
    country = g$country,
    sex = g$sex,
    division = g$division,
    home = g$home,
    away = g$away,
    has_odds = g$has_odds,
    bet = all_games$bet[[i]]
  )
})

# Build summary (same shape as pipeline_status.R schedule_summary)
summary_data <- split(all_games, all_games$date) |>
  lapply(function(day) {
    sports <- table(day$sport)
    list(
      basketball = as.integer(ifelse(is.na(sports["basketball"]), 0L, sports["basketball"])),
      handball = as.integer(ifelse(is.na(sports["handball"]), 0L, sports["handball"])),
      football = as.integer(ifelse(is.na(sports["football"]), 0L, sports["football"])),
      total = nrow(day),
      no_odds = sum(!day$has_odds)
    )
  })

result <- list(
  games = games_list,
  summary = summary_data
)

cat(toJSON(result, auto_unbox = TRUE, pretty = TRUE, na = "null"))
