#!/usr/bin/env Rscript
#' Pipeline Status — JSON endpoint for Raycast betting monitor
#'
#' Aggregates all observable pipeline state into a single JSON object.
#' Designed to be called every ~15 minutes by a Raycast menu bar extension.
#'
#' Usage:
#'   cd Sports/
#'   Rscript R/status/pipeline_status.R              # Full status (~2s)
#'   Rscript R/status/pipeline_status.R --quick       # Skip settleable check (~1s)
#'
#' Output: JSON to stdout. All messages/warnings go to stderr.
#'
#' JSON schema:
#'   {
#'     timestamp: ISO 8601,
#'     recommendations: { total, today, tomorrow, total_stake, by_date: [{date, count, stake}] },
#'     unsettled: { total, by_league: [{league, count}], settleable: { count, wins, losses, total_pnl, bets: [...] } },
#'     freshness: {
#'       livesport_data: { last_commit, age_hours },
#'       lengjan_odds:   { last_commit, age_hours },
#'       models: [{ league, fit_time, age_hours }],
#'       data_sync_stale: bool   # TRUE if livesport-data has newer commits than Sports data files
#'     },
#'     bankroll: { initial_pool, current_pool, outstanding, settled_pnl, week_pnl }
#'   }

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(jsonlite)
})

sports_dir <- here::here()
args <- commandArgs(trailingOnly = TRUE)
quick_mode <- "--quick" %in% args
now <- Sys.time()
today <- Sys.Date()

# Auto-sync livesport-data (need fresh results for settleable check)
ls_path <- file.path(dirname(sports_dir), "livesport-data")
if (!quick_mode && dir.exists(file.path(ls_path, ".git"))) {
  branch <- trimws(system2("git", c("-C", ls_path, "rev-parse", "--abbrev-ref", "HEAD"), stdout = TRUE))
  system2("git", c("-C", ls_path, "fetch", "origin", "-q"), stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", ls_path, "reset", "--hard", paste0("origin/", branch)),
          stdout = FALSE, stderr = FALSE)
}

msg <- function(...) message(sprintf(...))

# ── Livesport-data direct reader (bypasses step_data processing) ─────────────

ls_data_dir <- file.path(dirname(sports_dir), "livesport-data", "data")

parse_ls_date <- function(x) {
  m <- regmatches(x, regexpr("^\\d{2}\\.\\d{2}\\.", x))
  day <- as.integer(substr(m, 1, 2))
  mon <- as.integer(substr(m, 4, 5))
  cur_yr <- as.integer(format(Sys.Date(), "%Y"))
  cur_mon <- as.integer(format(Sys.Date(), "%m"))
  yr <- ifelse(mon >= 8 & cur_mon <= 7, cur_yr - 1L, cur_yr)
  as.Date(sprintf("%04d-%02d-%02d", yr, mon, day))
}

load_livesport_results <- function(sport, country, sex) {
  ls_sport <- if (sport == "football") "soccer" else sport
  base <- file.path(ls_data_dir, ls_sport, country, sex)
  if (!dir.exists(base)) return(NULL)
  divs <- list.dirs(base, full.names = TRUE, recursive = FALSE)
  results <- lapply(divs, function(d) {
    f <- file.path(d, "results.csv")
    if (!file.exists(f)) return(NULL)
    tryCatch({
      r <- read_csv(f, show_col_types = FALSE,
                    col_types = cols(home_score = "i", away_score = "i"))
      if (!all(c("date", "home", "away", "home_score", "away_score") %in% names(r))) return(NULL)
      r |>
        filter(!is.na(home_score), !is.na(away_score)) |>
        mutate(date = parse_ls_date(date))
    }, error = function(e) NULL)
  })
  bind_rows(Filter(Negate(is.null), results))
}

# ═══════════════════════════════════════════════════════════════════════════════
# 1. Recommendations
# ═══════════════════════════════════════════════════════════════════════════════

recs_path <- file.path(sports_dir, "recommendations.csv")

recs <- if (file.exists(recs_path)) {
  tryCatch(
    read_csv(recs_path, show_col_types = FALSE),
    error = function(e) { msg("Failed to read recommendations: %s", e$message); tibble() }
  )
} else {
  tibble()
}

if (nrow(recs) > 0) {
  recs_by_date <- recs |>
    mutate(date = as.Date(date)) |>
    group_by(date) |>
    summarise(count = n(), stake = round(sum(bet_amount, na.rm = TRUE)), .groups = "drop") |>
    arrange(date)

  recs_summary <- list(
    total = nrow(recs),
    today = sum(recs_by_date$date == today, na.rm = TRUE),
    tomorrow = sum(recs_by_date$date == today + 1, na.rm = TRUE),
    total_stake = round(sum(recs$bet_amount, na.rm = TRUE)),
    by_date = recs_by_date
  )
} else {
  recs_summary <- list(total = 0L, today = 0L, tomorrow = 0L, total_stake = 0L, by_date = list())
}


# ═══════════════════════════════════════════════════════════════════════════════
# 2. Unsettled Bets
# ═══════════════════════════════════════════════════════════════════════════════

log_files <- Sys.glob(file.path(sports_dir, "*", "*", "history", "bets_log.csv"))

all_bets <- if (length(log_files) > 0) {
  lapply(log_files, function(f) {
    tryCatch(
      read_csv(f, show_col_types = FALSE, col_types = cols(info = "c", pnl = "d", win = "l")),
      error = function(e) { msg("Failed to read %s: %s", f, e$message); NULL }
    )
  }) |>
    Filter(Negate(is.null), x = _) |>
    bind_rows()
} else {
  tibble()
}

unsettled <- if (nrow(all_bets) > 0) filter(all_bets, is.na(win)) else tibble()

unsettled_by_league <- if (nrow(unsettled) > 0) {
  unsettled |>
    group_by(sport, country) |>
    summarise(count = n(), .groups = "drop") |>
    mutate(league = paste(sport, country, sep = "_")) |>
    select(league, count)
} else {
  tibble(league = character(), count = integer())
}


# ═══════════════════════════════════════════════════════════════════════════════
# 3. Settleable Check (skip in --quick mode)
# ═══════════════════════════════════════════════════════════════════════════════

# Replicates settle.R logic in read-only mode:
# For each league with unsettled bets, load results and check which matches
# have finished. Returns preview of win/loss/PnL.

check_settleable <- function() {
  if (nrow(unsettled) == 0) {
    return(list(count = 0L, wins = 0L, losses = 0L, total_pnl = 0, bets = list()))
  }

  # Load leagues.yml
  leagues_yml <- tryCatch({
    raw <- read_file(file.path(sports_dir, "config", "leagues.yml"))
    yaml::yaml.load(raw)
  }, error = function(e) { msg("Failed to read leagues.yml: %s", e$message); list() })
  leagues_yml$defaults <- NULL

  # Column name normalisation map (same as settle.R)
  col_map <- c(
    dagsetning  = "date",  dags = "date",
    heima       = "home",  gestir = "away",
    home_goals  = "home_score", away_goals = "away_score",
    stig_heima  = "home_score", stig_gestir = "away_score"
  )

  all_settleable <- list()

  for (league_key in names(leagues_yml)) {
    lcfg <- leagues_yml[[league_key]]
    if (!isTRUE(lcfg$has_bets)) next

    league_dir <- file.path(sports_dir, lcfg$dir)
    log_path <- file.path(league_dir, "history", "bets_log.csv")
    if (!file.exists(log_path)) next

    log <- tryCatch(
      read_csv(log_path, show_col_types = FALSE, col_types = cols(info = "c", pnl = "d", win = "l")),
      error = function(e) NULL
    )
    if (is.null(log)) next

    league_unsettled <- log |> filter(is.na(win))
    if (nrow(league_unsettled) == 0) next

    sexes <- if (is.list(lcfg$sex)) unlist(lcfg$sex) else lcfg$sex

    # Load results for each sex (processed data.csv + fresh livesport-data)
    all_results <- NULL
    for (sex in sexes) {
      for (candidate in c("data.csv", "results.csv")) {
        path <- file.path(league_dir, "data", sex, candidate)
        if (!file.exists(path)) next

        d <- tryCatch(read_csv(path, show_col_types = FALSE), error = function(e) NULL)
        if (is.null(d)) next

        # Normalise columns
        for (old in names(col_map)) {
          new <- col_map[[old]]
          if (old %in% names(d) && !new %in% names(d)) names(d)[names(d) == old] <- new
        }

        required <- c("date", "home", "away", "home_score", "away_score")
        if (!all(required %in% names(d))) next

        d <- d |>
          mutate(date = as.Date(date), sex = sex) |>
          select(date, home, away, home_score, away_score, sex)
        all_results <- bind_rows(all_results, d)
        break
      }

      # Supplement: fresh livesport-data (may have newer results not yet processed)
      ls_results <- tryCatch(
        load_livesport_results(lcfg$sport, lcfg$country, sex),
        error = function(e) NULL
      )
      if (!is.null(ls_results) && nrow(ls_results) > 0) {
        ls_results <- ls_results |>
          mutate(sex = sex) |>
          select(date, home, away, home_score, away_score, sex)
        all_results <- bind_rows(all_results, ls_results) |>
          distinct(date, home, away, sex, .keep_all = TRUE)
      }
    }

    if (is.null(all_results)) next

    # Join unsettled bets against results
    matched <- league_unsettled |>
      mutate(date_match = as.Date(date_match)) |>
      left_join(all_results, by = c("date_match" = "date", "home", "away", "sex"))

    has_result <- matched |> filter(!is.na(home_score))
    if (nrow(has_result) == 0) next

    # Compute settlement preview
    settled_preview <- has_result |>
      mutate(
        info_num = suppressWarnings(as.numeric(info)),
        win = case_when(
          market == "outcome" & outcome == "home" ~ home_score > away_score,
          market == "outcome" & outcome == "away" ~ away_score > home_score,
          market == "outcome" & outcome == "tie"  ~ home_score == away_score,
          market == "handicap" & !is.na(info_num) & outcome == "home" ~
            (home_score - away_score) + info_num > 0,
          market == "handicap" & !is.na(info_num) & outcome == "away" ~
            (home_score - away_score) + info_num < 0,
          market == "handicap" & !is.na(info_num) & outcome == "tie" ~
            (home_score - away_score) + info_num == 0,
          market == "totals" & !is.na(info_num) & outcome == "over" ~
            (home_score + away_score) > info_num,
          market == "totals" & !is.na(info_num) & outcome == "under" ~
            (home_score + away_score) <= info_num
        ),
        pnl = ifelse(win, odds * bet_amount - bet_amount, -bet_amount)
      ) |>
      filter(!is.na(win)) |>
      select(
        date_match, sport, country, sex, market, home, away, outcome,
        odds, bet_amount, info, home_score, away_score, win, pnl
      )

    if (nrow(settled_preview) > 0) {
      all_settleable[[length(all_settleable) + 1]] <- settled_preview
    }
  }

  if (length(all_settleable) > 0) {
    combined <- bind_rows(all_settleable)
    list(
      count   = nrow(combined),
      wins    = sum(combined$win, na.rm = TRUE),
      losses  = sum(!combined$win, na.rm = TRUE),
      total_pnl = round(sum(combined$pnl, na.rm = TRUE)),
      bets    = combined
    )
  } else {
    list(count = 0L, wins = 0L, losses = 0L, total_pnl = 0, bets = list())
  }
}

settleable <- if (quick_mode) {
  list(count = NA_integer_)
} else {
  tryCatch(check_settleable(), error = function(e) {
    msg("Settleable check failed: %s", e$message)
    list(count = NA_integer_, error = e$message)
  })
}


# ═══════════════════════════════════════════════════════════════════════════════
# 4. Freshness
# ═══════════════════════════════════════════════════════════════════════════════

git_last_commit <- function(repo_path) {
  if (!dir.exists(repo_path)) return(NULL)
  ts <- tryCatch(
    system2("git", c("-C", repo_path, "log", "-1", "--format=%cI"), stdout = TRUE, stderr = FALSE),
    error = function(e) NULL
  )
  if (is.null(ts) || length(ts) == 0 || ts == "") return(NULL)
  # Replace trailing Z with +0000 for strptime compatibility on macOS
  ts <- sub("Z$", "+0000", ts)
  as.POSIXct(ts, format = "%Y-%m-%dT%H:%M:%S%z")
}

livesport_path <- file.path(dirname(sports_dir), "livesport-data")
odds_path <- file.path(dirname(sports_dir), "lengjan-odds")

livesport_commit <- git_last_commit(livesport_path)
odds_commit <- git_last_commit(odds_path)

format_freshness <- function(commit_time) {
  if (is.null(commit_time)) return(list(last_commit = NA, age_hours = NA))
  list(
    last_commit = format(commit_time, "%Y-%m-%dT%H:%M:%S%z"),
    age_hours = round(as.numeric(difftime(now, commit_time, units = "hours")), 1)
  )
}

# Model freshness: find fit.rds for each betting league
model_freshness <- tryCatch({
  leagues_yml <- yaml::yaml.load(read_file(file.path(sports_dir, "config", "leagues.yml")))
  leagues_yml$defaults <- NULL

  model_info <- list()
  for (league_key in names(leagues_yml)) {
    lcfg <- leagues_yml[[league_key]]
    if (!isTRUE(lcfg$has_bets)) next

    league_dir <- file.path(sports_dir, lcfg$dir)
    sexes <- if (is.list(lcfg$sex)) unlist(lcfg$sex) else lcfg$sex

    # Find newest fit.rds across sexes
    fit_files <- file.path(league_dir, "results", sexes, "fit.rds")
    fit_exists <- file.exists(fit_files)

    if (any(fit_exists)) {
      newest <- max(file.mtime(fit_files[fit_exists]))
      model_info[[length(model_info) + 1]] <- list(
        league = league_key,
        fit_time = format(newest, "%Y-%m-%dT%H:%M:%S"),
        age_hours = round(as.numeric(difftime(now, newest, units = "hours")), 1)
      )
    } else {
      model_info[[length(model_info) + 1]] <- list(
        league = league_key,
        fit_time = NA,
        age_hours = NA
      )
    }
  }
  model_info
}, error = function(e) { msg("Model freshness check failed: %s", e$message); list() })

# Check if livesport-data has newer data than Sports data dirs
data_sync_stale <- tryCatch({
  if (is.null(livesport_commit)) FALSE
  else {
    # Find newest data file in Sports/
    data_files <- Sys.glob(file.path(sports_dir, "*", "*", "data", "*", "data.csv"))
    if (length(data_files) == 0) TRUE
    else {
      newest_data <- max(file.mtime(data_files))
      livesport_commit > newest_data
    }
  }
}, error = function(e) FALSE)

freshness <- list(
  livesport_data = format_freshness(livesport_commit),
  lengjan_odds   = format_freshness(odds_commit),
  models         = model_freshness,
  data_sync_stale = data_sync_stale
)


# ═══════════════════════════════════════════════════════════════════════════════
# 5. Bankroll
# ═══════════════════════════════════════════════════════════════════════════════

bankroll <- tryCatch({
  cfg <- yaml::yaml.load(read_file(file.path(sports_dir, "config", "bankroll.yml")))
  initial <- cfg$initial_pool
  epoch <- as.Date(cfg$epoch)

  epoch_bets <- all_bets |>
    mutate(date_recommended = as.Date(date_recommended)) |>
    filter(date_recommended >= epoch)

  settled_pnl <- epoch_bets |>
    filter(!is.na(win)) |>
    mutate(pnl_actual = ifelse(win, odds * bet_amount - bet_amount, -bet_amount)) |>
    pull(pnl_actual) |>
    sum(na.rm = TRUE)

  outstanding <- epoch_bets |>
    filter(is.na(win)) |>
    pull(bet_amount) |>
    sum(na.rm = TRUE)

  # Week PnL (last 7 days of settled bets)
  week_pnl <- epoch_bets |>
    filter(!is.na(win), as.Date(date_match) >= today - 7) |>
    mutate(pnl_actual = ifelse(win, odds * bet_amount - bet_amount, -bet_amount)) |>
    pull(pnl_actual) |>
    sum(na.rm = TRUE)

  list(
    initial_pool = initial,
    current_pool = round(initial + settled_pnl - outstanding),
    outstanding  = round(outstanding),
    settled_pnl  = round(settled_pnl),
    week_pnl     = round(week_pnl)
  )
}, error = function(e) {
  msg("Bankroll calculation failed: %s", e$message)
  list(error = e$message)
})


# ═══════════════════════════════════════════════════════════════════════════════
# Output
# ═══════════════════════════════════════════════════════════════════════════════

status <- list(
  timestamp = format(now, "%Y-%m-%dT%H:%M:%S"),
  recommendations = recs_summary,
  unsettled = list(
    total = nrow(unsettled),
    by_league = unsettled_by_league,
    settleable = settleable
  ),
  freshness = freshness,
  bankroll = bankroll
)

cat(toJSON(status, auto_unbox = TRUE, pretty = TRUE, na = "null"))
cat("\n")
