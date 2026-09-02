#!/usr/bin/env Rscript
# scripts/02_scrape_odds.R --
# Scrape Lengjan odds for active leagues with upcoming games.
#
# Writes data/facts/odds/sport=*/country=*/scraped_date=YYYY-MM-DD/.
# Skips leagues with no game in the next 14 days (rule: "don't scrape
# odds if there are no upcoming games").
#
# Usage:
#   Rscript scripts/02_scrape_odds.R                    # all active leagues
#   Rscript scripts/02_scrape_odds.R --league football_iceland
#   Rscript scripts/02_scrape_odds.R --force            # bypass guard

invisible(Sys.setlocale("LC_ALL", "en_US.UTF-8"))
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))
source(here::here("scripts", "_lib.R"))

opts <- parse_pipeline_args()
leagues <- load_leagues()
active <- filter_leagues(leagues, active_only = TRUE, has_lengjan = TRUE)
if (!is.null(opts$league)) {
  if (!opts$league %in% names(active)) {
    cli::cli_alert_warning(
      "--league '{opts$league}' has no Lengjan config or is not active; nothing to do."
    )
    quit(save = "no", status = 0L)
  }
  active <- active[opts$league]
}

active_path <- here::here("config", "active_competitions.json")
if (!fs::file_exists(active_path)) {
  stop(
    "config/active_competitions.json missing. ",
    "Run scripts/00_active_competitions.R first."
  )
}

cli::cli_h1("Scrape odds ({length(active)} Lengjan-eligible leagues)")
n_inseason <- 0L
total_rows <- 0L
for (key in names(active)) {
  league_def <- active[[key]]
  static <- league_def[c(
    "sport", "country", "sexes", "active", "stan_model", "data_source"
  )]
  lengjan <- league_def$lengjan

  any_upcoming <- FALSE
  for (sx in league_def$sexes) {
    if (has_upcoming_games(static, sx)) {
      any_upcoming <- TRUE
      break
    }
  }

  if (!any_upcoming && !opts$force) {
    cli::cli_alert_info(
      "Skipping {key}: no games in the next 14 days (use --force to override)."
    )
    next
  }

  cli::cli_h2("{key}")
  n_inseason <- n_inseason + 1L
  total_rows <- total_rows + ingest_one_lengjan(
    static, lengjan, key, active_path,
    betting = league_def[["betting"]]
  )
}

# A run-wide 0-row result is usually benign, not an outage: between rounds --
# and football is the only in-season Lengjan league while basketball + handball
# are paused -- Lengjan posts no odds for days at a time. Aborting here red-Xed
# the workflow (and so fired the health alert email) on every benign gap,
# desensitising that channel. Staleness escalation now belongs to the
# healthcheck's `odds_freshness` check (data/health, 2x/day): if odds stay
# absent past its threshold it FAILs and fires the email -- which a transient
# between-rounds gap, self-healing when Lengjan reposts, never reaches. So warn
# and exit clean instead of aborting; a real outage still surfaces via
# odds_freshness rather than an immediate red scrape.
if (odds_scrape_empty_failure(n_inseason, total_rows, force = opts$force)) {
  cli::cli_alert_warning(
    "Odds scrape wrote 0 rows across {n_inseason} in-season league(s); no odds posted right now (likely between rounds). odds_freshness escalates if this persists."
  )
} else {
  cli::cli_alert_success(
    "Odds scrape complete ({total_rows} rows, {n_inseason} in-season league(s))"
  )
}
