#!/usr/bin/env Rscript
# Run publish_<sport>_iceland for all active leagues x both sexes.
#
# Reads the legacy backup fit.rds for each league and writes JSONs into
# data/publish/. Skips leagues where a backup fit isn't available.
#
# Usage:
#   Rscript scripts/publish_all.R
#   SPORTS_BACKUP_ROOT=/path/to/backup Rscript scripts/publish_all.R

suppressPackageStartupMessages(devtools::load_all(here::here()))

run_date <- Sys.Date()
backup_root <- Sys.getenv(
  "SPORTS_BACKUP_ROOT",
  "/Users/brynjolfurjonsson/sports-backup-20260424-163153"
)

publish_dispatch <- list(
  football_iceland   = publish_football_iceland,
  basketball_iceland = publish_basketball_iceland,
  handball_iceland   = publish_handball_iceland
)

leagues <- load_leagues()
active <- leagues[vapply(leagues, function(l) isTRUE(l$active), logical(1))]

t0 <- Sys.time()
for (key in names(active)) {
  league <- active[[key]]
  pub_fn <- publish_dispatch[[key]]
  if (is.null(pub_fn)) {
    cli::cli_alert_info("Skipping {key} — no publish dispatcher")
    next
  }
  for (sex in league$sexes) {
    cli::cli_h1("{key} / {sex}")
    fit_path <- file.path(
      backup_root, "Sports",
      league$sport, league$country,
      "results", sex, "fit.rds"
    )
    if (!file.exists(fit_path)) {
      cli::cli_alert_warning("No fit at {fit_path} — skipping")
      next
    }
    tryCatch(
      {
        fit <- readRDS(fit_path)
        pub_fn(fit, league, sex = sex, end_date = run_date)
        cli::cli_alert_success("published {key}/{sex}")
      },
      error = function(e) {
        cli::cli_alert_danger("FAILED {key}/{sex}: {conditionMessage(e)}")
      }
    )
  }
}
cli::cli_alert_info("Total wall-clock: {round(difftime(Sys.time(), t0, units = 'mins'), 2)} min")
