#' One-off migration: populate Parquet bet store from existing CSV history
#'
#' Globs all bets_log.csv files, reads each, and writes to the Parquet store
#' using sport/country from the CSV's own columns.
#'
#' Usage:
#'   Rscript R/storage/migrate_history.R

library(readr)

sports_dir <- here::here()
source(file.path(sports_dir, "R", "storage", "store.R"))

# Clean stale Parquet partitions with non-standard sex values
stale_patterns <- c("sex=kk", "sex=kvk", "sex=NA")
store_bets_dir <- file.path(sports_dir, "store", "bets")
if (dir.exists(store_bets_dir)) {
  all_sex_dirs <- list.dirs(store_bets_dir, recursive = TRUE, full.names = TRUE)
  stale_dirs <- all_sex_dirs[basename(all_sex_dirs) %in% stale_patterns]
  if (length(stale_dirs) > 0) {
    cat("Removing", length(stale_dirs), "stale partition(s):\n")
    for (d in stale_dirs) {
      cat("  ", d, "\n")
      unlink(d, recursive = TRUE)
    }
    cat("\n")
  }
}

logs <- Sys.glob(file.path(sports_dir, "*", "*", "history", "bets_log.csv"))
cat("Found", length(logs), "bets_log.csv file(s)\n\n")

if (length(logs) == 0) {
  cat("Nothing to migrate.\n")
  quit(save = "no")
}

for (log_path in logs) {
  cat("Processing:", log_path, "\n")
  df <- read_csv(log_path, show_col_types = FALSE)

  if (nrow(df) == 0) {
    cat("  Empty — skipping\n")
    next
  }

  # Extract sport/country from the CSV columns
  sport <- unique(df$sport)[1]
  country <- unique(df$country)[1]

  if (is.na(sport) || is.na(country)) {
    cat("  Missing sport/country columns — skipping\n")
    next
  }

  # Write per-sex partitions
  for (sex in unique(df$sex)) {
    sex_df <- df[df$sex == sex, ]
    store_bets(sex_df, sport, country, sex, sports_dir)
  }

  # Also write an "all" partition for settle compatibility
  store_bets(df, sport, country, sex = "all", sports_dir)

  cat("  Migrated", nrow(df), "rows for", sport, "/", country, "\n\n")
}

cat("Migration complete.\n")
