#### Scan schedules and export active competitions for lengjan-odds ####
#
# Run from Sports/ directory:
#   source("check_schedules.R")
#
# Reads all schedule.csv files, finds leagues with upcoming games,
# prints a summary, and writes active_competitions.json to lengjan-odds.
#

box::use(
  R / schedule / scan[scan_schedules]
)

lookahead_days <- 7
sports_root <- here::here()

cat(sprintf(
  "\nScanning schedules for games in the next %d days (until %s)...\n\n",
  lookahead_days,
  Sys.Date() + lookahead_days
))

result <- scan_schedules(sports_root, lookahead_days = lookahead_days)

# ── Print summary ─────────────────────────────────────────────────────────────

cat("=== Schedule Summary ===\n\n")
cat(sprintf("  %-30s %6s   %s\n", "Sport", "Games", "Next game"))
cat(sprintf("  %-30s %6s   %s\n", "-----", "-----", "---------"))

for (i in seq_len(nrow(result$summary))) {
  row <- result$summary[i, ]
  next_str <- if (is.na(row$next_game)) "-" else format(row$next_game, "%a %d %b")
  marker <- if (row$status == "active") "*" else " "
  cat(sprintf(
    " %s %-30s %5d   %s\n",
    marker,
    row$sport,
    row$n_upcoming,
    next_str
  ))
}

cat(sprintf("\n  Total upcoming games: %d\n", nrow(result$upcoming)))

# ── Lengjan active competitions ───────────────────────────────────────────────

lengjan_path <- file.path(dirname(sports_root), "lengjan-odds", "config")

if (!dir.exists(lengjan_path)) {
  cat("\n  [WARN] lengjan-odds/config/ not found at:", lengjan_path, "\n")
  cat("  Skipping JSON export.\n")
} else {
  # Read competitions.yml to get all configured keys
  # Use readBin + yaml::yaml.load to handle UTF-8 league names (Icelandic/Nordic)
  competitions_yml <- file.path(lengjan_path, "competitions.yml")
  if (file.exists(competitions_yml)) {
    yml_text <- readr::read_file(competitions_yml)
    all_lengjan_keys <- names(yaml::yaml.load(yml_text))
  } else {
    all_lengjan_keys <- character(0)
  }

  # Build active map: TRUE for keys with upcoming games, FALSE for others
  active_map <- stats::setNames(
    all_lengjan_keys %in% result$active_lengjan,
    all_lengjan_keys
  )

  output <- list(
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    lookahead_days = lookahead_days,
    active = as.list(active_map)
  )

  json_file <- file.path(lengjan_path, "active_competitions.json")
  jsonlite::write_json(output, json_file, auto_unbox = TRUE, pretty = TRUE)

  cat("\n=== Lengjan Active Competitions ===\n\n")
  for (key in all_lengjan_keys) {
    status <- if (active_map[[key]]) "SCRAPE" else "skip"
    cat(sprintf("  %-25s %s\n", key, status))
  }
  cat(sprintf("\n  Wrote: %s\n", json_file))
}

cat("\nDone!\n")
