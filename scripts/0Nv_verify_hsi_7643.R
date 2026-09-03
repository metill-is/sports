#!/usr/bin/env Rscript
# Verify the recovered female Grill 66 2025 tournament id (7643) before it is
# registered. HSI_HISTORICAL_IDS recorded 7644 for this cell, which is a
# copy-paste of the male div2 id, and concluded the real id was unrecoverable.
# 7643 sits between the verified female div1 2025 (7642) and male div2 2025
# (7644) -- adjacency is a hypothesis, so it gets three independent checks:
#
#   (a) page title matches the female Grill 66 pattern
#   (b) .assert_season_stamp() passes for season 2025 (dates in the
#       Sept 2024 - May 2025 span)
#   (c) parsed teams intersect the known 2024 and 2026 female G66 squads
#
# Read-only: writes nothing but its own log. Registration is a separate step.
#
#   Rscript scripts/0Nv_verify_hsi_7643.R

options(width = 120)
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))

CANDIDATE <- 7643L
url <- sprintf("https://www.hsi.is/tournament/%d", CANDIDATE)

cli::cli_h1("Verifying HSI tournament {CANDIDATE} as female div2 season 2025")

html <- fetch_hsi_html(url)

# (a) Title.
title <- hsi_page_title(html)
cli::cli_alert_info("Page title: {title}")
mapped <- .hsi_match_title(title)
ok_title <- identical(mapped$sex[[1L]], "female") &&
  identical(mapped$division[[1L]], "div2")
if (ok_title) {
  cli::cli_alert_success("(a) Title maps to female/div2.")
} else {
  cli::cli_alert_danger("(a) Title maps to {mapped$sex} / {mapped$division} -- REJECT.")
}

# (b) Season stamp.
rows <- parse_hsi_results_page(
  html, sport = "handball", country = "iceland",
  sex = "female", division = "G66", season = 2025L
)
cli::cli_alert_info("Parsed {nrow(rows)} rows; date range {min(rows$match_date)} to {max(rows$match_date)}.")
print(table(format(rows$match_date, "%Y")))
ok_stamp <- tryCatch(
  {
    .assert_season_stamp(rows, 2025L, source = paste("candidate", CANDIDATE))
    TRUE
  },
  sports_season_stamp_error = function(e) {
    cli::cli_alert_danger("(b) {conditionMessage(e)}")
    FALSE
  }
)
if (ok_stamp) cli::cli_alert_success("(b) Season stamp passes for 2025.")

# (c) Roster intersection against what is already on disk.
known <- read_table(
  "results",
  filter = list(sport = "handball", country = "iceland", sex = "female")
)
known <- known[known$division == "G66" & known$season %in% c(2024L, 2026L), ]
known_teams <- sort(unique(c(known$home_team, known$away_team)))
candidate_teams <- sort(unique(c(rows$home_team, rows$away_team)))
shared <- intersect(known_teams, candidate_teams)
cli::cli_alert_info("Candidate teams ({length(candidate_teams)}): {paste(candidate_teams, collapse = ', ')}")
cli::cli_alert_info("Shared with known 2024/2026 G66 squads ({length(shared)}): {paste(shared, collapse = ', ')}")
ok_roster <- length(shared) >= 5L
if (ok_roster) {
  cli::cli_alert_success("(c) Roster intersection >= 5 clubs.")
} else {
  cli::cli_alert_danger("(c) Only {length(shared)} shared clubs -- REJECT.")
}

if (ok_title && ok_stamp && ok_roster) {
  cli::cli_alert_success(
    "ALL THREE PASS. Register {CANDIDATE} as female/div2/2025 with source 'inferred-verified'."
  )
} else {
  cli::cli_abort(
    "Verification FAILED -- leave female div2 2025 unregistered and let the unresolved-season report show the gap."
  )
}
