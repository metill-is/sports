#!/usr/bin/env Rscript
# Automatic refresh of the 2027 Women's World Cup UEFA play-off forecast,
# run by .github/workflows/wwc2027-playoff.yml on the leg nights (1 and 5
# December 2026). Reads the round-2 matches from UEFA's match feed, fills the
# finished legs into data/wc/structure/wwc2027_playoff_round2.csv (first leg:
# its 90-minute score; second leg: the tie's winner), and only if that table
# changed republishes data/publish/world_cup/kvenna/ from the compact pre-leg
# fit data/wc/wwc2027/playoff_fit.rds, by the same code path as the manual
# scripts/nt/wwc2027_publish_playoff.R. Light work: no refit.
#
# Writes `changed=true|false` to $GITHUB_OUTPUT when that is set.
#
# Usage:
#   Rscript scripts/nt/wwc2027_auto_playoff.R [--uefa-json=<file>] [--fit=<rds>]
#     [--ties=<csv>] [--root=<data root>] [--force]
#
#   --uefa-json  read the feed from a saved file instead of UEFA (a dry run)
#   --force      republish even if no result changed

suppressMessages(devtools::load_all(quiet = TRUE))
options(width = 160)

args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit) == 0L) default else sub(paste0("^--", name, "="), "", hit[[1]])
}
fit_path <- opt("fit", here::here("data", "wc", "wwc2027", "playoff_fit.rds"))
ties_csv <- opt("ties", here::here("data", "wc", "structure", "wwc2027_playoff_round2.csv"))
root <- opt("root", here::here("data"))
uefa_json <- opt("uefa-json", NA_character_)
force <- "--force" %in% args

matches <- if (is.na(uefa_json)) {
  wwc2027_fetch_uefa()
} else {
  jsonlite::read_json(uefa_json, simplifyVector = FALSE)
}
legs <- wwc2027_uefa_legs(matches)
cat(sprintf(
  "UEFA feed: %d round-2 matches, %d finished\n",
  nrow(legs), sum(legs$status == "FINISHED")
))
if (nrow(legs) == 0L) {
  # WHY stop: the feed listed all 16 legs on 27 November; none at all means a
  # changed feed, which should fail the run loudly, not pass as "no news".
  cli::cli_abort("The UEFA feed returned no round-2 play-off matches.")
}

ties <- wwc2027_read_ties(ties_csv)
updated <- wwc2027_apply_results(ties, legs)
changed <- !identical(ties, updated)
if (changed) {
  wwc2027_write_ties(updated, ties_csv)
  cat("Results changed:\n")
  print(as.data.frame(updated[c("team_a", "team_b", "leg1_goals_a", "leg1_goals_b", "winner")]))
}

publish <- changed || force
if (publish) {
  saved <- readRDS(fit_path)
  out <- wwc2027_publish_from_saved(saved, updated, root = root)
  pl <- jsonlite::read_json(file.path(out, "playoff.json"))
  cat(sprintf("Wrote %s (effective draws %.0f of %d)\n", out, pl$effective_draws, pl$n_draws))
  print(data.frame(
    tie = vapply(pl$ties, function(t) paste(t$team_a, "v", t$team_b), ""),
    status = vapply(pl$ties, `[[`, "", "status"),
    p_a = vapply(pl$ties, `[[`, 1, "p_a"), p_b = vapply(pl$ties, `[[`, 1, "p_b")
  ))
} else {
  cat("No new results: nothing published.\n")
}

gh_out <- Sys.getenv("GITHUB_OUTPUT")
if (nzchar(gh_out)) {
  cat(sprintf("changed=%s\n", tolower(as.character(publish))), file = gh_out, append = TRUE)
}
