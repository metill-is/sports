#!/usr/bin/env Rscript
# EURO 2028 qualifying odds as the publish JSON contract (meta.json,
# groups.json, teams.json; see publish_euro2028()), from the odds.rds that
# scripts/nt/euro2028_odds.R writes on draw night.
#
# The default output, data/publish/euro2028/karla/, is mirrored by
# metill-platform's pull-sports-data workflow: committing it there publishes
# it, which needs the owner's approval. Write to another folder with out= to
# look at the files first. See docs/runbooks/euro2028-draw-night.md.
#
# Usage:
#   Rscript scripts/nt/euro2028_publish.R odds=<odds.rds> [out=data/publish/euro2028/karla]

suppressMessages(devtools::load_all(quiet = TRUE))
args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default = NULL) {
  hit <- grep(paste0("^", name, "="), args, value = TRUE)
  if (length(hit) == 0L) default else sub(paste0("^", name, "="), "", hit[[1]])
}
odds_path <- opt("odds")
if (is.null(odds_path)) stop("Usage: euro2028_publish.R odds=<odds.rds> [out=<dir>]")
out_dir <- opt("out", here::here("data", "publish", "euro2028", "karla"))
odds <- readRDS(odds_path)
if (is.null(odds$quarter_finals) || is.null(odds$fixtures_source)) {
  stop("This odds.rds predates the publish contract; rerun scripts/nt/euro2028_odds.R to rebuild it.")
}
publish_euro2028(odds, out_dir = out_dir)
