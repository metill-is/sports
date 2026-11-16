# How much of the walk-forward was fitted under a prior that the corrected
# reversion-group definition changes?
#
# The walk-forward in the report was run before review findings 1 and 5 were
# fixed. This rebuilds the specification at every weekly cutoff it used (no
# model is fitted) and compares, for every season-break step, who reverts and
# toward which teams' mean under the definition used then and the one used
# now (fv2_reversion_groups()).
#
#   Rscript research/football-v2/scan_reversion_groups.R [variant]
# Run from the repo root. Writes group_change_scan.rds to the output folder.
options(width = 170)
suppressMessages(devtools::load_all(".", quiet = TRUE))
source("research/football-v2/data.R")
source("research/football-v2/variants.R")
fv2_source()
args <- commandArgs(trailingOnly = TRUE)
variant <- if (length(args) >= 1L) args[[1]] else "rev80_sq_static_lvl"
out <- fv2_out_dir()
league <- load_leagues()[["football_iceland"]]

# The definition the walk-forward ran with: a break reverts when the team did
# not change rank and its old division is known, and the mean is over the
# breaks of the same division and season that exist, that is over the teams
# that already have a slot in a later season.
old_groups <- function(spec) {
  st <- spec$steps
  reverts <- st$class == "brk" & st$move == 0 & !is.na(st$from_div)
  key <- ifelse(reverts, paste(st$from_div, st$from_season), NA)
  lapply(seq_len(nrow(st)), function(i) if (reverts[i]) sort(st$prev[which(key == key[i])]))
}
new_groups <- function(spec) {
  st <- spec$steps
  lapply(seq_len(nrow(st)), function(i) if (st$group[i] > 0L) sort(spec$groups$members[[st$group[i]]]))
}

files <- list.files(out, pattern = sprintf("^wf_%s_(male|female)_[0-9]{4}[.]rds$", variant), full.names = TRUE)
stopifnot(length(files) > 0L)
rows <- list()
for (f in files) {
  pred <- readRDS(f)$pred
  sex <- pred$sex[1]
  res <- fv2_results(sex)
  divisions <- fv2_division_table(res, fv2_rank_order(league))
  for (cutoff in sort(unique(as.character(pred$cutoff)))) {
    here <- pred[as.character(pred$cutoff) == cutoff, , drop = FALSE]
    sp <- fv2_split(res, league, cutoff, horizon_days = 7L)
    spec <- do.call(fv2_build, c(list(sp$train, sp$pred, divisions = divisions), fv2_variants()[[variant]]))
    old <- old_groups(spec)
    new <- new_groups(spec)
    st <- spec$steps
    differs <- !mapply(identical, old, new)
    latest <- st$class == "brk" & st$from_season == suppressWarnings(max(st$from_season[st$class == "brk"]))
    rows[[length(rows) + 1L]] <- data.frame(
      sex = sex, season = here$season[1], cutoff = cutoff, n_pred = nrow(here),
      breaks = sum(st$class == "brk"), differing = sum(differs),
      # A difference at the newest break is the early-season case of finding 1;
      # one at an older break is a team that left, or sat out, a season.
      differing_newest = sum(differs & latest), differing_older = sum(differs & !latest),
      no_longer_reverts = sum(differs & !vapply(new, length, integer(1)) & vapply(old, length, integer(1)) > 0L)
    )
  }
  cat(basename(f), "done\n")
}
scan <- do.call(rbind, rows)

# The three cutoffs at which INLA was compared with NUTS on this model.
validation <- do.call(rbind, lapply(c("female", "male"), function(sex) {
  res <- fv2_results(sex)
  divisions <- fv2_division_table(res, fv2_rank_order(league))
  cuts <- data.frame(
    cutoff = c("2026-07-31", "2026-05-03", "2022-05-08"),
    horizon_end = c("2026-10-31", "2026-05-31", "2022-06-05")
  )
  do.call(rbind, lapply(seq_len(nrow(cuts)), function(i) {
    sp <- fv2_split(res, league, cuts$cutoff[i], horizon_end = as.Date(cuts$horizon_end[i]))
    spec <- do.call(fv2_build, c(list(sp$train, sp$pred, divisions = divisions), fv2_variants()[[variant]]))
    differs <- !mapply(identical, old_groups(spec), new_groups(spec))
    data.frame(
      sex = sex, cutoff = cuts$cutoff[i], breaks = sum(spec$steps$class == "brk"),
      differing = sum(differs), fixtures = nrow(sp$pred)
    )
  }))
}))
saveRDS(
  list(variant = variant, generated = format(Sys.time(), "%Y-%m-%d %H:%M"), scan = scan, validation = validation),
  file.path(out, "group_change_scan.rds")
)
cat("\nvalidation cutoffs:\n")
print(validation, row.names = FALSE)

total <- sum(scan$n_pred)
cat(sprintf(
  "\n%d cutoffs, %d forecasts. Fits whose prior differs at all: %d cutoffs, %d forecasts (%.1f %%).\n",
  nrow(scan), total, sum(scan$differing > 0), sum(scan$n_pred[scan$differing > 0]),
  100 * sum(scan$n_pred[scan$differing > 0]) / total
))
cat(sprintf(
  "  at the newest break (early season): %d cutoffs, %d forecasts\n  only at an older break (a team that left or sat out): %d cutoffs, %d forecasts\n",
  sum(scan$differing_newest > 0), sum(scan$n_pred[scan$differing_newest > 0]),
  sum(scan$differing_newest == 0 & scan$differing_older > 0), sum(scan$n_pred[scan$differing_newest == 0 & scan$differing_older > 0])
))
print(stats::aggregate(cbind(n_pred, affected = n_pred * (differing > 0), newest = n_pred * (differing_newest > 0)) ~ sex + season, scan, sum), row.names = FALSE)
