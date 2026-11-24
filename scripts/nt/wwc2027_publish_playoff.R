#!/usr/bin/env Rscript
# Publish the 2027 Women's World Cup UEFA play-off forecast (round 2) to
# data/publish/world_cup/kvenna/ (meta.json, playoff.json). Sports-side JSON
# only (owner, 24 November 2026: "Sports-side JSON only"): no
# config/publish-schemas/ entry and no metill-platform route reads this tree.
# Light work: reads the saved draws of scripts/nt/wwc2027_fit.R, no refit.
#
# The ties are data/wc/structure/wwc2027_playoff_round2.csv (ties, hosts and
# ranks from uefa.com, 11 November 2026; dates known only for Iceland's tie).
# After the first legs fill in leg1_goals_a and leg1_goals_b, after the
# second legs the winner, and run this again. A first-leg score reweights the
# draws by its likelihood, so use it only with a fit whose data predate the
# first legs (or the match is counted twice).
#
# Usage:
#   Rscript scripts/nt/wwc2027_publish_playoff.R <fit.rds> [ties.csv] [data root]

suppressMessages(devtools::load_all(quiet = TRUE))
options(width = 160)

args <- commandArgs(trailingOnly = TRUE)
saved <- readRDS(args[[1]])
ties_csv <- if (length(args) >= 2L) {
  args[[2]]
} else {
  here::here("data", "wc", "structure", "wwc2027_playoff_round2.csv")
}
root <- if (length(args) >= 3L) args[[3]] else here::here("data")
ties <- utils::read.csv(ties_csv,
  stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = c("", "NA")
)
fc <- nt_two_leg_forecast(saved$fit, ties)
out <- publish_wwc2027_playoff(fc,
  n_direct = 7L, fit_date = saved$cutoff, data_through = saved$last_match, root = root
)
pl <- jsonlite::read_json(file.path(out, "playoff.json"))
cat(sprintf("Wrote %s (effective draws %.0f of %d)\n", out, pl$effective_draws, pl$n_draws))
print(data.frame(
  tie = vapply(pl$ties, function(t) paste(t$team_a, "v", t$team_b), ""),
  status = vapply(pl$ties, `[[`, "", "status"),
  p_a = vapply(pl$ties, `[[`, 1, "p_a"), p_b = vapply(pl$ties, `[[`, 1, "p_b")
))
