#!/usr/bin/env Rscript
# EURO 2028 qualifying odds from UEFA's drawn groups (draw night, 6 December
# 2026): group, direct, host-place, play-off and overall qualification
# probabilities for all 54 entrants, under both readings of Article 15 d),
# and how often the readings differ. See docs/runbooks/euro2028-draw-night.md.
#
# Usage:
#   Rscript scripts/nt/euro2028_odds.R fit=<fit.rds> out=<dir>
#       [groups=data/nt/euro2028/groups.csv] [fixtures=data/nt/euro2028/fixtures_2027.csv]
#       [n_sims=20000] [seed=2028] [team=Iceland]
# The default fixtures file is used when it exists: without it every pair
# plays home and away, Belarus, Ukraine and Israel at neutral venues. A
# fixtures= file that does not exist stops the script.
# odds.rds is nt_euro2028_odds()'s list, the input of
# scripts/nt/euro2028_publish.R (the publish JSON).
# Measured 2026-11-24 with the real fit: 62 s and 1.44 GB peak at 20,000
# simulations (both readings), 31 s and 1.32 GB at 10,000; under every heavy
# threshold, so `ml light -- Rscript ...`. Identical output on a rerun.

suppressMessages(devtools::load_all(quiet = TRUE))
options(width = 160)
args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default = NULL) {
  hit <- grep(paste0("^", name, "="), args, value = TRUE)
  if (length(hit) == 0L) default else sub(paste0("^", name, "="), "", hit[[1]])
}
fit_path <- opt("fit")
out_dir <- opt("out")
if (is.null(fit_path) || is.null(out_dir)) stop("Usage: euro2028_odds.R fit=<fit.rds> out=<dir> [key=value ...]")
groups_csv <- opt("groups", here::here("data", "nt", "euro2028", "groups.csv"))
fixtures_given <- opt("fixtures")
fixtures_csv <- fixtures_given %||% here::here("data", "nt", "euro2028", "fixtures_2027.csv")
if (!file.exists(fixtures_csv)) {
  # WHY: a fixture list named on the command line and missing is a typing
  # error; falling back to generated fixtures would hide it.
  if (!is.null(fixtures_given)) stop("fixtures= names a file that does not exist: ", fixtures_given)
  fixtures_csv <- NULL
}
n_sims <- as.integer(opt("n_sims", "20000"))
seed <- as.integer(opt("seed", "2028"))
focus <- opt("team", "Iceland")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

fit <- readRDS(fit_path)
draw <- nt_read_euro2028_draw(groups_csv, fixtures_csv, fit = fit)
t0 <- Sys.time()
odds <- nt_euro2028_odds(fit, draw, n_sims = n_sims, seed = seed, fixtures_source = fixtures_csv %||% "generated")
secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
cmp <- odds$compare

round_df <- function(x) {
  num <- vapply(x, is.numeric, logical(1))
  x[num] <- lapply(x[num], round, 4)
  x
}
order_teams <- function(x) x[order(x$group, -x$p_qualify), , drop = FALSE]
teams <- order_teams(odds$teams)
utils::write.csv(round_df(teams), file.path(out_dir, "teams.csv"), row.names = FALSE, fileEncoding = "UTF-8")
utils::write.csv(round_df(order_teams(odds$teams_once)), file.path(out_dir, "teams_once.csv"), row.names = FALSE, fileEncoding = "UTF-8")
utils::write.csv(round_df(odds$slots), file.path(out_dir, "slots.csv"), row.names = FALSE, fileEncoding = "UTF-8")

pct <- function(p) sprintf("%.1f%%", 100 * p)
qf <- draw$quarter_finals
label <- function(slot) {
  k <- match(slot, c(qf$winner_slot, qf$defeated_slot))
  ifelse(is.na(k), slot, sprintf("%s (%s/%s)", slot, rep(qf$first_leg_home, 2)[k], rep(qf$second_leg_home, 2)[k]))
}
src <- fit$source %||% list()
lines <- c(
  "# EURO 2028 qualifying odds",
  "",
  sprintf("Groups: `%s`. Fixtures: %s.", groups_csv, if (is.null(fixtures_csv)) "generated (Belarus, Ukraine, Israel at neutral venues)" else sprintf("`%s`", fixtures_csv)),
  sprintf(
    "Fit: `%s` (cutoff %s, last match %s, martj42 %s, %d draws). %s simulations, seed %d, %.0f s for both readings; run %s.",
    fit_path, format(fit$settings$cutoff %||% NA), src$last_match %||% "?", src$martj42_commit %||% "?",
    length(fit$draws$mu), format(n_sims, big.mark = ","), seed, secs, format(Sys.time(), "%Y-%m-%d %H:%M %Z")
  ),
  "",
  "Probabilities under the published reading of Article 15 d) (`h2h_reapply = \"repeat\"`). Direct = group winner or one of the eight best runners-up; host place = Article 14.04; play-off = enters the March 2028 play-offs (as a runner-up or via the Nations League); qualify = any route.",
  ""
)
ft <- teams[teams$team == focus, ]
if (nrow(ft) == 1L) {
  lines <- c(
    lines, sprintf("## %s", focus), "",
    sprintf(
      "Group %s. Win the group %s; qualify directly %s; play-offs %s (as runner-up %s, via the Nations League %s); win a play-off path or tie %s; **qualify %s**.",
      ft$group, pct(ft$p_winner), pct(ft$p_qualify_direct), pct(ft$p_playoff), pct(ft$p_playoff_runner_up),
      pct(ft$p_playoff_nations_league), pct(ft$p_qualify_playoff), pct(ft$p_qualify)
    ), ""
  )
}
lines <- c(lines, "## Groups", "")
for (g in names(draw$groups)) {
  sl <- odds$slots[odds$slots$group == g, , drop = FALSE]
  sl <- sl[order(-sl$p_qualify), , drop = FALSE]
  lines <- c(
    lines, sprintf("### Group %s", g), "",
    "| Team | Win group | 2nd | Direct | Play-offs | Host place | Qualify | Exp. points |",
    "| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |",
    sprintf(
      "| %s | %s | %s | %s | %s | %s | %s | %.1f |", label(sl$slot), pct(sl$p_winner), pct(sl$p_pos2),
      pct(sl$p_qualify_direct), pct(sl$p_playoff), pct(sl$p_host_place), pct(sl$p_qualify), sl$exp_points
    ), ""
  )
}
if (nrow(qf) > 0L) {
  qt <- teams[teams$team %in% c(qf$first_leg_home, qf$second_leg_home), , drop = FALSE]
  lines <- c(
    lines, "## Nations League quarter-finalists", "",
    "Each tie (March 2027, two legs) decides which group its teams play in; the group column shows the winner's group, then the defeated team's.", "",
    "| Team | Groups | Win group | Direct | Qualify |", "| --- | --- | ---: | ---: | ---: |",
    sprintf("| %s | %s | %s | %s | %s |", qt$team, qt$group, pct(qt$p_winner), pct(qt$p_qualify_direct), pct(qt$p_qualify)), ""
  )
}
hp <- table(factor(odds$n_host_places, 0:2))
lines <- c(
  lines, "## Host places and play-off format", "",
  sprintf(
    "Reserved host places used: none %s (8 teams, four home-and-away ties), one %s (12 teams, three paths), two %s (8 teams, two paths).",
    pct(hp[["0"]] / n_sims), pct(hp[["1"]] / n_sims), pct(hp[["2"]] / n_sims)
  ), "",
  "| Host | Direct | Host place | Play-offs | Qualify |", "| --- | ---: | ---: | ---: | ---: |",
  with(teams[teams$team %in% draw$hosts, ], sprintf("| %s | %s | %s | %s | %s |", team, pct(p_qualify_direct), pct(p_host_place), pct(p_playoff), pct(p_qualify))), ""
)

by_g <- cmp$by_group[cmp$by_group > 0]
dq <- merge(odds$teams[, c("team", "p_qualify_direct", "p_qualify")], odds$teams_once[, c("team", "p_qualify_direct", "p_qualify")], by = "team", suffixes = c("_repeat", "_once"))
lines <- c(
  lines, "## Article 15 d): the two readings", "",
  "Both runs share every simulated score (same seed), so differences come from the reading alone.", "",
  sprintf("- Simulations in which any group position differs: %d of %d (%s).", round(cmp$any_position * cmp$n_sims), cmp$n_sims, pct(cmp$any_position)),
  sprintf("- ... per group: %s.", if (length(by_g) == 0L) "none" else paste(sprintf("%s %s", names(by_g), pct(by_g)), collapse = ", ")),
  sprintf("- Simulations in which the 20 direct qualifiers differ: %s; the host places or play-off field: %s.", pct(cmp$direct), pct(cmp$field)),
  sprintf(
    "- Largest change in a team's direct-qualification probability: %.2f points; in its overall qualification probability: %.2f points (the latter includes play-off noise).",
    100 * max(abs(dq$p_qualify_direct_repeat - dq$p_qualify_direct_once)), 100 * max(abs(dq$p_qualify_repeat - dq$p_qualify_once))
  ), ""
)
writeLines(lines, file.path(out_dir, "odds.md"), useBytes = FALSE)
saveRDS(odds, file.path(out_dir, "odds.rds"))
cat(lines, sep = "\n")
