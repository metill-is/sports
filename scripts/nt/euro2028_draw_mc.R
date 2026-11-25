#!/usr/bin/env Rscript
# Before the EURO 2028 qualifying draw: simulate the draw under UEFA's
# published procedure and conditions, then qualifying for each simulated
# draw, giving every team's odds averaged over the draw, one team's likely
# groups and opponents, and how often the two readings of Article 15 d)
# change a group table (both readings run on every draw with one seed).
#
# Usage:
#   Rscript scripts/nt/euro2028_draw_mc.R fit=<fit.rds> out=<dir>
#       [n_draws=500] [n_sims=1000] [seed=2028] [team=Iceland]
# Heavy (measured 2026-11-24: 2.3 s per draw at 1,000 simulations, both readings;
# 500 draws in 21 minutes): run it as `ml run -- Rscript ...`.

suppressMessages(devtools::load_all(quiet = TRUE))
options(width = 160)
args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default = NULL) {
  hit <- grep(paste0("^", name, "="), args, value = TRUE)
  if (length(hit) == 0L) default else sub(paste0("^", name, "="), "", hit[[1]])
}
fit_path <- opt("fit")
out_dir <- opt("out")
if (is.null(fit_path) || is.null(out_dir)) stop("Usage: euro2028_draw_mc.R fit=<fit.rds> out=<dir> [key=value ...]")
n_draws <- as.integer(opt("n_draws", "500"))
n_sims <- as.integer(opt("n_sims", "1000"))
seed <- as.integer(opt("seed", "2028"))
focus <- opt("team", "Iceland")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

fit <- readRDS(fit_path)
facts <- nt_euro2028_facts()
t0 <- Sys.time()
draws <- nt_simulate_uefa_draw(facts, n_draws = n_draws, seed = seed)
t_draw <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
cat(sprintf("%d draws in %.0f s\n", n_draws, t_draw))

teams <- facts$seeding$team
metrics <- c("p_winner", "p_qualify_direct", "p_host_place", "p_playoff", "p_playoff_nations_league", "p_qualify")
res <- array(NA_real_, c(n_draws, length(teams), length(metrics)), dimnames = list(NULL, teams, metrics))
focus_group <- character(n_draws)
focus_mates <- vector("list", n_draws)
cmp_rows <- vector("list", n_draws)
qf <- facts$quarter_finals
t1 <- Sys.time()
for (i in seq_len(n_draws)) {
  g <- draws[[i]]
  d <- nt_euro2028_draw(g, facts = facts, fit = fit)
  a <- nt_simulate_euro2028(fit, d, n_sims = n_sims, seed = seed + i)
  b <- nt_simulate_euro2028(fit, d, n_sims = n_sims, h2h_reapply = "once", seed = seed + i)
  tab <- a$teams[match(teams, a$teams$team), ]
  for (m in metrics) res[i, , m] <- tab[[m]]
  hit <- names(g)[vapply(g, function(x) focus %in% x, logical(1))]
  focus_group[i] <- hit
  focus_mates[[i]] <- setdiff(g[[hit]], focus)
  cmp <- nt_compare_h2h_readings(a, b)
  five <- names(g) %in% LETTERS[7:12]
  cmp_rows[[i]] <- data.frame(
    draw = i, any_position = cmp$any_position,
    four = max(cmp$by_group[!five]),
    five_any = max(cmp$by_group[five]),
    five_mean = mean(cmp$by_group[five]),
    direct = cmp$direct, field = cmp$field
  )
  if (i %% 25L == 0L) {
    el <- as.numeric(difftime(Sys.time(), t1, units = "secs"))
    cat(sprintf("draw %d of %d, %.1f s per draw\n", i, n_draws, el / i))
  }
}
t_sim <- as.numeric(difftime(Sys.time(), t1, units = "secs"))
cmp_tab <- do.call(rbind, cmp_rows)

summ <- function(m) {
  x <- res[, , m]
  data.frame(mean = colMeans(x), q10 = apply(x, 2, stats::quantile, 0.1), q90 = apply(x, 2, stats::quantile, 0.9))
}
team_tab <- data.frame(team = teams, pot = facts$seeding$pot)
for (m in metrics) {
  s <- summ(m)
  team_tab[[m]] <- s$mean
  if (m %in% c("p_qualify_direct", "p_qualify")) {
    team_tab[[paste0(m, "_q10")]] <- s$q10
    team_tab[[paste0(m, "_q90")]] <- s$q90
  }
}
team_tab <- team_tab[order(-team_tab$p_qualify), ]
utils::write.csv(team_tab, file.path(out_dir, "team_odds.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# Focus team: groups, opponents and odds by draw.
slot_pot <- stats::setNames(c(rep(1L, 2L * nrow(qf)), facts$seeding$pot), c(qf$winner_slot, qf$defeated_slot, facts$seeding$team))
mates <- unlist(focus_mates)
opp <- as.data.frame(table(opponent = mates), stringsAsFactors = FALSE)
opp$pot <- slot_pot[opp$opponent]
opp$share <- opp$Freq / n_draws
opp <- opp[order(opp$pot, -opp$share), c("pot", "opponent", "share")]
utils::write.csv(opp, file.path(out_dir, "focus_opponents.csv"), row.names = FALSE, fileEncoding = "UTF-8")
fq <- res[, focus, ]
by_draw <- data.frame(
  draw = seq_len(n_draws), group = focus_group,
  opponents = vapply(focus_mates, paste, "", collapse = ", "),
  p_winner = fq[, "p_winner"], p_qualify_direct = fq[, "p_qualify_direct"],
  p_playoff = fq[, "p_playoff"], p_qualify = fq[, "p_qualify"]
)
utils::write.csv(by_draw, file.path(out_dir, "focus_by_draw.csv"), row.names = FALSE, fileEncoding = "UTF-8")
utils::write.csv(cmp_tab, file.path(out_dir, "h2h_readings_by_draw.csv"), row.names = FALSE)
saveRDS(
  list(
    draws = draws, res = res, focus = focus, focus_group = focus_group, focus_mates = focus_mates,
    compare = cmp_tab, n_sims = n_sims, seed = seed, fit_source = fit$source, fit_settings = fit$settings
  ),
  file.path(out_dir, "draw_mc.rds")
)

pct <- function(p) sprintf("%.1f%%", 100 * p)
src <- fit$source %||% list()
ft <- team_tab[team_tab$team == focus, ]
five <- focus_group %in% LETTERS[7:12]
by_size <- tapply(by_draw$p_qualify, ifelse(five, "five", "four"), mean)
best <- by_draw[order(-by_draw$p_qualify), ][1:5, ]
worst <- by_draw[order(by_draw$p_qualify), ][1:5, ]
n_tables <- n_draws * n_sims
lines <- c(
  sprintf("# EURO 2028 qualifying before the draw: %d simulated draws x %s simulations", n_draws, format(n_sims, big.mark = ",")),
  "",
  sprintf(
    "Fit `%s` (cutoff %s, last match %s, martj42 %s). Seed %d. Draws %.0f s, simulations %.0f s; run %s.",
    fit_path, format(fit$settings$cutoff %||% NA), src$last_match %||% "?", src$martj42_commit %||% "?", seed, t_draw, t_sim,
    format(Sys.time(), "%Y-%m-%d %H:%M %Z")
  ),
  "",
  sprintf("## %s", focus), "",
  sprintf(
    "Averaged over the draw: win the group %s, qualify directly %s (10th to 90th percentile over draws %s to %s), play-offs %s (via the Nations League %s), **qualify %s** (%s to %s).",
    pct(ft$p_winner), pct(ft$p_qualify_direct), pct(ft$p_qualify_direct_q10), pct(ft$p_qualify_direct_q90),
    pct(ft$p_playoff), pct(ft$p_playoff_nations_league), pct(ft$p_qualify), pct(ft$p_qualify_q10), pct(ft$p_qualify_q90)
  ),
  "",
  sprintf(
    "Group of five (G-L): %s of draws; qualify %s in a group of five, %s in a group of four.",
    pct(mean(five)), pct(by_size[["five"]] %||% NA), pct(by_size[["four"]] %||% NA)
  ),
  "",
  sprintf("Group letter: %s.", paste(sprintf("%s %s", names(table(focus_group)), pct(as.vector(table(focus_group)) / n_draws)), collapse = ", ")),
  "",
  "Most likely opponents by pot (share of draws):", ""
)
for (p in sort(unique(opp$pot))) {
  o <- opp[opp$pot == p, ]
  lines <- c(lines, sprintf("- Pot %d: %s", p, paste(sprintf("%s %s", o$opponent, pct(o$share)), collapse = ", ")))
}
lines <- c(
  lines, "", "Best five simulated draws for the focus team (qualify):", "",
  sprintf("- %s: %s (group %s)", pct(best$p_qualify), best$opponents, best$group), "",
  "Worst five:", "", sprintf("- %s: %s (group %s)", pct(worst$p_qualify), worst$opponents, worst$group), "",
  "## All teams, averaged over the draw", "",
  "| Team | Pot | Win group | Direct | Play-offs | Qualify (10th-90th pct over draws) |",
  "| --- | ---: | ---: | ---: | ---: | --- |",
  sprintf(
    "| %s | %d | %s | %s | %s | %s (%s-%s) |", team_tab$team, team_tab$pot, pct(team_tab$p_winner),
    pct(team_tab$p_qualify_direct), pct(team_tab$p_playoff), pct(team_tab$p_qualify),
    pct(team_tab$p_qualify_q10), pct(team_tab$p_qualify_q90)
  ), "",
  "## Article 15 d): the two readings across simulated draws", "",
  sprintf(
    "Across %s simulated qualifying campaigns (%d draws x %s), each run under both readings with one seed:",
    format(n_tables, big.mark = ","), n_draws, format(n_sims, big.mark = ",")
  ), "",
  sprintf("- Any group position differs: %d campaigns (%s).", round(sum(cmp_tab$any_position * n_sims)), sprintf("%.4f%%", 100 * mean(cmp_tab$any_position))),
  sprintf("- In groups of four: %d draws with any difference.", sum(cmp_tab$four > 0)),
  sprintf(
    "- Five-team group tables that differ: %s of %s (%s).", round(sum(cmp_tab$five_mean * 6 * n_sims)),
    format(6 * n_tables, big.mark = ",", scientific = FALSE), sprintf("%.5f%%", 100 * mean(cmp_tab$five_mean))
  ),
  sprintf(
    "- The 20 direct qualifiers differ: %d campaigns; the host places or play-off field: %d.",
    round(sum(cmp_tab$direct * n_sims)), round(sum(cmp_tab$field * n_sims))
  ), ""
)
writeLines(lines, file.path(out_dir, "draw_mc.md"), useBytes = FALSE)
cat(lines, sep = "\n")
