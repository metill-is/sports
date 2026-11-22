#!/usr/bin/env Rscript
# Reproduce the Euro 2024 qualifying tables and overall ranking from the
# results with the UEFA ranking code, and write a log of what decided each
# tie. The same checks run as tests in tests/testthat/test-nt-qualifying.R.
#
# Usage: Rscript scripts/nt/validate_euro2024_qualifying.R <log file>

suppressMessages(devtools::load_all(quiet = TRUE))
options(width = 150)

log_path <- commandArgs(trailingOnly = TRUE)[[1]]
dir_f <- here::here("tests", "testthat", "fixtures", "euro2024_qualifying")
read <- function(f) utils::read.csv(file.path(dir_f, f), stringsAsFactors = FALSE, encoding = "UTF-8")
m <- read("matches.csv")
tb <- read("tables.csv")
ov <- read("overall.csv")
nl <- read("nations_league_rank.csv")
dc <- read("disciplinary.csv")
groups <- split(tb$team, tb$group)
nl_rank <- stats::setNames(nl$rank, nl$team)
disc <- stats::setNames(dc$points, dc$team)

lines <- c(
  "Euro 2024 qualifying reproduced from the results with R/nt-qualifying.R",
  sprintf("Run %s; %d matches, %d groups.", format(Sys.time(), "%Y-%m-%d %H:%M %Z"), nrow(m), length(groups)),
  ""
)
all_ok <- TRUE
for (g in names(groups)) {
  r <- nt_rank_uefa_group(m[m$group == g, ], groups[[g]], nl_rank = nl_rank)
  want <- tb[tb$group == g, ]
  ok <- identical(r$team, want$team) &&
    all(r$points == want$points, r$gf == want$gf, r$ga == want$ga, r$won == want$won)
  all_ok <- all_ok && ok
  lines <- c(lines, sprintf("Group %s: %s  %s", g, if (ok) "matches the official table" else "DIFFERS",
    paste(sprintf("%d %s (%d)", r$position, r$team, r$points), collapse = ", ")))
  ties <- which(nzchar(r$decided_by) & r$decided_by != "points")
  for (i in ties) {
    note <- want$tiebreak_note[i]
    lines <- c(lines, sprintf("  %s above %s on %s; official note: %s", r$team[i], r$team[i + 1],
      r$decided_by[i], if (nzchar(note)) note else "(none)"))
  }
}
o <- nt_uefa_overall_ranking(m, groups, nl_rank = nl_rank, disciplinary_overall = disc)
same_order <- identical(o$team, ov$team)
all_ok <- all_ok && same_order
lines <- c(lines, "", sprintf("Overall ranking (results against sixth place discarded): %s",
  if (same_order) "all 53 ranks match" else "DIFFERS"))
ru <- o[o$group_position == 2, ]
lines <- c(lines, "Runners-up in order: ", paste(sprintf("  %d %s (%s): %d pts, GD %+d, GF %d", ru$rank, ru$team, ru$group,
  ru$points, ru$gd, ru$gf), collapse = "\n"))
for (i in which(o$decided_by %in% c("away_goals", "wins", "away_wins", "disciplinary", "nations_league_rank"))) {
  lines <- c(lines, sprintf("  rank %d %s above %s on %s; official note: %s", o$rank[i], o$team[i], o$team[i + 1],
    o$decided_by[i], ov$tiebreak_note[i]))
}
w <- ov[match(o$team, ov$team), ]
diffs <- which(o$won != w$won | o$drawn != w$drawn | o$lost != w$lost | o$gf != w$gf | o$ga != w$ga)
lines <- c(lines, sprintf("Statistics differing from the published overall table: %s",
  if (length(diffs) == 0) "none" else paste(sprintf("%s (gf %d here, %d published)", o$team[diffs], o$gf[diffs], w$gf[diffs]), collapse = "; ")))
lines <- c(lines, "", if (all_ok) "RESULT: all ten tables and the overall order reproduced." else "RESULT: differences found.")
writeLines(lines, log_path, useBytes = FALSE)
cat(lines, sep = "\n")
