# Prototype, step 3 of the period-score ingest: parse every fetched game card and attach
# regulation-time scores to the stored results. Output: <OUT>/kki/periods.tsv (one row
# per game id) and <OUT>/kki/periods_matched.rds (stored result rows + regulation scores).
# Usage: Rscript -e 'source("docs/reports/2026-11-15-hb-bb-v2/kki_periods.R")'
source("docs/reports/2026-11-15-hb-bb-v2/common.R")
kki <- file.path(OUT, "kki")
games <- utils::read.delim(file.path(kki, "games.tsv"), encoding = "UTF-8", quote = "", stringsAsFactors = FALSE)
games$match_date <- as.Date(games$match_date)
parsed <- lapply(games$game_id, function(id) {
  f <- file.path(kki, "raw", paste0(id, ".js"))
  if (!file.exists(f) || file.size(f) == 0) {
    return(parse_baskethotel_game_periods(""))
  }
  parse_baskethotel_game_periods(readChar(f, file.size(f), useBytes = TRUE))
})
per <- cbind(games, do.call(rbind, parsed))
per$card_matches_export <- !is.na(per$home_final) & per$home_final == per$home_score & per$away_final == per$away_score
per$ok <- per$ok & per$card_matches_export
per$overtime <- per$ok & per$overtime %in% TRUE
per$reg_tie <- per$overtime
utils::write.table(per, file.path(kki, "periods.tsv"), sep = "\t", quote = FALSE, row.names = FALSE, fileEncoding = "UTF-8")

cat(sprintf("game cards: %d, parsed ok: %d (%.1f %%), not ok: %d\n", nrow(per), sum(per$ok), 100 * mean(per$ok), sum(!per$ok)))
cat("not ok by reason: no period block", sum(per$n_periods == 0), "| fewer than 4 periods", sum(per$n_periods > 0 & per$n_periods < 4),
    "| final is neither the quarter sum nor a broken tie, or card differs from export", sum(per$n_periods >= 4 & !per$ok), "\n")
print(table(n_periods = per$n_periods, ok = per$ok))
cat("ok share by sex and season:\n")
print(round(tapply(per$ok, list(per$sex, per$season), mean), 3))
cat("ok share by sex and division:\n")
print(round(tapply(per$ok, list(per$sex, per$division), mean), 3))

# Attach to the stored results. The stored names carry the ingest aliases, so the join is
# on what both sides share exactly: sex, date and the final score; team names are then
# compared as a check, not as a key.
stored <- do.call(rbind, lapply(c("bb_m", "bb_f"), function(cell) {
  r <- cell_results(cell)
  r[!is.na(r$home_score), ]
}))
key <- function(x) paste(x$sex, x$match_date, x$home_score, x$away_score)
dup_stored <- key(stored) %in% key(stored)[duplicated(key(stored))]
dup_per <- key(per) %in% key(per)[duplicated(key(per))]
cat(sprintf("stored rows %d (ambiguous key %d); card rows %d (ambiguous key %d)\n", nrow(stored), sum(dup_stored), nrow(per), sum(dup_per)))
aliases <- load_leagues()$basketball_iceland$data_source$team_aliases
canon <- function(name, sex) {
  a <- unlist(aliases[[sex]])
  ifelse(name %in% names(a), unname(a[name]), name)
}
per$home_canon <- ifelse(per$sex == "male", canon(per$home_team, "male"), canon(per$home_team, "female"))
per$away_canon <- ifelse(per$sex == "male", canon(per$away_team, "male"), canon(per$away_team, "female"))
full_key <- function(x, h, a) paste(x$sex, x$match_date, h, a)
hit <- match(full_key(stored, stored$home_team, stored$away_team), full_key(per, per$home_canon, per$away_canon))
loose <- is.na(hit) & !dup_stored
hit[loose] <- match(key(stored)[loose], ifelse(dup_per, NA, key(per)))
m <- cbind(stored, per[hit, c("game_id", "n_periods", "home_reg", "away_reg", "home_ot", "away_ot", "periods", "ok", "overtime", "reg_tie")])
m$ok[is.na(m$ok)] <- FALSE
m$score_agrees <- !is.na(hit) & per$home_score[hit] == m$home_score & per$away_score[hit] == m$away_score
m$ok <- m$ok & m$score_agrees
saveRDS(m, file.path(kki, "periods_matched.rds"))
forfeit <- .is_forfeit(m, "basketball")
cat(sprintf("stored played rows: %d; matched to a game card: %d; with usable regulation score: %d (%.1f %%); forfeits among the rest: %d\n",
            nrow(m), sum(!is.na(hit)), sum(m$ok), 100 * mean(m$ok), sum(forfeit & !m$ok)))
cat("usable share by sex, division, season:\n")
print(round(tapply(m$ok, list(paste(m$sex, m$division), m$season), mean), 3))
u <- m[m$ok, ]
# WHY no count of double overtimes: a game card lists the four quarters only, so the
# number of overtime periods is not recorded. Overtime points are: one five-minute period
# is about 20 to 25 points, so 45 or more points most likely means two or more periods.
cat("\nregulation ties (= overtime games) by sex and division: n, share, and overtime games with 45 or more overtime points\n")
print(do.call(rbind, lapply(split(u, paste(u$sex, u$division)), function(x) data.frame(
  n = nrow(x), overtime = sum(x$overtime), share = mean(x$overtime), reg_tie = sum(x$reg_tie),
  ot_points_45_up = sum(x$overtime & x$home_ot + x$away_ot >= 45), home_wins_ot = sum(x$overtime & x$home_score > x$away_score)
))), digits = 3)
cat("regulation margin -3..3 counts (all usable games):\n")
print(table(factor(pmax(pmin(u$home_reg - u$away_reg, 4), -4), levels = -4:4), u$sex))
ot <- u[u$overtime, ]
cat(sprintf("overtime games: %d; overtime points: mean total %.1f, sd of overtime margin %.2f; home side won %d (%.1f %%)\n",
            nrow(ot), mean(ot$home_ot + ot$away_ot), sd(ot$home_ot - ot$away_ot),
            sum(ot$home_ot > ot$away_ot), 100 * mean(ot$home_ot > ot$away_ot)))
cat("overtime total points, binned by 5 (one 5-minute period is about 20 to 25 points):\n")
print(table(pmin(5 * floor((ot$home_ot + ot$away_ot) / 5), 50)))
