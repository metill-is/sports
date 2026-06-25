# GISKO retrospective backtest: how many points would we have if we had used
# the model's optimal picks from the start? Leak-free -- every match is scored
# from its frozen pre-match prediction (data/wc/accountability/prediction_log.json,
# fit_date <= match_date), against the actual result. Read-only; never on CI.
#   Rscript scripts/wc/gisko_backtest.R
suppressMessages(devtools::load_all(here::here()))
options(width = 120)

GROUP_ROUND_TOTAL <- 24L # group matches per matchday in the 48-team format
LEADER <- 164L

pl <- jsonlite::read_json(
  here::here("data", "wc", "accountability", "prediction_log.json")
)
mc <- pl$matches

flt <- list(sport = "football", country = "world", sex = "male")
res <- read_table("results", filter = flt)
res <- res[res$division == "FIFA World Cup" & res$season == 2026L, , drop = FALSE]
res_key <- paste(res$home_team, res$away_team, as.character(res$match_date))

raw <- utils::read.csv(
  here::here("data", "wc", "structure", "wc2026_schedule.csv"),
  check.names = FALSE, colClasses = "character", encoding = "UTF-8"
)
rmap <- stats::setNames(
  raw[["Round Number"]],
  .wc_pair_key(
    sports:::.wc_alias(trimws(raw[["Home Team"]])),
    sports:::.wc_alias(trimws(raw[["Away Team"]]))
  )
)

entries <- lapply(mc, function(m) {
  list(
    home = m$home, away = m$away, date = m$match_date,
    round = unname(rmap[.wc_pair_key(m$home, m$away)]),
    marg = gisko_marginals_from_log(m)
  )
})
all_exp <- vapply(
  entries, function(e) gisko_optimal_scoreline_marginal(e$marg)$exp_points, numeric(1)
)
keys <- vapply(entries, function(e) paste(e$home, e$away, e$date), character(1))
played_idx <- which(keys %in% res_key)

played <- do.call(rbind, lapply(played_idx, function(i) {
  e <- entries[[i]]
  r <- res[match(keys[i], res_key), ]
  tibble::tibble(
    round = ifelse(is.na(e$round), "G?", paste0("G", e$round)),
    label = paste(e$home, "v", e$away),
    marg = list(e$marg),
    act_home = as.integer(r$home_score), act_away = as.integer(r$away_score)
  )
}))

bt <- gisko_backtest_score(played)

# round completeness: a round's joker is only realised once that round's chosen
# match has been played; report joker only for fully-played matchdays.
by_round <- bt$by_round
by_round$complete <- by_round$n == GROUP_ROUND_TOTAL
realised_joker <- sum(by_round$joker_bonus[by_round$complete])
realised_total <- bt$base_total + realised_joker

cli::cli_h1("GISKO backtest -- model's optimal picks, leak-free")
cli::cli_text("Matches scored: {nrow(played)} of 72 group fixtures (pre-match predictions).")

cat("\n")
print(by_round[, c("round", "n", "base", "joker_match", "joker_bonus", "complete")])

cat("\nPer-match points distribution:\n")
print(table(points = bt$picks$points))
cli::cli_text(
  "Mean points/match: {round(mean(bt$picks$points), 2)} | exact (5): {sum(bt$picks$points == 5)} | outcome+ (>=3): {sum(bt$picks$points >= 3)}"
)

# pending group-round-3 joker (optimal target may be an unplayed match)
g3 <- which(vapply(entries, function(e) identical(e$round, "3"), logical(1)))
j3 <- g3[which.max(all_exp[g3])]
j3_played <- j3 %in% played_idx

cli::cli_h2("Totals")
cli::cli_text("Base match points (all {nrow(played)}): {bt$base_total}")
cli::cli_text("Realised joker bonus (matchdays 1-2): {realised_joker}")
cli::cli_alert_success("Realised so far: {realised_total} points")
cli::cli_text("Current leader: {LEADER}  ->  difference: {realised_total - LEADER}")
if (!j3_played) {
  cli::cli_text(
    "Matchday-3 joker is optimally pending on {entries[[j3]]$home} v {entries[[j3]]$away} (not yet played) -- not counted above."
  )
}
