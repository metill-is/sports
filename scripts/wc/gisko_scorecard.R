# GISKO scorecard: how the model's optimal picks would be doing, leak-free,
# vs the live leader. Re-run any time as the tournament progresses.
#
# UPDATE WORKFLOW
#   1. git pull            # fresh results + frozen pre-match predictions (cron)
#   2. Rscript scripts/wc/gisko_scorecard.R \
#        --leader-match 145 --leader-pool 9 --leader-qual 10
#      (read the leader's three numbers off the gisko.is leaderboard; they are
#       the only things that change by hand. Defaults are the 2026-06-25 values.)
#   For the HTML version: scripts/wc/gisko_report.R (same args + --field-size).
#
# Auto-detects newly played matches and completed pools. Read-only; never on CI.
# Scope (group stage): match scoring covers every played match; structural
# scoring covers pool placement + R32 qualification for completed pools. Once
# the knockout begins, knockout-round jokers and reach-points are a further
# extension.
suppressMessages(devtools::load_all(here::here()))
options(width = 120)
set.seed(1) # determinism only for the rare all-equal group-table tiebreak

args <- commandArgs(trailingOnly = TRUE)
getarg <- function(flag, default) {
  i <- which(args == flag)
  if (length(i) == 1L) as.integer(args[i + 1L]) else default
}
LEADER_MATCH <- getarg("--leader-match", 145L)
LEADER_POOL <- getarg("--leader-pool", 9L)
LEADER_QUAL <- getarg("--leader-qual", 10L)

d <- gisko_scorecard_data()
leader_total <- LEADER_MATCH + LEADER_POOL + LEADER_QUAL
scorecard <- tibble::tibble(
  category = c("Match predictions", "Pool placement", "Qualification (R32)", "TOTAL"),
  model = c(d$match_total, d$pool_pts, d$qual_pts, d$model_total),
  leader = c(LEADER_MATCH, LEADER_POOL, LEADER_QUAL, leader_total)
)
scorecard$gap <- scorecard$model - scorecard$leader

cli::cli_h1("GISKO scorecard -- model's optimal picks vs leader (leak-free)")
print(scorecard)

cli::cli_h2("Match detail ({d$n_played} played; mean {round(mean(d$picks$points),2)} pts; {sum(d$picks$points==5)} exact)")
print(d$by_round[, c("round", "n", "base", "joker_match", "joker_bonus", "complete")])
if (!is.null(d$pending_joker)) {
  cli::cli_text("Matchday-3 joker optimally pending on {d$pending_joker$home} v {d$pending_joker$away} (unplayed).")
}
if (!is.null(d$structural)) {
  cli::cli_h2("Structural detail ({length(d$complete_pools)} completed pools; {length(d$complete_pools)*4} placement + {length(d$complete_pools)*4} qualification points available)")
  print(d$structural[, c("pool", "placement", "qualification")])
}

dir.create(here::here("data", "wc", "gisko"), showWarnings = FALSE, recursive = TRUE)
jsonlite::write_json(
  list(scorecard = scorecard, by_round = d$by_round, structural = d$structural),
  here::here("data", "wc", "gisko", "scorecard.json"),
  auto_unbox = TRUE, pretty = TRUE, dataframe = "rows", null = "null"
)
cli::cli_alert_success("Model {d$model_total} vs leader {leader_total}. Wrote data/wc/gisko/scorecard.json")
