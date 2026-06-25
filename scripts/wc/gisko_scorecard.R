# GISKO scorecard: how the model's optimal picks would be doing, leak-free,
# vs the live leader. Re-run this any time as the tournament progresses.
#
# UPDATE WORKFLOW
#   1. git pull            # fresh results + frozen pre-match predictions (cron)
#   2. Rscript scripts/wc/gisko_scorecard.R \
#        --leader-match 145 --leader-pool 9 --leader-qual 10
#      (read the leader's three numbers off the gisko.is leaderboard; they are
#       the only things that change by hand. Defaults are the 2026-06-25 values.)
#
# It auto-detects newly played matches and newly completed pools. Read-only;
# never on CI.
#
# Scope note (group stage): match scoring covers every played match; structural
# scoring covers pool placement + R32 qualification for completed pools. Once
# the knockout begins, knockout-round jokers and reach-points (R16/QF/SF/champion)
# are a further extension.
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

GROUP_ROUND_TOTAL <- 24L
PRE_DEADLINE_REV <- "d4425564" # last groups.json before the 11 Jun 19:00 deadline

flt <- list(sport = "football", country = "world", sex = "male")
res <- read_table("results", filter = flt)
res <- res[res$division == "FIFA World Cup" & res$season == 2026L, , drop = FALSE]
res_key <- paste(res$home_team, res$away_team, as.character(res$match_date))
s <- wc_structure()
res$grp <- s$group_of[res$home_team]

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

# ---- match backtest -------------------------------------------------------
pl <- jsonlite::read_json(
  here::here("data", "wc", "accountability", "prediction_log.json")
)
entries <- lapply(pl$matches, function(m) {
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
    label = paste(e$home, "v", e$away), marg = list(e$marg),
    act_home = as.integer(r$home_score), act_away = as.integer(r$away_score)
  )
}))
bt <- gisko_backtest_score(played)
by_round <- bt$by_round
by_round$complete <- by_round$n == GROUP_ROUND_TOTAL
match_total <- bt$base_total + sum(by_round$joker_bonus[by_round$complete])
g3 <- which(vapply(entries, function(e) identical(e$round, "3"), logical(1)))
pend <- entries[[g3[which.max(all_exp[g3])]]]
pend_played <- (g3[which.max(all_exp[g3])]) %in% played_idx

# ---- structural backtest --------------------------------------------------
complete_pools <- names(which(table(res$grp) == 6L))
gj <- jsonlite::parse_json(paste(system2(
  "git",
  c(
    "-C", here::here(), "show",
    paste0(PRE_DEADLINE_REV, ":data/publish/world_cup/karla/groups.json")
  ),
  stdout = TRUE
), collapse = "\n"))
forecast <- list()
for (grp in gj$groups) {
  m <- t(vapply(
    grp$teams,
    function(tm) c(tm$p_first, tm$p_second, tm$p_third, tm$p_fourth), numeric(4)
  ))
  rownames(m) <- vapply(grp$teams, function(tm) tm$team, character(1))
  forecast[[grp$group]] <- m
}
struct <- if (length(complete_pools) == 0L) {
  NULL
} else {
  do.call(rbind, lapply(complete_pools, function(g) {
    gm <- res[res$grp == g, ]
    actual <- sports:::.wc_group_table(s$groups[[g]], data.frame(
      home_team = gm$home_team, away_team = gm$away_team,
      home_score = gm$home_score, away_score = gm$away_score
    ))$team
    model <- gisko_optimal_group_order(forecast[[g]][s$groups[[g]], , drop = FALSE])
    tibble::tibble(
      pool = g, placement = sum(model == actual),
      qualification = 2L * length(intersect(model[1:2], actual[1:2])),
      model_order = paste(model, collapse = " > "),
      actual_order = paste(actual, collapse = " > ")
    )
  }))
}
pool_pts <- if (is.null(struct)) 0L else sum(struct$placement)
qual_pts <- if (is.null(struct)) 0L else sum(struct$qualification)

# ---- scorecard ------------------------------------------------------------
model_total <- match_total + pool_pts + qual_pts
leader_total <- LEADER_MATCH + LEADER_POOL + LEADER_QUAL
scorecard <- tibble::tibble(
  category = c("Match predictions", "Pool placement", "Qualification (R32)", "TOTAL"),
  model = c(match_total, pool_pts, qual_pts, model_total),
  leader = c(LEADER_MATCH, LEADER_POOL, LEADER_QUAL, leader_total)
)
scorecard$gap <- scorecard$model - scorecard$leader

cli::cli_h1("GISKO scorecard -- model's optimal picks vs leader (leak-free)")
print(scorecard)

cli::cli_h2("Match detail ({nrow(played)} played; mean {round(mean(bt$picks$points),2)} pts; {sum(bt$picks$points==5)} exact)")
print(by_round[, c("round", "n", "base", "joker_match", "joker_bonus", "complete")])
if (!pend_played) {
  cli::cli_text("Matchday-3 joker optimally pending on {pend$home} v {pend$away} (unplayed).")
}
if (!is.null(struct)) {
  cli::cli_h2("Structural detail ({length(complete_pools)} completed pools; {length(complete_pools)*4} placement + {length(complete_pools)*4} qualification points available)")
  print(struct[, c("pool", "placement", "qualification")])
}

dir.create(here::here("data", "wc", "gisko"), showWarnings = FALSE, recursive = TRUE)
jsonlite::write_json(
  list(scorecard = scorecard, by_round = by_round, structural = struct),
  here::here("data", "wc", "gisko", "scorecard.json"),
  auto_unbox = TRUE, pretty = TRUE, dataframe = "rows", null = "null"
)
cli::cli_alert_success("Model {model_total} vs leader {leader_total}. Wrote data/wc/gisko/scorecard.json")
