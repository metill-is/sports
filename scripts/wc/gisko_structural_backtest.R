# GISKO structural backtest: score the model's PRE-TOURNAMENT group rankings
# (the last forecast before the 11 Jun 19:00 UTC structural deadline) against
# the pools that are now fully over -- the pool-placement and knockout-
# qualification points, like-for-like with the leaderboard. Read-only.
#   Rscript scripts/wc/gisko_structural_backtest.R
suppressMessages(devtools::load_all(here::here()))
options(width = 120)
set.seed(1) # determinism only for the rare all-equal group-table tiebreak

PRE_DEADLINE_REV <- "d4425564" # last groups.json before 11 Jun 19:00 UTC
LEADER_POOL <- 9L
LEADER_QUAL <- 10L

flt <- list(sport = "football", country = "world", sex = "male")
res <- read_table("results", filter = flt)
res <- res[res$division == "FIFA World Cup" & res$season == 2026L, , drop = FALSE]
s <- wc_structure()
res$grp <- s$group_of[res$home_team]
complete <- names(which(table(res$grp) == 6L))

gj_raw <- system2(
  "git",
  c(
    "-C", here::here(), "show",
    paste0(PRE_DEADLINE_REV, ":data/publish/world_cup/karla/groups.json")
  ),
  stdout = TRUE
)
gj <- jsonlite::parse_json(paste(gj_raw, collapse = "\n"))
forecast <- list()
for (grp in gj$groups) {
  m <- t(vapply(
    grp$teams,
    function(tm) c(tm$p_first, tm$p_second, tm$p_third, tm$p_fourth), numeric(4)
  ))
  rownames(m) <- vapply(grp$teams, function(tm) tm$team, character(1))
  forecast[[grp$group]] <- m
}

rows <- lapply(complete, function(g) {
  gm <- res[res$grp == g, ]
  actual <- sports:::.wc_group_table(
    s$groups[[g]],
    data.frame(
      home_team = gm$home_team, away_team = gm$away_team,
      home_score = gm$home_score, away_score = gm$away_score
    )
  )$team
  model <- gisko_optimal_group_order(forecast[[g]][s$groups[[g]], , drop = FALSE])
  tibble::tibble(
    pool = g,
    placement = sum(model == actual),
    qualification = 2L * length(intersect(model[1:2], actual[1:2])),
    model_order = paste(model, collapse = " > "),
    actual_order = paste(actual, collapse = " > ")
  )
})
tab <- do.call(rbind, rows)

cli::cli_h1("GISKO structural backtest -- {length(complete)} completed pools ({paste(complete, collapse=', ')})")
print(tab[, c("pool", "placement", "qualification")])
for (i in seq_len(nrow(tab))) {
  cli::cli_h3("Pool {tab$pool[i]}")
  cli::cli_text("model:  {tab$model_order[i]}")
  cli::cli_text("actual: {tab$actual_order[i]}")
}
cli::cli_h2("Totals (resolved pools)")
cli::cli_text("Pool placement:    model {sum(tab$placement)}/12   vs leader {LEADER_POOL}/12")
cli::cli_text("Qualification:     model {sum(tab$qualification)}/12   vs leader {LEADER_QUAL}/12")
cli::cli_alert_success(
  "Model structural so far: {sum(tab$placement) + sum(tab$qualification)} pts  (leader {LEADER_POOL + LEADER_QUAL})"
)
