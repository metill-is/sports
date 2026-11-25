#!/usr/bin/env Rscript
# Build a replay's spec (config/intl-handball/<id>/) from the match table and
# a facts file sourced from the championship's own pages (groups and their
# venues, final standings, knockout results; see the facts' "sources").
#
# Writes groups.csv and schedule.csv in the live championships' format, and
# standings.csv and knockout.csv, the actual outcome the replay's checks
# compare against (R/ih-replay.R). Refuses to write when the facts and the
# match table disagree (a match in no group, a count that is off).
#
# Usage: IH_DATA_DIR=... Rscript scripts/ih/build_replay_spec.R --replay euro2024w --facts FILE
options(width = 120)
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))
args <- commandArgs(trailingOnly = TRUE)
arg <- function(name, default = NULL) {
  i <- match(paste0("--", name), args)
  if (is.na(i)) default else args[[i + 1L]]
}
id <- arg("replay")
spec <- IH_REPLAYS[[id]]
if (is.null(spec)) cli::cli_abort("--replay must be one of {.val {names(IH_REPLAYS)}}.")
facts <- jsonlite::read_json(arg("facts"))
names_tbl <- ih_team_names()
official <- function(team) {
  out <- names_tbl$official[match(team, names_tbl$team)]
  if (anyNA(out)) cli::cli_abort("No row in teams.csv for {.val {unique(team[is.na(out)])}}.")
  out
}

m <- ih_read_matches(spec$sex)
c_all <- m[m$competition == spec$competition & m$edition == spec$edition &
  !grepl("qualif", m$block, ignore.case = TRUE) & m$match_date >= spec$start & m$match_date <= spec$end, ]
group_of <- unlist(lapply(names(facts$groups), function(g) stats::setNames(rep(g, 4), unlist(facts$groups[[g]]$standings))))
main_of <- unlist(lapply(names(facts$main_round), function(G) {
  stats::setNames(rep(G, length(facts$main_round[[G]]$standings)), unlist(facts$main_round[[G]]$standings))
}))
teams <- names(group_of)
stopifnot(length(teams) == length(unique(teams)), all(c(c_all$home_team, c_all$away_team) %in% teams))
slot_of <- unlist(lapply(names(facts$groups), function(g) {
  s <- unlist(facts$groups[[g]]$standings)[seq_len(spec$n_main)]
  stats::setNames(paste0(seq_len(spec$n_main), g), s)
}))

ko <- do.call(rbind, lapply(facts$knockout, as.data.frame))
first_ko <- min(as.Date(ko$date))
pre <- c_all[c_all$match_date < first_ko & group_of[c_all$home_team] == group_of[c_all$away_team], ]
main <- c_all[c_all$match_date < first_ko & group_of[c_all$home_team] != group_of[c_all$away_team] &
  !is.na(main_of[c_all$home_team]) & !is.na(main_of[c_all$away_team]) &
  main_of[c_all$home_team] == main_of[c_all$away_team], ]
n_groups <- length(facts$groups)
n_pre <- n_groups * 6L
n_main_new <- length(facts$main_round) * (choose(length(facts$main_round[[1]]$standings), 2) -
  length(spec$main[[1]]) * choose(spec$n_main, 2))
if (nrow(pre) != n_pre) cli::cli_abort("{nrow(pre)} preliminary matches in the table, {n_pre} expected.")
if (nrow(main) != n_main_new) cli::cli_abort("{nrow(main)} main-round matches in the table, {n_main_new} expected.")
last_pre <- max(pre$match_date)
if (min(main$match_date) <= last_pre) cli::cli_abort("A main-round match is dated on or before the last preliminary day.")

city <- function(x) if (is.null(x)) NA_character_ else x
tz <- "CET (UTC+1)"
sched <- rbind(
  data.frame(
    match_no = seq_len(nrow(pre)), stage = "Preliminary Round", group = group_of[pre$home_team],
    date = format(pre$match_date), local_time = "", tz = tz,
    venue_city = vapply(group_of[pre$home_team], function(g) city(facts$groups[[g]]$venue_city), ""),
    home_team = official(pre$home_team), away_team = official(pre$away_team)
  ),
  data.frame(
    match_no = 100L + seq_len(nrow(main)), stage = "Main Round", group = main_of[main$home_team],
    date = format(main$match_date), local_time = "", tz = tz,
    venue_city = vapply(main_of[main$home_team], function(G) city(facts$main_round[[G]]$venue_city), ""),
    home_team = slot_of[main$home_team], away_team = slot_of[main$away_team]
  ),
  data.frame(
    match_no = spec$knockout$match_no[match(ko$code, spec$knockout$code)], stage = ko$code, group = "",
    date = ko$date, local_time = "", tz = tz, venue_city = ko$venue_city,
    home_team = spec$knockout$home[match(ko$code, spec$knockout$code)],
    away_team = spec$knockout$away[match(ko$code, spec$knockout$code)]
  )
)
if (anyNA(sched$match_no) || anyNA(sched$venue_city)) cli::cli_abort("A knockout code or a venue is missing from the facts.")
unknown_city <- setdiff(sched$venue_city, names(spec$venue_country))
if (length(unknown_city)) cli::cli_abort("No venue_country in IH_REPLAYS for {.val {unknown_city}}.")

groups <- do.call(rbind, lapply(names(facts$groups), function(g) {
  s <- unlist(facts$groups[[g]]$standings)
  data.frame(group = g, position = seq_along(s), team = official(s), venue_city = facts$groups[[g]]$venue_city)
}))
standings <- rbind(
  do.call(rbind, lapply(names(facts$groups), function(g) {
    s <- unlist(facts$groups[[g]]$standings)
    data.frame(stage = "preliminary", group = g, position = seq_along(s), team = s)
  })),
  do.call(rbind, lapply(names(facts$main_round), function(G) {
    s <- unlist(facts$main_round[[G]]$standings)
    data.frame(stage = "main", group = G, position = seq_along(s), team = s)
  }))
)
knock <- data.frame(code = ko$code, winner = ko$winner, loser = ifelse(ko$winner == ko$home, ko$away, ko$home))

dir <- here::here("config", "intl-handball", id)
dir.create(dir, showWarnings = FALSE)
w <- function(x, f) utils::write.csv(x, file.path(dir, f), row.names = FALSE, fileEncoding = "UTF-8", na = "")
w(groups, "groups.csv")
w(sched[order(sched$date, sched$match_no), ], "schedule.csv")
w(standings, "standings.csv")
w(knock, "knockout.csv")
cli::cli_inform("Wrote {.path {dir}}: {nrow(pre)} preliminary, {nrow(main)} main-round and {nrow(ko)} knockout matches.")
