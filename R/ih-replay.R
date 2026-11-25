# International handball replays: a past championship run through the live
# forecast path as if it were being played, one forecast per day, each checked
# against what actually happened. Used to shake down the refresh before a
# championship (.github/workflows/ih-tournament.yml, input `replay`) and by
# hand (scripts/ih/replay_tournament.R). A replay never publishes: its files go
# to a folder of its own.

#' Past championships that can be replayed, in the live championships' format
#'
#' Each entry is shaped like an `IH_TOURNAMENTS` one; its groups, schedule and
#' actual final standings are in `config/intl-handball/<id>/` (`groups.csv`,
#' `schedule.csv`, `standings.csv`, `knockout.csv`), built by
#' `scripts/ih/build_replay_spec.R` from the match table and the sourced facts.
#' @noRd
IH_REPLAYS <- list(
  euro2024w = list(
    title = "Women's EHF EURO 2024 (replay)", sex = "female", competition = "EURO", edition = 2024L,
    n_main = 2L, main = list(I = c("A", "B", "C"), II = c("D", "E", "F")),
    knockout = data.frame(
      code = c("SF1", "SF2", "P5", "Third", "Final"),
      round = c("SF", "SF", "Fifth", "Third", "Final"),
      match_no = c(61L, 62L, 63L, 64L, 65L),
      home = c("MR_I_1", "MR_I_2", "MR_I_3", "L_SF1", "W_SF1"),
      away = c("MR_II_2", "MR_II_1", "MR_II_3", "L_SF2", "W_SF2"),
      stringsAsFactors = FALSE
    ),
    rounds = c("MR", "SF", "Final", "Champion"),
    venue_country = c(Innsbruck = "Austria", Debrecen = "Hungary", Basel = "Switzerland", Vienna = "Austria"),
    start = as.Date("2024-11-28"), end = as.Date("2024-12-15"),
    publish = c("replay", "euro2024w"), focus_team = "Iceland", focus_round = "MR"
  ),
  wc2025m = list(
    title = "Men's IHF World Championship 2025 (replay)", sex = "male", competition = "WC", edition = 2025L,
    n_main = 3L, main = list(I = c("A", "B"), II = c("C", "D"), III = c("E", "F"), IV = c("G", "H")),
    knockout = data.frame(
      code = c("QF1", "QF2", "QF3", "QF4", "SF1", "SF2", "Third", "Final"),
      round = c("QF", "QF", "QF", "QF", "SF", "SF", "Third", "Final"),
      match_no = c(901L, 902L, 903L, 904L, 911L, 912L, 921L, 922L),
      # The 2025 semi-finals paired the winners of QF1 and QF2, and of QF3
      # and QF4 (unlike the 2027 schedule's QF1 and QF3).
      home = c("MR_I_1", "MR_III_1", "MR_II_1", "MR_IV_1", "W_QF1", "W_QF3", "L_SF1", "W_SF1"),
      away = c("MR_III_2", "MR_I_2", "MR_IV_2", "MR_II_2", "W_QF2", "W_QF4", "L_SF2", "W_SF2"),
      stringsAsFactors = FALSE
    ),
    rounds = c("MR", "QF", "SF", "Final", "Champion"),
    venue_country = stats::setNames(
      c("Croatia", "Croatia", "Croatia", "Denmark", "Norway", "Norway"),
      c("Zagreb", "Pore\u010d", "Vara\u017edin", "Herning", "Oslo", "B\u00e6rum")
    ),
    start = as.Date("2025-01-14"), end = as.Date("2025-02-02"),
    publish = c("replay", "wc2025m"), focus_team = "Iceland"
  )
)

#' The days a replay forecasts on
#'
#' @param spec [ih_tournament_spec()] of a replay.
#' @param results Its [ih_tournament_results()] (all of them).
#' @param which `"per-round"` (the day before the start, every match day and
#'   the day after the last match) or dates (a character vector or a
#'   comma-separated string).
#' @return Sorted unique dates.
#' @noRd
ih_replay_as_of_dates <- function(spec, results, which = "per-round") {
  if (length(which) == 1L && identical(which, "per-round")) {
    days <- c(spec$start - 1L, unique(results$match_date), max(results$match_date) + 1L)
  } else {
    days <- as.Date(trimws(unlist(strsplit(which, ",", fixed = TRUE))))
  }
  sort(unique(days))
}

#' The actual final standings and knockout winners of a replay
#' @return list(standings = data.frame(stage, group, position, team),
#'   knockout = data.frame(code, winner, loser)).
#' @noRd
ih_replay_facts <- function(id) {
  dir <- here::here("config", "intl-handball", id)
  list(
    standings = utils::read.csv(file.path(dir, "standings.csv"), stringsAsFactors = FALSE, encoding = "UTF-8"),
    knockout = utils::read.csv(file.path(dir, "knockout.csv"), stringsAsFactors = FALSE, encoding = "UTF-8")
  )
}

#' Check one day's replay forecast against what is known on that day
#'
#' @param dir Folder holding the day's five files.
#' @param spec [ih_tournament_spec()].
#' @param results The championship's results up to the day
#'   ([ih_tournament_results()]).
#' @param as_of The day.
#' @param facts [ih_replay_facts()].
#' @param tol Tolerance of a probability (the files round to 4 decimals).
#' @return data.frame `check`, `ok`, `detail`, one row per check.
#' @noRd
ih_replay_checks <- function(dir, spec, results, as_of, facts, tol = 2e-3) {
  rd <- function(f) jsonlite::read_json(file.path(dir, f))
  groups <- rd("groups.json")$groups
  preds <- rd("predictions.json")$matches
  place <- rd("tournament_placements.json")
  out <- list()
  add <- function(check, problems) {
    out[[length(out) + 1L]] <<- data.frame(
      check = check, ok = !length(problems),
      detail = if (length(problems)) paste(utils::head(problems, 5), collapse = "; ") else "",
      stringsAsFactors = FALSE
    )
  }
  # 1. Position probabilities: each team's sum to 1, each position's to 1.
  probs <- character()
  for (g in groups) {
    pp <- do.call(rbind, lapply(g$teams, function(t) unlist(t$p_pos)))
    if (any(abs(rowSums(pp) - 1) > tol)) probs <- c(probs, sprintf("group %s: a team's p_pos does not sum to 1", g$group))
    if (any(abs(colSums(pp) - 1) > tol * nrow(pp))) probs <- c(probs, sprintf("group %s: a position does not sum to 1", g$group))
  }
  add("position probabilities sum to 1", probs)

  # 2. A finished group is listed in its actual final order, certain.
  actual <- ih_actual_standings(spec, results)
  probs <- character()
  for (g in groups) {
    stage <- if (g$group %in% names(spec$main)) "main" else "preliminary"
    done <- if (stage == "main") isTRUE(actual$main[[g$group]]$complete) else isTRUE(actual$prelim[[g$group]]$complete)
    if (!done) next
    want <- facts$standings$team[facts$standings$stage == stage & facts$standings$group == g$group]
    want <- want[order(facts$standings$position[facts$standings$stage == stage & facts$standings$group == g$group])]
    have <- vapply(g$teams, function(t) t$team, "")
    if (!identical(have, want)) probs <- c(probs, sprintf("group %s: %s, actually %s", g$group, paste(have, collapse = ">"), paste(want, collapse = ">")))
    pp <- do.call(rbind, lapply(g$teams, function(t) unlist(t$p_pos)))
    if (any(abs(pp - diag(nrow(pp))) > tol)) probs <- c(probs, sprintf("group %s: finished but p_pos not 0/1", g$group))
  }
  add("finished groups in their actual order", probs)

  # 3. Who is out has 0 for every later round; who is through has 1.
  rec <- do.call(rbind, lapply(place$records, function(r) data.frame(team = r$team, round = r$round_name, p = r$probability)))
  reached <- ih_replay_reached(spec, results, facts)
  probs <- character()
  for (i in seq_len(nrow(reached))) {
    p <- rec$p[rec$team == reached$team[i] & rec$round == reached$round[i]]
    if (!length(p)) p <- 0
    if (abs(p - reached$value[i]) > tol) {
      probs <- c(probs, sprintf("%s %s: %.4f, known %d", reached$team[i], reached$round[i], p, reached$value[i]))
    }
  }
  add("eliminated teams 0, qualified teams 1", probs)

  # 4. The main round carries the right results.
  probs <- character()
  for (g in groups) {
    if (!g$group %in% names(spec$main)) next
    teams <- vapply(g$teams, function(t) t$team, "")
    feeder <- unlist(spec$groups[spec$main[[g$group]]], use.names = FALSE)
    same_group <- function(h, a) {
      vapply(seq_along(h), function(k) {
        any(vapply(spec$main[[g$group]], function(G) all(c(h[k], a[k]) %in% spec$groups[[G]]), logical(1)))
      }, logical(1))
    }
    r <- results[results$home_team %in% teams & results$away_team %in% teams &
      ((results$stage == "preliminary" & same_group(results$home_team, results$away_team)) | results$stage == "main"), , drop = FALSE]
    for (t in g$teams) {
      mine <- r[r$home_team == t$team | r$away_team == t$team, , drop = FALSE]
      gf <- sum(ifelse(mine$home_team == t$team, mine$home_60, mine$away_60))
      ga <- sum(ifelse(mine$home_team == t$team, mine$away_60, mine$home_60))
      pts <- sum(ifelse(mine$home_team == t$team, 2L * (mine$home_60 > mine$away_60), 2L * (mine$away_60 > mine$home_60)) + (mine$home_60 == mine$away_60))
      if (t$played != nrow(mine) || t$points != pts || t$gd != gf - ga) {
        probs <- c(probs, sprintf("%s in %s: played %d points %d gd %d, expected %d %d %d", t$team, g$group, t$played, t$points, t$gd, nrow(mine), pts, gf - ga))
      }
    }
    if (!all(teams %in% feeder)) probs <- c(probs, sprintf("group %s holds a team from outside its feeder groups", g$group))
  }
  add("main round carries the right results", probs)

  # 5. After the final: the champion is certain and every placement is 0/1.
  probs <- character()
  if (as_of > max(spec$knockout$match_date)) {
    champ <- facts$knockout$winner[facts$knockout$code == "Final"]
    pc <- vapply(place$summary, function(s) if (identical(s$team, champ)) s$p_champion else NA_real_, 0)
    if (!isTRUE(abs(max(pc, na.rm = TRUE) - 1) <= tol)) probs <- c(probs, sprintf("champion %s has p %.4f", champ, max(pc, na.rm = TRUE)))
    if (any(pmin(rec$p, 1 - rec$p) > tol)) probs <- c(probs, "a placement is not 0/1 after the final")
  }
  add("after the final: champion 1, placements 0/1", probs)

  # 6. Goal-difference bin 0 is the draw.
  probs <- character()
  for (m in preds) {
    b0 <- sum(vapply(m$goal_diff_distribution, function(b) if (identical(as.integer(b$diff), 0L)) b$p else 0, 0))
    if (abs(b0 - m$p_draw) > 1e-6) probs <- c(probs, sprintf("%s-%s: bin 0 %.4f, p_draw %.4f", m$home, m$away, b0, m$p_draw))
  }
  add("goal-difference bin 0 is p_draw", probs)

  do.call(rbind, out)
}

#' What is known on a day about who reached which round
#'
#' @return data.frame `team`, `round` (a code of `spec$rounds`), `value` (1
#'   reached, 0 out), for the rounds already decided on that day.
#' @noRd
ih_replay_reached <- function(spec, results, facts) {
  actual <- ih_actual_standings(spec, results)
  teams <- unlist(spec$groups, use.names = FALSE)
  rows <- list()
  put <- function(team, round, value) rows[[length(rows) + 1L]] <<- data.frame(team = team, round = round, value = value)
  prelim_done <- all(vapply(actual$prelim, function(x) isTRUE(x$complete), logical(1)))
  if (prelim_done) {
    mr <- facts$standings$team[facts$standings$stage == "main"]
    for (t in teams) put(t, "MR", as.integer(t %in% mr))
  }
  main_done <- prelim_done && length(actual$main) == length(spec$main) &&
    all(vapply(actual$main, function(x) isTRUE(x$complete), logical(1)))
  ko_round <- setdiff(spec$rounds, c("MR", "Champion"))[1]
  if (main_done) {
    st <- facts$standings[facts$standings$stage == "main", ]
    through <- st$team[st$position <= 2L]
    for (t in teams) put(t, ko_round, as.integer(t %in% through))
  }
  # Knockout matches played by the day decide the rounds after them.
  ko <- spec$knockout
  played <- results[results$stage == "knockout", , drop = FALSE]
  later <- c(QF = "SF", SF = "Final", Final = "Champion")
  for (i in seq_len(nrow(ko))) {
    nxt <- later[ko$round[i]]
    if (is.na(nxt)) next
    f <- facts$knockout[facts$knockout$code == ko$code[i], ]
    if (!nrow(f)) next
    hit <- played$match_date == ko$match_date[i] & ((played$home_team == f$winner & played$away_team == f$loser) |
      (played$home_team == f$loser & played$away_team == f$winner))
    if (!any(hit)) next
    put(f$winner, unname(nxt), 1L)
    put(f$loser, unname(nxt), 0L)
  }
  if (!length(rows)) {
    return(data.frame(team = character(), round = character(), value = integer()))
  }
  do.call(rbind, rows)
}
