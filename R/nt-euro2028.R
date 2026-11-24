#' @include nt-qualifying.R
NULL

# EURO 2028 qualifying: the facts of the 6 December 2026 draw, the loader for
# the drawn groups and fixture list, reserved host places and play-offs.
#
# Sources (read 2026-11-24):
# - Qualifying draw procedure, European Qualifiers 2026-28 (UEFA, PDF created
#   17 Nov 2026): pots and the interim overall 2026/27 Nations League ranking
#   (pot list numbered 1 to 54), draw conditions (pages 5-7), procedure (8).
#   https://editorial.uefa.com/resources/02ab-221968265425-d5c7eee03b62-1000/eq_draw_procedure.pdf
# - Regulations of the UEFA European Football Championship 2026-28, Articles
#   14.04 (reserved host places), 16 (play-off path formation), 17 (play-off
#   match system), 23 (overall rankings):
#   https://documents.uefa.com/r/Regulations-of-the-UEFA-European-Football-Championship-2026-28-Online
# - UEFA.com, 20 Nov 2026: Nations League league-phase placings and the
#   quarter-final draw (two-legged ties, the second-named side at home in the
#   second leg).
# The CSVs in data/nt/euro2028/ hold these facts; its README.md cites them.

# UEFA spellings that differ from the martj42 results file. Every other UEFA
# member is spelled the same in both.
.nt_uefa_names <- c(
  "Türkiye" = "Turkey",
  "Turkiye" = "Turkey",
  "Czechia" = "Czech Republic",
  "Bosnia-Herzegovina" = "Bosnia and Herzegovina",
  "Bosnia & Herzegovina" = "Bosnia and Herzegovina",
  "Rep. of Ireland" = "Republic of Ireland"
)

#' Map UEFA team names to the spelling of the martj42 results file.
#'
#' @param x Character vector of team names as UEFA writes them.
#' @return `x`, trimmed, with UEFA-only spellings (Türkiye, Czechia,
#'   Bosnia-Herzegovina and a few variants) replaced by the results file's.
#' @export
nt_uefa_to_martj42 <- function(x) {
  x <- trimws(as.character(x))
  hit <- x %in% names(.nt_uefa_names)
  x[hit] <- unname(.nt_uefa_names[x[hit]])
  x
}

.nt_read_csv <- function(path) {
  utils::read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = c("", "NA"))
}

#' Facts of the EURO 2028 qualifying draw.
#'
#' Reads `seeding.csv`, `quarter_finals.csv` and `draw_conditions.csv` from
#' `facts_dir` (see its README.md for the sources) and returns them with team
#' names in the martj42 spelling.
#'
#' @param facts_dir Directory holding the three CSVs.
#' @return List: `seeding` (tibble: `team`, `pot`, `nl_rank` (interim overall
#'   2026/27 Nations League ranking), `nl_league`, `nl_group`,
#'   `nl_group_position`, `nl_knockout` (`"QF"`, `"AB_PO"`, `"BC_PO"` or
#'   `""`), `host`), `quarter_finals` (`qf`, `winner_slot`, `defeated_slot`,
#'   `first_leg_home`, `second_leg_home`), `conditions` (`rule`, `team_a`,
#'   `team_b`), `hosts` and `separate` (the hosts plus Northern Ireland).
#' @export
nt_euro2028_facts <- function(facts_dir = here::here("data", "nt", "euro2028")) {
  seeding <- .nt_read_csv(file.path(facts_dir, "seeding.csv"))
  seeding$team <- nt_uefa_to_martj42(seeding$team)
  seeding$nl_knockout[is.na(seeding$nl_knockout)] <- ""
  seeding$host <- as.logical(seeding$host)
  seeding <- tibble::as_tibble(seeding[order(seeding$nl_rank), , drop = FALSE])
  qf <- .nt_read_csv(file.path(facts_dir, "quarter_finals.csv"))
  qf <- data.frame(
    qf = qf$qf,
    winner_slot = paste0("QF", qf$qf, " winner"),
    defeated_slot = paste0("QF", qf$qf, " defeated"),
    first_leg_home = nt_uefa_to_martj42(qf$first_leg_home),
    second_leg_home = nt_uefa_to_martj42(qf$second_leg_home),
    stringsAsFactors = FALSE
  )
  cond <- .nt_read_csv(file.path(facts_dir, "draw_conditions.csv"))
  cond$team_a <- nt_uefa_to_martj42(cond$team_a)
  cond$team_b <- ifelse(is.na(cond$team_b), NA_character_, nt_uefa_to_martj42(cond$team_b))
  unknown <- setdiff(
    stats::na.omit(c(qf$first_leg_home, qf$second_leg_home, cond$team_a, cond$team_b)),
    seeding$team
  )
  if (length(unknown) > 0L) {
    cli::cli_abort("Draw facts name team{?s} not in the seeding: {.val {unknown}}.")
  }
  list(
    seeding = seeding, quarter_finals = qf, conditions = cond,
    hosts = seeding$team[seeding$host],
    separate = cond$team_a[cond$rule == "separate"]
  )
}

# Canonical slot label ("QF2 winner") for a placeholder written by number or
# by pairing ("QF2 winners", "Winner QF2", "Norway/England defeated team",
# "England / Norway loser"); NA when `x` is not a placeholder.
.nt_parse_qf_slot <- function(x, qf) {
  low <- tolower(x)
  kind <- ifelse(
    grepl("\\bwinners?\\b", low, perl = TRUE), "winner",
    ifelse(grepl("\\b(defeated|losers?)\\b", low, perl = TRUE), "defeated", NA_character_)
  )
  num <- suppressWarnings(as.integer(sub(".*\\bqf\\s*([0-9]+).*", "\\1", low, perl = TRUE)))
  num[!grepl("\\bqf\\s*[0-9]+", low, perl = TRUE)] <- NA_integer_
  out <- rep(NA_character_, length(x))
  for (i in which(!is.na(kind))) {
    k <- num[i]
    if (is.na(k) && grepl("/", x[i])) {
      rest <- gsub("(?i)\\b(winners?|defeated|team|losers?)\\b", "", x[i], perl = TRUE)
      pair <- sort(nt_uefa_to_martj42(strsplit(rest, "/", fixed = TRUE)[[1]]))
      hit <- which(vapply(seq_len(nrow(qf)), function(j) {
        identical(sort(c(qf$first_leg_home[j], qf$second_leg_home[j])), pair)
      }, logical(1)))
      if (length(hit) == 1L) k <- qf$qf[hit]
    }
    if (!is.na(k) && k %in% qf$qf) out[i] <- paste0("QF", k, " ", kind[i])
  }
  out
}

# Venue flag from "home"/"neutral" or 1/0: 1 when the listed home side plays
# in its own country.
.nt_venue_flag <- function(v) {
  low <- tolower(trimws(as.character(v)))
  out <- ifelse(low %in% c("home", "1", "true"), 1L, ifelse(low %in% c("neutral", "0", "false"), 0L, NA_integer_))
  if (anyNA(out)) {
    bad <- unique(v[is.na(out)])
    cli::cli_abort("{.field venue} must be home/neutral or 1/0, not {.val {bad}}.")
  }
  out
}

#' Read the drawn EURO 2028 qualifying groups and fixture list.
#'
#' `groups` is a CSV with one row per drawn team: `group` (A to L), `team`
#' as UEFA writes it, and optionally `pot` (checked against the seeding).
#' The eight Nations League quarter-finalists enter the draw as one
#' placeholder per tie for its winner and one for its defeated team, written
#' `QF1 winner` / `QF1 defeated` (numbers as in `quarter_finals.csv`) or by
#' pairing, e.g. `Spain/France winner`.
#'
#' `fixtures` is an optional CSV of the fixture list: `group`, `home_team`,
#' `away_team`, `venue` (`home` or `neutral`, or 1/0), and optionally
#' `match_date`, `home_score`, `away_score`. Without it every pair plays home
#' and away, at home except for `neutral_home` teams, whose home matches are
#' at a neutral venue.
#'
#' @param groups Path to the groups CSV.
#' @param fixtures Optional path to the fixture-list CSV.
#' @param facts_dir Directory of the draw facts (see [nt_euro2028_facts()]).
#' @param fit Optional national-team fit: every drawn team must be in
#'   `fit$teams`.
#' @param neutral_home Teams whose home matches are played on neutral ground:
#'   Belarus and Ukraine (UEFA Executive Committee decisions, draw procedure
#'   page 6) and Israel (every competitive home match since June 2025 in
#'   Hungary, from the results file). Used only for generated fixtures.
#' @return List: `groups` (named list of slot labels: team names in the
#'   martj42 spelling or `QFk winner` / `QFk defeated`), `fixtures` (in slot
#'   labels, with an integer `venue`), `teams` (the 54 entrants),
#'   `quarter_finals`, `seeding`, `hosts`, `separate` and `conditions` (see
#'   [nt_euro2028_facts()]).
#' @export
nt_read_euro2028_draw <- function(groups, fixtures = NULL,
                                  facts_dir = here::here("data", "nt", "euro2028"),
                                  fit = NULL,
                                  neutral_home = c("Belarus", "Ukraine", "Israel")) {
  facts <- nt_euro2028_facts(facts_dir)
  qf <- facts$quarter_finals
  seed <- facts$seeding
  g <- .nt_read_csv(groups)
  need <- c("group", "team")
  if (!all(need %in% names(g))) cli::cli_abort("The groups file needs columns {.field {need}}.")
  slot <- .nt_parse_qf_slot(g$team, qf)
  slot[is.na(slot)] <- nt_uefa_to_martj42(g$team[is.na(slot)])
  g$slot <- slot
  g$group <- trimws(g$group)

  qf_teams <- c(qf$first_leg_home, qf$second_leg_home)
  placeholders <- c(qf$winner_slot, qf$defeated_slot)
  want <- c(setdiff(seed$team, qf_teams), placeholders)
  problems <- c(
    if (length(x <- unique(slot[duplicated(slot)])) > 0L) paste("repeated:", toString(x)),
    if (length(x <- setdiff(slot, c(want, qf_teams))) > 0L) paste("not an entrant:", toString(x)),
    if (length(x <- intersect(slot, qf_teams)) > 0L) {
      paste("quarter-finalists drawn as themselves, not as QF1-QF4 winner/defeated:", toString(x))
    },
    if (length(x <- setdiff(want, slot)) > 0L) paste("missing:", toString(x))
  )
  if (length(problems) > 0L) {
    cli::cli_abort(c("The groups file is not the 54-team draw.", stats::setNames(problems, rep("x", length(problems)))))
  }
  if (!is.null(g$pot)) {
    team_pot <- stats::setNames(seed$pot, seed$team)
    slot_pot <- ifelse(slot %in% placeholders, 1L, team_pot[slot])
    off <- !is.na(g$pot) & g$pot != slot_pot
    if (any(off)) {
      cli::cli_abort("Pot differs from the seeding for {.val {g$team[off]}}.")
    }
  }

  group_list <- split(g$slot, factor(g$group, levels = sort(unique(g$group))))
  group_list <- lapply(group_list, unname)
  teams <- seed$team
  if (!is.null(fit)) {
    absent <- setdiff(teams, fit$teams)
    if (length(absent) > 0L) {
      cli::cli_abort("Drawn team{?s} not in the fit: {.val {absent}}.")
    }
  }

  fx <- if (is.null(fixtures)) {
    out <- nt_qualifying_fixtures(group_list)
    out$venue <- ifelse(out$home_team %in% neutral_home, 0L, 1L)
    out
  } else {
    f <- .nt_read_csv(fixtures)
    need <- c("group", "home_team", "away_team", "venue")
    if (!all(need %in% names(f))) cli::cli_abort("The fixtures file needs columns {.field {need}}.")
    to_slot <- function(x) {
      s <- .nt_parse_qf_slot(x, qf)
      ifelse(is.na(s), nt_uefa_to_martj42(x), s)
    }
    data.frame(
      group = trimws(f$group),
      match_date = if (is.null(f$match_date)) NA_character_ else as.character(f$match_date),
      home_team = to_slot(f$home_team), away_team = to_slot(f$away_team),
      home_score = if (is.null(f$home_score)) NA_integer_ else as.integer(f$home_score),
      away_score = if (is.null(f$away_score)) NA_integer_ else as.integer(f$away_score),
      venue = .nt_venue_flag(f$venue),
      stringsAsFactors = FALSE
    )
  }
  fx <- .nt_check_qualifying_fixtures(fx, group_list)
  list(
    groups = group_list, fixtures = fx, teams = teams, quarter_finals = qf,
    seeding = seed, hosts = facts$hosts, separate = facts$separate,
    conditions = facts$conditions
  )
}
