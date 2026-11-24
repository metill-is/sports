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
#'   [nt_euro2028_facts()]), and `neutral_home`.
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
    conditions = facts$conditions, neutral_home = neutral_home
  )
}

# ---- Reserved host places and play-offs ------------------------------------

# Play-off field of one simulation. Article 14.04: two places are reserved
# for hosts that do not qualify directly, the two best-ranked by Article 23 if
# more than two; a host among the lowest runners-up takes its reserved place
# instead. Article 16.01: 8 play-off teams if both or neither reserved place
# is used, 12 if one is. Article 16.02: the remaining runners-up, then group
# winners of Nations League A, B and C not already through (by the interim
# overall ranking), then the best-ranked League D group winner unless it is
# through, then the best-ranked teams not yet through.
#
# `direct` are the 20 direct qualifiers, `ru_out` the other runners-up in
# Article 23 order, `hosts_out` the hosts not through directly in Article 23
# order. Returns `reserved`, `runners_up` and `nations_league` (each in
# seeding order) and `format` ("paths" or "ties").
.nt_euro_playoff_field <- function(direct, ru_out, hosts_out, seeding) {
  reserved <- utils::head(hosts_out, 2L)
  runners_up <- setdiff(ru_out, reserved)
  n_teams <- if (length(reserved) == 1L) 12L else 8L
  n_nl <- n_teams - length(runners_up)
  taken <- c(direct, reserved, runners_up)
  by_rank <- seeding[order(seeding$nl_rank), , drop = FALSE]
  winner <- by_rank$nl_group_position == 1L
  abc <- by_rank$team[winner & by_rank$nl_league %in% c("A", "B", "C")]
  nl <- utils::head(setdiff(abc, taken), n_nl)
  if (length(nl) < n_nl) {
    best_d <- by_rank$team[winner & by_rank$nl_league == "D"][1]
    if (!is.na(best_d) && !best_d %in% taken) nl <- c(nl, best_d)
  }
  if (length(nl) < n_nl) {
    nl <- c(nl, utils::head(setdiff(by_rank$team, c(taken, nl)), n_nl - length(nl)))
  }
  list(
    reserved = reserved, runners_up = runners_up,
    nations_league = nl[order(match(nl, by_rank$team))],
    format = if (length(reserved) == 0L) "ties" else "paths"
  )
}

.nt_cache <- new.env(parent = emptyenv())

# All permutations of 1..k, one per row.
.nt_perms <- function(k) {
  if (k == 1L) return(matrix(1L, 1, 1))
  sub <- .nt_perms(k - 1L)
  do.call(rbind, lapply(seq_len(k), function(i) cbind(i, matrix(setdiff(seq_len(k), i)[sub], nrow(sub)))))
}

# Every way to place `n_pots` pots of `size` teams into `size` rows, the first
# pot's order fixed (rows are interchangeable): a matrix, one layout per row,
# of the row each participant (in pot order) goes to.
.nt_pot_layouts <- function(size, n_pots) {
  key <- paste(size, n_pots)
  if (!is.null(.nt_cache[[key]])) return(.nt_cache[[key]])
  perm <- .nt_perms(size)
  combos <- as.matrix(expand.grid(rep(list(seq_len(nrow(perm))), n_pots - 1L)))
  out <- matrix(rep(seq_len(size), each = nrow(combos)), nrow(combos), size)
  for (q in seq_len(n_pots - 1L)) out <- cbind(out, perm[combos[, q], , drop = FALSE])
  .nt_cache[[key]] <- out
  out
}

# Play-off draw (Articles 16.03 to 16.05). `participants` are in seeding
# order. Paths: four pots of 2 or 3 (pot 1 the best-seeded), one team of each
# per path; returns a paths x 4 matrix (column = pot). Ties: seeded (first
# four) against unseeded; returns a 4 x 2 matrix. Hosts go to separate paths
# or ties and Northern Ireland, if possible, to one without a host. Each
# allocation that keeps these conditions is equally likely: UEFA's draw may
# add conditions approved by its Executive Committee (16.03), not modelled.
.nt_euro_playoff_draw <- function(participants, format, hosts,
                                  northern_ireland = "Northern Ireland") {
  n <- length(participants)
  size <- if (format == "ties") 4L else n %/% 4L
  n_pots <- n %/% size
  layouts <- .nt_pot_layouts(size, n_pots)
  ok <- rep(TRUE, nrow(layouts))
  hp <- which(participants %in% hosts)
  if (length(hp) >= 2L) {
    ok <- apply(layouts[, hp, drop = FALSE], 1L, function(r) !anyDuplicated(r))
  }
  ni <- which(participants == northern_ireland)
  if (length(ni) == 1L && length(hp) >= 1L) {
    apart <- ok & !apply(layouts[, c(ni, hp), drop = FALSE], 1L, function(r) r[1] %in% r[-1])
    if (any(apart)) ok <- apart
  }
  if (!any(ok)) ok[] <- TRUE
  pick <- which(ok)
  row <- layouts[pick[sample.int(length(pick), 1L)], ]
  out <- matrix(NA_character_, size, n_pots)
  out[cbind(row, rep(seq_len(n_pots), each = size))] <- participants
  out
}

#' Simulate the EURO 2028 reserved host places and play-offs.
#'
#' Follows Articles 14.04, 16 and 17 of the 2026-28 regulations for each
#' simulated qualifying group stage: hosts that do not qualify directly take
#' up to two reserved places (the two best-ranked by Article 23 if more than
#' two); the play-off field (8 or 12 teams) is the remaining runners-up and
#' Nations League group winners not yet through, then the best League D
#' group winner and the best-ranked teams by the interim overall Nations
#' League ranking; paths are single-leg semi-finals (pot 1 and pot 2 sides at
#' home) and a final at the home of the winner of the pot 1 semi-final, and
#' four home-and-away ties if no reserved place is used. Each simulation uses
#' the posterior draw of its group stage. Matches level after 90 minutes go
#' to extra time and penalties ([nt_simulate_knockout()],
#' [nt_simulate_two_legged()]).
#'
#' @param qual Output of [nt_simulate_qualifying()].
#' @param fit The fit `qual` was simulated from.
#' @param seeding Seeding table (see [nt_euro2028_facts()]): `team`,
#'   `nl_rank`, `nl_league`, `nl_group_position`, for every team in `qual`.
#' @param hosts The host teams with reserved places.
#' @param northern_ireland Team kept away from hosts in the play-off draw.
#' @param n_best_runners_up Runners-up that qualify directly.
#' @param neutral_home Teams whose home matches are on neutral ground.
#' @param et_factor,shootout_p See [nt_simulate_knockout()].
#' @param seed Optional seed.
#' @return List: `teams` (tibble: `team`, `p_host_place`, `p_playoff`,
#'   `p_playoff_runner_up`, `p_playoff_nations_league`, `p_qualify_playoff`,
#'   `p_qualify`), n_sims x teams logical matrices `qualified`, `host_place`,
#'   `playoff`, `playoff_winner`, the integer matrix `playoff_seed` (seeding
#'   position among the play-off teams, `NA` outside them) and
#'   `n_host_places` (reserved places used per simulation).
#' @export
nt_simulate_euro_playoffs <- function(qual, fit, seeding, hosts,
                                      northern_ireland = "Northern Ireland",
                                      n_best_runners_up = 8L,
                                      neutral_home = c("Belarus", "Ukraine", "Israel"),
                                      et_factor = 1 / 3, shootout_p = 0.5, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  teams <- colnames(qual$positions)
  missing <- setdiff(teams, seeding$team)
  if (length(missing) > 0L) cli::cli_abort("No seeding for {.val {missing}}.")
  seeding <- seeding[seeding$team %in% teams, , drop = FALSE]
  hosts <- intersect(hosts, teams)
  n <- qual$n_sims
  sims <- seq_len(n)
  n_teams <- length(teams)
  pos <- qual$positions
  ru <- qual$runner_up
  rr <- qual$runner_up_rank
  n_groups <- ncol(ru)

  direct <- pos == 1L
  ru_direct <- rr <= n_best_runners_up
  direct[cbind(row(ru)[ru_direct], match(ru[ru_direct], teams))] <- TRUE
  ord <- t(apply(rr, 1L, order))
  ru_sorted <- matrix(ru[cbind(rep(sims, n_groups), c(ord))], n, n_groups)
  ru_out <- ru_sorted[, seq.int(n_best_runners_up + 1L, n_groups), drop = FALSE]
  ov <- qual$overall
  host_rank <- .nt_lexi_rank(c(
    list(-pos[, hosts, drop = FALSE]),
    lapply(ov[c("points", "gd", "gf", "away_gf", "won", "away_won", "tiebreak")], function(m) m[, hosts, drop = FALSE])
  ))

  host_place <- playoff <- via_ru <- matrix(FALSE, n, n_teams, dimnames = list(NULL, teams))
  playoff_seed <- matrix(NA_integer_, n, n_teams, dimnames = list(NULL, teams))
  n_res <- integer(n)
  paths <- ties <- vector("list", n)
  for (s in sims) {
    out_h <- hosts[!direct[s, hosts]]
    out_h <- out_h[order(host_rank[s, !direct[s, hosts]])]
    f <- .nt_euro_playoff_field(teams[direct[s, ]], ru_out[s, ], out_h, seeding)
    part <- c(f$runners_up, f$nations_league)
    n_res[s] <- length(f$reserved)
    host_place[s, f$reserved] <- TRUE
    playoff[s, part] <- TRUE
    via_ru[s, f$runners_up] <- TRUE
    playoff_seed[s, part] <- seq_along(part)
    b <- .nt_euro_playoff_draw(part, f$format, hosts, northern_ireland)
    if (f$format == "paths") paths[[s]] <- cbind(s, b) else ties[[s]] <- cbind(s, b)
  }

  fit_idx <- function(x) match(x, fit$teams)
  venue <- function(home) matrix(ifelse(home %in% neutral_home, 0, 1))
  rates <- function(sim, home, away) {
    .nt_sim_rates(fit, qual$draw[sim], fit_idx(home), fit_idx(away), venue(home))
  }
  single <- function(sim, home, away) {
    r <- rates(sim, home, away)
    res <- nt_simulate_knockout(r$home[, 1], r$away[, 1], "extra_time", et_factor, shootout_p)
    ifelse(res$a_wins, home, away)
  }
  winners <- list()
  p <- do.call(rbind, paths)
  if (!is.null(p)) {
    sim <- as.integer(p[, 1])
    sf1 <- single(sim, p[, 2], p[, 5])
    sf2 <- single(sim, p[, 3], p[, 4])
    winners$paths <- cbind(sim, single(sim, sf1, sf2))
  }
  t4 <- do.call(rbind, ties)
  if (!is.null(t4)) {
    sim <- as.integer(t4[, 1])
    seeded <- t4[, 2]
    unseeded <- t4[, 3]
    r1 <- rates(sim, unseeded, seeded)
    r2 <- rates(sim, seeded, unseeded)
    res <- nt_simulate_two_legged(
      list(a = r1$home[, 1], b = r1$away[, 1]), list(a = r2$away[, 1], b = r2$home[, 1]),
      et_factor, shootout_p
    )
    winners$ties <- cbind(sim, ifelse(res$a_wins, unseeded, seeded))
  }
  w <- do.call(rbind, winners)
  playoff_winner <- matrix(FALSE, n, n_teams, dimnames = list(NULL, teams))
  playoff_winner[cbind(as.integer(w[, 1]), match(w[, 2], teams))] <- TRUE
  qualified <- direct | host_place | playoff_winner

  list(
    teams = tibble::tibble(
      team = teams,
      p_host_place = colMeans(host_place),
      p_playoff = colMeans(playoff),
      p_playoff_runner_up = colMeans(via_ru),
      p_playoff_nations_league = colMeans(playoff & !via_ru),
      p_qualify_playoff = colMeans(playoff_winner),
      p_qualify = colMeans(qualified)
    ),
    qualified = qualified, host_place = host_place, playoff = playoff,
    playoff_winner = playoff_winner, playoff_seed = playoff_seed,
    n_host_places = n_res
  )
}

#' Simulate EURO 2028 qualifying from the drawn groups.
#'
#' Runs [nt_simulate_qualifying()] on the draw (with the Nations League
#' quarter-final placeholders) and [nt_simulate_euro_playoffs()] on its
#' result, one posterior draw per simulation throughout.
#'
#' @param fit A fit from [nt_fit_inla()] covering every drawn team.
#' @param draw Output of [nt_read_euro2028_draw()].
#' @param n_sims Number of simulations.
#' @param h2h_reapply Passed to [nt_simulate_qualifying()].
#' @param seed Optional seed.
#' @param ... Passed to [nt_simulate_euro_playoffs()].
#' @return List: `teams` (tibble per team: `team`, `group`, `p_winner`,
#'   `p_runner_up_direct`, `p_qualify_direct`, `p_host_place`, `p_playoff`,
#'   `p_playoff_runner_up`, `p_playoff_nations_league`, `p_qualify_playoff`,
#'   `p_qualify`, `exp_points`, `p_pos1` ... `p_pos5`), `slots` (per group
#'   slot, from [nt_simulate_qualifying()]), `qualifying` (its full output),
#'   and the per-simulation matrices of [nt_simulate_euro_playoffs()].
#' @export
nt_simulate_euro2028 <- function(fit, draw, n_sims = 10000L,
                                 h2h_reapply = c("repeat", "once"), seed = NULL, ...) {
  h2h_reapply <- match.arg(h2h_reapply)
  q <- nt_simulate_qualifying(
    fit, draw$groups, draw$fixtures, n_sims = n_sims, h2h_reapply = h2h_reapply,
    quarter_finals = draw$quarter_finals, seed = seed
  )
  args <- list(...)
  if (is.null(args$neutral_home) && !is.null(draw$neutral_home)) args$neutral_home <- draw$neutral_home
  po <- do.call(nt_simulate_euro_playoffs, c(list(q, fit, draw$seeding, draw$hosts), args))
  qt <- q$teams
  pt <- po$teams[match(qt$team, po$teams$team), , drop = FALSE]
  pos_cols <- grep("^p_pos", names(qt), value = TRUE)
  teams <- tibble::tibble(
    team = qt$team, group = qt$group, p_winner = qt$p_winner,
    p_runner_up_direct = qt$p_runner_up_direct, p_qualify_direct = qt$p_qualify_direct,
    p_host_place = pt$p_host_place, p_playoff = pt$p_playoff,
    p_playoff_runner_up = pt$p_playoff_runner_up,
    p_playoff_nations_league = pt$p_playoff_nations_league,
    p_qualify_playoff = pt$p_qualify_playoff, p_qualify = pt$p_qualify,
    exp_points = qt$exp_points, qt[, pos_cols]
  )
  c(list(teams = teams, slots = q$slots, qualifying = q), po[setdiff(names(po), "teams")])
}
