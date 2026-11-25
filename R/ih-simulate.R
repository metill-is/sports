#' @include ih-model.R
NULL

# International handball championships: the preliminary round, the main round
# with carried-over results, and the knockouts, simulated from the nations
# model's draws (R/ih-model.R).
#
# Format and rules (sources and page numbers in
# docs/reports/2026-11-24-intl-handball/tournament-facts.md):
# * Women's EHF EURO 2026, Regulations Art. 9: six groups of four, the top two
#   to two main-round groups of six (A-C, D-F) carrying over the result
#   against the other qualifier of their group; 1st of one main-round group v
#   2nd of the other in the semi-finals, the two 3rd-placed teams for 5th
#   place; knockouts level after 60 minutes go to 2 x 5 minutes of extra time,
#   then another 2 x 5, then penalties.
# * Men's IHF World Championship 2027, Regulations for Competitions
#   (21 Aug 2024) Chapter II: eight groups of four, the top three to four
#   main-round groups of six (A/B, C/D, E/F, G/H) with results against fellow
#   qualifiers carried over; quarter-finals 1 MR I v 2 MR III, 1 MR III v
#   2 MR I, 1 MR II v 2 MR IV, 1 MR IV v 2 MR II; semi-finals QF1 v QF3,
#   QF2 v QF4; extra time as above, then a 7 m shoot-out.
# * Ranking in every group, both bodies: points (2 for a win, 1 for a draw);
#   then points, goal difference and goals in the matches among the teams
#   level on points; then goal difference and goals in all the group's
#   matches; then Fair Play (EHF) or lots, drawn at random here. Whether the
#   head-to-head table is recomputed for teams still level after one is
#   separated is not stated by either body; `h2h_reapply` switches it, and
#   its default `"none"` is the EHF text read literally.

#' Length of one extra-time period (2 x 5 minutes) as a share of 60 minutes
#' @noRd
IH_ET_SHARE <- 10 / 60

#' The championships the code knows: format, knockout bracket, publish folder
#' @noRd
IH_TOURNAMENTS <- list(
  euro2026w = list(
    title = "Women's EHF EURO 2026", sex = "female", competition = "EURO", edition = 2026L,
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
    venue_country = c(
      Oradea = "Romania", `Cluj-Napoca` = "Romania", Antalya = "Turkey", Brno = "Czech Republic",
      Katowice = "Poland", Bratislava = "Slovakia"
    ),
    start = as.Date("2026-12-03"), end = as.Date("2026-12-20"),
    publish = c("handball_euro", "kvenna"), focus_team = "Iceland", focus_round = "MR"
  ),
  wc2027m = list(
    title = "Men's IHF World Championship 2027", sex = "male", competition = "WC", edition = 2027L,
    n_main = 3L, main = list(I = c("A", "B"), II = c("C", "D"), III = c("E", "F"), IV = c("G", "H")),
    knockout = data.frame(
      code = c("QF1", "QF2", "QF3", "QF4", "SF1", "SF2", "Third", "Final"),
      round = c("QF", "QF", "QF", "QF", "SF", "SF", "Third", "Final"),
      match_no = c(307705L, 307707L, 307709L, 307711L, 307721L, 307723L, 307731L, 307729L),
      home = c("MR_I_1", "MR_III_1", "MR_II_1", "MR_IV_1", "W_QF1", "W_QF2", "L_SF1", "W_SF1"),
      away = c("MR_III_2", "MR_I_2", "MR_IV_2", "MR_II_2", "W_QF3", "W_QF4", "L_SF2", "W_SF2"),
      stringsAsFactors = FALSE
    ),
    rounds = c("MR", "QF", "SF", "Final", "Champion"),
    venue_country = c(
      Munich = "Germany", Stuttgart = "Germany", Kiel = "Germany", Magdeburg = "Germany",
      Cologne = "Germany", Hanover = "Germany"
    ),
    start = as.Date("2027-01-13"), end = as.Date("2027-01-31"),
    publish = c("handball_world_cup", "karla"), focus_team = "Iceland"
  )
)

#' Team names: the model's spelling, the federations', Icelandic, flag code
#'
#' @param path CSV with `team` (the name in the match table), `official` (the
#'   EHF / IHF spelling in the schedules), `team_is`, `iso2`.
#' @return data.frame.
#' @noRd
ih_team_names <- function(path = here::here("config", "intl-handball", "teams.csv")) {
  utils::read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = "")
}

#' A championship's groups, fixtures, bracket and venues
#'
#' @param id A name of [IH_TOURNAMENTS].
#' @param names [ih_team_names()].
#' @return The [IH_TOURNAMENTS] entry with `id`, `groups` (named list of the
#'   teams of each preliminary group, in the model's spelling), `prelim`
#'   (the preliminary fixtures: `match_no`, `group`, `match_date`, `kickoff`
#'   (UTC), `venue_city`, `host`, `home_team`, `away_team`), `main_fixtures`
#'   (`match_no`, `group`, `match_date`, `home_slot`, `away_slot`, e.g. "1C"),
#'   `main_host` (host nation of each main-round group's venue, or NA) and
#'   `knockout` with `match_date`, `venue_city` and `host`.
#' @noRd
ih_tournament_spec <- function(id, names = ih_team_names()) {
  spec <- IH_TOURNAMENTS[[id]]
  if (is.null(spec)) cli::cli_abort("Unknown championship {.val {id}}.")
  dir <- here::here("config", "intl-handball", id)
  g <- utils::read.csv(file.path(dir, "groups.csv"), stringsAsFactors = FALSE, encoding = "UTF-8")
  s <- utils::read.csv(file.path(dir, "schedule.csv"), stringsAsFactors = FALSE, encoding = "UTF-8")
  to_model <- function(x) {
    out <- names$team[match(x, names$official)]
    if (anyNA(out)) cli::cli_abort("No model spelling in {.file teams.csv} for {.val {unique(x[is.na(out)])}}.")
    out
  }
  g$team <- to_model(g$team)
  g <- g[order(g$group, g$position), ]
  host_of <- function(city) unname(spec$venue_country[city])
  utc <- function(date, time, tz) {
    off <- as.numeric(sub("^.*UTC([+-][0-9]+).*$", "\\1", tz))
    t <- as.POSIXct(paste(date, ifelse(nzchar(time) & !is.na(time), time, NA)), format = "%Y-%m-%d %H:%M", tz = "UTC")
    t - off * 3600
  }
  pre <- s[s$stage == "Preliminary Round", ]
  prelim <- data.frame(
    match_no = pre$match_no, group = pre$group, match_date = as.Date(pre$date),
    kickoff = utc(pre$date, pre$local_time, pre$tz), venue_city = pre$venue_city,
    host = host_of(pre$venue_city), home_team = to_model(pre$home_team), away_team = to_model(pre$away_team),
    stringsAsFactors = FALSE
  )
  prelim <- prelim[order(prelim$kickoff, prelim$match_no), ]
  mr <- s[s$stage == "Main Round", ]
  main_fixtures <- data.frame(
    match_no = mr$match_no, group = mr$group, match_date = as.Date(mr$date),
    kickoff = utc(mr$date, mr$local_time, mr$tz),
    home_slot = mr$home_team, away_slot = mr$away_team, stringsAsFactors = FALSE
  )
  main_city <- vapply(names(spec$main), function(G) {
    city <- unique(mr$venue_city[mr$group == G])
    if (length(city) != 1L) NA_character_ else city
  }, character(1))
  main_host <- stats::setNames(host_of(main_city), names(main_city))
  ko <- spec$knockout
  at <- match(ko$match_no, s$match_no)
  ko$match_date <- as.Date(s$date[at])
  ko$kickoff <- utc(s$date[at], s$local_time[at], s$tz[at])
  ko$venue_city <- s$venue_city[at]
  ko$host <- host_of(ko$venue_city)
  spec$knockout <- NULL
  c(spec, list(
    id = id, groups = split(g$team, g$group), prelim = prelim, main_fixtures = main_fixtures,
    main_city = main_city, main_host = main_host, knockout = ko
  ))
}

#' Margin and total locations for draw-varying pairings (team indices)
#' @param h,a Integer matrices, draws by matches, of team indices in `fit$teams`.
#' @param venue Numeric matrix of the same shape (see [ih_venue()]).
#' @param draws The fit draw behind each simulation (one per row).
#' @noRd
.ih_loc <- function(fit, h, a, venue, draws) {
  nd <- length(draws)
  row <- matrix(draws, nd, ncol(h))
  host <- ifelse(venue < 0, a, h)
  pick <- function(m, idx) matrix(m[cbind(as.vector(row), as.vector(idx))], nd, ncol(h))
  list(
    loc_m = pick(fit$m_state, h) - pick(fit$m_state, a) + venue * (fit$hfa[draws] + pick(fit$ha, host)),
    loc_t = fit$level[draws] + pick(fit$t_state, h) + pick(fit$t_state, a),
    sigma_m = fit$sigma_m[draws], sigma_t = fit$sigma_t[draws]
  )
}

#' Venue of draw-varying pairings: 1 when the home side is the host, -1 the away side
#' @noRd
.ih_venue_of <- function(h, a, host_idx) {
  if (length(host_idx) == 1L && is.na(host_idx)) {
    return(matrix(0, nrow(h), ncol(h)))
  }
  hv <- matrix(rep_len(host_idx, ncol(h)), nrow(h), ncol(h), byrow = TRUE)
  (h == hv & !is.na(hv)) * 1 - (a == hv & !is.na(hv)) * 1
}

#' Simulate 60-minute scores of draw-varying pairings
#' @return list(home, away, loc_m, sigma_m): integer score matrices and what
#'   extra time needs.
#' @noRd
.ih_sim_scores <- function(fit, h, a, venue, draws) {
  loc <- .ih_loc(fit, h, a, venue, draws)
  sc <- v2_sample_scores(loc$loc_m, loc$sigma_m, loc$loc_t, loc$sigma_t)
  list(home = sc$home, away = sc$away, loc_m = loc$loc_m, sigma_m = loc$sigma_m)
}

#' Who goes through in a knockout match
#'
#' A match level after 60 minutes goes to extra time: a period of 2 x 5
#' minutes is a short continuation of the match, its margin drawn with mean
#' `IH_ET_SHARE * loc_m` and sd `sqrt(IH_ET_SHARE) * sigma_m`; a level period
#' is followed by a second, and a second level one by penalties, a coin flip.
#' @param gh,ga 60-minute scores (matrices).
#' @param loc_m Margin locations of the same shape; `sigma_m` per row.
#' @return Logical matrix: the home side goes through.
#' @noRd
.ih_knockout_home_wins <- function(gh, ga, loc_m, sigma_m) {
  sig <- matrix(sigma_m, nrow(gh), ncol(gh))
  home <- gh > ga
  open <- gh == ga
  for (period in 1:2) {
    idx <- which(open)
    if (!length(idx)) break
    d <- round(loc_m[idx] * IH_ET_SHARE + sig[idx] * sqrt(IH_ET_SHARE) * stats::rnorm(length(idx)))
    home[idx] <- d > 0
    open[idx] <- d == 0
  }
  idx <- which(open)
  home[idx] <- stats::runif(length(idx)) < 0.5
  home
}

#' Add matches to per-draw group records
#'
#' @param x list(points, gd, gf) of n x k x k arrays: what team i obtained
#'   against team j.
#' @param sh,sa Group slot of the home and away side: matrices n x m (or
#'   vectors of length m, the same in every simulation).
#' @param gh,ga Scores, n x m.
#' @param valid Optional logical n x m: which entries are matches of the group.
#' @noRd
.ih_add_records <- function(x, sh, sa, gh, ga, valid = NULL) {
  n <- dim(x$points)[1]
  m <- ncol(gh)
  if (!is.matrix(sh)) sh <- matrix(sh, n, m, byrow = TRUE)
  if (!is.matrix(sa)) sa <- matrix(sa, n, m, byrow = TRUE)
  if (is.null(valid)) valid <- matrix(TRUE, n, m)
  for (j in seq_len(m)) {
    i <- which(valid[, j])
    if (!length(i)) next
    hi <- cbind(i, sh[i, j], sa[i, j])
    ai <- cbind(i, sa[i, j], sh[i, j])
    h <- gh[i, j]
    a <- ga[i, j]
    x$points[hi] <- x$points[hi] + 2 * (h > a) + (h == a)
    x$points[ai] <- x$points[ai] + 2 * (a > h) + (h == a)
    x$gd[hi] <- x$gd[hi] + h - a
    x$gd[ai] <- x$gd[ai] + a - h
    x$gf[hi] <- x$gf[hi] + h
    x$gf[ai] <- x$gf[ai] + a
  }
  x
}

.ih_empty_records <- function(n, k) {
  list(points = array(0, c(n, k, k)), gd = array(0, c(n, k, k)), gf = array(0, c(n, k, k)))
}

#' Rank per-draw group records under the EHF / IHF rules
#' @return list(pos, points, gd, gf): n x k matrices (position 1 is first).
#' @noRd
.ih_rank_records <- function(x, h2h_reapply = "none") {
  total <- function(v) apply(v, c(1, 2), sum)
  gd <- total(x$gd)
  gf <- total(x$gf)
  n <- nrow(gd)
  k <- ncol(gd)
  pos <- .nt_rank_group_vec(
    x$points, x$gd, x$gf, list(gd, gf),
    matrix(stats::runif(n * k), n, k), h2h_reapply
  )
  list(pos = pos, points = total(x$points), gd = gd, gf = gf)
}

#' Rank one group from its played matches (the real table)
#'
#' @param teams The group's teams.
#' @param home,away,gh,ga The played matches among them.
#' @param h2h_reapply See [ih_simulate_tournament()].
#' @return data.frame: `team`, `played`, `won`, `drawn`, `lost`, `gf`, `ga`,
#'   `gd`, `points`, `position` (a tie on every criterion is settled at random).
#' @noRd
ih_rank_table <- function(teams, home, away, gh, ga, h2h_reapply = "none") {
  k <- length(teams)
  x <- .ih_add_records(
    .ih_empty_records(1L, k), match(home, teams), match(away, teams),
    matrix(gh, 1L), matrix(ga, 1L)
  )
  r <- .ih_rank_records(x, h2h_reapply)
  side <- data.frame(team = c(home, away), gf = c(gh, ga), ga = c(ga, gh))
  per_team <- function(x) vapply(teams, function(t) sum(x[side$team == t]), numeric(1))
  data.frame(
    team = teams, played = per_team(rep(1, nrow(side))), won = per_team(side$gf > side$ga),
    drawn = per_team(side$gf == side$ga), lost = per_team(side$gf < side$ga),
    gf = per_team(side$gf), ga = per_team(side$ga), gd = as.vector(r$gd), points = as.vector(r$points),
    position = as.vector(r$pos), stringsAsFactors = FALSE, row.names = NULL
  )
}

#' Which matches belong to the championship of `spec`
#' @return Logical per row of `matches`: the championship's competition and
#'   edition, outside qualification, within its dates, between its nations.
#' @noRd
.ih_championship_rows <- function(matches, spec) {
  teams <- unlist(spec$groups, use.names = FALSE)
  matches$competition == spec$competition & matches$edition == spec$edition &
    !grepl("qualif", matches$block, ignore.case = TRUE) &
    matches$match_date >= spec$start & matches$match_date <= spec$end &
    matches$home_team %in% teams & matches$away_team %in% teams
}

#' Stage of a championship match from its date
#' @return "preliminary", "main" or "knockout" per date.
#' @noRd
.ih_stage <- function(spec, date) {
  last_prelim <- max(spec$prelim$match_date)
  first_ko <- min(spec$knockout$match_date, na.rm = TRUE)
  ifelse(date <= last_prelim, "preliminary", ifelse(date < first_ko, "main", "knockout"))
}

#' Give the championship being forecast its own schedule's venues
#'
#' Its matches train the model with the venue the simulator gives its
#' fixtures: a preliminary match is at home for the host of its group's
#' venue, a main-round match for the host of its group's venue
#' (`spec$main_host`), a knockout match for the host of that day's knockout
#' venue.
#' @param matches [ih_read_matches()] rows.
#' @param spec [ih_tournament_spec()].
#' @return `matches`, with `venue` replaced on the championship's matches.
#' @noRd
ih_with_spec_venues <- function(matches, spec) {
  idx <- which(.ih_championship_rows(matches, spec))
  if (!length(idx)) {
    return(matches)
  }
  r <- matches[idx, , drop = FALSE]
  stage <- .ih_stage(spec, r$match_date)
  host <- rep(NA_character_, length(idx))
  pair <- function(h, a) paste(pmin(h, a), pmax(h, a))
  pre <- stage == "preliminary"
  at <- match(pair(r$home_team[pre], r$away_team[pre]), pair(spec$prelim$home_team, spec$prelim$away_team))
  host[pre] <- spec$prelim$host[at]
  mr <- stage == "main"
  team_group <- stats::setNames(rep(names(spec$groups), lengths(spec$groups)), unlist(spec$groups, use.names = FALSE))
  main_of <- stats::setNames(rep(names(spec$main), lengths(spec$main)), unlist(spec$main, use.names = FALSE))
  host[mr] <- unname(spec$main_host[main_of[team_group[r$home_team[mr]]]])
  ko <- which(stage == "knockout")
  host[ko] <- vapply(ko, function(i) {
    h <- unique(spec$knockout$host[spec$knockout$match_date == r$match_date[i]])
    if (length(h) == 1L) h else NA_character_
  }, character(1))
  known <- !is.na(host)
  v <- integer(length(idx))
  v[known] <- as.integer(r$home_team[known] == host[known]) - as.integer(r$away_team[known] == host[known])
  matches$venue[idx] <- v
  matches
}

#' The played matches of a championship, by stage
#'
#' @param matches [ih_read_matches()] rows of the championship's sex.
#' @param spec [ih_tournament_spec()].
#' @return data.frame: `stage` ("preliminary", "main", "knockout"),
#'   `match_date`, `home_team`, `away_team`, `home_60`, `away_60` (a
#'   preliminary or main-round match without them takes its final score),
#'   `home_score`, `away_score`, `pen_home`, `pen_away`, `home_wins` (went
#'   through, knockouts).
#' @noRd
ih_tournament_results <- function(matches, spec) {
  r <- matches[.ih_championship_rows(matches, spec) & !is.na(matches$home_score), , drop = FALSE]
  r$stage <- .ih_stage(spec, r$match_date)
  # WHY: the tables and the simulator read the 60-minute score, and one NA
  # stops the ranking. A preliminary or main-round match never goes to extra
  # time, so its final score is its 60-minute score; a knockout match keeps NA
  # (its final score may include extra time, and only home_wins is read).
  no_60 <- r$stage != "knockout" & (is.na(r$home_60) | is.na(r$away_60))
  r$home_60[no_60] <- r$home_score[no_60]
  r$away_60[no_60] <- r$away_score[no_60]
  r$home_wins <- r$home_score > r$away_score |
    (r$home_score == r$away_score & !is.na(r$pen_home) & r$pen_home > r$pen_away)
  r[, c(
    "stage", "match_date", "home_team", "away_team", "home_60", "away_60", "home_score",
    "away_score", "pen_home", "pen_away", "home_wins"
  )]
}

#' Put played results into simulated score matrices
#'
#' @param th,ta Team indices of the simulated pairings (n x m).
#' @param played Played matches of the stage with team indices `h`, `a`.
#' @return list(home, away, fixed, home_wins): scores with the played ones in
#'   place (oriented to the simulated home side), which entries were played,
#'   and for knockouts who went through.
#' @noRd
.ih_apply_played <- function(gh, ga, th, ta, played) {
  fixed <- matrix(FALSE, nrow(gh), ncol(gh))
  home_wins <- matrix(NA, nrow(gh), ncol(gh))
  if (is.null(played) || !nrow(played)) {
    return(list(home = gh, away = ga, fixed = fixed, home_wins = home_wins))
  }
  key <- paste(th, ta)
  same <- match(key, paste(played$h, played$a))
  flip <- match(key, paste(played$a, played$h))
  i <- which(!is.na(same))
  gh[i] <- played$home_60[same[i]]
  ga[i] <- played$away_60[same[i]]
  home_wins[i] <- played$home_wins[same[i]]
  j <- which(is.na(same) & !is.na(flip))
  gh[j] <- played$away_60[flip[j]]
  ga[j] <- played$home_60[flip[j]]
  home_wins[j] <- !played$home_wins[flip[j]]
  fixed[c(i, j)] <- TRUE
  list(home = gh, away = ga, fixed = fixed, home_wins = home_wins)
}

#' Simulate a championship from the nations model
#'
#' Each simulation uses one posterior draw of every nation's state for all
#' its matches, so a nation's uncertain strength carries through the whole
#' championship. Played matches keep their results.
#'
#' @param fit [ih_fit()] for the championship's season.
#' @param spec [ih_tournament_spec()].
#' @param results [ih_tournament_results()], or `NULL` before the start.
#' @param n_sims Simulations (the fit's draws are reused in turn).
#' @param seed Seed; the caller's random stream is left as it was.
#' @param h2h_reapply `"none"`, `"once"` or `"repeat"`, see the file header.
#' @return list: `teams` (one row per nation: `team`, `group`, `p_pos1` to
#'   `p_pos4`, `p_advance`, `proj_points`, `proj_gd`, and `p_<round>` for each
#'   of `spec$rounds`), `main` (per main-round group: the nations that can
#'   reach it with `p_in`, `p_pos1`..., `proj_points`, `proj_gd`; meaningful
#'   once its feeder groups are complete), `knockout_pairs` (the most likely
#'   pairing of each knockout match), `n_sims`.
#' @noRd
ih_simulate_tournament <- function(fit, spec, results = NULL, n_sims = 20000L, seed = 1L,
                                   h2h_reapply = c("none", "once", "repeat")) {
  h2h_reapply <- match.arg(h2h_reapply)
  withr::with_seed(as.integer(seed), .ih_simulate(fit, spec, results, as.integer(n_sims), h2h_reapply))
}

.ih_simulate <- function(fit, spec, results, n, h2h_reapply) {
  ti <- function(x) {
    i <- match(x, fit$teams)
    if (anyNA(i)) cli::cli_abort("Not in the fit: {.val {unique(x[is.na(i)])}}.")
    i
  }
  draws <- rep_len(seq_len(nrow(fit$m_state)), n)
  played <- function(stage) {
    if (is.null(results)) {
      return(NULL)
    }
    p <- results[results$stage == stage, , drop = FALSE]
    p$h <- ti(p$home_team)
    p$a <- ti(p$away_team)
    p
  }
  all_teams <- unlist(spec$groups, use.names = FALSE)
  nt <- length(all_teams)
  n_main <- spec$n_main

  # ---- preliminary round ----
  pf <- spec$prelim
  m <- nrow(pf)
  h <- matrix(ti(pf$home_team), n, m, byrow = TRUE)
  a <- matrix(ti(pf$away_team), n, m, byrow = TRUE)
  venue <- .ih_venue_of(h, a, match(pf$host, fit$teams))
  sc <- .ih_sim_scores(fit, h, a, venue, draws)
  sc <- .ih_apply_played(sc$home, sc$away, h, a, played("preliminary"))
  pre_pos <- pre_pts <- pre_gd <- matrix(NA_real_, n, nt, dimnames = list(NULL, all_teams))
  team_at <- list()
  for (g in names(spec$groups)) {
    gt <- spec$groups[[g]]
    j <- which(pf$group == g)
    x <- .ih_add_records(
      .ih_empty_records(n, length(gt)), match(pf$home_team[j], gt), match(pf$away_team[j], gt),
      sc$home[, j, drop = FALSE], sc$away[, j, drop = FALSE]
    )
    r <- .ih_rank_records(x, h2h_reapply)
    pre_pos[, gt] <- r$pos
    pre_pts[, gt] <- r$points
    pre_gd[, gt] <- r$gd
    team_at[[g]] <- t(apply(r$pos, 1, function(p) ti(gt)[order(p)]))
  }

  # ---- main round ----
  main_pos <- main_pts <- main_gd <- matrix(NA_real_, n, nt, dimnames = list(NULL, all_teams))
  mr_team <- list()
  for (G in names(spec$main)) {
    fg <- spec$main[[G]]
    slots <- as.vector(t(outer(fg, seq_len(n_main), function(g, p) paste0(p, g))))
    k <- length(slots)
    occ <- do.call(cbind, lapply(fg, function(g) team_at[[g]][, seq_len(n_main), drop = FALSE]))
    x <- .ih_empty_records(n, k)
    for (g in fg) {
      j <- which(pf$group == g)
      gt <- spec$groups[[g]]
      ph <- pre_pos[, pf$home_team[j], drop = FALSE]
      pa <- pre_pos[, pf$away_team[j], drop = FALSE]
      valid <- ph <= n_main & pa <= n_main
      sh <- matrix(match(paste0(ph, g), slots), n)
      sa <- matrix(match(paste0(pa, g), slots), n)
      x <- .ih_add_records(x, sh, sa, sc$home[, j, drop = FALSE], sc$away[, j, drop = FALSE], valid)
    }
    mf <- spec$main_fixtures[spec$main_fixtures$group == G, , drop = FALSE]
    if (nrow(mf)) {
      hs <- match(mf$home_slot, slots)
      as <- match(mf$away_slot, slots)
      th <- occ[, hs, drop = FALSE]
      ta <- occ[, as, drop = FALSE]
      v <- .ih_venue_of(th, ta, match(spec$main_host[[G]], fit$teams))
      ms <- .ih_sim_scores(fit, th, ta, v, draws)
      ms <- .ih_apply_played(ms$home, ms$away, th, ta, played("main"))
      x <- .ih_add_records(x, hs, as, ms$home, ms$away)
    }
    r <- .ih_rank_records(x, h2h_reapply)
    mr_team[[G]] <- t(vapply(seq_len(n), function(i) occ[i, order(r$pos[i, ])], numeric(k)))
    row <- rep(seq_len(n), k)
    col <- match(fit$teams[as.vector(occ)], all_teams)
    main_pos[cbind(row, col)] <- as.vector(r$pos)
    main_pts[cbind(row, col)] <- as.vector(r$points)
    main_gd[cbind(row, col)] <- as.vector(r$gd)
  }

  # ---- knockouts ----
  ko <- spec$knockout
  won <- lost <- list()
  ref_team <- function(ref) {
    p <- strsplit(ref, "_")[[1]]
    switch(p[1],
      MR = mr_team[[p[2]]][, as.integer(p[3])],
      W = won[[p[2]]],
      L = lost[[p[2]]]
    )
  }
  reach_ko <- list()
  pair_counts <- list()
  for (r in seq_len(nrow(ko))) {
    th <- matrix(ref_team(ko$home[r]), n)
    ta <- matrix(ref_team(ko$away[r]), n)
    v <- .ih_venue_of(th, ta, match(ko$host[r], fit$teams))
    ks <- .ih_sim_scores(fit, th, ta, v, draws)
    kp <- .ih_apply_played(ks$home, ks$away, th, ta, played("knockout"))
    hw <- .ih_knockout_home_wins(kp$home, kp$away, ks$loc_m, ks$sigma_m)
    hw[kp$fixed] <- kp$home_wins[kp$fixed]
    won[[ko$code[r]]] <- ifelse(hw, th, ta)
    lost[[ko$code[r]]] <- ifelse(hw, ta, th)
    reach_ko[[ko$code[r]]] <- cbind(th, ta)
    pk <- table(paste(fit$teams[th], fit$teams[ta], sep = "\t"))
    pair_counts[[ko$code[r]]] <- pk[order(-pk)][1:min(5L, length(pk))] / n
  }

  # ---- summaries ----
  p_pos <- sapply(1:4, function(p) colMeans(pre_pos == p))
  colnames(p_pos) <- paste0("p_pos", 1:4)
  teams <- data.frame(
    team = all_teams, group = rep(names(spec$groups), lengths(spec$groups)),
    p_pos, p_advance = colMeans(pre_pos <= n_main),
    proj_points = colMeans(pre_pts), proj_gd = colMeans(pre_gd),
    stringsAsFactors = FALSE, row.names = NULL
  )
  reached <- function(codes) {
    out <- matrix(FALSE, n, nt)
    for (cd in codes) {
      tm <- reach_ko[[cd]]
      for (c in seq_len(ncol(tm))) out[cbind(seq_len(n), match(fit$teams[tm[, c]], all_teams))] <- TRUE
    }
    colMeans(out)
  }
  for (rd in spec$rounds) {
    teams[[paste0("p_", rd)]] <- switch(rd,
      MR = colMeans(pre_pos <= n_main),
      Champion = vapply(all_teams, function(t) mean(fit$teams[won$Final] == t), numeric(1)),
      reached(ko$code[ko$round == rd])
    )
  }
  main <- lapply(names(spec$main), function(G) {
    members <- unlist(spec$groups[spec$main[[G]]], use.names = FALSE)
    k <- n_main * length(spec$main[[G]])
    inn <- !is.na(main_pos[, members, drop = FALSE])
    d <- data.frame(team = members, p_in = colMeans(inn), stringsAsFactors = FALSE)
    mp <- main_pos[, members, drop = FALSE]
    for (p in seq_len(k)) d[[paste0("p_pos", p)]] <- colMeans(!is.na(mp) & mp == p)
    cond <- function(mat) {
      vapply(members, function(t) {
        x <- mat[, t]
        if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
      }, numeric(1))
    }
    d$proj_points <- cond(main_pts)
    d$proj_gd <- cond(main_gd)
    d
  })
  names(main) <- names(spec$main)
  list(teams = teams, main = main, knockout_pairs = pair_counts, n_sims = n)
}
