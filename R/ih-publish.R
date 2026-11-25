#' @include ih-simulate.R
NULL

# International handball championships: the files metill-platform renders.
# The contract (five JSON files) is metill-platform's
# data/hm2027-handbolti/README.md (schema_version 1); the folder is a
# top-level one under data/publish/ (handball_euro/kvenna,
# handball_world_cup/karla), because the publish validators pick a schema by
# the first path segment and handball/ holds the league schemas.

.ih_empty_results <- function() {
  data.frame(
    stage = character(), match_date = as.Date(character()), home_team = character(), away_team = character(),
    home_60 = integer(), away_60 = integer(), home_score = integer(), away_score = integer(),
    pen_home = integer(), pen_away = integer(), home_wins = logical(), stringsAsFactors = FALSE
  )
}

#' The real tables so far: preliminary groups, main-round groups, knockouts
#'
#' @param spec [ih_tournament_spec()].
#' @param results [ih_tournament_results()] (may be `NULL`).
#' @param h2h_reapply See [ih_simulate_tournament()].
#' @return list: `prelim` and `main` (per group: `table` from
#'   [ih_rank_table()], `complete`, and for main-round groups `members`, NULL
#'   until every feeder group is complete), `knockout` (per bracket code:
#'   `home`, `away`, `winner`, `loser`, NA where not known yet).
#' @noRd
ih_actual_standings <- function(spec, results, h2h_reapply = "none") {
  if (is.null(results)) results <- .ih_empty_results()
  pre <- results[results$stage == "preliminary", , drop = FALSE]
  prelim <- lapply(names(spec$groups), function(g) {
    gt <- spec$groups[[g]]
    r <- pre[pre$home_team %in% gt & pre$away_team %in% gt, , drop = FALSE]
    n_fix <- sum(spec$prelim$group == g)
    list(
      table = ih_rank_table(gt, r$home_team, r$away_team, r$home_60, r$away_60, h2h_reapply),
      complete = nrow(r) == n_fix, matches = r
    )
  })
  names(prelim) <- names(spec$groups)
  mr <- results[results$stage == "main", , drop = FALSE]
  main <- lapply(names(spec$main), function(G) {
    fg <- spec$main[[G]]
    if (!all(vapply(prelim[fg], `[[`, logical(1), "complete"))) {
      return(list(members = NULL, complete = FALSE))
    }
    members <- unlist(lapply(fg, function(g) {
      t <- prelim[[g]]$table
      t$team[order(t$position)][seq_len(spec$n_main)]
    }), use.names = FALSE)
    carried <- do.call(rbind, lapply(prelim[fg], function(p) {
      p$matches[p$matches$home_team %in% members & p$matches$away_team %in% members, , drop = FALSE]
    }))
    new <- mr[mr$home_team %in% members & mr$away_team %in% members, , drop = FALSE]
    all_m <- rbind(carried, new)
    n_new <- sum(spec$main_fixtures$group == G)
    slots <- as.vector(t(outer(fg, seq_len(spec$n_main), function(g, p) paste0(p, g))))
    list(
      members = members, slots = stats::setNames(members, slots),
      table = ih_rank_table(members, all_m$home_team, all_m$away_team, all_m$home_60, all_m$away_60, h2h_reapply),
      complete = nrow(new) == n_new, n_played_new = nrow(new)
    )
  })
  names(main) <- names(spec$main)
  ko <- results[results$stage == "knockout", , drop = FALSE]
  known <- list()
  resolve <- function(ref) {
    p <- strsplit(ref, "_")[[1]]
    if (p[1] == "MR") {
      g <- main[[p[2]]]
      if (!isTRUE(g$complete)) {
        return(NA_character_)
      }
      return(g$table$team[order(g$table$position)][as.integer(p[3])])
    }
    k <- known[[p[2]]]
    if (is.null(k)) {
      return(NA_character_)
    }
    if (p[1] == "W") k$winner else k$loser
  }
  for (r in seq_len(nrow(spec$knockout))) {
    code <- spec$knockout$code[r]
    h <- resolve(spec$knockout$home[r])
    a <- resolve(spec$knockout$away[r])
    winner <- loser <- NA_character_
    if (!is.na(h) && !is.na(a)) {
      i <- which((ko$home_team == h & ko$away_team == a) | (ko$home_team == a & ko$away_team == h))
      if (length(i)) {
        hw <- ko$home_wins[i[1]]
        winner <- if (hw) ko$home_team[i[1]] else ko$away_team[i[1]]
        loser <- setdiff(c(h, a), winner)
      }
    }
    known[[code]] <- list(home = h, away = a, winner = winner, loser = loser)
  }
  list(prelim = prelim, main = main, knockout = known)
}

#' The matches still to play whose two teams are known
#' @return data.frame: `stage`, `group`, `round`, `code`, `match_date`,
#'   `kickoff`, `venue_city`, `host`, `home`, `away`.
#' @noRd
ih_upcoming <- function(spec, results, actual) {
  played_key <- if (is.null(results) || !nrow(results)) {
    character()
  } else {
    c(paste(results$stage, results$home_team, results$away_team), paste(results$stage, results$away_team, results$home_team))
  }
  rows <- list()
  pf <- spec$prelim
  rows$pre <- data.frame(
    stage = "preliminary", group = pf$group, round = NA_character_, code = NA_character_,
    match_date = pf$match_date, kickoff = pf$kickoff, venue_city = pf$venue_city, host = pf$host,
    home = pf$home_team, away = pf$away_team, stringsAsFactors = FALSE
  )
  for (G in names(spec$main)) {
    s <- actual$main[[G]]$slots
    if (is.null(s)) next
    mf <- spec$main_fixtures[spec$main_fixtures$group == G, , drop = FALSE]
    rows[[G]] <- data.frame(
      stage = "main", group = G, round = NA_character_, code = NA_character_,
      match_date = mf$match_date, kickoff = mf$kickoff, venue_city = NA_character_,
      host = unname(spec$main_host[G]), home = unname(s[mf$home_slot]), away = unname(s[mf$away_slot]),
      stringsAsFactors = FALSE
    )
  }
  ko <- spec$knockout
  kh <- vapply(ko$code, function(cd) actual$knockout[[cd]]$home, character(1))
  ka <- vapply(ko$code, function(cd) actual$knockout[[cd]]$away, character(1))
  rows$ko <- data.frame(
    stage = "knockout", group = NA_character_, round = ko$round, code = ko$code,
    match_date = ko$match_date, kickoff = ko$kickoff, venue_city = ko$venue_city, host = ko$host,
    home = unname(kh), away = unname(ka), stringsAsFactors = FALSE
  )
  up <- do.call(rbind, rows)
  up <- up[!is.na(up$home) & !is.na(up$away), , drop = FALSE]
  up <- up[!paste(up$stage, up$home, up$away) %in% played_key, , drop = FALSE]
  up[order(up$match_date, up$kickoff), , drop = FALSE]
}

#' Probabilities and score summaries of known pairings
#'
#' @param fit [ih_fit()].
#' @param up Rows of [ih_upcoming()].
#' @param n_sims Simulations per match (the fit's draws reused in turn).
#' @param seed Seed; the caller's stream is left as it was.
#' @return `up` with `p_home`, `p_draw`, `p_away` (after 60 minutes),
#'   `p_advance` (knockouts: the home side goes through), `eg_home`,
#'   `eg_away` and `goal_diff_distribution` (a list of data frames `diff`, `p`
#'   in the Iceland leagues' two-goal buckets).
#' @noRd
ih_predict_upcoming <- function(fit, up, n_sims = 20000L, seed = 1L) {
  if (!nrow(up)) {
    return(up)
  }
  withr::with_seed(as.integer(seed), {
    draws <- rep_len(seq_len(nrow(fit$m_state)), n_sims)
    h <- matrix(match(up$home, fit$teams), n_sims, nrow(up), byrow = TRUE)
    a <- matrix(match(up$away, fit$teams), n_sims, nrow(up), byrow = TRUE)
    venue <- .ih_venue_of(h, a, match(up$host, fit$teams))
    sc <- .ih_sim_scores(fit, h, a, venue, draws)
    hw <- .ih_knockout_home_wins(sc$home, sc$away, sc$loc_m, sc$sigma_m)
  })
  d <- sc$home - sc$away
  up$p_home <- colMeans(d > 0)
  up$p_draw <- colMeans(d == 0)
  up$p_away <- colMeans(d < 0)
  up$p_advance <- ifelse(up$stage == "knockout", colMeans(hw), NA_real_)
  up$eg_home <- colMeans(sc$home)
  up$eg_away <- colMeans(sc$away)
  up$goal_diff_distribution <- lapply(seq_len(nrow(up)), function(j) {
    b <- .bucket_centre_2dt(d[, j], 2L, -20L, 20L)
    tb <- table(b) / n_sims
    data.frame(diff = as.integer(names(tb)), p = as.numeric(tb))
  })
  up
}

#' Write the five files of a championship forecast
#'
#' @param fit [ih_fit()]; `sim` [ih_simulate_tournament()]; `spec`
#'   [ih_tournament_spec()]; `results` [ih_tournament_results()].
#' @param out_dir Folder to write into (created).
#' @param names [ih_team_names()].
#' @param h2h_reapply As used for `sim`.
#' @param generated_at Timestamp written into every file.
#' @return Invisibly, the paths written.
#' @noRd
ih_publish_tournament <- function(fit, sim, spec, results, out_dir, names = ih_team_names(),
                                  h2h_reapply = "none", n_sims_matches = 20000L, seed = 1L,
                                  generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  is_name <- function(x) names$team_is[match(x, names$team)]
  iso <- function(x) names$iso2[match(x, names$team)]
  missing_is <- setdiff(unlist(spec$groups), names$team[!is.na(names$team_is)])
  if (length(missing_is)) cli::cli_abort("No Icelandic name for {.val {missing_is}}.")
  rnd <- function(x, d = 4) round(x, d)
  write <- function(x, file) {
    path <- file.path(out_dir, file)
    jsonlite::write_json(x, path, auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null", na = "null")
    path
  }
  actual <- ih_actual_standings(spec, results, h2h_reapply)
  tm <- sim$teams
  venue_is <- function(city) if (length(city) && !is.na(city)) city else NULL
  team_row <- function(t, tab, proj_points, proj_gd, p_pos, p_advance) {
    r <- tab[tab$team == t, ]
    list(
      team = t, team_is = is_name(t), iso2 = iso(t),
      played = as.integer(r$played), won = as.integer(r$won), drawn = as.integer(r$drawn),
      lost = as.integer(r$lost), points = as.integer(r$points), gf = as.integer(r$gf),
      ga = as.integer(r$ga), gd = as.integer(r$gd),
      proj_points = rnd(proj_points, 2), proj_gd = rnd(proj_gd, 2),
      p_pos = as.list(rnd(p_pos)), p_advance = rnd(p_advance)
    )
  }
  # WHY two orders: a finished group is listed as the rules rank it
  # (ih_rank_table()'s position, head to head before overall goal difference),
  # the order its p_pos and the main-round slots already follow; while it is
  # being played, by points, goal difference and goals, then the simulated
  # expected position (lots would shuffle teams that have not played).
  ordered_teams <- function(tab, exp_pos, complete) {
    if (isTRUE(complete)) {
      return(tab$team[order(tab$position)])
    }
    tab$exp_pos <- exp_pos[tab$team]
    tab$team[order(-tab$points, -tab$gd, -tab$gf, tab$exp_pos)]
  }

  # ---- groups.json ----
  groups <- list()
  for (g in names(spec$groups)) {
    tab <- actual$prelim[[g]]$table
    rows <- tm[tm$group == g, , drop = FALSE]
    pp <- as.matrix(rows[, paste0("p_pos", 1:4)])
    exp_pos <- stats::setNames(as.vector(pp %*% (1:4)), rows$team)
    ord <- ordered_teams(tab, exp_pos, actual$prelim[[g]]$complete)
    groups[[length(groups) + 1L]] <- list(
      group = g, label_is = paste("Ri\u00f0ill", g), stage = "preliminary",
      venue_is = venue_is(unique(spec$prelim$venue_city[spec$prelim$group == g])),
      advance_n = spec$n_main,
      teams = lapply(ord, function(t) {
        r <- rows[rows$team == t, ]
        team_row(t, tab, r$proj_points, r$proj_gd, as.numeric(r[, paste0("p_pos", 1:4)]), r$p_advance)
      })
    )
  }
  for (G in names(spec$main)) {
    am <- actual$main[[G]]
    if (is.null(am$members)) next
    d <- sim$main[[G]]
    d <- d[d$team %in% am$members, , drop = FALSE]
    k <- length(am$members)
    pp <- as.matrix(d[, paste0("p_pos", seq_len(k))])
    exp_pos <- stats::setNames(as.vector(pp %*% seq_len(k)), d$team)
    ord <- ordered_teams(am$table, exp_pos, am$complete)
    adv <- 2L
    groups[[length(groups) + 1L]] <- list(
      group = G, label_is = paste("Milliri\u00f0ill", G), stage = "main",
      venue_is = venue_is(unname(spec$main_city[G])), advance_n = adv,
      teams = lapply(ord, function(t) {
        r <- d[d$team == t, ]
        team_row(
          t, am$table, r$proj_points, r$proj_gd, as.numeric(r[, paste0("p_pos", seq_len(k))]),
          sum(as.numeric(r[, paste0("p_pos", seq_len(adv))]))
        )
      })
    )
  }
  paths <- write(list(schema_version = 1L, generated_at = generated_at, groups = groups), "groups.json")

  # ---- predictions.json ----
  up <- ih_predict_upcoming(fit, ih_upcoming(spec, results, actual), n_sims_matches, seed)
  matches <- lapply(seq_len(nrow(up)), function(i) {
    r <- up[i, ]
    out <- list(
      match_date = as.character(r$match_date),
      kickoff = if (!is.na(r$kickoff)) format(r$kickoff, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC") else NULL,
      stage = r$stage, group = if (!is.na(r$group)) r$group else NULL,
      round = if (!is.na(r$round)) r$round else NULL,
      venue_is = venue_is(r$venue_city),
      home = r$home, home_is = is_name(r$home), away = r$away, away_is = is_name(r$away),
      p_home = rnd(r$p_home), p_draw = rnd(r$p_draw), p_away = rnd(r$p_away),
      p_advance = if (!is.na(r$p_advance)) rnd(r$p_advance) else NULL,
      eg_home = rnd(r$eg_home, 2), eg_away = rnd(r$eg_away, 2),
      goal_diff_distribution = lapply(seq_len(nrow(r$goal_diff_distribution[[1]])), function(k) {
        gd <- r$goal_diff_distribution[[1]]
        list(diff = gd$diff[k], p = rnd(gd$p[k]))
      })
    )
    out[!vapply(out, is.null, logical(1))]
  })
  paths <- c(paths, write(list(schema_version = 1L, generated_at = generated_at, matches = matches), "predictions.json"))

  # ---- tournament_placements.json ----
  records <- do.call(c, lapply(seq_len(nrow(tm)), function(i) {
    lapply(spec$rounds, function(rd) {
      list(
        team = tm$team[i], team_is = is_name(tm$team[i]), round_name = rd,
        probability = rnd(tm[[paste0("p_", rd)]][i])
      )
    })
  }))
  summ <- tm[order(-tm$p_Champion), c("team", "p_Champion")]
  paths <- c(paths, write(list(
    schema_version = 1L, generated_at = generated_at,
    rounds = lapply(spec$rounds, function(rd) list(code = rd)),
    records = records,
    summary = lapply(seq_len(nrow(summ)), function(i) {
      list(team = summ$team[i], team_is = is_name(summ$team[i]), p_champion = rnd(summ$p_Champion[i]))
    })
  ), "tournament_placements.json"))

  # ---- team_strengths.json ----
  teams <- unlist(spec$groups, use.names = FALSE)
  m <- fit$m_state[, teams, drop = FALSE]
  tt <- fit$t_state[, teams, drop = FALSE]
  mc <- m - rowMeans(m)
  tc <- tt - rowMeans(tt)
  comp <- list(total = mc, offence = (mc + tc) / 2, defence = (mc - tc) / 2)
  rating <- colMeans(mc)
  rank <- rank(-rating, ties.method = "first")
  st_teams <- lapply(teams, function(t) {
    list(
      team = t, team_is = is_name(t), iso2 = iso(t), rating = rnd(rating[[t]], 3),
      offence = rnd(mean(comp$offence[, t]), 3), defence = rnd(mean(comp$defence[, t]), 3),
      rating_rank = as.integer(rank[[t]])
    )
  })
  st_teams <- st_teams[order(vapply(st_teams, `[[`, integer(1), "rating_rank"))]
  st_records <- list()
  for (t in teams) {
    for (cn in names(comp)) {
      for (cv in c(0.5, 0.8, 0.95)) {
        q <- stats::quantile(comp[[cn]][, t], c((1 - cv) / 2, 0.5, (1 + cv) / 2), names = FALSE)
        st_records[[length(st_records) + 1L]] <- list(
          team = t, team_is = is_name(t), component = cn, location = "avg", coverage = cv,
          median = rnd(q[2], 3), lower = rnd(q[1], 3), upper = rnd(q[3], 3)
        )
      }
    }
  }
  paths <- c(paths, write(list(
    schema_version = 1L, generated_at = generated_at, fit_date = as.character(fit$as_of),
    teams = st_teams, records = st_records
  ), "team_strengths.json"))

  # ---- meta.json ----
  n_played <- if (is.null(results)) 0L else sum(results$stage == "preliminary")
  fav <- summ[1, ]
  paths <- c(paths, write(list(
    schema_version = 1L, placeholder = FALSE, generated_at = generated_at,
    fit_date = as.character(fit$as_of), start_date = as.character(spec$start), end_date = as.character(spec$end),
    title = spec$title, n_teams = length(teams), n_groups = length(spec$groups), n_draws = as.integer(sim$n_sims),
    matches_played = as.integer(n_played), matches_group_total = nrow(spec$prelim),
    focus_team = spec$focus_team, focus_round = if ("QF" %in% spec$rounds) "QF" else "SF",
    champion_favourite = list(team = fav$team, team_is = is_name(fav$team), p_champion = rnd(fav$p_Champion)),
    model = list(
      engine = "inla", model_version = "ih-v2.0",
      n_train = fit$n_train, hfa = rnd(mean(fit$hfa), 3),
      sigma_margin = rnd(mean(fit$sigma_m), 3), h2h_reapply = h2h_reapply
    )
  ), "meta.json"))
  invisible(paths)
}
