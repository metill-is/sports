# 2027 Women's World Cup: the UEFA play-off forecast between and after its
# legs, the finals forecast after the 11 December 2026 draw, and their publish
# tree data/publish/world_cup/kvenna/. The tree has no schema in
# config/publish-schemas/ and no metill-platform route reads it (the platform
# reads world_cup/karla/ for the 2026 men's tournament), so it is sports-side
# JSON only until the owner decides otherwise (owner, 24 November 2026:
# "Sports-side JSON only"). Icelandic names at the boundary.

#' Forecast a round of two-legged ties from a national-team fit.
#'
#' Each tie is played over two legs with no away goals; level on aggregate,
#' extra time is played at the second leg, then penalties
#' ([nt_two_leg_win_prob()]). Results already known enter in two ways: a
#' played first leg reweights the posterior draws by the probability of its
#' score (the update from that match, joint over every tie played, since all
#' ties share the draws) and its margin is carried into the second leg; a
#' decided tie is certain.
#'
#' @param fit A fit from [nt_fit_inla()].
#' @param ties Data frame, one row per tie: `team_a` (hosts the first leg),
#'   `team_b`; optionally `leg1_venue` and `leg2_venue` (1 = the host is at
#'   home, 0 = neutral; default 1), `leg1_goals_a`, `leg1_goals_b` (the first
#'   leg's score, `NA` while unplayed) and `winner` (`NA` until decided).
#' @param et_factor,shootout_p See [nt_two_leg_win_prob()].
#' @return List: `ties` (the input with `status`: `"scheduled"`,
#'   `"first_leg_played"` or `"decided"`), `draws` (draws x ties matrices
#'   `a_wins`, `a_regulation`, `a_extra_time`, `a_shootout`, `b_regulation`,
#'   `b_extra_time`, `b_shootout`; the parts are `NA` for a decided tie),
#'   `weights` (one per draw, summing to one), `ess` (effective number of
#'   draws), `rates` (`leg1` and `leg2` from [nt_match_rates()], the first
#'   leg with `team_a` as home side, the second with `team_b`), `et_factor`
#'   and `shootout_p`.
#' @export
nt_two_leg_forecast <- function(fit, ties, et_factor = 1 / 3, shootout_p = 0.5) {
  ties <- tibble::as_tibble(ties)
  k <- nrow(ties)
  col <- function(name, default) if (name %in% names(ties)) ties[[name]] else rep(default, k)
  g1a <- as.integer(col("leg1_goals_a", NA_integer_))
  g1b <- as.integer(col("leg1_goals_b", NA_integer_))
  winner <- as.character(col("winner", NA_character_))
  winner[!is.na(winner) & !nzchar(winner)] <- NA_character_
  if (any(is.na(g1a) != is.na(g1b))) {
    cli::cli_abort("A first-leg score needs both {.field leg1_goals_a} and {.field leg1_goals_b}.")
  }
  if (any(g1a < 0L | g1b < 0L, na.rm = TRUE)) {
    cli::cli_abort("First-leg goals cannot be negative.")
  }
  bad <- !is.na(winner) & winner != ties$team_a & winner != ties$team_b
  if (any(bad)) {
    cli::cli_abort("A winner must be one of the tie's teams: {.val {winner[bad]}}.")
  }
  leg1 <- nt_match_rates(fit, ties$team_a, ties$team_b, venue = col("leg1_venue", 1L))
  leg2 <- nt_match_rates(fit, ties$team_b, ties$team_a, venue = col("leg2_venue", 1L))
  n <- nrow(leg1$home)
  played <- !is.na(g1a)
  logw <- numeric(n)
  for (j in which(played)) {
    logw <- logw + stats::dpois(g1a[j], leg1$home[, j], log = TRUE) +
      stats::dpois(g1b[j], leg1$away[, j], log = TRUE)
  }
  w <- exp(logw - max(logw))
  w <- w / sum(w)

  status <- ifelse(!is.na(winner), "decided", ifelse(played, "first_leg_played", "scheduled"))
  parts <- c("a_wins", "a_regulation", "a_extra_time", "a_shootout", "b_regulation", "b_extra_time", "b_shootout")
  draws <- stats::setNames(rep(list(matrix(NA_real_, n, k)), length(parts)), parts)
  for (j in seq_len(k)) {
    if (status[j] == "decided") {
      draws$a_wins[, j] <- as.numeric(winner[j] == ties$team_a[j])
      next
    }
    tie <- if (played[j]) {
      nt_two_leg_win_prob(0, 0, leg2$away[, j], leg2$home[, j], et_factor, shootout_p,
        lead = g1a[j] - g1b[j]
      )
    } else {
      nt_two_leg_win_prob(
        leg1$home[, j], leg1$away[, j], leg2$away[, j], leg2$home[, j],
        et_factor, shootout_p
      )
    }
    for (p in parts) draws[[p]][, j] <- tie[[p]]
  }
  ties$leg1_goals_a <- g1a
  ties$leg1_goals_b <- g1b
  ties$winner <- winner
  ties$status <- status
  list(
    ties = ties, draws = draws, weights = w, ess = 1 / sum(w^2),
    rates = list(leg1 = leg1, leg2 = leg2), et_factor = et_factor, shootout_p = shootout_p
  )
}

# Weighted quantiles of per-draw values; with equal weights the same estimator
# as stats::quantile().
.nt_wquantile <- function(x, w, probs) {
  if (diff(range(w)) < 1e-12) {
    return(unname(stats::quantile(x, probs)))
  }
  o <- order(x)
  cw <- cumsum(w[o]) / sum(w)
  vapply(probs, function(p) x[o][which(cw >= p - 1e-12)[1]], numeric(1))
}

# Scoreline grid (home goals in rows, 0..max_goals) averaged over draws with
# weights `w`.
.nt_weighted_grid <- function(rate_home, rate_away, w, max_goals = 10L) {
  g <- 0:max_goals
  ph <- outer(rate_home, g, function(l, x) stats::dpois(x, l))
  pa <- outer(rate_away, g, function(l, x) stats::dpois(x, l))
  grid <- crossprod(ph * w, pa)
  dimnames(grid) <- list(g, g)
  grid
}

# Distribution of A's goals minus B's (plus `lead`), averaged over draws.
.nt_margin_probs <- function(lam_a, lam_b, w, lead = 0L, margins = -30:30, max_goals = 40L) {
  g <- 0:max_goals
  pb <- outer(lam_b, g, function(l, x) stats::dpois(x, l))
  vapply(margins, function(m) {
    pa <- outer(lam_a, g + m - lead, function(l, x) stats::dpois(x, l))
    sum(w * rowSums(pb * pa))
  }, numeric(1))
}

.wwc_namer <- function(country_csv) {
  is_name <- .wc_country_namer(country_csv)
  known <- utils::read.csv(country_csv, fileEncoding = "UTF-8", stringsAsFactors = FALSE)$team
  function(teams) {
    miss <- setdiff(unique(teams), known)
    if (length(miss) > 0L) {
      cli::cli_abort(c(
        "No Icelandic name for {.val {miss}}.",
        i = "Add {cli::qty(length(miss))}{?it/them} to {.path {country_csv}}."
      ))
    }
    is_name(teams)
  }
}

.wwc_write_json <- function(x, path) {
  jsonlite::write_json(x, path,
    auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null", na = "null"
  )
}

.wwc_meta <- function(stage, fit_date, data_through, n_draws, generated_at, extra = list()) {
  c(
    list(
      tournament = "HM kvenna 2027", tournament_en = "FIFA Women's World Cup 2027",
      sport = "football", country = "world", sex = "female", stage = stage,
      generated_at = generated_at, fit_date = as.character(fit_date),
      data_through = as.character(data_through), n_draws = as.integer(n_draws)
    ),
    extra
  )
}

#' Publish the 2027 Women's World Cup play-off forecast.
#'
#' Writes `meta.json` and `playoff.json` under
#' `<root>/publish/world_cup/kvenna/`. Per tie: status, the two legs (score
#' once played; otherwise home/draw/away probabilities, expected goals and the
#' ten likeliest scores), P(each side wins) with the 90% interval of the
#' per-draw value, how the tie is decided (180 minutes, extra time,
#' penalties), the aggregate margin over 180 minutes and, while the first leg
#' is unplayed, P(`team_a` wins) after each first-leg score up to 4-4. Then
#' every team's chance of a direct place and of the inter-confederation
#' play-off ([nt_playoff_places()]). Probabilities are averaged over the
#' forecast's weighted draws and rounded to four decimals.
#'
#' @param forecast Output of [nt_two_leg_forecast()]; its `ties` need
#'   `rank_a` and `rank_b` (smaller is better) and may carry `leg1_date` and
#'   `leg2_date`.
#' @param n_direct Number of winners that qualify directly (seven of eight in
#'   the 2027 UEFA play-offs).
#' @param fit_date,data_through The fit's cutoff and its last match date.
#' @param root Data root.
#' @param country_csv English to Icelandic team names; a team missing from it
#'   is an error.
#' @param featured_team The team a page leads with (recorded in `meta.json`).
#' @param generated_at Timestamp string; default now (UTC).
#' @return Invisibly the output directory.
#' @export
publish_wwc2027_playoff <- function(forecast, n_direct = 7L, fit_date, data_through,
                                    root = here::here("data"),
                                    country_csv = here::here(
                                      "data", "wc", "structure", "country_names_is.csv"
                                    ),
                                    featured_team = "Iceland",
                                    generated_at = NULL) {
  ties <- forecast$ties
  for (need in c("rank_a", "rank_b")) {
    if (!need %in% names(ties)) cli::cli_abort("{.arg forecast} ties need {.field {need}}.")
  }
  is_name <- .wwc_namer(country_csv)
  all_teams <- c(ties$team_a, ties$team_b)
  is_name(c(all_teams, featured_team))
  generated_at <- generated_at %||% format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  out_dir <- file.path(root, "publish", "world_cup", "kvenna")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  w <- forecast$weights
  d <- forecast$draws
  r1 <- forecast$rates$leg1
  r2 <- forecast$rates$leg2
  wmean <- function(x) sum(x * w)
  rnd <- function(x, digits = 4L) round(x, digits)
  date_of <- function(name, j) {
    if (!name %in% names(ties) || is.na(ties[[name]][j]) || !nzchar(ties[[name]][j])) {
      return(NULL)
    }
    as.character(ties[[name]][j])
  }

  leg_record <- function(leg, j, home, away, rate_home, rate_away, played, goals_home, goals_away, open) {
    rec <- list(
      leg = leg, date = date_of(paste0("leg", leg, "_date"), j),
      home = home, home_is = is_name(home), away = away, away_is = is_name(away),
      played = played,
      goals_home = if (played) goals_home else NULL,
      goals_away = if (played) goals_away else NULL
    )
    if (played || !open) {
      return(rec)
    }
    grid <- .nt_weighted_grid(rate_home, rate_away, w)
    top <- arrayInd(order(grid, decreasing = TRUE)[1:10], dim(grid))
    c(rec, list(
      p_home = rnd(sum(grid[lower.tri(grid)])), p_draw = rnd(sum(diag(grid))),
      p_away = rnd(sum(grid[upper.tri(grid)])),
      xg_home = rnd(wmean(rate_home), 3L), xg_away = rnd(wmean(rate_away), 3L),
      scorelines = lapply(seq_len(nrow(top)), function(i) {
        list(home_goals = top[i, 1] - 1L, away_goals = top[i, 2] - 1L, p = rnd(grid[top[i, , drop = FALSE]]))
      })
    ))
  }

  tie_record <- function(j) {
    a <- ties$team_a[j]
    b <- ties$team_b[j]
    st <- ties$status[j]
    played <- st != "scheduled" && !is.na(ties$leg1_goals_a[j])
    decided <- st == "decided"
    p_a <- wmean(d$a_wins[, j])
    q <- .nt_wquantile(d$a_wins[, j], w, c(0.05, 0.95))
    rec <- list(
      tie = j, status = st,
      team_a = a, team_a_is = is_name(a), rank_a = as.integer(ties$rank_a[j]),
      team_b = b, team_b_is = is_name(b), rank_b = as.integer(ties$rank_b[j]),
      winner = if (decided) ties$winner[j] else NULL,
      winner_is = if (decided) is_name(ties$winner[j]) else NULL,
      p_a = rnd(p_a), p_b = rnd(1 - p_a),
      p_a_interval = rnd(q), p_b_interval = rnd(rev(1 - q)),
      legs = list(
        leg_record(1L, j, a, b, r1$home[, j], r1$away[, j], played,
          ties$leg1_goals_a[j], ties$leg1_goals_b[j],
          open = !decided
        ),
        leg_record(2L, j, b, a, r2$home[, j], r2$away[, j], FALSE, NA, NA, open = !decided)
      )
    )
    if (decided) {
      return(c(rec, list(
        decided_by = NULL, p_extra_time = NULL, p_penalties = NULL,
        aggregate_margin = NULL, after_first_leg = NULL
      )))
    }
    part <- function(p) wmean(d[[p]][, j])
    lead <- if (played) ties$leg1_goals_a[j] - ties$leg1_goals_b[j] else 0L
    lam_a <- (if (played) 0 else r1$home[, j]) + r2$away[, j]
    lam_b <- (if (played) 0 else r1$away[, j]) + r2$home[, j]
    margins <- -30:30
    pm <- .nt_margin_probs(lam_a, lam_b, w, lead = lead, margins = margins)
    mid <- abs(margins) <= 4L
    agg <- c(sum(pm[margins <= -5]), pm[mid], sum(pm[margins >= 5]))
    after <- NULL
    if (!played) {
      grid1 <- .nt_weighted_grid(r1$home[, j], r1$away[, j], w)
      cells <- expand.grid(goals_a = 0:4, goals_b = 0:4)
      after <- lapply(seq_len(nrow(cells)), function(i) {
        x <- cells$goals_a[i]
        y <- cells$goals_b[i]
        lw <- w * stats::dpois(x, r1$home[, j]) * stats::dpois(y, r1$away[, j])
        second <- nt_two_leg_win_prob(0, 0, r2$away[, j], r2$home[, j],
          forecast$et_factor, forecast$shootout_p,
          lead = x - y
        )
        list(
          goals_a = x, goals_b = y, p_score = rnd(grid1[x + 1L, y + 1L]),
          p_a = rnd(sum(lw * second$a_wins) / sum(lw))
        )
      })
    }
    c(rec, list(
      decided_by = list(
        a_regulation = rnd(part("a_regulation")), a_extra_time = rnd(part("a_extra_time")),
        a_penalties = rnd(part("a_shootout")), b_regulation = rnd(part("b_regulation")),
        b_extra_time = rnd(part("b_extra_time")), b_penalties = rnd(part("b_shootout"))
      ),
      p_extra_time = rnd(part("a_extra_time") + part("b_extra_time") + part("a_shootout") + part("b_shootout")),
      p_penalties = rnd(part("a_shootout") + part("b_shootout")),
      aggregate_margin = lapply(seq_along(agg), function(i) {
        list(margin_a = c("<= -5", as.character(margins[mid]), ">= 5")[i], p = rnd(agg[i]))
      }),
      after_first_leg = after
    ))
  }

  places <- nt_playoff_places(d$a_wins, ties, n_direct = n_direct, weights = w)
  playoff <- list(
    generated_at = generated_at, fit_date = as.character(fit_date),
    data_through = as.character(data_through), n_draws = length(w),
    effective_draws = round(forecast$ess, 1),
    rules = list(
      away_goals = FALSE, extra_time = "two 15-minute periods at the second leg",
      penalties = TRUE, n_direct = as.integer(n_direct)
    ),
    ties = lapply(seq_len(nrow(ties)), tie_record),
    places = lapply(seq_len(nrow(places)), function(i) {
      list(
        team = places$team[i], team_is = is_name(places$team[i]), rank = as.integer(places$rank[i]),
        p_win = rnd(places$p_win[i]), p_direct = rnd(places$p_direct[i]),
        p_interconf_playoff = rnd(places$p_other[i])
      )
    })
  )
  .wwc_write_json(playoff, file.path(out_dir, "playoff.json"))
  .wwc_write_json(
    .wwc_meta("playoff", fit_date, data_through, length(w), generated_at, list(
      featured_team = featured_team, featured_team_is = is_name(featured_team),
      n_ties = nrow(ties), files = list("playoff.json")
    )),
    file.path(out_dir, "meta.json")
  )
  invisible(out_dir)
}

#' Forecast the 2027 Women's World Cup finals from the draw.
#'
#' Simulates the tournament with [nt_simulate_world_cup()] on
#' [nt_wwc2027_structure()]. At the draw of 11 December 2026 three places
#' belong to the inter-confederation play-off of February 2027; a row of
#' `draw` holding such a place names a placeholder in `team` and its
#' candidates with weights in `candidates`. Every combination of candidates is
#' simulated with its share of `n_sims` (rounded; combinations rounding to no
#' runs are dropped), and the results are pooled, so each team's numbers are
#' the mixture over who qualifies.
#'
#' @param fit A fit from [nt_fit_inla()].
#' @param draw Data frame `group` (A to H), `team` (martj42 spelling, or a
#'   placeholder label) and optionally `candidates`
#'   (`"Team A:0.5;Team B:0.3"`, weights normalised; empty for a drawn team).
#' @param n_sims Simulated tournaments in all.
#' @param group_rule,host_knockout_home Passed to [nt_wwc2027_structure()];
#'   the defaults are the owner's decision of 24 November 2026 (the 2023
#'   group order, Brazil at home in knockouts).
#' @param seed Integer; combination `c` runs with seed `1000 * seed + c`.
#' @return List: `teams` (tibble `team`, `group`, `placeholder` (`NA` for a
#'   drawn team), `p_in_finals`, `p_first`, `p_second`, `p_third`,
#'   `p_fourth`, `p_advance` (reaching the round of 16), `p_qf`, `p_sf`,
#'   `p_final`, `p_champion`, `p_bronze`; by `p_champion`), `group_rule`,
#'   `knockout_hosts`, `n_sims` (runs used), `combos` and `weight`.
#' @export
nt_wwc2027_finals <- function(fit, draw, n_sims = 20000L, group_rule = "wwc_2023",
                              host_knockout_home = TRUE, seed = 1L) {
  draw <- as.data.frame(draw, stringsAsFactors = FALSE)
  if (!"candidates" %in% names(draw)) draw$candidates <- ""
  draw$candidates[is.na(draw$candidates)] <- ""
  holders <- which(nzchar(trimws(draw$candidates)))
  cand <- lapply(holders, function(i) {
    parts <- strsplit(strsplit(draw$candidates[i], ";", fixed = TRUE)[[1]], ":", fixed = TRUE)
    w <- as.numeric(vapply(parts, `[`, "", 2))
    if (anyNA(w) || any(w < 0) || sum(w) <= 0) {
      cli::cli_abort("Bad candidate weights for {.val {draw$team[i]}}: {.val {draw$candidates[i]}}.")
    }
    stats::setNames(w / sum(w), trimws(vapply(parts, `[`, "", 1)))
  })
  combos <- if (length(holders) == 0L) {
    data.frame(row.names = 1L)
  } else {
    expand.grid(lapply(cand, names), stringsAsFactors = FALSE)
  }
  weight <- if (length(holders) == 0L) {
    1
  } else {
    Reduce(`*`, lapply(seq_along(holders), function(k) cand[[k]][combos[[k]]]))
  }
  runs <- as.integer(round(n_sims * weight))
  cols <- c(
    "p_first", "p_second", "p_third", "p_fourth", "p_advance", "p_qf", "p_sf",
    "p_final", "p_champion", "p_bronze"
  )
  pooled <- NULL
  s <- NULL
  for (c in which(runs > 0L)) {
    d <- draw
    for (k in seq_along(holders)) d$team[holders[k]] <- combos[c, k]
    groups <- split(d$team, d$group)[LETTERS[1:8]]
    s <- nt_wwc2027_structure(groups, group_rule = group_rule, host_knockout_home = host_knockout_home)
    sim <- nt_simulate_world_cup(fit, s, n_sims = runs[c], seed = 1000L * seed + c)$teams
    hold <- match(sim$team, d$team[holders])
    sim$placeholder <- draw$team[holders][hold]
    sim$n <- runs[c]
    pooled <- rbind(pooled, as.data.frame(sim[c("team", "group", "placeholder", "n", cols)]))
  }
  if (is.null(pooled)) cli::cli_abort("{.arg n_sims} leaves no combination with a run.")
  used <- sum(runs[runs > 0L])
  teams <- do.call(rbind, lapply(split(pooled, pooled$team), function(x) {
    data.frame(
      team = x$team[1], group = x$group[1], placeholder = x$placeholder[1],
      p_in_finals = sum(x$n) / used,
      t(colSums(as.matrix(x[cols]) * x$n) / used)
    )
  }))
  teams <- tibble::as_tibble(teams[order(-teams$p_champion, teams$team), ])
  list(
    teams = teams, group_rule = s$group_rule, knockout_hosts = s$knockout_hosts,
    n_sims = used, combos = combos, weight = weight
  )
}

#' Publish the 2027 Women's World Cup finals forecast.
#'
#' Writes `groups.json` (per group, every team that can be in it: Icelandic
#' name, placeholder label, P(in the finals), group-position probabilities,
#' P(reaching the round of 16)), `tournament_placements.json` (the shape of
#' the 2026 men's tree: one record per team and round reached, R16 to
#' Champion, Third and Fourth) and `meta.json` (`stage = "finals"`) under
#' `<root>/publish/world_cup/kvenna/`. `playoff.json`, if there, is kept.
#'
#' @param finals Output of [nt_wwc2027_finals()].
#' @param fit_date,data_through The fit's cutoff and its last match date.
#' @param n_draws Posterior draws in the fit (recorded in `meta.json`).
#' @inheritParams publish_wwc2027_playoff
#' @return Invisibly the output directory.
#' @export
publish_wwc2027_finals <- function(finals, fit_date, data_through, n_draws = NA_integer_,
                                   root = here::here("data"),
                                   country_csv = here::here(
                                     "data", "wc", "structure", "country_names_is.csv"
                                   ),
                                   generated_at = NULL) {
  t <- finals$teams
  is_name <- .wwc_namer(country_csv)
  is_name(t$team)
  generated_at <- generated_at %||% format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  out_dir <- file.path(root, "publish", "world_cup", "kvenna")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  rnd <- function(x) round(x, 4L)

  groups <- lapply(LETTERS[1:8], function(g) {
    rows <- t[t$group == g, , drop = FALSE]
    rows <- rows[order(-rows$p_advance, rows$team), , drop = FALSE]
    list(group = g, teams = lapply(seq_len(nrow(rows)), function(i) {
      r <- rows[i, ]
      list(
        team = r$team, team_is = is_name(r$team),
        placeholder = if (is.na(r$placeholder)) NULL else r$placeholder,
        p_in_finals = rnd(r$p_in_finals),
        p_first = rnd(r$p_first), p_second = rnd(r$p_second),
        p_third = rnd(r$p_third), p_fourth = rnd(r$p_fourth),
        p_advance = rnd(r$p_advance)
      )
    }))
  })
  .wwc_write_json(
    list(
      generated_at = generated_at, fit_date = as.character(fit_date),
      group_rule = finals$group_rule, groups = groups
    ),
    file.path(out_dir, "groups.json")
  )

  reach <- list(
    R16 = t$p_advance, QF = t$p_qf, SF = t$p_sf, Final = t$p_final, Champion = t$p_champion,
    Third = t$p_bronze, Fourth = pmax(t$p_sf - t$p_final - t$p_bronze, 0)
  )
  records <- unlist(lapply(seq_len(nrow(t)), function(i) {
    lapply(names(reach), function(rn) {
      list(team = t$team[i], team_is = is_name(t$team[i]), round_name = rn, probability = rnd(reach[[rn]][i]))
    })
  }), recursive = FALSE)
  .wwc_write_json(
    list(
      generated_at = generated_at, season = 2027L, n_teams = 32L, records = records,
      summary = lapply(seq_len(nrow(t)), function(i) {
        list(
          team = t$team[i], team_is = is_name(t$team[i]),
          p_champion = rnd(t$p_champion[i]), p_bronze = rnd(t$p_bronze[i])
        )
      })
    ),
    file.path(out_dir, "tournament_placements.json")
  )

  fav <- which.max(t$p_champion)
  files <- intersect(c("groups.json", "tournament_placements.json", "playoff.json"), list.files(out_dir))
  .wwc_write_json(
    .wwc_meta("finals", fit_date, data_through, n_draws, generated_at, list(
      n_teams = 32L, n_groups = 8L, n_sims = as.integer(finals$n_sims),
      group_rule = finals$group_rule, knockout_hosts = as.list(finals$knockout_hosts),
      champion_favourite = list(
        team = t$team[fav], team_is = is_name(t$team[fav]), p_champion = rnd(t$p_champion[fav])
      ),
      files = as.list(files)
    )),
    file.path(out_dir, "meta.json")
  )
  invisible(out_dir)
}
