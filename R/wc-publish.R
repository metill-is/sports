#' @include wc-simulate.R
NULL

# Publish layer for the World Cup forecast. Writes the JSON data contract under
# data/publish/world_cup/karla/ (its own namespace so it never collides with the
# Iceland football schemas). Icelandic names at the boundary.

.wc_country_namer <- function(country_csv) {
  cn <- utils::read.csv(country_csv, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  function(team) {
    i <- match(team, cn$team)
    ifelse(is.na(i), team, cn$name_is[i])
  }
}

# Posterior coverage intervals per (team, component) for the platform's
# strength forest plot — the league team_strengths contract (median/lower/
# upper at 50/80/95% coverage). Location is fixed to "avg": every WC
# knockout match is at a neutral venue, so the league's home/away axis
# does not exist here.
.wc_strength_records <- function(sim_inputs_team, teams) {
  sit <- sim_inputs_team[sim_inputs_team$team %in% teams, , drop = FALSE]
  long <- dplyr::bind_rows(
    tibble::tibble(
      team = sit$team, component = "total",
      value = sit$cur_offense + sit$cur_defense
    ),
    tibble::tibble(team = sit$team, component = "offence", value = sit$cur_offense),
    tibble::tibble(team = sit$team, component = "defence", value = sit$cur_defense)
  )
  covs <- c(0.5, 0.8, 0.95)
  out <- long |>
    dplyr::reframe(
      coverage = covs,
      median = stats::median(.data$value),
      lower = unname(stats::quantile(.data$value, 0.5 - covs / 2)),
      upper = unname(stats::quantile(.data$value, 0.5 + covs / 2)),
      .by = c("team", "component")
    )
  bad <- out$lower > out$median | out$median > out$upper
  if (any(bad)) {
    cli::cli_abort(".wc_strength_records: non-monotone quantiles for {sum(bad)} row(s).")
  }
  out
}

#' Publish the World Cup forecast JSON contract.
#'
#' @param sim_out Output of [simulate_world_cup()].
#' @param sim_inputs_team Per-draw team strengths (from `.extract_sim_inputs_pfi`).
#' @param structure Output of [wc_structure()].
#' @param group_fixtures Output of [wc_group_fixtures()].
#' @param fit_date Date of the underlying fit.
#' @param root Data root.
#' @param country_csv Path to the English->Icelandic team-name map.
#' @return Invisibly the output directory.
#' @importFrom rlang .data
#' @export
publish_world_cup <- function(sim_out, sim_inputs_team, structure, group_fixtures,
                              fit_date = Sys.Date(),
                              head_to_head = NULL,
                              root = here::here("data"),
                              country_csv = here::here(
                                "data", "wc", "structure", "country_names_is.csv"
                              ),
                              schedule_csv = here::here(
                                "data", "wc", "structure", "wc2026_schedule.csv"
                              )) {
  out_dir <- file.path(root, "publish", "world_cup", "karla")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  is_name <- .wc_country_namer(country_csv)
  generated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  teams <- unlist(structure$groups, use.names = FALSE)
  n_draws <- sim_out$n_draws

  ratings <- sim_inputs_team[sim_inputs_team$team %in% teams, , drop = FALSE] |>
    dplyr::summarise(
      rating = mean(.data$cur_offense + .data$cur_defense),
      offence = mean(.data$cur_offense), defence = mean(.data$cur_defense),
      .by = "team"
    )
  ratings <- ratings[order(-ratings$rating), , drop = FALSE]
  ratings$rating_rank <- seq_len(nrow(ratings))

  perf <- sim_out$performance
  team_tbl <- sim_out$group_probs |>
    dplyr::left_join(
      perf[, c("team", "n_played", "gf", "ga", "pts", "xg_for", "xg_against", "xpts")],
      by = "team"
    ) |>
    dplyr::left_join(ratings[, c("team", "rating", "rating_rank")], by = "team") |>
    dplyr::mutate(
      team_is = is_name(.data$team),
      gd = .data$gf - .data$ga,
      ou_pts = .data$pts - .data$xpts,
      ou_gf = .data$gf - .data$xg_for
    )

  rnd <- function(x, d = 4) round(x, d)

  # ---- groups.json ----
  groups_payload <- lapply(names(structure$groups), function(g) {
    rows <- team_tbl[team_tbl$group == g, , drop = FALSE]
    rows <- rows[order(-rows$pts, -rows$gd, -rows$gf, -rows$p_advance), , drop = FALSE]
    list(group = g, teams = lapply(seq_len(nrow(rows)), function(i) {
      r <- rows[i, ]
      list(
        team = r$team, team_is = r$team_is,
        played = as.integer(r$n_played), points = as.integer(r$pts),
        gf = as.integer(r$gf), ga = as.integer(r$ga), gd = as.integer(r$gd),
        proj_points = rnd(r$proj_points, 2), proj_gf = rnd(r$proj_gf, 2),
        proj_ga = rnd(r$proj_ga, 2),
        xg_for = rnd(r$xg_for, 2), xg_against = rnd(r$xg_against, 2),
        xpts = rnd(r$xpts, 2), ou_pts = rnd(r$ou_pts, 2), ou_gf = rnd(r$ou_gf, 2),
        rating = rnd(r$rating, 3),
        p_first = rnd(r$p_first), p_second = rnd(r$p_second),
        p_third = rnd(r$p_third), p_fourth = rnd(r$p_fourth),
        p_advance = rnd(r$p_advance)
      )
    }))
  })
  jsonlite::write_json(
    list(
      generated_at = generated_at, fit_date = as.character(fit_date),
      groups = groups_payload
    ),
    file.path(out_dir, "groups.json"),
    auto_unbox = TRUE, pretty = TRUE
  )

  # ---- predictions.json (upcoming match cards) ----
  pr <- sim_out$predictions
  pred_payload <- if (!is.null(pr) && nrow(pr) > 0L) {
    sched <- wc_schedule(schedule_csv)
    pr$pair_key <- .wc_pair_key(pr$home, pr$away)
    j <- match(pr$pair_key, sched$pair_key)
    pr$match_no <- sched$match_no[j]
    pr$kickoff <- sched$kickoff[j]
    miss <- is.na(pr$match_no) & !is.na(pr$group)
    if (any(miss)) {
      cli::cli_warn(c(
        "publish_world_cup: {sum(miss)} group fixture(s) unmatched in schedule:",
        paste(pr$home[miss], "vs", pr$away[miss], collapse = "; ")
      ))
    }
    # NA kickoff (future knockout rows) sort last via order()'s default.
    pr <- pr[order(pr$match_date, pr$kickoff), , drop = FALSE]
    lapply(seq_len(nrow(pr)), function(i) {
      r <- pr[i, ]
      out <- list(
        match_date = as.character(r$match_date),
        home = r$home, home_is = is_name(r$home),
        away = r$away, away_is = is_name(r$away),
        p_home = rnd(r$p_home), p_draw = rnd(r$p_draw), p_away = rnd(r$p_away),
        eg_home = rnd(r$eg_home, 2), eg_away = rnd(r$eg_away, 2)
      )
      # Group cards carry `group`; knockout cards carry `round` + `p_advance`
      # (the tie-resolved progression probability). Mutually exclusive per row;
      # the columns may be absent entirely (pure group- or knockout-stage runs).
      if ("group" %in% names(r) && !is.na(r$group)) out$group <- r$group
      if ("round" %in% names(r) && !is.na(r$round)) out$round <- r$round
      if ("p_advance" %in% names(r) && !is.na(r$p_advance)) {
        out$p_advance <- rnd(r$p_advance)
      }
      if (!is.na(r$match_no)) out$match_no <- as.integer(r$match_no)
      if (!is.na(r$kickoff)) {
        out$kickoff <- format(r$kickoff, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      }
      gdd <- if ("goal_diff_distribution" %in% names(pr)) {
        r$goal_diff_distribution[[1]]
      }
      if (!is.null(gdd) && nrow(gdd) > 0L) {
        if (abs(sum(gdd$p) - 1) > 1e-6) {
          cli::cli_abort(
            "publish_world_cup: goal-diff distribution for {r$home} vs {r$away} sums to {sum(gdd$p)}."
          )
        }
        out$goal_diff_distribution <- lapply(seq_len(nrow(gdd)), function(k) {
          list(diff = as.integer(gdd$diff[k]), p = rnd(gdd$p[k], 4))
        })
      }
      # Marginal goal distributions (home / away / total) — the score-prediction
      # accountability contract. Same {value, p} element shape as goal-diff; the
      # value field is `goals` (home/away) or `total`.
      ser_dist <- function(df, vfield) {
        lapply(seq_len(nrow(df)), function(k) {
          el <- list(as.integer(df[[vfield]][k]), rnd(df$p[k], 4))
          names(el) <- c(vfield, "p")
          el
        })
      }
      for (spec in list(
        c("home_goal_distribution", "goals"),
        c("away_goal_distribution", "goals"),
        c("total_goal_distribution", "total")
      )) {
        nm <- spec[1]
        d <- if (nm %in% names(pr)) r[[nm]][[1]] else NULL
        if (!is.null(d) && nrow(d) > 0L) out[[nm]] <- ser_dist(d, spec[2])
      }
      out
    })
  } else {
    list()
  }
  jsonlite::write_json(
    list(generated_at = generated_at, matches = pred_payload),
    file.path(out_dir, "predictions.json"),
    auto_unbox = TRUE, pretty = TRUE
  )

  # ---- tournament_placements.json ----
  pp <- sim_out$placement_probs
  records <- lapply(seq_len(nrow(pp)), function(i) {
    list(
      team = pp$team[i], team_is = is_name(pp$team[i]),
      round_name = as.character(pp$round_name[i]), probability = rnd(pp$probability[i])
    )
  })
  champ <- pp[pp$round_name == "Champion", c("team", "probability")]
  champ <- champ[order(-champ$probability), , drop = FALSE]
  bronze <- pp[pp$round_name == "Third", c("team", "probability")]
  bz <- stats::setNames(bronze$probability, bronze$team)
  summary_payload <- lapply(seq_len(nrow(champ)), function(i) {
    list(
      team = champ$team[i], team_is = is_name(champ$team[i]),
      p_champion = rnd(champ$probability[i]),
      p_bronze = rnd(unname(bz[champ$team[i]]))
    )
  })
  jsonlite::write_json(
    list(
      generated_at = generated_at, season = 2026L, n_teams = length(teams),
      records = records, summary = summary_payload
    ),
    file.path(out_dir, "tournament_placements.json"),
    auto_unbox = TRUE, pretty = TRUE
  )

  # ---- team_strengths.json ----
  strengths_payload <- lapply(seq_len(nrow(ratings)), function(i) {
    r <- ratings[i, ]
    list(
      team = r$team, team_is = is_name(r$team),
      group = unname(structure$group_of[r$team]),
      rating = rnd(r$rating, 3), offence = rnd(r$offence, 3),
      defence = rnd(r$defence, 3), rating_rank = as.integer(r$rating_rank)
    )
  })
  strength_records <- .wc_strength_records(sim_inputs_team, teams)
  records_payload <- lapply(seq_len(nrow(strength_records)), function(i) {
    r <- strength_records[i, ]
    list(
      team = r$team, team_is = is_name(r$team),
      component = r$component, location = "avg", coverage = r$coverage,
      median = rnd(r$median, 4), lower = rnd(r$lower, 4), upper = rnd(r$upper, 4)
    )
  })
  jsonlite::write_json(
    list(
      generated_at = generated_at, fit_date = as.character(fit_date),
      teams = strengths_payload, records = records_payload
    ),
    file.path(out_dir, "team_strengths.json"),
    auto_unbox = TRUE, pretty = TRUE
  )

  # ---- bracket.json (forward bracket model for the interactive what-if) ----
  # 0-based head-to-head win matrix W[a][b] = P(a beats b) in a neutral knockout
  # match, the bracket structure, and the (sparse) R32 slot occupancy. The page
  # forward-propagates this and re-propagates on each what-if pin.
  bm <- sim_out$bracket_model
  r32_rows <- which(structure$bracket$round == "R32")
  sparse_occ <- function(v) {
    idx <- which(v > 5e-4)
    lapply(idx, function(i) list(i = i - 1L, p = rnd(v[i], 4)))
  }
  bracket_payload <- list(
    generated_at = generated_at, n_draws = n_draws,
    teams = teams, teams_is = unname(vapply(teams, is_name, character(1))),
    matchup = round(bm$W, 4),
    matches = local({
      allm <- rbind(structure$bracket, structure$third_place)
      lapply(seq_len(nrow(allm)), function(i) {
        list(
          match_no = allm$match_no[i], round = allm$round[i],
          feeder_a = allm$feeder_a[i], feeder_b = allm$feeder_b[i]
        )
      })
    }),
    r32 = lapply(seq_along(r32_rows), function(m) {
      list(
        match_no = structure$bracket$match_no[r32_rows[m]],
        a = sparse_occ(bm$occ_a[m, ]), b = sparse_occ(bm$occ_b[m, ])
      )
    }),
    # Decided knockout matches (Phase 2): the page renders these as settled
    # facts — winner row locked, loser greyed, downstream slots collapsed.
    # 0-based winner/loser to match `teams`. Empty until a knockout is played.
    played = lapply(bm$played, function(p) {
      list(
        match_no = p$match_no, winner = p$winner - 1L, loser = p$loser - 1L,
        winner_score = p$winner_score, loser_score = p$loser_score,
        shootout = p$shootout
      )
    })
  )
  jsonlite::write_json(
    bracket_payload, file.path(out_dir, "bracket.json"),
    auto_unbox = TRUE, matrix = "rowmajor"
  )

  # ---- meta.json ----
  fav <- champ[1, ]
  n_played <- sum(group_fixtures$played %in% TRUE)
  jsonlite::write_json(
    list(
      tournament = "HM 2026", sport = "football", country = "world", sex = "male",
      generated_at = generated_at, fit_date = as.character(fit_date),
      n_teams = length(teams), n_groups = 12L, n_draws = n_draws,
      matches_played = as.integer(n_played), matches_group_total = 72L,
      champion_favourite = list(
        team = fav$team, team_is = is_name(fav$team),
        p_champion = rnd(fav$probability)
      )
    ),
    file.path(out_dir, "meta.json"),
    auto_unbox = TRUE, pretty = TRUE
  )

  # ---- results.json (accountability: "did the model call it?") ----
  # Snapshot today's upcoming-match predictions (so they survive being pruned
  # once played), then build the played-fixtures-vs-prediction payload from the
  # accumulated log. Empty `matches` until a played fixture has a pre-match
  # snapshot on record -> the platform's accountability section stays gated off.
  wc_snapshot_predictions(sim_out$predictions, fit_date = fit_date, root = root)
  results_payload <- wc_build_results(group_fixtures, root = root, is_name = is_name)
  jsonlite::write_json(
    c(list(generated_at = generated_at), results_payload),
    file.path(out_dir, "results.json"),
    auto_unbox = TRUE, pretty = TRUE
  )

  # ---- head_to_head.json (team-vs-team joint Monte Carlo) ----
  # Powers the page's "Einvígi" section: P(A farther than B), P(meet),
  # P(A wins | meet) for every pair. Computed by wc_head_to_head() — a separate
  # joint MC pass (the bracket model above is only marginal). Optional: skipped
  # when not supplied so the forecast still publishes if H2H is disabled.
  if (!is.null(head_to_head)) {
    jsonlite::write_json(
      wc_head_to_head_payload(head_to_head, is_name, generated_at, fit_date),
      file.path(out_dir, "head_to_head.json"),
      auto_unbox = TRUE, matrix = "rowmajor", digits = 6
    )
  }

  cli::cli_alert_success("Wrote WC publish JSON to {.path {out_dir}}")
  invisible(out_dir)
}
