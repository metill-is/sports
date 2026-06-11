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
                              root = here::here("data"),
                              country_csv = here::here(
                                "data", "wc", "structure", "country_names_is.csv"
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
    pr <- pr[order(pr$match_date, pr$group), , drop = FALSE]
    lapply(seq_len(nrow(pr)), function(i) {
      r <- pr[i, ]
      list(
        match_date = as.character(r$match_date), group = r$group,
        home = r$home, home_is = is_name(r$home),
        away = r$away, away_is = is_name(r$away),
        p_home = rnd(r$p_home), p_draw = rnd(r$p_draw), p_away = rnd(r$p_away),
        eg_home = rnd(r$eg_home, 2), eg_away = rnd(r$eg_away, 2)
      )
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
  summary_payload <- lapply(seq_len(nrow(champ)), function(i) {
    list(
      team = champ$team[i], team_is = is_name(champ$team[i]),
      p_champion = rnd(champ$probability[i])
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
  jsonlite::write_json(
    list(
      generated_at = generated_at, fit_date = as.character(fit_date),
      teams = strengths_payload
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
    matches = lapply(seq_len(nrow(structure$bracket)), function(i) {
      list(
        match_no = structure$bracket$match_no[i], round = structure$bracket$round[i],
        feeder_a = structure$bracket$feeder_a[i], feeder_b = structure$bracket$feeder_b[i]
      )
    }),
    r32 = lapply(seq_along(r32_rows), function(m) {
      list(
        match_no = structure$bracket$match_no[r32_rows[m]],
        a = sparse_occ(bm$occ_a[m, ]), b = sparse_occ(bm$occ_b[m, ])
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

  cli::cli_alert_success("Wrote WC publish JSON to {.path {out_dir}}")
  invisible(out_dir)
}
