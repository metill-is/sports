# GISKO retrospective backtest: score the model's leak-free pre-match
# predictions against actual results, as if we had been participating from the
# start. Read-only; never on CI. The per-match predictions come from the frozen
# accountability log (data/wc/accountability/prediction_log.json), whose
# fit_date <= match_date for every match -- no look-ahead.

#' Marginals from a `prediction_log.json` match entry
#'
#' Same `marg` shape as [gisko_marginals_from_pbar()], adapted from the
#' accountability log's `dist_home` / `dist_away` / `dist_diff` list-columns
#' (each a list of `{goals|diff, p}`).
#'
#' @param m One element of `prediction_log.json$matches`.
#' @return A `marg` list.
#' @export
gisko_marginals_from_log <- function(m) {
  pull <- function(dist, key) {
    stats::setNames(
      vapply(dist, function(e) as.numeric(e$p), numeric(1)),
      vapply(dist, function(e) as.numeric(e[[key]]), numeric(1))
    )
  }
  list(
    p_outcome = c(
      home = as.numeric(m$p_home), draw = as.numeric(m$p_draw),
      away = as.numeric(m$p_away)
    ),
    home_pmf = pull(m$dist_home, "goals"),
    away_pmf = pull(m$dist_away, "goals"),
    gd_pmf = pull(m$dist_diff, "diff")
  )
}

# All permutations of 1..n as rows (n! x n). Used for the size-4 group
# assignment; not meant for large n.
.gisko_perms <- function(n) {
  if (n == 1L) {
    return(matrix(1L, 1L, 1L))
  }
  sub <- .gisko_perms(n - 1L)
  do.call(rbind, lapply(seq_len(n), function(i) {
    cbind(i, matrix(ifelse(sub >= i, sub + 1L, sub), nrow(sub)))
  }))
}

#' Optimal group ranking: maximise expected correctly-placed teams
#'
#' GISKO awards 1 point per team placed in its correct final group position.
#' The optimal entry assigns teams to positions to maximise the expected number
#' of correct placements -- a linear assignment on the placement-probability
#' matrix (brute-forced; groups are size 4). The top two of the returned order
#' double as the qualification (reach-R32) prediction.
#'
#' @param p_matrix Numeric matrix, rows = teams (rownamed), cols = positions
#'   `1..n`, with `p_matrix[i, j]` = P(team i finishes j-th).
#' @return Character vector of team names ordered 1st..last.
#' @export
gisko_optimal_group_order <- function(p_matrix) {
  teams <- rownames(p_matrix)
  n <- nrow(p_matrix)
  perms <- .gisko_perms(n)
  scores <- apply(perms, 1L, function(pi) sum(p_matrix[cbind(pi, seq_len(n))]))
  teams[perms[which.max(scores), ]]
}

#' Score played matches under GISKO with a per-round joker
#'
#' For each played match: take the leak-free pre-match marginals, pick the
#' expected-points-optimal scoreline (Lemma 1), and score it against the actual
#' result. Per round, the joker doubles the match with the highest *pre-match*
#' expected points (Lemma 2 -- a leak-free choice); its bonus is that match's
#' realised points.
#'
#' @param played Tibble with `round`, `label`, `marg` (list-column of marginals),
#'   `act_home`, `act_away`.
#' @param max_goals Candidate-grid upper bound.
#' @return list `picks` (per-match tibble), `by_round` (per-round base + joker),
#'   `base_total`, `joker_total`, `grand_total`.
#' @export
gisko_backtest_score <- function(played, max_goals = 8L) {
  rows <- lapply(seq_len(nrow(played)), function(i) {
    opt <- gisko_optimal_scoreline_marginal(played$marg[[i]], max_goals)
    pts <- gisko_match_points(
      opt$home, opt$away, played$act_home[i], played$act_away[i]
    )
    tibble::tibble(
      round = played$round[i], label = played$label[i],
      pick = paste0(opt$home, "-", opt$away),
      actual = paste0(played$act_home[i], "-", played$act_away[i]),
      exp_points = opt$exp_points, points = pts
    )
  })
  picks <- do.call(rbind, rows)
  by_round <- do.call(rbind, lapply(
    split(seq_len(nrow(picks)), picks$round),
    function(idx) {
      sub <- picks[idx, , drop = FALSE]
      j <- which.max(sub$exp_points)
      tibble::tibble(
        round = sub$round[1], n = nrow(sub), base = sum(sub$points),
        joker_match = sub$label[j], joker_bonus = sub$points[j]
      )
    }
  ))
  list(
    picks = picks, by_round = by_round,
    base_total = sum(picks$points),
    joker_total = sum(by_round$joker_bonus),
    grand_total = sum(picks$points) + sum(by_round$joker_bonus)
  )
}

#' Restrict WC results to the group stage (schedule Round Number 1-3)
#'
#' Once the knockout begins, played knockout matches enter `res` and inherit a
#' `grp` label from their home team, inflating those groups past six round-robin
#' fixtures. Structural (pool placement + R32 qualification) scoring must ignore
#' them. Knockout pairings are unmapped by `rmap`, so their `res_round` is `NA`.
#' @noRd
.gisko_group_stage <- function(res, res_round) {
  res[res_round %in% c("1", "2", "3"), , drop = FALSE]
}

#' Assemble the full backtest dataset (matches + structural) from disk
#'
#' Shared loader for the text scorecard and the HTML report. Reads the frozen,
#' leak-free accountability log + results and the pre-deadline group forecast,
#' and returns everything both views need.
#'
#' @param pre_deadline_rev Git rev of the last `groups.json` before the 11 Jun
#'   structural deadline.
#' @return list: `picks` (per-match), `by_round`, `structural` (or NULL),
#'   `base_total`, `match_total` (base + realised joker), `pool_pts`, `qual_pts`,
#'   `model_total`, `n_played`, `ceiling`, `pending_joker`, `complete_pools`.
#' @export
gisko_scorecard_data <- function(pre_deadline_rev = "d4425564") {
  group_round_total <- 24L
  res <- read_table("results", filter = list(
    sport = "football", country = "world", sex = "male"
  ))
  res <- res[res$division == "FIFA World Cup" & res$season == 2026L, , drop = FALSE]
  res_key <- paste(res$home_team, res$away_team, as.character(res$match_date))
  s <- wc_structure()
  res$grp <- s$group_of[res$home_team]

  raw <- utils::read.csv(
    here::here("data", "wc", "structure", "wc2026_schedule.csv"),
    check.names = FALSE, colClasses = "character", encoding = "UTF-8"
  )
  rmap <- stats::setNames(
    raw[["Round Number"]],
    .wc_pair_key(.wc_alias(trimws(raw[["Home Team"]])), .wc_alias(trimws(raw[["Away Team"]])))
  )

  # WHY: pool placement + R32 qualification are group-stage concepts. Once the
  # knockout begins, played knockout matches enter `res` and get a `grp` label
  # from their home team, inflating those groups past 6 and silently dropping
  # them from `complete_pools`. Restrict structural scoring to the 72 round-robin
  # fixtures (schedule Round Number 1-3); knockout pairings are unmapped (NA).
  res_round <- unname(rmap[.wc_pair_key(.wc_alias(res$home_team), .wc_alias(res$away_team))])
  res_grp_stage <- .gisko_group_stage(res, res_round)

  pl <- jsonlite::read_json(
    here::here("data", "wc", "accountability", "prediction_log.json")
  )
  entries <- lapply(pl$matches, function(m) {
    list(
      home = m$home, away = m$away, date = m$match_date,
      round = unname(rmap[.wc_pair_key(m$home, m$away)]),
      marg = gisko_marginals_from_log(m)
    )
  })
  all_exp <- vapply(
    entries, function(e) gisko_optimal_scoreline_marginal(e$marg)$exp_points, numeric(1)
  )
  keys <- vapply(entries, function(e) paste(e$home, e$away, e$date), character(1))
  played_idx <- which(keys %in% res_key)
  played <- do.call(rbind, lapply(played_idx, function(i) {
    e <- entries[[i]]
    r <- res[match(keys[i], res_key), ]
    tibble::tibble(
      round = ifelse(is.na(e$round), "G?", paste0("G", e$round)),
      label = paste(e$home, "v", e$away), marg = list(e$marg),
      act_home = as.integer(r$home_score), act_away = as.integer(r$away_score)
    )
  }))
  bt <- gisko_backtest_score(played)
  by_round <- bt$by_round
  by_round$complete <- by_round$n == group_round_total
  match_total <- bt$base_total + sum(by_round$joker_bonus[by_round$complete])
  g3 <- which(vapply(entries, function(e) identical(e$round, "3"), logical(1)))
  pend_i <- if (length(g3) > 0L) g3[which.max(all_exp[g3])] else integer(0)
  pending_joker <- if (length(pend_i) == 1L && !(pend_i %in% played_idx)) {
    entries[[pend_i]]
  } else {
    NULL
  }

  complete_pools <- names(which(table(res_grp_stage$grp) == 6L))
  gj <- jsonlite::parse_json(paste(system2(
    "git",
    c(
      "-C", here::here(), "show",
      paste0(pre_deadline_rev, ":data/publish/world_cup/karla/groups.json")
    ),
    stdout = TRUE
  ), collapse = "\n"))
  forecast <- list()
  for (grp in gj$groups) {
    mm <- t(vapply(
      grp$teams,
      function(tm) c(tm$p_first, tm$p_second, tm$p_third, tm$p_fourth), numeric(4)
    ))
    rownames(mm) <- vapply(grp$teams, function(tm) tm$team, character(1))
    forecast[[grp$group]] <- mm
  }
  structural <- if (length(complete_pools) == 0L) {
    NULL
  } else {
    do.call(rbind, lapply(complete_pools, function(g) {
      gm <- res_grp_stage[res_grp_stage$grp == g, ]
      actual <- .wc_group_table(s$groups[[g]], data.frame(
        home_team = gm$home_team, away_team = gm$away_team,
        home_score = gm$home_score, away_score = gm$away_score
      ))$team
      model <- gisko_optimal_group_order(forecast[[g]][s$groups[[g]], , drop = FALSE])
      tibble::tibble(
        pool = g, placement = sum(model == actual),
        qualification = 2L * length(intersect(model[1:2], actual[1:2])),
        model_order = paste(model, collapse = " > "),
        actual_order = paste(actual, collapse = " > ")
      )
    }))
  }
  pool_pts <- if (is.null(structural)) 0L else sum(structural$placement)
  qual_pts <- if (is.null(structural)) 0L else sum(structural$qualification)
  list(
    picks = bt$picks, by_round = by_round, structural = structural,
    base_total = bt$base_total, match_total = match_total,
    pool_pts = pool_pts, qual_pts = qual_pts,
    model_total = match_total + pool_pts + qual_pts,
    n_played = nrow(played), ceiling = nrow(played) * 5L,
    pending_joker = pending_joker, complete_pools = complete_pools
  )
}
