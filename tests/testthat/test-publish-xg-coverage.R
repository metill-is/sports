# Coverage alignment between the actual and expected halves of the standings.
#
# The Delta columns render goals_for - xg_for and points - xpts. Those two halves
# were never over the same matches: goals_for/points span EVERY played round,
# while xg_for/xpts span only the rounds that resolved a pre-round fit. On the
# live Besta deild that is 12 of 23, so Delta ~= goals_for/2 -- every team read as
# massively overperforming, and teams near the mean had their SIGN flipped
# (Breidablik showed +21.8 while its per-game rate was -0.17).
#
# The aggregation already iterates exactly the rounds it can model, and the
# played frame it is handed already carries the scores. So it sums the ACTUAL
# goals and points for those same rounds, and the standings row carries them as
# goals_for_predicted / goals_against_predicted / points_predicted -- the
# like-for-like base the Delta columns subtract from.

.football_scheme <- c(win = 3L, draw = 1L, loss = 0L)

.write_scoreline_fit <- function(root, fit_date, matches) {
  pdir <- file.path(
    root, "sport=football", "country=iceland", "sex=male",
    paste0("fit_date=", fit_date)
  )
  dir.create(pdir, recursive = TRUE)
  set.seed(11L)
  draws <- 200L
  beliefs <- do.call(rbind, lapply(seq_len(nrow(matches)), function(i) {
    tibble::tibble(
      match_date = matches$match_date[i],
      home_team = matches$home_team[i],
      away_team = matches$away_team[i],
      draw_id = seq_len(draws),
      home_goals = stats::rpois(draws, lambda = 1.6),
      away_goals = stats::rpois(draws, lambda = 1.0)
    )
  }))
  arrow::write_parquet(beliefs, file.path(pdir, "part-0.parquet"))
  invisible(pdir)
}

test_that(".aggregate_round_predictions_pfi: reports the actual goals of each modelled round", {
  tmp <- withr::local_tempdir()
  m <- tibble::tibble(
    home_team = "A", away_team = "B", match_date = as.Date("2026-04-15")
  )
  .write_scoreline_fit(tmp, "2026-04-10", m)

  played <- tibble::tibble(
    home_team = "A", away_team = "B",
    match_date = as.Date("2026-04-15"),
    home_score = 3L, away_score = 1L
  )

  out <- sports:::.aggregate_round_predictions_pfi(
    played_matches = played,
    extracts_root = withr::local_tempdir(), archive_root = tmp,
    sport = "football", country = "iceland", sex = "male",
    points = .football_scheme
  )

  home <- out[out$team == "A", ]
  expect_equal(home$goals_for_actual, 3)
  expect_equal(home$goals_against_actual, 1)
  expect_equal(home$pts_actual, 3)

  away <- out[out$team == "B", ]
  expect_equal(away$goals_for_actual, 1)
  expect_equal(away$goals_against_actual, 3)
  expect_equal(away$pts_actual, 0)
})

test_that(".aggregate_round_predictions_pfi: actuals cover only the modelled rounds", {
  tmp <- withr::local_tempdir()
  # The fit predicts round 1 only. Round 2 is played but unmodelled, so its
  # goals must NOT reach the totals the Delta columns subtract from.
  .write_scoreline_fit(tmp, "2026-04-10", tibble::tibble(
    home_team = "A", away_team = "B", match_date = as.Date("2026-04-15")
  ))

  played <- tibble::tibble(
    home_team = c("A", "A"), away_team = c("B", "B"),
    match_date = as.Date(c("2026-04-15", "2026-04-22")),
    home_score = c(3L, 7L), away_score = c(1L, 0L)
  )

  out <- sports:::.aggregate_round_predictions_pfi(
    played_matches = played,
    extracts_root = withr::local_tempdir(), archive_root = tmp,
    sport = "football", country = "iceland", sex = "male",
    points = .football_scheme
  )

  # A scored 3 + 7 = 10 across both rounds, but only round 1 was modelled.
  expect_equal(sum(out$goals_for_actual[out$team == "A"]), 3)
  expect_equal(sum(out$pts_actual[out$team == "A"]), 3)
  expect_equal(nrow(out[out$team == "A", ]), 1L)
})

test_that(".aggregate_round_predictions_pfi: actual points honour the points scheme", {
  tmp <- withr::local_tempdir()
  .write_scoreline_fit(tmp, "2026-04-10", tibble::tibble(
    home_team = "A", away_team = "B", match_date = as.Date("2026-04-15")
  ))
  played <- tibble::tibble(
    home_team = "A", away_team = "B",
    match_date = as.Date("2026-04-15"),
    home_score = 3L, away_score = 1L
  )

  out2 <- sports:::.aggregate_round_predictions_pfi(
    played_matches = played,
    extracts_root = withr::local_tempdir(), archive_root = tmp,
    sport = "football", country = "iceland", sex = "male",
    points = c(win = 2L, draw = 1L, loss = 0L)
  )
  expect_equal(out2$pts_actual[out2$team == "A"], 2)
})

test_that(".aggregate_round_predictions_pfi: a drawn modelled round pays the draw weight", {
  tmp <- withr::local_tempdir()
  .write_scoreline_fit(tmp, "2026-04-10", tibble::tibble(
    home_team = "A", away_team = "B", match_date = as.Date("2026-04-15")
  ))
  played <- tibble::tibble(
    home_team = "A", away_team = "B",
    match_date = as.Date("2026-04-15"),
    home_score = 2L, away_score = 2L
  )

  out <- sports:::.aggregate_round_predictions_pfi(
    played_matches = played,
    extracts_root = withr::local_tempdir(), archive_root = tmp,
    sport = "football", country = "iceland", sex = "male",
    points = .football_scheme
  )
  expect_equal(out$pts_actual[out$team == "A"], 1)
  expect_equal(out$pts_actual[out$team == "B"], 1)
})
