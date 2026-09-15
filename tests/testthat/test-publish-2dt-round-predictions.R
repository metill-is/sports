# Round-prediction aggregation for the 2DT sports (handball / basketball).
#
# Football's beliefs are `scoreline_counts`: one row per integer goal pair with
# a count, which .aggregate_round_predictions_pfi() collapses into per-match
# expected goals and win/draw/loss probabilities. The 2DT extractors publish
# `match_summary` instead -- .summarise_predicted_matches_2dt() has already
# reduced the posterior to exactly those five quantities -- so the aggregation
# reads them rather than re-deriving them from scorelines this shape does not
# carry.
#
# CRITICALLY, the shape is a property of the FILE, not of the sport.
# .find_pre_round_fit_path_pfi() picks the newest fit across two trees:
# extracts/predicted_matches.parquet (match_summary for 2DT) and
# archive/part-0.parquet (always long-form scorelines). prune_extracts() deletes
# old extracts partitions and nothing prunes the archive, so a handball round
# resolves to a long-form archive file as soon as retention bites -- roughly
# 2-3 weeks into a season. Keying the branch on the sport would abort the whole
# handball cell there.

.write_match_summary_fit <- function(root, sport, sex, fit_date, rows) {
  pdir <- file.path(
    root, paste0("sport=", sport), "country=iceland", paste0("sex=", sex),
    paste0("fit_date=", fit_date)
  )
  dir.create(pdir, recursive = TRUE)
  arrow::write_parquet(rows, file.path(pdir, "predicted_matches.parquet"))
  invisible(pdir)
}

.write_scoreline_archive <- function(root, sport, sex, fit_date, matches) {
  pdir <- file.path(
    root, paste0("sport=", sport), "country=iceland", paste0("sex=", sex),
    paste0("fit_date=", fit_date)
  )
  dir.create(pdir, recursive = TRUE)
  set.seed(5L)
  draws <- 200L
  beliefs <- do.call(rbind, lapply(seq_len(nrow(matches)), function(i) {
    tibble::tibble(
      match_date = matches$match_date[i],
      home_team = matches$home_team[i],
      away_team = matches$away_team[i],
      draw_id = seq_len(draws),
      home_goals = stats::rpois(draws, 32),
      away_goals = stats::rpois(draws, 29)
    )
  }))
  arrow::write_parquet(beliefs, file.path(pdir, "part-0.parquet"))
  invisible(pdir)
}

.hb_summary_rows <- function() {
  tibble::tibble(
    game_nr = 1L, match_date = as.Date("2026-09-10"),
    home_team = "Valur", away_team = "Haukar", division = "OD",
    mean_home_goals = 34.0, mean_away_goals = 30.0, mean_goal_diff = 4.0,
    p_home_win = 0.70, p_draw = 0.05, p_away_win = 0.25
  )
}

.hb_played <- function() {
  tibble::tibble(
    home_team = "Valur", away_team = "Haukar",
    match_date = as.Date("2026-09-10"),
    home_score = 31L, away_score = 28L
  )
}

.hb_points <- c(win = 2L, draw = 1L, loss = 0L)

test_that("match_summary beliefs use their pre-aggregated expected goals", {
  extracts <- withr::local_tempdir()
  .write_match_summary_fit(extracts, "handball", "male", "2026-09-07", .hb_summary_rows())

  out <- sports:::.aggregate_round_predictions_pfi(
    played_matches = .hb_played(),
    extracts_root = extracts, archive_root = withr::local_tempdir(),
    sport = "handball", country = "iceland", sex = "male",
    target_div = "OD", points = .hb_points
  )

  expect_setequal(out$team, c("Valur", "Haukar"))
  home <- out[out$team == "Valur", ]
  expect_equal(home$xg_for, 34.0, tolerance = 1e-9)
  expect_equal(home$xg_against, 30.0, tolerance = 1e-9)
  expect_equal(home$p_win, 0.70, tolerance = 1e-9)

  away <- out[out$team == "Haukar", ]
  expect_equal(away$xg_for, 30.0, tolerance = 1e-9)
  expect_equal(away$p_win, 0.25, tolerance = 1e-9)
})

test_that("xpts weights a 2-point win, not football's 3", {
  extracts <- withr::local_tempdir()
  .write_match_summary_fit(extracts, "handball", "male", "2026-09-07", .hb_summary_rows())

  out <- sports:::.aggregate_round_predictions_pfi(
    played_matches = .hb_played(),
    extracts_root = extracts, archive_root = withr::local_tempdir(),
    sport = "handball", country = "iceland", sex = "male",
    target_div = "OD", points = .hb_points
  )

  # Exact arithmetic, not a bound: 2*0.70 + 1*0.05 = 1.45. Football's weighting
  # would give 2.15, so this pins the scheme rather than merely bracketing it.
  home <- out[out$team == "Valur", ]
  expect_equal(home$xpts, 1.45, tolerance = 1e-9)
  away <- out[out$team == "Haukar", ]
  expect_equal(away$xpts, 0.55, tolerance = 1e-9)
})

test_that("the actual result of a modelled handball round is reported", {
  extracts <- withr::local_tempdir()
  .write_match_summary_fit(extracts, "handball", "male", "2026-09-07", .hb_summary_rows())

  out <- sports:::.aggregate_round_predictions_pfi(
    played_matches = .hb_played(),
    extracts_root = extracts, archive_root = withr::local_tempdir(),
    sport = "handball", country = "iceland", sex = "male",
    target_div = "OD", points = .hb_points
  )
  home <- out[out$team == "Valur", ]
  expect_equal(home$goals_for_actual, 31)
  expect_equal(home$pts_actual, 2)
})

test_that("a 2DT round falling back to a long-form archive fit still aggregates", {
  # The state extracts retention produces: no extracts partition survives, and
  # the newest fit available is a long-form archive file. Keying the shape on
  # the sport aborts here with "Column `mean_home_goals` not found".
  archive <- withr::local_tempdir()
  .write_scoreline_archive(
    archive, "handball", "male", "2026-09-07",
    tibble::tibble(
      home_team = "Valur", away_team = "Haukar",
      match_date = as.Date("2026-09-10")
    )
  )

  out <- sports:::.aggregate_round_predictions_pfi(
    played_matches = .hb_played(),
    extracts_root = withr::local_tempdir(), archive_root = archive,
    sport = "handball", country = "iceland", sex = "male",
    target_div = "OD", points = .hb_points
  )

  expect_setequal(out$team, c("Valur", "Haukar"))
  home <- out[out$team == "Valur", ]
  expect_gt(home$xg_for, 0)
  # Still the handball points scheme, whichever tree the file came from.
  expect_lte(home$xpts, 2.0)
  expect_equal(home$pts_actual, 2)
})

# ---- Profile surface ------------------------------------------------------

test_that("xg is a handball surface but not a basketball one", {
  expect_true("xg" %in% sport_publish_profile("handball")$surfaces)
  # Expected *points* on a ~90-point game is a different claim; it stays off
  # until it has been designed.
  expect_false("xg" %in% sport_publish_profile("basketball")$surfaces)
})

test_that("handball's cell file set is unchanged by taking the xg surface", {
  # xg names no cell JSON, so the published file set must not move -- this is
  # what the acceptance and schema suites derive their expectations from.
  expect_equal(length(sports:::.publish_cell_surfaces("handball")), 10L)
  expect_false("xg" %in% sports:::.publish_cell_surfaces("handball"))
})
