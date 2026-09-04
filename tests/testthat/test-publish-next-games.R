# One next_games contract behind two predicted_matches shapes. Both branches
# must emit the SAME columns in the SAME order with the SAME classes, or
# next_games.json stops being one contract and the platform's fixture strip
# needs a per-sport reader again.

.NEXT_GAMES_CONTRACT <- c(
  "date", "venue", "division", "division_code", "home", "away",
  "mean_home_goals", "mean_away_goals", "mean_goal_diff",
  "p_home_win", "p_draw", "p_away_win", "goal_diff_distribution"
)

test_that("the scoreline-counts shape aggregates football's count table", {
  # One fixture, five posterior scorelines. Hand-computed:
  #   total 5; mean_home 7/5 = 1.4; mean_away 6/5 = 1.2; mean_diff 1/5 = 0.2
  #   p_home 3/5; p_draw 1/5; p_away 1/5
  predicted <- tibble::tibble(
    home_team = "FOM BD 01", away_team = "FOM BD 02",
    match_date = as.Date("2100-01-20"),
    home_goals = c(2L, 1L, 0L), away_goals = c(1L, 1L, 2L),
    count = c(3L, 1L, 1L)
  )
  pred_d <- tibble::tibble(
    home_team = "FOM BD 01", away_team = "FOM BD 02",
    match_date = as.Date("2100-01-20"), division = "BD"
  )

  out <- .next_games_rows_pfi(
    predicted = predicted,
    profile = sport_publish_profile("football"),
    pred_d = pred_d,
    family_divs = "BD",
    division_badges = .iceland_division_badges("football_iceland", "male"),
    end_date = FIXTURE_END_DATE,
    venues = tibble::tibble(team = "FOM BD 01", venue = "Fixture Park")
  )

  expect_equal(names(out), .NEXT_GAMES_CONTRACT)
  expect_equal(nrow(out), 1L)
  expect_equal(out$date, "2100-01-20")
  expect_equal(out$venue, "Fixture Park")
  expect_equal(out$division, "BD")
  expect_equal(out$division_code, "BD")
  expect_equal(out$home, "FOM BD 01")
  expect_equal(out$away, "FOM BD 02")
  expect_equal(out$mean_home_goals, 1.4, tolerance = 1e-9)
  expect_equal(out$mean_away_goals, 1.2, tolerance = 1e-9)
  expect_equal(out$mean_goal_diff, 0.2, tolerance = 1e-9)
  expect_equal(out$p_home_win, 0.6, tolerance = 1e-9)
  expect_equal(out$p_draw, 0.2, tolerance = 1e-9)
  expect_equal(out$p_away_win, 0.2, tolerance = 1e-9)

  dist <- out$goal_diff_distribution[[1]]
  expect_equal(names(dist), c("diff", "p"))
  expect_equal(dist$diff, c(-2L, 0L, 1L))
  expect_equal(dist$p, c(0.2, 0.2, 0.6), tolerance = 1e-9)
  expect_equal(sum(dist$p), 1, tolerance = 1e-9)
})

test_that("the scoreline-counts shape drops fixtures outside the horizon", {
  predicted <- tibble::tibble(
    home_team = c("FOM BD 01", "FOM BD 03"),
    away_team = c("FOM BD 02", "FOM BD 04"),
    match_date = as.Date(c("2100-01-20", "2100-03-01")),
    home_goals = 1L, away_goals = 0L, count = 4L
  )
  pred_d <- tibble::tibble(
    home_team = c("FOM BD 01", "FOM BD 03"),
    away_team = c("FOM BD 02", "FOM BD 04"),
    match_date = as.Date(c("2100-01-20", "2100-03-01")),
    division = "BD"
  )
  out <- .next_games_rows_pfi(
    predicted = predicted,
    profile = sport_publish_profile("football"),
    pred_d = pred_d,
    family_divs = "BD",
    division_badges = .iceland_division_badges("football_iceland", "male"),
    end_date = FIXTURE_END_DATE
  )
  expect_equal(nrow(out), 1L)
  expect_equal(out$home, "FOM BD 01")
  # No venues supplied -> the field is present and null, not absent.
  expect_true(is.na(out$venue))
  expect_type(out$venue, "character")
})

test_that("the match-summary shape passes the 2DT parquet through unchanged", {
  predicted <- arrow::read_parquet(testthat::test_path(
    "fixtures", "extracts", "sport=basketball", "country=iceland",
    "sex=male", "fit_date=2100-01-01", "predicted_matches.parquet"
  ))

  out <- .next_games_rows_pfi(
    predicted = predicted,
    profile = sport_publish_profile("basketball"),
    pred_d = NULL,
    family_divs = "BD",
    division_badges = .iceland_division_badges("basketball_iceland", "male"),
    end_date = FIXTURE_END_DATE
  )

  expect_equal(names(out), .NEXT_GAMES_CONTRACT)
  expect_equal(nrow(out), 3L)
  expect_true(all(out$division == "BD"))
  # 1D is filtered out by family_divs even though it is in the same parquet.
  expect_false(any(out$division == "1D"))
  # The configured badge, not the raw code -- "1D" would break
  # next_games.schema.json's ^[A-Z][A-Z0-9_]*$ pattern.
  expect_true(all(out$division_code == "BON"))
  expect_true(all(is.na(out$venue)))
  # basketball has_ties is FALSE, so the extractor emits no draw mass.
  expect_true(all(out$p_draw == 0))
  expect_false(is.unsorted(out$date))

  dist <- out$goal_diff_distribution[[1]]
  expect_true(is.data.frame(dist))
  expect_equal(ncol(dist), 2L)
  expect_equal(names(dist), c("diff", "p"))
  expect_equal(sum(dist$p), 1, tolerance = 1e-9)
})

test_that("both shapes degrade to the identical empty tibble", {
  football <- sport_publish_profile("football")
  basketball <- sport_publish_profile("basketball")

  empty_football <- .next_games_rows_pfi(
    predicted = football$empty_extracts$predicted_matches,
    profile = football,
    pred_d = tibble::tibble(
      home_team = character(), away_team = character(),
      match_date = as.Date(character()), division = character()
    ),
    family_divs = "BD",
    division_badges = .iceland_division_badges("football_iceland", "male"),
    end_date = FIXTURE_END_DATE
  )
  empty_2dt <- .next_games_rows_pfi(
    predicted = basketball$empty_extracts$predicted_matches,
    profile = basketball,
    pred_d = NULL,
    family_divs = "BD",
    division_badges = .iceland_division_badges("basketball_iceland", "male"),
    end_date = FIXTURE_END_DATE
  )

  expect_equal(nrow(empty_football), 0L)
  expect_equal(nrow(empty_2dt), 0L)
  expect_equal(names(empty_football), .NEXT_GAMES_CONTRACT)
  expect_equal(names(empty_2dt), .NEXT_GAMES_CONTRACT)
  expect_equal(
    vapply(empty_football, function(x) class(x)[[1]], character(1)),
    vapply(empty_2dt, function(x) class(x)[[1]], character(1))
  )
})

test_that("match_summary accepts the reader's division-stripped per-cell slice", {
  # read_extracted_iceland() filters predicted_matches by `division` and then
  # DROPS the column, so the publisher hands this helper a slice without it.
  # Reading the raw partition parquet (as the block above does) hides that:
  # there the column is present. This is the shape production actually passes.
  raw <- arrow::read_parquet(testthat::test_path(
    "fixtures", "extracts", "sport=basketball", "country=iceland",
    "sex=male", "fit_date=2100-01-01", "predicted_matches.parquet"
  ))
  bd <- raw[raw$division == "BD", , drop = FALSE]
  bd$division <- NULL
  expect_gt(nrow(bd), 0L)

  out <- .next_games_rows_pfi(
    predicted = tibble::as_tibble(bd),
    profile = sport_publish_profile("basketball"),
    pred_d = NULL,
    family_divs = "BD",
    division_badges = .iceland_division_badges("basketball_iceland", "male"),
    end_date = FIXTURE_END_DATE
  )

  expect_equal(names(out), .NEXT_GAMES_CONTRACT)
  expect_gt(nrow(out), 0L)
  expect_setequal(unique(out$division), "BD")
  expect_setequal(
    unique(out$division_code),
    unname(.iceland_division_badges("basketball_iceland", "male")[["BD"]])
  )
})
