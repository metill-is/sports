# Season, format and fixtures for the 2DT sports (spec 2026-09-16 §5-§6).

.res <- function(season, division = "BD", date = "2100-03-01") {
  tibble::tibble(
    season = as.integer(season), division = division,
    match_date = as.Date(date), home_team = "A", away_team = "B",
    home_score = 80L, away_score = 70L
  )
}
.sch <- function(season, division = "BD", date) {
  tibble::tibble(
    season = as.integer(season), division = division,
    match_date = as.Date(date), home_team = "A", away_team = "B"
  )
}
.end <- as.Date("2100-09-16")

test_that("a published next season is current before any of it is played", {
  expect_identical(
    .current_season_2dt(.res(2100), .sch(2101, date = "2100-09-29"), .end, "BD"),
    2101L
  )
})

test_that("without a future fixture the last results season stands", {
  expect_identical(.current_season_2dt(.res(2100), NULL, .end, "BD"), 2100L)
  expect_identical(
    .current_season_2dt(.res(2100), .sch(2101, date = "2100-09-16"), .end, "BD"),
    2100L
  )
})

test_that("another division's schedule does not move this one", {
  expect_identical(
    .current_season_2dt(.res(2100, "1D"), .sch(2101, "BD", "2100-09-29"), .end, "1D"),
    2100L
  )
})

test_that("a held division ignores its schedule and never passes its hold", {
  expect_identical(
    .current_season_2dt(.res(2100, "1D"), .sch(2101, "1D", "2100-09-29"), .end, "1D", hold = 2100L),
    2100L
  )
  started <- dplyr::bind_rows(.res(2100, "1D"), .res(2101, "1D", "2100-09-10"))
  expect_identical(
    .current_season_2dt(started, NULL, .end, "1D", hold = 2100L),
    2100L
  )
})

test_that("results after end_date do not count and an empty cell uses the calendar year", {
  expect_identical(
    .current_season_2dt(.res(2101, date = "2100-10-01"), NULL, .end, "BD"),
    2100L
  )
})

.double_rr <- function(teams) {
  g <- expand.grid(home_team = teams, away_team = teams, stringsAsFactors = FALSE)
  tibble::as_tibble(g[g$home_team != g$away_team, ])
}
.single_rr <- function(teams) {
  p <- utils::combn(teams, 2L)
  tibble::tibble(home_team = p[1, ], away_team = p[2, ])
}
.dated <- function(fx, season, division = "OD",
                   start = as.Date("2100-01-01"), scored = FALSE) {
  fx$season <- as.integer(season)
  fx$division <- division
  fx$match_date <- start + seq_len(nrow(fx))
  if (scored) {
    fx$home_score <- 25L
    fx$away_score <- 24L
    fx$round <- seq_len(nrow(fx))
  }
  fx
}

# ---- .division_format_2dt() -------------------------------------------------

test_that("a format change at the season boundary follows the new schedule (F17)", {
  old <- LETTERS[1:8]
  new <- LETTERS[1:10]
  triple <- .dated(dplyr::bind_rows(.double_rr(old), .single_rr(old)), 2100L, scored = TRUE)
  next_season <- .dated(.double_rr(new), 2101L, start = as.Date("2100-09-05"))
  expect_identical(
    .division_format_2dt(triple, next_season, 2101L, "OD",
      expected_meetings = 3L, regular_season_rounds = NA_integer_
    ),
    list(meetings = 2L, source = "schedule")
  )
})

test_that("a partial fixture list is not a format signal", {
  teams <- LETTERS[1:6]
  played <- .dated(.single_rr(teams), 2100L, scored = TRUE)
  legs <- .single_rr(teams)[1:3, ]
  return_legs <- .dated(
    tibble::tibble(home_team = legs$away_team, away_team = legs$home_team),
    2100L,
    start = as.Date("2100-02-01")
  )
  expect_identical(
    .division_format_2dt(played, return_legs, 2100L, "OD",
      expected_meetings = 2L, regular_season_rounds = NA_integer_
    ),
    list(meetings = 2L, source = "config")
  )
})

test_that("with no usable schedule and no config, the last completed season decides", {
  teams <- LETTERS[1:4]
  prior <- .dated(.double_rr(teams), 2099L, scored = TRUE)
  fragment <- .dated(.single_rr(teams)[1:2, ], 2100L, scored = TRUE)
  expect_identical(
    .division_format_2dt(dplyr::bind_rows(prior, fragment), NULL, 2100L, "OD",
      expected_meetings = NA_integer_, regular_season_rounds = NA_integer_
    ),
    list(meetings = 2L, source = "prior_results")
  )
})

test_that("a stated regular_season_rounds leaves the meetings unknown", {
  expect_identical(
    .division_format_2dt(NULL, NULL, 2100L, "1D",
      expected_meetings = NA_integer_, regular_season_rounds = 18L
    ),
    list(meetings = NA_integer_, source = "not_applicable")
  )
})

test_that("nothing to go on is 'none'", {
  expect_identical(
    .division_format_2dt(NULL, NULL, 2100L, "OD", NA_integer_, NA_integer_),
    list(meetings = NA_integer_, source = "none")
  )
})

# ---- .remaining_fixtures_2dt() ----------------------------------------------

.pairs <- function(df) paste(df$home_team, df$away_team)

test_that("a double round robin is completed with the right venues", {
  played <- tibble::tibble(home_team = c("A", "B", "C"), away_team = c("B", "C", "A"))
  out <- .remaining_fixtures_2dt(c("A", "B", "C"), played, NULL, meetings = 2L)
  expect_setequal(.pairs(out), c("B A", "C B", "A C"))
})

test_that("a pairing missing from the schedule is simulated anyway (F16)", {
  teams <- c("A", "B", "C", "D")
  sched <- .dated(.double_rr(teams), 2101L)
  sched <- sched[!(sched$home_team == "C" & sched$away_team == "D"), ]
  out <- .remaining_fixtures_2dt(teams, NULL, sched, meetings = 2L)
  expect_equal(nrow(out), 12L)
  expect_true("C D" %in% .pairs(out))
  expect_false(any(duplicated(.pairs(out))))
})

test_that("a triple round robin splits each pairing's venues two and one", {
  out <- .remaining_fixtures_2dt(c("A", "B", "C"), NULL, NULL, meetings = 3L)
  expect_equal(nrow(out), 9L)
  unordered <- paste(pmin(out$home_team, out$away_team), pmax(out$home_team, out$away_team))
  expect_true(all(table(unordered) == 3L))
  expect_setequal(as.integer(table(.pairs(out))), c(1L, 2L))
})

test_that("scheduled games past a pairing's meetings are not simulated", {
  # An embedded play-off game between two sides whose regular meetings are
  # done: the forward half of the regular-season cut (F9).
  played <- tibble::tibble(home_team = c("A", "B"), away_team = c("B", "A"))
  sched <- tibble::tibble(home_team = "A", away_team = "B", match_date = as.Date("2100-04-01"))
  expect_equal(nrow(.remaining_fixtures_2dt(c("A", "B"), played, sched, 2L)), 0L)
})

test_that("scheduled venues are kept and a venue already used up is skipped", {
  played <- tibble::tibble(home_team = "A", away_team = "B")
  sched <- tibble::tibble(
    home_team = c("A", "B"), away_team = c("B", "A"),
    match_date = as.Date(c("2100-02-01", "2100-03-01"))
  )
  out <- .remaining_fixtures_2dt(c("A", "B"), played, sched, meetings = 2L)
  expect_identical(.pairs(out), "B A")
})

test_that("unknown meetings fall back to the schedule alone", {
  sched <- tibble::tibble(home_team = "A", away_team = "B", match_date = as.Date("2100-02-01"))
  out <- .remaining_fixtures_2dt(c("A", "B", "C"), NULL, sched, meetings = NA_integer_)
  expect_identical(.pairs(out), "A B")
})

# ---- .base_standings_2dt() --------------------------------------------------

test_that("the base table carries every division team, played or not", {
  played <- tibble::tibble(
    home_team = "A", away_team = "B", home_score = 30L, away_score = 30L
  )
  hb <- .base_standings_2dt(played, c("A", "B", "C"), has_ties = TRUE, tie_threshold = 0.5)
  expect_identical(hb$team, c("A", "B", "C"))
  expect_identical(hb$base_points, c(1L, 1L, 0L))
  expect_identical(hb$base_gd, c(0L, 0L, 0L))
  expect_identical(hb$base_gf, c(30L, 30L, 0L))

  bb <- .base_standings_2dt(NULL, c("B", "A"))
  expect_identical(bb$team, c("A", "B"))
  expect_identical(bb$base_points, c(0L, 0L))
  expect_identical(bb$base_gf, c(0L, 0L))
})
