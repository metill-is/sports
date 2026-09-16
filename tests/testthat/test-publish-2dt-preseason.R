# A 2DT cell between seasons publishes the NEW season: round 0, its whole
# team list, a full projected table, and an empty standings table that does
# not erase last season's history (spec 2026-09-16 §5, §9.1, §10, §11).

# Extract and publish one 2DT league-sex from the committed fixture. The
# extract is always taken at FIXTURE_END_DATE; `publish_end_date` moves only
# the publish, so a second call with `reuse_extract = TRUE` republishes the
# same fit on a later day. (Not `extract = FALSE`: R would partially match
# that name to `.hb_publish()`'s `extracts_root`.)
.publish_2dt <- function(sport, sex, root, out, extracts_root,
                         publish_end_date = FIXTURE_END_DATE,
                         reuse_extract = FALSE) {
  league <- load_leagues()[[paste0(sport, "_iceland")]]
  if (!reuse_extract) {
    st <- suppressMessages(local_stub_2dt(league, sex, root = root, n_draws = 200L))
    extractor <- switch(sport,
      basketball = extract_basketball_iceland,
      handball = extract_handball_iceland
    )
    suppressMessages(extractor(
      fit = st$fit, league = league, sex = sex,
      fit_date = FIXTURE_FIT_DATE, end_date = FIXTURE_END_DATE,
      root = root, extracts_root = extracts_root, prep = st$prep
    ))
  }
  extracted <- read_extracted_iceland(
    league,
    sex = sex, fit_date = FIXTURE_FIT_DATE,
    extracts_root = extracts_root
  )
  suppressMessages(suppressWarnings(publish_iceland_league(
    extracted = extracted, league = league, sex = sex,
    end_date = publish_end_date,
    root = root, output_root = out, extracts_root = extracts_root,
    archive_root = file.path(root, "beliefs", "archive"),
    round_predictions_history_root = file.path(root, "beliefs", "round_predictions_history")
  )))
  file.path(out, sport, "iceland")
}

.hb_publish <- function(root, out, extracts_root, ...) {
  file.path(
    .publish_2dt("handball", "male", root, out, extracts_root, ...),
    "karla-od"
  )
}

# Publish handball men's OD 2101 as a double round robin, one game a week from
# FIXTURE_END_DATE + `offset` days; none of it is played. Every other schedule
# row (G66's included) is kept.
.schedule_od_2101 <- function(root, offset) {
  teams <- fixture_division_teams("handball", "male", "OD")
  g <- expand.grid(home_team = teams, away_team = teams, stringsAsFactors = FALSE)
  g <- g[g$home_team != g$away_team, ]
  sched <- read_table("schedules", root = root)
  keep <- !(sched$sport == "handball" & sched$sex == "male" & sched$division == "OD")
  write_table(dplyr::bind_rows(sched[keep, ], tibble::tibble(
    sport = "handball", country = "iceland", sex = "male", season = 2101L,
    match_date = FIXTURE_END_DATE + offset + 7L * (seq_len(nrow(g)) - 1L),
    home_team = g$home_team, away_team = g$away_team, division = "OD",
    round = seq_len(nrow(g)), kickoff_time = "19:30"
  )), "schedules", root = root)
  teams
}

.record_field <- function(records, field) {
  vapply(records, function(r) as.character(r[[field]]), "")
}

test_that("a cell between seasons publishes the new season at round 0 and keeps its history", {
  root <- fixture_facts_root()
  out <- file.path(withr::local_tempdir(), "publish")
  extracts_root <- file.path(withr::local_tempdir(), "extracts")

  # 1. In season: the fixture's 2100 table publishes and seeds the history.
  cell <- .hb_publish(root, out, extracts_root)
  before <- jsonlite::read_json(file.path(cell, "standings_history.json"))
  expect_gt(length(before$records), 0L)
  expect_identical(jsonlite::read_json(file.path(cell, "meta.json"))$season, 2100L)

  # 2. The 2101 double round robin is published from the next day; none of it
  # is played.
  teams <- .schedule_od_2101(root, offset = 1L)
  cell <- .hb_publish(root, out, extracts_root)

  meta <- jsonlite::read_json(file.path(cell, "meta.json"))
  expect_identical(meta$season, 2101L)
  expect_identical(meta$round, 0L)
  expect_identical(meta$n_rounds, 6L)
  expect_identical(meta$n_rounds_meetings_source, "schedule")
  keys <- names(meta)
  expect_identical(keys[match("n_rounds_source", keys) + 1L], "n_rounds_meetings_source")

  fp <- jsonlite::read_json(file.path(cell, "final_positions.json"))
  expect_identical(fp$season, 2101L)
  expect_identical(fp$n_teams, 4L)
  expect_setequal(vapply(fp$records, function(r) r$team, ""), teams)

  st <- jsonlite::read_json(file.path(cell, "standings.json"))
  expect_identical(st$season, 2101L)
  expect_length(st$rows, 0L)
  after <- jsonlite::read_json(file.path(cell, "standings_history.json"))
  expect_length(after$records, length(before$records))

  # The fixture's far-future seasons (2099-2101) sit inside meta.season's
  # typo-guard range, so the next season validates like any other.
  v <- validate_publish_dir(
    file.path(out, "handball"),
    schema_dir = here::here("config", "publish-schemas"),
    sport = "handball"
  )
  expect_true(v$ok, info = paste(v$errors, collapse = "\n"))
  expect_gt(v$n_passed, 0L)
})

test_that("a new season beyond Stan's window still covers every team, and a republish adds no heatmap step", {
  root <- fixture_facts_root()
  out <- file.path(withr::local_tempdir(), "publish")
  extracts_root <- file.path(withr::local_tempdir(), "extracts")

  # Four weeks out: no OD fixture reaches pred_d's 14-day window, so the
  # predicted fixtures name no OD team at all. G66 keeps pred_d non-empty.
  teams <- .schedule_od_2101(root, offset = 28L)
  cell <- .hb_publish(root, out, extracts_root)

  meta <- jsonlite::read_json(file.path(cell, "meta.json"))
  expect_identical(meta$season, 2101L)
  expect_identical(meta$round, 0L)

  ha <- jsonlite::read_json(file.path(cell, "home_advantage.json"))
  expect_setequal(unique(.record_field(ha$records, "team")), teams)

  # The same fit, republished a day later, is the same pre-season snapshot:
  # its history rows carry the fit date, so they replace rather than append.
  .hb_publish(
    root, out, extracts_root,
    publish_end_date = FIXTURE_END_DATE + 1L, reuse_extract = TRUE
  )
  fph <- jsonlite::read_json(file.path(cell, "final_positions_history.json"))
  round0 <- Filter(function(r) r$round == 0L, fph$records)
  expect_gt(length(round0), 0L)
  expect_identical(
    unique(.record_field(round0, "as_of")),
    format(FIXTURE_FIT_DATE, "%Y-%m-%d")
  )
  expect_setequal(unique(.record_field(round0, "team")), teams)
})

test_that("basketball's publisher reports each division's meetings source", {
  root <- fixture_facts_root()
  out <- file.path(withr::local_tempdir(), "publish")
  extracts_root <- file.path(withr::local_tempdir(), "extracts")

  league_dir <- .publish_2dt("basketball", "female", root, out, extracts_root)

  # Women's 1. deild states regular_season_rounds (18) instead of a meetings
  # count.
  d1 <- jsonlite::read_json(file.path(league_dir, "kvenna-1d", "meta.json"))
  expect_identical(d1$n_rounds_meetings_source, "not_applicable")
  expect_identical(d1$n_rounds, 18L)

  bd <- jsonlite::read_json(file.path(league_dir, "kvenna-bd", "meta.json"))
  expect_true(
    bd$n_rounds_meetings_source %in% c("schedule", "config"),
    info = bd$n_rounds_meetings_source
  )
})
