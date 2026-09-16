# A 2DT cell between seasons publishes the NEW season: round 0, its whole
# team list, a full projected table, and an empty standings table that does
# not erase last season's history (spec 2026-09-16 §5, §9.1, §10, §11).

.hb_publish <- function(root, out, extracts_root) {
  league <- load_leagues()[["handball_iceland"]]
  st <- suppressMessages(local_stub_2dt(league, "male", root = root, n_draws = 200L))
  suppressMessages(extract_handball_iceland(
    fit = st$fit, league = league, sex = "male",
    fit_date = FIXTURE_FIT_DATE, end_date = FIXTURE_END_DATE,
    root = root, extracts_root = extracts_root, prep = st$prep
  ))
  extracted <- read_extracted_iceland(
    league, sex = "male", fit_date = FIXTURE_FIT_DATE,
    extracts_root = extracts_root
  )
  suppressMessages(suppressWarnings(publish_iceland_league(
    extracted = extracted, league = league, sex = "male",
    end_date = FIXTURE_END_DATE,
    root = root, output_root = out, extracts_root = extracts_root,
    archive_root = file.path(root, "beliefs", "archive"),
    round_predictions_history_root = file.path(root, "beliefs", "round_predictions_history")
  )))
  file.path(out, "handball", "iceland", "karla-od")
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

  # 2. The 2101 double round robin is published; none of it is played.
  teams <- fixture_division_teams("handball", "male", "OD")
  g <- expand.grid(home_team = teams, away_team = teams, stringsAsFactors = FALSE)
  g <- g[g$home_team != g$away_team, ]
  sched <- read_table("schedules", root = root)
  keep <- !(sched$sport == "handball" & sched$sex == "male" & sched$division == "OD")
  write_table(dplyr::bind_rows(sched[keep, ], tibble::tibble(
    sport = "handball", country = "iceland", sex = "male", season = 2101L,
    match_date = FIXTURE_END_DATE + 7L * seq_len(nrow(g)) - 6L,
    home_team = g$home_team, away_team = g$away_team, division = "OD",
    round = seq_len(nrow(g)), kickoff_time = "19:30"
  )), "schedules", root = root)
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

  v <- validate_publish_dir(
    file.path(out, "handball"),
    schema_dir = here::here("config", "publish-schemas"),
    sport = "handball"
  )
  expect_true(v$ok, info = paste(v$errors, collapse = "\n"))
  expect_gt(v$n_passed, 0L)
})
