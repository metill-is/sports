# Every football-only publish surface is switched by one predicate with a
# per-sport data answer: `"<surface>" %in% profile$surfaces`. These blocks
# publish FOOTBALL twice -- once with its real profile, once with one surface
# removed -- so each gate is proven two-sided on the live payload rather than
# inferred from a sport that has no such payload to begin with.
#
# The golden manifest (test-publish-football-golden.R) proves the other half:
# football's profile carries every surface, so all gates evaluate TRUE and its
# 92 payloads are byte-identical.

# An earlier partition than the one being published, so the preseason lookup
# (latest fit strictly before the cell's first played kickoff) has something to
# find. The fixture's season-2100 BD male opener is 2100-01-03.
.SURFACES_PRESEASON_FIT_DATE <- as.Date("2099-12-01")

# ...and a synthetic pre-round archive partition dated BETWEEN the published
# fit (2100-01-01) and that opener, so it is the latest candidate
# .find_pre_round_fit_path_pfi() sees for the season's first matchweek. Without
# it round_predictions is empty for every round and the xg surface has no
# observable effect at all -- the gate would be untestable rather than tested.
.SURFACES_ARCHIVE_FIT_DATE <- as.Date("2100-01-02")

.build_preround_archive <- function(facts_root, archive_root, sex) {
  results <- read_table(
    "results",
    root = facts_root,
    filter = list(sport = "football", country = "iceland", sex = sex)
  )
  played <- results[
    results$season == max(results$season) &
      results$division == "BD" &
      !is.na(results$home_score), ,
    drop = FALSE
  ]
  # Four deterministic "draws" per match, centred on the observed scoreline.
  draws <- dplyr::bind_rows(lapply(0:3, function(k) {
    tibble::tibble(
      home_team = played$home_team,
      away_team = played$away_team,
      match_date = played$match_date,
      home_goals = as.integer(pmax(0L, played$home_score + (k %% 2L))),
      away_goals = as.integer(pmax(0L, played$away_score + (k %/% 2L)))
    )
  }))
  part <- file.path(
    archive_root, "sport=football", "country=iceland",
    paste0("sex=", sex),
    paste0("fit_date=", format(.SURFACES_ARCHIVE_FIT_DATE, "%Y-%m-%d"))
  )
  dir.create(part, recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(draws, file.path(part, "part-0.parquet"))
  invisible(NULL)
}

.publish_football_with_profile <- function(profile, sex = "male",
                                           env = parent.frame()) {
  facts_root <- fixture_facts_root(env)
  extracts_root <- file.path(
    withr::local_tempdir(.local_envir = env), "extracts"
  )
  archive_root <- file.path(
    withr::local_tempdir(.local_envir = env), "archive"
  )
  out <- withr::local_tempdir(.local_envir = env)
  rph <- withr::local_tempdir(.local_envir = env)
  league <- load_leagues()[["football_iceland"]]

  build_football_extracts_fixture(
    facts_root, extracts_root, sex,
    fit_date = .SURFACES_PRESEASON_FIT_DATE
  )
  build_football_extracts_fixture(facts_root, extracts_root, sex)
  .build_preround_archive(facts_root, archive_root, sex)

  extracted <- read_extracted_iceland(
    league,
    sex = sex, fit_date = FIXTURE_FIT_DATE, extracts_root = extracts_root
  )
  suppressMessages(suppressWarnings(publish_iceland_league(
    extracted = extracted, league = league, sex = sex, profile = profile,
    end_date = FIXTURE_END_DATE,
    root = facts_root,
    output_root = out,
    extracts_root = extracts_root,
    archive_root = archive_root,
    round_predictions_history_root = rph
  )))
  list(out = out, rph = rph)
}

.without_surface <- function(sport, surface) {
  profile <- sport_publish_profile(sport)
  profile$surfaces <- setdiff(profile$surfaces, surface)
  profile
}

.cell_json <- function(root, cell, file) {
  jsonlite::read_json(
    file.path(root, "football", "iceland", cell, file),
    simplifyVector = FALSE
  )
}

test_that("round_predictions_history is written only when its surface is on", {
  on <- .publish_football_with_profile(sport_publish_profile("football"))
  rel <- file.path(
    "football", "iceland", "karla-bd", "round_predictions_history.json"
  )
  expect_true(file.exists(file.path(on$rph, rel)))
  expect_gt(length(jsonlite::read_json(file.path(on$rph, rel))$records), 0L)

  off <- .publish_football_with_profile(
    .without_surface("football", "round_predictions_history")
  )
  expect_false(file.exists(file.path(off$rph, rel)))
})

test_that("xg columns go null when the xg surface is off", {
  on <- .publish_football_with_profile(sport_publish_profile("football"))
  st_on <- .cell_json(on$out, "karla-bd", "standings.json")
  expect_gt(length(st_on$rows), 0L)
  expect_true(any(vapply(
    st_on$rows, function(r) !is.null(r$xg_for), logical(1)
  )))

  off <- .publish_football_with_profile(.without_surface("football", "xg"))
  st_off <- .cell_json(off$out, "karla-bd", "standings.json")
  expect_equal(length(st_off$rows), length(st_on$rows))
  for (r in st_off$rows) {
    expect_true("xg_for" %in% names(r))
    expect_null(r$xg_for)
    expect_null(r$xg_against)
    expect_null(r$xpts)
  }
})

test_that("preseason is embedded only when its surface is on", {
  has_preseason <- function(root) {
    ts <- .cell_json(root, "karla-bd", "team_strengths.json")
    any(vapply(ts$records, function(r) "preseason" %in% names(r), logical(1)))
  }

  on <- .publish_football_with_profile(sport_publish_profile("football"))
  expect_true(has_preseason(on$out))

  off <- .publish_football_with_profile(
    .without_surface("football", "preseason_strengths")
  )
  expect_false(has_preseason(off$out))
})

test_that("cup payloads are written only when the cup_bracket surface is on", {
  on <- .publish_football_with_profile(sport_publish_profile("football"))
  tp <- file.path(
    "football", "iceland", "karla-bikar", "tournament_placements.json"
  )
  expect_true(file.exists(file.path(on$out, tp)))

  off <- .publish_football_with_profile(
    .without_surface("football", "cup_bracket")
  )
  expect_false(file.exists(file.path(off$out, tp)))
  expect_false(file.exists(file.path(
    off$out, "football", "iceland", "karla-bikar", "bracket.json"
  )))
})

test_that("meta.split is emitted only when the split surface is on", {
  on <- .publish_football_with_profile(sport_publish_profile("football"))
  expect_true("split" %in% names(.cell_json(on$out, "karla-bd", "meta.json")))

  off <- .publish_football_with_profile(.without_surface("football", "split"))
  expect_false("split" %in% names(.cell_json(off$out, "karla-bd", "meta.json")))
})

test_that("the points scheme comes from the profile, not a 3-1-0 literal", {
  expect_equal(
    .publish_points_scheme(sport_publish_profile("football")),
    c(win = 3L, draw = 1L, loss = 0L)
  )
  expect_equal(
    .publish_points_scheme(sport_publish_profile("handball")),
    c(win = 2L, draw = 1L, loss = 0L)
  )
  # basketball's profile$points$draw is NULL (no draw is possible), which the
  # tally must read as a zero weight rather than dropping the term.
  expect_equal(
    .publish_points_scheme(sport_publish_profile("basketball")),
    c(win = 2L, draw = 0L, loss = 0L)
  )
})
