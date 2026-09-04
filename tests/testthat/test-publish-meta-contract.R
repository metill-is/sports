# THE meta.json v2 CONTRACT, asserted on every published cell of every sport.
#
# Two consumer-side defects motivate it, both of them the same shape -- the
# platform doing league arithmetic the producer could have done:
#   * ithrottir.py:406 computes `total_rounds = 2 * (n_teams - 1)`. Basketball
#     male Bonusdeild is 12 teams with 162 played rows spanning an embedded
#     urslitakeppni, so "Umferdir eftir" would render -13 af 22. Icelandic
#     women's handball plays a TRIPLE round robin, so the formula is wrong
#     there in the other direction (21 rounds, not 14).
#   * og.py:696 hardcodes `max_points = round_num * 3`, wrong for both 2DT
#     sports (2 points a win).
#
# And D3 (design section 15): a basketball or handball league table decides the
# *deildarmeistari*; the Islandsmeistari comes out of an urslitakeppni this
# model does not simulate. That has to be encoded in the PAYLOAD, or a future
# consumer reuses football's champion copy for a sport where it is false.

.META_CONTRACT_KEYS <- c(
  "sport", "sex", "league", "division", "is_cup", "season", "generated_at",
  "fit_date", "round", "n_draws",
  "n_rounds", "n_rounds_source", "units", "points",
  "season_scope", "postseason", "qualify", "relegation"
)

test_that("every published cell ships the full meta v2 key set", {
  out <- .publish_all_cells()
  cells <- .published_cells(out)
  expect_equal(length(cells), 17L)

  for (cell in cells) {
    id <- paste(cell$sport, cell$sex, cell$division)
    meta <- .read_cell_json(cell, "meta.json")
    profile <- sport_publish_profile(cell$sport)

    expect_true(all(.META_CONTRACT_KEYS %in% names(meta)), info = id)
    expect_equal(meta$sport, cell$sport, info = id)
    expect_equal(meta$division, cell$division, info = id)
    expect_equal(meta$units$strength, profile$units$strength, info = id)
    expect_equal(
      meta$units$diff_bin_width, profile$units$diff_bin_width, info = id
    )
    expect_equal(meta$points$win, profile$points$win, info = id)
    expect_equal(meta$season_scope, profile$season_scope, info = id)
    expect_true("qualify" %in% names(meta), info = id)
    expect_true("relegation" %in% names(meta), info = id)
    expect_true(
      meta$n_rounds_source %in%
        c("config", "schedule", "none", "not_applicable"),
      info = id
    )
  }
})

test_that("n_rounds is never below round, on any cell", {
  out <- .publish_all_cells()
  for (cell in .published_cells(out)) {
    id <- paste(cell$sport, cell$sex, cell$division)
    meta <- .read_cell_json(cell, "meta.json")
    expect_true(
      is.null(meta$n_rounds) || meta$n_rounds >= meta$round,
      info = id
    )
    standings <- .read_cell_json(cell, "standings.json")
    if (length(standings$rows) > 0L) {
      played <- vapply(standings$rows, function(r) r$played, integer(1))
      expect_lte(meta$round, max(played))
    }
  }
})

test_that("standings agree with the points scheme meta itself declares", {
  # The assertion that catches a mislabelled scoring scheme in any sport: it
  # must hold for football's 3/1/0 exactly as for basketball's 2/-/0.
  out <- .publish_all_cells()
  checked <- 0L
  for (cell in .published_cells(out)) {
    id <- paste(cell$sport, cell$sex, cell$division)
    meta <- .read_cell_json(cell, "meta.json")
    draw_pts <- if (is.null(meta$points$draw)) 0L else meta$points$draw
    for (r in .read_cell_json(cell, "standings.json")$rows) {
      expect_equal(r$played, r$wins + r$draws + r$losses, info = id)
      expect_equal(
        r$points,
        r$wins * meta$points$win + r$draws * draw_pts +
          r$losses * meta$points$loss,
        info = id
      )
      checked <- checked + 1L
    }
  }
  expect_gt(checked, 0L)
})

test_that("n_draws is the real posterior draw count on every cell", {
  # THE 2DT GAP. fit_meta.parquet is the one partition-level extract -- no
  # `division` column by design -- so the reader's per-division split filtered
  # it to zero rows and every basketball and handball cell published
  # `n_draws: 0` while the fit ran on FIXTURE_N_DRAWS draws.
  out <- .publish_all_cells()
  for (cell in .published_cells(out)) {
    id <- paste(cell$sport, cell$sex, cell$division)
    meta <- .read_cell_json(cell, "meta.json")
    expect_gt(meta$n_draws, 0L)
    if (identical(cell$sport, "football")) {
      # Football recovers the count from its scoreline COUNT table, which is
      # the number its live payloads carry. The fixture builds that table as
      # 16 scorelines x as.integer(50 / 16) = 48, so 48 rather than 50 here is
      # a fixture-arithmetic artefact, not a publisher one; on a real partition
      # the counts sum to the draw count exactly.
      expect_lte(meta$n_draws, FIXTURE_N_DRAWS)
      next
    }
    expect_equal(meta$n_draws, FIXTURE_N_DRAWS, info = id)
  }
})

test_that("basketball and handball publish a regular-season scope, not a title", {
  out <- .publish_all_cells()
  for (cell in .published_cells(out)) {
    if (identical(cell$sport, "football")) next
    id <- paste(cell$sport, cell$sex, cell$division)
    meta <- .read_cell_json(cell, "meta.json")
    expect_equal(meta$season_scope, "regular_season", info = id)
    expect_equal(meta$postseason$name_is, "\u00darslitakeppni", info = id)
    expect_false(meta$postseason$modelled, info = id)
    # ID-B15: four cells, four post-season structures, and the women's top
    # flight takes every team through. No configured cut, so no p_qualify.
    expect_null(meta$qualify, info = id)

    fp <- .read_cell_json(cell, "final_positions.json")
    expect_equal(fp$basis, "regular_season_table", info = id)
  }
})

test_that("football keeps its full-season semantics", {
  out <- .publish_all_cells()
  for (cell in .published_cells(out)) {
    if (!identical(cell$sport, "football")) next
    id <- paste(cell$sport, cell$sex, cell$division)
    meta <- .read_cell_json(cell, "meta.json")
    expect_equal(meta$season_scope, "full_season", info = id)
    expect_null(meta$postseason, info = id)
    if (identical(cell$division, "BD")) {
      expect_equal(meta$qualify$slots, 6L, info = id)
      expect_equal(meta$qualify$label_is, "Efri hluti", info = id)
    } else {
      expect_null(meta$qualify, info = id)
    }
    fp <- .read_cell_json(cell, "final_positions.json")
    expect_equal(fp$basis, "final_table", info = id)
  }
})

# Recursively collect every KEY name in a parsed JSON payload.
.json_key_names <- function(x) {
  if (!is.list(x)) {
    return(character())
  }
  c(names(x), unlist(lapply(x, .json_key_names), use.names = FALSE))
}

test_that("no bb/hb payload carries a champion probability or the word Islandsmeistari", {
  out <- .publish_all_cells()
  files <- character()
  for (sport in c("basketball", "handball")) {
    files <- c(files, list.files(
      file.path(out, sport), pattern = "[.]json$",
      recursive = TRUE, full.names = TRUE
    ))
  }
  expect_gt(length(files), 0L)
  for (f in files) {
    keys <- .json_key_names(jsonlite::read_json(f, simplifyVector = FALSE))
    expect_length(intersect(keys, c("p_winner", "p_top_six")), 0L)
    raw <- paste(readLines(f, warn = FALSE), collapse = "")
    expect_false(grepl("\u00cdslandsmeistar", raw), info = basename(f))
    expect_false(grepl("Islandsmeistar", raw, fixed = TRUE), info = basename(f))
  }
})

test_that("Umferdir eftir cannot go negative on the real overhang data", {
  # The concrete blocker, on the committed real-federation rows rather than on
  # a fit. Basketball male Bonusdeild season 2026 has 162 played rows and
  # standings.played tops out at 35 against a 22-round regular season.
  overhang <- arrow::read_parquet(
    testthat::test_path("fixtures", "facts", "playoff-overhang.parquet")
  )
  bd_male <- overhang[
    overhang$sex == "male" & overhang$division == "BD", ,
    drop = FALSE
  ]
  expect_equal(max(table(c(bd_male$home_team, bd_male$away_team))), 35L)

  fmt <- .publish_n_rounds(
    results = bd_male, schedules = bd_male[0, , drop = FALSE],
    season = 2026L, division_codes = "BD",
    end_date = as.Date("2026-06-01"),
    expected_meetings = .iceland_division_expected_meetings(
      "basketball_iceland", "male"
    )[["BD"]]
  )
  regular <- .regular_season_cut(bd_male, fmt)
  expect_equal(
    max(table(c(regular$home_team, regular$away_team))), 22L
  )
  round <- .publish_round(bd_male, 2026L, "BD", fmt$cut)
  expect_equal(fmt$n_rounds - round, 0L)
})
