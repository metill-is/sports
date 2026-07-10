# Post-split BD-cell publish surfaces: standings tabulate over
# BD + BD_UPPER_PO + BD_LOWER_PO with a group-locked rank, next_games
# carries split-phase fixtures, meta.round continues counting, and the
# xG/xPts round aggregation crosses into the split phase.
#
# Synthetic 8-team season against the real male config (split 6/6).
# Regular phase: .mini_reg_results() all-pairs -> ranking = team order A..H,
# 14 games each. Split phase: A beats F (upper); G beats H three times
# (lower) -> G ends on 6 + 9 = 15 pts, F on 12 + 0 = 12 pts. The
# group-locked table must still rank every efri team (A..F) above every
# nedri team (G, H).

.split_po_results <- function() {
  tibble::tibble(
    home_team = c("A", "G", "H", "G"),
    away_team = c("F", "H", "G", "H"),
    home_score = c(1L, 2L, 0L, 3L),
    away_score = c(0L, 0L, 2L, 1L),
    division = c("BD_UPPER_PO", "BD_LOWER_PO", "BD_LOWER_PO", "BD_LOWER_PO"),
    season = 2026L,
    match_date = as.Date(c(
      "2026-09-07", "2026-09-07", "2026-09-14", "2026-09-21"
    )),
    round = 23L
  )
}

.write_split_fixture_root <- function(root, end_date) {
  teams8 <- LETTERS[1:8]
  regular <- .mini_reg_results(teams8) |>
    dplyr::mutate(round = 1L)
  results <- dplyr::bind_rows(regular, .split_po_results()) |>
    dplyr::mutate(sport = "football", country = "iceland", sex = "male")
  write_table(results, "results", root = root)

  schedules <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male",
    season = 2026L, match_date = end_date + 2L,
    home_team = "B", away_team = "C",
    division = "BD_UPPER_PO", round = 24L,
    kickoff_time = NA_character_
  )
  write_table(schedules, "schedules", root = root)
  invisible(NULL)
}

.split_test_extracted <- function(end_date) {
  bd <- .empty_extracted_pfi()
  bd$predicted_matches <- tibble::tibble(
    home_team = "B", away_team = "C",
    match_date = end_date + 2L,
    home_goals = c(0L, 1L, 2L), away_goals = c(0L, 0L, 1L),
    count = c(10L, 20L, 10L)
  )
  list(BD = bd, fit_date = end_date)
}

.publish_split_fixture <- function(root, out, end_date,
                                   extracts_root = withr::local_tempdir(
                                     .local_envir = parent.frame()
                                   ),
                                   hist_root = withr::local_tempdir(
                                     .local_envir = parent.frame()
                                   )) {
  league <- load_leagues()[["football_iceland"]]
  suppressWarnings(suppressMessages(
    publish_football_iceland(
      extracted = .split_test_extracted(end_date),
      league = league, sex = "male", end_date = end_date,
      root = root, output_root = out,
      extracts_root = extracts_root,
      archive_root = withr::local_tempdir(.local_envir = parent.frame()),
      round_predictions_history_root = hist_root
    )
  ))
}

test_that("split cell: standings tabulate the family and rank group-locked", {
  root <- withr::local_tempdir()
  out <- withr::local_tempdir()
  end_date <- as.Date("2026-09-22")
  .write_split_fixture_root(root, end_date)

  .publish_split_fixture(root, out, end_date)

  standings <- jsonlite::fromJSON(
    file.path(out, "football", "iceland", "karla-bd", "standings.json"),
    simplifyDataFrame = TRUE
  )
  rows <- tibble::as_tibble(standings$rows)

  # Family tabulation: split matches count. G played 14 regular + 3 split.
  expect_equal(rows$played[rows$team == "G"], 17L)
  expect_equal(rows$points[rows$team == "G"], 15L)
  expect_equal(rows$points[rows$team == "F"], 12L)

  # Group lock: A..F occupy ranks 1..6 even though G out-points F.
  expect_setequal(rows$team[rows$rank <= 6L], LETTERS[1:6])
  expect_equal(rows$team[rows$rank == 7L], "G")
  expect_equal(rows$team[rows$rank == 8L], "H")

  # as_of advances to the last split match.
  expect_equal(standings$as_of, "2026-09-21")
})

test_that("split cell: next_games carries split-phase fixtures with BDU code", {
  root <- withr::local_tempdir()
  out <- withr::local_tempdir()
  end_date <- as.Date("2026-09-22")
  .write_split_fixture_root(root, end_date)

  .publish_split_fixture(root, out, end_date)

  ng <- jsonlite::fromJSON(
    file.path(out, "football", "iceland", "karla-bd", "next_games.json"),
    simplifyDataFrame = TRUE
  )
  matches <- tibble::as_tibble(ng$matches)
  expect_equal(nrow(matches), 1L)
  expect_equal(matches$home, "B")
  expect_equal(matches$away, "C")
  expect_equal(matches$division, "BD_UPPER_PO")
  expect_equal(matches$division_code, "BDU")

  meta <- jsonlite::read_json(
    file.path(out, "football", "iceland", "karla-bd", "meta.json")
  )
  # round = min per-team appearance count over the family (B..E have 14).
  expect_equal(meta$round, 14L)

  # The BD cell must stay schema-valid (no schema change shipped). The other
  # cells fail only on n_draws >= 1 -- a fixture artifact (no extracts were
  # synthesised for them), not publisher behaviour under test here.
  v <- validate_publish_dir(
    out,
    schema_dir = testthat::test_path("..", "..", "config", "publish-schemas")
  )
  expect_equal(grep("karla-bd/", v$errors, value = TRUE), character(0))
  expect_equal(grep("karla-bd/", v$unmatched, value = TRUE), character(0))
})
