# One reader for all three Icelandic leagues. The football-only reader it
# replaces was generalised by PARAMETERISATION -- the descending fit_date scan, the
# completeness check and the per-division split exist exactly once -- so these
# assertions cover a 2DT tree and the live football tree through the same
# function.

test_that("read_extracted_iceland splits a 2DT partition by division", {
  league <- load_leagues()[["basketball_iceland"]]
  root <- fixture_extracts_root("basketball")

  out <- read_extracted_iceland(
    league,
    sex = "male", fit_date = FIXTURE_FIT_DATE, extracts_root = root
  )

  expect_setequal(
    setdiff(names(out), c("fit_date", "sim_inputs", "cup_bracket")),
    .iceland_division_codes("basketball_iceland", "male")
  )
  expect_equal(out$fit_date, FIXTURE_FIT_DATE)

  profile <- sport_publish_profile("basketball")
  for (code in .iceland_division_codes("basketball_iceland", "male")) {
    expect_true(
      all(profile$required_extracts %in% names(out[[code]])),
      info = code
    )
  }

  # predicted_matches.parquet is the one file carrying `division` today, so it
  # is what proves the split works on a 2DT tree.
  expect_gt(nrow(out$BD$predicted_matches), 0L)
  expect_gt(nrow(out[["1D"]]$predicted_matches), 0L)
  bd_teams <- unique(c(
    out$BD$predicted_matches$home_team, out$BD$predicted_matches$away_team
  ))
  d1_teams <- unique(c(
    out[["1D"]]$predicted_matches$home_team,
    out[["1D"]]$predicted_matches$away_team
  ))
  expect_length(intersect(bd_teams, d1_teams), 0L)
  expect_false("division" %in% names(out$BD$predicted_matches))
})

test_that("read_extracted_iceland aborts when a REQUIRED parquet is missing", {
  league <- load_leagues()[["handball_iceland"]]
  root <- fixture_extracts_root("handball")
  part <- file.path(
    root, "sport=handball", "country=iceland", "sex=male",
    paste0("fit_date=", format(FIXTURE_FIT_DATE, "%Y-%m-%d"))
  )
  expect_true(file.remove(file.path(part, "predicted_matches.parquet")))

  expect_error(
    read_extracted_iceland(
      league,
      sex = "male", fit_date = FIXTURE_FIT_DATE, extracts_root = root
    ),
    "is incomplete"
  )
})

test_that("absent OPTIONAL parquets degrade to 0-row tibbles", {
  # A partition written before a file type existed must still read. The
  # committed fixture now carries every file the extractor writes (WS8), so the
  # older shape is reproduced by DELETING the optional files from the temp copy
  # rather than by relying on the fixture being incomplete.
  for (sport in c("basketball", "handball")) {
    league <- load_leagues()[[paste0(sport, "_iceland")]]
    root <- fixture_extracts_root(sport)
    part <- file.path(
      root, paste0("sport=", sport), "country=iceland", "sex=female",
      paste0("fit_date=", format(FIXTURE_FIT_DATE, "%Y-%m-%d"))
    )
    optionals <- sport_publish_profile(sport)$optional_extracts
    expect_gt(length(optionals), 0L)
    for (optional in optionals) {
      path <- file.path(part, paste0(optional, ".parquet"))
      expect_true(file.exists(path), info = optional)
      expect_true(file.remove(path))
    }

    out <- read_extracted_iceland(
      league,
      sex = "female", fit_date = FIXTURE_FIT_DATE, extracts_root = root
    )
    code <- .iceland_division_codes(paste0(sport, "_iceland"), "female")[[1]]
    for (optional in optionals) {
      expect_equal(nrow(out[[code]][[optional]]), 0L, info = optional)
      expect_true(tibble::is_tibble(out[[code]][[optional]]), info = optional)
    }
  }
})

test_that("a 2DT partition missing round_strengths_quantiles is INCOMPLETE", {
  # It is a required extract for the 2DT sports, unlike football's fit_meta:
  # data/beliefs/extracts/ holds no basketball or handball partition at all, so
  # there is no pre-contract history for the requirement to strand.
  league <- load_leagues()[["basketball_iceland"]]
  root <- fixture_extracts_root("basketball")
  part <- file.path(
    root, "sport=basketball", "country=iceland", "sex=male",
    paste0("fit_date=", format(FIXTURE_FIT_DATE, "%Y-%m-%d"))
  )
  expect_true(file.remove(file.path(part, "round_strengths_quantiles.parquet")))

  expect_error(
    read_extracted_iceland(
      league,
      sex = "male", fit_date = FIXTURE_FIT_DATE, extracts_root = root
    ),
    "is incomplete"
  )
})

test_that("football still reads through the generalised reader", {
  league <- load_leagues()[["football_iceland"]]
  facts_root <- fixture_facts_root()
  extracts_root <- file.path(withr::local_tempdir(), "extracts")
  build_football_extracts_fixture(facts_root, extracts_root, "male")

  out <- read_extracted_iceland(
    league,
    sex = "male", fit_date = FIXTURE_FIT_DATE, extracts_root = extracts_root
  )

  expect_setequal(
    setdiff(names(out), c("fit_date", "sim_inputs", "cup_bracket")),
    .iceland_division_codes("football_iceland", "male")
  )
  expect_true(
    all(
      sport_publish_profile("football")$required_extracts %in% names(out$BD)
    )
  )
  expect_gt(nrow(out$BD$predicted_matches), 0L)
  # tournament_placements is football's optional 7th file and IS in the fixture.
  expect_gt(nrow(out$BD$tournament_placements), 0L)
})

test_that("extract_partition_exists is sport-neutral", {
  root <- withr::local_tempdir()
  cell <- file.path(
    root, "sport=basketball", "country=iceland", "sex=male",
    "fit_date=2100-01-01"
  )
  dir.create(cell, recursive = TRUE)
  expect_true(extract_partition_exists(root, "basketball", "iceland", "male"))
  expect_false(extract_partition_exists(root, "basketball", "iceland", "female"))
  expect_false(extract_partition_exists(root, "handball", "iceland", "male"))
})
