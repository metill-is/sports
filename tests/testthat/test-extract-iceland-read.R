# One reader for all three Icelandic leagues. read_extracted_football() was
# generalised by PARAMETERISATION -- the descending fit_date scan, the
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
  # This is the committed fixture's own state -- it carries the five 2DT
  # parquets and neither round_strengths_quantiles nor fit_meta -- so the
  # default case for basketball and handball is "reads fine, optional files
  # empty". Without that the publisher could not run before WS8 lands.
  for (sport in c("basketball", "handball")) {
    league <- load_leagues()[[paste0(sport, "_iceland")]]
    root <- fixture_extracts_root(sport)
    part <- file.path(
      root, paste0("sport=", sport), "country=iceland", "sex=female",
      paste0("fit_date=", format(FIXTURE_FIT_DATE, "%Y-%m-%d"))
    )
    expect_false(file.exists(file.path(part, "round_strengths_quantiles.parquet")))
    expect_false(file.exists(file.path(part, "fit_meta.parquet")))

    out <- read_extracted_iceland(
      league,
      sex = "female", fit_date = FIXTURE_FIT_DATE, extracts_root = root
    )
    code <- .iceland_division_codes(paste0(sport, "_iceland"), "female")[[1]]
    for (optional in sport_publish_profile(sport)$optional_extracts) {
      expect_equal(nrow(out[[code]][[optional]]), 0L, info = optional)
      expect_true(tibble::is_tibble(out[[code]][[optional]]), info = optional)
    }
  }
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
