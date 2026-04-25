test_that("_targets.R parses and validates", {
  testthat::skip_if_not_installed("targets")
  manifest <- targets::tar_manifest(
    script = here::here("_targets.R"),
    fields = "name",
    callr_function = NULL
  )
  expect_true(nrow(manifest) > 0L)
  expect_true("leagues_config" %in% manifest$name)
  expect_true("active_competitions" %in% manifest$name)
})

test_that("ingest targets exist for every active league", {
  testthat::skip_if_not_installed("targets")
  manifest <- targets::tar_manifest(
    script = here::here("_targets.R"),
    fields = "name",
    callr_function = NULL
  )
  leagues <- load_leagues()
  active <- filter_leagues(leagues, active_only = TRUE)
  for (key in names(active)) {
    expect_true(
      paste0("ingest_", key) %in% manifest$name,
      info = paste("missing ingest target for", key)
    )
  }
})

test_that("odds targets exist for every Lengjan-configured active league", {
  testthat::skip_if_not_installed("targets")
  manifest <- targets::tar_manifest(
    script = here::here("_targets.R"),
    fields = "name",
    callr_function = NULL
  )
  leagues <- load_leagues()
  active_lengjan <- filter_leagues(leagues, active_only = TRUE, has_lengjan = TRUE)
  for (key in names(active_lengjan)) {
    expect_true(
      paste0("odds_", key) %in% manifest$name,
      info = paste("missing odds target for", key)
    )
  }
})
