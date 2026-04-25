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

test_that("fit targets exist for every active (league x sex) combo", {
  testthat::skip_if_not_installed("targets")
  manifest <- targets::tar_manifest(
    script = here::here("_targets.R"),
    fields = "name",
    callr_function = NULL
  )
  leagues <- load_leagues()
  active <- filter_leagues(leagues, active_only = TRUE)
  for (key in names(active)) {
    for (sex in active[[key]]$sexes) {
      expect_true(
        paste0("fit_", key, "_", sex) %in% manifest$name,
        info = paste("missing fit target for", key, sex)
      )
    }
  }
})

test_that("decide targets exist for every active (league x sex)", {
  testthat::skip_if_not_installed("targets")
  manifest <- targets::tar_manifest(
    script = here::here("_targets.R"),
    fields = "name",
    callr_function = NULL
  )
  leagues <- load_leagues()
  active <- filter_leagues(leagues, active_only = TRUE)
  for (key in names(active)) {
    for (sex in active[[key]]$sexes) {
      expect_true(
        paste0("decide_", key, "_", sex) %in% manifest$name,
        info = paste("missing decide target for", key, sex)
      )
    }
  }
})

test_that("publish targets exist for every active (league x sex)", {
  testthat::skip_if_not_installed("targets")
  manifest <- targets::tar_manifest(
    script = here::here("_targets.R"),
    fields = "name",
    callr_function = NULL
  )
  leagues <- load_leagues()
  active <- filter_leagues(leagues, active_only = TRUE)
  for (key in names(active)) {
    for (sex in active[[key]]$sexes) {
      expect_true(
        paste0("publish_", key, "_", sex) %in% manifest$name,
        info = paste("missing publish target for", key, sex)
      )
    }
  }
})
