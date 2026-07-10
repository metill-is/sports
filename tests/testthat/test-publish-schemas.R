test_that("validate_publish_dir() succeeds on the live publish tree", {
  # Real-output regression: every JSON in data/publish/ must conform to its
  # corresponding schema in config/publish-schemas/. If this test fails,
  # either the schemas are stale or the publisher has introduced drift —
  # both are bugs that warrant inspection rather than schema relaxation.
  skip_if_not(
    dir.exists(here::here("data", "publish", "football", "iceland")),
    "Live publish tree not present"
  )

  result <- validate_publish_dir(
    here::here("data", "publish"),
    schema_dir = here::here("config", "publish-schemas")
  )

  expect_true(result$ok)
  expect_equal(length(result$errors), 0L)
  expect_gt(result$n_files, 0L)
})

test_that("validate_publish_dir() rejects meta.json missing required field", {
  tmp <- withr::local_tempdir()
  cell <- file.path(tmp, "football", "iceland", "karla-bd")
  dir.create(cell, recursive = TRUE)

  # Missing required `sport` field
  bad_meta <- list(
    sex = "male",
    league = "Besta deild",
    division = "BD",
    is_cup = FALSE,
    season = 2026L,
    generated_at = "2026-05-25T12:00:00+0000",
    fit_date = "2026-05-25",
    round = 8L,
    n_draws = 4000L
  )
  jsonlite::write_json(bad_meta, file.path(cell, "meta.json"),
    auto_unbox = TRUE, null = "null"
  )

  result <- validate_publish_dir(
    tmp,
    schema_dir = here::here("config", "publish-schemas")
  )

  expect_false(result$ok)
  expect_gt(length(result$errors), 0L)
  expect_true(any(grepl("sport|required", paste(result$errors, collapse = "\n"))))
})

test_that("validate_publish_dir() rejects wrong-type field", {
  tmp <- withr::local_tempdir()
  cell <- file.path(tmp, "football", "iceland", "karla-bd")
  dir.create(cell, recursive = TRUE)

  bad_meta <- list(
    sport = "football",
    sex = "male",
    league = "Besta deild",
    division = "BD",
    is_cup = "no", # WRONG: should be boolean
    season = 2026L,
    generated_at = "2026-05-25T12:00:00+0000",
    fit_date = "2026-05-25",
    round = 8L,
    n_draws = 4000L
  )
  jsonlite::write_json(bad_meta, file.path(cell, "meta.json"),
    auto_unbox = TRUE, null = "null"
  )

  result <- validate_publish_dir(
    tmp,
    schema_dir = here::here("config", "publish-schemas")
  )

  expect_false(result$ok)
  expect_gt(length(result$errors), 0L)
})

test_that("validate_publish_dir() ignores files without a schema", {
  # Files in data/publish/ that don't have a corresponding *.schema.json
  # (e.g. round_predictions_history.json was relocated; nothing prevents
  # a future stray) should NOT cause validation to fail. They're flagged
  # but treated as informational.
  tmp <- withr::local_tempdir()
  cell <- file.path(tmp, "football", "iceland", "karla-bd")
  dir.create(cell, recursive = TRUE)

  jsonlite::write_json(list(foo = "bar"), file.path(cell, "stray.json"),
    auto_unbox = TRUE
  )

  result <- validate_publish_dir(
    tmp,
    schema_dir = here::here("config", "publish-schemas")
  )

  # No schemas to match → 0 validated files → ok (vacuously true).
  expect_true(result$ok)
  expect_equal(result$n_files, 0L)
  expect_gt(length(result$unmatched), 0L)
})

test_that("validate_publish_dir() handles cup cells with is_cup=true", {
  # Cup cells ship is_cup=true plus tournament_placements.json. The
  # validator is path-aware (looks up schemas via the JSON's first path
  # segment after `dir`), so we always call it against the publish root,
  # not a sub-cell. This test confirms cup-cell files don't surface in
  # `errors`.
  skip_if_not(
    file.exists(here::here(
      "data", "publish", "football", "iceland", "karla-bikar",
      "tournament_placements.json"
    )),
    "Cup cell not present"
  )

  result <- validate_publish_dir(
    here::here("data", "publish"),
    schema_dir = here::here("config", "publish-schemas")
  )

  cup_errors <- grep("karla-bikar|kvenna-bikar", result$errors, value = TRUE)
  expect_equal(length(cup_errors), 0L,
    info = paste(cup_errors, collapse = "\n")
  )
  expect_gt(result$n_files, 0L)
})

test_that("meta.json schema accepts a well-formed split object and rejects a malformed one", {
  base_meta <- list(
    sport = "football", sex = "male", league = "Besta deild",
    division = "BD", is_cup = FALSE, season = 2026L,
    generated_at = "2026-07-10T12:00:00+0000", fit_date = "2026-07-10",
    round = 14L, n_draws = 4000L
  )

  write_cell <- function(meta) {
    tmp <- withr::local_tempdir(.local_envir = parent.frame())
    cell <- file.path(tmp, "football", "iceland", "karla-bd")
    dir.create(cell, recursive = TRUE)
    jsonlite::write_json(meta, file.path(cell, "meta.json"),
      auto_unbox = TRUE, null = "null"
    )
    tmp
  }

  good <- write_cell(c(base_meta, list(split = list(upper = 6L, lower = 6L))))
  result_good <- validate_publish_dir(
    good,
    schema_dir = here::here("config", "publish-schemas")
  )
  expect_true(result_good$ok)

  bad <- write_cell(c(base_meta, list(split = list(upper = "six"))))
  result_bad <- validate_publish_dir(
    bad,
    schema_dir = here::here("config", "publish-schemas")
  )
  expect_false(result_bad$ok)
})
