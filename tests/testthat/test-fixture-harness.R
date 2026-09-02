# Contract tests for the committed fixture harness (WS2, spec section 4).
#
# The generator under tools/ must be source()-able from a test *without*
# loading a package: inside a git worktree `here::here()` resolves to the MAIN
# checkout, so a top-level `devtools::load_all(here::here())` would regenerate
# fixtures against a different package than the one being edited.

test_that("the fixture generator is source-able without loading a package", {
  gen <- testthat::test_path("..", "..", "tools", "make-extract-fixtures.R")
  expect_true(file.exists(gen))

  src <- readLines(gen, warn = FALSE)
  # No unguarded load_all / here::here() at column 0 (top level).
  top_level <- grep("^[^ \t#]", src, value = TRUE)
  expect_false(any(grepl("load_all", top_level, fixed = TRUE)))
  expect_false(any(grepl("here::here", top_level, fixed = TRUE)))

  env <- new.env(parent = globalenv())
  expect_silent(sys.source(gen, envir = env))
  expect_true(is.function(env$make_extract_fixtures))
})

test_that("the facts fixture drives prepare_data for all three sports", {
  root <- fixture_facts_root()
  leagues <- load_leagues()

  cells <- list(
    list(key = "basketball_iceland", sex = "male"),
    list(key = "basketball_iceland", sex = "female"),
    list(key = "handball_iceland", sex = "male"),
    list(key = "handball_iceland", sex = "female"),
    list(key = "football_iceland", sex = "male"),
    list(key = "football_iceland", sex = "female")
  )

  for (cell in cells) {
    prep <- suppressMessages(prepare_data(
      leagues[[cell$key]], cell$sex,
      end_date = FIXTURE_END_DATE, root = root
    ))
    info <- paste(cell$key, cell$sex)
    # Training data present, upcoming fixtures inside the DEFAULT 14-day horizon.
    expect_gt(nrow(prep$teams), 5L)
    expect_gt(prep$stan_data$N, 10L)
    expect_gt(nrow(prep$pred_d), 0L)
    expect_equal(prep$stan_data$N_pred, nrow(prep$pred_d), info = info)
    expect_true(all(prep$pred_d$match_date > FIXTURE_END_DATE), info = info)
    expect_true(all(prep$pred_d$match_date <= FIXTURE_END_DATE + 14L), info = info)
  }
})
