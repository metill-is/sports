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
