# The INLA engine has tests that only mean something where INLA is installed.
# CI Tests does not install it, so there they skip. This file pins the
# workflow that runs them for real (.github/workflows/inla-engine.yml), and the
# rule that makes a missing INLA a failure in that job, not a skip: a skip
# there would be a green check for tests that never ran.

inla_workflow <- here::here(".github", "workflows", "inla-engine.yml")

read_inla_workflow <- function() {
  yml <- yaml::read_yaml(inla_workflow)
  # YAML 1.1 reads the key `on` as the boolean TRUE.
  yml$triggers <- yml[["on"]] %||% yml[["TRUE"]]
  yml
}

test_that("the INLA engine tests run on pull requests that touch what the engine depends on", {
  skip_if_not(dir.exists(dirname(inla_workflow)), "workflow dir not present")
  expect_true(file.exists(inla_workflow))
  on <- read_inla_workflow()$triggers
  expect_identical(unlist(on$pull_request$branches), "main")
  # The fit path reaches most of R/ (prepare, storage, config, extract), so
  # the filter is the package, its tests and its config, not a list of files
  # someone has to keep up to date.
  expect_true(all(
    c("R/**", "tests/testthat/**", "config/**", "DESCRIPTION", ".github/workflows/inla-engine.yml") %in%
      unlist(on$pull_request$paths)
  ))
  # It is not tied to the branch it was first written on.
  expect_false(any(grepl("claude/", readLines(inla_workflow, warn = FALSE), fixed = TRUE)))
  expect_false(file.exists(here::here(".github", "workflows", "inla-trial.yml")))
})

test_that("the INLA engine workflow cannot write to the repository", {
  skip_if_not(file.exists(inla_workflow), "workflow not present")
  yml <- read_inla_workflow()
  expect_identical(yml$permissions, list(contents = "read"))
  lines <- readLines(inla_workflow, warn = FALSE)
  for (forbidden in c("git commit", "git push", "git add", "push-with-retry")) {
    expect_false(any(grepl(forbidden, lines, fixed = TRUE)), info = forbidden)
  }
  # It checks for itself that nothing under data/ changed.
  expect_true(any(grepl("git status --porcelain -- data", lines, fixed = TRUE)))
})

test_that("in the INLA engine workflow a test that needs INLA fails when INLA is missing", {
  skip_if_not(file.exists(inla_workflow), "workflow not present")
  job <- read_inla_workflow()$jobs[[1]]
  expect_identical(job$env$SPORTS_REQUIRE_INLA, "1")
  # The tests reach INLA only through the helper that enforces that, and the
  # workflow runs every file that calls it.
  files <- list.files(testthat::test_path(), pattern = "^test-.*[.]R$", full.names = TRUE)
  mentions <- function(pattern, fixed = FALSE) {
    hit <- vapply(files, function(f) any(grepl(pattern, readLines(f, warn = FALSE), fixed = fixed)), logical(1))
    basename(files[hit])
  }
  expect_identical(mentions("skip_if_not_installed\\(\"(INLA|sn)\"\\)"), character())
  expect_true("test-model-v2.R" %in% mentions("skip_if_no_inla()", fixed = TRUE))
  expect_true(any(grepl("skip_if_no_inla(", readLines(inla_workflow, warn = FALSE), fixed = TRUE)))
})

test_that("skip_if_no_inla() skips by default and fails where INLA is required", {
  all_there <- c(INLA = TRUE, sn = TRUE)
  withr::with_envvar(c(SPORTS_REQUIRE_INLA = NA), {
    expect_condition(skip_if_no_inla(c(INLA = FALSE, sn = TRUE)), class = "skip")
    expect_no_condition(skip_if_no_inla(all_there))
  })
  withr::with_envvar(c(SPORTS_REQUIRE_INLA = "1"), {
    expect_error(skip_if_no_inla(c(INLA = TRUE, sn = FALSE)), "sn")
    expect_error(skip_if_no_inla(c(INLA = FALSE, sn = FALSE)), "INLA, sn")
    expect_no_condition(skip_if_no_inla(all_there))
  })
})
