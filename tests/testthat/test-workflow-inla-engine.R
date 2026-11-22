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

test_that("the fit workflow installs R-INLA as the INLA engine tests do, and cannot be stopped by it before a league uses it", {
  # WHY: when a league switches to engine: inla, fit.yml is where it fits. The
  # install mirrors inla-engine.yml step for step, at the same pinned version,
  # so it is exercised on the runner that will use it before any league
  # depends on it. While no league does, the steps are continue-on-error, so an
  # outage at R-INLA's host or an apt mirror cannot cost a day of Stan fits;
  # the commit that switches a league must make them blocking.
  # R-INLA is not in DESCRIPTION: pak, which installs every workflow's
  # packages, does not read Additional_repositories, so a Suggests entry would
  # break the install step of every workflow that resolves suggested packages.
  fit_workflow <- here::here(".github", "workflows", "fit.yml")
  skip_if_not(file.exists(fit_workflow) && file.exists(inla_workflow), "workflows not present")
  engine <- read_inla_workflow()$jobs[[1]]
  fit <- yaml::read_yaml(fit_workflow)$jobs$fit
  expect_identical(fit$env$INLA_VERSION, engine$env$INLA_VERSION)
  expect_identical(fit$env$INLA_REPO, engine$env$INLA_REPO)

  first_step <- function(steps, pattern) {
    hit <- which(vapply(steps, function(s) any(grepl(pattern, c(s$uses %||% "", s$run %||% ""))), logical(1)))
    if (length(hit)) hit[[1]] else NA_integer_
  }
  lines_of <- function(step, pattern) trimws(grep(pattern, strsplit(step$run %||% "", "\n")[[1]], value = TRUE))
  steps <- fit$steps
  at <- c(
    sysreqs = first_step(steps, "apt-get install"),
    rdeps = first_step(steps, "install.packages\\(c\\(\"fmesher\""),
    inla = first_step(steps, "INLA_%s.tar.gz"),
    smoke = first_step(steps, "INLA::inla\\("),
    fit = first_step(steps, "scripts/03_fit.R")
  )
  # sf's system libraries, then R-INLA's R dependencies, R-INLA, a fit with
  # it, and all of it before the league fits.
  expect_false(anyNA(at), info = paste(names(at)[is.na(at)], collapse = ", "))
  expect_identical(order(at), seq_along(at))

  # The same commands as inla-engine.yml: system libraries, the install and its
  # version check, the smoke fit.
  e_steps <- engine$steps
  e_apt <- e_steps[[first_step(e_steps, "apt-get install")]]
  expect_identical(lines_of(steps[[at[["sysreqs"]]]], "apt-get"), lines_of(e_apt, "apt-get"))
  pin <- "INLA_%s.tar.gz|packageVersion\\(\"INLA\"\\)"
  expect_identical(lines_of(steps[[at[["inla"]]]], pin), lines_of(e_steps[[first_step(e_steps, "INLA_%s.tar.gz")]], pin))
  smoke <- "INLA::inla\\("
  expect_identical(lines_of(steps[[at[["smoke"]]]], smoke), lines_of(e_steps[[first_step(e_steps, smoke)]], smoke))
  # R-INLA's R dependencies are the ones the engine job installs for it.
  e_deps <- e_steps[[first_step(e_steps, "setup-r-dependencies")]]
  e_extra <- sub("^any::", "", strsplit(trimws(e_deps$with$`extra-packages`), "\\s+")[[1]])
  rdeps <- regmatches(steps[[at[["rdeps"]]]]$run, gregexpr("\"[A-Za-z0-9.]+\"", steps[[at[["rdeps"]]]]$run))[[1]]
  expect_setequal(gsub("\"", "", rdeps), setdiff(e_extra, c("devtools", "testthat")))
  # The fit's own dependency step is untouched by them.
  f_extra <- strsplit(trimws(steps[[first_step(steps, "setup-r-dependencies")]]$with$`extra-packages`), "\\s+")[[1]]
  expect_false(any(c("any::fmesher", "any::MatrixModels", "any::sn") %in% f_extra))

  # Non-blocking exactly while no league uses the engine; never skipped.
  inla_steps <- steps[at[c("sysreqs", "rdeps", "inla", "smoke")]]
  engines <- vapply(load_leagues(), function(l) l$engine %||% "stan", character(1))
  blocking <- vapply(inla_steps, function(s) !isTRUE(s[["continue-on-error"]]), logical(1))
  if (any(engines == "inla")) {
    expect_true(all(blocking), info = "a league uses engine: inla, so its install must stop the fit when it fails")
  } else {
    expect_false(any(blocking), info = "no league uses engine: inla, so its install must not stop the Stan fits")
  }
  expect_true(all(vapply(inla_steps, function(s) is.null(s[["if"]]), logical(1))))
})
