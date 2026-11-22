# The weekly Stan reference fit's workflow (.github/workflows/engine-reference.yml).
#
# It fits football v2.0 with R-INLA and with NUTS once a week and commits one
# summary row per cell under data/health/engine_reference/, which the health
# check reads. It must not be able to touch anything else on main, must not
# set off the workflows that push data (fit, decide-publish), and must install
# its two engines the way the workflows that already run them do.

reference_workflow <- here::here(".github", "workflows", "engine-reference.yml")

read_reference_workflow <- function() {
  yml <- yaml::read_yaml(reference_workflow)
  # YAML 1.1 reads the key `on` as the boolean TRUE.
  yml$triggers <- yml[["on"]] %||% yml[["TRUE"]]
  yml
}

first_step <- function(steps, pattern) {
  hit <- which(vapply(steps, function(s) any(grepl(pattern, c(s$uses %||% "", s$run %||% "", s$name %||% ""))), logical(1)))
  if (length(hit)) hit[[1]] else NA_integer_
}

test_that("the reference fit runs weekly and by hand, and nothing else starts it", {
  skip_if_not(dir.exists(dirname(reference_workflow)), "workflow dir not present")
  expect_true(file.exists(reference_workflow))
  yml <- read_reference_workflow()
  expect_setequal(names(yml$triggers), c("schedule", "workflow_dispatch"))
  expect_length(yml$triggers$schedule, 1L)
  # One weekly cron: minute hour * * day-of-week.
  expect_match(yml$triggers$schedule[[1]]$cron, "^[0-9]+ [0-9]+ \\* \\* [0-6]$")
  # workflow_run filters match on `name:` as a glob (ci-conventions.md): a
  # name with glob characters would be unmatchable as a parent.
  expect_false(grepl("[][*?!]", yml$name))
  expect_true(nzchar(yml$name))
})

test_that("the reference fit can only write its own summary rows", {
  skip_if_not(file.exists(reference_workflow), "workflow not present")
  yml <- read_reference_workflow()
  expect_identical(yml$permissions, list(contents = "write"))
  lines <- readLines(reference_workflow, warn = FALSE)
  code <- lines[!grepl("^\\s*#", lines)]
  adds <- trimws(grep("git add", code, value = TRUE))
  expect_length(adds, 1L)
  # The paths staged: everything after `git add` up to a redirect or `||`.
  staged <- strsplit(trimws(sub("(2>|\\|\\|).*$", "", sub("^git add", "", adds))), "\\s+")[[1]]
  expect_identical(staged, "data/health/engine_reference/")
  # Pushes like every data-writing workflow, and without --prefer-ours: its
  # one path is written by nothing else, so a conflict there means something
  # unexpected happened and should fail loudly.
  expect_true(any(grepl("\\.github/scripts/push-with-retry\\.sh\\s*$", code)))
  expect_false(any(grepl("prefer-ours", code, fixed = TRUE)))
  # It never sets off the workflows whose commit steps push to main.
  for (forbidden in c("gh workflow run", "workflow_dispatch:\\s*\\{?\\s*fit", "repository_dispatch", "03_fit.R", "04_decide.R", "05_publish.R")) {
    expect_false(any(grepl(forbidden, code)), info = forbidden)
  }
  expect_true(any(grepl("scripts/engine_reference.R", code, fixed = TRUE)))
  # A run longer than this is broken, not slow: two cells, each one INLA and
  # one four-chain NUTS fit.
  expect_lte(yml$jobs[[1]][["timeout-minutes"]], 120)
})

test_that("the reference fit follows the CI conventions for R and its packages", {
  skip_if_not(file.exists(reference_workflow), "workflow not present")
  job <- read_reference_workflow()$jobs[[1]]
  steps <- job$steps
  deps <- steps[[first_step(steps, "setup-r-dependencies")]]
  expect_identical(deps$env$PKG_SYSREQS, "false")
  expect_true(all(c("any::devtools", "any::cmdstanr") %in% strsplit(trimws(deps$with$`extra-packages`), "\\s+")[[1]]))
  setup <- steps[[first_step(steps, "r-lib/actions/setup-r@")]]
  expect_identical(setup$with$`r-version`, "4.5.3")
  expect_match(setup$with$`extra-repositories`, "stan-dev.r-universe.dev", fixed = TRUE)
  expect_false(is.na(first_step(steps, "DOWNLOAD_STATIC_LIBV8|Reinstall V8")))
})

test_that("the reference fit installs R-INLA as fit.yml and inla-engine.yml do, and needs it", {
  skip_if_not(file.exists(reference_workflow), "workflow not present")
  job <- read_reference_workflow()$jobs[[1]]
  engine <- yaml::read_yaml(here::here(".github", "workflows", "inla-engine.yml"))$jobs[[1]]
  expect_identical(job$env$INLA_VERSION, engine$env$INLA_VERSION)
  expect_identical(job$env$INLA_REPO, engine$env$INLA_REPO)
  steps <- job$steps
  lines_of <- function(step, pattern) trimws(grep(pattern, strsplit(step$run %||% "", "\n")[[1]], value = TRUE))
  at <- c(
    sysreqs = first_step(steps, "apt-get install"),
    rdeps = first_step(steps, "install.packages\\(c\\(\"fmesher\""),
    inla = first_step(steps, "INLA_%s.tar.gz"),
    run = first_step(steps, "scripts/engine_reference.R")
  )
  expect_false(anyNA(at), info = paste(names(at)[is.na(at)], collapse = ", "))
  expect_identical(order(at), seq_along(at))
  e_steps <- engine$steps
  expect_identical(
    lines_of(steps[[at[["sysreqs"]]]], "apt-get"),
    lines_of(e_steps[[first_step(e_steps, "apt-get install")]], "apt-get")
  )
  pin <- "INLA_%s.tar.gz|packageVersion\\(\"INLA\"\\)"
  expect_identical(lines_of(steps[[at[["inla"]]]], pin), lines_of(e_steps[[first_step(e_steps, "INLA_%s.tar.gz")]], pin))
  # The reference cannot run without INLA, so its install is blocking here,
  # unlike fit.yml's while no league uses the engine.
  expect_false(any(vapply(steps[at[c("sysreqs", "rdeps", "inla")]], function(s) isTRUE(s[["continue-on-error"]]), logical(1))))
})

test_that("the reference fit builds CmdStan into the cache the other Stan workflows share", {
  skip_if_not(file.exists(reference_workflow), "workflow not present")
  job <- read_reference_workflow()$jobs[[1]]
  fit <- yaml::read_yaml(here::here(".github", "workflows", "fit.yml"))$jobs$fit
  expect_identical(job$env$CMDSTAN_VERSION, fit$env$CMDSTAN_VERSION)
  steps <- job$steps
  cache <- steps[[first_step(steps, "actions/cache@")]]
  expect_identical(cache$with$path, "~/.cmdstan")
  expect_identical(cache$with$key, "cmdstan-${{ env.CMDSTAN_VERSION }}-${{ steps.image.outputs.id }}")
  expect_lt(first_step(steps, "/etc/os-release"), first_step(steps, "actions/cache@"))
  expect_lt(first_step(steps, "actions/cache@"), first_step(steps, "scripts/engine_reference.R"))
})
