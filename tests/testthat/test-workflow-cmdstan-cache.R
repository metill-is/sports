# tests/testthat/test-workflow-cmdstan-cache.R
#
# Self-enforcing guard, in the same spirit as test-workflow-push-retry.R and
# test-wc-workflow-staging.R.
#
# ~/.cmdstan is not just source. Installing CmdStan compiles main.o, libtbb,
# the SUNDIALS archives and a precompiled model header, and every Stan model
# compiled later is linked against those objects. The cache key used to end in
# the literal `ubuntu-latest`. When GitHub moved that label from Ubuntu 24.04
# to 26.04 on 2026-11-11, the objects built on 24.04 (GCC 13.3.0, cached
# 2026-04-27) were restored onto 26.04, where the models are compiled by GCC
# 15.2.0. Both 2DT executables then died at the start of sampling with
# glibc's "double free or corruption (out)" (SIGABRT, cmdstanr return code
# -6): four days without a handball or basketball fit (2026-11-12..15, runs
# 43237541505, 43436868442, 43596971307, 43718485883) and a red CI Tests on
# main.
#
# Proof (run 43858721937, same commit, three jobs): the restored cache on 26.04
# aborts; CmdStan 2.38.0 built on 26.04 samples cleanly; the restored cache on
# 24.04 samples cleanly. So neither the image nor the cache is broken alone --
# the pairing is.
#
# The key must therefore name the image and compiler the job is actually on.
# Two spellings that look right and are not:
#   - a literal label (`ubuntu-latest`, `ubuntu-24.04`): it describes what the
#     workflow asked for, and goes stale the day `runs-on` or the label moves;
#   - `${{ env.ImageOS }}`: the `env` context holds only variables the workflow
#     itself sets, so a runner-provided variable is the empty string there
#     (checked in run 43859642089) and every image would share one key again.
# This test pins the working spelling in all three workflows that share the
# cache.

workflow_dir <- here::here(".github", "workflows")

# The one key expression every CmdStan cache step must use. The three
# workflows share it on purpose: a PR run then restores what main built.
CMDSTAN_CACHE_KEY <-
  "cmdstan-${{ env.CMDSTAN_VERSION }}-${{ steps.image.outputs.id }}"

# Every (workflow, job) that caches ~/.cmdstan, with its parsed steps.
.cmdstan_cache_jobs <- function() {
  files <- list.files(workflow_dir, pattern = "\\.ya?ml$", full.names = TRUE)
  out <- list()
  for (f in files) {
    jobs <- yaml::read_yaml(f)$jobs
    for (job_name in names(jobs)) {
      job <- jobs[[job_name]]
      steps <- job$steps
      is_cache <- vapply(steps, function(s) {
        grepl("^actions/cache(/restore|/save)?@", s$uses %||% "") &&
          identical(s$with$path, "~/.cmdstan")
      }, logical(1))
      if (!any(is_cache)) next
      out[[paste0(basename(f), ":", job_name)]] <- list(
        steps = steps, cache_at = which(is_cache), env = job$env
      )
    }
  }
  out
}

test_that("the CmdStan cache is used by the workflows this test expects", {
  skip_if_not(dir.exists(workflow_dir), "workflow dir not present")
  # Guards the tests below against passing vacuously (a renamed path, a moved
  # step), and makes a new CmdStan workflow show up here for review.
  expect_setequal(
    names(.cmdstan_cache_jobs()),
    c("ci-tests.yml:test", "fit.yml:fit", "world-cup.yml:forecast")
  )
})

test_that("every CmdStan cache key carries the runner image, never a label", {
  skip_if_not(dir.exists(workflow_dir), "workflow dir not present")
  jobs <- .cmdstan_cache_jobs()
  skip_if(length(jobs) == 0L, "no workflow caches CmdStan")

  for (nm in names(jobs)) {
    job <- jobs[[nm]]
    for (i in job$cache_at) {
      key <- job$steps[[i]]$with$key
      expect_identical(
        key, CMDSTAN_CACHE_KEY,
        info = paste0(
          nm, ": the CmdStan cache key must be exactly `", CMDSTAN_CACHE_KEY,
          "`. A literal image label (`ubuntu-latest`) does not change when ",
          "the image behind it does, and `env.ImageOS` is empty in an ",
          "expression (the env context only holds variables the workflow ",
          "sets), so both restore one image's compiled CmdStan onto another."
        )
      )
    }
  }
})

test_that("the image id is computed from the runner before the cache step", {
  skip_if_not(dir.exists(workflow_dir), "workflow dir not present")
  jobs <- .cmdstan_cache_jobs()
  skip_if(length(jobs) == 0L, "no workflow caches CmdStan")

  for (nm in names(jobs)) {
    job <- jobs[[nm]]
    ids <- vapply(job$steps, function(s) s$id %||% "", character(1))
    image_at <- which(ids == "image")
    expect_length(image_at, 1L)
    if (length(image_at) != 1L) next
    expect_lt(
      image_at, min(job$cache_at),
      label = paste0(nm, ": image step position")
    )

    script <- job$steps[[image_at]]$run %||% ""
    # The id must come from the machine the job is on: the OS release, the
    # architecture and the compiler that will build the models.
    needles <- c(
      "/etc/os-release", "VERSION_ID", "uname -m", "g++ -dumpfullversion"
    )
    for (needle in needles) {
      expect_true(
        grepl(needle, script, fixed = TRUE),
        info = paste0(nm, ": the image step no longer reads `", needle, "`")
      )
    }
    expect_match(
      script, "echo \"id=[^\"]+\" >> \"\\$GITHUB_OUTPUT\"",
      info = paste0(nm, ": the image step must write `id=` to $GITHUB_OUTPUT")
    )
  }
})

test_that("workflows sharing the CmdStan cache pin one CmdStan version", {
  skip_if_not(dir.exists(workflow_dir), "workflow dir not present")
  jobs <- .cmdstan_cache_jobs()
  skip_if(length(jobs) == 0L, "no workflow caches CmdStan")

  versions <- vapply(
    jobs, function(j) j$env$CMDSTAN_VERSION %||% NA_character_, character(1)
  )
  expect_false(anyNA(versions))
  expect_length(unique(versions), 1L)
})
