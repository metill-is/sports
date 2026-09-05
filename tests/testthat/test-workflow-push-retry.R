# tests/testthat/test-workflow-push-retry.R
#
# Self-enforcing guard, in the same spirit as test-placer-ci-isolation.R and
# test-wc-workflow-staging.R.
#
# Eight workflows commit generated data to main. Each declares its OWN
# `concurrency` group, which serialises a workflow against itself but does
# nothing across workflows -- so several are routinely in flight at once (on
# 2026-08-25 a stan fit pushed at 08:39, an odds scrape at 08:40 and a
# decide+publish at 08:44).
#
# The historic `git pull --rebase origin main && git push` had no retry and
# lost two ways:
#
#   1. Ref-lock race -- rebase succeeds, origin advances before the push:
#        ! [remote rejected] main -> main (cannot lock ref 'refs/heads/main')
#      This killed a ~2 h stan fit (run 32698702043, 2026-08-24).
#   2. Content conflict -- two writers regenerate the same paths; 97 conflicts
#      in run 32827816691 (2026-08-25), same signature 2026-08-10 / 2026-08-18.
#
# Both discard a whole run's output even though the work itself was fine.
# .github/scripts/push-with-retry.sh centralises the fetch/rebase/push retry.
# This test pins the contract so a new workflow (or a revert) cannot quietly
# reintroduce a bare, unretried push.

workflow_dir <- here::here(".github", "workflows")

test_that("every workflow that commits to main pushes via push-with-retry.sh", {
  skip_if_not(dir.exists(workflow_dir), "workflow dir not present")
  files <- list.files(workflow_dir, pattern = "\\.ya?ml$", full.names = TRUE)
  skip_if(length(files) == 0L, "no workflows found")

  for (f in files) {
    lines <- readLines(f, warn = FALSE)
    # Only workflows that actually create a commit are in scope.
    if (!any(grepl("^\\s*git commit\\b", lines))) next

    expect_true(
      any(grepl("push-with-retry\\.sh", lines, fixed = FALSE)),
      info = paste0(
        basename(f), " creates a commit but never calls ",
        ".github/scripts/push-with-retry.sh -- an unretried push will lose ",
        "the run to a concurrent sibling push."
      )
    )
  }
})

test_that("no workflow reintroduces a bare pull --rebase / push pair", {
  skip_if_not(dir.exists(workflow_dir), "workflow dir not present")
  files <- list.files(workflow_dir, pattern = "\\.ya?ml$", full.names = TRUE)
  skip_if(length(files) == 0L, "no workflows found")

  for (f in files) {
    lines <- readLines(f, warn = FALSE)

    expect_false(
      any(grepl("git pull --rebase", lines, fixed = TRUE)),
      info = paste0(
        basename(f), " uses `git pull --rebase`; use ",
        ".github/scripts/push-with-retry.sh instead so the push is retried."
      )
    )
    # A bare `git push` as its own statement. The helper script is the only
    # sanctioned pusher; `git push` inside the script itself lives outside
    # .github/workflows/ and so is not matched here.
    expect_false(
      any(grepl("^\\s*git push\\s*$", lines)),
      info = paste0(
        basename(f), " calls `git push` directly; route it through ",
        ".github/scripts/push-with-retry.sh."
      )
    )
  }
})

test_that("push-with-retry.sh exists, is executable, and is valid bash", {
  script <- here::here(".github", "scripts", "push-with-retry.sh")
  expect_true(file.exists(script))
  # Executable bit matters: the workflows invoke it directly, not via `bash`.
  expect_true(file.access(script, mode = 1L) == 0L,
              info = "push-with-retry.sh is not executable (chmod +x)")
  skip_if(Sys.which("bash") == "", "bash not available")
  expect_equal(
    system2("bash", c("-n", shQuote(script)), stdout = FALSE, stderr = FALSE),
    0L
  )
})

test_that("--prefer-ours is confined to workflows that fully regenerate output", {
  # `-X theirs` supersedes a sibling's version of a conflicting file. That is
  # only safe where every path the job writes is recomputed from the freshest
  # inputs. Three regenerate data/publish/ (and the WC tree) wholesale, and
  # the fit regenerates each fit_date partition it writes; the scrapers write
  # disjoint paths and must fail loudly on a conflict instead, because a
  # conflict there means something unexpected.
  # fit.yml joined the list on 2026-09-05: two fit runs in the same
  # concurrency group can both fit the same cell on the same day (the queued
  # run's checkout predates the running run's push by seconds), and each
  # writes the same fit_date=<today> partition under data/beliefs/. Run
  # 33960811873 fitted handball's first two cells of the season, then lost the
  # whole commit to a binary add/add conflict on football's parquets. Every
  # path fit.yml stages is regenerated from the freshest data by the run that
  # writes it, so the later fit is authoritative.
  allowed <- c("decide-publish.yml", "republish.yml", "world-cup.yml", "fit.yml")
  skip_if_not(dir.exists(workflow_dir), "workflow dir not present")
  files <- list.files(workflow_dir, pattern = "\\.ya?ml$", full.names = TRUE)

  for (f in files) {
    lines <- readLines(f, warn = FALSE)
    if (!any(grepl("--prefer-ours", lines, fixed = TRUE))) next
    expect_true(
      basename(f) %in% allowed,
      info = paste0(
        basename(f), " passes --prefer-ours. Only ",
        paste(allowed, collapse = ", "), " fully regenerate their output; ",
        "adding a new one needs a deliberate review of whether its paths are ",
        "recomputed or accumulated (see the caveat in push-with-retry.sh)."
      )
    )
  }
})

test_that("fit.yml prefers its own side on a same-partition conflict", {
  # The positive half of the allowlist above: a queued fit that collides with
  # the previous run's push must keep its fit, not discard three hours of
  # sampling. Located by step and scanned forward, like the always() test.
  yml <- readLines(
    testthat::test_path("..", "..", ".github", "workflows", "fit.yml"),
    warn = FALSE
  )
  idx <- grep("^\\s*- name: Commit if beliefs changed\\s*$", yml)
  expect_length(idx, 1L)
  step <- yml[idx:length(yml)]
  expect_true(any(grepl("push-with-retry.sh --prefer-ours", step, fixed = TRUE)))
})
