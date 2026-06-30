# tests/testthat/test-wc-workflow-staging.R
#
# Self-enforcing guard, in the same spirit as test-placer-ci-isolation.R and
# test-skill-conventions.R.
#
# The World Cup Forecast workflow (.github/workflows/world-cup.yml) commits its
# outputs by staging an explicit set of paths in a `git add ... \` block before
# `git commit`. If the WC pipeline grows a NEW tracked output that the `git add`
# block does not cover, that file is left as an *unstaged* change after the
# commit -- and the subsequent `git rebase -X theirs FETCH_HEAD` aborts with
# "cannot rebase: You have unstaged changes", failing the whole ~50-min run with
# nothing published.
#
# This is exactly what broke run 28437698745 on 2026-06-30: Phase 2 knockout
# conditioning added `data/wc/shootouts.csv` (rewritten every run by
# scripts/wc/ingest.R via wc_ingest_shootouts()), but the workflow's leaf-list
# `git add` predated it and never staged it.
#
# The fix is to stage by OWNED SUBTREE (e.g. `data/wc/`) rather than by
# enumerated leaf, so any future WC output under the world-cup-exclusive roots
# is committed automatically. This test pins that contract: every currently
# tracked file under the WC-owned roots must be covered by some `git add`
# pathspec in the workflow.

test_that("world-cup workflow stages every tracked WC output path", {
  workflow <- here::here(".github", "workflows", "world-cup.yml")
  skip_if_not(file.exists(workflow), "world-cup.yml not present")
  skip_if(Sys.which("git") == "", "git not available")

  lines <- readLines(workflow, warn = FALSE)

  # Pull the pathspecs out of the step's `git add ... \` line-continuation block.
  add_start <- grep("^\\s*git add\\b", lines)
  skip_if(length(add_start) == 0L, "no `git add` step in world-cup.yml")
  add_start <- add_start[[1L]]

  block <- lines[[add_start]]
  i <- add_start
  while (i < length(lines) && grepl("\\\\\\s*$", lines[[i]])) {
    i <- i + 1L
    block <- c(block, lines[[i]])
  }
  block <- sub("git add", " ", paste(block, collapse = " "), fixed = TRUE)
  specs <- unlist(strsplit(block, "\\s+"))
  specs <- sub("\\\\$", "", specs) # strip trailing backslashes
  specs <- sub("/$", "", specs) # normalise trailing slashes
  specs <- specs[nzchar(specs)]
  specs <- specs[startsWith(specs, "data/")]
  skip_if(length(specs) == 0L, "no data/ pathspecs found in `git add` block")

  covered <- function(target) {
    any(target == specs | startsWith(target, paste0(specs, "/")))
  }

  repo <- here::here()
  wc_roots <- c(
    "data/wc",
    "data/publish/world_cup",
    "data/facts/results/sport=football/country=world",
    "data/facts/schedules/sport=football/country=world"
  )
  tracked <- suppressWarnings(system2(
    "git",
    c("-C", shQuote(repo), "ls-files", "--", shQuote(wc_roots)),
    stdout = TRUE, stderr = FALSE
  ))
  skip_if(length(tracked) == 0L, "no tracked WC outputs (shallow/edge checkout)")

  uncovered <- tracked[!vapply(tracked, covered, logical(1L))]
  expect(
    length(uncovered) == 0L,
    sprintf(
      paste0(
        "world-cup.yml `git add` does not cover %d tracked WC output(s):\n  %s\n",
        "Stage by owned subtree (e.g. `data/wc/`) so new outputs are committed ",
        "automatically -- a leaf that escapes the stage wedges the rebase/push ",
        "(see run 28437698745, 2026-06-30)."
      ),
      length(uncovered), paste(uncovered, collapse = "\n  ")
    )
  )

  # Named regression anchor: the file whose omission caused the 2026-06-30 break.
  expect_true(covered("data/wc/shootouts.csv"))
})
