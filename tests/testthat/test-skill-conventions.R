# tests/testthat/test-skill-conventions.R
#
# Mirrors test-placer-ci-isolation.R's pattern: a self-enforcing CLAUDE.md
# claim. The four skills under .claude/skills/ are intentionally
# model-invocable (CLAUDE.md "Skills" section), which means Claude will
# *execute* the commands they contain.  After the Plan 6 cutover the
# pre-migration four-repo layout (lengjan-bets/, lengjan-odds/,
# livesport-data/, sports/Sports/) is gone and the deprecated
# scripts/{fit,decide,publish}_all.R + scripts/backfill_ingest.R runners
# bypass the {targets} cache.  Skills referencing any of those would
# silently fail mid-run.
#
# This test scans every SKILL.md under .claude/skills/ and fails the build
# if any line contains a token from the forbidden list below.  Update the
# forbidden list when the canonical entrypoints change.

test_that("no .claude/skills/*/SKILL.md references deprecated paths or flags", {
  skills_dir <- here::here(".claude", "skills")
  if (!dir.exists(skills_dir)) {
    testthat::skip("no .claude/skills/ directory")
  }
  skill_files <- list.files(
    skills_dir,
    pattern = "^SKILL\\.md$",
    recursive = TRUE,
    full.names = TRUE
  )
  if (length(skill_files) == 0L) {
    testthat::skip("no SKILL.md files yet")
  }

  forbidden <- c(
    # Pre-Plan-6 multi-repo cd commands (paths no longer exist)
    "cd /Users/brynjolfurjonsson/sports/lengjan-bets",
    "cd /Users/brynjolfurjonsson/sports/lengjan-odds",
    "cd /Users/brynjolfurjonsson/sports/livesport-data",
    "cd /Users/brynjolfurjonsson/sports/Sports",
    # Pre-Plan-7 flags / entrypoints (run.R + targets removed; use scripts/0N_*.R)
    "--sync",
    "--due",
    "--stale",
    "Rscript run.R",
    "run.R --step",
    "--step data",
    "--step odds",
    "--step fit",
    "--step decide",
    "--step publish",
    "--step all",
    # Deprecated runners (deleted in Plan 7)
    "scripts/fit_all.R",
    "scripts/decide_all.R",
    "scripts/publish_all.R",
    "scripts/backfill_ingest.R",
    # Removed legacy scripts and ledger filename
    "R/bets/log_placed.R",
    "bets_log.csv"
  )

  failures <- character(0)
  for (f in skill_files) {
    contents <- readLines(f, warn = FALSE)
    for (token in forbidden) {
      hit <- grep(token, contents, fixed = TRUE)
      if (length(hit) > 0L) {
        rel <- sub(here::here(""), "", f, fixed = TRUE)
        failures <- c(failures, sprintf(
          "%s:%d references forbidden token %s",
          rel, hit[1L], shQuote(token)
        ))
      }
    }
  }

  if (length(failures) > 0L) {
    fail(paste(
      "Skill(s) reference deprecated paths or flags.",
      "Skills are model-invocable; references to legacy entrypoints will fail at runtime.",
      "See CLAUDE.md `Skills` section for the canonical entrypoints.",
      paste("  -", failures, collapse = "\n"),
      sep = "\n"
    ))
  }
  expect_true(TRUE)
})

test_that("/bet and /place-bets do not set context: fork in frontmatter", {
  skills_dir <- here::here(".claude", "skills")
  if (!dir.exists(skills_dir)) {
    testthat::skip("no .claude/skills/ directory")
  }
  presentational <- c("bet", "place-bets")

  failures <- character(0)
  for (name in presentational) {
    f <- file.path(skills_dir, name, "SKILL.md")
    if (!file.exists(f)) next
    contents <- readLines(f, warn = FALSE)
    fm_bounds <- which(contents == "---")
    if (length(fm_bounds) < 2L) next
    frontmatter <- contents[(fm_bounds[1L] + 1L):(fm_bounds[2L] - 1L)]
    if (any(grepl("^context:\\s*fork\\s*$", frontmatter))) {
      failures <- c(failures, sprintf(
        ".claude/skills/%s/SKILL.md sets `context: fork`",
        name
      ))
    }
  }

  if (length(failures) > 0L) {
    fail(paste(
      "Presentational skills must not set `context: fork`.",
      "`context: fork` runs the skill in a sub-session whose final message",
      "is captured as <local-command-stdout> for the next parent turn but",
      "never rendered as a visible chat bubble. Skills that exist to *show*",
      "tables (`/bet`) or to gate placement on a user-visible preview",
      "(`/place-bets`) silently produce nothing in the UI under fork.",
      paste("  -", failures, collapse = "\n"),
      sep = "\n"
    ))
  }
  expect_true(TRUE)
})
