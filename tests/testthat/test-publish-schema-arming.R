# Plan B, WS11 T2 (spec section 11). Two coupled defects in the arming path:
#
# 1. .validate_or_abort() gates per-sport -- it returns early unless
#    config/publish-schemas/<sport>/ exists -- but then validates the WHOLE
#    publish tree. So merely CREATING config/publish-schemas/basketball/ arms
#    basketball validation inside FOOTBALL's publish call, and basketball's
#    non-conforming JSON aborts football. scripts/05_publish.R calls
#    publish_one() bare in a loop with no tryCatch, so the run dies before the
#    commit step and football stops publishing.
#
# 2. The obvious fix -- passing the sport subtree as `dir` -- FAILS OPEN.
#    validate_publish_dir() derives the sport from the first path segment
#    (R/validate-publish.R:48, `sport <- rel_parts[1]`). Given a subtree that
#    segment is "iceland", no schema resolves, every file lands in `unmatched`,
#    and ok = TRUE with nothing actually checked. The explicit `sport`
#    argument IS the fix, not the subtree.

.arm_tree <- function() {
  root <- withr::local_tempdir(.local_envir = parent.frame())
  fb <- file.path(root, "publish", "football", "iceland", "karla-bd")
  bb <- file.path(root, "publish", "basketball", "iceland", "karla-bd")
  dir.create(fb, recursive = TRUE); dir.create(bb, recursive = TRUE)
  # Football's meta conforms; basketball's is the stale pre-v2 shape with no
  # `division` and no `is_cup` -- exactly what is on disk today.
  writeLines(jsonlite::toJSON(list(
    sport = "football", sex = "male", league = "Besta deild", division = "BD",
    is_cup = FALSE, season = 2026L, generated_at = "2026-09-04T00:00:00+0000",
    fit_date = "2026-09-04", round = 21L, n_draws = 4000L
  ), auto_unbox = TRUE), file.path(fb, "meta.json"))
  writeLines(jsonlite::toJSON(list(
    sport = "basketball", sex = "male", league = "Bonusdeild",
    season = 2026L, generated_at = "2026-06-23T00:00:00+0000",
    fit_date = "2026-06-23", round = 22L, n_draws = 4000L
  ), auto_unbox = TRUE), file.path(bb, "meta.json"))
  file.path(root, "publish")
}

test_that("validating a subtree without an explicit sport FAILS OPEN", {
  # Documents the trap so nobody 'simplifies' the fix back into it.
  out <- .arm_tree()
  res <- validate_publish_dir(
    file.path(out, "football"),
    schema_dir = here::here("config", "publish-schemas")
  )
  expect_true(res$ok)                 # green...
  expect_gt(length(res$unmatched), 0) # ...only because nothing matched
  expect_equal(res$n_passed, 0L)
})

test_that("an explicit sport validates that sport's subtree for real", {
  out <- .arm_tree()
  res <- validate_publish_dir(
    file.path(out, "football"),
    schema_dir = here::here("config", "publish-schemas"),
    sport = "football"
  )
  expect_true(res$ok)
  expect_gt(res$n_passed, 0L)         # actually checked something
  expect_length(res$unmatched, 0L)
})

test_that("arming one sport cannot abort another sport's publish", {
  # THE regression test for the hazard. Basketball's stale meta.json is
  # present and non-conforming; validating football must not see it.
  out <- .arm_tree()
  res <- validate_publish_dir(
    file.path(out, "football"),
    schema_dir = here::here("config", "publish-schemas"),
    sport = "football"
  )
  expect_true(res$ok)
  expect_false(any(grepl("basketball", unlist(res$errors))))
  expect_false(any(grepl("basketball", res$unmatched)))
})

# ---- WS11 T1: an overridable schema_dir ------------------------------------
#
# Every later arming task needs to point the publisher at a schema directory
# that is NOT config/publish-schemas/ -- `_draft/` while the bb/hb schemas are
# being authored, a tempdir in the negative tests below. Without the parameter
# the only way to exercise arming is to write into the real config tree, which
# arms it for the live pipeline at the same time.
#
# SC-7 fixes the signature as
#   publish_one(static, betting, key, sex, root, validate, end_date, schema_dir)
# -- `end_date` seventh (WS9 T1), `schema_dir` EIGHTH. Both are passed by name
# everywhere; nothing relies on position past `sex`.

test_that(".validate_or_abort accepts an explicit schema_dir", {
  out <- .arm_tree()
  sch <- withr::local_tempdir()
  file.copy(
    here::here("config", "publish-schemas", "football"), sch,
    recursive = TRUE
  )
  expect_no_error(.validate_or_abort(
    out,
    sport = "football", key = "football_iceland", sex = "male",
    schema_dir = sch
  ))
})

test_that("publish_one carries schema_dir as its eighth formal", {
  fmls <- names(formals(publish_one))
  expect_equal(
    fmls,
    c("static", "betting", "key", "sex", "root", "validate", "end_date", "schema_dir")
  )
})

# ---- WS11 T8: the missing-schema default is fail-CLOSED ---------------------
#
# Until now a sport with no config/publish-schemas/<sport>/ directory published
# with an informational "skipping validation" note and exited 0. That is the
# fail-open end of the contract, and it is the same shape as B4: a sport can be
# completely unchecked and the pipeline stays green. All three sports that go
# through publish_one() are now armed, so the default inverts.
#
# publish_world_cup() is NOT affected, re-verified by grep rather than trusted:
# R/wc-publish.R contains no reference to publish_one, .validate_or_abort or
# validate_publish_dir, and its only caller is scripts/wc/forecast.R. world_cup
# has no schema directory and is not getting one.

.sch_football_only <- function(env = parent.frame()) {
  sch <- withr::local_tempdir(.local_envir = env)
  file.copy(
    here::here("config", "publish-schemas", "football"), sch,
    recursive = TRUE
  )
  sch
}

test_that(".validate_or_abort aborts when a sport has no schema directory", {
  out <- .arm_tree()
  hb <- file.path(out, "handball", "iceland", "karla-od")
  dir.create(hb, recursive = TRUE)
  writeLines("{}", file.path(hb, "meta.json"))
  sch <- .sch_football_only()

  expect_error(
    .validate_or_abort(
      out,
      sport = "handball", key = "handball_iceland", sex = "male",
      schema_dir = sch
    ),
    "no schemas"
  )
  # ...and football, which IS armed there, still passes.
  expect_no_error(.validate_or_abort(
    out,
    sport = "football", key = "football_iceland", sex = "male",
    schema_dir = sch
  ))
})

test_that(".validate_or_abort aborts when the schema ROOT is missing", {
  # A missing schema root is a broken checkout, not a reason to publish
  # unchecked.
  out <- .arm_tree()
  expect_error(.validate_or_abort(
    out,
    sport = "football", key = "football_iceland", sex = "male",
    schema_dir = file.path(tempdir(), "definitely-not-a-schema-root")
  ))
})

test_that("a sport that published nothing is a warning, not an abort", {
  # The publisher just ran; an absent sport directory means it wrote nothing,
  # which is worth a line in the log but is not a contract breach. Aborting
  # here would turn every legitimate no-op into a red run.
  out <- .arm_tree()
  expect_no_error(suppressMessages(.validate_or_abort(
    out,
    sport = "handball", key = "handball_iceland", sex = "male",
    schema_dir = here::here("config", "publish-schemas")
  )))
})

test_that("publish_world_cup never reaches the validation path", {
  # Re-grepped rather than trusted: the inversion above would otherwise abort
  # world_cup, which has no schema directory by design.
  src <- readLines(
    testthat::test_path("..", "..", "R", "wc-publish.R"),
    warn = FALSE
  )
  for (fn in c("publish_one", ".validate_or_abort", "validate_publish_dir")) {
    expect_equal(grep(fn, src, fixed = TRUE), integer(), info = fn)
  }
  expect_null(.resolve_schema_path(
    here::here("config", "publish-schemas"), "world_cup", "meta.json"
  ))
})
