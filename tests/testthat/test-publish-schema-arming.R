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
