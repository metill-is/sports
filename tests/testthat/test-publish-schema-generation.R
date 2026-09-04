# The per-sport schema directories under config/publish-schemas/ are GENERATED
# from config/publish-schemas/_base/ + config/publish-schemas/_delta/<sport>/ by
# tools/gen-publish-schemas.R.
#
# WHY generate rather than hand-write three copies: the three sports share a
# contract that is ~90% identical, and the only two ways to express that by hand
# are to duplicate it (three copies drift, and the drift is invisible until the
# platform 404s) or to promote a file to the sport-agnostic root of
# config/publish-schemas/ -- which silently becomes the fallback for EVERY
# sport, world_cup included. The generator makes the shared part literally one
# file and each sport's divergence an explicit, reviewable RFC-7386 patch.
#
# The invariant this file pins is REGENERABILITY, not byte-equality with any
# hand-written predecessor: `Rscript tools/gen-publish-schemas.R` must reproduce
# exactly what is committed, or the committed tree is no longer the rendering of
# its source and a future render silently reverts someone's hand edit.

.gen_env <- function() {
  env <- new.env(parent = globalenv())
  sys.source(
    testthat::test_path("..", "..", "tools", "gen-publish-schemas.R"),
    envir = env
  )
  env
}

.schema_src <- function() testthat::test_path("..", "..", "config", "publish-schemas")

test_that("the generator reproduces the committed per-sport schemas byte-for-byte", {
  env <- .gen_env()
  tmp <- withr::local_tempdir()
  written <- env$gen_publish_schemas(source_dir = .schema_src(), out_dir = tmp)
  expect_gt(length(written), 0L)

  for (sport in env$SCHEMA_ARMED_SPORTS) {
    committed_dir <- file.path(.schema_src(), sport)
    committed <- list.files(committed_dir, pattern = "[.]schema[.]json$")
    generated <- list.files(file.path(tmp, sport), pattern = "[.]schema[.]json$")
    expect_setequal(generated, committed)
    for (f in committed) {
      a <- readBin(file.path(tmp, sport, f), "raw", n = 1e7)
      b <- readBin(file.path(committed_dir, f), "raw", n = 1e7)
      expect_identical(a, b, info = file.path(sport, f))
    }
  }
})

test_that("a delta file's EXISTENCE is the per-sport surface manifest", {
  # There is no separate manifest to drift: `_delta/<sport>/<name>.json` being
  # present is what declares that <sport> emits <name>.json.
  env <- .gen_env()
  for (sport in env$SCHEMA_ARMED_SPORTS) {
    deltas <- sub("[.]json$", "", list.files(
      file.path(.schema_src(), "_delta", sport),
      pattern = "[.]json$"
    ))
    rendered <- sub("[.]schema[.]json$", "", list.files(
      file.path(.schema_src(), sport),
      pattern = "[.]schema[.]json$"
    ))
    expect_setequal(deltas, rendered)
  }
})

test_that("no sport-agnostic schema sits at the config/publish-schemas root", {
  # A *.schema.json there becomes the fallback for EVERY sport on BOTH sides of
  # the rsync (R/validate-publish.R::.resolve_schema_path and
  # metill-platform/scripts/validate_publish.py::resolve_schema_path each try
  # <root>/<sport>/<name> then <root>/<name>), which would arm a contract on
  # world_cup that nobody wrote for it.
  expect_length(
    list.files(.schema_src(), pattern = "[.]schema[.]json$"),
    0L
  )
})

test_that("the generator's source directories resolve as no sport", {
  # _base/_delta/_draft ride the rsync to metill-platform as
  # data/ithrottir-schemas/. They are inert because no publish JSON can ever
  # have one of them as its first path segment, and neither resolver looks
  # anywhere else.
  for (d in c("_base", "_delta", "_draft")) {
    expect_null(.resolve_schema_path(.schema_src(), d, "meta.json"))
  }
})

test_that("config/publish-schemas is pure ASCII", {
  # jsonlite::toJSON() renders a UTF-8 em-dash as the literal seven-character
  # string <U+2014> even when Encoding() is already "UTF-8", so a single
  # non-ASCII character in a _base description would be silently rewritten into
  # all three rendered sports at once. The generator aborts on it; this asserts
  # the committed tree is clean.
  files <- list.files(.schema_src(), recursive = TRUE, full.names = TRUE)
  offenders <- character()
  for (f in files) {
    bytes <- as.integer(readBin(f, "raw", n = file.size(f)))
    bad <- bytes[!(bytes == 9L | bytes == 10L | (bytes >= 32L & bytes <= 126L))]
    if (length(bad) > 0L) offenders <- c(offenders, basename(f))
  }
  expect_equal(offenders, character())
})

# ---- The meta v2 contract in _base (WS10 task 8, handed to WS11) -----------
#
# WS10 shipped meta.json v2 -- units, points, n_rounds, n_rounds_source,
# season_scope, postseason, qualify, relegation -- plus final_positions.basis
# and the p_top_of_table / p_qualify summary fields. None of them was typed
# anywhere, so a publisher emitting `"n_rounds": "twenty-two"` or
# `"season_scope": "playoffs"` would have passed validation on both sides of
# the rsync.
#
# They are typed in `_base` but deliberately NOT added to any `required` array
# there. The live data/publish/football tree was last published 2026-09-02,
# BEFORE WS10 landed, so every one of its nine meta.json files is v1.
# .validate_or_abort() validates the publishing sport's WHOLE subtree, not just
# the cell it wrote, so a `required` v2 key would make football's very next
# publish abort on its own not-yet-republished siblings -- male's publish
# failing on the four female cells and vice versa. The bb/hb deltas DO require
# them (those cells are new and emit the full v2 contract from their first
# publish); football's follow up when its tree is next republished.

.v2_meta <- function() list(
  sport = "football", sex = "male", league = "Besta deild", division = "BD",
  is_cup = FALSE, season = 2026L, generated_at = "2026-09-04T12:00:00+0000",
  fit_date = "2026-09-04", round = 21L, n_draws = 4000L,
  n_rounds = 22L, n_rounds_source = "config",
  units = list(strength = "log_goals", home_advantage = "goal_multiplier",
               diff_bin_width = 1L),
  points = list(win = 3L, draw = 1L, loss = 0L),
  season_scope = "full_season", postseason = NULL,
  qualify = list(slots = 6L, label_is = "Efri hluti"),
  relegation = list(slots = NULL)
)

.validate_meta <- function(meta, env = parent.frame()) {
  tmp <- withr::local_tempdir(.local_envir = env)
  cell <- file.path(tmp, "football", "iceland", "karla-bd")
  dir.create(cell, recursive = TRUE)
  jsonlite::write_json(
    meta, file.path(cell, "meta.json"),
    auto_unbox = TRUE, null = "null"
  )
  validate_publish_dir(tmp, schema_dir = .schema_src())
}

test_that("meta.json v2 keys are typed, not merely tolerated", {
  expect_true(.validate_meta(.v2_meta())$ok)

  bad_types <- list(
    n_rounds = "twenty-two",
    n_rounds_source = "vibes",
    season_scope = "playoffs",
    units = list(strength = "log_goals", home_advantage = "goal_multiplier"),
    points = list(win = 3L, draw = 1L)
  )
  for (nm in names(bad_types)) {
    meta <- .v2_meta()
    meta[[nm]] <- bad_types[[nm]]
    expect_false(.validate_meta(meta)$ok, info = nm)
  }
})

test_that("meta.n_rounds is nullable for a cup and n_rounds_source has four values", {
  # Football's two bikar cells publish n_rounds = null / not_applicable; a
  # two-value {schedule, config} enum -- which WS11's own Consumes bullet
  # asked for -- would reject them, and `none` as well.
  cup <- .v2_meta()
  cup$division <- "CUP"
  cup$is_cup <- TRUE
  cup$n_rounds <- NULL
  cup$n_rounds_source <- "not_applicable"
  cup$qualify <- NULL
  expect_true(.validate_meta(cup)$ok)

  for (src in c("config", "schedule", "none", "not_applicable")) {
    meta <- .v2_meta()
    meta$n_rounds_source <- src
    expect_true(.validate_meta(meta)$ok, info = src)
  }
})

test_that("final_positions basis and the placement summary fields are typed", {
  fp <- function(basis, summary) list(
    generated_at = "2026-09-04T12:00:00+0000", season = 2026L, n_teams = 12L,
    basis = basis, records = list(), summary = list(summary)
  )
  ok_row <- list(
    team = "KR", p_top_six = 0.5, p_winner = 0.1, p_relegation = 0.05,
    p_top_of_table = 0.5, p_qualify = 0.5
  )
  write_fp <- function(obj, env = parent.frame()) {
    tmp <- withr::local_tempdir(.local_envir = env)
    cell <- file.path(tmp, "football", "iceland", "karla-bd")
    dir.create(cell, recursive = TRUE)
    jsonlite::write_json(
      obj, file.path(cell, "final_positions.json"),
      auto_unbox = TRUE, null = "null"
    )
    validate_publish_dir(tmp, schema_dir = .schema_src())
  }
  expect_true(write_fp(fp("final_table", ok_row))$ok)
  expect_true(write_fp(fp("regular_season_table", ok_row))$ok)
  expect_false(write_fp(fp("guesswork", ok_row))$ok)

  bad <- ok_row
  bad$p_top_of_table <- 1.4
  expect_false(write_fp(fp("final_table", bad))$ok)
})
