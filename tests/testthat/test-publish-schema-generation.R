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
