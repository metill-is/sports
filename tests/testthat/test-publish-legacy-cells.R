# The un-suffixed data/publish/{basketball,handball}/iceland/{karla,kvenna}/
# cells were produced on a laptop in June 2026 by the retired per-sport 2DT
# publishers, never by CI. They are at the pre-division path shape (no
# {sex}-{slug} segment) and their meta.json carries neither `division` nor
# `is_cup`, both of which the generated schema requires.
#
# They therefore had to go BEFORE the schemas were armed, not after. Arming
# with them on disk fails closed on both sides of the rsync: R-side
# .validate_or_abort(sport = "basketball") aborts that cell's publish, and
# platform-side validate_publish.py exits non-zero, which stops
# pull-sports-data.yml before its commit and freezes fly.metill.is on the
# last-known-good payload.

test_that("no un-suffixed bb/hb publish cell survives", {
  for (sport in c("basketball", "handball")) {
    for (d in c("karla", "kvenna")) {
      expect_false(
        dir.exists(here::here("data", "publish", sport, "iceland", d)),
        info = paste(sport, d)
      )
    }
  }
})

test_that("every bb/hb publish cell carries a division slug", {
  # So the old shape cannot come back by any route: a publisher regression, a
  # stale checkout, or a hand-copied directory all fail here. Collected across
  # both sports into one vector rather than looped with an early `next`, so
  # this block still asserts something once the directories are gone -- an
  # expectation-free run registers as a SKIP, and a guard that can skip itself
  # into silence is the shape of the problem it exists to catch.
  cells <- character()
  for (sport in c("basketball", "handball")) {
    dir <- here::here("data", "publish", sport, "iceland")
    if (dir.exists(dir)) {
      cells <- c(cells, list.dirs(dir, recursive = FALSE, full.names = FALSE))
    }
  }
  expect_true(
    all(grepl("^(karla|kvenna)-[a-z0-9]+$", cells)),
    info = paste(cells, collapse = ", ")
  )
})

test_that("the publish tree stays well clear of the platform's rsync floor", {
  # pull-sports-data.yml mirrors data/publish -> data/ithrottir with
  # --delete, so it refuses to rsync when the upstream tree holds fewer than
  # 50 JSONs -- a half-fetched partial clone would otherwise wipe the site's
  # data and commit the deletion as though every league had been retired.
  # Deleting the 32 stale cells took the tree from 134 to 102, which clears
  # that floor by 52. Any FUTURE deletion must be checked against this number
  # rather than assumed safe.
  n <- length(list.files(
    here::here("data", "publish"),
    pattern = "[.]json$", recursive = TRUE
  ))
  expect_gt(n, 50L)
})
