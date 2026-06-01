test_that("placement_kill_switched reflects the sentinel file", {
  root <- withr::local_tempdir()
  expect_false(placement_kill_switched(root))
  fs::file_create(fs::path(root, "AUTO_PLACE_DISABLED"))
  expect_true(placement_kill_switched(root))
})

test_that("acquire_auto_place_lock blocks a live lock and reclaims a dead one", {
  root <- withr::local_tempdir()
  expect_true(acquire_auto_place_lock(root)) # fresh acquire
  expect_true(fs::file_exists(fs::path(root, ".auto_place.lock")))

  expect_false(acquire_auto_place_lock(root)) # held by this (live) PID

  writeLines("999999", fs::path(root, ".auto_place.lock")) # simulate dead holder
  expect_true(acquire_auto_place_lock(root)) # stale lock reclaimed

  release_auto_place_lock(root)
  expect_false(fs::file_exists(fs::path(root, ".auto_place.lock")))
})
