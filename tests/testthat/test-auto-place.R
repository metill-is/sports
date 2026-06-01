test_that("placement_kill_switched reflects the sentinel file", {
  root <- withr::local_tempdir()
  expect_false(placement_kill_switched(root))
  fs::file_create(fs::path(root, "AUTO_PLACE_DISABLED"))
  expect_true(placement_kill_switched(root))
})
