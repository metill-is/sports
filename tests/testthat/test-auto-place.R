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

test_that(".daily_room never goes negative", {
  expect_equal(.daily_room(daily_budget = 5000, placed_today = 2000), 3000)
  expect_equal(.daily_room(daily_budget = 5000, placed_today = 9000), 0)
})

test_that(".placed_today_stake sums only today's placed stakes", {
  now <- as.POSIXct("2026-06-01 12:00:00", tz = "UTC")
  led <- tibble::tibble(
    placed_at = as.POSIXct(c("2026-06-01 09:00:00", "2026-05-31 20:00:00"), tz = "UTC"),
    bet_amount = c(1500, 4000)
  )
  expect_equal(.placed_today_stake(led, now), 1500)
  expect_equal(.placed_today_stake(tibble::tibble(), now), 0)
})

test_that("placement status round-trips and missing reads as NULL", {
  root <- withr::local_tempdir()
  expect_null(read_placement_status(root))

  record_placement_status("placed",
    n_pending = 3L, n_placed = 2L,
    run_at = as.POSIXct("2026-06-01 12:00:00", tz = "UTC"), root = root
  )
  got <- read_placement_status(root)
  expect_equal(got$status, "placed")
  expect_equal(got$n_placed, 2L)
  expect_match(got$run_at, "^2026-06-01T12:00")
})
