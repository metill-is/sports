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

test_that("auto_place_decide resolves the action precedence", {
  base <- list(kill = FALSE, locked = FALSE, sync = TRUE, pending = 3L, room = 5000)
  d <- function(o = list()) {
    a <- utils::modifyList(base, o)
    auto_place_decide(a$kill, a$locked, a$sync, a$pending, a$room)
  }
  expect_equal(d(list(kill = TRUE)), "disabled")
  expect_equal(d(list(locked = TRUE)), "locked")
  expect_equal(d(list(sync = FALSE)), "sync_failed")
  expect_equal(d(list(pending = 0L)), "nothing_pending")
  expect_equal(d(list(room = 0)), "daily_cap_reached")
  expect_equal(d(), "place")
})

seed_pending_rec <- function(root) {
  recs <- tibble::tibble(
    run_id = as.POSIXct("2026-06-01 10:00:00", tz = "UTC"),
    sex = "male",
    match_date = as.Date("2026-06-02"),
    home_team = "A", away_team = "B",
    market = "moneyline", outcome = "home", line = NA_real_,
    p = 0.6, odds = 2.1, ev = 0.26, kelly = 0.02, bet_amount = 1500,
    sport = "football", country = "iceland"
  )
  write_table(recs, "recommendations", root = root)
}

test_that("run_auto_place records 'placed' when a pending bet is placed", {
  root <- withr::local_tempdir()
  seed_pending_rec(root)
  fake_place <- function(...) tibble::tibble(status = "placed")
  run_auto_place(
    root = root, now = as.POSIXct("2026-06-01 12:00:00", tz = "UTC"),
    sync_fn = function(...) TRUE, place_fn = fake_place,
    bankroll_fn = function() {
      list(
        daily_budget_frac = 0.05, current_pool = 1e5,
        daily_budget_min_isk = 1000
      )
    }
  )
  expect_equal(read_placement_status(root)$status, "placed")
})

test_that("run_auto_place short-circuits on the kill switch", {
  root <- withr::local_tempdir()
  seed_pending_rec(root)
  fs::file_create(fs::path(root, "AUTO_PLACE_DISABLED"))
  called <- FALSE
  run_auto_place(
    root = root, sync_fn = function(...) TRUE,
    place_fn = function(...) {
      called <<- TRUE
      tibble::tibble(status = "placed")
    },
    bankroll_fn = function() {
      list(
        daily_budget_frac = 0.05, current_pool = 1e5,
        daily_budget_min_isk = 1000
      )
    }
  )
  expect_false(called)
  expect_equal(read_placement_status(root)$status, "disabled")
})

test_that("run_auto_place records 'nothing_pending' with no recs", {
  root <- withr::local_tempdir()
  run_auto_place(
    root = root, sync_fn = function(...) TRUE,
    place_fn = function(...) stop("should not be called"),
    bankroll_fn = function() {
      list(
        daily_budget_frac = 0.05, current_pool = 1e5,
        daily_budget_min_isk = 1000
      )
    }
  )
  expect_equal(read_placement_status(root)$status, "nothing_pending")
})
