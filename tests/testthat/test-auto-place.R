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
    match_date = Sys.Date() + 7L,
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

test_that("run_auto_place records 'failed:<reason>' and re-throws when placement errors", {
  root <- withr::local_tempdir()
  seed_pending_rec(root)
  expect_error(
    run_auto_place(
      root = root, now = as.POSIXct("2026-06-01 12:00:00", tz = "UTC"),
      sync_fn = function(...) TRUE,
      place_fn = function(...) stop("network timeout"),
      bankroll_fn = function() {
        list(
          daily_budget_frac = 0.05, current_pool = 1e5,
          daily_budget_min_isk = 1000
        )
      }
    )
  )
  expect_equal(read_placement_status(root)$status, "failed:network timeout")
})

test_that("run_auto_place records 'ev_rejected' when placement returns no placed rows", {
  root <- withr::local_tempdir()
  seed_pending_rec(root)
  run_auto_place(
    root = root, now = as.POSIXct("2026-06-01 12:00:00", tz = "UTC"),
    sync_fn = function(...) TRUE,
    place_fn = function(...) tibble::tibble(status = "rejected_p4"),
    bankroll_fn = function() {
      list(
        daily_budget_frac = 0.05, current_pool = 1e5,
        daily_budget_min_isk = 1000
      )
    }
  )
  expect_equal(read_placement_status(root)$status, "ev_rejected")
})

test_that("run_auto_place records 'locked' when a live lock is held", {
  root <- withr::local_tempdir()
  seed_pending_rec(root)
  writeLines(as.character(Sys.getpid()), fs::path(root, ".auto_place.lock"))
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
  expect_equal(read_placement_status(root)$status, "locked")
})

# --- sync_recs against real scratch git repos ---------------------------------
#
# The 2026-06-10 incident: sync_recs built git calls with an unquoted
# system2(), so the stash message "auto_place sync" split into message
# "auto_place" + pathspec "sync" -- the stash silently no-opped (exit 0),
# and `pull --rebase` then died on the dirty working tree. These tests run
# the real git binary so any future quoting/sequencing regression fails.
# Host git config is isolated (GIT_CONFIG_GLOBAL/GIT_CONFIG_NOSYSTEM):
# e.g. a global rebase.autostash=true would otherwise mask the quoting
# regression by making `pull --rebase` stash the dirty tree itself.

.tgit <- function(dir, ...) {
  args <- vapply(c("-C", dir, ...), shQuote, character(1))
  out <- suppressWarnings(system2("git", args, stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status")
  if (!is.null(status) && status != 0L) {
    stop("git ", paste(c(...), collapse = " "), " failed: ", paste(out, collapse = "\n"))
  }
  as.character(out)
}

# A working clone tracking a bare origin, one commit behind it, so
# `pull --rebase origin main` has real work to do.
.scratch_synced_repo <- function(env = parent.frame()) {
  empty_cfg <- withr::local_tempfile(.local_envir = env)
  file.create(empty_cfg)
  withr::local_envvar(
    c(GIT_CONFIG_GLOBAL = empty_cfg, GIT_CONFIG_NOSYSTEM = "1"),
    .local_envir = env
  )
  remote <- withr::local_tempdir(.local_envir = env)
  work <- withr::local_tempdir(.local_envir = env)
  .tgit(remote, "init", "--bare", "-b", "main")
  .tgit(work, "init", "-b", "main")
  .tgit(work, "config", "user.email", "test@example.invalid")
  .tgit(work, "config", "user.name", "sync_recs test")
  writeLines("v1", file.path(work, "remote_file.txt"))
  writeLines("v1", file.path(work, "local_file.txt"))
  .tgit(work, "add", ".")
  .tgit(work, "commit", "-m", "init")
  .tgit(work, "remote", "add", "origin", remote)
  .tgit(work, "push", "-u", "origin", "main")
  writeLines("v2", file.path(work, "remote_file.txt"))
  .tgit(work, "add", "remote_file.txt")
  .tgit(work, "commit", "-m", "remote change")
  .tgit(work, "push", "origin", "main")
  .tgit(work, "reset", "--hard", "HEAD~1")
  work
}

test_that("sync_recs fast-forwards a clean clone", {
  work <- .scratch_synced_repo()
  expect_true(sync_recs(work))
  expect_equal(readLines(file.path(work, "remote_file.txt")), "v2")
})

test_that("sync_recs stashes dirty non-ledger files through the pull (quoting regression)", {
  work <- .scratch_synced_repo()
  writeLines("local WIP", file.path(work, "local_file.txt"))
  expect_true(sync_recs(work))
  expect_equal(readLines(file.path(work, "remote_file.txt")), "v2")
  expect_equal(readLines(file.path(work, "local_file.txt")), "local WIP")
})

test_that("sync_recs commits uncommitted ledger rows before syncing (L1 rescue)", {
  work <- .scratch_synced_repo()
  ledger_dir <- file.path(work, "data", "decisions", "ledger")
  dir.create(ledger_dir, recursive = TRUE)
  writeLines("row1", file.path(ledger_dir, "part-0.parquet"))
  .tgit(work, "add", ".")
  .tgit(work, "commit", "-m", "seed ledger")
  writeLines(c("row1", "row2"), file.path(ledger_dir, "part-0.parquet"))

  expect_true(sync_recs(work))

  porcelain <- .tgit(work, "status", "--porcelain", "--", "data/decisions/ledger")
  expect_length(porcelain, 0L)
  subjects <- .tgit(work, "log", "--format=%s", "-3")
  expect_true(any(grepl("rescue uncommitted rows", subjects)))
  expect_equal(
    readLines(file.path(ledger_dir, "part-0.parquet")),
    c("row1", "row2")
  )
})

test_that("sync_recs leaves a pre-existing user stash alone (own-stash discipline)", {
  work <- .scratch_synced_repo()
  writeLines("precious", file.path(work, "wip.txt"))
  .tgit(work, "stash", "push", "-u", "-m", "user precious WIP")

  expect_true(sync_recs(work))

  stashes <- .tgit(work, "stash", "list")
  expect_length(stashes, 1L)
  expect_match(stashes, "user precious WIP")
  expect_false(file.exists(file.path(work, "wip.txt")))
  expect_equal(readLines(file.path(work, "remote_file.txt")), "v2")
})

test_that("sync_recs refuses to run with a feature branch checked out", {
  work <- .scratch_synced_repo()
  .tgit(work, "checkout", "-q", "-b", "feat/wip")
  sha_before <- .tgit(work, "rev-parse", "HEAD")

  expect_false(sync_recs(work))

  expect_equal(.tgit(work, "rev-parse", "HEAD"), sha_before)
  expect_equal(.tgit(work, "rev-parse", "--abbrev-ref", "HEAD"), "feat/wip")
})

test_that("sync_recs aborts a conflicted pull; money rows end up committed, never stashed", {
  work <- .scratch_synced_repo()
  .tgit(work, "pull", "--rebase", "origin", "main")
  ledger_dir <- file.path(work, "data", "decisions", "ledger")
  dir.create(ledger_dir, recursive = TRUE)
  writeLines("base", file.path(ledger_dir, "part-0.parquet"))
  .tgit(work, "add", ".")
  .tgit(work, "commit", "-m", "seed ledger")
  .tgit(work, "push", "origin", "main")
  writeLines("origin version", file.path(ledger_dir, "part-0.parquet"))
  .tgit(work, "add", ".")
  .tgit(work, "commit", "-m", "conflicting origin ledger change")
  .tgit(work, "push", "origin", "main")
  .tgit(work, "reset", "--hard", "HEAD~1")
  writeLines("local money row", file.path(ledger_dir, "part-0.parquet"))

  expect_false(sync_recs(work))

  # The rescue commit holds the money rows; nothing is stranded in a stash
  # (a rescue-after-stash mutant strands them in a kept, conflicted stash).
  expect_length(.tgit(work, "stash", "list"), 0L)
  expect_true(any(grepl("rescue uncommitted rows", .tgit(work, "log", "--format=%s", "-1"))))
  expect_equal(readLines(file.path(ledger_dir, "part-0.parquet")), "local money row")
  # The conflicted rebase was aborted, not left wedging the next cycle.
  expect_false(dir.exists(file.path(work, ".git", "rebase-merge")))
  expect_false(dir.exists(file.path(work, ".git", "rebase-apply")))
})
