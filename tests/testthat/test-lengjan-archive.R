# Reader for a local copy of the raw Lengjan odds archive (format version 1;
# see docs/runbooks/odds-archive.md). Archives are built per test by
# `.local_archive()` (helper-lengjan-archive.R) from the committed API
# fixtures. No network; nothing is written under data/.

test_that("the archive directory comes from the argument, the option, then the env var", {
  withr::local_options(sports.lengjan_archive_dir = NULL)
  withr::local_envvar(LENGJAN_ARCHIVE_DIR = NA)
  expect_error(.lengjan_archive_dir(), "sports.lengjan_archive_dir|LENGJAN_ARCHIVE_DIR")
  withr::local_envvar(LENGJAN_ARCHIVE_DIR = "/env")
  expect_equal(.lengjan_archive_dir(), "/env")
  withr::local_options(sports.lengjan_archive_dir = "/opt")
  expect_equal(.lengjan_archive_dir(), "/opt")
  expect_equal(.lengjan_archive_dir("/arg"), "/arg")
})

test_that("lengjan_archive_index returns one row per capture, sorted", {
  dir <- .local_archive(list(
    list(ts = "2026-09-23T12:30:00Z", program = "current-program.json", markets = "markets.json"),
    list(ts = "2026-09-23T12:00:00Z", program = "current-program.json", markets = character())
  ))
  ix <- lengjan_archive_index(dir)
  expect_equal(format(ix$ts, "%H:%M", tz = "UTC"), c("12:00", "12:30"))
  expect_s3_class(ix$ts, "POSIXct")
  expect_equal(lengths(ix$markets_files), c(0L, 1L))
  expect_equal(ix$status, c("ok", "ok"))
})

test_that("a truncated manifest line is skipped with a warning", { # Review Focus 2
  dir <- .local_archive(list(list(ts = "2026-09-23T12:00:00Z", program = "current-program.json", markets = character())))
  cat('{"v": 1, "ts": "2026-09-23T12:3', file = file.path(dir, "manifest", "2026-09.jsonl"), append = TRUE)
  expect_warning(ix <- lengjan_archive_index(dir), "manifest")
  expect_equal(nrow(ix), 1L)
})

test_that("an archive with no manifest gives a zero-row index", {
  ix <- lengjan_archive_index(withr::local_tempdir())
  expect_equal(nrow(ix), 0L)
  expect_named(ix, c("ts", "reason", "status", "program_file", "n_events", "markets_files", "n_detail_events", "n_requests", "error"))
})

test_that("a manifest line of another format version is skipped with a warning", {
  dir <- .local_archive(list(list(ts = "2026-09-23T12:00:00Z", program = "current-program.json", markets = character())))
  cat('{"v": 2, "ts": "2026-09-23T12:30:00Z", "status": "ok", "markets": []}\n',
    file = file.path(dir, "manifest", "2026-09.jsonl"), append = TRUE
  )
  expect_warning(ix <- lengjan_archive_index(dir), "version 2")
  expect_equal(format(ix$ts, "%H:%M", tz = "UTC"), "12:00")
})

test_that("an archive directory that does not exist is an error, not an empty index", {
  gone <- file.path(withr::local_tempdir(), "not-mounted")
  expect_error(lengjan_archive_index(gone), "does not exist")
})

test_that("parse_lengjan_program_markets maps the embedded 3WAY to moneyline", {
  prog <- list(events = list(
    list(id = "1", market = list(typeName = "3WAY", primary = TRUE, status = "open",
      selections = list(list(name = "1", odds = 194L), list(name = "X", odds = 264L), list(name = "2", odds = 406L)))),
    list(id = "2", market = list(typeName = "2WAY", primary = TRUE, status = "open",
      selections = list(list(name = "1", odds = 150L), list(name = "2", odds = 250L)))),
    list(id = "3", market = NULL),
    list(id = "4", market = list(typeName = "3WAY", primary = TRUE, status = "suspended",
      selections = list(list(name = "1", odds = 194L))))
  ), liveSoon = list(list(id = "1", market = NULL)))
  out <- parse_lengjan_program_markets(prog)
  expect_equal(out$event_id, c("1", "1", "1"))
  expect_equal(out$outcome, c("home", "draw", "away"))
  expect_equal(out$odds, c(1.94, 2.64, 4.06))
  expect_equal(unique(out$market), "moneyline")
  expect_true(all(is.na(out$line)))
})

test_that("parse_lengjan_program_markets returns the empty shape when nothing qualifies", {
  out <- parse_lengjan_program_markets(list(events = list(), liveSoon = list()))
  expect_equal(nrow(out), 0L)
  expect_named(out, c("event_id", "market", "outcome", "line", "odds"))
})
