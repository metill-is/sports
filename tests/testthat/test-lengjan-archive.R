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

.two_captures <- function(env = parent.frame()) {
  .local_archive(list(
    list(ts = "2026-09-23T12:00:00Z", program = "current-program.json", markets = "markets.json"),
    list(ts = "2026-09-23T12:30:00Z", program = "current-program.json", markets = "markets.json")
  ), env = env)
}
# The fixtures' only events WITH markets are 4616742 (handball, comp 9281, DE,
# 9 rows), 4412294 (basketball, comp 9445, US, 5 rows) and 4601670 (football,
# comp 10046, INT, 25 rows). The Icelandic events carry none, so a league keyed
# on comp 1269 would make every assertion below vacuous.
.hb <- function() {
  list(handball_x = list(sport = "handball", country = "germany",
    lengjan = list(source = "api", competitions = list(list(id = "9281", name = "x", sex = "male", division = "OD")))))
}

test_that("detail rows for a configured competition equal lengjan_api_odds_rows()", {
  dir <- .two_captures()
  got <- read_lengjan_archive(dir, what = "detail", leagues = .hb())
  ts <- as.POSIXct("2026-09-23 12:00:00", tz = "UTC")
  want <- lengjan_api_odds_rows(parse_lengjan_program(.fx("current-program.json")),
    parse_lengjan_markets(.fx("markets.json")), .hb()$handball_x, ts)
  expect_equal(nrow(want), 9L)                                   # guards against a vacuous comparison
  mine <- got[got$scraped_at == ts & got$competition_id %in% "9281", names(want)]
  key <- c("event_id", "market", "outcome", "line")
  expect_equal(dplyr::arrange(mine, dplyr::across(dplyr::all_of(key))),
               dplyr::arrange(want, dplyr::across(dplyr::all_of(key))))
  expect_equal(nrow(got[got$scraped_at == ts, ]), 39L)           # 9 + 5 + 25: every event, configured or not
})

test_that("an event in two captures yields rows per capture", {            # Review Focus 4
  got <- read_lengjan_archive(.two_captures(), what = "detail", leagues = .hb())
  expect_equal(length(unique(got$scraped_at)), 2L)
  n <- table(got$scraped_at)
  expect_equal(n[[1]], n[[2]])
})

test_that("unconfigured competitions come back with sex NA and payload fields", {
  got <- read_lengjan_archive(.two_captures(), what = "detail", leagues = list())
  expect_gt(nrow(got), 0L)
  expect_true(all(is.na(got$sex)))
  expect_false(anyNA(got$sport_id)); expect_false(anyNA(got$country_code)); expect_false(anyNA(got$sport_name))
})

test_that("unknown sport ids and country codes give NA sport and country", {  # Review Focus 5
  expect_equal(.lengjan_sport_from_id(c(1L, 2L, 6L, 4L)), c("football", "basketball", "handball", NA))
  expect_equal(.lengjan_country_from_code(c("IS", "DE", "US", NA)), c("iceland", "germany", NA, NA))
  got <- read_lengjan_archive(.two_captures(), what = "program", leagues = list())
  expect_equal(unique(got$country[got$country_code == "DE"]), "germany")
  expect_true(all(is.na(got$country[got$country_code == "US"])))   # the fixture's basketball event
  expect_equal(unique(got$sport_name[got$sport_id == 2L]), "Karfa")
})

test_that("program mode returns moneyline rows for events with an embedded market", {
  got <- read_lengjan_archive(.two_captures(), what = "program", leagues = .hb())
  expect_equal(nrow(got), 18L)                                     # 3 events x 3 selections x 2 captures
  expect_equal(unique(got$sex[got$competition_id == "9281"]), "male")
  expect_equal(unique(got$market), "moneyline")
  expect_named(got, c("sport", "country", "scraped_at", "match_date", "home_team", "away_team", "market",
    "outcome", "line", "odds", "sex", "event_id", "competition_id", "kickoff_at", "sport_id", "sport_name",
    "country_code", "competition_name"))
})

test_that("from and to bound the captures, and an empty window is a zero-row tibble", {  # Review Focus 3
  dir <- .two_captures()
  one <- read_lengjan_archive(dir, from = "2026-09-23T12:15:00Z", leagues = .hb())
  expect_equal(format(unique(one$scraped_at), "%H:%M", tz = "UTC"), "12:30")
  none <- read_lengjan_archive(dir, from = as.Date("2027-01-01"), leagues = .hb())
  expect_equal(nrow(none), 0L); expect_equal(ncol(none), 18L)
  expect_equal(nrow(read_lengjan_archive(withr::local_tempdir(), leagues = .hb())), 0L)
})

test_that("a capture naming a missing file is skipped with a warning", {    # Review Focus 1
  dir <- .two_captures()
  unlink(list.files(file.path(dir, "raw"), pattern = "123000Z-markets", recursive = TRUE, full.names = TRUE))
  expect_warning(got <- read_lengjan_archive(dir, what = "detail", leagues = .hb()), "123000Z-markets")
  expect_equal(length(unique(got$scraped_at)), 1L)
})

test_that("error and skipped captures are not read", {
  dir <- .local_archive(list(
    list(ts = "2026-09-23T12:00:00Z", program = "current-program.json", markets = "markets.json"),
    list(ts = "2026-09-23T12:30:00Z", program = "current-program.json", markets = character(), status = "error"),
    list(ts = "2026-09-23T13:00:00Z", program = NULL, markets = character(), status = "skipped", reason = "budget")
  ))
  got <- suppressMessages(read_lengjan_archive(dir, what = "program", leagues = .hb()))
  expect_equal(length(unique(got$scraped_at)), 1L)
})

test_that(".lengjan_country_code still maps canonical names", {
  expect_equal(.lengjan_country_code("iceland"), "IS")
  expect_error(.lengjan_country_code("atlantis"), "Unknown Lengjan country code")
})

test_that("from and to are inclusive, and a date given as to covers its whole day", {
  dir <- .two_captures()
  hm <- function(x) format(unique(x$scraped_at), "%H:%M", tz = "UTC")
  expect_equal(hm(read_lengjan_archive(dir, from = "2026-09-23T12:30:00Z", leagues = .hb())), "12:30")
  expect_equal(hm(read_lengjan_archive(dir, to = "2026-09-23T12:15:00Z", leagues = .hb())), "12:00")
  at_1230 <- as.POSIXct("2026-09-23 12:30:00", tz = "UTC")
  expect_equal(hm(read_lengjan_archive(dir, to = at_1230, leagues = .hb())), c("12:00", "12:30"))
  expect_equal(hm(read_lengjan_archive(dir, to = as.Date("2026-09-23"), leagues = .hb())), c("12:00", "12:30"))
  expect_equal(hm(read_lengjan_archive(dir, to = "2026-09-23", leagues = .hb())), c("12:00", "12:30"))
  expect_equal(nrow(read_lengjan_archive(dir, to = "2026-09-22", leagues = .hb())), 0L)
  expect_error(read_lengjan_archive(dir, from = "last tuesday", leagues = .hb()), "`from` must be")
})

test_that("a capture whose body cannot be read is skipped with a warning", {
  dir <- .two_captures()
  cut_off <- list.files(file.path(dir, "raw"), pattern = "123000Z-program", recursive = TRUE, full.names = TRUE)
  writeBin(readBin(cut_off, "raw", 200L), cut_off) # a pull interrupted mid-file
  expect_warning(got <- read_lengjan_archive(dir, what = "program", leagues = .hb()), "123000Z-program")
  expect_equal(format(unique(got$scraped_at), "%H:%M", tz = "UTC"), "12:00")
})

test_that("a body of an unexpected shape stops the read, naming the capture", {
  dir <- .two_captures()
  odd <- list.files(file.path(dir, "raw"), pattern = "123000Z-program", recursive = TRUE, full.names = TRUE)
  con <- gzfile(odd, "wb")
  writeLines('{"data": []}', con)
  close(con)
  expect_error(
    read_lengjan_archive(dir, what = "program", leagues = .hb()),
    "2026-09-23T12:30:00Z.*current-program shape"
  )
})

test_that("sex needs the league's sport to match, not only the competition id", {
  other_sport <- .hb()
  other_sport$handball_x$sport <- "football"
  got <- read_lengjan_archive(.two_captures(), what = "program", leagues = other_sport)
  expect_equal(nrow(got[got$competition_id == "9281", ]), 6L)
  expect_true(all(is.na(got$sex)))
})

test_that("rows of an event with no home or away team are dropped", {
  dir <- .two_captures()
  prog <- .fx("current-program.json")
  expect_equal(prog$events[[1]]$id, "4616742")
  prog$events[[1]]$participants <- prog$events[[1]]$participants[1]
  first <- list.files(file.path(dir, "raw"), pattern = "120000Z-program", recursive = TRUE, full.names = TRUE)
  jsonlite::write_json(prog, gzfile(first), auto_unbox = TRUE, null = "null", digits = NA)
  got <- read_lengjan_archive(dir, what = "detail", leagues = .hb())
  n <- table(format(got$scraped_at, "%H:%M", tz = "UTC"))
  expect_equal(n[["12:00"]], 30L) # 5 + 25: the handball event's 9 rows are gone
  expect_equal(n[["12:30"]], 39L)
})

test_that("a partial capture is read", {
  dir <- .local_archive(list(
    list(ts = "2026-09-23T12:00:00Z", program = "current-program.json", markets = "markets.json", status = "partial")
  ))
  expect_equal(nrow(read_lengjan_archive(dir, what = "detail", leagues = .hb())), 39L)
})

test_that("an error capture is named in a note, a skipped one passes silently", {
  dir <- .local_archive(list(
    list(ts = "2026-09-23T12:30:00Z", program = "current-program.json", markets = character(), status = "error")
  ))
  expect_message(read_lengjan_archive(dir, leagues = .hb()), "2026-09-23T12:30:00Z")
  dir <- .local_archive(list(
    list(ts = "2026-09-23T13:00:00Z", program = NULL, markets = character(), status = "skipped", reason = "budget")
  ))
  expect_no_message(none <- read_lengjan_archive(dir, leagues = .hb()))
  expect_equal(nrow(none), 0L)
})

# parse_lengjan_program() costs about 3 ms per event (3.3 s for a 1,000-event
# programme, measured 2026-11-16), so a capture hands it only the events that
# have odds rows: the 3 with markets, not the fixture programme's 6.
test_that("only events with odds rows reach parse_lengjan_program()", {
  n_parsed <- integer()
  real <- parse_lengjan_program
  local_mocked_bindings(parse_lengjan_program = function(prog) {
    out <- real(prog)
    n_parsed <<- c(n_parsed, nrow(out))
    out
  })
  got <- read_lengjan_archive(.two_captures(), what = "detail", leagues = .hb())
  expect_equal(n_parsed, c(3L, 3L))
  expect_equal(nrow(got), 78L)
})

test_that("an event whose kickoff does not parse keeps its rows, with NA dates", {
  dir <- .two_captures()
  prog <- .fx("current-program.json")
  prog$events[[1]]$datePlayed <- "soon"
  first <- list.files(file.path(dir, "raw"), pattern = "120000Z-program", recursive = TRUE, full.names = TRUE)
  jsonlite::write_json(prog, gzfile(first), auto_unbox = TRUE, null = "null", digits = NA)
  got <- read_lengjan_archive(dir, what = "detail", leagues = .hb())
  hb <- got[got$event_id == "4616742", ]
  expect_equal(nrow(hb), 18L)
  expect_equal(sum(is.na(hb$kickoff_at)), 9L)
  expect_equal(is.na(hb$match_date), is.na(hb$kickoff_at))
})

# A scalar lubridate::ymd_hms() call costs about 2.5 ms, so parsing per line
# made a year-long manifest (35,040 lines) take about 100 s to index.
test_that("the index parses the manifest's timestamps in one vectorised call", {
  calls <- 0L
  real <- .lengjan_parse_utc
  local_mocked_bindings(.lengjan_parse_utc = function(x) {
    calls <<- calls + 1L
    real(x)
  })
  ix <- lengjan_archive_index(.two_captures())
  expect_equal(calls, 1L)
  expect_equal(format(ix$ts, "%H:%M", tz = "UTC"), c("12:00", "12:30"))
  expect_equal(attr(ix$ts, "tzone"), "UTC")
})

test_that("skipped manifest lines are reported in one warning naming each line", {
  dir <- .local_archive(list(list(ts = "2026-09-23T12:00:00Z", program = "current-program.json", markets = character())))
  cat('{"v": 1, "ts": "nonsense", "status": "ok"}\n{"v": 1, "ts": "2026-09-2',
    file = file.path(dir, "manifest", "2026-09.jsonl"), append = TRUE
  )
  w <- capture_warnings(ix <- lengjan_archive_index(dir))
  expect_length(w, 1L)
  expect_match(w, "2026-09.jsonl line 2: no parseable `ts`", fixed = TRUE)
  expect_match(w, "2026-09.jsonl line 3: not a JSON object", fixed = TRUE)
  expect_equal(nrow(ix), 1L)
})
