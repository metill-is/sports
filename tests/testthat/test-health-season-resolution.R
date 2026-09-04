# check_season_resolution is the alarm that distinguishes "the season is
# genuinely over" from "the scraper went blind in October". Both look identical
# from the results table: zero new rows. The difference is whether the
# federation's season id RESOLVED, and only the registry knows that.
#
# Resolvers are injected rather than read from the real registries so these
# blocks stay hermetic and hit no network. The real-registry behaviour is
# checked separately, at the bottom of this file, against measured values.

.hb_league <- function(active = TRUE, results = "hsi_handball") {
  list(handball_iceland = list(
    sport = "handball", country = "iceland",
    sexes = list("male", "female"), active = active,
    data_source = list(results = results)
  ))
}

.res <- function(unresolved_rows) {
  list(hsi = list(
    current = function(today) 2027L,
    unresolved = function(season, ...) unresolved_rows,
    league_divisions = c("div1", "div2")
  ))
}

.no_gaps <- function() {
  tibble::tibble(sex = character(), division = character(), season = integer())
}

test_that("a federation with no gaps yields one OK row", {
  res <- check_season_resolution(
    .hb_league(), tempdir(), Sys.time(),
    resolvers = .res(.no_gaps())
  )
  expect_equal(nrow(res), 1L)
  expect_equal(res$status, "OK")
  expect_equal(res$check, "season_resolution")
  expect_equal(res$scope, "hsi")
})

test_that("an unresolvable league division FAILs and names sex, division, season", {
  gaps <- tibble::tibble(sex = "male", division = "div1", season = 2027L)
  res <- check_season_resolution(
    .hb_league(), tempdir(), Sys.time(),
    resolvers = .res(gaps)
  )
  expect_equal(res$status, "FAIL")
  expect_match(res$scope, "hsi male div1")
  expect_match(res$value, "2027")
})

test_that("a federation-deferred cup or playoffs WARNs, never FAILs", {
  # INT-4, and it is a measurement not a preference. hsi_unresolved_seasons(2027)
  # returns exactly these three rows today, because HSI does not create the
  # urslitakeppni or the 2026-27 bikar tournaments until later in the season
  # (R/ingest-hsi-handball.R documents that deferral as correct). FAILing on
  # them would leave the check permanently red from day one -- alarm fatigue,
  # which is the failure mode this workstream exists to prevent, on a channel
  # that is a twice-daily email.
  gaps <- tibble::tibble(
    sex = c("male", "male", "female"),
    division = c("cup", "playoffs", "playoffs"),
    season = c(2027L, 2027L, 2027L)
  )
  res <- check_season_resolution(
    .hb_league(), tempdir(), Sys.time(),
    resolvers = .res(gaps)
  )
  expect_true(all(res$status %in% c("OK", "WARN")))
  expect_false(any(res$status == "FAIL"))
  expect_equal(sum(res$status == "WARN"), 3L)
})

test_that("a league division gap and a deferred cup coexist at their own severities", {
  gaps <- tibble::tibble(
    sex = c("male", "male"), division = c("div1", "cup"),
    season = c(2027L, 2027L)
  )
  res <- check_season_resolution(
    .hb_league(), tempdir(), Sys.time(),
    resolvers = .res(gaps)
  )
  expect_setequal(res$status, c("FAIL", "WARN"))
  expect_equal(res$status[res$scope == "hsi male div1"], "FAIL")
  expect_equal(res$status[res$scope == "hsi male cup"], "WARN")
})

test_that("an inactive league contributes no rows", {
  res <- check_season_resolution(
    .hb_league(active = FALSE), tempdir(), Sys.time(),
    resolvers = .res(.no_gaps())
  )
  expect_equal(nrow(res), 0L)
})

test_that("football contributes no rows at all", {
  # INT-5. Its data_source$results is ksi_football, for which there is no
  # unresolved-seasons resolver and no season registry to go stale. A fake OK
  # row would imply a guarantee that does not exist.
  fb <- list(football_iceland = list(
    sport = "football", country = "iceland", sexes = list("male"),
    active = TRUE, data_source = list(results = "ksi_football")
  ))
  res <- check_season_resolution(
    fb, tempdir(), Sys.time(),
    resolvers = .res(.no_gaps())
  )
  expect_equal(nrow(res), 0L)
})

test_that("both sexes of one league produce one federation's rows, not two", {
  # The registry is per-federation, not per-sex; hsi_unresolved_seasons()
  # already covers both sexes in one call.
  calls <- 0L
  resolvers <- list(hsi = list(
    current = function(today) 2027L,
    unresolved = function(season, ...) {
      calls <<- calls + 1L
      .no_gaps()
    },
    league_divisions = c("div1", "div2")
  ))
  res <- check_season_resolution(
    .hb_league(), tempdir(), Sys.time(),
    resolvers = resolvers
  )
  expect_equal(calls, 1L)
  expect_equal(nrow(res), 1L)
})

test_that("the real registries are reachable and report the measured state", {
  # No injection: this is the one block that runs the shipped resolvers. Both
  # are pure registry + cache lookups and touch no network. Measured
  # 2026-09-04: kki has no gaps for 2027; hsi has exactly three, all of them
  # federation-deferred. A FAIL here means the registry regressed.
  leagues <- load_leagues()
  res <- check_season_resolution(leagues, here::here("data"), Sys.time())
  expect_true(all(res$check == "season_resolution"))
  expect_setequal(sub(" .*$", "", res$scope), c("hsi", "kki"))
  expect_false(any(res$status == "FAIL"))
  expect_equal(res$status[res$scope == "kki"], "OK")
  expect_setequal(
    res$scope[res$status == "WARN"],
    c("hsi male cup", "hsi male playoffs", "hsi female playoffs")
  )
})
