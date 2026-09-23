# Odds schema evolution (spec 2026-09-23 WS2): four nullable columns, `sex` in
# the natural key, and a read path that surfaces columns older fragments lack.

.evo_odds <- function(scraped_at, sex = NULL, event_id = NULL, odds = 2.0) {
  df <- tibble::tibble(
    sport = "handball", country = "iceland",
    scraped_at = as.POSIXct(scraped_at, tz = "UTC"),
    match_date = as.Date("2100-01-05"),
    home_team = "Valur", away_team = "Haukar",
    market = "moneyline", outcome = "home", line = NA_real_, odds = odds
  )
  if (!is.null(sex)) df$sex <- sex
  if (!is.null(event_id)) df$event_id <- event_id
  df
}

# Recreate the legacy on-disk format 47 real odds files have: only the 10
# original columns and a tz-NAIVE scraped_at. Written straight through arrow so
# neither write_table() nor its optional-column fill touches it.
.write_legacy_odds <- function(root, scraped_at) {
  t <- as.POSIXct(scraped_at, tz = "UTC")
  tab <- arrow::arrow_table(
    scraped_at = arrow::Array$create(t)$cast(arrow::timestamp("us")),
    match_date = as.Date("2100-01-05"),
    home_team = "Valur", away_team = "Haukar",
    market = "moneyline", outcome = "home", line = NA_real_, odds = 1.9,
    sport = "handball", country = "iceland",
    scraped_date = format(as.Date(t, tz = "UTC"))
  )
  arrow::write_dataset(
    tab, fs::path(root, "facts", "odds"),
    partitioning = c("sport", "country", "scraped_date")
  )
}

test_that("a writer that omits the optional columns still writes; they read back NA", {
  root <- withr::local_tempdir()
  write_table(.evo_odds("2100-01-01 08:00:00"), "odds", root = root)
  back <- read_table("odds", root = root)
  expect_true(all(c("sex", "event_id", "competition_id", "kickoff_at") %in% names(back)))
  expect_true(is.na(back$sex))
  expect_s3_class(back$kickoff_at, "POSIXct")
})

test_that("upserting into a legacy-format same-day partition keeps the legacy rows", {
  # The first post-deploy scrape lands in a scraped_date partition the morning's
  # legacy-format scrape already wrote. With `sex` in the natural key but absent
  # from the existing rows, upsert_table() must still merge, not overwrite:
  # odds snapshots cannot be re-scraped. A new-format partition on another day
  # makes the store mixed-tz like production, so upsert's partition read goes
  # through read_table()'s explicit-schema fallback with a Date filter -- the
  # exact path the real store takes (verified 2026-09-23).
  root <- withr::local_tempdir()
  write_table(.evo_odds("2100-01-03 08:00:00", sex = "male"), "odds", root = root)
  .write_legacy_odds(root, "2100-01-01 08:00:00")
  upsert_table(
    .evo_odds("2100-01-01 14:00:00", sex = "male", event_id = "4602104"),
    "odds",
    root = root
  )
  back <- read_table("odds", root = root)
  day1 <- back[as.Date(back$scraped_at, tz = "UTC") == as.Date("2100-01-01"), ]
  hh <- format(day1$scraped_at, "%H", tz = "UTC")
  expect_equal(nrow(back), 3L)
  expect_equal(nrow(day1), 2L)
  expect_setequal(hh, c("08", "14"))
  expect_equal(day1$sex[hh == "14"], "male")
  expect_true(is.na(day1$sex[hh == "08"]))
})

test_that("read_table surfaces a new column when fragments cannot be unified", {
  # tz-naive legacy fragment + tz-aware new fragment: unify_schemas fails, and a
  # first-fragment-wins open would drop `event_id` for every reader.
  root <- withr::local_tempdir()
  .write_legacy_odds(root, "2100-01-01 08:00:00")
  write_table(
    .evo_odds("2100-01-02 08:00:00", sex = "male", event_id = "42"),
    "odds",
    root = root
  )
  back <- read_table("odds", root = root)
  expect_equal(nrow(back), 2L)
  expect_true("event_id" %in% names(back))
  expect_equal(sort(back$event_id, na.last = TRUE), c("42", NA))
  expect_equal(attr(back$scraped_at, "tzone"), "UTC")
  # The tz-naive instant reads back unchanged.
  expect_true(as.POSIXct("2100-01-01 08:00:00", tz = "UTC") %in% back$scraped_at)
})

test_that("upsert fills optional columns a legacy-only partition lacks before its key check", {
  # Pins fill_optional_columns(existing): with ONLY legacy fragments the store
  # unifies cleanly, so read_table() returns rows with no `sex` column, and
  # without the fill the natural-key check fails and the partition is
  # overwritten. Passes on the pre-change code (whose key lacks `sex`); Step 5
  # proves it guards the fill by deleting the fill line and watching it fail.
  root <- withr::local_tempdir()
  .write_legacy_odds(root, "2100-01-01 08:00:00")
  upsert_table(.evo_odds("2100-01-01 14:00:00", sex = "male"), "odds", root = root)
  back <- read_table("odds", root = root)
  expect_equal(nrow(back), 2L)
  expect_setequal(format(back$scraped_at, "%H", tz = "UTC"), c("08", "14"))
})

test_that("men's and women's odds between the same clubs in one scrape are both kept", {
  # Two upserts, so the second meets the first in an existing partition and the
  # natural key decides: without `sex` in it, the female row replaces the male.
  root <- withr::local_tempdir()
  upsert_table(.evo_odds("2100-01-01 08:00:00", sex = "male", odds = 1.8), "odds", root = root)
  upsert_table(.evo_odds("2100-01-01 08:00:00", sex = "female", odds = 2.4), "odds", root = root)
  back <- read_table("odds", root = root)
  expect_equal(nrow(back), 2L)
  expect_setequal(back$sex, c("male", "female"))
})
