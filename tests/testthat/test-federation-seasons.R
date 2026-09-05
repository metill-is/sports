entry <- function(sex, division, season, id, source = "live-nav",
                  verified = TRUE, title = NA_character_) {
  tibble::tibble(
    federation = "hsi", sex = sex, division = division,
    season = as.integer(season), id = as.integer(id),
    title = title, source = source,
    discovered_at = "2026-09-02", verified = verified,
    note = NA_character_
  )
}

test_that("read_federation_seasons round-trips what refresh writes", {
  path <- withr::local_tempfile(fileext = ".json")
  refresh_federation_seasons(entry("male", "div1", 2027L, 9142L), path = path)
  got <- read_federation_seasons(path)
  expect_equal(nrow(got), 1L)
  expect_identical(got$id, 9142L)
  expect_type(got$season, "integer")
  expect_type(got$verified, "logical")
})

test_that("read_federation_seasons returns a typed zero-row frame when absent", {
  got <- read_federation_seasons(file.path(tempdir(), "no-such-file.json"))
  expect_equal(nrow(got), 0L)
  expect_named(
    got,
    c("federation", "sex", "division", "season", "id", "title",
      "source", "discovered_at", "verified", "note")
  )
})

test_that("federation_season_id resolves only verified, season-attributed rows", {
  path <- withr::local_tempfile(fileext = ".json")
  refresh_federation_seasons(
    dplyr::bind_rows(
      entry("male", "div1", 2027L, 9142L),
      entry("male", "cup", NA_integer_, 8437L,
            source = "live-nav-unattributed", verified = FALSE)
    ),
    path = path
  )
  expect_identical(federation_season_id("hsi", "male", "div1", 2027L, path), 9142L)
  expect_null(federation_season_id("hsi", "male", "cup", 2027L, path))
  expect_null(federation_season_id("hsi", "female", "div1", 2027L, path))
  expect_null(federation_season_id("kki", "male", "div1", 2027L, path))
})

test_that("merge_federation_seasons upgrades an unverified row from a trusted source", {
  existing <- entry("female", "div2", 2025L, 7643L,
                    source = "inferred-candidate", verified = FALSE)
  incoming <- entry("female", "div2", 2025L, 7643L,
                    source = "inferred-verified", verified = TRUE)
  merged <- merge_federation_seasons(incoming, existing)
  expect_equal(nrow(merged), 1L)
  expect_true(merged$verified)
  expect_identical(merged$source, "inferred-verified")
})

test_that("merge_federation_seasons aborts when two verified rows disagree", {
  existing <- entry("male", "div1", 2027L, 9142L)
  incoming <- entry("male", "div1", 2027L, 9999L)
  expect_error(
    merge_federation_seasons(incoming, existing),
    class = "sports_federation_id_conflict"
  )
})

test_that("the committed cache seeds the four 2027 league ids", {
  cache <- read_federation_seasons()
  hsi27 <- cache[cache$federation == "hsi" &
                   !is.na(cache$season) & cache$season == 2027L, ]
  expect_setequal(
    hsi27$id,
    c(9142L, 9140L, 9141L, 9143L)
  )
  expect_true(all(hsi27$verified))
  expect_true(all(nzchar(hsi27$discovered_at)))
})

test_that("unattributed live-nav observations are recorded but unresolvable", {
  cache <- read_federation_seasons()
  unattr <- cache[is.na(cache$season), ]
  expect_setequal(unattr$id, c(8437L, 8436L))
  expect_false(any(unattr$verified))
})
