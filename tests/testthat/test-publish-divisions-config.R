# Schema + helper-shape tests for football_iceland.publish_divisions.
# Lightweight (no Stan, no fit) — kept fast so the wiring is regression-tested
# on every CI run, independent of the slow end-to-end fit-based tests.

test_that("publish_divisions: config block exists with both sexes", {
  cfg <- load_leagues()[["football_iceland"]][["publish_divisions"]]
  expect_true(!is.null(cfg))
  expect_setequal(names(cfg), c("male", "female"))
})

test_that("publish_divisions: every entry has the 4 required fields", {
  cfg <- load_leagues()[["football_iceland"]][["publish_divisions"]]
  required <- sort(c("code", "slug", "label_is", "is_cup"))
  for (sex_key in names(cfg)) {
    for (i in seq_along(cfg[[sex_key]])) {
      entry <- cfg[[sex_key]][[i]]
      expect_equal(
        sort(names(entry)), required,
        info = sprintf("sex=%s entry %d missing fields", sex_key, i)
      )
    }
  }
})

test_that("publish_divisions: every non-CUP code appears in training_filter.divisions", {
  league <- load_leagues()[["football_iceland"]]
  cfg <- league$publish_divisions
  training_divs <- league$training_filter$divisions

  for (sex_key in names(cfg)) {
    for (entry in cfg[[sex_key]]) {
      if (!isTRUE(entry$is_cup)) {
        expect_true(
          entry$code %in% training_divs,
          info = sprintf(
            "publish_divisions[%s].code=%s must be in training_filter.divisions=[%s]",
            sex_key, entry$code, paste(training_divs, collapse = ",")
          )
        )
      }
    }
  }
})

test_that(".football_iceland_division_codes: returns the expected set per sex", {
  expect_equal(
    .football_iceland_division_codes("male"),
    c("BD", "LD1", "LD2", "LD3", "CUP")
  )
  expect_equal(
    .football_iceland_division_codes("female"),
    c("BD", "LD1", "LD2", "CUP")
  )
})

test_that(".football_iceland_division_slugs: returns named vector matching consumer URL slugs", {
  m <- .football_iceland_division_slugs("male")
  expect_equal(
    m,
    c(BD = "bd", LD1 = "ld", LD2 = "2deild", LD3 = "3deild", CUP = "bikar")
  )
  f <- .football_iceland_division_slugs("female")
  expect_equal(
    f,
    c(BD = "bd", LD1 = "ld", LD2 = "2deild", CUP = "bikar")
  )
})

test_that(".football_iceland_division_labels: returns Icelandic display labels", {
  m <- .football_iceland_division_labels("male")
  expect_equal(m[["BD"]], "Besta deild")
  expect_equal(m[["LD2"]], "2. deild")
  expect_equal(m[["LD3"]], "3. deild")
  expect_equal(m[["CUP"]], "Mjólkurbikar")

  f <- .football_iceland_division_labels("female")
  # Women's cup has its own brand on Lengjan ("Bikar kvenna"); BD/LD1 share
  # the male labels because the rendered title is "{label} {sex_slug}" on the
  # consumer side and the sex differentiator comes from sex_slug, not label.
  expect_equal(f[["BD"]], "Besta deild")
  expect_equal(f[["CUP"]], "Bikar kvenna")
  expect_false("LD3" %in% names(f)) # no women's 3. deild in Iceland
})

test_that("publish_divisions: sex helper errors for invalid sex", {
  expect_error(.football_iceland_division_codes("nonsense"))
  expect_error(.football_iceland_division_slugs("nonsense"))
  expect_error(.football_iceland_division_labels("nonsense"))
})

test_that("publish_divisions: slugs collide-free per sex", {
  for (sex_key in c("male", "female")) {
    slugs <- .football_iceland_division_slugs(sex_key)
    expect_equal(
      length(slugs), length(unique(slugs)),
      info = sprintf("slug collision in sex=%s", sex_key)
    )
  }
})

test_that("extract_football_iceland: target_divs validation rejects out-of-config codes", {
  league <- load_leagues()[["football_iceland"]]
  # Passing an explicit valid subset works
  expect_silent(
    stopifnot(
      all(c("BD", "LD2") %in% .football_iceland_division_codes("male"))
    )
  )
  # The validator inside extract_football_iceland: passing LD4 for male should
  # error because LD4 is not in publish_divisions[[male]] (excluded by design).
  fit <- list() # placeholder — won't be reached
  expect_error(
    extract_football_iceland(
      fit, league,
      sex = "male",
      fit_date = as.Date("2026-05-24"),
      end_date = as.Date("2026-05-24"),
      target_divs = c("BD", "LD4"),
      extracts_root = tempfile()
    ),
    regexp = "target_divs"
  )
})
