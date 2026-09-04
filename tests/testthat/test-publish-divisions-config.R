# Schema + helper-shape tests for football_iceland.publish_divisions.
# Lightweight (no Stan, no fit) — kept fast so the wiring is regression-tested
# on every CI run, independent of the slow end-to-end fit-based tests.

test_that("publish_divisions: config block exists with both sexes", {
  cfg <- load_leagues()[["football_iceland"]][["publish_divisions"]]
  expect_true(!is.null(cfg))
  expect_setequal(names(cfg), c("male", "female"))
})

test_that("publish_divisions: every entry has the 4 required fields and no unknown ones", {
  cfg <- load_leagues()[["football_iceland"]][["publish_divisions"]]
  required <- c("code", "slug", "label_is", "is_cup")
  optional <- c(
    "split", "code_badge", "expected_meetings", "relegation_slots", "qualify"
  )
  for (sex_key in names(cfg)) {
    for (i in seq_along(cfg[[sex_key]])) {
      entry <- cfg[[sex_key]][[i]]
      expect_true(
        all(required %in% names(entry)),
        info = sprintf("sex=%s entry %d missing required fields", sex_key, i)
      )
      expect_true(
        all(names(entry) %in% c(required, optional)),
        info = sprintf("sex=%s entry %d has unknown fields", sex_key, i)
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

test_that("publish_football_iceland: every division_code emitted matches next_games schema regex", {
  # WHY: 2deild/3deild cells added 2026-05-24 introduced non-ASCII recode
  # outputs "ÖD"/"ÞD" into publish_football_iceland()'s
  # division_labels map, which the JSON schema's
  # ^[A-Z][A-Z0-9_]*$ pattern rejects -- decide-publish.yml then aborts.
  # This test parameterises over every cell in publish_divisions.{male,female}
  # so the next addition of a non-ASCII recode is caught at devtools::test()
  # rather than in CI.
  schema <- jsonlite::fromJSON(
    here::here(
      "config", "publish-schemas", "football", "next_games.schema.json"
    )
  )
  pattern <- schema$properties$matches$items$properties$division_code$pattern
  expect_true(nzchar(pattern))

  labels <- .football_iceland_division_code_labels()
  # The recode map's own values must all be schema-compliant -- even those
  # for codes that aren't currently in publish_divisions (e.g. _PO playoff
  # codes), because they could land in `division` payloads via training
  # data and ride through the recode unchanged otherwise.
  for (code in names(labels)) {
    expect_match(
      labels[[code]], pattern,
      info = sprintf(
        "division_labels[%s] = '%s' violates schema pattern %s",
        code, labels[[code]], pattern
      )
    )
  }

  # And the publisher's emitted division_code for every (sex, cell) pair
  # in publish_divisions must match too.
  for (sex_key in c("male", "female")) {
    codes <- .football_iceland_division_codes(sex_key)
    for (code in codes) {
      emitted <- if (code %in% names(labels)) labels[[code]] else code
      expect_match(
        emitted, pattern,
        info = sprintf(
          "publish_divisions[%s] code=%s emits division_code='%s' which violates schema pattern %s",
          sex_key, code, emitted, pattern
        )
      )
    }
  }
})

# ---- Split-season format config (efri/nedri hluti) --------------------------
# Verified 2026-07-10 (design doc 2026-07-10-split-season-simulator-design.md):
# men 12 teams split 6/6, women 10 teams split 6/4, single RR, full carry-over.

test_that("publish_divisions: BD carries the verified split format per sex", {
  cfg <- load_leagues()[["football_iceland"]][["publish_divisions"]]
  bd_male <- Filter(function(e) identical(e$code, "BD"), cfg$male)[[1]]
  bd_female <- Filter(function(e) identical(e$code, "BD"), cfg$female)[[1]]
  expect_equal(bd_male$split, list(upper = 6L, lower = 6L))
  expect_equal(bd_female$split, list(upper = 6L, lower = 4L))
})

test_that(".football_iceland_division_split: split config keyed by division code", {
  m <- .football_iceland_division_split("male")
  f <- .football_iceland_division_split("female")
  expect_equal(m$BD, list(upper = 6L, lower = 6L))
  expect_equal(f$BD, list(upper = 6L, lower = 4L))
  expect_null(m$LD1)
  expect_null(m$CUP)
  expect_null(f$LD2)
})

# ---- code_badge + qualify (Plan B WS7 task 2) -------------------------------
# The badge map used to live in R as .football_iceland_division_code_labels().
# It moves into config so basketball's Bónusdeild (also coded BD) can carry its
# own badge instead of colliding with football's BD on the consumer's filter
# key. Values here MUST stay byte-identical to the retired R map.

test_that("football publish_divisions carries the legacy badge map as code_badge", {
  legacy <- .football_iceland_division_code_labels()
  cfg <- load_leagues()[["football_iceland"]][["publish_divisions"]]
  n_checked <- 0L
  for (sex_key in names(cfg)) {
    for (entry in cfg[[sex_key]]) {
      expect_identical(
        entry$code_badge, unname(legacy[[entry$code]]),
        info = sprintf("sex=%s code=%s", sex_key, entry$code)
      )
      n_checked <- n_checked + 1L
    }
  }
  expect_gt(n_checked, 0L)
})

test_that("football BD carries qualify {6, Efri hluti} for both sexes", {
  # 6 is split$upper (config/leagues.yml BD entries, verified 2026-07-10), so
  # p_qualify reproduces the existing p_top_six rule `placement <= 6L` at
  # R/publish-football-iceland.R exactly. No other football cell qualifies.
  cfg <- load_leagues()[["football_iceland"]][["publish_divisions"]]
  for (sex_key in c("male", "female")) {
    bd <- Filter(function(e) identical(e$code, "BD"), cfg[[sex_key]])[[1]]
    expect_identical(bd$qualify, list(slots = 6L, label_is = "Efri hluti"))
    others <- Filter(function(e) !identical(e$code, "BD"), cfg[[sex_key]])
    for (entry in others) {
      expect_null(entry$qualify, info = sprintf("sex=%s code=%s", sex_key, entry$code))
    }
  }
})
