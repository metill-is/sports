# Sport-neutral publish-division accessors (R/publish-divisions.R).
#
# Block A is the football-unchanged proof: while the old
# .football_iceland_division_* helpers are still live, the new accessors are
# asserted byte-for-byte against them in the same process. That is a genuine
# equivalence assertion, not a restatement of literals -- once WS7 task 4
# deletes the originals this block is rewritten against config.

test_that(".iceland_division_* reproduce the football-only helpers exactly", {
  for (sex_key in c("male", "female")) {
    expect_identical(
      .iceland_division_codes("football_iceland", sex_key),
      .football_iceland_division_codes(sex_key),
      info = sex_key
    )
    expect_identical(
      .iceland_division_slugs("football_iceland", sex_key),
      .football_iceland_division_slugs(sex_key),
      info = sex_key
    )
    expect_identical(
      .iceland_division_labels("football_iceland", sex_key),
      .football_iceland_division_labels(sex_key),
      info = sex_key
    )
    expect_identical(
      .iceland_division_split("football_iceland", sex_key),
      .football_iceland_division_split(sex_key),
      info = sex_key
    )
  }
})

test_that(".iceland_division_badges reproduces the static map where reachable", {
  legacy <- .football_iceland_division_code_labels()
  for (sex_key in c("male", "female")) {
    badges <- .iceland_division_badges("football_iceland", sex_key)
    shared <- intersect(names(badges), names(legacy))
    expect_gt(length(shared), 0L)
    expect_identical(badges[shared], legacy[shared], info = sex_key)

    # Derived from BD's `split` object: the publisher recodes the split-phase
    # playoff divisions BD_UPPER_PO / BD_LOWER_PO through the same map.
    expect_identical(badges[["BD_UPPER_PO"]], "BDU")
    expect_identical(badges[["BD_LOWER_PO"]], "BDL")

    # LD1_PO and LD4 are in the retired static map but NOT here, deliberately.
    # publish_football_iceland() filters `division %in% family_divs` before the
    # recode, and family_divs is the target division plus only its own
    # _UPPER_PO/_LOWER_PO (.split_family_divisions_pfi). LD1 carries no `split`
    # and LD4 is in no publish_divisions list, so neither code can reach a
    # payload. The football golden test is the empirical check on that trace.
    expect_false("LD1_PO" %in% names(badges))
    expect_false("LD4" %in% names(badges))
  }
})

test_that(".iceland_division_is_cup flags the cup cell only", {
  cup <- .iceland_division_is_cup("football_iceland", "male")
  expect_type(cup, "logical")
  expect_true(cup[["CUP"]])
  expect_false(cup[["BD"]])
  expect_identical(
    names(cup), .iceland_division_codes("football_iceland", "male")
  )
})

test_that(".iceland_division_qualify returns NULL where unconfigured", {
  for (sex_key in c("male", "female")) {
    q <- .iceland_division_qualify("football_iceland", sex_key)
    expect_identical(q$BD, list(slots = 6L, label_is = "Efri hluti"))
    expect_null(q$LD1)
    expect_null(q$CUP)
    expect_identical(
      names(q), .iceland_division_codes("football_iceland", sex_key)
    )
  }
})

test_that(".iceland_division_relegation / _expected_meetings are all-NA for football", {
  for (sex_key in c("male", "female")) {
    codes <- .iceland_division_codes("football_iceland", sex_key)
    rel <- .iceland_division_relegation("football_iceland", sex_key)
    em <- .iceland_division_expected_meetings("football_iceland", sex_key)
    expect_type(rel, "integer")
    expect_type(em, "integer")
    expect_length(rel, length(codes))
    expect_length(em, length(codes))
    expect_identical(names(rel), codes)
    expect_identical(names(em), codes)
    expect_true(all(is.na(rel)))
    expect_true(all(is.na(em)))
  }
})

test_that("every accessor stops on an unknown sex or a league with no publish_divisions", {
  accessors <- list(
    codes = .iceland_division_codes,
    slugs = .iceland_division_slugs,
    labels = .iceland_division_labels,
    split = .iceland_division_split,
    badges = .iceland_division_badges,
    is_cup = .iceland_division_is_cup,
    qualify = .iceland_division_qualify,
    relegation = .iceland_division_relegation,
    expected_meetings = .iceland_division_expected_meetings
  )
  expect_length(accessors, 9L)
  for (nm in names(accessors)) {
    expect_error(accessors[[nm]]("football_iceland", "nonsense"), info = nm)
    # world_cup is a real pipeline namespace but not a config/leagues.yml league,
    # so it has no publish_divisions block at all.
    expect_error(accessors[[nm]]("world_cup", "male"), info = nm)
  }
})
