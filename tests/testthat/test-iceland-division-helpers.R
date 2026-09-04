# Sport-neutral publish-division accessors (R/publish-divisions.R).
#
# Blocks A and B pinned the accessors byte-for-byte against the five
# five football-only division helpers they replace, in the same process, while
# both were live. Those helpers are now deleted (spec section 9: no compatibility
# aliases), so the same values are asserted against the literals that
# equivalence proved -- football's nine published cells must not move.

test_that(".iceland_division_* return football's configured cells", {
  expect_identical(
    .iceland_division_codes("football_iceland", "male"),
    c("BD", "LD1", "LD2", "LD3", "CUP")
  )
  expect_identical(
    .iceland_division_codes("football_iceland", "female"),
    c("BD", "LD1", "LD2", "CUP")
  )
  expect_identical(
    .iceland_division_slugs("football_iceland", "male"),
    c(BD = "bd", LD1 = "ld", LD2 = "2deild", LD3 = "3deild", CUP = "bikar")
  )
  expect_identical(
    .iceland_division_slugs("football_iceland", "female"),
    c(BD = "bd", LD1 = "ld", LD2 = "2deild", CUP = "bikar")
  )
  expect_identical(
    unname(.iceland_division_labels("football_iceland", "male")[["CUP"]]),
    "Mj\u00f3lkurbikar"
  )
  expect_identical(
    unname(.iceland_division_labels("football_iceland", "female")[["CUP"]]),
    "Bikar kvenna"
  )
  expect_identical(
    .iceland_division_split("football_iceland", "male")$BD,
    list(upper = 6L, lower = 6L)
  )
  expect_identical(
    .iceland_division_split("football_iceland", "female")$BD,
    list(upper = 6L, lower = 4L)
  )
  expect_null(.iceland_division_split("football_iceland", "male")$LD1)
})

test_that(".iceland_division_badges reproduces the retired static map", {
  # These five are byte-identical to the values
  # the retired football-only static badge map carried before the cutover, and
  # metill-platform's DIVISIONS dict mirrors them.
  legacy <- c(BD = "BD", LD1 = "LD", LD2 = "D2", LD3 = "D3", CUP = "MB")
  for (sex_key in c("male", "female")) {
    badges <- .iceland_division_badges("football_iceland", sex_key)
    shared <- intersect(names(badges), names(legacy))
    expect_gt(length(shared), 0L)
    expect_identical(badges[shared], legacy[shared], info = sex_key)

    # Derived from BD's `split` object: the publisher recodes the split-phase
    # playoff divisions BD_UPPER_PO / BD_LOWER_PO through the same map.
    expect_identical(badges[["BD_UPPER_PO"]], "BDU")
    expect_identical(badges[["BD_LOWER_PO"]], "BDL")

    # LD1_PO and LD4 were in the retired static map but are NOT here,
    # deliberately. publish_football_iceland() filters
    # `division %in% family_divs` before the recode, and family_divs is the
    # target division plus only its own _UPPER_PO/_LOWER_PO
    # (.split_family_divisions_pfi), so neither code can reach a payload. The
    # football golden test is the empirical check on that trace.
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

test_that("no legacy football-only division helper survives in the live tree", {
  # Spec section 9: no compatibility aliases. Two live names for one symbol is
  # exactly the drift the rename removes, so the absence is asserted rather
  # than assumed.
  #
  # Scope is deliberately R/, scripts/, tests/testthat/ and man/ only.
  # .claude/worktrees/ holds stale checkouts of the same files, and docs/ plus
  # _legacy/ hold historical prose; a tree-wide grep could never go green.
  root <- normalizePath(testthat::test_path("..", ".."))
  hits <- withr::with_dir(root, suppressWarnings(system2(
    "grep",
    c(
      "-rl", "--include=*.R", "--include=*.Rd", "--include=*.qmd",
      # Assembled from pieces so this file is not itself a hit -- the grep
      # scope deliberately includes tests/testthat.
      shQuote(paste0("\\.football", "_iceland_division_")),
      "R", "scripts", "tests/testthat", "man"
    ),
    stdout = TRUE
  )))
  expect_length(hits, 0L)
  if (length(hits) > 0L) {
    cat("\nsurviving references in:\n", paste(hits, collapse = "\n"), "\n")
  }
})
