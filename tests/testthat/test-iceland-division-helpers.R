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
    # deliberately. publish_iceland_league() filters
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

test_that(".iceland_division_regular_season_rounds names exactly one cell", {
  # It is the escape hatch for a format `expected_meetings` cannot describe, so
  # it should stay rare: any second cell carrying it is a claim that a second
  # federation calendar is irregular, and that claim wants its own measurement.
  configured <- list()
  for (key in c("football_iceland", "basketball_iceland", "handball_iceland")) {
    for (sex_key in c("male", "female")) {
      codes <- .iceland_division_codes(key, sex_key)
      rsr <- .iceland_division_regular_season_rounds(key, sex_key)
      expect_type(rsr, "integer")
      expect_length(rsr, length(codes))
      expect_identical(names(rsr), codes)
      for (code in codes[!is.na(rsr)]) {
        configured[[length(configured) + 1L]] <- c(key, sex_key, code)
      }
    }
  }
  expect_identical(configured, list(c("basketball_iceland", "female", "1D")))
  expect_identical(
    .iceland_division_regular_season_rounds("basketball_iceland", "female")[["1D"]],
    18L
  )
})

test_that("regular_season_rounds is re-derived from data/facts/results, not restated", {
  # The same contract the expected_meetings block above holds itself to: the
  # constant is a MEASUREMENT, and a federation format change is supposed to
  # turn this red so the number is re-measured rather than the test loosened.
  #
  # basketball female 1D, season 2026: rounds 1-18 are the regular season and
  # rounds 19-24 are a 4-team promotion playoff that brings in an eleventh team.
  results <- read_table("results", root = testthat::test_path("..", "..", "data"))
  cell <- results[
    results$sport == "basketball" & results$country == "iceland" &
      results$sex == "female" & results$division == "1D" &
      results$season == 2026L, ,
    drop = FALSE
  ]
  boundary <- .iceland_division_regular_season_rounds(
    "basketball_iceland", "female"
  )[["1D"]]
  expect_equal(boundary, 18L)

  regular <- cell[cell$round <= boundary, , drop = FALSE]
  post <- cell[cell$round > boundary, , drop = FALSE]
  expect_equal(nrow(cell), 98L)
  expect_equal(nrow(regular), 89L)
  expect_equal(nrow(post), 9L)

  reg_teams <- unique(c(regular$home_team, regular$away_team))
  all_teams <- unique(c(cell$home_team, cell$away_team))
  post_teams <- unique(c(post$home_team, post$away_team))
  expect_length(reg_teams, 10L)
  expect_length(all_teams, 11L)
  # The whole reason `expected_meetings * (n_teams - 1)` cannot express this:
  # the post-season introduces a team that plays no regular round at all.
  expect_length(post_teams, 4L)
  expect_length(setdiff(all_teams, reg_teams), 1L)

  # ... and inside the cut the pair table is not a constant either.
  pair <- paste(
    pmin(regular$home_team, regular$away_team),
    pmax(regular$home_team, regular$away_team),
    sep = " v "
  )
  expect_setequal(as.integer(unique(table(pair))), c(1L, 2L))
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
    expected_meetings = .iceland_division_expected_meetings,
    regular_season_rounds = .iceland_division_regular_season_rounds
  )
  expect_length(accessors, 10L)
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

# ---- basketball + handball publish cells (Plan B WS7 task 5) ----------------

test_that(".iceland_division_* return the basketball and handball cells", {
  expected <- list(
    basketball_iceland = list(
      codes = c("BD", "1D"),
      slugs = c(BD = "bd", `1D` = "1d"),
      labels = c(BD = "B\u00f3nusdeild", `1D` = "1. deild"),
      badges = c(BD = "BON", `1D` = "B1D")
    ),
    handball_iceland = list(
      codes = c("OD", "G66"),
      slugs = c(OD = "od", G66 = "g66"),
      labels = c(OD = "Ol\u00edsdeild", G66 = "Grill 66-deild"),
      badges = c(OD = "OD", G66 = "G66")
    )
  )
  for (key in names(expected)) {
    e <- expected[[key]]
    for (sex_key in c("male", "female")) {
      info <- paste(key, sex_key)
      expect_identical(.iceland_division_codes(key, sex_key), e$codes, info = info)
      expect_identical(.iceland_division_slugs(key, sex_key), e$slugs, info = info)
      expect_identical(.iceland_division_labels(key, sex_key), e$labels, info = info)
      expect_identical(.iceland_division_badges(key, sex_key), e$badges, info = info)
      cup <- .iceland_division_is_cup(key, sex_key)
      expect_false(any(cup), info = info)
      # No split-season format in either sport, so no derived _PO badges.
      expect_identical(names(.iceland_division_badges(key, sex_key)), e$codes, info = info)
    }
  }
})

test_that("every configured badge satisfies the next_games division_code pattern", {
  # The one hard constraint on a badge. Basketball's code "1D" violates it on
  # its own (leading digit), which is why code_badge exists as a separate key.
  schema <- jsonlite::fromJSON(testthat::test_path(
    "..", "..", "config", "publish-schemas", "football", "next_games.schema.json"
  ))
  pattern <- schema$properties$matches$items$properties$division_code$pattern
  expect_true(nzchar(pattern))
  n_checked <- 0L
  for (key in c("football_iceland", "basketball_iceland", "handball_iceland")) {
    for (sex_key in c("male", "female")) {
      badges <- .iceland_division_badges(key, sex_key)
      for (code in names(badges)) {
        expect_match(
          badges[[code]], pattern,
          info = sprintf("%s %s %s", key, sex_key, code)
        )
        n_checked <- n_checked + 1L
      }
    }
  }
  expect_gt(n_checked, 0L)
})

test_that("expected_meetings is re-derived from data/facts/results, not restated", {
  # This asserts against live git-tracked results. When a federation changes a
  # competition format between seasons it is SUPPOSED to go red; the fix is to
  # re-measure and rewrite the constant, never to loosen the assertion.
  results <- read_table("results", root = testthat::test_path("..", "..", "data"))
  season_max <- max(results$season, na.rm = TRUE)
  n_checked <- 0L
  for (key in c("basketball_iceland", "handball_iceland")) {
    sport <- sub("_iceland$", "", key)
    for (sex_key in c("male", "female")) {
      em <- .iceland_division_expected_meetings(key, sex_key)
      for (code in names(em)) {
        cell <- results[
          results$sport == sport & results$sex == sex_key &
            results$season == season_max & results$division == code,
        ]
        if (nrow(cell) == 0L || is.na(em[[code]])) {
          next
        }
        teams <- unique(c(cell$home_team, cell$away_team))
        cut <- em[[code]] * (length(teams) - 1L)
        reg <- cell[!is.na(cell$round) & cell$round <= cut, ]
        pair <- paste(
          pmin(reg$home_team, reg$away_team),
          pmax(reg$home_team, reg$away_team),
          sep = " v "
        )
        meetings <- table(pair)
        info <- sprintf("%s %s %s season %d", key, sex_key, code, season_max)
        # Every pair inside the regular-season cut meets exactly
        # expected_meetings times, and every pair is present.
        expect_setequal(as.integer(unique(meetings)), em[[code]])
        expect_identical(
          length(meetings),
          as.integer(length(teams) * (length(teams) - 1L) / 2L),
          info = info
        )
        n_checked <- n_checked + 1L
      }
    }
  }
  # 7 of the 8 cells carry expected_meetings; basketball female 1D is
  # deliberately unset (an irregular 11-team cell -- 46 of 55 possible pairs
  # inside the cut, meeting 1x/2x/4x -- so no constant is correct and the
  # schedule derivation must be the only source).
  expect_identical(n_checked, 7L)
})

test_that("basketball and handball configure no qualify cut (Plan B ID-B15)", {
  # Measured teams reaching the post-season, season 2026: bb male BD 8 of 12,
  # male 1D 8 of 12, female BD 10 of 10, female 1D 4 of 11. Four cells, four
  # structures, and women's Bonusdeild carries every team through. No single
  # per-division integer expresses that, so the key is absent and the publisher
  # emits meta.qualify: null with no p_qualify rather than a plausible-looking
  # number wearing a playoff label.
  for (key in c("basketball_iceland", "handball_iceland")) {
    for (sex_key in c("male", "female")) {
      q <- .iceland_division_qualify(key, sex_key)
      expect_length(q, length(.iceland_division_codes(key, sex_key)))
      for (code in names(q)) {
        expect_null(q[[code]], info = sprintf("%s %s %s", key, sex_key, code))
      }
    }
  }
})

test_that("the publish surface matches what is actually ingested", {
  # Nothing is published that is not ingested, and nothing ingested is silently
  # dropped -- except handball's PO, the shared post-season bracket, which is a
  # division in results but not a league to publish. No CUP division exists for
  # either sport (distinct 2026 values are basketball {BD, 1D} and handball
  # {OD, G66, PO}), which is why every bb/hb entry is is_cup: false and there
  # is no cup cell to build.
  results <- read_table("results", root = testthat::test_path("..", "..", "data"))
  season_max <- max(results$season, na.rm = TRUE)
  not_a_publish_division <- "PO"
  n_checked <- 0L
  for (key in c("basketball_iceland", "handball_iceland")) {
    sport <- sub("_iceland$", "", key)
    for (sex_key in c("male", "female")) {
      ingested <- unique(results$division[
        results$sport == sport & results$sex == sex_key &
          results$season == season_max
      ])
      expect_gt(length(ingested), 0L)
      expect_setequal(
        .iceland_division_codes(key, sex_key),
        setdiff(ingested, not_a_publish_division)
      )
      n_checked <- n_checked + 1L
    }
  }
  expect_identical(n_checked, 4L)
})
