# R/extract-football-iceland.R — split-state derivation suite.
#
# .league_split_state_pfi() decides, for a division with a configured split
# (efri/nedri hluti), which simulator phase applies and assembles its inputs:
#   phase 1 (regular ongoing)  -> regular base/remaining + split_format only;
#   phase 2 (regular complete) -> carry-over base incl. played split matches,
#     membership from the realised table overridden by observed playoff-
#     division appearances, remaining split fixtures = scheduled (valid teams,
#     unplayed) + KSI-template completion for missing pairs.
# Format facts: docs/superpowers/specs/2026-07-10-split-season-simulator-design.md.

# `.mini_reg_results()` comes from helper-split-season.R.

.ctt <- function(teams) tibble::tibble(team = teams)

.empty_schedule <- function() {
  tibble::tibble(
    home_team = character(), away_team = character(),
    division = character(), match_date = as.Date(character())
  )
}

.split_cfg_44 <- list(upper = 4L, lower = 4L)

test_that("no split config: flat passthrough with NULL split fields", {
  teams <- LETTERS[1:4]
  res <- .mini_reg_results(teams, skip_pairs = c("A B"))
  st <- .league_split_state_pfi(
    results = res, current_season = 2026L, current_top_teams = .ctt(teams),
    season_schedule = .empty_schedule(), target_div = "BD",
    multiplicity = 2L, split_config = NULL
  )
  br <- .league_base_and_remaining_pfi(
    res, .ctt(teams), .empty_schedule(), "BD",
    multiplicity = 2L
  )
  expect_equal(st$base_standings, br$base_standings)
  expect_equal(st$remaining_fixtures, br$remaining_fixtures)
  expect_null(st$split_format)
  expect_null(st$split_groups)
})

test_that("phase 1 (regular ongoing): split_format forwarded, no groups", {
  teams <- LETTERS[1:8]
  res <- .mini_reg_results(teams, skip_pairs = c("A B", "C D"))
  st <- .league_split_state_pfi(
    results = res, current_season = 2026L, current_top_teams = .ctt(teams),
    season_schedule = .empty_schedule(), target_div = "BD",
    multiplicity = 2L, split_config = .split_cfg_44
  )
  expect_equal(nrow(st$remaining_fixtures), 2L)
  expect_equal(st$split_format, .split_cfg_44)
  expect_null(st$split_groups)
})

test_that("phase 2 (regular complete): groups from the realised table, template fixtures", {
  teams <- LETTERS[1:8]
  res <- .mini_reg_results(teams)
  st <- .league_split_state_pfi(
    results = res, current_season = 2026L, current_top_teams = .ctt(teams),
    season_schedule = .empty_schedule(), target_div = "BD",
    multiplicity = 2L, split_config = .split_cfg_44
  )

  expect_equal(st$split_format, .split_cfg_44)
  expect_setequal(
    st$split_groups$team[st$split_groups$group == "upper"], c("A", "B", "C", "D")
  )
  expect_setequal(
    st$split_groups$team[st$split_groups$group == "lower"], c("E", "F", "G", "H")
  )

  # Full 4-team template per group, oriented by regular-season rank:
  # rank1 hosts {2,3}; rank2 hosts {3,4}; rank3 hosts {4}; rank4 hosts {1}.
  expect_setequal(
    paste(st$remaining_fixtures$home_team, st$remaining_fixtures$away_team),
    c(
      "A B", "A C", "B C", "B D", "C D", "D A",
      "E F", "E G", "F G", "F H", "G H", "H E"
    )
  )
})

test_that("phase 2: played split matches enter the carry-over base and leave the fixture set", {
  teams <- LETTERS[1:8]
  reg <- .mini_reg_results(teams)
  po <- tibble::tibble(
    home_team = "A", away_team = "B",
    home_score = 2L, away_score = 0L,
    division = "BD_UPPER_PO", season = 2026L,
    match_date = as.Date("2026-09-13")
  )
  st <- .league_split_state_pfi(
    results = dplyr::bind_rows(reg, po), current_season = 2026L,
    current_top_teams = .ctt(teams),
    season_schedule = .empty_schedule(), target_div = "BD",
    multiplicity = 2L, split_config = .split_cfg_44
  )

  reg_only <- .league_split_state_pfi(
    results = reg, current_season = 2026L, current_top_teams = .ctt(teams),
    season_schedule = .empty_schedule(), target_div = "BD",
    multiplicity = 2L, split_config = .split_cfg_44
  )
  a_reg <- reg_only$base_standings[reg_only$base_standings$team == "A", ]
  a_now <- st$base_standings[st$base_standings$team == "A", ]
  expect_equal(a_now$base_points, a_reg$base_points + 3L)
  expect_equal(a_now$base_gd, a_reg$base_gd + 2L)
  expect_equal(a_now$base_gf, a_reg$base_gf + 2L)

  pairs <- paste(st$remaining_fixtures$home_team, st$remaining_fixtures$away_team)
  expect_false("A B" %in% pairs)
  expect_false("B A" %in% pairs)
  expect_equal(length(pairs), 11L)
})

test_that("phase 2: scheduled split fixtures keep their orientation; placeholders are ignored", {
  teams <- LETTERS[1:8]
  reg <- .mini_reg_results(teams)
  sched <- tibble::tibble(
    # Real scheduled fixture with reversed orientation vs the template (which
    # would generate "A B"), plus a KSI placeholder row (round label / dot).
    home_team = c("B", "23. Umfer\u00f0"),
    away_team = c("A", "."),
    division = c("BD_UPPER_PO", "BD_UPPER_PO"),
    match_date = as.Date(c("2026-09-13", "2026-09-20"))
  )
  st <- .league_split_state_pfi(
    results = reg, current_season = 2026L, current_top_teams = .ctt(teams),
    season_schedule = sched, target_div = "BD",
    multiplicity = 2L, split_config = .split_cfg_44
  )
  pairs <- paste(st$remaining_fixtures$home_team, st$remaining_fixtures$away_team)
  expect_true("B A" %in% pairs)
  expect_false("A B" %in% pairs)
  expect_equal(length(pairs), 12L)
})

test_that("phase 2: observed playoff appearances override the computed groups", {
  teams <- LETTERS[1:8]
  reg <- .mini_reg_results(teams)
  # Computed ranking puts D 4th (upper) and E 5th (lower); observation says
  # the opposite (e.g. KSI's deeper tiebreak went the other way).
  po <- tibble::tibble(
    home_team = c("E", "D"), away_team = c("A", "F"),
    home_score = c(1L, 1L), away_score = c(1L, 1L),
    division = c("BD_UPPER_PO", "BD_LOWER_PO"), season = 2026L,
    match_date = as.Date("2026-09-13")
  )
  st <- .league_split_state_pfi(
    results = dplyr::bind_rows(reg, po), current_season = 2026L,
    current_top_teams = .ctt(teams),
    season_schedule = .empty_schedule(), target_div = "BD",
    multiplicity = 2L, split_config = .split_cfg_44
  )
  g <- st$split_groups
  expect_equal(g$group[g$team == "E"], "upper")
  expect_equal(g$group[g$team == "D"], "lower")
  expect_setequal(g$team[g$group == "upper"], c("A", "B", "C", "E"))
})

test_that(".split_family_divisions_pfi: flat cell -> own code; split cell -> family", {
  expect_equal(.split_family_divisions_pfi("BD", NULL), "BD")
  expect_equal(
    .split_family_divisions_pfi("BD", list(upper = 6L, lower = 6L)),
    c("BD", "BD_UPPER_PO", "BD_LOWER_PO")
  )
  expect_equal(
    .split_family_divisions_pfi("LD1", list(upper = 4L, lower = 4L)),
    c("LD1", "LD1_UPPER_PO", "LD1_LOWER_PO")
  )
})
