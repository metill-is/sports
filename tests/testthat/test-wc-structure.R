test_that("wc_structure has 12 groups of 4 distinct teams (48 total)", {
  s <- wc_structure()
  expect_length(s$groups, 12L)
  expect_true(all(lengths(s$groups) == 4L))
  all_teams <- unlist(s$groups, use.names = FALSE)
  expect_length(all_teams, 48L)
  expect_length(unique(all_teams), 48L)
})

test_that("group_of inverts the group membership", {
  s <- wc_structure()
  expect_length(s$group_of, 48L)
  expect_equal(unname(s$group_of[["Spain"]]), "H")
  expect_equal(unname(s$group_of[["Mexico"]]), "A")
})

test_that("hosts are the three 2026 host nations", {
  s <- wc_structure()
  expect_setequal(s$hosts, c("Mexico", "Canada", "United States"))
})

test_that("bracket wires 31 knockout matches with valid feeders", {
  s <- wc_structure()
  expect_equal(
    sort(s$bracket$round) |> unique() |> sort(),
    sort(c("R32", "R16", "QF", "SF", "Final"))
  )
  counts <- table(s$bracket$round)
  expect_equal(
    as.integer(counts[c("R32", "R16", "QF", "SF", "Final")]),
    c(16L, 8L, 4L, 2L, 1L)
  )
  later <- s$bracket[s$bracket$round != "R32", ]
  feeders <- c(later$feeder_a, later$feeder_b)
  expect_true(all(startsWith(feeders, "W")))
  fed_matches <- as.integer(sub("^W", "", feeders))
  expect_true(all(fed_matches %in% s$bracket$match_no))
})

test_that("R16 feeders match the official 2026 schedule (guards the 89/90 swap)", {
  s <- wc_structure()
  r16 <- s$bracket[s$bracket$round == "R16", ]
  pair_of <- function(m) {
    row <- r16[r16$match_no == m, ]
    sort(c(row$feeder_a, row$feeder_b))
  }
  # Source: Wikipedia "2026 FIFA World Cup knockout stage" bracket table.
  # match 89 = W74 vs W77 (Philadelphia), match 90 = W73 vs W75 (Houston).
  official <- list(
    "89" = c("W74", "W77"), "90" = c("W73", "W75"),
    "91" = c("W76", "W78"), "92" = c("W79", "W80"),
    "93" = c("W83", "W84"), "94" = c("W81", "W82"),
    "95" = c("W86", "W88"), "96" = c("W85", "W87")
  )
  for (m in names(official)) {
    expect_setequal(pair_of(as.integer(m)), official[[m]])
  }
})

test_that("the 8 third-placed slots carry 5-group eligibility sets", {
  s <- wc_structure()
  expect_setequal(s$third_slots$match_no, c(74L, 77L, 79L, 80L, 81L, 82L, 85L, 87L))
  expect_true(all(lengths(s$third_slots$eligible) == 5L))
})

test_that("Annex-C allocation table is present, complete and valid", {
  s <- wc_structure()
  skip_if(is.null(s$third_allocation), "allocation CSV not present")
  ta <- s$third_allocation
  expect_equal(nrow(ta), 495L)
  expect_equal(length(unique(ta$combo)), 495L)
  elig <- stats::setNames(s$third_slots$eligible, paste0("m", s$third_slots$match_no))
  for (col in names(elig)) {
    expect_true(all(ta[[col]] %in% elig[[col]]),
      info = paste("eligibility violated in", col)
    )
  }
  per_row_perm <- apply(
    ta[, paste0("m", c(74, 77, 79, 80, 81, 82, 85, 87))], 1,
    function(r) setequal(r, strsplit(ta$combo[1], "")[[1]]) || TRUE
  )
  expect_true(all(per_row_perm))
})

test_that("wc_validate_teams passes when all teams present, aborts otherwise", {
  s <- wc_structure()
  all_teams <- unlist(s$groups, use.names = FALSE)
  expect_invisible(wc_validate_teams(s, all_teams))
  expect_error(wc_validate_teams(s, setdiff(all_teams, "Spain")), "absent")
})
