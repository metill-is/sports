# UEFA qualifying group rules (R/nt-qualifying.R). The rule references are to
# the Regulations of the UEFA European Football Championship 2026-28, Article
# 15 (equality of points) and Article 23 (overall rankings).

test_that("group statistics count away goals and away wins for the listed away side", {
  m <- nt_q_matches(
    "A", "B", 1, 2,
    "B", "A", 0, 0,
    "A", "C", 3, NA
  )
  s <- nt_uefa_group_stats(m, c("A", "B", "C"))
  expect_equal(s$played, c(2, 2, 0))
  expect_equal(s$points, c(1, 4, 0))
  expect_equal(s$away_gf, c(0, 2, 0))
  expect_equal(s$away_won, c(0, 1, 0))
  expect_equal(s$gd, c(-1, 1, 0))
})

test_that("a) head-to-head points rank above a better overall goal difference", {
  m <- nt_q_matches(
    "A", "B", 1, 0,
    "B", "A", 0, 0,
    "A", "C", 1, 0,
    "C", "A", 1, 0,
    "B", "C", 5, 0,
    "C", "B", 0, 5
  )
  r <- nt_rank_uefa_group(m)
  expect_equal(r$team, c("A", "B", "C"))
  expect_equal(r$points, c(7, 7, 3))
  expect_gt(r$gd[2], r$gd[1])
  expect_equal(r$decided_by, c("a_h2h_points", "points", ""))
})

test_that("d) head-to-head criteria are re-applied to the teams still level", {
  # A, B and C are level on 6 points and on head-to-head points and goal
  # difference; head-to-head goals put A last; B and C are then separated by
  # their own match (B won), although C has more away goals overall and
  # would come first if the re-application were skipped.
  m <- nt_q_matches(
    "A", "B", 1, 0,
    "C", "A", 1, 0,
    "B", "C", 2, 1,
    "A", "D", 3, 0,
    "B", "D", 3, 0,
    "C", "D", 3, 0
  )
  r <- nt_rank_uefa_group(m)
  expect_equal(r$team, c("B", "C", "A", "D"))
  expect_equal(r$decided_by, c("a_h2h_points (re-applied)", "c_h2h_goals_for", "points", ""))
  expect_gt(r$away_gf[r$team == "C"], r$away_gf[r$team == "B"])
})

test_that("e) and g) apply when the head-to-head record is level", {
  by_gd <- nt_q_matches(
    "A", "B", 1, 1,
    "B", "A", 1, 1,
    "A", "C", 3, 0,
    "B", "C", 1, 0
  )
  r <- nt_rank_uefa_group(by_gd)
  expect_equal(r$team[1:2], c("A", "B"))
  expect_equal(r$decided_by[1], "e_goal_difference")

  by_away <- nt_q_matches(
    "A", "B", 1, 1,
    "B", "A", 1, 1,
    "A", "C", 2, 0,
    "C", "B", 0, 2
  )
  r <- nt_rank_uefa_group(by_away)
  expect_equal(r$team[1:2], c("B", "A"))
  expect_equal(r$decided_by[1], "g_away_goals")
  expect_equal(r$gd[1:2], c(2, 2))
  expect_equal(r$gf[1:2], c(4, 4))
})

test_that("j) and k) decide only when known, and an unknown j) stops the cascade", {
  m <- nt_q_matches(
    "A", "B", 1, 1,
    "B", "A", 1, 1
  )
  open <- nt_rank_uefa_group(m)
  expect_true(open$unresolved[1])
  expect_equal(open$decided_by[1], "")

  r <- nt_rank_uefa_group(m, disciplinary = c(A = 5, B = 3))
  expect_equal(r$team, c("B", "A"))
  expect_equal(r$decided_by[1], "j_disciplinary")
  expect_false(any(r$unresolved))

  stuck <- nt_rank_uefa_group(m, disciplinary = c(A = 5), nl_rank = c(A = 10, B = 4))
  expect_true(stuck$unresolved[1])

  r <- nt_rank_uefa_group(m, disciplinary = c(A = 3, B = 3), nl_rank = c(A = 10, B = 4))
  expect_equal(r$team, c("B", "A"))
  expect_equal(r$decided_by[1], "k_nations_league_rank")
})

test_that("the Euro 2024 qualifying group tables are reproduced from the results", {
  e <- nt_e24_fixture()
  expect_equal(nrow(e$matches), 230L)
  note_criterion <- function(note) {
    ifelse(grepl("^Head.to.head points", note), "a_h2h_points",
      ifelse(grepl("^Goal difference in all group matches", note), "e_goal_difference",
        ifelse(nzchar(note), "unknown note", "points")
      )
    )
  }
  for (g in names(e$groups)) {
    r <- nt_rank_uefa_group(e$matches[e$matches$group == g, ], e$groups[[g]], nl_rank = e$nl_rank)
    want <- e$tables[e$tables$group == g, ]
    expect_identical(r$team, want$team, info = paste("Group", g))
    for (col in c("won", "drawn", "lost", "gf", "ga", "points")) {
      expect_equal(r[[col]], want[[col]], info = paste("Group", g, col))
    }
    expected <- note_criterion(want$tiebreak_note)
    expected[length(expected)] <- ""
    expect_identical(r$decided_by, expected, info = paste("Group", g))
    expect_false(any(r$unresolved))
  }
})

test_that("the Euro 2024 tie-breaks include head-to-head and overall criteria", {
  e <- nt_e24_fixture()
  rank_group <- function(g) nt_rank_uefa_group(e$matches[e$matches$group == g, ], e$groups[[g]])
  c_tab <- rank_group("C")
  expect_equal(c_tab$team[2:3], c("Italy", "Ukraine"))
  expect_equal(c_tab$points[2:3], c(14, 14))
  expect_equal(c_tab$decided_by[2], "a_h2h_points")
  h_tab <- rank_group("H")
  expect_equal(h_tab$team[1:4], c("Denmark", "Slovenia", "Finland", "Kazakhstan"))
  expect_equal(h_tab$decided_by[c(1, 3)], c("a_h2h_points", "e_goal_difference"))
})

test_that("the Euro 2024 overall ranking is reproduced, discarding results against sixth place", {
  e <- nt_e24_fixture()
  o <- nt_uefa_overall_ranking(e$matches, e$groups, e$disciplinary, e$nl_rank)
  expect_identical(o$team, e$overall$team)
  w <- e$overall[match(o$team, e$overall$team), ]
  # Wikipedia's overall table gives Iceland 7 goals; its own scores give 6
  # (17 in the group table, 11 of them against sixth-placed Liechtenstein).
  iceland <- o$team == "Iceland"
  expect_equal(o$gf[iceland], 6)
  expect_equal(w$gf[iceland], 7)
  for (col in c("won", "drawn", "lost", "ga", "points")) {
    expect_equal(o[[col]], w[[col]], info = col)
  }
  expect_equal(o$gf[!iceland], w$gf[!iceland])
  expect_equal(o$decided_by[o$team == "Belgium"], "away_goals")
  expect_equal(o$decided_by[o$team == "Bosnia and Herzegovina"], "disciplinary")
  expect_identical(
    o$team[o$group_position == 2],
    c(
      "Austria", "Netherlands", "Scotland", "Croatia", "Slovenia", "Slovakia",
      "Czech Republic", "Italy", "Serbia", "Switzerland"
    )
  )
  slovenia <- o[o$team == "Slovenia", ]
  expect_equal(slovenia$points, 16)
  expect_equal(slovenia$played, 8)
})

test_that("without disciplinary points the overall ranking leaves that tie open", {
  e <- nt_e24_fixture()
  o <- nt_uefa_overall_ranking(e$matches, e$groups, nl_rank = e$nl_rank)
  pair <- o[o$team %in% c("Bosnia and Herzegovina", "Latvia"), ]
  expect_equal(diff(pair$rank), 1)
  expect_true(pair$unresolved[1])
  expect_identical(o$team[-(44:45)], e$overall$team[-(44:45)])
})

nt_random_group_check <- function(k, n, rate, seed) {
  withr::local_seed(seed)
  teams <- paste0("T", seq_len(k))
  fx <- nt_qualifying_fixtures(list(G = teams))
  gh <- matrix(stats::rpois(n * nrow(fx), rate), n)
  ga <- matrix(stats::rpois(n * nrow(fx), rate), n)
  x <- .nt_group_arrays(fx$home_team, fx$away_team, teams, gh, ga)
  tb <- matrix(stats::runif(n * k), n, k)
  pos <- .nt_rank_group_arrays(x, tb)
  agree <- logical(n)
  criteria <- character()
  for (s in seq_len(n)) {
    m <- data.frame(
      home_team = fx$home_team, away_team = fx$away_team,
      home_score = gh[s, ], away_score = ga[s, ]
    )
    ref <- nt_rank_uefa_group(m, teams, disciplinary = stats::setNames(-tb[s, ], teams))
    agree[s] <- identical(as.integer(pos[s, match(ref$team, teams)]), seq_len(k))
    criteria <- c(criteria, ref$decided_by)
  }
  list(agree = agree, criteria = table(criteria))
}

test_that("the vectorised ranking agrees with the reference on random low-scoring groups", {
  # Re-application (d) is rare, about 2 in 1,000 random tables, so it is
  # checked over all three group sizes together (the seeds give 17 cases).
  reapplied <- 0
  for (k in c(4L, 5L, 6L)) {
    res <- nt_random_group_check(k, n = 2000L, rate = 0.45, seed = 7 + k)
    expect_true(all(res$agree), info = paste(k, "teams:", sum(!res$agree), "disagreements"))
    used <- names(res$criteria)
    expect_true(all(c("points", "a_h2h_points", "e_goal_difference", "j_disciplinary") %in% used),
      info = paste(used, collapse = ", ")
    )
    reapplied <- reapplied + sum(res$criteria[grepl("re-applied", used)])
  }
  expect_gt(reapplied, 10)
})

test_that("the vectorised ranking handles the hand-built re-application case", {
  teams <- c("A", "B", "C", "D")
  m <- nt_q_matches(
    "A", "B", 1, 0,
    "C", "A", 1, 0,
    "B", "C", 2, 1,
    "A", "D", 3, 0,
    "B", "D", 3, 0,
    "C", "D", 3, 0
  )
  x <- .nt_group_arrays(
    m$home_team, m$away_team, teams,
    matrix(m$home_score, 1), matrix(m$away_score, 1)
  )
  pos <- .nt_rank_group_arrays(x, matrix(c(0.9, 0.1, 0.5, 0.3), 1))
  expect_equal(as.integer(pos[1, ]), c(3L, 1L, 2L, 4L))
})

test_that("lexicographic ranks share a position only when every key is level", {
  r <- .nt_lexi_rank(list(matrix(c(1, 1, 0), 1), matrix(c(2, 2, 9), 1)))
  expect_equal(as.integer(r), c(1L, 1L, 3L))
})

test_that("fixtures are a double round robin per group", {
  fx <- nt_qualifying_fixtures(list(A = c("x", "y", "z"), B = c("p", "q", "r", "s")))
  expect_equal(nrow(fx), 3 * 2 + 4 * 3)
  expect_true(all(is.na(fx$home_score)))
  expect_equal(nrow(unique(fx[, c("home_team", "away_team")])), nrow(fx))
  expect_true(all(fx$venue == 1L))
})

test_that("simulating the played Euro 2024 groups returns the real tables in every run", {
  e <- nt_e24_fixture()
  fit <- nt_test_fit(stats::setNames(rep(0, 53), unlist(e$groups)))
  fx <- data.frame(
    group = e$matches$group, home_team = e$matches$home_team,
    away_team = e$matches$away_team, home_score = e$matches$home_score,
    away_score = e$matches$away_score, venue = as.integer(!e$matches$neutral)
  )
  s <- nt_simulate_qualifying(fit, e$groups, fx, n_sims = 50L, seed = 1L)
  for (g in names(e$groups)) {
    want <- e$tables$position[e$tables$group == g]
    expect_true(all(t(s$positions[, e$groups[[g]]]) == want), info = paste("Group", g))
  }
  order_ru <- s$runner_up[1, ][order(s$runner_up_rank[1, ])]
  expect_identical(unname(order_ru), e$overall$team[e$overall$group_position == 2])
  expect_true(all(apply(s$runner_up_rank, 2, function(x) length(unique(x)) == 1L)))
  winners <- s$teams$team[s$teams$p_winner == 1]
  expect_setequal(winners, e$tables$team[e$tables$position == 1])
})

test_that("a Euro 2028-shaped simulation fills the places exactly", {
  withr::local_seed(5)
  teams <- paste0("T", 1:55)
  groups <- split(teams, rep(LETTERS[1:12], c(rep(5, 7), rep(4, 5))))
  fit <- nt_test_fit(stats::setNames(stats::rnorm(55, 0, 0.5), teams), mu = 0.2, n_draws = 10L)
  s <- nt_simulate_qualifying(fit, groups, n_sims = 1500L, seed = 6L)
  x <- s$teams
  expect_equal(sum(x$p_winner), 12)
  expect_equal(sum(x$p_runner_up_direct), 8)
  expect_equal(sum(x$p_runner_up_other), 4)
  expect_equal(sum(x$p_qualify_direct), 20)
  by_group <- split(x, x$group)
  for (g in names(by_group)) {
    k <- length(groups[[g]])
    sums <- colSums(by_group[[g]][, paste0("p_pos", 1:5)])
    expect_equal(unname(sums), c(rep(1, k), rep(0, 5 - k)), info = g)
  }
  expect_true(all(apply(s$runner_up_rank, 1, function(r) setequal(r, 1:12))))
  pos_ok <- vapply(names(groups), function(g) {
    all(apply(s$positions[, groups[[g]], drop = FALSE], 1, function(p) setequal(p, seq_along(p))))
  }, logical(1))
  expect_true(all(pos_ok))
})

test_that("a far stronger team wins its group and played results stay fixed", {
  groups <- list(A = c("W", "X", "Y", "Z"), B = c("P", "Q", "R", "S"))
  theta <- c(W = 3, X = 0, Y = 0, Z = 0, P = 0, Q = 0, R = 0, S = 0)
  fit <- nt_test_fit(theta, mu = 0.2)
  fx <- nt_qualifying_fixtures(groups)
  b <- fx$group == "B"
  fx$home_score[b] <- c(1L, 2L, 3L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L)
  fx$away_score[b] <- 0L
  s <- nt_simulate_qualifying(fit, groups, fx, n_sims = 400L, n_best_runners_up = 1L, seed = 2L)
  expect_gt(s$teams$p_winner[s$teams$team == "W"], 0.95)
  fixed <- nt_rank_uefa_group(fx[b, ], groups$B)
  for (t in groups$B) {
    expect_equal(mean(s$positions[, t] == fixed$position[fixed$team == t]), 1)
  }
})

test_that("bad inputs are refused", {
  fit <- nt_test_fit(c(A = 0, B = 0, C = 0, D = 0))
  expect_error(nt_simulate_qualifying(fit, list(G = c("A", "B"), H = c("B", "C"))), "more than one group")
  fx <- nt_qualifying_fixtures(list(G = c("A", "B")))
  fx$away_team[1] <- "C"
  expect_error(nt_simulate_qualifying(fit, list(G = c("A", "B")), fx, n_best_runners_up = 1L), "outside the group")
  expect_error(nt_simulate_qualifying(fit, list(G = c("A", "B"))), "at least that many groups")
})
