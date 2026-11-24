# International handball championships: ranking rules and the simulator.

# A fit-shaped list: fixed strengths (margin states), no uncertainty in them.
fake_ih_fit <- function(strength, n_draws = 200L, sigma_m = 5, hfa = 0) {
  teams <- names(strength)
  k <- length(teams)
  m <- matrix(rep(strength, each = n_draws), n_draws, k, dimnames = list(NULL, teams))
  list(
    teams = teams, m_state = m, t_state = m * 0, level = rep(55, n_draws), hfa = rep(hfa, n_draws),
    ha = m * 0, sigma_m = rep(sigma_m, n_draws), sigma_t = rep(6, n_draws)
  )
}

# A small championship in the women's EURO format: groups A-F of four, the top
# two to main-round groups I (A-C) and II (D-F), then the EURO bracket.
fake_spec <- function() {
  teams <- sprintf("T%02d", 1:24)
  groups <- split(teams, rep(LETTERS[1:6], each = 4))
  prelim <- do.call(rbind, lapply(names(groups), function(g) {
    p <- t(utils::combn(groups[[g]], 2))
    data.frame(
      match_no = NA, group = g, match_date = as.Date("2026-12-03"), kickoff = as.POSIXct(NA),
      venue_city = "X", host = NA_character_, home_team = p[, 1], away_team = p[, 2]
    )
  }))
  slots <- function(gs) as.vector(t(outer(gs, 1:2, function(g, p) paste0(p, g))))
  mf <- do.call(rbind, lapply(list(I = LETTERS[1:3], II = LETTERS[4:6]), function(gs) {
    s <- slots(gs)
    pr <- t(utils::combn(s, 2))
    pr <- pr[substr(pr[, 1], 2, 2) != substr(pr[, 2], 2, 2), ]
    data.frame(home_slot = pr[, 1], away_slot = pr[, 2])
  }))
  mf$group <- rep(c("I", "II"), each = 12)
  spec <- IH_TOURNAMENTS$euro2026w
  spec$groups <- groups
  spec$prelim <- prelim
  spec$main_fixtures <- mf
  spec$main_host <- c(I = NA_character_, II = NA_character_)
  spec$knockout$match_date <- as.Date("2026-12-18")
  spec$knockout$host <- NA_character_
  spec
}

test_that("handball points: a win is worth 2, and head-to-head decides before goal difference", {
  # A beat B by 3, B beat C by 1, C beat A by 1; all beat D. Level on 4 points
  # and on head-to-head points; head-to-head goal difference: A +2, C 0, B -2.
  t <- ih_rank_table(
    c("A", "B", "C", "D"),
    home = c("A", "B", "C", "A", "B", "C"), away = c("B", "C", "A", "D", "D", "D"),
    gh = c(30, 28, 26, 40, 30, 41), ga = c(27, 27, 25, 20, 29, 20)
  )
  expect_equal(t$points, c(4, 4, 4, 0))
  expect_equal(t$position, c(1, 3, 2, 4))

  # Three level on 2 points, head-to-head goal difference B +19, A 0, C -19.
  t2 <- ih_rank_table(
    c("A", "B", "C"),
    home = c("A", "A", "B"), away = c("B", "C", "C"),
    gh = c(25, 20, 40), ga = c(24, 21, 20)
  )
  expect_equal(t2$points, c(2, 2, 2))
  expect_equal(t2$position, c(2, 1, 3))

  # Two level on points: their match decides, against a far better goal difference.
  t3 <- ih_rank_table(
    c("A", "B", "C", "D"),
    home = c("A", "C", "A", "B"), away = c("B", "D", "D", "C"),
    gh = c(25, 20, 30, 40), ga = c(24, 30, 31, 10)
  )
  expect_equal(t3$points, c(2, 2, 0, 4))
  expect_lt(t3$position[t3$team == "A"], t3$position[t3$team == "B"])
  expect_gt(t3$gd[t3$team == "B"], t3$gd[t3$team == "A"])
})

test_that("with h2h_reapply none the head-to-head table is not recomputed for the teams still level", {
  # A, B, C level on 4 points, each 2 points among themselves. Among the three,
  # A and B are both +1 with 50 goals, C -2: C is third. "none" then goes on
  # to goal difference in all matches (B +11, A +5); "repeat" recomputes the
  # table of A and B alone, and A won their match.
  home <- c("A", "B", "C", "A", "B", "C")
  away <- c("B", "C", "A", "D", "D", "D")
  gh <- c(26, 26, 25, 30, 36, 30)
  ga <- c(24, 23, 24, 26, 26, 20)
  none <- ih_rank_table(c("A", "B", "C", "D"), home, away, gh, ga, h2h_reapply = "none")
  rep <- ih_rank_table(c("A", "B", "C", "D"), home, away, gh, ga, h2h_reapply = "repeat")
  expect_equal(none$points, c(4, 4, 4, 0))
  expect_equal(none$position, c(2, 1, 3, 4))
  expect_equal(rep$position, c(1, 2, 3, 4))
})

test_that("the simulated probabilities add up: places, rounds and the title", {
  set.seed(1)
  spec <- fake_spec()
  strength <- stats::setNames(rep(c(8, 2, -2, -8), 6) + rep(seq(5, -5, length.out = 6), each = 4), unlist(spec$groups))
  fit <- fake_ih_fit(strength)
  sim <- ih_simulate_tournament(fit, spec, n_sims = 2000L, seed = 3)
  tm <- sim$teams
  expect_equal(unname(rowSums(tm[, paste0("p_pos", 1:4)])), rep(1, 24))
  expect_equal(as.vector(tapply(tm$p_advance, tm$group, sum)), rep(2, 6))
  expect_equal(sum(tm$p_MR), 12)
  expect_equal(sum(tm$p_SF), 4)
  expect_equal(sum(tm$p_Final), 2)
  expect_equal(sum(tm$p_Champion), 1)
  expect_true(all(tm$p_Champion <= tm$p_Final & tm$p_Final <= tm$p_SF & tm$p_SF <= tm$p_MR))
  for (G in names(sim$main)) {
    d <- sim$main[[G]]
    expect_equal(sum(d$p_in), 6)
    expect_equal(unname(colSums(d[, paste0("p_pos", 1:6)])), rep(1, 6))
  }
  # The strongest nation of each group tops it most often.
  top <- tm[!duplicated(tm$group), ]
  expect_true(all(top$p_pos1 > 0.5))
})

test_that("played results are kept: a finished preliminary round fixes who goes through", {
  spec <- fake_spec()
  strength <- stats::setNames(rep(0, 24), unlist(spec$groups))
  fit <- fake_ih_fit(strength)
  pf <- spec$prelim
  # The team listed later in the group wins every match by five.
  res <- data.frame(
    stage = "preliminary", match_date = pf$match_date, home_team = pf$home_team, away_team = pf$away_team,
    home_60 = 25L, away_60 = 30L, home_score = 25L, away_score = 30L, pen_home = NA, pen_away = NA,
    home_wins = FALSE
  )
  sim <- ih_simulate_tournament(fit, spec, results = res, n_sims = 400L, seed = 2)
  tm <- sim$teams
  last_two <- unlist(lapply(spec$groups, function(g) g[3:4]))
  expect_equal(tm$p_advance[tm$team %in% last_two], rep(1, 12))
  expect_equal(tm$p_advance[!tm$team %in% last_two], rep(0, 12))
  expect_equal(tm$proj_points[tm$team == "T04"], 6)
  expect_equal(tm$p_pos1[tm$team == "T04"], 1)
  # Each main-round group now has known members.
  expect_setequal(sim$main$I$team[sim$main$I$p_in == 1], last_two[1:6])
})

test_that("a knockout level after 60 minutes goes to extra time, then penalties", {
  set.seed(4)
  gh <- matrix(25L, 20000, 1)
  hw <- .ih_knockout_home_wins(gh, gh, matrix(30, 20000, 1), rep(5, 20000))
  expect_gt(mean(hw), 0.95)
  hw0 <- .ih_knockout_home_wins(gh, gh, matrix(0, 20000, 1), rep(5, 20000))
  expect_lt(abs(mean(hw0) - 0.5), 0.02)
  expect_true(all(.ih_knockout_home_wins(matrix(30L), matrix(20L), matrix(-50), 5)))
})

test_that("the two championships' specs read: every fixture names group teams", {
  skip_if_not(file.exists(here::here("config", "intl-handball", "teams.csv")))
  for (id in names(IH_TOURNAMENTS)) {
    spec <- ih_tournament_spec(id)
    teams <- unlist(spec$groups)
    expect_length(teams, 4L * length(spec$groups))
    expect_true(all(spec$prelim$home_team %in% teams & spec$prelim$away_team %in% teams))
    expect_equal(nrow(spec$prelim), 6L * length(spec$groups))
    expect_equal(nrow(spec$main_fixtures), length(spec$main) * spec$n_main^2 * choose(length(spec$main[[1]]), 2))
    expect_false(anyNA(spec$prelim$kickoff))
    expect_false(anyNA(spec$knockout$match_date))
  }
})
