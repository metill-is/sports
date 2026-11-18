# Football model v2: the shared specification, the INLA engine and the
# adapter onto the production fit's variable names (R/model-v2-*.R). Only the
# engine tests need INLA. Without it they skip, except in the workflow that
# exists to run them, where a missing INLA fails (skip_if_no_inla()).

skip_if_not_installed("Matrix")

test_that("weeks run Monday to Sunday from a fixed Monday", {
  expect_identical(weekdays(FV2_EPOCH), weekdays(as.Date("2024-01-01")))
  monday <- as.Date("2026-07-27")
  expect_identical(fv2_week(monday), fv2_week(monday + 6))
  expect_identical(fv2_week(monday + 7), fv2_week(monday) + 1L)
  expect_identical(fv2_week(monday - 1), fv2_week(monday) - 1L)
})

test_that("a PC prior on the precision is an exponential prior on the sd", {
  rate <- fv2_pc_rate(c(0.3, 0.01))
  expect_equal(stats::pexp(0.3, rate, lower.tail = FALSE), 0.01)
})

test_that("one state slot per team and week, fixtures included", {
  spec <- fv2_build(toy_matches(), toy_fixture())
  expect_identical(spec$teams, c("A", "B", "C"))
  expect_identical(spec$K, 3L)
  # A plays B twice in the week of 2021-05-03: one slot each, not two.
  expect_identical(spec$obs$home_slot[1], spec$obs$away_slot[2])
  expect_identical(spec$obs$away_slot[1], spec$obs$home_slot[2])
  expect_identical(anyDuplicated(spec$slots[c("team", "week")]), 0L)
  expect_identical(spec$S, 12L)
  expect_identical(sum(!spec$slots$has_obs), 2L)
  expect_false(spec$slots$has_obs[spec$pred$home_slot])
  expect_identical(spec$slots$team[spec$first_slot], 1:3)
  expect_identical(nrow(spec$steps), spec$S - spec$K)
})

test_that("a fixture for a team with no training match is refused", {
  bad <- toy_fixture()
  bad$away_team <- "Z"
  expect_error(fv2_build(toy_matches(), bad))
})

test_that("the calendar clock counts weeks, the break clock counts seasons", {
  cal <- fv2_build(toy_matches(), toy_fixture(), clock = "calendar")
  brk <- fv2_build(toy_matches(), toy_fixture(), clock = "break")
  expect_true(all(cal$steps$class == "in"))
  expect_identical(sum(brk$steps$class == "brk"), 3L)
  a_break <- brk$steps$team == 1L & brk$steps$class == "brk"
  expect_equal(brk$steps$delta[a_break], 1)
  expect_equal(
    cal$steps$delta[a_break],
    as.numeric(fv2_week("2022-05-02") - fv2_week("2021-05-12"))
  )
  expect_identical(brk$steps$delta[!a_break & brk$steps$class == "in"], cal$steps$delta[!a_break & brk$steps$class == "in"])
})

test_that("the INLA precision and the Stan steps describe the same prior", {
  tau <- 0.6
  sigma <- 0.05
  kappa <- 0.25
  cal <- fv2_build(toy_matches(), toy_fixture(), clock = "calendar")
  r <- fv2_structure(cal)
  expect_named(r, c("init", "in"))
  q <- r$init / tau^2 + r$`in` / sigma^2
  expect_equal(as.matrix(solve(q)), stan_form_cov(cal, tau, sigma), ignore_attr = TRUE)

  brk <- fv2_build(toy_matches(), toy_fixture(), clock = "break")
  r <- fv2_structure(brk)
  expect_named(r, c("init", "in", "brk"))
  q <- r$init / tau^2 + r$`in` / sigma^2 + r$brk / kappa^2
  expect_equal(as.matrix(solve(q)), stan_form_cov(brk, tau, sigma, kappa), ignore_attr = TRUE)
})

test_that("a step across d weeks has d times the one-week variance", {
  spec <- fv2_build(toy_matches())
  r <- fv2_structure(spec)
  v <- as.matrix(solve(r$init / 0.5^2 + r$`in` / 0.1^2))
  a <- spec$slots$slot[spec$slots$team == 1L]
  weeks <- spec$slots$week[a] - spec$slots$week[a[1]]
  expect_equal(diag(v)[a], 0.5^2 + 0.1^2 * weeks)
})

test_that("only teams that stay in their division revert", {
  rho <- 0.8
  spec <- fv2_build(
    toy_matches(),
    clock = "break", rho = rho, divisions = toy_divisions()
  )
  st <- spec$steps
  # A stays in L1 and reverts; B (relegated) and C (promoted) take plain jumps.
  expect_identical(st$rho[st$class == "brk"], c(rho, 1, 1))
  expect_identical(st$move[st$class == "brk"], c(0, -1, 1))
})

test_that("reversion pulls every stayer toward the mean of its division's stayers", {
  # Written out from the model, not from the package: with one slot per team
  # and season, x_new = m + rho (x_old - m) + kappa z, m the mean of x_old.
  tau <- 0.6
  kappa <- 0.25
  rho <- 0.5
  spec <- fv2_build(stayers_matches(), clock = "break", rho = rho, divisions = stayers_divisions())
  expect_identical(spec$S, 6L)
  old <- spec$first_slot
  new <- old + 1L
  r <- fv2_structure(spec)
  expect_equal(r$brk[new[1], old[1]], -(rho + (1 - rho) / 3))
  expect_equal(r$brk[new[1], old[2]], -(1 - rho) / 3)

  pull <- rho * diag(3) + (1 - rho) / 3
  expected <- matrix(0, 6, 6)
  expected[old, old] <- tau^2 * diag(3)
  expected[new, old] <- tau^2 * pull
  expected[old, new] <- t(expected[new, old])
  expected[new, new] <- tau^2 * pull %*% t(pull) + kappa^2 * diag(3)
  expect_equal(as.matrix(solve(r$init / tau^2 + r$brk / kappa^2)), expected, ignore_attr = TRUE)
  # The toy can tell reversion from a plain jump: without it the old and new
  # states of different teams would be uncorrelated.
  expect_gt(expected[new[1], old[2]], 0.05)
})

test_that("the reverting break is the same prior in INLA's and Stan's form", {
  tau <- 0.6
  sigma <- 0.05
  kappa <- 0.25
  sim <- simulate_league(n_team = 4L, n_week = 8L)
  div <- stayers_divisions(teams = paste0("T", 1:4), division = "L")
  cov_of <- function(rho) {
    spec <- fv2_build(sim, clock = "break", rho = rho, divisions = div)
    r <- fv2_structure(spec)
    q <- r$init / tau^2 + r$`in` / sigma^2 + r$brk / kappa^2
    list(inla = as.matrix(solve(q)), stan = recursion_cov(spec, tau, sigma, kappa, rho))
  }
  plain <- cov_of(1)
  for (rho in c(1, 0.8, 0.5)) {
    both <- cov_of(rho)
    expect_equal(both$inla, both$stan, ignore_attr = TRUE)
    # Four stayers in one division: the coefficient has to change the prior,
    # or this test compares a plain jump with a plain jump.
    if (rho < 1) expect_gt(max(abs(both$inla - plain$inla)), 0.01)
  }
})

test_that("a stayer reverts toward all of its division's stayers, slot in the new season or not", {
  # Early in a season only some teams have a match or a fixture. A plays a cup
  # tie against X; B and C (same division as A) and Y (same as X) have no
  # 2022 slot yet. The division mean A reverts toward is still A, B and C.
  rho <- 0.5
  train <- rbind(
    stayers_matches(2021),
    data.frame(
      match_date = stayers_matches(2021)$match_date[1], home_team = "X", away_team = "Y",
      home_score = 1L, away_score = 1L, season = 2021L, cell = "L2@2021"
    )
  )
  cup_tie <- data.frame(
    match_date = as.Date("2022-03-10"), home_team = "A", away_team = "X",
    home_score = NA_integer_, away_score = NA_integer_, season = 2022L, cell = "CUP@2022"
  )
  div <- rbind(stayers_divisions(), stayers_divisions(c("X", "Y"), division = "L2", rank = 2L))
  spec <- fv2_build(train, cup_tie, clock = "break", rho = rho, divisions = div)
  old <- stats::setNames(spec$first_slot, spec$teams)
  expect_identical(spec$slots$season[old[["A"]] + 1L], 2022L)
  expect_identical(sum(spec$slots$season == 2022L), 2L)

  brk <- fv2_structure(spec)$brk
  a_new <- old[["A"]] + 1L
  expect_equal(brk[a_new, old[["A"]]], -(rho + (1 - rho) / 3))
  expect_equal(brk[a_new, old[["B"]]], -(1 - rho) / 3)
  expect_equal(brk[a_new, old[["C"]]], -(1 - rho) / 3)
  expect_equal(brk[old[["X"]] + 1L, old[["Y"]]], -(1 - rho) / 2)
  # One definition of the group, read by both engines.
  members <- lapply(spec$groups$members, function(m) sort(spec$teams[spec$slots$team[m]]))
  expect_identical(unname(members[order(lengths(members))]), list(c("X", "Y"), c("A", "B", "C")))
  r <- fv2_structure(spec)
  expect_equal(
    as.matrix(solve(r$init / 0.6^2 + r$brk / 0.25^2)), recursion_cov(spec, 0.6, 0.05, 0.25, rho),
    ignore_attr = TRUE
  )
})

test_that("INLA's and Stan's forms agree when a team sits out a season", {
  # A is in the division all three seasons but has no 2022 match in training.
  # Its 2021 to 2023 break reverts toward the 2021 mean of A, B and C, the
  # same centre B's and C's 2021 to 2022 breaks use, in both forms.
  sit_out <- rbind(
    stayers_matches(2021),
    data.frame(
      match_date = stayers_matches(2022)$match_date[1], home_team = "B", away_team = "C",
      home_score = 0L, away_score = 1L, season = 2022L, cell = "L1@2022"
    ),
    stayers_matches(2023)
  )
  div <- stayers_divisions(seasons = 2021:2023)
  for (rho in c(0.8, 0.5)) {
    spec <- fv2_build(sit_out, clock = "break", rho = rho, divisions = div)
    a <- spec$steps[spec$steps$team == 1L, ]
    expect_identical(c(a$class, a$delta), c("brk", "2"))
    expect_identical(a$rho, rho)
    r <- fv2_structure(spec)
    expect_equal(
      as.matrix(solve(r$init / 0.6^2 + r$brk / 0.25^2)), recursion_cov(spec, 0.6, 0.05, 0.25, rho),
      ignore_attr = TRUE
    )
  }
})

test_that("a team that left the division's next season takes a plain jump", {
  # Same matches, but the schedule shows the 2022 division without A. A is
  # then not one of the 2021 stayers: it does not revert when it comes back,
  # and it is not part of the mean B and C revert toward.
  sit_out <- rbind(
    stayers_matches(2021),
    data.frame(
      match_date = stayers_matches(2022)$match_date[1], home_team = "B", away_team = "C",
      home_score = 0L, away_score = 1L, season = 2022L, cell = "L1@2022"
    ),
    stayers_matches(2023)
  )
  div <- stayers_divisions(seasons = 2021:2023)
  div <- div[!(div$team == "A" & div$season == 2022L), ]
  spec <- fv2_build(sit_out, clock = "break", rho = 0.5, divisions = div)
  st <- spec$steps
  expect_identical(st$rho[st$team == 1L], 1)
  first <- which(spec$groups$season == 2021L)
  expect_identical(sort(spec$teams[spec$slots$team[spec$groups$members[[first]]]]), c("B", "C"))
  r <- fv2_structure(spec)
  expect_equal(
    as.matrix(solve(r$init / 0.6^2 + r$brk / 0.25^2)), recursion_cov(spec, 0.6, 0.05, 0.25, 0.5),
    ignore_attr = TRUE
  )
})

test_that("segments are one team's run of slots within a season", {
  spec <- fv2_build(
    toy_matches(), toy_fixture(),
    clock = "break", rho = 0.8, divisions = toy_divisions()
  )
  seg <- fv2_segments(spec)
  expect_identical(seg$n_seg, 6L)
  expect_identical(seg$seg_team, c(1L, 1L, 2L, 2L, 3L, 3L))
  expect_identical(seg$seg_prev, c(0L, 1L, 0L, 3L, 0L, 5L))
  # Only A stays in its division, so only A's second segment reverts.
  expect_identical(seg$seg_stay, c(0, 1, 0, 0, 0, 0))
  expect_identical(seg$seg_group, c(0L, 1L, 0L, 0L, 0L, 0L))
  expect_identical(seg$n_wave, 2L)
  expect_identical(sort(seg$seg_order[seg$wave_start[1]:seg$wave_end[1]]), c(1L, 3L, 5L))
  expect_true(all(seg$step_in[seg$seg_first] == 0))
  expect_identical(tabulate(seg$seg_of_slot), as.integer(seg$seg_last - seg$seg_first + 1L))
})

test_that("each spec maps to the Stan program that covers it", {
  cal <- fv2_build(toy_matches())
  brk <- fv2_build(toy_matches(), clock = "break")
  rev <- fv2_build(toy_matches(), clock = "break", rho = 0.8, divisions = toy_divisions())
  sq <- fv2_build(toy_matches(), param = "sq", style = "none")
  expect_identical(fv2_stan_program(cal), "fv2.stan")
  expect_identical(fv2_stan_program(brk), "fv2.stan")
  expect_identical(fv2_stan_program(brk, est_rho = TRUE), "fv2_rev.stan")
  expect_identical(fv2_stan_program(rev), "fv2_rev.stan")
  expect_identical(fv2_stan_program(sq), "fv2_sq.stan")
  expect_identical(fv2_stan_program(cal, est_rho = TRUE), "fv2.stan")
  d <- fv2_stan_data(rev)
  expect_identical(d$n_field, 2L)
  expect_identical(d$rho_fixed, 0.8)
  expect_identical(d$est_rho, 0L)
  expect_identical(fv2_stan_data(brk, est_rho = TRUE)$est_rho, 1L)
})

test_that("movers get their own step class and a cumulative shock covariate", {
  spec <- fv2_build(
    toy_matches(), toy_fixture(),
    clock = "break", movers = TRUE, divisions = toy_divisions()
  )
  expect_identical(sum(spec$steps$class == "mov"), 2L)
  expect_named(fv2_structure(spec), c("init", "in", "brk", "mov"))
  c_slots <- spec$slots$team == 3L
  expect_identical(spec$promoted[c_slots], as.numeric(spec$slots$season[c_slots] == 2022L))
  expect_identical(sum(spec$relegated[spec$slots$team == 2L & spec$slots$season == 2022L] == 1), 2L)
  expect_true(all(spec$promoted[spec$slots$team == 1L] == 0))
  expect_error(fv2_build(toy_matches(), clock = "break", movers = TRUE))
  expect_error(fv2_build(toy_matches(), clock = "calendar", movers = TRUE, divisions = toy_divisions()))
})

test_that("league-level walks enter both sides and split the home advantage", {
  spec <- fv2_build(toy_matches(), toy_fixture(), level_walk = TRUE)
  expect_identical(spec$weeks, sort(unique(fv2_week(c(toy_matches()$match_date, toy_fixture()$match_date)))))
  des <- fv2_design(spec)
  n <- spec$N
  expect_equal(Matrix::rowSums(des$lvl_goal), rep(1, 2 * n))
  expect_equal(Matrix::rowSums(des$lvl_ha), rep(c(0.5, -0.5), each = n))
  expect_identical(max.col(as.matrix(des$lvl_goal)), rep(spec$obs$week_nr, 2))
  # Prediction applies what the design encodes.
  n_week <- length(spec$weeks)
  draws <- list(
    c_home = 0, c_away = 0, att = matrix(0, 1, spec$S), def = matrix(0, 1, spec$S),
    ha_att = matrix(0, 1, spec$K), ha_def = matrix(0, 1, spec$K),
    lvl_goal = matrix(seq_len(n_week) / 100, 1), lvl_ha = matrix(seq_len(n_week) / 50, 1)
  )
  p <- fv2_predict(draws, spec)
  pd <- fv2_design(spec, "pred")
  via_design <- as.numeric(pd$lvl_goal %*% draws$lvl_goal[1, ] + pd$lvl_ha %*% draws$lvl_ha[1, ])
  expect_equal(log(c(p$lam_home, p$lam_away)), via_design)
  w <- spec$pred$week_nr
  expect_equal(via_design, c(w / 100 + w / 100, w / 100 - w / 100))
  expect_null(fv2_design(fv2_build(toy_matches()))$lvl_goal)
})

test_that("the level walk is a random walk over the weeks, centred on zero", {
  weeks <- c(3L, 4L, 6L, 30L, 31L)
  step <- c(0, sqrt(diff(weeks)))
  sigma <- 0.02
  a <- vapply(seq_along(weeks), function(i) {
    e <- numeric(length(weeks))
    e[i] <- 1
    fv2_level_walk(step, e, sigma)
  }, numeric(length(weeks)))
  expect_equal(colSums(a), rep(0, 5))
  expect_equal(a[, 1], rep(0, 5))
  # The same prior as an intrinsic RW1 on the week values under a zero sum:
  # its covariance is the pseudo-inverse of the RW1 precision.
  d <- diff(diag(5))
  q <- t(d) %*% diag(1 / (sigma^2 * diff(weeks))) %*% d
  e <- eigen(q, symmetric = TRUE)
  keep <- e$values > 1e-8
  pinv <- e$vectors[, keep] %*% diag(1 / e$values[keep]) %*% t(e$vectors[, keep])
  expect_equal(tcrossprod(a), pinv)
  sd <- fv2_stan_data(fv2_build(
    toy_matches(), toy_fixture(),
    clock = "break", rho = 0.8, level_walk = TRUE, divisions = toy_divisions()
  ))
  expect_identical(sd$use_level, 1L)
  expect_identical(sd$week_step[1], 0)
  expect_identical(length(sd$week_step), sd$n_week)
  expect_error(fv2_stan_data(fv2_build(toy_matches(), level_walk = TRUE)))
})

test_that("the shock covariate in the design is what prediction applies", {
  spec <- fv2_build(
    toy_matches(), toy_fixture(),
    clock = "break", movers = TRUE, divisions = toy_divisions()
  )
  # A shock of beta shifts a team's attack and defence together.
  beta <- 0.3
  shift <- beta * spec$promoted - 0.2 * spec$relegated
  draws <- list(
    c_home = 0, c_away = 0,
    att = matrix(shift, 1), def = matrix(shift, 1),
    ha_att = matrix(0, 1, spec$K), ha_def = matrix(0, 1, spec$K)
  )
  p <- fv2_predict(draws, spec)
  des <- fv2_design(spec, "pred")
  via_design <- beta * des$promoted - 0.2 * des$relegated
  expect_equal(log(c(p$lam_home, p$lam_away)), via_design)
  # C was promoted and hosts A, which was not: C's rows gain, A's lose.
  expect_equal(via_design, c(beta, -beta))
  train <- fv2_design(spec, "obs")
  m <- spec$obs
  expect_equal(
    train$promoted,
    c(
      spec$promoted[m$home_slot] - spec$promoted[m$away_slot],
      spec$promoted[m$away_slot] - spec$promoted[m$home_slot]
    )
  )
})

test_that("each training row reads one attack state and one defence state", {
  spec <- fv2_build(toy_matches(), toy_fixture(), div_offsets = TRUE)
  des <- fv2_design(spec)
  n <- spec$N
  expect_identical(dim(des$att), c(2L * n, spec$S))
  expect_equal(Matrix::rowSums(des$att), rep(1, 2 * n))
  expect_equal(Matrix::rowSums(des$def), rep(-1, 2 * n))
  expect_identical(spec$slots$team[max.col(as.matrix(des$att))], c(spec$obs$home_team, spec$obs$away_team))
  expect_equal(Matrix::rowSums(des$ha_att), rep(c(1, 0), each = n))
  expect_equal(Matrix::rowSums(des$ha_def), rep(c(0, -1), each = n))
  expect_equal(Matrix::rowSums(des$g_goal), rep(1, 2 * n))
  expect_equal(Matrix::rowSums(des$g_ha), rep(c(0.5, -0.5), each = n))
  expect_identical(spec$cells, c("L@2021", "L@2022"))
  expect_identical(nrow(fv2_design(spec, "pred")$att), 2L)
})

test_that("strength and style are a linear re-expression of attack and defence", {
  spec <- fv2_build(toy_matches())
  des <- fv2_design(spec)
  set.seed(1)
  s <- stats::rnorm(spec$S)
  q <- stats::rnorm(spec$S)
  via_sq <- (des$att + des$def) %*% s + (des$att - des$def) %*% q
  via_ad <- des$att %*% (s + q) + des$def %*% (s - q)
  expect_equal(as.numeric(via_sq), as.numeric(via_ad))
})

test_that("the Stan data refuses variants the Stan program does not cover", {
  expect_error(fv2_stan_data(fv2_build(toy_matches(), param = "sq")))
  expect_error(fv2_stan_data(fv2_build(
    toy_matches(),
    clock = "break", movers = TRUE, divisions = toy_divisions()
  )))
  sq <- fv2_stan_data(fv2_build(toy_matches(), param = "sq", style = "static"))
  expect_identical(sq$use_style, 1L)
  expect_identical(fv2_stan_data(fv2_build(toy_matches(), param = "sq", style = "none"))$use_style, 0L)
  sd <- fv2_stan_data(fv2_build(toy_matches(), toy_fixture(), clock = "break"))
  expect_identical(sd$use_style, 0L)
  expect_identical(sd$use_break, 1L)
  expect_identical(sd$step_in[sd$first_slot], rep(0, 3))
  expect_identical(sum(sd$step_brk > 0), 3L)
  expect_true(all((sd$step_in > 0) + (sd$step_brk > 0) <= 1))
})

test_that("predictions combine intercept, states and home advantage", {
  spec <- fv2_build(toy_matches(), toy_fixture(), div_offsets = TRUE)
  n_draw <- 2L
  draws <- list(
    c_home = c(0.4, 0.5), c_away = c(0.1, 0.2),
    att = matrix(seq_len(n_draw * spec$S) / 100, n_draw),
    def = matrix(seq_len(n_draw * spec$S) / 200, n_draw),
    ha_att = matrix(0.05, n_draw, spec$K), ha_def = matrix(-0.02, n_draw, spec$K),
    g_goal = matrix(c(0, 0, 0.1, 0.2), n_draw), g_ha = matrix(c(0, 0, 0.04, 0.06), n_draw)
  )
  p <- fv2_predict(draws, spec)
  h <- spec$pred$home_slot
  a <- spec$pred$away_slot
  expect_identical(dim(p$lam_home), c(1L, n_draw))
  expect_equal(
    p$lam_home[1, ],
    exp(draws$c_home + draws$att[, h] + 0.05 - draws$def[, a] + c(0.1, 0.2) + 0.5 * c(0.04, 0.06))
  )
  expect_equal(
    p$lam_away[1, ],
    exp(draws$c_away + draws$att[, a] - draws$def[, h] + 0.02 + c(0.1, 0.2) - 0.5 * c(0.04, 0.06))
  )
})

test_that("hyperparameter names follow the step classes and parameterisation", {
  brk <- fv2_build(toy_matches(), clock = "break")
  expect_identical(
    fv2_hyper_names(brk),
    list(att = c("tau_att", "sigma_att", "kappa_att"), def = c("tau_def", "sigma_def", "kappa_def"))
  )
  sq <- fv2_build(toy_matches(), param = "sq", style = "static")
  expect_identical(fv2_hyper_names(sq), list(str = c("tau_str", "sigma_str")))
})

test_that("the prior guard counts PC priors against the spec", {
  spec <- fv2_build(toy_matches(), clock = "break")
  expect_identical(fv2_n_theta(spec), 8L)
  pc <- function(fixed = FALSE) list(prior = "pc.prec", fixed = fixed)
  default <- list(prior = "loggamma", fixed = FALSE)
  field <- list(hyper = list(theta1 = pc(), theta2 = pc(), theta3 = pc(), theta4 = default))
  iid <- list(hyper = list(theta = pc()))
  good <- list(all.hyper = list(random = list(field, field, iid, iid)))
  expect_true(fv2_check_inla_priors(good, spec))
  # One effect whose specification was not read falls back to the default.
  bad <- good
  bad$all.hyper$random[[4]]$hyper$theta <- default
  expect_error(fv2_check_inla_priors(bad, spec), "was not read")
  expect_identical(fv2_n_theta(fv2_build(toy_matches())), 6L)
  expect_identical(
    fv2_n_theta(fv2_build(toy_matches(), param = "sq", style = "static", level_walk = TRUE)), 7L
  )
})

test_that("the INLA engine returns draws in the shared layout", {
  skip_if_no_inla()
  sim <- simulate_league()
  train <- sim[sim$match_date < max(sim$match_date), ]
  pred <- sim[sim$match_date == max(sim$match_date), ]
  spec <- fv2_build(train, pred, clock = "break")
  f <- suppressWarnings(fv2_fit_inla(spec, int_strategy = "eb", n_samp = 50L, threads = "1:1"))
  expect_identical(dim(f$draws$att), c(50L, spec$S))
  expect_identical(dim(f$draws$ha_def), c(50L, spec$K))
  expect_identical(colnames(f$draws$hyper), c(unlist(fv2_hyper_names(spec), use.names = FALSE), "omega_att", "omega_def"))
  expect_lt(max(abs(rowSums(f$draws$att[, spec$first_slot]))), 1e-6)
  p <- fv2_predict(f$draws, spec)
  expect_identical(dim(p$lam_home), c(nrow(pred), 50L))
  expect_true(all(is.finite(p$lam_home)) && all(p$lam_home > 0))

  held <- suppressWarnings(fv2_fit_inla(
    spec,
    int_strategy = "eb", n_samp = 20L, threads = "1:1",
    theta_init = f$theta_mode, theta_fixed = TRUE
  ))
  expect_equal(held$draws$hyper[1, ], f$draws$hyper[1, ], tolerance = 1e-6)
})

test_that("the seed fixes an INLA fit's draws, whatever R's own generator is doing", {
  skip_if_no_inla()
  sim <- simulate_league()
  train <- sim[sim$match_date < max(sim$match_date), ]
  pred <- sim[sim$match_date == max(sim$match_date), ]
  spec <- fv2_build(train, pred, clock = "break")
  # Full integration, as production fits: INLA spreads the draws over its
  # hyperparameter configurations with R's generator, which the INLA seed
  # does not touch. A daily fit and its rerun start from different R states.
  fit_after <- function(r_state) {
    set.seed(r_state)
    suppressWarnings(fv2_fit_inla(spec, int_strategy = "auto", n_samp = 60L, threads = "1:1", seed = 11L))
  }
  a <- fit_after(1)
  b <- fit_after(2)
  expect_gt(nrow(unique(round(a$draws$hyper, 8))), 1L)
  expect_equal(b$draws$hyper, a$draws$hyper, tolerance = 1e-6)
  expect_equal(b$draws$att, a$draws$att, tolerance = 1e-6)
  other <- suppressWarnings(fv2_fit_inla(spec, int_strategy = "auto", n_samp = 60L, threads = "1:1", seed = 12L))
  expect_gt(max(abs(other$draws$att - a$draws$att)), 1e-3)
})

test_that("an INLA fit runs on one thread with R-INLA's online optimisation off", {
  # WHY: with R-INLA's defaults the same fit does not return the same numbers
  # in two R processes. Of 140 production fits of the two football cells, one
  # R process each (main at 26d55c550, R-INLA 25.6.7, one machine,
  # 2026-11-17): on four threads no two processes agreed, on one thread with
  # the option on 39 of 40 did, on one thread with it off 40 of 40.
  # The stand-in records what the fit was asked for and stops, so nothing is
  # fitted.
  skip_if_no_inla()
  sim <- simulate_league()
  train <- sim[sim$match_date < max(sim$match_date), ]
  pred <- sim[sim$match_date == max(sim$match_date), ]
  spec <- fv2_build(train, pred, clock = "break")
  at_the_call <- NULL
  local_mocked_bindings(
    inla = function(..., control.compute = NULL, num.threads = NULL) {
      at_the_call <<- list(internal_opt = control.compute$internal.opt, threads = num.threads)
      stop("no fit today")
    },
    .package = "INLA"
  )
  session_option <- INLA::inla.getOption("internal.opt")

  expect_error(fv2_fit_inla(spec), "no fit today")
  expect_identical(at_the_call, list(internal_opt = FALSE, threads = "1:1"))

  # The production entry, called the way fit_league() calls it: no `threads`.
  root <- withr::local_tempdir()
  s <- v2_store(root)
  prep <- prepare_data(s$league, "male", end_date = s$end_date, root = root)
  at_the_call <- NULL
  expect_error(
    suppressMessages(fit_model_v2_inla(prep, s$league, "male", end_date = s$end_date, root = root)),
    "no fit today"
  )
  expect_identical(at_the_call, list(internal_opt = FALSE, threads = "1:1"))

  # The option goes to the fit; R-INLA's session-wide setting is the caller's.
  expect_identical(INLA::inla.getOption("internal.opt"), session_option)
})

test_that("the production INLA fit returns the same numbers in two separate R processes", {
  skip_if_no_inla()
  skip_if_not_installed("callr")
  root <- withr::local_tempdir()
  s <- v2_store(root)
  pkg <- normalizePath(testthat::test_path("..", ".."))
  fit_elsewhere <- function() {
    callr::r(function(pkg, root, league, end_date) {
      pkgload::load_all(pkg, quiet = TRUE)
      prep <- prepare_data(league, "male", end_date = end_date, root = root)
      # As fit_league() calls it: full integration and the default threads.
      fit <- suppressWarnings(suppressMessages(fit_model_v2_inla(
        prep, league, "male",
        end_date = end_date, root = root, n_draws = 200L, seed = 7L
      )))
      published <- unclass(posterior::as_draws_matrix(fit$draws()))
      attributes(published) <- attributes(published)[c("dim", "dimnames")]
      list(passed = fit$gate$passed, hyper_sd = fit$hyper_sd, published = published)
    }, args = list(pkg = pkg, root = root, league = s$league, end_date = s$end_date))
  }

  first <- fit_elsewhere()
  second <- fit_elsewhere()

  expect_true(first$passed)
  expect_gt(ncol(first$published), 0L)
  expect_identical(first, second)
})

test_that("the adapter speaks the production fit's variable names", {
  prep <- toy_prep()
  pred <- fv2_pred_from_prep(prep$pred_d)
  expect_identical(pred$season, 2022L)
  expect_identical(pred$cell, "L@2022")
  spec <- fv2_build(toy_matches(), pred)
  draws <- toy_draws(spec)
  fit <- fv2_as_fit(draws, spec, prep)
  get <- function(v) unclass(posterior::as_draws_matrix(fit$draws(v)))
  expect_setequal(fit$variables, c(
    "mean_log_goals", "alpha_mu3", "beta_mu3_strength_diff",
    "home_advantage_off", "home_advantage_def", "home_advantage_tot",
    "cur_offense_away", "cur_defense_away", "cur_strength_away",
    "cur_offense_home", "cur_defense_home", "cur_strength_home",
    "offense", "defense", "lp__", "goals1_pred", "goals2_pred"
  ))
  # Production's simulators rebuild the rates as intercept + home advantage.
  expect_equal(
    as.numeric(get("mean_log_goals")) + get("home_advantage_off")[, 2] - draws$ha_att[, 1],
    draws$c_home,
    ignore_attr = TRUE
  )
  expect_equal(
    as.numeric(get("mean_log_goals")) - get("home_advantage_def")[, 2] + draws$ha_def[, 1],
    draws$c_away,
    ignore_attr = TRUE
  )
  expect_equal(
    get("home_advantage_tot")[, 1], draws$c_home - draws$c_away + draws$ha_att[, 3] + draws$ha_def[, 3],
    ignore_attr = TRUE
  )
  # Team "A" is production's team 2 and the spec's team 1. Its current state
  # is the one at its next match (the fixture), not at its last result.
  a_slots <- spec$slots$slot[spec$slots$team == 1L & spec$slots$has_obs]
  a_next <- spec$slots$slot[spec$slots$team == 1L & !spec$slots$has_obs]
  expect_identical(a_next, max(a_slots) + 1L)
  expect_equal(get("cur_offense_away")[, 2], draws$att[, a_next], ignore_attr = TRUE)
  # B has no fixture, so its current state is still its last result's.
  b_slots <- spec$slots$slot[spec$slots$team == 2L]
  expect_equal(get("cur_offense_away")[, 3], draws$att[, max(b_slots)], ignore_attr = TRUE)
  off <- posterior::draws_of(posterior::as_draws_rvars(fit$draws("offense"))$offense)
  expect_identical(dim(off), c(3L, 5L, 3L))
  # A's two matches in its first week are rounds 1 and 2: the same weekly state.
  expect_equal(off[, 1, 2], draws$att[, a_slots[1]], ignore_attr = TRUE)
  expect_equal(off[, 2, 2], draws$att[, a_slots[1]], ignore_attr = TRUE)
  expect_equal(off[, 4, 2], draws$att[, a_slots[3]], ignore_attr = TRUE)
  # C has played three matches; later rounds carry its latest state forward.
  c_slots <- spec$slots$slot[spec$slots$team == 3L & spec$slots$has_obs]
  expect_equal(off[, 4, 1], draws$att[, max(c_slots)], ignore_attr = TRUE)
  expect_equal(off[, 5, 1], draws$att[, max(c_slots)], ignore_attr = TRUE)
  expect_true(all(get("alpha_mu3") < -30))
  expect_identical(dim(get("goals1_pred")), c(3L, 1L))
  expect_true(all(get("goals1_pred") == round(get("goals1_pred"))))
  expect_error(fit$draws("sigma"), "no draws")
})

test_that("the adapter hands the simulators the league levels of the next match week", {
  prep <- toy_prep()
  spec <- fv2_build(toy_matches(), fv2_pred_from_prep(prep$pred_d), level_walk = TRUE)
  draws <- toy_draws(spec)
  n_week <- length(spec$weeks)
  draws$lvl_goal <- matrix(rep(seq_len(n_week) / 10, each = 3), 3)
  draws$lvl_ha <- matrix(rep(seq_len(n_week) / 20, each = 3), 3)
  fit <- fv2_as_fit(draws, spec, prep)
  get <- function(v) unclass(posterior::as_draws_matrix(fit$draws(v)))
  # The fixture is a week after the last result; the simulators get the
  # levels the forecast of that fixture uses.
  upcoming <- min(spec$pred$week_nr)
  expect_gt(upcoming, max(spec$obs$week_nr))
  expect_equal(
    as.numeric(get("mean_log_goals")), (draws$c_home + draws$c_away) / 2 + upcoming / 10
  )
  expect_equal(
    get("home_advantage_tot")[, 1],
    draws$c_home - draws$c_away + upcoming / 20 + draws$ha_att[, 3] + draws$ha_def[, 3],
    ignore_attr = TRUE
  )
  # With nothing to predict they get the levels of the last played week.
  bare <- prep
  bare$pred_d <- prep$pred_d[0, ]
  spec0 <- fv2_build(toy_matches(), level_walk = TRUE)
  draws0 <- toy_draws(spec0)
  draws0$lvl_goal <- matrix(rep(seq_along(spec0$weeks) / 10, each = 3), 3)
  draws0$lvl_ha <- matrix(rep(seq_along(spec0$weeks) / 20, each = 3), 3)
  mlg0 <- unclass(posterior::as_draws_matrix(fv2_as_fit(draws0, spec0, bare)$draws("mean_log_goals")))
  expect_equal(as.numeric(mlg0), (draws0$c_home + draws0$c_away) / 2 + max(spec0$obs$week_nr) / 10)
})

test_that("the simulators and the forecast give a team's next match the same rates", {
  # In season: C v A is a week after the last results.
  prep <- toy_prep()
  spec <- fv2_build(toy_matches(), fv2_pred_from_prep(prep$pred_d), level_walk = TRUE)
  draws <- toy_draws(spec)
  n_week <- length(spec$weeks)
  set.seed(2)
  draws$lvl_goal <- matrix(stats::rnorm(3 * n_week, 0, 0.1), 3)
  draws$lvl_ha <- matrix(stats::rnorm(3 * n_week, 0, 0.1), 3)
  sim <- simulator_rates(fv2_as_fit(draws, spec, prep), prep$teams, "C", "A")
  forecast <- fv2_predict(draws, spec)
  expect_equal(sim$home, forecast$lam_home[1, ])
  expect_equal(sim$away, forecast$lam_away[1, ])
})

test_that("across a season break the simulators see what the forecast sees", {
  # Before the season: results from 2021, A v B opens 2022, and C's first
  # match is beyond the horizon. The forecast of A v B uses states that have
  # crossed the break. The simulators must use the same ones, and C must be
  # on the same side of the break as the teams it will be simulated against.
  train <- stayers_matches(2021)
  pred_d <- data.frame(
    game_nr = 1L, match_date = as.Date("2022-05-02"), home_team = "A", away_team = "B", division = "L1"
  )
  prep <- prep_like(train, pred_d, c("C", "A", "B"))
  carry <- data.frame(team = "C", match_date = as.Date("2022-05-30"), season = 2022L)
  spec <- fv2_build(
    train, fv2_pred_from_prep(prep$pred_d),
    carry = carry, clock = "break", rho = 0.5, param = "sq", style = "static",
    level_walk = TRUE, divisions = stayers_divisions()
  )
  draws <- toy_draws(spec)
  n_week <- length(spec$weeks)
  set.seed(3)
  draws$lvl_goal <- matrix(stats::rnorm(3 * n_week, 0, 0.1), 3)
  draws$lvl_ha <- matrix(stats::rnorm(3 * n_week, 0, 0.1), 3)
  fit <- fv2_as_fit(draws, spec, prep)
  sim <- simulator_rates(fit, prep$teams, "A", "B")
  forecast <- fv2_predict(draws, spec)
  expect_equal(sim$home, forecast$lam_home[1, ])
  expect_equal(sim$away, forecast$lam_away[1, ])

  now <- stats::setNames(spec$cur_slot, spec$teams)
  expect_identical(spec$slots$season[now], rep(2022L, 3))
  c_now <- unclass(posterior::as_draws_matrix(fit$draws("cur_offense_away")))[, 1]
  expect_equal(as.numeric(c_now), draws$att[, now[["C"]]])
})

test_that("a carried team gets a state in the new season and no forecast", {
  train <- stayers_matches(2021)
  fixture <- data.frame(
    match_date = as.Date("2022-05-02"), home_team = "A", away_team = "B",
    home_score = NA_integer_, away_score = NA_integer_, season = 2022L, cell = "L1@2022"
  )
  carry <- data.frame(team = "C", match_date = as.Date("2022-05-30"), season = 2022L)
  spec <- fv2_build(train, fixture, carry = carry, clock = "break", rho = 0.5, divisions = stayers_divisions())
  c_slots <- spec$slots[spec$slots$team == 3L, ]
  expect_identical(c_slots$season, c(2021L, 2022L))
  expect_identical(c_slots$has_obs, c(TRUE, FALSE))
  expect_identical(length(spec$pred$home_slot), 1L)
  # It crosses the break like the others: a reverting step toward A, B and C.
  step <- spec$steps[spec$steps$team == 3L, ]
  expect_identical(c(step$class, as.character(step$rho)), c("brk", "0.5"))
  expect_identical(length(spec$groups$members[[step$group]]), 3L)
  # Without it C would still be in last season.
  without <- fv2_build(train, fixture, clock = "break", rho = 0.5, divisions = stayers_divisions())
  expect_identical(without$slots$season[without$cur_slot], c(2022L, 2022L, 2021L))
  expect_error(
    fv2_build(train, fixture, carry = transform(carry, team = "Z"), clock = "break"),
    "carry"
  )
})

test_that("teams the schedule puts in the new season are carried into it", {
  train <- stayers_matches(2021)
  fixture <- data.frame(
    match_date = as.Date("2022-05-02"), home_team = "A", away_team = "B",
    home_score = NA_integer_, away_score = NA_integer_, season = 2022L
  )
  known <- data.frame(
    home_team = c("A", "C", "C", "Z"), away_team = c("B", "Z", "A", "Y"),
    match_date = as.Date(c("2022-05-02", "2022-06-20", "2022-05-30", "2022-05-30")),
    season = 2022L
  )
  carry <- fv2_carry_slots(train, fixture, known)
  # C is in the model, plays in 2022 and has no slot there; Z and Y are not
  # in the model at all.
  expect_identical(carry$team, "C")
  expect_identical(carry$match_date, as.Date("2022-05-30"))
  expect_identical(carry$season, 2022L)
  # In season every team already has a slot in the season being forecast.
  in_season <- fv2_carry_slots(stayers_matches(2021:2022), transform(fixture, match_date = as.Date("2022-06-06")), known)
  expect_identical(nrow(in_season), 0L)
  expect_identical(nrow(fv2_carry_slots(train, fixture[0, ], known)), 0L)
})

test_that("production's extract_posteriors reads the adapted fit", {
  prep <- toy_prep()
  spec <- fv2_build(toy_matches(), fv2_pred_from_prep(prep$pred_d))
  fit <- fv2_as_fit(toy_draws(spec, n_draw = 4L), spec, prep)
  b <- extract_posteriors(
    fit, prep$pred_d, list(sport = "football", country = "iceland"), "male",
    fit_date = as.Date("2022-05-15")
  )
  expect_identical(nrow(b), 4L)
  expect_identical(b$home_team, rep("C", 4))
  expect_identical(sort(b$draw_id), 1:4)
  expect_true(all(b$home_goals >= 0))
})

test_that("fixtures from prepare_data keep their shape when there are none", {
  none <- fv2_pred_from_prep(toy_prep()$pred_d[0, ])
  expect_identical(nrow(none), 0L)
  expect_identical(names(none), names(fv2_pred_from_prep(toy_prep()$pred_d)))
})

test_that("the adapter refuses training rows in a different order", {
  prep <- toy_prep()
  spec <- fv2_build(toy_matches()[c(2, 1, 3:6), ], fv2_pred_from_prep(prep$pred_d))
  expect_error(fv2_as_fit(toy_draws(spec), spec, prep))
})

test_that("division codes fold into leagues and rank from the top", {
  expect_identical(
    fv2_league_division(c("BD", "BD_UPPER_PO", "BD_LOWER_PO", "LD1_PO", "CUP", "LD3")),
    c("BD", "BD", "BD", "LD1", NA, "LD3")
  )
  expect_identical(fv2_cell(c("BD_UPPER_PO", "CUP"), 2026L), c("BD@2026", "CUP@2026"))
  league <- list(training_filter = list(divisions = c("BD", "BD_UPPER_PO", "BD_LOWER_PO", "LD1", "LD2")))
  expect_identical(as.character(fv2_rank_order(league)), c("BD", "LD1", "LD2"))
  m <- data.frame(
    home_team = c("A", "A", "B", "A"), away_team = c("B", "C", "C", "B"),
    season = c(2025L, 2025L, 2025L, 2026L),
    league_division = c("LD1", NA, "LD1", "BD")
  )
  tab <- fv2_division_table(m, c("BD", "LD1"))
  expect_identical(tab$division[tab$team == "A"], c("LD1", "BD"))
  expect_identical(tab$rank[tab$team == "A"], c(2L, 1L))
  expect_identical(tab$division[tab$team == "C"], "LD1")
  expect_identical(fv2_division_table(m, "BD")$rank[1], 2L)
})

# --- v2.0 on the production fit path (R/model-v2-fit.R) ----------------------

test_that("the production model is v2.0, the specification the report recommends", {
  expect_identical(FOOTBALL_V2_VERSION, "2.0")
  expect_identical(
    football_v2_options(),
    list(clock = "break", rho = 0.8, param = "sq", style = "static", level_walk = TRUE)
  )
})

test_that("no league is on the INLA engine", {
  # The switch is the owner's: setting `engine: inla` for a league is a
  # deliberate change that edits this expectation in the same commit.
  engines <- vapply(load_leagues(), function(l) l$engine %||% "stan", character(1))
  expect_true(all(engines == "stan"))
})

test_that("the INLA gate passes a sound fit and names each kind of failure", {
  spec <- fv2_build(toy_matches(), toy_fixture())
  ok <- fv2_inla_gate(gate_fit(spec), spec)
  expect_true(ok$passed)
  expect_identical(ok$breaches, character())
  # The jump check needs a previous fit and is skipped without one, and the
  # gate says so.
  expect_identical(c(ok$evaluated, ok$skipped), c(4L, 1L))
  expect_false(ok$jump_checked)

  breach <- function(change, ...) {
    f <- change(gate_fit(spec))
    fv2_inla_gate(f, spec, ...)
  }
  no_mode <- breach(function(f) {
    f$mode_status <- 1L
    f
  })
  expect_false(no_mode$passed)
  expect_match(no_mode$breaches, "mode not found")
  # A status that is absent, empty or NA must fail the check, not skip it.
  for (status in list(NULL, integer(0), NA_integer_)) {
    absent <- breach(function(f) {
      f["mode_status"] <- list(status)
      f
    })
    expect_false(absent$passed)
    expect_match(absent$breaches, "mode not found")
  }

  wide <- breach(function(f) {
    f$draws$hyper[, "tau_att"] <- 9
    f
  })
  expect_match(wide$breaches, "tau_att = 9")
  unknown <- breach(function(f) {
    f$draws$hyper[1, "sigma_att"] <- NA
    f
  })
  expect_match(unknown$breaches, "sigma_att")
  one_column <- breach(function(f) {
    f$draws$hyper <- f$draws$hyper[, "tau_att", drop = FALSE]
    f
  })
  expect_true(one_column$passed)

  not_finite <- breach(function(f) {
    f$draws$att[1, 1] <- NaN
    f
  })
  expect_match(not_finite$breaches, "non-finite")

  unconstrained <- breach(function(f) {
    f$draws$att[, spec$first_slot[1]] <- f$draws$att[, spec$first_slot[1]] + 0.3
    f
  })
  expect_match(unconstrained$breaches, "do not sum to zero")

  goals <- breach(function(f) {
    f$draws$c_home <- f$draws$c_home + 3
    f
  })
  expect_match(goals$breaches, "predicted goals")

  jumped <- breach(identity, previous_sd = c(tau_att = 0.4, sigma_att = 0.005))
  expect_false(jumped$passed)
  expect_match(jumped$breaches, "sigma_att")
  expect_no_match(jumped$breaches, "tau_att")
  steady <- breach(identity, previous_sd = c(tau_att = 0.5, sigma_att = 0.04))
  expect_true(steady$passed)
  expect_identical(c(steady$evaluated, steady$skipped), c(5L, 0L))
  expect_true(steady$jump_checked)
})

test_that("the INLA gate skips the goals check when there is nothing to predict", {
  spec <- fv2_build(toy_matches())
  g <- fv2_inla_gate(gate_fit(spec), spec)
  expect_true(g$passed)
  expect_identical(c(g$evaluated, g$skipped), c(3L, 2L))
})

test_that("history is counted in seasons before the one being forecast", {
  # One earlier season, before or during the next: the regime where INLA was
  # measurably worse than NUTS.
  expect_identical(fv2_earlier_seasons(2021L, 2022L), 1L)
  expect_identical(fv2_earlier_seasons(c(2021L, 2021L, 2022L), 2022L), 1L)
  # Two earlier seasons give the same answer before the third starts and
  # once it is under way.
  expect_identical(fv2_earlier_seasons(c(2021L, 2022L), 2023L), 2L)
  expect_identical(fv2_earlier_seasons(c(2021L, 2022L, 2023L), 2023L), 2L)
  expect_identical(FOOTBALL_V2_MIN_EARLIER_SEASONS, 2L)
})

test_that("the INLA engine is refused outside football and on short histories", {
  root <- withr::local_tempdir()
  s <- v2_store(root, n_season = 2L)
  prep <- prepare_data(s$league, "male", end_date = s$end_date, root = root)
  expect_error(
    fit_model_v2_inla(prep, s$league, "male", end_date = s$end_date, root = root),
    "at least 2 seasons of results before season 2022; this cell has 1"
  )
  handball <- utils::modifyList(s$league, list(sport = "handball"))
  expect_error(
    fit_model_v2_inla(prep, handball, "male", end_date = s$end_date, root = root),
    "football model only"
  )
})

test_that("fit_league fits with Stan unless the league says engine: inla", {
  root <- withr::local_tempdir()
  s <- v2_store(root)
  called <- character()
  fake_fit <- list(
    save_object = function(file) saveRDS(NULL, file),
    gate = list(passed = TRUE, evaluated = 4L, skipped = 1L, jump_checked = FALSE),
    hyper_sd = c(tau_str = 0.4), version = FOOTBALL_V2_VERSION
  )
  inla_args <- NULL
  stan_args <- NULL
  testthat::local_mocked_bindings(
    fit_model = function(...) {
      called <<- c(called, "stan")
      stan_args <<- list(...)
      structure(fake_fit, class = "CmdStanMCMC")
    },
    fit_model_v2_inla = function(prep, league, sex, ...) {
      called <<- c(called, "inla")
      inla_args <<- list(...)
      fake_fit
    },
    persist_fit_diagnostics = function(...) invisible(NULL),
    extract_posteriors = function(...) tibble::tibble(),
    extract_football_iceland = function(...) invisible(NULL),
    .package = "sports"
  )
  run <- function(league) {
    suppressMessages(fit_league(
      league = league, sex = "male", fit_date = s$end_date, root = root,
      chains = 2L, iter_warmup = 120L, iter_sampling = 150L, adapt_delta = 0.9, seed = 99L
    ))
  }

  run(s$league)
  expect_identical(called, "stan")
  # The Stan call is the one fit_league() made before the engine key existed.
  expect_setequal(
    names(stan_args),
    c("stan_data", "stan_model_path", "method", "chains", "iter_warmup", "iter_sampling", "adapt_delta", "seed")
  )
  expect_identical(
    stan_args[c("method", "chains", "iter_warmup", "iter_sampling", "adapt_delta", "seed")],
    list(method = "sample", chains = 2L, iter_warmup = 120L, iter_sampling = 150L, adapt_delta = 0.9, seed = 99L)
  )
  expect_identical(stan_args$stan_data$N, s$n_obs)
  expect_identical(basename(stan_args$stan_model_path), "bivariate_poisson_no_inflation.stan")
  run(utils::modifyList(s$league, list(engine = "stan")))
  expect_identical(called, c("stan", "stan"))
  expect_false(dir.exists(file.path(root, "beliefs", "diagnostics")))

  run(utils::modifyList(s$league, list(engine = "inla")))
  expect_identical(called, c("stan", "stan", "inla"))
  expect_identical(inla_args$n_draws, 300L)
  expect_identical(inla_args$seed, 99L)
  expect_identical(inla_args$end_date, s$end_date)
  # The thread count is the engine's to choose: its default, one thread, is
  # the only setting at which a fit repeats between R processes.
  expect_false("threads" %in% names(inla_args))

  # needs_refit() reads n_obs off the newest diagnostics row, so an INLA fit
  # has to write one; the sampler metrics have nothing to report.
  row <- as.data.frame(read_table("fit_diagnostics", root = root))
  expect_identical(nrow(row), 1L)
  expect_identical(row$n_obs, s$n_obs)
  expect_identical(row$total_iter, 300L)
  sampler_only <- c(
    "n_divergent", "div_frac", "n_max_treedepth", "treedepth_frac", "min_ebfmi", "max_rhat",
    "min_ess_bulk", "min_ess_tail", "adapt_delta", "iter_sampling", "chains"
  )
  expect_identical(names(row)[vapply(row, anyNA, logical(1))], sampler_only)
  expect_identical(row$passed, TRUE)
  expect_identical(c(row$gates_evaluated, row$gates_skipped), c(4L, 1L))

  expect_error(run(utils::modifyList(s$league, list(engine = "laplace"))), "Unknown engine")
  expect_identical(called, c("stan", "stan", "inla"))
})

test_that("an INLA fit of a football store reads like a production fit", {
  skip_if_no_inla()
  root <- withr::local_tempdir()
  s <- v2_store(root)
  prep <- prepare_data(s$league, "male", end_date = s$end_date, root = root)
  fit_once <- function() {
    suppressWarnings(suppressMessages(fit_model_v2_inla(
      prep, s$league, "male",
      end_date = s$end_date, root = root,
      n_draws = 200L, seed = 7L, int_strategy = "eb", threads = "1:1"
    )))
  }
  fit <- fit_once()
  expect_identical(fit$engine, "inla")
  expect_identical(fit$version, "2.0")
  expect_true(fit$gate$passed)
  expect_identical(fit$n_obs, s$n_obs)
  expect_identical(fit$n_obs, as.integer(prep$stan_data$N))
  # In season every team already has a state in the season being forecast.
  expect_identical(fit$carried, character())

  beliefs <- extract_posteriors(fit, prep$pred_d, league = s$league, sex = "male", fit_date = s$end_date)
  expect_identical(nrow(beliefs), s$n_pred * 200L)
  expect_true(all(beliefs$home_goals >= 0 & beliefs$away_goals >= 0))

  # What fit_league() saves: the draws under their production names and the
  # record of how they were made.
  file <- withr::local_tempfile(fileext = ".rds")
  fit$save_object(file = file)
  saved <- readRDS(file)
  expect_identical(saved$meta$engine, "inla")
  expect_identical(saved$meta$version, "2.0")
  expect_identical(saved$meta$n_obs, s$n_obs)
  expect_true(all(sprintf("goals1_pred[%d]", seq_len(s$n_pred)) %in% posterior::variables(saved$draws)))
  expect_identical(posterior::ndraws(saved$draws), 200L)

  # A round-mode replay at the end of a season has no fixture left.
  nothing_ahead <- prep
  nothing_ahead$pred_d <- prep$pred_d[0, ]
  bare <- suppressWarnings(suppressMessages(fit_model_v2_inla(
    nothing_ahead, s$league, "male",
    end_date = s$end_date, root = root,
    n_draws = 50L, seed = 7L, int_strategy = "eb", threads = "1:1"
  )))
  expect_true(bare$gate$passed)
  expect_false("goals1_pred" %in% bare$variables)
  expect_error(bare$draws("goals1_pred"), "no draws")
  expect_identical(posterior::ndraws(bare$draws("home_advantage_tot")), 50L)

  # A rerun in the same process, compared with a tolerance far below the
  # posterior sd and far above rounding; the two-process test above compares
  # exactly.
  again <- fit_once()
  mean_of <- function(f, v) colMeans(posterior::as_draws_matrix(f$draws(v)))
  expect_equal(mean_of(again, "home_advantage_tot"), mean_of(fit, "home_advantage_tot"), tolerance = 0.01)
  expect_equal(mean_of(again, "cur_strength_home"), mean_of(fit, "cur_strength_home"), tolerance = 0.01)
})

test_that("before a season an INLA fit carries every scheduled team across the break", {
  skip_if_no_inla()
  root <- withr::local_tempdir()
  s <- v2_store(root, preseason = TRUE)
  prep <- prepare_data(s$league, "male", end_date = s$end_date, root = root)
  # One fixture inside the horizon; the other four teams open four weeks later.
  expect_identical(nrow(prep$pred_d), 1L)
  fit <- suppressWarnings(suppressMessages(fit_model_v2_inla(
    prep, s$league, "male",
    end_date = s$end_date, root = root,
    n_draws = 100L, seed = 7L, int_strategy = "eb", threads = "1:1"
  )))
  expect_true(fit$gate$passed)
  expect_identical(fit$forecast_season, 2024L)
  expect_setequal(fit$carried, s$teams[3:6])
  beliefs <- extract_posteriors(fit, prep$pred_d, league = s$league, sex = "male", fit_date = s$end_date)
  expect_identical(nrow(beliefs), 100L)
})

test_that("an INLA fit's hyperparameters are stored and read back for the next fit's gate", {
  root <- withr::local_tempdir()
  league <- list(sport = "football", country = "iceland")
  record <- function(date, sds, version = FOOTBALL_V2_VERSION, passed = TRUE) {
    persist_fit_diagnostics_inla(
      list(
        gate = list(passed = passed, evaluated = 5L, skipped = 0L, jump_checked = TRUE),
        hyper_sd = sds, version = version
      ),
      league,
      sex = "male", fit_date = date, n_obs = 100L, n_draws = 400L, root = root
    )
  }
  expect_null(fv2_previous_hyper(league, "male", "2027-04-10", root))
  first <- c(tau_str = 0.40, sigma_str = 0.030)
  second <- c(tau_str = 0.41, sigma_str = 0.031)
  record("2027-04-08", first)
  record("2027-04-09", second)
  row <- as.data.frame(read_table("fit_diagnostics", root = root))
  expect_identical(unique(row$engine), "inla")
  expect_identical(unique(row$model_version), "2.0")

  expect_equal(fv2_previous_hyper(league, "male", "2027-04-10", root), second)
  # Strictly earlier: a rerun of the 9th is checked against the 8th.
  expect_equal(fv2_previous_hyper(league, "male", "2027-04-09", root), first)
  expect_null(fv2_previous_hyper(league, "male", "2027-04-08", root))
  expect_null(fv2_previous_hyper(league, "female", "2027-04-10", root))

  # A Stan fit's row, a row of another model version and a fit that failed
  # its gate are not references.
  stan_row <- row[1, ]
  stan_row$fit_date <- as.Date("2027-04-10")
  stan_row$engine <- NA_character_
  stan_row$model_version <- NA_character_
  stan_row$hyper_sd <- NA_character_
  write_table(tibble::as_tibble(stan_row), "fit_diagnostics", root = root)
  record("2027-04-11", c(tau_str = 9, sigma_str = 9), version = "9.9")
  record("2027-04-12", c(tau_str = 8, sigma_str = 8), passed = FALSE)
  expect_equal(fv2_previous_hyper(league, "male", "2027-04-13", root), second)
})

test_that("fit_league hands an INLA fit the previous fit's hyperparameters, and says when there are none", {
  root <- withr::local_tempdir()
  s <- v2_store(root)
  sds <- c(tau_str = 0.4, sigma_str = 0.03)
  seen <- list()
  testthat::local_mocked_bindings(
    fit_model_v2_inla = function(prep, league, sex, ...) {
      args <- list(...)
      seen[[length(seen) + 1L]] <<- args
      checked <- !is.null(args$previous_sd)
      list(
        save_object = function(file) saveRDS(NULL, file),
        gate = list(passed = TRUE, evaluated = 4L + checked, skipped = 1L - checked, jump_checked = checked),
        hyper_sd = sds, version = FOOTBALL_V2_VERSION
      )
    },
    extract_posteriors = function(...) tibble::tibble(),
    extract_football_iceland = function(...) invisible(NULL),
    .package = "sports"
  )
  inla <- utils::modifyList(s$league, list(engine = "inla"))
  run <- function(date) {
    fit_league(league = inla, sex = "male", fit_date = date, end_date = s$end_date, root = root, chains = 1L, iter_sampling = 50L)
  }
  first <- testthat::capture_messages(run(s$end_date))
  # A check that did not run is reported, not passed over.
  expect_true(any(grepl("hyperparameter-jump check will not run", first)))
  expect_null(seen[[1]]$previous_sd)

  second <- testthat::capture_messages(run(s$end_date + 1L))
  expect_false(any(grepl("will not run", second)))
  expect_equal(seen[[2]]$previous_sd, sds)
  rows <- as.data.frame(read_table("fit_diagnostics", root = root))
  rows <- rows[order(rows$fit_date), ]
  expect_identical(rows$jump_checked, c(FALSE, TRUE))
  expect_identical(rows$gates_skipped, c(1L, 0L))
})

test_that("a real INLA fit through fit_league is checked against the fit before it", {
  skip_if_no_inla()
  root <- withr::local_tempdir()
  s <- v2_store(root)
  inla <- utils::modifyList(s$league, list(engine = "inla"))
  testthat::local_mocked_bindings(extract_football_iceland = function(...) invisible(NULL), .package = "sports")
  run <- function(date) {
    suppressWarnings(fit_league(
      league = inla, sex = "male", fit_date = date, end_date = s$end_date, root = root,
      chains = 1L, iter_sampling = 100L, seed = 5L
    ))
  }
  rows <- function() {
    d <- as.data.frame(read_table("fit_diagnostics", root = root))
    d[order(d$fit_date), ]
  }
  expect_true(any(grepl("will not run", testthat::capture_messages(run(s$end_date)))))
  expect_identical(rows()$jump_checked, FALSE)
  suppressMessages(run(s$end_date + 1L))
  expect_identical(rows()$jump_checked, c(FALSE, TRUE))
  expect_identical(rows()$gates_skipped, c(1L, 0L))
  expect_identical(nrow(read_table("beliefs_latest", root = root)), s$n_pred * 100L)

  # The stored reference now says the innovation sd was 100 times larger:
  # the next fit's sd has collapsed against it, and the fit is refused.
  ref <- rows()[2, ]
  ref$fit_date <- as.Date(ref$fit_date)
  sds <- unlist(jsonlite::fromJSON(ref$hyper_sd))
  sds[["sigma_str"]] <- sds[["sigma_str"]] * 100
  ref$hyper_sd <- as.character(jsonlite::toJSON(as.list(sds), auto_unbox = TRUE, digits = NA))
  write_table(tibble::as_tibble(ref), "fit_diagnostics", root = root)
  expect_error(suppressMessages(run(s$end_date + 2L)), "moved by more than a factor")
  expect_identical(nrow(rows()), 2L)

  # An operator who has looked and agrees with the new values can say so.
  withr::local_envvar(SPORTS_INLA_ACCEPT_JUMP = "1")
  accepted <- testthat::capture_messages(run(s$end_date + 2L))
  expect_true(any(grepl("accepted", accepted)))
  expect_identical(nrow(rows()), 3L)
})
