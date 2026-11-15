# Football v2 prototype (research/football-v2/). The prototype is not package
# code, so its files are sourced here; nothing below needs INLA or CmdStan
# except the two smoke tests at the end, which skip without them.

fv2_dir <- testthat::test_path("..", "..", "research", "football-v2")
skip_if_not(dir.exists(fv2_dir), "research/football-v2 not present")
skip_if_not_installed("Matrix")
fv2 <- new.env()
for (f in c("spec.R", "engines.R", "score.R", "evaluate.R", "adapter.R", "data.R")) {
  sys.source(file.path(fv2_dir, f), envir = fv2)
}

toy_matches <- function() {
  data.frame(
    match_date = as.Date(c(
      "2021-05-03", "2021-05-06", "2021-05-12", "2021-09-20",
      "2022-05-02", "2022-05-11"
    )),
    home_team = c("A", "B", "A", "C", "A", "B"),
    away_team = c("B", "A", "C", "B", "B", "C"),
    home_score = c(2L, 0L, 1L, 3L, 1L, 2L),
    away_score = c(1L, 0L, 1L, 0L, 0L, 2L),
    season = c(2021L, 2021L, 2021L, 2021L, 2022L, 2022L),
    cell = c("L@2021", "L@2021", "L@2021", "L@2021", "L@2022", "L@2022")
  )
}

toy_fixture <- function() {
  data.frame(
    match_date = as.Date("2022-05-18"), home_team = "C", away_team = "A",
    home_score = NA_integer_, away_score = NA_integer_, season = 2022L, cell = "L@2022"
  )
}

toy_divisions <- function() {
  data.frame(
    team = c("A", "B", "C", "A", "B", "C"),
    season = c(2021L, 2021L, 2021L, 2022L, 2022L, 2022L),
    division = c("L1", "L1", "L2", "L1", "L2", "L1"),
    rank = c(1L, 1L, 2L, 1L, 2L, 1L)
  )
}

# Covariance of one field implied by the steps Stan reads: every team starts
# at N(0, tau^2) and adds independent steps.
stan_form_cov <- function(spec, tau, sigma, kappa = 0) {
  sd <- fv2$fv2_stan_data(spec)
  step_var <- (sigma * sd$step_in + kappa * sd$step_brk)^2
  cum <- unlist(tapply(step_var, sd$slot_team, cumsum))
  same <- outer(sd$slot_team, sd$slot_team, "==")
  (tau^2 + outer(cum, cum, pmin)) * same
}

test_that("weeks run Monday to Sunday from a fixed Monday", {
  expect_identical(weekdays(fv2$FV2_EPOCH), weekdays(as.Date("2024-01-01")))
  monday <- as.Date("2026-07-27")
  expect_identical(fv2$fv2_week(monday), fv2$fv2_week(monday + 6))
  expect_identical(fv2$fv2_week(monday + 7), fv2$fv2_week(monday) + 1L)
  expect_identical(fv2$fv2_week(monday - 1), fv2$fv2_week(monday) - 1L)
})

test_that("a PC prior on the precision is an exponential prior on the sd", {
  rate <- fv2$fv2_pc_rate(c(0.3, 0.01))
  expect_equal(stats::pexp(0.3, rate, lower.tail = FALSE), 0.01)
})

test_that("one state slot per team and week, fixtures included", {
  spec <- fv2$fv2_build(toy_matches(), toy_fixture())
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
  expect_error(fv2$fv2_build(toy_matches(), bad))
})

test_that("the calendar clock counts weeks, the break clock counts seasons", {
  cal <- fv2$fv2_build(toy_matches(), toy_fixture(), clock = "calendar")
  brk <- fv2$fv2_build(toy_matches(), toy_fixture(), clock = "break")
  expect_true(all(cal$steps$class == "in"))
  expect_identical(sum(brk$steps$class == "brk"), 3L)
  a_break <- brk$steps$team == 1L & brk$steps$class == "brk"
  expect_equal(brk$steps$delta[a_break], 1)
  expect_equal(
    cal$steps$delta[a_break],
    as.numeric(fv2$fv2_week("2022-05-02") - fv2$fv2_week("2021-05-12"))
  )
  expect_identical(brk$steps$delta[!a_break & brk$steps$class == "in"], cal$steps$delta[!a_break & brk$steps$class == "in"])
})

test_that("the INLA precision and the Stan steps describe the same prior", {
  tau <- 0.6
  sigma <- 0.05
  kappa <- 0.25
  cal <- fv2$fv2_build(toy_matches(), toy_fixture(), clock = "calendar")
  r <- fv2$fv2_structure(cal)
  expect_named(r, c("init", "in"))
  q <- r$init / tau^2 + r$`in` / sigma^2
  expect_equal(as.matrix(solve(q)), stan_form_cov(cal, tau, sigma), ignore_attr = TRUE)

  brk <- fv2$fv2_build(toy_matches(), toy_fixture(), clock = "break")
  r <- fv2$fv2_structure(brk)
  expect_named(r, c("init", "in", "brk"))
  q <- r$init / tau^2 + r$`in` / sigma^2 + r$brk / kappa^2
  expect_equal(as.matrix(solve(q)), stan_form_cov(brk, tau, sigma, kappa), ignore_attr = TRUE)
})

test_that("a step across d weeks has d times the one-week variance", {
  spec <- fv2$fv2_build(toy_matches())
  r <- fv2$fv2_structure(spec)
  v <- as.matrix(solve(r$init / 0.5^2 + r$`in` / 0.1^2))
  a <- spec$slots$slot[spec$slots$team == 1L]
  weeks <- spec$slots$week[a] - spec$slots$week[a[1]]
  expect_equal(diag(v)[a], 0.5^2 + 0.1^2 * weeks)
})

test_that("reversion pulls a staying team toward its division mean", {
  rho <- 0.8
  spec <- fv2$fv2_build(
    toy_matches(),
    clock = "break", rho = rho, divisions = toy_divisions()
  )
  st <- spec$steps
  # A stays in L1 and reverts; B (relegated) and C (promoted) take plain jumps.
  expect_identical(st$rho[st$class == "brk"], c(rho, 1, 1))
  expect_identical(st$move[st$class == "brk"], c(0, -1, 1))
  d <- st[st$class == "brk" & st$team == 1L, ]
  q <- fv2$fv2_structure(spec)$brk
  # L1 in 2021 held A and B; only A's break reverts, toward A's own end state.
  expect_equal(q[d$slot, d$prev], -(rho + (1 - rho) / 1))
  expect_equal(q[d$slot, d$slot], 1)
})

# Covariance of one field implied by the season-segment recursion Stan uses
# for a reverting break: the states are linear in (entry levels, innovations).
recursion_cov <- function(spec, tau, sigma, kappa, rho) {
  seg <- fv2$fv2_segments(spec)
  n <- spec$K + spec$S + seg$n_seg
  a <- vapply(seq_len(n), function(i) {
    e <- numeric(n)
    e[i] <- 1
    fv2$fv2_walk(
      seg, tau * e[seq_len(spec$K)], e[spec$K + seq_len(spec$S)],
      e[spec$K + spec$S + seq_len(seg$n_seg)], sigma, kappa, rho
    )
  }, numeric(spec$S))
  tcrossprod(a)
}

test_that("the reverting break is the same prior in INLA's and Stan's form", {
  tau <- 0.6
  sigma <- 0.05
  kappa <- 0.25
  for (rho in c(1, 0.8, 0.5)) {
    spec <- fv2$fv2_build(
      toy_matches(), toy_fixture(),
      clock = "break", rho = rho, divisions = toy_divisions()
    )
    r <- fv2$fv2_structure(spec)
    q <- r$init / tau^2 + r$`in` / sigma^2 + r$brk / kappa^2
    expect_equal(as.matrix(solve(q)), recursion_cov(spec, tau, sigma, kappa, rho), ignore_attr = TRUE)
  }
})

test_that("segments are one team's run of slots within a season", {
  spec <- fv2$fv2_build(
    toy_matches(), toy_fixture(),
    clock = "break", rho = 0.8, divisions = toy_divisions()
  )
  seg <- fv2$fv2_segments(spec)
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
  cal <- fv2$fv2_build(toy_matches())
  brk <- fv2$fv2_build(toy_matches(), clock = "break")
  rev <- fv2$fv2_build(toy_matches(), clock = "break", rho = 0.8, divisions = toy_divisions())
  sq <- fv2$fv2_build(toy_matches(), param = "sq", style = "none")
  expect_identical(fv2$fv2_stan_program(cal), "fv2.stan")
  expect_identical(fv2$fv2_stan_program(brk), "fv2.stan")
  expect_identical(fv2$fv2_stan_program(brk, est_rho = TRUE), "fv2_rev.stan")
  expect_identical(fv2$fv2_stan_program(rev), "fv2_rev.stan")
  expect_identical(fv2$fv2_stan_program(sq), "fv2_sq.stan")
  expect_identical(fv2$fv2_stan_program(cal, est_rho = TRUE), "fv2.stan")
  d <- fv2$fv2_stan_data(rev)
  expect_identical(d$n_field, 2L)
  expect_identical(d$rho_fixed, 0.8)
  expect_identical(d$est_rho, 0L)
  expect_identical(fv2$fv2_stan_data(brk, est_rho = TRUE)$est_rho, 1L)
})

test_that("movers get their own step class and a cumulative shock covariate", {
  spec <- fv2$fv2_build(
    toy_matches(), toy_fixture(),
    clock = "break", movers = TRUE, divisions = toy_divisions()
  )
  expect_identical(sum(spec$steps$class == "mov"), 2L)
  expect_named(fv2$fv2_structure(spec), c("init", "in", "brk", "mov"))
  c_slots <- spec$slots$team == 3L
  expect_identical(spec$promoted[c_slots], as.numeric(spec$slots$season[c_slots] == 2022L))
  expect_identical(sum(spec$relegated[spec$slots$team == 2L & spec$slots$season == 2022L] == 1), 2L)
  expect_true(all(spec$promoted[spec$slots$team == 1L] == 0))
  expect_error(fv2$fv2_build(toy_matches(), clock = "break", movers = TRUE))
  expect_error(fv2$fv2_build(toy_matches(), clock = "calendar", movers = TRUE, divisions = toy_divisions()))
})

test_that("the shock covariate in the design is what prediction applies", {
  spec <- fv2$fv2_build(
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
  p <- fv2$fv2_predict(draws, spec)
  des <- fv2$fv2_design(spec, "pred")
  via_design <- beta * des$promoted - 0.2 * des$relegated
  expect_equal(log(c(p$lam_home, p$lam_away)), via_design)
  # C was promoted and hosts A, which was not: C's rows gain, A's lose.
  expect_equal(via_design, c(beta, -beta))
  train <- fv2$fv2_design(spec, "obs")
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
  spec <- fv2$fv2_build(toy_matches(), toy_fixture(), div_offsets = TRUE)
  des <- fv2$fv2_design(spec)
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
  expect_identical(nrow(fv2$fv2_design(spec, "pred")$att), 2L)
})

test_that("strength and style are a linear re-expression of attack and defence", {
  spec <- fv2$fv2_build(toy_matches())
  des <- fv2$fv2_design(spec)
  set.seed(1)
  s <- stats::rnorm(spec$S)
  q <- stats::rnorm(spec$S)
  via_sq <- (des$att + des$def) %*% s + (des$att - des$def) %*% q
  via_ad <- des$att %*% (s + q) + des$def %*% (s - q)
  expect_equal(as.numeric(via_sq), as.numeric(via_ad))
})

test_that("the Stan data refuses variants the Stan program does not cover", {
  expect_error(fv2$fv2_stan_data(fv2$fv2_build(toy_matches(), param = "sq")))
  expect_error(fv2$fv2_stan_data(fv2$fv2_build(
    toy_matches(),
    clock = "break", movers = TRUE, divisions = toy_divisions()
  )))
  sq <- fv2$fv2_stan_data(fv2$fv2_build(toy_matches(), param = "sq", style = "static"))
  expect_identical(sq$use_style, 1L)
  expect_identical(fv2$fv2_stan_data(fv2$fv2_build(toy_matches(), param = "sq", style = "none"))$use_style, 0L)
  sd <- fv2$fv2_stan_data(fv2$fv2_build(toy_matches(), toy_fixture(), clock = "break"))
  expect_identical(sd$use_style, 0L)
  expect_identical(sd$use_break, 1L)
  expect_identical(sd$step_in[sd$first_slot], rep(0, 3))
  expect_identical(sum(sd$step_brk > 0), 3L)
  expect_true(all((sd$step_in > 0) + (sd$step_brk > 0) <= 1))
})

test_that("predictions combine intercept, states and home advantage", {
  spec <- fv2$fv2_build(toy_matches(), toy_fixture(), div_offsets = TRUE)
  n_draw <- 2L
  draws <- list(
    c_home = c(0.4, 0.5), c_away = c(0.1, 0.2),
    att = matrix(seq_len(n_draw * spec$S) / 100, n_draw),
    def = matrix(seq_len(n_draw * spec$S) / 200, n_draw),
    ha_att = matrix(0.05, n_draw, spec$K), ha_def = matrix(-0.02, n_draw, spec$K),
    g_goal = matrix(c(0, 0, 0.1, 0.2), n_draw), g_ha = matrix(c(0, 0, 0.04, 0.06), n_draw)
  )
  p <- fv2$fv2_predict(draws, spec)
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

test_that("scores match a direct Poisson calculation", {
  lam_h <- matrix(c(1.2, 1.8, 0.7, 0.9), 2, byrow = TRUE)
  lam_a <- matrix(c(0.9, 1.1, 2.0, 2.4), 2, byrow = TRUE)
  sc <- fv2$fv2_score(lam_h, lam_a, c(2L, 0L), c(1L, 0L))
  expect_equal(sc$p_home + sc$p_draw + sc$p_away, c(1, 1))
  grid <- function(i) {
    g <- 0:fv2$FV2_MAX_GOALS
    m <- Reduce(`+`, lapply(1:2, function(d) outer(dpois(g, lam_h[i, d]), dpois(g, lam_a[i, d])))) / 2
    m / sum(m)
  }
  g1 <- grid(1)
  expect_equal(sc$ls_score[1], log(g1[3, 2]))
  expect_equal(sc$p_home[1], sum(g1[lower.tri(g1)]))
  expect_equal(sc$ls_1x2[1], log(sc$p_home[1]))
  g2 <- grid(2)
  expect_equal(sc$ls_1x2[2], log(sum(diag(g2))))
  expect_equal(sc$xg_home, rowMeans(lam_h))
  expect_true(all(sc$rps >= 0 & sc$rps <= 1))
  expect_equal(
    fv2$fv2_log_score_1x2(0.5, 0.3, 0.2, c(1, 1, 0), c(0, 1, 2)),
    log(c(0.5, 0.3, 0.2))
  )
})

test_that("a home win is certain when the home rate dwarfs the away rate", {
  sc <- fv2$fv2_score(matrix(8, 1, 3), matrix(0.01, 1, 3), 5L, 0L)
  expect_gt(sc$p_home, 0.99)
  expect_lt(sc$rps, 1e-3)
})

test_that("de-vigged odds sum to one under both methods", {
  odds <- rbind(c(1.25, 5.92, 7.23), c(2.4, 3.3, 2.9))
  prop <- fv2$fv2_devig(odds)
  pow <- fv2$fv2_devig(odds, "power")
  expect_equal(rowSums(prop), c(1, 1))
  expect_equal(rowSums(pow), c(1, 1))
  expect_equal(prop[1, ] / prop[1, 1], (1 / odds[1, ]) / (1 / odds[1, 1]), ignore_attr = TRUE)
  # The power method takes more of the margin from the long shots.
  expect_lt(pow[1, "away"], prop[1, "away"])
  expect_gt(pow[1, "home"], prop[1, "home"])
  fair <- 1 / c(0.5, 0.3, 0.2)
  expect_equal(as.numeric(fv2$fv2_devig(rbind(fair), "power")), c(0.5, 0.3, 0.2))
  expect_equal(as.numeric(fv2$fv2_devig(rbind(fair))), c(0.5, 0.3, 0.2))
})

test_that("season phase follows the round, play-offs are late, cups borrow", {
  m <- data.frame(
    match_date = as.Date("2025-04-07") + 7 * c(0:17, 18, 1, 16),
    season = 2025L,
    round = c(1:18, 1, 1, 4),
    division = c(rep("BD", 18), "BD_UPPER_PO", "CUP", "CUP"),
    league_division = c(rep("BD", 18), "BD", NA, NA)
  )
  ph <- as.character(fv2$fv2_phase(m))
  expect_identical(ph[1:18], rep(c("early", "middle", "late"), c(5, 7, 6)))
  expect_identical(ph[19], "late")
  expect_identical(ph[20:21], c("early", "late"))
})

test_that("the paired interval is the usual one without clusters", {
  d <- c(0.03, -0.01, 0.02, 0.05, -0.04, 0.01)
  out <- fv2$fv2_paired(d)
  expect_equal(out$mean, mean(d))
  expect_equal(out$se, stats::sd(d) / sqrt(length(d)))
  expect_equal(out$hi - out$lo, 2 * stats::qnorm(0.975) * out$se)
  expect_identical(out$n, 6L)
})

test_that("clustering widens the interval when matches move together", {
  d <- c(0.04, 0.05, 0.03, -0.04, -0.05, -0.03)
  cl <- rep(1:2, each = 3)
  plain <- fv2$fv2_paired(d)
  clustered <- fv2$fv2_paired(d, cl)
  expect_identical(clustered$n_cluster, 2L)
  sums <- tapply(d - mean(d), cl, sum)
  expect_equal(clustered$se, sqrt(2 / 1 * sum(sums^2)) / 6)
  expect_gt(clustered$se, plain$se)
  expect_identical(fv2$fv2_paired(c(d, NA), c(cl, 2L))$n, 6L)
})

test_that("hyperparameter names follow the step classes and parameterisation", {
  brk <- fv2$fv2_build(toy_matches(), clock = "break")
  expect_identical(
    fv2$fv2_hyper_names(brk),
    list(att = c("tau_att", "sigma_att", "kappa_att"), def = c("tau_def", "sigma_def", "kappa_def"))
  )
  sq <- fv2$fv2_build(toy_matches(), param = "sq", style = "static")
  expect_identical(fv2$fv2_hyper_names(sq), list(str = c("tau_str", "sigma_str")))
})

simulate_league <- function(n_team = 6L, n_week = 30L, seed = 11L) {
  set.seed(seed)
  teams <- paste0("T", seq_len(n_team))
  strength <- stats::rnorm(n_team, 0, 0.4)
  rows <- lapply(seq_len(n_week), function(w) {
    pair <- matrix(sample(n_team), ncol = 2)
    data.frame(
      match_date = fv2$FV2_EPOCH + 7 * (w + 20 * (w > n_week / 2)),
      home = pair[, 1], away = pair[, 2], season = 2021L + (w > n_week / 2)
    )
  })
  d <- do.call(rbind, rows)
  data.frame(
    match_date = d$match_date, home_team = teams[d$home], away_team = teams[d$away],
    home_score = stats::rpois(nrow(d), exp(0.45 + strength[d$home] - strength[d$away])),
    away_score = stats::rpois(nrow(d), exp(0.25 + strength[d$away] - strength[d$home])),
    season = d$season, cell = paste0("L@", d$season)
  )
}

test_that("the INLA engine returns draws in the shared layout", {
  skip_on_ci()
  skip_if_not_installed("INLA")
  sim <- simulate_league()
  train <- sim[sim$match_date < max(sim$match_date), ]
  pred <- sim[sim$match_date == max(sim$match_date), ]
  spec <- fv2$fv2_build(train, pred, clock = "break")
  f <- suppressWarnings(fv2$fv2_fit_inla(spec, int_strategy = "eb", n_samp = 50L, threads = "1:1"))
  expect_identical(dim(f$draws$att), c(50L, spec$S))
  expect_identical(dim(f$draws$ha_def), c(50L, spec$K))
  expect_identical(colnames(f$draws$hyper), c(unlist(fv2$fv2_hyper_names(spec), use.names = FALSE), "omega_att", "omega_def"))
  expect_lt(max(abs(rowSums(f$draws$att[, spec$first_slot]))), 1e-6)
  p <- fv2$fv2_predict(f$draws, spec)
  expect_identical(dim(p$lam_home), c(nrow(pred), 50L))
  expect_true(all(is.finite(p$lam_home)) && all(p$lam_home > 0))

  held <- suppressWarnings(fv2$fv2_fit_inla(
    spec,
    int_strategy = "eb", n_samp = 20L, threads = "1:1",
    theta_init = f$theta_mode, theta_fixed = TRUE
  ))
  expect_equal(held$draws$hyper[1, ], f$draws$hyper[1, ], tolerance = 1e-6)
})

test_that("the Stan programs compile and read the spec", {
  skip_on_ci()
  skip_if(Sys.getenv("FV2_TEST_STAN") != "1", "set FV2_TEST_STAN=1 to compile and sample")
  skip_if_not_installed("cmdstanr")
  sim <- simulate_league()
  tmp <- withr::local_tempdir()
  stan_dir <- file.path(fv2_dir, "stan")
  run <- function(spec) {
    fv2$fv2_fit_stan(
      spec, fv2$fv2_stan_file(spec, stan_dir), tmp, tmp,
      chains = 2L, iter_warmup = 150L, iter_sampling = 100L, refresh = 0L
    )
  }
  ad <- fv2$fv2_build(sim, clock = "break")
  f <- run(ad)
  expect_identical(dim(f$draws$att), c(200L, ad$S))
  expect_identical(colnames(f$draws$hyper)[1:3], c("tau_att", "sigma_att", "kappa_att"))
  # CmdStan writes six significant figures, so "zero" is about 1e-7.
  expect_lt(max(abs(rowSums(f$draws$att[, ad$first_slot]))), 1e-6)

  sq <- fv2$fv2_build(sim, param = "sq", style = "static")
  expect_identical(basename(fv2$fv2_stan_file(sq, stan_dir)), "fv2_sq.stan")
  g <- run(sq)
  expect_identical(colnames(g$draws$hyper), c("tau_str", "sigma_str", "tau_sty", "omega_att", "omega_def"))
  # Attack minus defence is twice the style: fixed over a team's slots.
  style <- (g$draws$att - g$draws$def) / 2
  a <- sq$slots$slot[sq$slots$team == 1L]
  expect_lt(max(abs(style[, a] - style[, a[1]])), 1e-6)
  expect_lt(max(abs(rowSums(style[, sq$first_slot]))), 1e-6)

  div <- unique(data.frame(
    team = c(sim$home_team, sim$away_team), season = rep(sim$season, 2),
    division = "L", rank = 1L
  ))
  rev <- fv2$fv2_build(sim, clock = "break", rho = 0.8, divisions = div)
  h <- fv2$fv2_fit_stan(
    rev, fv2$fv2_stan_file(rev, stan_dir, est_rho = TRUE), tmp, tmp,
    chains = 2L, iter_warmup = 150L, iter_sampling = 100L, refresh = 0L, est_rho = TRUE
  )
  expect_identical(
    colnames(h$draws$hyper),
    c("tau_att", "sigma_att", "kappa_att", "tau_def", "sigma_def", "kappa_def", "omega_att", "omega_def", "rho")
  )
  expect_true(all(h$draws$hyper[, "rho"] > 0 & h$draws$hyper[, "rho"] < 1))
  expect_identical(dim(h$draws$att), c(200L, rev$S))
})

toy_draws <- function(spec, n_draw = 3L, seed = 5L) {
  set.seed(seed)
  list(
    c_home = stats::rnorm(n_draw, 0.5, 0.05), c_away = stats::rnorm(n_draw, 0.3, 0.05),
    att = matrix(stats::rnorm(n_draw * spec$S, 0, 0.3), n_draw),
    def = matrix(stats::rnorm(n_draw * spec$S, 0, 0.3), n_draw),
    ha_att = matrix(stats::rnorm(n_draw * spec$K, 0, 0.05), n_draw),
    ha_def = matrix(stats::rnorm(n_draw * spec$K, 0, 0.05), n_draw)
  )
}

# What prepare_data() hands over for the toy league, with teams numbered in a
# different order from the spec's so the adapter has to map them.
toy_prep <- function() {
  m <- toy_matches()
  teams <- data.frame(team = c("C", "A", "B"), team_nr = 1:3)
  nr <- function(x) match(x, teams$team)
  appearance <- function(team, i) sum(c(m$home_team[seq_len(i)], m$away_team[seq_len(i)]) == team)
  list(
    teams = teams,
    stan_data = list(
      N = nrow(m), N_rounds = 5L,
      team1 = nr(m$home_team), team2 = nr(m$away_team),
      round1 = vapply(seq_len(nrow(m)), function(i) appearance(m$home_team[i], i), integer(1)),
      round2 = vapply(seq_len(nrow(m)), function(i) appearance(m$away_team[i], i), integer(1)),
      goals1 = m$home_score, goals2 = m$away_score
    ),
    pred_d = data.frame(
      game_nr = 1L, match_date = as.Date("2022-05-18"), home_team = "C", away_team = "A",
      division = "L"
    )
  )
}

test_that("the adapter speaks the production fit's variable names", {
  prep <- toy_prep()
  pred <- fv2$fv2_pred_from_prep(prep$pred_d)
  expect_identical(pred$season, 2022L)
  expect_identical(pred$cell, "L@2022")
  spec <- fv2$fv2_build(toy_matches(), pred)
  draws <- toy_draws(spec)
  fit <- fv2$fv2_as_fit(draws, spec, prep)
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
  # Team "A" is production's team 2 and the spec's team 1.
  a_slots <- spec$slots$slot[spec$slots$team == 1L & spec$slots$has_obs]
  expect_equal(get("cur_offense_away")[, 2], draws$att[, max(a_slots)], ignore_attr = TRUE)
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

test_that("production's extract_posteriors reads the adapted fit", {
  prep <- toy_prep()
  spec <- fv2$fv2_build(toy_matches(), fv2$fv2_pred_from_prep(prep$pred_d))
  fit <- fv2$fv2_as_fit(toy_draws(spec, n_draw = 4L), spec, prep)
  b <- extract_posteriors(
    fit, prep$pred_d, list(sport = "football", country = "iceland"), "male",
    fit_date = as.Date("2022-05-15")
  )
  expect_identical(nrow(b), 4L)
  expect_identical(b$home_team, rep("C", 4))
  expect_identical(sort(b$draw_id), 1:4)
  expect_true(all(b$home_goals >= 0))
})

test_that("the adapter refuses training rows in a different order", {
  prep <- toy_prep()
  spec <- fv2$fv2_build(toy_matches()[c(2, 1, 3:6), ], fv2$fv2_pred_from_prep(prep$pred_d))
  expect_error(fv2$fv2_as_fit(toy_draws(spec), spec, prep))
})

test_that("division codes fold into leagues and rank from the top", {
  expect_identical(
    fv2$fv2_league_division(c("BD", "BD_UPPER_PO", "BD_LOWER_PO", "LD1_PO", "CUP", "LD3")),
    c("BD", "BD", "BD", "LD1", NA, "LD3")
  )
  expect_identical(fv2$fv2_cell(c("BD_UPPER_PO", "CUP"), 2026L), c("BD@2026", "CUP@2026"))
  league <- list(training_filter = list(divisions = c("BD", "BD_UPPER_PO", "BD_LOWER_PO", "LD1", "LD2")))
  expect_identical(as.character(fv2$fv2_rank_order(league)), c("BD", "LD1", "LD2"))
  m <- data.frame(
    home_team = c("A", "A", "B", "A"), away_team = c("B", "C", "C", "B"),
    season = c(2025L, 2025L, 2025L, 2026L),
    league_division = c("LD1", NA, "LD1", "BD")
  )
  tab <- fv2$fv2_division_table(m, c("BD", "LD1"))
  expect_identical(tab$division[tab$team == "A"], c("LD1", "BD"))
  expect_identical(tab$rank[tab$team == "A"], c(2L, 1L))
  expect_identical(tab$division[tab$team == "C"], "LD1")
  expect_identical(fv2$fv2_division_table(m, "BD")$rank[1], 2L)
})

toy_wf <- function() {
  one <- function(variant, shift) {
    data.frame(
      sex = rep(c("male", "female"), each = 3), variant = variant,
      match_date = as.Date("2025-05-01") + c(0, 1, 7, 0, 1, 7),
      home_team = c("A", "C", "A", "E", "G", "E"), away_team = c("B", "D", "C", "F", "H", "G"),
      home_score = c(2, 0, 1, 3, 1, 0), away_score = c(1, 0, 2, 0, 1, 2),
      cutoff = as.Date("2025-04-27") + c(0, 0, 7, 0, 0, 7),
      phase = factor(c("early", "early", "late", "early", "early", "late"), c("early", "middle", "late")),
      ls_1x2 = c(-0.9, -1.2, -1.0, -0.6, -1.3, -1.1) + shift,
      ls_score = c(-3.1, -2.8, -3.3, -3.6, -2.9, -3.2) + shift,
      rps = 0.2, p_home = c(0.5, 0.4, 0.45, 0.6, 0.35, 0.5)
    )
  }
  d <- rbind(one("base", 0), one("alt", c(0.02, 0.01, 0.03, -0.01, 0.02, 0.01)))
  d$key <- fv2$fv2_match_key(d)
  d
}

test_that("score tables report losses and the home-win rate per group", {
  d <- toy_wf()
  tab <- fv2$fv2_score_table(d, c("variant", "sex"))
  expect_identical(nrow(tab), 4L)
  row <- tab[tab$variant == "base" & tab$sex == "male", ]
  expect_identical(row$n, 3L)
  expect_equal(row$logloss_1x2, mean(c(0.9, 1.2, 1.0)))
  expect_equal(row$scoreline, mean(c(3.1, 2.8, 3.3)))
  expect_equal(row$obs_home, 1 / 3)
  expect_equal(row$p_home, mean(c(0.5, 0.4, 0.45)))
})

test_that("paired comparisons use only the matches both variants predicted", {
  d <- toy_wf()
  cmp <- fv2$fv2_compare(d, "base")
  shift <- c(0.02, 0.01, 0.03, -0.01, 0.02, 0.01)
  expect_identical(cmp$n, 6L)
  expect_equal(cmp$diff, mean(shift))
  expect_equal(cmp$se, stats::sd(shift) / sqrt(6))
  by_sex <- fv2$fv2_compare(d, "base", "sex")
  expect_equal(by_sex$diff[by_sex$group == "female"], mean(shift[4:6]))
  dropped <- d[!(d$variant == "alt" & d$home_team == "A"), ]
  expect_identical(fv2$fv2_compare(dropped, "base")$n, 4L)
  expect_equal(fv2$fv2_compare(d, "base", metric = "ls_score")$diff, mean(shift))
})

test_that("an outside forecast is scored on the matches it shares", {
  d <- toy_wf()
  ext <- data.frame(
    key = d$key[d$variant == "base"][1:2],
    line_home = c(0.5, 0.2), line_draw = c(0.3, 0.5), line_away = c(0.2, 0.3)
  )
  b <- fv2$fv2_benchmark(d, ext, "line", "base")
  expect_identical(nrow(b), 2L)
  expect_equal(b$ls_line, log(c(0.5, 0.5)))
  expect_equal(b$ls_model, c(-0.9, -1.2))
  expect_identical(b$phome_line, c(0.5, 0.2))
})
