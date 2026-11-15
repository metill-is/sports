# Football v2 prototype (research/football-v2/). The prototype is not package
# code, so its files are sourced here; nothing below needs INLA or CmdStan
# except the two smoke tests at the end, which skip without them.

fv2_dir <- testthat::test_path("..", "..", "research", "football-v2")
skip_if_not(dir.exists(fv2_dir), "research/football-v2 not present")
skip_if_not_installed("Matrix")
fv2 <- new.env()
for (f in c("spec.R", "engines.R", "score.R")) sys.source(file.path(fv2_dir, f), envir = fv2)

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
    clock = "break", rho = 0.8, divisions = toy_divisions()
  )))
  sd <- fv2$fv2_stan_data(fv2$fv2_build(toy_matches(), toy_fixture(), clock = "break"))
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

test_that("the Stan program compiles and reads the spec", {
  skip_on_ci()
  skip_if(Sys.getenv("FV2_TEST_STAN") != "1", "set FV2_TEST_STAN=1 to compile and sample")
  skip_if_not_installed("cmdstanr")
  sim <- simulate_league()
  spec <- fv2$fv2_build(sim, clock = "break")
  tmp <- withr::local_tempdir()
  f <- fv2$fv2_fit_stan(
    spec, file.path(fv2_dir, "stan", "fv2.stan"), tmp, tmp,
    chains = 2L, iter_warmup = 150L, iter_sampling = 100L, refresh = 0L
  )
  expect_identical(dim(f$draws$att), c(200L, spec$S))
  expect_identical(colnames(f$draws$hyper)[1:3], c("tau_att", "sigma_att", "kappa_att"))
  expect_lt(max(abs(rowSums(f$draws$att[, spec$first_slot]))), 1e-8)
})
