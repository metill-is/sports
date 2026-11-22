test_that("Elo updates match the World Football Elo formula", {
  r <- nt_test_results(
    "2024-01-01", "A", "B", 1, 0, "Friendly", "X", TRUE,
    "2024-01-02", "C", "D", 3, 0, "FIFA World Cup", "C", FALSE
  )
  e <- nt_elo(r)
  expect_equal(e$pre_diff, c(0, 100))
  expect_equal(unname(e$ratings[c("A", "B")]), c(1510, 1490))
  expected_home <- 1 / (1 + 10^(-100 / 400))
  step <- 60 * 1.75 * (1 - expected_home)
  expect_equal(unname(e$ratings[c("C", "D")]), c(1500 + step, 1500 - step))
})

test_that("Elo K factors follow the competition tiers", {
  expect_equal(
    .nt_elo_k(c(
      "FIFA World Cup", "UEFA Euro", "FIFA World Cup qualification",
      "UEFA Nations League", "Friendly", "Copa América"
    )),
    c(60, 50, 40, 30, 20, 50)
  )
})

test_that("the ordered logit recovers its parameters", {
  withr::local_seed(42)
  x <- stats::rnorm(20000)
  u <- stats::runif(20000)
  p_le_a <- stats::plogis(-0.6 - 0.8 * x)
  p_le_d <- stats::plogis(0.5 - 0.8 * x)
  outcome <- ifelse(u < p_le_a, 1L, ifelse(u < p_le_d, 2L, 3L))
  fit <- .nt_ordered_logit(x, outcome)
  expect_equal(fit$slope, 0.8, tolerance = 0.05)
  expect_equal(fit$cut_away, -0.6, tolerance = 0.05)
  expect_equal(fit$cut_draw, 0.5, tolerance = 0.05)
})

test_that("the Elo baseline ignores everything on or after the cutoff", {
  withr::local_seed(1)
  n <- 400L
  teams <- LETTERS[1:8]
  home <- sample(teams, n, replace = TRUE)
  away <- vapply(home, function(h) sample(setdiff(teams, h), 1L), "")
  r <- tibble::tibble(
    match_date = as.Date("2015-01-01") + sort(sample.int(3000L, n)),
    home_team = home, away_team = unname(away),
    home_score = stats::rpois(n, 1.5), away_score = stats::rpois(n, 1.1),
    tournament = "Friendly", country = home, neutral = FALSE
  )
  cutoff <- as.Date("2020-01-01")
  r$match_date[200L] <- cutoff
  r <- r[order(r$match_date), ]
  tampered <- r
  late <- tampered$match_date >= cutoff
  expect_true(any(tampered$match_date == cutoff))
  tampered$home_score[late] <- 9L
  a <- nt_elo_baseline(r, cutoff, c("A", "B"), c("C", "D"), venue = c(1, 0))
  b <- nt_elo_baseline(tampered, cutoff, c("A", "B"), c("C", "D"), venue = c(1, 0))
  expect_identical(a$probs, b$probs)
  before <- r
  before$home_score[before$match_date == max(before$match_date[before$match_date < cutoff])] <- 9L
  c_ <- nt_elo_baseline(before, cutoff, c("A", "B"), c("C", "D"), venue = c(1, 0))
  expect_false(identical(a$probs, c_$probs))
  expect_equal(a$probs$p_home + a$probs$p_draw + a$probs$p_away, c(1, 1))
  expect_equal(apply(a$grid, 1L, sum), c(1, 1), tolerance = 1e-6)
})

test_that("production rates reproduce the production match primitive", {
  team <- tibble::tibble(
    team = rep(c("A", "B"), each = 2L), .draw = rep(1:2, 2L),
    cur_offense = c(0.3, 0.2, -0.1, 0), cur_defense = c(0.1, 0.2, 0.05, -0.1),
    home_advantage_off = c(0.2, 0.1, 0.3, 0.2), home_advantage_def = c(0.1, 0.1, 0.2, 0.1)
  )
  scalar <- tibble::tibble(
    .draw = 1:2, mean_log_goals = c(0.2, 0.3), alpha_mu3 = c(-2, -1.5),
    beta_mu3_strength_diff = c(-0.5, -0.2)
  )
  r <- nt_production_rates(list(team = team, scalar = scalar), c("A", "A"), c("B", "B"), venue = c(1, 0))
  for (d in 1:2) {
    pick <- function(col) stats::setNames(team[[col]][team$.draw == d], c("A", "B"))
    for (v in 1:2) {
      want <- .wc_match_lambdas(
        "A", "B", c("home", "neutral")[v],
        pick("cur_offense"), pick("cur_defense"),
        pick("home_advantage_off"), pick("home_advantage_def"),
        scalar$mean_log_goals[d], scalar$alpha_mu3[d], scalar$beta_mu3_strength_diff[d]
      )
      expect_equal(c(r$home[d, v], r$away[d, v], r$shared[d, v]), want, ignore_attr = TRUE)
    }
  }
})

test_that("bivariate scoreline probabilities reduce to independent Poisson", {
  rates <- list(
    home = matrix(1.4, 1, 1), away = matrix(0.9, 1, 1), shared = matrix(1e-12, 1, 1)
  )
  grid <- nt_bivariate_scoreline_probs(rates, max_goals = 10L)
  expect_equal(grid[1, , ], outer(dpois(0:10, 1.4), dpois(0:10, 0.9)), tolerance = 1e-8)
})

test_that("the shared component adds to both sides' goals", {
  rates <- list(
    home = matrix(c(1.0, 1.2), 2, 1), away = matrix(c(0.7, 0.9), 2, 1),
    shared = matrix(c(0.3, 0.2), 2, 1)
  )
  grid <- nt_bivariate_scoreline_probs(rates, max_goals = 20L)[1, , ]
  g <- 0:20
  expect_equal(sum(grid), 1, tolerance = 1e-8)
  expect_equal(sum(rowSums(grid) * g), mean(c(1.3, 1.4)), tolerance = 1e-6)
  expect_equal(sum(colSums(grid) * g), mean(c(1.0, 1.1)), tolerance = 1e-6)
  want_11 <- mean(c(
    dpois(1, 1.0) * dpois(1, 0.7) * dpois(0, 0.3) + dpois(0, 1.0) * dpois(0, 0.7) * dpois(1, 0.3),
    dpois(1, 1.2) * dpois(1, 0.9) * dpois(0, 0.2) + dpois(0, 1.2) * dpois(0, 0.9) * dpois(1, 0.2)
  ))
  expect_equal(grid[2, 2], want_11)
})

test_that("the Elo baseline orders the outcome by rating and venue", {
  r <- tibble::tibble(
    match_date = as.Date("2015-01-01") + seq_len(300L) * 5L,
    home_team = rep(c("Strong", "Weak", "Strong", "Mid"), 75L),
    away_team = rep(c("Weak", "Mid", "Mid", "Weak"), 75L),
    home_score = rep(c(3L, 0L, 2L, 1L), 75L), away_score = rep(c(0L, 1L, 1L, 1L), 75L),
    tournament = "Friendly", country = "X", neutral = TRUE
  )
  f <- nt_elo_baseline(r, as.Date("2020-01-01"), c("Strong", "Weak", "Mid"), c("Weak", "Strong", "Mid2"),
    venue = c(0, 0, 0)
  ) |>
    expect_error("Mid2")
  f <- nt_elo_baseline(
    r, as.Date("2020-01-01"), c("Strong", "Weak", "Strong"), c("Weak", "Strong", "Weak"),
    venue = c(0, 0, 1)
  )
  expect_gt(f$probs$p_home[1], f$probs$p_away[1])
  expect_lt(f$probs$p_home[2], f$probs$p_away[2])
  expect_gte(f$probs$p_home[3], f$probs$p_home[1])
  expect_gt(f$probs$eg_home[1], f$probs$eg_away[1])
})

test_that("the Elo baseline's neutral-venue forecast ignores which side is listed first", {
  withr::local_seed(2)
  n <- 600L
  teams <- LETTERS[1:8]
  home <- sample(teams, n, replace = TRUE)
  away <- vapply(home, function(h) sample(setdiff(teams, h), 1L), "")
  # Home sides score far more than a 100-point Elo bonus explains, so the
  # links' intercepts carry extra home advantage.
  r <- tibble::tibble(
    match_date = as.Date("2015-01-01") + sort(sample.int(3000L, n)),
    home_team = home, away_team = unname(away),
    home_score = stats::rpois(n, 2), away_score = stats::rpois(n, 0.8),
    tournament = "Friendly", country = home, neutral = FALSE
  )
  f <- nt_elo_baseline(r, as.Date("2024-01-01"), c("A", "B", "A"), c("B", "A", "B"), venue = c(0, 0, 1))
  p <- f$probs
  expect_equal(p$p_home[1], p$p_away[2])
  expect_equal(p$p_draw[1], p$p_draw[2])
  expect_equal(p$eg_home[1], p$eg_away[2])
  expect_equal(f$grid[1, , ], t(f$grid[2, , ]))
  expect_gt(p$p_home[3], p$p_home[1])
})

test_that("the Elo baseline recycles a single venue over all fixtures", {
  withr::local_seed(3)
  n <- 300L
  teams <- LETTERS[1:6]
  home <- sample(teams, n, replace = TRUE)
  away <- vapply(home, function(h) sample(setdiff(teams, h), 1L), "")
  r <- tibble::tibble(
    match_date = as.Date("2015-01-01") + sort(sample.int(3000L, n)),
    home_team = home, away_team = unname(away),
    home_score = stats::rpois(n, 1.8), away_score = stats::rpois(n, 0.9),
    tournament = "Friendly", country = home, neutral = FALSE
  )
  one <- nt_elo_baseline(r, as.Date("2024-01-01"), c("A", "B"), c("B", "A"), venue = 0)
  both <- nt_elo_baseline(r, as.Date("2024-01-01"), c("A", "B"), c("B", "A"), venue = c(0, 0))
  expect_equal(one$probs, both$probs)
})
