# The INLA engine against a stored NUTS reference: the "reference test that
# needs no Stan run" designed in docs/reports/2026-11-15-football-v2-proposal.qmd
# (decision 5 of the football cutover). The fixture holds a frozen training set
# (the women's cell to 2026-07-31, as the production fit builds it), what NUTS
# found on it, and how that was made. It is written by
# research/football-v2/make_reference.R on a machine with CmdStan.
#
# The test rebuilds the specification from the stored inputs, never from the
# live store, fits it with R-INLA as production does, and compares. It catches
# an R-INLA version that changes the approximation, a change to the
# specification builder that silently changes the model, and a broken mapping
# of draws to quantities. It cannot catch the approximation getting worse on
# new data (the weekly reference fit's job) or a wrong model (both engines
# would reproduce it).

reference_file <- testthat::test_path("fixtures", "football-v2-reference.rds")
reference_program <- function(ref) {
  testthat::test_path("..", "..", "research", "football-v2", "stan", ref$provenance$stan_program)
}

test_that("the stored NUTS reference is whole and came from a healthy fit", {
  ref <- readRDS(reference_file)
  expect_named(ref, c("inputs", "opts", "priors", "summary", "provenance"))
  expect_named(ref$inputs, c("train", "pred", "carry", "divisions"))
  n_fixtures <- nrow(ref$inputs$pred)
  expect_gt(n_fixtures, 0L)
  expect_identical(dim(ref$summary$probs), c(n_fixtures, 3L))
  expect_equal(unname(rowSums(ref$summary$probs)), rep(1, n_fixtures), tolerance = 1e-9)
  # Keyed by name, so a comparison survives a different team order.
  expect_false(anyDuplicated(rownames(ref$summary$probs)) > 0 || is.null(rownames(ref$summary$probs)))
  expect_false(anyDuplicated(ref$summary$att$team) > 0)
  expect_identical(nrow(ref$summary$att), nrow(ref$summary$def))
  # A reference worth comparing against: no divergences, chains that agree.
  expect_identical(ref$provenance$n_divergent, 0L)
  expect_lt(ref$provenance$max_rhat, 1.01)
  expect_identical(ref$provenance$n_draws, 4000L)
})

test_that("a reference made under other options, priors or Stan program is reported as stale", {
  ref <- readRDS(reference_file)
  program <- reference_program(ref)
  skip_if_not(file.exists(program), "Stan reference program not present")
  expect_identical(reference_staleness(ref, program), NULL)
  moved <- ref
  moved$opts$rho <- 0.7
  expect_identical(reference_staleness(moved, program), "football_v2_options()")
  moved <- ref
  moved$priors$sigma <- c(0.3, 0.01)
  expect_identical(reference_staleness(moved, program), "fv2_priors()")
  moved <- ref
  moved$provenance$stan_md5 <- "0"
  expect_identical(reference_staleness(moved, program), "the Stan reference program")
})

# The comparisons, at limits about twice the differences seen when the engine
# was validated.
compare_with_reference <- function(ref) {
  spec <- do.call(fv2_build, c(ref$inputs, football_v2_options()))
  # As production fits: full integration, one thread (fv2_fit_inla()'s
  # defaults); as many draws as the reference has.
  f <- suppressWarnings(fv2_fit_inla(spec, int_strategy = "auto", n_samp = ref$provenance$n_draws, seed = 1L))
  expect_equal(f$mode_status, 0)
  got <- reference_summary(f$draws, spec)
  want <- ref$summary

  # The limits are the proposal's (about twice what its validation saw), so
  # the test catches a break and not Monte Carlo noise. On this fixture, when
  # it was made (R-INLA 25.6.7, macOS): match probabilities 0.0023 apart on
  # average and 0.0078 at most; walk and style sds within 1.2 %; near-zero
  # sds within a factor 1.36; states 0.026 posterior sds apart on average.
  expect_setequal(rownames(got$probs), rownames(want$probs))
  d_prob <- abs(got$probs[rownames(want$probs), , drop = FALSE] - want$probs)
  expect_lt(mean(d_prob), 0.005, label = sprintf("mean |p_INLA - p_NUTS| = %.4f", mean(d_prob)))
  expect_lt(max(d_prob), 0.025, label = sprintf("largest |p_INLA - p_NUTS| = %.4f", max(d_prob)))

  ratio <- stats::setNames(got$hyper$mean / want$hyper$mean[match(got$hyper$name, want$hyper$name)], got$hyper$name)
  expect_false(anyNA(ratio))
  # The grouping is the proposal's: the team home-advantage deviations and the
  # home-advantage level walk sit near zero, where INLA loses the lower tail.
  # Means of the draws, the statistic production stores as hyper_sd.
  near_zero <- c("omega_att", "omega_def", "sigma_lvl_ha")
  walk <- setdiff(names(ratio), near_zero)
  expect_true(all(near_zero %in% names(ratio)))
  # Entry-level, innovation, season-break, style and scoring-level sds: within 10 %.
  expect_lt(max(abs(ratio[walk] - 1)), 0.10, label = paste(sprintf("%s %.3f", walk, ratio[walk]), collapse = ", "))
  # Sds near zero: within a factor 1.5.
  expect_lt(max(abs(log(ratio[near_zero]))), log(1.5), label = paste(sprintf("%s %.3f", near_zero, ratio[near_zero]), collapse = ", "))

  # Current states and the league home advantage: how far apart the posterior
  # means are, in NUTS posterior sds, on average.
  expect_setequal(got$att$team, want$att$team)
  att <- got$att[match(want$att$team, got$att$team), ]
  def <- got$def[match(want$def$team, got$def$team), ]
  z <- c(
    abs(att$mean - want$att$mean) / want$att$sd,
    abs(def$mean - want$def$mean) / want$def$sd,
    abs(got$ha[["mean"]] - want$ha[["mean"]]) / want$ha[["sd"]]
  )
  expect_lt(mean(z), 0.15, label = sprintf("mean state difference %.3f posterior sds", mean(z)))
}

test_that("the INLA engine agrees with the stored NUTS reference on one frozen cell", {
  skip_if_no_inla()
  ref <- readRDS(reference_file)
  stale <- reference_staleness(ref, reference_program(ref))
  # A changed specification makes the numbers incomparable, so the test stops
  # at the reason instead of reporting differences that mean nothing.
  if (length(stale)) {
    fail(paste0(
      "stale reference: ", paste(stale, collapse = ", "), " changed since it was made. ",
      "Regenerate it with research/football-v2/make_reference.R."
    ))
  } else {
    compare_with_reference(ref)
  }
})

