# The weekly Stan reference fit for handball and basketball v2 compares
# R-INLA with NUTS on the Stan programs of the same model
# (R/model-v2-hbbb-reference.R). These tests read the priors, constraints
# and fixed settings R-INLA is actually handed, at the call to INLA::inla()
# that a production fit makes, and hold the Stan programs' data to them: a
# prior changed in fit_v2_inla() and not in the Stan data fails here.

# The arguments of the INLA::inla() call fit_v2_inla() makes for one half,
# stopped there.
.inla_call <- function(fit) {
  seen <- NULL
  local_mocked_bindings(
    inla = function(formula, ..., control.family = NULL, control.fixed = NULL) {
      seen <<- list(formula = formula, control.family = control.family, control.fixed = control.fixed)
      stop("stopped at the INLA call")
    },
    .package = "INLA"
  )
  expect_error(fit(), "stopped at the INLA call")
  seen
}

test_that("the Stan data carries the priors and fixed settings R-INLA is handed", {
  skip_if_no_inla()
  rate <- function(param) {
    expect_identical(param[[2]], 0.01)
    -log(0.01) / param[[1]]
  }
  for (sport in c("handball", "basketball")) {
    spec <- hbbb_fixture_spec(sport, root = fixture_hbbb_root())$spec
    priors <- v2_priors(sport)
    for (resp in c("margin", "total")) {
      # As hbbb_v2_fit_halves() calls it.
      call <- .inla_call(function() {
        fit_v2_inla(
          spec$v, resp, priors,
          within = spec$opts$within, rho_week = spec$opts$rho_week, int_strategy = "auto",
          n_samples = 40L, y = spec$y[[resp]], latent = spec$latent[[resp]]
        )
      })
      env <- environment(call$formula)
      terms <- paste(labels(stats::terms(call$formula)), collapse = " + ")
      sd <- hbbb_v2_stan_data(spec, resp, priors)
      info <- paste(sport, resp)

      expect_equal(sd$rate_sigma, rate(call$control.family$hyper$prec$param), info = info)
      expect_equal(sd$rate_b, rate(env$hyper_b$prec$param), info = info)
      expect_equal(sd$rate_u, rate(env$hyper_u$prec$param), info = info)
      # The season AR(1)'s correlation: normal on log((1 + rho) / (1 - rho)),
      # R-INLA's precision 1 / sd^2.
      expect_equal(c(sd$rho_mu, 1 / sd$rho_sd^2), env$hyper_u$rho$param, info = info)
      within <- spec$opts$within
      expect_identical(sd$within_mode, c(none = 0L, rw1 = 1L, ar1 = 2L)[[within]], info = info)
      if (identical(within, "ar1")) {
        expect_equal(sd$rate_w, rate(env$hyper_w_ar1$prec$param), info = info)
        expect_true(isTRUE(env$hyper_w_ar1$rho$fixed), info = info)
        expect_equal(log((1 + sd$rho_week) / (1 - sd$rho_week)), env$hyper_w_ar1$rho$initial, info = info)
        expect_match(terms, 'f(w_t, model = "ar1"', fixed = TRUE)
      } else if (identical(within, "rw1")) {
        expect_equal(sd$rate_w, rate(env$hyper_w$prec$param), info = info)
        expect_match(terms, 'f(w_t, model = "rw1", values = seq_len(v$W), replicate = w_r, constr = TRUE', fixed = TRUE)
      }
      # Club level iid and season level AR(1), neither constrained.
      expect_match(terms, 'f(b, model = "iid", values = seq_len(v$K), hyper = hyper_b)', fixed = TRUE)
      expect_match(terms, 'f(u_s, model = "ar1", values = seq_len(v$NS), replicate = u_k, hyper = hyper_u)', fixed = TRUE)
      if (resp == "margin") {
        expect_equal(sd$rate_ha, rate(env$hyper_ha$prec$param), info = info)
        expect_equal(c(0, 1 / sd$hfa_sd^2), c(call$control.fixed$mean, call$control.fixed$prec), info = info)
      } else {
        expect_equal(sd$rate_season, rate(env$hyper_season$prec$param), info = info)
        expect_equal(c(sd$level_mean, 1 / sd$level_sd^2), c(call$control.fixed$mean, call$control.fixed$prec), info = info)
        # Each division's season level sums to zero, as the Stan program's
        # path - mean(path).
        expect_match(terms, 'f(s_s, model = "rw1", values = seq_len(v$NS), replicate = s_d, constr = TRUE', fixed = TRUE)
      }
    }
  }
})

test_that("production hands R-INLA the margin half's priors as the test above reads them", {
  skip_if_no_inla()
  spec <- hbbb_fixture_spec("basketball", root = fixture_hbbb_root())$spec
  direct <- .inla_call(function() {
    fit_v2_inla(
      spec$v, "margin", v2_priors("basketball"),
      within = spec$opts$within, rho_week = spec$opts$rho_week, int_strategy = "auto",
      n_samples = 40L, y = spec$y$margin, latent = spec$latent$margin
    )
  })
  production <- .inla_call(function() hbbb_v2_fit_halves(spec, "basketball", n_draws = 40L))
  hyper <- function(call) {
    env <- environment(call$formula)
    list(env$hyper_b, env$hyper_u, env$hyper_w_ar1, env$hyper_ha, call$control.family, call$control.fixed)
  }
  expect_equal(hyper(production), hyper(direct))
  expect_identical(deparse(production$formula), deparse(direct$formula))
})
