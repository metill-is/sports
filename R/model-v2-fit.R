# Football model v2.0 on the production fit path. Reached only when a
# league's config says `engine: inla`; the default engine is Stan and nothing
# here runs for it.

#' Version of the football model the INLA engine fits.
#'
#' v2.0 is provisional: it was chosen on the 2022 to 2026 walk-forward it was
#' scored on, and is due a re-examination on 2027 matches it has never seen.
#' @noRd
FOOTBALL_V2_VERSION <- "2.0"

#' The v2.0 specification, as `fv2_build()` options: one weekly strength walk
#' per team with reversion 0.8 toward the division mean at the season break,
#' a fixed style value per team, and league-wide scoring-level and
#' home-advantage walks.
#' @noRd
football_v2_options <- function() {
  list(clock = "break", rho = 0.8, param = "sq", style = "static", level_walk = TRUE)
}

#' Fewest seasons of results the INLA engine accepts.
#'
#' With one earlier season INLA's forecasts were measurably worse than NUTS's
#' (exact-scoreline log score down 0.008 to 0.018 with full integration), so
#' a league that short stays on Stan.
#' @noRd
FOOTBALL_V2_MIN_SEASONS <- 3L

#' Checks an INLA fit must pass before anything is extracted from it.
#'
#' The Stan gate (divergences, R-hat, ESS, tree depth, E-BFMI) has no INLA
#' counterpart. These are the failures an INLA fit can have instead.
#'
#' @param f Result of `fv2_fit_inla()`.
#' @param spec The spec it was fitted on.
#' @param previous_sd Optional named vector of the previous fit's
#'   hyperparameter sds; when given, a jump by more than `max_jump` fails.
#' @return `list(passed, breaches, evaluated, skipped)`.
#' @noRd
fv2_inla_gate <- function(f, spec, previous_sd = NULL, max_jump = 3,
                          sd_range = c(1e-5, 5), goals_range = c(1, 8),
                          zero_tol = 1e-4) {
  breaches <- character()
  skipped <- character()
  d <- f$draws

  if (!identical(as.integer(f$mode_status), 0L)) {
    breaches <- c(breaches, sprintf("hyperparameter mode not found (mode.status = %s)", format(f$mode_status)))
  }

  sds <- colMeans(d$hyper)
  bad <- !is.finite(sds) | sds < sd_range[[1]] | sds > sd_range[[2]]
  if (any(bad)) {
    breaches <- c(breaches, sprintf(
      "hyperparameter sd outside [%g, %g]: %s", sd_range[[1]], sd_range[[2]],
      paste(sprintf("%s = %.3g", names(sds)[bad], sds[bad]), collapse = ", ")
    ))
  }

  finite <- all(is.finite(d$att)) && all(is.finite(d$def)) && all(is.finite(d$c_home)) &&
    all(is.finite(d$c_away)) && all(is.finite(d$ha_att)) && all(is.finite(d$ha_def))
  if (!finite) {
    breaches <- c(breaches, "non-finite posterior draws")
  } else {
    # WHY: the entry levels are the model's identification. Attack and
    # defence each sum to zero over teams' entry slots in both
    # parameterisations (strength and style each do, so their sum and
    # difference do); a fit where they do not has lost the constraint and its
    # intercepts mean something else.
    drift <- max(
      abs(rowSums(d$att[, spec$first_slot, drop = FALSE])),
      abs(rowSums(d$def[, spec$first_slot, drop = FALSE]))
    )
    if (drift > zero_tol) {
      breaches <- c(breaches, sprintf("entry levels do not sum to zero (largest sum %.3g)", drift))
    }
  }

  if (length(spec$pred$home_slot) > 0L && finite) {
    rates <- fv2_predict(d, spec)
    goals <- mean(rates$lam_home + rates$lam_away)
    if (!is.finite(goals) || goals < goals_range[[1]] || goals > goals_range[[2]]) {
      breaches <- c(breaches, sprintf(
        "mean predicted goals per match %.2f outside [%g, %g]", goals, goals_range[[1]], goals_range[[2]]
      ))
    }
  } else {
    skipped <- c(skipped, "predicted goals")
  }

  if (is.null(previous_sd)) {
    skipped <- c(skipped, "hyperparameter jump")
  } else {
    common <- intersect(names(previous_sd), names(sds))
    ratio <- sds[common] / previous_sd[common]
    jumped <- !is.finite(ratio) | ratio > max_jump | ratio < 1 / max_jump
    if (any(jumped)) {
      breaches <- c(breaches, sprintf(
        "hyperparameter moved by more than a factor %g since the previous fit: %s",
        max_jump, paste(common[jumped], collapse = ", ")
      ))
    }
  }

  list(
    passed = length(breaches) == 0L, breaches = breaches,
    evaluated = 5L - length(skipped), skipped = length(skipped)
  )
}

#' Fit football v2.0 with R-INLA and return a fit the extract and publish
#' layers can read.
#'
#' The training rows are `model_training_results()` at `end_date`, the one
#' definition of what a fit sees, in the store's own order (the order
#' `prepare_data()` trains in). The fixtures are `prep$pred_d`.
#'
#' @param prep Output of `prepare_data()` for the same league, sex and date.
#' @param n_draws Posterior draws to return.
#' @param int_strategy "auto" integrates over the hyperparameters; "eb" fixes
#'   them at their mode.
#' @return The adapted fit from `fv2_as_fit()`, with `$engine`, `$version`,
#'   `$gate`, `$hyper` and `$n_obs` attached.
#' @noRd
fit_model_v2_inla <- function(prep, league, sex, end_date,
                              root = here::here("data"), from_season = NULL,
                              n_draws = 4000L, seed = 1L,
                              int_strategy = "auto", threads = "4:1") {
  if (!identical(league$sport, "football")) {
    cli::cli_abort("The INLA engine fits the football model only; {.val {league$sport}} stays on Stan.")
  }
  results <- read_table(
    "results",
    root = root,
    filter = list(sport = league$sport, country = league$country, sex = sex)
  )
  results <- as.data.frame(results)
  results$league_division <- fv2_league_division(results$division)
  results$cell <- fv2_cell(results$division, results$season)
  train <- as.data.frame(model_training_results(results, league, end_date = end_date, from_season = from_season))

  n_seasons <- length(unique(train$season))
  if (n_seasons < FOOTBALL_V2_MIN_SEASONS) {
    cli::cli_abort(c(
      "The INLA engine needs at least {FOOTBALL_V2_MIN_SEASONS} seasons of results; this cell has {n_seasons}.",
      i = "With so little history INLA's forecasts were measurably worse than NUTS's. Use {.code engine: stan}."
    ))
  }

  if (!requireNamespace("INLA", quietly = TRUE)) {
    cli::cli_abort(c(
      "League asks for {.code engine: inla} but the {.pkg INLA} package is not installed.",
      i = "Install it from {.url https://inla.r-inla-download.org/R/stable} or set {.code engine: stan}."
    ))
  }

  # WHY: which division a team plays in is schedule knowledge, public before
  # a season's first match, and the season-break step needs it for every team
  # with a fixture ahead: a team with no division row for the new season is
  # treated as having stayed, and reverts toward the wrong division's mean.
  # So the table uses every known match of a season, played or scheduled, on
  # either side of `end_date`. No score enters it.
  schedules <- tryCatch(
    as.data.frame(read_table(
      "schedules",
      root = root,
      filter = list(sport = league$sport, country = league$country, sex = sex)
    )),
    error = function(e) NULL
  )
  membership <- function(d, season) {
    data.frame(
      home_team = d$home_team, away_team = d$away_team,
      season = as.integer(season), league_division = fv2_league_division(d$division)
    )
  }
  divisions <- fv2_division_table(
    rbind(
      membership(results, results$season),
      if (!is.null(schedules) && nrow(schedules)) membership(schedules, schedules$season),
      membership(prep$pred_d, format(prep$pred_d$match_date, "%Y"))
    ),
    fv2_rank_order(league)
  )
  spec <- do.call(fv2_build, c(
    list(train, fv2_pred_from_prep(prep$pred_d, cell_of = fv2_cell), divisions = divisions),
    football_v2_options()
  ))

  f <- fv2_fit_inla(spec, int_strategy = int_strategy, n_samp = n_draws, threads = threads, seed = seed)
  gate <- fv2_inla_gate(f, spec)
  if (!gate$passed) {
    cli::cli_abort(c(
      "INLA fit for {league$sport}/{league$country}/{sex} failed its diagnostics gate.",
      stats::setNames(gate$breaches, rep("x", length(gate$breaches)))
    ))
  }

  fit <- fv2_as_fit(f$draws, spec, prep, seed = seed)
  meta <- list(
    engine = "inla", version = FOOTBALL_V2_VERSION, options = football_v2_options(),
    int_strategy = int_strategy, gate = gate, hyper = f$hyper_summary,
    n_obs = spec$N, n_teams = spec$K, n_states = spec$S,
    seconds = c(fit = f$t_fit, draws = f$t_samp),
    inla_version = as.character(utils::packageVersion("INLA"))
  )
  cli::cli_alert_info(paste0(
    "INLA engine, football v{FOOTBALL_V2_VERSION}: {spec$N} matches, {spec$K} teams, ",
    "{spec$S} states; fit {round(f$t_fit)} s, draws {round(f$t_samp)} s; ",
    "gate passed ({gate$evaluated} checks, {gate$skipped} skipped)"
  ))
  fit[names(meta)] <- meta
  # WHY: fit_league() saves every fit with fit$save_object(). What is worth
  # keeping from this engine is the draws under their production names and
  # the record of how they were produced.
  fit$save_object <- function(file) saveRDS(list(meta = meta, draws = fit$draws()), file)
  fit
}

#' Record an INLA fit in the `fit_diagnostics` store.
#'
#' `needs_refit()` reads `n_obs` from the latest row of a cell, so an INLA
#' fit must write one or an old Stan row stays "latest". The sampler metrics
#' have no INLA counterpart and are `NA`, which the health check reads as
#' nothing to report.
#' @noRd
persist_fit_diagnostics_inla <- function(fit, league, sex, fit_date,
                                         n_obs = NA_integer_, n_draws = NA_integer_,
                                         root = here::here("data")) {
  gate <- fit$gate
  row <- tibble::tibble(
    sport = league$sport,
    country = league$country,
    sex = sex,
    fit_date = as.Date(fit_date),
    n_obs = as.integer(n_obs %||% NA_integer_),
    n_divergent = NA_integer_,
    total_iter = as.integer(n_draws),
    div_frac = NA_real_,
    n_max_treedepth = NA_integer_,
    treedepth_frac = NA_real_,
    min_ebfmi = NA_real_,
    max_rhat = NA_real_,
    min_ess_bulk = NA_real_,
    min_ess_tail = NA_real_,
    adapt_delta = NA_real_,
    iter_sampling = as.integer(n_draws),
    chains = NA_integer_,
    gates_evaluated = as.integer(gate$evaluated %||% 0L),
    gates_skipped = as.integer(gate$skipped %||% 0L),
    passed = isTRUE(gate$passed) && isTRUE(gate$evaluated > 0L)
  )
  tryCatch(
    write_table(row, "fit_diagnostics", root = root),
    error = function(e) {
      cli::cli_alert_warning(
        "Failed to persist fit diagnostics: {conditionMessage(e)}"
      )
    }
  )
  invisible(row)
}
