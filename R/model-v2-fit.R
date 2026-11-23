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

#' Fewest seasons of results, before the season being forecast, that the INLA
#' engine accepts.
#'
#' With one earlier season INLA's forecasts were measurably worse than NUTS's
#' (exact-scoreline log score down 0.008 to 0.018 with full integration), so
#' a league with only one season of history stays on Stan.
#' @noRd
FOOTBALL_V2_MIN_EARLIER_SEASONS <- 2L

#' Seasons of results before the one being forecast.
#'
#' Counted against the season of the fixtures, not against the calendar, so
#' the answer is the same before a season starts and a month into it.
#' @noRd
fv2_earlier_seasons <- function(train_seasons, forecast_season) {
  sum(unique(train_seasons) < forecast_season)
}

#' A `save_object()` for an adapted fit that keeps only what it needs alive.
#' @noRd
fv2_saver <- function(draws, meta) {
  force(draws)
  force(meta)
  function(file) saveRDS(list(meta = meta, draws = draws()), file)
}

#' Checks an INLA fit must pass before anything is extracted from it.
#'
#' The Stan gate (divergences, R-hat, ESS, tree depth, E-BFMI) has no INLA
#' counterpart. These are the failures an INLA fit can have instead.
#'
#' @param f Result of `fv2_fit_inla()`.
#' @param spec The spec it was fitted on.
#' @param previous_sd Named vector of the previous fit's hyperparameter sds
#'   (`fv2_previous_hyper()`); a move by more than a factor `max_jump` in
#'   either direction fails. Over the 2022 to 2026 walk-forward the largest
#'   move between consecutive re-estimates of any hyperparameter was a factor
#'   1.65. `NULL` skips the check, and `jump_checked` records that it did
#'   not run: a caller must say so, not treat the gate as having passed it.
#' @return `list(passed, breaches, evaluated, skipped, jump_checked)`.
#' @noRd
fv2_inla_gate <- function(f, spec, previous_sd = NULL, max_jump = 3,
                          sd_range = c(1e-5, 5), goals_range = c(1, 8),
                          zero_tol = 1e-4) {
  breaches <- character()
  skipped <- character()
  d <- f$draws

  # WHY: a status that is absent must fail the check, not skip it. sprintf()
  # on a zero-length value returns nothing, which would add no breach.
  status <- f$mode_status
  if (length(status) != 1L || is.na(status) || status != 0L) {
    shown <- if (length(status)) paste(format(status), collapse = ", ") else "missing"
    breaches <- c(breaches, sprintf("hyperparameter mode not found (mode.status = %s)", shown))
  }

  sds <- colMeans(as.matrix(d$hyper))
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
    evaluated = 5L - length(skipped), skipped = length(skipped),
    jump_checked = !is.null(previous_sd)
  )
}

#' The hyperparameter sds of the latest earlier INLA fit of a cell, for the
#' gate's jump check.
#'
#' Read from the `fit_diagnostics` store, which the fit workflow commits, so
#' the reference survives from one run to the next. Only a fit of the same
#' engine and model version that passed its own gate counts, and only one
#' from a strictly earlier date: a rerun is checked against the day before.
#'
#' @param version The model version a previous fit must have
#'   (`inla_model_version()` of the sport).
#' @return Named numeric vector, or `NULL` when there is no such fit.
#' @noRd
fv2_previous_hyper <- function(league, sex, fit_date, root = here::here("data"),
                               version = FOOTBALL_V2_VERSION) {
  rows <- tryCatch(
    as.data.frame(read_table(
      "fit_diagnostics",
      root = root,
      filter = list(sport = league$sport, country = league$country, sex = sex)
    )),
    error = function(e) NULL
  )
  if (is.null(rows) || !nrow(rows) || !all(c("engine", "model_version", "hyper_sd") %in% names(rows))) {
    return(NULL)
  }
  ok <- !is.na(rows$engine) & rows$engine == "inla" &
    !is.na(rows$model_version) & rows$model_version == version &
    !is.na(rows$hyper_sd) & rows$passed %in% TRUE &
    as.Date(rows$fit_date) < as.Date(fit_date)
  if (!any(ok)) {
    return(NULL)
  }
  rows <- rows[ok, , drop = FALSE]
  unlist(jsonlite::fromJSON(rows$hyper_sd[which.max(as.Date(rows$fit_date))]))
}

#' The inputs of the v2.0 specification for one cell, as a production fit
#' builds them.
#'
#' One function, so that what the INLA fit trains on and what the weekly Stan
#' reference fit compares it on cannot drift apart: the training rows are
#' `model_training_results()` at `end_date` in the store's own order; the
#' fixtures are `prep$pred_d` unless `pred` is given; the division table uses
#' every known match of a season, played or scheduled; and teams whose first
#' match of the forecast season is beyond the horizon are carried across the
#' break.
#'
#' @param prep Output of `prepare_data()` for the same league, sex and date.
#' @param pred Fixture rows in `fv2_pred_from_prep()`'s shape to use in place
#'   of `prep$pred_d` (the weekly reference fit's off-season pairings).
#' @return `list(train, pred, carry, divisions, forecast_season, n_earlier)`;
#'   `pred` is `NULL` when there is nothing to predict.
#' @noRd
fv2_production_inputs <- function(prep, league, sex, end_date,
                                  root = here::here("data"), from_season = NULL,
                                  pred = NULL) {
  results <- read_table(
    "results",
    root = root,
    filter = list(sport = league$sport, country = league$country, sex = sex)
  )
  results <- as.data.frame(results)
  results$league_division <- fv2_league_division(results$division)
  results$cell <- fv2_cell(results$division, results$season)
  train <- as.data.frame(model_training_results(results, league, end_date = end_date, from_season = from_season))

  if (is.null(pred) && nrow(prep$pred_d) > 0L) {
    pred <- fv2_pred_from_prep(prep$pred_d, cell_of = fv2_cell)
  }
  fixture_season <- as.integer(format(prep$pred_d$match_date, "%Y"))
  forecast_season <- max(c(train$season, if (!is.null(pred)) as.integer(pred$season)))
  n_earlier <- fv2_earlier_seasons(train$season, forecast_season)

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
      membership(prep$pred_d, fixture_season)
    ),
    fv2_rank_order(league)
  )
  # Before a season, teams whose first match is beyond the horizon are carried
  # across the break with the rest: the simulators play the whole season.
  matches <- c("home_team", "away_team", "match_date", "season")
  carry <- fv2_carry_slots(
    train, pred,
    rbind(results[matches], if (!is.null(schedules) && nrow(schedules)) schedules[matches])
  )
  list(
    train = train, pred = pred, carry = carry, divisions = divisions,
    forecast_season = forecast_season, n_earlier = n_earlier
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
#' @param threads R-INLA's `num.threads`. One thread is what makes a refit of
#'   the same data return the same numbers (`fv2_fit_inla()`); `fit_league()`
#'   does not pass it.
#' @param previous_sd The previous fit's hyperparameter sds, for the gate's
#'   jump check (`fv2_previous_hyper()`); `NULL` when there is none.
#' @param accept_jump Let a fit through whose only breach is the jump check.
#'   For an operator who has looked at the new values and agrees with them;
#'   without it a real, lasting change would be refused every day.
#' @return The adapted fit from `fv2_as_fit()`, with `$engine`, `$version`,
#'   `$gate`, `$hyper` and `$n_obs` attached.
#' @noRd
fit_model_v2_inla <- function(prep, league, sex, end_date,
                              root = here::here("data"), from_season = NULL,
                              n_draws = 4000L, seed = 1L,
                              int_strategy = "auto", threads = "1:1",
                              previous_sd = NULL, accept_jump = FALSE) {
  if (!identical(league$sport, "football")) {
    cli::cli_abort("The INLA engine fits the football model only; {.val {league$sport}} stays on Stan.")
  }
  inputs <- fv2_production_inputs(prep, league, sex, end_date = end_date, root = root, from_season = from_season)
  forecast_season <- inputs$forecast_season
  n_earlier <- inputs$n_earlier
  if (n_earlier < FOOTBALL_V2_MIN_EARLIER_SEASONS) {
    cli::cli_abort(c(
      "The INLA engine needs at least {FOOTBALL_V2_MIN_EARLIER_SEASONS} seasons of results before season {forecast_season}; this cell has {n_earlier}.",
      i = "With so little history INLA's forecasts were measurably worse than NUTS's. Use {.code engine: stan}."
    ))
  }

  # WHY: INLA lists sn under Suggests, but its posterior sampler stops without
  # it. Found on a GitHub runner, where sn was not already there by accident.
  absent <- c("INLA", "sn")[!vapply(c("INLA", "sn"), requireNamespace, logical(1), quietly = TRUE)]
  if (length(absent)) {
    cli::cli_abort(c(
      "League asks for {.code engine: inla} but {.pkg {absent}} {?is/are} not installed.",
      i = "INLA installs from {.url https://inla.r-inla-download.org/R/stable}, sn from CRAN; or set {.code engine: stan}."
    ))
  }

  carry <- inputs$carry
  spec <- do.call(fv2_build, c(
    list(inputs$train, inputs$pred, carry = carry, divisions = inputs$divisions),
    football_v2_options()
  ))

  f <- fv2_fit_inla(spec, int_strategy = int_strategy, n_samp = n_draws, threads = threads, seed = seed)
  gate <- fv2_inla_gate(f, spec, previous_sd = previous_sd)
  jumped <- grepl("since the previous fit", gate$breaches, fixed = TRUE)
  if (isTRUE(accept_jump) && any(jumped)) {
    cli::cli_alert_warning(c(
      "INLA gate: hyperparameter jump accepted on request for {league$sport}/{league$country}/{sex}: ",
      "{gate$breaches[jumped]}"
    ))
    gate$breaches <- gate$breaches[!jumped]
    gate$passed <- length(gate$breaches) == 0L
    gate$jump_accepted <- TRUE
  }
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
    forecast_season = forecast_season, carried = as.character(carry$team),
    hyper_sd = colMeans(as.matrix(f$draws$hyper)),
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
  # the record of how they were produced. The saver is built outside this
  # function so it does not keep the INLA result and the results table alive.
  fit$save_object <- fv2_saver(fit$draws, meta)
  fit
}

#' Record an INLA fit in the `fit_diagnostics` store.
#'
#' `needs_refit()` reads `n_obs` from the latest row of a cell, so an INLA
#' fit must write one or an old Stan row stays "latest". The sampler metrics
#' have no INLA counterpart and are `NA`, which the health check reads as
#' nothing to report. The row also carries the engine, the model version and
#' the fit's hyperparameter sds (as JSON): the next fit's gate reads them
#' back with `fv2_previous_hyper()`. Stan rows do not have these columns and
#' read as `NA`.
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
    # Per-chain draws have no meaning here; the total is in `total_iter`.
    iter_sampling = NA_integer_,
    chains = NA_integer_,
    gates_evaluated = as.integer(gate$evaluated %||% 0L),
    gates_skipped = as.integer(gate$skipped %||% 0L),
    passed = isTRUE(gate$passed) && isTRUE(gate$evaluated > 0L),
    engine = "inla",
    model_version = as.character(fit$version %||% NA_character_),
    hyper_sd = if (length(fit$hyper_sd)) {
      as.character(jsonlite::toJSON(as.list(fit$hyper_sd), auto_unbox = TRUE, digits = NA))
    } else {
      NA_character_
    },
    jump_checked = isTRUE(gate$jump_checked)
  )
  # Basketball v2 trains on regulation time: the rows that had it, which
  # needs_refit() recounts. Written only for such a model, so a fit on final
  # scores keeps the row it always had.
  n_reg <- as.integer(fit$n_obs_regulation %||% NA_integer_)
  if (!is.na(n_reg)) row$n_obs_regulation <- n_reg
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
