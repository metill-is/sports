#' @include model-v2-fit.R
NULL

# The weekly Stan reference fit (decision 5 of the football cutover plan,
# docs/reports/2026-11-15-football-v2-proposal.qmd, "Shadow period" step 4).
#
# Once R-INLA fits football every day, nothing else would check that its
# approximation stays close to the exact posterior on data it has never seen;
# the stored-reference test (test-model-v2-reference.R) watches one frozen
# cell only. So once a week, for each football cell, the v2.0 specification is
# built from the latest data exactly as a production fit builds it
# (fv2_production_inputs()), fitted by R-INLA as production fits it and by
# NUTS with the Stan reference program, and the two engines' outcome
# probabilities for the same fixtures are compared. A mean absolute difference
# above FV2_REFERENCE_MAX_MEAN_ABS_DIFF, an unconverged NUTS fit or an INLA fit
# without a mode does not pass, and the health check
# (check_engine_reference()) warns.
#
# The fitters are arguments: scripts/engine_reference.R passes R-INLA exactly
# as fit_league() would call it that day (production seed, full integration,
# one thread, the INLA gate), which on one thread reproduces the production
# fit of the same data when both runners have the same CPU vendor; across
# Intel and AMD runners R-INLA's MKL build does not (model-decide.md,
# "Repeatability"), and the research
# Stan wrapper (research/football-v2/engines.R); the tests pass fakes. With no
# fixture inside the horizon (the off-season) the comparison is made on every
# pairing of the season's league divisions on the first day of the next
# season, or a week ahead if later: the season-break path a pre-season fit
# takes, so the check runs all year.

#' The largest mean absolute difference in outcome probabilities between INLA
#' and NUTS that passes (decision 5: a breach raises a health warning).
#' @noRd
FV2_REFERENCE_MAX_MEAN_ABS_DIFF <- 0.01

#' What a football row's `nuts_max_rhat` covers: the hyperparameters and
#' every state (the fit's summary and `state_max_rhat`), where a handball /
#' basketball row's covers the forecasts (`HBBB_REFERENCE_RHAT_SCOPE`).
#' @noRd
FV2_REFERENCE_RHAT_SCOPE <- "hyperparameters and states"

#' Every pairing, both ways, of the teams of each league division in a season.
#'
#' @param divisions `fv2_division_table()` output (team, season, division,
#'   rank).
#' @param season Season whose division membership is used.
#' @param match_date Date of every fixture.
#' @param teams Optional: keep only these teams (a fixture's teams must have
#'   played, or fv2_build() refuses it).
#' @return Fixture rows in `fv2_pred_from_prep()`'s shape.
#' @noRd
fv2_offseason_fixtures <- function(divisions, season, match_date, teams = NULL) {
  d <- divisions[divisions$season == season & !is.na(divisions$division), , drop = FALSE]
  if (!is.null(teams)) {
    d <- d[d$team %in% teams, , drop = FALSE]
  }
  pairs <- do.call(rbind, lapply(split(d$team, d$division), function(tm) {
    tm <- sort(unique(tm))
    if (length(tm) < 2L) {
      return(NULL)
    }
    g <- expand.grid(home_team = tm, away_team = tm, stringsAsFactors = FALSE)
    g <- g[g$home_team != g$away_team, , drop = FALSE]
    g$division <- d$division[match(g$home_team, d$team)]
    g
  }))
  if (is.null(pairs) || nrow(pairs) == 0L) {
    pairs <- data.frame(home_team = character(), away_team = character(), division = character())
  }
  pairs$match_date <- rep(as.Date(match_date), nrow(pairs))
  rownames(pairs) <- NULL
  fv2_pred_from_prep(pairs, cell_of = fv2_cell)
}

#' Home, draw and away probabilities of each fixture from posterior draws.
#'
#' Independent Poisson scores per draw, averaged over draws, on a grid of
#' `0:max_goals` goals each, renormalised.
#'
#' @param draws Draws in the engines' shared layout.
#' @param spec The specification they were fitted on.
#' @param fixtures The fixture rows `spec` was built with; names the rows
#'   "home | away | date". Without it the rows are named by spec week.
#' @return Matrix, one row per fixture, columns home, draw, away.
#' @noRd
fv2_outcome_probs <- function(draws, spec, fixtures = NULL, max_goals = 12L) {
  pr <- fv2_predict(draws, spec)
  g <- 0:max_goals
  probs <- t(vapply(seq_len(nrow(pr$lam_home)), function(i) {
    ph <- outer(g, pr$lam_home[i, ], stats::dpois)
    pa <- outer(g, pr$lam_away[i, ], stats::dpois)
    grid <- tcrossprod(ph, pa) / ncol(ph)
    grid <- grid / sum(grid)
    c(home = sum(grid[lower.tri(grid)]), draw = sum(diag(grid)), away = sum(grid[upper.tri(grid)]))
  }, numeric(3)))
  if (length(probs) == 0L) {
    probs <- matrix(numeric(), 0L, 3L, dimnames = list(NULL, c("home", "draw", "away")))
  }
  rownames(probs) <- if (is.null(fixtures)) {
    paste(spec$teams[spec$pred$home_team], spec$teams[spec$pred$away_team], spec$pred$week_nr, sep = " | ")
  } else {
    paste(fixtures$home_team, fixtures$away_team, format(as.Date(fixtures$match_date), "%Y-%m-%d"), sep = " | ")
  }
  probs
}

#' Fit one football cell with R-INLA and with NUTS and compare them.
#'
#' @param league League config (`football_iceland`).
#' @param sex `"male"` or `"female"`.
#' @param end_date The data date: training is everything played before it,
#'   as a production fit on that day sees it.
#' @param root Storage root.
#' @param fit_inla,fit_nuts Functions of a spec, each returning a fit with
#'   `$draws` in the shared layout; `fit_inla`'s also carries `$mode_status`
#'   and `$gate_passed` (`fv2_inla_gate()`), `fit_nuts`'s `$diagnostics`
#'   (`n_divergent`, `state_max_rhat`) and `$summary` (`rhat`).
#' @param provenance Named list: `inla_version`, `cmdstan_version`,
#'   `stan_md5`, recorded with the result.
#' @return `list(row, fixtures, spec)`: `row` in the `engine_reference`
#'   schema, `fixtures` one row per fixture with both engines' probabilities.
#' @noRd
fv2_weekly_reference <- function(league, sex, end_date = Sys.Date(),
                                 root = here::here("data"),
                                 fit_inla, fit_nuts, from_season = NULL,
                                 provenance = list()) {
  end_date <- as.Date(end_date)
  prep <- prepare_data(league, sex, end_date = end_date, root = root, from_season = from_season)
  inputs <- fv2_production_inputs(prep, league, sex, end_date = end_date, root = root, from_season = from_season)
  kind <- "upcoming"
  if (is.null(inputs$pred) || nrow(inputs$pred) == 0L) {
    kind <- "off-season pairings"
    played <- unique(c(inputs$train$home_team, inputs$train$away_team))
    season <- max(inputs$train$season)
    # The next season's first day, or a week ahead if later: the break step a
    # pre-season fit takes, not a mid-season week production never forecasts.
    match_date <- max(end_date + 7L, as.Date(sprintf("%d-01-01", season + 1L)))
    pairs <- fv2_offseason_fixtures(
      inputs$divisions,
      season = season, match_date = match_date, teams = played
    )
    inputs <- fv2_production_inputs(
      prep, league, sex,
      end_date = end_date, root = root, from_season = from_season, pred = pairs
    )
  }
  spec <- do.call(fv2_build, c(
    list(inputs$train, inputs$pred, carry = inputs$carry, divisions = inputs$divisions),
    football_v2_options()
  ))
  cli::cli_alert_info(
    "Weekly reference {league$sport}/{league$country}/{sex} at {end_date}: {spec$N} matches, {spec$K} teams, {nrow(inputs$pred)} fixtures ({kind})"
  )

  inla <- fit_inla(spec)
  nuts <- fit_nuts(spec)
  p_inla <- fv2_outcome_probs(inla$draws, spec, inputs$pred)
  p_nuts <- fv2_outcome_probs(nuts$draws, spec, inputs$pred)
  diff <- abs(p_inla - p_nuts)

  # Is the NUTS reference sound enough to compare against? The health layer's
  # own bar for a production Stan fit (health_thresholds(): divergences under
  # 0.5 % of transitions, Rhat at most 1.02), not the stored reference's
  # zero-divergence, 1.01 bar: on 2026-11-22 the men's reference had 1
  # divergence in 4,000 and Rhat 1.019 while the engines agreed to 0.003, and
  # a weekly check that warns on that would warn most weeks. With no Rhat at
  # all there is no evidence the chains agree, which does not pass.
  th <- health_thresholds()
  rhats <- c(nuts$summary$rhat, nuts$diagnostics$state_max_rhat)
  nuts_rhat <- if (any(!is.na(rhats))) max(rhats, na.rm = TRUE) else NA_real_
  divergences <- as.integer(nuts$diagnostics$n_divergent)
  div_frac <- divergences / max(1L, nrow(nuts$draws$att))
  mode_status <- as.integer(inla$mode_status)
  gate_passed <- isTRUE(inla$gate_passed)
  mad <- mean(diff)
  passed <- isTRUE(mad <= FV2_REFERENCE_MAX_MEAN_ABS_DIFF) && isTRUE(div_frac <= th$div_frac_warn) &&
    isTRUE(nuts_rhat <= th$rhat_warn) && identical(mode_status, 0L) && gate_passed
  prov <- function(name) as.character(provenance[[name]] %||% NA_character_)

  row <- tibble::tibble(
    sport = league$sport, country = league$country, sex = sex,
    run_date = end_date,
    run_at = as.POSIXct(floor(as.numeric(Sys.time())), origin = "1970-01-01", tz = "UTC"),
    fixtures = kind, n_fixtures = nrow(inputs$pred), n_train = as.integer(spec$N),
    mean_abs_diff = mad, max_abs_diff = max(diff),
    threshold = FV2_REFERENCE_MAX_MEAN_ABS_DIFF,
    nuts_divergences = divergences, nuts_max_rhat = nuts_rhat,
    inla_mode_status = mode_status, inla_gate_passed = gate_passed, passed = passed,
    inla_version = prov("inla_version"), cmdstan_version = prov("cmdstan_version"),
    stan_md5 = prov("stan_md5"), model_version = FOOTBALL_V2_VERSION,
    code_commit = {
      sha <- Sys.getenv("GITHUB_SHA")
      if (nzchar(sha)) sha else NA_character_
    },
    nuts_rhat_scope = FV2_REFERENCE_RHAT_SCOPE
  )
  fixtures <- tibble::tibble(
    match_date = as.Date(inputs$pred$match_date),
    home_team = inputs$pred$home_team, away_team = inputs$pred$away_team,
    inla_home = p_inla[, "home"], inla_draw = p_inla[, "draw"], inla_away = p_inla[, "away"],
    nuts_home = p_nuts[, "home"], nuts_draw = p_nuts[, "draw"], nuts_away = p_nuts[, "away"]
  )
  list(row = row, fixtures = fixtures, spec = spec)
}
