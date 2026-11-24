#' @include model-v2-hbbb.R model-v2-reference.R
NULL

# The weekly Stan reference fit for handball and basketball v2 (the shadow
# period's "reference check" in docs/reports/2026-11-15-hb-bb-v2/report.md),
# the counterpart of football's (R/model-v2-reference.R).
#
# Once a week, for each handball and basketball cell, the v2 inputs are built
# from the latest data exactly as a production fit builds them
# (hbbb_v2_spec()), fitted by R-INLA as fit_league() would fit them that day
# (hbbb_v2_reference_inla()) and by NUTS with the Stan programs of the same
# margin and total models (hbbb_v2_reference_nuts(),
# docs/reports/2026-11-15-hb-bb-v2/stan/v2_margin.stan and v2_total.stan), and
# the two engines' predictive probabilities for the same fixtures are
# compared: the result (home, level, away) and the probability of clearing
# the half-point spread and total lines nearest the fixture's expected margin
# and total, where a predictive is steepest. A mean absolute difference above
# its limit (HBBB_REFERENCE_LIMITS: one pair within a season, a looser one
# across the season break), an unsound NUTS fit or an INLA fit without a mode
# or failing its gate does not pass, and the health check
# (check_engine_reference()) warns.
#
# The comparison reads the predictive locations and observation sds, not the
# score draws, so no score-sampling noise enters it; the level-score shift
# (hbbb_v2_zero_shift()) and basketball's overtime step are left out on both
# sides: they are applied after the fit and are the same for either engine.
# With no fixture inside the horizon the comparison is made on every pairing
# of each division on the date of the cell's next scheduled fixture (a break
# within the season) or a week ahead in the next season (the off-season), so
# the check runs all year.

#' The largest mean absolute differences between INLA and NUTS that pass,
#' per regime: `result` for the result probabilities (home, level, away),
#' `line` for each of the spread and total lines nearest the fixture's
#' expected margin and total.
#'
#' Set from measured differences (2026-11-24, `.claude/rules/model-decide.md`,
#' `docs/evidence/hbbb-engine-reference/`): within a season, where the check
#' is to catch the approximation getting worse as data arrive, the largest
#' measured means were 0.0031 (result) and 0.0116 (a line), over eight
#' cell-runs at three dates. Across the season break -- a fixture in a season
#' the cell has no match of yet: pre-season, and the first fixtures of a
#' season -- a team's next-season state depends on how its strength splits
#' between club and season level, which the data identify only as a sum; the
#' engines differ more there, for that reason (basketball women's mean total
#' gap 0.0250, measured with NUTS converged, Rhat 1.005), so the limits are
#' looser.
#' @noRd
HBBB_REFERENCE_LIMITS <- list(
  in_season = c(result = 0.01, line = 0.025),
  across_break = c(result = 0.02, line = 0.05)
)

#' Does a spec forecast a fixture in a season it has no training match of?
#' The season-break regime of `HBBB_REFERENCE_LIMITS`.
#' @noRd
hbbb_v2_across_break <- function(spec) {
  spec$v$N_pred > 0L && max(spec$v$season_pred) > max(spec$v$season)
}

#' Where the Stan reference programs live, relative to the repository root.
#' @noRd
HBBB_REFERENCE_STAN_DIR <- c("docs", "reports", "2026-11-15-hb-bb-v2", "stan")

#' One fingerprint of the two Stan reference programs and the fragments they
#' include: a change to any of them changes the model the reference fits.
#' @param stan_dir Directory of the programs.
#' @return The md5 of the files' contents, in a fixed order.
#' @noRd
hbbb_v2_stan_md5 <- function(stan_dir) {
  files <- file.path(stan_dir, c(
    "v2_margin.stan", "v2_total.stan", "_data_v2.stan", "_tdata_v2.stan", "_states_v2.stan"
  ))
  stopifnot(all(file.exists(files)))
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(unlist(lapply(files, readLines, warn = FALSE)), tmp, useBytes = TRUE)
  unname(tools::md5sum(tmp))
}

#' Data for the Stan program of one half of v2, from the spec R-INLA fits.
#'
#' The indices are the spec's own (`build_v2_data()`), so both engines see the
#' same grid of states. A training row without a response (a basketball row
#' without regulation-time scores) is an `NA` to R-INLA: it keeps its indices
#' and leaves the likelihood. Stan takes no `NA`, so the row is dropped from
#' the likelihood here; the states it indexed are still in the grid (every
#' team and season, every team-season and week of the spec), with their prior
#' only, as in R-INLA. Each prior scale `u` is passed as the exponential rate
#' `-log(0.01) / u` on the sd, which is R-INLA's `pc.prec(u, 0.01)` on the
#' precision; the within-season sd's scale depends on its form (`u_w_ar1` for
#' the AR(1), `u_w` for the random walk), as in `fit_v2_inla()`.
#'
#' @param spec Output of `hbbb_v2_spec()`.
#' @param response `"margin"` or `"total"`.
#' @param priors `v2_priors()` of the sport.
#' @return A named list for `v2_margin.stan` or `v2_total.stan`.
#' @noRd
hbbb_v2_stan_data <- function(spec, response = c("margin", "total"), priors) {
  response <- match.arg(response)
  v <- spec$v
  obs <- spec$observed
  y <- spec$y[[response]]
  stopifnot(length(obs) == v$N, length(y) == v$N, !anyNA(y[obs]), v$W >= 2L)
  pc <- function(u) -log(0.01) / u
  within <- spec$opts$within
  # WHY as.array(): cmdstanr writes a length-one vector as a scalar, which Stan refuses
  # for an array of size one.
  idx <- function(x) as.array(as.integer(x))
  base <- list(
    K = v$K, NS = v$NS, TS = v$TS, W = v$W, ND = v$ND, N = sum(obs),
    home = idx(v$home[obs]), away = idx(v$away[obs]), season = idx(v$season[obs]),
    div = idx(v$div[obs]), wk = idx(v$wk[obs]),
    ts_home = idx(v$ts_home[obs]), ts_away = idx(v$ts_away[obs]),
    y = as.array(as.numeric(y[obs])),
    N_pred = v$N_pred,
    home_pred = idx(v$home_pred), away_pred = idx(v$away_pred), season_pred = idx(v$season_pred),
    div_pred = idx(v$div_pred), wk_pred = idx(v$wk_pred),
    ts_home_pred = idx(v$ts_home_pred), ts_away_pred = idx(v$ts_away_pred),
    rate_sigma = pc(if (response == "margin") priors$u_sigma_m else priors$u_sigma_t),
    rate_b = pc(priors$u_b), rate_u = pc(priors$u_u),
    rate_w = pc(if (identical(within, "ar1")) priors$u_w_ar1 else priors$u_w),
    rho_mu = priors$rho_mu, rho_sd = priors$rho_sd,
    within_mode = c(none = 0L, rw1 = 1L, ar1 = 2L)[[within]],
    rho_week = spec$opts$rho_week
  )
  if (response == "margin") {
    c(base, list(rate_ha = pc(priors$u_ha), hfa_sd = priors$hfa_sd))
  } else {
    c(base, list(rate_season = pc(priors$u_season), level_mean = priors$total_mean, level_sd = priors$total_mean_sd))
  }
}

#' Fixtures to compare the engines on when none lies inside the horizon.
#'
#' Every pairing, both ways, of the teams of each division, on one date. When
#' the schedules hold a later fixture of the cell, the date and season are
#' that fixture's: a break inside the season (a national-team window) keeps
#' the season being played, and a schedule already published for the next
#' season puts the pairings where a pre-season fit's forecasts are. With
#' nothing scheduled, the pairings are a week ahead in the next season, the
#' break step a pre-season fit takes. The teams and divisions are those of the
#' season being played, or of the last one; each team in the division it
#' played most of that season's matches in, after the spec's division map
#' (handball's play-off rows are the top division's).
#'
#' @param train The spec's training rows (`season`, `division`, `home_team`,
#'   `away_team`).
#' @param sched The cell's schedules, or `NULL`.
#' @param end_date The data date.
#' @param division_map `hbbb_v2_options()$division_map`.
#' @return Data frame with `match_date`, `season`, `division`, `home_team`,
#'   `away_team`, and attribute `kind`: `"mid-season pairings"` or
#'   `"off-season pairings"`.
#' @noRd
hbbb_v2_break_fixtures <- function(train, sched, end_date, division_map = NULL) {
  end_date <- as.Date(end_date)
  last <- max(train$season)
  ahead <- if (is.null(sched) || !nrow(sched)) {
    NULL
  } else {
    sched[!is.na(sched$match_date) & as.Date(sched$match_date) > end_date, , drop = FALSE]
  }
  if (!is.null(ahead) && nrow(ahead)) {
    first <- which.min(as.Date(ahead$match_date))
    match_date <- as.Date(ahead$match_date[first])
    season <- as.integer(ahead$season[first])
  } else {
    match_date <- end_date + 7L
    season <- as.integer(last) + 1L
  }
  kind <- if (season > last) "off-season pairings" else "mid-season pairings"
  rows <- train[train$season == min(season, last), , drop = FALSE]
  division <- as.character(rows$division)
  hit <- division %in% names(division_map)
  division[hit] <- unname(division_map[division[hit]])
  tab <- table(c(rows$home_team, rows$away_team), c(division, division))
  team_div <- stats::setNames(colnames(tab)[apply(tab, 1L, which.max)], rownames(tab))
  pairs <- do.call(rbind, lapply(split(names(team_div), team_div), function(tm) {
    if (length(tm) < 2L) {
      return(NULL)
    }
    g <- expand.grid(home_team = sort(tm), away_team = sort(tm), stringsAsFactors = FALSE)
    g[g$home_team != g$away_team, , drop = FALSE]
  }))
  if (is.null(pairs)) {
    pairs <- data.frame(home_team = character(), away_team = character())
  }
  out <- data.frame(
    match_date = rep(match_date, nrow(pairs)), season = rep(season, nrow(pairs)),
    division = unname(team_div[pairs$home_team]),
    home_team = pairs$home_team, away_team = pairs$away_team,
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  structure(out, kind = kind)
}

#' The compared probabilities of each fixture, from posterior draws of the
#' predictive locations and observation sds.
#'
#' Scores are whole numbers and the predictive is the Gaussian rounded to
#' them (`v2_integer_pmf()`): `home`, `draw` and `away` are the probabilities
#' of a positive, zero and negative margin (for basketball, the regulation
#' margin), with no level-score shift; `spread` is `P(margin > line_m)` and
#' `total` is `P(total > line_t)`.
#'
#' @param loc_m,loc_t Draws-by-fixtures matrices of the margin and total
#'   locations.
#' @param sigma_m,sigma_t One observation sd per draw.
#' @param line_m,line_t Half-point lines, one per fixture.
#' @return Matrix, one row per fixture, columns `home`, `draw`, `away`,
#'   `spread`, `total`.
#' @noRd
hbbb_v2_reference_probs <- function(loc_m, sigma_m, loc_t, sigma_t, line_m, line_t) {
  o <- v2_outcome_probs(loc_m, sigma_m)
  cbind(
    home = o$p_home, draw = o$p_draw, away = o$p_away,
    spread = 1 - v2_cdf(line_m, loc_m, sigma_m),
    total = 1 - v2_cdf(line_t, loc_t, sigma_t)
  )
}

#' Fit one handball or basketball cell with R-INLA and with NUTS and compare
#' them.
#'
#' @param league League config (`handball_iceland` or `basketball_iceland`).
#' @param sex `"male"` or `"female"`.
#' @param end_date The data date: training is everything played before it,
#'   as a production fit on that day sees it.
#' @param root Storage root.
#' @param fit_inla,fit_nuts Functions of a spec, each returning a list with
#'   `loc_m`, `loc_t` (draws by fixtures) and `sigma_m`, `sigma_t` (one per
#'   draw); `fit_inla`'s also carries `mode_status` (one per half) and
#'   `gate_passed` (`hbbb_v2_reference_inla()`), `fit_nuts`'s `diagnostics`,
#'   per half (`margin`, `total`) a list of `n_divergent`, `n_draws` and
#'   `max_rhat` (`hbbb_v2_reference_nuts()`).
#' @param provenance Named list: `inla_version`, `cmdstan_version`,
#'   `stan_md5`, recorded with the result.
#' @return `list(row, fixtures, spec, diagnostics)`: `row` in the
#'   `engine_reference` schema, `fixtures` one row per fixture with both
#'   engines' probabilities, `diagnostics` one row per half (NUTS divergences,
#'   Rhat by group, effective sample size, time; INLA mode status and gate).
#' @noRd
hbbb_v2_weekly_reference <- function(league, sex, end_date = Sys.Date(),
                                     root = here::here("data"),
                                     fit_inla, fit_nuts, provenance = list()) {
  end_date <- as.Date(end_date)
  sport <- league$sport
  if (!sport %in% c("handball", "basketball")) {
    cli::cli_abort("{.fn hbbb_v2_weekly_reference} compares handball and basketball; {.val {sport}} is not one of them.")
  }
  opts <- hbbb_v2_options(sport, sex)
  # The 14-day horizon fit_league() uses.
  prep <- prepare_data(league, sex, end_date = end_date, root = root)
  spec <- hbbb_v2_spec(prep, league, sex, end_date = end_date, root = root, opts = opts)
  kind <- "upcoming"
  if (spec$v$N_pred == 0L) {
    filt <- list(sport = league$sport, country = league$country, sex = sex)
    sched <- tryCatch(as.data.frame(read_table("schedules", root = root, filter = filt)), error = function(e) NULL)
    pairs <- hbbb_v2_break_fixtures(spec$train, sched, end_date, division_map = opts$division_map)
    kind <- attr(pairs, "kind")
    spec <- hbbb_v2_spec(prep, league, sex, end_date = end_date, root = root, opts = opts, pred = pairs)
  }
  across <- hbbb_v2_across_break(spec)
  if (across) kind <- paste(kind, "across the season break", sep = ", ")
  cli::cli_alert_info(
    "Weekly reference {sport}/{league$country}/{sex} at {end_date}: {sum(spec$observed)} matches in the likelihood, {spec$v$K} teams, {spec$v$N_pred} fixtures ({kind})"
  )

  inla <- fit_inla(spec)
  nuts <- fit_nuts(spec)
  line_m <- floor((colMeans(inla$loc_m) + colMeans(nuts$loc_m)) / 2) + 0.5
  line_t <- floor((colMeans(inla$loc_t) + colMeans(nuts$loc_t)) / 2) + 0.5
  p_inla <- hbbb_v2_reference_probs(inla$loc_m, inla$sigma_m, inla$loc_t, inla$sigma_t, line_m, line_t)
  p_nuts <- hbbb_v2_reference_probs(nuts$loc_m, nuts$sigma_m, nuts$loc_t, nuts$sigma_t, line_m, line_t)
  diff <- abs(p_inla - p_nuts)
  result <- diff[, c("home", "draw", "away"), drop = FALSE]

  # The health layer's bar for a production Stan fit, held by each half
  # (health_thresholds(): divergences under 0.5 % of transitions, Rhat at most
  # 1.02). A half with no Rhat has no evidence its chains agree: no pass.
  th <- health_thresholds()
  halves <- nuts$diagnostics[c("margin", "total")]
  div_frac <- max(vapply(halves, function(h) as.numeric(h$n_divergent) / max(1, as.numeric(h$n_draws)), numeric(1)))
  rhats <- vapply(halves, function(h) as.numeric(h$max_rhat %||% NA_real_), numeric(1))
  nuts_rhat <- if (anyNA(rhats)) NA_real_ else max(rhats)
  divergences <- as.integer(sum(vapply(halves, function(h) as.numeric(h$n_divergent), numeric(1))))
  ms <- as.integer(inla$mode_status)
  mode_ok <- length(ms) == 2L && all(v2_mode_ok(ms))
  mode_status <- if (mode_ok) 0L else c(ms[!v2_mode_ok(ms)], NA_integer_)[[1L]]
  gate_passed <- isTRUE(inla$gate_passed)
  mad <- mean(result)
  spread_mad <- mean(diff[, "spread"])
  total_mad <- mean(diff[, "total"])
  limits <- HBBB_REFERENCE_LIMITS[[if (across) "across_break" else "in_season"]]
  passed <- isTRUE(mad <= limits[["result"]]) &&
    isTRUE(spread_mad <= limits[["line"]]) && isTRUE(total_mad <= limits[["line"]]) &&
    isTRUE(div_frac <= th$div_frac_warn) && isTRUE(nuts_rhat <= th$rhat_warn) &&
    mode_ok && gate_passed
  prov <- function(name) as.character(provenance[[name]] %||% NA_character_)

  row <- tibble::tibble(
    sport = league$sport, country = league$country, sex = sex,
    run_date = end_date,
    run_at = as.POSIXct(floor(as.numeric(Sys.time())), origin = "1970-01-01", tz = "UTC"),
    fixtures = kind, n_fixtures = as.integer(spec$v$N_pred), n_train = as.integer(sum(spec$observed)),
    mean_abs_diff = mad, max_abs_diff = max(result),
    threshold = limits[["result"]],
    nuts_divergences = divergences, nuts_max_rhat = nuts_rhat,
    inla_mode_status = mode_status, inla_gate_passed = gate_passed, passed = passed,
    inla_version = prov("inla_version"), cmdstan_version = prov("cmdstan_version"),
    stan_md5 = prov("stan_md5"), model_version = HBBB_V2_VERSION,
    code_commit = {
      sha <- Sys.getenv("GITHUB_SHA")
      if (nzchar(sha)) sha else NA_character_
    },
    spread_mean_abs_diff = spread_mad, spread_max_abs_diff = max(diff[, "spread"]),
    total_mean_abs_diff = total_mad, total_max_abs_diff = max(diff[, "total"]),
    line_threshold = limits[["line"]]
  )
  fx <- spec$pred
  fixtures <- tibble::tibble(
    match_date = as.Date(fx$match_date), division = as.character(fx$division),
    home_team = fx$home_team, away_team = fx$away_team,
    inla_home = p_inla[, "home"], inla_draw = p_inla[, "draw"], inla_away = p_inla[, "away"],
    nuts_home = p_nuts[, "home"], nuts_draw = p_nuts[, "draw"], nuts_away = p_nuts[, "away"],
    line_margin = line_m, inla_spread = p_inla[, "spread"], nuts_spread = p_nuts[, "spread"],
    line_total = line_t, inla_total = p_inla[, "total"], nuts_total = p_nuts[, "total"],
    inla_mean_margin = colMeans(inla$loc_m), nuts_mean_margin = colMeans(nuts$loc_m),
    inla_mean_total = colMeans(inla$loc_t), nuts_mean_total = colMeans(nuts$loc_t)
  )
  diagnostics <- do.call(rbind, lapply(names(halves), function(h) {
    d <- halves[[h]]
    data.frame(
      half = h,
      n_divergent = as.integer(d$n_divergent), n_draws = as.integer(d$n_draws),
      n_max_treedepth = as.integer(d$n_max_treedepth %||% NA_integer_),
      max_rhat = as.numeric(d$max_rhat %||% NA_real_),
      rhat_hyper = as.numeric(d$rhat_hyper %||% NA_real_),
      rhat_states = as.numeric(d$rhat_states %||% NA_real_),
      rhat_pred = as.numeric(d$rhat_pred %||% NA_real_),
      min_ess_bulk = as.numeric(d$min_ess_bulk %||% NA_real_),
      nuts_secs = as.numeric(d$wall_secs %||% NA_real_),
      inla_mode_status = as.integer(ms[match(h, c("margin", "total"))]),
      # WHY [[ ]]: `$gate` would partially match `gate_passed` on a fit without a gate.
      inla_gate = if (gate_passed) "passed" else paste(inla[["gate"]][["breaches"]] %||% "failed", collapse = "; ")
    )
  }))
  list(row = row, fixtures = fixtures, spec = spec, diagnostics = diagnostics)
}

#' The INLA side of the weekly reference: the cell fitted as `fit_league()`
#' would fit it on `end_date`.
#'
#' Its seed (`.fit_seed()` of the cell and the date), 4,000 draws (four
#' chains of 1,000, as `fit_league()` converts them), full integration, the
#' fitter's one thread and per-call `internal.opt = FALSE`
#' (`hbbb_v2_fit_halves()`), the production pairing of the two halves
#' (`hbbb_v2_pair()`), and its gate against the cell's previous INLA fit when
#' there is one (`fv2_previous_hyper()`). A failed gate is recorded, not
#' raised.
#'
#' @return A function of a spec for `hbbb_v2_weekly_reference()`.
#' @noRd
hbbb_v2_reference_inla <- function(league, sex, end_date, root = here::here("data"), n_draws = 4000L) {
  sport <- league$sport
  seed <- .fit_seed(sport, league$country, sex, format(as.Date(end_date)))
  previous_sd <- fv2_previous_hyper(league, sex, end_date, root = root, version = HBBB_V2_VERSION)
  function(spec) {
    fits <- hbbb_v2_fit_halves(spec, sport, n_draws = n_draws, seed = seed, int_strategy = "auto")
    gate <- hbbb_v2_gate(fits, spec, sport, previous_sd = previous_sd)
    if (!isTRUE(gate$passed)) {
      cli::cli_alert_warning("{sport}/{sex}: INLA gate: {paste(gate$breaches, collapse = '; ')}")
    }
    comp <- hbbb_v2_pair(fits, spec, seed = seed)
    list(
      loc_m = comp$loc_m, loc_t = comp$loc_t, sigma_m = comp$sigma_m, sigma_t = comp$sigma_t,
      mode_status = c(margin = fits$margin$mode_status, total = fits$total$mode_status),
      gate_passed = isTRUE(gate$passed), gate = gate, hyper_sd = hbbb_v2_hyper_sd(fits)
    )
  }
}

#' The largest Rhat of each group of a v2 Stan fit's quantities.
#'
#' `pred`: what the reference comparison reads, every fixture's predictive
#' location (`mu_pred`) and the observation sd (`sigma`); the reference is
#' held to it. `states`: the club and season levels (`b`, `u`). `hyper`: the
#' hyperparameters and fixed effects. A team's club level and its season
#' levels are identified only as a sum (the season level's memory is high),
#' so NUTS moves slowly along that ridge and their Rhat sits near 1.02 to 1.03
#' while the sum, and every fixture's location, has converged (handball women,
#' 2026-11-24: `b[6]` and `u[6, ]` at 1.023 to 1.026, `sigma_u` 1.024, the
#' locations at most 1.003). The production gate's jump check leaves the
#' same split ungated (`HBBB_V2_JUMP_CHECKED`).
#'
#' @param summary `posterior::summarise_draws()` output with `variable` and
#'   `rhat`.
#' @return `list(pred, states, hyper)`, each `NA` when the group has no Rhat.
#' @noRd
hbbb_v2_rhat_groups <- function(summary) {
  top <- function(pattern) {
    r <- summary$rhat[grepl(pattern, summary$variable)]
    if (!length(r) || all(is.na(r))) NA_real_ else max(r, na.rm = TRUE)
  }
  list(
    pred = top("^(mu_pred\\[|sigma$)"),
    states = top("^(b|u)\\["),
    hyper = top("^(sigma|sigma_b|sigma_u|theta_rho|sigma_w|sigma_ha|hfa|sigma_season|alpha)(\\[|$)")
  )
}

#' The NUTS side of the weekly reference: both halves fitted with the Stan
#' reference programs.
#'
#' Four chains of 1,000 warm-up and 1,000 sampling iterations and a tree
#' depth of 10, started at zero, as the programs were validated in the report,
#' but with `adapt_delta` 0.99 where the report used 0.95: on 2026-11-24 the
#' women's basketball margin model had 41 divergences in 4,000 transitions at
#' 0.95 (twice the health bar) and none at 0.99, for 1.7 times the sampling
#' time; the forecasts' Rhat was 1.003 either way. The Rhat the reference is held to
#' (`max_rhat`) is that of what the comparison reads: every fixture's
#' predictive location and the observation sd. The hyperparameters' and the
#' club and season levels' are recorded (`rhat_hyper`, `rhat_states`), not
#' held: a team's club and season levels are identified only as a sum, and
#' their Rhat runs above 1.02 on fits whose forecasts have converged.
#'
#' @param sport `"handball"` or `"basketball"`.
#' @param stan_dir Directory of `v2_margin.stan`, `v2_total.stan` and the
#'   fragments they include.
#' @param exe_dir Where the compiled programs are kept.
#' @param seed Sampler seed.
#' @return A function of a spec for `hbbb_v2_weekly_reference()`.
#' @noRd
hbbb_v2_reference_nuts <- function(sport, stan_dir, exe_dir, seed, chains = 4L, iter_warmup = 1000L,
                                   iter_sampling = 1000L, adapt_delta = 0.99, refresh = 0L) {
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    cli::cli_abort("The NUTS reference needs {.pkg cmdstanr} and CmdStan.")
  }
  priors <- v2_priors(sport)
  one <- function(spec, response) {
    mod <- cmdstanr::cmdstan_model(
      file.path(stan_dir, sprintf("v2_%s.stan", response)),
      dir = exe_dir, include_paths = stan_dir, quiet = TRUE
    )
    csv_dir <- tempfile("hbbb-reference-csv-")
    dir.create(csv_dir)
    on.exit(unlink(csv_dir, recursive = TRUE), add = TRUE)
    t0 <- Sys.time()
    fit <- mod$sample(
      data = hbbb_v2_stan_data(spec, response, priors),
      chains = chains, parallel_chains = chains, iter_warmup = iter_warmup, iter_sampling = iter_sampling,
      adapt_delta = adapt_delta, max_treedepth = 10L, init = 0, seed = seed, refresh = refresh,
      output_dir = csv_dir, show_messages = FALSE, show_exceptions = FALSE
    )
    wall <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    hyper <- c(
      "sigma", "sigma_b", "sigma_u", "theta_rho",
      if (!identical(spec$opts$within, "none")) "sigma_w",
      if (response == "margin") c("sigma_ha", "hfa") else c("sigma_season", "alpha")
    )
    mat <- function(v) unclass(posterior::as_draws_matrix(fit$draws(v)))
    loc <- mat("mu_pred")
    dimnames(loc) <- NULL
    sm <- posterior::summarise_draws(fit$draws(c(hyper, "b", "u", "mu_pred")), "rhat", "ess_bulk")
    rh <- hbbb_v2_rhat_groups(sm)
    dg <- fit$diagnostic_summary(quiet = TRUE)
    list(
      loc = loc, sigma = as.numeric(mat("sigma")),
      diagnostics = list(
        n_divergent = sum(dg$num_divergent), n_draws = chains * iter_sampling,
        n_max_treedepth = sum(dg$num_max_treedepth),
        max_rhat = rh$pred,
        rhat_hyper = rh$hyper, rhat_states = rh$states, rhat_pred = rh$pred,
        min_ess_bulk = suppressWarnings(min(sm$ess_bulk, na.rm = TRUE)),
        wall_secs = wall
      )
    )
  }
  function(spec) {
    m <- one(spec, "margin")
    t <- one(spec, "total")
    list(
      loc_m = m$loc, loc_t = t$loc, sigma_m = m$sigma, sigma_t = t$sigma,
      diagnostics = list(margin = m$diagnostics, total = t$diagnostics)
    )
  }
}
