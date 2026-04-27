#' @include model-prepare.R storage.R config.R
NULL

# ---- Internal helpers --------------------------------------------------------

# Row-wise log-sum-exp, numerically stable.
.row_log_sum_exp_pfi <- function(m) {
  m_max <- do.call(pmax, as.data.frame(m))
  m_max + log(rowSums(exp(m - m_max)))
}

# Vectorised bivariate-Poisson log-PMF (trivariate reduction).
# Mirrors poisson_2d_log_lpmf in bivariate_poisson_no_inflation.stan.
.log_pmf_bvp_pfi <- function(g1, g2, log_lambda1, log_lambda2, log_lambda3) {
  lambda1 <- exp(log_lambda1)
  lambda2 <- exp(log_lambda2)
  lambda3 <- exp(log_lambda3)

  K <- min(g1, g2)
  if (K < 0L) {
    return(rep(-Inf, length(log_lambda1)))
  }

  n <- length(log_lambda1)
  log_terms <- matrix(NA_real_, nrow = n, ncol = K + 1L)
  for (k in 0:K) {
    log_terms[, k + 1L] <-
      k * log_lambda3 +
      (g1 - k) * log_lambda1 +
      (g2 - k) * log_lambda2 -
      (lgamma(k + 1) + lgamma(g1 - k + 1) + lgamma(g2 - k + 1))
  }

  lse <- if (ncol(log_terms) == 1L) log_terms[, 1L] else .row_log_sum_exp_pfi(log_terms)
  result <- -(lambda1 + lambda2 + lambda3) + lse

  small <- lambda3 <= 1e-4
  if (any(small)) {
    result[small] <-
      stats::dpois(g1, lambda1[small], log = TRUE) +
      stats::dpois(g2, lambda2[small], log = TRUE)
  }
  result
}

# Simulate (Y1, Y2) from bivariate Poisson via trivariate reduction.
.simulate_bvp_pfi <- function(mu1, mu2, mu3) {
  n <- length(mu1)
  X1 <- stats::rpois(n, exp(mu1))
  X2 <- stats::rpois(n, exp(mu2))
  X3 <- stats::rpois(n, exp(mu3))
  list(y1 = X1 + X3, y2 = X2 + X3)
}

# Build model_d from results tibble + teams registry.
# Returns the same structure as the legacy model_d.csv with columns:
#   match_date, home_team, away_team, home_score, away_score,
#   division, season, game_nr, home_nr, away_nr,
#   home_round, away_round, season_first, model_row
.build_model_d_pfi <- function(results, teams) {
  results <- results[order(results$match_date), ]
  results$game_nr <- seq_len(nrow(results))

  # Per-team round counter (cumulative appearances across all seasons)
  long <- dplyr::bind_rows(
    dplyr::transmute(results,
      game_nr = .data$game_nr, match_date = .data$match_date,
      team = .data$home_team, season = .data$season, side = "home"
    ),
    dplyr::transmute(results,
      game_nr = .data$game_nr, match_date = .data$match_date,
      team = .data$away_team, season = .data$season, side = "away"
    )
  )
  long <- long[order(long$team, long$match_date), ]
  long <- long |>
    dplyr::group_by(.data$team) |>
    dplyr::mutate(round = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::group_by(.data$team, .data$season) |>
    dplyr::mutate(
      season_round = dplyr::row_number(),
      is_first = as.integer(.data$season_round == 1L)
    ) |>
    dplyr::ungroup()

  home_long <- long[long$side == "home",
    c("game_nr", "round", "is_first"),
    drop = FALSE
  ]
  names(home_long) <- c("game_nr", "home_round", "season_first")

  away_long <- long[long$side == "away",
    c("game_nr", "round"),
    drop = FALSE
  ]
  names(away_long) <- c("game_nr", "away_round")

  model_d <- results |>
    dplyr::inner_join(home_long, by = "game_nr") |>
    dplyr::inner_join(away_long, by = "game_nr") |>
    dplyr::inner_join(
      dplyr::rename(teams, home_nr = "team_nr"),
      by = c("home_team" = "team")
    ) |>
    dplyr::inner_join(
      dplyr::rename(teams, away_nr = "team_nr"),
      by = c("away_team" = "team")
    ) |>
    dplyr::arrange(.data$match_date)

  model_d$model_row <- seq_len(nrow(model_d))
  model_d
}

# Compute per-team posterior-predictive xG and xPts for played current-season
# top-division matches.  Returns a tibble with columns:
#   team, xg_for, xg_against, xpts  (posterior means, averaged over draws)
#
# If the fit dimensions do not match model_d, returns NULL with a warning.
.aggregate_played_match_expected_pfi <- function(
  fit, model_d, teams,
  top_div = "BD",
  check_sample_size = 5L
) {
  current_season <- max(model_d$season, na.rm = TRUE)
  matches <- model_d[
    model_d$season == current_season &
      model_d$division == top_div &
      !is.na(model_d$home_score) &
      !is.na(model_d$away_score), ,
    drop = FALSE
  ]

  if (nrow(matches) == 0L) {
    return(tibble::tibble(
      team = character(), xg_for = numeric(),
      xg_against = numeric(), xpts = numeric()
    ))
  }

  rv <- fit$draws(c(
    "offense", "defense",
    "home_advantage_off", "home_advantage_def",
    "mean_log_goals",
    "alpha_mu3", "beta_mu3_strength_diff",
    "log_lik"
  )) |> posterior::as_draws_rvars()

  n_draws <- posterior::ndraws(rv$mean_log_goals)

  # Validate: log_lik[n] must cover model_d
  loglik_dr <- posterior::draws_of(rv$log_lik)
  n_loglik <- ncol(loglik_dr)
  if (n_loglik != nrow(model_d)) {
    warning(sprintf(
      paste0(
        "publish_football_iceland: fit has log_lik[%d] but model_d has %d rows. ",
        "The fit was trained on different data -- skipping xG/xPts computation."
      ),
      n_loglik, nrow(model_d)
    ))
    return(NULL)
  }

  offense_dr <- posterior::draws_of(rv$offense) # [draws, N_rounds, K]
  defense_dr <- posterior::draws_of(rv$defense)
  ha_off_dr <- posterior::draws_of(rv$home_advantage_off) # [draws, K]
  ha_def_dr <- posterior::draws_of(rv$home_advantage_def)
  mlg_dr <- as.numeric(posterior::draws_of(rv$mean_log_goals))
  a_mu3_dr <- as.numeric(posterior::draws_of(rv$alpha_mu3))
  b_mu3_dr <- as.numeric(posterior::draws_of(rv$beta_mu3_strength_diff))

  check_n <- as.integer(min(check_sample_size, nrow(matches)))
  check_idx <- integer(0)
  recon_ll <- NULL
  if (check_n > 0L) {
    check_idx <- withr::with_seed(42L, sample.int(nrow(matches), check_n))
    recon_ll <- matrix(NA_real_, nrow = n_draws, ncol = check_n)
  }

  team_per_match <- vector("list", nrow(matches))

  for (m in seq_len(nrow(matches))) {
    mi <- matches[m, ]
    t1 <- mi$home_nr
    t2 <- mi$away_nr
    r1 <- mi$home_round
    r2 <- mi$away_round

    off_home <- offense_dr[, r1, t1] + ha_off_dr[, t1]
    def_home <- defense_dr[, r1, t1] + ha_def_dr[, t1]
    off_away <- offense_dr[, r2, t2]
    def_away <- defense_dr[, r2, t2]

    mu1 <- mlg_dr + off_home - def_away
    mu2 <- mlg_dr + off_away - def_home

    strength_diff <- abs(off_home + def_home - off_away - def_away)
    logit_rho <- a_mu3_dr + b_mu3_dr * strength_diff
    mu3 <- stats::plogis(logit_rho, log.p = TRUE) + 0.5 * (mu1 + mu2)

    sim <- .simulate_bvp_pfi(mu1, mu2, mu3)

    pts_home <- ifelse(sim$y1 > sim$y2, 3L, ifelse(sim$y1 == sim$y2, 1L, 0L))
    pts_away <- ifelse(sim$y2 > sim$y1, 3L, ifelse(sim$y2 == sim$y1, 1L, 0L))

    team_per_match[[m]] <- dplyr::bind_rows(
      tibble::tibble(
        .draw = seq_len(n_draws), team = teams$team[t1],
        xg_for = sim$y1, xg_against = sim$y2, xpts = pts_home
      ),
      tibble::tibble(
        .draw = seq_len(n_draws), team = teams$team[t2],
        xg_for = sim$y2, xg_against = sim$y1, xpts = pts_away
      )
    )

    slot <- match(m, check_idx)
    if (!is.na(slot)) {
      recon_ll[, slot] <- .log_pmf_bvp_pfi(
        mi$home_score, mi$away_score, mu1, mu2, mu3
      )
    }
  }

  team_expected_draw <- dplyr::bind_rows(team_per_match) |>
    dplyr::summarise(
      xg_for = sum(.data$xg_for),
      xg_against = sum(.data$xg_against),
      xpts = sum(.data$xpts),
      .by = c("team", ".draw")
    )

  # Log-likelihood cross-check
  if (check_n > 0L) {
    stan_ll <- loglik_dr[, matches$model_row[check_idx], drop = FALSE]
    stan_mean <- colMeans(stan_ll)
    recon_mean <- colMeans(recon_ll)
    pooled_sd <- pmax(apply(stan_ll, 2, stats::sd), apply(recon_ll, 2, stats::sd))
    mc_se <- pooled_sd / sqrt(n_draws)
    tol <- 4 * mc_se
    diff_ <- abs(stan_mean - recon_mean)
    bad <- diff_ > tol
    if (any(bad)) {
      warning(sprintf(
        paste0(
          "publish_football_iceland: log_lik reconstruction disagrees with Stan on %d/%d ",
          "sampled matches (>4 x MC SE). xG/xPts may be inaccurate."
        ),
        sum(bad), check_n
      ))
    }
  }

  team_expected_draw |>
    dplyr::summarise(
      xg_for = mean(.data$xg_for),
      xg_against = mean(.data$xg_against),
      xpts = mean(.data$xpts),
      .by = "team"
    )
}

# Extract per-team draws for a single Stan parameter vector indexed by team.
.extract_team_draws_pfi <- function(fit, var, teams, component, location) {
  fit$draws(var) |>
    posterior::as_draws_df() |>
    tibble::as_tibble() |>
    tidyr::pivot_longer(
      c(-".chain", -".draw", -".iteration"),
      names_to = "name", values_to = "value"
    ) |>
    dplyr::mutate(
      team_idx  = as.integer(readr::parse_number(.data$name)),
      team      = teams$team[.data$team_idx],
      component = component,
      location  = location
    )
}

.summarise_team_intervals_pfi <- function(draws, coverages = c(0.5, 0.8, 0.95)) {
  draws |>
    dplyr::reframe(
      median = stats::median(.data$value),
      coverage = coverages,
      lower = stats::quantile(.data$value, 0.5 - coverages / 2),
      upper = stats::quantile(.data$value, 0.5 + coverages / 2),
      .by = c("team", "component", "location")
    )
}

# ---- Public API --------------------------------------------------------------

#' Publish football Iceland posterior summaries as JSON
#'
#' Consumes a CmdStanMCMC fit from `fit_model()` / `fit_league()` and writes
#' seven JSON files into `output_root/football/iceland/{karla|kvenna}/`:
#'   - `meta.json`
#'   - `next_games.json`
#'   - `standings.json`   (male / karla only)
#'   - `team_strengths.json`
#'   - `final_positions.json`
#'   - `points_distribution.json`
#'   - `home_advantage.json`
#'
#' @param fit CmdStanMCMC returned by `fit_model()`.  Must have been trained on
#'   data consistent with `(league, sex, end_date)`.
#' @param league A single entry from `load_leagues()` (must have `sport` and
#'   `country` set).
#' @param sex `"male"` or `"female"`.
#' @param end_date Training cutoff passed to `prepare_data()`. Default `Sys.Date()`.
#' @param root Data root for `read_table()`. Default `here::here("data")`.
#' @param output_root Root for JSON output. Default `here::here("data", "publish")`.
#' @return `invisible(NULL)`.
#' @importFrom rlang .data
#' @export
publish_football_iceland <- function(fit,
                                     league,
                                     sex,
                                     end_date = Sys.Date(),
                                     root = here::here("data"),
                                     output_root = here::here("data", "publish")) {
  stopifnot(sex %in% c("male", "female"))
  stopifnot(!is.null(league$sport), !is.null(league$country))
  stopifnot(league$sport == "football", league$country == "iceland")
  stopifnot(inherits(end_date, "Date"))

  # Icelandic sex folder names
  sex_folder <- if (sex == "male") "karla" else "kvenna"
  out_dir <- file.path(output_root, "football", "iceland", sex_folder)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # -- Rebuild prep data -------------------------------------------------------
  prep <- prepare_data(league, sex, end_date = end_date, root = root)
  teams <- prep$teams
  pred_d <- prep$pred_d # next-game matches with home_team / away_team / match_date / division

  # -- Training results --------------------------------------------------------
  results <- read_table(
    "results",
    root   = root,
    filter = list(sport = league$sport, country = league$country, sex = sex)
  )
  results <- results[results$match_date <= end_date, , drop = FALSE]
  results <- results[!is.na(results$home_score) & !is.na(results$away_score), , drop = FALSE]

  # Top division label in the new schema
  top_div <- "BD"

  current_season <- max(results$season, na.rm = TRUE)

  # -- Reconstruct model_d (needed for played_match_expected) -----------------
  model_d <- .build_model_d_pfi(results, teams)

  # -- Current top-division teams (for strengths / home-advantage filter) -----
  current_top_teams <- results[
    results$season == current_season & results$division == top_div, ,
    drop = FALSE
  ] |>
    dplyr::select("home_team", "away_team") |>
    tidyr::pivot_longer(c("home_team", "away_team"), values_to = "team") |>
    dplyr::distinct(.data$team)

  # Top teams with upcoming BD fixtures (for home_advantage filter)
  top_teams_upcoming <- pred_d[pred_d$division == top_div, , drop = FALSE] |>
    dplyr::select("home_team", "away_team") |>
    tidyr::pivot_longer(c("home_team", "away_team"), values_to = "team") |>
    dplyr::distinct(.data$team)

  # Fallback: if no upcoming BD fixtures, use current-season BD teams
  if (nrow(top_teams_upcoming) == 0L) {
    top_teams_upcoming <- current_top_teams
  }

  # -- xG / xPts per team (NULL if data-fit mismatch) ------------------------
  team_expected <- .aggregate_played_match_expected_pfi(
    fit, model_d, teams,
    top_div = top_div
  )

  # -- Posterior goals draws --------------------------------------------------
  posterior_goals_raw <- fit$draws(c("goals1_pred", "goals2_pred")) |>
    posterior::as_draws_df() |>
    tibble::as_tibble() |>
    tidyr::pivot_longer(
      -c(".draw", ".chain", ".iteration"),
      names_to  = "parameter",
      values_to = "value"
    ) |>
    dplyr::mutate(
      type = dplyr::if_else(
        stringr::str_detect(.data$parameter, "goals1"), "home_goals", "away_goals"
      ),
      game_nr = as.integer(stringr::str_match(.data$parameter, "\\[(\\d+)\\]$")[, 2])
    ) |>
    dplyr::select(".draw", "type", "game_nr", "value") |>
    tidyr::pivot_wider(names_from = "type", values_from = "value")

  # Validate pred dimension matches
  n_pred_fit <- max(posterior_goals_raw$game_nr, na.rm = TRUE)
  n_pred_data <- nrow(pred_d)
  if (n_pred_fit != n_pred_data) {
    warning(sprintf(
      paste0(
        "publish_football_iceland: fit was trained with N_pred=%d prediction matches ",
        "but prepare_data returned %d. JSONs that depend on posterior_goals will be empty."
      ),
      n_pred_fit, n_pred_data
    ))
    posterior_goals <- tibble::tibble(
      .draw = integer(), game_nr = integer(),
      home_goals = numeric(), away_goals = numeric(),
      match_date = as.Date(character()),
      home_team = character(), away_team = character(),
      division = character()
    )
  } else {
    posterior_goals <- posterior_goals_raw |>
      dplyr::inner_join(
        pred_d[, c("game_nr", "match_date", "home_team", "away_team", "division")],
        by = "game_nr"
      )
  }

  generated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")

  # ---- meta.json ------------------------------------------------------------

  round_num <- results[
    results$season == current_season & results$division == top_div, ,
    drop = FALSE
  ] |>
    tidyr::pivot_longer(c("home_team", "away_team"), values_to = "team") |>
    dplyr::count(.data$team) |>
    dplyr::pull("n") |>
    (\(x) if (length(x) == 0L) 0L else min(x))()

  n_draws <- posterior::ndraws(fit$draws("home_advantage_tot"))

  meta <- list(
    sex          = sex,
    league       = "Besta deild",
    season       = current_season,
    generated_at = generated_at,
    fit_date     = format(end_date, "%Y-%m-%d"),
    round        = as.integer(round_num),
    n_draws      = as.integer(n_draws)
  )
  jsonlite::write_json(
    meta,
    file.path(out_dir, "meta.json"),
    auto_unbox = TRUE
  )

  # ---- next_games.json ------------------------------------------------------

  male_top_division_venues <- tibble::tribble(
    ~team, ~venue,
    "Brei\u00f0ablik", "K\u00f3pavogsv\u00f6llur",
    "FH", "Kaplakrikav\u00f6llur",
    "Fram", "Laugardalsv\u00f6llur",
    "KA", "KA-v\u00f6llurinn",
    "KR", "KR-v\u00f6llur",
    "Keflav\u00edk", "Nettov\u00f6llurinn",
    "Stjarnan", "Stj\u00f6rnuv\u00f6llur",
    "Valur", "Hl\u00ed\u00f0arendi",
    "V\u00edkingur R.", "V\u00edkingsv\u00f6llur",
    "\u00cdA", "Nor\u00f0ur\u00e1lsv\u00f6llurinn",
    "\u00cdBV", "H\u00e1steinv\u00f6llur",
    "\u00de\u00f3r", "\u00de\u00f3rsv\u00f6llur"
  )

  # Division display codes (BD = Besta deild = division 1)
  division_labels <- c(
    BD = "BD", LD1 = "LD", LD2 = "\u00d6D", LD3 = "\u00deD",
    LD4 = "FjD", CUP = "MB",
    BD_UPPER_PO = "BD PO", BD_LOWER_PO = "BD N-PO",
    LD1_PO = "LD PO"
  )

  if (nrow(posterior_goals) > 0L) {
    next_games_out <- posterior_goals |>
      dplyr::filter(
        .data$match_date >= end_date,
        .data$match_date <= end_date + 14L
      ) |>
      dplyr::mutate(goal_diff = .data$home_goals - .data$away_goals) |>
      dplyr::summarise(
        mean_home_goals = mean(.data$home_goals),
        mean_away_goals = mean(.data$away_goals),
        mean_goal_diff = mean(.data$goal_diff),
        p_home_win = mean(.data$goal_diff > 0),
        p_draw = mean(.data$goal_diff == 0),
        p_away_win = mean(.data$goal_diff < 0),
        # NB: tibble::tibble() has no data-mask context, so .data$goal_diff
        # would fail with "Column `goal_diff` not found in `.data`". The bare
        # `goal_diff` resolves via summarise()'s outer mask before the call.
        goal_diff_distribution = list(
          tibble::tibble(diff = goal_diff) |>
            dplyr::count(.data$diff) |>
            dplyr::mutate(p = .data$n / sum(.data$n)) |>
            dplyr::select("diff", "p")
        ),
        .by = c("game_nr", "division", "match_date", "home_team", "away_team")
      ) |>
      dplyr::arrange(.data$match_date, .data$game_nr) |>
      dplyr::left_join(male_top_division_venues, by = c("home_team" = "team")) |>
      dplyr::mutate(
        division_code = dplyr::recode(.data$division, !!!division_labels, .default = .data$division),
        date          = format(.data$match_date, "%Y-%m-%d")
      ) |>
      dplyr::select(
        "date", "venue", "division", "division_code",
        home = "home_team", away = "away_team",
        "mean_home_goals", "mean_away_goals", "mean_goal_diff",
        "p_home_win", "p_draw", "p_away_win",
        "goal_diff_distribution"
      )
  } else {
    next_games_out <- tibble::tibble(
      date = character(), venue = character(),
      division = character(), division_code = character(),
      home = character(), away = character(),
      mean_home_goals = numeric(), mean_away_goals = numeric(),
      mean_goal_diff = numeric(), p_home_win = numeric(),
      p_draw = numeric(), p_away_win = numeric(),
      goal_diff_distribution = list()
    )
  }

  jsonlite::write_json(
    list(generated_at = generated_at, matches = next_games_out),
    file.path(out_dir, "next_games.json"),
    auto_unbox = TRUE, dataframe = "rows", digits = 5, na = "null"
  )

  # ---- standings.json (top division, male only) ----------------------------

  if (sex == "male") {
    bd_results <- results[
      results$season == current_season & results$division == top_div, ,
      drop = FALSE
    ]

    pad_form <- function(x, n = 5L) {
      tail(c(rep(NA_character_, n), x), n)
    }

    short_code <- function(team) {
      team |>
        stringr::str_remove_all("\\s|\\.") |>
        stringr::str_to_upper() |>
        stringr::str_sub(1L, 3L)
    }

    if (nrow(bd_results) > 0L) {
      long_bd <- dplyr::bind_rows(
        dplyr::transmute(bd_results,
          team = .data$home_team, match_date = .data$match_date,
          gf = .data$home_score, ga = .data$away_score
        ),
        dplyr::transmute(bd_results,
          team = .data$away_team, match_date = .data$match_date,
          gf = .data$away_score, ga = .data$home_score
        )
      ) |>
        dplyr::mutate(
          result = dplyr::case_when(
            .data$gf > .data$ga ~ "W",
            .data$gf < .data$ga ~ "L",
            TRUE ~ "D"
          )
        ) |>
        dplyr::arrange(.data$team, .data$match_date)

      standings_rows <- long_bd |>
        dplyr::summarise(
          played = dplyr::n(),
          wins = sum(.data$result == "W"),
          draws = sum(.data$result == "D"),
          losses = sum(.data$result == "L"),
          goals_for = sum(.data$gf),
          goals_against = sum(.data$ga),
          goal_diff = .data$goals_for - .data$goals_against,
          points = 3L * .data$wins + .data$draws,
          form = list(pad_form(tail(.data$result, 5L))),
          xg_trend = list(numeric(0)),
          .by = "team"
        ) |>
        dplyr::arrange(
          dplyr::desc(.data$points), dplyr::desc(.data$goal_diff),
          dplyr::desc(.data$goals_for)
        ) |>
        dplyr::mutate(
          rank  = dplyr::row_number(),
          short = short_code(.data$team)
        )

      if (!is.null(team_expected)) {
        standings_rows <- standings_rows |>
          dplyr::left_join(team_expected, by = "team")
      } else {
        standings_rows <- standings_rows |>
          dplyr::mutate(
            xg_for = NA_real_, xg_against = NA_real_, xpts = NA_real_
          )
      }

      standings_rows <- standings_rows |>
        dplyr::select(
          "team", "short", "played", "wins", "draws", "losses",
          "goals_for", "goals_against", "goal_diff", "points",
          "xg_for", "xg_against", "xpts",
          "rank", "form", "xg_trend"
        )

      jsonlite::write_json(
        list(
          generated_at = generated_at,
          season       = current_season,
          as_of        = format(max(bd_results$match_date), "%Y-%m-%d"),
          rows         = standings_rows
        ),
        file.path(out_dir, "standings.json"),
        auto_unbox = TRUE, dataframe = "rows", digits = 5, na = "null"
      )
    } else {
      jsonlite::write_json(
        list(
          generated_at = generated_at, season = current_season,
          as_of = format(end_date, "%Y-%m-%d"), rows = list()
        ),
        file.path(out_dir, "standings.json"),
        auto_unbox = TRUE, dataframe = "rows", digits = 5, na = "null"
      )
    }
  } else {
    # kvenna: write an empty standings file so the file always exists
    jsonlite::write_json(
      list(
        generated_at = generated_at, season = current_season,
        as_of = format(end_date, "%Y-%m-%d"), rows = list()
      ),
      file.path(out_dir, "standings.json"),
      auto_unbox = TRUE, dataframe = "rows", digits = 5, na = "null"
    )
  }

  # ---- team_strengths.json -------------------------------------------------

  team_strengths <- dplyr::bind_rows(
    .extract_team_draws_pfi(fit, "cur_offense_home", teams, "offence", "home"),
    .extract_team_draws_pfi(fit, "cur_defense_home", teams, "defence", "home"),
    .extract_team_draws_pfi(fit, "cur_strength_home", teams, "total", "home"),
    .extract_team_draws_pfi(fit, "cur_offense_away", teams, "offence", "away"),
    .extract_team_draws_pfi(fit, "cur_defense_away", teams, "defence", "away"),
    .extract_team_draws_pfi(fit, "cur_strength_away", teams, "total", "away")
  ) |>
    .summarise_team_intervals_pfi() |>
    dplyr::semi_join(current_top_teams, by = "team")

  jsonlite::write_json(
    list(generated_at = generated_at, records = team_strengths),
    file.path(out_dir, "team_strengths.json"),
    auto_unbox = TRUE, dataframe = "rows", digits = 5
  )

  # ---- final_positions.json + points_distribution.json ---------------------

  if (nrow(posterior_goals) > 0L) {
    # Points already accumulated from played matches
    base_points <- results[
      results$season == current_season & results$division == top_div, ,
      drop = FALSE
    ] |>
      dplyr::mutate(
        result = dplyr::case_when(
          .data$home_score > .data$away_score ~ "home",
          .data$home_score < .data$away_score ~ "away",
          TRUE ~ "tie"
        )
      ) |>
      tidyr::pivot_longer(c("home_team", "away_team"), values_to = "team") |>
      dplyr::mutate(
        name = dplyr::if_else(.data$name == "home_team", "home", "away"),
        points = dplyr::case_when(
          .data$result == "tie" ~ 1L,
          .data$result == .data$name ~ 3L,
          TRUE ~ 0L
        )
      ) |>
      dplyr::summarise(base_points = sum(.data$points), .by = "team")

    # Per-draw points from remaining (predicted) top-division matches
    iter_team_points <- posterior_goals |>
      dplyr::filter(.data$division == top_div) |>
      dplyr::mutate(
        result = dplyr::case_when(
          .data$home_goals > .data$away_goals ~ "home",
          .data$home_goals < .data$away_goals ~ "away",
          TRUE ~ "tie"
        )
      ) |>
      tidyr::pivot_longer(c("home_team", "away_team"), values_to = "team") |>
      dplyr::mutate(
        name = dplyr::if_else(.data$name == "home_team", "home", "away"),
        points = dplyr::case_when(
          .data$result == "tie" ~ 1L,
          .data$result == .data$name ~ 3L,
          TRUE ~ 0L
        )
      ) |>
      dplyr::summarise(points = sum(.data$points), .by = c(".draw", "team")) |>
      dplyr::left_join(base_points, by = "team") |>
      dplyr::mutate(
        base_points = dplyr::coalesce(.data$base_points, 0L),
        points      = .data$points + .data$base_points
      )

    iter_positions <- iter_team_points |>
      dplyr::arrange(.data$.draw, dplyr::desc(.data$points)) |>
      dplyr::mutate(placement = dplyr::row_number(), .by = ".draw")

    n_teams_top <- iter_positions |>
      dplyr::distinct(.data$team) |>
      nrow()

    final_positions <- iter_positions |>
      dplyr::count(.data$team, .data$placement) |>
      tidyr::complete(
        team,
        placement = seq_len(n_teams_top),
        fill = list(n = 0)
      ) |>
      dplyr::mutate(
        probability = .data$n / sum(.data$n),
        .by = "team"
      ) |>
      dplyr::select("team", "placement", "probability") |>
      dplyr::arrange(.data$team, .data$placement)

    top_six <- iter_positions |>
      dplyr::summarise(
        p_top_six = mean(.data$placement <= 6L),
        p_winner = mean(.data$placement == 1L),
        p_relegation = mean(.data$placement >= n_teams_top - 1L),
        .by = "team"
      )

    jsonlite::write_json(
      list(
        generated_at = generated_at,
        season       = current_season,
        n_teams      = n_teams_top,
        records      = final_positions,
        summary      = top_six
      ),
      file.path(out_dir, "final_positions.json"),
      auto_unbox = TRUE, dataframe = "rows", digits = 5
    )

    points_distribution <- iter_team_points |>
      dplyr::count(.data$team, .data$points) |>
      dplyr::mutate(
        probability = .data$n / sum(.data$n),
        .by = "team"
      ) |>
      dplyr::select("team", "points", "probability") |>
      dplyr::arrange(.data$team, .data$points)

    points_summary <- iter_team_points |>
      dplyr::summarise(
        mean_points = mean(.data$points),
        median_points = stats::median(.data$points),
        lower_80 = stats::quantile(.data$points, 0.1),
        upper_80 = stats::quantile(.data$points, 0.9),
        .by = "team"
      ) |>
      dplyr::left_join(base_points, by = "team") |>
      dplyr::mutate(base_points = dplyr::coalesce(.data$base_points, 0L)) |>
      dplyr::left_join(top_six, by = "team")

    jsonlite::write_json(
      list(
        generated_at = generated_at,
        season       = current_season,
        records      = points_distribution,
        summary      = points_summary
      ),
      file.path(out_dir, "points_distribution.json"),
      auto_unbox = TRUE, dataframe = "rows", digits = 5
    )
  } else {
    # Empty outputs when fit dimensions don't match
    jsonlite::write_json(
      list(
        generated_at = generated_at, season = current_season,
        n_teams = 0L, records = list(), summary = list()
      ),
      file.path(out_dir, "final_positions.json"),
      auto_unbox = TRUE, dataframe = "rows", digits = 5
    )
    jsonlite::write_json(
      list(
        generated_at = generated_at, season = current_season,
        records = list(), summary = list()
      ),
      file.path(out_dir, "points_distribution.json"),
      auto_unbox = TRUE, dataframe = "rows", digits = 5
    )
  }

  # ---- home_advantage.json -------------------------------------------------

  extract_home_adv_pfi <- function(var, component, transform = identity) {
    fit$draws(var) |>
      posterior::as_draws_df() |>
      tibble::as_tibble() |>
      tidyr::pivot_longer(c(-".chain", -".draw", -".iteration")) |>
      dplyr::mutate(
        team_idx  = as.integer(readr::parse_number(.data$name)),
        team      = teams$team[.data$team_idx],
        component = component,
        value     = transform(.data$value)
      )
  }

  home_advantage <- dplyr::bind_rows(
    extract_home_adv_pfi("home_advantage_off", "offence"),
    extract_home_adv_pfi("home_advantage_def", "defence"),
    extract_home_adv_pfi("home_advantage_tot", "total", transform = function(x) x / 2)
  ) |>
    dplyr::mutate(multiplier = exp(.data$value)) |>
    dplyr::reframe(
      median = stats::median(.data$multiplier),
      coverage = c(0.5, 0.8, 0.95),
      lower = stats::quantile(.data$multiplier, 0.5 - .data$coverage / 2),
      upper = stats::quantile(.data$multiplier, 0.5 + .data$coverage / 2),
      .by = c("team", "component")
    ) |>
    dplyr::semi_join(top_teams_upcoming, by = "team")

  jsonlite::write_json(
    list(generated_at = generated_at, records = home_advantage),
    file.path(out_dir, "home_advantage.json"),
    auto_unbox = TRUE, dataframe = "rows", digits = 5
  )

  message(sprintf(
    "publish_football_iceland: wrote 7 JSONs to %s", out_dir
  ))
  invisible(NULL)
}
