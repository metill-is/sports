#' @include model-prepare.R storage.R config.R publish-football-iceland.R
NULL

# ---- Internal helpers --------------------------------------------------------

# Summarise per-draw values into 99-quantile bands per group.
# Input: tibble with `value` column + grouping columns.
# Output: tibble with same grouping columns + `quantile` (int 1..99) + `value`.
# Uses group_modify rather than reframe to dodge dplyr's per-row recycle
# heuristics on multi-column same-length expressions inside .by.
.summarise_quantile_band_pfi <- function(draws, group_keys) {
  probs <- seq(0.01, 0.99, by = 0.01)
  draws |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_keys))) |>
    dplyr::group_modify(~ tibble::tibble(
      quantile = seq_len(99L),
      value = unname(stats::quantile(
        .x$value,
        probs = probs,
        names = FALSE
      ))
    )) |>
    dplyr::ungroup()
}

# Canonical division codes the football iceland fit covers.
# `BD` = Besta deild (top tier), `LD1` = Lengjudeild (second tier).
# Order matches publish_football_iceland()'s loop and read_extracted_football()'s
# return list ordering.
.FOOTBALL_ICELAND_DIVISIONS_PFI <- c("BD", "LD1")

# Per-division extraction. Writes the 6 parquets into
# `<archive_dir_base>/division=<target_div>/`. The cross-division inputs
# (`posterior_goals_long`, `team_strengths_draws`, `home_advantage_draws`,
# `results`, `teams`) are computed once by the caller and passed in here.
.extract_division_parquets_pfi <- function(target_div,
                                           archive_dir_base,
                                           fit,
                                           teams,
                                           results,
                                           current_season,
                                           posterior_goals_long,
                                           team_strengths_draws,
                                           home_advantage_draws,
                                           n_pred_fit,
                                           n_pred_data) {
  archive_dir <- file.path(
    archive_dir_base, paste0("division=", target_div)
  )
  dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)

  top_results <- results[
    results$season == current_season & results$division == target_div, ,
    drop = FALSE
  ]
  current_top_teams <- if (nrow(top_results) > 0L) {
    top_results |>
      dplyr::select("home_team", "away_team") |>
      tidyr::pivot_longer(c("home_team", "away_team"), values_to = "team") |>
      dplyr::distinct(.data$team)
  } else {
    tibble::tibble(team = character())
  }

  # ---- 1. predicted_matches.parquet (count of integer score pairs) ---------

  predicted_matches <- if (n_pred_fit != n_pred_data || n_pred_data == 0L) {
    tibble::tibble(
      home_team = character(), away_team = character(),
      match_date = as.Date(character()),
      home_goals = integer(), away_goals = integer(),
      count = integer()
    )
  } else {
    posterior_goals_long |>
      dplyr::filter(.data$division == target_div) |>
      dplyr::mutate(
        home_goals = as.integer(round(.data$home_goals)),
        away_goals = as.integer(round(.data$away_goals))
      ) |>
      dplyr::count(
        .data$home_team, .data$away_team, .data$match_date,
        .data$home_goals, .data$away_goals,
        name = "count"
      ) |>
      dplyr::mutate(count = as.integer(.data$count)) |>
      dplyr::arrange(
        .data$match_date, .data$home_team, .data$away_team,
        .data$home_goals, .data$away_goals
      )
  }

  arrow::write_parquet(
    predicted_matches,
    file.path(archive_dir, "predicted_matches.parquet")
  )

  # ---- 2. team_strengths_quantiles.parquet --------------------------------
  # 9-cell grid: component ∈ {offence, defence, total} × location ∈ {home, away, avg}.
  # `avg` is per-draw mean of home/away pre-quantile so uncertainty intervals
  # reflect the joint posterior, not a post-hoc point average.

  team_strengths_avg <- team_strengths_draws |>
    dplyr::summarise(
      value = mean(.data$value),
      .by = c(".draw", "team", "component")
    ) |>
    dplyr::mutate(location = "avg")

  team_strengths_quantiles <- dplyr::bind_rows(
    team_strengths_draws,
    team_strengths_avg
  ) |>
    dplyr::semi_join(current_top_teams, by = "team") |>
    .summarise_quantile_band_pfi(c("team", "component", "location"))

  arrow::write_parquet(
    team_strengths_quantiles,
    file.path(archive_dir, "team_strengths_quantiles.parquet")
  )

  # ---- 3. round_strengths_quantiles.parquet -------------------------------
  # Per (round, team) trajectory, scoped to the division's chronological
  # matchweeks. The fit holds offense/defense matrices indexed by the
  # team's global round number; the helper maps that to division-specific
  # matchweeks. For LD this gives an LD-only round trajectory.

  trajectory_long <- .compute_team_strength_trajectory_pfi(
    fit = fit,
    results = results,
    teams = teams,
    current_top_teams = current_top_teams,
    current_season = current_season,
    top_div = target_div
  )

  round_strengths_quantiles <- if (nrow(trajectory_long) > 0L) {
    avg_long <- trajectory_long |>
      dplyr::summarise(
        value = mean(.data$value),
        .by = c(".draw", "round", "team", "component")
      ) |>
      dplyr::mutate(location = "avg")

    dplyr::bind_rows(trajectory_long, avg_long) |>
      .summarise_quantile_band_pfi(c("round", "team", "component", "location"))
  } else {
    tibble::tibble(
      round = integer(), team = character(),
      component = character(), location = character(),
      quantile = integer(), value = numeric()
    )
  }

  arrow::write_parquet(
    round_strengths_quantiles,
    file.path(archive_dir, "round_strengths_quantiles.parquet")
  )

  # ---- 4. home_advantage_quantiles.parquet --------------------------------
  # Multiplicative form: exp(home_advantage_*). `total` halved per the
  # publisher's per-side allocation convention. Cross-division draws are
  # filtered to the division's teams here.

  home_advantage_quantiles <- home_advantage_draws |>
    dplyr::semi_join(current_top_teams, by = "team") |>
    .summarise_quantile_band_pfi(c("team", "component"))

  arrow::write_parquet(
    home_advantage_quantiles,
    file.path(archive_dir, "home_advantage_quantiles.parquet")
  )

  # ---- 5 + 6. final_positions.parquet + points_distribution.parquet -------
  # Pre-computed at extract time (when full draws are still in memory).
  # Logic mirrors the publisher's existing simulation block, scoped to
  # the target division's teams + matches.

  base_points <- if (nrow(top_results) > 0L) {
    top_results |>
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
  } else {
    tibble::tibble(team = character(), base_points = integer())
  }

  posterior_goals <- if (n_pred_fit == n_pred_data && n_pred_data > 0L) {
    posterior_goals_long
  } else {
    tibble::tibble(
      .draw = integer(), game_nr = integer(),
      home_goals = numeric(), away_goals = numeric(),
      match_date = as.Date(character()),
      home_team = character(), away_team = character(),
      division = character()
    )
  }

  if (nrow(posterior_goals) > 0L && nrow(current_top_teams) > 0L) {
    iter_team_points <- posterior_goals |>
      dplyr::filter(.data$division == target_div) |>
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
  } else {
    iter_team_points <- tibble::tibble(
      .draw = integer(), team = character(),
      points = integer(), base_points = integer()
    )
  }

  if (nrow(iter_team_points) > 0L) {
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

    points_distribution <- iter_team_points |>
      dplyr::count(.data$team, .data$points) |>
      dplyr::mutate(
        probability = .data$n / sum(.data$n),
        .by = "team"
      ) |>
      dplyr::select("team", "points", "probability") |>
      dplyr::arrange(.data$team, .data$points)
  } else {
    final_positions <- tibble::tibble(
      team = character(),
      placement = integer(),
      probability = numeric()
    )
    points_distribution <- tibble::tibble(
      team = character(),
      points = integer(),
      probability = numeric()
    )
  }

  arrow::write_parquet(
    final_positions,
    file.path(archive_dir, "final_positions.parquet")
  )
  arrow::write_parquet(
    points_distribution,
    file.path(archive_dir, "points_distribution.parquet")
  )

  invisible(archive_dir)
}

# ---- Public API --------------------------------------------------------------

#' Extract publish-layer summaries from a football iceland fit
#'
#' Writes six Parquet files **per division** into the per-fit archive
#' partition at
#' `data/beliefs/archive/sport=football/country=iceland/sex={male|female}/fit_date=YYYY-MM-DD/division={BD|LD1}/`.
#' The fit covers both Icelandic football divisions (Besta deild + Lengjudeild)
#' so each call writes 12 parquets total. The publisher consumes the per-division
#' partitions independently to produce the four `(sex × division)` cells on disk.
#'
#' Per-division parquets:
#'
#' - `predicted_matches.parquet` — per-(home_team, away_team, match_date,
#'   home_goals, away_goals) row with the integer-pair occurrence count
#'   across the posterior. Filtered to the division's matches.
#' - `team_strengths_quantiles.parquet` — 99-quantile band per
#'   (team, component, location) where component ∈ {offence, defence, total}
#'   and location ∈ {home, away, avg}. The `avg` row is the per-draw mean of
#'   home/away (computed pre-quantile so intervals reflect the joint posterior).
#'   Filtered to the division's teams.
#' - `round_strengths_quantiles.parquet` — same 9-cell grid but per
#'   (round, team), where `round` is the team's chronological matchweek
#'   *within the division*. Drives the strength trajectory.
#' - `home_advantage_quantiles.parquet` — 99-quantile band per (team, component)
#'   for the multiplicative home-advantage parameter `exp(home_advantage_*)`.
#'   The `total` component is `exp(home_advantage_tot / 2)` matching the
#'   publisher's per-side allocation. Filtered to the division's teams.
#' - `final_positions.parquet` — per-(team, placement) probability over the
#'   posterior, pre-computed at extract time because the count representation
#'   above doesn't preserve the cross-match draw alignment that the simulation
#'   requires.
#' - `points_distribution.parquet` — per-(team, points) probability, same
#'   reasoning.
#'
#' Together these six Parquets capture everything the football publisher
#' currently reads from the in-memory fit RDS, at roughly 500 KB per fit per
#' division (vs. 100–500 MB RDS). See
#' `Sports/Knowledge/Publish Pipeline/extraction-layer` in the Metill Obsidian
#' vault for the design rationale.
#'
#' @param fit CmdStanMCMC fit object.
#' @param league League list with `sport == "football"` and `country == "iceland"`.
#' @param sex `"male"` or `"female"`.
#' @param fit_date Date stamped on the archive partition. Default `Sys.Date()`.
#' @param end_date Training cutoff used for prepare_data() reconstruction.
#'   Default = `fit_date`.
#' @param root Data root. Default `here::here("data")`.
#' @param prep Optional pre-computed `prepare_data()` output. When NULL
#'   (default), reconstructs from disk; pass directly when calling from
#'   `fit_league()` to avoid a redundant prepare_data call (and to
#'   guarantee consistency with the fit).
#' @param archive_root Optional write root for the per-fit archive partition.
#'   Defaults to `file.path(root, "beliefs", "archive")`. Tests can override
#'   this to write into an isolated tempdir while still reading facts from
#'   the real `root`.
#' @param target_divs Character vector of division codes to extract. Defaults
#'   to `c("BD", "LD1")` (both). Useful in tests to extract one division only.
#' @return invisible(NULL). 6 × length(target_divs) Parquet files written into
#'   the archive partition.
#' @export
extract_football_iceland <- function(fit, league, sex,
                                     fit_date = Sys.Date(),
                                     end_date = fit_date,
                                     root = here::here("data"),
                                     prep = NULL,
                                     archive_root = NULL,
                                     target_divs = .FOOTBALL_ICELAND_DIVISIONS_PFI) {
  stopifnot(league$sport == "football", league$country == "iceland")
  stopifnot(sex %in% c("male", "female"))
  stopifnot(inherits(fit_date, "Date") || is.character(fit_date))
  stopifnot(
    is.character(target_divs),
    length(target_divs) >= 1L,
    all(target_divs %in% .FOOTBALL_ICELAND_DIVISIONS_PFI)
  )

  if (is.null(prep)) {
    prep <- prepare_data(league, sex, end_date = end_date, root = root)
  }
  teams <- prep$teams
  pred_d <- prep$pred_d

  if (is.null(archive_root)) {
    archive_root <- file.path(root, "beliefs", "archive")
  }
  archive_dir_base <- file.path(
    archive_root,
    paste0("sport=", league$sport),
    paste0("country=", league$country),
    paste0("sex=", sex),
    paste0("fit_date=", format(as.Date(fit_date), "%Y-%m-%d"))
  )
  dir.create(archive_dir_base, recursive = TRUE, showWarnings = FALSE)

  results <- read_table(
    "results",
    root   = root,
    filter = list(sport = league$sport, country = league$country, sex = sex)
  )
  results <- results[
    !is.na(results$match_date) & results$match_date <= end_date, ,
    drop = FALSE
  ]
  current_season <- if (nrow(results) > 0L) {
    max(results$season, na.rm = TRUE)
  } else {
    as.integer(format(as.Date(end_date), "%Y"))
  }

  # ---- Cross-division: posterior_goals long + team_strengths_draws + home_adv

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
        stringr::str_detect(.data$parameter, "goals1"),
        "home_goals", "away_goals"
      ),
      game_nr = as.integer(
        stringr::str_match(.data$parameter, "\\[(\\d+)\\]$")[, 2]
      )
    ) |>
    dplyr::select(".draw", "type", "game_nr", "value") |>
    tidyr::pivot_wider(names_from = "type", values_from = "value")

  n_pred_fit <- if (nrow(posterior_goals_raw) > 0L) {
    max(posterior_goals_raw$game_nr, na.rm = TRUE)
  } else {
    0L
  }
  n_pred_data <- nrow(pred_d)

  if (n_pred_fit != n_pred_data && n_pred_data > 0L) {
    warning(sprintf(
      paste0(
        "extract_football_iceland: fit was trained with N_pred=%d ",
        "prediction matches but prepare_data returned %d. ",
        "predicted_matches.parquet will be empty for every division."
      ),
      n_pred_fit, n_pred_data
    ))
  }

  posterior_goals_long <- if (n_pred_fit == n_pred_data && n_pred_data > 0L) {
    posterior_goals_raw |>
      dplyr::inner_join(
        pred_d[, c("game_nr", "match_date", "home_team", "away_team", "division")],
        by = "game_nr"
      )
  } else {
    tibble::tibble(
      .draw = integer(), game_nr = integer(),
      home_goals = numeric(), away_goals = numeric(),
      match_date = as.Date(character()),
      home_team = character(), away_team = character(),
      division = character()
    )
  }

  team_strengths_draws <- dplyr::bind_rows(
    .extract_team_draws_pfi(fit, "cur_offense_home", teams, "offence", "home"),
    .extract_team_draws_pfi(fit, "cur_defense_home", teams, "defence", "home"),
    .extract_team_draws_pfi(fit, "cur_strength_home", teams, "total", "home"),
    .extract_team_draws_pfi(fit, "cur_offense_away", teams, "offence", "away"),
    .extract_team_draws_pfi(fit, "cur_defense_away", teams, "defence", "away"),
    .extract_team_draws_pfi(fit, "cur_strength_away", teams, "total", "away")
  )

  extract_home_adv <- function(var, component, transform = identity) {
    fit$draws(var) |>
      posterior::as_draws_df() |>
      tibble::as_tibble() |>
      tidyr::pivot_longer(c(-".chain", -".draw", -".iteration")) |>
      dplyr::mutate(
        team_idx  = as.integer(readr::parse_number(.data$name)),
        team      = teams$team[.data$team_idx],
        component = component,
        value     = exp(transform(.data$value))
      ) |>
      dplyr::select("team", "component", ".draw", "value")
  }

  home_advantage_draws <- dplyr::bind_rows(
    extract_home_adv("home_advantage_off", "offence"),
    extract_home_adv("home_advantage_def", "defence"),
    extract_home_adv("home_advantage_tot", "total",
      transform = function(x) x / 2
    )
  )

  for (target_div in target_divs) {
    .extract_division_parquets_pfi(
      target_div = target_div,
      archive_dir_base = archive_dir_base,
      fit = fit,
      teams = teams,
      results = results,
      current_season = current_season,
      posterior_goals_long = posterior_goals_long,
      team_strengths_draws = team_strengths_draws,
      home_advantage_draws = home_advantage_draws,
      n_pred_fit = n_pred_fit,
      n_pred_data = n_pred_data
    )
  }

  # cli::cli_alert_success treats `{...}` as expression interpolation, so
  # use cli::cli_alert_info with a literal string assembled via sprintf — or
  # avoid braces entirely.
  message(sprintf(
    "extract_football_iceland: wrote %d Parquets to %s [div: %s]",
    6L * length(target_divs),
    archive_dir_base,
    paste(target_divs, collapse = ", ")
  ))
  invisible(NULL)
}

#' Load a per-fit football iceland extraction archive partition
#'
#' Reads the six Parquet files written by [`extract_football_iceland()`] from
#' `data/beliefs/archive/sport=football/country=iceland/sex=Z/fit_date=D/division=Y/`,
#' for one or more divisions, into a named list keyed by division code.
#'
#' Auto-discovery (default `fit_date = NULL`) walks the `fit_date=*`
#' partitions in descending order and returns the first one whose
#' `division=BD/` sub-partition contains all six expected files.
#' (BD is required because the platform always renders the BD page;
#' an LD1 sub-partition is optional and degrades to an empty list of
#' tibbles when missing — pre-2026-05-04 archives never wrote LD1.)
#'
#' Legacy partitions written before this refactor — six parquets directly
#' under `fit_date=D/`, without the `division=Y/` sub-partition — are
#' transparently lifted into BD and treated as empty for LD1.
#'
#' @param league League list with `sport == "football"` and
#'   `country == "iceland"`.
#' @param sex `"male"` or `"female"`.
#' @param fit_date `Date` or `NULL`. When `NULL` (default), reads the latest
#'   partition that contains the full extracted set for BD.
#' @param archive_root Beliefs archive root.
#'   Default `here::here("data", "beliefs", "archive")`.
#' @param target_divs Character vector of divisions to load. Defaults to
#'   `c("BD", "LD1")`. Returned list always includes a slot per requested
#'   division (with empty-tibble parquets when the division partition is
#'   missing or LD1 in a legacy archive).
#' @return Named list. Each requested division key (e.g. `"BD"`, `"LD1"`)
#'   maps to a list with the six tibbles
#'   (`predicted_matches`, `team_strengths_quantiles`,
#'   `round_strengths_quantiles`, `home_advantage_quantiles`,
#'   `final_positions`, `points_distribution`). Plus `fit_date` (the `Date`
#'   of the partition that was loaded).
#' @export
read_extracted_football <- function(league, sex, fit_date = NULL,
                                    archive_root = here::here(
                                      "data", "beliefs", "archive"
                                    ),
                                    target_divs = .FOOTBALL_ICELAND_DIVISIONS_PFI) {
  stopifnot(league$sport == "football", league$country == "iceland")
  stopifnot(sex %in% c("male", "female"))
  stopifnot(
    is.character(target_divs),
    length(target_divs) >= 1L,
    all(target_divs %in% .FOOTBALL_ICELAND_DIVISIONS_PFI)
  )

  expected <- c(
    "predicted_matches.parquet",
    "team_strengths_quantiles.parquet",
    "round_strengths_quantiles.parquet",
    "home_advantage_quantiles.parquet",
    "final_positions.parquet",
    "points_distribution.parquet"
  )

  base <- file.path(
    archive_root,
    paste0("sport=", league$sport),
    paste0("country=", league$country),
    paste0("sex=", sex)
  )
  if (!dir.exists(base)) {
    stop("No archive directory at ", base, call. = FALSE)
  }

  # A fit_date partition is "complete" if either the per-division layout
  # has all 6 BD parquets, or the legacy layout has all 6 directly under
  # fit_date=D/. The legacy layout is treated as BD-only.
  .partition_is_complete <- function(fit_dir) {
    bd_dir <- file.path(fit_dir, "division=BD")
    if (all(file.exists(file.path(bd_dir, expected)))) {
      return(TRUE)
    }
    # Legacy: extract pre-refactor wrote 6 parquets directly under fit_dir.
    if (all(file.exists(file.path(fit_dir, expected)))) {
      return(TRUE)
    }
    FALSE
  }

  if (is.null(fit_date)) {
    parts <- list.dirs(base, full.names = TRUE, recursive = FALSE)
    fit_dirs <- parts[grepl("/fit_date=", parts)]
    if (length(fit_dirs) == 0L) {
      stop("No fit_date partitions under ", base, call. = FALSE)
    }
    fit_dates_chr <- sub(".*fit_date=", "", fit_dirs)
    ord <- order(as.Date(fit_dates_chr), decreasing = TRUE)
    fit_dir <- NULL
    for (i in ord) {
      d <- fit_dirs[i]
      if (.partition_is_complete(d)) {
        fit_dir <- d
        break
      }
    }
    if (is.null(fit_dir)) {
      stop(
        "No fit_date partition under ", base,
        " contains a complete extracted set ",
        "(legacy partitions with only part-0.parquet are skipped). ",
        "Force-trigger fit.yml or run extract_football_iceland() locally.",
        call. = FALSE
      )
    }
    fit_date_out <- as.Date(sub(".*fit_date=", "", fit_dir))
  } else {
    fit_date_out <- as.Date(fit_date)
    fit_dir <- file.path(
      base, paste0("fit_date=", format(fit_date_out, "%Y-%m-%d"))
    )
    if (!dir.exists(fit_dir)) {
      stop("Archive partition not found: ", fit_dir, call. = FALSE)
    }
    if (!.partition_is_complete(fit_dir)) {
      stop(
        "Archive partition ", fit_dir,
        " is incomplete (BD parquets missing under either ",
        "division=BD/ or directly under the fit partition). ",
        "Re-run extract_football_iceland() against this fit.",
        call. = FALSE
      )
    }
  }

  # Read each requested division's parquets. For BD, fall back to the legacy
  # layout (parquets directly under fit_dir) when division=BD/ is missing.
  empty_tibbles <- list(
    predicted_matches = tibble::tibble(
      home_team = character(), away_team = character(),
      match_date = as.Date(character()),
      home_goals = integer(), away_goals = integer(),
      count = integer()
    ),
    team_strengths_quantiles = tibble::tibble(
      team = character(), component = character(), location = character(),
      quantile = integer(), value = numeric()
    ),
    round_strengths_quantiles = tibble::tibble(
      round = integer(), team = character(),
      component = character(), location = character(),
      quantile = integer(), value = numeric()
    ),
    home_advantage_quantiles = tibble::tibble(
      team = character(), component = character(),
      quantile = integer(), value = numeric()
    ),
    final_positions = tibble::tibble(
      team = character(), placement = integer(), probability = numeric()
    ),
    points_distribution = tibble::tibble(
      team = character(), points = integer(), probability = numeric()
    )
  )

  read_one_division <- function(target_div) {
    div_dir <- file.path(fit_dir, paste0("division=", target_div))
    if (!all(file.exists(file.path(div_dir, expected)))) {
      # Legacy archive: 6 parquets at fit_dir level → BD only
      if (
        target_div == "BD" &&
          all(file.exists(file.path(fit_dir, expected)))
      ) {
        div_dir <- fit_dir
      } else {
        return(empty_tibbles)
      }
    }
    list(
      predicted_matches = arrow::read_parquet(
        file.path(div_dir, "predicted_matches.parquet")
      ),
      team_strengths_quantiles = arrow::read_parquet(
        file.path(div_dir, "team_strengths_quantiles.parquet")
      ),
      round_strengths_quantiles = arrow::read_parquet(
        file.path(div_dir, "round_strengths_quantiles.parquet")
      ),
      home_advantage_quantiles = arrow::read_parquet(
        file.path(div_dir, "home_advantage_quantiles.parquet")
      ),
      final_positions = arrow::read_parquet(
        file.path(div_dir, "final_positions.parquet")
      ),
      points_distribution = arrow::read_parquet(
        file.path(div_dir, "points_distribution.parquet")
      )
    )
  }

  out <- lapply(target_divs, read_one_division)
  names(out) <- target_divs
  out$fit_date <- fit_date_out
  out
}
