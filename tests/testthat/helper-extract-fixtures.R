# Materialise the committed extracts fixture into a temp tree so tests that
# publish from it can write alongside without dirtying the repo.

# Copy the committed 2DT extracts partitions into a temp extracts root.
fixture_extracts_root <- function(sports = c("basketball", "handball"),
                                  env = parent.frame()) {
  tmp <- file.path(withr::local_tempdir(.local_envir = env), "extracts")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  src <- testthat::test_path("fixtures", "extracts")
  for (sport in sports) {
    from <- file.path(src, paste0("sport=", sport))
    if (dir.exists(from)) {
      file.copy(from, tmp, recursive = TRUE)
    }
  }
  tmp
}

# Materialise a football extracts partition from the committed facts fixture.
#
# Football's 99-quantile bands over ~34 teams (BD 12 + LD1/LD2/LD3 6 + CUP 4,
# male) do not fit the committed-fixture budget -- team_strengths_quantiles
# alone is ~240 KB, and round_strengths_quantiles is larger again -- so the
# partition is rebuilt as a deterministic pure function of the facts fixture
# instead of stored. No RNG anywhere below.
build_football_extracts_fixture <- function(facts_root, extracts_root, sex,
                                            fit_date = FIXTURE_FIT_DATE) {
  divs <- .iceland_division_codes("football_iceland", sex)
  part <- file.path(
    extracts_root, "sport=football", "country=iceland",
    paste0("sex=", sex), paste0("fit_date=", format(fit_date, "%Y-%m-%d"))
  )
  dir.create(part, recursive = TRUE, showWarnings = FALSE)

  schedules <- read_table(
    "schedules", root = facts_root,
    filter = list(sport = "football", country = "iceland", sex = sex)
  )

  per_div <- lapply(divs, function(div) {
    teams <- fixture_division_teams("football", sex, div)
    centre <- stats::setNames(seq(1.2, -1.2, length.out = length(teams)), teams)
    grid <- tidyr::expand_grid(
      team = teams,
      component = c("offence", "defence", "total"),
      location = c("home", "away", "avg")
    )
    ts <- grid |>
      tidyr::expand_grid(quantile = seq_len(99L)) |>
      dplyr::mutate(
        value = round(
          centre[.data$team] + 0.4 * stats::qnorm(.data$quantile / 100),
          4L
        ),
        division = div
      )
    rs <- ts |>
      tidyr::expand_grid(round = 1:2) |>
      dplyr::mutate(value = round(.data$value + 0.05 * .data$round, 4L)) |>
      dplyr::select("round", "team", "component", "location", "quantile", "value", "division")
    ha <- tidyr::expand_grid(
      team = teams, component = c("offence", "defence", "total"),
      quantile = seq_len(99L)
    ) |>
      dplyr::mutate(
        value = round(0.15 + 0.05 * stats::qnorm(.data$quantile / 100), 4L),
        division = div
      )
    fp <- tidyr::expand_grid(team = teams, placement = seq_along(teams)) |>
      dplyr::mutate(probability = 1 / length(teams), division = div)
    pd <- tidyr::expand_grid(team = teams, points = seq.int(10L, 14L)) |>
      dplyr::mutate(probability = 0.2, division = div)

    sched <- schedules[schedules$division == div, , drop = FALSE]
    pm <- tidyr::expand_grid(
      idx = seq_len(nrow(sched)), home_goals = 0:3, away_goals = 0:3
    ) |>
      dplyr::mutate(
        home_team = sched$home_team[.data$idx],
        away_team = sched$away_team[.data$idx],
        match_date = sched$match_date[.data$idx],
        home_goals = as.integer(.data$home_goals),
        away_goals = as.integer(.data$away_goals),
        count = as.integer(FIXTURE_N_DRAWS / 16L),
        division = div
      ) |>
      dplyr::select("home_team", "away_team", "match_date", "home_goals",
                    "away_goals", "count", "division")
    tp <- tibble::tibble(
      team = teams, round_name = "winner",
      probability = 1 / length(teams), division = div
    )
    list(
      predicted_matches = pm, team_strengths_quantiles = ts,
      round_strengths_quantiles = rs, home_advantage_quantiles = ha,
      final_positions = fp, points_distribution = pd,
      tournament_placements = tp
    )
  })

  for (ft in names(per_div[[1]])) {
    arrow::write_parquet(
      dplyr::bind_rows(lapply(per_div, function(d) d[[ft]])),
      file.path(part, paste0(ft, ".parquet"))
    )
  }
  invisible(NULL)
}

# Recursively drop every `generated_at` key, then hash the canonical
# serialisation -- "byte-identical modulo generated_at".
.strip_generated_at <- function(x) {
  if (!is.list(x)) return(x)
  x[names(x) == "generated_at"] <- NULL
  lapply(x, .strip_generated_at)
}

publish_json_digest <- function(path) {
  payload <- .strip_generated_at(
    jsonlite::read_json(path, simplifyVector = FALSE)
  )
  digest::digest(
    jsonlite::toJSON(payload, auto_unbox = TRUE, digits = NA, null = "null"),
    algo = "sha256"
  )
}
