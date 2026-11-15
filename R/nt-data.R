#' Read a martj42-schema international results file.
#'
#' Works unchanged on the men's (`martj42/international_results`) and women's
#' (`martj42/womens-international-results`) files, which share one schema.
#' Scores are the result after extra time, shootouts excluded.
#'
#' @param path Path to a `results.csv`.
#' @return Tibble `match_date, home_team, away_team, home_score, away_score,
#'   tournament, country, neutral`, sorted by date.
#' @export
nt_read_results <- function(path) {
  raw <- readr::read_csv(
    path,
    col_types = readr::cols(
      date = readr::col_date(),
      home_team = readr::col_character(),
      away_team = readr::col_character(),
      home_score = readr::col_integer(),
      away_score = readr::col_integer(),
      tournament = readr::col_character(),
      city = readr::col_character(),
      country = readr::col_character(),
      neutral = readr::col_logical()
    ),
    na = c("", "NA")
  )
  out <- tibble::tibble(
    match_date = raw$date,
    home_team = raw$home_team,
    away_team = raw$away_team,
    home_score = raw$home_score,
    away_score = raw$away_score,
    tournament = raw$tournament,
    country = raw$country,
    neutral = raw$neutral
  )
  out[order(out$match_date), , drop = FALSE]
}

#' Evaluation tournaments for the national-team model.
#'
#' @return Tibble `key, tournament, start, knockout_start, end`.
#' @export
nt_tournaments <- function() {
  tibble::tibble(
    key = c("wc2022", "euro2024", "wc2026"),
    label = c("World Cup 2022", "Euro 2024", "World Cup 2026"),
    tournament = c("FIFA World Cup", "UEFA Euro", "FIFA World Cup"),
    start = as.Date(c("2022-11-20", "2024-06-14", "2026-06-11")),
    knockout_start = as.Date(c("2022-12-03", "2024-06-29", "2026-06-28")),
    end = as.Date(c("2022-12-18", "2024-07-14", "2026-07-19"))
  )
}

#' Knockout matches that went to extra time, with their 90-minute scores.
#'
#' The source file stores the score after extra time, so a knockout match that
#' was level after 90 minutes shows either its extra-time winner or, for a
#' shootout, the 120-minute draw. The model predicts 90 minutes, so scoring
#' needs the regulation score. Goal minutes are from the English Wikipedia
#' match boxes of each tournament's knockout pages (read 2026-11-15).
#'
#' @return Tibble `match_date, team_a, team_b, a_90, b_90, shootout`; the pair
#'   is unordered (match on both orientations).
#' @export
nt_extra_time_matches <- function() {
  et <- tibble::tribble(
    ~match_date, ~team_a, ~team_b, ~a_90, ~b_90, ~shootout,
    "2022-12-05", "Japan", "Croatia", 1L, 1L, TRUE,
    "2022-12-06", "Morocco", "Spain", 0L, 0L, TRUE,
    "2022-12-09", "Croatia", "Brazil", 0L, 0L, TRUE,
    "2022-12-09", "Netherlands", "Argentina", 2L, 2L, TRUE,
    "2022-12-18", "Argentina", "France", 2L, 2L, TRUE,
    "2024-06-30", "England", "Slovakia", 1L, 1L, FALSE,
    "2024-07-01", "Portugal", "Slovenia", 0L, 0L, TRUE,
    "2024-07-05", "Spain", "Germany", 1L, 1L, FALSE,
    "2024-07-05", "Portugal", "France", 0L, 0L, TRUE,
    "2024-07-06", "England", "Switzerland", 1L, 1L, TRUE,
    "2026-06-29", "Germany", "Paraguay", 1L, 1L, TRUE,
    "2026-06-29", "Netherlands", "Morocco", 1L, 1L, TRUE,
    "2026-07-01", "Belgium", "Senegal", 2L, 2L, FALSE,
    "2026-07-03", "Australia", "Egypt", 1L, 1L, TRUE,
    "2026-07-03", "Argentina", "Cape Verde", 1L, 1L, FALSE,
    "2026-07-07", "Switzerland", "Colombia", 0L, 0L, TRUE,
    "2026-07-11", "Norway", "England", 1L, 1L, FALSE,
    "2026-07-11", "Argentina", "Switzerland", 1L, 1L, FALSE,
    "2026-07-19", "Spain", "Argentina", 0L, 0L, FALSE
  )
  et$match_date <- as.Date(et$match_date)
  et
}

#' The matches of one evaluation tournament, with 90-minute scores.
#'
#' @param results Output of [nt_read_results()].
#' @param key Tournament key from [nt_tournaments()].
#' @return The tournament's rows plus `stage` (`"group"`/`"knockout"`),
#'   `home_90`, `away_90` (regulation score) and `extra_time`.
#' @export
nt_tournament_matches <- function(results, key) {
  spec <- nt_tournaments()
  spec <- spec[spec$key == key, , drop = FALSE]
  if (nrow(spec) != 1L) {
    cli::cli_abort("Unknown tournament key {.val {key}}.")
  }
  m <- results[
    results$tournament == spec$tournament &
      results$match_date >= spec$start & results$match_date <= spec$end, ,
    drop = FALSE
  ]
  m$key <- key
  m$stage <- ifelse(m$match_date >= spec$knockout_start, "knockout", "group")
  m$home_90 <- m$home_score
  m$away_90 <- m$away_score
  m$extra_time <- FALSE
  et <- nt_extra_time_matches()
  for (i in seq_len(nrow(et))) {
    fwd <- which(m$match_date == et$match_date[i] &
      m$home_team == et$team_a[i] & m$away_team == et$team_b[i])
    rev <- which(m$match_date == et$match_date[i] &
      m$home_team == et$team_b[i] & m$away_team == et$team_a[i])
    if (length(fwd) == 1L) {
      m$home_90[fwd] <- et$a_90[i]
      m$away_90[fwd] <- et$b_90[i]
      m$extra_time[fwd] <- TRUE
    } else if (length(rev) == 1L) {
      m$home_90[rev] <- et$b_90[i]
      m$away_90[rev] <- et$a_90[i]
      m$extra_time[rev] <- TRUE
    }
  }
  m
}

#' Teams that belong to the sanctioned international game.
#'
#' The source files also carry non-FIFA football (CONIFA, Island Games, regional
#' selections). A team is kept when it has played at least one qualifying match
#' of a sanctioned competition in the window; that admits every FIFA member and
#' the confederation-only members that enter continental qualifying.
#'
#' @param results Results already restricted to the training window.
#' @param pattern Regex a tournament name must match to count.
#' @param exclude Regex of tournament names that never count.
#' @return Character vector of team names.
#' @export
nt_core_teams <- function(results, pattern = "qualif",
                          exclude = "^CONIFA|Island Games") {
  ok <- grepl(pattern, results$tournament, ignore.case = TRUE) &
    !grepl(exclude, results$tournament)
  sort(unique(c(results$home_team[ok], results$away_team[ok])))
}

.nt_period_index <- function(date, origin, step_months) {
  d <- as.POSIXlt(date)
  o <- as.POSIXlt(origin)
  months <- 12L * (d$year - o$year) + (d$mon - o$mon)
  as.integer(months %/% step_months + 1L)
}

#' Prepare training data for the national-team model.
#'
#' Keeps played matches in `[cutoff - window_years, cutoff)` between core teams
#' (see [nt_core_teams()]) that each have at least `min_matches` such matches,
#' and attaches the model's indices: team numbers, a calendar time index in
#' steps of `step_months`, the venue indicator (1 when the listed home side
#' plays in its own country) and a friendly flag.
#'
#' @param results Output of [nt_read_results()].
#' @param cutoff Date; only matches strictly before it are used.
#' @param window_years Length of the training window in years.
#' @param min_matches Minimum in-window matches for a team to be kept.
#' @param step_months Length of one time step of the strength process, in
#'   months. National teams play in a handful of windows a year, so a quarterly
#'   grid loses little and keeps the latent field small.
#' @param core_pattern,core_exclude Passed to [nt_core_teams()].
#' @return List: `matches` (tibble), `teams` (character), `origin` (Date, first
#'   day of the first month), `n_periods` (the time index of `cutoff`),
#'   `step_months`, `cutoff`.
#' @export
nt_prepare <- function(results, cutoff, window_years = 12, min_matches = 5L,
                       step_months = 3L,
                       core_pattern = "qualif",
                       core_exclude = "^CONIFA|Island Games") {
  cutoff <- as.Date(cutoff)
  window_start <- cutoff - round(365.25 * window_years)
  d <- results[
    !is.na(results$home_score) & !is.na(results$away_score) &
      results$match_date >= window_start & results$match_date < cutoff, ,
    drop = FALSE
  ]
  core <- nt_core_teams(d, core_pattern, core_exclude)
  d <- d[d$home_team %in% core & d$away_team %in% core, , drop = FALSE]
  repeat {
    n <- table(c(d$home_team, d$away_team))
    keep <- names(n)[n >= min_matches]
    ok <- d$home_team %in% keep & d$away_team %in% keep
    if (all(ok)) break
    d <- d[ok, , drop = FALSE]
  }
  teams <- sort(unique(c(d$home_team, d$away_team)))
  origin <- as.Date(format(min(d$match_date), "%Y-%m-01"))
  matches <- tibble::tibble(
    match_date = d$match_date,
    home_team = d$home_team,
    away_team = d$away_team,
    home_score = d$home_score,
    away_score = d$away_score,
    tournament = d$tournament,
    home_nr = match(d$home_team, teams),
    away_nr = match(d$away_team, teams),
    period = .nt_period_index(d$match_date, origin, step_months),
    venue = as.integer(!d$neutral),
    friendly = d$tournament == "Friendly"
  )
  list(
    matches = matches,
    teams = teams,
    origin = origin,
    n_periods = .nt_period_index(cutoff, origin, step_months),
    step_months = step_months,
    cutoff = cutoff
  )
}
