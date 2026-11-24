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
#' needs the regulation score, and so does training (see the `ninety`
#' argument of [nt_prepare()]).
#'
#' Men: the knockout matches of World Cup 2022, Euro 2024 and World Cup 2026;
#' goal minutes from the English Wikipedia match boxes of each tournament's
#' knockout pages (read 2026-11-15). Women: the second legs of round 1 of the
#' UEFA play-offs for the 2027 Women's World Cup (October 2026). Of its 16
#' ties only three had scores that allowed extra time; Poland v Belgium went
#' to extra time and penalties (2-1 after 90 minutes, 3-2 after 120:
#' Flashscore's match report and uefa.com's round-2 preview, read
#' 2026-11-24), while Scotland v Czechia and Slovenia v Turkey were settled
#' in 90 minutes (Football Web Pages match listings, read 2026-11-24). In a
#' second leg the 90-minute score need not be level; the aggregate is.
#'
#' @param sex `"men"` or `"women"`.
#' @return Tibble `match_date, team_a, team_b, a_90, b_90, shootout`; the pair
#'   is unordered (match on both orientations).
#' @export
nt_extra_time_matches <- function(sex = c("men", "women")) {
  sex <- match.arg(sex)
  if (sex == "women") {
    et <- tibble::tribble(
      ~match_date, ~team_a, ~team_b, ~a_90, ~b_90, ~shootout,
      "2026-10-13", "Poland", "Belgium", 2L, 1L, TRUE
    )
    et$match_date <- as.Date(et$match_date)
    return(et)
  }
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
  hit <- .nt_ninety_rows(m, nt_extra_time_matches())
  m$home_90 <- m$home_score
  m$away_90 <- m$away_score
  m$home_90[hit$row] <- hit$home_90
  m$away_90[hit$row] <- hit$away_90
  m$extra_time <- seq_len(nrow(m)) %in% hit$row
  m
}

# Rows of `m` that each correction in `et` (an nt_extra_time_matches() table)
# names, in either listing order, with the 90-minute score in m's orientation.
# `matched` flags, per correction, whether a unique row was found.
.nt_ninety_rows <- function(m, et) {
  row <- home_90 <- away_90 <- integer()
  matched <- logical(nrow(et))
  for (i in seq_len(nrow(et))) {
    fwd <- which(m$match_date == et$match_date[i] &
      m$home_team == et$team_a[i] & m$away_team == et$team_b[i])
    rev <- which(m$match_date == et$match_date[i] &
      m$home_team == et$team_b[i] & m$away_team == et$team_a[i])
    if (length(fwd) == 1L) {
      row <- c(row, fwd)
      home_90 <- c(home_90, et$a_90[i])
      away_90 <- c(away_90, et$b_90[i])
      matched[i] <- TRUE
    } else if (length(rev) == 1L) {
      row <- c(row, rev)
      home_90 <- c(home_90, et$b_90[i])
      away_90 <- c(away_90, et$a_90[i])
      matched[i] <- TRUE
    }
  }
  list(row = row, home_90 = as.integer(home_90), away_90 = as.integer(away_90), matched = matched)
}

# Qualifiers of any sanctioned competition, plus the finals of the World Cup,
# the continental championships and the Olympic tournament. The finals matter:
# some confederations qualify through a finals tournament (CONMEBOL women reach
# the World Cup through the Copa America, Oceania's men through the Nations Cup
# in some cycles), so their teams never play a match named "qualification".
.nt_core_pattern <- paste(
  "qualif", "^FIFA World Cup$", "^Copa Am", "^UEFA Euro$", "Cup of Nations$",
  "^AFC Asian Cup$", "^Gold Cup$", "^CONCACAF Championship$", "Nations Cup$",
  "^Olympic Games$",
  sep = "|"
)

#' Teams that belong to the sanctioned international game.
#'
#' The source files also carry non-FIFA football (CONIFA, Island Games, regional
#' selections). A team is kept when it has played, in the window, at least one
#' qualifier of a sanctioned competition or one match at the finals of the
#' World Cup, a continental championship or the Olympic tournament. That admits
#' every FIFA member and the confederation-only members that enter continental
#' competitions.
#'
#' @param results Results already restricted to the training window.
#' @param pattern Regex a tournament name must match to count.
#' @param exclude Regex of tournament names that never count.
#' @return Character vector of team names.
#' @export
nt_core_teams <- function(results, pattern = .nt_core_pattern,
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
#' @param ninety Optional table of 90-minute scores in the layout of
#'   [nt_extra_time_matches()]. The source files store scores after extra
#'   time; the model is of 90 minutes, so each listed match is trained on its
#'   90-minute score. A listed match inside the window that is not in
#'   `results` raises a warning (usually a misspelt team). `NULL`, the
#'   default, trains on the scores as stored, as every fit before 2026-11-24
#'   did.
#' @return List: `matches` (tibble), `teams` (character), `origin` (Date, first
#'   day of the first month), `n_periods` (the time index of `cutoff`),
#'   `step_months`, `cutoff`, `n_corrected` (matches trained on a 90-minute
#'   score from `ninety`).
#' @export
nt_prepare <- function(results, cutoff, window_years = 12, min_matches = 5L,
                       step_months = 3L,
                       core_pattern = .nt_core_pattern,
                       core_exclude = "^CONIFA|Island Games",
                       ninety = NULL) {
  cutoff <- as.Date(cutoff)
  window_start <- cutoff - round(365.25 * window_years)
  d <- results[
    !is.na(results$home_score) & !is.na(results$away_score) &
      results$match_date >= window_start & results$match_date < cutoff, ,
    drop = FALSE
  ]
  corrected <- logical(nrow(d))
  if (!is.null(ninety)) {
    hit <- .nt_ninety_rows(d, ninety)
    # WHY: extra time only adds goals, so a 90-minute count above the stored
    # one is a mistyped or swapped entry.
    above <- hit$home_90 > d$home_score[hit$row] | hit$away_90 > d$away_score[hit$row]
    if (any(above)) {
      cli::cli_warn(c(
        "{sum(above)} 90-minute score{?s} above the stored score: check the order of {.arg ninety}'s teams.",
        i = "{paste(d$match_date[hit$row][above], d$home_team[hit$row][above], 'v', d$away_team[hit$row][above])}"
      ))
    }
    d$home_score[hit$row] <- hit$home_90
    d$away_score[hit$row] <- hit$away_90
    corrected[hit$row] <- TRUE
    inside <- ninety$match_date >= window_start & ninety$match_date < cutoff
    missing <- inside & !hit$matched
    if (any(missing)) {
      cli::cli_warn(c(
        "{sum(missing)} 90-minute correction{?s} in the window match no played match.",
        i = "{paste(ninety$match_date[missing], ninety$team_a[missing], 'v', ninety$team_b[missing])}"
      ))
    }
  }
  core <- nt_core_teams(d, core_pattern, core_exclude)
  in_core <- d$home_team %in% core & d$away_team %in% core
  d <- d[in_core, , drop = FALSE]
  corrected <- corrected[in_core]
  repeat {
    n <- table(c(d$home_team, d$away_team))
    keep <- names(n)[n >= min_matches]
    ok <- d$home_team %in% keep & d$away_team %in% keep
    if (all(ok)) break
    d <- d[ok, , drop = FALSE]
    corrected <- corrected[ok]
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
    cutoff = cutoff,
    n_corrected = sum(corrected)
  )
}
