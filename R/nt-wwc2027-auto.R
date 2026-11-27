# Automatic refresh of the 2027 Women's World Cup UEFA play-off forecast
# (round 2, legs on 1 and 5 December 2026) by .github/workflows/wwc2027-playoff.yml:
# results from UEFA's public match feed, the forecast from a compact copy of
# the pre-leg fit committed under data/wc/wwc2027/, so CI never refits. WHY
# no refit: the play-off forecast conditions on first-leg scores by
# reweighting draws whose data predate the legs (a refit that saw the leg
# would count it twice), and R-INLA's Linux build on a runner does not give
# the Mac's draws bit for bit, so a refit would also move every number.

.WWC2027_UEFA_COMPETITION <- 39L
.WWC2027_UEFA_ROUND <- "WEQ Play-offs Round 2"

#' Compact copy of a women's fit for the play-off forecast.
#'
#' Keeps only what [nt_match_rates()] reads, for the teams of the ties: the
#' draws of `theta` and `delta` for those teams, `mu`, `home_for`,
#' `home_against`, and `kappa`. Match rates from the copy are identical to
#' those from the full fit, so a forecast from it is too.
#'
#' @param saved The list saved by `scripts/nt/wwc2027_fit.R` (`fit`,
#'   `cutoff`, `last_match`, ...).
#' @param teams Teams to keep.
#' @return The same list with `fit` replaced by the compact fit.
#' @noRd
wwc2027_slim_fit <- function(saved, teams) {
  fit <- saved$fit
  teams <- unique(as.character(teams))
  miss <- setdiff(teams, fit$teams)
  if (length(miss) > 0L) cli::cli_abort("Team{?s} not in the fit: {.val {miss}}.")
  d <- fit$draws
  saved$fit <- list(
    draws = list(
      theta = d$theta[, teams, drop = FALSE], delta = d$delta[, teams, drop = FALSE],
      mu = d$mu, home_for = d$home_for, home_against = d$home_against
    ),
    teams = teams, kappa = fit$kappa, hyper = fit$hyper, fixed = fit$fixed,
    settings = fit$settings
  )
  saved$source <- basename(saved$source %||% NA_character_)
  saved
}

#' Read the play-off's matches from UEFA's match feed.
#'
#' @param from,to Dates bounding the kick-offs.
#' @return The feed's JSON parsed with `simplifyVector = FALSE`: a list of
#'   matches.
#' @noRd
wwc2027_fetch_uefa <- function(from = "2026-11-30", to = "2026-12-06") {
  req <- httr2::request("https://match.uefa.com/v5/matches") |>
    httr2::req_url_query(
      competitionId = .WWC2027_UEFA_COMPETITION, seasonYear = 2027L,
      fromDate = from, toDate = to, limit = 100L, order = "ASC", offset = 0L
    ) |>
    httr2::req_user_agent("sports-pipeline (+https://github.com/metill-is/sports)") |>
    httr2::req_retry(max_tries = 4L) |>
    httr2::req_timeout(60)
  httr2::resp_body_json(httr2::req_perform(req), simplifyVector = FALSE)
}

#' Finished play-off legs from UEFA's match feed.
#'
#' Keeps the round-2 matches only. The score of a leg is its 90-minute
#' (`regular`) score, the model's unit; a first leg has no extra time. The
#' winner of a tie is the feed's aggregate winner on the second leg, which
#' covers extra time and penalties.
#'
#' @param matches Parsed feed ([wwc2027_fetch_uefa()]).
#' @param round The feed's round name to keep.
#' @return Tibble `home`, `away`, `leg`, `status`, `goals_home`,
#'   `goals_away` (`NA` unless finished), `winner` (`NA` unless a finished
#'   second leg names one).
#' @noRd
wwc2027_uefa_legs <- function(matches, round = .WWC2027_UEFA_ROUND) {
  get <- function(x, ...) {
    for (k in c(...)) {
      if (is.null(x) || !is.list(x)) {
        return(NULL)
      }
      x <- x[[k]]
    }
    x
  }
  num <- function(x) if (is.null(x)) NA_integer_ else as.integer(x)
  chr <- function(x) if (is.null(x)) NA_character_ else as.character(x)
  rows <- lapply(matches, function(m) {
    if (!identical(chr(get(m, "round", "metaData", "name")), round)) {
      return(NULL)
    }
    status <- chr(m$status)
    done <- identical(status, "FINISHED")
    tibble::tibble(
      home = chr(get(m, "homeTeam", "internationalName")),
      away = chr(get(m, "awayTeam", "internationalName")),
      leg = num(get(m, "leg", "number")),
      status = status,
      goals_home = if (done) num(get(m, "score", "regular", "home")) else NA_integer_,
      goals_away = if (done) num(get(m, "score", "regular", "away")) else NA_integer_,
      winner = if (done) chr(get(m, "winner", "aggregate", "team", "internationalName")) else NA_character_
    )
  })
  out <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(out)) {
    out <- tibble::tibble(
      home = character(), away = character(), leg = integer(), status = character(),
      goals_home = integer(), goals_away = integer(), winner = character()
    )
  }
  out
}

#' Fill the play-off ties with finished legs.
#'
#' A finished first leg (`team_a` at home) sets `leg1_goals_a` and
#' `leg1_goals_b`; a finished second leg (`team_b` at home) with a winner
#' sets `winner`. A value the feed does not have is left as it is, so an
#' empty or failed feed never erases a result. Every round-2 match in the
#' feed must belong to a tie, the right way round, or this stops: a renamed
#' team must not silently drop a result.
#'
#' @param ties The ties table (`data/wc/structure/wwc2027_playoff_round2.csv`).
#' @param legs Output of [wwc2027_uefa_legs()].
#' @return `ties` with the results filled in.
#' @noRd
wwc2027_apply_results <- function(ties, legs) {
  key1 <- paste(ties$team_a, ties$team_b, sep = "|")
  key2 <- paste(ties$team_b, ties$team_a, sep = "|")
  for (i in seq_len(nrow(legs))) {
    l <- legs[i, ]
    key <- paste(l$home, l$away, sep = "|")
    j <- if (identical(l$leg, 1L)) match(key, key1) else if (identical(l$leg, 2L)) match(key, key2) else NA_integer_
    if (is.na(j)) {
      cli::cli_abort("UEFA match {.val {l$home}} v {.val {l$away}} (leg {l$leg}) is not a tie the right way round in the ties table.")
    }
    if (!identical(l$status, "FINISHED")) next
    if (l$leg == 1L) {
      if (is.na(l$goals_home) || is.na(l$goals_away)) {
        cli::cli_abort("Finished first leg {.val {l$home}} v {.val {l$away}} has no score.")
      }
      ties$leg1_goals_a[j] <- l$goals_home
      ties$leg1_goals_b[j] <- l$goals_away
    } else if (!is.na(l$winner)) {
      if (!l$winner %in% c(ties$team_a[j], ties$team_b[j])) {
        cli::cli_abort("Winner {.val {l$winner}} is not in the tie {.val {ties$team_a[j]}} v {.val {ties$team_b[j]}}.")
      }
      ties$winner[j] <- l$winner
    }
  }
  ties
}

#' Read the play-off ties table.
#' @noRd
wwc2027_read_ties <- function(path) {
  ties <- utils::read.csv(path,
    stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = c("", "NA")
  )
  ties$leg1_goals_a <- as.integer(ties$leg1_goals_a)
  ties$leg1_goals_b <- as.integer(ties$leg1_goals_b)
  ties$winner <- as.character(ties$winner)
  ties
}

#' Write the play-off ties table in its committed format (no quotes, empty
#' for missing, LF line ends).
#' @noRd
wwc2027_write_ties <- function(ties, path) {
  utils::write.csv(ties, path, row.names = FALSE, na = "", quote = FALSE, fileEncoding = "UTF-8")
}

#' Publish the play-off forecast from a saved fit and the ties table.
#'
#' The one code path of both `scripts/nt/wwc2027_publish_playoff.R` (by
#' hand) and `scripts/nt/wwc2027_auto_playoff.R` (CI), so the two write the
#' same files for the same fit and ties.
#'
#' @param saved The list saved by `scripts/nt/wwc2027_fit.R`, or its compact
#'   copy ([wwc2027_slim_fit()]).
#' @param ties The ties table.
#' @param root Data root.
#' @param generated_at Passed to [publish_wwc2027_playoff()].
#' @return Invisibly the output directory.
#' @noRd
wwc2027_publish_from_saved <- function(saved, ties, root = here::here("data"), generated_at = NULL) {
  fc <- nt_two_leg_forecast(saved$fit, ties)
  publish_wwc2027_playoff(fc,
    n_direct = 7L, state_as_of = saved$cutoff, data_through = saved$last_match,
    root = root, generated_at = generated_at
  )
}
