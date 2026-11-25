# International handball (national teams): the match table, its seasons and
# venues. The model is the handball v2 (margin, total) engine of
# R/v2-margin-total-inla.R with nations as teams; see R/ih-model.R.
#
# The match table is not kept in this repository. It is read from a folder
# named by the option `ih.data_dir` or the environment variable
# `IH_DATA_DIR`, which must hold `handball_matches.parquet` (one row per
# national-team match, both sexes; columns listed in ih_read_matches()).

#' Folder holding the international handball match table
#' @return The folder from option `ih.data_dir`, else `IH_DATA_DIR`.
#' @noRd
ih_data_dir <- function() {
  d <- getOption("ih.data_dir", Sys.getenv("IH_DATA_DIR", ""))
  if (!nzchar(d)) {
    cli::cli_abort(
      "Set {.envvar IH_DATA_DIR} (or option {.code ih.data_dir}) to the folder holding {.file handball_matches.parquet}."
    )
  }
  d
}

#' Season of a national-team match
#'
#' A season runs from July to June, so a December championship and the
#' January one after it share a season with the autumn's qualifiers.
#' @param date Dates.
#' @return Integer season label: the year the season starts.
#' @noRd
ih_season <- function(date) {
  date <- as.Date(date)
  y <- as.integer(format(date, "%Y"))
  y - (as.integer(format(date, "%m")) < 7L)
}

#' Host nations of the championships
#'
#' @param path CSV with `competition` (the competition code without its sex
#'   prefix: `EURO`, `WC`, `OG`, ...), `sex`, `edition` (the year in the
#'   championship's name) and `host`.
#' @return data.frame.
#' @noRd
ih_hosts <- function(path = here::here("config", "intl-handball", "hosts.csv")) {
  utils::read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8")
}

#' Hosts of the friendly tournaments named after their host country
#' @noRd
IH_FRIENDLY_HOSTS <- c(
  GL_DEN = "Denmark", GL_FRA = "France", GL_NED = "Netherlands", GL_NOR = "Norway", POSTEN = "Norway"
)

#' Venues of the matches hosts played at co-hosted championships
#'
#' Where each such match was held, so that a host counts as at home only in
#' its own country.
#' @param path CSV with `competition`, `sex`, `edition`, `match_date`,
#'   `home_team`, `away_team` (as in the match table), `venue_city`,
#'   `venue_country` (a team name) and `source`.
#' @return data.frame.
#' @noRd
ih_championship_venues <- function(path = here::here("config", "intl-handball", "championship_venues.csv")) {
  v <- utils::read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8")
  v$match_date <- as.Date(v$match_date)
  v
}

#' Venues of the qualification tournaments played at one venue
#'
#' @param path CSV with `competition`, `sex`, `edition`, `first`, `last` (the
#'   tournament's dates), `teams` (its nations, separated by "; "),
#'   `venue_city`, `venue_country` (a team name, or a country that did not
#'   play) and `source`.
#' @return data.frame.
#' @noRd
ih_qualifier_venues <- function(path = here::here("config", "intl-handball", "qualifier_venues.csv")) {
  v <- utils::read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8")
  v$first <- as.Date(v$first)
  v$last <- as.Date(v$last)
  v
}

#' Block suffix of play-off and placement matches within a qualification stage
#' @noRd
IH_PLACEMENT_BLOCK <- " - (Play Offs|[0-9]+(st|nd|rd|th)-[0-9]+(st|nd|rd|th) places)$"

#' Groups of matches linked by a team playing again within `window` days
#' @param date,home,away One entry per match.
#' @return Integer label per match; matches sharing a label form a group.
#' @noRd
.ih_link_components <- function(date, home, away, window) {
  n <- length(date)
  team <- c(home, away)
  idx <- c(seq_len(n), seq_len(n))
  d <- as.numeric(c(date, date))
  o <- order(team, d)
  team <- team[o]
  idx <- idx[o]
  d <- d[o]
  k <- which(team[-1] == team[-length(team)] & diff(d) <= window)
  from <- c(idx[k], idx[k + 1L])
  to <- c(idx[k + 1L], idx[k])
  lab <- seq_len(n)
  repeat {
    lo <- tapply(lab[to], from, min)
    at <- as.integer(names(lo))
    new <- lab
    new[at] <- pmin(new[at], lo)
    if (identical(new, lab)) break
    lab <- new
  }
  lab
}

#' Qualification matches played as a tournament at one venue
#'
#' The match table records no venue. A qualification stage's matches linked
#' by a team playing again within `window` days are taken to be one
#' tournament at one venue when they are a single round robin of three or
#' more nations within `window` days whose pairs meet nowhere else in the
#' stage (a home-and-away group plays its pairs twice, and a group short of
#' its return legs is no round robin). A play-off or placement match of the
#' stage joins when both its nations played such a tournament of the stage
#' within `window` days.
#' @param m Matches with `sex`, `competition`, `kind`, `edition`, `block`,
#'   `match_date`, `home_team`, `away_team`.
#' @param window Days.
#' @return Logical per match; FALSE for every match outside qualification.
#' @noRd
ih_single_venue <- function(m, window = 10L) {
  out <- logical(nrow(m))
  qual <- which(m$kind == "tournament" & grepl("qualif", m$block, ignore.case = TRUE))
  if (!length(qual)) {
    return(out)
  }
  stage <- paste(m$sex, m$competition, m$edition, sub(IH_PLACEMENT_BLOCK, "", m$block))
  placement <- grepl(IH_PLACEMENT_BLOCK, m$block)
  pair <- paste(pmin(m$home_team, m$away_team), pmax(m$home_team, m$away_team))
  for (s in unique(stage[qual])) {
    in_s <- qual[stage[qual] == s]
    g <- in_s[!placement[in_s]]
    if (length(g)) {
      comp <- .ih_link_components(m$match_date[g], m$home_team[g], m$away_team[g], window)
      for (k in unique(comp)) {
        y <- g[comp == k]
        n <- length(unique(c(m$home_team[y], m$away_team[y])))
        span <- as.numeric(max(m$match_date[y]) - min(m$match_date[y]))
        round_robin <- !anyDuplicated(pair[y]) && length(y) == choose(n, 2)
        again <- any(pair[setdiff(g, y)] %in% pair[y])
        out[y] <- n >= 3L && span <= window && round_robin && !again
      }
    }
    for (i in in_s[placement[in_s]]) {
      near <- g[out[g] & abs(as.numeric(m$match_date[g] - m$match_date[i])) <= window]
      played <- c(m$home_team[near], m$away_team[near])
      out[i] <- m$home_team[i] %in% played && m$away_team[i] %in% played
    }
  }
  out
}

#' Venue of each match from the listed home side's point of view
#'
#' As in the championship simulator, a nation is at home only in a match
#' played in its own country: 1 when the listed home side plays at home, -1
#' when the listed away side does, 0 on neutral ground.
#'
#' * Qualifiers and the EHF Euro Cup played home and away: 1.
#' * A qualification tournament at one venue ([ih_single_venue()], or listed
#'   in [ih_qualifier_venues()]): at home for the nation of the venue's
#'   country when [ih_qualifier_venues()] names it, else neutral.
#' * Championship matches: at home for the host when there is one host. With
#'   several hosts, at home for the nation of the venue's country as
#'   [ih_championship_venues()] lists it; a host's match missing from that
#'   list is counted neutral, with a warning.
#' * A friendly is the listed home side's home match (1), except in a
#'   friendly tournament named after its host, where the host's matches are
#'   home matches (1 or -1) and the rest neutral (0).
#' @param m Matches with `sex`, `competition`, `kind`, `edition`, `block`,
#'   `match_date`, `home_team`, `away_team`.
#' @param hosts [ih_hosts()].
#' @param venues [ih_championship_venues()].
#' @param qualifier_venues [ih_qualifier_venues()].
#' @return Integer vector of -1, 0, 1.
#' @noRd
ih_venue <- function(m, hosts = ih_hosts(), venues = ih_championship_venues(),
                     qualifier_venues = ih_qualifier_venues()) {
  n <- nrow(m)
  qual <- m$kind == "tournament" & grepl("qualif", m$block, ignore.case = TRUE)
  home_away <- (qual & !ih_single_venue(m)) | (m$kind == "tournament" & m$competition == "EUROCUP")
  champ <- m$kind == "tournament" & !qual & m$competition != "EUROCUP"
  key <- paste(m$competition, m$sex, m$edition, sep = "|")
  host_key <- paste(hosts$competition, hosts$sex, hosts$edition, sep = "|")
  n_hosts <- table(host_key)
  country <- rep(NA_character_, n)
  single <- champ & key %in% names(n_hosts)[n_hosts == 1L]
  country[single] <- hosts$host[match(key[single], host_key)]
  match_key <- paste(key, m$match_date, m$home_team, m$away_team)
  venue_key <- paste(
    paste(venues$competition, venues$sex, venues$edition, sep = "|"),
    venues$match_date, venues$home_team, venues$away_team
  )
  listed <- champ & match_key %in% venue_key
  country[listed] <- venues$venue_country[match(match_key[listed], venue_key)]
  is_host <- function(team) paste(key, team) %in% paste(host_key, hosts$host)
  unplaced <- champ & !single & !listed & (is_host(m$home_team) | is_host(m$away_team))
  if (any(unplaced)) {
    cli::cli_warn(c(
      "{sum(unplaced)} co-hosted championship match{?es} with a host ha{?s/ve} no venue in {.file championship_venues.csv}; counted neutral.",
      i = "{.val {unique(key[unplaced])}}"
    ))
  }
  for (r in seq_len(nrow(qualifier_venues))) {
    q <- qualifier_venues[r, ]
    teams <- trimws(strsplit(q$teams, ";", fixed = TRUE)[[1]])
    hit <- qual & m$sex == q$sex & m$competition == q$competition & m$edition == q$edition &
      m$match_date >= q$first & m$match_date <= q$last & m$home_team %in% teams & m$away_team %in% teams
    country[hit] <- q$venue_country
    home_away[hit] <- FALSE
  }
  at <- !is.na(country)
  side <- integer(n)
  side[at] <- as.integer(m$home_team[at] == country[at]) - as.integer(m$away_team[at] == country[at])
  fr_host <- unname(IH_FRIENDLY_HOSTS[m$competition])
  fr_tour <- ifelse(
    is.na(fr_host), 1L,
    ifelse(m$home_team == fr_host, 1L, ifelse(m$away_team == fr_host, -1L, 0L))
  )
  as.integer(ifelse(m$kind == "friendly", fr_tour, ifelse(home_away, 1L, side)))
}

#' One nation under two names in the match table: the second is used
#' @noRd
IH_TEAM_ALIASES <- c(RHF = "Russia")

#' Read the international handball matches of one sex
#'
#' @param sex `"female"` or `"male"`.
#' @param dir Folder holding `handball_matches.parquet`, see [ih_data_dir()].
#' @param hosts [ih_hosts()].
#' @param venues [ih_championship_venues()].
#' @param qualifier_venues [ih_qualifier_venues()].
#' @return data.frame, one row per match in date order: `match_id`,
#'   `competition`, `kind` (`"tournament"` or `"friendly"`), `edition`,
#'   `block`, `round`, `match_date`, `season` ([ih_season()]),
#'   `home_team`, `away_team`, `home_score`, `away_score` (final, after any
#'   extra time, shootout excluded), `home_60`, `away_60` (after 60 minutes;
#'   NA when unknown), `extra_time`, `penalties`, `pen_home`, `pen_away`,
#'   `awarded` (a result awarded or a walkover: its score is not a played
#'   one), `venue` ([ih_venue()]) and `division` (`"intl"`).
#' @noRd
ih_read_matches <- function(sex = c("female", "male"), dir = ih_data_dir(), hosts = ih_hosts(),
                            venues = ih_championship_venues(), qualifier_venues = ih_qualifier_venues()) {
  sex <- match.arg(sex)
  raw <- as.data.frame(arrow::read_parquet(file.path(dir, "handball_matches.parquet")))
  raw <- raw[raw$sex == sex, , drop = FALSE]
  m <- data.frame(
    match_id = raw$match_id,
    sex = raw$sex,
    competition = sub("^H[MW]_", "", raw$code),
    kind = raw$kind,
    edition = as.integer(raw$season_end),
    block = raw$block,
    round = raw$round,
    match_date = as.Date(raw$match_date),
    kickoff = raw$kickoff,
    home_team = raw$home_team,
    away_team = raw$away_team,
    home_score = as.integer(raw$home_score),
    away_score = as.integer(raw$away_score),
    home_60 = as.integer(raw$home_score_60),
    away_60 = as.integer(raw$away_score_60),
    extra_time = as.logical(raw$extra_time),
    penalties = as.logical(raw$penalties),
    pen_home = as.integer(raw$pen_home),
    pen_away = as.integer(raw$pen_away),
    status = raw$status,
    stringsAsFactors = FALSE
  )
  m <- m[!is.na(m$match_date), , drop = FALSE]
  for (side in c("home_team", "away_team")) {
    hit <- m[[side]] %in% names(IH_TEAM_ALIASES)
    m[[side]][hit] <- unname(IH_TEAM_ALIASES[m[[side]][hit]])
  }
  m$awarded <- !is.na(m$status) & m$status %in% c("Awrd", "WO")
  m$season <- ih_season(m$match_date)
  m$venue <- ih_venue(m, hosts, venues, qualifier_venues)
  m$division <- "intl"
  m <- m[order(m$match_date, m$match_id), , drop = FALSE]
  rownames(m) <- NULL
  m
}
