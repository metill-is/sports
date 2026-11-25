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

#' Venue of each match from the listed home side's point of view
#'
#' * Qualifiers and the EHF Euro Cup are played home and away: 1.
#' * A friendly is the listed home side's home match (1), except in a
#'   friendly tournament named after its host, where the host's matches are
#'   home matches (1 or -1) and the rest neutral (0).
#' * Championship matches are neutral (0) unless a host nation plays: 1 when
#'   it is listed home, -1 when listed away. A host is taken to play at home
#'   in every match of its championship.
#' @param m Matches with `sex`, `competition`, `kind`, `edition`, `block`,
#'   `home_team`, `away_team`.
#' @param hosts [ih_hosts()].
#' @return Integer vector of -1, 0, 1.
#' @noRd
ih_venue <- function(m, hosts = ih_hosts()) {
  key <- paste(m$competition, m$sex, m$edition, sep = "|")
  host_key <- paste(hosts$competition, hosts$sex, hosts$edition, sep = "|")
  is_host <- function(team) paste(key, team) %in% paste(host_key, hosts$host)
  champ <- ifelse(is_host(m$home_team), 1L, ifelse(is_host(m$away_team), -1L, 0L))
  fr_host <- unname(IH_FRIENDLY_HOSTS[m$competition])
  fr_tour <- ifelse(
    is.na(fr_host), 1L,
    ifelse(m$home_team == fr_host, 1L, ifelse(m$away_team == fr_host, -1L, 0L))
  )
  home_away <- grepl("qualif", m$block, ignore.case = TRUE) | m$competition == "EUROCUP"
  out <- ifelse(m$kind == "friendly", fr_tour, ifelse(home_away, 1L, champ))
  as.integer(out)
}

#' One nation under two names in the match table: the second is used
#' @noRd
IH_TEAM_ALIASES <- c(RHF = "Russia")

#' Read the international handball matches of one sex
#'
#' @param sex `"female"` or `"male"`.
#' @param dir Folder holding `handball_matches.parquet`, see [ih_data_dir()].
#' @param hosts [ih_hosts()].
#' @return data.frame, one row per match in date order: `match_id`,
#'   `competition`, `kind` (`"tournament"` or `"friendly"`), `edition`,
#'   `block`, `round`, `match_date`, `season` ([ih_season()]),
#'   `home_team`, `away_team`, `home_score`, `away_score` (final, after any
#'   extra time, shootout excluded), `home_60`, `away_60` (after 60 minutes;
#'   NA when unknown), `extra_time`, `penalties`, `pen_home`, `pen_away`,
#'   `awarded` (a result awarded or a walkover: its score is not a played
#'   one), `venue` ([ih_venue()]) and `division` (`"intl"`).
#' @noRd
ih_read_matches <- function(sex = c("female", "male"), dir = ih_data_dir(), hosts = ih_hosts()) {
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
  m$venue <- ih_venue(m, hosts)
  m$division <- "intl"
  m <- m[order(m$match_date, m$match_id), , drop = FALSE]
  rownames(m) <- NULL
  m
}
