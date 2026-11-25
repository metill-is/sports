#' @include config.R lengjan-api.R ingest-lengjan-odds.R
NULL

# Shadow comparison of the two Lengjan odds scrapers (spec 2026-09-23 WS3):
# the Chromote DOM scraper and the JSON API, read at the same moment and
# written nowhere. scripts/0Nc_compare_odds_sources.R drives it; nothing here
# touches the network or storage.

.ODDS_COMPARE_KEY <- c(
  "match_date", "home_team", "away_team", "market", "outcome", "line"
)

#' Causes a DOM row can fail with, in the order they are tested.
#' @noRd
.DOM_FAIL_CAUSES <- c(
  "dom_detail_mispaired", "odds_mismatch", "suspended_on_api",
  "line_not_on_api", "market_not_on_api", "date_differs", "name_whitespace",
  "match_not_on_api"
)

#' Causes an API row with no DOM row is listed with, in the order tested.
#' @noRd
.API_ONLY_CAUSES <- c(
  "dom_detail_mispaired", "name_whitespace", "date_differs",
  "match_not_on_dom", "dom_detail_not_visited", "dom_detail_failed",
  "market_not_on_dom", "changed_during_run", "outcome_not_on_dom",
  "extra_line_on_api"
)

#' @noRd
.cmp_sep <- function(...) paste(..., sep = "\u001f")

#' @noRd
.cmp_match_key <- function(d, squish = FALSE) {
  h <- tag_utf8(d$home_team)
  a <- tag_utf8(d$away_team)
  if (squish) {
    h <- stringr::str_squish(h)
    a <- stringr::str_squish(a)
  }
  .cmp_sep(format(d$match_date), h, a)
}

#' @noRd
.cmp_teams_key <- function(d) {
  .cmp_sep(tag_utf8(d$home_team), tag_utf8(d$away_team))
}

#' @noRd
.cmp_line <- function(line) ifelse(is.na(line), "NA", sprintf("%.2f", line))

#' @noRd
.cmp_key <- function(d) {
  .cmp_sep(.cmp_match_key(d), d$market, d$outcome, .cmp_line(d$line))
}

#' @noRd
.cmp_cents <- function(x) round(x * 100)

#' Judge a DOM odds scrape against two API snapshots taken around it.
#'
#' Rows are keyed on (match_date, home_team, away_team, market, outcome,
#' line), names as each scraper writes them. A DOM row **passes** when an API
#' row with the same key carries the same price, to the cent, in either
#' snapshot: the one taken just before the DOM run or the one just after.
#' Prices that move during the run therefore still pass, as long as the DOM
#' saw one of the two prices the API showed. A row that does not pass gets
#' the first cause that applies:
#'
#' - `dom_detail_mispaired`: a handicap or total row from a detail page whose
#'   link's event is another match on the API (the DOM pairs detail pages
#'   with matches by position);
#' - `odds_mismatch`: the key is on the API at another price in both
#'   snapshots;
#' - `suspended_on_api`: the API lists the selection, but not open or
#'   without a valid price, so the API scrape leaves it out;
#' - `line_not_on_api` / `market_not_on_api`: the match is on the API but this
#'   line, or the whole market, is not;
#' - `date_differs`: the API has the same two teams on another date;
#' - `name_whitespace`: the API has the match once names are squished (the
#'   DOM's text extraction trims and collapses spaces; the API does not);
#' - `match_not_on_api`: none of these.
#'
#' Every API row (from either snapshot) without a DOM row is listed with a
#' cause: `dom_detail_mispaired`, `name_whitespace`, `date_differs`,
#' `match_not_on_dom`, `dom_detail_not_visited` (beyond `max_detail`),
#' `dom_detail_failed`, `market_not_on_dom` (the detail page loaded but showed
#' no row of this market), `changed_during_run` (the key is in one snapshot
#' only), `outcome_not_on_dom` (the DOM has the line but not this outcome) or
#' `extra_line_on_api` (the DOM has the market, not this line).
#'
#' @param dom DOM rows: `rows` from [scrape_lengjan_dom()].
#' @param api_before,api_after API rows: `odds` from two
#'   [lengjan_api_snapshot()] calls, before and after the DOM run.
#' @param visits `visits` from [scrape_lengjan_dom()], or NULL.
#' @param api_events Program rows carrying `event_id`, `home_team`,
#'   `away_team` and `kickoff_at` (`events` from [lengjan_api_snapshot()]), or
#'   NULL. With `visits`, they find mispaired detail pages.
#' @param api_closed `closed` from the snapshots (bound together), or NULL.
#' @return A list of class `odds_source_comparison`: `dom` (the DOM rows with
#'   `odds_before`, `odds_after`, `verdict` "pass"/"fail" and `cause`),
#'   `api_only` (API rows with no DOM row: key, `odds_before`, `odds_after`,
#'   `cause`), `mispaired` (visits whose link is another match),
#'   `duplicates` (keys a side lists more than once, with the side) and
#'   `summary` (one row of counts). `pass` is TRUE only when there are DOM
#'   rows, every one passes, at least one match on both sides has DOM handicap
#'   or total rows (a detail page was compared), and, when `visits` is given,
#'   at least one detail page loaded. Otherwise `fail_reason` lists, in this
#'   order, `no_dom_rows`, `dom_rows_fail`, `no_shared_detail` and
#'   `no_detail_page_ok`; it is NA on a pass.
#' @export
compare_odds_sources <- function(dom, api_before, api_after, visits = NULL,
                                 api_events = NULL, api_closed = NULL) {
  pick <- function(d) {
    d <- d[, c(.ODDS_COMPARE_KEY, "odds"), drop = FALSE]
    d$key <- .cmp_key(d)
    d$match_key <- .cmp_match_key(d)
    d
  }
  dom <- pick(dom)
  a <- pick(api_before)
  b <- pick(api_after)

  dup <- function(d, side) {
    k <- unique(d$key[duplicated(d$key)])
    tibble::tibble(side = rep(side, length(k)), key = k)
  }
  duplicates <- dplyr::bind_rows(
    dup(dom, "dom"), dup(a, "api_before"), dup(b, "api_after")
  )
  a <- a[!duplicated(a$key), , drop = FALSE]
  b <- b[!duplicated(b$key), , drop = FALSE]

  mispaired <- .cmp_mispaired(visits, api_events)
  mis_keys <- .cmp_match_key(mispaired)
  detail_market <- function(d) d$market %in% c("spread", "total")

  # --- DOM rows ---------------------------------------------------------
  dom$odds_before <- a$odds[match(dom$key, a$key)]
  dom$odds_after <- b$odds[match(dom$key, b$key)]
  same <- function(x) !is.na(x) & .cmp_cents(x) == .cmp_cents(dom$odds)
  dom$verdict <- ifelse(same(dom$odds_before) | same(dom$odds_after),
    "pass", "fail"
  )

  api_any <- dplyr::bind_rows(a, b)
  api_match <- unique(api_any$match_key)
  api_market <- unique(.cmp_sep(api_any$match_key, api_any$market))
  api_squish <- unique(.cmp_match_key(api_any, squish = TRUE))
  api_teams <- unique(.cmp_teams_key(api_any))
  closed_keys <- if (is.null(api_closed)) character() else .cmp_key(api_closed)

  dom$cause <- NA_character_
  todo <- function() dom$verdict == "fail" & is.na(dom$cause)
  dom$cause[todo() & detail_market(dom) & dom$match_key %in% mis_keys] <-
    "dom_detail_mispaired"
  dom$cause[todo() & (!is.na(dom$odds_before) | !is.na(dom$odds_after))] <-
    "odds_mismatch"
  dom$cause[todo() & dom$key %in% closed_keys] <- "suspended_on_api"
  on_api <- dom$match_key %in% api_match
  has_market <- .cmp_sep(dom$match_key, dom$market) %in% api_market
  dom$cause[todo() & on_api & has_market] <- "line_not_on_api"
  dom$cause[todo() & on_api] <- "market_not_on_api"
  dom$cause[todo() & .cmp_teams_key(dom) %in% api_teams] <- "date_differs"
  dom$cause[todo() & .cmp_match_key(dom, squish = TRUE) %in% api_squish] <-
    "name_whitespace"
  dom$cause[todo()] <- "match_not_on_api"

  # --- API rows without a DOM row ---------------------------------------
  api <- dplyr::bind_rows(b, a[!a$key %in% b$key, , drop = FALSE])
  api$odds_before <- a$odds[match(api$key, a$key)]
  api$odds_after <- b$odds[match(api$key, b$key)]
  api <- api[!api$key %in% dom$key, , drop = FALSE]

  dom_match <- unique(dom$match_key)
  dom_market <- unique(.cmp_sep(dom$match_key, dom$market))
  dom_line <- unique(.cmp_sep(dom$match_key, dom$market, .cmp_line(dom$line)))
  dom_squish <- unique(.cmp_match_key(dom, squish = TRUE))
  dom_teams <- unique(.cmp_teams_key(dom))
  visit_state <- if (is.null(visits) || nrow(visits) == 0L) {
    stats::setNames(character(), character())
  } else {
    stats::setNames(visits$detail, .cmp_match_key(visits))
  }

  api$cause <- NA_character_
  todo <- function() is.na(api$cause)
  api$cause[todo() & detail_market(api) & api$match_key %in% mis_keys] <-
    "dom_detail_mispaired"
  off_dom <- !api$match_key %in% dom_match
  api$cause[todo() & off_dom &
    .cmp_match_key(api, squish = TRUE) %in% dom_squish] <- "name_whitespace"
  api$cause[todo() & off_dom & .cmp_teams_key(api) %in% dom_teams] <-
    "date_differs"
  api$cause[todo() & off_dom] <- "match_not_on_dom"
  state <- unname(visit_state[api$match_key])
  no_market <- !.cmp_sep(api$match_key, api$market) %in% dom_market
  api$cause[todo() & no_market & state %in% "not_visited"] <-
    "dom_detail_not_visited"
  api$cause[todo() & no_market & state %in% "failed"] <- "dom_detail_failed"
  api$cause[todo() & no_market] <- "market_not_on_dom"
  api$cause[todo() & (is.na(api$odds_before) | is.na(api$odds_after))] <-
    "changed_during_run"
  api$cause[todo() &
    .cmp_sep(api$match_key, api$market, .cmp_line(api$line)) %in% dom_line] <-
    "outcome_not_on_dom"
  api$cause[todo()] <- "extra_line_on_api"

  dom_out <- dom[, c(
    .ODDS_COMPARE_KEY, "odds", "odds_before", "odds_after",
    "verdict", "cause"
  ), drop = FALSE]
  api_out <- api[, c(.ODDS_COMPARE_KEY, "odds_before", "odds_after", "cause"),
    drop = FALSE
  ]
  shared <- intersect(dom_match, api_match)
  detail_shared <- unique(dom$match_key[detail_market(dom) &
    dom$match_key %in% shared])
  reasons <- c(
    if (nrow(dom) == 0L) "no_dom_rows",
    if (any(dom$verdict == "fail")) "dom_rows_fail",
    if (length(detail_shared) == 0L) "no_shared_detail",
    if (!is.null(visits) && !any(visits$detail == "ok")) "no_detail_page_ok"
  )
  summary <- tibble::tibble(
    pass = length(reasons) == 0L,
    fail_reason = if (length(reasons) == 0L) {
      NA_character_
    } else {
      paste(reasons, collapse = "; ")
    },
    dom_rows = nrow(dom),
    dom_pass = sum(dom$verdict == "pass"),
    dom_fail = sum(dom$verdict == "fail"),
    api_rows = length(unique(c(a$key, b$key))),
    api_only = nrow(api),
    dom_matches = length(dom_match),
    api_matches = length(api_match),
    shared_matches = length(shared),
    shared_detail_matches = length(detail_shared),
    detail_visits = if (is.null(visits)) NA_integer_ else nrow(visits),
    detail_ok = if (is.null(visits)) NA_integer_ else sum(visits$detail == "ok"),
    mispaired_visits = nrow(mispaired),
    duplicate_keys = nrow(duplicates)
  )
  structure(
    list(
      dom = dom_out, api_only = api_out, mispaired = mispaired,
      duplicates = duplicates, summary = summary
    ),
    class = "odds_source_comparison"
  )
}

#' Detail-page visits whose link's event is a different match on the API.
#'
#' The link's event id names the event the detail page shows; the visit
#' carries the teams the DOM scraper paired it with, by position. When the
#' API's teams for that event differ (spaces squished), the page's handicap
#' and total rows were written under the wrong match.
#' @noRd
.cmp_mispaired <- function(visits, api_events) {
  none <- empty_lengjan_dom_visits()
  none$api_home_team <- character()
  none$api_away_team <- character()
  if (is.null(visits) || is.null(api_events) || nrow(visits) == 0L) {
    return(none)
  }
  v <- visits[visits$detail == "ok" & !is.na(visits$event_id), , drop = FALSE]
  i <- match(v$event_id, api_events$event_id)
  v <- v[!is.na(i), , drop = FALSE]
  i <- i[!is.na(i)]
  v$api_home_team <- api_events$home_team[i]
  v$api_away_team <- api_events$away_team[i]
  sq <- function(x) stringr::str_squish(tag_utf8(x))
  wrong <- sq(v$home_team) != sq(v$api_home_team) |
    sq(v$away_team) != sq(v$api_away_team)
  dplyr::bind_rows(none, v[wrong, , drop = FALSE])
}

#' Counts of a comparison's causes, DOM failures and API-only rows apart.
#'
#' @param x An `odds_source_comparison` from [compare_odds_sources()].
#' @return Tibble: side ("dom_fail" or "api_only"), cause, market, n.
#' @export
odds_comparison_causes <- function(x) {
  stopifnot(inherits(x, "odds_source_comparison"))
  count <- function(d, side) {
    if (nrow(d) == 0L) {
      return(tibble::tibble(
        side = character(), cause = character(), market = character(),
        n = integer()
      ))
    }
    out <- dplyr::count(d, .data$cause, .data$market, name = "n")
    tibble::tibble(side = side, cause = out$cause, market = out$market, n = out$n)
  }
  dplyr::bind_rows(
    count(x$dom[x$dom$verdict == "fail", , drop = FALSE], "dom_fail"),
    count(x$api_only, "api_only")
  )
}

#' Audit Lengjan participant names against a league's `team_names`.
#'
#' Each name is checked against the renderings of its own sex (the sex of
#' the competition it was listed under) and of the other sex:
#'
#' - `mapped`: a rendering of this sex; decide maps it to its canonical;
#' - `whitespace`: a rendering of this sex once spaces are squished (the DOM
#'   always shows the squished form; the API may not);
#' - `canonical_unlisted`: equal to a canonical name of this sex but not
#'   listed as a rendering: decide passes it through with a warning, and it
#'   still joins the beliefs;
#' - `other_sex`: a rendering of the other sex only, so it would be priced
#'   against the wrong cell's team or dropped;
#' - `unmapped`: none of these; decide warns and the match finds no beliefs.
#'
#' A name with sex `NA` (DOM history, which carries no sex) is checked
#' against both sexes; its `status` is the better of the two, and `sex_hit`
#' says which sex gave it.
#'
#' @param names Tibble with `team` and `sex` ("male", "female" or NA), and
#'   any other columns (source, counts, dates), which are kept.
#' @param league A league definition carrying `lengjan$team_names`.
#' @return `names` with `status`, `sex_hit` and `canonical` added.
#' @export
audit_lengjan_team_names <- function(names, league) {
  tn <- league$lengjan$team_names %||% list()
  inv <- function(sex) {
    r <- tn_renderings(tn[[sex]])
    stats::setNames(
      tag_utf8(rep(names(r), lengths(r))),
      tag_utf8(unlist(r, use.names = FALSE))
    )
  }
  maps <- list(male = inv("male"), female = inv("female"))
  canon <- lapply(c(male = "male", female = "female"), function(s) {
    tag_utf8(names(tn_renderings(tn[[s]])))
  })
  rank <- c(
    mapped = 1L, whitespace = 2L, canonical_unlisted = 3L,
    other_sex = 4L, unmapped = 5L
  )
  one <- function(team, sex) {
    team <- tag_utf8(team)
    sq <- stringr::str_squish(team)
    other <- setdiff(c("male", "female"), sex)
    if (team %in% names(maps[[sex]])) {
      return(c("mapped", maps[[sex]][[team]]))
    }
    if (sq %in% names(maps[[sex]])) {
      return(c("whitespace", maps[[sex]][[sq]]))
    }
    if (sq %in% canon[[sex]]) {
      return(c("canonical_unlisted", sq))
    }
    if (sq %in% names(maps[[other]])) {
      return(c("other_sex", maps[[other]][[sq]]))
    }
    c("unmapped", NA_character_)
  }
  status <- character(nrow(names))
  sex_hit <- character(nrow(names))
  canonical <- character(nrow(names))
  for (k in seq_len(nrow(names))) {
    sexes <- if (is.na(names$sex[[k]])) c("male", "female") else names$sex[[k]]
    res <- lapply(sexes, function(s) one(names$team[[k]], s))
    best <- which.min(rank[vapply(res, `[[`, character(1), 1L)])
    status[[k]] <- res[[best]][[1]]
    canonical[[k]] <- res[[best]][[2]]
    sex_hit[[k]] <- if (status[[k]] %in% c("unmapped", "other_sex")) {
      NA_character_
    } else {
      sexes[[best]]
    }
  }
  names$status <- status
  names$sex_hit <- sex_hit
  names$canonical <- canonical
  names
}

#' Each configured rendering, and whether any audited source showed it.
#'
#' @param league A league definition carrying `lengjan$team_names`.
#' @param seen Character vector of names seen (any source, any sex).
#' @return Tibble: sex, canonical, rendering, primary (the first rendering,
#'   which the placer types), seen.
#' @export
team_name_rendering_coverage <- function(league, seen) {
  tn <- league$lengjan$team_names %||% list()
  seen <- tag_utf8(seen)
  out <- lapply(c("male", "female"), function(sex) {
    r <- tn_renderings(tn[[sex]])
    if (length(r) == 0L) {
      return(NULL)
    }
    tibble::tibble(
      sex = sex,
      canonical = tag_utf8(rep(names(r), lengths(r))),
      rendering = tag_utf8(unlist(r, use.names = FALSE)),
      primary = unlist(lapply(unname(lengths(r)), function(n) {
        c(TRUE, rep(FALSE, n - 1L))
      }))
    )
  })
  out <- dplyr::bind_rows(c(list(tibble::tibble(
    sex = character(), canonical = character(), rendering = character(),
    primary = logical()
  )), out))
  out$seen <- out$rendering %in% seen
  out
}

#' The `--out` folder of `scripts/0Nc_compare_odds_sources.R` and
#' `scripts/0Nc_audit_team_names.R`: an absolute path with `.` and `..`
#' collapsed and symlinks resolved as far as it exists (`.closing_abs_path()`),
#' refused when it is `data/` or under it. A relative path resolves against
#' the working directory first, so `data/reports` is refused before it exists:
#' `normalizePath(mustWork = FALSE)` returns such a path unchanged, which
#' passed the old prefix check. NULL (no `--out`) stays NULL.
#' @noRd
odds_compare_out_dir <- function(out, repo = here::here()) {
  if (is.null(out)) {
    return(NULL)
  }
  path <- .closing_abs_path(out)
  data_dir <- .closing_abs_path(file.path(repo, "data"))
  if (identical(path, data_dir) || startsWith(path, paste0(data_dir, "/"))) {
    stop(
      "--out must not be under data/ (", path, "): these scripts write ",
      "nothing there.",
      call. = FALSE
    )
  }
  path
}
