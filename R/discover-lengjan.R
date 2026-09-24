# R/discover-lengjan.R
#' @include ingest-lengjan-odds.R lengjan-api.R storage.R config.R
NULL

#' Classify a Lengjan competition name into (sex, division).
#'
#' Deterministic, advisory name match. Sex from a "kvenna"/"kv" marker;
#' division from the league-name pattern, per sport -- each federation has its
#' own codes (football BD/LD1-4, handball OD/G66, basketball BD/1D; spec
#' 2026-09-23 WS4). A name that matches no pattern is `division = NA`,
#' `confidence = "low"` -- surfaced for a human, never auto-wired. Non-ASCII
#' letters are written as `\\u` escapes (R-source non-ASCII rule).
#'
#' @param lengjan_name Competition display name from Lengjan.
#' @param sport "football", "handball" or "basketball".
#' @param country Pass-through context.
#' @return Tibble `{sex, division, confidence}` (one row).
#' @export
classify_competition <- function(lengjan_name, sport, country) {
  nm <- lengjan_name
  Encoding(nm) <- "UTF-8"
  has <- function(p) grepl(p, nm, ignore.case = TRUE)
  female <- has("kvenna") || has("(^| )kv\\.?( |$)")
  sex <- if (female) "female" else "male"

  division <- if (has("bikar")) {
    "CUP"
  } else if (identical(sport, "handball")) {
    if (has("ol[i\u00ed]s")) {
      "OD"
    } else if (has("grill *66")) {
      "G66"
    } else {
      NA_character_
    }
  } else if (identical(sport, "basketball")) {
    if (has("b[o\u00f3]nus")) {
      "BD"
    } else if (has("1\\. *deild")) {
      "1D"
    } else {
      NA_character_
    }
  } else if (has("3\\. *deild")) {
    "LD3"
  } else if (has("4\\. *deild")) {
    "LD4"
  } else if (has("2\\. *deild")) {
    "LD2"
  } else if (has("lengjudeild")) {
    "LD1"
  } else if (has("besta *deild")) {
    "BD"
  } else {
    NA_character_
  }
  tibble::tibble(
    sex = sex,
    division = division,
    confidence = if (is.na(division)) "low" else "high"
  )
}

#' Normalise a team name for fuzzy comparison (comparison key only).
#'
#' Lowercases, strips a trailing women's marker (" kv"/" kv."), transliterates
#' diacritics to ASCII, removes dots, collapses whitespace, and folds the common
#' "Rvk" -> "r" and (post-transliteration) "ol" -> "o" abbreviations so
#' "Víkingur Rvk kv" and "Víkingur R." collide. The original canonical
#' string is always what gets emitted -- this key is never shown.
#' @noRd
.norm_team <- function(x) {
  x <- tolower(trimws(x))
  x <- sub("\\s*kv\\.?$", "", x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- gsub("\\.", "", x)
  x <- gsub("\\brvk\\b", "r", x)
  x <- gsub("\\bol\\b", "o", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

#' Fuzzy-match Lengjan team renderings to our canonical team names.
#'
#' Exact normalised match -> "high"; nearest within Levenshtein distance 2 ->
#' "medium"; otherwise `canonical_guess = NA`, "low". Low/medium guesses are
#' fail-safe: a wrong `team_names` entry makes `decide_league` warn-skip the
#' match, never mis-bet (existing normaliser invariant).
#'
#' @param renderings Character vector of Lengjan display names.
#' @param known_teams Character vector of canonical (federation) team names.
#' @return Tibble `{lengjan, canonical_guess, confidence}`.
#' @export
match_team_names <- function(renderings, known_teams) {
  empty <- tibble::tibble(
    lengjan = character(0), canonical_guess = character(0), confidence = character(0)
  )
  if (length(renderings) == 0L) {
    return(empty)
  }
  kn <- unique(known_teams)
  if (length(kn) == 0L) {
    return(tibble::tibble(
      lengjan = renderings, canonical_guess = NA_character_, confidence = "low"
    ))
  }
  kn_norm <- vapply(kn, .norm_team, character(1))
  rows <- lapply(renderings, function(r) {
    rn <- .norm_team(r)
    hit <- which(kn_norm == rn)
    if (length(hit) >= 1L) {
      return(tibble::tibble(lengjan = r, canonical_guess = kn[[hit[[1L]]]], confidence = "high"))
    }
    d <- utils::adist(rn, kn_norm)[1L, ]
    dmin <- min(d)
    if (is.finite(dmin) && dmin <= 2L && sum(d == dmin) == 1L) {
      tibble::tibble(lengjan = r, canonical_guess = kn[[which.min(d)]], confidence = "medium")
    } else {
      tibble::tibble(lengjan = r, canonical_guess = NA_character_, confidence = "low")
    }
  })
  dplyr::bind_rows(rows)
}

#' List every competition Lengjan currently offers for a (sport, country).
#'
#' Read from the JSON program, not the site's country dropdown: Icelandic
#' events carry no `countryCode`, so the dropdown omits Iceland for every sport
#' (spec 2026-09-23 L4); [parse_lengjan_program()] falls back to `countryName`.
#'
#' @param sport,country Canonical names ("handball", "iceland").
#' @param events Output of [parse_lengjan_program()].
#' @return Tibble `{sport, country, comp_id, lengjan_name}` (possibly empty).
#' @export
lengjan_list_competitions <- function(sport, country, events) {
  hit <- events[events$sport_id == .lengjan_sport_id(sport) &
    events$country_code %in% .lengjan_country_code(country), , drop = FALSE]
  hit <- hit[!duplicated(hit$competition_id), , drop = FALSE]
  tibble::tibble(
    sport = rep(sport, nrow(hit)),
    country = rep(country, nrow(hit)),
    comp_id = hit$competition_id,
    lengjan_name = hit$competition_name
  )
}

#' Draft team_names for a competition from the program's participants.
#'
#' @param comp_id Lengjan competition id.
#' @param events Output of [parse_lengjan_program()].
#' @param known_teams Canonical team names to match against.
#' @return Tibble `{lengjan, canonical_guess, confidence}`.
#' @export
propose_team_names <- function(comp_id, events, known_teams) {
  ev <- events[events$competition_id == comp_id, , drop = FALSE]
  renderings <- unique(c(ev$home_team, ev$away_team))
  renderings <- renderings[!is.na(renderings) & nzchar(renderings)]
  match_team_names(renderings, known_teams)
}

#' @noRd
.configured_comp_ids <- function(leagues, sport, country) {
  ids <- character(0)
  for (lg in leagues) {
    if (identical(lg$sport, sport) && identical(lg$country, country)) {
      for (cmp in lg$lengjan$competitions %||% list()) {
        ids <- c(ids, as.character(cmp$id))
      }
    }
  }
  unique(ids)
}

#' @noRd
.modelled_division_codes <- function(league, sex) {
  pd <- league$publish_divisions[[sex]]
  if (is.null(pd) || length(pd) == 0L) {
    return(NULL) # NULL = no division gating (single-division sport)
  }
  vapply(pd, function(d) d$code, character(1))
}

#' @noRd
.known_teams_for <- function(sport, country, sex, division, root) {
  res <- tryCatch(
    read_table("results",
      root = root,
      filter = list(sport = sport, country = country, sex = sex)
    ),
    error = function(e) tibble::tibble()
  )
  if (nrow(res) == 0L) {
    return(character(0))
  }
  if (!is.na(division) && "division" %in% names(res)) {
    res <- res[!is.na(res$division) & res$division == division, , drop = FALSE]
  }
  unique(c(res$home_team, res$away_team))
}

#' Discover competitions Lengjan now offers that we model but do not yet scrape.
#'
#' For each active modelled `(sport, country)`, lists the live competitions
#' from Lengjan's JSON program, diffs against configured comp IDs, classifies
#' each new one, keeps those whose inferred division we model, and drafts
#' `team_names`. Pure inputs are injectable (`list_fn`, `team_names_fn`) so the
#' orchestration is unit-tested without network or Stan. Competitions for a
#' modelled `(sport, country)` whose division we do NOT model are counted in
#' `unmodelled_offered_count`.
#'
#' @param leagues Full leagues list (`load_leagues()`).
#' @param events Output of [parse_lengjan_program()]; \code{NULL} fetches the live program once.
#' @param root Data root for `read_table("results")`.
#' @param list_fn,team_names_fn Injectable closures for testing.
#' @return `list(competitions = <list>, unmodelled_offered_count = int)`.
#' @export
discover_new_competitions <- function(leagues, events = NULL,
                                      root = here::here("data"),
                                      list_fn = NULL, team_names_fn = NULL) {
  if ((is.null(list_fn) || is.null(team_names_fn)) && is.null(events)) {
    events <- parse_lengjan_program(lengjan_fetch_program())
  }
  if (is.null(list_fn)) {
    list_fn <- function(sport, country) lengjan_list_competitions(sport, country, events)
  }
  if (is.null(team_names_fn)) {
    team_names_fn <- function(comp_id, sport, country, sex, division) {
      kt <- .known_teams_for(sport, country, sex, division, root)
      propose_team_names(comp_id, events, kt)
    }
  }

  # Every active league from betting.mode "scrape" up -- including one with no
  # competitions yet, which is exactly the league discovery exists for (spec
  # 2026-09-23 WS4; the old has_lengjan filter made HB/BB invisible).
  active <- filter_leagues(leagues, active_only = TRUE)
  active <- active[vapply(active, betting_mode_at_least, logical(1), stage = "scrape")]
  pairs <- list()
  for (key in names(active)) {
    lg <- active[[key]]
    pk <- paste(lg$sport, lg$country, sep = "\r")
    if (is.null(pairs[[pk]])) {
      pairs[[pk]] <- list(sport = lg$sport, country = lg$country, league = lg)
    }
  }

  findings <- list()
  unmodelled <- 0L
  for (p in pairs) {
    live <- tryCatch(list_fn(p$sport, p$country), error = function(e) NULL)
    if (is.null(live) || nrow(live) == 0L) next
    have <- .configured_comp_ids(leagues, p$sport, p$country)
    new <- live[!(as.character(live$comp_id) %in% have), , drop = FALSE]
    if (nrow(new) == 0L) next
    for (i in seq_len(nrow(new))) {
      cls <- classify_competition(new$lengjan_name[[i]], p$sport, p$country)
      codes <- .modelled_division_codes(p$league, cls$sex)
      modelled <- is.null(codes) || (!is.na(cls$division) && cls$division %in% codes)
      if (!modelled) {
        unmodelled <- unmodelled + 1L
        next
      }
      tn <- tryCatch(
        team_names_fn(new$comp_id[[i]], p$sport, p$country, cls$sex, cls$division),
        error = function(e) {
          tibble::tibble(
            lengjan = character(0), canonical_guess = character(0),
            confidence = character(0)
          )
        }
      )
      findings[[length(findings) + 1L]] <- list(
        sport = p$sport, country = p$country,
        comp_id = as.character(new$comp_id[[i]]),
        lengjan_name = new$lengjan_name[[i]],
        inferred_sex = cls$sex,
        inferred_division = cls$division,
        classify_confidence = cls$confidence,
        modelled = TRUE, status = "new",
        proposed_team_names = tn
      )
    }
  }
  list(competitions = findings, unmodelled_offered_count = unmodelled)
}

#' @noRd
.tn_to_list <- function(tn) {
  if (is.null(tn) || nrow(tn) == 0L) {
    return(list())
  }
  lapply(seq_len(nrow(tn)), function(j) {
    list(
      lengjan = tn$lengjan[[j]],
      canonical_guess = if (is.na(tn$canonical_guess[[j]])) NULL else tn$canonical_guess[[j]],
      confidence = tn$confidence[[j]]
    )
  })
}

#' @noRd
.write_discovery_summary <- function(payload, path) {
  lines <- c(
    "# Lengjan discovery — proposed competitions",
    "",
    paste0("Generated: ", payload$generated_at),
    paste0(
      "Unmodelled competitions offered (in our sports/countries): ",
      payload$unmodelled_offered_count
    ),
    ""
  )
  if (length(payload$competitions) == 0L) {
    lines <- c(lines, "_No new modelled competitions to wire._")
  } else {
    for (comp in payload$competitions) {
      lines <- c(
        lines,
        sprintf(
          "## %s / %s — %s (id=%s)", comp$sport, comp$inferred_sex,
          comp$inferred_division, comp$comp_id
        ),
        sprintf("- Lengjan name: %s", comp$lengjan_name),
        sprintf("- Classify confidence: %s", comp$classify_confidence),
        "- Proposed team_names:"
      )
      if (length(comp$proposed_team_names) == 0L) {
        lines <- c(lines, "  - (none scraped yet)")
      } else {
        for (t in comp$proposed_team_names) {
          cg <- t$canonical_guess %||% "??? (verify)"
          lines <- c(lines, sprintf("  - %s -> %s (%s)", t$lengjan, cg, t$confidence))
        }
      }
      lines <- c(lines, "")
    }
  }
  writeLines(enc2utf8(lines), path, useBytes = TRUE)
  invisible(path)
}

#' Write the discovery proposal to JSON + a human-readable summary.
#' @param findings Output of [discover_new_competitions()].
#' @param root Data root; writes under `root/discovery/`.
#' @param now Timestamp for the payload.
#' @return invisible(path to proposals.json).
#' @export
write_discovery_proposal <- function(findings, root = here::here("data"),
                                     now = Sys.time()) {
  comps <- lapply(findings$competitions, function(f) {
    f$proposed_team_names <- .tn_to_list(f$proposed_team_names)
    f
  })
  payload <- list(
    generated_at = format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    competitions = comps,
    unmodelled_offered_count = findings$unmodelled_offered_count
  )
  dir <- file.path(root, "discovery")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, "proposals.json")
  write_json_consistent(payload, path, pretty = TRUE, auto_unbox = TRUE)
  .write_discovery_summary(payload, file.path(dir, "SUMMARY.md"))
  invisible(path)
}
