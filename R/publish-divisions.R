#' @include config.R
NULL

# ---- Sport-neutral publish-division accessors --------------------------------
#
# One set of accessors over `config/leagues.yml::<key>.publish_divisions[[sex]]`
# for all three Icelandic leagues. They replace the five football-only helpers
# that used to live in R/extract-football-iceland.R and duplicated the same
# lookup-and-validate boilerplate once per attribute.
#
# `key` is a config/leagues.yml top-level league key ("football_iceland",
# "basketball_iceland", "handball_iceland"); `sex` is "male" or "female".
#
# The one hard constraint on a badge is
# config/publish-schemas/football/next_games.schema.json's
# ^[A-Z][A-Z0-9_]*$ pattern on next_games.json::division_code. Basketball's
# division code "1D" violates it (leading digit), which is the whole reason
# `code_badge` exists as a separate configurable key rather than being derived
# from `code`.

# Fetch the raw publish_divisions entries for one (league, sex) cell.
# `caller` names the accessor in the error so a config gap points at the call
# site rather than at this worker.
.iceland_division_entries <- function(key, sex, caller) {
  stopifnot(sex %in% c("male", "female"))
  cfg <- load_leagues()[[key]][["publish_divisions"]][[sex]]
  if (is.null(cfg) || length(cfg) == 0L) {
    stop(
      caller, ": no publish_divisions[\"", sex, "\"] entry for ", key,
      " in config/leagues.yml.",
      call. = FALSE
    )
  }
  cfg
}

# Codes in YAML order, unnamed.
.iceland_division_codes <- function(key, sex) {
  cfg <- .iceland_division_entries(key, sex, ".iceland_division_codes")
  vapply(cfg, function(d) d$code, character(1))
}

# code -> URL/directory slug.
.iceland_division_slugs <- function(key, sex) {
  cfg <- .iceland_division_entries(key, sex, ".iceland_division_slugs")
  .name_by_code(vapply(cfg, function(d) d$slug, character(1)), cfg)
}

# code -> Icelandic display label (meta.json::league).
.iceland_division_labels <- function(key, sex) {
  cfg <- .iceland_division_entries(key, sex, ".iceland_division_labels")
  .name_by_code(vapply(cfg, function(d) d$label_is, character(1)), cfg)
}

# code -> NULL (flat league) or list(upper, lower) for a split-season format.
.iceland_division_split <- function(key, sex) {
  cfg <- .iceland_division_entries(key, sex, ".iceland_division_split")
  .name_by_code(
    lapply(cfg, function(d) {
      if (is.null(d$split)) {
        return(NULL)
      }
      list(
        upper = as.integer(d$split$upper),
        lower = as.integer(d$split$lower)
      )
    }),
    cfg
  )
}

# code -> short ASCII badge emitted as next_games.json::division_code.
# Falls back to `code` when `code_badge` is unset. Every entry carrying a
# `split` object also contributes its two split-phase playoff codes, since the
# publisher recodes BD_UPPER_PO / BD_LOWER_PO through this same map.
.iceland_division_badges <- function(key, sex) {
  cfg <- .iceland_division_entries(key, sex, ".iceland_division_badges")
  badges <- .name_by_code(
    vapply(
      cfg,
      function(d) if (is.null(d$code_badge)) d$code else d$code_badge,
      character(1)
    ),
    cfg
  )
  for (d in cfg) {
    if (is.null(d$split)) {
      next
    }
    badge <- badges[[d$code]]
    badges[paste0(d$code, c("_UPPER_PO", "_LOWER_PO"))] <-
      paste0(badge, c("U", "L"))
  }
  badges
}

# code -> is this cell a knockout cup (no points table)?
.iceland_division_is_cup <- function(key, sex) {
  cfg <- .iceland_division_entries(key, sex, ".iceland_division_is_cup")
  .name_by_code(vapply(cfg, function(d) isTRUE(d$is_cup), logical(1)), cfg)
}

# code -> NULL (no configured qualification cut; the consumer publishes
# meta.qualify: null and emits no p_qualify) or list(slots, label_is).
.iceland_division_qualify <- function(key, sex) {
  cfg <- .iceland_division_entries(key, sex, ".iceland_division_qualify")
  .name_by_code(
    lapply(cfg, function(d) {
      if (is.null(d$qualify)) {
        return(NULL)
      }
      list(
        slots = as.integer(d$qualify$slots),
        label_is = as.character(d$qualify$label_is)
      )
    }),
    cfg
  )
}

# code -> teams relegated from this division, NA_integer_ where unset.
.iceland_division_relegation <- function(key, sex) {
  cfg <- .iceland_division_entries(key, sex, ".iceland_division_relegation")
  .name_by_code(vapply(cfg, function(d) .as_opt_int(d$relegation_slots), integer(1)), cfg)
}

# code -> times each pair meets in the REGULAR season, NA_integer_ where unset.
# This is an assertion and a fallback, never the source: n_rounds is derived
# from schedule + results, and an unset value (basketball female 1D, a genuinely
# irregular 11-team cell) means the schedule derivation is the only source.
.iceland_division_expected_meetings <- function(key, sex) {
  cfg <- .iceland_division_entries(
    key, sex, ".iceland_division_expected_meetings"
  )
  .name_by_code(vapply(cfg, function(d) .as_opt_int(d$expected_meetings), integer(1)), cfg)
}

.name_by_code <- function(x, cfg) {
  stats::setNames(x, vapply(cfg, function(d) d$code, character(1)))
}

.as_opt_int <- function(x) {
  if (is.null(x)) NA_integer_ else as.integer(x)
}
