#' Load and validate leagues.yml
#'
#' @param path Path to leagues.yml.
#' @param schema_path Path to leagues.schema.json. Defaults to here().
#' @param validate If TRUE (default), validate the loaded YAML against the schema.
#' @return Named list keyed by league_key (e.g. "basketball_iceland").
#' @export
load_leagues <- function(path = here::here("config", "leagues.yml"),
                         schema_path = here::here("config", "leagues.schema.json"),
                         validate = TRUE) {
  raw <- readr::read_file(path)
  leagues <- yaml::yaml.load(raw)

  if (is.null(leagues)) {
    stop(
      "leagues.yml parsed to NULL (empty or malformed file): ", path,
      call. = FALSE
    )
  }

  if (isTRUE(validate)) {
    if (!file.exists(schema_path)) {
      stop("leagues.schema.json not found: ", schema_path, call. = FALSE)
    }
    validate_leagues(leagues, schema_path)
    check_team_names_injective(leagues)
  }

  leagues
}

# Coerce known-array fields to lists so that jsonlite::toJSON(auto_unbox = TRUE)
# does not flatten single-element arrays into scalars (which would trip the
# schema's "must be array" checks on e.g. `sexes: [male]`).
# Note: betting$markets is now an object (boolean toggles), not an array —
# do NOT wrap it here.
coerce_array_fields <- function(leagues) {
  for (key in names(leagues)) {
    l <- leagues[[key]]
    if (!is.null(l$sexes) && !is.list(l$sexes)) {
      l$sexes <- as.list(l$sexes)
    }
    # lengjan$competitions is already a list-of-lists in yaml.load output; leave alone.
    leagues[[key]] <- l
  }
  leagues
}

validate_leagues <- function(leagues, schema_path) {
  leagues <- coerce_array_fields(leagues)
  json_text <- jsonlite::toJSON(leagues, auto_unbox = TRUE, null = "null", na = "null")
  schema_text <- readr::read_file(schema_path)

  result <- jsonvalidate::json_validate(json_text, schema_text, verbose = TRUE, engine = "ajv")
  if (!isTRUE(result)) {
    errors <- attr(result, "errors")
    err_lines <- if (!is.null(errors) && nrow(errors) > 0) {
      paste(sprintf("  %s: %s", errors$instancePath, errors$message), collapse = "\n")
    } else {
      "  (no detailed errors returned by validator)"
    }
    stop(paste0("leagues.yml failed schema validation:\n", err_lines), call. = FALSE)
  }
  invisible(TRUE)
}

#' Normalise a per-sex team_names sub-map to canonical -> renderings form.
#'
#' A `team_names` value is either a single Lengjan rendering (scalar string) or
#' a list of acceptable renderings — used when Lengjan renders the same team
#' under more than one byte-distinct string (e.g. "Grindavík / Njarðvík kv" vs
#' "Grindavik/Njarðvík kv"). This collapses both shapes to a named list whose
#' values are always character vectors of one or more renderings, the first
#' being the primary (the rendering the placer types into the bet slip). It is
#' the single source of truth that lets the decode path
#' ([normalise_lengjan_team_names()]) and the encode path ([resolve_bet_match_id()])
#' read a scalar-or-list value uniformly.
#'
#' @param tn A per-sex sub-map (named list) or NULL.
#' @return Named list: canonical name -> character vector of renderings.
#'   `list()` for NULL / empty input.
#' @noRd
tn_renderings <- function(tn) {
  if (is.null(tn) || length(tn) == 0L) {
    return(list())
  }
  lapply(tn, function(v) as.character(unlist(v)))
}

#' Assert a canonical -> display name map is injective.
#'
#' Each display (Lengjan) value must come from at most one canonical name, or
#' the decide-time inverse map (normalise_lengjan_team_names()) silently picks
#' one canonical name at lookup time. List-valued entries (multiple renderings
#' for one canonical) are flattened, so the invariant is enforced across the
#' union of every rendering. NULL / empty maps pass. Shared by the load-time
#' guard and the placer's pre-flight check.
#' @noRd
assert_injective_map <- function(map, label) {
  if (is.null(map) || length(map) == 0L) {
    return(invisible(TRUE))
  }
  vals <- unname(unlist(map))
  if (anyDuplicated(vals) > 0L) {
    dups <- unique(vals[duplicated(vals)])
    cli::cli_abort(
      c(
        "{label} has non-injective team_names:",
        "x" = "multiple canonical names map to {.val {dups}}"
      ),
      call = NULL
    )
  }
  invisible(TRUE)
}

#' Assert every league's per-sex team_names sub-map is injective.
#'
#' Shifts the injectivity fault from "first bet on team X silently warn-skips"
#' (decide/placer time) to every load_leagues() / test / CI run.
#' @noRd
check_team_names_injective <- function(leagues) {
  for (key in names(leagues)) {
    tn_all <- leagues[[key]]$lengjan$team_names
    if (is.null(tn_all)) next
    for (sx in names(tn_all)) {
      assert_injective_map(tn_all[[sx]], label = paste0(key, " (", sx, ")"))
    }
  }
  invisible(TRUE)
}

#' Load + validate the global bankroll config.
#'
#' Reads `config/bankroll.yml`. If `current_pool` is missing from the YAML
#' (the usual case), derives it as `initial_pool + sum(ledger$pnl[settled])`
#' so the bankroll evolves with realised PnL.
#'
#' @param path Path to bankroll.yml.
#' @param ledger_root Root directory for Parquet stores (parent of `decisions/ledger/`).
#' @return List with `initial_pool`, `current_pool`, `daily_budget_frac`,
#'   `daily_budget_min_isk`, `kelly_ceiling`, `max_match_stake_default`.
#' @export
load_bankroll <- function(path = here::here("config", "bankroll.yml"),
                          ledger_root = here::here("data")) {
  cfg <- yaml::yaml.load(readr::read_file(path))
  if (is.null(cfg$current_pool)) {
    # Distinguish "ledger not yet created" (fall back to initial_pool) from
    # "ledger exists but is unreadable" (let the error surface). Pre-flight
    # the directory rather than swallowing read_table errors blindly.
    ledger_dir <- file.path(ledger_root, "decisions", "ledger")
    if (!dir.exists(ledger_dir)) {
      led <- tibble::tibble(pnl = numeric(0), settled = logical(0))
    } else {
      led <- read_table("ledger", root = ledger_root)
    }
    if (!all(c("pnl", "settled") %in% names(led))) {
      led <- tibble::tibble(pnl = numeric(0), settled = logical(0))
    }
    settled_pnl <- led$pnl[!is.na(led$settled) & led$settled]
    realised_pnl <- sum(settled_pnl, na.rm = TRUE)
    cfg$current_pool <- cfg$initial_pool + realised_pnl
    # WHY: the canonical ledger spans multiple bookmakers (Lengjan / EpicBet /
    # CoolBet) and tracks no deposits/withdrawals, so summing it is NOT the
    # real Lengjan bankroll. Setting current_pool explicitly in bankroll.yml is
    # the supported path; warn loudly when falling back here so the ~7x
    # over-stake bug (2026-06-05) cannot silently return.
    cli::cli_warn(c(
      "!" = "{.field current_pool} derived from the unscoped ledger ({.val {cfg$current_pool}} ISK).",
      "i" = "The ledger mixes bookmakers; set {.field current_pool} explicitly in bankroll.yml to the real Lengjan balance."
    ))
  }
  if (is.null(cfg$kelly_ceiling)) cfg$kelly_ceiling <- 0.25
  if (is.null(cfg$max_match_stake_default)) cfg$max_match_stake_default <- 1.0
  cfg
}

#' Filter a loaded leagues list by selector.
#'
#' @param leagues Named list from `load_leagues()`.
#' @param sport,country,league Optional filters.
#' @param active_only If TRUE, keep only leagues with `active = TRUE`.
#' @param has_lengjan If TRUE, drop leagues whose `lengjan$competitions` block
#'   is empty/missing — i.e. nothing to scrape from Lengjan.
#' @return Filtered named list.
#' @export
filter_leagues <- function(leagues, sport = NULL, country = NULL,
                           league = NULL, active_only = FALSE,
                           has_lengjan = FALSE) {
  keep <- rep(TRUE, length(leagues))
  names(keep) <- names(leagues)

  if (!is.null(sport)) {
    keep <- keep & vapply(leagues, function(l) identical(l$sport, sport), logical(1))
  }
  if (!is.null(country)) {
    keep <- keep & vapply(leagues, function(l) identical(l$country, country), logical(1))
  }
  if (!is.null(league)) {
    keep <- keep & (names(leagues) == league)
  }
  if (isTRUE(active_only)) {
    keep <- keep & vapply(leagues, function(l) isTRUE(l$active), logical(1))
  }
  if (isTRUE(has_lengjan)) {
    keep <- keep & vapply(leagues, function(l) {
      length(l$lengjan$competitions) > 0L
    }, logical(1))
  }

  leagues[keep]
}

#' Is betting enabled for a league?
#'
#' Reads `betting$enabled` from a league definition. The key is optional in
#' `config/leagues.schema.json`, so an absent key means enabled and only an
#' explicit `enabled: false` disarms a league. This is the single predicate
#' consulted by the odds-ingest guard ([ingest_one_lengjan()]), the decide
#' guard ([decide_league()]), the placer's recommendation loader
#' ([load_recommendations()]) and the placer pre-flight
#' ([validate_betting_enabled()]) -- so a league can never be disabled in one
#' layer while another stays armed.
#'
#' @param league A league definition (an element of [load_leagues()]), or any
#'   list carrying a `betting` slice.
#' @return `TRUE` unless `betting$enabled` is exactly `FALSE`.
#' @export
betting_enabled <- function(league) {
  !isFALSE(league$betting$enabled)
}
