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

  leagues <- normalise_betting_modes(leagues)

  if (isTRUE(validate)) {
    if (!file.exists(schema_path)) {
      stop("leagues.schema.json not found: ", schema_path, call. = FALSE)
    }
    validate_leagues(leagues, schema_path)
    check_team_names_injective(leagues)
    check_team_aliases(leagues)
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
    # A length-1 character vector is unboxed to a JSON scalar by
    # toJSON(auto_unbox = TRUE), so a single-element YAML sequence fails the
    # schema's "must be array". Both of these are free-length lists that can
    # legitimately hold exactly one code -- exclude_divisions does today
    # ([LD1_PO]) -- so both must be forced to a JSON array.
    if (!is.null(l$betting$exclude_divisions) &&
      !is.list(l$betting$exclude_divisions)) {
      l$betting$exclude_divisions <- as.list(l$betting$exclude_divisions)
    }
    if (!is.null(l$training_filter$divisions) &&
      !is.list(l$training_filter$divisions)) {
      l$training_filter$divisions <- as.list(l$training_filter$divisions)
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

#' Assert every league's per-sex team_aliases map settles in one pass.
#'
#' [.apply_team_aliases()] looks each name up once. A self-map (`A: A`) marks
#' rows stale that never move. A value that is itself a key makes ingest store
#' a name the store must not hold: a chain (`A: B`, `B: C`) then needs one
#' [renormalise_team_aliases()] run per link, and a cycle (`A: B`, `B: A`)
#' never settles. Checked per sex: the maps are independent, so a name may be
#' stored for one sex and an alias key for the other.
#' @noRd
check_team_aliases <- function(leagues) {
  for (key in names(leagues)) {
    by_sex <- leagues[[key]]$data_source$team_aliases
    for (sx in names(by_sex)) {
      map <- unlist(by_sex[[sx]])
      if (length(map) == 0L) next
      label <- paste0(key, " (", sx, ")")
      self <- names(map)[names(map) == map]
      if (length(self) > 0L) {
        cli::cli_abort(
          c(
            "{label} team_aliases has an alias that maps to itself:",
            "x" = "{.val {self}}"
          ),
          call = NULL
        )
      }
      linked <- map %in% names(map)
      if (any(linked)) {
        links <- paste0(names(map)[linked], " -> ", map[linked])
        cli::cli_abort(
          c(
            "{label} team_aliases maps to a name that is also an alias key:",
            "x" = "{links}",
            "i" = "Point every source spelling straight at the stored name."
          ),
          call = NULL
        )
      }
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

#' The betting ladder, lowest stage first (spec 2026-09-23 WS1).
#'
#' `off`: nothing. `scrape`: odds only. `paper`: + decide (candidates and
#' recommendations the placer never places). `manual`: + `place_bets.R`.
#' `auto`: + the unattended launchd placer.
#' @noRd
.BETTING_MODES <- c("off", "scrape", "paper", "manual", "auto")

#' A league's stage on the betting ladder.
#'
#' Reads `betting$mode`. Without it the legacy boolean decides: an explicit
#' `enabled: false` is `"off"`; anything else (absent key, `TRUE`, no betting
#' block) is `"auto"`, so football, which carries neither key, is unchanged.
#' The schema forbids setting both keys.
#'
#' @param league A league definition (an element of [load_leagues()]), or any
#'   list carrying a `betting` slice.
#' @return One of `"off"`, `"scrape"`, `"paper"`, `"manual"`, `"auto"`.
#' @export
betting_mode <- function(league) {
  mode <- league$betting$mode
  # YAML 1.1 reads an unquoted `off` as FALSE (see normalise_betting_modes()).
  if (isFALSE(mode)) {
    return("off")
  }
  if (!is.null(mode)) {
    if (!(is.character(mode) && length(mode) == 1L && mode %in% .BETTING_MODES)) {
      stop(
        "betting.mode must be one of: ", paste(.BETTING_MODES, collapse = ", "),
        call. = FALSE
      )
    }
    return(mode)
  }
  if (isFALSE(league$betting$enabled)) "off" else "auto"
}

#' Is a league at or above a stage of the betting ladder?
#'
#' Each layer asks for the stage it needs: odds ingest `"scrape"`, decide
#' `"paper"`, the placer `"manual"`, the unattended placer `"auto"`.
#'
#' @param league A league definition.
#' @param stage One of `"off"`, `"scrape"`, `"paper"`, `"manual"`, `"auto"`,
#'   matched exactly. Anything else (`NULL`, `NA`, a partial or vector value)
#'   is an error, so a gate fails closed rather than defaulting to `"off"`.
#' @return `TRUE` or `FALSE`.
#' @export
betting_mode_at_least <- function(league, stage) {
  if (!(is.character(stage) && length(stage) == 1L && !is.na(stage) && stage %in% .BETTING_MODES)) {
    stop(
      "betting_mode_at_least: stage must be one of: ",
      paste(.BETTING_MODES, collapse = ", "),
      call. = FALSE
    )
  }
  match(betting_mode(league), .BETTING_MODES) >= match(stage, .BETTING_MODES)
}

#' Does the decide layer run for a league?
#'
#' `betting.mode` at least `"paper"`. Kept under this name for readers that
#' predate the ladder -- [order_fit_targets()] fits these leagues first, which
#' is right for paper leagues too (their recommendations want fresh fits).
#'
#' @param league A league definition.
#' @return `TRUE` when decide runs for the league.
#' @export
betting_enabled <- function(league) {
  betting_mode_at_least(league, "paper")
}

#' Map a YAML-boolean `betting.mode` back to `"off"`.
#'
#' YAML 1.1 (the `yaml` package) reads an unquoted `mode: off` as logical
#' `FALSE` -- the "Norway problem". Undone before schema validation so a
#' hand-written `off` loads instead of failing the string schema.
#' @noRd
normalise_betting_modes <- function(leagues) {
  for (key in names(leagues)) {
    if (isFALSE(leagues[[key]]$betting$mode)) {
      leagues[[key]]$betting$mode <- "off"
    }
  }
  leagues
}
