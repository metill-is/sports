# R/backtest-universe.R
#' @include storage.R
NULL

# The 2026-05-13 spread sign-flip fix landed for decide runs on/after this date.
# Earlier runs carry contaminated spread probabilities/EV that current code
# cannot reproduce (forensic review 2026-05-30) -- excluded from the backtest by
# default so the headline reflects the code that actually runs.
bt_spread_fix_date <- function() as.Date("2026-05-14")

bt_universe_cols <- function() {
  c(
    "run_date", "run_id", "sport", "country", "sex", "match_date",
    "home_team", "away_team", "market", "outcome", "line",
    "p", "odds", "ev", "kelly_raw", "kelly", "bet_amount_recorded",
    "stage", "strategy"
  )
}

bt_empty_universe <- function() {
  tibble::tibble(
    run_date = as.Date(character()), run_id = character(),
    sport = character(), country = character(), sex = character(),
    match_date = as.Date(character()), home_team = character(),
    away_team = character(), market = character(), outcome = character(),
    line = numeric(), p = numeric(), odds = numeric(), ev = numeric(),
    kelly_raw = numeric(), kelly = numeric(),
    bet_amount_recorded = numeric(), stage = character(),
    strategy = character()
  )
}

#' Load the backtest bet universe from stored decisions.
#'
#' Reads the leak-free `candidates` store (every evaluated market per decide
#' run, with `p`/`odds`/`ev`/`kelly_raw` frozen at decide-time) and left-joins
#' `recommendations` to attach the recorded effective Kelly fraction (`kelly`)
#' and stake (`bet_amount`) for bets that were actually kept. The `strategy`
#' argument selects the bet subset to evaluate -- the strategy *is* the filter.
#'
#' @param root Data root (default `here::here("data")`).
#' @param strategy One of `"kept"` (our actual picks; default), `"positive_ev"`
#'   (every candidate with `ev > 0`), `"all"` (every candidate).
#' @param leagues Optional character vector of `sport` values to keep.
#' @param sex Optional character vector of `sex` values to keep.
#' @param from,to Optional `Date` or `YYYY-MM-DD` bounds on `run_date`.
#' @param exclude_pre_fix If `TRUE` (default), drop decide runs before the
#'   2026-05-13 spread sign-flip fix (`run_date < bt_spread_fix_date()`); those
#'   bets are bug-contaminated and not reproducible by current code. Set `FALSE`
#'   to include the full (contaminated) history.
#' @return Tibble, one row per bet (see `bt_universe_cols()`); empty-with-columns
#'   if nothing matches.
#' @export
bt_load_universe <- function(root = here::here("data"),
                             strategy = c("kept", "positive_ev", "all"),
                             leagues = NULL, sex = NULL,
                             from = NULL, to = NULL,
                             exclude_pre_fix = TRUE) {
  strategy <- match.arg(strategy)

  cand <- tryCatch(read_table("candidates", root = root),
    error = function(e) NULL
  )
  if (is.null(cand) || nrow(cand) == 0L) {
    return(bt_empty_universe())
  }
  # read_table returns the hive-partition run_date as character; make it a real
  # Date so ordering, from/to filters, and the rolling pool walk are typed.
  cand$run_date <- as.Date(cand$run_date)

  recs <- tryCatch(read_table("recommendations", root = root),
    error = function(e) NULL
  )

  join_key <- c(
    "run_id", "sport", "country", "sex", "match_date",
    "home_team", "away_team", "market", "outcome", "line"
  )
  if (!is.null(recs) && nrow(recs) > 0L) {
    rec_slim <- dplyr::rename(
      recs[, c(join_key, "kelly", "bet_amount")],
      bet_amount_recorded = "bet_amount"
    )
    cand <- dplyr::left_join(cand, rec_slim, by = join_key)
  } else {
    cand$kelly <- NA_real_
    cand$bet_amount_recorded <- NA_real_
  }

  cand <- switch(strategy,
    kept = cand[cand$stage == "kept", , drop = FALSE],
    positive_ev = cand[cand$ev > 0, , drop = FALSE],
    all = cand
  )
  cand$strategy <- strategy

  if (!is.null(leagues)) cand <- cand[cand$sport %in% leagues, , drop = FALSE]
  if (!is.null(sex)) cand <- cand[cand$sex %in% sex, , drop = FALSE]
  if (!is.null(from)) cand <- cand[cand$run_date >= as.Date(from), , drop = FALSE]
  if (!is.null(to)) cand <- cand[cand$run_date <= as.Date(to), , drop = FALSE]

  # Drop the spread sign-flip bug era by default (forensic review 2026-05-30):
  # those candidates' contaminated EV inflated the headline, and the whole
  # pre-fix slate sizing is suspect because the buggy spreads co-sized with
  # other bets. Opt back in with exclude_pre_fix = FALSE.
  if (isTRUE(exclude_pre_fix)) {
    cand <- cand[cand$run_date >= bt_spread_fix_date(), , drop = FALSE]
  }

  # A bet recommended on several decide runs was placed once (placer P1
  # idempotency). Dedup to one row per unique bet, keeping the earliest run --
  # when it would first have been placed -- so multi-day recommendations are
  # not counted multiple times.
  ord <- order(cand$run_date)
  cand <- cand[ord, , drop = FALSE]
  bet_key <- paste(cand$sport, cand$country, cand$sex, cand$match_date,
    cand$home_team, cand$away_team, cand$market,
    cand$outcome, cand$line,
    sep = "\r"
  )
  cand <- cand[!duplicated(bet_key), , drop = FALSE]

  if (nrow(cand) == 0L) {
    return(bt_empty_universe())
  }
  cand[, bt_universe_cols(), drop = FALSE]
}
