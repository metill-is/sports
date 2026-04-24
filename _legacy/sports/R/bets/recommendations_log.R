#' Pre-filter recommendations log
#'
#' WHY THIS EXISTS
#' ---------------
#' `recommendations.csv` only contains bets that survived every gate: EV
#' threshold, Kelly floor, minimum bet amount, daily budget, per-league
#' cap, already-placed dedup, date cut-off. That's a convenient view for
#' placing bets *today*, but it is a lossy view for answering the question
#' "what if our filters were different?"
#'
#' Counterfactual back-tests need the raw pre-filter candidate set: what
#' bets did the model flag before any gate fired? Without this, you can
#' only ever sharpen the current policy against itself. Logging every
#' candidate at each pipeline stage gives you the evidence to tune the
#' gates after the fact instead of guessing.
#'
#' SCHEMA
#' ------
#' Written to `history/recommendations_log.csv` at the Sports/ root. One
#' row per (candidate, stage). `stage` takes the values:
#'
#'   "candidate"     — raw output of the betting pipeline (pre-portfolio,
#'                     pre-filter). This is the authoritative "bets the
#'                     model considered" set for this run.
#'   "post_portfolio" — kelly values after portfolio optimisation,
#'                     per-league cap and kelly_frac scaling. Rows that
#'                     survive are the ones about to hit bet_amount and
#'                     dedup gates.
#'   "kept"          — bets that made it into recommendations.csv for this
#'                     run. Subset of "post_portfolio".
#'   "dropped_min_bet"  — dropped because bet_amount < min_bet_amount
#'   "dropped_stale"    — dropped because match date < today
#'   "dropped_dedup"    — dropped because already in bets_log.csv
#'
#' All rows share a `run_id` (ISO timestamp) so queries can reconstruct a
#' single run. Run again, append again — never overwrite, never dedupe at
#' write time.
#'
#' APPEND-ONLY
#' -----------
#' The file grows roughly linearly with runs × candidates per run. Back of
#' envelope: 6 active buckets × ~5 candidates per run × 3 runs per day ×
#' 365 = ~33k rows per year. CSV is fine for a decade before it needs
#' partitioning.

box::use(
  readr[read_csv, write_csv],
  dplyr[bind_rows, mutate, any_of],
  tibble[tibble],
  here[here]
)

LOG_PATH_REL <- c("history", "recommendations_log.csv")

.log_path <- function(sports_dir = NULL) {
  if (is.null(sports_dir)) sports_dir <- here()
  dir <- file.path(sports_dir, LOG_PATH_REL[1])
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  file.path(sports_dir, LOG_PATH_REL[1], LOG_PATH_REL[2])
}

#' Generate a fresh run_id for a pipeline run
#'
#' Uses an ISO 8601 timestamp in UTC. Collisions are impossible at normal
#' operator cadence (one run per second would require intent).
#'
#' @export
new_run_id <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

#' Append recommendation rows to the log
#'
#' @param rows Tibble of candidate rows. Expected columns match
#'   `recommendations.csv` (sport, country, sex, date, heima, gestir,
#'   market, outcome, o, p, ev, kelly, bet_amount, limit, booker). Any
#'   missing column is written as NA — the log is permissive on schema
#'   drift so that early-stage rows (no bet_amount yet) also fit.
#' @param stage One of "candidate", "post_portfolio", "kept",
#'   "dropped_min_bet", "dropped_stale", "dropped_dedup".
#' @param run_id Shared identifier for this run (see `new_run_id()`).
#' @param sports_dir Absolute path to the Sports/ root; defaults to here().
#' @export
log_candidates <- function(rows, stage, run_id, sports_dir = NULL) {
  if (is.null(rows) || nrow(rows) == 0) {
    return(invisible(NULL))
  }

  # Standardise column names and add metadata columns. Any column missing
  # from `rows` is filled with NA of the appropriate type so that schema
  # is stable across stages.
  standard_cols <- c(
    "sport", "country", "sex", "date", "heima", "gestir",
    "market", "outcome", "o", "p", "ev", "kelly",
    "bet_amount", "limit", "booker"
  )
  for (col in standard_cols) {
    if (!col %in% names(rows)) rows[[col]] <- NA
  }

  rows <- rows |>
    mutate(
      run_id = run_id,
      stage = stage,
      logged_at = Sys.time()
    )

  rows <- rows[, c("run_id", "logged_at", "stage", standard_cols)]

  path <- .log_path(sports_dir)
  append <- file.exists(path)
  write_csv(rows, path, append = append)

  invisible(path)
}

#' Read the recommendations log
#'
#' @param sports_dir Optional override for Sports/ root.
#' @param run_id Optional: filter to a single run.
#' @param stage Optional: filter to one stage.
#' @return Tibble (may be empty if log has not been written yet).
#' @export
read_recommendations_log <- function(sports_dir = NULL, run_id = NULL, stage = NULL) {
  path <- .log_path(sports_dir)
  if (!file.exists(path)) {
    return(tibble())
  }
  d <- read_csv(path, show_col_types = FALSE)
  if (!is.null(run_id)) d <- d[d$run_id == run_id, , drop = FALSE]
  if (!is.null(stage)) d <- d[d$stage == stage, , drop = FALSE]
  d
}
