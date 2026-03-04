#' Step: Settle — resolve unsettled bets against actual results
#'
#' Reads per-league bets_log.csv, matches against results data,
#' and updates win/pnl for completed matches.
#'
#' @usage
#' box::use(R/pipeline/step_settle[run_settle_step])
#' run_settle_step(league, sports_dir)

box::use(
  R/bets/settle[settle_league]
)

#' @param league League config list from leagues.yml
#' @param sports_dir Absolute path to Sports/ root
#' @export
run_settle_step <- function(league, sports_dir) {
  if (!isTRUE(league$has_bets)) {
    cat("  Skipping settle (has_bets: false)\n")
    return(invisible(NULL))
  }

  league_dir <- file.path(sports_dir, league$dir)
  log_path <- file.path(league_dir, "history", "bets_log.csv")

  if (!file.exists(log_path)) {
    cat("  Skipping settle (no history/bets_log.csv)\n")
    return(invisible(NULL))
  }

  settle_league(league_dir, sexes = league$sex)
}
