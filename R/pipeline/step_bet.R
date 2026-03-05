#' Step: Bet — run betting pipeline for a league
#'
#' Reads the per-league config/bets.yml and delegates to the
#' shared run_betting_pipeline() which handles sex iteration internally.
#'
#' Calibration: kelly_frac values are overridden at runtime from
#' historical performance via compute_calibration() (rules K1–K6).
#'
#' @usage
#' box::use(R/pipeline/step_bet[run_bet_step])
#' run_bet_step(league, sports_dir)

box::use(
  R/bets/run[run_betting_pipeline],
  R/bets/output[compute_bankroll],
  R/bets/calibration[compute_calibration],
  yaml[yaml.load],
  readr[read_file]
)

#' @param league League config list from leagues.yml
#' @param sports_dir Absolute path to Sports/ root
#' @export
run_bet_step <- function(league, sports_dir) {
  if (!isTRUE(league$has_bets)) {
    cat("  Skipping bets (has_bets: false)\n")
    return(invisible(NULL))
  }

  league_dir <- file.path(sports_dir, league$dir)
  bets_yml <- file.path(league_dir, "config", "bets.yml")

  if (!file.exists(bets_yml)) {
    cat("  Skipping bets (no config/bets.yml found)\n")
    return(invisible(NULL))
  }

  # Read bets config (UTF-8 safe)
  Sys.setlocale("LC_ALL", "is_IS.UTF-8")
  cfg <- yaml.load(read_file(bets_yml))

  # Merge global bankroll defaults (local values override global)
  bankroll_yml <- file.path(sports_dir, "config", "bankroll.yml")
  if (file.exists(bankroll_yml)) {
    global_bankroll <- yaml.load(read_file(bankroll_yml))
    # Global provides defaults; local bets.yml bankroll fields take precedence
    merged <- global_bankroll
    merged[names(cfg$bankroll)] <- cfg$bankroll
    cfg$bankroll <- merged
  }

  # Compute cur_pool dynamically from initial_pool and bet history
  if (!is.null(cfg$bankroll$initial_pool)) {
    cfg$bankroll$cur_pool <- compute_bankroll(cfg$bankroll$initial_pool, sports_dir)
    cat(sprintf("  Bankroll: %s %s (initial: %s)\n",
        cfg$bankroll$cur_pool, cfg$bankroll$currency, cfg$bankroll$initial_pool))
  }

  # Override kelly_frac with live calibration from settled bets (K1–K6)
  cal <- compute_calibration(
    sports_dir = sports_dir,
    sport = cfg$sport,
    country = cfg$country,
    sexes = cfg$sex
  )
  for (sex in cfg$sex) {
    sex_key <- paste0("kelly_frac_", sex)
    if (!is.null(cal[[sex_key]])) {
      cfg$bankroll[[sex_key]] <- cal[[sex_key]]
    }
  }

  run_betting_pipeline(cfg, sport_dir = league_dir)
}
