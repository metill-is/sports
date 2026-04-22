#' Pre-flight config validation
#'
#' Cross-checks `lengjan-odds/config/competitions.yml` against
#' `lengjan-odds/config/team_names_*.csv` filenames and aborts on
#' misconfiguration before any browser work happens.
#'
#' Failure modes caught:
#'   - CSV on disk, no matching key in competitions.yml           (dead CSV)
#'   - CSV on disk, key present but `team_names:` field missing   (unwired CSV — the football_iceland bug)
#'   - CSV on disk, `team_names:` field points at different file  (mismatched wiring)
#'   - `team_names:` field set, file missing                      (dangling reference)

box::use(
  readr[read_file],
  yaml[yaml.load],
  cli[cli_abort, cli_alert_success]
)

#' Validate team-name configuration consistency.
#'
#' @param odds_dir Path to the lengjan-odds project root.
#' @param verbose If TRUE, prints a success line on pass.
#' @return invisible(TRUE) on success. Aborts (cli_abort) on any issue.
#' @export
validate_team_names_config <- function(odds_dir, verbose = FALSE) {
  config_dir <- file.path(odds_dir, "config")

  competitions <- yaml.load(
    read_file(file.path(config_dir, "competitions.yml"))
  )

  csv_files <- list.files(
    config_dir,
    pattern = "^team_names_.*\\.csv$",
    full.names = FALSE
  )

  issues <- character()

  # Direction A: every CSV on disk must be wired to a matching competition entry.
  for (csv in csv_files) {
    key <- sub("^team_names_(.*)\\.csv$", "\\1", csv)
    comp <- competitions[[key]]

    if (is.null(comp)) {
      issues <- c(issues, sprintf(
        "'%s' exists on disk but competitions.yml has no '%s' entry (dead CSV).",
        csv, key
      ))
      next
    }

    if (is.null(comp$team_names)) {
      issues <- c(issues, sprintf(
        "'%s' exists on disk but competitions.yml['%s'] has no 'team_names:' key (unwired — lengjan-bets will silently produce empty lookup and fail with no_match_id). Add `team_names: \"%s\"` under the '%s' entry.",
        csv, key, csv, key
      ))
      next
    }

    if (!identical(comp$team_names, csv)) {
      issues <- c(issues, sprintf(
        "'%s' exists on disk but competitions.yml['%s'].team_names points at '%s' (mismatched wiring).",
        csv, key, comp$team_names
      ))
    }
  }

  # Direction B: every team_names: reference must resolve to a real file.
  for (key in names(competitions)) {
    tn <- competitions[[key]]$team_names
    if (is.null(tn)) next
    if (!file.exists(file.path(config_dir, tn))) {
      issues <- c(issues, sprintf(
        "competitions.yml['%s'].team_names = '%s' but file is missing.",
        key, tn
      ))
    }
  }

  if (length(issues) > 0) {
    cli_abort(c(
      "Team-name config inconsistencies found (pre-flight check):",
      stats::setNames(issues, rep("x", length(issues))),
      "i" = "See lengjan-odds/CLAUDE.md ('Adding a new sport') and .claude/skills/add-league/SKILL.md (Step 8) for correct wiring."
    ))
  }

  if (verbose) {
    cli_alert_success(
      "Config valid: {length(csv_files)} team_names CSV(s) wired correctly."
    )
  }
  invisible(TRUE)
}
