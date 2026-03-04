#' Shared betting pipeline orchestrator
#'
#' Entry point for all sports. Reads config, finds the latest posterior,
#' loads odds, runs market modules, deduplicates, displays, and logs.
#'
#' Usage from a sport-specific run_bets.R:
#'   box::use(../../R/bets/run[run_betting_pipeline])
#'   cfg <- yaml::read_yaml(here::here("config", "bets.yml"))
#'   run_betting_pipeline(cfg, sport_dir = here::here())

box::use(
  ./market_1x2[run_1x2],
  ./market_handicap[run_handicap],
  ./market_totals[run_totals],
  ./kelly_joint[run_joint_kelly],
  ./odds[load_odds],
  ./output[print_market, make_clipboard_rows, write_to_clipboard,
           load_existing_bets, dedup, log_bets],
  readr[read_csv],
  dplyr[filter, bind_rows]
)

#' Find the latest posterior_goals.csv
#'
#' Checks two layouts:
#'   1. Date subdirs: {base_path}/{sex}/YYYY-MM-DD/posterior_goals.csv (picks latest)
#'   2. Direct: {base_path}/{sex}/posterior_goals.csv
#'
#' @param base_path Base results path (e.g., "results")
#' @param sex Sex subdirectory (e.g., "male")
#' @return Full path to the latest posterior_goals.csv, or NULL
find_latest_posterior <- function(base_path, sex) {
  # Try date subdirectories first
  pattern <- file.path(base_path, sex, "*", "posterior_goals.csv")
  candidates <- Sys.glob(pattern)

  if (length(candidates) > 0) {
    dirs <- basename(dirname(candidates))
    return(candidates[order(dirs, decreasing = TRUE)[1]])
  }

  # Fall back to direct file in sex directory
  direct <- file.path(base_path, sex, "posterior_goals.csv")
  if (file.exists(direct)) return(direct)

  NULL
}

#' Run the full betting pipeline for one sport
#'
#' @param cfg Config list from bets.yml
#' @param sport_dir Root directory of the sport project (absolute path)
#' @export
run_betting_pipeline <- function(cfg, sport_dir) {
  Sys.setlocale("LC_ALL", "is_IS.UTF-8")

  cat("=== Betting pipeline:", cfg$sport, "/", cfg$country, "===\n\n")

  # Load existing bets for dedup (once, shared across sexes)
  existing_bets <- load_existing_bets(cfg)

  # Collect all clipboard rows across sexes
  all_clipboard <- list()

  for (sex in cfg$sex) {
    cat("--- Sex:", sex, "---\n\n")

    # Resolve sex-specific kelly_frac (e.g., kelly_frac_male overrides kelly_frac)
    sex_key <- paste0("kelly_frac_", sex)
    effective_kf <- cfg$bankroll[[sex_key]] %||% cfg$bankroll$kelly_frac
    cfg_sex <- cfg
    cfg_sex$bankroll$kelly_frac <- effective_kf
    cat("  Kelly fraction:", effective_kf, "\n")

    # 1. Find latest posterior
    base_path <- file.path(sport_dir, cfg$predictions$path)
    post_path <- find_latest_posterior(base_path, sex)

    if (is.null(post_path)) {
      cat("  No posterior found at", base_path, "/", sex, "/*/posterior_goals.csv\n")
      cat("  Run the model first.\n\n")
      next
    }

    cat("  Posterior:", post_path, "\n")

    # Freshness warning
    post_age <- difftime(Sys.time(), file.info(post_path)$mtime, units = "hours")
    max_age <- cfg$predictions$max_age_hours %||% 48
    if (as.numeric(post_age) > max_age) {
      cat("  Warning: posterior is", round(as.numeric(post_age)),
          "hours old (threshold:", max_age, "h).\n")
    }

    # 2. Load and filter posterior
    post <- read_csv(post_path, show_col_types = FALSE) |>
      filter(date >= Sys.Date())

    if (nrow(post) == 0) {
      cat("  No future games in posterior. Skipping.\n\n")
      next
    }
    cat("  Found", length(unique(paste(post$home, post$away))),
        "future matches in posterior.\n")

    # 3. Load odds
    odds <- load_odds(cfg, sport_dir, sex = sex)

    # 4. Run enabled markets
    res_1x2 <- NULL
    res_hc <- NULL
    res_tot <- NULL

    kelly_mode <- cfg_sex$bankroll$kelly_mode %||% "independent"

    if (kelly_mode == "joint") {
      cat("  Using joint Kelly optimisation\n")
      joint_res <- run_joint_kelly(
        post, odds$outcome, odds$handicap, odds$totals, cfg_sex
      )
      if (!is.null(joint_res)) {
        res_1x2 <- joint_res |> filter(market == "outcome")
        res_hc  <- joint_res |> filter(market == "handicap")
        res_tot <- joint_res |> filter(market == "totals")
        if (nrow(res_1x2) == 0) res_1x2 <- NULL
        if (nrow(res_hc) == 0)  res_hc  <- NULL
        if (nrow(res_tot) == 0) res_tot <- NULL
      }
      res_1x2 <- dedup(res_1x2, existing_bets, "Niðurstaða")
      res_hc  <- dedup(res_hc, existing_bets, "Forgjöf")
      res_tot <- dedup(res_tot, existing_bets, "Markafjöldi")
    } else {
      if (isTRUE(cfg_sex$markets$outcome)) {
        res_1x2 <- run_1x2(post, odds$outcome, cfg_sex)
        res_1x2 <- dedup(res_1x2, existing_bets, "Niðurstaða")
      }

      if (isTRUE(cfg_sex$markets$handicap)) {
        res_hc <- run_handicap(post, odds$handicap, cfg_sex)
        res_hc <- dedup(res_hc, existing_bets, "Forgjöf")
      }

      if (isTRUE(cfg_sex$markets$totals)) {
        res_tot <- run_totals(post, odds$totals, cfg_sex)
        res_tot <- dedup(res_tot, existing_bets, "Markafjöldi")
      }
    }

    # 5. Display
    print_market(res_1x2, paste0("1x2 (Niðurstaða) [", sex, "]"))
    print_market(res_hc, paste0("Handicap (Forgjöf) [", sex, "]"))
    print_market(res_tot, paste0("Totals (Markafjöldi) [", sex, "]"))

    # 6. Log bets to history
    log_bets(res_1x2, cfg_sex, sport_dir, sex, "outcome")
    log_bets(res_hc, cfg_sex, sport_dir, sex, "handicap", info_col = "change")
    log_bets(res_tot, cfg_sex, sport_dir, sex, "totals", info_col = "limit")

    # 7. Clipboard rows
    all_clipboard <- c(all_clipboard, list(
      make_clipboard_rows(res_1x2, "Niðurstaða"),
      make_clipboard_rows(res_hc, "Forgjöf", info_col = "change"),
      make_clipboard_rows(res_tot, "Markafjöldi", info_col = "limit")
    ))

    cat("\n")
  }

  # 8. Write combined clipboard
  clipboard_rows <- bind_rows(all_clipboard)
  write_to_clipboard(clipboard_rows)

  invisible(clipboard_rows)
}
