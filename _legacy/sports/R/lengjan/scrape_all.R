#' Orchestrator: scrape all Lengjan odds for a sport and save CSVs
#'
#' Two-pass approach:
#'   Pass 1: scrape_competition() for each sex/league → 1x2 CSVs (fast)
#'   Pass 2: scrape_match_detail() for each match URL → handicap/totals CSVs (slow)
#'
#' Output lands in {sport_dir}/odds/iceland/{sex}/ to match existing paths.

#' @export
#' @param sport_name Character: key into config registry (e.g., "handball_iceland")
#' @return List of all results (invisibly)
scrape_all_odds <- function(sport_name) {
  box::use(
    R/lengjan/config[get_config],
    R/lengjan/scrape[scrape_competition, scrape_match_detail, sleep_between_pages],
    readr[write_csv],
    dplyr[bind_rows]
  )

  config <- get_config(sport_name)
  sport_dir <- config$sport_dir

  message(
    "=== Lengjan odds scraper ===\n",
    "Sport: ", sport_name, "\n",
    "Time: ", Sys.time(), "\n"
  )

  all_results <- list()

  for (sex in config$sexes) {
    leagues <- config[[sex]]$leagues
    message("\n--- ", sex, " (", length(leagues), " league(s)) ---")

    # Ensure output directory exists (use file.path relative to getwd() = Sports/)
    out_dir <- file.path(sport_dir, "odds", "iceland", sex)
    if (!dir.exists(out_dir)) {
      dir.create(out_dir, recursive = TRUE)
    }

    sex_results <- list()

    for (i in seq_along(leagues)) {
      league <- leagues[[i]]
      league_label <- if (!is.null(league$label)) league$label else paste0("league_", i)
      message("\n[", i, "/", length(leagues), "] ", league_label)

      # --- Pass 1: Competition page → 1x2 odds ---
      comp_result <- tryCatch(
        scrape_competition(config$sport, config$country, league$competition),
        error = function(e) {
          message("  FAILED: ", e$message)
          NULL
        }
      )

      if (is.null(comp_result) || nrow(comp_result$odds_1x2) == 0 ||
          all(is.na(comp_result$odds_1x2$dates))) {
        message("  No valid 1x2 odds found, skipping")
        next
      }

      # Save 1x2 CSV (same format as existing league_N.csv)
      csv_path <- file.path(out_dir, paste0("league_", i, ".csv"))
      write_csv(comp_result$odds_1x2, csv_path)
      message("  Saved 1x2: ", csv_path, " (", nrow(comp_result$odds_1x2), " rows)")

      # --- Pass 2: Match detail pages → handicap + totals ---
      handicap_all <- list()
      totals_all <- list()

      if (length(comp_result$match_urls) > 0) {
        message("  Scraping ", length(comp_result$match_urls), " match detail pages...")

        for (j in seq_along(comp_result$match_urls)) {
          match_url <- comp_result$match_urls[j]

          # Get corresponding match info for labelling
          match_home <- if (j <= nrow(comp_result$odds_1x2)) comp_result$odds_1x2$home[j] else NA_character_
          match_away <- if (j <= nrow(comp_result$odds_1x2)) comp_result$odds_1x2$away[j] else NA_character_
          match_date <- if (j <= nrow(comp_result$odds_1x2)) comp_result$odds_1x2$dates[j] else NA

          message("  [", j, "/", length(comp_result$match_urls), "] ",
                  match_home, " v ", match_away)

          sleep_between_pages()

          detail <- tryCatch(
            scrape_match_detail(match_url, match_home, match_away, match_date),
            error = function(e) {
              message("    Detail scrape failed: ", e$message)
              list(handicap = NULL, totals = NULL)
            }
          )

          if (!is.null(detail$handicap)) handicap_all[[j]] <- detail$handicap
          if (!is.null(detail$totals)) totals_all[[j]] <- detail$totals
        }
      }

      # Save handicap CSV
      if (length(handicap_all) > 0) {
        handicap_df <- bind_rows(handicap_all)
        hc_path <- file.path(out_dir, paste0("league_", i, "_handicap.csv"))
        write_csv(handicap_df, hc_path)
        message("  Saved handicap: ", hc_path, " (", nrow(handicap_df), " rows)")
      } else {
        message("  No handicap odds found (detail selectors may need tuning)")
      }

      # Save totals CSV
      if (length(totals_all) > 0) {
        totals_df <- bind_rows(totals_all)
        tot_path <- file.path(out_dir, paste0("league_", i, "_totals.csv"))
        write_csv(totals_df, tot_path)
        message("  Saved totals: ", tot_path, " (", nrow(totals_df), " rows)")
      } else {
        message("  No totals odds found (detail selectors may need tuning)")
      }

      sex_results[[i]] <- list(
        league = league_label,
        odds_1x2 = comp_result$odds_1x2,
        match_urls = comp_result$match_urls,
        handicap = if (length(handicap_all) > 0) bind_rows(handicap_all) else NULL,
        totals = if (length(totals_all) > 0) bind_rows(totals_all) else NULL
      )

      # Rate limit between leagues
      if (i < length(leagues)) sleep_between_pages()
    }

    all_results[[sex]] <- sex_results
  }

  message("\n=== Done (", Sys.time(), ") ===")

  invisible(all_results)
}
