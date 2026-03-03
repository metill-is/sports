#' Pipeline functions for {targets}
#'
#' These are the top-level functions called by _targets.R.
#' Each function is a single target in the pipeline.

#' Load competition config from YAML
#' @param path Path to competitions.yml
#' @return Named list of competition configs
load_competitions <- function(path) {
  yaml::read_yaml(path)
}

#' Scrape all odds for all competitions in config
#' @param config List from load_competitions()
#' @return Named list: $odds_1x2, $odds_handicap, $odds_totals (each a tibble)
scrape_all <- function(config) {
  team_names <- readr::read_csv(
    here::here("config", "team_names.csv"),
    show_col_types = FALSE
  )

  odds_1x2_all <- list()
  handicap_all <- list()
  totals_all <- list()

  for (league_name in names(config$leagues)) {
    league <- config$leagues[[league_name]]
    competition <- league$competition
    message("\n=== ", league_name, " (competition ", competition, ") ===")

    # Stage 1: competition page -> 1x2 + match URLs
    comp <- tryCatch(
      scrape_competition(config$sport, config$country, competition),
      error = function(e) {
        message("  FAILED: ", conditionMessage(e))
        message("  ", paste(capture.output(traceback()), collapse = "\n  "))
        NULL
      }
    )

    if (is.null(comp) || nrow(comp$odds_1x2) == 0) {
      message("  No matches, skipping")
      next
    }

    odds_1x2_all[[league_name]] <- comp$odds_1x2 |>
      dplyr::mutate(league = league_name)

    # Stage 2: match detail pages -> handicap + totals
    if (length(comp$match_urls) > 0) {
      message("  Scraping ", length(comp$match_urls), " match detail pages...")

      for (j in seq_along(comp$match_urls)) {
        match_url <- comp$match_urls[j]
        match_home <- if (j <= nrow(comp$odds_1x2)) comp$odds_1x2$home[j] else NA_character_
        match_away <- if (j <= nrow(comp$odds_1x2)) comp$odds_1x2$away[j] else NA_character_
        match_date <- if (j <= nrow(comp$odds_1x2)) comp$odds_1x2$dates[j] else NA

        message(
          "  [", j, "/", length(comp$match_urls), "] ",
          match_home, " v ", match_away
        )

        sleep_between_pages()

        detail <- tryCatch(
          scrape_match_detail(match_url, match_home, match_away, match_date),
          error = function(e) {
            message("    Detail scrape failed: ", e$message)
            list(handicap = NULL, totals = NULL)
          }
        )

        if (!is.null(detail$handicap)) {
          handicap_all[[length(handicap_all) + 1]] <- detail$handicap |>
            dplyr::mutate(league = league_name)
        }
        if (!is.null(detail$totals)) {
          totals_all[[length(totals_all) + 1]] <- detail$totals |>
            dplyr::mutate(league = league_name)
        }
      }
    }

    sleep_between_pages()
  }

  # Standardise team names
  standardise_teams <- function(d) {
    d |>
      dplyr::mutate(dplyr::across(c(home, away), stringr::str_squish)) |>
      dplyr::inner_join(team_names, by = dplyr::join_by("home" == "in")) |>
      dplyr::select(-home) |>
      dplyr::rename(home = out) |>
      dplyr::inner_join(team_names, by = dplyr::join_by("away" == "in")) |>
      dplyr::select(-away) |>
      dplyr::rename(away = out)
  }

  # Build output tibbles
  odds_1x2 <- if (length(odds_1x2_all) > 0) {
    dplyr::bind_rows(odds_1x2_all) |>
      dplyr::mutate(
        dplyr::across(c(o_home, o_draw, o_away), as.numeric)
      ) |>
      standardise_teams() |>
      dplyr::select(
        date = dates, league, home, away, o_home, o_draw, o_away
      ) |>
      dplyr::arrange(date, league)
  } else {
    tibble::tibble()
  }

  odds_handicap <- if (length(handicap_all) > 0) {
    dplyr::bind_rows(handicap_all) |>
      dplyr::mutate(
        dplyr::across(c(o_home, o_away), as.numeric),
        o_draw = as.numeric(o_draw)
      ) |>
      standardise_teams() |>
      dplyr::rename(date = dates, change = line) |>
      dplyr::select(
        date, league, home, away, change, o_home, o_draw, o_away
      ) |>
      dplyr::arrange(date, league)
  } else {
    tibble::tibble()
  }

  odds_totals <- if (length(totals_all) > 0) {
    dplyr::bind_rows(totals_all) |>
      dplyr::mutate(
        dplyr::across(c(o_over, o_under), as.numeric)
      ) |>
      standardise_teams() |>
      dplyr::rename(date = dates) |>
      dplyr::select(
        date, league, home, away, limit, o_over, o_under
      ) |>
      dplyr::arrange(date, league)
  } else {
    tibble::tibble()
  }

  list(
    odds_1x2 = odds_1x2,
    odds_handicap = odds_handicap,
    odds_totals = odds_totals
  )
}

#' Accumulate new odds into existing parquet files
#'
#' Reads existing data, appends new rows with scraped_at timestamp,
#' deduplicates (same match + same odds = duplicate scrape), writes back.
#'
#' @param new_odds List from scrape_all()
#' @param data_dir Path to data directory
#' @return Character vector of file paths written (for targets file tracking)
accumulate_odds <- function(new_odds, data_dir = here::here("data", "football_england")) {
  if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

  scraped_at <- Sys.time()
  files_written <- character()

  # --- 1x2 ---
  path_1x2 <- file.path(data_dir, "odds_1x2.csv")
  if (nrow(new_odds$odds_1x2) > 0) {
    new <- new_odds$odds_1x2 |> dplyr::mutate(scraped_at = scraped_at)

    existing <- if (file.exists(path_1x2)) {
      readr::read_csv(path_1x2, show_col_types = FALSE) |>
        dplyr::mutate(scraped_at = as.POSIXct(scraped_at))
    } else {
      tibble::tibble()
    }

    combined <- dplyr::bind_rows(existing, new) |>
      dplyr::distinct(date, league, home, away, o_home, o_draw, o_away,
                      .keep_all = TRUE) |>
      dplyr::arrange(date, league, home)

    readr::write_csv(combined, path_1x2)
    files_written <- c(files_written, path_1x2)
    message("1x2: ", nrow(new), " scraped, ", nrow(combined), " total after dedup")
  }

  # --- Handicap ---
  path_hc <- file.path(data_dir, "odds_handicap.csv")
  if (nrow(new_odds$odds_handicap) > 0) {
    new <- new_odds$odds_handicap |> dplyr::mutate(scraped_at = scraped_at)

    existing <- if (file.exists(path_hc)) {
      readr::read_csv(path_hc, show_col_types = FALSE) |>
        dplyr::mutate(scraped_at = as.POSIXct(scraped_at))
    } else {
      tibble::tibble()
    }

    combined <- dplyr::bind_rows(existing, new) |>
      dplyr::distinct(date, league, home, away, change, o_home, o_draw, o_away,
                      .keep_all = TRUE) |>
      dplyr::arrange(date, league, home)

    readr::write_csv(combined, path_hc)
    files_written <- c(files_written, path_hc)
    message("Handicap: ", nrow(new), " scraped, ", nrow(combined), " total after dedup")
  }

  # --- Totals ---
  path_tot <- file.path(data_dir, "odds_totals.csv")
  if (nrow(new_odds$odds_totals) > 0) {
    new <- new_odds$odds_totals |> dplyr::mutate(scraped_at = scraped_at)

    existing <- if (file.exists(path_tot)) {
      readr::read_csv(path_tot, show_col_types = FALSE) |>
        dplyr::mutate(scraped_at = as.POSIXct(scraped_at))
    } else {
      tibble::tibble()
    }

    combined <- dplyr::bind_rows(existing, new) |>
      dplyr::distinct(date, league, home, away, limit, o_over, o_under,
                      .keep_all = TRUE) |>
      dplyr::arrange(date, league, home)

    readr::write_csv(combined, path_tot)
    files_written <- c(files_written, path_tot)
    message("Totals: ", nrow(new), " scraped, ", nrow(combined), " total after dedup")
  }

  files_written
}
