#' Scrape all Lengjan odds for English football
#'
#' Three-market pipeline: 1x2, handicap, and total goals.
#' Uses the shared R/lengjan/ modules for scraping.
#'
#' Output:
#'   data/odds.csv             — 1x2 odds (all leagues)
#'   data/odds_handicap.csv    — handicap odds (all leagues)
#'   data/odds_totals.csv      — over/under odds (all leagues)

box::use(
  R/common/lengjan_info,
  R/common/scrape_1x2[get_odds, get_match_detail],
  R/lengjan/scrape[sleep_between_pages],
  dplyr[mutate, select, rename, inner_join, filter, bind_rows, arrange,
        join_by, across],
  readr[read_csv, write_csv],
  stringr[str_squish]
)

# --- Config -----------------------------------------------------------------

sport <- lengjan_info$england$sport
country_code <- lengjan_info$england$country
leagues <- lengjan_info$england$leagues
team_names <- read_csv("data/england/team_names.csv", show_col_types = FALSE)

# Helper: standardise team names via the lookup table
standardise_teams <- function(d) {
  d |>
    mutate(across(c(home, away), str_squish)) |>
    inner_join(team_names, by = join_by("home" == "in")) |>
    select(-home) |>
    rename(home = out) |>
    inner_join(team_names, by = join_by("away" == "in")) |>
    select(-away) |>
    rename(away = out)
}

# --- Scrape -----------------------------------------------------------------

odds_1x2_all <- list()
handicap_all <- list()
totals_all <- list()

for (league_name in names(leagues)) {
  competition <- leagues[[league_name]]$competition
  message("\n=== ", league_name, " (competition ", competition, ") ===")

  # Stage 1: competition page → 1x2 + match URLs
  comp <- tryCatch(
    get_odds(sport, country_code, competition),
    error = function(e) {
      message("  FAILED: ", e$message)
      NULL
    }
  )

  if (is.null(comp) || nrow(comp$odds_1x2) == 0) {
    message("  No matches, skipping")
    next
  }

  odds_1x2_all[[league_name]] <- comp$odds_1x2 |>
    mutate(league = league_name)

  # Stage 2: match detail pages → handicap + totals
  if (length(comp$match_urls) > 0) {
    message("  Scraping ", length(comp$match_urls), " match detail pages...")

    for (j in seq_along(comp$match_urls)) {
      match_url <- comp$match_urls[j]
      match_home <- if (j <= nrow(comp$odds_1x2)) comp$odds_1x2$home[j] else NA_character_
      match_away <- if (j <= nrow(comp$odds_1x2)) comp$odds_1x2$away[j] else NA_character_
      match_date <- if (j <= nrow(comp$odds_1x2)) comp$odds_1x2$dates[j] else NA

      message("  [", j, "/", length(comp$match_urls), "] ",
              match_home, " v ", match_away)

      sleep_between_pages()

      detail <- tryCatch(
        get_match_detail(match_url, match_home, match_away, match_date),
        error = function(e) {
          message("    Detail scrape failed: ", e$message)
          list(handicap = NULL, totals = NULL)
        }
      )

      if (!is.null(detail$handicap)) {
        handicap_all[[length(handicap_all) + 1]] <- detail$handicap |>
          mutate(league = league_name)
      }
      if (!is.null(detail$totals)) {
        totals_all[[length(totals_all) + 1]] <- detail$totals |>
          mutate(league = league_name)
      }
    }
  }

  # Rate limit between leagues
  sleep_between_pages()
}

# --- Output -----------------------------------------------------------------

# 1x2 odds
if (length(odds_1x2_all) > 0) {
  odds_1x2_df <- bind_rows(odds_1x2_all) |>
    mutate(
      across(c(o_home, o_draw, o_away), as.numeric),
      country = "england"
    ) |>
    standardise_teams() |>
    select(date = dates, country, league, home, away, o_home, o_draw, o_away) |>
    arrange(date, league)

  write_csv(odds_1x2_df, "data/odds.csv")
  message("\nSaved 1x2: data/odds.csv (", nrow(odds_1x2_df), " rows)")
}

# Handicap odds
if (length(handicap_all) > 0) {
  handicap_df <- bind_rows(handicap_all) |>
    mutate(
      across(c(o_home, o_away), as.numeric),
      o_draw = as.numeric(o_draw),
      country = "england"
    ) |>
    standardise_teams() |>
    rename(date = dates, change = line) |>
    select(date, country, league, home, away, change, o_home, o_draw, o_away) |>
    arrange(date, league)

  write_csv(handicap_df, "data/odds_handicap.csv")
  message("Saved handicap: data/odds_handicap.csv (", nrow(handicap_df), " rows)")
}

# Totals odds
if (length(totals_all) > 0) {
  totals_df <- bind_rows(totals_all) |>
    mutate(
      across(c(o_over, o_under), as.numeric),
      country = "england"
    ) |>
    standardise_teams() |>
    rename(date = dates) |>
    select(date, country, league, home, away, limit, o_over, o_under) |>
    arrange(date, league)

  write_csv(totals_df, "data/odds_totals.csv")
  message("Saved totals: data/odds_totals.csv (", nrow(totals_df), " rows)")
}

message("\n=== Done (", Sys.time(), ") ===")
