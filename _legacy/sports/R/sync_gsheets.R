#' Download all 6 Google Sheets to local CSVs
#'
#' One-shot script to pull historical odds and bets data from GSheets
#' into Sports/data/gsheets/{sport}/ for offline analysis and pipeline use.
#'
#' Run: Rscript R/sync_gsheets.R
#'
#' Requires: googlesheets4 + GOOGLE_MAIL env var for auth

Sys.setlocale("LC_ALL", "is_IS.UTF-8")

library(googlesheets4)
library(readr)
library(dplyr)
library(here)

gs4_auth(email = Sys.getenv("GOOGLE_MAIL"))

# --- Sheet registry ---
# Each entry: id, output_dir, tabs (name → local filename), tab_style (is/en)
sheets <- list(
  list(
    name = "Handball (Iceland)",
    id = "1Q2OIOTgKNZ1w-9Drgth6MT9OzI7LU6aD_OTfB0tr8WU",
    dir = "handball",
    tabs = list(
      "Niðurstaða"   = "odds_outcome.csv",
      "Forgjöf"      = "odds_handicap.csv",
      "Mörk"         = "odds_totals.csv",
      "Bets"         = "bets.csv",
      "Bets_Lengjan" = "bets_lengjan.csv"
    )
  ),
  list(
    name = "Handball (International)",
    id = "1cE1LB2iuTwiV5GSf8JUSF3dO61Jhal0B3x0WrFpRADQ",
    dir = "handball_international",
    tabs = list(
      "Niðurstaða"   = "odds_outcome.csv",
      "Forgjöf"      = "odds_handicap.csv",
      "Mörk"         = "odds_totals.csv",
      "Bets"         = "bets.csv",
      "Bets_Lengjan" = "bets_lengjan.csv"
    )
  ),
  list(
    name = "Basketball (Iceland)",
    id = "1ZFa79A3fEXcAkxDz6cxbCMby4Q2WUT0CI9dp6N4UQlk",
    dir = "basketball",
    tabs = list(
      "Niðurstaða"   = "odds_outcome.csv",
      "Forgjöf"      = "odds_handicap.csv",
      "Mörk"         = "odds_totals.csv",
      "Bets"         = "bets.csv"
    )
  ),
  list(
    name = "Basketball (International)",
    id = "1xB7Pqa95KJqGQfDn3JkX_Y8cwq1XDZsFBHCJNBKY7lg",
    dir = "basketball_international",
    tabs = list(
      "Niðurstaða"   = "odds_outcome.csv",
      "Forgjöf"      = "odds_handicap.csv",
      "Mörk"         = "odds_totals.csv",
      "Bets"         = "bets.csv"
    )
  ),
  list(
    name = "Football (Iceland/Norway)",
    id = "1cJJo8SVJsMQvdzkM3h_pltjS55gbxvuSu_4ID_f8RUM",
    dir = "football",
    tabs = list(
      "Outcome"      = "odds_outcome.csv",
      "Handicap"     = "odds_handicap.csv",
      "Total Goals"  = "odds_totals.csv",
      "Bets"         = "bets.csv",
      "Bets_Lengjan" = "bets_lengjan.csv"
    )
  ),
  list(
    name = "Football (England/Italy/Spain)",
    id = "1n5Wcrg-eO3urluOrm5bTwbe4B3yUPGFkVQ0_0h-t_jg",
    dir = "football_england",
    tabs = list(
      "Outcome"      = "odds_outcome.csv",
      "Handicap"     = "odds_handicap.csv",
      "Total Goals"  = "odds_totals.csv",
      "Bets"         = "bets.csv",
      "Bets_Lengjan" = "bets_lengjan.csv"
    )
  )
)

# --- Column normalisation ---
normalise_odds <- function(d) {
  if (is.null(d) || nrow(d) == 0) return(d)

  # date_game → date
  if ("date_game" %in% names(d) && !"date" %in% names(d)) {
    d <- d |> rename(date = date_game)
  }

  # o_tie → o_draw (some IS sheets use o_tie)
  if ("o_tie" %in% names(d) && !"o_draw" %in% names(d)) {
    d <- d |> rename(o_draw = o_tie)
  }

  # Ensure date is Date type
  if ("date" %in% names(d)) {
    d <- d |> mutate(date = as.Date(date))
  }

  d
}

# --- Download loop ---
base_dir <- here("data", "gsheets")

for (sheet in sheets) {
  cat("\n===", sheet$name, "===\n")
  out_dir <- file.path(base_dir, sheet$dir)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  for (tab_name in names(sheet$tabs)) {
    filename <- sheet$tabs[[tab_name]]
    out_path <- file.path(out_dir, filename)

    d <- tryCatch(
      read_sheet(sheet$id, sheet = tab_name),
      error = function(e) {
        cat("  SKIP", tab_name, ":", e$message, "\n")
        NULL
      }
    )

    if (is.null(d)) next

    # Normalise odds tabs (not bets tabs)
    if (grepl("^odds_", filename)) {
      d <- normalise_odds(d)
    }

    write_csv(d, out_path)
    cat("  ", tab_name, "→", filename, "(", nrow(d), "rows)\n")
  }
}

cat("\nDone. CSVs written to", base_dir, "\n")
