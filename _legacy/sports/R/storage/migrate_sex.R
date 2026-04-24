#' One-off migration: normalise sex values in all bets_log.csv files
#'
#' Fixes historical inconsistencies:
#'   - handball/iceland: kk → male, kvk → female
#'   - basketball/iceland: NA → inferred from team names in data/{sex}/
#'   - Other leagues with NA sex: → male (male-only leagues)
#'
#' Usage:
#'   cd Sports && Rscript R/storage/migrate_sex.R

library(readr)
library(dplyr)

sports_dir <- here::here()

# -----------------------------------------------------------------------
# Sex normalisation map (Icelandic → English)
# -----------------------------------------------------------------------
sex_norm <- c(kk = "male", kvk = "female")

# -----------------------------------------------------------------------
# Build team-sex lookup for basketball/iceland from data files
# -----------------------------------------------------------------------
build_team_sex_lookup <- function(league_dir) {
  lookup <- list()
  for (sex in c("male", "female")) {
    data_path <- file.path(league_dir, "data", sex, "data.csv")
    if (!file.exists(data_path)) next
    d <- read_csv(data_path, show_col_types = FALSE)
    # Normalise column names
    if ("heima" %in% names(d)) names(d)[names(d) == "heima"] <- "home"
    if ("gestir" %in% names(d)) names(d)[names(d) == "gestir"] <- "away"
    teams <- unique(c(d$home, d$away))
    for (t in teams) lookup[[t]] <- c(lookup[[t]], sex)
  }
  lookup
}

# -----------------------------------------------------------------------
# Infer sex for NA rows using team name matching
# -----------------------------------------------------------------------
infer_sex <- function(df, league_dir) {
  na_rows <- which(is.na(df$sex) | df$sex == "NA")
  if (length(na_rows) == 0) return(df)

  lookup <- build_team_sex_lookup(league_dir)

  for (i in na_rows) {
    home <- df$home[i]
    away <- df$away[i]
    home_sexes <- lookup[[home]]
    away_sexes <- lookup[[away]]

    # Intersect: if both teams appear in only one sex, use that
    candidates <- intersect(
      if (is.null(home_sexes)) c("male", "female") else home_sexes,
      if (is.null(away_sexes)) c("male", "female") else away_sexes
    )
    candidates <- unique(candidates)

    if (length(candidates) == 1) {
      df$sex[i] <- candidates
    } else {
      # Ambiguous — default to male (most common)
      df$sex[i] <- "male"
    }
  }
  df
}

# -----------------------------------------------------------------------
# Process each bets_log.csv
# -----------------------------------------------------------------------
logs <- Sys.glob(file.path(sports_dir, "*", "*", "history", "bets_log.csv"))
cat("Found", length(logs), "bets_log.csv file(s)\n\n")

for (log_path in logs) {
  cat("Processing:", log_path, "\n")
  df <- read_csv(log_path, show_col_types = FALSE)
  if (nrow(df) == 0) {
    cat("  Empty — skipping\n\n")
    next
  }

  n_before <- table(df$sex, useNA = "ifany")
  changed <- FALSE

  # Step 1: Apply kk/kvk → male/female mapping
  mapped <- sex_norm[df$sex]
  has_map <- !is.na(mapped)
  if (any(has_map)) {
    df$sex[has_map] <- mapped[has_map]
    changed <- TRUE
  }

  # Step 2: Infer NA sex values from team names
  na_sex <- is.na(df$sex) | df$sex == "NA"
  if (any(na_sex)) {
    league_dir <- dirname(dirname(log_path))

    # Check if data files exist for inference
    has_data <- dir.exists(file.path(league_dir, "data", "male")) ||
                dir.exists(file.path(league_dir, "data", "female"))

    if (has_data) {
      df <- infer_sex(df, league_dir)
    } else {
      # No data files — default to male (single-sex leagues)
      df$sex[na_sex] <- "male"
    }
    changed <- TRUE
  }

  if (changed) {
    n_after <- table(df$sex, useNA = "ifany")
    cat("  Before:", paste(names(n_before), n_before, collapse = ", "), "\n")
    cat("  After: ", paste(names(n_after), n_after, collapse = ", "), "\n")
    write_csv(df, log_path)
    cat("  Written.\n\n")
  } else {
    cat("  No changes needed.\n\n")
  }
}

cat("Migration complete.\n")
