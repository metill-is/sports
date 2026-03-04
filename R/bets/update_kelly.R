#' Update kelly_frac values in bets.yml files from historical performance
#'
#' Reads history/bets_all.csv, computes optimal kelly fractions per
#' sport/country/sex via recommend_kelly(), and writes kelly_frac_male /
#' kelly_frac_female back into each league's config/bets.yml.
#'
#' Usage:
#'   cd Sports/
#'   Rscript R/bets/update_kelly.R              # apply changes
#'   Rscript R/bets/update_kelly.R --dry-run    # preview only

library(readr)
library(dplyr)
library(here)
library(yaml)

here::i_am(".here")

dry_run <- "--dry-run" %in% commandArgs(trailingOnly = TRUE)
if (dry_run) cat("=== DRY RUN — no files will be modified ===\n\n")

# ---------------------------------------------------------------------------
# 1. Load history and compute recommendations
# ---------------------------------------------------------------------------

bets_path <- here("history", "bets_all.csv")
if (!file.exists(bets_path)) stop("No history file at ", bets_path)

box::use(R/bets/history[recommend_kelly, load_all_history])

d <- read_csv(bets_path, show_col_types = FALSE)
recs <- recommend_kelly(d, by = c("sport", "country", "sex"), min_bets = 20)

cat("=== Kelly recommendations from history ===\n")
print(recs |> select(sport, country, sex, n_bets, recommended_kelly, confidence), n = 40)
cat("\n")

# ---------------------------------------------------------------------------
# 2. Load leagues.yml to find bets.yml paths
# ---------------------------------------------------------------------------

leagues_yml <- here("config", "leagues.yml")
leagues_raw <- yaml.load(read_file(leagues_yml))
leagues_raw$defaults <- NULL  # remove defaults block

bankroll_yml <- here("config", "bankroll.yml")
global_bankroll <- if (file.exists(bankroll_yml)) {
  yaml.load(read_file(bankroll_yml))
} else {
  list(cur_pool = 5000, min_bet_amount = 200)
}

min_bet <- global_bankroll$min_bet_amount %||% 200
cur_pool <- global_bankroll$cur_pool %||% 5000

# ---------------------------------------------------------------------------
# 3. For each league with bets, update kelly_frac values
# ---------------------------------------------------------------------------

comparison <- list()

for (league_key in names(leagues_raw)) {
  league <- leagues_raw[[league_key]]
  if (!isTRUE(league$has_bets)) next

  bets_yml_path <- here(league$dir, "config", "bets.yml")
  if (!file.exists(bets_yml_path)) next

  sport <- league$sport
  country <- league$country
  sexes <- league$sex

  # Read current bets.yml as text (preserve formatting + UTF-8)
  yml_text <- read_file(bets_yml_path)
  cfg <- yaml.load(yml_text)
  cur_kf <- cfg$bankroll$kelly_frac %||% 0.10

  changed <- FALSE

  for (sex in sexes) {
    rec_row <- recs |>
      filter(.data$sport == .env$sport,
             .data$country == .env$country,
             .data$sex == .env$sex)

    if (nrow(rec_row) == 0) {
      comparison <- c(comparison, list(tibble(
        league = league_key, sex = sex,
        current = cur_kf, recommended = NA_real_,
        new = cur_kf, n_bets = 0L, action = "no data"
      )))
      next
    }

    rec_kf <- rec_row$recommended_kelly[1]
    n <- rec_row$n_bets[1]
    conf <- rec_row$confidence[1]

    # Guard: floor if recommended × pool < min_bet
    if (rec_kf * cur_pool < min_bet) {
      rec_kf <- 0.01
    }

    # Hard cap + confidence taper
    # High confidence (100+ bets): cap at 0.40
    # Moderate confidence (20-99 bets): cap at 0.25
    max_kf <- if (conf == "high") 0.40 else 0.25
    rec_kf <- max(0.01, min(max_kf, rec_kf))
    rec_kf <- round(rec_kf, 2)

    # Current sex-specific value (if any)
    sex_key <- paste0("kelly_frac_", sex)
    cur_sex_kf <- cfg$bankroll[[sex_key]] %||% cur_kf

    action <- if (abs(rec_kf - cur_sex_kf) < 0.005) "unchanged" else "update"

    comparison <- c(comparison, list(tibble(
      league = league_key, sex = sex,
      current = cur_sex_kf, recommended = rec_row$recommended_kelly[1],
      new = rec_kf, n_bets = as.integer(n), action = action
    )))

    # Insert/update kelly_frac_{sex} in YAML text
    pattern <- paste0("kelly_frac_", sex, ":\\s*[0-9.]+")
    replacement <- paste0("kelly_frac_", sex, ": ", format(rec_kf, nsmall = 2))

    if (grepl(pattern, yml_text)) {
      yml_text <- sub(pattern, replacement, yml_text)
      changed <- TRUE
    } else {
      # Insert after the last kelly_frac* line in the bankroll block
      # Find all kelly_frac lines and insert after the last one
      lines <- strsplit(yml_text, "\n")[[1]]
      last_kf_idx <- max(grep("kelly_frac", lines))
      if (length(last_kf_idx) > 0 && last_kf_idx > 0) {
        lines <- append(lines, paste0("  ", replacement), after = last_kf_idx)
        yml_text <- paste(lines, collapse = "\n")
        changed <- TRUE
      }
    }
  }

  # Update base kelly_frac as weighted average of sex-specific values
  # Use negative lookahead to only match the base key, not sex-specific ones
  if (length(sexes) > 1 && changed) {
    sex_kfs <- vapply(sexes, function(s) {
      row <- Filter(function(x) x$league == league_key && x$sex == s, comparison)
      if (length(row) > 0) row[[1]]$new else cur_kf
    }, numeric(1))
    avg_kf <- round(mean(sex_kfs), 2)
    yml_text <- sub(
      "kelly_frac:(?!_)\\s*[0-9.]+",
      paste0("kelly_frac: ", format(avg_kf, nsmall = 2)),
      yml_text,
      perl = TRUE
    )
  }

  if (changed && !dry_run) {
    writeLines(yml_text, bets_yml_path, useBytes = TRUE)
    cat("  Updated:", bets_yml_path, "\n")
  }
}

# ---------------------------------------------------------------------------
# 4. Print comparison table
# ---------------------------------------------------------------------------

comp_df <- bind_rows(comparison)

cat("\n=== Kelly fraction comparison: current → new ===\n")
comp_df |>
  mutate(
    current = sprintf("%.2f", current),
    recommended = ifelse(is.na(recommended), "—", sprintf("%.2f", recommended)),
    new = sprintf("%.2f", new),
    n_bets = ifelse(n_bets == 0, "—", as.character(n_bets))
  ) |>
  print(n = 40)

if (dry_run) {
  cat("\n(Dry run — no files modified. Remove --dry-run to apply.)\n")
} else {
  cat("\nDone. Run `grep -r 'kelly_frac' */*/config/bets.yml` to verify.\n")
}
