#' Bet settlement module
#'
#' Matches unsettled bets in bets_log.csv against actual match results,
#' determines win/loss, computes PnL, and updates the log in place.
#'
#' Usage:
#'   box::use(R/bets/settle[settle_league])
#'   settle_league(league_dir, sexes)

box::use(
  readr[read_csv, write_csv],
  dplyr[filter, mutate, select, left_join, any_of, rename, coalesce, case_when]
)

# ---------------------------------------------------------------------------
# Results loading — normalise all sport formats to:
#   date, home, away, home_score, away_score
# ---------------------------------------------------------------------------

#' Try common results file locations
find_results_file <- function(league_dir, sex) {
  candidates <- c(
    file.path(league_dir, "data", sex, "data.csv"),
    file.path(league_dir, "data", sex, "results.csv")
  )
  found <- candidates[file.exists(candidates)]
  if (length(found) == 0) return(NULL)
  found[1]
}

#' Load and normalise results to common schema
load_results <- function(league_dir, sex) {
  path <- find_results_file(league_dir, sex)
  if (is.null(path)) return(NULL)

  d <- read_csv(path, show_col_types = FALSE)

  # Normalise column names to: date, home, away, home_score, away_score
  col_map <- c(
    dagsetning  = "date",
    dags        = "date",
    heima       = "home",
    gestir      = "away",
    home_goals  = "home_score",
    away_goals  = "away_score",
    stig_heima  = "home_score",
    stig_gestir = "away_score"
  )

  for (old in names(col_map)) {
    new <- col_map[[old]]
    if (old %in% names(d) && !new %in% names(d)) {
      names(d)[names(d) == old] <- new
    }
  }

  required <- c("date", "home", "away", "home_score", "away_score")
  if (!all(required %in% names(d))) {
    warning("Results file missing columns: ",
            paste(setdiff(required, names(d)), collapse = ", "),
            " in ", path)
    return(NULL)
  }

  d |>
    mutate(date = as.Date(date)) |>
    select(date, home, away, home_score, away_score)
}

# ---------------------------------------------------------------------------
# Settlement logic per market type
# ---------------------------------------------------------------------------

#' Determine win/loss and PnL for each bet row
#'
#' @param bets Tibble of unsettled bets (joined with results)
#' @return Same tibble with win (logical) and pnl (numeric) filled in
compute_settlement <- function(bets) {
  bets |>
    mutate(
      info_num = suppressWarnings(as.numeric(info)),

      win = case_when(
        # 1x2: simple result comparison
        market == "outcome" & outcome == "home" ~ home_score > away_score,
        market == "outcome" & outcome == "away" ~ away_score > home_score,
        market == "outcome" & outcome == "tie"  ~ home_score == away_score,

        # Handicap: add line to home, then compare
        # info = change value (positive = home advantage)
        market == "handicap" & outcome == "home" ~
          (home_score - away_score) + info_num > 0,
        market == "handicap" & outcome == "away" ~
          (home_score - away_score) + info_num < 0,
        market == "handicap" & outcome == "tie" ~
          (home_score - away_score) + info_num == 0,

        # Totals: compare sum to limit
        market == "totals" & outcome == "over" ~
          (home_score + away_score) > info_num,
        market == "totals" & outcome == "under" ~
          (home_score + away_score) <= info_num
      ),

      pnl = ifelse(win, odds * bet_amount - bet_amount, -bet_amount)
    ) |>
    select(-info_num)
}

# ---------------------------------------------------------------------------
# Main entry point
# ---------------------------------------------------------------------------

#' Settle bets for a single league directory
#'
#' Reads bets_log.csv, finds unsettled bets, joins against results data,
#' updates win/pnl, and writes back.
#'
#' @param league_dir Absolute path to league directory
#' @param sexes Character vector of sexes to check (e.g., c("male", "female"))
#' @return Number of bets settled (invisibly)
#' @export
settle_league <- function(league_dir, sexes = c("male", "female")) {
  log_path <- file.path(league_dir, "history", "bets_log.csv")
  if (!file.exists(log_path)) return(invisible(0L))

  log <- read_csv(log_path, show_col_types = FALSE) |>
    mutate(
      date_match = as.Date(date_match),
      info = as.character(info)
    )

  # Only process unsettled bets (win is NA)
  unsettled <- log |> filter(is.na(win) | win == "NA")
  if (nrow(unsettled) == 0) {
    cat("    No unsettled bets.\n")
    return(invisible(0L))
  }

  # Load results for each sex and combine
  all_results <- NULL
  for (sex in sexes) {
    res <- load_results(league_dir, sex)
    if (!is.null(res)) {
      res$sex <- sex
      all_results <- if (is.null(all_results)) res else rbind(all_results, res)
    }
  }

  if (is.null(all_results)) {
    cat("    No results data found.\n")
    return(invisible(0L))
  }

  # Join unsettled bets with results
  matched <- unsettled |>
    left_join(
      all_results,
      by = c("date_match" = "date", "home", "away", "sex")
    )

  # Split: bets with results vs still pending
  has_result <- matched |> filter(!is.na(home_score))
  no_result <- matched |> filter(is.na(home_score))

  if (nrow(has_result) == 0) {
    n_pending <- nrow(no_result)
    cat(sprintf("    %d unsettled bet(s), no results yet.\n", n_pending))
    return(invisible(0L))
  }

  # Settle
  settled <- compute_settlement(has_result) |>
    select(-home_score, -away_score)

  # Print settled bets
  for (i in seq_len(nrow(settled))) {
    b <- settled[i, ]
    result_icon <- if (b$win) "\u2705" else "\u274c"
    cat(sprintf("    %s %s vs %s [%s %s]: %s (%.0f kr)\n",
      result_icon,
      b$home, b$away,
      b$market, b$outcome,
      ifelse(b$win, "WON", "LOST"),
      b$pnl
    ))
  }

  # Merge back into full log: update settled rows, keep rest unchanged
  # Use row matching on the dedup key set
  dedup_keys <- c("date_match", "sport", "country", "sex", "market",
                  "home", "away", "outcome", "info")

  # Mark settled rows in log
  log <- log |> mutate(info = as.character(info))
  settled_slim <- settled |>
    select(all_of(dedup_keys), win_new = win, pnl_new = pnl)

  log <- log |>
    left_join(settled_slim, by = dedup_keys) |>
    mutate(
      win = coalesce(as.character(win_new), as.character(win)),
      pnl = coalesce(pnl_new, pnl)
    ) |>
    select(-win_new, -pnl_new)

  write_csv(log, log_path)

  n_settled <- nrow(settled)
  total_pnl <- sum(settled$pnl)
  cat(sprintf("    Settled %d bet(s). PnL: %.0f kr\n", n_settled, total_pnl))

  invisible(n_settled)
}
