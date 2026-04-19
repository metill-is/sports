#' ROI / PnL analysis across all settled bets in the Sports repo.
#'
#' Rerunnable tool that replaces the ad-hoc /tmp/kelly_audit/06_roi_analysis.R
#' script behind the 2026-04-19 ROI snapshot. Emits the full breakdown
#' (overall, by country, by sport x country x sex, by market, trailing
#' windows, monthly, pending exposure, top wins/losses). Optionally
#' upserts the markdown summary into the Metill Obsidian vault.
#'
#' CLI entry point: run from Sports/
#'   Rscript R/bets/roi_report.R                     # terminal output
#'   Rscript R/bets/roi_report.R --write-obsidian    # + vault note
#'
#' Programmatic entry points:
#'   compute_roi(df)        — single-group ROI summary tibble
#'   summarise_roi(df, by)  — grouped ROI summary
#'   load_bets_logs(dir)    — read every */history/bets_log.csv under dir
#'   roi_report(dir, ...)   — orchestrator that prints / returns tables

box::use(
  dplyr[
    tibble, bind_rows, group_by, group_modify, arrange, desc, filter,
    mutate, ungroup, slice_max, slice_min, rename, pull, select
  ],
  readr[read_csv, cols, col_character, col_guess],
  rlang[syms]
)

# ─── core ─────────────────────────────────────────────────────────────────

#' Compute ROI / PnL / calibration for a block of settled bets.
#'
#' Rows with NA `win` or NA `pnl` are dropped before aggregation — they
#' represent still-pending placements that would otherwise corrupt the
#' calibration ratio.
#'
#' @param df data frame with bet_amount, win, pnl, probability
#' @return one-row tibble: n, wins, exp_wins, calibration, turnover, pnl, roi_pct
#' @export
compute_roi <- function(df) {
  settled <- df[!is.na(df$win) & !is.na(df$pnl), , drop = FALSE]
  turnover <- sum(settled$bet_amount, na.rm = TRUE)
  pnl_sum <- sum(settled$pnl, na.rm = TRUE)
  wins <- sum(settled$win, na.rm = TRUE)
  exp_wins <- sum(settled$probability, na.rm = TRUE)
  tibble(
    n = nrow(settled),
    wins = as.integer(wins),
    exp_wins = exp_wins,
    calibration = if (exp_wins > 0) wins / exp_wins else NA_real_,
    turnover = turnover,
    pnl = pnl_sum,
    roi_pct = if (turnover > 0) 100 * pnl_sum / turnover else NA_real_
  )
}

#' Summarise ROI grouped by one or more columns.
#'
#' @param df data frame of settled bets
#' @param by character vector of grouping columns (may be length 0)
#' @return tibble sorted by roi_pct descending
#' @export
summarise_roi <- function(df, by = character(0)) {
  if (length(by) == 0) {
    return(compute_roi(df))
  }
  df |>
    group_by(!!!syms(by)) |>
    group_modify(~ compute_roi(.x)) |>
    ungroup() |>
    arrange(desc(roi_pct))
}

# ─── io ───────────────────────────────────────────────────────────────────

#' Load every `*/history/bets_log.csv` under `sports_dir`.
#'
#' Forces `info` to character (the production log has mixed JSON blobs,
#' numeric market-id shims, and NA), then normalises date / numeric / logical
#' columns so downstream summarisation doesn't re-dispatch on type.
#'
#' @param sports_dir repo root (e.g. the working directory or `here::here()`)
#' @return tibble of all settled + pending bets
#' @export
load_bets_logs <- function(sports_dir) {
  files <- list.files(
    sports_dir,
    pattern = "^bets_log\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )
  spec <- cols(info = col_character(), .default = col_guess())
  frames <- lapply(files, function(f) {
    d <- tryCatch(
      read_csv(f, col_types = spec, progress = FALSE),
      error = function(e) NULL
    )
    if (is.null(d) || nrow(d) == 0) {
      return(NULL)
    }
    d
  })
  frames <- Filter(Negate(is.null), frames)
  if (length(frames) == 0) {
    return(tibble())
  }
  bind_rows(frames) |>
    mutate(
      date_match = as.Date(date_match),
      date_recommended = as.Date(date_recommended),
      bet_amount = as.numeric(bet_amount),
      odds = as.numeric(odds),
      probability = as.numeric(probability),
      win = as.logical(win),
      pnl = as.numeric(pnl),
      info = as.character(info)
    )
}

# ─── orchestration ────────────────────────────────────────────────────────

format_num <- function(x, digits = 0) {
  if (is.na(x)) {
    return("NA")
  }
  formatC(x, format = "d", big.mark = " ", digits = digits)
}

format_pct <- function(x, digits = 2) {
  if (is.na(x)) {
    return("NA")
  }
  sprintf("%+.*f %%", digits, x)
}

#' Markdown row-dump for a summarise_roi result.
roi_md_table <- function(tbl, key_cols) {
  head <- paste(c(key_cols, "n", "turnover", "pnl", "ROI", "calib"),
    collapse = " | "
  )
  sep <- paste(rep("---", length(key_cols) + 5), collapse = " | ")
  rows <- vapply(seq_len(nrow(tbl)), function(i) {
    r <- tbl[i, ]
    keys <- vapply(key_cols, function(k) as.character(r[[k]]), character(1))
    paste(c(
      keys,
      format_num(r$n),
      format_num(r$turnover),
      format_num(r$pnl),
      format_pct(r$roi_pct),
      sprintf("%.3f", r$calibration)
    ), collapse = " | ")
  }, character(1))
  paste(c(
    paste("|", head, "|"), paste("|", sep, "|"),
    paste("|", rows, "|")
  ), collapse = "\n")
}

#' Run the full ROI report.
#'
#' @param sports_dir repo root (defaults to cwd)
#' @param today reference date for trailing windows (defaults to Sys.Date())
#' @param write_obsidian if TRUE, upsert a Metill-vault note summarising the
#'   output (requires Obsidian MCP — not used by unit tests).
#' @return named list with `overall`, `by_country`, `by_sport_country`,
#'   `by_sport_country_sex`, `by_market`, `by_sport_country_market`,
#'   `monthly`, `trailing`, `iceland_vs_rest`, `pending`, `top_wins`,
#'   `top_losses`.
#' @export
roi_report <- function(sports_dir = getwd(),
                       today = Sys.Date(),
                       write_obsidian = FALSE) {
  all_bets <- load_bets_logs(sports_dir)
  if (nrow(all_bets) == 0) {
    message("No bets_log.csv files found under ", sports_dir)
    return(invisible(list()))
  }
  settled <- all_bets |> filter(!is.na(.data$win), !is.na(.data$pnl))
  pending <- all_bets |> filter(is.na(.data$win))

  out <- list(
    overall = compute_roi(settled),
    iceland_vs_rest = settled |>
      mutate(segment = ifelse(.data$country == "iceland", "iceland", "rest")) |>
      summarise_roi(by = "segment"),
    by_country = summarise_roi(settled, by = "country"),
    by_sport_country = summarise_roi(settled, by = c("sport", "country")),
    by_sport_country_sex = summarise_roi(
      settled,
      by = c("sport", "country", "sex")
    ),
    by_market = summarise_roi(settled, by = "market"),
    by_sport_country_market = summarise_roi(
      settled,
      by = c("sport", "country", "market")
    ),
    monthly = settled |>
      mutate(month = format(.data$date_match, "%Y-%m")) |>
      summarise_roi(by = "month") |>
      arrange(.data$month),
    trailing = lapply(
      stats::setNames(c(7, 14, 30, 90), c("7d", "14d", "30d", "90d")),
      function(w) compute_roi(settled[settled$date_match >= today - w, ])
    ),
    pending = summarise_roi(pending, by = c("sport", "country", "sex")),
    top_wins = settled |> slice_max(.data$pnl, n = 10),
    top_losses = settled |> slice_min(.data$pnl, n = 10)
  )

  if (write_obsidian) {
    message(
      "--write-obsidian set; caller should use MCP write_note. ",
      "Returning tables for the caller to format + upsert."
    )
  }

  out
}

# ─── CLI ──────────────────────────────────────────────────────────────────

if (sys.nframe() == 0L && identical(Sys.getenv("R_MAIN"), "")) {
  Sys.setenv(R_MAIN = "1")
  args <- commandArgs(trailingOnly = TRUE)
  write_obs <- "--write-obsidian" %in% args
  res <- roi_report(getwd(), write_obsidian = write_obs)
  cat("=== Overall ===\n")
  print(as.data.frame(res$overall))
  cat("\n=== Iceland vs rest ===\n")
  print(as.data.frame(res$iceland_vs_rest))
  cat("\n=== By sport x country ===\n")
  print(as.data.frame(res$by_sport_country))
  cat("\n=== By market ===\n")
  print(as.data.frame(res$by_market))
  cat("\n=== Trailing 7/14/30/90 days ===\n")
  print(do.call(rbind, lapply(names(res$trailing), function(k) {
    cbind(window = k, as.data.frame(res$trailing[[k]]))
  })))
  cat("\n=== Monthly ===\n")
  print(as.data.frame(res$monthly))
  cat("\n=== Pending exposure ===\n")
  print(as.data.frame(res$pending))
  if (write_obs) {
    cat(
      "\n--write-obsidian: assemble markdown and call MCP write_note ",
      "for Knowledge/Betting Optimisation/roi-snapshot.md.\n"
    )
    md <- paste(
      "# ROI Snapshot", "",
      "## Overall", roi_md_table(res$overall, character(0)),
      "", "## By sport x country",
      roi_md_table(res$by_sport_country, c("sport", "country")),
      "", "## By market", roi_md_table(res$by_market, "market"),
      sep = "\n"
    )
    out_path <- file.path(getwd(), "roi-snapshot-latest.md")
    writeLines(md, out_path)
    cat(
      "Wrote fallback markdown to ", out_path,
      " (pipe into MCP write_note to finish)\n"
    )
  }
}
