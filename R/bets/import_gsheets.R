#' One-time import: consolidate all historical bets from Google Sheets
#'
#' Reads 3 Google Sheets (football, handball, basketball),
#' normalises to bets_log format, converts EUR → ISK, and writes
#' a single consolidated history/bets_all.csv.
#'
#' Usage: Rscript R/bets/import_gsheets.R

library(googlesheets4)
library(dplyr)
library(readr)
library(here)

here::i_am(".here")

EUR_ISK <- 150

# --- Mappings ---
type_map <- c(
  "Niðurstaða"         = "outcome",
  "Forgjöf"            = "handicap",
  "Markafjöldi"        = "totals",
  "Markafjöldi (Liðs)" = "totals"
)

bet_map <- c(
  heima     = "home",
  gestir    = "away",
  jafntefli = "tie",
  yfir      = "over",
  undir     = "under"
)

sex_map <- c(kk = "male", kvk = "female")

safe_info <- function(x) {
  if (is.null(x)) return(NA_character_)
  if (is.list(x)) {
    return(sapply(x, function(v) {
      if (is.null(v) || length(v) == 0) NA_character_ else as.character(v)
    }))
  }
  as.character(x)
}

gs4_auth(email = Sys.getenv("GOOGLE_MAIL"))

# ── Sheet IDs ────────────────────────────────────────────────────────────────
football_id  <- "1n5Wcrg-eO3urluOrm5bTwbe4B3yUPGFkVQ0_0h-t_jg"
handball_id  <- "1Q2OIOTgKNZ1w-9Drgth6MT9OzI7LU6aD_OTfB0tr8WU"
basketball_id <- "1ZFa79A3fEXcAkxDz6cxbCMby4Q2WUT0CI9dp6N4UQlk"

# ── 1. Football: Bets (EUR) + Bets_Lengjan (ISK) ────────────────────────────
cat("Football Bets (EUR)...\n")
fb <- read_sheet(football_id, sheet = "Bets")
fb$info <- safe_info(fb$info)
fb_clean <- fb |>
  transmute(
    date_recommended = as.Date(dags_bet),
    date_match       = as.Date(dags_leikur),
    sport            = "football",
    country          = "england",
    sex              = "male",
    market           = type_map[type],
    home             = heima,
    away             = gestir,
    outcome          = bet_map[bet],
    odds, probability = prob, ev,
    kelly_frac       = NA_real_,
    bet_amount       = round(amount * EUR_ISK, 0),
    info,
    win              = ifelse(is.na(win), NA, as.logical(win)),
    pnl              = ifelse(is.na(win), NA_real_,
                        round(ifelse(win == 1, (payout - amount) * EUR_ISK, -amount * EUR_ISK), 0)),
    currency_original = "EUR",
    booker           = booker,
    source           = "gsheets"
  )
cat("  ", nrow(fb_clean), "bets\n")

cat("Football Bets_Lengjan (ISK)...\n")
fl <- read_sheet(football_id, sheet = "Bets_Lengjan")
fl$info <- safe_info(fl$info)
fl_clean <- fl |>
  transmute(
    date_recommended = as.Date(dags_bet),
    date_match       = as.Date(dags_leikur),
    sport            = "football",
    country          = "england",
    sex              = "male",
    market           = type_map[type],
    home             = heima,
    away             = gestir,
    outcome          = bet_map[bet],
    odds, probability = prob, ev,
    kelly_frac       = NA_real_,
    bet_amount       = round(amount, 0),
    info,
    win              = ifelse(is.na(win), NA, as.logical(win)),
    pnl              = ifelse(is.na(win), NA_real_,
                        round(ifelse(win == 1, payout - amount, -amount), 0)),
    currency_original = "ISK",
    booker           = "Lengjan",
    source           = "gsheets"
  )
cat("  ", nrow(fl_clean), "bets\n")

# ── 2. Handball: Bets (EUR) + Bets_Lengjan (ISK) ────────────────────────────
cat("Handball Bets (EUR)...\n")
hb <- read_sheet(handball_id, sheet = "Bets")
hb$info <- safe_info(hb$info)
hb_clean <- hb |>
  transmute(
    date_recommended = as.Date(dags_bet),
    date_match       = as.Date(dags_leikur),
    sport            = "handball",
    country          = tolower(country),
    sex              = sex_map[sex],
    market           = type_map[type],
    home             = heima,
    away             = gestir,
    outcome          = bet_map[bet],
    odds, probability = prob, ev,
    kelly_frac       = NA_real_,
    bet_amount       = round(amount * EUR_ISK, 0),
    info,
    win              = ifelse(is.na(win), NA, as.logical(win)),
    pnl              = ifelse(is.na(win), NA_real_,
                        round(ifelse(win == 1, (payout - amount) * EUR_ISK, -amount * EUR_ISK), 0)),
    currency_original = "EUR",
    booker           = booker,
    source           = "gsheets"
  )
cat("  ", nrow(hb_clean), "bets\n")

cat("Handball Bets_Lengjan (ISK)...\n")
hl <- read_sheet(handball_id, sheet = "Bets_Lengjan")
hl$info <- safe_info(hl$info)
hl_clean <- hl |>
  transmute(
    date_recommended = as.Date(dags_bet),
    date_match       = as.Date(dags_leikur),
    sport            = "handball",
    country          = tolower(country),
    sex              = sex_map[sex],
    market           = type_map[type],
    home             = heima,
    away             = gestir,
    outcome          = bet_map[bet],
    odds, probability = prob, ev,
    kelly_frac       = NA_real_,
    bet_amount       = round(amount, 0),
    info,
    win              = ifelse(is.na(win), NA, as.logical(win)),
    pnl              = ifelse(is.na(win), NA_real_,
                        round(ifelse(win == 1, payout - amount, -amount), 0)),
    currency_original = "ISK",
    booker           = "Lengjan",
    source           = "gsheets"
  )
cat("  ", nrow(hl_clean), "bets\n")

# ── 3. Basketball: Bets (EUR) ────────────────────────────────────────────────
cat("Basketball Bets (EUR)...\n")
bi <- read_sheet(basketball_id, sheet = "Bets")
bi$info <- safe_info(bi$info)
bi_clean <- bi |>
  transmute(
    date_recommended = as.Date(dags_bet),
    date_match       = as.Date(dags_leikur),
    sport            = "basketball",
    country          = "iceland",
    sex              = sex_map[Kyn],
    market           = type_map[type],
    home             = heima,
    away             = gestir,
    outcome          = bet_map[bet],
    odds, probability = prob, ev,
    kelly_frac       = NA_real_,
    bet_amount       = round(amount * EUR_ISK, 0),
    info,
    win              = ifelse(is.na(win), NA, as.logical(win)),
    pnl              = ifelse(is.na(win), NA_real_,
                        round(ifelse(win == 1, (payout - amount) * EUR_ISK, -amount * EUR_ISK), 0)),
    currency_original = "EUR",
    booker           = booker,
    source           = "gsheets"
  )
cat("  ", nrow(bi_clean), "bets\n")

# ── 4. Local pipeline bets ───────────────────────────────────────────────────
cat("Local pipeline bets...\n")
local_files <- list.files(
  here(),
  pattern = "bets_log\\.csv$",
  recursive = TRUE,
  full.names = TRUE
) |>
  grep("history/bets_all", x = _, value = TRUE, invert = TRUE)

local <- lapply(local_files, function(f) {
  read_csv(f, show_col_types = FALSE) |>
    mutate(
      date_match = as.Date(date_match),
      date_recommended = as.Date(date_recommended),
      info = as.character(info),
      win = ifelse(is.na(win) | win == "NA", NA, as.logical(win)),
      pnl = ifelse(is.na(pnl), NA_real_, pnl),
      currency_original = "ISK",
      booker = "Lengjan",
      source = "pipeline"
    )
}) |> bind_rows()

cat("  ", nrow(local), "bets from", length(local_files), "file(s)\n")

# ── Combine ──────────────────────────────────────────────────────────────────
all_bets <- bind_rows(fb_clean, fl_clean, hb_clean, hl_clean, bi_clean, local) |>
  mutate(across(where(is.list), ~ sapply(.x, function(v) {
    if (is.null(v) || length(v) == 0) NA else v[[1]]
  }))) |>
  mutate(
    bet_amount = as.numeric(bet_amount),
    pnl = as.numeric(pnl),
    odds = as.numeric(odds),
    probability = as.numeric(probability),
    ev = as.numeric(ev),
    kelly_frac = as.numeric(kelly_frac)
  ) |>
  arrange(date_match, sport, country)

# Write immediately (summary stats below may fail on edge cases)
out_path <- here("history", "bets_all.csv")
if (!dir.exists(dirname(out_path))) dir.create(dirname(out_path), recursive = TRUE)
write_csv(all_bets, out_path)
cat("\nWritten to:", out_path, "\n")

cat("\n", strrep("=", 50), "\n")
cat("  CONSOLIDATED BETTING HISTORY\n")
cat(strrep("=", 50), "\n\n")

cat("Total bets:", nrow(all_bets), "\n\n")

cat("By sport + country:\n")
print(all_bets |> count(sport, country) |> arrange(desc(n)), n = 30)

cat("\nBy booker:\n")
print(all_bets |> count(booker) |> arrange(desc(n)))

cat("\nBy market:\n")
print(all_bets |> count(market) |> arrange(desc(n)))

settled <- all_bets |> filter(!is.na(win))
unsettled <- all_bets |> filter(is.na(win))
cat("\nSettled:", nrow(settled), " Unsettled:", nrow(unsettled), "\n\n")

if (nrow(settled) > 0) {
  cat("Settled results:\n")
  cat("  Won:", sum(settled$win), "/", nrow(settled),
      sprintf("(%.1f%%)\n", 100 * mean(settled$win)))
  cat("  Total wagered (ISK):", format(sum(settled$bet_amount), big.mark = ","), "\n")
  cat("  Total PnL (ISK):", format(round(sum(settled$pnl)), big.mark = ","), "\n")
  cat("  ROI:", sprintf("%.1f%%\n", 100 * sum(settled$pnl) / sum(settled$bet_amount)))

  cat("\nPnL by sport:\n")
  print(settled |>
    group_by(sport) |>
    summarise(
      n = n(),
      won = sum(win),
      hit_rate = sprintf("%.1f%%", 100 * mean(win)),
      roi = sprintf("%.1f%%", 100 * sum(pnl) / sum(bet_amount)),
      wagered = format(sum(bet_amount), big.mark = ","),
      pnl = format(round(sum(pnl)), big.mark = ","),
      .groups = "drop"
    ) |> arrange(desc(n)))

  cat("\nPnL by market:\n")
  print(settled |>
    group_by(market) |>
    summarise(
      n = n(),
      won = sum(win),
      hit_rate = sprintf("%.1f%%", 100 * mean(win)),
      roi = sprintf("%.1f%%", 100 * sum(pnl) / sum(bet_amount)),
      pnl = format(round(sum(pnl)), big.mark = ","),
      .groups = "drop"
    ) |> arrange(desc(n)))
}

cat("\nDone.\n")
