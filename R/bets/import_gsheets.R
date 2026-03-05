#' One-time import: consolidate all historical bets from Google Sheets
#'
#' Reads 6 Google Sheets (football, handball, basketball — domestic + international),
#' normalises to bets_log format, converts EUR → ISK, and writes:
#'   1. history/bets_all.csv — full archive with currency/booker metadata
#'   2. {sport}/{country}/history/bets_log.csv — per-league files (pipeline schema)
#'
#' After running, rebuild the Parquet store: Rscript R/storage/migrate_history.R
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
  "Markafjöldi (Liðs)" = "totals",
  "Bæði skora"         = "btts"
)

bet_map <- c(
  heima     = "home",
  gestir    = "away",
  jafntefli = "tie",
  yfir      = "over",
  undir     = "under",
  "já"      = "yes",
  nei       = "no"
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
football_id       <- "1n5Wcrg-eO3urluOrm5bTwbe4B3yUPGFkVQ0_0h-t_jg"   # Odds England
handball_id       <- "1Q2OIOTgKNZ1w-9Drgth6MT9OzI7LU6aD_OTfB0tr8WU"   # Handball Odds
basketball_id     <- "1ZFa79A3fEXcAkxDz6cxbCMby4Q2WUT0CI9dp6N4UQlk"   # Basketball Odds
football_gen_id   <- "1cJJo8SVJsMQvdzkM3h_pltjS55gbxvuSu_4ID_f8RUM"   # Football Odds (general)
handball_intl_id  <- "1cE1LB2iuTwiV5GSf8JUSF3dO61Jhal0B3x0WrFpRADQ"   # Handball Odds (International)
basketball_intl_id <- "1xB7Pqa95KJqGQfDn3JkX_Y8cwq1XDZsFBHCJNBKY7lg"  # Basketball Odds (International)

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

# ── 4. Football General: Bets (EUR) + Bets_Lengjan (ISK) ───────────────────
cat("Football General Bets (EUR)...\n")
fg <- read_sheet(football_gen_id, sheet = "Bets")
fg$info <- safe_info(fg$info)
fg_clean <- fg |>
  transmute(
    date_recommended = as.Date(dags_bet),
    date_match       = as.Date(dags_leikur),
    sport            = "football",
    country          = tolower(deild),
    sex              = sex,
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
cat("  ", nrow(fg_clean), "bets\n")

cat("Football General Bets_Lengjan (ISK)...\n")
fgl <- read_sheet(football_gen_id, sheet = "Bets_Lengjan")
fgl$info <- safe_info(fgl$info)
fgl_clean <- fgl |>
  transmute(
    date_recommended = as.Date(dags_bet),
    date_match       = as.Date(dags_leikur),
    sport            = "football",
    country          = tolower(deild),
    sex              = sex,
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
cat("  ", nrow(fgl_clean), "bets\n")

# ── 5. Handball International: Bets (EUR) ─────────────────────────────────
cat("Handball International Bets (EUR)...\n")
hi <- read_sheet(handball_intl_id, sheet = "Bets")
hi$info <- safe_info(hi$info)
hi_clean <- hi |>
  transmute(
    date_recommended = as.Date(dags_bet),
    date_match       = as.Date(dags_leikur),
    sport            = "handball",
    country          = "international",
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
cat("  ", nrow(hi_clean), "bets\n")

# ── 6. Basketball International: Bets (EUR) ───────────────────────────────
cat("Basketball International Bets (EUR)...\n")
bii <- read_sheet(basketball_intl_id, sheet = "Bets")
bii$info <- safe_info(bii$info)
bii_clean <- bii |>
  transmute(
    date_recommended = as.Date(dags_bet),
    date_match       = as.Date(dags_leikur),
    sport            = "basketball",
    country          = "international",
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
cat("  ", nrow(bii_clean), "bets\n")

# ── 7. Local pipeline bets ───────────────────────────────────────────────────
cat("Local pipeline bets...\n")
local_files <- list.files(
  here(),
  pattern = "bets_log\\.csv$",
  recursive = TRUE,
  full.names = TRUE
) |>
  grep("history/bets_all", x = _, value = TRUE, invert = TRUE)

local <- lapply(local_files, function(f) {
  d <- read_csv(f, show_col_types = FALSE) |>
    mutate(
      date_match = as.Date(date_match),
      date_recommended = as.Date(date_recommended),
      info = as.character(info),
      win = ifelse(is.na(win) | win == "NA", NA, as.logical(win)),
      pnl = ifelse(is.na(pnl), NA_real_, pnl),
      currency_original = "ISK",
      booker = "Lengjan"
    )
  # Respect existing source column; default to "pipeline" only if missing
  if (!"source" %in% names(d)) d$source <- "pipeline"
  # Only keep pipeline-era bets — gsheets bets will come from the fresh import
  d |> filter(source == "pipeline")
}) |> bind_rows()

cat("  ", nrow(local), "bets from", length(local_files), "file(s)\n")

# ── Combine ──────────────────────────────────────────────────────────────────
all_bets <- bind_rows(
  fb_clean, fl_clean, hb_clean, hl_clean, bi_clean,
  fg_clean, fgl_clean, hi_clean, bii_clean,
  local
) |>
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

# Recalculate PnL for settled bets where win is set but pnl is NA
all_bets <- all_bets |>
  mutate(
    pnl = ifelse(
      !is.na(win) & is.na(pnl),
      round(ifelse(win, odds * bet_amount - bet_amount, -bet_amount), 0),
      pnl
    )
  )

# Write full archive (includes currency_original, booker metadata)
out_path <- here("history", "bets_all.csv")
if (!dir.exists(dirname(out_path))) dir.create(dirname(out_path), recursive = TRUE)
write_csv(all_bets, out_path)
cat("\nWritten to:", out_path, "\n")

# ── Per-league CSV output ──────────────────────────────────────────────────
# Split by sport+country and write to {sport}/{country}/history/bets_log.csv
# This replaces old per-league CSVs (which had EUR amounts) with ISK-converted data

bets_log_cols <- c(
  "date_recommended", "date_match", "sport", "country", "sex",
  "market", "home", "away", "outcome", "odds", "probability", "ev",
  "kelly_frac", "bet_amount", "info", "win", "pnl", "source"
)

per_league <- all_bets |>
  # Dedup: prefer gsheets (ISK-converted) over pipeline for same bet
  arrange(
    date_match, sport, country, sex, market, home, away, outcome,
    factor(source, levels = c("gsheets", "pipeline"))
  ) |>
  distinct(date_match, sport, country, sex, market, home, away, outcome, info,
           .keep_all = TRUE) |>
  select(any_of(bets_log_cols))

league_groups <- per_league |>
  group_by(sport, country) |>
  group_split()

cat("\nWriting per-league bets_log.csv files:\n")
for (lg in league_groups) {
  sport <- lg$sport[1]
  country <- lg$country[1]
  league_dir <- here(sport, country, "history")

  if (!dir.exists(league_dir)) {
    cat("  Skipping", sport, "/", country, "— directory does not exist\n")
    next
  }

  league_path <- file.path(league_dir, "bets_log.csv")
  write_csv(lg, league_path)
  cat("  ", sport, "/", country, ":", nrow(lg), "bets →", league_path, "\n")
}
cat("\n")

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
