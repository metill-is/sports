# Prototype, step 1 of the period-score ingest: list every KKI game id.
# Downloads the Baskethotel results export for each (sex, division, season) the
# production ingest knows and writes one row per played game, WITH the export's game id
# (the production parser drops it). Output: <OUT>/kki/games.tsv.
# Usage: Rscript docs/reports/2026-11-15-hb-bb-v2/kki_games.R
source("docs/reports/2026-11-15-hb-bb-v2/common.R")
out_dir <- file.path(OUT, "kki")
dir.create(file.path(out_dir, "exports"), showWarnings = FALSE, recursive = TRUE)
rows <- list()
for (sex in names(KKI_SEASON_IDS)) {
  for (div in names(KKI_SEASON_IDS[[sex]])) {
    for (season in names(KKI_SEASON_IDS[[sex]][[div]])) {
      sid <- KKI_SEASON_IDS[[sex]][[div]][[season]]
      dest <- file.path(out_dir, "exports", sprintf("%s_%s_%s.xlsx", sex, div, season))
      if (!file.exists(dest)) {
        tmp <- download_baskethotel_xlsx(sid, "results_only")
        file.copy(tmp, dest)
        Sys.sleep(0.5)
      }
      raw <- readxl::read_excel(dest, sheet = 1L, skip = 1L, col_types = "text")
      names(raw)[1:8] <- c("id", "vikudagur", "dags", "timi", "heimalid", "gestalid", "leikvollur", "stig")
      raw <- raw[!is.na(raw$dags) & nzchar(raw$dags) & !is.na(raw$id), , drop = FALSE]
      sc <- stringr::str_match(raw$stig, "^\\s*(\\d+)\\s*-\\s*(\\d+)\\s*$")
      rows[[length(rows) + 1L]] <- data.frame(
        game_id = raw$id, sex = sex, division = unname(KKI_DIVISION_LABELS[div]),
        season = as.integer(season), season_id = sid,
        match_date = as.character(as.Date(raw$dags, format = "%d-%m-%Y")),
        home_team = raw$heimalid, away_team = raw$gestalid,
        home_score = as.integer(sc[, 2L]), away_score = as.integer(sc[, 3L])
      )
      cat(sprintf("%s %s %s (season_id %s): %d rows\n", sex, div, season, sid, nrow(raw)))
    }
  }
}
games <- do.call(rbind, rows)
games <- games[!is.na(games$home_score) & (games$home_score + games$away_score) > 0, ]
write.table(games, file.path(out_dir, "games.tsv"), sep = "\t", quote = FALSE, row.names = FALSE,
            fileEncoding = "UTF-8")
cat(sprintf("games.tsv: %d played games, %d distinct ids\n", nrow(games), length(unique(games$game_id))))
print(table(games$sex, games$season))
