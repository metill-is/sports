#' Baskethotel game-view widget URL (per-period scores for one game).
#'
#' Used by the KKI ingest's regulation-time step
#' ([kki_add_regulation_scores()]). The results export carries a game id in
#' its first column (stored as `results.game_id`). Widget 400
#' (`GAME_FULL_VIEW_WIDGET` in Baskethotel's `api.js`) renders that game's
#' card, including one `home - away` score per period.
#'
#' @param game_id Baskethotel game id (first column of the results export).
#' @param season_id Baskethotel season id, see `KKI_SEASON_IDS`.
#' @param sex `"male"` or `"female"`; selects KKI's league id (190 / 189, the
#'   `MENS_LEAGUE` / `WOMENS_LEAGUE` constants on kki.is).
#' @return URL string.
#' @noRd
baskethotel_game_url <- function(game_id, season_id, sex = c("male", "female")) {
  sex <- match.arg(sex)
  league_id <- if (sex == "male") 190L else 189L
  sprintf(
    paste0(
      "https://widgets.baskethotel.com/widget-service/show?api=%s&lang=is",
      "&nnav=1&nav_object=0&flash=0&request[0][container]=view4",
      "&request[0][widget]=400&request[0][param][game_id]=%s",
      "&request[0][param][season_id]=%s&request[0][param][league_id]=%d"
    ),
    BASKETHOTEL_API, game_id, season_id, league_id
  )
}

#' Parse per-period scores out of a Baskethotel game-view widget response.
#'
#' Pure function: takes the response body, returns one row. Only digits and ASCII class names are
#' matched, byte-wise, so the response's ISO-8859-1 team names cannot break
#' the parse.
#'
#' Regulation time is the first four periods. KKI's cards list exactly four
#' quarters even when the game went to overtime (measured on 3,286 cards,
#' seasons 2021 to 2027: 138 cards had a level four-quarter sum and a final
#' score above it), so overtime is read off the scores: the quarters are level
#' and the final is not. Overtime points are the final score minus the
#' regulation score. A card that lists further periods is handled too.
#'
#' `ok = FALSE` when the card has fewer than four periods, or when the final
#' score is neither the regulation score nor a tie broken in overtime (a
#' walkover recorded over a played game, or a data-entry error), so such a
#' card can never pass as a regulation-time score.
#'
#' @param text Response body as one string.
#' @return One-row data frame: `home_final`, `away_final`, `n_periods`,
#'   `home_reg`, `away_reg` (first four periods), `home_ot`, `away_ot` (final
#'   minus regulation), `overtime`, `periods` (`"27-29;27-22;..."`) and `ok`.
#' @noRd
parse_baskethotel_game_periods <- function(text) {
  stopifnot(is.character(text), length(text) == 1L)
  grab <- function(pattern, x) {
    m <- regmatches(x, regexec(pattern, x, perl = TRUE, useBytes = TRUE))[[1]]
    if (length(m) < 2L) NA_character_ else m[2L]
  }
  out <- data.frame(
    home_final = NA_integer_, away_final = NA_integer_, n_periods = 0L,
    home_reg = NA_integer_, away_reg = NA_integer_,
    home_ot = NA_integer_, away_ot = NA_integer_, overtime = NA,
    periods = NA_character_, ok = FALSE
  )
  score <- function(side) {
    pattern <- sprintf("mbt-game-card-score-%s[^>]*>\\s*(\\d+)\\s*<", side)
    suppressWarnings(as.integer(grab(pattern, text)))
  }
  out$home_final <- score("a")
  out$away_final <- score("b")
  block <- grab("mbt-gamecard-quarters[^>]*>(.*?)<\\\\?/div>", text)
  if (is.na(block)) {
    return(out)
  }
  spans <- regmatches(
    block,
    gregexpr("<span>\\s*\\d+\\s*-\\s*\\d+\\s*<", block, perl = TRUE, useBytes = TRUE)
  )[[1]]
  if (length(spans) == 0L) {
    return(out)
  }
  nums <- regmatches(spans, gregexpr("\\d+", spans, perl = TRUE, useBytes = TRUE))
  per <- matrix(as.integer(unlist(nums)), ncol = 2L, byrow = TRUE)
  out$n_periods <- nrow(per)
  out$periods <- paste(per[, 1L], per[, 2L], sep = "-", collapse = ";")
  if (nrow(per) < 4L || is.na(out$home_final) || is.na(out$away_final)) {
    return(out)
  }
  out$home_reg <- sum(per[1:4, 1L])
  out$away_reg <- sum(per[1:4, 2L])
  out$home_ot <- out$home_final - out$home_reg
  out$away_ot <- out$away_final - out$away_reg
  decided_in_regulation <- out$home_ot == 0L && out$away_ot == 0L
  out$overtime <- out$home_reg == out$away_reg && out$home_ot >= 0L &&
    out$away_ot >= 0L && out$home_ot != out$away_ot
  listed_all <- sum(per[, 1L]) == out$home_final && sum(per[, 2L]) == out$away_final
  out$ok <- decided_in_regulation || (out$overtime && (nrow(per) == 4L || listed_all))
  out
}
