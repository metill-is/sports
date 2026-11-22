#' Fetch one Baskethotel game card.
#'
#' One polite request: the pipeline's user agent, a 30-second timeout and one
#' retry. The body is returned byte for byte; the card is ISO-8859-1 and
#' [parse_baskethotel_game_periods()] matches it byte-wise.
#'
#' @param game_id Baskethotel game id (`results.game_id`).
#' @param season_id Baskethotel season id ([kki_season_id()]).
#' @param sex `"male"` or `"female"`.
#' @return The response body as one string.
#' @keywords internal
#' @noRd
fetch_baskethotel_card <- function(game_id, season_id, sex) {
  resp <- httr2::request(baskethotel_game_url(game_id, season_id, sex)) |>
    httr2::req_user_agent("sports-pipeline (+https://github.com/metill-is/sports)") |>
    httr2::req_retry(max_tries = 2L) |>
    httr2::req_timeout(30L) |>
    httr2::req_perform()
  rawToChar(httr2::resp_body_raw(resp))
}

#' Add regulation-time scores to KKI results from each game's card.
#'
#' The ingest's second step for KKI (registered as the source's
#' `enrich_results`). The results export has no period scores, so each played
#' game's card is fetched once and parsed with
#' [parse_baskethotel_game_periods()]; a game whose regulation score is
#' already stored is not fetched again, and its stored score is copied in.
#'
#' Polite by construction: at most `max_cards` requests per run, newest games
#' first, one at a time with `pause()` between them. A daily run fetches the
#' games played since the day before; a backlog is worked off over several
#' runs (or in one, by the backfill script, which lifts the cap).
#'
#' Never fatal: a card that cannot be fetched, cannot be read, or gives a final
#' score other than the result's leaves that game's regulation score `NA`
#' (fetched again on a later run), with one warning for the failed requests.
#'
#' @param results KKI results for one sex, after the team aliases, with
#'   `game_id`.
#' @param sex `"male"` or `"female"`.
#' @param root Data root, to read the regulation scores already stored.
#' @param fetch_card `function(game_id, season_id, sex)` returning the card's
#'   text; [fetch_baskethotel_card()] by default.
#' @param max_cards Most cards to request in one run.
#' @param pause Called between two requests.
#' @return `results` with `home_score_reg` and `away_score_reg`.
#' @keywords internal
#' @noRd
kki_add_regulation_scores <- function(results, sex, root = here::here("data"),
                                      fetch_card = fetch_baskethotel_card,
                                      max_cards = getOption("sports.kki_max_cards", 200L),
                                      pause = function() Sys.sleep(1)) {
  for (col in c("home_score_reg", "away_score_reg")) {
    if (!col %in% names(results)) results[[col]] <- NA_integer_
  }
  if (!"game_id" %in% names(results)) results$game_id <- NA_character_
  if (nrow(results) == 0L) {
    return(results)
  }
  key_cols <- c("season", "division", "match_date", "home_team", "away_team")
  key <- function(x) do.call(paste, c(lapply(x[, key_cols, drop = FALSE], as.character), sep = "\r"))

  # What earlier runs stored: copy it in, and do not ask for those cards again.
  stored <- tryCatch(
    read_table("results", root = root, filter = list(sport = "basketball", country = "iceland", sex = sex)),
    error = function(e) tibble::tibble()
  )
  if (nrow(stored) > 0L && all(c(key_cols, "home_score_reg", "away_score_reg") %in% names(stored))) {
    stored <- stored[!is.na(stored$home_score_reg) & !is.na(stored$away_score_reg), , drop = FALSE]
    at <- match(key(results), key(stored))
    # Only while the final score is the stored one: a corrected final makes the stored
    # regulation score suspect, so the card is fetched again.
    same <- !is.na(at)
    same[same] <- results$home_score[same] == stored$home_score[at[same]] &
      results$away_score[same] == stored$away_score[at[same]]
    same[is.na(same)] <- FALSE
    results$home_score_reg[same] <- as.integer(stored$home_score_reg[at][same])
    results$away_score_reg[same] <- as.integer(stored$away_score_reg[at][same])
  }

  div_slug <- names(KKI_DIVISION_LABELS)[match(results$division, KKI_DIVISION_LABELS)]
  season_id <- vapply(seq_len(nrow(results)), function(i) {
    if (is.na(div_slug[i])) return(NA_integer_)
    id <- kki_season_id(sex, div_slug[i], results$season[i])
    if (is.null(id)) NA_integer_ else as.integer(id)
  }, integer(1))
  todo <- which(!is.na(results$home_score) & !is.na(results$away_score) &
    is.na(results$home_score_reg) & !is.na(results$game_id) & nzchar(results$game_id) &
    !is.na(season_id))
  todo <- todo[order(results$match_date[todo], decreasing = TRUE)]
  left <- max(length(todo) - max_cards, 0L)
  todo <- utils::head(todo, max_cards)

  failed <- 0L
  unusable <- 0L
  for (k in seq_along(todo)) {
    i <- todo[k]
    if (k > 1L) pause()
    text <- tryCatch(fetch_card(results$game_id[i], season_id[i], sex), error = function(e) NULL)
    if (is.null(text) || !is.character(text) || length(text) != 1L) {
      failed <- failed + 1L
      next
    }
    p <- parse_baskethotel_game_periods(text)
    if (!isTRUE(p$ok) || !identical(c(p$home_final, p$away_final), as.integer(c(results$home_score[i], results$away_score[i])))) {
      unusable <- unusable + 1L
      next
    }
    results$home_score_reg[i] <- p$home_reg
    results$away_score_reg[i] <- p$away_reg
  }
  if (failed > 0L) {
    cli::cli_warn("KKI {sex}: {failed} card{?s} could not be fetched; {?its/their} regulation-time score{?s} {?is/are} left for a later run.")
  }
  if (length(todo) > 0L || left > 0L) {
    cli::cli_inform("KKI {sex}: {length(todo)} card{?s} requested, {length(todo) - failed - unusable} regulation-time score{?s} added, {unusable} card{?s} unusable, {left} game{?s} left for later runs.")
  }
  results
}
