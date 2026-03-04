#' 1x2 (match result) market pipeline
#'
#' Joins posterior summaries with outcome odds, runs two-pass Kelly.
#' Sport-agnostic: tie handling driven by cfg$scoring.

box::use(
  ./kelly[get_kelly, format_bet_text],
  dplyr[filter, mutate, summarise, select, any_of, across, rename, inner_join,
        group_by, ungroup, slice, arrange, everything],
  tidyr[pivot_longer, pivot_wider]
)

#' @export
run_1x2 <- function(post, odds, cfg) {
  if (is.null(odds) || nrow(odds) == 0) return(NULL)

  leagues <- cfg$leagues
  divisions <- cfg$predictions$divisions
  tie_threshold <- cfg$scoring$tie_threshold
  has_ties <- cfg$scoring$has_ties

  # Filter to configured divisions
  post_filtered <- post
  if (!is.null(divisions)) {
    post_filtered <- post_filtered |> filter(division %in% divisions)
  }

  # Compute probabilities from posterior draws
  probs <- post_filtered |>
    mutate(goal_diff = home_goals - away_goals) |>
    summarise(
      p_home = mean(goal_diff > tie_threshold),
      p_away = mean(goal_diff < -tie_threshold),
      p_tie = if (has_ties) mean(abs(goal_diff) <= tie_threshold) else 0,
      .by = c(date, division, home, away)
    )

  # Map division numbers to league names
  if (!is.null(leagues)) {
    league_vec <- unlist(leagues)
    probs <- probs |>
      mutate(division = {
        d <- as.character(division)
        ifelse(d %in% names(league_vec), league_vec[d], d)
      })
  }

  # Select odds columns based on whether ties exist
  if (has_ties) {
    odds_cols <- odds |>
      select(date, any_of("league"), booker, home, away, o_home, o_tie = o_draw, o_away)
  } else {
    odds_cols <- odds |>
      select(date, any_of("league"), booker, home, away, o_home, o_away)
  }

  result <- probs |>
    inner_join(odds_cols, by = c("date", "home", "away")) |>
    select(date, division, any_of("league"), booker, home, away, everything())

  # Build pivot columns
  if (has_ties) {
    pivot_cols <- c("p_home", "p_away", "p_tie", "o_home", "o_tie", "o_away")
  } else {
    pivot_cols <- c("p_home", "p_away", "o_home", "o_away")
  }

  result <- result |>
    rename(heima = home, gestir = away) |>
    pivot_longer(
      all_of(pivot_cols),
      names_to = c("type", "outcome"),
      names_sep = "_"
    ) |>
    pivot_wider(names_from = type) |>
    # Kelly pass 1: optimal allocation across all outcomes per match
    mutate(
      kelly = get_kelly(p, o),
      .by = c(gestir, division, any_of("league"), date, heima, booker)
    ) |>
    filter(
      kelly == max(kelly),
      .by = c(heima, division, any_of("league"), date, gestir, outcome)
    ) |>
    group_by(date, division, across(any_of("league")), heima, gestir, outcome) |>
    slice(1) |>
    ungroup() |>
    # Kelly pass 2: recompute with filtered outcome set
    mutate(
      kelly = get_kelly(p, o),
      .by = c(date, division, any_of("league"), heima, gestir)
    ) |>
    format_bet_text(cfg) |>
    filter(bet_amount >= cfg$bankroll$min_bet_amount)

  if (nrow(result) == 0) return(NULL)
  result
}
