#' Over/under (total goals/points) market pipeline
#'
#' Joins posterior draws with totals odds (many-to-many across limit lines),
#' runs two-pass Kelly. Sport-agnostic.

box::use(
  ./kelly[get_kelly, format_bet_text],
  dplyr[filter, mutate, summarise, select, any_of, across, rename, inner_join,
        group_by, ungroup, slice, arrange],
  tidyr[pivot_longer, pivot_wider]
)

#' @export
run_totals <- function(post, odds, cfg) {
  if (is.null(odds) || nrow(odds) == 0) return(NULL)

  leagues <- cfg$leagues
  divisions <- cfg$predictions$divisions

  post_filtered <- post
  if (!is.null(divisions)) {
    post_filtered <- post_filtered |> filter(division %in% divisions)
  }

  result <- post_filtered |>
    inner_join(
      odds |> rename(date_game = date),
      by = c("date" = "date_game", "home", "away"),
      relationship = "many-to-many"
    ) |>
    mutate(total_goals = home_goals + away_goals)

  if (!is.null(leagues)) {
    league_vec <- unlist(leagues)
    result <- result |>
      mutate(division = {
        d <- as.character(division)
        ifelse(d %in% names(league_vec), league_vec[d], d)
      })
  }

  result <- result |>
    select(
      date, division, any_of("league"), booker, heima = home, gestir = away,
      total_goals, limit, o_over, o_under
    ) |>
    summarise(
      p_over = mean(total_goals > limit),
      p_under = mean(total_goals <= limit),
      .by = c(date, division, any_of("league"), heima, gestir, booker, o_over, o_under, limit)
    ) |>
    pivot_longer(
      c(o_over, o_under, p_over, p_under),
      names_to = c("type", "outcome"),
      names_sep = "_"
    ) |>
    pivot_wider(names_from = type) |>
    # Kelly pass 1: optimal allocation per match x limit line
    mutate(
      kelly = get_kelly(p, o),
      .by = c(date, division, any_of("league"), booker, heima, gestir, limit)
    ) |>
    filter(
      kelly == max(kelly),
      .by = c(date, any_of("league"), heima, gestir, outcome)
    ) |>
    group_by(date, across(any_of("league")), heima, gestir, outcome) |>
    slice(1) |>
    ungroup() |>
    # Kelly pass 2: recompute with filtered outcome set
    mutate(
      kelly = get_kelly(p, o),
      .by = c(date, division, any_of("league"), booker, heima, gestir)
    ) |>
    format_bet_text(cfg) |>
    filter(bet_amount >= cfg$bankroll$min_bet_amount)

  if (nrow(result) == 0) return(NULL)
  result
}
