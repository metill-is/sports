#' Over/under (total goals/points) market pipeline
#'
#' Joins posterior draws with totals odds (many-to-many across limit lines),
#' runs two-pass Kelly. Sport-agnostic.

box::use(
  ./kelly[apply_two_pass_kelly],
  dplyr[filter, mutate, summarise, select, any_of, rename, inner_join],
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
    apply_two_pass_kelly(cfg, extra_group = "limit")

  if (nrow(result) == 0) return(NULL)
  result
}
