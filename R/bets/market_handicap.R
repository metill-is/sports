#' Handicap market pipeline
#'
#' Handles two types of handicap lines:
#'
#' - **Whole-goal** (+-1, +-2, ...): European 3-way handicap. Draw-after-handicap
#'   is a separate bettable outcome with its own odds (home/draw/away).
#' - **Fractional** (+-0.5, +-1.5, ...): Asian handicap. No draw possible, so
#'   purely 2-way (home/away).
#'
#' Sport-agnostic: tie threshold driven by cfg$scoring.

box::use(
  ./kelly[apply_two_pass_kelly],
  dplyr[filter, mutate, summarise, select, any_of, rename, inner_join, bind_rows],
  tidyr[pivot_longer, pivot_wider],
  stringr[str_split_fixed]
)

#' Parse Lengjan handicap strings to signed numeric
#'
#' "0-1" -> -1 (away gets 1-goal head start)
#' "1-0" -> +1 (home gets 1-goal head start)
#' @export
parse_handicap <- function(change_str) {
  parts <- str_split_fixed(change_str, "-", n = 2)
  result <- as.numeric(parts[, 1]) - as.numeric(parts[, 2])
  if (any(is.na(result))) {
    warning("Could not parse handicap values: ",
            paste(change_str[is.na(result)], collapse = ", "))
  }
  result
}

#' European 3-way handicap (whole-goal lines)
#'
#' Draw-after-handicap is a bettable outcome with its own odds.
run_european <- function(odds, post_goals, cfg) {
  if (nrow(odds) == 0) return(NULL)

  leagues <- cfg$leagues
  tie_threshold <- cfg$scoring$tie_threshold

  result <- odds |>
    inner_join(
      post_goals,
      by = c("date", "home", "away"),
      relationship = "many-to-many"
    )

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
      change, o_home, o_tie = o_draw, o_away, value
    ) |>
    summarise(
      p_home = mean(value + change > tie_threshold),
      p_tie = mean(abs(value + change) <= tie_threshold),
      p_away = mean(value + change < -tie_threshold),
      .by = c(date, division, any_of("league"), booker, heima, gestir, change, o_home, o_tie, o_away)
    ) |>
    pivot_longer(
      c(o_home, o_tie, o_away, p_home, p_tie, p_away),
      names_to = c("type", "outcome"),
      names_sep = "_"
    ) |>
    pivot_wider(names_from = type) |>
    apply_two_pass_kelly(cfg, extra_group = "change")

  if (is.null(result) || nrow(result) == 0) return(NULL)
  result
}

#' Asian handicap (fractional lines)
#'
#' No draw possible -- purely 2-way (home/away).
run_asian <- function(odds, post_goals, cfg) {
  if (nrow(odds) == 0) return(NULL)

  leagues <- cfg$leagues

  result <- odds |>
    select(-any_of("o_draw")) |>
    inner_join(
      post_goals,
      by = c("date", "home", "away"),
      relationship = "many-to-many"
    )

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
      change, o_home, o_away, value
    ) |>
    summarise(
      p_home = mean(value + change > 0),
      p_away = mean(value + change < 0),
      .by = c(date, division, any_of("league"), booker, heima, gestir, change, o_home, o_away)
    ) |>
    pivot_longer(
      c(o_home, o_away, p_home, p_away),
      names_to = c("type", "outcome"),
      names_sep = "_"
    ) |>
    pivot_wider(names_from = type) |>
    apply_two_pass_kelly(cfg, extra_group = "change")

  if (is.null(result) || nrow(result) == 0) return(NULL)
  result
}

#' @export
run_handicap <- function(post, odds, cfg) {
  if (is.null(odds) || nrow(odds) == 0) return(NULL)

  divisions <- cfg$predictions$divisions

  # Parse handicap if change column is character (Lengjan "0-1" format)
  if (is.character(odds$change)) {
    odds <- odds |> mutate(change = parse_handicap(change))
  }
  odds <- odds |> mutate(is_whole = change == round(change))

  post_goals <- post
  if (!is.null(divisions)) {
    post_goals <- post_goals |> filter(division %in% divisions)
  }
  post_goals <- post_goals |>
    mutate(value = home_goals - away_goals) |>
    select(date, division, home, away, value)

  if (isTRUE(cfg$scoring$has_ties)) {
    result <- bind_rows(
      run_european(odds |> filter(is_whole), post_goals, cfg),
      run_asian(odds |> filter(!is_whole), post_goals, cfg)
    )
  } else {
    # No ties (e.g. basketball): all lines are 2-way, skip European 3-way
    result <- run_asian(odds, post_goals, cfg)
  }

  if (nrow(result) == 0) return(NULL)
  result
}
