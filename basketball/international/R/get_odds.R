box::use(
  R / common / lengjan_info,
  R / common / scrape_1x2[get_odds]
)

library(tidyverse)
library(purrr)

output <- list()
sport <- 1
country <- "england"

for (league in names(lengjan_info[[country]]$leagues)) {
  output[[country]][[league]] <- get_odds(
    sport,
    lengjan_info[[country]]$country,
    lengjan_info[[country]]$leagues[[league]]$competition
  ) |>
    mutate(
      country = country,
      league = league
    )
  Sys.sleep(1)
}

output |>
  map(list_rbind) |>
  list_rbind() |>
  select(date = dates, country, league, home, away, o_home, o_draw, o_away) |>
  mutate_at(vars(home, away), str_squish) |>
  inner_join(
    read_csv("data/england/team_names.csv"),
    by = join_by("home" == "in")
  ) |>
  select(-home) |>
  rename(home = out) |>
  inner_join(
    read_csv("data/england/team_names.csv"),
    by = join_by("away" == "in")
  ) |>
  select(-away) |>
  rename(away = out) |>
  write_csv("data/odds.csv")
