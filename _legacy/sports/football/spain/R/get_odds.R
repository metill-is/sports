box::use(
  R / league_info,
  R / scrape_1x2[get_odds]
)

library(tidyverse)
library(purrr)

output <- list()
sport <- 1
country <- "spain"

for (league in names(league_info[[country]]$leagues)) {
  output[[country]][[league]] <- get_odds(
    sport,
    league_info[[country]]$country,
    league_info[[country]]$leagues[[league]]$competition
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
    read_csv("data/team_names.csv"),
    by = join_by("home" == "in")
  ) |>
  select(-home) |>
  rename(home = out) |>
  inner_join(
    read_csv("data/team_names.csv"),
    by = join_by("away" == "in")
  ) |>
  select(-away) |>
  rename(away = out) |>
  write_csv("data/odds.csv")

output$spain$`LaLiga` |>
  pivot_longer(c(home, away)) |>
  distinct(value) |>
  arrange(value) |>
  print(n = Inf)
