box::use(
  R / utils / lengjan_info,
  R / utils / scrape_1x2[get_odds]
)

library(tidyverse)
library(purrr)

output <- list()
sport <- 1
country <- "iceland"

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

name_fun <- function(name_string) {
  case_when(
    name_string == "Víkingur Rvk" ~ "Víkingur R.",
    TRUE ~ name_string
  )
}

output |>
  map(list_rbind) |>
  list_rbind() |> 
  select(date = dates, country, league, home, away, o_home, o_draw, o_away) |>
  mutate_at(vars(home, away), str_squish) |>
  mutate_at(
    vars(home, away),
    name_fun
  ) |> 
  write_csv("data/odds.csv")
